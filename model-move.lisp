;;;; model-move.lisp — XGBoost monster AI via Unix-socket inference server
;;;;
;;;; Provides MODEL-NEXT-COORD, called from DO-CHASE (chase.lisp) when
;;;; the ISMODEL flag is set on a monster.
;;;;
;;;; Protocol (one query per call):
;;;;   Lisp → server: JSON line (ASCII, newline-terminated)
;;;;   Server → Lisp: 1 byte  (action index 0–8)
;;;;
;;;; Action indices match ACTION_ORDER in train_model.py:
;;;;   0=y  1=k  2=u  3=h  4=.  5=l  6=b  7=j  8=n

(in-package :cl-rogue)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-bsd-sockets))

;;; ── Action index → (delta-row . delta-col) ────────────────────────────── ;;;
;;; Matches ACTION_ORDER = ['y','k','u','h','.','l','b','j','n']
(defparameter *model-action-deltas*
  #((-1 . -1)    ; 0  y  up-left
    (-1 .  0)    ; 1  k  up
    (-1 . +1)    ; 2  u  up-right
    ( 0 . -1)    ; 3  h  left
    ( 0 .  0)    ; 4  .  stay
    ( 0 . +1)    ; 5  l  right
    (+1 . -1)    ; 6  b  down-left
    (+1 .  0)    ; 7  j  down
    (+1 . +1)))  ; 8  n  down-right

(defparameter *model-socket-path* "/tmp/cl-rogue-model.sock")

(defvar *model-socket* nil
  "Persistent SB-BSD-SOCKETS:LOCAL-SOCKET connection, or NIL.")

;;; ── Connection management ─────────────────────────────────────────────── ;;;

(defun model-disconnect ()
  "Close the model-server connection."
  (when *model-socket*
    (ignore-errors (sb-bsd-sockets:socket-close *model-socket*))
    (setf *model-socket* nil)))

(defun model-connect ()
  "Open a fresh connection to the model server.
  Returns T on success, NIL if the server is not running."
  (model-disconnect)
  (handler-case
      (let ((sock (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
        (sb-bsd-sockets:socket-connect sock *model-socket-path*)
        (setf *model-socket* sock)
        t)
    (error () nil)))

;;; ── Wire I/O ──────────────────────────────────────────────────────────── ;;;

(defun model-send (json-str)
  "Send JSON string + newline to the model server."
  (let ((bytes (map '(vector (unsigned-byte 8))
                    #'char-code
                    (concatenate 'string json-str (string #\Newline)))))
    (sb-bsd-sockets:socket-send *model-socket* bytes (length bytes))))

(defun model-recv-byte ()
  "Block until the model server sends 1 byte; return it as integer or NIL."
  (let ((buf (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)))
    (let ((n (sb-bsd-sockets:socket-receive *model-socket* buf 1)))
      (when (and n (= n 1))
        (aref buf 0)))))

;;; ── Feature-state builder ─────────────────────────────────────────────── ;;;

(defun model-build-grid-window (mr mc)
  "Return 9×9 ASCII grid window as a list of 9 strings, centred on (MR, MC).
  Out-of-bounds cells are spaces; the monster's own cell is 'M'."
  (let (rows)
    (dotimes (dr 9)
      (let ((r (+ mr (- dr 4)))
            chars)
        (dotimes (dc 9)
          (let ((c (+ mc (- dc 4))))
            (push
             (cond
               ;; Monster's own cell
               ((and (= dr 4) (= dc 4)) #\M)
               ;; In-bounds dungeon cell (rows 1–22, cols 0–79)
               ((and (>= r 1) (<= r 22) (>= c 0) (< c 80))
                (let* ((raw (winat r c))
                       (code (if (characterp raw) (char-code raw) 32)))
                  ;; Clamp to printable ASCII to avoid encoding surprises
                  (if (and (>= code 32) (< code 128))
                      (code-char code)
                      #\Space)))
               ;; Out of dungeon bounds
               (t #\Space))
             chars)))
        (push (coerce (nreverse chars) 'string) rows)))
    (nreverse rows)))

(defun model-json-string (s)
  "Emit S as a JSON double-quoted string (handles backslash and quote)."
  (with-output-to-string (out)
    (write-char #\" out)
    (dotimes (i (length s))
      (let ((c (char s i)))
        (cond ((char= c #\\) (write-string "\\\\" out))
              ((char= c #\") (write-string "\\\"" out))
              (t             (write-char c out)))))
    (write-char #\" out)))

(defun model-build-json (th)
  "Build the JSON request line for monster TH."
  (let* ((pos      (thing-t-pos th))
         (mr       (coord-y pos))
         (mc       (coord-x pos))
         (pr       hero.y)
         (pc       hero.x)
         (php      (if (plusp max-hp)
                       (float (/ (stats-s-hpt pstats) max-hp))
                       1.0))
         (n-allies (max 0 (1- (length mlist))))
         (window   (model-build-grid-window mr mc))
         (mtype    (string (thing-t-type th))))
    (format nil
            "{\"monster_type\":~a,\"mr\":~d,\"mc\":~d,\"pr\":~d,\"pc\":~d,\
\"player_hp_frac\":~,3f,\"monster_hp_frac\":1.0,\"n_allies\":~d,\
\"grid_window\":[~{~a~^,~}]}"
            (model-json-string mtype)
            mr mc pr pc
            php
            n-allies
            (mapcar #'model-json-string window))))

;;; ── Public API ────────────────────────────────────────────────────────── ;;;

(defun model-query (th)
  "Query the model server for an action index (0–8).
  Lazily connects on first call; reconnects after errors.
  Returns NIL when the server is unavailable."
  (unless *model-socket*
    (model-connect))
  (when *model-socket*
    (handler-case
        (progn
          (model-send (model-build-json th))
          (model-recv-byte))
      (error ()
        (model-disconnect)
        nil))))

(defun model-next-coord (th)
  "Return the next COORD for model-driven monster TH, or NIL if the
  model server is unavailable (causing fallback to normal A* chase)."
  (let ((idx (model-query th)))
    (when (and idx (< idx 9))
      (let* ((delta (aref *model-action-deltas* idx))
             (dr    (car delta))
             (dc    (cdr delta))
             (pos   (thing-t-pos th))
             (ny    (+ (coord-y pos) dr))
             (nx    (+ (coord-x pos) dc)))
        ;; Only return coord if still within dungeon bounds
        (when (and (>= ny 1) (<= ny 22) (>= nx 0) (< nx 80))
          (make-coord :y ny :x nx))))))
