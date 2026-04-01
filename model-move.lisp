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
;;;;
;;;; Behaviour overrides (applied after model prediction):
;;;;   • monster HP < 15% of max  → return NIL (fall back to A* retreat)
;;;;   • model returns stay ('.')  and player is within 10 tiles → return NIL
;;;;     (A* will chase instead)

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
  "Persistent SB-BSD-SOCKETS:LOCAL-SOCKET, or NIL.")
(defvar *model-stream* nil
  "Bivalent stream wrapping *model-socket*, or NIL.")

;;; ── Connection management ─────────────────────────────────────────────── ;;;

(defun model-disconnect ()
  "Close the model-server connection."
  (when *model-stream*
    (ignore-errors (close *model-stream*))
    (setf *model-stream* nil))
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
        (setf *model-stream*
              (sb-bsd-sockets:socket-make-stream
               sock :input t :output t
               :element-type :default
               :buffering :full))
        t)
    (error () nil)))

;;; ── Wire I/O ──────────────────────────────────────────────────────────── ;;;

(defun model-send (json-str)
  "Send JSON string + newline to the model server."
  (write-string json-str *model-stream*)
  (write-char #\Newline *model-stream*)
  (finish-output *model-stream*))

(defun model-recv-byte ()
  "Block until the model server sends 1 byte; return it as integer or NIL."
  (let ((byte (read-byte *model-stream* nil nil)))
    byte))

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
               ;; Player's cell — winat reads stdscr which has floor,
               ;; but training data had '@' from the VT100 screen.
               ((and (= r hero.y) (= c hero.x)) #\@)
               ;; In-bounds dungeon cell (rows 1–22, cols 0–79)
               ((and (>= r 1) (<= r 22) (>= c 0) (< c 80))
                (let* ((raw  (winat r c))
                       (code (if (characterp raw) (char-code raw) 32)))
                  ;; Clamp to printable ASCII
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

(defun model-monster-hp-frac (th)
  "Return monster current HP / max HP as a float in [0,1].
  Uses THING-T-RESERVED as the max HP stored at spawn time."
  (let ((cur (stats-s-hpt (thing-t-stats th)))
        (mx  (thing-t-reserved th)))
    (if (plusp mx)
        (float (/ cur mx))
        1.0)))

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
         (mhp      (model-monster-hp-frac th))
         (n-allies (max 0 (1- (length mlist))))
         (window   (model-build-grid-window mr mc))
         (mtype    (string (thing-t-type th))))
    (format nil
            "{\"monster_type\":~a,\"mr\":~d,\"mc\":~d,\"pr\":~d,\"pc\":~d,~
\"player_hp_frac\":~,3f,\"monster_hp_frac\":~,3f,\"n_allies\":~d,~
\"grid_window\":[~{~a~^,~}]}"
            (model-json-string mtype)
            mr mc pr pc
            php mhp
            n-allies
            (mapcar #'model-json-string window))))

;;; ── Public API ────────────────────────────────────────────────────────── ;;;

(defun model-query (th)
  "Query the model server for an action index (0–8).
  Lazily connects on first call; reconnects after errors.
  Returns NIL when the server is unavailable."
  (unless *model-stream*
    (model-connect))
  (when *model-stream*
    (handler-case
        (progn
          (model-send (model-build-json th))
          (model-recv-byte))
      (error ()
        (model-disconnect)
        nil))))

(defun model-next-coord (th)
  "Return the next COORD for model-driven monster TH, or NIL to fall
  back to normal A* chase.

  Returns NIL in override cases:
    1. Monster HP < 15% of max  (A* retreat logic handles it better)
    2. Model says stay + player within 10 tiles  (A* will keep pressure on)"
  ;; Override 1: badly wounded → let A* retreat
  (let ((mhp-frac (model-monster-hp-frac th)))
    (when (< mhp-frac 0.15)
      (return-from model-next-coord nil)))

  ;; Query model
  (let ((idx (model-query th)))
    (unless (and idx (< idx 9))
      (return-from model-next-coord nil))

    ;; Override 2: model says stay, but player is reachable → let A* chase
    (when (= idx 4)
      (let* ((pos  (thing-t-pos th))
             (dist (+ (abs (- hero.y (coord-y pos)))
                      (abs (- hero.x (coord-x pos))))))
        (when (<= dist 10)
          (return-from model-next-coord nil))))

    ;; Map to coord, validating it's a legal move
    (let* ((delta (aref *model-action-deltas* idx))
           (dr    (car delta))
           (dc    (cdr delta))
           (pos   (thing-t-pos th))
           (ny    (+ (coord-y pos) dr))
           (nx    (+ (coord-x pos) dc)))
      ;; Only return coord if in-bounds, walkable, and diagonal-ok
      (when (and (>= ny 1) (<= ny 22) (>= nx 0) (< nx 80))
        (let ((dest (make-coord :y ny :x nx)))
          (if (and (diag-ok pos dest)
                   (or (equalp dest hero)         ; attacking hero is always ok
                       (step-ok (winat ny nx))))
              dest
              nil))))))

;;; ── Training data collection ─────────────────────────────────────────── ;;;
;;; Records (state, action) pairs from live gameplay using the same feature
;;; builder as inference, ensuring training/inference feature parity.

(defparameter *collect-training-data* t
  "When non-nil, record monster (state, action) pairs during gameplay.")
(defparameter *training-data-path* "/tmp/cl-rogue-training.jsonl")
(defvar *training-data-stream* nil)

(defparameter *delta-to-action*
  '(((-1 . -1) . "y") ((-1 . 0) . "k") ((-1 . 1) . "u")
    (( 0 . -1) . "h") (( 0 . 0) . ".") (( 0 . 1) . "l")
    ((1 . -1) . "b")  ((1 . 0) . "j")  ((1 . 1) . "n")))

(defun training-data-ensure-stream ()
  "Open the training data file for appending (lazy open)."
  (unless (and *training-data-stream*
               (open-stream-p *training-data-stream*))
    (setf *training-data-stream*
          (open *training-data-path* :direction :output
                                     :if-exists :append
                                     :if-does-not-exist :create
                                     :external-format :utf-8))))

(defun collect-training-example (json-state old-pos new-pos)
  "Append one training example: the JSON state + the action actually taken.
  JSON-STATE is the model-build-json output (without action field).
  OLD-POS / NEW-POS are monster coords before/after the move."
  (ignore-errors
    (let* ((dy (- (coord-y new-pos) (coord-y old-pos)))
           (dx (- (coord-x new-pos) (coord-x old-pos)))
           (act (cdr (assoc (cons dy dx) *delta-to-action* :test #'equal))))
      (when act
        (training-data-ensure-stream)
        ;; Splice "action":"X" into the JSON before the closing }
        (let ((base (string-right-trim "}" json-state)))
          (format *training-data-stream* "~a,\"action\":\"~a\"}~%" base act)
          (finish-output *training-data-stream*))))))

;;; ── Debug state dump ────────────────────────────────────────────────── ;;;

(defparameter *state-dump-path* "/tmp/cl-rogue-state.log")

(defun dump-game-state ()
  "Write player + monster positions to *state-dump-path* (overwritten each turn)."
  (ignore-errors
    (with-open-file (out *state-dump-path* :direction :output
                                           :if-exists :supersede
                                           :if-does-not-exist :create)
      (format out "player ~d,~d  hp=~d/~d  level=~d~%"
              hero.y hero.x
              (stats-s-hpt pstats) max-hp level)
      (dolist (tp mlist)
        (let* ((pos  (thing-t-pos tp))
               (my   (coord-y pos))
               (mx   (coord-x pos))
               (dist (+ (abs (- hero.y my)) (abs (- hero.x mx))))
               (run  (if (on tp ISRUN) "RUN" "   "))
               (mdl  (if (on tp ISMODEL) "MDL" "   ")))
          (format out "  ~a ~a ~a @~d,~d  dist=~d~%"
                  (thing-t-type tp) run mdl my mx dist))))))
