;;;; routines dealing specifically with rings
;;;; 
;;;; @(#)rings.c	3.17 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defmacro left-ring ()
  `(aref cur-ring LEFT))

(defmacro right-ring ()
  `(aref cur-ring RIGHT))

(defun ring-on ()
  (when-let (obj (get-item "put on" RING))
    ;; Make certain that it is somethings that we want to wear
    (unless (eql (object-o-type obj) RING)
      (if terse
          (msg "Not a ring")
          (msg "It would be difficult to wrap that around a finger"))
      (return-from ring-on))

    ;; find out which hand to put it on
    (when (is-current obj)
      (return-from ring-on))

    (let (which-ring)
      (cond
        ((and
          (null (aref cur-ring LEFT))
          (null (aref cur-ring RIGHT)))
         (when (minusp (setf which-ring (gethand)))
           (return-from ring-on)))
        ((null (aref cur-ring LEFT))
         (setf which-ring LEFT))
        ((null (aref cur-ring RIGHT))
         (setf which-ring RIGHT))
        (t
         (if terse
             (msg "Wearing two")
             (msg "You already have a ring on each hand"))
         (return-from ring-on)))
      (setf (aref cur-ring which-ring) obj)

      ;; Calculate the effect it has on the poor guy.
      (case (object-o-which obj)
        (#.R-ADDSTR
         (let ((save-max (stats-s-str max-stats))) ; TODO: max-stats should be a macro??
           (chg-str (object-o-ac obj))
           (setf (stats-s-str max-stats) save-max)))
        (#.R-SEEINVIS
         (logior! (thing-t-flags *player*) CANSEE)
         (light hero)
         (rogue-mvwaddch cw hero.y hero.x PLAYER))
        (#.R-AGGR
         (aggravate)))
      (status)
      (cond
        ((and (aref r-know (object-o-which obj))
              (aref r-guess (object-o-which obj)))
         (setf (aref r-guess (object-o-which obj)) nil))
        ((and
          (not (aref r-know (object-o-which obj)))
          askme
          (null (aref r-guess (object-o-which obj))))
         (zero! mpos)
         (msg (if terse "Call it: " "What do you want to call it? "))
         (let ((buf ""))
           (when (eql (get-str buf cw) NORM)
             (setf (aref r-guess (object-o-which obj)) buf)))
         (msg ""))))))

(defun ring-off ()
  (let (which-ring)
    (cond
      ((and (null (left-ring))
            (null (right-ring)))
       (msg (if "No rings" "You aren't wearing any rings"))
       (return-from ring-off))
      ((null (left-ring))
       (setf which-ring RIGHT))
      ((null (right-ring))
       (setf which-ring LEFT))
      (t 
       (when (minusp (setf which-ring (gethand))))
       (return-from ring-off)))
    (zero! mpos)
    (let ((obj (aref cur-ring which-ring)))
      (unless obj
        (msg "Not wearing such a ring")
        (return-from ring-off))
      (when (dropcheck obj)
        (msg "Was wearing ~a" (inv-name obj t))))))

(defun gethand ()
  (loop
     (msg (if terse "Left or Right ring? " "Left hand or right hand? "))
     (let ((c (readchar)))
       (case c
         ((#\l #\L) 
          (return-from gethand LEFT))
         ((#\r #\R) 
          (return-from gethand RIGHT))
         (#\Escape
          (return-from gethand -1)))
       (zero! mpos)
       (msg (if terse "L or R" "Please type L or R")))))

(defun ring-eat (hand)
  "How much food does this ring use up?"
  (let ((obj (aref cur-ring hand)))
    (if obj
        (case (object-o-which obj)
          (#.R-REGEN 2)
          (#.R-SUSTSTR 1)
          (#.R-SEARCH (if (< (rnd 100) 33) 1 0))
          (#.R-DIGEST (if (< (rnd 100) 50) -1 0))
          (otherwise 0))
        0)))

(defun ring-num (obj)
  "Print ring bonuses."
  (unless (logtest (object-o-flags obj) ISKNOW)
    (return-from ring-num ""))
  (case (object-o-which obj)
    ((#.R-PROTECT
      #.R-ADDSTR
      #.R-ADDDAM
      #.R-ADDHIT)
     (concatenate 'string " " (num (object-o-ac obj) 0)))
    (otherwise "")))
