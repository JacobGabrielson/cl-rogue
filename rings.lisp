;;;; routines dealing specifically with rings
;;;; 
;;;; @(#)rings.c	3.17 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defmacro left-ring ()
  `(aref cur_ring LEFT))

(defmacro right-ring ()
  `(aref cur_ring RIGHT))

(defun ring_on ()
  (when-let (obj (get_item "put on" RING))
    ;; Make certain that it is somethings that we want to wear
    (unless (eql (object-o_type obj) RING)
      (if terse
          (msg "Not a ring")
          (msg "It would be difficult to wrap that around a finger"))
      (return-from ring_on))

    ;; find out which hand to put it on
    (when (is_current obj)
      (return-from ring_on))

    (let (which-ring)
      (cond
        ((and
          (null (aref cur_ring LEFT))
          (null (aref cur_ring RIGHT)))
         (when (minusp (setf which-ring (gethand)))
           (return-from ring_on)))
        ((null (aref cur_ring LEFT))
         (setf which-ring LEFT))
        ((null (aref cur_ring RIGHT))
         (setf which-ring RIGHT))
        (t
         (if terse
             (msg "Wearing two")
             (msg "You already have a ring on each hand"))
         (return-from ring_on)))
      (setf (aref cur_ring which-ring) obj)

      ;; Calculate the effect it has on the poor guy.
      (case (object-o_which obj)
        (#.R_ADDSTR
         (let ((save_max (stats-s_str max_stats))) ; TODO: max_stats should be a macro??
           (chg_str (object-o_ac obj))
           (setf (stats-s_str max_stats) save_max)))
        (#.R_SEEINVIS
         (logior! (thing-t_flags *player*) CANSEE)
         (light hero)
         (rogue-mvwaddch cw hero.y hero.x PLAYER))
        (#.R_AGGR
         (aggravate)))
      (status)
      (cond
        ((and (aref r_know (object-o_which obj))
              (aref r_guess (object-o_which obj)))
         (setf (aref r_guess (object-o_which obj)) nil))
        ((and
          (not (aref r_know (object-o_which obj)))
          askme
          (null (aref r_guess (object-o_which obj))))
         (zero! mpos)
         (msg (if terse "Call it: " "What do you want to call it? "))
         (let ((buf ""))
           (when (eql (get_str buf cw) NORM)
             (setf (aref r_guess (object-o_which obj)) buf)))
         (msg ""))))))

(defun ring_off ()
  (let (which-ring)
    (cond
      ((and (null (left-ring))
            (null (right-ring)))
       (msg (if "No rings" "You aren't wearing any rings"))
       (return-from ring_off))
      ((null (left-ring))
       (setf which-ring RIGHT))
      ((null (right-ring))
       (setf which-ring LEFT))
      (t 
       (when (minusp (setf which-ring (gethand))))
       (return-from ring_off)))
    (zero! mpos)
    (let ((obj (aref cur_ring which-ring)))
      (unless obj
        (msg "Not wearing such a ring")
        (return-from ring_off))
      (when (dropcheck obj)
        (msg "Was wearing ~a" (inv_name obj t))))))

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

(defun ring_eat (hand)
  "How much food does this ring use up?"
  (let ((obj (aref cur_ring hand)))
    (if obj
        (case (object-o_which obj)
          (#.R_REGEN 2)
          (#.R_SUSTSTR 1)
          (#.R_SEARCH (if (< (rnd 100) 33) 1 0))
          (#.R_DIGEST (if (< (rnd 100) 50) -1 0))
          (otherwise 0))
        0)))

(defun ring_num (obj)
  "Print ring bonuses."
  (unless (logtest (object-o_flags obj) ISKNOW)
    (return-from ring_num ""))
  (case (object-o_which obj)
    ((#.R_PROTECT
      #.R_ADDSTR
      #.R_ADDDAM
      #.R_ADDHIT)
     (concatenate 'string " " (num (object-o_ac obj) 0)))
    (otherwise "")))
