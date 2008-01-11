;;;; All the daemon and fuse functions are in here
;;;;
;;;; @(#)daemons.c	3.7 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defun doctor (&optional arg)
  "A healing daemon that restors hit points after rest."
  (declare (ignore arg))
  (let ((lv (stats-s-lvl pstats))
        (ohp (stats-s-hpt pstats)))
    (incf quiet)
    (if (< lv 8)
        (when (> quiet (- 20 (* lv 2)))
          (incf (stats-s-hpt pstats)))
        (when (>= quiet 3)
          (incf (stats-s-hpt pstats) (1+ (rnd (- lv 7))))))
    (when (isring LEFT R-REGEN)
      (incf (stats-s-hpt pstats)))
    (when (isring RIGHT R-REGEN)
      (incf (stats-s-hpt pstats)))
    (unless (= ohp (stats-s-hpt pstats))
      (when (> (stats-s-hpt pstats) max-hp)
        (setf (stats-s-hpt pstats) max-hp))
      (setf quiet 0))))

(defun swander (&optional arg)
  "Called when it is time to start rolling for wandering monsters."
  (declare (ignore arg))
  (daemon 'rollwand 0 BEFORE))

(define-resettable between 0)

(defun rollwand (&optional arg)
  "Called to roll to see if a wandering monster starts up."
  (declare (ignore arg))
  (when (>= (incf between) 4)
    (when (= (roll 1 6) 4)
      (wanderer)
      (kill-daemon 'rollwand)
      (fuse 'swander 0 WANDERTIME BEFORE))
    (setf between 0)))

(defun unconfuse (&optional arg)
  "Release the poor player from his confusion."
  (declare (ignore arg))
  (logclr! (thing-t-flags *player*) ISHUH)
  (msg "You feel less confused now"))

(defun unsee (&optional arg)
  "He lost his see invisible power."
  (declare (ignore arg))
  (logclr! (thing-t-flags *player*) CANSEE))

(defun sight (&optional arg)
  "He gets his sight back."
  (declare (ignore arg))
  (when (on *player* ISBLIND)
    (extinguish #'sight)
    (logclr! (thing-t-flags *player*) ISBLIND)
    (light hero)
    (msg "The veil of darkness lifts")))

(defun nohaste (&optional arg)
  "End the hasting."
  (declare (ignore arg))
  (logclr! (thing-t-flags *player*) ISHASTE)
  (msg "You feel yourself slowing down."))

(defun stomach (&optional arg)
  "Digest the hero's food."
  (declare (ignore arg))
  (if (<= food-left 0)
      (progn
        ;;
        ;; the hero is fainting
        ;;
        (unless (or (nonzerop no-command) (> (rnd 100) 20))
          (setf no-command (+ (rnd 8) 4))
          (unless terse
            (addmsg "You feel too weak from lack of food.  "))
          (msg "You faint")
          (setf running nil
                *count* 0
                hungry-state 3)))
      (progn
        (let ((oldfood food-left))
          (decf food-left (+ (ring-eat LEFT) 
                             (ring-eat RIGHT) 
                             1 
                             (- (if *amulet* 1 0))))
          (if (and (< food-left MORETIME) (>= oldfood MORETIME))
              (progn
                (msg "You are starting to feel weak")
                (setf hungry-state 2))
              (progn
                (when (and (< food-left (* 2 MORETIME)) (>= oldfood (* 2 MORETIME)))
                  (if terse
                      (msg "Getting hungry")
                      (msg "You are starting to get hungry"))
                  (setf hungry-state 1))))))))
