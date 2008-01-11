;;;; Functions for dealing with problems brought about by weapons
;;;; @(#)weapons.c	3.17 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defparameter w-names
  (vector
   "mace"
   "long sword"
   "short bow"
   "arrow"
   "dagger"
   "rock"
   "two handed sword"
   "sling"
   "dart"
   "crossbow"
   "crossbow bolt"
   "spear"))

(defparameter init-dam
  (vector
   (list "2d4" "1d3" 100 0)                  ; Mace  
   (list "1d10" "1d2" 100 0)                 ; Long sword 
   (list "1d1" "1d1" 100 0)                  ; Bow 
   (list "1d1" "1d6" BOW (logior ISMANY ISMISL)) ; Arrow 
   (list "1d6" "1d4" 100 ISMISL)                 ; Dagger 
   (list "1d2" "1d4" SLING (logior ISMANY ISMISL)) ; Rock 
   (list "3d6" "1d2" 100 0)                        ; 2h sword 
   (list "0d0" "0d0" 100 0)                        ; Sling 
   (list "1d1" "1d3" 100 (logior ISMANY ISMISL))   ; Dart 
   (list "1d1" "1d1" 100 0)                        ; Crossbow 
   (list "1d2" "1d10" CROSSBOW (logior ISMANY ISMISL)) ; Crossbow bolt 
   (list "1d8" "1d6" 100 ISMISL)))                     ; Spear 

(defun missile (ydelta xdelta)
  "Fire a missile in a given direction."
  ;; Get which thing we are hurling
  (when-let (obj (get-item "throw" WEAPON))
    (symbol-macrolet ((obj->o-count (object-o-count obj)))
      (when (or (not (dropcheck obj))
                (is-current obj))
        (return-from missile))
      ;; Get rid of the thing.  If it is a non-multiple item object,
      ;; or if it is the last thing, just drop it.  Otherwise, create
      ;; a new item with a count of one.
      (if (< obj->o-count 2)
          (progn
            (detach pack obj)
            (decf inpack))
          (progn
            (decf obj->o-count)
            (when (zerop (object-o-group obj))
              (decf inpack))
            (setf obj (copy-structure obj))
            (setf obj->o-count 1)))
      (do-motion obj ydelta xdelta)
      ;; AHA! Here it has hit something.  If it is a wall or a door,
      ;; or if it misses (combat) the monster, put it on the floor
      (when (or (not (upper-case-p (rogue-mvwinch mw
                                                  (coord-y (object-o-pos obj))
                                                  (coord-x (object-o-pos obj)))))
                (not (hit-monster (coord-y (object-o-pos obj))
                                  (coord-x (object-o-pos obj)) obj)))
        (fall obj t))
      (rogue-mvwaddch cw hero.y hero.x PLAYER))))

(defun do-motion (obj ydelta xdelta)
  "Do the actual motion on the screen done by an object traveling
across the room."
  ;; Come fly with us ...
  (symbol-macrolet ((obj->o-pos (object-o-pos obj))
                    (obj->o-type (object-o-type obj))
                    (obj->o-pos.x (coord-x (object-o-pos obj)))
                    (obj->o-pos.y (coord-y (object-o-pos obj))))
    ;; COPY-STRUCTURE needed to avoid moving the player!
    (setf obj->o-pos (copy-structure hero))

    (loop
       ;; Erase the old one
       (when (and (not (equalp obj->o-pos hero))
                  (cansee obj->o-pos.y obj->o-pos.x)
                  (not (eql (rogue-mvwinch cw obj->o-pos.y obj->o-pos.x)
                            #\Space)))
         (rogue-mvwaddch cw
                         obj->o-pos.y
                         obj->o-pos.x 
                         (show obj->o-pos.y obj->o-pos.x)))
       ;; Get the new position
       (incf obj->o-pos.y ydelta)
       (incf obj->o-pos.x xdelta)
       (let ((ch (winat obj->o-pos.y obj->o-pos.x)))
         (if (and (step-ok ch) 
                  (not (eql ch DOOR)))
             (progn
               ;; It hasn't hit anything yet, so display it if it's
               ;; alright.
               (when (and (cansee obj->o-pos.y obj->o-pos.x)
                          (not (eql (rogue-mvwinch cw obj->o-pos.y obj->o-pos.x) 
                                    #\Space)))
                 (rogue-mvwaddch cw obj->o-pos.y obj->o-pos.x obj->o-type)
                 (draw cw)))
             (return-from do-motion))))))

(defun fall (obj pr)
  "Drop an item someplace around here."
  (let ((fpos (make-coord)))
    (when (fallpos (object-o-pos obj) fpos t)
      (rogue-mvaddch (coord-y fpos) (coord-x fpos) (object-o-type obj))
      (setf (object-o-pos obj) fpos)
      (when-let (rp (roomin hero))
        (when (not (logtest (moor-r-flags rp) ISDARK))
          (light hero)
          (rogue-mvwaddch cw hero.y hero.x PLAYER)))
      (attach lvl-obj obj)
      (return-from fall)))
  (when pr
    (msg "Your ~a vanishes as it hits the ground." (aref w-names (object-o-which obj)))))

(defun init-weapon (weap type)
  "Set up the initial goodies for a weapon."
  (let ((iwp (aref init-dam type)))
    (setf (object-o-damage weap)  (copy-seq (first iwp))
          (object-o-hurldmg weap) (copy-seq (second iwp))
          (object-o-launch weap)  (third iwp)
          (object-o-flags weap)   (fourth iwp))
    (if (logtest (object-o-flags weap) ISMANY)
        (setf (object-o-count weap) (+ (rnd 8) 8)
              (object-o-group weap) (newgrp))
        (setf (object-o-count weap) 1))))

(defun hit-monster (y x obj)
  "Does the missile hit the monster?"
  (fight (make-coord :y y :x x) (winat y x) obj t))

(defun num (n1 n2)
  "Figure out the plus number for armor/weapons."
  (cond
    ((and (zerop n1)
          (zerop n2))
     "+0")
    ((zerop n2)
     (format nil "~a~d" (if (minusp n1) "" "+") n1))
    (t
     (format nil 
             "~a~d,~a~d"
             (if (minusp n1) "" "+")
             n1
             (if (minusp n2) "" "+")
             n2))))

(defun wield ()
  "Pull out a certain weapon."
  (let ((oweapon cur-weapon))
    (unless (dropcheck cur-weapon)
      (setf cur-weapon oweapon)
      (return-from wield))

    (setf cur-weapon oweapon)
    (let ((obj (get-item "wield" WEAPON)))
      (unless obj
        (setf *after* nil)
        (return-from wield))

      (when (eql (object-o-type obj) ARMOR)
        (msg "You can't wield armor")
        (setf *after* nil)
        (return-from wield))

      (when (is-current obj)
        (setf *after* nil)
        (return-from wield))

      (if terse
          (addmsg "W")
          (addmsg "You are now w"))
      (msg "ielding ~a" (inv-name obj t))
      (setf cur-weapon obj))))

(defun fallpos (pos newpos passages)
  "Pick a random position around the given (y, x) coordinates."
  (let ((cnt 0))
    (for (y (1- (coord-y pos)) (1+ (coord-y pos)))
      (for (x (1- (coord-x pos)) (1+ (coord-x pos)))
        ;; Check to make certain the spot is empty, if it is, put the
        ;; object there, set it in the level list and re-draw the room
        ;; if he can see it.
        (unless (and (= y hero.y) (= x hero.x))
          (let ((ch (winat y x)))
            (when (or (eql ch FLOOR)
                      (and passages
                           (eql ch PASSAGE)))
              (incf cnt)
              (when (zerop (rnd cnt))
                (setf (coord-y newpos) y
                      (coord-x newpos) x)))))))
    (nonzerop cnt)))
