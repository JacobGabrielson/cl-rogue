;;;; Functions for dealing with problems brought about by weapons
;;;;
;;;; @(#)weapons.c	3.17 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defparameter w_names
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

(defparameter init_dam
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
  (when-let (obj (get_item "throw" WEAPON))
    (symbol-macrolet ((obj->o_count (object-o_count obj)))
      (when (or (not (dropcheck obj)) 
                (is_current obj))
        (return-from missile)
        ;; Get rid of the thing.  If it is a non-multiple item object, or
        ;; if it is the last thing, just drop it.  Otherwise, create a new
        ;; item with a count of one.
        (if (< obj->o_count 2)
            (progn
              (detach pack obj)
              (decf inpack))
            (progn
              (decf obj->o_count)
              (when (zerop (object-o_group obj))
                (decf inpack))
              (setf obj (copy-structure obj))
              (setf obj->o_count 1)))
        (do_motion obj ydelta xdelta)
        ;; AHA! Here it has hit something.  If it is a wall or a door,
        ;; or if it misses (combat) the mosnter, put it on the floor
        (when (or (upper-case-p (rogue-mvwinch mw
                                                     (coord-y (object-o_pos obj))
                                                     (coord-x (object-o_pos obj))))
                  (not (hit_monster (coord-y (object-o_pos obj)) 
                                    (coord-x (object-o_pos obj)) obj)))
          (fall obj t))
        (rogue-mvwaddch cw hero.y hero.x PLAYER)))))

(defun do_motion (obj ydelta xdelta)
  "Do the actual motion on the screen done by an object traveling
across the room."
  ;; Come fly with us ...
  (symbol-macrolet ((obj->o_pos (object-o_pos obj))
                    (obj->o_type (object-o_type obj))
                    (obj->o_pos.x (coord-x (object-o_pos obj)))
                    (obj->o_pos.y (coord-y (object-o_pos obj))))
    (setf obj->o_pos hero)
    (loop
       ;; Erase the old one
       (when (and (not (ce obj->o_pos hero)) 
                  (cansee obj->o_pos.y obj->o_pos.x)
                  (not (eq (rogue-mvwinch cw obj->o_pos.y obj->o_pos.x) #\Space)))
         (rogue-mvwaddch cw
                               obj->o_pos.y
                               obj->o_pos.x 
                               (show obj->o_pos.y obj->o_pos.x)))
       ;; Get the new position
       (incf obj->o_pos.y ydelta)
       (incf obj->o_pos.x xdelta)
       (let ((ch (winat obj->o_pos.y obj->o_pos.x)))
         (if (and (step_ok ch) 
                  (not (eq ch DOOR)))
             (progn
               ;; It hasn't hit anything yet, so display it
               ;; If it alright.
               (when (and (cansee obj->o_pos.y obj->o_pos.x)
                          (not (eq (rogue-mvwinch cw obj->o_pos.y obj->o_pos.x) #\Space)))
                 (rogue-mvwaddch cw obj->o_pos.y obj->o_pos.x obj->o_type)
                 (draw cw)))
             (return-from do_motion))))))

(defun fall (obj pr)
  "Drop an item someplace around here."
  (let ((fpos (make-coord)))
    (when (fallpos (object-o_pos obj) fpos t)
      (rogue-mvaddch (coord-y fpos) (coord-x fpos) (object-o_type obj))
      (setf (object-o_pos obj) fpos)
      (when-let (rp (roomin hero))
        (when (not (logtest (moor-r_flags rp) ISDARK))
          (light hero)
          (rogue-mvwaddch cw hero.y hero.x PLAYER)))
      (attach lvl_obj obj)
      (return-from fall)))
  (when pr
    (msg "Your ~a vanishes as it hits the ground." (aref w_names (object-o_which obj)))))

(defun init_weapon (weap type)
  "Set up the initial goodies for a weapon."
  (let ((iwp (aref init_dam type)))
    (setf (object-o_damage weap)  (copy-seq (first iwp))
          (object-o_hurldmg weap) (copy-seq (second iwp))
          (object-o_launch weap)  (third iwp)
          (object-o_flags weap)   (fourth iwp))
    (if (logtest (object-o_flags weap) ISMANY)
        (setf (object-o_count weap) (+ (rnd 8) 8)
              (object-o_group weap) (newgrp))
        (setf (object-o_count weap) 1))))

(defun hit_monster (y x obj)
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
  (let ((oweapon cur_weapon))
    (unless (dropcheck cur_weapon)
      (setf cur_weapon oweapon)
      (return-from wield))

    (setf cur_weapon oweapon)
    (let ((obj (get_item "wield" WEAPON)))
      (unless obj
        (setf *after* nil)
        (return-from wield))

      (when (eq (object-o_type obj) ARMOR)
        (msg "You can't wield armor")
        (setf *after* nil)
        (return-from wield))

      (when (is_current obj)
        (setf *after* nil)
        (return-from wield))

      (if terse
          (addmsg "W")
          (addmsg "You are now w"))
      (msg "ielding ~a" (inv_name obj t))
      (setf cur_weapon obj))))

(defun fallpos (pos newpos passages)
  "Pick a random position around the give (y, x) coordinates."
  (let ((cnt 0))
    (for (y (1- (coord-y pos)) (1+ (coord-y pos)))
      (for (x (1- (coord-x pos)) (1+ (coord-x pos)))
        ;; check to make certain the spot is empty, if it is,
        ;; put the object there, set it in the level list
        ;; and re-draw the room if he can see it
        (unless (or (not (= y hero.y))
                    (not (= x hero.x)))
          (let ((ch (winat y x)))
            (when (or (eq ch FLOOR) 
                      (and passages 
                           (eq ch PASSAGE)))
              (incf cnt) 
              (when (zerop (rnd cnt))
                (setf (coord-y newpos) y)
                (setf (coord-x newpos) x)))))))
    (nonzerop cnt)))
