;;;; new_level:
;;;; 	Dig and draw a new level
;;;; 
;;;; @(#)new_level.c	3.7 (Berkeley) 6/2/81

(in-package :cl-rogue)

(defun new_level ()
  (setf max_level (max level max_level))
  (cl-ncurses:wclear cw)
  (cl-ncurses:wclear mw)
  (cl-ncurses:clear)
  (status)

  ;; Free up the monsters on the last level
  (setq mlist '())
  (do_rooms)                            ; Draw rooms 
  (do_passages)                         ; Draw passages 
  (incf no_food)
  (put_things)                          ; Place objects (if any) 

  ;; Place the staircase down.
  (let ((stair (make-coord)))
    (loop
       (rnd_pos (aref rooms (rnd_room)) stair)
       (when (eq (winat (coord-y stair) (coord-x stair)) FLOOR)
         (return)))
    (rogue-addch STAIRS)

  ;; Place the traps
  (when (< (rnd 10) level)
    (dotimes (i (min (rnd (1+ (truncate (/ level
                                           4))))
                     MAXTRAPS))
      (loop
         (rnd_pos (aref rooms (rnd_room)) STAIR)
         (when (eq (winat (coord-y stair) (coord-x stair)) FLOOR)
           (return)))
      (let ((ch (aref 
                 (vector TRAPDOOR BEARTRAP SLEEPTRAP ARROWTRAP TELTRAP DARTTRAP)
                 (rnd 6))))
        (rogue-addch TRAP)
        (setf (rogue-trap-tr_type (aref traps i)) ch)
        (setf (rogue-trap-tr_flags (aref traps i)) 0)
        (setf (rogue-trap-tr_pos (aref traps i)) (copy-structure stair)))))

    (loop
       (rnd_pos (aref rooms (rnd_room)) hero)
       (when (eq (winat hero.y hero.x) FLOOR)
         (return)))
    (light hero)
    (cl-ncurses:wmove cw hero.y hero.x)
    (rogue-waddch cw PLAYER)))

(defun rnd_room ()
  "Pick a room that is really there."
  (loop
     (let ((rm (rnd MAXROOMS)))
       (unless (logtest (moor-r_flags (aref rooms rm)) ISGONE)
         (return rm)))))

(defun put_things ()
  "Put potions and scrolls on this level."
  ;; Throw away stuff left on the previous level (if anything)
  (setf lvl_obj '())

  ;; Once you have found the amulet, the only way to get new stuff is
  ;; go down into the dungeon.
  (when (and *amulet* 
             (< level max_level))
    (return-from put_things))

  ;; Do MAXOBJ attempts to put things on a level
  (dotimes (i MAXOBJ)
    (when (< (rnd 100) 35)
      ;; Pick a new object and link it in the list
      (let ((cur (new_thing)))
        (attach lvl_obj cur)
        ;; Put it somewhere
        (let ((rm (rnd_room))
              (tp (make-coord)))
          (loop
             (rnd_pos (aref rooms rm) tp)
             (when (eq (winat (coord-y tp) (coord-x tp))
                       FLOOR)
               (return)))
          (rogue-mvaddch (coord-y tp) (coord-x tp) (object-o_type cur))
          (setf (object-o_pos cur) tp)))))

  ;; If he is really deep in the dungeon and he hasn't found the
  ;; amulet yet, put it somewhere on the ground
  (when (and (> level 25) 
             (not *amulet*))
    (let ((cur (make-object)))
      (attach lvl_obj cur)
      (zero! (object-o_hplus cur)
             (object-o_dplus cur))
      (setf (object-o_damage cur) (copy-seq "0d0")
            (object-o_hurldmg cur) (copy-seq "0d0")
            (object-o_ac cur) 11
            (object-o_type cur) AMULET)
      ;; Put it somewhere
      (let ((rm (rnd_room))
            (tp (make-coord)))
        (loop
           (rnd_pos (aref rooms rm) tp)
           (when (eq (winat (coord-y tp) (coord-x tp)) FLOOR)
             (return)))
        (rogue-mvaddch (coord-y tp) (coord-x tp) (object-o_type cur))
        (setf (object-o_pos cur) tp)))))
