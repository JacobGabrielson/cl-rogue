;;;; new-level:
;;;; 	Dig and draw a new level
;;;; 
;;;; @(#)new-level.c	3.7 (Berkeley) 6/2/81

(in-package :cl-rogue)

(defun new-level ()
  (setf max-level (max level max-level))
  (cl-ncurses:wclear cw)
  (cl-ncurses:wclear mw)
  (cl-ncurses:clear)
  (status)

  ;; Free up the monsters on the last level
  (setq mlist '())
  (do-rooms)                            ; Draw rooms 
  (do-passages)                         ; Draw passages 
  (incf no-food)
  (put-things)                          ; Place objects (if any) 

  ;; Place the staircase down.
  (let ((stair (make-coord)))
    (loop
       (rnd-pos (aref rooms (rnd-room)) stair)
       (when (eql (winat (coord-y stair) (coord-x stair)) THE-FLOOR)
         (return)))
    (rogue-addch STAIRS)

  ;; Place the traps
  (when (< (rnd 10) level)
    (dotimes (i (min (rnd (1+ (truncate (/ level
                                           4))))
                     MAXTRAPS))
      (loop
         (rnd-pos (aref rooms (rnd-room)) STAIR)
         (when (eql (winat (coord-y stair) (coord-x stair)) THE-FLOOR)
           (return)))
      (let ((ch (aref 
                 (vector TRAPDOOR BEARTRAP SLEEPTRAP ARROWTRAP TELTRAP DARTTRAP)
                 (rnd 6))))
        (rogue-addch TRAP)
        (setf (rogue-trap-tr-type (aref traps i)) ch)
        (setf (rogue-trap-tr-flags (aref traps i)) 0)
        (setf (rogue-trap-tr-pos (aref traps i)) (copy-structure stair)))))

    (loop
       (rnd-pos (aref rooms (rnd-room)) hero)
       (when (eql (winat hero.y hero.x) THE-FLOOR)
         (return)))
    (light hero)
    (cl-ncurses:wmove cw hero.y hero.x)
    (rogue-waddch cw PLAYER)))

(defun rnd-room ()
  "Pick a room that is really there."
  (loop
     (let ((rm (rnd MAXROOMS)))
       (unless (logtest (moor-r-flags (aref rooms rm)) ISGONE)
         (return rm)))))

(defun put-things ()
  "Put potions and scrolls on this level."
  ;; Throw away stuff left on the previous level (if anything)
  (setf lvl-obj '())

  ;; Once you have found the amulet, the only way to get new stuff is
  ;; go down into the dungeon.
  (when (and *amulet* 
             (< level max-level))
    (return-from put-things))

  ;; Do MAXOBJ attempts to put things on a level
  (dotimes (i MAXOBJ)
    (when (< (rnd 100) 35)
      ;; Pick a new object and link it in the list
      (let ((cur (new-thing)))
        (attach lvl-obj cur)
        ;; Put it somewhere
        (let ((rm (rnd-room))
              (tp (make-coord)))
          (loop
             (rnd-pos (aref rooms rm) tp)
             (when (eql (winat (coord-y tp) (coord-x tp))
                       THE-FLOOR)
               (return)))
          (rogue-mvaddch (coord-y tp) (coord-x tp) (object-o-type cur))
          (setf (object-o-pos cur) tp)))))

  ;; If he is really deep in the dungeon and he hasn't found the
  ;; amulet yet, put it somewhere on the ground
  (when (and (> level 25) 
             (not *amulet*))
    (let ((cur (make-object)))
      (attach lvl-obj cur)
      (zero! (object-o-hplus cur)
             (object-o-dplus cur))
      (setf (object-o-damage cur) (copy-seq "0d0")
            (object-o-hurldmg cur) (copy-seq "0d0")
            (object-o-ac cur) 11
            (object-o-type cur) AMULET)
      ;; Put it somewhere
      (let ((rm (rnd-room))
            (tp (make-coord)))
        (loop
           (rnd-pos (aref rooms rm) tp)
           (when (eql (winat (coord-y tp) (coord-x tp)) THE-FLOOR)
             (return)))
        (rogue-mvaddch (coord-y tp) (coord-x tp) (object-o-type cur))
        (setf (object-o-pos cur) tp)))))
