;;;; All sorts of miscellaneous routines
;;;;
;;;; @(#)misc.c	3.13 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defun tr_name (ch)
  "Print the name of a trap."
  (ecase ch
    (#.TRAPDOOR (if terse "A trapdoor." "You found a trapdoor."))
    (#.BEARTRAP (if terse "A beartrap." "You found a beartrap."))
    (#.SLEEPTRAP (if terse "A sleeping gas trap." "You found a sleeping gas trap."))
    (#.ARROWTRAP (if terse "An arrow trap." "You found an arrow trap."))
    (#.TELTRAP (if terse "A teleport trap." "You found a teleport trap."))
    (#.DARTTRAP (if terse "A dart trap." "You found a poison dart trap."))))

(defun look (wakeup)
  "A quick glance all around the player."
  (let (ch
        oldx oldy
        inpass
        (passcount 0)
        rp
        ey ex)
    (cl-ncurses:getyx cw oldy oldx)
    (when (and oldrp 
               (logtest (moor-r_flags oldrp) ISDARK) 
               (off *player* ISBLIND))
      (for (x (1- (coord-x oldpos)) (1+ (coord-x oldpos)))
        (for (y (1- (coord-y oldpos)) (1+ (coord-y oldpos)))
          (when (and (or (not (= y hero.y))
                         (not (= x hero.x)))
                     (eql (show y x) FLOOR))
            (rogue-mvwaddch cw y x #\Space)))))
    (setf rp (roomin hero)
          inpass (null rp)
          ey (1+ hero.y)
          ex (1+ hero.x))
    (for (x (1- hero.x) ex)
      (when (and (>= x 0) 
                 (< x cl-ncurses:*cols*))
        (for (y (1- hero.y) ey)
          (block continue
            (when (and (> y 0) 
                       (< y (1- cl-ncurses:*lines*)))
              (when (upper-case-p (rogue-mvwinch mw y x))
                (let ((tp (if wakeup
                              (wake_monster y x)
                              (find_mons y x))))
                  (when (eql (setf (thing-t_oldch tp) (rogue-mvinch y x))
                            TRAP)
                    (setf (thing-t_oldch tp)
                          (if (logtest (rogue-trap-tr_flags (trap_at y x)) ISFOUND) 
                              TRAP
                              FLOOR)))
                  (when (and (eql (thing-t_oldch tp) FLOOR)
                             (logtest (moor-r_flags rp) ISDARK)
                             (off *player* ISBLIND))
                    (setf (thing-t_oldch tp) #\Space))))
              ;;
              ;; Secret doors show as walls
              ;;
              (when (eql (setf ch (show y x)) SECRETDOOR)
                (setf ch (secretdoor y x)))
              ;;
              ;; Don't show room walls if he is in a passage
              ;;
              (if (off *player* ISBLIND)
                  (if (or (and (= y hero.y) 
                               (= x hero.x))
                          (and inpass 
                               (or (eql ch #\-) 
                                   (eql ch #\|))))
                      (return-from continue))
                  (when (or (not (= y hero.y)) 
                            (not (= x hero.x)))
                    (return-from continue)))
              (cl-ncurses:wmove cw y x)
              (rogue-waddch cw ch)
              (when (and door_stop 
                         (not firstmove) 
                         running)
                (case runch
                  (#\h (when (= x ex)
                         (return-from continue)))
                  (#\j (when (= y (1- hero.y))
                         (return-from continue)))
                  (#\k (when (= y ey)
                         (return-from continue)))
                  (#\l (when (= x (1- hero.x))
                         (return-from continue)))
                  (#\y (when (plusp (- (+ x y) (+ hero.x hero.y)))
                         (return-from continue)))
                  (#\u (when (plusp (- (- y x) (- hero.y hero.x)))
                         (return-from continue)))
                  (#\n (when (minusp (- (+ x y) (+ hero.x hero.y)))
                         (return-from continue)))
                  (#\b (when (minusp (- (- y x) (- hero.y hero.x)))
                         (return-from continue))))
                (case ch
                  (#.DOOR
                   (when (or (= x hero.x)
                             (= y hero.y))
                     (setf running nil)))
                  (#.PASSAGE
                   (when (or (= x hero.x) 
                             (= y hero.y))
                     (incf passcount)))
                  ((#.FLOOR #\| #\- #\Space)
                   nil)
                  (otherwise (setf running nil)))))))))
    (when (and door_stop (not firstmove) (> passcount 1))
      (set running nil))
    (rogue-mvwaddch cw hero.y hero.x PLAYER)
    (cl-ncurses:wmove cw oldy oldx)
    (setf oldpos hero
          oldrp rp)))

(defun secretdoor (y x)
  "Figure out what a secret door looks like."
  (let ((cp (make-coord :x x :y y)))
    (map nil
         #'(lambda (rp)
             (when (inroom rp cp)
               (if (or (= y (coord-y (moor-r_pos rp)))
                       (= y (+ (coord-y (moor-r_pos rp))
                               (1- (coord-y (moor-r_max rp))))))
                   (return-from secretdoor #\-)
                   (return-from secretdoor #\|))))
         rooms)
    #\p))

(defun find_obj (y x)
  "Find the unclaimed object at y, x."
  (map nil
       #'(lambda (obj)
           (when (and (= y (coord-y (object-o_pos obj)))
                      (= x (coord-x (object-o_pos obj))))
             (return-from find_obj obj)))
          lvl_obj)
  (rogue-debug "Non-object ~d,~d" y x)
  nil)

(defun eat ()
  "She wants to eat something, so let her try."
  (when-let (obj (get_item "eat" FOOD))
    (when (not (eql (object-o_type obj) FOOD))
      (if terse
          (msg "That's Inedible!")
          (msg "Ugh, you would get ill if you ate that."))
      (return-from eat))
    (decf inpack)
    (if (onep (object-o_which obj))
        (msg "My, that was a yummy ~a" fruit)
        (if (> (rnd 100) 70)
            (progn 
              (msg "Yuk, this food tastes awful")
              (incf (stats-s_exp pstats))
              (check_level))
            (msg "Yum, that tasted good")))
    (when (> (+ (incf food_left HUNGERTIME) (rnd 400) (- 200)) STOMACHSIZE)
      (setf food_left STOMACHSIZE))
    (setf hungry_state 0)
    (when (eql obj cur_weapon)
      (setf cur_weapon nil))
    (when (< (decf (object-o_count obj)) 1)
      (detach pack obj))))

(defun chg_str (amt)
  "Used to modify the player's strength.  It keeps track of the
highest it has been, just in case."
  (unless (zerop amt)
    (let* ((stat (stats-s_str pstats))
           (str (str_t-st_str stat))
           (add (str_t-st_add stat)))
      (if (plusp amt)
          (progn
            (dotimes (i amt)
              (cond
                ((< str 18)
                 (incf (str_t-st_str stat)))
                ((zerop add)
                 (setf (str_t-st_add stat) (1+ (rnd 50))))
                ((<= add 50)
                 (setf (str_t-st_add stat) (+ 51 (rnd 24))))
                ((<= add 75)
                 (setf (str_t-st_add stat) (+ 76 (rnd 14))))
                ((<= add 90)
                 (setf (str_t-st_add stat) 91))
                ((< add 100)
                 (incf (str_t-st_add stat)))))
            (when (or (> (str_t-st_str (stats-s_str pstats)) 
                         (str_t-st_str (stats-s_str max_stats)))
                      (and 
                       (= (str_t-st_str (stats-s_str pstats)) 18)
                       (> (str_t-st_add (stats-s_str pstats)) 
                          (str_t-st_add (stats-s_str max_stats)))))
              (setf (stats-s_str max_stats) 
                    (copy-structure (stats-s_str pstats)))))
          (progn 
            (dotimes (i (abs amt))
              (cond
                ((or (< str 18) (zerop add))
                 (decf (str_t-st_str stat)))
                ((< add 51)
                 (setf (str_t-st_add stat) 0))
                ((< add 76)
                 (setf (str_t-st_add stat) (1+ (rnd 50))))
                ((< add 91)
                 (setf (str_t-st_add stat) (+ 51 (rnd 25))))
                ((< add 100)
                 (setf (str_t-st_add stat) (+ 76 (rnd 14))))
                (t
                 (setf (str_t-st_add stat) (+ 91 (rnd 8))))))
            (when (< (str_t-st_str stat) 3)
              (setf (str_t-st_str stat) 3)))))))

(defun add_haste (from-potion)
  "Add a haste to the player."
  (if (on *player* ISHASTE)
      (progn
        (msg "You faint from exhaustion.")
        (incf no_command (rnd 8))
        (extinguish #'nohaste))
      (progn
        (setf (thing-t_flags *player*) (logior (thing-t_flags *player*) ISHASTE))
        (when from-potion
          (fuse 'nohaste 0 (+ (rnd 4) 4) AFTER)))))

(defun aggravate ()
  "Aggravate all the monsters on this level."
  (dolist (m mlist)
    (runto (thing-t_pos m) hero)))

(defun vowelstr (str)
  "For formats: if string starts with a vowel, return 'n' for an
'an'."
  (case (aref str 0)
    ((#\a #\e #\i #\o #\u) "n")
    (t "")))

(defun is_current (obj)
  "See if the object is one of the currently used items."
  (when obj
    (when (or (eql obj cur_armor)
              (eql obj cur_weapon)
              (eql obj (aref cur_ring LEFT))
              (eql obj (aref cur_ring RIGHT)))
      (msg (if terse "In use." "That's already in use."))
      t)))

(defun get_dir ()
  "Set up the direction coordinate for use in various 'prefix'
commands."
  (let ((prompt (if terse "Direction: " "Which direction? "))
        gotit)
    (verbose (msg prompt))

    (while (not gotit)
      (setf gotit t)
      (case (readchar)
        ((#\h #\H) (setf delta.y 0) (setf delta.x -1))
        ((#\j #\J) (setf delta.y 1) (setf delta.x 0))
        ((#\k #\K) (setf delta.y -1) (setf delta.x 0))
        ((#\l #\L) (setf delta.y 0) (setf delta.x 1))
        ((#\y #\Y) (setf delta.y -1) (setf delta.x -1))
        ((#\u #\U) (setf delta.y -1) (setf delta.x 1))
        ((#\b #\B) (setf delta.y 1) (setf delta.x -1))
        ((#\n #\N) (setf delta.y 1) (setf delta.x 1))
        (#\Escape (return-from get_dir nil))
        (otherwise
         (zero! mpos)
         (msg prompt)
         (setf gotit nil))))
    (when (and (on *player* ISHUH) 
               (> (rnd 100) 80))
      (while (and (zerop delta.y)
                  (zerop delta.x))
        (setf delta.y (1- (rnd 3))
              delta.x (1- (rnd 3)))))
    (zero! mpos)
    t))
