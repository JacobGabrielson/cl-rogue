;;;; All sorts of miscellaneous routines
;;;;
;;;; @(#)misc.c	3.13 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defun tr-name (ch)
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
               (logtest (moor-r-flags oldrp) ISDARK) 
               (off *player* ISBLIND))
      (for (x (1- (coord-x oldpos)) (1+ (coord-x oldpos)))
        (for (y (1- (coord-y oldpos)) (1+ (coord-y oldpos)))
          (when (and (or (not (= y hero.y))
                         (not (= x hero.x)))
                     (eql (show y x) THE-FLOOR))
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
                              (wake-monster y x)
                              (find-mons y x))))
                  (when (eql (setf (thing-t-oldch tp) (rogue-mvinch y x))
                            TRAP)
                    (setf (thing-t-oldch tp)
                          (if (logtest (rogue-trap-tr-flags (trap-at y x)) ISFOUND) 
                              TRAP
                              THE-FLOOR)))
                  (when (and (eql (thing-t-oldch tp) THE-FLOOR)
                             (logtest (moor-r-flags rp) ISDARK)
                             (off *player* ISBLIND))
                    (setf (thing-t-oldch tp) #\Space))))
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
              (when (and door-stop 
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
                  ((#.THE-FLOOR #\| #\- #\Space)
                   nil)
                  (otherwise (setf running nil)))))))))
    (when (and door-stop (not firstmove) (> passcount 1))
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
               (if (or (= y (coord-y (moor-r-pos rp)))
                       (= y (+ (coord-y (moor-r-pos rp))
                               (1- (coord-y (moor-r-max rp))))))
                   (return-from secretdoor #\-)
                   (return-from secretdoor #\|))))
         rooms)
    #\p))

(defun find-obj (y x)
  "Find the unclaimed object at y, x."
  (map nil
       #'(lambda (obj)
           (when (and (= y (coord-y (object-o-pos obj)))
                      (= x (coord-x (object-o-pos obj))))
             (return-from find-obj obj)))
          lvl-obj)
  (rogue-debug "Non-object ~d,~d" y x)
  nil)

(defun eat ()
  "She wants to eat something, so let her try."
  (when-let (obj (get-item "eat" FOOD))
    (when (not (eql (object-o-type obj) FOOD))
      (if terse
          (msg "That's Inedible!")
          (msg "Ugh, you would get ill if you ate that."))
      (return-from eat))
    (decf inpack)
    (if (onep (object-o-which obj))
        (msg "My, that was a yummy ~a" fruit)
        (if (> (rnd 100) 70)
            (progn 
              (msg "Yuk, this food tastes awful")
              (incf (stats-s-exp pstats))
              (check-level))
            (msg "Yum, that tasted good")))
    (when (> (+ (incf food-left HUNGERTIME) (rnd 400) (- 200)) STOMACHSIZE)
      (setf food-left STOMACHSIZE))
    (setf hungry-state 0)
    (when (eql obj cur-weapon)
      (setf cur-weapon nil))
    (when (< (decf (object-o-count obj)) 1)
      (detach pack obj))))

(defun chg-str (amt)
  "Used to modify the player's strength.  It keeps track of the
highest it has been, just in case."
  (unless (zerop amt)
    (let* ((stat (stats-s-str pstats))
           (str (str-t-st-str stat))
           (add (str-t-st-add stat)))
      (if (plusp amt)
          (progn
            (dotimes (i amt)
              (cond
                ((< str 18)
                 (incf (str-t-st-str stat)))
                ((zerop add)
                 (setf (str-t-st-add stat) (1+ (rnd 50))))
                ((<= add 50)
                 (setf (str-t-st-add stat) (+ 51 (rnd 24))))
                ((<= add 75)
                 (setf (str-t-st-add stat) (+ 76 (rnd 14))))
                ((<= add 90)
                 (setf (str-t-st-add stat) 91))
                ((< add 100)
                 (incf (str-t-st-add stat)))))
            (when (or (> (str-t-st-str (stats-s-str pstats)) 
                         (str-t-st-str (stats-s-str max-stats)))
                      (and 
                       (= (str-t-st-str (stats-s-str pstats)) 18)
                       (> (str-t-st-add (stats-s-str pstats)) 
                          (str-t-st-add (stats-s-str max-stats)))))
              (setf (stats-s-str max-stats) 
                    (copy-structure (stats-s-str pstats)))))
          (progn 
            (dotimes (i (abs amt))
              (cond
                ((or (< str 18) (zerop add))
                 (decf (str-t-st-str stat)))
                ((< add 51)
                 (setf (str-t-st-add stat) 0))
                ((< add 76)
                 (setf (str-t-st-add stat) (1+ (rnd 50))))
                ((< add 91)
                 (setf (str-t-st-add stat) (+ 51 (rnd 25))))
                ((< add 100)
                 (setf (str-t-st-add stat) (+ 76 (rnd 14))))
                (t
                 (setf (str-t-st-add stat) (+ 91 (rnd 8))))))
            (when (< (str-t-st-str stat) 3)
              (setf (str-t-st-str stat) 3)))))))

(defun add-haste (from-potion)
  "Add a haste to the player."
  (if (on *player* ISHASTE)
      (progn
        (msg "You faint from exhaustion.")
        (incf no-command (rnd 8))
        (extinguish #'nohaste))
      (progn
        (setf (thing-t-flags *player*) (logior (thing-t-flags *player*) ISHASTE))
        (when from-potion
          (fuse 'nohaste 0 (+ (rnd 4) 4) AFTER)))))

(defun aggravate ()
  "Aggravate all the monsters on this level."
  (dolist (m mlist)
    (runto (thing-t-pos m) hero)))

(defun vowelstr (str)
  "For formats: if string starts with a vowel, return 'n' for an
'an'."
  (case (aref str 0)
    ((#\a #\e #\i #\o #\u) "n")
    (t "")))

(defun is-current (obj)
  "See if the object is one of the currently used items."
  (when obj
    (when (or (eql obj cur-armor)
              (eql obj cur-weapon)
              (eql obj (aref cur-ring LEFT))
              (eql obj (aref cur-ring RIGHT)))
      (msg (if terse "In use." "That's already in use."))
      t)))

(defun get-dir ()
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
        (#\Escape (return-from get-dir nil))
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
