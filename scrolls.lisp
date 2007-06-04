;;;; Read a scroll and let it happen
;;;; @(#)scrolls.c	3.5 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defun read_scroll ()
  (when-let (obj (get_item "read" SCROLL))
    (let ((which (object-o_which obj)))
      (if (not (eql (object-o_type obj) SCROLL))
          (progn
            (msg (if terse
                     "Nothing to read"
                     "There is nothing on it to read")
                 (return-from read_scroll))))
      (msg "As you read the scroll, it vanishes.")
      ;; Calculate the effect it has on the poor guy.
      (when (eql obj cur_weapon)
        (setf cur_weapon nil))
      (case which
        (#.S_CONFUSE
         ;; Scroll of monster confusion.  Give him that power.
         (msg "Your hands begin to glow red")
         (logior! (thing-t_flags *player*) CANHUH))
        (#.S_LIGHT
         (setf (aref s_know S_LIGHT) t)
         (let ((rp (roomin hero)))
           (if (null rp)
               (msg "The corridor glows and then fades")
               (progn
                 (addmsg "The room is lit")
                 (verbose (addmsg " by a shimmering blue light."))
                 (endmsg)
                 (logclr! (moor-r_flags rp) ISDARK)
                 ;; Light the room and put the player back up
                 (light hero)
                 (rogue-mvwaddch cw hero.y hero.x PLAYER)))))
        (#.S_ARMOR
         (when cur_armor
           (msg "Your armor glows faintly for a moment")
           (decf (object-o_ac cur_armor))
           (logclr! (object-o_flags cur_armor) ISCURSED)))
        (#.S_HOLD
         ;; Hold monster scroll.  Stop all monsters within two spaces
         ;; from chasing after the hero.
         (for (x (- hero.x 2) (+ hero.x 2))
           (for (y (- hero.y 2) (+ hero.y 2))
             (when (and (plusp y)
                        (plusp x)
                        (upper-case-p (rogue-mvwinch mw y x)))
               (when-let (th (find_mons y x))
                 (logclr! (thing-t_flags th) ISRUN)
                 (logior! (thing-t_flags th) ISHELD))))))
        (#.S_SLEEP
         ;; Scroll which makes you fall asleep
         (setf (aref s_know S_SLEEP) t)
         (msg "You fall asleep.")
         (incf no_command (+ 4 (rnd SLEEPTIME))))
        (#.S_CREATE
         ;; Create a monster
         ;; First look in a circle around him, next try his room
         ;; otherwise give up
         ;; Search for an open place
         (let ((appear 0)
               (mp (make-coord)))
           (for (y hero.y (1+ hero.y))
             (for (x hero.x (1+ hero.x))
               ;; Don't put a monster in top of the player.
               (unless (and (= y hero.y)
                            (= x hero.x))
                 ;; Or anything else nasty
                 (when (step_ok (winat y x))
                   (incf appear)
                   (when (zerop (rnd appear))
                     (setf (coord-y mp) y)
                     (setf (coord-x mp) x))))))
           (if (nonzerop appear)
               (let ((titem (make-thing)))
                 (new_monster titem (randmonster nil) mp))
               (msg "You hear a faint cry of anguish in the distance."))))
        (#.S_IDENT
         ;; Identify, let the rogue figure something out
         (msg "This scroll is an identify scroll")
         (setf (aref s_know S_IDENT) t)
         (whatis))
        (#.S_MAP
         ;; Scroll of magic mapping.
         (setf (aref s_know S_MAP) t)
         (msg "Oh, now this scroll has a map on it.")
         (cl-ncurses:overwrite cl-ncurses:*stdscr* hw)
         ;; Take all the things we want to keep hidden out of the window
         (dotimes (i cl-ncurses:*lines*)
           (dotimes (j cl-ncurses:*cols*)
             (let* ((ch (rogue-mvwinch hw i j))
                    (nch ch))
               (case ch
                 (#.SECRETDOOR
                  (setf nch DOOR)
                  (rogue-mvaddch i j nch))
                 ((#\-
                   #\|
                   #.DOOR
                   #.PASSAGE
                   #\Space
                   #.STAIRS)
                  (unless (eql (rogue-mvwinch mw i j) #\Space)
                    (let ((it (find_mons i j)))
                      (when (eql (thing-t_oldch it) #\Space)
                        (setf (thing-t_oldch it) nch)))))
                 (otherwise
                  (setf nch #\Space)))
               (unless (eql nch ch)
                 (rogue-waddch hw nch)))))
         ;; Copy in what he has discovered
         (cl-ncurses:overlay cw hw)
         ;; And set up for display
         (cl-ncurses:overwrite hw cw))
        (#.S_GFIND
         ;; Potion of gold detection
         (let ((gtotal 0))
           (cl-ncurses:wclear hw)
           (dotimes (i MAXROOMS)
             (incf gtotal (moor-r_goldval (aref rooms i)))
             (when (and (not (zerop (moor-r_goldval (aref rooms i))))
                        (eql (rogue-mvwinch cl-ncurses:*stdscr*
                                           (coord-y (moor-r_gold (aref rooms i)))
                                           (coord-x (moor-r_gold (aref rooms i))))
                            GOLD))
               (rogue-mvwaddch hw
                               (coord-y (moor-r_gold (aref rooms i)))
                               (coord-x (moor-r_gold (aref rooms i)))
                               GOLD)))
           (if (plusp gtotal)
               (progn
                 (setf (aref s_know S_GFIND) t)
                 (show_win hw
                           "You begin to feel greedy and you sense gold.--More--"))
               (msg "You begin to feel a pull downward"))))
        (#.S_TELEP
         ;; Scroll of teleportation:
         ;; Make him dissapear and reappear
         (let ((cur_room (roomin hero))
               (rm (teleport)))
           (when (not (eql cur_room (aref rooms rm)))
             (setf (aref s_know S_TELEP) t))))
        (#.S_ENCH
         (if (null cur_weapon)
             (msg "You feel a strange sense of loss.")
             (progn
               (logclr! (object-o_flags cur_weapon) ISCURSED)
               (if (> (rnd 100) 50)
                   (incf (object-o_hplus cur_weapon))
                   (incf (object-o_dplus cur_weapon)))
               (msg "Your ~a glows blue for a moment." 
                    (aref w_names (object-o_which cur_weapon))))))
        (#.S_SCARE
         ;; A monster will refuse to step on a scare monster scroll if
         ;; it is dropped.  Thus reading it is a mistake and produces
         ;; laughter at the poor rogue's boo boo.
         (msg "You hear maniacal laughter in the distance."))
        (#.S_REMOVE
         (dolist (obj (list cur_armor
                            cur_weapon
                            (aref cur_ring RIGHT)
                            (aref cur_ring LEFT)))
           (when obj 
             (logclr! (object-o_flags obj) ISCURSED)))
         (msg "You feel as if somebody is watching over you."))
        (#.S_AGGR
         ;; This scroll aggravates all the monsters on the current level
         ;; and sets them running towards the hero
         (aggravate)
         (msg "You hear a high pitched humming noise."))
        (#.S_NOP
         (msg "This scroll seems to be blank."))
        (#.S_GENOCIDE
         (msg "You have been granted the boon of genocide")
         (genocide)
         (setf (aref s_know S_GENOCIDE) t))
        (otherwise
         (msg"What a puzzling scroll!")
         (return-from read_scroll)))
      (look t)           ; put the result of the scroll on the screen 
      (status)
      (cond
        ((and (aref s_know which)
              (aref s_guess which))
         (setf (aref s_guess which) nil))
        ((and (not (aref s_know which))
              askme
              (null (aref s_guess which)))
         (msg (if terse "Call it: " "What do you want to call it? "))
         (multiple-value-bind (buf status)
             (get_str "" cw)
           (when (eql status NORM)
             (setf (aref s_guess which) (copy-seq buf))))))
      ;; Get rid of the thing
      (decf inpack)
      (if (>1 (object-o_count obj))
          (decf (object-o_count obj))
          (detach pack obj)))))
