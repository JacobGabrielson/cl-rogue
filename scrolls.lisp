;;;; Read a scroll and let it happen
;;;; @(#)scrolls.c	3.5 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defun read-scroll ()
  (when-let (obj (get-item "read" SCROLL))
    (let ((which (object-o-which obj)))
      (if (not (eql (object-o-type obj) SCROLL))
          (progn
            (msg (if terse
                     "Nothing to read"
                     "There is nothing on it to read"))
	    (return-from read-scroll)))
      (msg "As you read the scroll, it vanishes.")
      ;; Calculate the effect it has on the poor guy.
      (when (eql obj cur-weapon)
        (setf cur-weapon nil))
      (case which
        (#.S-CONFUSE
         ;; Scroll of monster confusion.  Give him that power.
         (msg "Your hands begin to glow red")
         (logior! (thing-t-flags *player*) CANHUH))
        (#.S-LIGHT
         (setf (aref s-know S-LIGHT) t)
         (let ((rp (roomin hero)))
           (if (null rp)
               (msg "The corridor glows and then fades")
               (progn
                 (addmsg "The room is lit")
                 (verbose (addmsg " by a shimmering blue light."))
                 (endmsg)
                 (logclr! (moor-r-flags rp) ISDARK)
                 ;; Light the room and put the player back up
                 (light hero)
                 (rogue-mvwaddch cw hero.y hero.x PLAYER)))))
        (#.S-ARMOR
         (when cur-armor
           (msg "Your armor glows faintly for a moment")
           (decf (object-o-ac cur-armor))
           (logclr! (object-o-flags cur-armor) ISCURSED)))
        (#.S-HOLD
         ;; Hold monster scroll.  Stop all monsters within two spaces
         ;; from chasing after the hero.
         (for (x (- hero.x 2) (+ hero.x 2))
           (for (y (- hero.y 2) (+ hero.y 2))
             (when (and (plusp y)
                        (plusp x)
                        (upper-case-p (rogue-mvwinch mw y x)))
               (when-let (th (find-mons y x))
                 (logclr! (thing-t-flags th) ISRUN)
                 (logior! (thing-t-flags th) ISHELD))))))
        (#.S-SLEEP
         ;; Scroll which makes you fall asleep
         (setf (aref s-know S-SLEEP) t)
         (msg "You fall asleep.")
         (incf no-command (+ 4 (rnd SLEEPTIME))))
        (#.S-CREATE
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
                 (when (step-ok (winat y x))
                   (incf appear)
                   (when (zerop (rnd appear))
                     (setf (coord-y mp) y)
                     (setf (coord-x mp) x))))))
           (if (nonzerop appear)
               (let ((titem (make-thing)))
                 (new-monster titem (randmonster nil) mp))
               (msg "You hear a faint cry of anguish in the distance."))))
        (#.S-IDENT
         ;; Identify, let the rogue figure something out
         (msg "This scroll is an identify scroll")
         (setf (aref s-know S-IDENT) t)
         (whatis))
        (#.S-MAP
         ;; Scroll of magic mapping.
         (setf (aref s-know S-MAP) t)
         (msg "Oh, now this scroll has a map on it.")
         (cl-charms/low-level:overwrite cl-charms/low-level:*stdscr* hw)
         ;; Take all the things we want to keep hidden out of the window
         (dotimes (i cl-charms/low-level:*lines*)
           (dotimes (j cl-charms/low-level:*cols*)
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
                    (let ((it (find-mons i j)))
                      (when (eql (thing-t-oldch it) #\Space)
                        (setf (thing-t-oldch it) nch)))))
                 (otherwise
                  (setf nch #\Space)))
               (unless (eql nch ch)
                 (rogue-waddch hw nch)))))
         ;; Copy in what he has discovered
         (cl-charms/low-level:overlay cw hw)
         ;; And set up for display
         (cl-charms/low-level:overwrite hw cw))
        (#.S-GFIND
         ;; Potion of gold detection
         (let ((gtotal 0))
           (cl-charms/low-level:wclear hw)
           (dotimes (i MAXROOMS)
             (incf gtotal (moor-r-goldval (aref rooms i)))
             (when (and (not (zerop (moor-r-goldval (aref rooms i))))
                        (eql (rogue-mvwinch cl-charms/low-level:*stdscr*
					    (coord-y (moor-r-gold (aref rooms i)))
					    (coord-x (moor-r-gold (aref rooms i))))
			     GOLD))
               (rogue-mvwaddch hw
                               (coord-y (moor-r-gold (aref rooms i)))
                               (coord-x (moor-r-gold (aref rooms i)))
                               GOLD)))
           (if (plusp gtotal)
               (progn
                 (setf (aref s-know S-GFIND) t)
                 (show-win hw
                           "You begin to feel greedy and you sense gold.--More--"))
               (msg "You begin to feel a pull downward"))))
        (#.S-TELEP
         ;; Scroll of teleportation:
         ;; Make him dissapear and reappear
         (let ((cur-room (roomin hero))
               (rm (teleport)))
           (when (not (eql cur-room (aref rooms rm)))
             (setf (aref s-know S-TELEP) t))))
        (#.S-ENCH
         (if (null cur-weapon)
             (msg "You feel a strange sense of loss.")
             (progn
               (logclr! (object-o-flags cur-weapon) ISCURSED)
               (if (> (rnd 100) 50)
                   (incf (object-o-hplus cur-weapon))
                   (incf (object-o-dplus cur-weapon)))
               (msg "Your ~a glows blue for a moment." 
                    (aref w-names (object-o-which cur-weapon))))))
        (#.S-SCARE
         ;; A monster will refuse to step on a scare monster scroll if
         ;; it is dropped.  Thus reading it is a mistake and produces
         ;; laughter at the poor rogue's boo boo.
         (msg "You hear maniacal laughter in the distance."))
        (#.S-REMOVE
         (dolist (obj (list cur-armor
                            cur-weapon
                            (aref cur-ring RIGHT)
                            (aref cur-ring LEFT)))
           (when obj 
             (logclr! (object-o-flags obj) ISCURSED)))
         (msg "You feel as if somebody is watching over you."))
        (#.S-AGGR
         ;; This scroll aggravates all the monsters on the current level
         ;; and sets them running towards the hero
         (aggravate)
         (msg "You hear a high pitched humming noise."))
        (#.S-NOP
         (msg "This scroll seems to be blank."))
        (#.S-GENOCIDE
         (msg "You have been granted the boon of genocide")
         (genocide)
         (setf (aref s-know S-GENOCIDE) t))
        (otherwise
         (msg"What a puzzling scroll!")
         (return-from read-scroll)))
      (look t)           ; put the result of the scroll on the screen 
      (status)
      (cond
        ((and (aref s-know which)
              (aref s-guess which))
         (setf (aref s-guess which) nil))
        ((and (not (aref s-know which))
              askme
              (null (aref s-guess which)))
         (msg (if terse "Call it: " "What do you want to call it? "))
         (multiple-value-bind (buf status)
             (get-str "" cw)
           (when (eql status NORM)
             (setf (aref s-guess which) (copy-seq buf))))))
      ;; Get rid of the thing
      (decf inpack)
      (if (>1 (object-o-count obj))
          (decf (object-o-count obj))
          (detach pack obj)))))
