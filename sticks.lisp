;;;; Functions to implement the various sticks one might find
;;;; while wandering around the dungeon.
;;;; @(#)sticks.c     3.14 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defun fix_stick (cur)
  (let ((which (object-o_which cur)))
    (setf (object-o_damage cur)
          (if (string= (aref ws_type which) "staff")
              "2d3" "1d1")
          (object-o_hurldmg cur) "1d1"
          (object-o_charges cur) (+ 3 (rnd 5)))
    (case which
      (#.WS_HIT
       (setf (object-o_hplus cur) 3
             (object-o_dplus cur) 3
             (object-o_damage cur) "1d8"))
      (#.WS_LIGHT
       (setf (object-o_charges cur) (+ 10 (rnd 10)))))))

(defparameter *bolt*
  (make-object :o_type #\* :o_hurldmg "1d4" :o_hplus 100 :o_dplus 1)
  "The bolt from a wand of magic missiles.")

(defparameter *ray*
  (make-object :o_type #\* :o_hurldmg "6d6" :o_hplus 100)
  "The ray from a wand of fire, cold, etc.")

(defun do_zap (gotdir)
  (when-let (obj (get_item "zap with" STICK))
    (when (not (eq (object-o_type obj) STICK))
      (msg "You can't zap with that!")
      (setf *after* nil)
      (return-from do_zap))
    (when (zerop (object-o_charges obj))
      (msg "Nothing happens.")
      (return-from do_zap))

    (let ((which (object-o_which obj)))
      (unless gotdir
        (loop
           (setf delta (make-coord :y (1- (rnd 3)) 
                                   :x (1- (rnd 3))))
           (unless (and (zerop delta.y)
                        (zerop delta.x))
             (return))))
      (case which
        (#.WS_LIGHT
         ;; Reddy Kilowat wand.  Light up the room
         (setf (aref ws_know WS_LIGHT) t)
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
        (#.WS_DRAIN
         ;; Take away 1/2 of hero's hit points, then take it away
         ;; evenly from the monsters in the room (or next to hero if
         ;; he is in a passage)
         (if (< (stats-s_hpt pstats) 2)
             (progn
               (msg "You are too weak to use it.")
               (return-from do_zap))
             (let ((rp (roomin hero)))
               (if (null rp)
                   (drain (1- hero.y) (1+ hero.y) (1- hero.x) (1+ hero.x))
                   (let* ((pos   (moor-r_pos rp))
                          (max   (moor-r_max rp))
                          (y     (coord-y pos))
                          (max-y (coord-y max))
                          (x     (coord-x pos))
                          (max-x (coord-x max)))
                     (drain y (+ y max-y) x (+ x max-x)))))))
        ((#.WS_POLYMORPH #.WS_TELAWAY #.WS_TELTO #.WS_CANCEL)
         (let ((y hero.y)
               (x hero.x))
           (while (step_ok (winat y x))
             (incf y delta.y)
             (incf x delta.x))
           (let* ((monster (rogue-mvwinch mw y x))
                  (omonst monster))
             (when (upper-case-p monster)
               (when (eq monster #\F)
                 (logclr! (thing-t_flags *player*) ISHELD))
               (let ((tp (find_mons y x)))
                 (case which
                   (#.WS_POLYMORPH
                    (detach mlist tp)
                    (let ((oldch (thing-t_oldch tp)))
                      (setf delta.y y)
                      (setf delta.x x)
                      (setf monster (code-char (+ (rnd 26) (char-code #\A))))
                      (new_monster tp monster delta)
                      (unless (logtest (thing-t_flags tp) ISRUN)
                        (runto delta hero))
                      (when (upper-case-p (rogue-mvwinch cw y x))
                        (rogue-mvwaddch cw y x monster))
                      (setf (thing-t_oldch tp) oldch)
                      (setf (aref ws_know WS_POLYMORPH)
                            (or (aref ws_know WS_POLYMORPH)
                                (not (eq monster omonst))))))
                   (#.WS_CANCEL
                    (logior! (thing-t_flags tp) ISCANC)
                    (logclr! (thing-t_flags tp) ISINVIS))
                   (otherwise
                    (case which
                      (#.WS_TELAWAY
                       (loop
                          (rnd_pos (aref rooms (rnd_room))
                                   (thing-t_pos tp))
                          (when (eq (winat (coord-y (thing-t_pos tp))
                                           (coord-x (thing-t_pos tp)))
                                    FLOOR))))
                      (otherwise
                       (setf (thing-t_pos tp) (make-coord :y (+ hero.y delta.y)
                                                          :x (+ hero.x delta.x)))))
                    (when (upper-case-p (rogue-mvwinch cw y x))
                      (rogue-mvwaddch cw y x (thing-t_oldch tp)))
                    (setf (thing-t_dest tp) hero)
                    (logior! (thing-t_flags tp) ISRUN)
                    (rogue-mvwaddch mw y x #\Space)
                    (rogue-mvwaddch mw
                                    (coord-y (thing-t_pos tp))
                                    (coord-x (thing-t_pos tp))
                                    monster)
                    (when (or (not (eq (coord-y (thing-t_pos tp))
                                       y))
                              (not (eq (coord-x (thing-t_pos tp))
                                       x)))
                      (setf (thing-t_oldch tp)
                            (rogue-mvwinch cw
                                           (coord-y (thing-t_pos tp))
                                           (coord-x (thing-t_pos tp))))))))))))
        (#.WS_MISSILE
         (do_motion *bolt* delta.y delta.x)
         (let* ((pos (object-o_pos *bolt*))
                (y (coord-y pos))
                (x (coord-x pos)))
           (cond
             ((and (upper-case-p (rogue-mvwinch mw (coord-y pos) (coord-x pos)))
                   (not (save_throw VS_MAGIC (find_mons y x))))
              (hit_monster y x *bolt*))
             (t (msg
                 (if terse
                     "Missle vanishes"
                     "The missle vanishes with a puff of smoke")))))
         (setf (aref ws_know WS_MISSILE) t))
        (#.WS_HIT
         (incf delta.y hero.y)
         (incf delta.x hero.x)
         (let ((ch (winat delta.y delta.x)))
           (when (upper-case-p ch)
             (multiple-value-bind (dmg dplus)
                 (if (zerop (rnd 20))
                     (values "3d8" 9)
                     (values "1d8" 3))
               (setf (object-o_damage obj) dmg
                     (object-o_dplus obj) dplus))
             (fight delta ch obj nil))))
        ((#.WS_HASTE_M #.WS_SLOW_M)
         (let ((y hero.y)
               (x hero.x))
           (while (step_ok (winat y x))
             (incf y delta.y)
             (incf x delta.x))
           (when (upper-case-p (rogue-mvwinch mw y x))
             (let ((tp (find_mons y x)))
               (if (eq which WS_HASTE_M)
                   (if (on tp ISSLOW)
                       (logclr! (thing-t_flags tp) ISSLOW)
                       (logior! (thing-t_flags tp) ISHASTE))
                   (progn
                     (if (on tp ISHASTE)
                         (logclr! (thing-t_flags tp) ISHASTE)
                         (logior! (thing-t_flags tp) ISSLOW))
                     (setf (thing-t_turn tp) t))))
             (setf delta.y y)
             (setf delta.x x)
             (runto delta hero))))
        ((#.WS_ELECT #.WS_FIRE #.WS_COLD)
         (let ((dirch (case (abs (+ delta.y delta.x))
                        (0 #\/)
                        (1 (if (zerop delta.y) #\- #\|))
                        (2 #\\)))
               ch 
               (y 0)
               (name (case which
                       (#.WS_ELECT "bolt")
                       (#.WS_FIRE "flame")
                       (otherwise "ice")))
               (bounced nil)
               (used nil)
               (pos (copy-structure hero))
               (spotpos (make-array BOLT_LENGTH)))
           (symbol-macrolet ((pos.x (coord-x pos))
                             (pos.y (coord-y pos)))
             (loop
                (when (or (>= y BOLT_LENGTH) 
                          used)
                  (return))
                (setf ch (winat pos.y pos.x))
                (setf (aref spotpos y) pos)
                (case ch
                  ((#.DOOR
                    #.SECRETDOOR
                    #\|
                    #\-
                    #\Space)
                   (setf bounced t
                         delta.y (- delta.y)
                         delta.x (- delta.x))
                   (decf y)
                   (msg "The bolt bounces"))
                  (otherwise
                   (if (and (not bounced)
                            (upper-case-p ch))
                       (if (not (save_throw VS_MAGIC (find_mons pos.y pos.x)))
                           (progn
                             (setf (object-o_pos *ray*) pos)
                             (hit_monster pos.y pos.x *ray*)
                             (setf used t))
                           (if (or (not (eq ch #\M))
                                   (eq (show pos.y pos.x) #\M))
                               (progn
                                 (if terse
                                     (msg "~a misses" name)
                                     (msg "The ~a whizzes past the ~a"  name (char-monster-name ch)))
                                 (runto pos hero))))
                       (if (and bounced 
                                (equalp pos hero))
                           (progn 
                             (setf bounced nil)
                             (if (not (save VS_MAGIC))
                                 (progn
                                   (msg (if terse
                                            "The ~a hits"
                                            "You are hit by the ~a") 
                                        name)
                                   (when (<= (decf (stats-s_hpt pstats)
                                                   (roll 6 6))
                                             0)
                                     (death #\b))
                                   (setf used t))
                                 (msg "The ~a whizzes by you" name)))))
                   (rogue-mvwaddch cw pos.y pos.x dirch)
                   (draw cw)))
                (incf pos.y delta.y)
                (incf pos.x delta.x)
                (incf y)))
           (for (x 0 (1- y))
             (let* ((spos (aref spotpos x))
                    (x (coord-x spos))
                    (y (coord-y spos)))
               (rogue-mvwaddch cw y x (show y x))))))
        (otherwise (msg "What a bizarre schtick!"))))
    (decf (object-o_charges obj))))

(defun drain (ymin ymax xmin xmax)
  "Do drain hit points from player shtick."
  ;; First count how many things we need to spread the hit points among
  (let ((count 0))
    (for (i ymin ymax)
      (for (j xmin xmax)
        (when (upper-case-p (rogue-mvwinch mw i j))
          (incf count))))
    (when (zerop count)
      (msg "You have a tingling feeling")
      (return-from drain))
    (setf count (truncate (/ (stats-s_hpt pstats)
                             count))
          (stats-s_hpt pstats) (truncate (/ (stats-s_hpt pstats)
                                            2)))
    ;; Now zot all of the monsters
    (for (i ymin ymax)
      (for (j xmin xmax)
        (when (upper-case-p (rogue-mvwinch mw i j))
          (when-let (ick (find_mons i j))
            (unless (plusp (decf (stats-s_hpt (thing-t_stats ick))
                                 count))
              (killed ick (and (cansee i j)
                               (not (on ick ISINVIS)))))))))))

(defun charge_str (obj)
  "Charge a wand for wizards."
  (cond
    ((not (logtest (object-o_flags obj) ISKNOW))
     "")
    (t
     (format nil
             (if terse " [~d]" " [~d charges]")
             (object-o_charges obj)))))
