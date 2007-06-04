;;;; Function(s) for dealing with potions
;;;; @(#)potions.c	3.1	3.1	5/7/81

(in-package :cl-rogue)

(defun quaff ()
  (let ((obj (get_item "quaff" POTION)))
    ;;
    ;; Make certain that it is somethings that we want to drink
    ;;
    (unless obj
      (return-from quaff))
    (unless (eql (object-o_type obj) POTION)
      (msg (if terse
               "That's undrinkable" 
               "Yuk! Why would you want to drink that?"))
      (return-from quaff))

    (if (eql obj cur_weapon)
        (setf cur_weapon nil))

    ;;
    ;; Calculate the effect it has on the poor guy.
    ;;
    (case (object-o_which  obj)
      (#.P_CONFUSE
       (when (off *player* ISHUH)
         (msg "Wait, what's going on here. Huh? What? Who?")
         (if (on *player* ISHUH)
             (lengthen 'unconfuse (+ (rnd 8) HUHDURATION))
             (fuse 'unconfuse 0 (+ (rnd 8) HUHDURATION) AFTER))
         (logior! (thing-t_flags *player*) ISHUH))
       (setf (aref p_know P_CONFUSE) t))
      (#.P_POISON
       (if (not (iswearing R_SUSTSTR))
           (progn
             (chg_str (- (1+ (rnd 3))))
             (msg "You feel very sick now."))
           (msg "You feel momentarily sick"))
       (setf (aref p_know P_POISON) t))
      (#.P_HEALING
       (when
           (> (incf (stats-s_hpt pstats)
                    (roll (stats-s_lvl pstats) 4))
              max_hp)
         (setf (stats-s_hpt pstats) (incf max_hp))
         (msg "You begin to feel better.")
         (sight)
         (setf (aref p_know P_HEALING) t)))
      (#.P_STRENGTH
       (msg "You feel stronger, now.  What bulging muscles!")
       (chg_str 1)
       (setf (aref p_know P_STRENGTH) t))
      (#.P_MFIND
       ;;
       ;; Potion of monster detection, if there are monters, detect them
       ;;
       (if mlist
           (progn
             (cl-ncurses:wclear hw)
             (cl-ncurses:overwrite mw hw)
             (show_win hw "You begin to sense the presence of monsters.--More--")
             (setf (aref p_know P_MFIND) t)) ;
           (msg "You have a strange feeling for a moment, then it passes.")))
      (#.P_TFIND
       ;;
       ;; Potion of magic detection.  Show the potions and scrolls
       ;;
       (let (show)
         (when lvl_obj
           (cl-ncurses:wclear hw)
           (dolist (tp lvl_obj)
             (when (is_magic tp)
               (setf show t)
               (rogue-mvwaddch hw 
                                     (coord-y (object-o_pos tp))
                                     (coord-x (object-o_pos tp))
                                     MAGIC))
             (setf (aref p_know P_TFIND) t))
           (dolist (th mlist)
             (dolist (pitem (thing-t_pack th))
               (when (is_magic pitem)
                 (setf show t)
                 (rogue-mvwaddch hw
                                       (coord-y (thing-t_pos th))
                                       (coord-x (thing-t_pos th))
                                       MAGIC))
               (setf (aref p_know P_TFIND) t))))
         (if show
             (show_win hw "You sense the presence of magic on this level.--More--")
             (msg "You have a strange feeling for a moment, then it passes."))))
        (#.P_PARALYZE
         (msg "You can't move.")
         (setf no_command HOLDTIME
               (aref p_know P_PARALYZE) t))
        (#.P_SEEINVIS
         (msg "This potion tastes like ~a juice." fruit)
         (when (off *player* CANSEE)
           (logior! (thing-t_flags *player*) CANSEE)
           (fuse 'unsee 0 SEEDURATION AFTER)
           (light hero))
         (sight))
        (#.P_RAISE
         (msg "You suddenly feel much more skillful")
         (setf (aref p_know P_RAISE) t)
         (raise_level))
        (#.P_XHEAL
         (when
             (> (incf (stats-s_hpt pstats)
                      (roll (stats-s_lvl pstats) 8))
                max_hp)
           (setf (stats-s_hpt pstats) (incf max_hp)))
         (msg "You begin to feel much better.")
         (setf (aref p_know P_XHEAL) t)
         (sight))
        (#.P_HASTE
         (add_haste t)
         (msg "You feel yourself moving much faster.")
         (setf (aref p_know P_HASTE) t))
        (#.P_RESTORE
         (msg "Hey, this tastes great.  It make you feel warm all over.")
         (when (or (< (str_t-st_str (stats-s_str pstats))
                      (str_t-st_str (stats-s_str max_stats)))
                   (and (= (str_t-st_str (stats-s_str pstats)) 18)
                        (< (str_t-st_add (stats-s_str pstats))
                           (str_t-st_add (stats-s_str max_stats)))))
           (setf (stats-s_str pstats) (stats-s_str max_stats))))
        (#.P_BLIND
         (msg "A cloak of darkness falls around you.")
         (when (off *player* ISBLIND)
           (logior! (thing-t_flags *player*) ISBLIND)
           (fuse 'sight 0 SEEDURATION AFTER)
           (look nil))
         (setf (aref p_know P_BLIND) t))
        (#.P_NOP
         (msg "This potion tastes extremely dull."))
        (otherwise
         (msg "What an odd tasting potion!")
         (return-from quaff)))
    (status)
    (if (and (aref p_know (object-o_which obj))
             (aref p_guess (object-o_which obj)))
        (setf (aref p_guess (object-o_which obj)) nil)
        (when (and
               (not (aref p_know (object-o_which obj)))
               askme 
               (not (aref p_guess (object-o_which obj))))
          (msg (if terse "Call it: " "What do you want to call it? "))
          (let ((buf ""))
            (when (eql (get_str buf cw) NORM)
              (setf (aref p_guess (object-o_which obj)) buf)))))
    ;;
    ;; Throw the item away
    ;;
    (decf inpack)
    (if (> (object-o_count obj) 1)
        (incf (object-o_count obj))
        (detach pack obj))))
