;;;; Function(s) for dealing with potions
;;;; @(#)potions.c	3.1	3.1	5/7/81

(in-package :cl-rogue)

(defun quaff ()
  (let ((obj (get-item "quaff" POTION)))
    ;;
    ;; Make certain that it is somethings that we want to drink
    ;;
    (unless obj
      (return-from quaff))
    (unless (eql (object-o-type obj) POTION)
      (msg (if terse
               "That's undrinkable" 
               "Yuk! Why would you want to drink that?"))
      (return-from quaff))

    (if (eql obj cur-weapon)
        (setf cur-weapon nil))

    ;;
    ;; Calculate the effect it has on the poor guy.
    ;;
    (case (object-o-which  obj)
      (#.P-CONFUSE
       (when (off *player* ISHUH)
         (msg "Wait, what's going on here. Huh? What? Who?")
         (if (on *player* ISHUH)
             (lengthen 'unconfuse (+ (rnd 8) HUHDURATION))
             (fuse 'unconfuse 0 (+ (rnd 8) HUHDURATION) AFTER))
         (logior! (thing-t-flags *player*) ISHUH))
       (setf (aref p-know P-CONFUSE) t))
      (#.P-POISON
       (if (not (iswearing R-SUSTSTR))
           (progn
             (chg-str (- (1+ (rnd 3))))
             (msg "You feel very sick now."))
           (msg "You feel momentarily sick"))
       (setf (aref p-know P-POISON) t))
      (#.P-HEALING
       (when
           (> (incf (stats-s-hpt pstats)
                    (roll (stats-s-lvl pstats) 4))
              max-hp)
         (setf (stats-s-hpt pstats) (incf max-hp))
         (msg "You begin to feel better.")
         (sight)
         (setf (aref p-know P-HEALING) t)))
      (#.P-STRENGTH
       (msg "You feel stronger, now.  What bulging muscles!")
       (chg-str 1)
       (setf (aref p-know P-STRENGTH) t))
      (#.P-MFIND
       ;;
       ;; Potion of monster detection, if there are monters, detect them
       ;;
       (if mlist
           (progn
             (cl-ncurses:wclear hw)
             (cl-ncurses:overwrite mw hw)
             (show-win hw "You begin to sense the presence of monsters.--More--")
             (setf (aref p-know P-MFIND) t)) ;
           (msg "You have a strange feeling for a moment, then it passes.")))
      (#.P-TFIND
       ;;
       ;; Potion of magic detection.  Show the potions and scrolls
       ;;
       (let (show)
         (when lvl-obj
           (cl-ncurses:wclear hw)
           (dolist (tp lvl-obj)
             (when (is-magic tp)
               (setf show t)
               (rogue-mvwaddch hw 
                                     (coord-y (object-o-pos tp))
                                     (coord-x (object-o-pos tp))
                                     MAGIC))
             (setf (aref p-know P-TFIND) t))
           (dolist (th mlist)
             (dolist (pitem (thing-t-pack th))
               (when (is-magic pitem)
                 (setf show t)
                 (rogue-mvwaddch hw
                                       (coord-y (thing-t-pos th))
                                       (coord-x (thing-t-pos th))
                                       MAGIC))
               (setf (aref p-know P-TFIND) t))))
         (if show
             (show-win hw "You sense the presence of magic on this level.--More--")
             (msg "You have a strange feeling for a moment, then it passes."))))
        (#.P-PARALYZE
         (msg "You can't move.")
         (setf no-command HOLDTIME
               (aref p-know P-PARALYZE) t))
        (#.P-SEEINVIS
         (msg "This potion tastes like ~a juice." fruit)
         (when (off *player* CANSEE)
           (logior! (thing-t-flags *player*) CANSEE)
           (fuse 'unsee 0 SEEDURATION AFTER)
           (light hero))
         (sight))
        (#.P-RAISE
         (msg "You suddenly feel much more skillful")
         (setf (aref p-know P-RAISE) t)
         (raise-level))
        (#.P-XHEAL
         (when
             (> (incf (stats-s-hpt pstats)
                      (roll (stats-s-lvl pstats) 8))
                max-hp)
           (setf (stats-s-hpt pstats) (incf max-hp)))
         (msg "You begin to feel much better.")
         (setf (aref p-know P-XHEAL) t)
         (sight))
        (#.P-HASTE
         (add-haste t)
         (msg "You feel yourself moving much faster.")
         (setf (aref p-know P-HASTE) t))
        (#.P-RESTORE
         (msg "Hey, this tastes great.  It make you feel warm all over.")
         (when (or (< (str-t-st-str (stats-s-str pstats))
                      (str-t-st-str (stats-s-str max-stats)))
                   (and (= (str-t-st-str (stats-s-str pstats)) 18)
                        (< (str-t-st-add (stats-s-str pstats))
                           (str-t-st-add (stats-s-str max-stats)))))
           (setf (stats-s-str pstats) (stats-s-str max-stats))))
        (#.P-BLIND
         (msg "A cloak of darkness falls around you.")
         (when (off *player* ISBLIND)
           (logior! (thing-t-flags *player*) ISBLIND)
           (fuse 'sight 0 SEEDURATION AFTER)
           (look nil))
         (setf (aref p-know P-BLIND) t))
        (#.P-NOP
         (msg "This potion tastes extremely dull."))
        (otherwise
         (msg "What an odd tasting potion!")
         (return-from quaff)))
    (status)
    (if (and (aref p-know (object-o-which obj))
             (aref p-guess (object-o-which obj)))
        (setf (aref p-guess (object-o-which obj)) nil)
        (when (and
               (not (aref p-know (object-o-which obj)))
               askme 
               (not (aref p-guess (object-o-which obj))))
          (msg (if terse "Call it: " "What do you want to call it? "))
          (let ((buf ""))
            (when (eql (get-str buf cw) NORM)
              (setf (aref p-guess (object-o-which obj)) buf)))))
    ;;
    ;; Throw the item away
    ;;
    (decf inpack)
    (if (> (object-o-count obj) 1)
        (incf (object-o-count obj))
        (detach pack obj))))
