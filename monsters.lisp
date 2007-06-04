;;;; File with various monster functions in it
;;;; @(#)monsters.c	3.18 (Berkeley) 6/15/81

(in-package :cl-rogue)

;;; List of monsters in rough order of vorpalness
(defparameter lvl_mons  "KJBSHEAOZGLCRQNYTWFIXUMVDP")
(defparameter wand_mons "KJBSH AOZG CRQ Y W IXU V  ")

(defun randmonster (wander)
  "Pick a monster to show up.  The lower the level, the meaner
the monster."
  (let (d 
        (mons (if wander wand_mons lvl_mons)))
    (loop
       (setf d (+ level (- (rnd 10) 
                           5)))
       (cond
         ((< d 1) (setf d (1+ (rnd 5))))
         ((> d 26) (setf d (+ (rnd 5) 22))))
       (decf d)
       (let ((mon (aref mons d)))
         (when (not (eql mon #\Space))
           (return-from randmonster mon))))))

(defun new_monster (tp type cp)
  "Pick a new monster and add it to the list."
  (push tp mlist)
  (setf (thing-t_type tp) type
        (thing-t_pos tp) cp
        (thing-t_oldch tp) (rogue-mvwinch cw (coord-y cp) (coord-x cp)))
  (rogue-mvwaddch mw (coord-y cp) (coord-x cp) (thing-t_type tp))
  (let ((mp (char-monster (thing-t_type tp))))
    (setf (stats-s_hpt (thing-t_stats tp)) (roll (stats-s_lvl (monster-m_stats mp)) 8)
          (stats-s_lvl (thing-t_stats tp)) (stats-s_lvl (monster-m_stats mp))
          (stats-s_arm (thing-t_stats tp)) (stats-s_arm (monster-m_stats mp))
          (stats-s_dmg (thing-t_stats tp)) (copy-seq (stats-s_dmg (monster-m_stats mp)))
          (stats-s_exp (thing-t_stats tp)) (stats-s_exp (monster-m_stats mp))
          (str_t-st_str (stats-s_str (thing-t_stats tp))) 10
          (thing-t_flags tp) (monster-m_flags mp)
          (thing-t_turn tp) t
          (thing-t_pack tp) nil)
    (when (iswearing R_AGGR)
      (runto cp hero))
    (when (eql type #\M)
      (setf (thing-t_disguise tp)
            (if (thing-t_pack tp)
                (object-o_type (car (thing-t_pack tp)))
                (progn
                  (ecase (rnd (if (> level 25) 
                                  9 
                                  8))
                    (0 GOLD)
                    (1 POTION)
                    (2 SCROLL)
                    (3 STAIRS)
                    (4 WEAPON)
                    (5 ARMOR)
                    (6 RING)
                    (7 STICK)
                    (8 AMULET))))))))

(defun wanderer ()
  "A wandering monster has awakened and is headed for the player."
  (let (i 
        ch 
        rp
        (hr (roomin hero))
        (tp (make-thing))
        (cp (make-coord)))
    (loop
       (setf i (rnd_room))
       (when (not (eql (setf rp (aref rooms i)) hr))
         (rnd_pos rp cp)
         (setf ch (rogue-mvwinch cl-ncurses:*stdscr* (coord-y cp) (coord-x cp)))
         (when (eql ch 'err)   ; XXX: figure out what ERR is in c world
           (rogue-debug "Routine wanderer: mvwinch failed to ~d,~d" (coord-y cp) (coord-x cp))
           (wait_for #\Newline)
           (return-from wanderer))
         (when (and
                (not (eql hr rp))
                (step_ok ch))
           (return))))
    (new_monster tp (randmonster t) cp)
    (setf (thing-t_flags tp) (logior (thing-t_flags tp) ISRUN)
          (thing-t_pos tp) cp
          (thing-t_dest tp) hero)
    (rogue-debug "Started a wandering ~a" (char-monster-name (thing-t_type tp)))))

(defun wake_monster (y x)
  "What to do when the hero steps next to a monster."
  (let ((tp (find_mons y x))
        rp 
        ch)
    (when (null tp)
      (msg "Can't find monster in show"))
    (setf ch (thing-t_type tp))
    ;; Every time he sees mean monster, it might start chasing him
    (when (and (> (rnd 100) 33) 
               (on tp ISMEAN) 
               (off tp ISHELD)
               (not (iswearing R_STEALTH)))
      (setf (thing-t_dest tp) hero
            (thing-t_flags tp) (logior (thing-t_flags tp) ISRUN))

      (when (and (eql ch #\U) 
                 (off *player* ISBLIND))
        (setf rp (roomin hero))
        (when (or
               (and (not (null rp))
                    (not (logtest (moor-r_flags rp) ISDARK)))
               (< (distance y x hero.y hero.x) 3))
          (when (and (off tp ISFOUND) 
                     (not (save VS_MAGIC)))
            (msg "The umber hulk's gaze has confused you.")
            (if (on *player* ISHUH)
                (lengthen 'unconfuse (+ (rnd 20) HUHDURATION))
                (fuse 'unconfuse 0 (+ (rnd 20) HUHDURATION) AFTER))
            (setf (thing-t_flags *player*) (logior (thing-t_flags *player*) ISHUH)))
          (setf (thing-t_flags tp) (logior (thing-t_flags tp) ISFOUND))))
      ;; Hide invisible monsters
      (when (and (on tp ISINVIS) (off *player* CANSEE))
        (setf ch (rogue-mvwinch cl-ncurses:*stdscr* y x)))

      ;; Let greedy ones guard gold
      (when (and (on tp ISGREED) 
                 (off tp ISRUN)
                 (not (null rp))
                 (plusp (moor-r_goldval rp)))
        (setf (thing-t_dest tp) (moor-r_gold rp)
              (thing-t_flags tp) (logior (thing-t_flags tp) ISRUN))))
    tp))

(defun genocide ()
  (let (c)
    (addmsg "Which monster")
    (unless terse
      (addmsg " do you wish to wipe out"))
    (msg "? ")
    (while (not (alpha-char-p (setf c (readchar))))
      (if (eql c #\Escape)
          (return-from genocide)
          (progn
            (zero! mpos)
            (msg "Please specifiy a letter between 'A' and 'Z'"))))
    (when (lower-case-p c)
      (setf c (char-upcase c)))
    (map nil
         #'(lambda (mp)
             (remove_monster (thing-t_pos mp) 
                             mp))
         mlist)
    (setf mlist (remove c mlist :key #'thing-t_type))
    (dotimes (i (length lvl_mons))
      (when (eql (aref lvl_mons i) c)
        (setf (aref lvl_mons i) #\Space)
        (setf (aref wand_mons i) #\Space)
        (return)))))
