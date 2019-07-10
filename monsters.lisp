;;;; File with various monster functions in it
;;;; @(#)monsters.c	3.18 (Berkeley) 6/15/81

(in-package :cl-rogue)

;;; List of monsters in rough order of vorpalness
(defparameter lvl-mons  "KJBSHEAOZGLCRQNYTWFIXUMVDP")
(defparameter wand-mons "KJBSH AOZG CRQ Y W IXU V  ")

(defun randmonster (wander)
  "Pick a monster to show up.  The lower the level, the meaner
the monster."
  (let (d 
        (mons (if wander wand-mons lvl-mons)))
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

(defun new-monster (tp type cp)
  "Pick a new monster and add it to the list."
  (push tp mlist)
  (setf (thing-t-type tp) type
        (thing-t-pos tp) cp
        (thing-t-oldch tp) (rogue-mvwinch cw (coord-y cp) (coord-x cp)))
  (rogue-mvwaddch mw (coord-y cp) (coord-x cp) (thing-t-type tp))
  (let ((mp (char-monster (thing-t-type tp))))
    (setf (stats-s-hpt (thing-t-stats tp)) (roll (stats-s-lvl (monster-m-stats mp)) 8)
          (stats-s-lvl (thing-t-stats tp)) (stats-s-lvl (monster-m-stats mp))
          (stats-s-arm (thing-t-stats tp)) (stats-s-arm (monster-m-stats mp))
          (stats-s-dmg (thing-t-stats tp)) (copy-seq (stats-s-dmg (monster-m-stats mp)))
          (stats-s-exp (thing-t-stats tp)) (stats-s-exp (monster-m-stats mp))
          (str-t-st-str (stats-s-str (thing-t-stats tp))) 10
          (thing-t-flags tp) (monster-m-flags mp)
          (thing-t-turn tp) t
          (thing-t-pack tp) nil)
    (when (iswearing R-AGGR)
      (runto cp hero))
    (when (eql type #\M)
      (setf (thing-t-disguise tp)
            (if (thing-t-pack tp)
                (object-o-type (car (thing-t-pack tp)))
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
       (setf i (rnd-room))
       (when (not (eql (setf rp (aref rooms i)) hr))
         (rnd-pos rp cp)
         (setf ch (rogue-mvwinch cl-charms/low-level:*stdscr* (coord-y cp) (coord-x cp)))
         (when (eql ch 'err)   ; XXX: figure out what ERR is in c world
           (rogue-debug "Routine wanderer: mvwinch failed to ~d,~d" (coord-y cp) (coord-x cp))
           (wait-for #\Newline)
           (return-from wanderer))
         (when (and
                (not (eql hr rp))
                (step-ok ch))
           (return))))
    (new-monster tp (randmonster t) cp)
    (setf (thing-t-flags tp) (logior (thing-t-flags tp) ISRUN)
          (thing-t-pos tp) cp
          (thing-t-dest tp) hero)
    (rogue-debug "Started a wandering ~a" (char-monster-name (thing-t-type tp)))))

(defun wake-monster (y x)
  "What to do when the hero steps next to a monster."
  (let ((tp (find-mons y x))
        rp 
        ch)
    (when (null tp)
      (msg "Can't find monster in show"))
    (setf ch (thing-t-type tp))
    ;; Every time he sees mean monster, it might start chasing him
    (when (and (> (rnd 100) 33) 
               (on tp ISMEAN) 
               (off tp ISHELD)
               (not (iswearing R-STEALTH)))
      (setf (thing-t-dest tp) hero
            (thing-t-flags tp) (logior (thing-t-flags tp) ISRUN))

      (when (and (eql ch #\U) 
                 (off *player* ISBLIND))
        (setf rp (roomin hero))
        (when (or
               (and (not (null rp))
                    (not (logtest (moor-r-flags rp) ISDARK)))
               (< (distance y x hero.y hero.x) 3))
          (when (and (off tp ISFOUND) 
                     (not (save VS-MAGIC)))
            (msg "The umber hulk's gaze has confused you.")
            (if (on *player* ISHUH)
                (lengthen 'unconfuse (+ (rnd 20) HUHDURATION))
                (fuse 'unconfuse 0 (+ (rnd 20) HUHDURATION) AFTER))
            (setf (thing-t-flags *player*) (logior (thing-t-flags *player*) ISHUH)))
          (setf (thing-t-flags tp) (logior (thing-t-flags tp) ISFOUND))))
      ;; Hide invisible monsters
      (when (and (on tp ISINVIS) (off *player* CANSEE))
        (setf ch (rogue-mvwinch cl-charms/low-level:*stdscr* y x)))

      ;; Let greedy ones guard gold
      (when (and (on tp ISGREED) 
                 (off tp ISRUN)
                 (not (null rp))
                 (plusp (moor-r-goldval rp)))
        (setf (thing-t-dest tp) (moor-r-gold rp)
              (thing-t-flags tp) (logior (thing-t-flags tp) ISRUN))))
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
             (remove-monster (thing-t-pos mp) 
                             mp))
         mlist)
    (setf mlist (remove c mlist :key #'thing-t-type))
    (dotimes (i (length lvl-mons))
      (when (eql (aref lvl-mons i) c)
        (setf (aref lvl-mons i) #\Space)
        (setf (aref wand-mons i) #\Space)
        (return)))))
