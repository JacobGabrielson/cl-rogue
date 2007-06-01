;;;; All the fighting gets done here
;;;; @(#)fight.c	3.28 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defparameter e_levels 
  #(10 20 40 80 160 320 640 1280 2560 5120 10240 20480
    40920 81920 163840 327680 655360 1310720 2621440 0))

(defun fight (mp mn weap thrown)
  "The player attacks the monster."
  (let ((did_hit t)
        (tp (find_mons (coord-y mp) (coord-x mp))))
    ;; Find the monster we want to fight
    (unless tp 
      (rogue-debug "Fight what @ ~d,~d" (coord-y mp) (coord-x mp)))
    ;; Since we are fighting, things are not quiet so no healing takes
    ;; place.
    (setf quiet 0)
    (runto mp hero)
    ;; Let him know it was really a mimic (if it was one).
    (when (and (eq (thing-t_type tp) #\M)
               (not (eq (thing-t_disguise tp) #\M))
               (off *player* ISBLIND))
      (msg "Wait! That's a mimic!")
      (setf (thing-t_disguise tp) #\M)  ;
      (setf did_hit thrown))
    (when did_hit
      (let ((mname (if (on *player* ISBLIND) "it" (monster-m_name (char-monster mn)))))
        (setf did_hit nil)
        (if (roll_em pstats (thing-t_stats tp) weap thrown)
            (progn
              (setf did_hit t)
              (if thrown
                  (thunk weap mname)
                  (hit nil mname))
              (when (on *player* CANHUH)
                (msg "Your hands stop glowing red")
                (msg "The ~a appears confused." mname)
                (logior! (thing-t_flags tp) ISHUH)
                (logclr! (thing-t_flags *player*) CANHUH))
              (when (<= (stats-s_hpt (thing-t_stats tp)) 0)
                (killed tp t)))
            (if thrown
                (bounce weap mname)
                (miss nil mname)))))
    (setf *count* 0)
    did_hit))

(defun attack (mp)
  "The monster attacks the player."
  ;; Since this is an attack, stop running and any healing that was
  ;; going on at the time.
  (setf running nil)
  (setf quiet 0)

  (when (and (eq (thing-t_type mp) #\M)
             (off *player* ISBLIND))
    (setf (thing-t_disguise mp) #\M))

  (let ((mname (if (on *player* ISBLIND) 
                   "it" 
                   (monster-m_name (char-monster (thing-t_type mp)))))
        (mtype (thing-t_type mp))
        (mstats (thing-t_stats mp)))
    (if (roll_em mstats pstats nil nil)
        (progn
          (unless (eq mtype #\E)
            (hit mname nil))
          (when (<= (stats-s_hpt pstats) 0)
            (death mtype))              ; Bye bye life ... 
          (when (off mp ISCANC)
            (case mtype
              (#\R
               ;;
               ;; If a rust monster hits, you lose armor
               ;;
               (when (and cur_armor (< (object-o_ac cur_armor) 9))
                 (if terse
                     (msg "Your armor weakens")
                     (msg "Your armor appears to be weaker now. Oh my!"))
                 (incf (object-o_ac cur_armor))))
              (#\E
               ;; The gaze of the floating eye hypnotizes you
               (unless (on *player* ISBLIND)
                 (unless (nonzerop no_command)
                   (addmsg "You are transfixed")
                   (unless terse
                     (addmsg " by the gaze of the floating eye."))
                   (endmsg))
                 (incf no_command 
                       (+ (rnd 2) 2))))
              (#\A
               ;;
               ;; Ants have poisonous bites
               ;;
               (unless (save VS_POISON)
                 (if (not (iswearing R_SUSTSTR))
                     (progn
                       (chg_str -1)
                       (if terse
                           (msg "A sting has weakened you")
                           (msg "You feel a sting in your arm and now feel weaker")))
                     (if terse
                         (msg "Sting has no effect")
                         (msg "A sting momentarily weakens you")))))
              (#\W
               ;;
               ;; Wraiths might drain energy levels
                                        ;
               (when (< (rnd 100) 15)
                 (when (zerop (decf (stats-s_exp pstats)))
                   (death #\W))         ; All levels gone 
                 (msg "You suddenly feel weaker.")
                 (if (zerop (decf (stats-s_lvl pstats)))
                     (setf (stats-s_exp pstats) 0
                           (stats-s_lvl pstats) 1)
                     (setf (stats-s_exp pstats) (1+ (aref e_levels
                                                          (1- (stats-s_lvl pstats))))))
                 (let ((fewer (roll 1 10)))
                   (decf (stats-s_hpt pstats) fewer)
                   (decf max_hp fewer))
                 (when (< (stats-s_hpt pstats) 1)
                   (setf (stats-s_hpt pstats) 1))
                 (when (< max_hp 1)
                   (death #\W))))
              (#\F
               ;; Violet fungi stops the poor guy from moving
               (logior! (thing-t_flags *player*) ISHELD)
               (setf (stats-s_dmg (monster-m_stats (char-monster #\F)))
                     (format nil "~dd1" (incf fung_hit))))
              (#\L
               ;; Leperachaun steals some gold
               (let ((lastpurse purse))
                 (decf purse (goldcalc))
                 (unless (save VS_MAGIC)
                   (decf purse (+ (goldcalc) (goldcalc) (goldcalc) (goldcalc))))
                 (when (< purse 0)
                   (setf purse 0))
                 (unless (= purse lastpurse)
                   (msg "Your purse feels lighter"))
                 (remove_monster (thing-t_pos mp)
                                 (find_mons (coord-y (thing-t_pos mp)) 
                                            (coord-x (thing-t_pos mp)))))
               (setf mp nil))
              (#\N
               ;; Nymphs steal a magic item, look through the pack
               ;; and pick out one we like.
               (let (obj
                     (nobj 0))
                 (map nil
                      #'(lambda (o)
                          (when (and (not (eq o cur_armor))
                                     (not (eq o cur_weapon))
                                     (is_magic o)
                                     (zerop (rnd (incf nobj))))
                            (setf obj o)))
                      pack)
                 (when obj
                   (remove_monster (thing-t_pos mp)
                                   (find_mons (coord-y (thing-t_pos mp))
                                              (coord-x (thing-t_pos mp))))
                   (setf mp nil)
                   (if (and (> (object-o_count obj) 1)
                            (zerop (object-o_group obj)))
                       (progn 
                         (let ((oc (1- (object-o_count obj))))
                           (setf (object-o_count obj) 1)
                           (msg "She stole ~a!" (inv_name obj t))
                           (setf (object-o_count obj) oc)))
                       (progn
                         (msg "She stole ~a!" (inv_name obj t))
                         (detach pack obj)))
                   (decf inpack)))))))
        ;; else of roll_em clause
        (unless (eq (thing-t_type mp) #\E)
          (when (eq (thing-t_type mp) #\F)
            (decf (stats-s_hpt pstats) fung_hit)
            (when (<= (stats-s_hpt pstats) 0)
              (death #\F)))             ; Bye bye life ... 
          (miss mname nil)))
    ;; Check to see if this is a regenerating monster and let it heal if
    ;; it is.
    (when (and mp (on mp ISREGEN) (< (rnd 100) 33))
      (incf (stats-s_hpt (thing-t_stats mp))))
    (when fight_flush
      (flush_type))                     ; flush typeahead 
    (setf *count* 0)
    (status)

    (if mp 0 -1)))

(defun swing (at_lvl op_arm wplus)
  "Returns true if the swing hits."
  (>= (+ (1+ (rnd 20))
         wplus)
      (- (- 21 at_lvl)
         op_arm)))

(defun check_level ()
  "Check to see if the guy has gone up a level."
  (when-let (i (position-if #'(lambda (l)
                                (> l (stats-s_exp pstats)))
                            e_levels))
    (incf i)
    (when (> i (stats-s_lvl pstats))
      (let ((add (roll (- i (stats-s_lvl pstats)) 10)))
        (incf max_hp add)
        (when (> (incf (stats-s_hpt pstats) add) max_hp)
          (setf (stats-s_hpt pstats) max_hp))
        (msg "Welcome to level ~d" i)))
    (setf (stats-s_lvl pstats) i)))

(defun parse-dice (dice)
  (values
   (parse-integer dice :junk-allowed t)
   (when-let (d (position #\d dice))
     (parse-integer dice :start (1+ d) :junk-allowed t))))

(defun roll_em (att def weap hurl)
  "Roll several attacks."
  (let (cp 
        def_arm
        did_hit
        (prop_hplus 0)
        (prop_dplus 0))
    (cond
      ((null weap) 
       (setf cp (stats-s_dmg att)))
      (hurl
       (if (and (logtest (object-o_flags weap) ISMISL)
                cur_weapon
                (eq (object-o_which cur_weapon) (object-o_launch weap)))
           (progn
             (setf cp (object-o_hurldmg weap))
             (setf prop_hplus (object-o_hplus cur_weapon))
             (setf prop_dplus (object-o_dplus cur_weapon)))
           (setf cp (if (logtest (object-o_flags weap) ISMISL)
                        (object-o_damage weap)
                        (object-o_hurldmg weap)))))
      (t
       (setf cp (object-o_damage weap))
       ;; Drain a staff of striking
       (when (and (eq (object-o_type weap) STICK)
                  (eq (object-o_which weap) WS_HIT)
                  (zerop (object-o_charges weap)))
         (setf (object-o_damage weap) "0d0"
               (object-o_hplus weap) 0
               (object-o_dplus weap) 0))))
    (loop
       (let (damage
             (hplus (+ prop_hplus (if weap (object-o_hplus weap) 0)))
             (dplus (+ prop_dplus (if weap (object-o_dplus weap) 0))))
         (when (eq weap cur_weapon)
           (cond
             ((isring LEFT R_ADDDAM)
              (incf dplus (object-o_ac (aref cur_ring LEFT))))
             ((isring LEFT R_ADDHIT)
              (incf hplus (object-o_ac (aref cur_ring LEFT))))
             ((isring RIGHT R_ADDDAM)
              (incf dplus (object-o_ac (aref cur_ring RIGHT))))
             ((isring RIGHT R_ADDHIT)
              (incf hplus (object-o_ac (aref cur_ring RIGHT))))))
         (multiple-value-bind (ndice nsides) (parse-dice cp)
           (when nsides
             (if (eq def pstats)
                 (progn
                   (setf def_arm
                         (if cur_armor
                             (object-o_ac cur_armor)
                             (stats-s_arm def)))
                   (decf def_arm
                         (cond
                           ((isring LEFT R_PROTECT)
                            (object-o_ac (aref cur_ring LEFT)))
                           ((isring RIGHT R_PROTECT)
                            (object-o_ac (aref cur_ring RIGHT)))
                           (t 0))))
                 (setf def_arm (stats-s_arm def)))
             (when (swing (stats-s_lvl att) def_arm (+ hplus (str_plus (stats-s_str att))))
               (let ((proll (roll ndice nsides)))
                 (when (and (plusp (+ ndice nsides)) (< proll 1))
                   (rogue-debug "Damage for ~dd~d came out ~d." ndice nsides proll))
                 (setf damage (+ dplus proll (add_dam (stats-s_str att))))
                 (decf (stats-s_hpt def) (max 0 damage))
                 (setf did_hit t)))
        
             (let ((slashpos (position #\/ cp)))
               (unless slashpos (return))
               (setf cp (subseq cp (1+ slashpos))))))))
    did_hit))

(defun prname (who upper)
  "The print name of a combatant."
  (funcall
   (if upper #'nstring-capitalize #'identity)
   (concatenate 
    'string
    (cond
      ((null who) "you")
      ((on *player* ISBLIND) "it")
      (t (format nil "the ~a" who))))))

(defun hit (er ee)
  "Print a message to indicate a succesful hit."
  (addmsg (prname er t))
  (addmsg
   (if terse
       " hit."
       (case (rnd 4)
         (0 " scored an excellent hit on ")
         (1 " hit ")
         (2 (if er " has injured " " have injured "))
         (3 (if er " swings and hits " " swing and hit ")))))
  (verbose (addmsg (prname ee nil)))
  (endmsg))

(defun miss (er ee)
  "Print a message to indicate a poor swing."
  (addmsg (prname er t))
  (addmsg
   (case (if terse 0 (rnd 4))
     (0 (if er " misses" " miss"))
     (1 (if er " swings and misses" " swing and miss"))
     (2 (if er " barely misses" " barely miss"))
     (3 (if er " doesn't hit" " don't hit"))))
  (verbose (addmsg " ~a" (prname ee nil)))
  (endmsg))

(defun save_throw (which tp)
  "See if a creature saves against something."
  (let ((need (+ 14 
                 which 
                 (- (truncate 
                     (/ (stats-s_lvl (thing-t_stats tp)) 
                        2))))))
    (>= (roll 1 20) 
        need)))

(defun save (which)
  "See if he saves against various nasty things."
  (save_throw which *player*))

(defun str_plus (str)
  "Compute bonus/penalties for strength on the 'to hit' roll."
  (let ((strstr (str_t-st_str str))
        (add (str_t-st_add str)))
    (when (= 18 strstr)
      (if (= add 100)
          (return-from str_plus 3)
          (when (> add 50) (return-from str_plus 2))))
    (cond 
      ((>= strstr 17) 1)
      ((> strstr 6) 0)
      (t (- strstr 7)))))

(defun add_dam (str)
  "Compute additional damage done for exceptionally high or low strength."
  (let ((strstr (str_t-st_str str))
        (add (str_t-st_add str)))
    (cond
      ((= strstr 18)
       (cond
         ((= add 100) 6)
         ((> add 90) 5)
         ((> add 75) 4)
         ((plusp add) 3)
         (t 2)))
      ((> strstr 15) 1)
      ((> strstr 6) 0)
      (t (- strstr 7)))))

(defun raise_level ()
  "The guy just magically went up a level."
  (setf (stats-s_exp pstats) (1+ (aref e_levels (1- (stats-s_lvl pstats)))))
  (check_level))

(defun thunk (weap mname)
  "A missile hits a monster."
  (if (eq (object-o_type weap) WEAPON)
      (msg "The ~a hits the ~a" (aref w_names (object-o_which weap)) mname)
      (msg "You hit the ~a." mname)))

(defun bounce (weap mname)
  "A missile misses a monster."
  (if (eq (object-o_type weap) WEAPON)
      (msg "The ~a misses the ~a" (aref w_names (object-o_which weap)) mname)
      (msg "You missed the ~a." mname)))

(defun remove_monster (mp item)
  "Remove a monster from the screen."
  (rogue-mvwaddch mw (coord-y mp) (coord-x mp) #\Space)
  (rogue-mvwaddch cw (coord-y mp) (coord-x mp) (thing-t_oldch item))
  (detach mlist item))

(defun is_magic (obj)
  "Returns true if an object radiates magic."
  (case (object-o_type obj)
    (#.ARMOR 
     (not (eq 
           (object-o_ac obj) 
           (aref a_class (object-o_which obj)))))
    (#.WEAPON
     (or (not (zerop (object-o_hplus obj))) 
         (not (zerop (object-o_dplus obj)))))
    ((#.POTION #.SCROLL #.STICK #.RING #.AMULET)
     t)
    (otherwise nil)))

(defun killed (tp pr)
  "Called to put a monster to death."
  (when pr
    (addmsg (if terse "Defeated " "You have defeated "))
    (if (on *player* ISBLIND)
        (msg "it.")
        (progn
          (verbose (addmsg "the "))
          (msg "~a." (monster-m_name (char-monster (thing-t_type tp))))))
    (incf (stats-s_exp pstats) 
          (stats-s_exp (thing-t_stats tp)))
    ;; Do adjustments if he went up a level
    (check_level)
    ;; If the monster was a violet fungi, un-hold him
    (case (thing-t_type tp)
      (#\F
       (logclr! (thing-t_flags *player*) ISHELD)
       (zero! fung_hit)
       (setf (stats-s_dmg (monster-m_stats (char-monster #\F))) "000d0"))
      (#\L
       (when-let (rp (roomin (thing-t_pos tp)))
         (if (or (nonzerop (moor-r_goldval rp)) 
                 (fallpos (thing-t_pos tp) (moor-r_gold rp) nil))
             (progn
               (incf (moor-r_goldval rp) (goldcalc))
               (when (save VS_MAGIC)
                 (incf (moor-r_goldval rp) (+ (goldcalc) (goldcalc) (goldcalc) (goldcalc))))
               (rogue-mvwaddch cl-ncurses:*stdscr* (coord-y (moor-r_gold rp)) (coord-x (moor-r_gold rp)) GOLD)
               (unless (logtest (moor-r_flags rp) ISDARK)
                 (light hero)
                 (rogue-mvwaddch cw hero.y hero.x PLAYER)))))))
    ;; Empty the monster's pack
    (let ((pitem (thing-t_pack tp)))
      ;; Get rid of the monster
      (remove_monster (thing-t_pos tp) tp)
      (map nil
           #'(lambda (obj)
               (setf (object-o_pos obj) (thing-t_pos tp))
               (fall obj nil))
           pitem))))
