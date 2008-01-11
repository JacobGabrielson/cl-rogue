;;;; All the fighting gets done here
;;;; @(#)fight.c	3.28 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defparameter e-levels 
  #(10 20 40 80 160 320 640 1280 2560 5120 10240 20480
    40920 81920 163840 327680 655360 1310720 2621440 0))

(defun fight (mp mn weap thrown)
  "The player attacks the monster."
  (let ((did-hit t)
        (tp (find-mons (coord-y mp) (coord-x mp))))
    ;; Find the monster we want to fight
    (unless tp 
      (rogue-debug "Fight what @ ~d,~d" (coord-y mp) (coord-x mp)))
    ;; Since we are fighting, things are not quiet so no healing takes
    ;; place.
    (setf quiet 0)
    (runto mp hero)
    ;; Let him know it was really a mimic (if it was one).
    (when (and (eql (thing-t-type tp) #\M)
               (not (eql (thing-t-disguise tp) #\M))
               (off *player* ISBLIND))
      (msg "Wait! That's a mimic!")
      (setf (thing-t-disguise tp) #\M)  ;
      (setf did-hit thrown))
    (when did-hit
      (let ((mname (if (on *player* ISBLIND) "it" (monster-m-name (char-monster mn)))))
        (setf did-hit nil)
        (if (roll-em pstats (thing-t-stats tp) weap thrown)
            (progn
              (setf did-hit t)
              (if thrown
                  (thunk weap mname)
                  (hit nil mname))
              (when (on *player* CANHUH)
                (msg "Your hands stop glowing red")
                (msg "The ~a appears confused." mname)
                (logior! (thing-t-flags tp) ISHUH)
                (logclr! (thing-t-flags *player*) CANHUH))
              (when (<= (stats-s-hpt (thing-t-stats tp)) 0)
                (killed tp t)))
            (if thrown
                (bounce weap mname)
                (miss nil mname)))))
    (setf *count* 0)
    did-hit))

(defun attack (mp)
  "The monster attacks the player."
  ;; Since this is an attack, stop running and any healing that was
  ;; going on at the time.
  (setf running nil)
  (setf quiet 0)

  (when (and (eql (thing-t-type mp) #\M)
             (off *player* ISBLIND))
    (setf (thing-t-disguise mp) #\M))

  (let ((mname (if (on *player* ISBLIND) 
                   "it" 
                   (monster-m-name (char-monster (thing-t-type mp)))))
        (mtype (thing-t-type mp))
        (mstats (thing-t-stats mp)))
    (if (roll-em mstats pstats nil nil)
        (progn
          (unless (eql mtype #\E)
            (hit mname nil))
          (when (<= (stats-s-hpt pstats) 0)
            (death mtype))              ; Bye bye life ... 
          (when (off mp ISCANC)
            (case mtype
              (#\R
               ;;
               ;; If a rust monster hits, you lose armor
               ;;
               (when (and cur-armor (< (object-o-ac cur-armor) 9))
                 (if terse
                     (msg "Your armor weakens")
                     (msg "Your armor appears to be weaker now. Oh my!"))
                 (incf (object-o-ac cur-armor))))
              (#\E
               ;; The gaze of the floating eye hypnotizes you
               (unless (on *player* ISBLIND)
                 (unless (nonzerop no-command)
                   (addmsg "You are transfixed")
                   (unless terse
                     (addmsg " by the gaze of the floating eye."))
                   (endmsg))
                 (incf no-command 
                       (+ (rnd 2) 2))))
              (#\A
               ;;
               ;; Ants have poisonous bites
               ;;
               (unless (save VS-POISON)
                 (if (not (iswearing R-SUSTSTR))
                     (progn
                       (chg-str -1)
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
                 (when (zerop (decf (stats-s-exp pstats)))
                   (death #\W))         ; All levels gone 
                 (msg "You suddenly feel weaker.")
                 (if (zerop (decf (stats-s-lvl pstats)))
                     (setf (stats-s-exp pstats) 0
                           (stats-s-lvl pstats) 1)
                     (setf (stats-s-exp pstats) (1+ (aref e-levels
                                                          (1- (stats-s-lvl pstats))))))
                 (let ((fewer (roll 1 10)))
                   (decf (stats-s-hpt pstats) fewer)
                   (decf max-hp fewer))
                 (when (< (stats-s-hpt pstats) 1)
                   (setf (stats-s-hpt pstats) 1))
                 (when (< max-hp 1)
                   (death #\W))))
              (#\F
               ;; Violet fungi stops the poor guy from moving
               (logior! (thing-t-flags *player*) ISHELD)
               (setf (stats-s-dmg (monster-m-stats (char-monster #\F)))
                     (format nil "~dd1" (incf fung-hit))))
              (#\L
               ;; Leperachaun steals some gold
               (let ((lastpurse purse))
                 (decf purse (goldcalc))
                 (unless (save VS-MAGIC)
                   (decf purse (+ (goldcalc) (goldcalc) (goldcalc) (goldcalc))))
                 (when (< purse 0)
                   (setf purse 0))
                 (unless (= purse lastpurse)
                   (msg "Your purse feels lighter"))
                 (remove-monster (thing-t-pos mp)
                                 (find-mons (coord-y (thing-t-pos mp)) 
                                            (coord-x (thing-t-pos mp)))))
               (setf mp nil))
              (#\N
               ;; Nymphs steal a magic item, look through the pack
               ;; and pick out one we like.
               (let (obj
                     (nobj 0))
                 (map nil
                      #'(lambda (o)
                          (when (and (not (eql o cur-armor))
                                     (not (eql o cur-weapon))
                                     (is-magic o)
                                     (zerop (rnd (incf nobj))))
                            (setf obj o)))
                      pack)
                 (when obj
                   (remove-monster (thing-t-pos mp)
                                   (find-mons (coord-y (thing-t-pos mp))
                                              (coord-x (thing-t-pos mp))))
                   (setf mp nil)
                   (if (and (> (object-o-count obj) 1)
                            (zerop (object-o-group obj)))
                       (progn 
                         (let ((oc (1- (object-o-count obj))))
                           (setf (object-o-count obj) 1)
                           (msg "She stole ~a!" (inv-name obj t))
                           (setf (object-o-count obj) oc)))
                       (progn
                         (msg "She stole ~a!" (inv-name obj t))
                         (detach pack obj)))
                   (decf inpack)))))))
        ;; else of roll-em clause
        (unless (eql (thing-t-type mp) #\E)
          (when (eql (thing-t-type mp) #\F)
            (decf (stats-s-hpt pstats) fung-hit)
            (when (<= (stats-s-hpt pstats) 0)
              (death #\F)))             ; Bye bye life ... 
          (miss mname nil)))
    ;; Check to see if this is a regenerating monster and let it heal if
    ;; it is.
    (when (and mp (on mp ISREGEN) (< (rnd 100) 33))
      (incf (stats-s-hpt (thing-t-stats mp))))
    (when fight-flush
      (flush-type))                     ; flush typeahead 
    (setf *count* 0)
    (status)

    (if mp 0 -1)))

(defun swing (at-lvl op-arm wplus)
  "Returns true if the swing hits."
  (>= (+ (1+ (rnd 20))
         wplus)
      (- (- 21 at-lvl)
         op-arm)))

(defun check-level ()
  "Check to see if the guy has gone up a level."
  (when-let (i (position-if #'(lambda (l)
                                (> l (stats-s-exp pstats)))
                            e-levels))
    (incf i)
    (when (> i (stats-s-lvl pstats))
      (let ((add (roll (- i (stats-s-lvl pstats)) 10)))
        (incf max-hp add)
        (when (> (incf (stats-s-hpt pstats) add) max-hp)
          (setf (stats-s-hpt pstats) max-hp))
        (msg "Welcome to level ~d" i)))
    (setf (stats-s-lvl pstats) i)))

(defun parse-dice (dice)
  (values
   (parse-integer dice :junk-allowed t)
   (when-let (d (position #\d dice))
     (parse-integer dice :start (1+ d) :junk-allowed t))))

(defun roll-em (att def weap hurl)
  "Roll several attacks."
  (let (cp 
        def-arm
        did-hit
        (prop-hplus 0)
        (prop-dplus 0))
    (cond
      ((null weap) 
       (setf cp (stats-s-dmg att)))
      (hurl
       (if (and (logtest (object-o-flags weap) ISMISL)
                cur-weapon
                (eql (object-o-which cur-weapon) (object-o-launch weap)))
           (progn
             (setf cp (object-o-hurldmg weap))
             (setf prop-hplus (object-o-hplus cur-weapon))
             (setf prop-dplus (object-o-dplus cur-weapon)))
           (setf cp (if (logtest (object-o-flags weap) ISMISL)
                        (object-o-damage weap)
                        (object-o-hurldmg weap)))))
      (t
       (setf cp (object-o-damage weap))
       ;; Drain a staff of striking
       (when (and (eql (object-o-type weap) STICK)
                  (eql (object-o-which weap) WS-HIT)
                  (zerop (object-o-charges weap)))
         (setf (object-o-damage weap) "0d0"
               (object-o-hplus weap) 0
               (object-o-dplus weap) 0))))
    (loop
       (let (damage
             (hplus (+ prop-hplus (if weap (object-o-hplus weap) 0)))
             (dplus (+ prop-dplus (if weap (object-o-dplus weap) 0))))
         (when (eql weap cur-weapon)
           (cond
             ((isring LEFT R-ADDDAM)
              (incf dplus (object-o-ac (aref cur-ring LEFT))))
             ((isring LEFT R-ADDHIT)
              (incf hplus (object-o-ac (aref cur-ring LEFT))))
             ((isring RIGHT R-ADDDAM)
              (incf dplus (object-o-ac (aref cur-ring RIGHT))))
             ((isring RIGHT R-ADDHIT)
              (incf hplus (object-o-ac (aref cur-ring RIGHT))))))
         (multiple-value-bind (ndice nsides) (parse-dice cp)
           (when nsides
             (if (eql def pstats)
                 (progn
                   (setf def-arm
                         (if cur-armor
                             (object-o-ac cur-armor)
                             (stats-s-arm def)))
                   (decf def-arm
                         (cond
                           ((isring LEFT R-PROTECT)
                            (object-o-ac (aref cur-ring LEFT)))
                           ((isring RIGHT R-PROTECT)
                            (object-o-ac (aref cur-ring RIGHT)))
                           (t 0))))
                 (setf def-arm (stats-s-arm def)))
             (when (swing (stats-s-lvl att) def-arm (+ hplus (str-plus (stats-s-str att))))
               (let ((proll (roll ndice nsides)))
                 (when (and (plusp (+ ndice nsides)) (< proll 1))
                   (rogue-debug "Damage for ~dd~d came out ~d." ndice nsides proll))
                 (setf damage (+ dplus proll (add-dam (stats-s-str att))))
                 (decf (stats-s-hpt def) (max 0 damage))
                 (setf did-hit t)))
        
             (let ((slashpos (position #\/ cp)))
               (unless slashpos (return))
               (setf cp (subseq cp (1+ slashpos))))))))
    did-hit))

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

(defun save-throw (which tp)
  "See if a creature saves against something."
  (let ((need (+ 14 
                 which 
                 (- (truncate 
                     (/ (stats-s-lvl (thing-t-stats tp)) 
                        2))))))
    (>= (roll 1 20) 
        need)))

(defun save (which)
  "See if he saves against various nasty things."
  (save-throw which *player*))

(defun str-plus (str)
  "Compute bonus/penalties for strength on the 'to hit' roll."
  (let ((strstr (str-t-st-str str))
        (add (str-t-st-add str)))
    (when (= 18 strstr)
      (if (= add 100)
          (return-from str-plus 3)
          (when (> add 50) (return-from str-plus 2))))
    (cond 
      ((>= strstr 17) 1)
      ((> strstr 6) 0)
      (t (- strstr 7)))))

(defun add-dam (str)
  "Compute additional damage done for exceptionally high or low strength."
  (let ((strstr (str-t-st-str str))
        (add (str-t-st-add str)))
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

(defun raise-level ()
  "The guy just magically went up a level."
  (setf (stats-s-exp pstats) (1+ (aref e-levels (1- (stats-s-lvl pstats)))))
  (check-level))

(defun thunk (weap mname)
  "A missile hits a monster."
  (if (eql (object-o-type weap) WEAPON)
      (msg "The ~a hits the ~a" (aref w-names (object-o-which weap)) mname)
      (msg "You hit the ~a." mname)))

(defun bounce (weap mname)
  "A missile misses a monster."
  (if (eql (object-o-type weap) WEAPON)
      (msg "The ~a misses the ~a" (aref w-names (object-o-which weap)) mname)
      (msg "You missed the ~a." mname)))

(defun remove-monster (mp item)
  "Remove a monster from the screen."
  (rogue-mvwaddch mw (coord-y mp) (coord-x mp) #\Space)
  (rogue-mvwaddch cw (coord-y mp) (coord-x mp) (thing-t-oldch item))
  (detach mlist item))

(defun is-magic (obj)
  "Returns true if an object radiates magic."
  (case (object-o-type obj)
    (#.ARMOR 
     (not (eql
           (object-o-ac obj) 
           (aref a-class (object-o-which obj)))))
    (#.WEAPON
     (or (not (zerop (object-o-hplus obj))) 
         (not (zerop (object-o-dplus obj)))))
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
          (msg "~a." (monster-m-name (char-monster (thing-t-type tp))))))
    (incf (stats-s-exp pstats) 
          (stats-s-exp (thing-t-stats tp)))
    ;; Do adjustments if he went up a level
    (check-level)
    ;; If the monster was a violet fungi, un-hold him
    (case (thing-t-type tp)
      (#\F
       (logclr! (thing-t-flags *player*) ISHELD)
       (zero! fung-hit)
       (setf (stats-s-dmg (monster-m-stats (char-monster #\F))) "000d0"))
      (#\L
       (when-let (rp (roomin (thing-t-pos tp)))
         (if (or (nonzerop (moor-r-goldval rp)) 
                 (fallpos (thing-t-pos tp) (moor-r-gold rp) nil))
             (progn
               (incf (moor-r-goldval rp) (goldcalc))
               (when (save VS-MAGIC)
                 (incf (moor-r-goldval rp) (+ (goldcalc) (goldcalc) (goldcalc) (goldcalc))))
               (rogue-mvwaddch cl-ncurses:*stdscr* (coord-y (moor-r-gold rp)) (coord-x (moor-r-gold rp)) GOLD)
               (unless (logtest (moor-r-flags rp) ISDARK)
                 (light hero)
                 (rogue-mvwaddch cw hero.y hero.x PLAYER)))))))
    ;; Empty the monster's pack
    (let ((pitem (thing-t-pack tp)))
      ;; Get rid of the monster
      (remove-monster (thing-t-pos tp) tp)
      (map nil
           #'(lambda (obj)
               (setf (object-o-pos obj) (thing-t-pos tp))
               (fall obj nil))
           pitem))))
