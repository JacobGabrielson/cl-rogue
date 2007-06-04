;;;; Contains functions for dealing with things like
;;;; potions and scrolls
;;;; 
;;;; @(#)things.c	3.37 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defun inv_name (obj drop)
  "Return the name of something as it would appear in an
inventory."
  (let ((obj->o_count (object-o_count obj))
        (obj->o_which (object-o_which obj))
        (obj->o_hplus (object-o_hplus obj))
        (obj->o_dplus (object-o_dplus obj))
        (obj->o_type  (object-o_type obj))
        (obj->o_ac  (object-o_ac obj))
        (obj->o_flags (object-o_flags obj))
        (prstring (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (with-output-to-string (prbuf prstring)
      (case obj->o_type
        (#.SCROLL
         (apply #'format prbuf
                (if (onep obj->o_count)
                    '("A scroll ") `("~d scrolls " ,obj->o_count)))
         (cond
           ((aref s_know obj->o_which)
            (format prbuf "of ~a" (magic_item-mi_name (aref s_magic obj->o_which))))
           ((aref s_guess obj->o_which)
            (format prbuf "called ~a" (aref s_guess obj->o_which)))
           (t 
            (format prbuf "titled '~a'" (aref s_names obj->o_which)))))
        (#.POTION
         (apply #'format prbuf
                (if (onep obj->o_count)
                    '("A potion ") `("~d potions " ,obj->o_count)))
         (cond
           ((aref p_know obj->o_which)
            (format prbuf "of ~a(~a)"
                    (magic_item-mi_name (aref p_magic obj->o_which))
                    (aref p_colors obj->o_which)))
           ((aref p_guess obj->o_which)
            (format prbuf "called ~a(~a)" 
                    (aref p_guess obj->o_which)
                    (aref p_colors obj->o_which)))
           ((onep obj->o_count)
            (format prbuf "A~a ~a potion"
                    (vowelstr (aref p_colors obj->o_which))
                    (aref p_colors obj->o_which)))
           (t
            (format prbuf "~d ~a potions" 
                    obj->o_count
                    (aref p_colors obj->o_which)))))
        (#.FOOD
         (cond
           ((onep obj->o_which)
            (apply #'format prbuf
                   (if (onep obj->o_count)
                       `("A~a ~a" ,(vowelstr fruit) ,fruit)
                       `("~d ~as" ,obj->o_count ,fruit))))
           (t
            (apply 
             #'format prbuf
             (if (onep obj->o_count)
                 '("Some food")
                 `("~d rations of food" ,obj->o_count))))))
        (#.WEAPON
         (if (>1 obj->o_count)
             (format prbuf "~d " obj->o_count)
             (format prbuf "A "))
         (if (logtest obj->o_flags isknow)
             (format prbuf 
                     "~a ~a" 
                     (num obj->o_hplus obj->o_dplus)
                     (aref w_names obj->o_which))
             (format prbuf "~a" (aref w_names obj->o_which)))
         (if (>1 obj->o_count)
             (format prbuf "s")))
        (#.ARMOR
         (if (logtest obj->o_flags isknow)
             (format prbuf "~a ~a"
                     (num (- (aref a_class obj->o_which) obj->o_ac) 0)
                     (aref a_names obj->o_which))
             (format prbuf "~a" (aref a_names obj->o_which))))
        (#.AMULET
         (format prbuf "The Amulet of Yendor"))
        (#.STICK
         (format prbuf "A ~a " (aref ws_type obj->o_which))
         (cond
           ((aref ws_know obj->o_which)
            (format prbuf "of ~a~a(~a)" 
                    (magic_item-mi_name (aref ws_magic obj->o_which))
                    (charge_str obj)
                    (aref ws_made obj->o_which)))
           ((aref ws_guess obj->o_which)
            (format prbuf "called ~a(~a)" 
                    (aref ws_guess obj->o_which)
                    (aref ws_made obj->o_which)))
           (t
            (format prbuf "~a ~a" 
                    (aref ws_made obj->o_which)
                    (aref ws_type obj->o_which)))))
        (#.RING
         (cond
           ((aref r_know obj->o_which)
            (format prbuf "A~a ring of ~a(~a)" 
                    (ring_num obj)
                    (magic_item-mi_name (aref r_magic obj->o_which))
                    (aref r_stones obj->o_which)))
           ((aref r_guess obj->o_which)
            (format prbuf "A ring called ~a(~a)"
                    (aref r_guess obj->o_which)
                    (aref r_stones obj->o_which)))
           (t
            (format prbuf "A~a ~a ring" 
                    (vowelstr (aref r_stones obj->o_which))
                    (aref r_stones obj->o_which)))))
        (otherwise
         (rogue-debug "Picked up something funny")
         (format prbuf "Something bizarre ~a" (unctrl-char obj->o_type))))
      (when (eql obj cur_armor)
        (format prbuf " (being worn)"))
      (when (eql obj cur_weapon)
        (format prbuf " (weapon in hand)"))
      (if (eql obj (aref cur_ring LEFT))
          (format prbuf " (on left hand)")
          (when (eql obj (aref cur_ring RIGHT))
            (format prbuf " (on right hand)")))
      (if (and drop
               (upper-case-p (aref prstring 0)))
          (setf (aref prstring 0)
                (char-downcase (aref prstring 0)))
          (when (and (not drop)
                     (lower-case-p (aref prstring 0)))
            (setf (aref prstring 0)
                  (char-upcase  (aref prstring 0)))))
      (unless drop
        (format prbuf ".")))
    prstring))

(defun money ()
  "Add to characters purse."
  (map nil
       #'(lambda (rp)
           (when (equalp hero (moor-r_gold rp))
             (when notify
               (verbose "You found ")
               (msg "~d gold pieces." (moor-r_goldval rp)))
             (incf purse (moor-r_goldval rp))
             (zero! (moor-r_goldval rp))
             (cmov (moor-r_gold rp))
             (rogue-addch FLOOR)
             (return-from money)))
       rooms)
  (msg "That gold must have been counterfeit"))

(defun drop ()
  "Put something down."
  (let ((ch (rogue-mvwinch cl-ncurses:*stdscr* hero.y hero.x)))
    (when (and (not (eql ch FLOOR))
               (not (eql ch PASSAGE)))
      (msg "There is something there already")
      (return-from drop))
    (when-let (op (get_item "drop" nil))
      (unless (dropcheck op)
        (return-from drop))
      ;; Take it out of the pack
      (if (and (>1 (object-o_count op))
               (not (eql (object-o_type op) WEAPON)))
          (progn
            (let ((nobj (copy-structure op)))
              (decf (object-o_count op))
              (setf (object-o_count nobj) 1)
              (unless (zerop (object-o_group nobj))
                (incf inpack))
              (setf op nobj)))          ; so attached to lvl below
          (detach pack op))
      (decf inpack)
      ;; Link it into the level object list
      (attach lvl_obj op)
      (rogue-mvaddch hero.y hero.x (object-o_type op))
      (setf (object-o_pos op) hero)
      (msg "Dropped ~a" (inv_name op t)))))

(defun dropcheck (op)
  "Do special checks for dropping or unweilding|unwearing|unringing."
  (unless op
    (return-from dropcheck t))
  (when (and (not (eql op cur_armor))
             (not (eql op cur_weapon))
             (not (eql op (aref cur_ring LEFT)))
             (not (eql op (aref cur_ring RIGHT))))
    (return-from dropcheck t))
  (when (logtest (object-o_flags op) ISCURSED)
    (msg "You can't.  It appears to be cursed.")
    (return-from dropcheck nil))
  (cond
    ((eql op cur_weapon)
     (setf cur_weapon nil))
    ((eql op cur_armor)
     (waste_time)
     (setf cur_armor nil))
    ((or
      (eql op (aref cur_ring LEFT))
      (eql op (aref cur_ring RIGHT)))
     (case (object-o_which op)
       (#.R_ADDSTR
        (let ((save_max (stats-s_str max_stats)))
          (chg_str (- (object-o_ac op)))
          (setf (stats-s_str max_stats) save_max)))
       (#.R_SEEINVIS
        (logclr! (thing-t_flags *player*) CANSEE)
        (extinguish #'unsee)
        (light hero)
        (rogue-mvwaddch cw hero.y hero.x PLAYER)))
     (setf (aref cur_ring 
                 (if (eql op (aref cur_ring LEFT))
                     LEFT RIGHT))
           nil)))
  t)

(defun new_thing ()
  "Return a new thing."
  (let ((cur (make-object)))
    (setf
     (object-o_hplus cur) 0
     (object-o_dplus cur) 0
     (object-o_damage cur) "0d0"
     (object-o_hurldmg cur) "0d0"
     (object-o_ac cur) 11
     (object-o_count cur) 1
     (object-o_group cur) 0
     (object-o_flags cur) 0)
    ;; Decide what kind of object it will be
    ;; If we haven't had food for a while let it be food.
    (case (if (> no_food 3) 
              2 
              (pick_one things NUMTHINGS))
      (0
       (setf (object-o_type cur) POTION
             (object-o_which cur) (pick_one p_magic MAXPOTIONS)))
      (1
       (setf (object-o_type cur) SCROLL
             (object-o_which cur) (pick_one s_magic MAXSCROLLS)))
      (2
       (setf no_food 0
             (object-o_type cur) FOOD
             (object-o_which cur) (if (> (rnd 100) 10) 
                                      0 1)))
      (3
       (setf (object-o_type cur) WEAPON
             (object-o_which cur) (rnd MAXWEAPONS))
       (init_weapon cur (object-o_which cur))
       (let ((k (rnd 100)))
         (if (< k 10)
             (progn
               (logior! (object-o_flags cur) ISCURSED)
               (decf (object-o_hplus cur) (1+ (rnd 3))))
             (when (< k 15)
               (incf (object-o_hplus cur) (1+ (rnd 3)))))))
      (4
       (setf (object-o_type cur) ARMOR)
       (let ((j (dotimes (i MAXARMORS i)
                  (when (< (rnd 100)
                           (aref a_chances i))
                    (return i)))))
         (when (>= j MAXARMORS)
           (progn
             (rogue-debug "Picked a bad armor ~d" j)
             (zero! j)))
         (setf (object-o_which cur) j
               (object-o_ac cur) (aref a_class j))
         (let ((k (rnd 100)))
           (if (< k 20)
               (progn
                 (logior! (object-o_flags cur) ISCURSED)
                 (incf (object-o_ac cur) (1+ (rnd 3))))
               (when (< k 28)
                 (decf (object-o_ac cur) (1+ (rnd 3))))))))
      (5
       (setf (object-o_type cur) RING
             (object-o_which cur) (pick_one r_magic MAXRINGS))
       (case (object-o_which cur)
         ((#.R_ADDSTR
           #.R_PROTECT
           #.R_ADDHIT
           #.R_ADDDAM)
          (setf (object-o_ac cur) (rnd 3))
          (when (zerop (object-o_ac cur))
            (setf (object-o_ac cur) -1)
            (logior! (object-o_flags cur) ISCURSED)))
         ((#.R_AGGR
           #.R_TELEPORT)
          (setf (object-o_flags cur) ISCURSED))))
      (6
       (setf (object-o_type cur) STICK
             (object-o_which cur) (pick_one ws_magic MAXSTICKS))
       (fix_stick cur))
      (otherwise
       (rogue-debug "Picked a bad kind of object")
       (wait_for #\Space)))
    cur))

(defun pick_one (magick nitems)
  "Pick an item out of a list of nitems possible magic items."
  (let ((end (length magick))
        (prob (rnd 100))
        (i 0))
    (dotimes (_ end)
      (if (< prob 
             (magic_item-mi_prob (aref magick i)))
          (return)
          (incf i)))
    (when (= i end)
      (when wizard
        (msg "bad pick_one: ~d from ~d items" i nitems)
        (dotimes (j end)
          (msg "~a: ~d%" 
               (magic_item-mi_name (aref magick j))
               (magic_item-mi_name (aref magick j))))
        (zero! i)))
    i))
