;;;; Contains functions for dealing with things like
;;;; potions and scrolls
;;;; 
;;;; @(#)things.c	3.37 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defun inv-name (obj drop)
  "Return the name of something as it would appear in an
inventory."
  (let ((obj->o-count (object-o-count obj))
        (obj->o-which (object-o-which obj))
        (obj->o-hplus (object-o-hplus obj))
        (obj->o-dplus (object-o-dplus obj))
        (obj->o-type  (object-o-type obj))
        (obj->o-ac  (object-o-ac obj))
        (obj->o-flags (object-o-flags obj))
        (prstring (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (with-output-to-string (prbuf prstring)
      (case obj->o-type
        (#.SCROLL
         (apply #'format prbuf
                (if (onep obj->o-count)
                    '("A scroll ") `("~d scrolls " ,obj->o-count)))
         (cond
           ((aref s-know obj->o-which)
            (format prbuf "of ~a" (magic-item-mi-name (aref s-magic obj->o-which))))
           ((aref s-guess obj->o-which)
            (format prbuf "called ~a" (aref s-guess obj->o-which)))
           (t 
            (format prbuf "titled '~a'" (aref s-names obj->o-which)))))
        (#.POTION
         (apply #'format prbuf
                (if (onep obj->o-count)
                    '("A potion ") `("~d potions " ,obj->o-count)))
         (cond
           ((aref p-know obj->o-which)
            (format prbuf "of ~a(~a)"
                    (magic-item-mi-name (aref p-magic obj->o-which))
                    (aref p-colors obj->o-which)))
           ((aref p-guess obj->o-which)
            (format prbuf "called ~a(~a)" 
                    (aref p-guess obj->o-which)
                    (aref p-colors obj->o-which)))
           ((onep obj->o-count)
            (format prbuf "A~a ~a potion"
                    (vowelstr (aref p-colors obj->o-which))
                    (aref p-colors obj->o-which)))
           (t
            (format prbuf "~d ~a potions" 
                    obj->o-count
                    (aref p-colors obj->o-which)))))
        (#.FOOD
         (cond
           ((onep obj->o-which)
            (apply #'format prbuf
                   (if (onep obj->o-count)
                       `("A~a ~a" ,(vowelstr fruit) ,fruit)
                       `("~d ~as" ,obj->o-count ,fruit))))
           (t
            (apply 
             #'format prbuf
             (if (onep obj->o-count)
                 '("Some food")
                 `("~d rations of food" ,obj->o-count))))))
        (#.WEAPON
         (if (>1 obj->o-count)
             (format prbuf "~d " obj->o-count)
             (format prbuf "A "))
         (if (logtest obj->o-flags isknow)
             (format prbuf 
                     "~a ~a" 
                     (num obj->o-hplus obj->o-dplus)
                     (aref w-names obj->o-which))
             (format prbuf "~a" (aref w-names obj->o-which)))
         (if (>1 obj->o-count)
             (format prbuf "s")))
        (#.ARMOR
         (if (logtest obj->o-flags isknow)
             (format prbuf "~a ~a"
                     (num (- (aref a-class obj->o-which) obj->o-ac) 0)
                     (aref a-names obj->o-which))
             (format prbuf "~a" (aref a-names obj->o-which))))
        (#.AMULET
         (format prbuf "The Amulet of Yendor"))
        (#.STICK
         (format prbuf "A ~a " (aref ws-type obj->o-which))
         (cond
           ((aref ws-know obj->o-which)
            (format prbuf "of ~a~a(~a)" 
                    (magic-item-mi-name (aref ws-magic obj->o-which))
                    (charge-str obj)
                    (aref ws-made obj->o-which)))
           ((aref ws-guess obj->o-which)
            (format prbuf "called ~a(~a)" 
                    (aref ws-guess obj->o-which)
                    (aref ws-made obj->o-which)))
           (t
            (format prbuf "~a ~a" 
                    (aref ws-made obj->o-which)
                    (aref ws-type obj->o-which)))))
        (#.RING
         (cond
           ((aref r-know obj->o-which)
            (format prbuf "A~a ring of ~a(~a)" 
                    (ring-num obj)
                    (magic-item-mi-name (aref r-magic obj->o-which))
                    (aref r-stones obj->o-which)))
           ((aref r-guess obj->o-which)
            (format prbuf "A ring called ~a(~a)"
                    (aref r-guess obj->o-which)
                    (aref r-stones obj->o-which)))
           (t
            (format prbuf "A~a ~a ring" 
                    (vowelstr (aref r-stones obj->o-which))
                    (aref r-stones obj->o-which)))))
        (otherwise
         (rogue-debug "Picked up something funny")
         (format prbuf "Something bizarre ~a" (unctrl-char obj->o-type))))
      (when (eql obj cur-armor)
        (format prbuf " (being worn)"))
      (when (eql obj cur-weapon)
        (format prbuf " (weapon in hand)"))
      (if (eql obj (aref cur-ring LEFT))
          (format prbuf " (on left hand)")
          (when (eql obj (aref cur-ring RIGHT))
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
           (when (equalp hero (moor-r-gold rp))
             (when notify
               (verbose "You found ")
               (msg "~d gold pieces." (moor-r-goldval rp)))
             (incf purse (moor-r-goldval rp))
             (zero! (moor-r-goldval rp))
             (cmov (moor-r-gold rp))
             (rogue-addch THE-FLOOR)
             (return-from money)))
       rooms)
  (msg "That gold must have been counterfeit"))

(defun drop ()
  "Put something down."
  (let ((ch (rogue-mvwinch cl-ncurses:*stdscr* hero.y hero.x)))
    (when (and (not (eql ch THE-FLOOR))
               (not (eql ch PASSAGE)))
      (msg "There is something there already")
      (return-from drop))
    (when-let (op (get-item "drop" nil))
      (unless (dropcheck op)
        (return-from drop))
      ;; Take it out of the pack
      (if (and (>1 (object-o-count op))
               (not (eql (object-o-type op) WEAPON)))
          (progn
            (let ((nobj (copy-structure op)))
              (decf (object-o-count op))
              (setf (object-o-count nobj) 1)
              (unless (zerop (object-o-group nobj))
                (incf inpack))
              (setf op nobj)))          ; so attached to lvl below
          (detach pack op))
      (decf inpack)
      ;; Link it into the level object list
      (attach lvl-obj op)
      (rogue-mvaddch hero.y hero.x (object-o-type op))
      (setf (object-o-pos op) hero)
      (msg "Dropped ~a" (inv-name op t)))))

(defun dropcheck (op)
  "Do special checks for dropping or unweilding|unwearing|unringing."
  (unless op
    (return-from dropcheck t))
  (when (and (not (eql op cur-armor))
             (not (eql op cur-weapon))
             (not (eql op (aref cur-ring LEFT)))
             (not (eql op (aref cur-ring RIGHT))))
    (return-from dropcheck t))
  (when (logtest (object-o-flags op) ISCURSED)
    (msg "You can't.  It appears to be cursed.")
    (return-from dropcheck nil))
  (cond
    ((eql op cur-weapon)
     (setf cur-weapon nil))
    ((eql op cur-armor)
     (waste-time)
     (setf cur-armor nil))
    ((or
      (eql op (aref cur-ring LEFT))
      (eql op (aref cur-ring RIGHT)))
     (case (object-o-which op)
       (#.R-ADDSTR
        (let ((save-max (stats-s-str max-stats)))
          (chg-str (- (object-o-ac op)))
          (setf (stats-s-str max-stats) save-max)))
       (#.R-SEEINVIS
        (logclr! (thing-t-flags *player*) CANSEE)
        (extinguish #'unsee)
        (light hero)
        (rogue-mvwaddch cw hero.y hero.x PLAYER)))
     (setf (aref cur-ring 
                 (if (eql op (aref cur-ring LEFT))
                     LEFT RIGHT))
           nil)))
  t)

(defun new-thing ()
  "Return a new thing."
  (let ((cur (make-object)))
    (setf
     (object-o-hplus cur) 0
     (object-o-dplus cur) 0
     (object-o-damage cur) "0d0"
     (object-o-hurldmg cur) "0d0"
     (object-o-ac cur) 11
     (object-o-count cur) 1
     (object-o-group cur) 0
     (object-o-flags cur) 0)
    ;; Decide what kind of object it will be
    ;; If we haven't had food for a while let it be food.
    (case (if (> no-food 3) 
              2 
              (pick-one things NUMTHINGS))
      (0
       (setf (object-o-type cur) POTION
             (object-o-which cur) (pick-one p-magic MAXPOTIONS)))
      (1
       (setf (object-o-type cur) SCROLL
             (object-o-which cur) (pick-one s-magic MAXSCROLLS)))
      (2
       (setf no-food 0
             (object-o-type cur) FOOD
             (object-o-which cur) (if (> (rnd 100) 10) 
                                      0 1)))
      (3
       (setf (object-o-type cur) WEAPON
             (object-o-which cur) (rnd MAXWEAPONS))
       (init-weapon cur (object-o-which cur))
       (let ((k (rnd 100)))
         (if (< k 10)
             (progn
               (logior! (object-o-flags cur) ISCURSED)
               (decf (object-o-hplus cur) (1+ (rnd 3))))
             (when (< k 15)
               (incf (object-o-hplus cur) (1+ (rnd 3)))))))
      (4
       (setf (object-o-type cur) ARMOR)
       (let ((j (dotimes (i MAXARMORS i)
                  (when (< (rnd 100)
                           (aref a-chances i))
                    (return i)))))
         (when (>= j MAXARMORS)
           (progn
             (rogue-debug "Picked a bad armor ~d" j)
             (zero! j)))
         (setf (object-o-which cur) j
               (object-o-ac cur) (aref a-class j))
         (let ((k (rnd 100)))
           (if (< k 20)
               (progn
                 (logior! (object-o-flags cur) ISCURSED)
                 (incf (object-o-ac cur) (1+ (rnd 3))))
               (when (< k 28)
                 (decf (object-o-ac cur) (1+ (rnd 3))))))))
      (5
       (setf (object-o-type cur) RING
             (object-o-which cur) (pick-one r-magic MAXRINGS))
       (case (object-o-which cur)
         ((#.R-ADDSTR
           #.R-PROTECT
           #.R-ADDHIT
           #.R-ADDDAM)
          (setf (object-o-ac cur) (rnd 3))
          (when (zerop (object-o-ac cur))
            (setf (object-o-ac cur) -1)
            (logior! (object-o-flags cur) ISCURSED)))
         ((#.R-AGGR
           #.R-TELEPORT)
          (setf (object-o-flags cur) ISCURSED))))
      (6
       (setf (object-o-type cur) STICK
             (object-o-which cur) (pick-one ws-magic MAXSTICKS))
       (fix-stick cur))
      (otherwise
       (rogue-debug "Picked a bad kind of object")
       (wait-for #\Space)))
    cur))

(defun pick-one (magick nitems)
  "Pick an item out of a list of nitems possible magic items."
  (let ((end (length magick))
        (prob (rnd 100))
        (i 0))
    (dotimes (_ end)
      (if (< prob 
             (magic-item-mi-prob (aref magick i)))
          (return)
          (incf i)))
    (when (= i end)
      (when wizard
        (msg "bad pick-one: ~d from ~d items" i nitems)
        (dotimes (j end)
          (msg "~a: ~d%" 
               (magic-item-mi-name (aref magick j))
               (magic-item-mi-name (aref magick j))))
        (zero! i)))
    i))
