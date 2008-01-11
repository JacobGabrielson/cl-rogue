;;;; Rogue definitions and variable declarations
;;;; @(#)rogue.h	3.38 (Berkeley) 6/15/81

(in-package :cl-rogue)

;;; Maximum number of different things
(defconstant MAXROOMS 9)
(defconstant MAXTHINGS 9)
(defconstant MAXOBJ 9)
(defconstant MAXPACK 23)
(defconstant MAXTRAPS 10)
(defconstant NUMTHINGS 7) ; number of types of things (scrolls, rings, etc.)

;;; Return values for get functions
(defconstant NORM 0)                    ; normal exit 
(defconstant QUIT 1)                    ; quit option setting 
(defconstant MINUS 2)                   ; back up one option 

;;; Things that appear on the screens
(defconstant PASSAGE #\#)
(defconstant DOOR #\+)
(defconstant FLOOR #\.)
(defconstant PLAYER #\@)
(defconstant TRAP #\^)
(defconstant TRAPDOOR #\>)
(defconstant ARROWTRAP #\{)
(defconstant SLEEPTRAP #\$)
(defconstant BEARTRAP #\})
(defconstant TELTRAP #\~)
(defconstant DARTTRAP #\`)
(defconstant SECRETDOOR #\&)
(defconstant STAIRS #\%)
(defconstant GOLD #\*)
(defconstant POTION #\!)
(defconstant SCROLL #\?)
(defconstant MAGIC #\$)
(defconstant FOOD #\:)
(defconstant WEAPON #\))
(defconstant ARMOR #\])
(defconstant AMULET #\,)
(defconstant RING #\=)
(defconstant STICK #\/)
(defconstant CALLABLE #\-)

;;; Various constants
(defparameter PASSWD "mTwA0qqnrlXTw")
(defconstant BEARTIME 3)
(defconstant SLEEPTIME 5)
(defconstant HEALTIME 30)
(defconstant HOLDTIME 2)
(defconstant STPOS 0)
(defconstant WANDERTIME 70)
(defconstant BEFORE 1)
(defconstant AFTER 2)
(defconstant HUHDURATION 20)
(defconstant SEEDURATION 850)
(defconstant HUNGERTIME 1300)
(defconstant MORETIME 150)
(defconstant STOMACHSIZE 2000)
(defconstant ESCAPE 27)
(defconstant LEFT 0)
(defconstant RIGHT 1)
(defconstant BOLT-LENGTH 6)

;;; Save against things
(defconstant VS-POISON #o00)
(defconstant VS-PARALYZATION #o00)
(defconstant VS-DEATH #o00)
(defconstant VS-PETRIFICATION #o01)
(defconstant VS-BREATH #o02)
(defconstant VS-MAGIC  #o03)

;;; Various flag bits
(defconstant ISDARK #o0000001)
(defconstant ISCURSED #o000001)
(defconstant ISBLIND #o0000001)
(defconstant ISGONE #o0000002)
(defconstant ISKNOW #o0000002)
(defconstant ISRUN #o0000004)
(defconstant ISFOUND #o0000010)
(defconstant ISINVIS #o0000020)
(defconstant ISMEAN #o0000040)
(defconstant ISGREED #o0000100)
(defconstant ISBLOCK #o0000200)
(defconstant ISHELD #o0000400)
(defconstant ISHUH #o0001000)
(defconstant ISREGEN #o0002000)
(defconstant CANHUH #o0004000)
(defconstant CANSEE #o0010000)
(defconstant ISMISL #o0020000)
(defconstant ISCANC #o0020000)
(defconstant ISMANY #o0040000)
(defconstant ISSLOW #o0040000)
(defconstant ISHASTE #o0100000)

;;; Potion types
(defconstant P-CONFUSE 0)
(defconstant P-PARALYZE 1)
(defconstant P-POISON 2)
(defconstant P-STRENGTH 3)
(defconstant P-SEEINVIS 4)
(defconstant P-HEALING 5)
(defconstant P-MFIND 6)
(defconstant P-TFIND 7)
(defconstant P-RAISE 8)
(defconstant P-XHEAL 9)
(defconstant P-HASTE 10)
(defconstant P-RESTORE 11)
(defconstant P-BLIND 12)
(defconstant P-NOP 13)
(defconstant MAXPOTIONS 14)

;;; Scroll types
(defconstant S-CONFUSE 0)
(defconstant S-MAP 1)
(defconstant S-LIGHT 2)
(defconstant S-HOLD 3)
(defconstant S-SLEEP 4)
(defconstant S-ARMOR 5)
(defconstant S-IDENT 6)
(defconstant S-SCARE 7)
(defconstant S-GFIND 8)
(defconstant S-TELEP 9)
(defconstant S-ENCH 10)
(defconstant S-CREATE 11)
(defconstant S-REMOVE 12)
(defconstant S-AGGR 13)
(defconstant S-NOP 14)
(defconstant S-GENOCIDE 15)
(defconstant MAXSCROLLS 16)

;;; Weapon types
(defconstant MACE 0)
(defconstant SWORD 1)
(defconstant BOW 2)
(defconstant ARROW 3)
(defconstant DAGGER 4)
(defconstant ROCK 5)
(defconstant TWOSWORD 6)
(defconstant SLING 7)
(defconstant DART 8)
(defconstant CROSSBOW 9)
(defconstant BOLT 10)
(defconstant SPEAR 11)
(defconstant MAXWEAPONS 12)

;;; Armor types
(defconstant LEATHER 0)
(defconstant RING-MAIL 1)
(defconstant STUDDED-LEATHER 2)
(defconstant SCALE-MAIL 3)
(defconstant CHAIN-MAIL 4)
(defconstant SPLINT-MAIL 5)
(defconstant BANDED-MAIL 6)
(defconstant PLATE-MAIL 7)
(defconstant MAXARMORS 8)

;;; Ring types
(defconstant R-PROTECT 0)
(defconstant R-ADDSTR 1)
(defconstant R-SUSTSTR 2)
(defconstant R-SEARCH 3)
(defconstant R-SEEINVIS 4)
(defconstant R-NOP 5)
(defconstant R-AGGR 6)
(defconstant R-ADDHIT 7)
(defconstant R-ADDDAM 8)
(defconstant R-REGEN 9)
(defconstant R-DIGEST 10)
(defconstant R-TELEPORT 11)
(defconstant R-STEALTH 12)
(defconstant MAXRINGS 13)

;;; Rod/Wand/Staff types
(defconstant WS-LIGHT 0)
(defconstant WS-HIT 1)
(defconstant WS-ELECT 2)
(defconstant WS-FIRE 3)
(defconstant WS-COLD 4)
(defconstant WS-POLYMORPH 5)
(defconstant WS-MISSILE 6)
(defconstant WS-HASTE-M 7)
(defconstant WS-SLOW-M 8)
(defconstant WS-DRAIN 9)
(defconstant WS-NOP 10)
(defconstant WS-TELAWAY 11)
(defconstant WS-TELTO 12)
(defconstant WS-CANCEL 13)
(defconstant MAXSTICKS 14)

;;; All the fun defines

(defmacro detach (list item)
  "Takes an item out of whatever linked list it might be in."
  `(setf ,list (remove ,item ,list :test #'eql)))

(defmacro attach (list item)
  "Add an item to the head of a list."
  `(push ,item ,list))

(defun inroom (rp cp)
  (and (<= (coord-x cp) (+ (coord-x (moor-r-pos rp)) (1- (coord-x (moor-r-max rp)))))
       (<= (coord-x (moor-r-pos rp)) (coord-x cp))
       (<= (coord-y cp) (+ (coord-y (moor-r-pos rp)) (1- (coord-y (moor-r-max rp)))))
       (<= (coord-y (moor-r-pos rp)) (coord-y cp))))

(defun winat (y x)
  (if (eql (rogue-mvwinch mw y x) #\Space)
      (rogue-mvwinch cl-ncurses:*stdscr* y x)
      (rogue-winch mw)))
(defun rogue-debug (&rest args)
;;  (apply #'error args))
;;    (when wizard (apply #'msg args)))
  (apply #'msg args))


;; should come from cl-ncurses...
(defun move (y x)
  (cl-ncurses:wmove cl-ncurses:*stdscr* y x))

(defun cmov (xy)
  (move (coord-y xy)
        (coord-x xy)))

(defun distance (y1 x1 y2 x2)
  (+ (* (- x2 x1) 
        (- x2 x1)) 
     (* (- y2 y1) 
        (- y2 y1))))

(defun draw (window)
  (cl-ncurses:wrefresh window))

(define-symbol-macro hero.x (coord-x (thing-t-pos *player*)))
(define-symbol-macro hero.y (coord-y (thing-t-pos *player*)))
(define-symbol-macro hero (thing-t-pos *player*))
(define-symbol-macro pstats (thing-t-stats *player*))
(define-symbol-macro pack (thing-t-pack *player*))

(defun on (thing flag) 
  (logtest (thing-t-flags thing) flag))

(defun off (thing flag)
  (not (on thing flag)))

(defun goldcalc ()
  (+ (rnd (+ 50 
             (* 10 level))) 
     2))

(defun isring (h r) 
  (and
   (not (null (aref cur-ring h)))
   (equal (object-o-which (aref cur-ring h)) r)))

(defun iswearing (r) 
  (or (isring LEFT r)
      (isring RIGHT r)))

(defun newgrp () 
  (incf group))

(defmacro object-o-charges (o)
  `(object-o-ac ,o))

(defun ismult (type) 
  (or (equal type POTION)
      (equal type SCROLL)
      (equal type FOOD)))

;;; Moved here from main.lisp
(defun rnd (range)
  "Pick a very random number."
  (case range
    (0 0)
    (otherwise (random range))))

;;; Now we define the structures and types

(defstruct coord
  "Coordinate data type"
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defstruct str-t 
  (st-str 0 :type fixnum)
  (st-add 0 :type fixnum))

(defstruct magic-item
  "Stuff about magic items"
  (mi-name "" :type string)
  (mi-prob 0 :type fixnum)
  (mi-worth 0 :type fixnum))

(defstruct moor
  "Room structure"
  (r-pos (make-coord) :type coord)
  (r-max (make-coord) :type coord)
  (r-gold (make-coord) :type coord)
  (r-goldval 0 :type fixnum)
  (r-flags 0 :type fixnum)
  (r-nexits 0 :type fixnum)
  ;; The following sucks, you have to initialize manually to a fresh
  ;; array of coords. Fortunately that's only in one place.
  r-exit)

(defstruct rogue-trap 
  "Array of all traps on this level"
  (tr-pos (make-coord) :type coord)
  (tr-type #\Nul :type character) 
  (tr-flags 0 :type fixnum))

(defstruct stats 
  "Structure describing a fighting being"
  (s-str (make-str-t) :type str-t)
  (s-exp 0 :type integer)
  (s-lvl 0 :type fixnum)
  (s-arm 0 :type fixnum)
  (s-hpt 0 :type fixnum)
  (s-dmg "" :type string))

(defstruct thing 
  "Structure for monsters and player"
  (t-pos (make-coord) :type coord)
  (t-turn nil :type boolean)
  (t-type #\Nul :type character)
  (t-disguise #\Nul :type character)
  (t-oldch #\Nul :type character)
  (t-dest (make-coord) :type coord)
  (t-flags 0 :type fixnum)
  (t-stats (make-stats) :type stats)
  (t-pack (list))
  (t-reserved 0 :type fixnum))

(defstruct monster
  "Array containing information on all the various types of mosnters"
  (m-name "" :type string)              ; What to call the monster
  (m-carry 0 :type fixnum)              ; Probability of carrying something
  (m-flags 0 :type fixnum)              ; Things about the monster
  (m-stats (make-stats) :type stats))   ; Initial stats

(defstruct object 
  "Structure for a thing that the rogue can carry"
  (o-type #\Nul :type character)               ; What kind of object it is
  (o-pos (make-coord) :type coord)
  (o-launch 100 :type fixnum)           ; "o-which" needed to launch it, 100 == impossible
  (o-damage "" :type string)
  (o-hurldmg "" :type string)
  (o-count 0 :type fixnum)
  (o-which 0 :type fixnum)
  (o-hplus 0 :type fixnum)
  (o-dplus 0 :type fixnum)
  (o-ac 0 :type fixnum)
  (o-flags 0 :type fixnum)
  (o-group 0 :type fixnum))

(defstruct words 
  (w-string "" :type string))

(defconstant NCOLORS 24)
(defconstant NSYLLS  159)
(defconstant NSTONES 20)
(defconstant NWOOD 22)
(defconstant NMETAL 11)

;;; Utilities

;;; From onlisp.lisp

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro when-let ((var value) &body body)
  "Evaluate VALUE, and if the result is non-nil bind it to VAR
and evaluate BODY."
  `(let ((,var ,value))
     (when ,var ,@body)))

;;; From the 'net

(defun shuffle-vector! (result)
  "Destructively shuffle elements in a vector like a deck of
cards."
  (loop
     finally (return result)
     for i from (length result) downto 1
     as j = (random i)
     do (rotatef (svref result j) (svref result (1- i)))))

(defun shuffle-vector (v)
  "Return copy of vector with elements shuffled like a deck of
cards."
  (shuffle-vector! (copy-seq v)))

(defun onep (n)
  (and (numberp n) (= n 1)))

;;; Misc

(defmacro verbose (&body body)
  `(unless terse ,@body))

(defun nonzerop (n)
  (and (numberp n) (not (zerop n))))

(defmacro logior! (place &rest integers)
  "Like C's |= operator."
  `(setf ,place (logior ,place ,@integers)))

(defmacro logclr! (place integer)
  "Destructively clear a bit."
  `(setf ,place (logand ,place (lognot ,integer))))

(defmacro zero! (&rest places)
  (let ((zeroed-places
         (mapcan #'(lambda (&rest args) args) 
                 places 
                 (make-list (length places) :initial-element 0))))
  `(setf ,@zeroed-places)))

;; Really should come from cl-ncurses
(defun ctrl (ch) 
  (code-char (logand (char-int ch) #o37)))

(defun unctrl-char (c)
  ;; XXX: this doesn't actually handle control chars as it should??
  ;; (I couldn't get cl-ncurses' unctrl to work).
  (format nil "~:c" c))

(defun >1 (number)
  (> number 1))

;; Curses helpers to deal with the fact that curses expects ints, not
;; Lisp chars.  TODO: do something more sane here.
(defun rogue-code-char (code)
  ;; Note this is a total guess as to what to do here
  ;; TODO: maybe logand with 255 instead?
  (if (minusp code)
      #\Nul
      (code-char code)))

(defun rogue-addch (ch)
  (cl-ncurses:addch (char-code ch)))

(defun rogue-waddch (win ch)
  (cl-ncurses:waddch win (char-code ch)))
    
(defun rogue-mvaddch (y x ch)
  (cl-ncurses:mvaddch y x (char-code ch)))

(defun rogue-mvwaddch (win y x ch)
  (cl-ncurses:mvwaddch win y x (char-code ch)))

(defun rogue-winch (win)
  (rogue-code-char (cl-ncurses:winch win)))

(defun rogue-mvinch (y x)
  (rogue-code-char (cl-ncurses:mvinch y x)))

(defun rogue-mvwinch (win y x)
  (rogue-code-char (cl-ncurses:mvwinch win y x)))

(defvar *resettable-symbols* '())

(defun reset-rogue-symbols ()
  (dolist (symb *resettable-symbols*)
    (setf (symbol-value symb) (funcall (get symb 'resetter)))))

(defmacro define-resettable (var val &optional doc)
  `(progn
     (setf (get ',var 'resetter) (lambda () ,val))
     (defparameter ,var (funcall (get ',var 'resetter)) ,doc)
     (pushnew ',var *resettable-symbols*)))

(defun save-resettables (stream)
  (dolist (symb *resettable-symbols*)
    (format stream "(setf ~a '" (symbol-name symb))
    (prin1 (symbol-value symb) stream)
    (format stream ")~%~%")))
