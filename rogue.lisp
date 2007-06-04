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
(defconstant BOLT_LENGTH 6)

;;; Save against things
(defconstant VS_POISON #o00)
(defconstant VS_PARALYZATION #o00)
(defconstant VS_DEATH #o00)
(defconstant VS_PETRIFICATION #o01)
(defconstant VS_BREATH #o02)
(defconstant VS_MAGIC  #o03)

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
(defconstant P_CONFUSE 0)
(defconstant P_PARALYZE 1)
(defconstant P_POISON 2)
(defconstant P_STRENGTH 3)
(defconstant P_SEEINVIS 4)
(defconstant P_HEALING 5)
(defconstant P_MFIND 6)
(defconstant P_TFIND 7)
(defconstant P_RAISE 8)
(defconstant P_XHEAL 9)
(defconstant P_HASTE 10)
(defconstant P_RESTORE 11)
(defconstant P_BLIND 12)
(defconstant P_NOP 13)
(defconstant MAXPOTIONS 14)

;;; Scroll types
(defconstant S_CONFUSE 0)
(defconstant S_MAP 1)
(defconstant S_LIGHT 2)
(defconstant S_HOLD 3)
(defconstant S_SLEEP 4)
(defconstant S_ARMOR 5)
(defconstant S_IDENT 6)
(defconstant S_SCARE 7)
(defconstant S_GFIND 8)
(defconstant S_TELEP 9)
(defconstant S_ENCH 10)
(defconstant S_CREATE 11)
(defconstant S_REMOVE 12)
(defconstant S_AGGR 13)
(defconstant S_NOP 14)
(defconstant S_GENOCIDE 15)
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
(defconstant RING_MAIL 1)
(defconstant STUDDED_LEATHER 2)
(defconstant SCALE_MAIL 3)
(defconstant CHAIN_MAIL 4)
(defconstant SPLINT_MAIL 5)
(defconstant BANDED_MAIL 6)
(defconstant PLATE_MAIL 7)
(defconstant MAXARMORS 8)

;;; Ring types
(defconstant R_PROTECT 0)
(defconstant R_ADDSTR 1)
(defconstant R_SUSTSTR 2)
(defconstant R_SEARCH 3)
(defconstant R_SEEINVIS 4)
(defconstant R_NOP 5)
(defconstant R_AGGR 6)
(defconstant R_ADDHIT 7)
(defconstant R_ADDDAM 8)
(defconstant R_REGEN 9)
(defconstant R_DIGEST 10)
(defconstant R_TELEPORT 11)
(defconstant R_STEALTH 12)
(defconstant MAXRINGS 13)

;;; Rod/Wand/Staff types
(defconstant WS_LIGHT 0)
(defconstant WS_HIT 1)
(defconstant WS_ELECT 2)
(defconstant WS_FIRE 3)
(defconstant WS_COLD 4)
(defconstant WS_POLYMORPH 5)
(defconstant WS_MISSILE 6)
(defconstant WS_HASTE_M 7)
(defconstant WS_SLOW_M 8)
(defconstant WS_DRAIN 9)
(defconstant WS_NOP 10)
(defconstant WS_TELAWAY 11)
(defconstant WS_TELTO 12)
(defconstant WS_CANCEL 13)
(defconstant MAXSTICKS 14)

;;; All the fun defines

(defmacro detach (list item)
  "Takes an item out of whatever linked list it might be in."
  `(setf ,list (remove ,item ,list :test #'eql)))

(defmacro attach (list item)
  "Add an item to the head of a list."
  `(push ,item ,list))

(defun inroom (rp cp)
  (and (<= (coord-x cp) (+ (coord-x (moor-r_pos rp)) (1- (coord-x (moor-r_max rp)))))
       (<= (coord-x (moor-r_pos rp)) (coord-x cp))
       (<= (coord-y cp) (+ (coord-y (moor-r_pos rp)) (1- (coord-y (moor-r_max rp)))))
       (<= (coord-y (moor-r_pos rp)) (coord-y cp))))

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

(define-symbol-macro hero.x (coord-x (thing-t_pos *player*)))
(define-symbol-macro hero.y (coord-y (thing-t_pos *player*)))
(define-symbol-macro hero (thing-t_pos *player*))
(define-symbol-macro pstats (thing-t_stats *player*))
(define-symbol-macro pack (thing-t_pack *player*))

(defun on (thing flag) 
  (logtest (thing-t_flags thing) flag))

(defun off (thing flag)
  (not (on thing flag)))

(defun goldcalc ()
  (+ (rnd (+ 50 
             (* 10 level))) 
     2))

(defun isring (h r) 
  (and
   (not (null (aref cur_ring h)))
   (equal (object-o_which (aref cur_ring h)) r)))

(defun iswearing (r) 
  (or (isring LEFT r)
      (isring RIGHT r)))

(defun newgrp () 
  (incf group))

(defmacro object-o_charges (o)
  `(object-o_ac ,o))

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

(defstruct str_t 
  (st_str 0 :type fixnum)
  (st_add 0 :type fixnum))

(defstruct magic_item
  "Stuff about magic items"
  (mi_name "" :type string)
  (mi_prob 0 :type fixnum)
  (mi_worth 0 :type fixnum))

(defstruct moor
  "Room structure"
  (r_pos (make-coord) :type coord)
  (r_max (make-coord) :type coord)
  (r_gold (make-coord) :type coord)
  (r_goldval 0 :type fixnum)
  (r_flags 0 :type fixnum)
  (r_nexits 0 :type fixnum)
  ;; The following sucks, you have to initialize manually to a fresh
  ;; array of coords. Fortunately that's only in one place.
  r_exit)

(defstruct rogue-trap 
  "Array of all traps on this level"
  (tr_pos (make-coord) :type coord)
  (tr_type #\Nul :type character) 
  (tr_flags 0 :type fixnum))

(defstruct stats 
  "Structure describing a fighting being"
  (s_str (make-str_t) :type str_t)
  (s_exp 0 :type integer)
  (s_lvl 0 :type fixnum)
  (s_arm 0 :type fixnum)
  (s_hpt 0 :type fixnum)
  (s_dmg "" :type string))

(defstruct thing 
  "Structure for monsters and player"
  (t_pos (make-coord) :type coord)
  (t_turn nil :type boolean)
  (t_type #\Nul :type character)
  (t_disguise #\Nul :type character)
  (t_oldch #\Nul :type character)
  (t_dest (make-coord) :type coord)
  (t_flags 0 :type fixnum)
  (t_stats (make-stats) :type stats)
  (t_pack (list))
  (t_reserved 0 :type fixnum))

(defstruct monster
  "Array containing information on all the various types of mosnters"
  (m_name "" :type string)              ; What to call the monster
  (m_carry 0 :type fixnum)              ; Probability of carrying something
  (m_flags 0 :type fixnum)              ; Things about the monster
  (m_stats (make-stats) :type stats))   ; Initial stats

(defstruct object 
  "Structure for a thing that the rogue can carry"
  (o_type #\Nul :type character)               ; What kind of object it is
  (o_pos (make-coord) :type coord)
  (o_launch 100 :type fixnum)           ; "o_which" needed to launch it, 100 == impossible
  (o_damage "" :type string)
  (o_hurldmg "" :type string)
  (o_count 0 :type fixnum)
  (o_which 0 :type fixnum)
  (o_hplus 0 :type fixnum)
  (o_dplus 0 :type fixnum)
  (o_ac 0 :type fixnum)
  (o_flags 0 :type fixnum)
  (o_group 0 :type fixnum))

(defstruct words 
  (w_string "" :type string))

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
