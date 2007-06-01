;;; global variable initializaton
;;; 
;;; @(#)init.c	3.33 (Berkeley) 6/15/81

(in-package :cl-rogue)

(define-resettable playing t)
(define-resettable running nil)
(define-resettable wizard t) ; TODO: change back to nil by default
(define-resettable notify t)
(define-resettable fight_flush nil)
(define-resettable terse nil)
(define-resettable door_stop nil)
(define-resettable jump nil)
(define-resettable slow_invent nil)
(define-resettable firstmove nil)
(define-resettable askme nil)
(define-resettable *amulet* nil)
(define-resettable in_shell nil)
(define-resettable lvl_obj '())
(define-resettable mlist '())
(define-resettable cur_weapon nil)
(define-resettable mpos 0)
(define-resettable no_move 0)
(define-resettable nh (make-coord) "Used to hold the new hero position")
(define-resettable no_command 0)
(define-resettable level 1)
(define-resettable purse 0)
(define-resettable inpack 0)
(define-resettable total 0)
(define-resettable no_food 0)
(define-resettable *count* 0)
(define-resettable fung_hit 0)
(define-resettable quiet 0)
(define-resettable food_left HUNGERTIME)
(define-resettable group 1)
(define-resettable hungry_state 0)
(define-resettable lastscore -1)

(define-resettable *player* (make-thing :t_type PLAYER :t_disguise PLAYER :t_oldch PLAYER))
(define-resettable rooms (let ((new-rooms (make-array MAXROOMS)))
                           (dotimes (i (length new-rooms))
                             (setf (aref new-rooms i) (make-moor :r_exit (vector (make-coord)
                                                                                 (make-coord)
                                                                                 (make-coord)
                                                                                 (make-coord)))))
                           new-rooms))

(define-resettable oldrp nil)
(define-resettable max_stats nil)
(define-resettable cur_armor nil)
(define-resettable cur_ring (vector nil nil))
(define-resettable *after* nil)
(define-resettable waswizard nil)
(define-resettable oldpos (make-coord))       ; Position before last look() call 

(define-resettable delta (make-coord))        ; Change indicated to get_dir()    
(define-symbol-macro delta.x (coord-x delta))
(define-symbol-macro delta.y (coord-y delta))

(define-resettable s_know (make-array MAXSCROLLS :initial-element nil)) ; Does he know what a scroll does 
(define-resettable p_know (make-array MAXPOTIONS :initial-element nil)) ; Does he know what a potion does 
(define-resettable r_know (make-array MAXRINGS :initial-element nil)) ; Does he know what a ring does
(define-resettable ws_know (make-array MAXSTICKS :initial-element nil)) ; Does he know what a stick does 

(define-resettable take nil)                 ; Thing the rogue is taking 
(define-resettable runch nil)                ; Direction player is running 
(defparameter whoami (sb-posix:getenv "USER")) ; Name of player (TODO: make less hacky)
(define-resettable fruit "")                   ; Favorite fruit 
(define-resettable huh "")                   ; The last message printed 
(define-resettable dnum 0)                   ; Dungeon number 
(define-resettable s_names (make-array MAXSCROLLS :initial-element nil)) ; Names of the scrolls 
(define-resettable p_colors (make-array MAXPOTIONS :initial-element nil)) ; Colors of the potions 
(define-resettable r_stones (make-array MAXRINGS :initial-element nil)) ; Stone settings of the rings 
(define-resettable ws_made (make-array MAXSTICKS :initial-element nil)) ; What sticks are made of 
(define-resettable s_guess (make-array MAXSCROLLS :initial-element nil)) ; Players guess at what scroll is 
(define-resettable p_guess (make-array MAXPOTIONS :initial-element nil)) ; Players guess at what potion is 
(define-resettable r_guess (make-array MAXRINGS :initial-element nil)) ; Players guess at what ring is 
(define-resettable ws_guess (make-array MAXSTICKS :initial-element nil)) ; Players guess at what wand is 
(define-resettable ws_type (make-array MAXSTICKS :initial-element nil)) ; Is it a wand or a staff 
(define-resettable file_name "")                      ; Save file name 
(defparameter home nil)                           ; User's home directory 
(define-resettable prbuf nil)                          ; Buffer for sprintfs 
(define-resettable outbuf nil)                         ; Output buffer for stdout 
(define-resettable max_hp 0)                       ; Player's max hit points 
(define-resettable ntraps 0)                       ; Number of traps on this level 
(define-resettable max_level 0)                    ; Deepest player has gone 

(define-resettable traps (map-into (make-array MAXTRAPS :element-type 'rogue-trap) #'make-rogue-trap))

(defun badcheck (name magica)
  (unless (= (magic_item-mi_prob (aref magica (1- (length magica)))) 100)
    (format *error-output* "~%Bad percentages for ~a:~%" name)
    (dotimes (i (length magica))
      (format *error-output* "~3d% ~a~%" (magic_item-mi_prob (aref magica i)) (magic_item-mi_name (aref magica i))))
    (format *error-output* "[hit RETURN to continue]")
    (read-line)))

(defun monmake (name carry flags exp lvl arm dmg)
  (make-monster 
   :m_name name 
   :m_carry carry 
   :m_flags (apply #'logior flags)
   :m_stats (make-stats
             :s_str (make-str_t :st_str 1 :st_add 1)
             :s_exp exp
             :s_lvl lvl
             :s_arm arm
             :s_hpt 1
             :s_dmg dmg)))

(define-resettable monsters 
  (make-array 
   26 :element-type 'monster
   :initial-contents
   (mapcar #'(lambda (l)
               (apply #'monmake l))
           `(("giant ant" 0 (,ISMEAN) 10 2 3 "1d6")
             ("bat" 0 (0) 1 1 3 "1d2" )
             ("centaur" 15 (0) 15 4 4 "1d6/1d6")
             ("dragon" 100 (,ISGREED) 9000 10 -1 "1d8/1d8/3d10")
             ("floating eye" 0 (0) 5 1 9 "0d0")
             ("violet fungi" 0 (,ISMEAN) 85 8 3 "000d0") 
             ("gnome" 10 (0) 8 1 5 "1d6") 
             ("hobgoblin" 0 (,ISMEAN) 3 1 5 "1d8") 
             ("invisible stalker" 0 (,ISINVIS) 120 8 3 "4d4") 
             ("jackal" 0 (,ISMEAN) 2 1 7 "1d2") 
             ("kobold" 0 (,ISMEAN) 1 1 7 "1d4") 
             ("leprechaun" 0 (0) 10 3 8 "1d1") 
             ("mimic" 30 (0) 140 7 7 "3d4") 
             ("nymph" 100 (0) 40 3 9 "0d0") 
             ("orc" 15 (,ISBLOCK) 5 1 6 "1d8") 
             ("purple worm" 70 (0) 7000 15 6 "2d12/2d4") 
             ("quasit" 30 (,ISMEAN) 35 3 2 "1d2/1d2/1d4") 
             ("rust monster" 0 (,ISMEAN) 25 5 2 "0d0/0d0") 
             ("snake" 0 (,ISMEAN) 3 1 5 "1d3") 
             ("troll" 50 (,ISREGEN ,ISMEAN) 55 6 4 "1d8/1d8/2d6") 
             ("umber hulk" 40 (,ISMEAN) 130 8 2 "3d4/3d4/2d5") 
             ("vampire" 20 (,ISREGEN ,ISMEAN) 380 8 1 "1d10") 
             ("wraith" 0 (0) 55 5 4 "1d6") 
             ("xorn" 0 (,ISMEAN) 120 7 -2 "1d3/1d3/1d3/4d6") 
             ("yeti" 30 (0) 50 4 6 "1d6/1d6")
             ("zombie" 0 (,ISMEAN) 7 2 8 "1d8")))))

(defun char-monster (c)
  (aref monsters (- (char-code c) (char-code #\A))))

(defun char-monster-name (c)
  (monster-m_name (aref monsters (- (char-code c) (char-code #\A)))))


(defun init_player ()
  "Roll up the rogue."
  (setf (stats-s_lvl pstats) 1
        (stats-s_exp pstats) 0
        max_hp 12
        (stats-s_hpt pstats) max_hp)
  (if (= (rnd 100) 7)
      (progn
        (setf (str_t-st_str (stats-s_str pstats)) 18)
        (setf (str_t-st_add (stats-s_str pstats)) (1+ (rnd 100))))
      (progn
        (setf (str_t-st_str (stats-s_str pstats)) 16)
        (setf (str_t-st_add (stats-s_str pstats)) 0)))
  (setf (stats-s_dmg pstats) "1d4"
        (stats-s_arm pstats) 10
        max_stats pstats
        pack nil))

;;; Contains defintions and functions for dealing with things like
;;; potions and scrolls

(defparameter rainbow 
  (vector
   "red"
   "blue"
   "green"
   "yellow"
   "black"
   "brown"
   "orange"
   "pink"
   "purple"
   "grey"
   "white"
   "silver"
   "gold"
   "violet"
   "clear"
   "vermilion"
   "ecru"
   "turquoise"
   "magenta"
   "amber"
   "topaz"
   "plaid"
   "tan"
   "tangerine"))

(defparameter sylls
  (vector
   "a" "ab" "ag" "aks" "ala" "an" "ankh" "app" "arg" "arze"
   "ash" "ban" "bar" "bat" "bek" "bie" "bin" "bit" "bjor"
   "blu" "bot" "bu" "byt" "comp" "con" "cos" "cre" "dalf"
   "dan" "den" "do" "e" "eep" "el" "eng" "er" "ere" "erk"
   "esh" "evs" "fa" "fid" "for" "fri" "fu" "gan" "gar"
   "glen" "gop" "gre" "ha" "he" "hyd" "i" "ing" "ion" "ip"
   "ish" "it" "ite" "iv" "jo" "kho" "kli" "klis" "la" "lech"
   "man" "mar" "me" "mi" "mic" "mik" "mon" "mung" "mur"
   "nej" "nelg" "nep" "ner" "nes" "nes" "nih" "nin" "o" "od"
   "ood" "org" "orn" "ox" "oxy" "pay" "pet" "ple" "plu" "po"
   "pot" "prok" "re" "rea" "rhov" "ri" "ro" "rog" "rok" "rol"
   "sa" "san" "sat" "see" "sef" "seh" "shu" "ski" "sna"
   "sne" "snik" "sno" "so" "sol" "sri" "sta" "sun" "ta"
   "tab" "tem" "ther" "ti" "tox" "trol" "tue" "turs" "u"
   "ulk" "um" "un" "uni" "ur" "val" "viv" "vly" "vom" "wah"
   "wed" "werg" "wex" "whon" "wun" "xo" "y" "yot" "yu"
   "zant" "zap" "zeb" "zim" "zok" "zon" "zum"))

(defparameter stones
  (vector
   "Agate"
   "Alexandrite"
   "Amethyst"
   "Carnelian"
   "Diamond"
   "Emerald"
   "Granite"
   "Jade"
   "Kryptonite"
   "Lapus lazuli"
   "Moonstone"
   "Obsidian"
   "Onyx"
   "Opal"
   "Pearl"
   "Ruby"
   "Saphire"
   "Tiger eye"
   "Topaz"
   "Turquoise"))

(defparameter wood
  (vector
   "Avocado wood"
   "Balsa"
   "Banyan"
   "Birch"
   "Cedar"
   "Cherry"
   "Cinnibar"
   "Driftwood"
   "Ebony"
   "Eucalyptus"
   "Hemlock"
   "Ironwood"
   "Mahogany"
   "Manzanita"
   "Maple"
   "Oak"
   "Persimmon wood"
   "Redwood"
   "Rosewood"
   "Teak"
   "Walnut"
   "Zebra wood"))

(defparameter metal
  (vector
   "Aluminium"
   "Bone"
   "Brass"
   "Bronze"
   "Copper"
   "Iron"
   "Lead"
   "Pewter"
   "Steel"
   "Tin"
   "Zinc"))

(defun mimake (name prob &optional (worth 0))
  (make-magic_item :mi_name name :mi_prob prob :mi_worth worth))

(define-resettable things
  (make-array 
   NUMTHINGS
   :element-type 'magic_item
   :initial-contents
   (mapcar #'(lambda (l)
               (apply #'mimake l))
           '(("" 27)                    ; potion 
             ("" 27)                    ; scroll 
             ("" 18)                    ; food 
             ("" 9)                     ; weapon 
             ("" 9)                     ; armor 
             ("" 5)                     ; ring 
             ("" 5)))))                 ; stick 

(define-resettable s_magic
  (make-array
   MAXSCROLLS
   :element-type 'magic_item
   :initial-contents
   (mapcar #'(lambda (l)
               (apply #'mimake l))
           '(("monster confusion" 8 170)
             ("magic mapping" 5 180)
             ("light" 10 100)
             ("hold monster" 2 200)
             ("sleep" 5 50)
             ("enchant armor" 8 130)
             ("identify" 21 100)
             ("scare monster" 4 180)
             ("gold detection" 4 110)
             ("teleportation" 7 175)
             ("enchant weapon" 10 150)
             ("create monster" 5 75)
             ("remove curse" 8 105)
             ("aggravate monsters" 1 60)
             ("blank paper" 1 50)
             ("genocide" 1 200)))))

(define-resettable p_magic
  (make-array 
   MAXPOTIONS
   :element-type 'magic_item
   :initial-contents
   (mapcar #'(lambda (l)
               (apply #'mimake l))
           '(("confusion" 8 50)
             ("paralysis" 10 50)
             ("poison" 8 50)
             ("gain strength" 15 150)
             ("see invisible" 2 170)
             ("healing" 15 130)
             ("monster detection" 6 120)
             ("magic detection" 6 105)
             ("raise level" 2 220)
             ("extra healing" 5 180)
             ("haste self" 4 200)
             ("restore strength" 14 120)
             ("blindness" 4 50)
             ("thirst quenching" 1 50)))))

(define-resettable r_magic
  (make-array 
   MAXRINGS
   :element-type 'magic_item
   :initial-contents
   (mapcar #'(lambda (l)
               (apply #'mimake l))
           '(("protection" 9 200)
             ("add strength" 9 200)
             ("sustain strength" 5 180)
             ("searching" 10 200)
             ("see invisible" 10 175)
             ("adornment" 1 100)
             ("aggravate monster" 11 100)
             ("dexterity" 8 220)
             ("increase damage" 8 220)
             ("regeneration" 4 260)
             ("slow digestion" 9 240)
             ("telportation" 9 100)
             ("stealth" 7 100)))))

(define-resettable ws_magic
  (make-array 
   MAXSTICKS
   :element-type 'magic_item
   :initial-contents
   (mapcar #'(lambda (l)
               (apply #'mimake l))
           '(("light" 12 120)
             ("striking" 9 115)
             ("lightning" 3 200)
             ("fire" 3 200)
             ("cold" 3 200)
             ("polymorph" 15 210)
             ("magic missile" 10 170)
             ("haste monster" 9 50)
             ("slow monster" 11 220)
             ("drain life" 9 210)
             ("nothing" 1 70)
             ("teleport away" 5 140)
             ("teleport to" 5 60)
             ("cancellation" 5 130)))))

(defparameter a_class #(8 7 7 6 5 4 4 3))

;; Names of armor types
(defparameter a_names #("leather armor" "ring mail" "studded leather armor"
                        "scale mail" "chain mail" "splint mail" "banded mail"
                        "plate mail"))

(defparameter a_chances #(20 35 50 63 75 85 95 100))

(defun init_things ()
  "Initialize the probabilities for types of things."
  (do
   ((cur 1 (1+ cur))
    (prev 0 (1+ prev)))
   ((>= cur NUMTHINGS))
    (incf (magic_item-mi_prob (aref things cur)) (magic_item-mi_prob (aref things prev))))
  (badcheck "things" things))


(defun init_colors ()
  "Initialize the potion color scheme for this time."
  (let ((random-colors (shuffle-vector rainbow)))
    (dotimes (i (length p_colors))
      (setf (aref p_colors i) (aref random-colors i))
      (setf (aref p_know i) nil)
      (setf (aref p_guess i) nil)
      (when (plusp i)
        (incf (magic_item-mi_prob (aref p_magic i)) (magic_item-mi_prob (aref p_magic (1- i)))))))
  (badcheck "potions" p_magic))

(defun init_names ()
  "Generate the names of the various scrolls."
  (dotimes (i MAXSCROLLS)
    (let ((nwords (+ (rnd 4) 2))
          (prbuf ""))
      (dotimes (_ nwords)
        (let ((nsyl (1+ (rnd 3))))
          (dotimes (_ nsyl)
            (setf prbuf (concatenate 'string prbuf (aref sylls (rnd NSYLLS))))))
        (setf prbuf (concatenate 'string prbuf " ")))
      (setf (aref s_names i) (string-trim '(#\Space) prbuf))
      (setf (aref s_know i) nil)
      (setf (aref s_guess i) nil)
      (when (plusp i)
        (incf (magic_item-mi_prob (aref s_magic i)) (magic_item-mi_prob (aref s_magic (1- i)))))))
  (badcheck "scrolls" s_magic))

(defun init_stones ()
  "Initialize the ring stone setting scheme for this time."
  (let ((random-stones (shuffle-vector stones)))
    (dotimes (i MAXRINGS)
      (setf (aref r_stones i) (aref random-stones i))
      (setf (aref r_know i) nil)
      (setf (aref r_guess i) nil)
      (when (plusp i)
        (incf (magic_item-mi_prob (aref r_magic i)) (magic_item-mi_prob (aref r_magic (1- i)))))))
  (badcheck "rings" r_magic))

(defun init_materials ()
  "Initialize the construction materials for wands and staves."
  (let ((random-material (shuffle-vector 
                          (concatenate 'vector 
                                       (map 'vector #'(lambda (x) (list x "staff")) wood) 
                                       (map 'vector #'(lambda (x) (list x "wand")) metal)))))
    (dotimes (i MAXSTICKS)
      (setf (aref ws_made i) (first (aref random-material i)))
      (setf (aref ws_type i) (second (aref random-material i)))
      (setf (aref ws_know i) nil)
      (setf (aref ws_guess i) nil)
      (when (plusp i)
        (incf (magic_item-mi_prob (aref ws_magic i)) (magic_item-mi_prob (aref ws_magic (1- i)))))))
  (badcheck "sticks" ws_magic))


(defparameter helpstr
  '((#\? . "	prints help")
    (#\/ . "	identify object")
    (#\h . "	left")
    (#\j . "	down")
    (#\k . "	up")
    (#\l . "	right")
    (#\y . "	up & left")
    (#\u . "	up & right")
    (#\b . "	down & left")
    (#\n . "	down & right")
    (#\H . "	run left")
    (#\J . "	run down")
    (#\K . "	run up")
    (#\L . "	run right")
    (#\Y . "	run up & left")
    (#\U . "	run up & right")
    (#\B . "	run down & left")
    (#\N . "	run down & right")
    (#\t . "<dir>	throw something")
    (#\f . "<dir>	forward until find something")
    (#\p . "<dir>	zap a wand in a direction")
    (#\z . "	zap a wand or staff")
    (#\> . "	go down a staircase")
    (#\s . "	search for trap/secret door")
    (#\  . "	(space) rest for a while")
    (#\i . "	inventory")
    (#\I . "	inventory single item")
    (#\q . "	quaff potion")
    (#\r . "	read paper")
    (#\e . "	eat food")
    (#\w . "	wield a weapon")
    (#\W . "	wear armor")
    (#\T . "	take armor off")
    (#\P . "	put on ring")
    (#\R . "	remove ring")
    (#\d . "	drop object")
    (#\c . "	call object")
    (#\o . "	examine/set options")
    (#.(ctrl #\L) . "	redraw screen")
    (#.(ctrl #\R) . "	repeat last message")
    (#\Escape . "	cancel command")
    (#\v . "	print program version number")
    (#\! . "	shell escape")
    (#\S . "	save game")
    (#\Q . "	quit")))
