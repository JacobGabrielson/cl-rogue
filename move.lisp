;;;; Hero movement commands
;;;; @(#)move.c       3.26 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defun do-run (ch)
  "Start the hero running."
  (setf running t
        *after* nil
        runch ch))

(defun do-move (dy dx)
  "Check to see that a move is legal.  If it is, handle the
consequences (fighting, picking up, etc."
  (setf firstmove nil)
  (when (plusp no-move)
    (decf no-move)
    (msg "You are still stuck in the bear trap")
    (return-from do-move))

  ;; Do a confused move (maybe)
  (cond
    ((and (< (rnd 100) 80)
          (on *player* ISHUH))
     (setf nh (rndmove *player*)))
    (t
     (setf (coord-y nh) (+ hero.y dy)
           (coord-x nh) (+ hero.x dx))))

  ;; Check if he tried to move off the screen or make an illegal
  ;; diagonal move, and stop him if he did.
  (when (or (minusp (coord-x nh))
            (> (coord-x nh) (1- cl-ncurses:*cols*))
            (minusp (coord-y nh))
            (> (coord-y nh) (1- cl-ncurses:*lines*))
            (not (diag-ok hero nh)))
    (setf *after* nil
          running nil)
    (return-from do-move))

  (when (and running 
             (equalp hero nh))
    (setf *after* nil
          running nil))
  (let ((ch (winat (coord-y nh) (coord-x nh))))
    (when (and (on *player* ISHELD) 
               (not (eql ch #\F)))
      (msg "You are being held")
      (return-from do-move))
    (case ch
      ((#\Space #\| #\- #.SECRETDOOR)
       (setf *after* nil
             running nil)
       (return-from do-move))
      (#.TRAP
       (setf ch (be-trapped nh))
       (case ch
         ((#.TRAPDOOR #.TELTRAP) 
          (return-from do-move))))
      ((#.GOLD #.POTION #.SCROLL #.FOOD #.WEAPON
               #.ARMOR #.RING #.AMULET #.STICK)
       (setf running nil
             take ch)))

    (cond
      ((and (eql ch PASSAGE) 
            (eql (winat hero.y hero.x) DOOR))
       (light hero))
      ((eql ch DOOR)
       (setf running nil)
       (when (eql (winat hero.y hero.x) PASSAGE)
         (light nh)))
      ((eql ch STAIRS)
       (setf running nil))
      ((upper-case-p ch)
       (setf running nil)
       (fight nh ch cur-weapon nil)
       (return-from do-move)))

    (setf ch (winat hero.y hero.x))
    (cl-ncurses::wmove cw hero.y hero.x)
    (rogue-waddch cw ch)
    (setf hero (copy-structure nh))     ; COPY-STRUCTURE for paranoia
    (cl-ncurses::wmove cw hero.y hero.x)
    (rogue-waddch cw PLAYER)))

(defun light (cp)
  "Called to illuminate a room. If it is dark, remove anything
that might move."
  (when-let (rp (roomin cp))
    (unless (on *player* ISBLIND)
      (dotimes (j (coord-y (moor-r-max rp)))
        (dotimes (k (coord-x (moor-r-max rp)))
          (let* ((dx (+ (coord-x (moor-r-pos rp)) 
                        k))
                 (dy (+ (coord-y (moor-r-pos rp)) 
                        j))
                 (ch (show dy dx))
                 (darkp (logtest (moor-r-flags rp) ISDARK)))
            (cl-ncurses::wmove cw dy dx)
            ;; Figure out how to display a secret door.
            (when (eql ch SECRETDOOR)
              (setf ch
                    (if (or (zerop j)
                            (= j (1- (coord-y (moor-r-max rp)))))
                        #\-
                        #\|)))
            ;; If the room is a dark room, we might want to remove
            ;; monsters and the like from it (since they might
            ;; move).
            (when (upper-case-p ch)
              (let ((item (wake-monster dy dx)))
                (when (and (eql (thing-t-oldch item) #\Space)
                           (not darkp))
                  (setf (thing-t-oldch item) (rogue-mvwinch 
                                              cl-ncurses:*stdscr* 
                                              dy dx)))))
            (rogue-mvwaddch
             cw dy dx
             (cond
               (darkp
                (let ((rch (rogue-mvwinch cw dy dx)))
                  (case rch
                    ((#.DOOR #.STAIRS #.TRAP #\| #\- #\Space)
                     rch)
                    (#.FLOOR
                     (if (on *player* ISBLIND) FLOOR #\Space))
                    (otherwise
                     #\Space))))
               (t ch)))))))))
                

(defun show (y x)
  "Returns what a certain thing will display as to the
un-initiated."
  (let ((ch (winat y x)))
    (case ch
      (#.TRAP
       (if (logtest (rogue-trap-tr-flags (trap-at y x)) ISFOUND)
           TRAP 
           FLOOR))
      ((#\M #\I)
       (let ((tp (find-mons y x)))
         (unless tp
           (msg "Can't find monster in show"))
         (if (eql ch #\M)
             (thing-t-disguise tp)
             (when (off *player* CANSEE)
               ;; Hide invisible monsters
               (rogue-mvwinch cl-ncurses:*stdscr* y x)))))
      (otherwise ch))))

(defun be-trapped (tc)
  "The guy stepped on a trap.... Make him pay."
  (let* ((tp        (trap-at (coord-y tc) (coord-x tc)))
         (trap-pos  (rogue-trap-tr-pos tp))
         (trap-type (rogue-trap-tr-type tp)))
    (setf *count* 0
          running nil)
    (rogue-mvwaddch cw (coord-y trap-pos) (coord-x trap-pos) TRAP)
    (logior! (rogue-trap-tr-flags tp) ISFOUND)
    (case trap-type
      (#.TRAPDOOR 
       (incf level)
       (new-level)
       (msg "You fell into a trap!"))
      (#.BEARTRAP
       (incf no-move BEARTIME)
       (msg "You are caught in a bear trap"))
      (#.SLEEPTRAP
       (incf no-command SLEEPTIME)
       (msg "A strange white mist envelops you and you fall asleep"))
      (#.ARROWTRAP
       (if (swing (1- (stats-s-lvl pstats))
                  (stats-s-arm pstats)
                  1)
           (progn
             (msg "Oh no! An arrow shot you")
             (when (< (decf (stats-s-hpt pstats) (roll 1 6))
                      1)
               (msg "The arrow killed you.")
               (death #\a)))
           (progn
             (msg "An arrow shoots past you.")
             (let ((obj (make-object)))
               (setf (object-o-type obj) WEAPON
                     (object-o-which obj) ARROW)
               (init-weapon obj ARROW)
               (setf (object-o-count obj) 1
                     (object-o-pos obj) hero
                     (object-o-hplus obj) 0
                     (object-o-dplus obj) 0)
               (fall obj nil)))))
      (#.TELTRAP 
       (teleport))
      (#.DARTTRAP
       (if (swing (1+ (stats-s-lvl pstats))
                  (stats-s-arm pstats)
                  1)
           (progn
             (msg "A small dart just hit you in the shoulder")
             (when (< (decf (stats-s-hpt pstats) 
                            (roll 1 4))
                      1)
               (msg "The dart killed you.")
               (death #\d))
             (unless (iswearing R-SUSTSTR)
               (chg-str -1)))
           (msg "A small dart whizzes by your ear and vanishes."))))
    (flush-type)                        ; flush typeahead  
    trap-type))

(defun trap-at (y x)
  "Find the trap at (y,x) on screen."
  (or
   (find (make-coord :x x :y y)
         traps
         :test #'equalp
         :key #'rogue-trap-tr-pos)
   (rogue-debug (format nil "Trap at ~d,~d not in array" y x))))

(defun object-at (y x)
  (find (make-coord :y y :x x)
        lvl-obj
        :key #'object-o-pos))

(defun rndmove (who)
  "Move in a random direction if the monster/person is confused."
  (let* ((ret (thing-t-pos who))
         (ey (1+ (coord-y ret)))
         (ex (1+ (coord-x ret)))
         (nopen 0))
    ;; Now go through the spaces surrounding the player and
    ;; set that place in the array to true if the space can be
    ;; moved into
    (for (y (1- (coord-y (thing-t-pos who))) 
            ey)
      (when (and (>= y 0)
                 (<  y cl-ncurses:*lines*))
        (for (x (1- (coord-x (thing-t-pos who))) 
                ex)
          (block continue
            (unless (or (minusp x)
                        (>= x cl-ncurses:*cols*))
              (let ((ch (winat y x)))
                (when (step-ok ch)
                  (let ((dest (make-coord :y y :x x)))
                    (when (diag-ok (thing-t-pos who) 
                                   dest)
                      (when (eql ch SCROLL)
                        (when-let (obj (object-at y x))
                          (when (eql (object-o-which obj) S-SCARE)
                            (return-from continue)))))
                    (when (zerop (rnd (incf nopen)))
                      (setf ret dest))))))))))
    ret))
