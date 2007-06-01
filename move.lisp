;;;; Hero movement commands
;;;; @(#)move.c       3.26 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defun do_run (ch)
  "Start the hero running."
  (setf running t
        *after* nil
        runch ch))

(defun do_move (dy dx)
  "Check to see that a move is legal.  If it is, handle the
consequences (fighting, picking up, etc."
  (setf firstmove nil)
  (when (plusp no_move)
    (decf no_move)
    (msg "You are still stuck in the bear trap")
    (return-from do_move))

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
            (not (diag_ok hero nh)))
    (setf *after* nil
          running nil)
    (return-from do_move))

  (when (and running 
             (ce hero nh))
    (setf *after* nil
          running nil))
  (let ((ch (winat (coord-y nh) (coord-x nh))))
    (when (and (on *player* ISHELD) 
               (not (eq ch #\F)))
      (msg "You are being held")
      (return-from do_move))
    (case ch
      ((#\Space #\| #\- #.SECRETDOOR)
       (setf *after* nil
             running nil)
       (return-from do_move))
      (#.TRAP
       (setf ch (be_trapped nh))
       (case ch
         ((#.TRAPDOOR #.TELTRAP) 
          (return-from do_move))))
      ((#.GOLD #.POTION #.SCROLL #.FOOD #.WEAPON
               #.ARMOR #.RING #.AMULET #.STICK)
       (setf running nil
             take ch)))

    (cond
      ((and (eq ch PASSAGE) 
            (eq (winat hero.y hero.x) DOOR))
       (light hero))
      ((eq ch DOOR)
       (setf running nil)
       (when (eq (winat hero.y hero.x) PASSAGE)
         (light nh)))
      ((eq ch STAIRS)
       (setf running nil))
      ((upper-case-p ch)
       (setf running nil)
       (fight nh ch cur_weapon nil)
       (return-from do_move)))

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
      (dotimes (j (coord-y (moor-r_max rp)))
        (dotimes (k (coord-x (moor-r_max rp)))
          (let* ((dx (+ (coord-x (moor-r_pos rp)) 
                        k))
                 (dy (+ (coord-y (moor-r_pos rp)) 
                        j))
                 (ch (show dy dx))
                 (darkp (logtest (moor-r_flags rp) ISDARK)))
            (cl-ncurses::wmove cw dy dx)
            ;; Figure out how to display a secret door.
            (when (eq ch SECRETDOOR)
              (setf ch
                    (if (or (zerop j)
                            (= j (1- (coord-y (moor-r_max rp)))))
                        #\-
                        #\|)))
            ;; If the room is a dark room, we might want to remove
            ;; monsters and the like from it (since they might
            ;; move).
            (when (upper-case-p ch)
              (let ((item (wake_monster dy dx)))
                (when (and (eq (thing-t_oldch item) #\Space)
                           (not darkp))
                  (setf (thing-t_oldch item) (rogue-mvwinch 
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
       (if (logtest (rogue-trap-tr_flags (trap_at y x)) ISFOUND)
           TRAP 
           FLOOR))
      ((#\M #\I)
       (let ((tp (find_mons y x)))
         (unless tp
           (msg "Can't find monster in show"))
         (if (eq ch #\M)
             (thing-t_disguise tp)
             (when (off *player* CANSEE)
               ;; Hide invisible monsters
               (rogue-mvwinch cl-ncurses:*stdscr* y x)))))
      (otherwise ch))))

(defun be_trapped (tc)
  "The guy stepped on a trap.... Make him pay."
  (let* ((tp        (trap_at (coord-y tc) (coord-x tc)))
         (trap-pos  (rogue-trap-tr_pos tp))
         (trap-type (rogue-trap-tr_type tp)))
    (setf *count* 0
          running nil)
    (rogue-mvwaddch cw (coord-y trap-pos) (coord-x trap-pos) TRAP)
    (logior! (rogue-trap-tr_flags tp) ISFOUND)
    (case trap-type
      (#.TRAPDOOR 
       (incf level)
       (new_level)
       (msg "You fell into a trap!"))
      (#.BEARTRAP
       (incf no_move BEARTIME)
       (msg "You are caught in a bear trap"))
      (#.SLEEPTRAP
       (incf no_command SLEEPTIME)
       (msg "A strange white mist envelops you and you fall asleep"))
      (#.ARROWTRAP
       (if (swing (1- (stats-s_lvl pstats))
                  (stats-s_arm pstats)
                  1)
           (progn
             (msg "Oh no! An arrow shot you")
             (when (< (decf (stats-s_hpt pstats) (roll 1 6))
                      1)
               (msg "The arrow killed you.")
               (death #\a)))
           (progn
             (msg "An arrow shoots past you.")
             (let ((obj (make-object)))
               (setf (object-o_type obj) WEAPON
                     (object-o_which obj) ARROW)
               (init_weapon obj ARROW)
               (setf (object-o_count obj) 1
                     (object-o_pos obj) hero
                     (object-o_hplus obj) 0
                     (object-o_dplus obj) 0)
               (fall obj nil)))))
      (#.TELTRAP 
       (teleport))
      (#.DARTTRAP
       (if (swing (1+ (stats-s_lvl pstats))
                  (stats-s_arm pstats)
                  1)
           (progn
             (msg "A small dart just hit you in the shoulder")
             (when (< (decf (stats-s_hpt pstats) 
                            (roll 1 4))
                      1)
               (msg "The dart killed you.")
               (death #\d))
             (unless (iswearing R_SUSTSTR)
               (chg_str -1)))
           (msg "A small dart whizzes by your ear and vanishes."))))
    (flush_type)                        ; flush typeahead  
    trap-type))

(defun trap_at (y x)
  "Find the trap at (y,x) on screen."
  (or
   (find (make-coord :x x :y y)
         traps
         :test #'equalp
         :key #'rogue-trap-tr_pos)
   (rogue-debug (format nil "Trap at ~d,~d not in array" y x))))

(defun object-at (y x)
  (find (make-coord :y y :x x)
        lvl_obj
        :key #'object-o_pos))

(defun rndmove (who)
  "Move in a random direction if the monster/person is confused."
  (let* ((ret (thing-t_pos who))
         (ey (1+ (coord-y ret)))
         (ex (1+ (coord-x ret)))
         (nopen 0))
    ;; Now go through the spaces surrounding the player and
    ;; set that place in the array to true if the space can be
    ;; moved into
    (for (y (1- (coord-y (thing-t_pos who))) 
            ey)
      (when (and (>= y 0)
                 (<  y cl-ncurses:*lines*))
        (for (x (1- (coord-x (thing-t_pos who))) 
                ex)
          (block continue
            (unless (or (minusp x)
                        (>= x cl-ncurses:*cols*))
              (let ((ch (winat y x)))
                (when (step_ok ch)
                  (let ((dest (make-coord :y y :x x)))
                    (when (diag_ok (thing-t_pos who) 
                                   dest)
                      (when (eq ch SCROLL)
                        (when-let (obj (object-at y x))
                          (when (eq (object-o_which obj) S_SCARE)
                            (return-from continue)))))
                    (when (zerop (rnd (incf nopen)))
                      (setf ret dest))))))))))
    ret))
