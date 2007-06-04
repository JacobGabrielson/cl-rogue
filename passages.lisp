;;;; Draw the connecting passages
;;;; @(#)passages.c	3.4 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defstruct rdes
  conn                               ; possible to connect to room i? 
  isconn                            ; connection been made to room i? 
  ingraph)                              ; this room in graph already? 

(defun do_passages ()
  "Draw all the passages on a level."
  (let ((rdes-array
         (vector (make-rdes :conn #*010100000 :isconn #*000000000 :ingraph nil)
                 (make-rdes :conn #*101010000 :isconn #*000000000 :ingraph nil)
                 (make-rdes :conn #*010001000 :isconn #*000000000 :ingraph nil)
                 (make-rdes :conn #*100010100 :isconn #*000000000 :ingraph nil)
                 (make-rdes :conn #*010101010 :isconn #*000000000 :ingraph nil)
                 (make-rdes :conn #*001010001 :isconn #*000000000 :ingraph nil)
                 (make-rdes :conn #*000100010 :isconn #*000000000 :ingraph nil)
                 (make-rdes :conn #*000010101 :isconn #*000000000 :ingraph nil)
                 (make-rdes :conn #*000001010 :isconn #*000000000 :ingraph nil))))
    ;; Starting with one room, connect it to a random adjacent room
    ;; and then pick a new room to start with.
    (let* ((roomcount 1)
           (r1-index (rnd MAXROOMS))
           (r1 (aref rdes-array r1-index)))
      (setf (rdes-ingraph r1) t)
      (loop
         ;; Find a room to connect with
         (let ((j 0)
               r2-index
               r2)
           (dotimes (i MAXROOMS)
             (when (and (nonzerop (bit (rdes-conn r1) i))
                        (not (rdes-ingraph (aref rdes-array i)))
                        (zerop (rnd (incf j))))
               (setf r2-index i
                     r2 (aref rdes-array r2-index))))
           ;; If no adjacent rooms are outside the graph, pick a new
           ;; room to look from
           (if (zerop j)
               (loop
                  (setf r1-index (rnd MAXROOMS)
                        r1 (aref rdes-array r1-index))
                  (when (rdes-ingraph r1) 
                    (return)))
               ;; otherwise, connect new room to the graph, and draw a
               ;; tunnel to it
               (progn
                 (setf (rdes-ingraph r2) t)
                 (conn r1-index r2-index)
                 (setf (aref (rdes-isconn r1) r2-index) 1
                       (aref (rdes-isconn r2) r1-index) 1)
                 (incf roomcount))))
         (unless (< roomcount MAXROOMS) 
           (return)))
      ;; Attempt to add passages to the graph a random number of times
      ;; so that there isn't just one unique passage through it.
      (loop repeat (rnd 5)
         do
         (let ((j 0)
               r2-index
               r2)
           (setf r1-index (rnd MAXROOMS)
                 r1 (aref rdes-array r1-index))
           ;; Find an adjacent room not already connected.
           (dotimes (i MAXROOMS)
             (when (and (nonzerop (bit (rdes-conn   r1) i))
                        (zerop    (bit (rdes-isconn r1) i))
                        (zerop    (rnd (incf j))))
               (setf r2-index i
                     r2 (aref rdes-array r2-index))))
           ;; If there is one, connect it and look for the next
           ;; added passage
           (unless (zerop j)
             (conn r1-index r2-index)
             (setf (aref (rdes-isconn r1) r2-index) 1
                   (aref (rdes-isconn r2) r1-index) 1)))))))

(defun conn (r1 r2)
  "Draw a corridor from a room in a certain direction."
  (let* ((rm (if (< r1 r2) r1 r2))
         (other-rm (if (not (< r1 r2)) r1 r2))
         (direc (if (= (1+ rm)
                       other-rm)
                    #\r #\d))
         (rpf (aref rooms rm))
         rpt epos spos rmt distance turn_delta turn_distance turn_spot)
    ;; Set up the movement variables, in two cases: first drawing one
    ;; down.
    (case direc
      (#\d
       (setf rmt (+ rm 3)                ; room # of dest
             rpt (aref rooms rmt)        ; room pointer of dest
             delta (make-coord :y 1) ; direction of move
             spos (copy-structure (moor-r_pos rpf))  ; start of move
             epos (copy-structure (moor-r_pos rpt))) ; end of move
       (unless (logtest (moor-r_flags rpf) ISGONE) ; if not gone pick door pos
         (incf (coord-x spos) (1+ (rnd (- (coord-x (moor-r_max rpf))
                                          2))))
         (incf (coord-y spos) (1- (coord-y (moor-r_max rpf)))))
       (unless (logtest (moor-r_flags rpt) ISGONE)
         (incf (coord-x epos) (1+ (rnd (- (coord-x (moor-r_max rpt)) 
                                          2)))))
       (setf distance (1- (abs (- (coord-y spos)
                                  (coord-y epos)))) ; distance to move
             turn_delta (make-coord     ; direction to turn
                         :x (if (< (coord-x spos) 
                                   (coord-x epos)) 
                                1 -1))
             turn_distance (abs (- (coord-x spos) ; how far to turn
                                   (coord-x epos)))
             turn_spot (1+ (rnd (1- distance))))) ; where turn starts
      (#\r                              ; setup for moving right 
       (setf rmt (1+ rm)
             rpt (aref rooms rmt)
             delta (make-coord :x 1)
             spos (copy-structure (moor-r_pos rpf))  ; start of move
             epos (copy-structure (moor-r_pos rpt))) ; end of move
       (unless (logtest (moor-r_flags rpf) ISGONE) ; if not gone pick door pos
         (incf (coord-x spos) (1- (coord-x (moor-r_max rpf))))
         (incf (coord-y spos) (1+ (rnd (- (coord-y (moor-r_max rpf)) 
                                          2)))))
       (unless (logtest (moor-r_flags rpt) ISGONE)
         (incf (coord-y epos) (1+ (rnd (- (coord-y (moor-r_max rpt)) 
                                          2)))))
       (setf distance (1- (abs (- (coord-x spos)
                                  (coord-x epos)))) ; distance to move
             turn_delta (make-coord     ; direction to turn
                         :y (if (< (coord-y spos) 
                                   (coord-y epos)) 
                                1 -1))
             turn_distance (abs (- (coord-y spos) ; how far to turn
                                   (coord-y epos)))
             turn_spot (1+ (rnd (1- distance)))))
      (otherwise (rogue-debug "error in connection tables")))
    ;; Draw in the doors on either side of the passage or just put #'s
    ;; if the rooms are gone.
    (if (not (logtest (moor-r_flags rpf) ISGONE))
        (add-door rpf spos)
        (progn
          (cmov spos)
          (rogue-addch #\#)))
    (if (not (logtest (moor-r_flags rpt) ISGONE))
        (add-door rpt epos)
        (progn
          (cmov epos)
          (rogue-addch #\#)))
    ;; Get ready to move...
    (let ((curr (copy-structure spos)))
      (while (nonzerop distance)
        ;; Move to new position
        (incf (coord-x curr) delta.x)
        (incf (coord-y curr) delta.y)
        ;; Check if we are at the turn place, if so do the turn
        (when (and (= distance turn_spot)
                   (plusp turn_distance))
          (while (plusp turn_distance)
            (decf turn_distance)
            (cmov curr)
            (rogue-addch PASSAGE)
            (incf (coord-x curr) (coord-x turn_delta))
            (incf (coord-y curr) (coord-y turn_delta))))
        ;; Continue digging along
        (cmov curr)
        (rogue-addch PASSAGE)
        (decf distance))
      (incf (coord-x curr) delta.x)
      (incf (coord-y curr) delta.y)
      (unless (equalp curr epos)
        (msg "Warning, connectivity problem on this level.")))))

(defun add-door (rm cp)
  "Add a door or possibly a secret door also enters the door in
the exits array of the room."
  (cmov cp)
  (rogue-addch (if (and (< (rnd 10) (1- level))
                        (< (rnd 100) 20))
                   SECRETDOOR DOOR))
  (setf (aref (moor-r_exit rm) (moor-r_nexits rm)) (copy-structure cp))
  (incf (moor-r_nexits rm)))

(defun add_pass ()
  "Add the passages to the current window (wizard command)."
  (for (y 1 (- cl-ncurses:*lines* 3))
    (for (x 0 (1- cl-ncurses:*cols*))
      (let ((ch (rogue-mvinch y x)))
        (case ch 
          ((#.PASSAGE #.DOOR #.SECRETDOOR)
           (rogue-mvwaddch cw y x ch)))))))
