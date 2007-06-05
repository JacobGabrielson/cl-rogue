;;;; Code for one object to chase another
;;;; @(#)chase.c	3.17 (Berkeley) 6/15/81

(in-package :cl-rogue)

(define-resettable ch_ret (make-coord)) ; where chasing takes you 

(defun runners (&optional arg)
  "Make all the running monsters move."
  (declare (ignore arg))
  (dolist (tp mlist)
    (when (and (off tp ISHELD) 
               (on tp ISRUN))
      (unless (and (or (off tp ISSLOW) 
                       (thing-t_turn tp))
                   (= (do_chase tp) -1))
        (unless (and (on tp ISHASTE)
                     (= (do_chase tp) -1))
          (setf (thing-t_turn tp) (not (thing-t_turn tp))))))))

(defun do_chase (th)
  "Make one thing chase another."
  (let ((rer (roomin (thing-t_pos th))) ; room of chaser, room of chasee 
        (ree (roomin (thing-t_dest th)))
        (mindist 32767) 
        dist
        (stoprun nil)                   ; t means we are there 
        sch
        (this (thing-t_dest th)))  ; temporary destination for chaser 
    ;; We don't count doors as inside rooms for this routine
    (when (eql (rogue-mvwinch cl-ncurses:*stdscr* (coord-y (thing-t_pos th)) (coord-x (thing-t_pos th)))
               DOOR)
      (setf rer nil))
    ;; If the object of our desire is in a different room than we are,
    ;; and we are not in a corridor, run to the door nearest to our
    ;; goal.
    (when (and rer 
               (not (eql rer ree)))
      (for (i 0 (1- (moor-r_nexits rer))) ; loop through doors
        (setf dist (distance (coord-y (thing-t_dest th))
                             (coord-x (thing-t_dest th))
                             (coord-y (aref (moor-r_exit rer) i))
                             (coord-x (aref (moor-r_exit rer) i))))
        (when (< dist mindist)          ; minimize distance
          (setf this (aref (moor-r_exit rer) i)
                mindist dist))))
    ;; THIS now contains what we want to run to this time so we run to
    ;; it.  If we hit it we either want to fight it or stop running.
    (if (not (chase th this))
        (if (equalp this hero)
            (return-from do_chase (attack th))
            (unless (eql (thing-t_type th) #\F)
              (setf stoprun t)))
        (when (eql (thing-t_type th) #\F)
          (return-from do_chase 0)))
    (rogue-mvwaddch cw
                    (coord-y (thing-t_pos th))
                    (coord-x (thing-t_pos th))
                    (thing-t_oldch th))
    (setf sch (rogue-mvwinch cw (coord-y ch_ret) (coord-x ch_ret)))
    (if (and rer
             (logtest (moor-r_flags rer) ISDARK)
             (eql sch FLOOR)
             (< (distance (coord-y ch_ret) 
                          (coord-x ch_ret) 
                          (coord-y (thing-t_pos th)) 
                          (coord-x (thing-t_pos th))) 3)
             (off *player* ISBLIND))
        (setf (thing-t_oldch th) #\Space)
        (setf (thing-t_oldch th) sch))

    (when (and (cansee (coord-y ch_ret) (coord-x ch_ret)) 
               (not (on th ISINVIS)))
      (rogue-mvwaddch cw 
                      (coord-y ch_ret) 
                      (coord-x ch_ret) 
                      (thing-t_type th)))

    (rogue-mvwaddch mw (coord-y (thing-t_pos th)) (coord-x (thing-t_pos th)) #\Space)
    (rogue-mvwaddch mw (coord-y ch_ret) (coord-x ch_ret) (thing-t_type th))
    (setf (thing-t_pos th) ch_ret)
    ;; And stop running if need be.
    (when (and stoprun
               (equalp (thing-t_pos th) (thing-t_dest th)))
      (logclr! (thing-t_flags th) ISRUN))
    0))

(defun runto (runner spot)
  "Set a monster running after something or stop it from
running (for when it dies)."
  (let ((tp (find_mons (coord-y runner) (coord-x runner))))
    ;; If we couldn't find him, something is funny
    (when (null tp)
      (msg "CHASER '~a'" (unctrl-char (winat (coord-y runner) (coord-x runner))))
      (rogue-done))
    ;; Start the beastie running
    (setf (thing-t_dest tp) spot)
    (logior! (thing-t_flags tp) ISRUN)
    (logclr! (thing-t_flags tp) ISHELD)))

(defun chase (tp ee)
  "Find the spot for the chaser(er) to move closer to the
chasee(ee).  Returns t if we want to keep on chasing later, nil
if we reach the goal."
  (let (dist 
        thisdist
        (er (thing-t_pos tp)))
    ;; If the thing is confused, let it move randomly. Invisible
    ;; Stalkers are slightly confused all of the time, and bats are
    ;; quite confused all the time
    (if (or 
         (and (on tp ISHUH)
              (< (rnd 10) 8))
         (and (eql (thing-t_type tp) #\I) 
              (< (rnd 100) 20))
         (and (eql (thing-t_type tp) #\B) 
              (< (rnd 100) 50)))
        ;; Get a valid random move
        (progn
          (setf ch_ret (rndmove tp)
                dist (distance (coord-y ch_ret) (coord-x ch_ret) 
                               (coord-y ee) (coord-x ee)))
          ;; Small chance that it will become un-confused 
          (when (< (rnd 1000) 50)
            (logclr! (thing-t_flags tp) ISHUH)))
        ;; Otherwise, find the empty spot next to the chaser that is
        ;; closest to the chasee.
        (progn 
          (let (ey ex)
            ;; This will eventually hold where we move to get closer
            ;; If we can't find an empty spot, we stay where we are.
            (setf dist (distance (coord-y er) (coord-x er) 
                                 (coord-y ee) (coord-x ee))
                  ch_ret er
                  ey (1+ (coord-y er))
                  ex (1+ (coord-x er)))
            (for (x (1- (coord-x er)) ex)
              (for (y (1- (coord-y er)) ey)
                (let ((tryp (make-coord :x x :y y)))
                  (when (diag_ok er tryp)
                    (let ((ch (winat y x)))
                      (when (step_ok ch)
                        ;; If it is a scroll, it might be a scare monster scroll
                        ;; so we need to look it up to see what type it is.
                        (when (or (not (eql ch SCROLL))
                                  (not (find-if
                                        #'(lambda (obj)
                                            (and (= y (coord-y (object-o_pos obj)))
                                                 (= x (coord-x (object-o_pos obj)))
                                                 (eql (object-o_which obj) S_SCARE)))
                                        lvl_obj)))
                          ;; If we didn't find any scrolls at this
                          ;; place or it wasn't a scare scroll, then
                          ;; this place counts
                          (setf thisdist (distance y x (coord-y ee) (coord-x ee)))
                          (when (< thisdist dist)
                            (setf ch_ret tryp
                                  dist thisdist))))))))))))
    (nonzerop dist)))

(defun roomin (cp)
  "Find what room some coordinates are in. NIL means they aren't
in any room."
  (find-if
   #'(lambda (rp)
       (inroom rp cp))
   rooms))

(defun find_mons (y x)
  "Find the monster from his corrdinates."
  (find-if
   #'(lambda (th)
       (and (= (coord-y (thing-t_pos th)) y)
            (= (coord-x (thing-t_pos th)) x)))
   mlist))

(defun diag_ok (sp ep)
  "Check to see if the move is legal if it is diagonal."
  (or (or (= (coord-x ep) (coord-x sp))
          (= (coord-y ep) (coord-y sp)))
      (and (step_ok (rogue-mvinch (coord-y ep) (coord-x sp)))
           (step_ok (rogue-mvinch (coord-y sp) (coord-x ep))))))

(defun cansee (y x)
  "Returns non-nil if the hero can see a certain coordinate."
  (unless (on *player* ISBLIND)
    (when-let (rer (roomin (make-coord :x x :y y)))
      ;; We can only see if the hero in the same room as the
      ;; coordinate and the room is lit or if it is close.
      (or (and rer
               (eql rer (roomin hero))
               (not (logtest (moor-r_flags rer) ISDARK)))
          (< (distance y x hero.y hero.x)
             3)))))
