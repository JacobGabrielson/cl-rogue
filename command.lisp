;;;; Read and execute the user commands
;;;; @(#)command.c	3.45 (Berkeley) 6/15/81

(in-package :cl-rogue)

(define-resettable countch nil)
(define-resettable direction nil)
(define-resettable newcount nil)

(defun command ()
  "Process the user commands."
  (let (ch
        (ntimes 1))                     ; number of player moves 
    (when (on *player* ishaste) 
      (incf ntimes))

    ;; Let the daemons start up
    (do-daemons before)
    (do-fuses before)

    (while (plusp ntimes)
      (decf ntimes)
      (look t)
      (unless running
        (setf door-stop nil))
      (status)
      (setf lastscore purse)
      (cl-charms/low-level:wmove cw hero.y hero.x)
      (unless (and (or running 
                       (plusp *count*)) 
                   jump)
        (draw cw))                      ; Draw screen 
      (setf take nil
            *after* t)
      ;; Read command or continue run
      (when wizard
        (setf waswizard t))
      (if (zerop no-command)
          (progn
            (if running
                (setf ch runch)
                (if (plusp *count*)
                    (setf ch countch)
                    (progn
                      (setf ch (readchar))
                      (when (and (not (zerop mpos)) (not running)) ; Erase message if it's there 
                        (msg ""))))))
          (setf ch #\Space))
      (if (plusp no-command)
          (when (zerop (decf no-command))
            (msg "You can move again."))
          (progn
            ;; Check for prefixes
            (when (digit-char-p ch)
              (setf *count* 0
                    newcount t)
              (while (digit-char-p ch)
                (setf *count* (+ (* *count* 10) (char-code ch) (- (char-code #\0)))
                      ch (readchar)))
              (setf countch ch)
              ;; Turn off count for commands which don't make sense to
              ;; repeat.
              (case ch
                ((#\h #\j #\k #\l
                      #\y #\u #\b #\n
                      #\H #\J #\K #\L
                      #\Y #\U #\B #\N
                      #\q #\r #\s #\f
                      #\t #\C #\I #\ 
                      #\z #\p) nil)
                (otherwise (setf *count* 0))))
            (case ch
              (#\f
               (unless (on *player* ISBLIND)
                 (setf door-stop t
                       firstmove t))
               (if (and (plusp *count*) (not newcount))
                   (setf ch direction)
                   (setf ch (readchar)))
               (case ch
                 ((#\h #\j #\k #\l #\y #\u #\b #\n)
                  (setf ch (char-upcase ch))))
               (setf direction ch)))
            (setf newcount nil)

            ;; Execute a command
            (when (and (plusp *count*) 
                       (not running))
              (decf *count*))
            (case ch
              (#\! (shell))
              (#\h (do-move 0 -1))
              (#\j (do-move 1 0))
              (#\k (do-move -1 0))
              (#\l (do-move 0 1))
              (#\y (do-move -1 -1))
              (#\u (do-move -1 1))
              (#\b (do-move 1 -1))
              (#\n (do-move 1 1))
              (#\H (do-run #\h))
              (#\J (do-run #\j))
              (#\K (do-run #\k))
              (#\L (do-run #\l))
              (#\Y (do-run #\y))
              (#\U (do-run #\u))
              (#\B (do-run #\b))
              (#\N (do-run #\n))
              (#\t
               (if (not (get-dir))
                   (setf *after* nil)
                   (missile delta.y delta.x)))
              (#\Q (setf *after* nil)
                   (rogue-quit -1))
              (#\i (setf *after* nil)
                   (inventory (thing-t-pack *player*) nil))
              (#\I (setf *after* nil)
                   (picky-inven))
              (#\d (drop))
              (#\q (quaff))
              (#\r (read-scroll))
              (#\e (eat))
              (#\w (wield))
              (#\W (wear))
              (#\T (take-off))
              (#\P (ring-on))
              (#\R (ring-off))
              (#\o (option))
              (#\c (call))
              (#\> (setf *after* nil)
                   (d-level))
              (#\< (setf *after* nil)
                   (u-level))
              (#\? (setf *after* nil)
                   (help))
              (#\/ (setf *after* nil)
                   (identify))
              (#\s (rogue-search))
              (#\z (do-zap nil))
              (#\p (if (get-dir)
                       (do-zap t)
                       (setf *after* nil)))
              (#\v (msg "Rogue version ~a. (mctesq was here)" release))
              (#.(ctrl #\L)
                 (setf *after* nil)
                 (cl-charms/low-level:clearok cl-charms/low-level:*curscr* cl-charms/low-level:true)
                 (draw cl-charms/low-level:*curscr*))
              (#.(ctrl #\R)
                 (setf *after* nil)
                 (msg huh))
              (#\S (setf *after* nil)
                   (when (save-game)
                     (cl-charms/low-level:wmove cw (1- cl-charms/low-level:*lines*) 0))
                   (cl-charms/low-level:wclrtoeol cw)
                   (draw cw)
                   (rogue-done))        ; throws
              (#\Space)                 ; Rest command 
              (#.(ctrl #\P)
                 (setf *after* nil)
                 (if wizard
                     (progn
                       (setf wizard nil)
                       (msg "Not wizard any more"))
                     (progn 
                       (if (equal wizard (passwd()))
                           (progn 
                             (msg "You are suddenly as smart as Ken Arnold in dungeon #~d" dnum)
                             (setf wizard t)
                             (setf waswizard t))
                           (msg "Sorry")))))
              (#\Escape
               (setf door-stop nil
                     *count* 0
                     *after* nil))
              (otherwise
               (setf *after* nil)
               (if wizard
                   (case ch
                     (#\@ (msg "@ ~d,~d" hero.y hero.x))
                     (#\C (create-obj))
                     (#.(ctrl #\I) (inventory lvl-obj nil))
                     (#.(ctrl #\W) (whatis))
                     (#.(ctrl #\D) (incf level) (new-level))
                     (#.(ctrl #\U) (decf level) (new-level))
                     (#.(ctrl #\F) (show-win cl-charms/low-level:*stdscr* "--More (level map)--"))
                     (#.(ctrl #\X) (show-win mw "--More (monsters)--"))
                     (#.(ctrl #\T) (teleport))
                     (#.(ctrl #\E) (msg "food left: ~d" food-left))
                     (#.(ctrl #\A) (msg "~d things in your pack" inpack))
                     (#.(ctrl #\C) (add-pass))
                     (#.(ctrl #\N) (when-let (obj (get-item "charge" STICK))
                                     (setf (object-o-charges obj) 10000)))
                     (#.(ctrl #\H)
                        (dotimes (i 9) (raise-level))
                        ;; Give the rogue a sword (+1,+1)
                        (let ((obj (make-object :o-type WEAPON 
                                                :o-which TWOSWORD)))
                          (init-weapon obj SWORD)
                          (setf (object-o-hplus obj) 1
                                (object-o-dplus obj) 1)
                          (add-pack obj t)
                          (setf cur-weapon obj))
                        ;; And his suit of armor
                        (let ((obj (make-object :o-type ARMOR 
                                                :o-which PLATE-MAIL
                                                :o-ac -5
                                                :o-flags ISKNOW)))
                          (setf cur-armor obj)
                          (add-pack obj t)))
                     (otherwise
                      (msg "Illegal command '~a'." (unctrl-char ch))
                      (setf *count* 0)))
                   (progn
                     (msg "Illegal command '~a'." (unctrl-char ch))
                     (setf *count* 0)))))
            ;; Turn off flags if no longer needed
            (unless running
              (setf door-stop nil))))
      ;; If he ran into something to take, let him pick it up.
      (when take
        (pick-up take))
      (unless running
        (setf door-stop nil)))

    ;; Kick off the rest if the daemons and fuses
    (when *after*
      (look nil)
      (do-daemons AFTER)
      (do-fuses AFTER)
      (if (isring LEFT R-SEARCH)
          (rogue-search)
          (when (and (isring LEFT R-TELEPORT) (< (rnd 100) 2))
            (teleport)))
      (if (isring RIGHT R-SEARCH)
          (rogue-search)
          (when (and (isring LEFT R-TELEPORT) (< (rnd 100) 2))
            (teleport))))))

(defun rogue-quit (p)
  "Have player make certain, then exit."
  (declare (ignore p))
  ;;
  ;; Reset the signal in case we got here via an interrupt
  ;;
  ;; not implementing this for now --jag (XXX)
  ;;if (signal(SIGINT, quit) != quit)
  ;;mpos = 0;
  (msg "Really quit?")
  (draw cw)
  (cond
    ((eql (readchar) #\y)
     (cl-charms/low-level:clear)
     (cl-charms/low-level:move (1- cl-charms/low-level:*lines*) 0)
     (draw cl-charms/low-level:*stdscr*)
     (score purse 1)
     (abort))
    (t 
     ;;(signal(SIGINT, quit);
     (cl-charms/low-level:wmove cw 0 0)
     (cl-charms/low-level:wclrtoeol cw)
     (status)
     (draw cw)
     (zero! mpos
            *count*))))

(defun rogue-search ()
  "Player gropes about him to find hidden things."
  ;; Look all around the hero, if there is something hidden there,
  ;; give him a chance to find it.  If its found, display it.
  (when (on *player* ISBLIND)
    (return-from rogue-search))
  (for (x (1- hero.x) (1+ hero.x))
    (for (y (1- hero.y) (1+ hero.y))
      (case (winat y x)
        (#.SECRETDOOR
         (when (< (rnd 100) 20)
           (rogue-mvaddch y x DOOR)
           (setf *count* 0)))
        (#.TRAP
         (unless (or (eql (rogue-mvwinch cw y x) TRAP)
                     (> (rnd 100) 50))
           (let ((tp (trap-at y x)))
             (logior! (rogue-trap-tr-flags tp) ISFOUND)
             (rogue-mvwaddch cw y x TRAP)
             (setf *count* 0
                   running nil)
             (msg (tr-name (rogue-trap-tr-type tp))))))))))

(defun help ()
  "Give single character help, or the whole mess if he wants it."
  (msg "Character you want help for (* for all): ")
  (let ((helpch (readchar)))
    (zero! mpos)
    ;; If it's not a *, print the right help string
    ;; or an error if he typed a funny character.
    (unless (eql helpch #\*)
      (cl-charms/low-level:wmove cw 0 0)
      (let ((help-description (cdr (assoc helpch helpstr))))
        (if help-description
            (msg "~a~a" helpch help-description)
            (msg "Unknown character '~c'" helpch))
        (return-from help)))
    ;; Here we print help for everything.
    ;; Then wait before we return to command mode
    (cl-charms/low-level:wclear hw)
    (let ((cnt 0))
      (map nil 
           #'(lambda (help-pair)
               (let ((key (car help-pair))
                     (help-description (cdr help-pair)))
                 (cl-charms/low-level:mvwaddstr hw (mod cnt 23) (if (> cnt 22) 40 0) (unctrl-char key))
                 (cl-charms/low-level:waddstr hw help-description)
                 (incf cnt)))
           helpstr))
    (cl-charms/low-level:wmove hw (1- cl-charms/low-level:*lines*) 0)
    (cl-charms/low-level:wprintw hw "--Press space to continue--")
    (draw hw)
    (wait-for #\Space)
    (cl-charms/low-level:wclear hw)
    (draw hw)
    (cl-charms/low-level:wmove cw 0 0)
    (cl-charms/low-level:wclrtoeol cw)
    (status)
    (cl-charms/low-level:touchwin cw)))

(defun identify ()
  "Tell the player what a certain thing is."
  (msg "What do you want identified? ")
  (let ((ch (readchar)))
    (zero! mpos)
    (when (eql ch #\Escape)
      (msg "")
      (return-from identify))
    (msg
     "'~a' : ~a"
     (unctrl-char ch)
     (if (and (alpha-char-p ch) (upper-case-p ch))
         (monster-m-name (char-monster ch))
         (case ch
           ((#\| #\-) "wall of a room")
           (#.GOLD "gold")
           (#.STAIRS "passage leading down")
           (#.DOOR "door")
           (#.THE-FLOOR "room floor")
           (#.PLAYER "you")
           (#.PASSAGE "passage")
           (#.TRAP "trap")
           (#.POTION "potion")
           (#.SCROLL "scroll")
           (#.FOOD "food")
           (#.WEAPON "weapon")
           (#\Space "solid rock")
           (#.ARMOR "armor")
           (#.AMULET "The Amulet of Yendor")
           (#.RING "ring")
           (#.STICK "wand or staff")
           (otherwise "unknown character"))))))

(defun d-level ()
  "He wants to go down a level."
  (if (not (eql (winat hero.y hero.x) STAIRS))
      (msg "I see no way down.")
      (progn
        (incf level)
        (new-level))))

(defun u-level ()
  "He wants to go up a level."
  (if (and (eql (winat hero.y hero.x) STAIRS)
           amulet)
      (progn
        (decf level)
        (when (zerop level)
          (total-winner))
        (new-level)
        (msg "You feel a wrenching sensation in your gut."))
      (msg "I see no way up.")))


(defun shell())
;; shell()
;; * Let him escape for a while
;; {
;;    register int pid;
;;    register char *sh;
;;    int ret-status;

;;    
;;     * Set the terminal back to original mode
;;     
;;    sh = getenv("SHELL");
;;    wclear(hw);
;;    wmove(hw, LINES-1, 0);
;;    draw(hw);
;;    endwin();
;;    in-shell = TRUE;
;;    fflush(stdout);
;;    
;;     * Fork and do a shell
;;     
;;    while((pid = fork()) < 0)
;;        sleep(1);
;;    if (pid == 0)
;;    {
;;        
;;         * Set back to original user, just in case
;;         
;;        setuid(getuid());
;;        setgid(getgid());
;;        execl(sh == NULL ? "/bin/sh" : sh, "shell", "-i", 0);
;;        perror("No shelly");
;;        exit(-1);
;;    }
;;    else
;;    {
;;        signal(SIGINT, SIG-IGN);
;;        signal(SIGQUIT, SIG-IGN);
;;        while (wait(&ret-status) != pid)
;;            continue;
;;        signal(SIGINT, endit);
;;        signal(SIGQUIT, endit);
;;        printf("\n[Press return to continue]");
;;        noecho();
;;        (cl-charms/low-level:cbreak);
;;        in-shell = FALSE;
;;        wait-for('\n');
;;        clearok(cw, TRUE);
;;        touchwin(cw);
;;    }
;; }

(defun call ()
  "Allow a user to call a potion, scroll, or ring something."
  ;; Make certain that it is something that we want to wear.
  (when-let (obj (get-item "call" CALLABLE))
    (multiple-value-bind (guess know elsewise)
        (case (object-o-type obj)
          (#.RING
           (values r-guess
                   r-know
                   (if (aref r-guess (object-o-which obj))
                       (aref r-guess (object-o-which obj))
                       (aref r-stones (object-o-which obj)))))
          (#.POTION
           (values p-guess
                   p-know
                   (if (aref p-guess (object-o-which obj))
                       (aref p-guess (object-o-which obj))
                       (aref p-colors (object-o-which obj)))))
          (#.SCROLL
           (values s-guess
                   s-know
                   (if (aref s-guess (object-o-which obj))
                       (aref s-guess (object-o-which obj))
                       (aref s-names (object-o-which obj)))))
          (#.STICK
           (values ws-guess
                   ws-know
                   (if (aref ws-guess (object-o-which obj))
                       (aref ws-guess (object-o-which obj))
                       (aref ws-made (object-o-which obj)))))
          (otherwise
           (msg "You can't call that anything")
           (return-from call)))
      (when (aref know (object-o-which obj))
        (msg "That has already been identified")
        (return-from call))
      (if terse
          (addmsg "C")
          (addmsg "Was c"))
      (msg "alled \"~a\"" elsewise)
      (if terse
          (msg "Call it: ")
          (msg "What do you want to call it? "))
      (multiple-value-bind (opt status)
          (get-str elsewise cw)
        (when (eql status NORM)
          (setf (aref guess (object-o-which obj)) opt))))))
