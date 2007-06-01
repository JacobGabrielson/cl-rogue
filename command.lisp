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
    (do_daemons before)
    (do_fuses before)

    (while (plusp ntimes)
      (decf ntimes)
      (look t)
      (unless running
        (setf door_stop nil))
      (status)
      (setf lastscore purse)
      (cl-ncurses:wmove cw hero.y hero.x)
      (unless (and (or running (plusp *count*)) jump)
        (draw cw))                      ; Draw screen 
      (setf take nil
            *after* t)
      ;; Read command or continue run
      (when wizard
        (setf waswizard t))
      (if (zerop no_command)
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
      (if (plusp no_command)
          (when (zerop (decf no_command))
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
                 (setf door_stop t
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
            (when (and (plusp *count*) (not running))
              (decf *count*))
            (case ch
              (#\! (shell))
              (#\h (do_move 0 -1))
              (#\j (do_move 1 0))
              (#\k (do_move -1 0))
              (#\l (do_move 0 1))
              (#\y (do_move -1 -1))
              (#\u (do_move -1 1))
              (#\b (do_move 1 -1))
              (#\n (do_move 1 1))
              (#\H (do_run #\h))
              (#\J (do_run #\j))
              (#\K (do_run #\k))
              (#\L (do_run #\l))
              (#\Y (do_run #\y))
              (#\U (do_run #\u))
              (#\B (do_run #\b))
              (#\N (do_run #\n))
              (#\t
               (if (not (get_dir))
                   (setf *after* nil)
                   (missile delta.y delta.x)))
              (#\Q (setf *after* nil)
                   (rogue-quit -1))
              (#\i (setf *after* nil)
                   (inventory (thing-t_pack *player*) nil))
              (#\I (setf *after* nil)
                   (picky_inven))
              (#\d (drop))
              (#\q (quaff))
              (#\r (read_scroll))
              (#\e (eat))
              (#\w (wield))
              (#\W (wear))
              (#\T (take_off))
              (#\P (ring_on))
              (#\R (ring_off))
              (#\o (option))
              (#\c (call))
              (#\> (setf *after* nil)
                   (d_level))
              (#\< (setf *after* nil)
                   (u_level))
              (#\? (setf *after* nil)
                   (help))
              (#\/ (setf *after* nil)
                   (identify))
              (#\s (rogue-search))
              (#\z (do_zap nil))
              (#\p (if (get_dir)
                       (do_zap t)
                       (setf *after* nil)))
              (#\v (msg "Rogue version ~a. (mctesq was here)" release))
              (#.(ctrl #\L)
                 (setf *after* nil)
                 (cl-ncurses:clearok cl-ncurses:*curscr* cl-ncurses:true)
                 (draw cl-ncurses:*curscr*))
              (#.(ctrl #\R)
                 (setf *after* nil)
                 (msg huh))
              (#\S (setf *after* nil)
                   (when (save_game)
                     (cl-ncurses:wmove cw (1- cl-ncurses:*lines*) 0))
                   (cl-ncurses:wclrtoeol cw)
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
               (setf door_stop nil
                     *count* 0
                     *after* nil))
              (otherwise
               (setf *after* nil)
               (if wizard
                   (case ch
                     (#\@ (msg "@ ~d,~d" hero.y hero.x))
                     (#\C (create_obj))
                     (#.(ctrl #\I) (inventory lvl_obj nil))
                     (#.(ctrl #\W) (whatis))
                     (#.(ctrl #\D) (incf level) (new_level))
                     (#.(ctrl #\U) (decf level) (new_level))
                     (#.(ctrl #\F) (show_win cl-ncurses:*stdscr* "--More (level map)--"))
                     (#.(ctrl #\X) (show_win mw "--More (monsters)--"))
                     (#.(ctrl #\T) (teleport))
                     (#.(ctrl #\E) (msg "food left: ~d" food_left))
                     (#.(ctrl #\A) (msg "~d things in your pack" inpack))
                     (#.(ctrl #\C) (add_pass))
                     (#.(ctrl #\N) (when-let (obj (get_item "charge" STICK))
                                     (setf (object-o_charges obj) 10000)))
                     (#.(ctrl #\H)
                        (dotimes (i 9) (raise_level))
                        ;; Give the rogue a sword (+1,+1)
                        (let ((obj (make-object :o_type WEAPON 
                                                :o_which TWOSWORD)))
                          (init_weapon obj SWORD)
                          (setf (object-o_hplus obj) 1
                                (object-o_dplus obj) 1)
                          (add_pack obj t)
                          (setf cur_weapon obj))
                        ;; And his suit of armor
                        (let ((obj (make-object :o_type ARMOR 
                                                :o_which PLATE_MAIL
                                                :o_ac -5
                                                :o_flags ISKNOW)))
                          (setf cur_armor obj)
                          (add_pack obj t)))
                     (otherwise
                      (msg "Illegal command '~a'." (unctrl-char ch))
                      (setf *count* 0)))
                   (progn
                     (msg "Illegal command '~a'." (unctrl-char ch))
                     (setf *count* 0)))))
            ;; Turn off flags if no longer needed
            (unless running
              (setf door_stop nil))))
      ;; If he ran into something to take, let him pick it up.
      (when take
        (pick_up take))
      (unless running
        (setf door_stop nil)))
    ;; Kick off the rest if the daemons and fuses
    (when *after*
      (look nil)
      (do_daemons AFTER)
      (do_fuses AFTER)
      (if (isring LEFT R_SEARCH)
          (rogue-search)
          (when (and (isring LEFT R_TELEPORT) (< (rnd 100) 2))
            (teleport)))
      (if (isring RIGHT R_SEARCH)
          (rogue-search)
          (when (and (isring LEFT R_TELEPORT) (< (rnd 100) 2))
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
    ((eq (readchar) #\y)
     (cl-ncurses:clear)
     (cl-ncurses:move (1- cl-ncurses:*lines*) 0)
     (draw cl-ncurses:*stdscr*)
     (score purse 1)
     (abort))
    (t 
     ;;(signal(SIGINT, quit);
     (cl-ncurses:wmove cw 0 0)
     (cl-ncurses:wclrtoeol cw)
     (status)
     (draw cw)
     (zero! mpos
            *count*))))

(defun rogue-search ()
  "Player gropes about him to find hidden things."
  ;;
  ;; Look all around the hero, if there is something hidden there,
  ;; give him a chance to find it.  If its found, display it.
  ;;
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
         (unless (or (eq (rogue-mvwinch cw y x) TRAP)
                     (> (rnd 100) 50))
           (let ((tp (trap_at y x)))
             (logior! (rogue-trap-tr_flags tp) ISFOUND)
             (rogue-mvwaddch cw y x TRAP)
             (setf *count* 0
                   running nil)
             (msg (tr_name (rogue-trap-tr_type tp))))))))))

(defun help ()
  "Give single character help, or the whole mess if he wants it."
  (msg "Character you want help for (* for all): ")
  (let ((helpch (readchar)))
    (setf mpos 0)
    ;;
    ;; If it's not a *, print the right help string
    ;; or an error if he typed a funny character.
    ;;
    (unless (eq helpch #\*)
      (cl-ncurses:wmove cw 0 0)
      (let ((help-description (cdr (assoc helpch helpstr))))
        (if help-description
            (msg "~a~a" helpch help-description)
            (msg "Unknown character '~c'" helpch))
        (return-from help)))
    ;;
    ;; Here we print help for everything.
    ;; Then wait before we return to command mode
    ;;
    (cl-ncurses:wclear hw)
    (let ((cnt 0))
      (map nil 
           #'(lambda (help-pair)
               (let ((key (car help-pair))
                     (help-description (cdr help-pair)))
                 (cl-ncurses:mvwaddstr hw (mod cnt 23) (if (> cnt 22) 40 0) (unctrl-char key))
                 (cl-ncurses:waddstr hw help-description)
                 (incf cnt)))
           helpstr))
    (cl-ncurses:wmove hw (1- cl-ncurses:*lines*) 0)
    (cl-ncurses:wprintw hw "--Press space to continue--")
    (draw hw)
    (wait_for #\Space)
    (cl-ncurses:wclear hw)
    (draw hw)
    (cl-ncurses:wmove cw 0 0)
    (cl-ncurses:wclrtoeol cw)
    (status)
    (cl-ncurses:touchwin cw)))

(defun identify ()
  "Tell the player what a certain thing is."
  (msg "What do you want identified? ")
  (let ((ch (readchar)))
    (setf mpos 0)
    (when (eq ch #\Escape)
      (msg "")
      (return-from identify))
    (msg
     "'~a' : ~a"
     (unctrl-char ch)
     (if (and (alpha-char-p ch) (upper-case-p ch))
         (monster-m_name (char-monster ch))
         (case ch
           ((#\| #\-) "wall of a room")
           (#.GOLD "gold")
           (#.STAIRS "passage leading down")
           (#.DOOR "door")
           (#.FLOOR "room floor")
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

(defun d_level ()
  "He wants to go down a level."
  (if (not (eq (winat hero.y hero.x) STAIRS))
      (msg "I see no way down.")
      (progn
        (incf level)
        (new_level))))

(defun u_level ()
  "He wants to go up a level."
  (if (and (eq (winat hero.y hero.x) STAIRS)
           amulet)
      (progn
        (decf level)
        (when (zerop level)
          (total_winner))
        (new_level)
        (msg "You feel a wrenching sensation in your gut."))
      (msg "I see no way up.")))


(defun shell())
;; shell()
;; * Let him escape for a while
;; {
;;    register int pid;
;;    register char *sh;
;;    int ret_status;

;;    
;;     * Set the terminal back to original mode
;;     
;;    sh = getenv("SHELL");
;;    wclear(hw);
;;    wmove(hw, LINES-1, 0);
;;    draw(hw);
;;    endwin();
;;    in_shell = TRUE;
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
;;        signal(SIGINT, SIG_IGN);
;;        signal(SIGQUIT, SIG_IGN);
;;        while (wait(&ret_status) != pid)
;;            continue;
;;        signal(SIGINT, endit);
;;        signal(SIGQUIT, endit);
;;        printf("\n[Press return to continue]");
;;        noecho();
;;        (cl-ncurses:cbreak);
;;        in_shell = FALSE;
;;        wait_for('\n');
;;        clearok(cw, TRUE);
;;        touchwin(cw);
;;    }
;; }

(defun call ()
  "Allow a user to call a potion, scroll, or ring something."
  ;; Make certain that it is something that we want to wear.
  (when-let (obj (get_item "call" CALLABLE))
    (multiple-value-bind (guess know elsewise)
        (case (object-o_type obj)
          (#.RING
           (values r_guess
                   r_know
                   (if (aref r_guess (object-o_which obj))
                       (aref r_guess (object-o_which obj))
                       (aref r_stones (object-o_which obj)))))
          (#.POTION
           (values p_guess
                   p_know
                   (if (aref p_guess (object-o_which obj))
                       (aref p_guess (object-o_which obj))
                       (aref p_colors (object-o_which obj)))))
          (#.SCROLL
           (values s_guess
                   s_know
                   (if (aref s_guess (object-o_which obj))
                       (aref s_guess (object-o_which obj))
                       (aref s_names (object-o_which obj)))))
          (#.STICK
           (values ws_guess
                   ws_know
                   (if (aref ws_guess (object-o_which obj))
                       (aref ws_guess (object-o_which obj))
                       (aref ws_made (object-o_which obj)))))
          (otherwise
           (msg "You can't call that anything")
           (return-from call)))
      (when (aref know (object-o_which obj))
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
          (get_str elsewise cw)
        (when (eq status NORM)
          (setf (aref guess (object-o_which obj)) opt))))))
