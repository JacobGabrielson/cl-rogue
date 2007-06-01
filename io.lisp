;;;; Various input/output functions
;;;;
;;;; @(#)io.c	3.10 (Berkeley) 6/15/81

(in-package :cl-rogue)

(define-resettable msgbuf (copy-seq ""))
(define-resettable newpos 0)

(defun msg (&rest args)
  "Display a message at the top of the screen."
  (case (length (first args))
    (0
     ;; If the string is "", just clear the line
     (cl-ncurses:wmove cw 0 0)
     (cl-ncurses:wclrtoeol cw)
     (zero! mpos))
    (otherwise
     ;; Add to the message and flush it out
     (apply #'doadd args)
     (endmsg))))

(defun addmsg (&rest args)
  "Add things to the current message."
  (apply #'doadd args))

(defun endmsg ()
  "Display a new msg (giving him a chance to see the previous one if it
is up there with the --More--)"
  (setf huh (copy-seq msgbuf))
  (unless (zerop mpos)
    (cl-ncurses:wmove cw 0 mpos)
    (cl-ncurses:waddstr cw "--More--")
    (draw cw)
    (wait_for #\Space))
  (cl-ncurses:mvwaddstr cw 0 0 msgbuf)
  (cl-ncurses:wclrtoeol cw)
  (setf mpos newpos
        newpos 0
        msgbuf (copy-seq ""))
  (cl-ncurses:wrefresh cw))

(defun doadd (&rest args)
  (setf msgbuf (concatenate 'string msgbuf (apply #'format nil args))
        newpos (length msgbuf)))

(defun step_ok (ch)
  "Returns true if it is ok to step on ch."
  (case ch
    ((#\Space #\| #\- #.SECRETDOOR) nil)
    (otherwise (not (alphanumericp ch)))))

(defvar *input-window* nil 
  "If non-nil, then READCHAR will use this window instead of
CW.")

(defun readchar ()
  "Flushes stdout so that screen is up to date and then returns
getchar."
  (let ((c (cl-ncurses:wgetch (or *input-window* cw))))
    (cond
      ((eq c cl-ncurses:err) nil)
      (t (code-char c)))))

(let ((buf "")
      (hpwidth 0)
      (s_hungry -1)
      (s_lvl -1)
      (s_pur 0)
      (s_hp -1)
      (s_str 0)
      (s_add 0)
      (s_ac 0)
      (s_exp 0))
  (defun status ()
    "Display the important stats line.  Keep the cursor where it
was."
    (unless 
        (and
         (= s_hp (stats-s_hpt pstats))
         (= s_exp (stats-s_exp pstats))
         (= s_pur purse)
         (= s_ac (if cur_armor 
                     (object-o_ac cur_armor)
                     (stats-s_arm pstats)))
         (= s_str (str_t-st_str (stats-s_str pstats)))
         (= s_add (str_t-st_add (stats-s_str pstats)))
         (= s_lvl level)
         (= s_hungry hungry_state))
      (let (oy ox temp)
        (cl-ncurses:getyx cw oy ox)
        (unless (= s_hp max_hp)
          (setf s_hp max_hp
                temp s_hp
                hpwidth 0)
          (do ()
              ((zerop (truncate temp)))
            (setf temp (/ temp 10))
            (incf hpwidth)))
        (setf buf (format nil 
                          "Level: ~d  Gold: ~5d  Hp: ~vd(~vd)  Str: ~2d"
                          level purse hpwidth (stats-s_hpt pstats) hpwidth max_hp
                          (str_t-st_str (stats-s_str pstats))))
        (when (plusp (str_t-st_add (stats-s_str pstats)))
          (setf buf (concatenate 'string buf 
                                 (format nil "/~d" (str_t-st_add (stats-s_str pstats))))))
        (setf buf (concatenate 'string buf 
                               (format nil "  Ac: ~2d  Exp: ~d/~d"
                                       (if cur_armor 
                                           (object-o_ac cur_armor)
                                           (stats-s_arm pstats))
                                       (stats-s_lvl pstats) (stats-s_exp pstats))))
        ;;
        ;; Save old status
        ;; 
        (setf s_lvl level
              s_pur purse
              s_hp (stats-s_hpt pstats)
              s_str (str_t-st_str (stats-s_str pstats))
              s_add (str_t-st_add (stats-s_str pstats))
              s_exp (stats-s_exp pstats)
              s_ac (if cur_armor 
                       (object-o_ac cur_armor)
                       (stats-s_arm pstats)))
        (cl-ncurses:mvwaddstr cw (1- cl-ncurses:*lines*) 0 buf)
        (cl-ncurses:waddstr cw (case hungry_state
                                  (1 "  Hungry")
                                  (2 "  Weak")
                                  (3 "  Fainting")))
        (cl-ncurses:wclrtoeol cw)
        (setf s_hungry hungry_state)
        (cl-ncurses:wmove cw oy ox)))))

(defun wait_for (ch)
  "Sit around until the guy types the right key."
  (do ((c nil (readchar)))
      ((if (eql ch #\Newline)
           (or (eql c #\Newline) (eql c #\Linefeed))
           (eql c ch)))))

(defun show_win (scr message)
  "Display a window and wait before returning."
  (cl-ncurses:mvwaddstr scr 0 0 message)
  (cl-ncurses:touchwin scr)
  (cl-ncurses:wmove scr hero.y hero.x)
  (draw scr)
  (wait_for #\Space)
  (cl-ncurses:clearok cw 1)
  (cl-ncurses:touchwin cw))

(defun flush_type ()
  (cl-ncurses:flushinp))
