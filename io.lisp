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
     (cl-charms/low-level:wmove cw 0 0)
     (cl-charms/low-level:wclrtoeol cw)
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
    (cl-charms/low-level:wmove cw 0 mpos)
    (cl-charms/low-level:waddstr cw "--More--")
    (draw cw)
    (wait-for #\Space))
  (cl-charms/low-level:mvwaddstr cw 0 0 msgbuf)
  (cl-charms/low-level:wclrtoeol cw)
  (setf mpos newpos
        newpos 0
        msgbuf (copy-seq ""))
  (cl-charms/low-level:wrefresh cw))

(defun doadd (&rest args)
  (setf msgbuf (concatenate 'string msgbuf (apply #'format nil args))
        newpos (length msgbuf)))

(defun step-ok (ch)
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
  (let ((c (cl-charms/low-level:wgetch (or *input-window* cw))))
    (cond
      ((eql c cl-charms/low-level:err) nil)
      (t (code-char c)))))

(let ((buf "")
      (hpwidth 0)
      (s-hungry -1)
      (s-lvl -1)
      (s-pur 0)
      (s-hp -1)
      (s-str 0)
      (s-add 0)
      (s-ac 0)
      (s-exp 0))
  (defun status ()
    "Display the important stats line.  Keep the cursor where it
was."
    (unless 
        (and
         (= s-hp (stats-s-hpt pstats))
         (= s-exp (stats-s-exp pstats))
         (= s-pur purse)
         (= s-ac (if cur-armor 
                     (object-o-ac cur-armor)
                     (stats-s-arm pstats)))
         (= s-str (str-t-st-str (stats-s-str pstats)))
         (= s-add (str-t-st-add (stats-s-str pstats)))
         (= s-lvl level)
         (= s-hungry hungry-state))
      (let (oy ox temp)
        (cl-charms/low-level:getyx cw oy ox)
        (unless (= s-hp max-hp)
          (setf s-hp max-hp
                temp s-hp
                hpwidth 0)
          (do ()
              ((zerop (truncate temp)))
            (setf temp (/ temp 10))
            (incf hpwidth)))
        (setf buf (format nil 
                          "Level: ~d  Gold: ~5d  Hp: ~vd(~vd)  Str: ~2d"
                          level purse hpwidth (stats-s-hpt pstats) hpwidth max-hp
                          (str-t-st-str (stats-s-str pstats))))
        (when (plusp (str-t-st-add (stats-s-str pstats)))
          (setf buf (concatenate 'string buf 
                                 (format nil "/~d" (str-t-st-add (stats-s-str pstats))))))
        (setf buf (concatenate 'string buf 
                               (format nil "  Ac: ~2d  Exp: ~d/~d"
                                       (if cur-armor 
                                           (object-o-ac cur-armor)
                                           (stats-s-arm pstats))
                                       (stats-s-lvl pstats) (stats-s-exp pstats))))
        ;;
        ;; Save old status
        ;; 
        (setf s-lvl level
              s-pur purse
              s-hp (stats-s-hpt pstats)
              s-str (str-t-st-str (stats-s-str pstats))
              s-add (str-t-st-add (stats-s-str pstats))
              s-exp (stats-s-exp pstats)
              s-ac (if cur-armor 
                       (object-o-ac cur-armor)
                       (stats-s-arm pstats)))

        (cl-charms/low-level:mvwaddstr cw (1- cl-charms/low-level:*lines*) 0 buf)

        (cl-charms/low-level:waddstr cw (case hungry-state
					  (0 "")
					  (1 "  Hungry")
					  (2 "  Weak")
					  (3 "  Fainting")))

	(cl-charms/low-level:wclrtoeol cw)
        (setf s-hungry hungry-state)
        (cl-charms/low-level:wmove cw oy ox)))))

(defun wait-for (ch)
  "Sit around until the guy types the right key."
  (do ((c nil (readchar)))
      ((if (eql ch #\Newline)
           (or (eql c #\Newline) (eql c #\Linefeed))
           (eql c ch)))))

(defun show-win (scr message)
  "Display a window and wait before returning."
  (cl-charms/low-level:mvwaddstr scr 0 0 message)
  (cl-charms/low-level:touchwin scr)
  (cl-charms/low-level:wmove scr hero.y hero.x)
  (draw scr)
  (wait-for #\Space)
  (cl-charms/low-level:clearok cw 1)
  (cl-charms/low-level:touchwin cw))

(defun flush-type ()
  (cl-charms/low-level:flushinp))
