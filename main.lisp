;; Rogue
;; Exploring the dungeons of doom
;; Copyright (C) 1980 by Michael Toy and Glenn Wichman
;; CL Version Copyright (C) 2007 by Jacob Gabrielson
;; All rights reserved
;; 
;; @(#)main.c	3.27 (Berkeley) 6/15/81

(in-package :cl-rogue)

(define-resettable num-checks 0)     ; times we've gone over in checkout() 

(defun rogue-done ()
  (throw 'rogue-done nil))

(defun rogue (&key (wizard-mode nil) (print-score nil) (restore-from nil))
  ;; Check for print-score option
  (catch 'rogue-done
    (when print-score
      (setf waswizard t)
      (score 0 -1)
      (return-from rogue))

    ;; Check to see if he is a wizard
    (when wizard-mode
      (setf wizard t))

    ;; Get home and options from environment
    (setf home (user-homedir-pathname))
    (setf file-name
          (concatenate 'string
                       (directory-namestring (probe-file 
                                              (make-pathname :directory (pathname-directory
                                                                         (user-homedir-pathname)))))
                       "rogue.sav"))
    (parse-opts (sb-posix:getenv "ROGUEOPTS"))
    (when (zerop (length whoami))
      (setf whoami 
            #-(or win32 mswindows)
            (sb-unix:uid-username (sb-posix:getuid))
            #+(or win32 mswindows)
            (or (sb-posix:getenv "USERNAME") "Unknown")
            ))
    (when (zerop (length fruit))
      (setf fruit "slime-mold"))

    (when restore-from
      (unless (restore restore-from)) ; note: restore will never return 
      (return-from rogue))

    ;; Do this after we know we're not restoring, otherwise it's
    ;; wasted.
    (reset-rogue-symbols)

    (format t "Hello ~a, just a moment while I dig the dungeon..." whoami)
    (init-player)                       ; roll up the rogue 
    (init-things)                    ; set up probabilities of things 
    (init-names)                        ; set up names of scrolls 
    (init-colors)                       ; set up colors of potions 
    (init-stones)                    ; set up stone settings of rings 
    (init-materials)                    ; set up materials of wands 

    (charms:with-curses ()

      (unwind-protect
	   (progn
	     (cl-charms/low-level:initscr)         ; start up cursor package 
	     
	     (when (< cl-charms/low-level:*cols*  70)
	       (format t "~%~%Sorry, ~a, but your terminal window has too few columns.~%" whoami)
	       (format t "Your terminal has ~a columns, needs 70.~%" cl-charms/low-level:*cols*)
	       (cl-charms/low-level:endwin)
	       (return-from rogue))

	     (when (< cl-charms/low-level:*lines* 22)
	       (format t "~%~%Sorry, ~a, but your terminal window has too few lines.~%" whoami)
	       (format t "Your terminal has ~d lines, needs 22.~%" cl-charms/low-level:*lines*)
	       (cl-charms/low-level:endwin)
	       (return-from rogue))

	     (setup)

	     ;; Set up windows
	     (setf cw (cl-charms/low-level:newwin cl-charms/low-level:*lines* cl-charms/low-level:*cols* 0 0)
		   mw (cl-charms/low-level:newwin cl-charms/low-level:*lines* cl-charms/low-level:*cols* 0 0)
		   hw (cl-charms/low-level:newwin cl-charms/low-level:*lines* cl-charms/low-level:*cols* 0 0)
		   waswizard wizard)

	     (new-level)                  ; Draw current level 

	     ;; Start up daemons and fuses
	     (daemon 'doctor 0 AFTER)
	     (fuse 'swander 0 WANDERTIME AFTER)
	     (daemon 'stomach 0 AFTER)
	     (daemon 'runners 0 AFTER)
	     ;; Give the rogue his weaponry.  First a mace.
	     (let (obj)
	       (setf obj (make-object :o-type WEAPON :o-which MACE))
	       (init-weapon obj MACE)
	       (setf (object-o-hplus obj) 1
		     (object-o-dplus obj) 1
		     (object-o-flags obj) (logior (object-o-flags obj) ISKNOW))
	       (add-pack obj t)
	       (setf cur-weapon obj)

	       ;; Now a +1 bow
	       (setf obj (make-object :o-type WEAPON :o-which BOW))
	       (init-weapon obj BOW)
	       (setf (object-o-hplus obj) 1
		     (object-o-dplus obj) 0
		     (object-o-flags obj) (logior (object-o-flags obj) ISKNOW))
	       (add-pack obj t)

	       ;; Now some arrows
	       (setf obj (make-object :o-type WEAPON :o-which ARROW))
	       (init-weapon obj ARROW)
	       (setf (object-o-count obj) (+ 25 (rnd 15))
		     (object-o-hplus obj) 0
		     (object-o-dplus obj) 0
		     (object-o-flags obj) (logior (object-o-flags obj) ISKNOW))
	       (add-pack obj t)

	       ;; And his suit of armor
	       (setf obj (make-object :o-type  ARMOR 
				      :o-which RING-MAIL
				      :o-ac    (1- (aref a-class RING-MAIL))
				      :o-flags ISKNOW)
		     cur-armor obj)
	       (add-pack obj t)

	       ;; Give him some food too
	       (setf obj (make-object :o-type FOOD :o-count 1 :o-which 0))
	       (add-pack obj t)

	       (playit)))

	;; cleanup
	(cl-charms/low-level:endwin)))))


(defun endit ()
  "Exit the program abnormally."
  (fatal (format nil "Ok, if you want to exit that badly, I'll have to allow it~%")))

(defun fatal (s)
  "Exit the program, printing a message."
  (cl-charms/low-level:clear)
  (cl-charms/low-level:move (- cl-charms/low-level:*lines* 2) 0)
  (cl-charms/low-level:printw s)
  (draw cl-charms/low-level:*stdscr*)
  (rogue-done))

(defun roll (number sides)
  "Roll a number of dice."
  (do ((dtotal 0 (+ dtotal (rnd sides) 1)))
      ((zerop (prog1 number (decf number))) dtotal)))

(defun setup ()
  (cl-charms/low-level:cbreak)
  (cl-charms/low-level:noecho))

(defun playit ()
  "The main loop of the program.  Loop until the game is over,
refreshing things and looking at the proper times."
  (setf oldpos hero
        oldrp (roomin hero))
  (while playing
    (command))
  (endit))
