;; Rogue
;; Exploring the dungeons of doom
;; Copyright (C) 1980 by Michael Toy and Glenn Wichman
;; CL Version Copyright (C) 2007 by Jacob Gabrielson
;; All rights reserved
;; 
;; @(#)main.c	3.27 (Berkeley) 6/15/81

(in-package :cl-rogue)

(define-resettable num_checks 0)     ; times we've gone over in checkout() 
(defparameter cw nil)                   ; Window that the player sees 
(defparameter hw nil)                   ; Used for the help command 
(defparameter mw nil)                   ; Used to store mosnters 

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
    (setf file_name
          (concatenate 'string
                       (directory-namestring (probe-file 
                                              (make-pathname :directory (pathname-directory
                                                                         (user-homedir-pathname)))))
                       "rogue.sav"))
    (parse_opts (sb-posix:getenv "ROGUEOPTS"))
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
    (init_player)                       ; roll up the rogue 
    (init_things)                    ; set up probabilities of things 
    (init_names)                        ; set up names of scrolls 
    (init_colors)                       ; set up colors of potions 
    (init_stones)                    ; set up stone settings of rings 
    (init_materials)                    ; set up materials of wands 
    (unwind-protect
         (progn
           (cl-ncurses:initscr)         ; start up cursor package 

           (when (< cl-ncurses:*cols*  70)
             (format t "~%~%Sorry, ~a, but your terminal window has too few columns.~%" whoami)
             (format t "Your terminal has ~a columns, needs 70.~%" cl-ncurses:*cols*)
             (cl-ncurses:endwin)
             (return-from rogue))

           (when (< cl-ncurses:*lines* 22)
             (format t "~%~%Sorry, ~a, but your terminal window has too few lines.~%" whoami)
             (format t "Your terminal has ~d lines, needs 22.~%" cl-ncurses:*lines*)
             (cl-ncurses:endwin)
             (return-from rogue))

           (setup)
           ;; Set up windows
           (setf cw (cl-ncurses:newwin cl-ncurses:*lines* cl-ncurses:*cols* 0 0)
                 mw (cl-ncurses:newwin cl-ncurses:*lines* cl-ncurses:*cols* 0 0)
                 hw (cl-ncurses:newwin cl-ncurses:*lines* cl-ncurses:*cols* 0 0)
                 waswizard wizard)
           (new_level)                  ; Draw current level 
           ;; Start up daemons and fuses
           (daemon 'doctor 0 AFTER)
           (fuse 'swander 0 WANDERTIME AFTER)
           (daemon 'stomach 0 AFTER)
           (daemon 'runners 0 AFTER)
           ;; Give the rogue his weaponry.  First a mace.
           (let (obj)
             (setf obj (make-object :o_type WEAPON :o_which MACE))
             (init_weapon obj MACE)
             (setf (object-o_hplus obj) 1
                   (object-o_dplus obj) 1
                   (object-o_flags obj) (logior (object-o_flags obj) ISKNOW))
             (add_pack obj t)
             (setf cur_weapon obj)

             ;; Now a +1 bow
             (setf obj (make-object :o_type WEAPON :o_which BOW))
             (init_weapon obj BOW)
             (setf (object-o_hplus obj) 1
                   (object-o_dplus obj) 0
                   (object-o_flags obj) (logior (object-o_flags obj) ISKNOW))
             (add_pack obj t)

             ;; Now some arrows
             (setf obj (make-object :o_type WEAPON :o_which ARROW))
             (init_weapon obj ARROW)
             (setf (object-o_count obj) (+ 25 (rnd 15))
                   (object-o_hplus obj) 0
                   (object-o_dplus obj) 0
                   (object-o_flags obj) (logior (object-o_flags obj) ISKNOW))
             (add_pack obj t)

             ;; And his suit of armor
             (setf obj (make-object :o_type  ARMOR 
                                    :o_which RING_MAIL
                                    :o_ac    (1- (aref a_class RING_MAIL))
                                    :o_flags ISKNOW)
                   cur_armor obj)
             (add_pack obj t)

             ;; Give him some food too
             (setf obj (make-object :o_type FOOD :o_count 1 :o_which 0))
             (add_pack obj t)
             (playit)))

      ;; cleanup
      (cl-ncurses:endwin))))
      

(defun endit ()
  "Exit the program abnormally."
  (fatal (format nil "Ok, if you want to exit that badly, I'll have to allow it~%")))

(defun fatal (s)
  "Exit the program, printing a message."
  (cl-ncurses:clear)
  (cl-ncurses:move (- cl-ncurses:*lines* 2) 0)
  (cl-ncurses:printw s)
  (draw cl-ncurses:*stdscr*)
  (rogue-done))

(defun roll (number sides)
  "Roll a number of dice."
  (do ((dtotal 0 (+ dtotal (rnd sides) 1)))
      ((zerop (prog1 number (decf number))) dtotal)))

(defun setup ()
  (cl-ncurses:cbreak)
  (cl-ncurses:noecho))

(defun playit ()
  "The main loop of the program.  Loop until the game is over,
refreshing things and looking at the proper times."
  (setf oldpos hero
        oldrp (roomin hero))
  (while playing
    (command))
  (endit))
