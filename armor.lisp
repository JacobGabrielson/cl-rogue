;;;; This file contains misc functions for dealing with armor
;;;; @(#)armor.c	3.9 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defun wear ()
  "The player wants to wear something, so let him/her put it on."
  (if cur_armor
      (progn
        (addmsg "You are already wearing some")
        (verbose (addmsg ".  You'll have to take it off first"))
        (endmsg)
        (setf *after* nil))
      (when-let (obj (get_item "wear" ARMOR))
        (if (not (eql (object-o_type obj)
                      ARMOR))
            (msg "You can't wear that.")
            (progn
              (waste_time)
              (addmsg (if terse "W" "You are now w"))
              (msg (format nil "earing ~a" 
                           (aref a_names (object-o_which obj))))
              (setf cur_armor obj)
              (logior! (object-o_flags obj) ISKNOW))))))

(defun take_off ()
  "Get the armor off of the player's back."
  (let ((obj cur_armor))
    (if obj
        (when (dropcheck cur_armor)
          (setf cur_armor nil)
          (addmsg (if terse "Was" "You used to be "))
          (msg (format 
                nil
                " wearing ~a) ~a" 
                (pack_char obj) (inv_name obj t))))
        (msg (if terse "Not wearing armor" "You aren't wearing any armor")))))

(defun waste_time ()
  "Do nothing but let other things happen."
  (do_daemons BEFORE)
  (do_fuses BEFORE)
  (do_daemons AFTER)
  (do_fuses AFTER))
      
