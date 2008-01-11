;;;; This file contains misc functions for dealing with armor
;;;; @(#)armor.c	3.9 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defun wear ()
  "The player wants to wear something, so let him/her put it on."
  (if cur-armor
      (progn
        (addmsg "You are already wearing some")
        (verbose (addmsg ".  You'll have to take it off first"))
        (endmsg)
        (setf *after* nil))
      (when-let (obj (get-item "wear" ARMOR))
        (if (not (eql (object-o-type obj)
                      ARMOR))
            (msg "You can't wear that.")
            (progn
              (waste-time)
              (addmsg (if terse "W" "You are now w"))
              (msg (format nil "earing ~a" 
                           (aref a-names (object-o-which obj))))
              (setf cur-armor obj)
              (logior! (object-o-flags obj) ISKNOW))))))

(defun take-off ()
  "Get the armor off of the player's back."
  (let ((obj cur-armor))
    (if obj
        (when (dropcheck cur-armor)
          (setf cur-armor nil)
          (addmsg (if terse "Was" "You used to be "))
          (msg (format 
                nil
                " wearing ~a) ~a" 
                (pack-char obj) (inv-name obj t))))
        (msg (if terse "Not wearing armor" "You aren't wearing any armor")))))

(defun waste-time ()
  "Do nothing but let other things happen."
  (do-daemons BEFORE)
  (do-fuses BEFORE)
  (do-daemons AFTER)
  (do-fuses AFTER))
      
