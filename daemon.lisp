;;;; Contains functions for dealing with things that happen in the
;;;; future.
;;;; @(#)daemon.c	3.3 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defparameter EMPTY 0)
(defparameter DAEMON -1)
(defparameter MAXDAEMONS 20)

(defstruct delayed_action
  (d_type EMPTY :type fixnum)
  (d_func nil :type symbol)
  (d_arg nil)
  (d_time 0 :type fixnum))

(define-resettable d_list (let ((new-dlist (make-array MAXDAEMONS :element-type 'delayed_action)))
                            (dotimes (i (length new-dlist))
                              (setf (aref new-dlist i) (make-delayed_action)))
                            new-dlist))

(defun d_slot ()
  "Find an empty slot in the daemon/fuse list."
  (or (find EMPTY d_list :key #'delayed_action-d_type)
      (rogue-debug "Ran out of fuse slots")))

(defun find_slot (func)
  "Find a particular slot in the table."
  (find-if #'(lambda (x) 
               (and (not (eql (delayed_action-d_type x) EMPTY))
                    (eql (delayed_action-d_func x) func)))
           d_list))

(defun daemon (func arg type)
  "Start a daemon, takes a function."
  (let ((dev (d_slot)))
    (setf (delayed_action-d_type dev) type
          (delayed_action-d_func dev) func
          (delayed_action-d_arg dev) arg
          (delayed_action-d_time dev) DAEMON)))

(defun kill_daemon (func)
  "Remove a daemon from the list."
  (when-let (dev (find_slot func))
    ;; Take it out of the list
    (setf (delayed_action-d_type dev) EMPTY)))

(defun do_daemons (flag)
  "Run all the daemons that are active with the current flag,
passing the argument to the function."
  (map nil 
       #'(lambda (d)
           (when (and (eql (delayed_action-d_type d) flag) 
                      (eql (delayed_action-d_time d) DAEMON))
             (funcall (delayed_action-d_func d) (delayed_action-d_arg d))))
       d_list))

(defun fuse (func arg time type)
  "Start a fuse to go off in a certain number of turns."
  (let ((wire (d_slot)))
    (setf (delayed_action-d_type wire) type
          (delayed_action-d_func wire) func
          (delayed_action-d_arg wire) arg
          (delayed_action-d_time wire) time)))

(defun lengthen (func xtime)
  "Increase the time until a fuse goes off."
  (when-let (wire (find_slot func))
    (incf (delayed_action-d_time wire) xtime)))

(defun extinguish (func)
  "Put out a fuse."
  (when-let (wire (find_slot func))
    (setf (delayed_action-d_type wire) EMPTY)))

(defun do_fuses (flag)
  "Decrement counters and start needed fuses."
  (map nil
       #'(lambda (wire)
           (when (and (eql flag (delayed_action-d_type wire))
                      (plusp (delayed_action-d_time wire))
                      (zerop (decf (delayed_action-d_time wire))))
             ;; Decrementing counters and starting things we want.  We also need
             ;; to remove the fuse from the list once it has gone off.
             (setf (delayed_action-d_type wire) EMPTY)
             (funcall (delayed_action-d_func wire) (delayed_action-d_arg wire))))
       d_list))
