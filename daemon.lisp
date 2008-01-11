;;;; Contains functions for dealing with things that happen in the
;;;; future.
;;;; @(#)daemon.c	3.3 (Berkeley) 6/15/81

(in-package :cl-rogue)

(defparameter EMPTY 0)
(defparameter DAEMON -1)
(defparameter MAXDAEMONS 20)

(defstruct delayed-action
  (d-type EMPTY :type fixnum)
  (d-func nil :type symbol)
  (d-arg nil)
  (d-time 0 :type fixnum))

(define-resettable d-list (let ((new-dlist (make-array MAXDAEMONS :element-type 'delayed-action)))
                            (dotimes (i (length new-dlist))
                              (setf (aref new-dlist i) (make-delayed-action)))
                            new-dlist))

(defun d-slot ()
  "Find an empty slot in the daemon/fuse list."
  (or (find EMPTY d-list :key #'delayed-action-d-type)
      (rogue-debug "Ran out of fuse slots")))

(defun find-slot (func)
  "Find a particular slot in the table."
  (find-if #'(lambda (x) 
               (and (not (eql (delayed-action-d-type x) EMPTY))
                    (eql (delayed-action-d-func x) func)))
           d-list))

(defun daemon (func arg type)
  "Start a daemon, takes a function."
  (let ((dev (d-slot)))
    (setf (delayed-action-d-type dev) type
          (delayed-action-d-func dev) func
          (delayed-action-d-arg dev) arg
          (delayed-action-d-time dev) DAEMON)))

(defun kill-daemon (func)
  "Remove a daemon from the list."
  (when-let (dev (find-slot func))
    ;; Take it out of the list
    (setf (delayed-action-d-type dev) EMPTY)))

(defun do-daemons (flag)
  "Run all the daemons that are active with the current flag,
passing the argument to the function."
  (map nil 
       #'(lambda (d)
           (when (and (eql (delayed-action-d-type d) flag) 
                      (eql (delayed-action-d-time d) DAEMON))
             (funcall (delayed-action-d-func d) (delayed-action-d-arg d))))
       d-list))

(defun fuse (func arg time type)
  "Start a fuse to go off in a certain number of turns."
  (let ((wire (d-slot)))
    (setf (delayed-action-d-type wire) type
          (delayed-action-d-func wire) func
          (delayed-action-d-arg wire) arg
          (delayed-action-d-time wire) time)))

(defun lengthen (func xtime)
  "Increase the time until a fuse goes off."
  (when-let (wire (find-slot func))
    (incf (delayed-action-d-time wire) xtime)))

(defun extinguish (func)
  "Put out a fuse."
  (when-let (wire (find-slot func))
    (setf (delayed-action-d-type wire) EMPTY)))

(defun do-fuses (flag)
  "Decrement counters and start needed fuses."
  (map nil
       #'(lambda (wire)
           (when (and (eql flag (delayed-action-d-type wire))
                      (plusp (delayed-action-d-time wire))
                      (zerop (decf (delayed-action-d-time wire))))
             ;; Decrementing counters and starting things we want.  We also need
             ;; to remove the fuse from the list once it has gone off.
             (setf (delayed-action-d-type wire) EMPTY)
             (funcall (delayed-action-d-func wire) (delayed-action-d-arg wire))))
       d-list))
