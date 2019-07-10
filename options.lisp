;;;; This file has all the code for the option command.
;;;; I would rather this command were not necessary, but
;;;; it is the only way to keep the wolves off of my back.
;;;;
;;;; @(#)options.c	3.3 (Berkeley) 5/25/81

(in-package :cl-rogue)

(defstruct option
  "Description of an option and what to do with it."
  (name "" :type string)                ; option name 
  (prompt "" :type string)             ; prompt for interactive entry 
  (opt nil :type symbol)                ; pointer to thing to set 
  (putfunc)                             ; function to print value 
  (getfunc))                    ; function to get value interactively 

(defun put-bool (b)
  "Put out a boolean."
  (cl-charms/low-level:waddstr hw (if b "True" "False")))

(defun put-str (str)
  "Put out a string."
  (cl-charms/low-level:waddstr hw str))

(defun get-bool (bp win)
  "Allow changing a boolean option and print it out."
  (let (oy
        ox
        (op-bad t)
        (new-bp bp))
    (cl-charms/low-level:getyx win oy ox)
    (cl-charms/low-level:waddstr win (if bp "True" "False"))
    (while op-bad
      (cl-charms/low-level:wmove win oy ox)
      (draw win)
      (case (readchar)
        ((#\t #\T)
         (setf new-bp t
               op-bad nil))
        ((#\f #\F)
         (setf new-bp nil
               op-bad nil))
        ((#\Linefeed #\Return)
         (setf op-bad nil))
        ((#\Esc #\Bel)
         (return-from get-bool (values new-bp QUIT)))
        (#\-
         (return-from get-bool (values new-bp MINUS)))
        (otherwise
         (cl-charms/low-level:mvwaddstr win  oy (+ ox 10) "(T or F)"))))
    (cl-charms/low-level:wmove win oy ox)
    (cl-charms/low-level:waddstr win (if bp "True" "False"))
    (rogue-waddch win #\Newline)
    (values new-bp NORM)))

(defun get-str (opt win)
  "Set a string option."
  (let (c oy ox (new-opt opt))
    (draw win)
    (cl-charms/low-level:getyx win oy ox)
    ;; Loop reading in the string, and put it in a temporary buffer
    (let ((sp 0)
          (buf (copy-seq "")))
      (loop
         (block continue
           (setf c (readchar))
           (case c
             ((#\Linefeed #\Return #\Esc #\Bel)
              (return)))
           (cl-charms/low-level:wclrtoeol win)
           (draw win)
           (cond
             ((null c) (return-from continue))
             ((eql c (code-char (cl-charms/low-level:erasechar)))
              (when (plusp sp)
                (decf sp)
                (dotimes (_ (length (unctrl-char (aref buf sp))))
                  (rogue-waddch win #\Backspace)))
              (return-from continue))
             ((eql c (code-char (cl-charms/low-level:killchar)))
              (zero! sp)
              (cl-charms/low-level:wmove win oy ox)
              (return-from continue))
             ((zerop sp)
              (if (eql c #\-)
                  (return)
                  (when (eql c #\~)
                    (setf buf (copy-seq home))
                    (cl-charms/low-level:waddstr win home)
                    (incf sp (length home))
                    (return-from continue)))))
           (setf buf (concatenate 'string buf (format nil "~c" c)))
           (incf sp)
           (cl-charms/low-level:waddstr win (unctrl-char c))))

      (when (plusp sp) ; only change option if something has been typed
        (setf new-opt (strucpy buf (length buf))))

      (cl-charms/low-level:wmove win oy ox) 
      (cl-charms/low-level:waddstr win new-opt)
      (rogue-waddch win #\Newline)
      (draw win)

      (when (eql win cw)
        (incf mpos sp))

      (case c
        (#\- (values new-opt MINUS))
        ((#.(code-char #o033) #.(code-char #o007))
         (values new-opt QUIT))
        (otherwise
         (values new-opt NORM))))))

(defparameter *options*
  (map 'vector
       #'(lambda (l)
           (apply 
            #'make-option
            (mapcan #'list
                    '(:name :prompt :opt :putfunc :getfunc)
                    l)))
       (list
        (list "terse" "Terse output: " 'terse #'put-bool #'get-bool)
        (list "flush" "Flush typeahead during battle: " 'fight-flush #'put-bool #'get-bool)
        (list "jump" "Show position only at end of run: " 'jump #'put-bool #'get-bool)
        (list "step" "Do inventories one line at a time: " 'slow-invent #'put-bool #'get-bool)
        (list "askme" "Ask me about unidentified things: " 'askme #'put-bool #'get-bool)
        (list "name" "Name: " 'whoami #'put-str #'get-str)
        (list "fruit" "Fruit: " 'fruit #'put-str #'get-str)
        (list "file" "Save file: " 'file-name #'put-str #'get-str))))

(defun option ()
  "Print and then set options from the terminal."
  (cl-charms/low-level:wclear hw)
  (cl-charms/low-level:touchwin hw)
  ;; Display current values of options
  (map nil
       #'(lambda (op)
           (cl-charms/low-level:waddstr hw (option-prompt op))
           (funcall (option-putfunc op) (symbol-value (option-opt op)))
           (rogue-waddch hw #\Newline))
       *options*)
  ;;* Set values
  (cl-charms/low-level:wmove hw 0 0)
  (for (i 0 (1- (length *options*)))
    (let ((op (aref *options* i)))
      (cl-charms/low-level:waddstr hw (option-prompt op))
      (multiple-value-bind (new-value status) 
          (funcall (option-getfunc op) (symbol-value (option-opt op)) hw)
        (setf (symbol-value (option-opt op)) new-value)
        (case status
          (#.NORM)                      ; continue
          (#.QUIT
           (return))
          (otherwise                    ; MINUS
           (if (plusp i)
               (progn
                 (cl-charms/low-level:wmove hw (1- i) 0)
                 (decf i 2))
               (progn               ; trying to back up beyond the top
                 (format t "~a" #\Bel)
                 (cl-charms/low-level:wmove hw 0 0)
                 (decf i 1))))))))
  ;; Switch back to original screen
  (cl-charms/low-level:mvwaddstr hw 
                         (1- cl-charms/low-level:*lines*) 
                         0 
                         "--Press space to continue--")
  (draw hw)
  (wait-for #\Space)
  (cl-charms/low-level:clearok cw cl-charms/low-level:true)
  (cl-charms/low-level:touchwin cw)
  (setf *after* nil))

(defun find-option (name)
  (find name
        *options*
        :key #'option-name
        :test #'string-equal))

(defun strip-no (string)
  "Returns copy of STRING with leading 2 characters stripped if
they spell #\n #\o and the remaining string would be at least one
character long."
  (and (> (length string) 2)            ; we cannot return ""
       (string= (subseq string 0 2) "no")
       (copy-seq (subseq string 2))))

(defun parse-opts (optstr)
  "Parse options from string, usually taken from the environment.
the string is a series of comma-separated values, with booleans
being stated as 'name' (true) or 'noname' (false), and strings
being 'name=....', with the string being defined up to a comma or
the end of the entire option string."
  (dolist (setting
            (mapcar #'(lambda (x)
                        (asdf::split-string x :separator '(#\= #\Space #\Tab)))
                    (asdf::split-string optstr :separator '(#\, #\Space #\Tab))))
    ;; setting = (list optname [value-if-string])
    (let* ((optname (first setting))
           (try-opt (find-option optname))
           (opt (or try-opt 
                    (find-option (strip-no optname)))))
      (when opt
        (funcall (option-putfunc opt)
                 (cond
                   ((and opt (second setting))) ; NB: only happens for strings
                   (opt t)))))))

(defun strucpy (string length)
  "Return copy of string using UNCTRL-CHAR for things."
  (apply #'concatenate 
         'string
         (map 'list 
              #'unctrl-char 
              (subseq string 0 length))))

