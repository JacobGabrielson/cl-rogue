;;;; Routines to deal with the pack
;;;; @(#)pack.c	3.6 (Berkeley) 6/15/81

(in-package :cl-rogue)

;; Big fat hack to avoid changing the code too much.  If it don't do
;; this then pack_char won't always be able to find the object that
;; was inserted into the pack.
(defvar *inserted-object* nil) 

(defun find-same-group (group)
  (find group pack :key #'object-o_group))

(defun insert-after-similar (list obj)
  "Inserts OBJ into LIST just before the first element of the
list that is of the same OBJECT-O_TYPE.  Returns non-nil if OBJ
was successfully inserted into LIST.  Only call this if
INSERT-AFTER-EXACT returned nil."
  (let* ((inserted nil)
         (new-list (mapcan #'(lambda (list-obj)
                               (if (and (not inserted)
                                        (eql (object-o_type list-obj)
                                             (object-o_type obj)))
                                   (progn
                                     (setf inserted t)
                                     ;; Big fat hack to make printing
                                     ;; of the pack char work later on
                                     (setf *inserted-object* obj)
                                     (list obj list-obj))
                                   (list list-obj)))
                           list)))
    (when inserted new-list)))

(defun insert-after-exact (list obj)
  (let* ((inserted nil)
         (new-list (mapcan #'(lambda (list-obj)
                               (if (and (not inserted)
                                        (eql (object-o_type list-obj)
                                             (object-o_type obj))
                                        (eql (object-o_which list-obj)
                                             (object-o_which obj)))
                                   (progn
                                     (setf inserted t)
                                     ;; If we found an exact match.  If it is a
                                     ;; potion, food, or a scroll, increase the
                                     ;; count, otherwise put it with its clones.
                                     (if (ismult (object-o_type list-obj))
                                         (progn
                                           (incf (object-o_count list-obj))
                                           (setf *inserted-object* list-obj)
                                           (list list-obj))
                                         (progn
                                           (setf *inserted-object* obj)
                                           (list list-obj obj))))
                                   (list list-obj)))
                           list)))
    (when inserted new-list)))
    
(defun insert-pack (obj)
  "Insert OBJ into the player's pack, doing various sorts of
grouping type stuff correctly."
  (setf pack
        (cond
          ((insert-after-exact pack obj))
          ((insert-after-similar pack obj))
          (t (progn
               (setf *inserted-object* obj)
               `(,@pack ,obj))))))

(defun add_pack (obj silent)
  "Pick up an object and add it to the pack.  If the argument is
non-null use it as the linked_list pointer instead of gettting it
off the ground."
  (let ((from_floor (not obj))
        (picked_up nil)
        (obj (or obj (find_obj hero.y hero.x))))
    (unless obj
      (return-from add_pack))

    ;; Link it into the pack.  Search the pack for a object of similar
    ;; type if there isn't one, stuff it at the beginning, if there
    ;; is, look for one that is exactly the same and just increment
    ;; the count if there is.  Food is always put at the beginning for
    ;; ease of access, but is not ordered so that you can't tell good
    ;; food from bad.  First check to see if there is something in the
    ;; same group and if there is then increment the count.
    (let ((group (object-o_group obj)))
      (unless (zerop group)
        (when-let (existing (find-same-group group))
          ;; Put it in the pack and notify the user
          (incf (object-o_count existing))
          (when from_floor
            (detach lvl_obj obj)
            (rogue-mvaddch hero.y hero.x
                           (if (roomin hero)
                               FLOOR
                               PASSAGE)))
          (setf picked_up t))))
    (unless picked_up
      ;; Check if there is room
      (when (= inpack (1- MAXPACK))
        (msg "You can't carry anything else.")
        (return-from add_pack))

      ;; Check for and deal with scare monster scrolls
      (when (and (eql (object-o_type obj) SCROLL)
                 (eql (object-o_which obj) S_SCARE))
        (if (logtest (object-o_flags obj) ISFOUND)
            (progn
              (msg "The scroll turns to dust as you pick it up.")
              (detach lvl_obj obj)
              (rogue-mvaddch hero.y hero.x FLOOR)
              (return-from add_pack))
            (logior! (object-o_flags obj) ISFOUND)))

      (incf inpack)
      (when from_floor
        (detach lvl_obj obj)
        (rogue-mvaddch hero.y hero.x (if (roomin hero) FLOOR PASSAGE)))
      (insert-pack obj))

    ;; Notify the user
    (when (and notify 
               (not silent))
      (verbose (addmsg "You now have "))
      (msg "~a (~a)" 
           (inv_name *inserted-object* (not terse)) 
           (pack_char *inserted-object*)))

    (when (eql (object-o_type obj) AMULET)
      (setf *amulet* t))))

(defun inventory (list type)
  "List what is in the pack."
  (let* ((inv_temp "")
         (n_objs
          (do* ((ch     #\a        (code-char (1+ (char-code ch))))
                (iter   list       (cdr iter))
                (obj    (car iter) (car iter))
                (n_objs 0          (1+ n_objs)))

               ((null iter) n_objs)
            
            (unless (and type
                         (not (eql type (object-o_type obj)))
                         (not (eql type CALLABLE))
                         (or (eql (object-o_type obj) SCROLL)
                             (eql (object-o_type obj) POTION)
                             (eql (object-o_type obj) RING)
                             (eql (object-o_type obj) STICK)))
              (if (zerop n_objs)
                  ;; For the first thing in the inventory, just save the string
                  ;; in case there is only one.
                  (setf inv_temp (format nil "~a) ~a" ch (inv_name obj nil)))
                  ;; If there is more than one, clear the screen, print the
                  ;; saved message and fall through to ...
                  (progn
                    (when (onep n_objs)
                      (if slow_invent
                          (msg inv_temp)
                          (progn
                            (cl-ncurses:wclear hw)
                            (cl-ncurses:waddstr hw inv_temp)
                            (rogue-waddch hw #\Newline))))
                    ;; Print the line for this object
                    (if slow_invent
                        (msg "~a) ~a" ch (inv_name obj nil))
                        (cl-ncurses:wprintw hw (format nil "~a) ~a~%" ch (inv_name obj nil))))))))))

    (when (zerop n_objs)
      (msg (if terse
               (if type
                   "Empty handed." 
                   "Nothing appropriate")
               (if type
                   "You are empty handed."
                   "You don't have anything appropriate")))
      (return-from inventory nil))

    (when (onep n_objs)
      (msg inv_temp)
      (return-from inventory t))

    (unless slow_invent
      (cl-ncurses:mvwaddstr hw (1- cl-ncurses:*lines*) 0 "--Press space to continue--")
      (cl-ncurses:wrefresh hw)
      ;; If we don't do this, then, if CW happens to have its x,y
      ;; coords set to 0,0 we will end up nuking the first line (not
      ;; sure why exactly, should happen in the C version too, but
      ;; presumably does not.
      (let ((*input-window* hw))
        (wait_for #\Space))
      (cl-ncurses:clearok cw cl-ncurses:true)
      (cl-ncurses:touchwin cw))

    t))

(defun pick_up (ch)
  "Add something to character's pack."
  (case ch
    (#.GOLD
     (money))
    ((#.ARMOR
      #.POTION
      #.FOOD
      #.WEAPON
      #.SCROLL	
      #.AMULET
      #.RING
      #.STICK)
     (add_pack nil nil))
    (otherwise
     (rogue-debug "Where did you pick that up???"))))

(defun picky_inven ()
  "Allow player to inventory a single item."
  (cond
    ((null pack)
     (msg "You aren't carrying anything"))
    ((null (cdr pack))
     (msg "a) ~a" (inv_name (car pack) nil)))
    (t
     (msg (if terse "Item: " "Which item do you wish to inventory: "))
     (zero! mpos)
     (let ((mch (readchar))
           (ch (char-code #\a)))
       (when (eql mch #\Escape)
         (msg "")
         (return-from picky_inven))

       (map 'nil 
            #'(lambda (item)
                (when (eql (code-char ch) mch)
                  (msg "~a) ~a" mch (inv_name item nil))
                  (return-from picky_inven))
                (incf ch))
            pack)
       (verbose (msg "'~a' not in pack" (unctrl-char mch)))
       (msg "Range is 'a' to '~a'" (code-char (1- ch)))))))

(defun get_item (purpose type)
  "Pick something out of a pack for a purpose."
  (if (null pack)
      (msg "You aren't carrying anything.")
      (loop
         (block continue
           (verbose (addmsg "Which object do you want to "))
           (addmsg purpose)
           (when terse (addmsg " what"))
           (msg "? (* for list): ")
           (let ((ch (readchar)))
             (zero! mpos)
             ;; Give the poor player a chance to abort the command
             (case ch
               ((#\Escape #.(ctrl #\G))
                (setf *after* nil)
                (msg "")
                (return-from get_item))
               (#\*
                (zero! mpos)
                (unless (inventory pack type)
                  (setf *after* nil)
                  (return-from get_item))
                (return-from continue)))

             (let ((och (char-code #\a))
                   obj)
               (dolist (o pack)
                 (when (eql (char-code ch)
                            och)
                   (setf obj o)
                   (return))
                 (incf och))
               (unless obj
                 (msg "Please specify a letter between 'a' and '~a'" (code-char (1- och)))
                 (return-from continue))
               (return-from get_item obj))))))
  nil)

(defun pack_char (obj)
  (let ((c (char-code #\a)))
    (dolist (item pack)
      (if (eql obj item)
          (return-from pack_char (code-char c))
          (incf c))))
  #\z)
