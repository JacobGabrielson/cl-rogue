;;;; Special wizard commands (some of which are also non-wizard commands
;;;; under strange circumstances)
;;;; @(#)wizard.c	3.8 (Berkeley) 6/3/81

(in-package :cl-rogue)

(defun whatis ()
  "What a certin object is."
  (when-let (obj (get_item "identify" nil))
    (let ((which (object-o_which obj)))
      (case (object-o_type obj)
        (#.SCROLL
         (setf (aref s_know which) t)
         (when (aref s_guess which)
           (setf (aref s_guess which) nil)))
        (#.POTION
         (setf (aref p_know which) t)
         (when (aref p_guess which)
           (setf (aref p_guess which) nil)))
        (#.STICK
         (setf (aref ws_know which) t)
         (logior! (object-o_flags obj) ISKNOW)
         (when (aref ws_guess which)
           (setf (aref ws_guess which) nil)))
        ((#.WEAPON #.ARMOR)
         (logior! (object-o_flags obj) ISKNOW))
        (#.RING
         (setf (aref r_know which) t)
         (logior! (object-o_flags obj) ISKNOW)
         (when (aref r_guess which)
           (setf (aref r_guess which) nil)))))
    (msg (inv_name obj nil))))

;; (defun create_obj ()
;;   "Wizard command for getting anything he wants."
;;   (let ((obj (make-object)))
;;     (msg "Type of item: ")
;;     (setf (object-o_type obj) (readchar))
;;     (zero! mpos)
;;     (msg "Which ~a do you want? (0-f)" (object-o_type obj))
;;     (setf (object-o_which obj) 23)
;;     ;;(let ((ch (readchar)))
;; ;;      )
    
;;     (add_pack obj nil)))

(defun create_obj ()
  "Wizard command for getting anything he wants."
  (let ((obj (make-object)))
    (msg "Type of item: ")
    (setf (object-o_type obj) (readchar))
    (zero! mpos)
    (msg "Which ~a do you want? (0-f)" (object-o_type obj))
    (let ((ch (readchar)))
      (setf (object-o_which obj) (if (digit-char-p ch)
                                     (- (char-code ch) (char-code #\0))
                                     (- (char-code ch) (char-code #\a) -10))
            (object-o_group obj) 0
            (object-o_count obj) 1
            mpos 0))
    (case (object-o_type obj)
      ((#.WEAPON #.ARMOR)
       (msg "Blessing? (+,-,n)")
       (let ((bless (readchar)))
         (zero! mpos)
         (when (eq bless #\-)
           (logior! (object-o_flags obj) ISCURSED))
         (case (object-o_type obj)
           (#.WEAPON
            (init_weapon obj (object-o_which obj))
            (when (eq bless #\-)
              (decf (object-o_hplus obj) (1+ (rnd 3))))
            (when (eq bless #\+)
              (incf (object-o_hplus obj) (1+ (rnd 3)))))
           (otherwise
            (setf (object-o_ac obj) (aref a_class (object-o_which obj)))
            (when (eq bless #\-)
              (incf (object-o_ac obj) (1+ (rnd 3))))
            (when (eq bless #\+)
              (decf (object-o_ac obj) (1+ (rnd 3))))))))
      (#.RING
       (case (object-o_which obj)
         ((#.R_PROTECT
           #.R_ADDSTR
           #.R_ADDHIT
           #.R_ADDDAM)
          (msg "Blessing? (+,-,n)")
          (let ((bless (readchar)))
            (zero! mpos)
            (if (eq bless #\-)
                (logior! (object-o_flags obj) ISCURSED)
                (setf (object-o_ac obj) (if (eq bless #\-)
                                            -1 (1+ (rnd 2)))))))))
      (#.STICK
       (fix_stick obj)))
    (add_pack obj nil)))

(defun teleport ()
  "Bamf the hero someplace else."
  (let (rm)
    (let ((c (copy-structure hero)))
      (rogue-mvwaddch cw hero.y hero.x (rogue-mvwinch cl-ncurses:*stdscr* hero.y hero.x))
      (loop
         (setf rm (rnd_room))
         (rnd_pos (aref rooms rm) hero)
         (when (eq (winat hero.y hero.x) FLOOR) 
           (return)))
      (light c)
      (light hero)
      (rogue-mvwaddch cw hero.y hero.x PLAYER)
      ;; turn off ISHELD in case teleportation was done while fighting
      ;; a Fungi
      (when (on *player* ISHELD)
        (logclr! (thing-t_flags *player*) ISHELD)
        (zero! fung_hit)
        (setf (stats-s_dmg (monster-m_stats (char-monster #\F))) (copy-seq "000d0")))
      (zero! *count*)
      (setf running nil)
      (flush_type)                      ; flush typeahead
      rm)))

(defun passwd (&optional a b c d e f g)
  "See if user knows password."
  (declare (ignore a b c d e f g))
  t)
;; I don't feel like porting this right now
;; {
;;     register char *sp, c;
;;     char buf[80], *xcrypt();

;;     msg("Wizard's Password:");
;;     mpos = 0;
;;     sp = buf;
;;     while ((c = getchar()) != '\n' && c != '\r' && c != '\033')
;;         if (c == terminal.c_cc[VKILL])
;;             sp = buf;
;;         else if (c == terminal.c_cc[VERASE] && sp > buf)
;;             sp--;
;;         else
;;             *sp++ = c;
;;     if (sp == buf)
;;         return FALSE;
;;     *sp = '\0';
;;     return (strcmp(PASSWD, xcrypt(buf, "mT")) == 0);

