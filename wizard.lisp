;;;; Special wizard commands (some of which are also non-wizard commands
;;;; under strange circumstances)
;;;; @(#)wizard.c	3.8 (Berkeley) 6/3/81

(in-package :cl-rogue)

(defun whatis ()
  "What a certin object is."
  (when-let (obj (get-item "identify" nil))
    (let ((which (object-o-which obj)))
      (case (object-o-type obj)
        (#.SCROLL
         (setf (aref s-know which) t)
         (when (aref s-guess which)
           (setf (aref s-guess which) nil)))
        (#.POTION
         (setf (aref p-know which) t)
         (when (aref p-guess which)
           (setf (aref p-guess which) nil)))
        (#.STICK
         (setf (aref ws-know which) t)
         (logior! (object-o-flags obj) ISKNOW)
         (when (aref ws-guess which)
           (setf (aref ws-guess which) nil)))
        ((#.WEAPON #.ARMOR)
         (logior! (object-o-flags obj) ISKNOW))
        (#.RING
         (setf (aref r-know which) t)
         (logior! (object-o-flags obj) ISKNOW)
         (when (aref r-guess which)
           (setf (aref r-guess which) nil)))))
    (msg (inv-name obj nil))))

;; (defun create-obj ()
;;   "Wizard command for getting anything he wants."
;;   (let ((obj (make-object)))
;;     (msg "Type of item: ")
;;     (setf (object-o-type obj) (readchar))
;;     (zero! mpos)
;;     (msg "Which ~a do you want? (0-f)" (object-o-type obj))
;;     (setf (object-o-which obj) 23)
;;     ;;(let ((ch (readchar)))
;; ;;      )
    
;;     (add-pack obj nil)))

(defun create-obj ()
  "Wizard command for getting anything he wants."
  (let ((obj (make-object)))
    (msg "Type of item: ")
    (setf (object-o-type obj) (readchar))
    (zero! mpos)
    (msg "Which ~a do you want? (0-f)" (object-o-type obj))
    (let ((ch (readchar)))
      (setf (object-o-which obj) (if (digit-char-p ch)
                                     (- (char-code ch) (char-code #\0))
                                     (- (char-code ch) (char-code #\a) -10))
            (object-o-group obj) 0
            (object-o-count obj) 1
            mpos 0))
    (case (object-o-type obj)
      ((#.WEAPON #.ARMOR)
       (msg "Blessing? (+,-,n)")
       (let ((bless (readchar)))
         (zero! mpos)
         (when (eql bless #\-)
           (logior! (object-o-flags obj) ISCURSED))
         (case (object-o-type obj)
           (#.WEAPON
            (init-weapon obj (object-o-which obj))
            (when (eql bless #\-)
              (decf (object-o-hplus obj) (1+ (rnd 3))))
            (when (eql bless #\+)
              (incf (object-o-hplus obj) (1+ (rnd 3)))))
           (otherwise
            (setf (object-o-ac obj) (aref a-class (object-o-which obj)))
            (when (eql bless #\-)
              (incf (object-o-ac obj) (1+ (rnd 3))))
            (when (eql bless #\+)
              (decf (object-o-ac obj) (1+ (rnd 3))))))))
      (#.RING
       (case (object-o-which obj)
         ((#.R-PROTECT
           #.R-ADDSTR
           #.R-ADDHIT
           #.R-ADDDAM)
          (msg "Blessing? (+,-,n)")
          (let ((bless (readchar)))
            (zero! mpos)
            (if (eql bless #\-)
                (logior! (object-o-flags obj) ISCURSED)
                (setf (object-o-ac obj) (if (eql bless #\-)
                                            -1 (1+ (rnd 2)))))))))
      (#.STICK
       (fix-stick obj)))
    (add-pack obj nil)))

(defun teleport ()
  "Bamf the hero someplace else."
  (let (rm)
    (let ((c (copy-structure hero)))
      (rogue-mvwaddch cw hero.y hero.x (rogue-mvwinch cl-ncurses:*stdscr* hero.y hero.x))
      (loop
         (setf rm (rnd-room))
         (rnd-pos (aref rooms rm) hero)
         (when (eql (winat hero.y hero.x) THE-FLOOR) 
           (return)))
      (light c)
      (light hero)
      (rogue-mvwaddch cw hero.y hero.x PLAYER)
      ;; turn off ISHELD in case teleportation was done while fighting
      ;; a Fungi
      (when (on *player* ISHELD)
        (logclr! (thing-t-flags *player*) ISHELD)
        (zero! fung-hit)
        (setf (stats-s-dmg (monster-m-stats (char-monster #\F))) (copy-seq "000d0")))
      (zero! *count*)
      (setf running nil)
      (flush-type)                      ; flush typeahead
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
;;         if (c == terminal.c-cc[VKILL])
;;             sp = buf;
;;         else if (c == terminal.c-cc[VERASE] && sp > buf)
;;             sp--;
;;         else
;;             *sp++ = c;
;;     if (sp == buf)
;;         return FALSE;
;;     *sp = '\0';
;;     return (strcmp(PASSWD, xcrypt(buf, "mT")) == 0);

