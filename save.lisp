;;;; save and restore routines
;;;; @(#)save.c	3.9 (Berkeley) 6/16/81

(in-package :cl-rogue)

(defun save-game ()
  (let (gotfile)
    (zero! mpos)
    (when (plusp (length file-name))
      (msg "Save file (~a)? " file-name)
      (let ((c #\Nul))
        (loop 
           (setf c (readchar))
           (case c
             ((#\n #\N #\y #\Y)
              (return))))
        (zero! mpos)
        (case c
          ((#\y #\Y)
           (setf gotfile t)
           (msg "File name: ~a" file-name)))))

    (loop
       (if gotfile
           (setf gotfile nil)
           ;; Prompt for the file unless the prompt above resulted in
           ;; a valid filename.
           (progn
             (msg "File name: ")
             (zero! mpos)
             (multiple-value-bind (file-name-chosen status)
                 (get-str "" cw)
               (when (eql status QUIT)
                 (msg "")
                 (return nil))
               (setf file-name file-name-chosen))))

       (handler-case 
           (with-open-file (savef file-name
                                  :direction :output 
                                  :if-exists :supersede)
             (save-file savef)
             (close savef)
             (return-from save-game t))
         (file-error (c)
           (msg "~a" (caddr (simple-condition-format-arguments c))))))))

;; 
;; automatically save a file.  This is used if a HUP signal is
;; recieved
;;  
;; void
;; auto-save(int p)
;; {
;;     register FILE *savef;
;;     register int i;

;;     for (i = 0; i < NSIG; i++)
;;         signal(i, SIG-IGN);
;;     if (file-name[0] != '\0' && (savef = fopen(file-name, "w")) != NULL)
;;         save-file(savef);
;;     endwin();
;;     exit(1);
;; }

(defun save-file (savef)
  "Write the saved game on the file."
  ;; register FILE *savef;
  ;; {
  ;;     int slines = LINES;
  ;;     int scols = COLS;
    
  ;;     wmove(cw, LINES-1, 0);
  ;;     draw(cw);
  ;;     fstat(fileno(savef), &sbuf);
  ;;     fwrite("junk", 1, 5, savef);
  ;;     fseek(savef, 0L, 0);
	
  ;;     encwrite(version,strlen(version)+1,savef);
  ;;     encwrite(&sbuf.st-ino,sizeof(sbuf.st-ino),savef);
  ;;     encwrite(&sbuf.st-dev,sizeof(sbuf.st-dev),savef);
  ;;     encwrite(&sbuf.st-ctime,sizeof(sbuf.st-ctime),savef);
  ;;     encwrite(&sbuf.st-mtime,sizeof(sbuf.st-mtime),savef);
  ;;     encwrite(&slines,sizeof(slines),savef);
  ;;     encwrite(&scols,sizeof(scols),savef);
    
  ;;     rs-save-file(savef);

  ;;     fclose(savef);
  ;; }
  (save-resettables savef))


(defun restore (file)
  (load file)
  ;; Since we're just doing a dumb LOAD, the few cases of things
  ;; referring to other things won't be fixed up correctly, so we take
  ;; care of them here.  Note that OLDRP also refers to ROOMS but it
  ;; doesn't matter as it's only used for its value.
  (setf cur-armor (find cur-armor pack :test 'equalp))
  (setf cur-weapon (find cur-weapon pack :test 'equalp))

  (setf cw (cl-charms/low-level:newwin cl-charms/low-level:*lines* cl-charms/low-level:*cols* 0 0)
        mw (cl-charms/low-level:newwin cl-charms/low-level:*lines* cl-charms/low-level:*cols* 0 0)
        hw (cl-charms/low-level:newwin cl-charms/low-level:*lines* cl-charms/low-level:*cols* 0 0))
  (cl-charms/low-level:nocbreak)
  (zero! mpos)
  ;;(cl-charms/low-level:mvwprintw cw 0 0 "%s: %s", file, ctime(&sbuf2.st-mtime));

  (setf file-name file)
  (setup)
  (cl-charms/low-level:clearok cl-charms/low-level:*curscr* cl-charms/low-level:true)
  (cl-charms/low-level:touchwin cw)
  (status)
  (playit)
  ;; NOTREACHED
  )

;; 
;; perform an encrypted write
;;  
;; encwrite(starta, size, outf)
;; register void *starta;
;; unsigned int size;
;; register FILE *outf;
;; {
;;     register char *ep;
;;     register char *start = starta;

;;     ep = encstr;

;;     while (size--)
;;     {
;;         putc(*start++ ^ *ep++, outf);
;;         if (*ep == '\0')
;;             ep = encstr;
;;     }
;; }

;; 
;; perform an encrypted read
;;  
;; encread(starta, size, inf)
;; register void *starta;
;; unsigned int size;
;; register int inf;
;; {
;;     register char *ep;
;;     register int read-size;
;;     register char *start = starta;

;;     if ((read-size = read(inf, start, size)) == -1 || read-size == 0)
;;         return read-size;

;;     ep = encstr;

;;     while (size--)
;;     {
;;         *start++ ^= *ep++;
;;         if (*ep == '\0')
;;             ep = encstr;
;;     }
;;     return read-size;
;; }
