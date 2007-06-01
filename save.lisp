;;;; save and restore routines
;;;; @(#)save.c	3.9 (Berkeley) 6/16/81

(in-package :cl-rogue)

(defun save_game ()
  (let (gotfile)
    (zero! mpos)
    (when (plusp (length file_name))
      (msg "Save file (~a)? " file_name)
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
           (msg "File name: ~a" file_name)))))

    (loop
       (if gotfile
           (setf gotfile nil)
           ;; Prompt for the file unless the prompt above resulted in
           ;; a valid filename.
           (progn
             (msg "File name: ")
             (zero! mpos)
             (multiple-value-bind (file_name_chosen status)
                 (get_str "" cw)
               (when (eql status QUIT)
                 (msg "")
                 (return nil))
               (setf file_name file_name_chosen))))

       (handler-case 
           (with-open-file (savef file_name
                                  :direction :output 
                                  :if-exists :supersede)
             (save-file savef)
             (close savef)
             (return-from save_game t))
         (file-error (c)
           (msg "~a" (caddr (simple-condition-format-arguments c))))))))

;; 
;; automatically save a file.  This is used if a HUP signal is
;; recieved
;;  
;; void
;; auto_save(int p)
;; {
;;     register FILE *savef;
;;     register int i;

;;     for (i = 0; i < NSIG; i++)
;;         signal(i, SIG_IGN);
;;     if (file_name[0] != '\0' && (savef = fopen(file_name, "w")) != NULL)
;;         save_file(savef);
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
  ;;     encwrite(&sbuf.st_ino,sizeof(sbuf.st_ino),savef);
  ;;     encwrite(&sbuf.st_dev,sizeof(sbuf.st_dev),savef);
  ;;     encwrite(&sbuf.st_ctime,sizeof(sbuf.st_ctime),savef);
  ;;     encwrite(&sbuf.st_mtime,sizeof(sbuf.st_mtime),savef);
  ;;     encwrite(&slines,sizeof(slines),savef);
  ;;     encwrite(&scols,sizeof(scols),savef);
    
  ;;     rs_save_file(savef);

  ;;     fclose(savef);
  ;; }
  (save-resettables savef))


(defun restore (file)
  (load file)
  ;; Since we're just doing a dumb LOAD, the few cases of things
  ;; referring to other things won't be fixed up correctly, so we take
  ;; care of them here.  Note that OLDRP also refers to ROOMS but it
  ;; doesn't matter as it's only used for its value.
  (setf cur_armor (find cur_armor pack :test 'equalp))
  (setf cur_weapon (find cur_weapon pack :test 'equalp))

  (setf cw (cl-ncurses:newwin cl-ncurses:*lines* cl-ncurses:*cols* 0 0)
        mw (cl-ncurses:newwin cl-ncurses:*lines* cl-ncurses:*cols* 0 0)
        hw (cl-ncurses:newwin cl-ncurses:*lines* cl-ncurses:*cols* 0 0))
  (cl-ncurses:nocbreak)
  (zero! mpos)
  ;;(cl-ncurses:mvwprintw cw 0 0 "%s: %s", file, ctime(&sbuf2.st_mtime));

  (setf file_name file)
  (setup)
  (cl-ncurses:clearok cl-ncurses:*curscr* cl-ncurses:true)
  (cl-ncurses:touchwin cw)
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
;;     register int read_size;
;;     register char *start = starta;

;;     if ((read_size = read(inf, start, size)) == -1 || read_size == 0)
;;         return read_size;

;;     ep = encstr;

;;     while (size--)
;;     {
;;         *start++ ^= *ep++;
;;         if (*ep == '\0')
;;             ep = encstr;
;;     }
;;     return read_size;
;; }
