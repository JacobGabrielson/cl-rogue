;;;; terminal.lisp — Pure CL terminal handling for cl-rogue
;;;;
;;;; Provides the cl-charms/low-level package without depending on ncurses or
;;;; any Quicklisp library. Uses ANSI/VT100 escape sequences for screen output,
;;;; sb-posix for termios (raw mode), and sb-alien/sb-unix for ioctl
;;;; (terminal size).
;;;;
;;;; See doc/remove-curses.md for the full design rationale.

(defpackage #:cl-charms/low-level
  (:use #:common-lisp)
  (:export
   ;; Variables
   #:*stdscr* #:*curscr* #:*lines* #:*cols*
   ;; Constants
   #:err #:true
   ;; Lifecycle
   #:initscr #:endwin #:cbreak #:nocbreak #:noecho
   ;; Window creation
   #:newwin
   ;; Clearing
   #:clear #:wclear #:wclrtoeol
   ;; Cursor
   #:move #:wmove #:getyx
   ;; Output
   #:addch #:waddch #:mvaddch #:mvwaddch
   #:waddstr #:mvwaddstr #:mvaddstr
   #:printw #:wprintw
   ;; Refresh
   #:wrefresh #:touchwin #:clearok
   ;; Read from buffer
   #:winch #:mvinch #:mvwinch
   ;; Window copy
   #:overlay #:overwrite
   ;; Input
   #:wgetch #:flushinp #:erasechar #:killchar))

(in-package #:cl-charms/low-level)

;;; ===== Constants =====

(defconstant err -1)
(defconstant true 1)

;;; ===== Global state =====

(defvar *lines* 24 "Terminal row count.")
(defvar *cols*  80 "Terminal column count.")
(defvar *stdscr* nil "Window struct mirroring the current physical screen.")
(defvar *curscr* nil "Alias for *stdscr*; used when forcing full redraws.")

;;; Saved termios for restoration on endwin/nocbreak.
(defvar *saved-termios* nil)

;;; Physical buffer: tracks what characters are actually on the terminal.
(defvar *physical-buffer* nil)
(defvar *physical-cursor-y* 0)
(defvar *physical-cursor-x* 0)

;;; When t, the next wrefresh clears the entire screen before redrawing.
(defvar *needs-full-redraw* nil)

;;; ===== Window structure =====

(defstruct (rogue-window (:conc-name win-))
  (rows        0   :type fixnum)
  (cols        0   :type fixnum)
  (cursor-y    0   :type fixnum)
  (cursor-x    0   :type fixnum)
  (buffer      nil)               ; (simple-array character (rows cols))
  (needs-clear nil :type boolean))

(defun make-empty-window (rows cols)
  (make-rogue-window
   :rows rows :cols cols
   :cursor-y 0 :cursor-x 0
   :buffer (make-array (list rows cols)
                       :element-type 'character
                       :initial-element #\Space)
   :needs-clear nil))

;;; ===== Terminal size via ioctl =====

(defun get-terminal-size ()
  "Return (values rows cols) by calling ioctl(1, TIOCGWINSZ, ...).
  Falls back to 24x80 when not connected to a real terminal."
  (sb-alien:with-alien ((ws (sb-alien:struct winsize
                               (ws-row sb-alien:unsigned-short)
                               (ws-col sb-alien:unsigned-short)
                               (ws-xpixel sb-alien:unsigned-short)
                               (ws-ypixel sb-alien:unsigned-short))))
    ;; sb-alien:with-alien does not zero-initialize the struct, so we must do
    ;; it ourselves. If ioctl fails (e.g. stdout is a pipe, not a tty) the
    ;; fields are left at these zeros and we fall back to defaults below.
    (setf (sb-alien:slot ws 'ws-row)    0
          (sb-alien:slot ws 'ws-col)    0
          (sb-alien:slot ws 'ws-xpixel) 0
          (sb-alien:slot ws 'ws-ypixel) 0)
    ;; TIOCGWINSZ = 0x5413 on Linux. Use stdout fd=1.
    (sb-unix:unix-ioctl 1 #x5413 (sb-alien:alien-sap ws))
    (let ((rows (sb-alien:slot ws 'ws-row))
          (cols (sb-alien:slot ws 'ws-col)))
      (values (if (zerop rows) 24 rows)
              (if (zerop cols) 80 cols)))))

;;; ===== Terminal raw mode via sb-posix =====

(defun enter-raw-mode ()
  "Save current termios and switch to raw/cbreak + no-echo mode.
  Silently does nothing if stdin is not a real tty (e.g. during testing)."
  (handler-case
      (progn
        ;; Save a fresh copy for later restoration.
        (setf *saved-termios* (sb-posix:tcgetattr 0))
        (let ((termios (sb-posix:tcgetattr 0)))
          ;; Disable canonical mode and echo.
          (setf (sb-posix:termios-lflag termios)
                (logand (sb-posix:termios-lflag termios)
                        (lognot (logior sb-posix:icanon
                                        sb-posix:echo))))
          ;; Disable CR→NL translation on input so we get bare CR.
          (setf (sb-posix:termios-iflag termios)
                (logand (sb-posix:termios-iflag termios)
                        (lognot sb-posix:icrnl)))
          ;; VMIN=1, VTIME=0: block until exactly one character is available.
          (let ((cc (sb-posix:termios-cc termios)))
            (setf (aref cc sb-posix:vmin)  1
                  (aref cc sb-posix:vtime) 0))
          (sb-posix:tcsetattr 0 sb-posix:tcsaflush termios)))
    (sb-posix:syscall-error () nil)))

(defun leave-raw-mode ()
  "Restore the termios that was saved by enter-raw-mode."
  (when *saved-termios*
    (handler-case
        (sb-posix:tcsetattr 0 sb-posix:tcsaflush *saved-termios*)
      (sb-posix:syscall-error () nil))))

;;; ===== ANSI output helpers =====

(defmacro esc (fmt &rest args)
  `(format t ,(concatenate 'string "~c" fmt) #\Escape ,@args))

(defun move-physical-cursor (y x)
  ;; ANSI cursor addressing is 1-indexed.
  (esc "[~d;~dH" (1+ y) (1+ x))
  (setf *physical-cursor-y* y
        *physical-cursor-x* x))

;;; ===== Lifecycle =====

(defun initscr ()
  "Initialize the terminal: enter alternate screen, raw mode, read dimensions."
  (multiple-value-bind (rows cols) (get-terminal-size)
    (setf *lines* rows *cols* cols))
  (setf *physical-buffer*
        (make-array (list *lines* *cols*)
                    :element-type 'character
                    :initial-element #\Space)
        *physical-cursor-y* 0
        *physical-cursor-x* 0
        *needs-full-redraw* nil)
  (setf *stdscr* (make-empty-window *lines* *cols*)
        *curscr* *stdscr*)
  (enter-raw-mode)
  ;; Enter the alternate screen buffer so the game doesn't trash the
  ;; user's scrollback.
  (esc "[?1049h")
  (finish-output)
  *stdscr*)

(defun endwin ()
  "Restore terminal to normal state and leave the alternate screen."
  (leave-raw-mode)
  ;; Move to the last line before leaving so the shell prompt appears cleanly.
  (esc "[~d;1H" *lines*)
  (esc "[?1049l")
  (finish-output))

(defun cbreak ()
  (enter-raw-mode))

(defun nocbreak ()
  (leave-raw-mode))

(defun noecho ()
  ;; Already handled by clearing ECHO in enter-raw-mode.
  nil)

;;; ===== Window creation =====

(defun newwin (rows cols y x)
  "Allocate a new full-screen window. y and x are ignored; rogue always uses 0 0."
  (declare (ignore y x))
  (make-empty-window rows cols))

;;; ===== Clearing =====

(defun win-fill-spaces (win)
  (let ((buf (win-buffer win)))
    (dotimes (r (win-rows win))
      (dotimes (c (win-cols win))
        (setf (aref buf r c) #\Space))))
  (setf (win-cursor-y win) 0
        (win-cursor-x win) 0))

(defun clear ()
  "Clear *stdscr* and schedule a full physical redraw."
  (win-fill-spaces *stdscr*)
  (setf *needs-full-redraw* t))

(defun wclear (win)
  "Clear WIN's buffer and flag it for a full redraw on next wrefresh."
  (win-fill-spaces win)
  (setf (win-needs-clear win) t))

(defun wclrtoeol (win)
  "Clear from WIN's cursor to end of the current row."
  (let ((buf (win-buffer win))
        (y   (win-cursor-y win)))
    (loop for x from (win-cursor-x win) below (win-cols win)
          do (setf (aref buf y x) #\Space))))

;;; ===== Cursor movement =====

(defun move (y x)
  (setf (win-cursor-y *stdscr*) y
        (win-cursor-x *stdscr*) x))

(defun wmove (win y x)
  (setf (win-cursor-y win) y
        (win-cursor-x win) x))

;;; getyx must be a macro: the game calls (getyx win oy ox) expecting the
;;; Lisp variables oy and ox to be mutated, which a function cannot do.
(defmacro getyx (win y x)
  `(setf ,y (win-cursor-y ,win)
         ,x (win-cursor-x ,win)))

;;; ===== Character and string output =====

(defun win-put-char (win code)
  "Write CODE (integer char code) at WIN's cursor, then advance cursor."
  (let ((y (win-cursor-y win))
        (x (win-cursor-x win)))
    (when (and (< -1 y (win-rows win))
               (< -1 x (win-cols win)))
      (setf (aref (win-buffer win) y x) (code-char code))
      (incf (win-cursor-x win)))))

(defun addch   (code)        (win-put-char *stdscr* code))
(defun waddch  (win code)    (win-put-char win code))
(defun mvaddch (y x code)    (wmove *stdscr* y x) (addch code))
(defun mvwaddch (win y x code) (wmove win y x) (waddch win code))

(defun win-put-string (win str)
  (loop for ch across str do (win-put-char win (char-code ch))))

(defun waddstr   (win str)       (win-put-string win str))
(defun mvwaddstr (win y x str)   (wmove win y x) (win-put-string win str))
(defun mvaddstr  (y x str)       (wmove *stdscr* y x) (win-put-string *stdscr* str))
(defun printw    (str)           (win-put-string *stdscr* str))
(defun wprintw   (win str)       (win-put-string win str))

;;; ===== Reading from window buffer =====

(defun winch  (win)      (char-code (aref (win-buffer win) (win-cursor-y win) (win-cursor-x win))))
(defun mvinch (y x)      (char-code (aref (win-buffer *stdscr*) y x)))
(defun mvwinch (win y x) (char-code (aref (win-buffer win) y x)))

;;; ===== Window copy operations =====

(defun overwrite (src dst)
  "Copy all characters from SRC's buffer to DST's buffer, including spaces."
  (let ((rows (min (win-rows src) (win-rows dst)))
        (cols (min (win-cols src) (win-cols dst))))
    (dotimes (r rows)
      (dotimes (c cols)
        (setf (aref (win-buffer dst) r c)
              (aref (win-buffer src) r c))))))

(defun overlay (src dst)
  "Copy non-space characters from SRC's buffer to DST's buffer."
  (let ((rows (min (win-rows src) (win-rows dst)))
        (cols (min (win-cols src) (win-cols dst))))
    (dotimes (r rows)
      (dotimes (c cols)
        (let ((ch (aref (win-buffer src) r c)))
          (unless (char= ch #\Space)
            (setf (aref (win-buffer dst) r c) ch)))))))

;;; ===== Screen refresh =====

(defun do-full-clear ()
  "Emit escape sequences to clear the physical terminal and reset tracking."
  (esc "[2J")
  (esc "[H")
  (dotimes (r *lines*)
    (dotimes (c *cols*)
      (setf (aref *physical-buffer* r c) #\Space)))
  (setf *physical-cursor-y* 0
        *physical-cursor-x* 0
        *needs-full-redraw* nil))

(defun wrefresh (win)
  "Sync WIN's buffer to the physical terminal incrementally.

  If a full redraw has been requested (via clearok or touchwin), the screen is
  cleared first. Then we walk every cell: wherever WIN differs from the physical
  buffer we emit an ANSI cursor-move (if needed) and the new character.
  Finally we copy WIN into *stdscr* so that subsequent mvwinch calls on
  *stdscr* see the current display state."
  (when (or *needs-full-redraw* (win-needs-clear win))
    (do-full-clear)
    (setf (win-needs-clear win) nil))
  ;; Incremental update.
  (let ((buf  (win-buffer win))
        (pbuf *physical-buffer*))
    (dotimes (r (min (win-rows win) *lines*))
      (dotimes (c (min (win-cols win) *cols*))
        (let ((new-ch (aref buf  r c))
              (old-ch (aref pbuf r c)))
          (unless (char= new-ch old-ch)
            (unless (and (= r *physical-cursor-y*)
                         (= c *physical-cursor-x*))
              (move-physical-cursor r c))
            (write-char new-ch)
            (setf (aref pbuf r c) new-ch)
            (incf *physical-cursor-x*))))))
  ;; Keep *stdscr* in sync with what is now on screen.
  (unless (eq win *stdscr*)
    (overwrite win *stdscr*))
  ;; Park the physical cursor at the window's logical cursor position.
  (let ((cy (win-cursor-y win))
        (cx (win-cursor-x win)))
    (when (and (< -1 cy *lines*) (< -1 cx *cols*))
      (move-physical-cursor cy cx)))
  (finish-output))

(defun touchwin (win)
  "Force a full redraw of WIN on the next wrefresh call."
  (setf (win-needs-clear win) t))

(defun clearok (win flag)
  "Schedule a clear-before-redraw. On *curscr*, set the global flag (affects
  any subsequent wrefresh); on other windows, set a per-window flag."
  (when (and flag (not (zerop flag)))
    (if (eq win *curscr*)
        (setf *needs-full-redraw* t)
        (setf (win-needs-clear win) t))))

;;; ===== Input =====

(defun wgetch (win)
  "Read one character from stdin. WIN is ignored (ncurses uses it to refresh
  before reading; we rely on the game calling wrefresh explicitly). Returns
  the character code, or ERR on EOF."
  (declare (ignore win))
  (finish-output)
  (let ((ch (read-char *standard-input* nil nil)))
    (if ch (char-code ch) err)))

(defun flushinp ()
  "Discard any pending unread input."
  (sb-posix:tcflush 0 sb-posix:tciflush))

(defun erasechar ()
  "Return the terminal's configured erase character (typically DEL or ^H)."
  (if *saved-termios*
      (aref (sb-posix:termios-cc *saved-termios*) sb-posix:verase)
      127))  ; DEL

(defun killchar ()
  "Return the terminal's configured line-kill character (typically ^U)."
  (if *saved-termios*
      (aref (sb-posix:termios-cc *saved-termios*) sb-posix:vkill)
      21))  ; ^U
