# Removing the ncurses Dependency

## Motivation

cl-rogue depends on `cl-charms`, a CFFI wrapper around libncurses. This is a
heavyweight dependency: it requires the C library at runtime, a working CFFI
toolchain at build time, and a grovelling step that compiles C code. For a
program this size, all of that is unnecessary — the terminal operations rogue
needs are a small, well-defined set that can be implemented directly in CL
using ANSI/VT100 escape sequences and POSIX termios, both of which SBCL
already exposes.

## Strategy

Rather than changing every call-site in the codebase, we implement a
compatibility package named `cl-charms/low-level` that exports exactly the
symbols the game already uses. This means:

- No changes to any game source file
- The ASD drops `:cl-charms` from `:depends-on` and adds `terminal.lisp`
- All terminal logic lives in one new file: `src/terminal.lisp`

## API Surface

39 symbols need to be provided. Grouped by role:

### Global variables
| Symbol | Purpose |
|--------|---------|
| `*stdscr*` | Window struct mirroring the physical screen state |
| `*curscr*` | Alias for `*stdscr*`; used when forcing full redraws |
| `*lines*` | Terminal row count, set by `initscr` |
| `*cols*`  | Terminal column count, set by `initscr` |

### Constants
| Symbol | Value | Purpose |
|--------|-------|---------|
| `err`  | -1    | Returned by `wgetch` on error/EOF |
| `true` | 1     | Boolean truthy value for `clearok` |

### Lifecycle
| Function | Notes |
|----------|-------|
| `initscr` | Enters alternate screen, sets raw mode, reads terminal size via `ioctl(TIOCGWINSZ)`, allocates `*stdscr*` |
| `endwin`  | Restores termios, leaves alternate screen |
| `cbreak`  | Enters raw/cbreak mode |
| `nocbreak`| Restores saved termios (cooked mode) |
| `noecho`  | No-op — already handled by clearing `ECHO` in `cbreak` |

### Window management
| Function | Notes |
|----------|-------|
| `newwin (rows cols y x)` | Allocates a new window struct; `y x` ignored (all windows full-screen in rogue) |

### Clearing
| Function | Notes |
|----------|-------|
| `clear`           | Fills `*stdscr*` with spaces, schedules full physical redraw |
| `wclear (win)`    | Fills window buffer with spaces, sets its clear flag |
| `wclrtoeol (win)` | Fills from cursor to end of row with spaces |

### Cursor movement
| Function | Notes |
|----------|-------|
| `move (y x)`         | Sets `*stdscr*` cursor position |
| `wmove (win y x)`    | Sets window cursor position |
| `getyx (win y x)`    | **Must be a macro** — sets the Lisp variables `y` and `x` to the window's cursor position |

`getyx` is the trickiest symbol: in ncurses it's a C macro that assigns to
its second and third arguments. The game calls it as
`(getyx cw oy ox)` where `oy` and `ox` are `let`-bound variables expecting
to be mutated. This is only possible if `getyx` is a CL macro.

### Character/string output
All output functions write into the window's in-memory buffer; nothing goes
to the terminal until `wrefresh` is called.

| Function | Notes |
|----------|-------|
| `addch (code)`              | Write char (integer code) at `*stdscr*` cursor, advance cursor |
| `waddch (win code)`         | Same, into `win` |
| `mvaddch (y x code)`        | Move then `addch` on `*stdscr*` |
| `mvwaddch (win y x code)`   | Move then `waddch` |
| `waddstr (win str)`         | Write string into `win` |
| `mvwaddstr (win y x str)`   | Move then `waddstr` |
| `mvaddstr (y x str)`        | Move then write string on `*stdscr*` |
| `printw (str)`              | Write string on `*stdscr*` |
| `wprintw (win str)`         | Write string on `win` |

### Reading from window buffer
These read from the in-memory buffer, not from the terminal.

| Function | Notes |
|----------|-------|
| `winch (win)`        | Char code at `win`'s current cursor position |
| `mvinch (y x)`       | Char code at `(y,x)` in `*stdscr*` |
| `mvwinch (win y x)`  | Char code at `(y,x)` in `win` |

### Window copy
| Function | Notes |
|----------|-------|
| `overwrite (src dst)` | Copy entire src buffer to dst, including spaces |
| `overlay (src dst)`   | Copy src to dst, skipping space characters |

### Screen refresh
| Function | Notes |
|----------|-------|
| `wrefresh (win)`    | Sync window to physical terminal (see below) |
| `touchwin (win)`    | Mark window as needing full redraw on next `wrefresh` |
| `clearok (win flag)`| If flag is non-zero: for `*curscr*`, set global full-redraw flag; for other windows, set per-window clear flag |

### Input
| Function | Notes |
|----------|-------|
| `wgetch (win)`  | Block and return one char code from stdin; `win` arg ignored |
| `flushinp`      | Discard pending input via `tcflush(TCIFLUSH)` |
| `erasechar`     | Return the terminal's erase character (read from saved termios `VERASE`) |
| `killchar`      | Return the terminal's line-kill character (read from saved termios `VKILL`) |

## Window Model

Each window is a struct containing:
- `rows`, `cols` — dimensions
- `cursor-y`, `cursor-x` — current logical cursor position
- `buffer` — a `(simple-array character (rows cols))` of displayed characters
- `needs-clear` — boolean flag: clear physical screen before next refresh

`*stdscr*` is an ordinary window struct. Its buffer is kept in sync with the
physical terminal: whenever `wrefresh(win)` is called for any window, we copy
`win`'s buffer into `*stdscr*`'s buffer after updating the terminal. This
ensures that subsequent calls to `mvwinch(*stdscr*, y, x)` return whatever is
currently visible — the same contract that ncurses `*stdscr*` provides.

`*curscr*` is bound to the same object as `*stdscr*`.

## Physical Screen Rendering

We maintain a separate `*physical-buffer*` array tracking exactly what
characters are currently on the terminal. On each `wrefresh`:

1. If a full redraw is requested (via `clearok` or `touchwin`): emit
   `ESC[2J ESC[H` to clear the terminal, reset `*physical-buffer*` to
   spaces, then proceed with a full redraw.
2. Scan every `(row, col)` position. If `win[row][col]` differs from
   `physical[row][col]`, emit an ANSI cursor-move sequence if the physical
   cursor isn't already there, then write the character.
3. Copy `win`'s buffer into `*stdscr*`'s buffer.
4. Move the physical cursor to the window's logical cursor position.
5. Call `finish-output` to flush.

This incremental approach avoids screen flicker while keeping the
implementation simple.

ANSI sequences used:
- `ESC[?1049h` / `ESC[?1049l` — enter/leave alternate screen buffer
- `ESC[2J` — clear screen
- `ESC[H` — cursor to home (1,1)
- `ESC[r;cH` — cursor to row r, col c (1-indexed)
- Plain characters for output

## Terminal Raw Mode

SBCL's `sb-posix` package provides Lisp bindings to `tcgetattr`/`tcsetattr`.
On `initscr` we save the current termios and set:
- Clear `ICANON` (disable line buffering)
- Clear `ECHO` (disable echo)
- Clear `ISIG` (disable signal characters like ^C... or leave enabled; rogue uses ^C to quit)
- `VMIN=1`, `VTIME=0` (block until one character is available)

On `endwin` / `nocbreak` we restore the saved termios.

## Terminal Size

`ioctl(stdout_fd, TIOCGWINSZ, &winsize)` returns the terminal dimensions.
This is accessed via `sb-unix:unix-ioctl` and `sb-alien:with-alien`.
`TIOCGWINSZ = 0x5413` on Linux.

## Files Changed

| File | Change |
|------|--------|
| `terminal.lisp` | New file — full implementation |
| `cl-rogue.asd` | Remove `:cl-charms` from `:depends-on`; add `(:file "terminal")` before `(:file "package")` |

No game source files are modified.
