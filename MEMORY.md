# Project Memory

> **This file is append-only.** Add new entries as needed, but do not modify or delete existing content. Commit changes when updated.

## Project

`cl-rogue` — a line-for-line Common Lisp port of BSD Rogue 3.6.2, by Jacob Gabrielson.
Repo: https://github.com/JacobGabrielson/cl-rogue

## Environment

- OS: Ubuntu (Linux), machine is an AWS instance
- Lisp: SBCL 2.2.9 (at `/usr/bin/sbcl`)
- Quicklisp installed at `~/quicklisp/`, init snippet in `~/.sbclrc`
- Key dependency: `cl-charms` (ncurses CFFI wrapper), installed via Quicklisp
- System ncurses: `libncurses-dev` already present

## Building

```bash
sbcl --non-interactive \
  --eval '(push #P"/home/ubuntu/projects/cl-rogue/" asdf:*central-registry*)' \
  --eval '(ql:quickload "cl-rogue")' \
  --eval '(sb-ext:save-lisp-and-die "/home/ubuntu/projects/cl-rogue/cl-rogue" :toplevel #'"'"'cl-rogue:rogue :executable t)'
```

The resulting binary is `./cl-rogue` (gitignored). Run it in a real terminal (not a pipe).

## Git

- Author: Jacob Gabrielson <jacobg23@pobox.com> (set globally)
- Active branch: `remove-curses`
- No "Co-Authored-By: Claude" lines in commits
- Commit style: explain *why* (intent/motivation); explain *how* only when not obvious from the diff

## Bugs Fixed (on current branch)

### 1. Double `endwin` on exit (`main.lisp`)
The original code used `charms:with-curses` (which calls `initscr`/`endwin` via its own `unwind-protect`) wrapped around an inner `unwind-protect` that also manually called `initscr` and `endwin`. This caused `endwin` to be called twice, with the second returning ERR and raising an unhandled error.

Fix: removed the inner `unwind-protect` and replaced `with-curses` with a manual `unwind-protect` using `ignore-errors` around `endwin`, so a crash during gameplay doesn't cascade into a second unhandled error.

### 2. Read-only bit-vector crash in `do-passages` (`passages.lisp`)
`save-lisp-and-die` moves literal constants (including `#*` bit-vector literals embedded in compiled code) to read-only memory. The `isconn` fields of `rdes` structs were initialized with `#*000000000` literals and then mutated via `(setf (aref ...))`, causing a `MEMORY-FAULT-ERROR` on every call to `new-level` — silent exit with no message.

Fix: replaced `#*000000000` literals with `(make-array 9 :element-type 'bit)` to allocate fresh writable bit-vectors at runtime.

## Bugs Fixed (on `remove-curses` branch — `terminal.lisp` + one game file)

### 3. `@` trail when moving (`terminal.lisp`)
`wrefresh` was calling `(overwrite win *stdscr*)`, copying `cw` (which has `@`) into `*stdscr*` (terrain buffer). `winat` reads terrain from `*stdscr*`, so it would return `@` at the old hero position, leaving a trail.
Fix: removed the `overwrite` call. `*physical-buffer*` handles incremental rendering independently; `*stdscr*` stays as pure terrain.

### 4. Echo not restored on exit (`terminal.lisp`)
`setup()` calls `cbreak()` → `enter-raw-mode()` a second time after `initscr` already called it. The second call overwrote `*saved-termios*` with already-raw settings (echo off), so `leave-raw-mode` restored the wrong state.
Fix: only save termios on the first call (`unless *saved-termios*`).

### 5. Garbled help screen — tab expansion (`terminal.lisp`)
Help strings use `\t` to align columns. `win-put-char` stored the raw tab byte as a single cell. ncurses expands tabs to the next 8-column stop.
Fix: detect code 9 in `win-put-char` and fill with spaces to next 8-column boundary.

### 6. Blank-space blockers / invisible monsters (`terminal.lisp`)
`mvwinch`/`mvinch` in ncurses move the window cursor to (y,x) before returning the character — `winat` relies on this so that the subsequent `winch(mw)` reads from the same cell. Our implementation read from (y,x) but never moved the cursor, so `winch(mw)` read from a stale position (one cell past the last `mvwaddch`), returning space. This blocked movement and hid monsters. Also broke stair/trap placement which expected `*stdscr*`'s cursor to be at the found position after `winat`.
Fix: `mvwinch` calls `wmove` before reading; `mvinch` calls `move`.

### 7. Monster ghost-through-player (`chase.lisp`)
In C rogue `tp->t_dest = &hero` is a live pointer. In CL, `do-move` uses `(setf hero (copy-structure nh))` which creates a new coord object, leaving `thing-t-dest` pointing to the old position. Monster chased old pos, stepped onto player's cell without fighting, then appeared behind the player.
Fix: in `do-chase`, added `(when (equalp ch-ret hero) (return-from do-chase (attack th)))` before moving the monster.

### 8. Inventory items all on one row (`terminal.lisp`)
`waddch(win, '\n')` in ncurses advances to the next row and resets x to 0. `win-put-char` was treating newline as an ordinary character, storing it in the buffer and advancing x.
Fix: detect code 10 in `win-put-char`, increment `cursor-y`, reset `cursor-x` to 0, write nothing to buffer.

## Key Files

| File | Purpose |
|------|---------|
| `cl-rogue.asd` | System definition; entry point is `cl-rogue:rogue` |
| `main.lisp` | `rogue` toplevel fn, curses init/teardown, `playit` game loop |
| `init.lisp` | Global state via `define-resettable`; monster/item tables |
| `passages.lisp` | Level passage generation (`do-passages`) |
| `rogue.lisp` | Shared constants, structs, and utility macros |

## Known Gotchas

- `define-resettable` creates params with a stored lambda resetter; `reset-rogue-symbols` re-initializes all of them. If a resetter returns the same frozen object (e.g. a literal), mutations to it will fail after image save.
- `cl-charms/low-level:*cols*` and `*lines*` are only valid after `initscr` is called.
- The `cl-charms` `finalize` function calls `check-status` on `endwin` and raises on ERR — don't use `with-curses` if you need `ignore-errors` behavior on cleanup.
