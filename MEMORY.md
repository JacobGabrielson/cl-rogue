# Project Memory

> **This file is read-only.** Do not auto-update it via the memory system. Edit manually and commit when needed.

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
- Active branch: `bugfix/double-endwin-on-exit`
- No "Co-Authored-By: Claude" lines in commits
- Commit style: explain *why* (intent/motivation); explain *how* only when not obvious from the diff

## Bugs Fixed (on current branch)

### 1. Double `endwin` on exit (`main.lisp`)
The original code used `charms:with-curses` (which calls `initscr`/`endwin` via its own `unwind-protect`) wrapped around an inner `unwind-protect` that also manually called `initscr` and `endwin`. This caused `endwin` to be called twice, with the second returning ERR and raising an unhandled error.

Fix: removed the inner `unwind-protect` and replaced `with-curses` with a manual `unwind-protect` using `ignore-errors` around `endwin`, so a crash during gameplay doesn't cascade into a second unhandled error.

### 2. Read-only bit-vector crash in `do-passages` (`passages.lisp`)
`save-lisp-and-die` moves literal constants (including `#*` bit-vector literals embedded in compiled code) to read-only memory. The `isconn` fields of `rdes` structs were initialized with `#*000000000` literals and then mutated via `(setf (aref ...))`, causing a `MEMORY-FAULT-ERROR` on every call to `new-level` — silent exit with no message.

Fix: replaced `#*000000000` literals with `(make-array 9 :element-type 'bit)` to allocate fresh writable bit-vectors at runtime.

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
