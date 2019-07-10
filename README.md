# CL-ROGUE

This is a nearly line-for-line port of Rogue (most likely 
[version 5.4.4](https://github.com/Davidslv/rogue)) 
from C to Common Lisp. The goal of this exercise was to see how easy
it would be to write idiomatic 1980s-era C, well-written but with lots of
side-effects and stuff, in Common Lisp. In the process of porting the
code, I gained an appreciation for the pragmatic elegance of the Rogue
code base. It also showed me that it was possible to keep the code
almost exactly the same across both languages (same variable names,
same control flow), with only a few exceptions.

## Dependencies

### Quicklisp 

CL-ROGUE is a lot easier to get running with
[Quicklisp](https://www.quicklisp.org/beta/); so it is highly
recommended you set that up first.

### Curses Library

Make sure you've installed ncurses. For example on Ubuntu 18.04 you'd
execute:

```bash
sudo apt-get install -y libncurses5-dev
```

Presently CL-ROGUE also depends on the
[CL-CHARMS](https://github.com/HiTECNOLOGYs/cl-charms) Common Lisp
library which wraps ncurses. The older CL-NCURSES package no longer
works reliably on all platforms.

## Running

Once you've ensured you have the above dependencies, you should be
able to execute the following commands in a screen-oriented terminal:

```lisp
(pushnew (merge-pathnames "src/cl-rogue/" (user-homedir-pathname)) asdf:*central-registry*)
(asdf:operate 'asdf:load-op 'cl-rogue)
(cl-rogue:rogue)
```

Note that you should substitute in something else for `"src/cl-rogue"`
if that's not where you put it.

Only SBCL v1.4.5 has been tested recently.
