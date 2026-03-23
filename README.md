# CL-ROGUE

This is a nearly line-for-line port of Rogue v3.6.2 from C to Common
Lisp. The goal of this exercise was to see how easy it would be to
write idiomatic 1980s-era C, well-written but with lots of
side-effects and stuff, in Common Lisp. In the process of porting the
code, I gained an appreciation for the pragmatic elegance of the Rogue
code base. It also showed me that it was possible to keep the code
almost exactly the same across both languages (same variable names,
same control flow), with only a few exceptions.

The source code is most likely
[here](http://www.roguelikedevelopment.org/archive/files/sourcecode/from_bsd_usenix87_rogue3.6.zip);
see also [version 5.4.4](https://github.com/Davidslv/rogue).

## Dependencies

### SBCL

Only SBCL has been tested. No external Common Lisp libraries are
required — terminal I/O is handled by a pure-CL VT100 implementation
(`terminal.lisp`) that replaces the original ncurses dependency.

## Running

### Locally

Build the binary once with the provided script:

```bash
sbcl --load load.lisp
```

This produces a standalone `cl-rogue` binary. Then run it in any
screen-oriented terminal:

```bash
./cl-rogue
```

Alternatively, load and run directly from SBCL without building:

```lisp
(pushnew (merge-pathnames "src/cl-rogue/" (user-homedir-pathname)) asdf:*central-registry*)
(asdf:load-system :cl-rogue)
(cl-rogue:rogue)
```

**Note**: substitute the actual path to the repository for `"src/cl-rogue/"`.

### Using Docker

Using the `Dockerfile` in the `cl-rogue` directory, you should be able
to do the following:

```bash
cd cl-rogue
docker build -t cl-rogue .
docker run -i -t cl-rogue
```

### Headless driver (Python)

The `driver/` directory contains a Python headless driver that runs
cl-rogue in a PTY (no real terminal required) and lets you send
keystrokes and inspect the screen programmatically — useful for
scripted play and bug-finding:

```bash
cd driver
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
python example.py
```
