This is a nearly line-for-line port of Rogue (most likely [version 5.4.4](https://github.com/Davidslv/rogue)) from C to Common Lisp. The goal of this exercise was to see how easy it would be to write idiomatic 1980s C, well-written but with lots of side-effects and stuff, in Common Lisp. In the process of porting the code, I gained an appreciation for the Rogue code base. It also showed me that it was possible to keep the code almost exactly the same across both languages (same variable names, same control flow), with a couple of exceptions.

If you execute the following commands in a screen-oriented terminal,
it should work:

```
(pushnew (merge-pathnames "src/cl-rogue/" (user-homedir-pathname)) asdf:*central-registry*)
(asdf:operate 'asdf:load-op 'cl-rogue)
(cl-rogue:rogue)
```

