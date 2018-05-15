This is a nearly line-for-line port of Rogue (most likely [version 5.4.4](https://github.com/Davidslv/rogue)) from C to Common Lisp. 

If you execute the following commands in a screen-oriented terminal,
it should work:

```
(pushnew (merge-pathnames "src/cl-rogue/" (user-homedir-pathname)) asdf:*central-registry*)
(asdf:operate 'asdf:load-op 'cl-rogue)
(cl-rogue:rogue)
```

