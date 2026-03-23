(require :asdf)
(pushnew *default-pathname-defaults* asdf:*central-registry* :test #'equal)
(asdf:load-system :cl-rogue)
(sb-ext:save-lisp-and-die "cl-rogue"
                          :toplevel #'cl-rogue:rogue
                          :executable t)
