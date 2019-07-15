#!/bin/sh

PORT=4099
echo "To debug use M-x slime-connect to port $PORT, in Emacs"

parent_dir=$(dirname $(readlink -f $(dirname "$0")))"/"
rogue=$(mktemp -t rogueXXXXXXX.lisp)
cat > $rogue <<EOF
(ql:quickload "cl-charms")
(ql:quickload "swank")
(swank:create-server :port $PORT)
(pushnew (merge-pathnames "$parent_dir" (user-homedir-pathname)) asdf:*central-registry*)
(format t "~&Added $parent_dir to asdf:*central-registry*")
(asdf:load-system :cl-rogue)
(format t "~&To debug, use M-x slime-connect to port $PORT, in Emacs
Hit RETURN to continue")
(read-line)
(cl-rogue:rogue)
EOF

# SBCL seems to turn off input (or something like that) if you don't
# use the --load argument.
sbcl --load $rogue
