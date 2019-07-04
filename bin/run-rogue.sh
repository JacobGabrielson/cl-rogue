#!/bin/sh

parent_dir=$(dirname $(readlink -f $(dirname "$0")))"/"
echo $parent_dir
sbcl <<EOF
(ql:quickload "cl-ncurses")
(pushnew (merge-pathnames "$parent_dir" (user-homedir-pathname)) asdf:*central-registry*)
(asdf:operate 'asdf:load-op 'cl-rogue)
(cl-rogue:rogue)
EOF
