;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(declaim (optimize (debug 3) (safety 3)))

(asdf:defsystem cl-rogue
  :description "A line-for-line port of C Rogue to Common Lisp" 
  :version "3.6.2"
  :author "Jacob Gabrielson <jacobg23@pobox.com>"
  :maintainer "Jacob Gabrielson <jacobg23@pobox.com>"
  :license "MIT"
  :depends-on (:cl-charms)
  :serial t
  :source-control (:git "https://github.com/JacobGabrielson/cl-rogue.git")
  :entry-point "cl-rogue:rogue"
  :components ((:file "package")
	       (:file "vers")
	       (:file "rogue")
	       (:file "init")
	       (:file "io")
	       (:file "save")
	       (:file "armor")
	       (:file "chase")
	       (:file "command")
	       (:file "daemon")
	       (:file "daemons")
	       (:file "weapons")
	       (:file "fight")
	       (:file "misc")
	       (:file "monsters")
	       (:file "move")
	       (:file "newlevel")
	       (:file "options")
	       (:file "pack")
	       (:file "passages")
	       (:file "potions")
	       (:file "rings")
	       (:file "rip")
	       (:file "rooms")
	       (:file "scrolls")
	       (:file "sticks")
	       (:file "things")
	       (:file "wizard")
	       (:file "main")))


