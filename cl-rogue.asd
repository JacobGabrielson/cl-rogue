;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(declaim (optimize (debug 3) (safety 3)))

(defpackage #:cl-rogue-asd
  (:use :cl :asdf))

(in-package :cl-rogue-asd)

(defsystem "cl-rogue"
           :version "0.0.2"
           :maintainer "Jacob Gabrielson <jacobg23@pobox.com>"
           :license "MIT"
           :depends-on (:cl-ncurses :sb-posix)
           :components (
                        (:file "armor" :depends-on ("init"))
                        (:file "chase" :depends-on ("init"))
                        (:file "command" :depends-on ("init"))
                        (:file "daemon" :depends-on ("init"))
                        (:file "daemons" :depends-on ("init"))
                        (:file "fight" :depends-on ("init"))
                        (:file "init" :depends-on ("rogue"))
                        (:file "io" :depends-on ("init"))
                        (:file "main" :depends-on ("init"))
                        (:file "misc" :depends-on ("init"))
                        (:file "monsters" :depends-on ("init"))
                        (:file "move" :depends-on ("init"))
                        (:file "newlevel" :depends-on ("init"))
                        (:file "options" :depends-on ("init"))
                        (:file "pack" :depends-on ("init"))
                        (:file "package")
                        (:file "passages" :depends-on ("init"))
                        (:file "potions" :depends-on ("init"))
                        (:file "rings" :depends-on ("init"))
                        (:file "rip" :depends-on ("init"))
                        (:file "rogue" :depends-on ("package" "vers"))
                        (:file "rooms" :depends-on ("init"))
                        (:file "scrolls" :depends-on ("init"))
                        (:file "sticks" :depends-on ("init"))
                        (:file "things" :depends-on ("init"))
                        (:file "vers" :depends-on ("package"))
                        (:file "weapons" :depends-on ("init"))
                        (:file "wizard" :depends-on ("init"))
                        ))
  
