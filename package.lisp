
(in-package :cl-user)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :cl-rogue
    (:use :common-lisp)
    (:export :ROGUE)))
