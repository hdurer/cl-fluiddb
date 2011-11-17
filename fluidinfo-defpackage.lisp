(in-package #:common-lisp-user)

(defpackage #:cl-fluidinfo
   (:use #:cl-fluiddb)
   (:export
    ))

(let ((old-package (find-package :cl-fluiddb))
      (new-package (find-package :cl-fluidinfo)))
  (loop for sym being the external-symbol in old-package
	do (export sym new-package)))
