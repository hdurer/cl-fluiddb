;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
(in-package :cl-user)

(defpackage :cl-fluiddb-asd
  (:use :cl :asdf))

(in-package :cl-fluiddb-asd)

(defvar *cl-fluiddb-version* "0.1"
  "A string denoting the current version of this package.  Used for
diagnostic output.")



(asdf:defsystem :cl-fluiddb
  :name "cl-fluiddb"
  :author "codemonkey@betareduction.info"
  
  :serial t
  :version #.*cl-fluiddb-version*
  :depends-on (:cl-json :drakma)
  :components ((:file "defpackage")
               (:file "fluiddb")))
