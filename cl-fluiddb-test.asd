;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
(in-package :cl-user)


(in-package :cl-user)




(asdf:defsystem :cl-fluiddb-test
  :name "cl-fluiddb-test"
  :author "codemonkey@betareduction.info"
  
  :serial t
  :depends-on (:cl-fluiddb :lift)
  :components ((:file "test/defpackage")
               (:file "test/cl-fluiddb-test")))
