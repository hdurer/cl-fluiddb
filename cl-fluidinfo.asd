;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
(in-package :cl-user)




(asdf:defsystem :cl-fluidinfo
  :name "cl-fluidinfo"
  :author "codemonkey@betareduction.info"
  
  :serial t
  :version "0.1"
  :depends-on (:cl-fluiddb)
  :components ((:file "fluidinfo-defpackage")))
