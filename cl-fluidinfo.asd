;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
(in-package :cl-user)

(asdf:defsystem :cl-fluidinfo
  :name "cl-fluidinfo"
  :author "codemonkey@betareduction.info"
  :licence "BSD 2-clause license - http://www.opensource.org/licenses/BSD-2-Clause"
  :description "Library to access the Fluidinfo database / storage and search platform"
  
  :serial t
  :version "0.1"
  :depends-on (:cl-fluiddb)
  :components ((:file "fluidinfo-defpackage")))
