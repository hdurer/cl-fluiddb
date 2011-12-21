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
  :licence "BSD 2-clause license - http://www.opensource.org/licenses/BSD-2-Clause"
  :description "Library to access the Fluidinfo database / storage and search platform"
  
  :serial t
  :version #.*cl-fluiddb-version*
  :depends-on (:cl-json :drakma :flexi-streams :bordeaux-threads :split-sequence)
  :components ((:file "defpackage")
               (:file "fluiddb")))
