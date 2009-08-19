(in-package #:common-lisp-user)

(defpackage #:cl-fluiddb
  (:use #:cl)
  (:export
   
   ;; functions
   #:get-user
   
   #:get-object
   #:query-objects
   #:create-object
   #:get-object-tag
   #:change-object-tag

   #:get-namespace
   #:create-namespace
   #:change-namespace
   #:delete-namespace
   ))