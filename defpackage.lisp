(in-package #:common-lisp-user)

(defpackage #:cl-fluiddb
  (:use #:cl)
  (:export

   ;; variables
   #:*credentials*
   #:proxy-server*
   #:proxy-credentials*

   
   ;; functions and macros
   #:with-credentials
   
   #:get-user
   
   #:get-object
   #:query-objects
   #:create-object
   #:get-object-tag-value
   #:set-object-tag-value

   #:get-namespace
   #:create-namespace
   #:change-namespace
   #:delete-namespace

   #:get-namespace-permissions
   #:set-namespace-permissions
   #:get-tag-permissions
   #:set-tag-permissions
   #:get-tag-value-permissions
   #:set-tag-value-permissions
   
   #:get-policy
   #:set-policy

   #:create-tag
   #:get-tag
   #:change-tag
   #:delete-tag
   
   ))