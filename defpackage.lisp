(in-package #:common-lisp-user)

(defpackage #:cl-fluiddb
  (:use #:cl)
  (:export

   ;; variables
   #:*use-https*
   #:*credentials*
   #:*proxy-server*
   #:*proxy-credentials*
   #:*user-agent*
   #:*call-timeout*
   
   ;; functions and macros
   #:with-credentials
   #:status-code
   #:status-message
   #:error-class
   #:request-id
   #:error-body
   
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