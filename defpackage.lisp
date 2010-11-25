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

   ;; helpers
   #:url-format-namespace-or-tag
   
   ;; functions and macros
   #:with-credentials
   #:status-code
   #:status-message
   #:error-class
   #:request-id
   #:error-body
   
   #:get-user
   
   #:get-object
   #:get-object-about
   #:create-object
   #:get-object-tag-value
   #:get-object-about-tag-value
   #:set-object-tag-value
   #:set-object-about-tag-value
   #:object-tag-has-value-p
   #:object-about-tag-has-value-p
   #:delete-object-tag-value
   #:delete-object-about-tag-value

   #:query-objects
   #:query-objects-tag-values
   #:set-objects-tag-values
   #:delete-objects-tag-values

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