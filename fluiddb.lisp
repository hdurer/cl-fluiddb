(in-package #:cl-fluiddb)

(defvar *server-url* "fluiddb.fluidinfo.com"
  "Base URL (without the http:// or https:// protocol bit) for the server to call")

(defvar *using-sandbox* nil)

(defvar *use-https* t
  "Flag whether to use HTTPS or just HTTP")

(defvar *connection* nil
  "A store for any existing connection to the server that can be re-used")

(defvar *credentials* nil
  "User credentidals to be used for a request.
Should be either nil (anonymous access) or a list of two strings (username password)")

(defvar *proxy-server* nil
  "Information about the proxy server to use for requests.
Should be either nil (direct access), a string (the server to use),
or a list of two values (server port)")

(defvar *proxy-credentials* nil
  "Credentials to be used to authenticate against the proxy configured via *proxy-server*")

(defvar *user-agent* "CL-FLUIDDB"
  "The string to be used as your user agent when making request.
This might allow FluidInfo to better monitor what app is using their service")

(defvar *call-timeout* nil
  "Timeout (in seconds) to use for any call to the FluidDB server.
If a timeout happens, a bordeaux-threads:timeout condition will be raised.
A value of nil means not to use any timeout.")

(define-condition call-error (error)
  ((status-code :initarg :status-code :accessor status-code)
   (status-message :initarg :status-message :accessor status-message)
   (error-class :initarg :error-class :accessor error-class)
   (request-id :initarg :request-id :accessor request-id)
   (error-body :initarg :error-body :accessor error-body))
  (:report (lambda (condition stream)
             (format stream "FluidDB server returned error code ~a - ~s of class ~s for request ~s (~s)"
                     (status-code condition)
                     (status-message condition)
                     (error-class condition)
                     (request-id condition)
                     (error-body condition)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun throw-call-error (status-code status-text headers response-body)
  (let ((error-class (or (cdr (assoc "X-FluidDB-Error-Class" headers
                                     :test #'string-equal))
                         "no error class set"))
        (request-id (or (cdr (assoc "X-FluidDB-Request-Id" headers
                                    :test #'string-equal))
                        "no request id set")))
    (error 'call-error
           :status-code status-code
           :status-message status-text
           :error-class error-class
           :request-id request-id
           :error-body response-body)))


(defun do-send-request (path &key body-data query-data (accept "application/json") (method :get) (content-type "application/json"))
  "Lower-level routine to send a request to FluidDB.
Set accept to the content-type you want (\"*/*\" if you don't know).

This does not interpret the results and just returns the HTTP response.
It may still throw errors on network problems, timeouts etc."
  (let ((drakma:*drakma-default-external-format* :utf-8)
        (url (concatenate 'string
                          (if *use-https* "https://" "http://")
                          *server-url* "/"
                          path))
        (body-data (if (and body-data (stringp body-data))
                       ;; convert to UTF-8 as my Drakma version gets length wrong otherwise
                       (flexi-streams:string-to-octets body-data :external-format :utf-8)
                       body-data))
        (additional-headers '(("accept-encoding" . "base64" #+nil"identity;1.0, base64; 0.5"))))
    (labels ((make-call ()
               (drakma:http-request url
                                    :parameters query-data
                                    :method method
                                    :close nil :keep-alive t 
                                    :stream *connection*
                                    :content body-data
                                    :content-type content-type
                                    :accept accept
                                    :additional-headers additional-headers
                                    :user-agent *user-agent*
                                    :basic-authorization *credentials*
                                    :proxy *proxy-server*
                                    :proxy-basic-authorization *proxy-credentials*))
             (do-call ()
               (multiple-value-bind (raw-response code headers url stream should-close status-text)
                   (handler-case
                       ;; try to make the call
                       (make-call)
                     (error (ex) ;; assume a stale file handle and just re-try with a fresh one
                       (when drakma:*header-stream*
                         (format drakma:*header-stream* "~%cl-fluiddb: caught condition ~a -- retrying with fresh stream" ex))
                       (setf *connection* nil)
                       ;; and call again...
                       (make-call)))
                 (if should-close
                     (progn
                       (close stream)
                       (setf *connection* nil))
                     (setf *connection* stream))
                 (values code status-text
                         raw-response
                         headers
                         url))))
      (if *call-timeout*
          (bordeaux-threads:with-timeout (*call-timeout*)
            (do-call))
          (do-call)))))

(defun send-request (path &key body-data query-data (accept "application/json") (method :get) (content-type "application/json"))
  "Send a request to FluidDB.
Set accept to the content-type you want (\"*/*\" if you don't know).

We inspect the return data and convert it to a lisp data structure if it is json"
  (multiple-value-bind (status-code status-text raw-response headers)
      (do-send-request path
                       :body-data body-data
                       :query-data query-data
                       :accept accept
                       :method method
                       :content-type content-type)
    (let* ((content-type (cdr (assoc :content-type headers)))
           (response (if (or (string-equal "application/json" content-type)
                             (string-equal "application/vnd.fluiddb.value+json" content-type))
                         (json:decode-json-from-string
                          (flexi-streams:octets-to-string raw-response :external-format :utf-8))
                       raw-response)))
      (if (<= 200 status-code 299)
          ;; a good answer
          (values response
                  status-code
                  status-text
                  raw-response
                  content-type
                  headers)
        ;; some error in the call
        (throw-call-error status-code status-text headers response)))))


(defun url-encode (string)
  "URL encode a string so it's safe to include in an URL"
  (drakma::url-encode string :utf-8))


(defgeneric url-format-namespace-or-tag (tag)
  (:documentation "Helper function to convert some object into a (url-escaped) string ready to be used in the request URL"))

(defmethod url-format-namespace-or-tag ((tag string))
  tag)

(defmethod url-format-namespace-or-tag ((tag list))
  (format nil "~{~A~^/~}" (mapcar #'url-encode tag)))
  


(defun to-string (something)
  "Do sensible conversion to a string"
  (typecase something
    (symbol (string-downcase (symbol-name something)))
    (string something)
    (t (format nil "~a" something))))

(defun make-permission-object (policy exceptions)
  (json:encode-json-alist-to-string
   `(("policy" . ,(to-string policy))
     ("exceptions" . ,(mapcar 'to-string exceptions)))))


;; the following is a bit of a hack as my copy of the json library
;; cannot encode a false value
(defmethod json:encode-json((obj (eql 'json::true)) stream)
  (json::write-json-chars "true" stream))
(defmethod json:encode-json((obj (eql 'json::false)) stream)
  (json::write-json-chars "false" stream))

(defun to-boolean (value)
  (if value 'json::true 'json::false))

(defstruct json-alist 
  (values))
(defmethod json:encode-json((obj json-alist) stream)
  (json::encode-json-alist (json-alist-values obj) stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpful things for users of this library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-credentials ((username password) &body body)
  `(let ((*credentials* (list ,username ,password)))
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-user (name)
  (send-request (concatenate 'string "users/" name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-object (id &key (show-about t))
  "Retrieve an object by its id"
  (send-request (concatenate 'string "objects/" id)
                :query-data `(("showAbout" . ,(if show-about "True" "False")))))


(defun get-object-about (about)
  "Retrieve an object by its about tag"
  (send-request (concatenate 'string "about/" (url-encode about))))


(defun query-objects (query)
  (send-request "objects"
                :query-data `(("query" . ,query))))

(defun query-objects-tag-values (query tags-list)
  (send-request "values"
                :query-data (cons (cons "query"  query)
                                  (loop for tag in tags-list
                                        collect (cons "tag" tag)))))

(defun set-objects-tag-values (query tags-values-list)
  (send-request "values"
                :method :put
                :query-data `(("query" . ,query))
                :body-data (json:encode-json-alist-to-string
                            (loop for (tag . value) in tags-values-list
                                  collect (cons tag 
                                                (make-json-alist :values `(("value" . ,value))))))))

(defun delete-objects-tag-values (query tags-list)
  (send-request "values"
                :method :delete
                :query-data (cons (cons "query"  query)
                                  (loop for tag in tags-list
                                        collect (cons "tag" tag)))))


(defun create-object (&optional about)
  (send-request "objects"
                :body-data (when about
                             (json:encode-json-plist-to-string
                              (list "about" about)))
                :method :post))


(defun get-object-tag-value (id tag &key want-json accept)
  (send-request (concatenate 'string "objects/" id "/" (url-format-namespace-or-tag tag))
                :accept (if want-json
                            "application/vnd.fluiddb.value+json"
                            (or accept "*/*"))))


(defun get-object-about-tag-value (about tag &key want-json accept)
  (send-request (concatenate 'string 
                             "about/" (url-encode about) 
                             "/" (url-format-namespace-or-tag tag))
                :accept (if want-json
                            "application/vnd.fluiddb.value+json"
                            (or accept "*/*"))))


(defun object-tag-has-value-p (id tag)
  (multiple-value-bind (status-code status-text body headers)
      (do-send-request (concatenate 'string 
                                    "objects/" id
                                    "/" (url-format-namespace-or-tag tag))
                       :method :head)
    (cond ((<= 200 status-code 299)
           ;; the tag values exist, return a true value (content-type if included in response)
           (or (cdr (assoc :content-type headers))
               t))
          ((= 404 status-code)
           ;; No such tag value
           nil)
          (t
           (throw-call-error status-code status-text headers body)))))


(defun object-about-tag-has-value-p (about tag)
  (multiple-value-bind (status-code status-text body headers)
      (do-send-request (concatenate 'string 
                                    "about/" (url-encode about) 
                                    "/" (url-format-namespace-or-tag tag))
                       :method :head)
    (cond ((<= 200 status-code 299)
           ;; the tag values exist, return a true value (content-type if included in response)
           (or (cdr (assoc :content-type headers))
               t))
          ((= 404 status-code)
           ;; No such tag value
           nil)
          (t
           (make-call-error status-code status-text headers body)))))


(defun set-object-tag-value (id tag content &optional content-type)
  (send-request (concatenate 'string "objects/" id "/" (url-format-namespace-or-tag tag))
                :method :put
                :body-data (if content-type
                               ;; assume pre-formatted
                               content
                               ;; encode into json
                               (json:encode-json-to-string content))
                :content-type (or content-type
                                  "application/vnd.fluiddb.value+json")))


(defun set-object-about-tag-value (about tag content &optional content-type)
  (send-request (concatenate 'string "about/" (url-encode about)
                             "/" (url-format-namespace-or-tag tag))
                :method :put
                :body-data (if content-type
                               ;; assume pre-formatted
                               content
                               ;; encode into json
                               (json:encode-json-to-string content))
                :content-type (or content-type
                                  "application/vnd.fluiddb.value+json")))


(defun delete-object-tag-value (id tag)
  (send-request (concatenate 'string "object/" id "/" (url-format-namespace-or-tag tag))
                :method :delete))


(defun delete-object-about-tag-value (about tag)
  (send-request (concatenate 'string "about/" (url-encode about)
                             "/" (url-format-namespace-or-tag tag))
                :method :delete))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-namespace (ns &key (return-description t) (return-namespace t) (return-tags t))
  (send-request (concatenate 'string "namespaces/" (url-format-namespace-or-tag ns))
                :query-data `(("returnDescription" . ,(if return-description "True" "False"))
                              ("returnNamespaces" . ,(if return-namespace "True" "False"))
                              ("returnTags" . ,(if return-tags "True" "False")))))


(defun create-namespace (ns name description)
  (send-request (concatenate 'string "namespaces/" (url-format-namespace-or-tag ns))
                :method :post
                :body-data (json:encode-json-plist-to-string
                            (list "description" description
                                  "name" name))))


(defun change-namespace (ns new-description)
  (send-request (concatenate 'string "namespaces/" (url-format-namespace-or-tag ns))
                :method :put
                :body-data (json:encode-json-plist-to-string
                            (list "description" new-description))))


(defun delete-namespace (ns)
  (send-request (concatenate 'string "namespaces/" (url-format-namespace-or-tag ns))
                :method :delete))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Permissions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-namespace-permissions (namespace action)
  (send-request (concatenate 'string "permissions/namespaces/" (url-format-namespace-or-tag namespace))
                :query-data `(("action" . ,(to-string action)))))


(defun set-namespace-permissions (namespace action policy exceptions)
  (send-request (concatenate 'string "permissions/namespaces/" (url-format-namespace-or-tag namespace))
                :method :put
                :query-data `(("action" . ,(to-string action)))
                :body-data (make-permission-object policy exceptions)))


(defun get-tag-permissions (tag action)
  (send-request (concatenate 'string "permissions/tags/" (url-format-namespace-or-tag tag))
                :query-data `(("action" . ,(to-string action)))))


(defun set-tag-permissions (tag action policy exceptions)
  (send-request (concatenate 'string "permissions/tags/" (url-format-namespace-or-tag tag))
                :method :put
                :query-data `(("action" . ,(to-string action)))
                :body-data (make-permission-object policy exceptions)))

(defun get-tag-value-permissions (tag action)
  (send-request (concatenate 'string "permissions/tag-values/" (url-format-namespace-or-tag tag))
                :query-data `(("action" . ,(to-string action)))))


(defun set-tag-value-permissions (tag action policy exceptions)
  (send-request (concatenate 'string "permissions/tag-values/" (url-format-namespace-or-tag tag))
                :method :put
                :query-data `(("action" . ,(to-string action)))
                :body-data (make-permission-object policy exceptions)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Policies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-policy (user-name category action)
  (send-request (concatenate 'string
                             "policies/"
                             user-name "/"
                             (to-string category) "/"
                             (to-string action))))


(defun set-policy (user-name category action policy exceptions)
  (send-request (concatenate 'string
                             "policies/"
                             user-name "/"
                             (to-string category) "/"
                             (to-string action))
                :method :put
                :body-data (make-permission-object policy exceptions)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun create-tag (namespace tag description indexed)
  (send-request (concatenate 'string
                             "tags/" (url-format-namespace-or-tag namespace))
                :method :post
                :body-data (json:encode-json-alist-to-string
                            `(("name" . ,tag)
                              ("description" . ,description)
                              ("indexed" . ,(to-boolean indexed))))))


(defun get-tag (namespace tag &key (return-description t))
  (send-request (concatenate 'string
                             "tags/" (url-format-namespace-or-tag namespace) "/"
                             tag)
                :query-data `(("returnDescription" . ,(if return-description "True" "False")))))


(defun change-tag (namespace tag description)
  (send-request (concatenate 'string
                             "tags/" (url-format-namespace-or-tag namespace) "/"
                             tag)
                :method :put
                :body-data `(("description" . ,description))))


(defun delete-tag (namespace tag)
  (send-request (concatenate 'string
                             "tags/" (url-format-namespace-or-tag namespace) "/"
                             tag)
                :method :delete))

