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


(define-condition call-error (error)
  ((status-code :initarg :status-code :accessor status-code)
   (status-message :initarg :status-message :accessor status-message)
   (error-body :initarg :error-body :accessor error-body))
  (:report (lambda (condition stream)
             (format stream "FluidDB server returned error code ~a - ~s (~s)"
                     (status-code condition)
                     (status-message condition)
                     (error-body condition)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun send-request (url &key body-data query-data (accept "application/json") (method :get) (content-type "application/json"))
  "Send a request to FluidDB.
Set accept to the content-type you want (\"*/*\" if you don't know).

We inspect the return data and convert it to a lisp data structure if it is json"
  (let ((drakma:*drakma-default-external-format* :utf-8)
        (url (concatenate 'string
                          (if *use-https* "https://" "http://")
                          *server-url* "/"
                          url))
        (body-data (if (and body-data (stringp body-data))
                       ;; convert to UTF-8 as my Drakma version get lenght wrong otherwise
                       (flexi-streams:string-to-octets body-data :external-format :utf-8)
                       body-data))
        (accept accept)
        (additional-headers '(("accept-encoding" . "base64" #+nil"identity;1.0, base64; 0.5"))))
    (multiple-value-bind (raw-response code headers url stream should-close status-text)
        (handler-case
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
                                 :proxy-basic-authorization *proxy-credentials*)
          (error (ex) ;; assume a stale file handle and just re-try with a fresh one
            (when drakma:*header-stream*
              (format drakma:*header-stream* "~%cl-fluiddb: caught condition ~a -- retrying with fresh stream" ex))
            (setf *connection* nil)
            (drakma:http-request url
                                 :parameters query-data
                                 :method method
                                 :close nil :keep-alive t 
                                 :stream nil
                                 :content body-data
                                 :content-type content-type
                                 :accept accept
                                 :additional-headers additional-headers
                                 :user-agent *user-agent*
                                 :basic-authorization *credentials*
                                 :proxy *proxy-server*
                                 :proxy-basic-authorization *proxy-credentials*)))
      (declare (ignore url))
      (if should-close
          (progn
            (close stream)
            (setf *connection* nil))
          (setf *connection* stream))
      (let* ((content-type (cdr (assoc :content-type headers)))
             (response (if (string-equal "application/json" content-type)
                           (json:decode-json-from-string
                            (flexi-streams:octets-to-string raw-response :external-format :utf-8))
                           raw-response)))
        (if (<= 200 code 299)
            ;; a good answer
            (values response
                    code
                    status-text
                    raw-response
                    content-type
                    headers)
            ;; some error in the call
            (error 'call-error
                    :status-code code
                    :status-message status-text
                    :error-body response))))))

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
  (send-request (concatenate 'string "objects/" id)
                :query-data `(("showAbout" . ,(if show-about "True" "False")))))


(defun query-objects (query)
  (send-request "objects"
                :query-data `(("query" . ,query))))

(defun create-object (&optional about)
  (send-request "objects"
                :body-data (json:encode-json-plist-to-string
                            (when about (list "about" about)))
                :method :post))

(defun get-object-tag-value (id tag &key want-json accept)
  (send-request (concatenate 'string "objects/" id "/" tag)
                :query-data (when want-json '(("format" . "json")))
                :accept (if want-json "application/json" (or accept "*/*"))))

(defun set-object-tag-value (id tag content &optional content-type)
  (send-request (concatenate 'string "objects/" id "/" tag)
                :method :put
                :query-data (unless content-type
                              '(("format" . "json")))
                :body-data (if content-type
                               ;; assume pre-formatted
                               content
                               ;; encode into json
                               (json:encode-json-alist-to-string content))
                :content-type (or content-type "application/json")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-namespace (ns &key (return-description t) (return-namespace t) (return-tags t))
  (send-request (concatenate 'string "namespaces/" ns)
                :query-data `(("returnDescription" . ,(if return-description "True" "False"))
                              ("returnNamespaces" . ,(if return-namespace "True" "False"))
                              ("returnTags" . ,(if return-tags "True" "False")))))
(defun create-namespace (ns name description)
  (send-request (concatenate 'string "namespaces/" ns)
                :method :post
                :body-data (json:encode-json-plist-to-string
                            (list "description" description
                                  "name" name))))
(defun change-namespace (ns new-description)
  (send-request (concatenate 'string "namespaces/" ns)
                :method :put
                :body-data (json:encode-json-plist-to-string
                            (list "description" new-description))))

(defun delete-namespace (ns)
  (send-request (concatenate 'string "namespaces/" ns)
                :method :delete))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Permissions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-namespace-permissions (namespace action)
  (send-request (concatenate 'string "permissions/namespaces/" namespace)
                :query-data `(("action" . ,action))))

(defun set-namespace-permissions (namespace action policy exceptions)
  (send-request (concatenate 'string "permissions/namespaces/" namespace)
                :method :put
                :query-data `(("action" . ,action))
                :body-data (make-permission-object policy exceptions)))

(defun get-tag-permissions (tag action)
  (send-request (concatenate 'string "permissions/tags/" tag)
                :query-data `(("action" . ,action))))

(defun set-tag-permissions (tag action policy exceptions)
  (send-request (concatenate 'string "permissions/tags/" tag)
                :method :put
                :query-data `(("action" . ,action))
                :body-data (make-permission-object policy exceptions)))

(defun get-tag-value-permissions (tag action)
  (send-request (concatenate 'string "permissions/tag-values/" tag)
                :query-data `(("action" . ,action))))

(defun set-tag-value-permissions (tag action policy exceptions)
  (send-request (concatenate 'string "permissions/tag-values/" tag)
                :method :put
                :query-data `(("action" . ,action))
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
                :body-data (make-permission-object policy exceptions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-tag (namespace tag description indexed)
  (send-request (concatenate 'string
                             "tags/" namespace)
                :method :post
                :body-data (json:encode-json-alist-to-string
                            `(("name" . ,tag)
                              ("description" . ,description)
                              ("indexed" . ,(to-boolean indexed))))))


(defun get-tag (namespace tag &key (return-description t))
  (send-request (concatenate 'string
                             "tags/" namespace "/"
                             tag)
                :query-data `(("returnDescription" . ,(if return-description t nil)))))
(defun change-tag (namespace tag description)
  (send-request (concatenate 'string
                             "tags/" namespace "/"
                             tag)
                :method :put
                :body-data `(("description" . ,description))))

(defun delete-tag (namespace tag)
  (send-request (concatenate 'string
                             "tags/" namespace "/"
                             tag)
                :method :delete))