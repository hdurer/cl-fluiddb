(in-package #:cl-fluiddb)


(defun send-request (url  &key body-data query-data (want-json t) (method :get) (content-type "application/json"))
  "Send a request to FluidDB.
Set want-json to nil if you do not want only application/json back (e.g. to get payload of a tag).

We inspect the return data and convert it to a lisp data structure if it is json"
  (let ((drakma:*drakma-default-external-format* :utf-8))
    (multiple-value-bind (raw-response code headers url stream close status-text)
        (drakma:http-request (concatenate 'string "http://fluiddb.fluidinfo.com/" url)
                             :parameters query-data
                             :method method
                             :content body-data
                             :content-type content-type
                             :additional-headers (if want-json
                                                     '((:accept . "application/json")))
                             :user-agent "CL-FLUIDDB")
      (declare (ignore url stream close))
      (let* ((content-type (cdr (assoc :content-type headers)))
             (response (if (string-equal "application/json" content-type)
                           (json:decode-json-from-string
                            (sb-ext:octets-to-string (coerce raw-response '(vector (unsigned-byte 8)))))
                           raw-response)))
        (values response
                code
                status-text
                raw-response
                content-type
                headers)))))

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
                :body-data (json:encode-json-plist-to-string (list "showAbout" show-about))))


(defun query-objects (query)
  (send-request "objects"
                :query-data `(("query" . ,query))))

(defun create-object (about)
  (send-request "objects"
                :body-data (json:encode-json-plist-to-string (list "about" about))
                :method :post))

(defun get-object-tag (id tag)
  (send-request (concatenate 'string "objects/" id "/" tag)
                :want-json nil))

(defun change-object-tag (id tag content content-type)
  (send-request (concatenate 'string "objects/" id "/" tag)
                :method :put
                :body-data content
                :content-type content-type))


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

