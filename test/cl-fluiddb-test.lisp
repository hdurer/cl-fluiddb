(in-package #:cl-fluiddb-test)

(defun clean-up-namespace (ns)
  "Do best effort of removing a namespace by recursively removing all tags and sub-namespaces before atempting to remove the namespace itself.
This function should never fail and nevr hang, but it may fail to complete its task if things go wrong."
  (let* ((*call-timeout* 5)
         (res (ignore-errors
                (get-namespace ns
                               :return-description nil)))
         (failures (unless res
                    (list (format nil "get ns ~a" ns)))))
    (when res
      (loop
         for tag in (cdr (assoc :tag-names res))
         unless (ignore-errors (delete-tag ns tag)
                               'ok)
         do (push (format nil "tag ~a/~a" ns tag)
                  failures))
      (loop
         for subns in (cdr (assoc :namespace-names res))
         for result = (clean-up-namespace (concatenate 'string ns "/" subns))
         unless result
         do (push result failures))
      (unless (ignore-errors (delete-namespace ns)
                             'ok)
        (push (format nil "ns ~a" ns) failures)))
    failures))


(deftestsuite simple-access ()
  (fluiddb-id
   test-id)
  (:documentation
   "Simple test to check we can read anonymously from the DB")
  (:timeout 120)
  (:dynamic-variables
   (*connection* nil)
   (*credentials* nil)
   (*call-timeout* 10))
  (:setup (setf fluiddb-id (cdr (assoc :id
                                       (ignore-errors (create-object "FluidDB"))))
                test-id (cdr (assoc :id
                                    (ignore-errors (create-object "Test"))))))
  (:tests
   (check-setup
    (ensure-null (not test-id))
    (ensure-null (not fluiddb-id)))
   (check-get-user
    (ensure-null
     (not (get-user "test")))
    (ensure-error
      (get-user ""))
    (ensure-error
      (get-user "some-really-long-name-that-should-not-really-exist")))
   (check-ids-are-consistent
    (ensure-same (cdr (assoc :id (create-object "FluidDB")))
                 fluiddb-id
                 :test #'string-equal)
    (when test-id
      (ensure-same (cdr (assoc :id (create-object "Test")))
                   test-id
                   :test #'string-equal)))
   (check-test-id-and-fluiddb-id-are-different
    (ensure (lambda () (not (equalp fluiddb-id test-id)))))
   (check-timeout-works
    (ensure-condition #+sbcl sb-ext:timeout #-sbcl bordeaux-threads:timeout
      (let ((*call-timeout* 1)
            (*connection* nil))
        ;; with a short timeout do a long-running query
        (query-objects "has fluiddb/about and has fluiddb/about  and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about"))))
   (check-get-object
    (let (returned-object-1
          returned-object-2)
      (ensure-no-warning
        (setf returned-object-1
              (get-object test-id :show-about nil)
              returned-object-2
              (get-object test-id :show-about t)))
      (ensure (lambda ()
                (equalp (assoc :id returned-object-1)
                        (assoc :id returned-object-2))))
      (ensure-null (assoc :about returned-object-1))
      (ensure-null (not (assoc :about returned-object-2)))))
   (check-query-objects
    (let ((*call-timeout* 100)) ;; increase timeout so we don't fail on lots of results
      (let ((ids (assoc :ids (query-objects "has fluiddb/about"))))
        (ensure (lambda () (> (length (cdr ids)) 5)))))
    (let ((ids (assoc :ids (query-objects "fluiddb/about contains \"something\""))))
      (ensure-null (not ids))
      (ensure-same 0 (length (cdr ids)))))))


(deftestsuite authenticated-operations ()
  ((prefix (format nil "~a-~a"
                   (symbol-name (gensym))
                   (get-universal-time)))
   (temp-ns  (concatenate 'string "test/cl-fluiddb/" prefix))
   temp-ns-oid)
  (:documentation
   "Simple tests requiring authentication (using the tet user)")
  (:timeout 300)
  (:dynamic-variables
   (*connection* nil)
   (*credentials* (list "test" "test"))
   (*call-timeout* 10))
  (:run-setup :once-per-suite)
  (:setup
   (ignore-errors
     (create-namespace "test" "cl-fluiddb" "Namespace for cl-fluiddb tests"))
   (setf temp-ns-oid
         (assoc :id
                (ignore-errors
                  (create-namespace "test/cl-fluiddb" prefix "Test namespace for unit tests")))))
  (:teardown (clean-up-namespace temp-ns))
  (:tests
   (have-a-temp-ns
    (ensure-null (not temp-ns-oid)))
   (can-create-tag
    (when temp-ns
      (ensure-no-warning
        (create-tag temp-ns "tag1" "" nil))
      (ensure-no-warning
        (create-tag temp-ns "tag2" "some other tag" t))))
   (can-create/delete-ns
    (let ((have-ns1 nil)
          (ns1-fullname (concatenate 'string temp-ns  "/ns1")))
      (when temp-ns
        (create-namespace temp-ns "ns1" "some namespace")
        (setf have-ns1 t)
        (when have-ns1
          (ensure-no-warning
            (change-namespace ns1-fullname "new description"))
          (ensure-no-warning
            (delete-namespace ns1-fullname))))))))