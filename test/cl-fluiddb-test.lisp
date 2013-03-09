(in-package #:cl-fluiddb-test)

(defun combine-namespace-and-tag (ns tag)
  (format nil "~a/~a" ns tag))


(defun clean-up-namespace (ns)
  "Do best effort of removing a namespace by recursively removing all tags and sub-namespaces before atempting to remove the namespace itself.
This function should never fail and never hang, but it may fail to complete its task if things go wrong."
  (let* ((*call-timeout* 5)
         (res (ignore-errors
                (get-namespace ns
                               :return-description nil)))
         (failures (unless res
                    (list (format nil "get ns ~a" ns)))))
    (when res
      (loop
         for tag in (cdr (assoc :tag-names res))
         unless (ignore-errors (delete-tag (combine-namespace-and-tag ns tag))
                               'ok)
         do (push (format nil "delete tag ~a/~a" ns tag)
                  failures))
      (loop
         for subns in (cdr (assoc :namespace-names res))
         for result = (clean-up-namespace (concatenate 'string ns "/" subns))
         unless result
         do (push result failures))
      (unless (ignore-errors (delete-namespace ns)
                             'ok)
        (push (format nil "delete ns ~a" ns) failures)))
    failures))

(defvar *fluiddb-id* nil "Id some some sample object 'FluidDB' we create for our tests")
(defvar *test-id* nil "Id some some sample object 'Test' we create for our tests")

(deftestsuite simple-access ()
  ()
  (:documentation
   "Simple test to check we can read anonymously from the DB")
  (:timeout 300)
  (:dynamic-variables
   (*connection* nil)
   (*credentials* nil)
   (*call-timeout* 10))
  (:run-setup :once-per-suite)
  (:setup (with-credentials ("test" "test")
	    (setf *fluiddb-id* (cdr (assoc :id
					   (ignore-errors (create-object "FluidDB"))))
		  *test-id* (cdr (assoc :id
					(ignore-errors (create-object "Test")))))
	    (ignore-errors (create-tag "test" "testtag" "A tag we can add to objects for testing" nil))
	    (loop for cnt from 1 to 10
	       do (set-object-about-tag-value (format nil "Test object #~a" cnt)
					      "test/testtag" cnt))))
  (:teardown (setf *fluiddb-id* nil
		   *test-id* nil))
  (:tests
   (check-setup
    (ensure-null (not *test-id*))
    (ensure-null (not *fluiddb-id*)))
   (check-get-user
    (ensure-null
     (not (get-user "test")))
    (ensure-error
      (get-user ""))
    (ensure-error
      (get-user "some-really-long-name-that-should-not-really-exist")))
   (check-ids-are-consistent
    (ensure-same (cdr (assoc :id (create-object "FluidDB")))
                 *fluiddb-id*
                 :test #'string-equal)
    (when *test-id*
      (ensure-same (cdr (assoc :id (create-object "Test")))
                   *test-id*
                   :test #'string-equal)))
   (check-test-id-and-fluiddb-id-are-different
    (ensure (lambda () (not (equalp *fluiddb-id* *test-id*)))))
   (check-get-object
    (let (returned-object-1
          returned-object-2)
      (ensure-no-warning
        (setf returned-object-1
              (get-object *test-id* :show-about nil)
              returned-object-2
              (get-object *test-id* :show-about t)))
      (ensure (lambda ()
                (equalp (assoc :id returned-object-1)
                        (assoc :id returned-object-2))))
      (ensure-null (assoc :about returned-object-1))
      (ensure-null (not (assoc :about returned-object-2)))))
   (check-query-objects1
    (let ((*call-timeout* 30)) ;; increase timeout so we don't fail on lots of results
      (let ((ids (assoc :ids (query-objects "has test/testtag"))))
        (ensure (lambda () (> (length (cdr ids)) 5))))))
   (check-query-objects2
    (let* ((*call-timeout* 100)
           (ids (assoc :ids (query-objects "fluiddb/about contains \"something\""))))
      (ensure-null (not ids))
      (ensure-same 0 (length (cdr ids)))))
   (check-timeout-works
    (ensure-condition #+sbcl sb-ext:timeout #-sbcl bordeaux-threads:timeout
         (let ((*call-timeout* 1)
               (*connection* nil))
          ;; with a short timeout do a long-running query
           (query-objects "has fluiddb/about and has fluiddb/about  and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about and has fluiddb/about"))))))


(defvar *temp-ns* "The temporary namespace id we'll create")
(defvar *temp-ns-oid* "The oid of the temporary namespace id we'll create")

(deftestsuite authenticated-operations ()
  ()
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
   (let ((prefix (format nil "~a-~a"
			 (symbol-name (gensym))
			 (get-universal-time))))
     (setf *temp-ns* (combine-namespace-and-tag "test/cl-fluiddb" prefix)
	   *temp-ns-oid* (assoc :id
				(ignore-errors
				  (create-namespace 
				   "test/cl-fluiddb" 
				   prefix
				   "Test namespace for unit tests"))))))
  (:teardown (clean-up-namespace *temp-ns*))
  (:tests
   (have-a-temp-ns
    (ensure-null (not *temp-ns-oid*)))
   (can-create-tag
    (when *temp-ns-oid*
      (ensure-no-warning
        (create-tag *temp-ns* "tag1" "" nil))
      (ensure-no-warning
        (create-tag *temp-ns* "tag2" "some other tag" t))))
   (can-create/delete-ns
    (let ((have-ns1 nil)
          (ns1-fullname (combine-namespace-and-tag *temp-ns* "ns1")))
      (when *temp-ns*
        (create-namespace *temp-ns* "ns1" "some namespace")
        (setf have-ns1 t)
        (when have-ns1
          (ensure-no-warning
            (change-namespace ns1-fullname "new description"))
          (ensure-no-warning
            (delete-namespace ns1-fullname))))))))