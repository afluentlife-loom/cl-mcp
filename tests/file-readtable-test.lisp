;;;; tests/file-readtable-test.lisp
;;;;
;;;; The structure editors read with the FILE's readtable: the last
;;;; (in-readtable NAME) before the target form, as its in-package is found.
;;;; A file declaring a readtable the image lacks is REPORTED, not refused,
;;;; unless the text an edit must read needs that readtable; then the
;;;; refusal names the readtable, not the reader macro.
;;;;
;;;; The readtable here is the test's own: #R"text" reads (:REF "text"),
;;;; the shape of a definition-reference reader. Read with the standard
;;;; readtable, #R is the radix reader and "radix missing" is the error.

(defpackage #:cl-mcp/tests/file-readtable-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:testing
                #:ok
                #:ng
                #:skip)
  (:import-from #:cl-mcp/src/lisp-edit-form
                #:lisp-edit-form)
  (:import-from #:cl-mcp/src/lisp-patch-form
                #:lisp-patch-form)
  (:import-from #:cl-mcp/src/cst
                #:readtable-not-in-image
                #:readtable-not-in-image-designator)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-write-file)
  (:import-from #:asdf
                #:system-source-directory))

(in-package #:cl-mcp/tests/file-readtable-test)

(setf cl-mcp/src/project-root:*project-root*
      (uiop:ensure-directory-pathname (system-source-directory :cl-mcp)))

(defparameter *ref* "#R\"tools/t.lisp#function(p::f)\""
  "A reference as the file writes it.")

(defun %readtable-api (name)
  (let ((pkg (or (find-package :named-readtables)
                 (find-package :editor-hints.named-readtables))))
    (and pkg (find-symbol name pkg))))

(defun %read-ref (stream sub-char arg)
  (declare (ignore sub-char arg))
  (list :ref (read stream t nil t)))

(defun ensure-test-readtable ()
  "Define :CL-MCP-TEST-REFS (standard + #R) and return T, or NIL when
named-readtables cannot be loaded."
  (handler-case
      (progn
        (unless (%readtable-api "FIND-READTABLE")
          (asdf:load-system :named-readtables))
        (let ((find (%readtable-api "FIND-READTABLE"))
              (make (%readtable-api "MAKE-READTABLE")))
          (unless (funcall find :cl-mcp-test-refs)
            (let ((rt (funcall make :cl-mcp-test-refs :merge '(:standard))))
              (set-dispatch-macro-character #\# #\R #'%read-ref rt)))
          t))
    (error () nil)))

(defun file-text (designator &rest forms)
  (format nil "(in-package #:cl-user)~%(named-readtables:in-readtable ~S)~%~%~{~A~%~%~}"
          designator forms))

(defun with-fixture (name text thunk)
  (let ((abs (uiop:native-namestring
              (uiop:merge-pathnames* name (system-source-directory :cl-mcp)))))
    (ensure-directories-exist abs)
    (fs-write-file name text)
    (unwind-protect (funcall thunk abs)
      (ignore-errors (delete-file abs)))))

(defmacro refusal-of (form)
  "The READTABLE-NOT-IN-IMAGE FORM signals, or NIL."
  `(handler-case (progn ,form nil)
     (readtable-not-in-image (c) c)))

;;; -- the readtable is in the image ----------------------------------

(deftest edit-form-reads-content-with-the-files-readtable
  (if (not (ensure-test-readtable))
      (skip "named-readtables not loadable")
      (let ((text (file-text :cl-mcp-test-refs
                             "(defparameter *anchor* 1)"
                             (format nil "(defparameter *existing* ~A)" *ref*))))
        (testing "insert_after: content with #R is read with the file's readtable"
          (with-fixture "tests/tmp/rt-insert.lisp" text
            (lambda (path)
              (lisp-edit-form :file-path path :form-type "defparameter"
                              :form-name "*anchor*" :operation "insert_after"
                              :content (format nil "(defparameter *new* ~A)" *ref*))
              (ok (search (format nil "(defparameter *new* ~A)" *ref*)
                          (fs-read-file path))))))
        (testing "replace: content with #R is read with the file's readtable"
          (with-fixture "tests/tmp/rt-replace.lisp" text
            (lambda (path)
              (lisp-edit-form :file-path path :form-type "defparameter"
                              :form-name "*anchor*" :operation "replace"
                              :content (format nil "(defparameter *anchor* ~A)" *ref*))
              (ok (search (format nil "(defparameter *anchor* ~A)" *ref*)
                          (fs-read-file path))))))
        (testing "the edit is not reported as missing a readtable"
          (with-fixture "tests/tmp/rt-present-value.lisp" text
            (lambda (path)
              (ok (null (nth-value 4 (lisp-edit-form
                                      :file-path path :form-type "defparameter"
                                      :form-name "*anchor*" :operation "replace"
                                      :content "(defparameter *anchor* 2)"))))))))))

(deftest patch-form-reads-the-patched-form-with-the-files-readtable
  (if (not (ensure-test-readtable))
      (skip "named-readtables not loadable")
      (testing "patch inside a form holding #R"
        (with-fixture "tests/tmp/rt-patch.lisp"
            (file-text :cl-mcp-test-refs
                       (format nil "(defparameter *existing* ~A)" *ref*))
          (lambda (path)
            (multiple-value-bind (updated changed-p unresolved missing)
                (lisp-patch-form :file-path path :form-type "defparameter"
                                 :form-name "*existing*"
                                 :old-text "p::f" :new-text "p::g")
              (declare (ignore updated unresolved))
              (ok changed-p)
              (ok (null missing))
              (ok (search "#R\"tools/t.lisp#function(p::g)\"" (fs-read-file path)))))))))

;;; -- the readtable is NOT in the image ------------------------------

(deftest missing-readtable-is-named-when-the-edit-needs-it
  (testing "a file whose #R the image cannot read: refused, naming the readtable"
    (with-fixture "tests/tmp/rt-absent-file.lisp"
        (file-text :cl-mcp-test-refs-absent
                   "(defparameter *anchor* 1)"
                   (format nil "(defparameter *existing* ~A)" *ref*))
      (lambda (path)
        (let* ((before (fs-read-file path))
               (c (refusal-of
                   (lisp-patch-form :file-path path :form-type "defparameter"
                                    :form-name "*existing*"
                                    :old-text "p::f" :new-text "p::g"))))
          (ok c "READTABLE-NOT-IN-IMAGE signalled")
          (ok (and c (eq :cl-mcp-test-refs-absent
                         (readtable-not-in-image-designator c))))
          (ok (and c (search "CL-MCP-TEST-REFS-ABSENT" (princ-to-string c)))
              "the report names the readtable")
          (ok (string= before (fs-read-file path)) "nothing written")))))
  (testing "content with #R under a missing readtable: refused, naming it"
    (with-fixture "tests/tmp/rt-absent-content.lisp"
        (file-text :cl-mcp-test-refs-absent "(defparameter *anchor* 1)")
      (lambda (path)
        (let* ((before (fs-read-file path))
               (c (refusal-of
                   (lisp-edit-form :file-path path :form-type "defparameter"
                                   :form-name "*anchor*" :operation "insert_after"
                                   :content (format nil "(defparameter *new* ~A)" *ref*)))))
          (ok c "READTABLE-NOT-IN-IMAGE signalled")
          (ok (string= before (fs-read-file path)) "nothing written"))))))

(deftest missing-readtable-is-reported-when-the-edit-does-not-need-it
  (testing "a form without #R, in a file with #R elsewhere: applied, reported"
    (with-fixture "tests/tmp/rt-absent-other.lisp"
        (file-text :cl-mcp-test-refs-absent
                   "(defparameter *anchor* 1)"
                   (format nil "(defparameter *existing* ~A)" *ref*)
                   "(defparameter *after* 3)")
      (lambda (path)
        (multiple-value-bind (updated changed-p unresolved missing)
            (lisp-patch-form :file-path path :form-type "defparameter"
                             :form-name "*after*" :old-text "3" :new-text "4")
          (declare (ignore updated unresolved))
          (ok changed-p)
          (ok (eq :cl-mcp-test-refs-absent missing) "the missing readtable is reported")
          (let ((text (fs-read-file path)))
            (ok (search "(defparameter *after* 4)" text))
            (ok (search *ref* text) "the #R form is untouched")))
        (multiple-value-bind (updated warning changed-p unresolved missing)
            (lisp-edit-form :file-path path :form-type "defparameter"
                            :form-name "*anchor*" :operation "replace"
                            :content "(defparameter *anchor* 2)")
          (declare (ignore updated warning unresolved))
          (ok changed-p)
          (ok (eq :cl-mcp-test-refs-absent missing)))))))
