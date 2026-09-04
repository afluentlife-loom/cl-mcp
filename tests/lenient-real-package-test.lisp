;;;; tests/lenient-real-package-test.lisp
;;;;
;;;; *LENIENT-REAL-PACKAGES*: a symbol the image does not know in a package
;;;; that EXISTS is accepted and reported, never interned, and the structure
;;;; editors carry the report instead of refusing the file. Measured
;;;; 2026-09-04: an editing session fell back to text tools for a whole arc
;;;; because the editor read a freshly exported symbol as a typo — the image
;;;; was stale, the file was not wrong.

(defpackage #:cl-mcp/tests/lenient-real-package-test
  (:use #:cl)
  (:import-from #:rove #:deftest #:testing #:ok #:ng #:signals)
  (:import-from #:cl-mcp/src/utils/lenient-read
                #:call-with-lenient-packages
                #:call-with-lenient-real-packages)
  (:import-from #:cl-mcp/src/lisp-patch-form #:lisp-patch-form)
  (:import-from #:cl-mcp/src/lisp-edit-form #:lisp-edit-form)
  (:import-from #:cl-mcp/src/fs #:fs-write-file))

(in-package #:cl-mcp/tests/lenient-real-package-test)

(setf cl-mcp/src/project-root:*project-root*
      (uiop:ensure-directory-pathname (asdf:system-source-directory :cl-mcp)))

;;; A REAL package that exists in the image and knows none of the symbols the
;;; fixtures name — what a package looks like from a stale image.
(defpackage #:lenient-real-fixture (:use #:cl))

(defun fixture () (find-package "LENIENT-REAL-FIXTURE"))

(deftest strict-by-default
  (testing "a missing symbol in a real package is still an error at the REPL"
    (ok (signals (call-with-lenient-packages
                  (lambda ()
                    (eclector.reader:read-from-string "(lenient-real-fixture:nope)")))
                 'error))))

(deftest eclector-accepts-and-reports
  (testing "the Eclector path accepts, reports once, and interns nothing"
    (multiple-value-bind (form unresolved)
        (call-with-lenient-real-packages
         (lambda ()
           (eclector.reader:read-from-string
            "(defun f () (lenient-real-fixture:nope lenient-real-fixture:nope))")))
      (ok (eq (first form) 'defun))
      (ok (equal unresolved '("LENIENT-REAL-FIXTURE:NOPE")) "reported once, deduplicated")
      (ng (find-symbol "NOPE" (fixture)) "the real package was not written to"))))

(deftest sbcl-reader-accepts-and-cleans-up
  (testing "the CL reader path interns to continue and uninterns on exit"
    (multiple-value-bind (form unresolved)
        (call-with-lenient-real-packages
         (lambda ()
           (read-from-string "(defun g () (lenient-real-fixture:nope2))")))
      (ok (eq (first form) 'defun))
      (ok (equal unresolved '("LENIENT-REAL-FIXTURE:NOPE2")))
      (ng (find-symbol "NOPE2" (fixture)) "uninterned after the extent"))))

(deftest stub-packages-unchanged
  (testing "an unknown package still gets a stub and is not reported as unresolved"
    (multiple-value-bind (form unresolved)
        (call-with-lenient-real-packages
         (lambda ()
           (eclector.reader:read-from-string "(lenient-real-nowhere:thing)")))
      (ok (string= (symbol-name (first form)) "THING"))
      (ok (null unresolved))
      (ng (find-package "LENIENT-REAL-NOWHERE") "stub cleaned up"))))

(defun call-with-fixture-file (thunk)
  "A file in the real fixture package naming a symbol the image has not loaded."
  (let* ((rel "tests/tmp/lenient-real-fixture.lisp")
         (abs (merge-pathnames rel cl-mcp/src/project-root:*project-root*)))
    (ensure-directories-exist abs)
    (fs-write-file rel (format nil ";;;; fixture~%(in-package #:lenient-real-fixture)~%~%(defun target (x)~%  (lenient-real-fixture:not-loaded-yet x))~%"))
    (unwind-protect (funcall thunk rel)
      (ignore-errors (delete-file abs)))))

(deftest patch-form-locates-despite-stale-symbol
  (testing "lisp-patch-form's dry run succeeds and names the unresolved symbol"
    (call-with-fixture-file
     (lambda (rel)
       (let ((r (lisp-patch-form :file-path rel :form-type "defun" :form-name "target"
                                 :old-text "(lenient-real-fixture:not-loaded-yet x)"
                                 :new-text "(lenient-real-fixture:not-loaded-yet x x)"
                                 :dry-run t)))
         (ok (eq t (gethash "would_change" r)))
         (ok (search "x x)" (gethash "preview" r)))
         (ok (equal (gethash "unresolved_symbols" r)
                    '("LENIENT-REAL-FIXTURE:NOT-LOADED-YET"))))
       (ng (find-symbol "NOT-LOADED-YET" (fixture)) "nothing interned by the edit")))))

(deftest edit-form-locates-despite-stale-symbol
  (testing "lisp-edit-form's dry run succeeds and reports the file's and the content's symbols"
    (call-with-fixture-file
     (lambda (rel)
       (let ((r (lisp-edit-form :file-path rel :form-type "defun" :form-name "target"
                                :operation "replace" :dry-run t
                                :content (format nil "(defun target (x)~%  (lenient-real-fixture:also-not-loaded x))"))))
         (ok (eq t (gethash "would_change" r)))
         (ok (equal (gethash "unresolved_symbols" r)
                    '("LENIENT-REAL-FIXTURE:NOT-LOADED-YET"
                      "LENIENT-REAL-FIXTURE:ALSO-NOT-LOADED"))))))))
