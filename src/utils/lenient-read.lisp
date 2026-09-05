;;;; src/utils/lenient-read.lisp
;;;;
;;;; Lenient package resolution for reader operations.
;;;; Creates ephemeral stub packages on demand when the reader encounters
;;;; unknown package-qualified symbols, and cleans them up afterward.

(defpackage #:cl-mcp/src/utils/lenient-read
  (:use #:cl)
  (:import-from #:eclector.reader
                #:package-does-not-exist
                #:symbol-does-not-exist
                #:symbol-is-not-external)
  (:export #:call-with-lenient-packages
           #:call-with-managed-packages
           #:call-with-lenient-real-packages
           #:*lenient-real-packages*
           #:*unresolved-symbols*
           #:*homeless-due-to-teardown*
           #:record-homeless-on-teardown
           ;; `#.' read as data: the marker and the readtable that does it.
           #:read-time-eval
           #:inert-read-eval-readtable))

(in-package #:cl-mcp/src/utils/lenient-read)

;;; `#.' is READ, never evaluated.
;;;
;;; Source reaches cl-mcp to be located and edited, not run, so a `#.'
;;; form is kept as data — (READ-TIME-EVAL <expression>) once read,
;;; verbatim in the text. The earlier posture refused the whole file,
;;; which protected the same property (nothing evaluates) at the cost
;;; of making every file with a `#.(char-code ...)' in it unreachable
;;; by the structure editors — measured 2026-09-05 on a 2,700-line
;;; parser whose one `#.' sat inside a name helper, so a patch to an
;;; unrelated function in it was refused. Every CL-reader site (the
;;; package-context header scan, the CST's custom-readtable path, the
;;; two editors' validators) uses this readtable; the eclector path
;;; has CST-CLIENT in cst.lisp doing the same through
;;; EVALUATE-EXPRESSION.
(defun %inert-sharp-dot (stream subchar parameter)
  "The `#.' reader macro for INERT-READ-EVAL-READTABLE: read the
expression, return (READ-TIME-EVAL expression), evaluate nothing."
  (declare (ignore subchar parameter))
  (list 'read-time-eval (read stream t nil t)))

(defun inert-read-eval-readtable (&optional base)
  "A copy of BASE (the standard readtable when NIL) in which `#.' reads
its expression and returns (READ-TIME-EVAL expression) without evaluating
it."
  (let ((rt (copy-readtable base)))
    (set-dispatch-macro-character #\# #\. #'%inert-sharp-dot rt)
    rt))

(defvar *managed-lenient-packages* nil
  "Additional package objects treated as writable temporary packages.
Bound by parent-side package-context synthesis so the lenient reader can
intern/export symbols into pre-created local nickname target stubs.")

(defvar *lenient-real-packages* nil
  "When true, a symbol the reader cannot find — or finds internal — in a
package that EXISTS is accepted instead of signalled, and its name is
recorded in *UNRESOLVED-SYMBOLS*. Off by default: at a REPL a missing
symbol in a real package is usually a typo. The structure editors bind
it, because for them the real-package case is usually STALENESS — the
file names a symbol the image has not loaded yet — and refusing to
locate a form for that refuses exactly the files an edit is for
(measured 2026-09-04: an editing session fell back to text tools for
a whole arc over one such symbol). The Eclector path uses an
UNINTERNED symbol of the same name, so the real package is never
written to; the SBCL-reader path has to intern to continue, and
CALL-WITH-LENIENT-PACKAGES uninterns again when its extent exits.")

(defvar *unresolved-symbols* nil
  "Names (\"PACKAGE:SYMBOL\") accepted under *LENIENT-REAL-PACKAGES*,
newest first. Bind it around a read to collect that read's report.")

(defvar *lenient-interned-symbols* nil
  "(symbol . package) pairs the SBCL-reader path interned under
*LENIENT-REAL-PACKAGES*; CALL-WITH-LENIENT-PACKAGES uninterns them
on exit so a lenient read leaves no trace in a real package.")

(defun %stub-package-p (pkg stubs)
  "True when PKG is a stub this extent created or a caller-managed one —
the packages the lenient reader may intern into."
  (or (member pkg (car stubs) :test #'eq)
      (member pkg *managed-lenient-packages* :test #'eq)))

(defun %record-unresolved (pkg sym-name)
  "Book SYM-NAME in PKG as accepted-but-unresolved."
  (push (format nil "~A:~A" (package-name pkg) sym-name)
        *unresolved-symbols*))

(defvar *homeless-due-to-teardown*
  #+sbcl (make-hash-table :test #'eq :weakness :key :synchronized t)
  #-sbcl (make-hash-table :test #'eq)
  "Weak-key hash table of symbols that became homeless because the
synthesized package they were interned in was torn down at the end
of a reader operation (either a lenient stub or a file-package
context). Used by display code like lisp-read-file's form printer to
decide whether to render a homeless symbol as `#:foo' (genuinely
uninterned in source) or as plain `foo' (became homeless only as a
side effect of package teardown).

Entries are weakly held, so they disappear automatically when the
owning CST form is garbage-collected.")

(defun record-homeless-on-teardown (pkg)
  "Register every symbol whose home is PKG as becoming homeless due
to teardown. Must be called BEFORE `delete-package' so that the
symbols' home-package relationship is still intact. Silently does
nothing for NIL or deleted packages."
  (when (and pkg (packagep pkg) (package-name pkg))
    (do-symbols (s pkg)
      (when (eq (symbol-package s) pkg)
        (setf (gethash s *homeless-due-to-teardown*) t)))))

(defun %find-restart-by-name (name-string condition)
  "Find the first restart whose name matches NAME-STRING by string comparison.
Uses SYMBOL-NAME to avoid package-qualification issues with restart names
that may be internal to other packages (e.g., Eclector)."
  (dolist (r (compute-restarts condition))
    (when (and (restart-name r)
               (string= name-string
                        (symbol-name (restart-name r))))
      (return r))))

#+sbcl
(defun %find-restart-by-name-and-description (name-string desc-pattern
                                              condition)
  "Find a restart matching NAME-STRING whose description contains DESC-PATTERN.
Used for SBCL reader restarts where multiple restarts share the same name
but differ in scope (e.g., reader RETRY vs file-loader RETRY)."
  (dolist (r (compute-restarts condition))
    (when (and (restart-name r)
               (string= name-string
                        (symbol-name (restart-name r))))
      (let ((desc (format nil "~A" r)))
        (when (search desc-pattern desc :test #'char-equal)
          (return r))))))

(defun %handle-eclector-package-missing (condition stubs)
  "Handle Eclector's package-does-not-exist condition.
Creates a stub package and invokes the USE-PACKAGE restart."
  (let ((name (eclector.reader::desired-package-name condition)))
    (unless (find-package name)
      (push (make-package name :use nil) (car stubs)))
    (let ((r (%find-restart-by-name "USE-PACKAGE" condition)))
      (when r
        (invoke-restart r (find-package name))))))

(defun %handle-eclector-symbol-missing (condition stubs)
  "Handle Eclector's symbol-does-not-exist condition.
In a stub package we created, interns and exports the symbol via USE-VALUE
to prevent subsequent symbol-is-not-external errors on repeated
single-colon access. In a REAL package: under *LENIENT-REAL-PACKAGES* the
value used is an UNINTERNED symbol of that name — the package is never
written to — and the name is recorded in *UNRESOLVED-SYMBOLS*; otherwise
the error propagates so typos are reported normally."
  (let ((pkg (eclector.reader::desired-symbol-package condition))
        (sym-name (eclector.reader::desired-symbol-name condition)))
    (when pkg
      (cond
        ((%stub-package-p pkg stubs)
         (let ((sym (intern sym-name pkg)))
           (export sym pkg)
           (let ((r (%find-restart-by-name "USE-VALUE" condition)))
             (when r
               (invoke-restart r sym)))))
        (*lenient-real-packages*
         (%record-unresolved pkg sym-name)
         (let ((r (%find-restart-by-name "USE-VALUE" condition)))
           (when r
             (invoke-restart r (make-symbol sym-name)))))))))

(defun %handle-eclector-symbol-not-external (condition stubs)
  "Handle Eclector's symbol-is-not-external condition.
Invokes the USE-ANYWAY restart to accept the internal symbol as-is when
the package is a stub we created, or — under *LENIENT-REAL-PACKAGES* —
any real package, recording the name in *UNRESOLVED-SYMBOLS*."
  (let ((pkg (eclector.reader::desired-symbol-package condition)))
    (when (and pkg
               (or (%stub-package-p pkg stubs)
                   *lenient-real-packages*))
      (unless (%stub-package-p pkg stubs)
        (%record-unresolved pkg (eclector.reader::desired-symbol-name condition)))
      (let ((r (%find-restart-by-name "USE-ANYWAY" condition)))
        (when r
          (invoke-restart r))))))

(defun %handle-sbcl-reader-package-error (condition stubs)
  "Handle SBCL's simple-reader-package-error condition.
Dispatches on the format control string to determine whether the error
is about a missing package, a missing symbol, or a non-external symbol.
A missing or internal symbol in a stub package is interned and exported;
in a REAL package it is accepted only under *LENIENT-REAL-PACKAGES*, and
then a missing one has to be INTERNED for the reader to continue — it is
booked in *LENIENT-INTERNED-SYMBOLS* and uninterned when the lenient
extent exits, and its name recorded in *UNRESOLVED-SYMBOLS*."
  (let ((ctrl (simple-condition-format-control condition))
        (args (simple-condition-format-arguments condition)))
    (cond
      ;; "Package ~A does not exist."
      ((search "does not exist" ctrl :test #'char-equal)
       (let ((name (first args)))
         (when (stringp name)
           (unless (find-package name)
             (push (make-package name :use nil) (car stubs)))
           (let ((r (%find-restart-by-name-and-description
                     "RETRY" "finding" condition)))
             (when r (invoke-restart r))))))
      ;; "Symbol ~S not found in the ~A package."
      ((search "not found" ctrl :test #'char-equal)
       (let ((sym-name (first args))
             (pkg-name (second args)))
         (when (and (stringp sym-name) (stringp pkg-name))
           (let ((pkg (find-package pkg-name)))
             (when pkg
               (cond
                 ((%stub-package-p pkg stubs)
                  (export (intern sym-name pkg) pkg)
                  (let ((r (%find-restart-by-name "CONTINUE" condition)))
                    (when r (invoke-restart r))))
                 (*lenient-real-packages*
                  (let ((sym (intern sym-name pkg)))
                    (push (cons sym pkg) *lenient-interned-symbols*)
                    (%record-unresolved pkg sym-name)
                    (let ((r (%find-restart-by-name "CONTINUE" condition)))
                      (when r (invoke-restart r)))))))))))
      ;; "The symbol ~S is not external in the ~A package."
      ((search "not external" ctrl :test #'char-equal)
       (let ((sym-name (first args))
             (pkg-name (second args)))
         (when (and (stringp sym-name) (stringp pkg-name))
           (let ((pkg (find-package pkg-name)))
             (when pkg
               (cond
                 ((%stub-package-p pkg stubs)
                  (export (intern sym-name pkg) pkg)
                  (let ((r (%find-restart-by-name "CONTINUE" condition)))
                    (when r (invoke-restart r))))
                 (*lenient-real-packages*
                  ;; The symbol exists; CONTINUE uses it without exporting.
                  (%record-unresolved pkg sym-name)
                  (let ((r (%find-restart-by-name "CONTINUE" condition)))
                    (when r (invoke-restart r)))))))))))))

(defun %cleanup-stub-packages (stubs)
  "Delete all stub packages, first recording their soon-to-be-homeless
symbols in *HOMELESS-DUE-TO-TEARDOWN* and uninterning them.
Safe to call even if packages have already been deleted."
  (dolist (pkg stubs)
    (let ((name (ignore-errors (package-name pkg))))
      (when (and name (find-package name))
        (record-homeless-on-teardown pkg)
        (do-symbols (s pkg)
          (unintern s pkg))
        (delete-package pkg)))))

(defun call-with-lenient-packages (thunk)
  "Call THUNK with handler-bind that intercepts unknown-package conditions.

When the reader (Eclector or SBCL's CL reader) encounters a package-qualified
symbol whose package does not exist, this function creates an ephemeral stub
package so the read can proceed. After THUNK returns (or signals an error),
all stub packages are cleaned up via UNWIND-PROTECT — and so is any symbol
the SBCL-reader path interned into a REAL package under
*LENIENT-REAL-PACKAGES* (see that variable).

Handles four condition types:
  1. ECLECTOR.READER:PACKAGE-DOES-NOT-EXIST -- invokes USE-PACKAGE restart
  2. ECLECTOR.READER:SYMBOL-DOES-NOT-EXIST -- interns+exports via USE-VALUE
  3. ECLECTOR.READER:SYMBOL-IS-NOT-EXTERNAL -- invokes USE-ANYWAY restart
  4. SB-INT:SIMPLE-READER-PACKAGE-ERROR (SBCL) -- creates stub, retries

Arguments:
  THUNK -- A function of zero arguments to call under lenient resolution.

Returns:
  The values returned by THUNK.

Examples:
  (call-with-lenient-packages
    (lambda ()
      (eclector.reader:read-from-string \"(unknown-pkg:sym)\")))
  => (UNKNOWN-PKG::SYM)"
  (let ((stubs-cell (list nil))
        (*lenient-interned-symbols* nil))
    (unwind-protect
         (handler-bind
             ((package-does-not-exist
                (lambda (c)
                  (%handle-eclector-package-missing c stubs-cell)))
              (symbol-does-not-exist
                (lambda (c)
                  (%handle-eclector-symbol-missing c stubs-cell)))
              (symbol-is-not-external
                (lambda (c)
                  (%handle-eclector-symbol-not-external c stubs-cell)))
              #+sbcl
              (sb-int:simple-reader-package-error
                (lambda (c)
                 (%handle-sbcl-reader-package-error c stubs-cell))))
           (funcall thunk))
      (dolist (entry *lenient-interned-symbols*)
        (let ((sym (car entry)) (pkg (cdr entry)))
          (when (and (package-name pkg)
                     (eq (find-symbol (symbol-name sym) pkg) sym))
            (unintern sym pkg))))
      (%cleanup-stub-packages (car stubs-cell)))))

(defun call-with-lenient-real-packages (thunk)
  "CALL-WITH-LENIENT-PACKAGES with *LENIENT-REAL-PACKAGES* on: a symbol
missing from, or internal to, a package that exists is accepted too.
Returns THUNK's primary value and, second, the names it could not
resolve (\"PACKAGE:SYMBOL\", in source order, deduplicated) — for a
caller that wants to locate or validate a form and REPORT what the
image does not know, rather than refuse. The structure editors are
the intended caller; a REPL is not."
  (let ((*lenient-real-packages* t)
        (*unresolved-symbols* nil))
    (let ((result (call-with-lenient-packages thunk)))
      (values result
              (remove-duplicates (reverse *unresolved-symbols*)
                                 :test #'string= :from-end t)))))

(defun call-with-managed-packages (packages thunk)
  "Call THUNK with PACKAGES added to the managed temporary package set.
These packages are treated like writable stubs by call-with-lenient-packages,
but their lifecycle is owned by the caller."
  (let ((*managed-lenient-packages*
          (append packages *managed-lenient-packages*)))
    (call-with-lenient-packages thunk)))
