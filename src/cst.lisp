;;;; src/cst.lisp

(defpackage #:cl-mcp/src/cst
  (:use #:cl)
  (:import-from #:eclector.parse-result
                #:parse-result-client
                #:make-expression-result
                #:make-skipped-input-result)
  (:import-from #:cl-mcp/src/package-context
                #:call-with-file-package-context
                #:call-with-package-context)
  (:import-from #:cl-mcp/src/utils/lenient-read
                #:call-with-lenient-packages
                #:read-time-eval
                #:inert-read-eval-readtable)
  (:export #:cst-node
           #:cst-node-kind
           #:cst-node-value
           #:cst-node-children
           #:cst-node-start
           #:cst-node-end
           #:cst-node-start-line
           #:cst-node-end-line
           #:parse-top-level-forms
           ;; Exported for cl-mcp/src/lisp-macroexpand, which has to answer
           ;; the same question this file answers while parsing: which
           ;; IN-PACKAGE is in effect at a given point in the file.
           #:%in-package-form-p
           ;; `#.' read as data: the marker and the readtable that does it.
           #:read-time-eval
           #:inert-read-eval-readtable
           ;; A file's IN-READTABLE names a readtable this image lacks.
           #:readtable-not-in-image
           #:readtable-not-in-image-designator
           #:named-readtable-in-image))

(in-package #:cl-mcp/src/cst)

(defstruct cst-node
  kind
  value
  children
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (start-line 1 :type fixnum)
  (end-line 1 :type fixnum))

(declaim (type (simple-array fixnum (*)) *line-table*))
(defvar *line-table* (make-array 1 :element-type 'fixnum :initial-element 1))

(defun %build-line-table (text)
  "Return a vector mapping character offsets in TEXT to 1-based line numbers."
  (let* ((len (length text))
         (table (make-array (1+ len) :element-type 'fixnum))
         (line 1))
    (loop for i fixnum from 0 below len
          for ch = (char text i) do
            (setf (aref table i) line)
            (when (char= ch #\Newline)
              (incf line)))
    (setf (aref table len) line)
    table))

(defun %pos->line (pos)
  (aref *line-table* (min pos (1- (length *line-table*)))))

(defun %in-readtable-form-p (form)
  "Return the readtable designator if FORM is an IN-READTABLE form, NIL otherwise."
  (and (consp form)
       (let ((head (car form)))
         (and (symbolp head)
              (string= (symbol-name head) "IN-READTABLE")
              (consp (cdr form))
              (second form)))))

(defun %in-package-form-p (form)
  "Return the package designator string if FORM is an IN-PACKAGE form, NIL otherwise."
  (and (consp form)
       (let ((head (car form)))
         (and (symbolp head)
              (string= (symbol-name head) "IN-PACKAGE")
              (consp (cdr form))
              (let ((designator (second form)))
                (cond ((stringp designator) designator)
                      ((symbolp designator) (symbol-name designator))
                      (t nil)))))))

(defun named-readtable-in-image (designator)
  "The named readtable DESIGNATOR names in this image, or NIL when
named-readtables is not loaded or no readtable of that name is defined —
the system that defines it is not loaded."
  (let ((pkg (or (find-package :named-readtables)
                 (find-package :editor-hints.named-readtables))))
    (when pkg
      (let ((find-fn (find-symbol "FIND-READTABLE" pkg)))
        (when (and find-fn (fboundp find-fn))
          (funcall find-fn designator))))))

(define-condition readtable-not-in-image (error)
  ((designator :initarg :designator :reader readtable-not-in-image-designator)
   (cause :initarg :cause :initform nil :reader readtable-not-in-image-cause))
  (:documentation "A file declares (IN-READTABLE DESIGNATOR), or a caller
named DESIGNATOR, and this image defines no readtable of that name, so text
written in it could not be read. Signalled instead of the bare reader error
that reading with the standard readtable produces, which names a reader
macro and not the cause: the system defining the readtable is not loaded.")
  (:report (lambda (c s)
             (format s "Readtable ~S is not defined in this image (the system that ~
defines it is not loaded), so this text was read with the standard readtable~
~@[ and did not read: ~A~]. Load the system that defines ~S and retry; ~
nothing was written."
                     (readtable-not-in-image-designator c)
                     (let ((cause (readtable-not-in-image-cause c)))
                       ;; one line: reader errors wrap and indent their stream
                       (and cause (format nil "~{~A~^ ~}"
                                          (remove "" (uiop:split-string
                                                      (substitute #\Space #\Newline
                                                                  (princ-to-string cause))
                                                      :separator " ")
                                                  :test #'string=))))
                     (readtable-not-in-image-designator c)))))

(defun %read-remaining-with-cl-reader (stream nodes custom-readtable)
  "Read remaining forms from STREAM using standard CL reader with CUSTOM-READTABLE.
Returns the complete list of nodes (including previously collected NODES)."
  (let ((*readtable* (inert-read-eval-readtable custom-readtable))
        (*read-eval* nil))
    (loop
      (let ((start-pos (file-position stream)))
        ;; Skip whitespace
        (loop for ch = (peek-char nil stream nil :eof)
              while (and (characterp ch)
                         (member ch '(#\Space #\Tab #\Newline #\Return)))
              do (read-char stream))
        (setf start-pos (file-position stream))
        (let ((form (handler-case (read stream nil :eof)
                      (error (e)
                        ;; On read error, return what we have so far
                        (declare (ignore e))
                        (return (nreverse nodes))))))
          (when (eq form :eof)
            (return (nreverse nodes)))
          (let* ((end-pos (file-position stream))
                 (node (make-cst-node :kind :expr
                                      :value form
                                      :children nil
                                      :start start-pos
                                      :end end-pos
                                      :start-line (%pos->line start-pos)
                                      :end-line (%pos->line end-pos))))
            ;; Track in-package to activate local-nicknames
            (let ((pkg-name (%in-package-form-p form)))
              (when pkg-name
                (let ((pkg (find-package pkg-name)))
                  (when pkg
                    (setf *package* pkg)))))
            (push node nodes)))))))

;; Eclector's MAKE-SKIPPED-INPUT-RESULT generic function changed shape
;; between releases:
;;   - Eclector <= 0.10 (and the Feb-2024 release vendored under
;;     local-projects): (client stream reason source)           -- 4 args
;;   - Eclector >= 0.11 (Quicklisp 2024-10-12 and later):
;;     (client stream reason children source)                   -- 5 args
;;
;; Rather than committing to one API and breaking the other, we inspect
;; the installed generic function at compile time and push the feature
;; :CL-MCP-ECLECTOR-SKIPPED-HAS-CHILDREN when the 5-argument signature is
;; in effect. The reader conditional below then emits the matching
;; DEFMETHOD. Note that we push the feature inside EVAL-WHEN with
;; :COMPILE-TOPLEVEL so the reader sees it on the *next* top-level form
;; in the same file.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((gf (fdefinition 'make-skipped-input-result))
         (ll (#+sbcl sb-mop:generic-function-lambda-list
              #+ccl ccl:generic-function-lambda-list
              #+(or ecl clasp) clos:generic-function-lambda-list
              #-(or sbcl ccl ecl clasp) closer-mop:generic-function-lambda-list
              gf))
         (required (loop for p in ll
                         until (and (symbolp p)
                                    (member p lambda-list-keywords))
                         count 1)))
    (cond
      ((= required 5)
       (pushnew :cl-mcp-eclector-skipped-has-children *features*))
      ((= required 4)
       (setf *features* (remove :cl-mcp-eclector-skipped-has-children *features*)))
      (t
       (warn "Unexpected arity ~D for ECLECTOR.PARSE-RESULT:MAKE-SKIPPED-INPUT-RESULT; ~
              cl-mcp/src/cst assumes 4 or 5 required arguments."
             required)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod make-expression-result ((client parse-result-client)
                                     result children source)
    (declare (ignore client))
    (destructuring-bind (start . end) source
      (make-cst-node :kind :expr
                     :value result
                     :children children
                     :start start
                     :end end
                     :start-line (%pos->line start)
                     :end-line (%pos->line end))))

  #+cl-mcp-eclector-skipped-has-children
  (defmethod make-skipped-input-result ((client parse-result-client)
                                        stream reason children source)
    (declare (ignore client stream children))
    (destructuring-bind (start . end) source
      (make-cst-node :kind :skipped
                     :value reason
                     :children nil
                     :start start
                     :end end
                     :start-line (%pos->line start)
                     :end-line (%pos->line end))))

  #-cl-mcp-eclector-skipped-has-children
  (defmethod make-skipped-input-result ((client parse-result-client)
                                        stream reason source)
    (declare (ignore client stream))
    (destructuring-bind (start . end) source
      (make-cst-node :kind :skipped
                     :value reason
                     :children nil
                     :start start
                     :end end
                     :start-line (%pos->line start)
                     :end-line (%pos->line end)))))

;;; `#.' is READ, never evaluated.
;;;
;;; Source is parsed here to be located and edited, not run, so a `#.'
;;; form is kept as data — (READ-TIME-EVAL <expression>) in the CST
;;; value, verbatim in the text. The earlier posture refused the whole
;;; file, which protected the same property (nothing evaluates) at the
;;; cost of making every file with a `#.(char-code ...)' in it
;;; unreachable by the structure editors — measured 2026-09-05 on a
;;; 2,700-line parser whose one `#.' sat inside a name helper, so a
;;; patch to an unrelated function in it was refused. The CL-reader
;;; sites (the package-context header scan, the custom-readtable path
;;; here, the two editors' validators) get the same treatment through
;;; INERT-READ-EVAL-READTABLE in utils/lenient-read.lisp, where the
;;; marker symbol lives.
(defclass cst-client (parse-result-client) ()
  (:documentation "The parse-result client whose read-time evaluation is
inert: EVALUATE-EXPRESSION answers (READ-TIME-EVAL expression)."))

(defmethod eclector.reader:evaluate-expression ((client cst-client) expression)
  (list 'read-time-eval expression))

(defun %parse-top-level-forms-core (text readtable)
  "Parse TEXT into CST nodes assuming *PACKAGE* and *LINE-TABLE* are already bound.
An IN-READTABLE naming a readtable this image lacks does not stop the parse:
the rest is read with the standard readtable, recovering from what that
cannot read (form boundaries stay exact; the values of forms that needed the
missing syntax do not). Only when recovery is impossible is
READTABLE-NOT-IN-IMAGE signalled, naming the readtable rather than the
reader macro it would have defined. The editors read the EDITED text with
the file's readtable separately (%TARGET-READTABLE), and refuse there."
  (if readtable
      (let ((custom-rt (named-readtable-in-image readtable)))
        (if custom-rt
            (call-with-lenient-packages
             (lambda ()
               (with-input-from-string (stream text)
                 (%read-remaining-with-cl-reader stream nil custom-rt))))
            (error 'readtable-not-in-image :designator readtable)))
      (let ((*readtable* (copy-readtable))
            ;; T so eclector reads a `#.' expression instead of refusing
            ;; the file; CST-CLIENT's EVALUATE-EXPRESSION then keeps it
            ;; as data. Nothing is evaluated on this path.
            (*read-eval* t)
            (nodes '())
            (missing nil)               ; a declared readtable the image lacks
            (client (make-instance 'cst-client)))
        (with-input-from-string (stream text)
          (handler-case
              (call-with-lenient-packages
               (lambda ()
                 (handler-bind
                     ;; After an IN-READTABLE the image lacks, read on with
                     ;; the standard readtable and RECOVER from what it
                     ;; cannot read: eclector keeps every top-level form's
                     ;; boundaries, which is all locating a form needs (the
                     ;; recovered form's VALUE is not what the file means).
                     ((reader-error
                        (lambda (e)
                          (declare (ignore e))
                          (let ((recover (and missing
                                              (find-restart 'eclector.reader:recover))))
                            (when recover
                              (invoke-restart recover))))))
                 (loop
                   (multiple-value-bind (result orphan-results)
                       (eclector.parse-result:read client stream nil :eof)
                     (dolist (orphan orphan-results)
                       (push orphan nodes))
                     (when (eq result :eof)
                       (return (nreverse nodes)))
                     (push result nodes)
                     (when (and (typep result 'cst-node)
                                (eq (cst-node-kind result) :expr))
                       (let ((pkg-name (%in-package-form-p
                                        (cst-node-value result))))
                         (when pkg-name
                           (let ((pkg (find-package pkg-name)))
                             (when pkg
                               (setf *package* pkg)))))
                       (let ((designator
                               (%in-readtable-form-p (cst-node-value result))))
                         (when designator
                           (let ((custom-rt (named-readtable-in-image designator)))
                             (unless custom-rt
                               (setf missing designator))
                             (when custom-rt
                               (return
                                 (%read-remaining-with-cl-reader
                                  stream nodes custom-rt))))))))))))
            (reader-error (e)
              (when missing
                (error 'readtable-not-in-image :designator missing :cause e))
              (error
               "Reader error: ~A~%~%If this file uses custom reader macros (e.g., cl-interpol's #?), ~
specify the 'readtable' parameter with the named-readtable designator ~
(e.g., readtable: \"interpol-syntax\")."
               e)))))))

(defun parse-top-level-forms (text &key readtable source-path initial-package)
  "Parse TEXT into CST-NODE values. When READTABLE is provided, use that named readtable.
Unknown package-qualified symbols are handled leniently by creating ephemeral
stub packages that are cleaned up after parsing.
When an IN-PACKAGE form is encountered, *PACKAGE* is updated so that
package-local-nicknames activate for subsequent forms."
  (let ((*line-table* (%build-line-table text))
        (*package* *package*))
    (cond
      (initial-package
       (call-with-package-context
        initial-package
        (lambda ()
          (%parse-top-level-forms-core text readtable))
        :source-path source-path))
      (source-path
       (call-with-file-package-context
        source-path
        (lambda ()
          (%parse-top-level-forms-core text readtable))
        :text text
        :readtable (and readtable (named-readtable-in-image readtable))))
      (t
       (%parse-top-level-forms-core text readtable)))))
