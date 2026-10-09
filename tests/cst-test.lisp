;;;; tests/cst-test.lisp

(defpackage #:cl-mcp/tests/cst-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/cst
                #:parse-top-level-forms
                #:cst-node-kind
                #:cst-node-start-line
                #:cst-node-end-line
                #:cst-node-value))

(in-package #:cl-mcp/tests/cst-test)

(deftest parse-top-level-forms-captures-comments-and-lines
  (testing "comment nodes are preserved with accurate line numbers"
    (let* ((text (format nil ";; heading~%~%(defun sample ()~%  :ok)~%"))
           (nodes (parse-top-level-forms text))
           (comment (find-if (lambda (n) (eq (cst-node-kind n) :skipped)) nodes))
           (expr (find-if (lambda (n) (eq (cst-node-kind n) :expr)) nodes)))
      (ok comment)
      (ok expr)
      (ok (= 1 (cst-node-start-line comment)))
      (ok (= 3 (cst-node-start-line expr)))
      (ok (<= (cst-node-end-line expr) 4))
      (ok (string= "sample" (string-downcase (second (cst-node-value expr))))))))

(deftest parse-top-level-forms-error-suggests-readtable
  (testing "reader error message suggests using readtable parameter"
    (let ((error-message
            (handler-case
                (progn
                  (parse-top-level-forms "(defun foo () #?\"test\")")
                  nil)
              (error (e)
                (format nil "~A" e)))))
      (ok error-message)
      (ok (search "readtable" error-message))
      (ok (search "interpol-syntax" error-message)))))

(deftest parse-top-level-forms-reads-sharp-dot-without-evaluating
  (testing "a #. form is parsed as data — (READ-TIME-EVAL expr) — and never run"
    (let* ((flag (gensym "READ-EVAL-FIRED"))
           (nodes (parse-top-level-forms
                   (format nil "(defun sample () #.(progn (setf (symbol-value '~S) t) 3))"
                           flag)))
           (expr (find-if (lambda (n) (eq (cst-node-kind n) :expr)) nodes))
           (body (fourth (cst-node-value expr))))
      (ok expr)
      (ok (string= "sample" (string-downcase (second (cst-node-value expr)))))
      (ok (eq 'cl-mcp/src/cst:read-time-eval (first body)))
      (ok (eq 'progn (first (second body))))
      (ok (not (boundp flag))))))

(deftest parse-top-level-forms-unknown-package-succeeds
  (testing "files with unknown package-qualified symbols parse successfully"
    (let* ((text (format nil "(defpackage #:demo (:use #:cl))~%~%(in-package #:demo)~%~%(defun process-data (x)~%  (unknown-dep:transform x))~%"))
           (nodes (parse-top-level-forms text))
           (expr-nodes (remove-if-not
                        (lambda (n) (eq (cst-node-kind n) :expr))
                        nodes)))
      (ok (= 3 (length expr-nodes))
          (format nil "Expected 3 expr nodes, got ~A" (length expr-nodes))))))

(deftest parse-top-level-forms-defmethod-unknown-class
  (testing "defmethod with unknown package-qualified specializer parses successfully"
    (let* ((text (format nil "(defmethod render ((widget ui-pkg:button))~%  (format nil \"<button>~~A</button>\" (ui-pkg:label widget)))~%"))
           (nodes (parse-top-level-forms text))
           (expr-nodes (remove-if-not
                        (lambda (n) (eq (cst-node-kind n) :expr))
                        nodes)))
      (ok (= 1 (length expr-nodes))
          (format nil "Expected 1 expr node, got ~A" (length expr-nodes))))))

(deftest parse-top-level-forms-host-character-names
  (testing "every character name the host's NAME-CHAR accepts reads as that character"
    ;; 2026-10-10: Eclector's table lacks SBCL's #\Nul, so heddle's
    ;; src/ast.lisp — `(char #\Nul :type character)' — refused every edit.
    (dolist (name '("Nul" "Null" "Escape" "Esc" "Bell" "Del" "Soh" "Rubout" "Page"
                    "Backspace" "Linefeed" "Tab" "U+41" "GREEK_SMALL_LETTER_ALPHA"))
      (let ((expected (name-char name)))
        (when expected                  ; a name this host lacks says nothing
          (let ((node (first (parse-top-level-forms
                              (format nil "(defstruct x (c #\\~A :type character))" name)))))
            (ok (eql expected (second (third (cst-node-value node))))
                (format nil "#\\~A" name)))))))
  (testing "a name the host does not know is still a reader error"
    (ok (handler-case (progn (parse-top-level-forms "(a #\\NoSuchCharacterName)") nil)
          (error () t)))))
