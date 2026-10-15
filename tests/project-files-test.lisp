;;;; tests/project-files-test.lisp
;;;;
;;;; Tests for cl-mcp/src/utils/project-files: which files are the project's own.

(defpackage #:cl-mcp/tests/project-files-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/utils/project-files
                #:project-file-predicate
                #:project-ignored-predicate)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  ;; Their internals are tested here; importing them makes run-tests reload them.
  (:import-from #:cl-mcp/src/code-core)
  (:import-from #:cl-mcp/src/clos-core)
  (:import-from #:cl-mcp/src/utils/fasls))

(in-package #:cl-mcp/tests/project-files-test)

(defun %call-with-project-tree (files thunk)
  "Write FILES, an alist of (name . text), into a fresh temporary directory, call
THUNK with that directory, and remove it."
  (let ((root (uiop:ensure-directory-pathname
               (uiop:merge-pathnames*
                (format nil "clmcp-project-files-~36R/"
                        (random most-positive-fixnum (make-random-state t)))
                (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (loop for (name . text) in files
                 for path = (uiop:merge-pathnames* name root)
                 do (ensure-directories-exist path)
                    (with-open-file (out path :direction :output :if-exists :supersede)
                      (write-string text out)))
           (funcall thunk (truename root)))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(deftest project-file-predicate-leaves-out-what-the-project-ignores
  (%call-with-project-tree
   `((".gitignore" . ,(format nil ".qlot/~%*.fasl~%")))
   (lambda (root)
     (let ((own-p (project-file-predicate root)))
       (flet ((at (name) (uiop:merge-pathnames* name root)))
         (testing "a source of the project is its own"
           (ok (funcall own-p (at "src/main.lisp"))))
         (testing "a dependency qlot vendors under the root is not"
           (ok (not (funcall own-p (at ".qlot/dists/quicklisp/software/dep/dep.lisp")))))
         (testing "nor is anything under .git, which no .gitignore lists"
           (ok (not (funcall own-p (at ".git/hooks/post-commit.lisp")))))
         (testing "nor a file outside the root"
           (ok (not (funcall own-p #p"/elsewhere/src/main.lisp"))))
         (testing "nor no file at all"
           (ok (not (funcall own-p nil)))))))))

(deftest project-file-predicate-matches-ignore-patterns-by-whole-component
  ;; A pattern matched as bare text anywhere in the path left out
  ;; src/routes.lisp for out and src/catalog.lisp for log, and project_only
  ;; dropped their callers without a word.
  (%call-with-project-tree
   `((".gitignore" . ,(format nil "out~%log~%build/~%")))
   (lambda (root)
     (let ((own-p (project-file-predicate root)))
       (flet ((at (name) (uiop:merge-pathnames* name root)))
         (ok (funcall own-p (at "src/routes.lisp")))
         (ok (funcall own-p (at "src/catalog.lisp")))
         (ok (funcall own-p (at "src/builder.lisp")))
         (ok (not (funcall own-p (at "out/generated.lisp"))))
         (ok (not (funcall own-p (at "src/log/trace.lisp"))))
         (ok (not (funcall own-p (at "build/x.lisp")))))))))

(deftest project-ignored-predicate-is-true-only-for-ignored-files-under-the-root
  (%call-with-project-tree
   '((".gitignore" . ".qlot/"))
   (lambda (root)
     (let ((ignored-p (project-ignored-predicate root)))
       (ok (funcall ignored-p (uiop:merge-pathnames* ".qlot/dep/dep.lisp" root)))
       (ok (not (funcall ignored-p (uiop:merge-pathnames* "src/main.lisp" root))))
       (ok (not (funcall ignored-p #p"/elsewhere/.qlot/dep.lisp"))
           "a file outside the root is not left out by the root's .gitignore")))))

(deftest project-file-predicate-without-a-root-takes-every-file
  (let ((own-p (project-file-predicate nil)))
    (ok (funcall own-p #p"/anywhere/.qlot/dep.lisp"))
    (ok (not (funcall own-p nil)))))

(deftest project-file-predicate-defaults-to-the-project-root
  (%call-with-project-tree
   '((".gitignore" . ".qlot/"))
   (lambda (root)
     (let ((*project-root* root))
       (ok (not (funcall (project-file-predicate)
                         (uiop:merge-pathnames* ".qlot/dep/dep.lisp" root))))))))

(defun %compile-and-load-under-own-name (file)
  "Compile and load FILE with its truename as the source namestring, so SBCL
records it, not a name the caller's compilation unit gives, as the source of
what FILE defines."
  (let ((truename (truename file)))
    (uiop:with-temporary-file (:pathname fasl :type "fasl")
      (with-compilation-unit (:override t :source-namestring (namestring truename))
        (handler-bind ((warning #'muffle-warning))
          (load (compile-file truename :output-file fasl :verbose nil :print nil)))))))

(defun %call-with-vendored-callers (thunk)
  "Under a fresh root whose .gitignore leaves out .qlot/, compile and load a
target function and generic function, and in src/own.lisp and .qlot/dep/dep.lisp
a caller of the one and a method on the other each, then call THUNK with the
root and remove it all.  .qlot/ sorts before src/, so a dependency taken for the
project's comes first."
  (%call-with-project-tree
   '((".gitignore" . ".qlot/")
     ("defs.lisp" . "(defpackage #:clmcp-pf-fixture (:use #:cl))
(in-package #:clmcp-pf-fixture)
(defun pf-target () 1)
(defgeneric pf-describe (x))")
     ("src/own.lisp" . "(in-package #:clmcp-pf-fixture)
(defun own-caller () (pf-target))
(defmethod pf-describe ((x integer)) :own)")
     (".qlot/dep/dep.lisp" . "(in-package #:clmcp-pf-fixture)
(defun dep-caller () (pf-target))
(defmethod pf-describe ((x string)) :dep)"))
   (lambda (root)
     (unwind-protect
          (progn
            (dolist (name '("defs.lisp" "src/own.lisp" ".qlot/dep/dep.lisp"))
              (%compile-and-load-under-own-name (uiop:merge-pathnames* name root)))
            (funcall thunk root))
       (ignore-errors (delete-package "CLMCP-PF-FIXTURE"))))))

(deftest code-find-references-project-only-leaves-out-vendored-dependencies
  ;; The xref half of code-find-references, which project_only filters; the
  ;; source scan honours .gitignore, so the two now agree on what is the
  ;; project's.
  (%call-with-vendored-callers
   (lambda (root)
     (let* ((*project-root* root)
            (target (find-symbol "PF-TARGET" "CLMCP-PF-FIXTURE"))
            (paths (mapcar (lambda (entry) (namestring (getf entry :truename)))
                           (cl-mcp/src/code-core::%collect-xref-entries
                            target :project-only t))))
       (ok (find "src/own.lisp" paths :test (lambda (name path) (search name path))))
       (ok (notany (lambda (path) (search "/.qlot/" path)) paths))
       (ok (find "/.qlot/" (mapcar (lambda (entry) (namestring (getf entry :truename)))
                                   (cl-mcp/src/code-core::%collect-xref-entries
                                    target :project-only nil))
                 :test (lambda (name path) (search name path)))
           "without project_only the dependency's caller is there")))))

(deftest clos-describe-lists-a-vendored-dependencys-methods-after-the-projects
  (%call-with-vendored-callers
   (lambda (root)
     (let* ((*project-root* root)
            (gf (fdefinition (find-symbol "PF-DESCRIBE" "CLMCP-PF-FIXTURE")))
            (sorted (cl-mcp/src/clos-core::%sorted-methods
                     (sb-mop:generic-function-methods gf) t)))
       (ok (= 2 (length sorted)))
       (ok (eq (find-class 'integer)
               (first (sb-mop:method-specializers (first sorted))))
           "the project's method on INTEGER first, the dependency's on STRING after")))))

(deftest same-second-fasl-check-skips-ignored-directories
  ;; Every load-system and run-tests walks the project's tree for FASLs a
  ;; same-second edit made look current.  Walking a vendored .qlot cost seconds
  ;; per call in a large project, and its FASLs are not the project's to delete.
  (%call-with-project-tree
   `(("probe-own.asd" . "(defsystem \"probe-own\" :components ((:file \"main\")))")
     ("main.lisp" . "(in-package #:cl-user)")
     (".gitignore" . ".qlot/")
     (".qlot/dists/dep/dep.lisp" . "(in-package #:cl-user)"))
   (lambda (root)
     (unwind-protect
          (progn
            (asdf:load-asd (uiop:merge-pathnames* "probe-own.asd" root))
            (let ((sources (mapcar (lambda (pair) (namestring (car pair)))
                                   (cl-mcp/src/utils/fasls::%source-fasl-pairs "probe-own"))))
              (ok (find (namestring (uiop:merge-pathnames* "main.lisp" root)) sources
                        :test #'string=))
              (ok (notany (lambda (source) (search "/.qlot/" source)) sources))))
       (asdf:clear-system "probe-own")))))

(deftest same-second-fasl-check-honours-the-roots-gitignore-below-it
  ;; A system in a subdirectory of the root: what the root's .gitignore leaves
  ;; out is not the system's to check, though the system's own directory has
  ;; no .gitignore saying so.
  (%call-with-project-tree
   `((".gitignore" . "apps/foo/vendor/")
     ("apps/foo/probe-sub.asd" . "(defsystem \"probe-sub\" :components ((:file \"main\")))")
     ("apps/foo/main.lisp" . "(in-package #:cl-user)")
     ("apps/foo/vendor/dep/dep.lisp" . "(in-package #:cl-user)"))
   (lambda (root)
     (unwind-protect
          (let ((*project-root* root))
            (asdf:load-asd (uiop:merge-pathnames* "apps/foo/probe-sub.asd" root))
            (let ((sources (mapcar (lambda (pair) (namestring (car pair)))
                                   (cl-mcp/src/utils/fasls::%source-fasl-pairs "probe-sub"))))
              (ok (find (namestring (uiop:merge-pathnames* "apps/foo/main.lisp" root)) sources
                        :test #'string=))
              (ok (notany (lambda (source) (search "/vendor/" source)) sources))))
       (asdf:clear-system "probe-sub")))))

(deftest same-second-fasl-check-walks-a-system-kept-in-an-ignored-directory
  ;; The system being loaded is itself in a directory the root's .gitignore
  ;; leaves out, as a library kept in .qlot/local-projects/ is: leaving out what
  ;; that .gitignore says would leave out the whole walk.
  (%call-with-project-tree
   `((".gitignore" . ".qlot/")
     (".qlot/local-projects/probe-dep/probe-dep.asd"
      . "(defsystem \"probe-dep\" :components ((:file \"main\")))")
     (".qlot/local-projects/probe-dep/main.lisp" . "(in-package #:cl-user)")
     (".qlot/local-projects/probe-dep/extra.lisp" . "(in-package #:cl-user)"))
   (lambda (root)
     (unwind-protect
          (let ((*project-root* root))
            (asdf:load-asd (uiop:merge-pathnames*
                            ".qlot/local-projects/probe-dep/probe-dep.asd" root))
            (let ((sources (mapcar (lambda (pair) (namestring (car pair)))
                                   (cl-mcp/src/utils/fasls::%source-fasl-pairs "probe-dep"))))
              ;; extra.lisp is no component: only the walk finds it.
              (ok (find "extra.lisp" sources
                        :test (lambda (name source) (search name source))))))
       (asdf:clear-system "probe-dep")))))
