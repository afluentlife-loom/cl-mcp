;;;; src/utils/compilation-unit.lisp
;;;;
;;;; A compilation unit around an ASDF operation, opened only when one is
;;;; needed.  Shared by load-system and run-tests' reload.

(defpackage #:cl-mcp/src/utils/compilation-unit
  (:use #:cl)
  (:export #:call-with-own-compilation-unit))

(in-package #:cl-mcp/src/utils/compilation-unit)

(defun %in-compilation-unit-p ()
  "True when a compilation unit is open on this thread.  SBCL keeps the open
unit in SB-C::*COMPILATION-UNIT* (2.4.8 on); releases before it only a flag,
SB-C::*IN-COMPILATION-UNIT*.  Neither is bound to anything but NIL outside a
unit.  When an SBCL has neither, the answer is T: an own unit always, which
counts a refused compile twice, rather than none inside an outer unit, which
would lose the end-of-unit report from the caller's capture."
  (let ((variables (remove nil (mapcar (lambda (name) (find-symbol name "SB-C"))
                                       '("*COMPILATION-UNIT*" "*IN-COMPILATION-UNIT*")))))
    (or (null variables)
        (some (lambda (variable)
                (and (boundp variable) (symbol-value variable) t))
              variables))))

(defun call-with-own-compilation-unit (thunk)
  "Call THUNK, an ASDF operation, so that the compiler's end-of-unit report --
its summary, and the undefined functions and variables it warns of last -- is
made while the caller's bindings and handlers are still in effect, not when a
unit some outer caller opened ends.

ASDF opens a unit of its own around its plan.  When no unit is open yet, that
one is the outermost and already reports within THUNK: THUNK is called as it
is.  Only inside a unit opened earlier does THUNK get one of its own
(:OVERRIDE T).  Opening it regardless miscounts: SBCL counts an aborted unit
once for every unit the abort unwinds through, so a file ASDF refuses would be
reported as 'caught 2 fatal ERROR conditions' where ASDF by itself reports 1.
Inside an outer unit that second count remains, the price of reporting there
at all.  None of load-system's and run-tests' entry points runs inside one: the
tools run them on a deadline thread or the worker's init thread, where no unit
is open."
  (if (%in-compilation-unit-p)
      (with-compilation-unit (:override t)
        (funcall thunk))
      (funcall thunk)))
