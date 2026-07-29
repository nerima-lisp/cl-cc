;;;; tests/unit/compile/ir/ir-printer-tests.lisp — Extended IR Printer Tests
;;;;
;;;; Tests for src/compile/ir/printer.lisp:
;;;; ir-format-value, ir-print-block, ir-print-function, ir-print-module,
;;;; ir-function-to-string — multi-block, parameterized blocks, modules.

(in-package :cl-cc/test)



;;; ─── ir-format-value ──────────────────────────────────────────────────────────

(it-sequential "ir-format-value-assigns-sequential-percent-ids"
  (let* ((fn (cl-cc/ir:ir-make-function 'test))
         (v0 (cl-cc/ir:ir-new-value fn))
         (v1 (cl-cc/ir:ir-new-value fn))
         (v2 (cl-cc/ir:ir-new-value fn)))
    (expect (cl-cc/ir:ir-format-value v0) :to-equal "%0")
    (expect (cl-cc/ir:ir-format-value v1) :to-equal "%1")
    (expect (cl-cc/ir:ir-format-value v2) :to-equal "%2")))

(it-sequential "ir-format-value-string-contains-literal-text"
  (expect (search "hello" (cl-cc/ir:ir-format-value "hello")) :to-be-truthy))

(it-sequential "ir-format-value-symbol-is-uppercased"
  (expect (search "FOO" (cl-cc/ir:ir-format-value 'foo)) :to-be-truthy))

;;; ─── ir-print-block ───────────────────────────────────────────────────────────

(it-sequential "ir-print-block-variations"
  (let* ((fn    (cl-cc/ir:ir-make-function 'test))
         (entry (cl-cc/ir:irf-entry fn))
         (then  (cl-cc/ir:ir-new-block fn :then))
         (join  (cl-cc/ir:ir-new-block fn :join))
         (v     (cl-cc/ir:ir-new-value fn))
         (v0    (cl-cc/ir:ir-new-value fn))
         (inst  (cl-cc/ir:make-ir-inst :result v0))
         (term  (cl-cc/ir:make-ir-inst)))
    ;; no predecessors shows (none)
    (let ((s (with-output-to-string (out) (cl-cc/ir:ir-print-block entry out))))
      (expect (search "(none)" s) :to-be-truthy))
    ;; block with predecessor lists it
    (cl-cc/ir:ir-add-edge entry then)
    (let ((s (with-output-to-string (out) (cl-cc/ir:ir-print-block then out))))
      (expect (search "entry" s) :to-be-truthy))
    ;; block with params shows label and param
    (push v (cl-cc/ir:irb-params join))
    (let ((s (with-output-to-string (out) (cl-cc/ir:ir-print-block join out))))
      (expect (search "join" s) :to-be-truthy)
      (expect (search (cl-cc/ir:ir-format-value v) s) :to-be-truthy))
    ;; block with instruction shows result value
    (cl-cc/ir:ir-emit entry inst)
    (let ((s (with-output-to-string (out) (cl-cc/ir:ir-print-block entry out))))
      (expect (search "%1" s) :to-be-truthy))
    ;; block with terminator shows its type name
    (cl-cc/ir:ir-set-terminator entry term)
    (let ((s (with-output-to-string (out) (cl-cc/ir:ir-print-block entry out))))
      (expect (search "IR-INST" s) :to-be-truthy))))

;;; ─── ir-print-function / ir-function-to-string ────────────────────────────────

(it-sequential "ir-print-function-aspects"
  (let* ((fn (cl-cc/ir:ir-make-function 'add))
         (s  (cl-cc/ir:ir-function-to-string fn)))
    (expect (search "define" s) :to-be-truthy)
    (expect (search "}" s) :to-be-truthy))
  (let* ((fn (cl-cc/ir:ir-make-function 'add :return-type :integer))
         (s  (cl-cc/ir:ir-function-to-string fn)))
    (expect (search "INTEGER" s) :to-be-truthy))
  (let* ((fn (cl-cc/ir:ir-make-function nil))
         (s  (cl-cc/ir:ir-function-to-string fn)))
    (expect (search "anonymous" s) :to-be-truthy))
  (let* ((fn (cl-cc/ir:ir-make-function 'f))
         (p0 (cl-cc/ir:ir-new-value fn))
         (p1 (cl-cc/ir:ir-new-value fn)))
    (setf (cl-cc/ir:irf-params fn) (list p0 p1))
    (let ((s (cl-cc/ir:ir-function-to-string fn)))
      (expect (search "%0" s) :to-be-truthy)
      (expect (search "%1" s) :to-be-truthy)))
  (let* ((fn    (cl-cc/ir:ir-make-function 'branch))
         (entry (cl-cc/ir:irf-entry fn))
         (then  (cl-cc/ir:ir-new-block fn :then))
         (else  (cl-cc/ir:ir-new-block fn :else)))
    (cl-cc/ir:ir-add-edge entry then)
    (cl-cc/ir:ir-add-edge entry else)
    (let ((s (cl-cc/ir:ir-function-to-string fn)))
      (expect (search "entry" s) :to-be-truthy)
      (expect (search "then" s) :to-be-truthy)
      (expect (search "else" s) :to-be-truthy))))

;;; ─── ir-print-module ──────────────────────────────────────────────────────────

(it-sequential "ir-print-module-empty-shows-zero-count"
  (let* ((mod (cl-cc/ir:make-ir-module :functions nil))
         (s   (with-output-to-string (out) (cl-cc/ir:ir-print-module mod out))))
    (expect (search "IR Module" s) :to-be-truthy)
    (expect (search "0" s) :to-be-truthy)))

(it-sequential "ir-print-module-single-function-included"
  (let* ((fn  (cl-cc/ir:ir-make-function 'my-fn))
         (mod (cl-cc/ir:make-ir-module :functions (list fn)))
         (s   (with-output-to-string (out) (cl-cc/ir:ir-print-module mod out))))
    (expect (search "1 function" s) :to-be-truthy)
    (expect (search "my-fn" s) :to-be-truthy)))

(it-sequential "ir-print-module-multiple-functions-in-order"
  (let* ((f1  (cl-cc/ir:ir-make-function 'alpha))
         (f2  (cl-cc/ir:ir-make-function 'beta))
         (mod (cl-cc/ir:make-ir-module :functions (list f1 f2)))
         (s   (with-output-to-string (out) (cl-cc/ir:ir-print-module mod out))))
    (expect (search "2 functions" s) :to-be-truthy)
    (let ((pos-a (search "alpha" s))
          (pos-b (search "beta" s)))
      (expect pos-a :to-be-truthy)
      (expect pos-b :to-be-truthy)
      (expect (< pos-a pos-b) :to-be-truthy))))
