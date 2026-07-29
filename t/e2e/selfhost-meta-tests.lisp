;;;; selfhost-meta-tests.lisp — Meta-circular compilation, our-load, and all-source-files self-hosting tests
(in-package :cl-cc/test)


;;; ─── Self-Hosting: Meta-Circular Compilation ─────────────────────────────

(it-sequential "selfhost-meta-circular-eval"
  (expect (run-string (format nil "~A" (run-string "(+ 21 21)"))) :to-be 42))

(it-sequential "selfhost-meta-circular-defun"
  (expect (run-repl-forms
     "(defun sh-meta-f (n) (if (<= n 1) 1 (* n (sh-meta-f (- n 1)))))"
     "(sh-meta-f 5)") :to-be 120))

(it-sequential "selfhost-meta-circular-closure"
  (expect (run-string "(let ((x 10)) (funcall (lambda (y) (+ x y)) 5))") :to-be 15))

;;; ─── Self-Hosting: True Meta-Circular (VM calls run-string) ─────────────
;;; These tests prove that code running in cl-cc's VM can invoke cl-cc's own
;;; compiler (run-string) via the host function bridge. This is the key
;;; demonstration of meta-circular compilation: the compiler compiles code
;;; that invokes the compiler.

(it-sequential "selfhost-meta-circular-compilation"
  (expect (run-string "(run-string \"(+ 21 21)\")") :to-be 42)
  (expect (run-string
     "(run-string \"(defun sh-meta-fact (n) (if (<= n 1) 1 (* n (sh-meta-fact (- n 1))))) (sh-meta-fact 5)\")") :to-be 120)
  (expect (run-string
     "(run-string \"(let ((x 10)) (funcall (lambda (y) (+ x y)) 5))\")") :to-be 15))

;;; ─── Self-Hosting: Load File + Use Definitions ──────────────────────────

(it-sequential "selfhost-load-and-use-defs"
  (%with-tmpfile (tmpfile "sh-defs"
      "(defvar *sh-greeting* \"hello\")
(defun sh-greet (name) (list *sh-greeting* name))")
    (expect (run-load-and-eval tmpfile "(sh-greet \"world\")") :to-equal '("hello" "world"))))


(it-sequential "selfhost-load-and-use-recursion"
  (%with-tmpfile (tmpfile "sh-rec"
      "(defun sh-fib (n)
  (if (<= n 1) n
      (+ (sh-fib (- n 1)) (sh-fib (- n 2)))))
(defun sh-ack (m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (sh-ack (- m 1) 1))
        (t (sh-ack (- m 1) (sh-ack m (- n 1))))))")
    (expect (run-load-and-eval tmpfile "(sh-fib 7)") :to-be 13)
    (expect (run-load-and-eval tmpfile "(sh-ack 2 2)") :to-be 7)))


(it-sequential "selfhost-load-chain"
  (%with-tmpfile (file1 "sh-chain1"
      "(defvar *sh-base* 1000)
(defun sh-offset (n) (+ *sh-base* n))")
    (%with-tmpfile (file2 "sh-chain2"
        "(defun sh-combined (a b)
  (+ (sh-offset a) (sh-offset b)))")
      (cl-cc:with-fresh-repl-state
        (cl-cc::our-load file1)
        (cl-cc::our-load file2)
        (expect (run-string-repl "(sh-combined 1 2)") :to-be 2003)))))

(it-sequential "selfhost-load-defun-implicit-block-return-from"
  (%with-tmpfile (tmpfile "sh-return-from"
      "(defun sh-return-from (x)
  (return-from sh-return-from (+ x 1))
  0)")
    (expect (run-load-and-eval tmpfile "(sh-return-from 41)") :to-be 42)))

;;; ─── Self-Hosting: Source File Loading ────────────────────────────────────


(it-sequential "selfhost-load-own-source"
  (cl-cc:with-fresh-repl-state
    (dolist (f *selfhost-representative-files*)
      (cl-cc::our-load f))))


;;; ─── Phase 1: Macro Eval Through Own VM ───────────────────────────────────

(it-sequential "selfhost-macro-eval-fn-basic-runtime"
  (expect (functionp cl-cc:*macro-eval-fn*) :to-be-truthy)
  (expect (funcall cl-cc:*macro-eval-fn* '(+ 1 2)) :to-be 3))


;;; ─── Phase 4: All Source Files Self-Load ──────────────────────────────────

(it-sequential "selfhost-all-source-files-smoke"
  (handler-bind ((warning #'muffle-warning))
    (let* ((files (selfhost-all-source-files))
           (ok 0))
      (%with-selfhost-features
        (cl-cc:with-fresh-repl-state
          (let ((cl-cc:*skip-optimizer-passes* t))
            (dolist (f files)
              (cl-cc::our-load f)
              (incf ok)))))
      (expect ok :to-be (length files)))))


(set-suite-test-timeout! 'selfhost-suite 30)
