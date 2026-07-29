;;;; tests/unit/expand/macros-plist-tests.lisp
;;;; Coverage tests for src/expand/macros-plist.lisp

(in-package :cl-cc/test)



;;; ── GETF ─────────────────────────────────────────────────────────────────────

(it-sequential "getf-expansion"
  (let* ((result     (our-macroexpand-1 '(getf plist :key)))
         (inner-let  (caddr result))
         (found-init (cadar (cadr inner-let)))
         (if-form    (caddr inner-let)))
    (expect (car result) :to-be 'let)
    (expect (car found-init) :to-be 'member)
    (expect (car if-form) :to-be 'if))
  (let* ((result    (our-macroexpand-1 '(getf plist :key :missing)))
         (inner-let (caddr result))
         (if-form   (caddr inner-let)))
    (expect (cadddr if-form) :to-equal :missing)))

;;; ── REMF ─────────────────────────────────────────────────────────────────────

(it-sequential "remf-expansion"
  (let* ((result (our-macroexpand-1 '(remf plist :key)))
         (body   (cddr result)))
    (expect (car result) :to-be 'let)
    (expect (some (lambda (f) (and (consp f) (eq (car f) 'loop))) body) :to-be-truthy)))

;;; ── %PLIST-PUT ───────────────────────────────────────────────────────────────

(it-sequential "plist-put-expansion"
  (let* ((result    (our-macroexpand-1 '(cl-cc/expand::%plist-put my-plist :key 42)))
         (loop-form (caddr result)))
    (expect (car result) :to-be 'let)
    (expect (car loop-form) :to-be 'loop)))
