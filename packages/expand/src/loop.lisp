(in-package :cl-cc/expand)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; LOOP — Generator Layer
;;;
;;; This file is intentionally short.  All grammar, parsing, and code
;;; generation are in the three preceding files:
;;;   loop-data.lisp     — grammar tables (the "Prolog database")
;;;   loop-parser.lisp   — CPS token parser → IR plist
;;;   loop-emitters.lisp — IR → code-fragment tables
;;;
;;; This file's only job is to assemble those fragments into a tagbody.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Return value assembly ────────────────────────────────────────────────

(defun %loop-build-return-forms (result-form acc-vars conditions)
  "Determine the loop's implicit return value.  Returns a list of 0 or 1 forms.

RESULT-FORM  — list of collected result expressions (:collect etc.)
ACC-VARS     — list of implicit accumulator gensyms (no INTO clause)
CONDITIONS   — parsed condition specs; consulted for vacuous-truth types

Vacuous-truth types (see *loop-vacuous-truth-conditions*) return T when no
violation was found.  All others return NIL when there is no accumulator."
  (let ((has-vacuous-truth
          (some (lambda (c) (member (car c) *loop-vacuous-truth-conditions*))
                conditions)))
    (cond (result-form
           (list (if (= (length result-form) 1)
                     (car result-form)
                     (cons 'values result-form))))
          (acc-vars
           (list (cons 'values (nreverse acc-vars))))
          (has-vacuous-truth '(t))
          (t nil))))

;;; ── FR-539: loop-finish replacement ─────────────────────────────────────

(defun %loop-replace-finish (forms end-tag)
  "Walk FORMS list and replace (LOOP-FINISH) calls with (GO end-tag)."
  (mapcar (lambda (form) (%loop-subst-finish form end-tag)) forms))

(defun %loop-subst-finish (form end-tag)
  "Replace (LOOP-FINISH) in a single form tree.  Skips QUOTE forms."
  (cond ((atom form) form)
        ((eq (car form) 'quote) form)
        ((and (eq (car form) 'loop-finish) (null (cdr form)))
         `(go ,end-tag))
        (t (mapcar (lambda (sub) (%loop-subst-finish sub end-tag)) form))))

;;; ── The LOOP macro ───────────────────────────────────────────────────────

(our-defmacro iota (n &optional (start 0) (step 1))
  "Return a list of N arithmetic sequence values: START, START+STEP, ..."
  (let ((nv (gensym "N")) (sv (gensym "START")) (stepv (gensym "STEP"))
        (i (gensym "I")) (acc (gensym "ACC")))
    `(let ((,nv ,n) (,sv ,start) (,stepv ,step) (,acc nil))
       (dotimes (,i ,nv (nreverse ,acc))
         (setq ,acc (cons (+ ,sv (* ,i ,stepv)) ,acc))))))

(defun %loop-iota-pattern (clauses)
  "Return an IOTA expansion for `(loop for i from 0 below n collect i)`, or NIL."
  (when (and (= (length clauses) 8)
             (loop-kw-p (first clauses) "FOR")
             (symbolp (second clauses))
             (loop-kw-p (third clauses) "FROM")
             (eql (fourth clauses) 0)
             (loop-kw-p (fifth clauses) "BELOW")
             (loop-kw-p (seventh clauses) "COLLECT")
             (eq (second clauses) (eighth clauses)))
    `(iota ,(sixth clauses))))

(defun %loop-nested-register-note (iterations)
  "Return a declaration hint for nested arithmetic loops.

Lexical LET*/TAGBODY lowering already exposes inner iteration variables to native
register allocation. This declaration documents the intent and lets CL hosts avoid
spurious dynamic-extent assumptions while keeping semantics unchanged."
  (when (> (count :from iterations :key (lambda (it) (getf it :type))) 1)
    '(declare (optimize (speed 3)))))

(our-defmacro loop (&rest clauses)
  "LOOP macro with full ANSI iteration, accumulation, filtering, and control.

Iteration:    FOR var FROM n [TO/BELOW/UPTO m] [BY k]
              FOR var IN list [BY fn]
              FOR var ON list [BY fn]
              FOR var ACROSS vector
              FOR var = expr [THEN step]
              FOR var BEING THE HASH-KEYS/HASH-VALUES OF table [USING (...)]
              WITH var [= expr]
              REPEAT n
Accumulation: COLLECT/SUM/COUNT/MAXIMIZE/MINIMIZE/APPEND/NCONC expr [INTO var]
              (and -ING synonyms for each)
Filtering:    WHEN/IF/UNLESS test
Control:      WHILE/UNTIL test   ALWAYS/NEVER/THEREIS test
Body:         DO forms...   INITIALLY forms...   FINALLY forms..."
  (or (%loop-iota-pattern clauses)
      (let* ((ir           (parse-loop-clauses clauses))
         (iterations   (getf ir :iterations))
         ;; DO forms in original clause order, kept in their OWN variable
         ;; (rather than seeding the shared push+nreverse `body` accumulator
         ;; below) — see the ④ assembly comment for why mixing them in there
         ;; is unsound.
         (do-forms     (nreverse (getf ir :body)))
         (accumulations (getf ir :accumulations))
         (conditions   (getf ir :conditions))
         (initially    (getf ir :initially))
         (finally      (getf ir :finally))
         (loop-name    (getf ir :loop-name)))   ; FR-638: named loop
    (let ((start-tag   (gensym "START"))
          (end-tag     (gensym "END"))
          (bindings    nil)
          (step-forms  nil)
          (end-tests   nil)
          (pre-body    nil)
          (acc-vars    nil)
          (result-form nil)
          (condition-forms nil)   ; WHILE/UNTIL/ALWAYS/… checks, own accumulator
          (body        nil))      ; accumulation (COLLECT/SUM/…) forms only

      ;; ① Emit iteration specs — uniform dispatch through *loop-iter-emitters*.
      ;;    :repeat and :with are registered in the same table as FOR variants.
      (dolist (iter iterations)
        (let ((emitter (gethash (getf iter :type) *loop-iter-emitters*)))
          (unless emitter
            (error "Unknown loop iteration type: ~S" (getf iter :type)))
          (multiple-value-bind (new-binds new-ends new-pre new-steps)
              (funcall emitter (getf iter :for) iter)
            (setf bindings   (nconc new-binds  bindings))
            (setf end-tests  (nconc new-ends   end-tests))
            (setf pre-body   (nconc new-pre    pre-body))
            (setf step-forms (nconc new-steps  step-forms)))))

      ;; ② Emit WHILE/UNTIL/ALWAYS/NEVER/THEREIS checks into CONDITION-FORMS,
      ;;    a dedicated accumulator (NOT the shared `body` list used by DO/
      ;;    accumulation forms below).  `body` was seeded with the already-
      ;;    reversed DO-forms until FR/regression review found that pushing
      ;;    condition checks onto that same list and nreversing once at the
      ;;    end put the checks AFTER the DO-forms instead of before them —
      ;;    i.e. `(loop while X do FORM)` silently became a do-while loop
      ;;    that always ran FORM at least once even when X was initially
      ;;    false.  Keeping condition checks in their own list and splicing
      ;;    them in first at assembly time (④) guarantees ANSI's "test before
      ;;    body" ordering regardless of push/reverse bookkeeping elsewhere.
      (dolist (cond conditions)
        (let* ((type    (car cond))
               (form    (cadr cond))
               (emitter (gethash type *loop-condition-emitters*)))
          (unless emitter
            (error "Unknown loop condition type: ~S" type))
          (push (funcall emitter form end-tag) condition-forms)))

      ;; ③ Emit accumulation specs — fully table-driven via *loop-acc-emitters*.
      (dolist (acc accumulations)
        (let* ((acc-type (car acc))
               (acc-form (cadr acc))
               (into-var (caddr acc))
               (filter   (cadddr acc))
               (acc-var  (or into-var (gensym "ACC")))
               (emitter  (gethash acc-type *loop-acc-emitters*)))
          (unless emitter
            (error "Unknown loop accumulation type: ~S" acc-type))
          (unless into-var (push acc-var acc-vars))
          (multiple-value-bind (acc-body new-bindings new-result)
              (funcall emitter acc-var acc-form bindings result-form into-var)
            (setf bindings    new-bindings)
            (when new-result  (setf result-form new-result))
            (push (%loop-wrap-filter acc-body filter) body))))

      ;; ④ Assemble tagbody.
      ;;    :initially/:finally — already forward-ordered by finalize-loop-state.
      ;;    bindings/end-tests/pre-body/step-forms — push-accumulated; nreverse corrects.
      ;;    condition-forms/body — push-accumulated in ②③; nreverse corrects each.
      ;;    Per-iteration order is CONDITIONS, then DO-forms, then accumulations
      ;;    (ANSI requires WHILE/UNTIL to test before any DO/accumulation form).
      ;; FR-539: replace (loop-finish) with (go end-tag) in body and finally forms.
      (let ((assembled-body    (append (%loop-replace-finish (nreverse condition-forms) end-tag)
                                        (%loop-replace-finish do-forms end-tag)
                                        (%loop-replace-finish (nreverse body) end-tag)))
            (assembled-finally (%loop-replace-finish finally end-tag)))
        `(block ,(or loop-name nil)  ; FR-638: named loop
           (let* ,(nreverse bindings)
              ,@(let ((hint (%loop-nested-register-note iterations)))
                  (when hint (list hint)))
              ,@initially
             (tagbody
                ,start-tag
                ,@(mapcar (lambda (test) `(when ,test (go ,end-tag))) (nreverse end-tests))
                ,@(nreverse pre-body)
                ,@assembled-body
                ,@(nreverse step-forms)
                (go ,start-tag)
                ,end-tag)
             ,@assembled-finally
              ,@(%loop-build-return-forms result-form acc-vars conditions))))))))
