;;;; packages/javascript/src/runtime-method-resolver-core.lisp — JS method resolver helpers

(in-package :cl-cc/javascript)

(defun %js-string-char-iter (s)
  "Return a JS Iterator that yields each character of string S as a one-char string."
  (let ((i 0))
    (%js-make-cl-iterator
     (lambda ()
       (if (>= i (length s))
           :done
           (let ((ch (string (char s i))))
             (incf i)
             (cons ch nil)))))))

(defun %js-strip-trailing-dot (s)
  "Drop the trailing '.' that CL's ~,0F format directive adds."
  (if (and (plusp (length s)) (char= (char s (1- (length s))) #\.))
      (subseq s 0 (1- (length s)))
      s))

(defun %js-strip-pre-exp-dot (s)
  "Remove a lone '.' immediately before the exponent marker."
  (let ((pos (or (search ".e" s) (search ".E" s))))
    (if pos
        (concatenate 'string (subseq s 0 pos) (subseq s (1+ pos)))
        s)))

(defun %js-number-to-precision (x p)
  "JS Number.prototype.toPrecision(P): render X to P significant digits."
  (cond
    ((or (< p 1) (> p 100)) (format nil "~A" x))
    ((zerop x)
     (if (= p 1) "0" (format nil "0.~A" (make-string (1- p) :initial-element #\0))))
    (t
     (let* ((neg (minusp x))
            (ax  (abs (coerce x 'double-float)))
            (e   (floor (log ax 10d0))))
       (loop while (>= (/ ax (expt 10d0 e)) 10d0) do (incf e))
       (loop while (<  (/ ax (expt 10d0 e)) 1d0)  do (decf e))
       (let ((body (if (or (< e -6) (>= e p))
                       (%js-strip-pre-exp-dot (format nil "~,v,,,,,'eE" (1- p) ax))
                       (%js-strip-trailing-dot (format nil "~,vF" (max 0 (- p 1 e)) ax)))))
         (if neg (concatenate 'string "-" body) body))))))

(defun %js-bound-method (table receiver name)
  "Look NAME up in TABLE; return a closure or +js-undefined+."
  (let ((entry (assoc name table :test #'string=)))
    (if entry
        (let ((fn (cdr entry)))
          (lambda (&rest args) (apply fn receiver args)))
        +js-undefined+)))

(defmacro define-js-type-resolver (name method-table &rest special-props)
  "Generate a resolver defun for a JS type."
  `(defun ,name (obj key)
     ,(if special-props
          `(cond
             ,@(loop for (k v) on special-props by #'cddr
                     collect `((string= key ,k) ,v))
             (t (%js-bound-method ,method-table obj key)))
          `(%js-bound-method ,method-table obj key))))
