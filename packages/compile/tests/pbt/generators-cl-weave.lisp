;;;; tests/pbt/generators-cl-weave.lisp - Shared domain generators on cl-weave
;;;
;;; Symbol generators shared by the AST and macro property tests, built from
;;; cl-weave 1.0.0's native combinators. These replace the home-grown
;;; cl-cc/pbt (gen-symbol :prefix P :package ...) for the two cases the test
;;; suite actually needs: a fresh uninterned symbol, and a keyword.
;;;
;;; cl-weave ships GEN-SYMBOL, but it draws from a fixed :NAMES list
;;; ('("x" "y" "value" "state")), which collides far too often for tests that
;;; compare bindings by EQ. Generating the name from GEN-INTEGER instead keeps
;;; the values distinct while staying a pure function of cl-weave's seeded
;;; RNG — so CL_WEAVE_PROPERTY_SEED reproduces a failing case exactly. The
;;; home-grown version called GENSYM, which leaked the global gensym counter
;;; into generation and made replay impossible.

(in-package :cl-cc/pbt)

(defun gen-pbt-symbol (prefix)
  "Generate a fresh uninterned symbol whose name is PREFIX plus a suffix."
  (cl-weave:gen-map (lambda (n) (make-symbol (format nil "~A-~D" prefix n)))
                    (cl-weave:gen-integer :min 0 :max 99999)
                    :name :pbt-symbol))

(defun gen-pbt-keyword (prefix)
  "Generate a keyword whose name is PREFIX plus a suffix."
  (cl-weave:gen-map (lambda (n) (intern (format nil "~A-~D" prefix n) :keyword))
                    (cl-weave:gen-integer :min 0 :max 99999)
                    :name :pbt-keyword))

(defun gen-pbt-single-float (&key (min -1000.0) (max 1000.0))
  "Generate a SINGLE-FLOAT in [MIN, MAX] to two decimal places.
cl-weave ships no float generator, so this scales GEN-INTEGER. Two decimal
places is ample for the typed-AST properties, which only inspect the node's
declared type, and keeping the draw integral means shrinking still works."
  (let ((scale 100))
    (cl-weave:gen-map
     (lambda (n) (float (/ n scale) 1.0f0))
     (cl-weave:gen-integer :min (round (* min scale)) :max (round (* max scale)))
     :name :pbt-single-float)))
