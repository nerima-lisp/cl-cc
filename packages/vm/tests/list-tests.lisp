;;;; tests/unit/vm/list-tests.lisp — VM List Instruction Tests
;;;
;;; Tests for execute-instruction on list operations, named accessors,
;;; destructive ops, association lists, coercions, arrays, and bit arrays.

(in-package :cl-cc/test)

;;; ─── make-list ──────────────────────────────────────────────────────────────

;;; ─── length / reverse / append ──────────────────────────────────────────────

;;; ─── member / nth / nthcdr ─────────────────────────────────────────────────

;;; ─── Named accessors ───────────────────────────────────────────────────────

;;; ─── Destructive + extended ops ─────────────────────────────────────────────

(defclass test-sequence ()
  ((payload :initarg :payload :accessor test-sequence-payload)))

(defmethod cl-cc/vm/data:vm-sequence-elt ((cl-cc/vm/data:sequence test-sequence) index)
  (aref (test-sequence-payload cl-cc/vm/data:sequence) index))

(defmethod cl-cc/vm/data:vm-sequence-length ((cl-cc/vm/data:sequence test-sequence))
  (cl-cc/vm/data:length (test-sequence-payload cl-cc/vm/data:sequence)))

(defmethod cl-cc/vm/data:vm-make-sequence-like ((cl-cc/vm/data:sequence test-sequence) size &key (initial-element nil))
  (declare (ignore cl-cc/vm/data:sequence))
  (make-instance 'test-sequence :payload (make-array size :initial-element initial-element)))

(defclass vm-protocol-backed-sequence (cl-cc/vm/data:sequence)
  ((payload :initarg :payload :accessor vm-protocol-backed-sequence-payload)))

(defmethod cl-cc/vm/data:length ((cl-cc/vm/data:sequence vm-protocol-backed-sequence))
  (cl-cc/vm/data:length (vm-protocol-backed-sequence-payload cl-cc/vm/data:sequence)))

(defmethod cl-cc/vm/data:elt ((cl-cc/vm/data:sequence vm-protocol-backed-sequence) index)
  (aref (vm-protocol-backed-sequence-payload cl-cc/vm/data:sequence) index))

(defmethod (setf cl-cc/vm/data:elt) (value (cl-cc/vm/data:sequence vm-protocol-backed-sequence) index)
  (setf (aref (vm-protocol-backed-sequence-payload cl-cc/vm/data:sequence) index) value))

(defmethod cl-cc/vm/data:make-sequence-like ((cl-cc/vm/data:sequence vm-protocol-backed-sequence)
                                          size
                                          &key
                                          initial-element)
  (declare (ignore cl-cc/vm/data:sequence))
  (make-instance 'vm-protocol-backed-sequence
                 :payload (make-array size :initial-element initial-element)))
