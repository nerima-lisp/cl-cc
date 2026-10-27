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

(defmethod cl-cc/vm:vm-sequence-elt ((sequence test-sequence) index)
  (aref (test-sequence-payload sequence) index))

(defmethod cl-cc/vm:vm-sequence-length ((sequence test-sequence))
  (length (test-sequence-payload sequence)))

(defmethod cl-cc/vm:vm-make-sequence-like ((sequence test-sequence) size &key (initial-element nil))
  (declare (ignore sequence))
  (make-instance 'test-sequence :payload (make-array size :initial-element initial-element)))

(defclass vm-protocol-backed-sequence (cl-cc/vm::sequence)
  ((payload :initarg :payload :accessor vm-protocol-backed-sequence-payload)))

(defmethod cl-cc/vm::length ((sequence vm-protocol-backed-sequence))
  (length (vm-protocol-backed-sequence-payload sequence)))

(defmethod cl-cc/vm::elt ((sequence vm-protocol-backed-sequence) index)
  (aref (vm-protocol-backed-sequence-payload sequence) index))

(defmethod (setf cl-cc/vm::elt) (value (sequence vm-protocol-backed-sequence) index)
  (setf (aref (vm-protocol-backed-sequence-payload sequence) index) value))

(defmethod cl-cc/vm::make-sequence-like ((sequence vm-protocol-backed-sequence)
                                          size
                                          &key
                                          initial-element)
  (declare (ignore sequence))
  (make-instance 'vm-protocol-backed-sequence
                 :payload (make-array size :initial-element initial-element)))

