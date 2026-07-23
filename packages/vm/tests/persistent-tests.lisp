(in-package :cl-cc/test)



(defun assert-pmap-entry (map key expected &key (test #'equal))
  (multiple-value-bind (value found-p) (cl-cc/vm:pmap-get map key)
    (expect found-p :to-be-truthy)
    (expect (funcall test expected value) :to-be-truthy)))

(it-sequential "persistent-map-assoc-get-and-dissoc"
  (let* ((m0 (cl-cc/vm:persistent-map))
         (m1 (cl-cc/vm:pmap-assoc m0 'a 1))
         (m2 (cl-cc/vm:pmap-assoc m1 'b 2))
         (m3 (cl-cc/vm:pmap-assoc m2 'a 10))
         (m4 (cl-cc/vm:dissoc m3 'b)))
    (multiple-value-bind (value found-p) (cl-cc/vm:pmap-get m0 'a :missing)
      (expect value :to-equal :missing)
      (expect found-p :to-be-falsy))
    (multiple-value-bind (value found-p) (cl-cc/vm:pmap-get m1 'a)
      (expect value :to-equal 1)
      (expect found-p :to-be-truthy))
    (expect (cl-cc/vm:persistent-map-count m2) :to-equal 2)
    (expect (cl-cc/vm:persistent-map-count m3) :to-equal 2)
    (expect (cl-cc/vm:pmap-get m1 'a) :to-equal 1)
    (expect (cl-cc/vm:pmap-get m3 'a) :to-equal 10)
    (multiple-value-bind (value found-p) (cl-cc/vm:pmap-get m4 'b :missing)
      (expect value :to-equal :missing)
      (expect found-p :to-be-falsy))
    (expect (cl-cc/vm:persistent-map-count m4) :to-equal 1)
    (assert-pmap-entry m2 'b 2)))

(it-sequential "persistent-map-supports-equal-keys-and-bitmap-branching"
  (let ((map (cl-cc/vm:persistent-map :test 'equal)))
    (dotimes (i 80)
      (setf map (cl-cc/vm:pmap-assoc map (format nil "k-~d" i) (* i 2))))
    (expect (cl-cc/vm:persistent-map-count map) :to-equal 80)
    (expect (cl-cc/vm:pmap-get map (copy-seq "k-21")) :to-equal 42)
    (let ((smaller (cl-cc/vm:dissoc map (copy-seq "k-21"))))
      (expect (cl-cc/vm:persistent-map-count map) :to-equal 80)
      (expect (cl-cc/vm:persistent-map-count smaller) :to-equal 79)
      (assert-pmap-entry map "k-21" 42)
      (multiple-value-bind (value found-p) (cl-cc/vm:pmap-get smaller "k-21" :missing)
        (expect value :to-equal :missing)
        (expect found-p :to-be-falsy)))))

(it-sequential "persistent-map-api-rejects-host-collection-fallbacks"
  (signals error (cl-cc/vm:persistent-map :a 1 :b))
  (signals error (cl-cc/vm:pmap-assoc 'a '((a . 1)) 0))
  (signals error (cl-cc/vm:pmap-get '(a 1) 'a))
  (signals error (cl-cc/vm:dissoc '(a 1) 'a)))

(it-sequential "persistent-vector-get-assoc-conj"
  (let* ((v0 (cl-cc/vm:pvec 1 2 3))
         (v1 (cl-cc/vm:pvec-assoc v0 1 20))
         (v2 (cl-cc/vm:pvec-conj v1 40)))
    (expect (cl-cc/vm:pvec-count v0) :to-equal 3)
    (expect (cl-cc/vm:pvec-get v0 1) :to-equal 2)
    (expect (cl-cc/vm:pvec-get v1 1) :to-equal 20)
    (expect (cl-cc/vm:pvec-count v1) :to-equal 3)
    (expect (cl-cc/vm:pvec-count v2) :to-equal 4)
    (expect (cl-cc/vm:pvec-get v2 3) :to-equal 40)
    (expect (cl-cc/vm:pvec-get v2 99 :missing) :to-equal :missing)))

(it-sequential "transient-map-assoc-dissoc-and-persistent-freeze"
  (let* ((m0 (cl-cc/vm:persistent-map :test 'equal "a" 1 "b" 2))
         (tm (cl-cc/vm:transient m0)))
    (expect (cl-cc/vm:assoc! tm (copy-seq "a") 10) :to-be tm)
    (expect (cl-cc/vm:dissoc! tm "b") :to-be tm)
    (let ((m1 (cl-cc/vm:persistent! tm)))
      (expect (cl-cc/vm:persistent-map-count m0) :to-equal 2)
      (expect (cl-cc/vm:persistent-map-count m1) :to-equal 1)
      (expect (cl-cc/vm:pmap-get m1 "a") :to-equal 10)
      (multiple-value-bind (value found-p) (cl-cc/vm:pmap-get m1 "b" :missing)
        (expect value :to-equal :missing)
        (expect found-p :to-be-falsy)))))

(it-sequential "transient-vector-conj-and-persistent-freeze"
  (let* ((v0 (cl-cc/vm:pvec 1 2))
         (tv (cl-cc/vm:transient! v0)))
    (expect (cl-cc/vm:conj! tv 3) :to-be tv)
    (expect (cl-cc/vm:conj! tv 4) :to-be tv)
    (let ((v1 (cl-cc/vm:persistent! tv)))
      (expect (cl-cc/vm:pvec-count v0) :to-equal 2)
      (expect (cl-cc/vm:pvec-count v1) :to-equal 4)
      (expect (cl-cc/vm:pvec-get v1 3) :to-equal 4))))

(it-sequential "lazy-seq-force-take-map-filter-iterate"
  (let ((forced 0))
    (let ((seq (cl-cc/vm:lazy-seq
                 (incf forced)
                 (cons 1 (cl-cc/vm:lazy-seq
                           (incf forced)
                           (cons 2 nil))))))
    (expect forced :to-equal 0)
    (expect (cl-cc/vm:lazy-take-seq 1 seq) :to-equal '(1))
    (expect forced :to-equal 1)
    (expect (cl-cc/vm:lazy-take-seq 2 seq) :to-equal '(1 2))
    (expect forced :to-equal 2)
    (expect (cl-cc/vm:lazy-take-seq 2 seq) :to-equal '(1 2))
      (expect forced :to-equal 2)))
  (let* ((naturals (cl-cc/vm:iterate #'1+ 0))
         (evens (cl-cc/vm:lazy-filter #'evenp naturals))
         (doubled (cl-cc/vm:lazy-map (lambda (x) (* x 2)) evens)))
    (expect (cl-cc/vm:lazy-take-seq 5 doubled) :to-equal '(0 4 8 12 16))))

(it-sequential "lazy-seq-preserves-nil-elements-and-descending-ranges"
  (let ((seq (cl-cc/vm:lazy-seq
               (cons nil
                     (cl-cc/vm:lazy-seq
                       (cons :tail nil))))))
    (expect (cl-cc/vm:lazy-take-seq 2 seq) :to-equal '(nil :tail)))
  (expect (cl-cc/vm:lazy-take-seq
                 5
                 (cl-cc/vm:lazy-range :start 3 :end 0 :step -1)) :to-equal '(3 2 1))
  (signals error (cl-cc/vm:lazy-range :step 0)))
