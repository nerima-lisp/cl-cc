;;;; packages/optimize/tests/native-advanced-optimizer-tests.lisp

(in-package :cl-cc/test)



(it-sequential "fr-662-path-profiling-and-block-versioning-plan"
  (let* ((instructions (list (cl-cc:make-vm-label :name "entry")
                             (cl-cc:make-vm-const :dst :C :value 1)
                             (cl-cc:make-vm-jump-zero :reg :C :label "taken")
                             (cl-cc:make-vm-const :dst :R0 :value 11)
                             (cl-cc:make-vm-halt :reg :R0)
                             (cl-cc:make-vm-label :name "taken")
                             (cl-cc:make-vm-const :dst :R0 :value 22)
                             (cl-cc:make-vm-halt :reg :R0)))
         (profile (cl-cc/optimize:opt-build-ball-larus-profile instructions :function-id :test-fn))
         (counts (make-hash-table :test #'equal)))
    (setf (gethash (list :test-fn 0) counts) 9)
    (let ((plan (cl-cc/optimize:opt-build-block-version-plan profile
                                                             :counts counts
                                                             :hot-threshold 5)))
      (expect (cl-cc/optimize:opt-ball-larus-profile-edges profile) :to-be-truthy)
      (expect (find 0 (cl-cc/optimize:opt-ball-larus-profile-paths profile)
                         :key (lambda (path) (getf path :sum))) :to-be-truthy)
      (expect (cl-cc/optimize:opt-block-version-plan-versions plan) :to-be-truthy))))

(it-sequential "fr-662-path-profiling-pass-instruments-edges-and-exits"
  (let* ((instructions (list (cl-cc:make-vm-label :name "entry")
                              (cl-cc:make-vm-const :dst :R0 :value 42)
                              (cl-cc:make-vm-halt :reg :R0)))
          (profile (cl-cc/optimize:opt-compute-path-profile instructions))
          (after-pass (cl-cc/optimize:opt-pass-path-profiling instructions)))
    (expect (> (length after-pass) (length instructions)) :to-be-truthy)
    (expect (some #'cl-cc/vm:vm-add-p after-pass) :to-be-truthy)
    (expect (some #'cl-cc/optimize::opt-vm-path-profile-record-p after-pass) :to-be-truthy)
    (expect profile :to-be-truthy)
    (expect (plusp (cl-cc/optimize:opt-path-profile-block-path-id (first profile))) :to-be-truthy)
    (expect (integerp (cl-cc/optimize:opt-path-profile-block-successor-count
                            (first profile))) :to-be-truthy)))

(it-sequential "fr-662-hot-path-superblock-clones-blocks"
  (let* ((instructions (list (cl-cc:make-vm-label :name "entry")
                             (cl-cc:make-vm-const :dst :R0 :value 42)
                             (cl-cc:make-vm-halt :reg :R0)))
         (counts (make-hash-table :test #'equal)))
    (setf (gethash (list :test-fn 0) counts) 10)
    (let ((versioned (cl-cc/optimize:opt-duplicate-hot-paths
                      instructions counts :function-id :test-fn :hot-threshold 5)))
      (expect (some (lambda (inst)
                           (and (cl-cc/vm:vm-label-p inst)
                                (search "BL_SUPER_0" (cl-cc/vm::vm-name inst))))
                         versioned) :to-be-truthy))))

(it-sequential "fr-582-autotune-simd-produces-valid-tile-sizes-and-retunes-lanes"
  (let* ((cache-info '(:l1 32768 :l2 262144 :l3 8388608 :source :test))
         (simd (cl-cc:make-vm-simd-vector-op :op :+
                                             :dst-array :D
                                             :lhs-array :A
                                             :rhs-array :B
                                             :index-reg :I
                                             :lanes 4
                                             :element-type :i32))
         (cl-cc/optimize:*autotune-simd-enabled* t)
         (cl-cc/optimize::*autotune-simd-cache-info* cache-info))
    (multiple-value-bind (l1 l2 l3)
        (cl-cc/optimize:autotune-simd-tile-sizes cache-info)
      (expect (member l1 '(16 32)) :to-be-truthy)
      (expect (member l2 '(128 256)) :to-be-truthy)
      (expect (member l3 '(256 512)) :to-be-truthy)
      (let ((retuned (first (cl-cc/optimize:opt-pass-autotune-simd (list simd)))))
        (expect (= l1 (cl-cc/vm:vm-simd-vector-op-lanes retuned)) :to-be-truthy)))))

(it-sequential "fr-703-compiler-self-profiling-build-analytics"
  (let ((summary (cl-cc/optimize:build-analytics-summary
                  :pass-count 3 :instruction-count 9 :elapsed-us 42 :changed-count 1)))
    (expect (getf summary :fr-id) :to-equal :fr-703)
    (expect (getf summary :pass-count) :to-equal 3)
    (expect (getf (getf summary :capabilities) :build-analytics) :to-be-truthy)))

(it-sequential "fr-723-load-widening-replaces-adjacent-byte-loads"
  (let* ((first-load (cl-cc:make-vm-aref :dst :r1 :array-reg :a :index-reg 0))
         (second-load (cl-cc:make-vm-aref :dst :r2 :array-reg :a :index-reg 1))
         (result (cl-cc/optimize:opt-pass-load-widening-store-coalescing
                  (list first-load second-load))))
    (expect (= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-aref)) result)) :to-be-truthy)
    (expect (= 2 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-logand)) result)) :to-be-truthy)
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-logand)
                              (eq :r1 (cl-cc/vm::vm-dst i))))
                       result) :to-be-truthy)
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-logand)
                              (eq :r2 (cl-cc/vm::vm-dst i))))
                       result) :to-be-truthy)))

(it-sequential "fr-723-store-coalescing-packs-adjacent-byte-stores"
  (let* ((first-store (cl-cc:make-vm-aset :array-reg :a :index-reg 0 :val-reg :v1))
         (second-store (cl-cc:make-vm-aset :array-reg :a :index-reg 1 :val-reg :v2))
         (result (cl-cc/optimize:opt-pass-load-widening-store-coalescing
                  (list first-store second-store))))
    (expect (= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-aset)) result)) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-logior)) result) :to-be-truthy)
    (let ((store (find-if (lambda (i) (typep i 'cl-cc/vm::vm-aset)) result)))
      (expect (cl-cc/vm::vm-array-reg store) :to-be :a)
      (expect (= 0 (cl-cc/vm::vm-index-reg store)) :to-be-truthy))))

(it-sequential "fr-723-load-store-coalescing-respects-natural-alignment"
  (let* ((first-load (cl-cc:make-vm-aref :dst :r1 :array-reg :a :index-reg 1))
         (second-load (cl-cc:make-vm-aref :dst :r2 :array-reg :a :index-reg 2))
         (result (cl-cc/optimize:opt-pass-load-widening-store-coalescing
                  (list first-load second-load))))
    (expect result :to-equal (list first-load second-load))))
