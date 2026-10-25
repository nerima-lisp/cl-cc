(cl-cc/vm:make-vm-program :instructions
                          (list (cl-cc/vm:make-vm-const :dst :r0 :value 0)
                                (cl-cc/vm:make-vm-const :dst :r1 :value 1)
                                (cl-cc/vm:sexp->instruction '(:atomic-swap :r2 :r0 :r1))
                                (cl-cc/vm:make-vm-const :dst :r3 :value 0)
                                (cl-cc/vm:make-vm-eq :dst :r4 :lhs :r2 :rhs :r3)
                                (cl-cc/vm:make-vm-const :dst :r5 :value 1)
                                (cl-cc/vm:make-vm-eq :dst :r6 :lhs :r0 :rhs :r5)
                                (cl-cc/vm:make-vm-mul :dst
                                                      :r7
                                                      :lhs
                                                      :r4
                                                      :rhs
                                                      :r6)
                                (cl-cc/vm:make-vm-halt :reg :r7))
                          :result-register
                          :r7)
