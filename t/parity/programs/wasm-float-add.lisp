(cl-cc/vm:make-vm-program :instructions
                          (list (cl-cc/vm:make-vm-const :dst :r0 :value 1.5d0)
                                (cl-cc/vm:make-vm-const :dst :r1 :value 2.0d0)
                                (cl-cc/vm:make-vm-float-add :dst
                                                            :r2
                                                            :lhs
                                                            :r0
                                                            :rhs
                                                            :r1)
                                (cl-cc/vm:make-vm-const :dst :r3 :value 3.5d0)
                                (cl-cc/vm:make-vm-eq :dst :r4 :lhs :r2 :rhs :r3)
                                (cl-cc/vm:make-vm-halt :reg :r4))
                          :result-register
                          :r4)
