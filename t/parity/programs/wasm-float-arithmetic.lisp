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
                                (cl-cc/vm:make-vm-float-sub :dst
                                                            :r5
                                                            :lhs
                                                            :r0
                                                            :rhs
                                                            :r1)
                                (cl-cc/vm:make-vm-const :dst :r6 :value -0.5d0)
                                (cl-cc/vm:make-vm-eq :dst :r7 :lhs :r5 :rhs :r6)
                                (cl-cc/vm:make-vm-float-mul :dst
                                                            :r8
                                                            :lhs
                                                            :r0
                                                            :rhs
                                                            :r1)
                                (cl-cc/vm:make-vm-const :dst :r9 :value 3.0d0)
                                (cl-cc/vm:make-vm-eq :dst
                                                     :r10
                                                     :lhs
                                                     :r8
                                                     :rhs
                                                     :r9)
                                (cl-cc/vm:make-vm-float-div :dst
                                                            :r11
                                                            :lhs
                                                            :r0
                                                            :rhs
                                                            :r1)
                                (cl-cc/vm:make-vm-const :dst :r12 :value 0.75d0)
                                (cl-cc/vm:make-vm-eq :dst
                                                     :r13
                                                     :lhs
                                                     :r11
                                                     :rhs
                                                     :r12)
                                (cl-cc/vm:make-vm-mul :dst
                                                      :r14
                                                      :lhs
                                                      :r4
                                                      :rhs
                                                      :r7)
                                (cl-cc/vm:make-vm-mul :dst
                                                      :r15
                                                      :lhs
                                                      :r10
                                                      :rhs
                                                      :r13)
                                (cl-cc/vm:make-vm-mul :dst
                                                      :r16
                                                      :lhs
                                                      :r14
                                                      :rhs
                                                      :r15)
                                (cl-cc/vm:make-vm-halt :reg :r16))
                          :result-register
                          :r16)
