((:check :stage2-fixpoint
  :id :stage2-output-unavailable
  :classification :unimplemented
  :reason "compile-file-to-native produces a host-native executable via %compile-native-file-source; it cannot provide the stage1 VM artifact or raw VM bytes for stage1-to-stage2 comparison, and no selfhost stage builder or serializer is available."))
