; Add only ANSI-FAILED-TEST names from runner.log for a matching chapter/mode
; whose run passes the integrity checks described in expected-counts.sexp.
; Row shape: (:chapter "..." :mode "..." :test "..." :category "..."
;             :reason "...")
; The checker rejects observed failures missing here, plus listed tests that pass
; or did not run. It matches by chapter, mode, and test name; categories are
; validated and counted.
; :reason explains the category to reviewers. The checker requires a nonempty
; string but does not print it.
; The checker prints counts as expected-category-<category>=<count>.
; Categories: 未実装 (in-scope feature absent), 不具合 (implemented behavior wrong),
; implementation-defined (ANSI permits implementation choices), 意図的な拡張 (intentional deviation),
; 対象外 (an unimplemented feature excluded from this conformance scope).
()
