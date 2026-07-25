# testing-framework → cl-weave 完全移行 計画

> ステータス: 計画(native 完全書き換え／段階実行）
> 目標: 独自 `packages/testing-framework` を廃止し、全テストを cl-weave の
> native API（`it-sequential` / `expect`）へ書き換える。

## 1. 現状（実測）

- **7176 の `deftest` / `deftest-each`**、**523 テストファイル**（packages/\*/tests, tests/）。
- `deftest`/`defsuite`/`assert-*` は **既に cl-weave-backed**（framework が cl-weave
  のスイートに登録する薄いアダプタ）。`cl-weave:run-all` で全テストが走る。
- framework 独自機能の利用は少数: `assert-run`(27) `assert-run-string`(5)
  `assert-compiles-to`(4) `assert-pbt`(2) `assert-faster`(2) `assert-no-consing`(1)。
- **php 逆依存**: `cl-cc-testing-framework` が `cl-cc-php` に依存
  （`package-imports-compile-parse.lisp` が `tokenize-php-source` /
  `parse-php-source` をテスト用に import）。← php 抽出のブロッカー。
- 独自の重複機構: `framework-meta`、`*suite-registry*`。
  （`framework-pbt` / `framework-fuzz` は削除済み: 前者は cl-weave の native
  property API、後者は native `it-fuzz` と重複していた。）

## 2. 移行を可能にする鍵

`cl-weave:it-sequential` は **トップレベルで使える**（`describe`/`it` の字句ネスト
不要）。cl-parser-kit の実テストと、本 split で抽出した cl-cc-ast/type/binary/
runtime の cl-weave 化で実証済み。したがって変換は機械的:

```
(deftest name "doc" body...)          → (it-sequential "name" body...)
(deftest-each base "doc"
   :cases ((label v...) ...) (vars) b) → 各 case を (it-sequential "base label"
                                            (destructuring-bind (vars) (list v...) b))
(in-suite X) / (defsuite X ...)        → 削除（flat）または (describe "X" ...) でラップ
```

assert → matcher（抽出repo の shim で確立済み）:

| assert | cl-weave |
|---|---|
| `assert-true x` / `assert-false x` | `(expect x :to-be-truthy)` / `:to-be-falsy` |
| `assert-null x` | `(expect x :to-be-null)` |
| `assert-eq a b` | `(expect b :to-be a)` |
| `assert-= a b` / `assert-equal a b` / `assert-string= a b` | `(expect b :to-equal a)` |
| `assert-type ty v` | `(expect (typep v 'ty) :to-be-truthy)` |
| `assert-signals c body` | `handler-case` + `(expect flag :to-be-truthy)` |
| `assert-type-equal a b` / `assert-unifies` / `assert-not-unifies` | 対応する述語 + `expect` |

## 3. framework 独自 assert の扱い

cl-weave に直接対応が無いものは **薄いヘルパ関数**（cl-weave の上）として
`tests/support/` 等に残す。tests から呼ぶだけで書き換え不要:

- `assert-run` / `assert-run-string`（compile+run して結果比較）→ ヘルパ関数化。
- `assert-compiles-to`（コンパイル結果検査）→ ヘルパ関数化。
- ✅ `assert-pbt`(2) → **cl-weave native property API へ書き換え済み**（重複解消）。
  `compiler-tests-runtime-hof-tests.lisp` の 2 箇所は
  `cl-weave:describe` + `cl-weave:it-property` + `cl-weave:gen-integer` に移行し、
  `framework-pbt` は削除。
- `assert-faster` / `assert-no-consing` → cl-weave `benchmark` / `:to-allocate-under`。

## 4. suite 階層の扱い（設計判断）

現在は `CL-CC-SUITE > CL-CC-UNIT-SUITE > CL-CC-JAVASCRIPT-SUITE > ...` の階層があり、
reporter/filter がこれを使う。flat な `it-sequential` は階層を失う。方針:

- **A（推奨・低コスト）**: flat 化。テスト名にパッケージ prefix を残して識別性を確保。
- **B**: 各ファイルを `(describe "pkg-suite" ...)` でラップし階層維持（字句ネスト構造化
  が要る＝変換が複雑化）。

まず A で移行し、階層が必要と判明したら B に個別昇格。

## 5. 段階実行（葉パッケージ→根、各段で cl-weave:run-all 緑を確認）

```
0. 変換ツール整備（deftest/deftest-each/assert-* → it-sequential/expect）。
   抽出repo の shim ロジックをソース変換に転用。
1. php 依存の切断（php 抽出のブロッカー解消・独立で価値）:
   testing-framework から php helper import を除去し、php parser を使うテストを
   php 側 test-support（or 直接 cl-cc/php: 参照）へ。testing-framework の
   :depends-on から :cl-cc-php を削除。
2. 独自 assert のヘルパ関数化（assert-run/-string/compiles-to）+ pbt の native 化。
3. パッケージ単位で変換（小さい順: cps(7)→debug(8)→bytecode(46)→ir(51)→...→
   vm/optimize/compile の大物）。各パッケージ変換後に該当テストを cl-weave で緑確認。
4. 全パッケージ変換後、packages/testing-framework を削除。cl-cc-test.asd /
   各 -test.asd の deps から :cl-cc-testing-framework を除去、cl-weave へ。
5. nix/asdf-systems.nix の testing-framework 参照を除去。
```

## 6. リスクと検証

- **規模**: 7176 テスト。**変換ツールの正確性が全て**。パッケージ単位で
  「変換前後の cl-weave:run-all の passed 数一致」を不変条件に（抽出repo で確立した
  検証法。ただし静的一致でなく実行時 pass 数一致）。
- **fixtures / before-each**: `defbefore` 等はスイート scoped。flat 化で
  cl-weave の root `before-each` へ（抽出repo の runtime で実証済み手法）。
- **js-e2e 等の検証ループ**: 各段で js-e2e 1117 / 該当パッケージテスト緑を確認
  （dev-path、cl-cc-js-e2e-verification.md）。
- **抽出済み repo（ast/type/binary/runtime）**: 現在 shim 方式。native 化する場合は
  同じ変換を各 repo にも適用（別トラック）。

## 7. 次アクション

- [ ] 段階1（php 依存切断）を先行実行 — php 抽出（task #9）も解錠する。
- [ ] 変換ツールの PoC + パイロット1パッケージ（cps）で pass 数一致を実証。
- [ ] 大物（vm/optimize/compile）は変換後に個別検証。
