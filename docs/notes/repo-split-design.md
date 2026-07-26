# リポジトリ分割 全体設計

> ステータス: ドラフト（全体設計のみ／実装は未着手）
> 対象: cl-cc モノレポの「良い単位」でのリポジトリ切り出し
> 前提: 直近で `cl-prolog` `cl-weave` `cl-cli` `cl-tty-kit` `cl-boundary-kit`
> `cl-dataflow` `cl-parser-kit` を外部化した「切り出しの型」を踏襲する。

## 1. 目的と原則

モノレポが約 30 万 loc / 35 パッケージに達し、以下が課題になっている:

- ビルド／テストの一括実行が重い（全スイートが遅い）。
- 責務の異なる領域（言語バックエンド・ネイティブ codegen・汎用ツール）が
  同じリリースサイクルに縛られている。
- 汎用的に再利用できる部品が compiler 本体と同居している。

### 1-0. 基本方針: 「分割」ではなく「剥離」

実測（§2-1）で、cl-cc は「凍った完成 repo をブロックに分割する」対象では
**ない**ことが確認されている:

- **本体は活発に変化中** — 過去 90 日で 578 コミット（1 日 6.4 回）、
  作業ツリーに常時数十ファイルの変更。churn の高い領域を切ると、切った先が
  日々ズレて `flake.lock` 追従コストが恒常化する。
- **核はセルフホストの自己参照ウェブ** — `our-eval` を `:cl-cc/bootstrap` に
  事前 intern して下流（expand/compile/selfhost）が共有し、compiler が自分自身を
  コンパイルする。`cl-cc/vm::` への跨ぎ内部参照は 301 箇所。これは行儀の問題では
  なくセルフホスト compiler の本質であり、リポジトリ境界で割ると壊れ続ける。

したがって本設計は「全体をガンガン分割」ではなく、**一枚岩の核を残したまま、
境界が凍った周縁だけを剥がす“剥離”**である。判断は「repo が完成しているか」では
なく「その**境界**が凍っているか」で行う。

**非目標（Non-goals）**:

- 核（bootstrap / vm / runtime / expand / compile / cps / selfhost）の分割 —
  自己参照・事前 intern のため **分割対象外**。monorepo のまま持つ。
- churn の高い領域の切り出し — 境界が動いている間は切らない。仕様が枯れ、
  逆依存が収束してから剥がす。
- 全パッケージの polyrepo 化 — 目的はビルド/リリースの分離であって、
  リポジトリ数の最大化ではない。

分割の原則（既存 `cl-*` 外部化の判断基準を明文化したもの）:

1. **ファンインが狭いものから切る** — 「誰が依存するか」が少ないほど、
   切っても本体側の変更が小さい。
2. **依存は非循環・単方向** — 切り出し先が本体へ逆流依存しない。
   循環がある場合は先にインタフェースを挿して断つ。
3. **ドメインが一貫している** — 1 リポジトリ = 1 責務。
4. **切り出しに見合うサイズ** — 配管コスト（flake input + Nix 派生 + CI）を
   上回る規模・独立性があること。
5. **配管は既存方式を再利用** — `flake = false` の source tree として取り込み、
   `sbcl.buildASDFSystem` で内部システムと同列にビルド（後述 §7）。
6. **境界が凍っているものだけ切る** — repo 全体の完成度ではなく、対象の
   *境界* の安定性（逆依存の収束・仕様の枯れ）を基準にする。

## 2. 現状の依存グラフ

内部 ASDF システムの依存（`packages/*/*.asd` の `:depends-on` を集約）:

```
Tier0 (依存ゼロ):  bootstrap  ast  binary  runtime  mir  target  ir
                   bytecode  docgen  formatter
Tier1:             vm(bootstrap,runtime)  parse(ast,bootstrap)
                   type(ast)  cps(ast,bootstrap)  stdlib(bootstrap)
Tier2:             optimize(vm,type,ast +cl-prolog,cl-parser-kit)
                   expand(type,vm)  debug(vm)
Tier3:             regalloc(vm,mir,target,optimize)
                   codegen(vm,mir,target,binary,optimize,regalloc)
Tier4:             emit(vm,ast,mir,optimize,codegen)
バックエンド:       php(ast,parse,vm)         逆依存 = {pipeline, testing-framework}
                   javascript(ast,parse)      逆依存 = {pipeline}
Orchestration:     compile → pipeline → selfhost → repl
Tools(葉):         cli  tools  testing-framework  prolog-tools  docgen  formatter
```

**核（切り出せない）**: `bootstrap` と `vm` は全層を貫く。`ast` も広く使われる。
これらは cl-cc 本体に残す前提で、他をこの核の周囲から剥がしていく。

### 2-1. 境界の実測（切れるかの根拠）

「跨ぎの内部シンボル参照（`cl-cc/PKG::symbol`）」＝境界の綺麗さの逆指標。
外部から内部実装に手を突っ込まれている数が少ないほど、そのまま切れる。

跨ぎ内部参照の被参照数（`packages/*/src/*.lisp` 実測）:

```
cl-cc/vm      301   ← 核。分割不可の裏付け
cl-cc/php     167   （うち 135 は php 自身の中で閉じている → 外部侵入は僅少）
cl-cc/codegen  64
cl-cc/runtime  44
...
```

切り出し候補への「外部からの内部侵入」ファイル数:

| 候補 | 外部から内部に侵入している他パッケージ | 判定 |
|------|------|------|
| formatter / docgen / prolog-tools | **0 ファイル** | 前準備なしで即切れる |
| php | `pipeline`（**1 ファイルのみ** = `pipeline-runtime-bridges.lisp`） | 1 点の脱結合で切れる |
| javascript | `pipeline`（**1 ファイルのみ**） | 同上 |
| 核（vm 他） | 多数（vm だけで 301） | 切らない |

→ php の内部参照 167 のうち外部から来ているのは実質 1 ファイル。§5-1 で名指しした
「唯一の障害」が実測でも文字どおり唯一であることが確認できる。周縁の境界は
（本体全体が未完成でも）既に凍っている。

## 3. 分割の判断: 2 段ゲート（切れるか × 切る価値があるか）

抽出は必ず 2 つのゲートを順に通す。片方だけでは不十分:

- **ゲート①「切れるか」** — 外部からの内部侵入(`::`)が 0 か、収束しているか
  （§2-1）。これは *前提条件* にすぎない。
- **ゲート②「切る価値があるか」** — サイズ × 独立性が配管コスト（flake input +
  Nix 派生 + CI + cachix + README + `flake.lock` 追従）を上回るか（原則④）。
  小さすぎる / cl-cc 専用ツールは、①満点でも②で落ちる。

| 候補 | 規模 | ①切れるか | ②価値 | 判定 |
|------|------|------|------|------|
| `type` | ~16k loc | ◎ 侵入0・外向きは ast public のみ | ◎ 型システムとして一貫 | **抽出** |
| php + javascript | ~48k loc | ○ 逆依存1ファイル(要脱結合) | ◎ 全体の30% | **抽出** |
| optimize | ~41k loc | △ vm 内部75(要 vm 硬化) | ◎ 最適化ドメイン | **抽出(硬化後)** |
| native codegen 一式 | ~34k loc | △ vm 内部100(要 vm 硬化) | ◎ 一貫ドメイン | **抽出(硬化後)** |
| formatter/docgen/prolog-tools | ~1k loc | ◎ 侵入0 | ✕ 小さすぎ / cl-cc専用 | **monorepoに残す** |
| testing-framework | ~3k loc | △ | ✕ cl-weave と重複 | **廃止方向** |

> **formatter/docgen/prolog-tools を切らない理由**: ①は満点(侵入0)だが②で落第。
> 221〜473 loc に repo 1 個ぶんの配管が付くと *コードより配管が重い*。docgen /
> prolog-tools はそもそも汎用ライブラリではなく cl-cc の構造に依存した内製ツール
> （cl-prolog が汎用エンジンなのとは別物）。既に `maybe-load-asd` の probe-file
> ガードで疎結合なので、monorepo 内モジュールのままで隔離の恩恵は得られている。

## 4. 提案するリポジトリ境界（最終ターゲット）

抽出する外部 repo は **5 個**。残りは `cl-cc-core`（monorepo）に留める。
Terraform 側の器は `takeokunn/private-terraform` の
`projects/github/repos_nerima_lisp_cl_cc.tf` に仮実装済み（apply は抽出 PR 待ち）。

```
cl-cc-core (monorepo として残す = 「compiler 本体」)
  kernel/vm/frontend(parse,type*,expand,cps)/optimize*/native*/compile/stdlib
  + 小物: formatter, docgen, prolog-tools, testing-framework(廃止方向)
  * type/optimize/native は抽出候補だが、core 内では公開APIモジュールとして整理

外部 repo（クラス A = 即, クラス B = vm 公開API硬化後）:
  [A] cl-cc-type            packages/type
  [A] cl-cc-php             packages/php
  [A] cl-cc-javascript      packages/javascript
  [B] cl-cc-optimize        packages/optimize
  [B] cl-cc-codegen-native  mir/target/regalloc/codegen/emit(+binary)
```

### フェーズ A — クラス A の抽出（境界が凍っている・低〜中リスク）

- **`cl-cc-type`** ← `packages/type`（~16k loc, deps: ast public のみ）。
  侵入0・外向き結合0 で、core で最もクリーンな抽出対象。配管 PoC に最適。
- **`cl-cc-php`** ← `packages/php`（~25.6k loc）
- **`cl-cc-javascript`** ← `packages/javascript`（~22.9k loc）
  php/js は逆依存が `pipeline` のみ。ただし脱結合 1 点が前提（次項）。

> **monorepo に残すもの**: formatter / docgen / prolog-tools（②価値ゲート落第、
> §3）。`testing-framework` は cl-weave のネイティブ property API と重複のため
> **cl-weave 移行 → 廃止**を別トラックで検討（切り出し対象外）。

### フェーズ B — php/javascript の脱結合（最大効果・要作業）

- **`cl-cc-php`** ← `packages/php`（~25.6k loc）
- **`cl-cc-javascript`** ← `packages/javascript`（~22.9k loc）

合計で全体の約 30%。逆依存が実質 `pipeline` のみで、切れば本体が大幅に軽くなる。

**唯一かつ最大の障害**: `pipeline` が php バックエンドの *内部シンボル* を
直接参照している。

```
packages/pipeline/src/pipeline-runtime-bridges.lisp:
  '((cl-cc/php::%php-array      . cl-cc/php:%php-array)   ; 内部→公開の対応表
    (cl-cc/php::%php-array-ref  . cl-cc/php:%php-array-ref)
    ... 数十エントリ ...)
```

`pipeline` → `php` の**コンパイル時・内部実装レベルの結合**であり、このままでは
php を別リポジトリに動かせない（内部パッケージ `cl-cc/php::` を外部から参照できない）。

→ 切り出し前に **§5 のバックエンド登録プロトコル**を導入して疎結合化する。

### フェーズ C — クラス B の抽出（vm 公開 API 硬化が前提）

optimize と native codegen は「②価値」は高いが「①切れるか」で vm 内部への
`::` 依存を抱える（optimize→vm 75、codegen→vm 100）。先に §5-2 の vm 公開 API
硬化（`::`→`:` + lint）を済ませてから抽出する。

- **`cl-cc-optimize`** ← `packages/optimize`（~41k loc）。最適化パス群。
- **`cl-cc-codegen-native`** ← `mir + target + regalloc + codegen + emit + binary`
  （~34k loc）。「IR → x86-64 / riscv64 / arm 機械語 + ELF/Mach-O 出力」の一貫ドメイン。

vm が安定インタフェース（IR/MIR/命令構築子/条件・アトミックの公開契約）を提供
してからでないと境界が濁る。フェーズ B 完了後、vm API を固める作業と合わせて着手。

## 5. インタフェース設計（脱結合の要点）

### 5-1. バックエンド登録プロトコル（フェーズ B の前提作業）— **完了 2026-07-27**

> 実装済み: `cl-cc/backend-protocol`（`packages/bootstrap/src/backend-protocol.lisp`）。
> php は `packages/php/src/backend.lisp`、js は `packages/javascript/src/backend.lisp`
> で自己登録する。js の双方向結合（`*js-apply-fn*` 等への VM クロージャ注入）は
> `vm-integration` 構造体で反転済み — pipeline が capability を渡し、policy は
> backend 側に残る。登録シンボル集合の同一性はテストで固定し、加えて
> Array.map の compiled-JS コールバック / メソッド内 `this` / ネスト時の
> レシーバ復元を e2e で確認済み。以下は当時の記述。

**現状の実結合**（`packages/pipeline/src/pipeline-runtime-bridges.lisp`, 173 行）:

- `%register-php-runtime-bridges`: php パッケージを `do-external-symbols` で走査し、
  `%PHP-*` 命名の fbound 関数を `cl-cc/vm:vm-register-host-bridge` で VM に登録
  （旧 `*php-runtime-bridge-entries*` alist はドリフトのため命名規約走査に置換済み）。
- `%register-js-runtime-bridges` / `seed-js-runtime-globals`: js パッケージを同様に
  走査してブリッジ＋グローバルを登録。
- これらを `pipeline.lisp`（694/697/773/776 行）が直接呼ぶ。

→ `pipeline` が **php/js の命名規約とパッケージを知っている**（`cl-cc/php`/`cl-cc/js`
への参照）。これが唯一のクリティカル結合。

**目標: 向きを反転（`backend → protocol ← pipeline` の Y 字）。**

```lisp
;; 核側の新パッケージ cl-cc/backend-protocol（vm にのみ依存）:
(defvar *registered-backends* '())            ; (language . backend) alist

(defgeneric backend-bridge-symbols (backend)
  (:documentation "host bridge として VM に登録する fbound シンボルのリスト"))
(defgeneric backend-global-symbols (backend)  ; js グローバル seed 用（任意）
  (:method (backend) '()))

(defun register-backend (language backend)
  (setf (getf *registered-backends* language) backend))

(defun register-all-backend-bridges ()        ; pipeline がこれだけ呼ぶ
  (loop for (lang backend) on *registered-backends* by #'cddr do
    (dolist (sym (backend-bridge-symbols backend))
      (cl-cc/vm:vm-register-host-bridge sym (fdefinition sym)))))
```

```lisp
;; cl-cc-php リポジトリ側で自己登録（load 時）:
(defclass php-backend () ())
(defmethod backend-bridge-symbols ((b php-backend))
  ;; %PHP-* 走査ロジックは php repo 内に移設（自分の内部を自分で知る）
  (loop for s being the external-symbols of :cl-cc/php
        when (and (fboundp s) (%php-bridge-name-p s)) collect s))
(register-backend :php (make-instance 'php-backend))
```

これにより:
- `pipeline` は `register-all-backend-bridges` を 1 回呼ぶだけ。`cl-cc/php`/`cl-cc/js`
  への参照が消える（`pipeline-runtime-bridges.lisp` は削除、走査ロジックは各 backend へ）。
- 依存が `php → backend-protocol ← pipeline` の Y 字になり、php/js を独立リポジトリへ
  移動できる（ただし php/js は core=`cl-cc` umbrella に依存する**プラグイン repo**。
  standalone ではなく「cl-cc に対するプラグイン」モデル）。
- 将来のバックエンド追加（wasm/native）も同じ登録口。

**php と js で難易度が大きく違う（実コード精査で判明）**:

- **php: 容易**。`%register-php-runtime-bridges` は php パッケージを走査して `%PHP-*`
  関数を `vm-register-host-bridge` に登録するだけ。走査ロジックを php 側の
  `backend-bridge-symbols` メソッドへ移設すれば、登録シンボル集合は不変（＝挙動不変）。
  検証は「登録シンボル集合の before/after 一致」で足りる（フル php テスト不要）。

- **js: 高難度・双方向結合**。pipeline は bridge 登録に加えて、js ランタイムの特殊変数へ
  **VM 統合クロージャを注入**している:
  - `cl-cc/javascript::*js-apply-fn*` ← `%vm-call-closure-sync` で VM クロージャを呼ぶ
    ロジック（Array.map 等のコールバックを host→VM へ逆ルート）。
  - `*js-callable-p*` ← `%vm-closure-object-p` を callable 判定に含める。
  - `*js-apply-with-this-fn*` ← `this` を host special と VM global の両方に束縛。
  - `seed-js-runtime-globals` ← `*JS-*` special を VM globals へ複写。

  これは「js が VM クロージャの呼び方を知らない」ための注入で、js↔vm の双方向依存。
  脱結合には「js 側が *js-apply-fn* を受け取る protocol（VM closure invoker を注入する
  口）」を定義し、pipeline/vm がそれを実装する形にする必要がある。**雑な移設は
  コンパイル済み JS を全滅させる**ため、js-e2e / selfhost の全緑確認が必須。

**移行手順（テスト緑を保ちながら）**:
1. 核に `cl-cc/backend-protocol` を追加（registry + 総称関数）。**registry は php/js/
   pipeline の共通依存に置く必要がある** — php/js の共通下位は bootstrap（js は vm に
   依存しない）なので、registry(defvar + register-backend + generic)は bootstrap、
   実登録(vm-register-host-bridge 呼び出し)は pipeline に残す。
2. **php を先に移行**（低リスク・検証容易）。走査を php 側メソッドへ、pipeline は
   protocol 経由。登録集合の一致を確認。
3. **js を移行**（高リスク）。`*js-apply-fn*` 等の VM 統合を「js が invoker を受け取る」
   protocol へ反転。js-e2e / selfhost 全緑を確認。ここが本設計の最難関で、専用の
   検証済み作業を要する。
4. php/js を `cl-cc-php` / `cl-cc-javascript` へ移設（`:depends-on (:cl-cc)` の
   プラグイン repo）。monorepo umbrella の deps から外し flake input 化。

> **実装フェーズの位置づけ**: 手順 3（js の双方向脱結合）は、コンパイル済み JS の
> 挙動を変えずに VM 統合を反転する繊細な作業で、フル e2e 検証(pipeline/selfhost/
> js-runtime-* テスト)が前提。leaf 抽出(ast/type/binary/runtime)のような
> 「単独ビルドで検証完結」とは性質が異なり、monorepo が clean な状態で腰を据えて
> 行うべき——原設計 §1-0 が予言した「核=セルフホスト結合」の一種。

### 5-2. 核の公開契約（フェーズ C の前提）

`ast` / `ir` / `mir` / `vm` について「外部リポジトリが依存してよい公開シンボル」を
明示パッケージ（例: `cl-cc-ast` の外部シンボルのみ）として固定し、
`::`（内部）参照を禁止する lint を CI に入れる。これが崩れると再び密結合に戻る。

## 6. 移行順序（切りやすい順 = ①②とも高い順）

```
0. フェーズ A-0: cl-cc-ast を外部化 ★着手済み(ローカル repo 作成・build 緑)
   └ 依存ゼロの葉。type/parse/php/js が依存するため必ず先頭。これを先に出さ
     ないと cl-cc → cl-cc-type → (cl-cc内ast) の repo 間循環になる。
1. フェーズ A-1: cl-cc-type を外部化(cl-cc-ast に依存)
   └ ast の公開 API のみ使用(侵入0)。type 抽出はこれで循環なく成立。
2. 脱結合 PR: §5-1 のバックエンド登録プロトコルを本体に導入
   └ この時点では php/js はまだモノレポ内。pipeline から内部参照を除去し、
     テスト（compiler-tests-stdlib 等）が緑のままを確認。
3. フェーズ A-2: php → cl-cc-php、javascript → cl-cc-javascript を外部化
   └ 逆依存が pipeline のみ。登録プロトコル経由で接続。
4. vm 公開 API 硬化: §5-2 の公開契約 + `::` 禁止 lint
5. フェーズ C: cl-cc-optimize → cl-cc-codegen-native を外部化
```

> **順序修正の教訓**: 当初 type を先頭に置いたが、type は ast に依存するため
> ast を先に出さないと repo 間循環になる。「切りやすさ」は *外向き依存が公開
> API か* だけでなく *その依存先が既に外部化済みか* にも依存する。真の第一
> ドミノは依存ゼロの葉 `ast`。

formatter / docgen / prolog-tools は外部化しない（§3・§4）。各ステップは
「本体テストが緑」を通過条件にし、特に `selfhost`（セルフホスト）と `pipeline`
の e2e が壊れないことを受け入れ基準にする。

## 7. 配管（Nix / ASDF）— 既存方式の再利用

新規リポジトリは既存 `cl-*` と全く同じ経路で取り込む。追加コストは小さい。

**flake.nix** — plain source tree として input 追加:

```nix
cl-cc-php = { url = "github:nerima-lisp/cl-cc-php"; flake = false; };
```

理由は既存コメント（flake.nix:10-32）と同じ:
- 各 `cl-*` の flake を評価すると transitive input（paredit-cli 等）を
  無駄に引き込む。source だけ欲しい。
- 依存先 flake が Linux `systems` しか宣言せず aarch64-darwin が壊れる問題も回避。

**nix/asdf-systems.nix** — `mkAsdfSystem` / `extraLispLibs` で内部システムと合流:

```nix
# 外部由来（非 cl-cc-* 派生）は extraLispLibs で threads-in する既存の仕組みに乗せる
```

**cl-cc.asd** — 開発時は `maybe-load-asd` のオプショナル読み込みで
ローカルソースを、CI/リリースでは Nix 派生を参照。probe-file ガードにより
外部リポジトリが無くてもプロダクションビルドは成功する（既存の設計を踏襲）。

## 8. リスクと未解決点

- **核は分割対象外（前提の再掲）**: bootstrap/vm/expand/compile/selfhost は
  `our-eval` の事前 intern とセルフホストで結合しており（`cl-cc/vm::` 跨ぎ参照 301）、
  リポジトリ境界で割ると壊れる。分割の誘惑が出ても核には手を付けない（§1-0 非目標）。
- **churn の高い領域を切らない**: 本体は 578 コミット/90 日で活発に変化中。
  境界が動いている領域を切ると flake.lock 追従が恒常コスト化する。仕様が枯れ、
  逆依存が収束した周縁のみを対象にする。
- **php ⇄ pipeline の runtime-bridge**: §5-1 が完了しない限り php/js 外部化は
  着手不可。ここが本設計の最重要クリティカルパス。
- **バージョン整合**: 外部リポジトリが増えると flake.lock の更新運用が要る。
  複数リポジトリ同時変更時の atomic 性は失われる（モノレポの利点の喪失）。
  → 変更頻度が低く安定した領域（php/js/native codegen）を優先し、活発に相互変更が
    走る核は残す、という本設計の順序で緩和する。
- **テスト分散**: 各バックエンドのテスト（`compiler-tests-*`, `js-runtime-*`）は
  切り出し先へ移す。本体 e2e には「代表シナリオのスモーク」だけ残す。
- **testing-framework の去就**: cl-weave 移行と重複解消を別トラックで先に判断する。

## 9. 次アクション（この設計を承認後）

- [x] Terraform 仮実装（`private-terraform` の `repos_nerima_lisp_cl_cc.tf`, 5 repo）
- [x] 葉パッケージ 4 個の外部化（ast / type / binary / runtime）— `nix flake check` 緑
- [x] §5-1 登録プロトコル（php / javascript 両方）— `cl-cc/backend-protocol`
- [x] vm 内部参照の棚卸し + ratchet lint（`packages/vm/tests/vm-public-api-lint-tests.lisp`）
      — 106 個中 37 個は既に export 済みで `::` を惰性で書いていただけだったため `:` に修正。
      残りの distinct 数は optimize 16 / codegen 34 / compile 6 / pipeline 7 / expand 7、
      regalloc・emit・mir・parse は 0。lint は「増やせない」だけを保証する
      （減らすのが作業本体、天井の置き去りも別テストで検出）。
- [ ] vm 公開契約そのものの決定（フェーズ C の前提）— 残りの大半は命令型と
      そのスロットアクセサで、§5-2 が「export すべき」と言っている当のもの。
      外部 repo のパスがどこまで依存してよいかの判断が要る。
- [ ] **§6 手順 4（php/js の repo 移設）の意味を決める** — 下記 §10 参照

## 10. 実測で判明した制約（2026-07-27 追記）

### 10-1. 「input として宣言済み」は「実際にビルドされている」ではない

ast / type は `flake.nix` の input として宣言され `nix/asdf-systems.nix` で
注入されていたにもかかわらず、**一度もコンパイルされていなかった**。
`cl-cc.asd` の `ensure-system-asd` は `find-system` が *外した時だけ* per-package
`.asd` を読むが、cl-cc のソースツリーは注入された derivation より先に走査される。
結果、Nix でもローカルでも in-tree のコピーが勝ち続け、両者は数か月ドリフトした。

検出は配管を読むのではなく **片方でしか通らないテストを走らせる**こと。
`infer-with-constraints` は `packages/type/src` にしか無いのに CI では緑だった。

4 個すべてでこの手順を実行済み: input + derivation + `externalCcSystems` 追加 →
`packages/X/{src,*.asd}` 削除 → `ensure-system-asd` の行を削除 →
`nix flake check` で出たものを潰す。ハードコードされた `packages/X/src/...`
文字列に注意（optimizer roadmap と `nix/checks.nix` にあった）。

### 10-2. php/js は葉と同じ手順では切り出せない（依存が逆）

`cl-cc-php` / `cl-cc-javascript` は **`cl-cc` を flake input に取っている**。
ASDF 上も `:cl-cc-vm` `:cl-cc-parse`（＝核）に依存する。したがって cl-cc 側が
これらを input に取ると**循環**になり、flake では表現できない。

§4 が「プラグイン repo」と書いていたのはこの意味であり、**「php/js の外部化」は
cl-cc がこれらを自分のビルドから外すこと**を意味する — `:language :php` /
`:language :javascript` を pipeline から落とし、対応するテスト群を各 repo へ
移すということ。これは配管作業ではなく「cl-cc とは何か」を決める製品判断であり、
`cl-cc.asd` の system description が両バックエンドを明示している以上、
設計の承認とは別に判断が要る。

§5-1（この判断の前提として設計が挙げていた作業）は完了しており、
`cl-cc/pipeline` はもう両バックエンドの内部シンボルを一切名指ししていない。
判断が「外す」に倒れた時点で、機械的な移設として実行できる状態にある。

> 参考: 会話中に検討した `cl-cc-runtime-concurrent`（並行ランタイム26ファイル/
> ~3.1k loc）は「compiler と target runtime の分離」という別軸の候補。runtime
> 単一パッケージの分割とオーファン判定が前提のため、本設計の 5 repo とは別トラック。

### 10-3. 核＝分割不可 という前提は実測で否定された（2026-07-27）

§1-0 は「核はセルフホストの自己参照ウェブ」「`cl-cc/vm::` 跨ぎ参照 301」を根拠に
vm を分割対象外としていた。**この根拠は方向を取り違えている。** 301 は
*他パッケージが vm の内部に手を突っ込んでいる数* であって、vm が何かに依存して
いる数ではない。前者は公開 API の問題であり、§5-2 の作業そのものである
（optimize / codegen は既に 0 に到達済み）。

実測: `(asdf:load-system :cl-cc-vm)` は cl-cc ツリー単独で成功し、その時点で
`cl-cc/expand` `cl-cc/compile` `cl-cc/optimize` `cl-cc/parse` `cl-cc/ast`
`cl-cc/type` はいずれも **未ロード**。存在するのは `cl-cc/bootstrap` と
`cl-cc/runtime`（＝既に外部化済み）だけ。`vm-bridge-io-docs.lisp` の
`cl-cc/expand` 参照は `find-package` による実行時ルックアップで、不在なら NIL に
落ちるだけ。残りは docstring / コメント。

したがって **vm は葉である**。依存は `bootstrap`(283 loc, 依存ゼロ) と
`runtime`(外部化済み) のみ。

**これが「cl-cc を小さくする」唯一の実効レバー。** vm の背後に積み上がっている量:

```
vm          26,897        ← bootstrap(283) の上の葉
optimize    25,092  ┐
codegen     19,468  │
php         19,251  ├ すべて vm 依存。vm が外部化されない限り
javascript  15,220  │ cl-cc 側が input に取ると循環になる（§10-2）
emit         2,808  │
regalloc     1,608  ┘
                    合計 ≈ 110,000 / 全体約 300,000
```

つまり順序は **bootstrap → vm → 残り全部**。vm を出さない限り大物は 1 つも
出せず、出せば約 1/3 が一度に外に出る。逆に、依存ゼロの小物
（ir 523 / mir 697 / target 302 / bytecode 1,073 / docgen 134 / formatter 101 /
prolog-tools 288）は今すぐ出せるが、合計 3.1k loc で本体はほとんど小さくならない。

**未着手の理由**: `cl-cc-bootstrap` と `cl-cc-vm` の GitHub repo が未作成
（Terraform が用意したのは type/php/javascript/optimize/codegen-native の 5 個）。
repo 作成は outward な操作なので判断を残している。