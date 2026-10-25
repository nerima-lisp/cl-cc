#!/usr/bin/env bash
set -euo pipefail

compare_outputs() {
  local expected_stdout=$1 expected_exit=$2 actual_stdout=$3 actual_exit=$4
  local actual_status=$5 actual_stderr=$6
  [[ $actual_status == exited && ! -s $actual_stderr ]] || return 1
  cmp -s "$expected_stdout" "$actual_stdout" && [[ $(tr -d '\n' < "$expected_exit") == "$actual_exit" ]]
}

matches_diagnostic() {
  local expected=$1 path=$2
  [[ $expected != - && -f $path ]] && printf '%s\n' "$expected" | cmp -s "$path" -
}

classify_outputs() {
  local expected_stage=$1 expected_failure_present=$2
  local expected_stdout=$3 expected_exit=$4 actual_stdout=$5 actual_exit=$6
  local actual_status=$7 actual_stderr=$8 expected_diagnostic=$9 actual_diagnostic=${10}
  if [[ $actual_status == spawn-failure ]]; then
    comparison_status=run-failure
    comparison_outcome=unregistered-failure
    return 1
  fi
  if [[ $actual_status == run-failure || -s $actual_stderr ]]; then
    comparison_status=run-failure
    if [[ $expected_failure_present -eq 1 && $expected_stage == run-failure ]] && matches_diagnostic "$expected_diagnostic" "$actual_diagnostic"; then
      comparison_outcome=expected-failure
      return 0
    fi
    comparison_outcome=unregistered-failure
    return 1
  fi
  if [[ $actual_status != exited ]]; then
    comparison_status=run-failure
    comparison_outcome=unregistered-failure
    return 1
  fi
  if compare_outputs "$expected_stdout" "$expected_exit" "$actual_stdout" "$actual_exit" "$actual_status" "$actual_stderr"; then
    comparison_status=match
    if [[ $expected_failure_present -eq 0 || $expected_stage == match ]]; then
      comparison_outcome=match
      return 0
    fi
    comparison_outcome=unexpected-pass
    return 1
  fi
  comparison_status=mismatch
  if [[ $expected_failure_present -eq 1 && $expected_stage == mismatch ]]; then
    comparison_outcome=expected-failure
    return 0
  fi
  comparison_outcome=unregistered-failure
  return 1
}

classify_and_record() {
  if classify_outputs "$@"; then
    return 0
  fi
  unexpected_count=$((unexpected_count + 1))
  return 1
}

classify_compile_failure() {
  local expected_key=$1 expected_stage=$2 expected_diagnostic=$3
  local actual_stdout=$4 actual_stderr=$5
  if [[ -n $expected_key && $expected_stage == compile-failure ]] && \
    [[ ! -s $actual_stdout ]] && matches_diagnostic "$expected_diagnostic" "$actual_stderr"; then
    comparison_outcome=expected-failure
    return 0
  fi
  comparison_outcome=unregistered-failure
  return 1
}

classify_compile_and_record() {
  if classify_compile_failure "$@"; then
    return 0
  fi
  unexpected_count=$((unexpected_count + 1))
  return 1
}

check_gate() {
  [[ $unexpected_count -eq 0 ]]
}

if [[ ${1:-} == --control-child ]]; then
  control_dir=$(mktemp -d "${TMPDIR:-/tmp}/cl-cc-parity-control.XXXXXX")
  trap 'rm -rf "$control_dir"' EXIT
  control_script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
  unexpected_count=0
  printf 'expected' > "$control_dir/expected.stdout"
  printf '7\n' > "$control_dir/expected.exit"
  printf 'wrong' > "$control_dir/actual.stdout"
  : > "$control_dir/empty.stderr"
  stdout_rejected=0
  if ! classify_and_record match 0 "$control_dir/expected.stdout" "$control_dir/expected.exit" \
    "$control_dir/actual.stdout" 7 exited "$control_dir/empty.stderr" - -; then
    [[ $comparison_status == mismatch && $comparison_outcome == unregistered-failure ]] || exit 0
    stdout_rejected=1
  fi
  cp "$control_dir/expected.stdout" "$control_dir/actual.stdout"
  exit_rejected=0
  if ! classify_and_record match 0 "$control_dir/expected.stdout" "$control_dir/expected.exit" \
    "$control_dir/actual.stdout" 8 exited "$control_dir/empty.stderr" - -; then
    [[ $comparison_status == mismatch && $comparison_outcome == unregistered-failure ]] || exit 0
    exit_rejected=1
  fi
  mkdir -p "$control_dir/runtime"
  runtime_expected_diagnostic='runtime failure marker'
  python3 "$control_script_dir/run-target.py" \
    "$control_dir/runtime" "$control_dir/runtime.stdout" "$control_dir/runtime.stderr" \
    "$control_dir/runtime.exit" "$control_dir/runtime.process" \
    sh -c 'printf expected; printf "%s\n" "different runtime error containing runtime failure marker" >&2; exit 7'
  runtime_status=$(<"$control_dir/runtime/target.status")
  runtime_exit=$(tr -d '\n' < "$control_dir/runtime.exit")
  runtime_diagnostic="$control_dir/runtime.stderr"
  runtime_rejected=0
  exact_diagnostic_rejected=0
  if [[ $runtime_status == run-failure && $runtime_exit == 7 ]] && \
    cmp -s "$control_dir/expected.stdout" "$control_dir/runtime.stdout" && \
    ! classify_and_record run-failure 1 "$control_dir/expected.stdout" "$control_dir/expected.exit" \
      "$control_dir/runtime.stdout" "$runtime_exit" "$runtime_status" \
      "$control_dir/runtime.stderr" "$runtime_expected_diagnostic" "$runtime_diagnostic"; then
    if [[ $comparison_status == run-failure && $comparison_outcome == unregistered-failure ]]; then
      runtime_rejected=1
      exact_diagnostic_rejected=1
    fi
  fi
  printf 'compiler failure\n' > "$control_dir/expected.compile.stderr"
  printf 'unexpected compiler stdout\n' > "$control_dir/actual.compile.stdout"
  printf 'compiler failure\n' > "$control_dir/actual.compile.stderr"
  compile_stdout_rejected=0
  if ! classify_compile_and_record control-fixture compile-failure \
    'compiler failure' "$control_dir/actual.compile.stdout" "$control_dir/actual.compile.stderr"; then
    if [[ $comparison_outcome == unregistered-failure ]]; then
      compile_stdout_rejected=1
    fi
  fi
  if [[ $stdout_rejected -eq 1 && $exit_rejected -eq 1 && $runtime_rejected -eq 1 && \
    $exact_diagnostic_rejected -eq 1 && $compile_stdout_rejected -eq 1 ]]; then
    printf 'CONTROL_CHECK_FAILED unknown-mismatch=%s stdout=1 exit=1 runtime=1 exact-diagnostic=1 compile-stdout=1\n' "$unexpected_count"
    if check_gate; then exit 0; else exit 1; fi
  fi
  printf 'CONTROL_COMPARATOR_ACCEPTED_WRONG_OUTPUT stdout_rejected=%s exit_rejected=%s runtime_rejected=%s exact_diagnostic_rejected=%s\n' \
    "$stdout_rejected" "$exit_rejected" "$runtime_rejected" "$exact_diagnostic_rejected"
  exit 0
fi

target=${1:?target is required}
case "$target" in
  x86-64|aarch64|riscv64|wasm) ;;
  *) echo "unknown parity target: $target" >&2; exit 2 ;;
esac

parity_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cases_file="$parity_root/t/parity/cases.tsv"
program_root="$parity_root/t/parity/programs"
support_root="$parity_root/t/parity/support"
failures_file="$parity_root/t/parity/expected-failures.tsv"
worker_file="$parity_root/t/parity/worker.lisp"
work_root=$(mktemp -d "${TMPDIR:-/tmp}/cl-cc-parity.XXXXXX")
output_file=${PARITY_OUTPUT:?PARITY_OUTPUT must name the matrix output path}
mkdir -p "$(dirname "$output_file")"
trap 'rm -rf "$work_root"' EXIT

declare -a case_names=()
declare -A seen_cases=() case_categories=() case_files=() case_stdlib=() case_source_kind=() case_targets=()
case_count=0
while IFS=$'\t' read -r name category filename stdlib source_kind targets extra; do
  [[ -z "$name" || "$name" == \#* ]] && continue
  if [[ -n ${extra:-} || -z ${category:-} || -z ${filename:-} || -z ${stdlib:-} || -z ${source_kind:-} || -z ${targets:-} ]]; then
    echo "INVALID_CASE_ROW $name" >&2
    exit 1
  fi
  if [[ -n ${seen_cases[$name]+present} ]]; then
    echo "DUPLICATE_CASE $name" >&2
    exit 1
  fi
  if [[ ! -f "$program_root/$filename" ]]; then
    echo "MISSING_PROGRAM $name $filename" >&2
    exit 1
  fi
  case "$source_kind" in source|vm-program) ;; *) echo "INVALID_SOURCE_KIND $name $source_kind" >&2; exit 1 ;; esac
  case "$targets" in
    all|x86-64|aarch64|riscv64|wasm) ;;
    *) echo "INVALID_TARGET_SET $name $targets" >&2; exit 1 ;;
  esac
  seen_cases[$name]=1
  case_names+=("$name")
  case_categories[$name]=$category
  case_files[$name]=$filename
  case_stdlib[$name]=$stdlib
  case_source_kind[$name]=$source_kind
  case_targets[$name]=$targets
  mkdir -p "$work_root/$name"
  cp "$program_root/$filename" "$work_root/$name/program.lisp"
  case_count=$((case_count + 1))
done < "$cases_file"
if [[ $case_count -ne 21 ]]; then
  echo "INVALID_CASE_COUNT actual=$case_count expected=21" >&2
  exit 1
fi

# Validate every expected failure before running the worker.
declare -A failure_stage=() failure_taxonomy=() failure_diagnostic=() failure_reason=() failure_usage=()
manifest_count=0
while IFS=$'\t' read -r failure_target fixture stage taxonomy diagnostic reason extra; do
  [[ -z "${failure_target:-}" || "$failure_target" == \#* ]] && continue
  if [[ -n ${extra:-} || -z ${fixture:-} || -z ${stage:-} || -z ${taxonomy:-} || -z ${diagnostic:-} || -z ${reason:-} ]]; then
    echo "INVALID_MANIFEST_ROW target=$failure_target fixture=${fixture:-missing}" >&2
    exit 1
  fi
  case "$failure_target" in x86-64|aarch64|riscv64|wasm) ;; *) echo "INVALID_MANIFEST_TARGET $failure_target" >&2; exit 1 ;; esac
  if [[ $fixture != '*' && -z ${seen_cases[$fixture]+present} ]]; then
    echo "UNKNOWN_MANIFEST_FIXTURE $fixture" >&2
    exit 1
  fi
  case "$stage" in compile-failure|run-failure|mismatch) ;; *) echo "INVALID_MANIFEST_STAGE $stage" >&2; exit 1 ;; esac
  case "$taxonomy" in
    :unimplemented|:bug|:implementation-defined|:intentional-extension|:out-of-scope) ;;
    *) echo "INVALID_MANIFEST_TAXONOMY $taxonomy" >&2; exit 1 ;;
  esac
  if [[ $stage == mismatch ]]; then
    [[ $diagnostic == - ]] || { echo "MISMATCH_DIAGNOSTIC_MUST_BE_DASH $failure_target $fixture" >&2; exit 1; }
  else
    [[ $diagnostic != - ]] || { echo "MISSING_MANIFEST_DIAGNOSTIC $failure_target $fixture" >&2; exit 1; }
  fi
  key="$failure_target|$fixture"
  if [[ -n ${failure_stage[$key]+present} ]]; then
    echo "DUPLICATE_MANIFEST_KEY $key" >&2
    exit 1
  fi
  failure_stage[$key]=$stage
  failure_taxonomy[$key]=$taxonomy
  failure_diagnostic[$key]=$diagnostic
  failure_reason[$key]=$reason
  failure_usage[$key]=0
  manifest_count=$((manifest_count + 1))
done < "$failures_file"

export PARITY_TARGET="$target"
export PARITY_CASES="$cases_file"
export PARITY_WORK_DIR="$work_root"
export PARITY_PROGRAM_DIR="$program_root"
export PARITY_SUPPORT_DIR="$support_root"
export CL_SOURCE_REGISTRY="$parity_root//${CL_SOURCE_REGISTRY:+:$CL_SOURCE_REGISTRY}"

set +e
sbcl --noinform --disable-debugger --non-interactive \
  --eval '(require :asdf)' \
  --eval '(asdf:load-system "cl-cc")' \
  --load "$worker_file" \
  > "$work_root/worker.stdout" 2> "$work_root/worker.stderr"
worker_exit=$?
set -e
worker_status=missing
[[ -s "$work_root/worker.status" ]] && worker_status=$(<"$work_root/worker.status")
if [[ $worker_exit -ne 0 || $worker_status != ok ]]; then
  echo "WORKER_FAILURE target=$target sbcl_exit=$worker_exit status=$worker_status" >&2
  cat "$work_root/worker.stdout" "$work_root/worker.stderr" >&2
  exit 1
fi

hex_file() {
  od -An -v -tx1 "$1" | tr -d ' \n'
}

manifest_key_for() {
  local wanted_target=$1 wanted_fixture=$2 exact="$1|$2" wildcard="$1|*"
  if [[ -n ${failure_stage[$exact]+present} ]]; then
    printf '%s' "$exact"
  elif [[ -n ${failure_stage[$wildcard]+present} ]]; then
    printf '%s' "$wildcard"
  fi
}

matrix="$work_root/matrix.tsv"
printf 'program\tcategory\tVM-status\tVM-value\tVM-exit\tVM-stdout-hex\tVM-diagnostic-hex\ttarget-stage\ttarget-status\ttarget-exit\ttarget-stdout-hex\ttarget-diagnostic-hex\toutcome\ttaxonomy\treason\n' > "$matrix"
worker_status_count=0
applicable_count=0
not_applicable_count=0
match_count=0
expected_failure_count=0
compile_failure_count=0
run_failure_count=0
mismatch_count=0
unexpected_count=0

for name in "${case_names[@]}"; do
  category=${case_categories[$name]}
  directory="$work_root/$name"
  vm_status=missing
  vm_value=-
  vm_exit=-
  vm_stdout="$directory/vm.stdout"
  vm_diagnostic="$directory/vm.stderr"
  target_stage=compile
  target_status=missing
  target_exit=-
  target_stdout="$directory/target.stdout"
  target_diagnostic="$directory/target.diagnostic"
  outcome=unexpected
  taxonomy=-
  reason=-
  expected_key=$(manifest_key_for "$target" "$name")
  expected_stage=match
  expected_diagnostic=-
  expected_taxonomy=-
  expected_reason=-
  if [[ -n $expected_key ]]; then
    expected_stage=${failure_stage[$expected_key]}
    expected_diagnostic=${failure_diagnostic[$expected_key]}
    expected_taxonomy=${failure_taxonomy[$expected_key]}
    expected_reason=${failure_reason[$expected_key]}
  fi

  if [[ ! -s "$directory/vm.status" || ! -s "$directory/compile.status" || ! -f "$directory/compile.stderr" || ! -f "$directory/compile.stdout" ]]; then
    echo "WORKER_CELL_MISSING target=$target fixture=$name" >&2
    unexpected_count=$((unexpected_count + 1))
    vm_status=missing
    target_status=missing
  else
    worker_status_count=$((worker_status_count + 1))
    vm_status=$(<"$directory/vm.status")
    if [[ $vm_status == ok ]]; then
      if [[ ! -s "$directory/vm.value" || ! -s "$directory/vm.exit" || ! -f "$vm_stdout" ]]; then
        echo "VM_BASELINE_ARTIFACT_MISSING target=$target fixture=$name" >&2
        unexpected_count=$((unexpected_count + 1))
        vm_status=invalid-artifacts
      else
        vm_value=$(tr -d '\n' < "$directory/vm.value")
        vm_exit=$(tr -d '\n' < "$directory/vm.exit")
      fi
    fi
    [[ -f "$vm_stdout" ]] || : > "$vm_stdout"
    if [[ -f "$vm_diagnostic" ]]; then :; else : > "$vm_diagnostic"; fi
    target_status=$(<"$directory/compile.status")

    if [[ ${case_targets[$name]} != all && ${case_targets[$name]} != "$target" ]]; then
      if [[ $target_status != not-applicable ]]; then
        echo "TARGET_RESTRICTION_BROKEN $target $name status=$target_status" >&2
        unexpected_count=$((unexpected_count + 1))
        target_status=invalid-not-applicable
      else
        target_stage=not-applicable
        target_status=not-applicable
        : > "$target_stdout"
        cp "$directory/compile.stderr" "$target_diagnostic"
        outcome=not-applicable
        not_applicable_count=$((not_applicable_count + 1))
      fi
    else
      applicable_count=$((applicable_count + 1))
      case "$target_status" in
        compile-failure)
          target_stage=compile
          target_diagnostic="$directory/compile.stderr"
          compile_failure_count=$((compile_failure_count + 1))
          if classify_compile_and_record "$expected_key" "$expected_stage" "$expected_diagnostic" \
            "$directory/compile.stdout" "$target_diagnostic"; then
            outcome=expected-failure
            taxonomy=$expected_taxonomy
            reason=$expected_reason
            failure_usage[$expected_key]=$((failure_usage[$expected_key] + 1))
            expected_failure_count=$((expected_failure_count + 1))
          else
            outcome=unregistered-failure
            if [[ -n $expected_key && $expected_stage == compile-failure ]]; then
              echo "DIAGNOSTIC_MISMATCH target=$target fixture=$name expected=$expected_diagnostic compile_stdout=$(cat "$directory/compile.stdout") observed_stderr=$(cat "$target_diagnostic")" >&2
            else
              echo "UNREGISTERED_FAILURE target=$target fixture=$name stage=compile-failure observed=$(cat "$target_diagnostic")" >&2
            fi
          fi
          ;;
        compiled)
          target_stage=run
          command=()
          case "$target" in
            x86-64) command=("$directory/program.elf") ;;
            aarch64) command=(qemu-aarch64 "$directory/program.elf") ;;
            riscv64) command=(qemu-riscv64 "$directory/program.elf") ;;
            wasm) command=(wasmtime run "$directory/program.wasm") ;;
          esac
          : > "$target_stdout"
          python3 "$parity_root/t/parity/run-target.py" \
            "$directory" "$target_stdout" "$directory/target.stderr" \
            "$directory/target.exit" "$directory/target.process" "${command[@]}"
          run_status=$(<"$directory/target.status")
          target_diagnostic="$directory/target.diagnostic"
          { cat "$directory/target.process"; cat "$directory/target.stderr"; } > "$target_diagnostic"
          if [[ -f $directory/target.exit ]]; then
            target_exit=$(tr -d '\n' < "$directory/target.exit")
          fi
          expected_failure_present=0
          [[ -n $expected_key ]] && expected_failure_present=1
          failure_diagnostic="$directory/target.process"
          if [[ -s $directory/target.stderr ]]; then
            if [[ -f $directory/target.exit ]]; then
              failure_diagnostic="$directory/target.stderr"
            else
              failure_diagnostic="$target_diagnostic"
            fi
          fi
          if classify_and_record "$expected_stage" "$expected_failure_present" \
            "$vm_stdout" "$directory/vm.exit" "$target_stdout" "$target_exit" \
            "$run_status" "$directory/target.stderr" "$expected_diagnostic" "$failure_diagnostic"; then
            target_status=$comparison_status
            outcome=$comparison_outcome
            if [[ $comparison_status == run-failure ]]; then
              run_failure_count=$((run_failure_count + 1))
            elif [[ $outcome == match ]]; then
              match_count=$((match_count + 1))
            elif [[ $outcome == expected-failure ]]; then
              taxonomy=$expected_taxonomy
              reason=$expected_reason
              failure_usage[$expected_key]=$((failure_usage[$expected_key] + 1))
              expected_failure_count=$((expected_failure_count + 1))
            fi
          else
            target_status=$comparison_status
            outcome=$comparison_outcome
            if [[ $comparison_status == run-failure ]]; then
              run_failure_count=$((run_failure_count + 1))
              echo "UNREGISTERED_FAILURE target=$target fixture=$name stage=run-failure diagnostic_hex=$(hex_file "$target_diagnostic")" >&2
            elif [[ $comparison_status == mismatch ]]; then
              mismatch_count=$((mismatch_count + 1))
              if [[ $outcome == unexpected-pass ]]; then
                echo "UNEXPECTED_PASS target=$target fixture=$name expected_stage=$expected_stage" >&2
              else
                echo "UNREGISTERED_FAILURE target=$target fixture=$name stage=mismatch vm_exit=$vm_exit target_exit=$target_exit vm_stdout_hex=$(hex_file "$vm_stdout") target_stdout_hex=$(hex_file "$target_stdout")" >&2
              fi
            else
              echo "UNREGISTERED_FAILURE target=$target fixture=$name status=$run_status outcome=$outcome" >&2
            fi
          fi
          ;;
        not-applicable)
          echo "UNEXPECTED_NOT_APPLICABLE target=$target fixture=$name" >&2
          target_stage=compile
          target_status=not-applicable
          outcome=unregistered-failure
          unexpected_count=$((unexpected_count + 1))
          : > "$target_stdout"
          : > "$target_diagnostic"
          ;;
        *)
          echo "UNKNOWN_COMPILE_STATUS target=$target fixture=$name status=$target_status" >&2
          target_status=unknown-compile-status
          outcome=unregistered-failure
          unexpected_count=$((unexpected_count + 1))
          : > "$target_stdout"
          : > "$target_diagnostic"
          ;;
      esac
    fi

    if [[ $vm_status != ok ]]; then
      outcome=vm-baseline-failure
      unexpected_count=$((unexpected_count + 1))
      echo "VM_BASELINE_FAILURE target=$target fixture=$name observed=$(cat "$vm_diagnostic" 2>/dev/null || true)" >&2
    fi
  fi

  vm_hex=-
  vm_diag_hex=-
  [[ -f "$vm_stdout" ]] && vm_hex=$(hex_file "$vm_stdout")
  [[ -f "$vm_diagnostic" ]] && vm_diag_hex=$(hex_file "$vm_diagnostic")
  target_hex=-
  target_diag_hex=-
  [[ -f "$target_stdout" ]] && target_hex=$(hex_file "$target_stdout")
  [[ -f "$target_diagnostic" ]] && target_diag_hex=$(hex_file "$target_diagnostic")
  [[ -f "$directory/compile.stderr" && $target_stage == compile ]] && target_diag_hex=$(hex_file "$directory/compile.stderr")
  [[ -f "$directory/target.exit" && $target_exit == - ]] && target_exit=$(tr -d '\n' < "$directory/target.exit")
  if [[ $target_status == match ]]; then
    echo "MATCH target=$target fixture=$name"
  elif [[ $outcome == expected-failure ]]; then
    echo "EXPECTED_FAILURE target=$target fixture=$name stage=$target_stage taxonomy=$taxonomy diagnostic=$expected_diagnostic reason=$reason"
  fi
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$name" "$category" "$vm_status" "$vm_value" "$vm_exit" "$vm_hex" "$vm_diag_hex" \
    "$target_stage" "$target_status" "$target_exit" "$target_hex" "$target_diag_hex" \
    "$outcome" "$taxonomy" "$reason" >> "$matrix"
done

if [[ $worker_status_count -ne 21 ]]; then
  echo "MATRIX_INCOMPLETE target=$target worker_cells=$worker_status_count expected=21" >&2
  unexpected_count=$((unexpected_count + 1))
fi
for key in "${!failure_stage[@]}"; do
  row_target=${key%%|*}
  [[ $row_target == "$target" || $row_target == '*' ]] || continue
  if [[ ${failure_usage[$key]} -ne 1 ]]; then
    echo "INVALID_MANIFEST_USAGE $key stage=${failure_stage[$key]} count=${failure_usage[$key]} expected=1" >&2
    unexpected_count=$((unexpected_count + 1))
  fi
done

control_case_id=w4-05-control-v1
set +e
control_output=$(bash "$parity_root/t/parity/run-parity.sh" --control-child 2>&1)
control_exit=$?
set -e
if [[ $control_exit -ne 1 || $control_output != 'CONTROL_CHECK_FAILED unknown-mismatch=4 stdout=1 exit=1 runtime=1 exact-diagnostic=1 compile-stdout=1' ]]; then
  echo "CONTROL_FAILURE case_id=$control_case_id exit=$control_exit output=$control_output" >&2
  unexpected_count=$((unexpected_count + 1))
else
    echo "$control_output"
    echo "CONTROL_CASE_ID=$control_case_id CONTROL_EXIT=$control_exit CONTROL_REJECTED_STDOUT=1 CONTROL_REJECTED_EXIT=1 CONTROL_REJECTED_RUNTIME=1 CONTROL_REJECTED_DIAGNOSTIC_SUBSTRING=1 CONTROL_REJECTED_COMPILE_STDOUT=1"
fi

cp "$matrix" "$output_file"
cat "$matrix"
echo "SUMMARY target=$target cases=$case_count applicable=$applicable_count not_applicable=$not_applicable_count vm_cells=$worker_status_count matches=$match_count compile_failures=$compile_failure_count run_failures=$run_failure_count mismatches=$mismatch_count expected_failures=$expected_failure_count manifest_rows=$manifest_count unexpected=$unexpected_count"
echo "GITHUB_RUN_ID=${GITHUB_RUN_ID:-local}"
check_gate
