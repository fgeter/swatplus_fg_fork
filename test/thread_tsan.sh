#!/usr/bin/env bash
#
# ThreadSanitizer race check for the multi-threading effort.
#
# Runs a ThreadSanitizer-instrumented SWAT+ executable on the fast 2-year
# Ames_sub1 cswat=1 scenario (same fixture as thread_oracle.sh) and fails if
# TSan reports any data race in SWAT+ code. Runtime-library noise (libgfortran
# I/O locks) is filtered via test/tsan.supp + detect_deadlocks=0.
#
# Build the instrumented exe first:
#   cmake -S . -B build-tsan -DCMAKE_BUILD_TYPE=Release -DSWAT_TSAN=ON
#   cmake --build build-tsan -j4
#
# Usage:
#   test/thread_tsan.sh <tsan_exe> [threads]   (threads default: 4)
#
# Exit status: 0 = no data races; 1 = data race(s) reported.
set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EXE="$(readlink -f "${1:?need the ThreadSanitizer executable (build with -DSWAT_TSAN=ON)}")"
THREADS="${2:-4}"

SCENARIO="$REPO/refdata/Ames_sub1"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
RUN="$WORK/run"; mkdir -p "$RUN"

# Derive the fast scenario (mirror thread_oracle.sh: 2-year, diagnostics off, yearly output)
find "$SCENARIO" -maxdepth 1 -type f -exec cp {} "$RUN"/ \;
sed -i '3s/1975/2019/'                 "$RUN"/time.sim
sed -i 's/1975/2019/'                  "$RUN"/print.prt
sed -i 's/\(cbn_diagnostics *\)1/\10/' "$RUN"/carb_coefs.cbn
sed -i \
  -e 's/^hru_wb .*/hru_wb        n  n  y  n/'  -e 's/^hru_nb .*/hru_nb        n  n  y  n/' \
  -e 's/^hru_ls .*/hru_ls        n  n  y  n/'  -e 's/^hru_pw .*/hru_pw        n  n  y  n/' \
  -e 's/^hru_cb .*/hru_cb        n  n  y  n/'  -e 's/^hru_cb_vars .*/hru_cb_vars   n  n  y  n/' \
  "$RUN"/print.prt

export TSAN_OPTIONS="suppressions=$REPO/test/tsan.supp detect_deadlocks=0 halt_on_error=0 exitcode=0"
echo "[tsan] running $EXE with OMP_NUM_THREADS=$THREADS"
( cd "$RUN" && OMP_NUM_THREADS="$THREADS" "$EXE" >run.log 2>&1 ) || true

# Preserve the full log outside $WORK (the trap deletes $WORK on exit).
KEEP="$REPO/tsan_last.log"
cp "$RUN/run.log" "$KEEP"
echo "[tsan] full log preserved at $KEEP"

# Only DATA RACES are application bugs; libgfortran I/O "unlock of an unlocked
# mutex" reports during the sequential read phase are runtime-library noise.
races="$(grep -c "ThreadSanitizer: data race" "$RUN/run.log" || true)"
if [ "$races" -gt 0 ]; then
  echo "[tsan] FAIL — $races data race report(s). Distinct SWAT frames (top of each stack):"
  # first src/ frame after each "data race" header = the racing app site
  awk '/ThreadSanitizer: data race/{f=1} f&&/#[0-9]+ .* src\//{print $0; f=0}' "$RUN/run.log" \
    | grep -oE 'src/[a-z0-9_]+\.f90:[0-9]+' | sort | uniq -c | sort -rn
  exit 1
fi
echo "[tsan] PASS — no data races reported (threads=$THREADS)"
