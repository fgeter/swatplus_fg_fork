#!/usr/bin/env bash
#
# Threading correctness oracle for the multi-threading effort.
#
# Runs a sequential SWAT+ executable and a (to-be-threaded) executable on a small,
# fast 2-year Ames_sub1 cswat=1 scenario and compares their outputs bit-for-bit
# (default) with spcheck.py. A missed threadprivate/data race shows up as a numeric
# diff in the annual-average outputs, which aggregate every simulated day.
#
# The fast scenario is DERIVED from refdata/Ams_sub1 at runtime (2-year window,
# carbon diagnostics off, YEARLY output only) so it stays in sync with the
# reference data and produces a few MB instead of ~4.7 GB of output.
#
# Yearly (not annual-average) is used deliberately: soil_nutcarb_write.f90 does not
# compute/emit average-annual carbon output regardless of print.prt, so an AA-only
# config would omit the carbon outputs. Yearly is the finest aggregate that folds in
# every simulated day AND carries the carbon writes.
#
# Usage:
#   test/thread_oracle.sh <seq_exe> [par_exe] [threads] [abserr] [relerr]
#     seq_exe : sequential reference executable (the golden)
#     par_exe : executable under test         (default: same as seq_exe)
#     threads : OMP_NUM_THREADS for par_exe   (default: 4)
#     abserr  : absolute tolerance            (default: 1e-30 = bit-for-bit)
#     relerr  : relative tolerance            (default: 0)
#
# Note: spcheck.py flags a field when abs(diff) >= abserr + relerr*abs(b), using >=,
# so a literal 0 tolerance flags even identical values. 1e-30 is effectively
# bit-for-bit for the ~7-significant-figure text outputs (any real diff is far larger)
# while letting identical values pass.
#
# Exit status: 0 = outputs match within tolerance; 1 = differences found.
set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SEQ_EXE="$(readlink -f "${1:?need a sequential executable}")"
PAR_EXE="$(readlink -f "${2:-$SEQ_EXE}")"
THREADS="${3:-4}"
AERR="${4:-1e-30}"
RERR="${5:-0}"

SCENARIO="$REPO/refdata/Ames_sub1"
[ -d "$SCENARIO" ] || { echo "scenario not found: $SCENARIO" >&2; exit 2; }

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# Derive the fast scenario into $1
gen_fast() {
  local d="$1"
  find "$SCENARIO" -maxdepth 1 -type f -exec cp {} "$d"/ \;  # regular files only (skip SWIFT/ subdir)
  sed -i '3s/1975/2019/'                 "$d"/time.sim      # 2-year window 2019-2020
  sed -i 's/1975/2019/'                  "$d"/print.prt     # match print window
  sed -i 's/\(cbn_diagnostics *\)1/\10/' "$d"/carb_coefs.cbn # no daily carbon diagnostics
  sed -i \
    -e 's/^hru_wb .*/hru_wb        n  n  y  n/'  -e 's/^hru_nb .*/hru_nb        n  n  y  n/' \
    -e 's/^hru_ls .*/hru_ls        n  n  y  n/'  -e 's/^hru_pw .*/hru_pw        n  n  y  n/' \
    -e 's/^hru_cb .*/hru_cb        n  n  y  n/'  -e 's/^hru_cb_vars .*/hru_cb_vars   n  n  y  n/' \
    "$d"/print.prt                                          # yearly output only (cols: daily monthly yearly avann)
}

GOLD="$WORK/gold"; mkdir -p "$GOLD"; gen_fast "$GOLD"

echo "[oracle] running sequential golden ($SEQ_EXE, 1 thread)"
( cd "$GOLD" && OMP_NUM_THREADS=1 "$SEQ_EXE" >run.log 2>&1 ) \
  || { echo "sequential run failed; see $GOLD/run.log" >&2; exit 2; }

# Compare the annual-average / basin numeric outputs (exclude timestamped logs)
( cd "$GOLD" && ls *_yr.txt basin_*.txt 2>/dev/null \
    | grep -vE 'simulation' | sort -u > .testfiles.tst )
echo "[oracle] comparing $(wc -l < "$GOLD/.testfiles.tst") output files (abserr=$AERR relerr=$RERR, threads=$THREADS)"

# spcheck ctest: copies GOLD to a temp dir, runs PAR_EXE there, diffs against GOLD.
OMP_NUM_THREADS="$THREADS" python3 "$REPO/test/spcheck.py" ctest \
  "$PAR_EXE" "$GOLD" "$WORK/run" --abserr "$AERR" --relerr "$RERR"
echo "[oracle] PASS — outputs match within tolerance"
