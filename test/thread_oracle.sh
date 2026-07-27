#!/usr/bin/env bash
#
# Threading correctness oracle for the multi-threading2 (from-scratch re-run) effort.
#
# Runs a sequential (golden) SWAT+ executable and a (to-be-threaded) executable on a
# short window of a chosen scenario and compares every produced numeric output file
# bit-for-bit (default) with spcheck.py. A missed threadprivate/data race shows up as
# a numeric diff in the aggregated (average-annual / yearly) outputs, which fold in
# every simulated day.
#
# Unlike the original multi-threading branch's oracle (which derived a minimized
# print.prt from refdata/Ames_sub1, 12 HRU), this version uses the FIXTURE'S OWN
# print.prt unchanged and only narrows the time window, per the multi-threading2
# instructions ("use the print.prt in that folder ... modify as necessary to do
# 2-year simulation runs"). Default scenario is workdata/Ames_sub1_144hru (144
# independent HRUs, cswat=1) -- the Stage-1 correctness fixture.
#
# Usage:
#   test/thread_oracle.sh <seq_exe> [par_exe] [threads] [abserr] [relerr]
#     seq_exe : sequential reference executable (the golden)
#     par_exe : executable under test         (default: same as seq_exe)
#     threads : OMP_NUM_THREADS for par_exe   (default: 4)
#     abserr  : absolute tolerance            (default: 1e-30 = bit-for-bit)
#     relerr  : relative tolerance            (default: 0)
#
# Env:
#   SCENARIO_DIR : scenario to run (default workdata/Ames_sub1_144hru)
#   YR_START     : first sim year for the short window (default 2019 -> 2019-2020)
#
# spcheck.py flags a field when abs(diff) >= abserr + relerr*abs(b) (uses >=), so a
# literal 0 tolerance would flag identical values; 1e-30 is effectively bit-for-bit
# for the ~7-sig-fig text outputs while letting identical values pass.
#
# Exit status: 0 = outputs match within tolerance; 1 = differences found.
set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SEQ_EXE="$(readlink -f "${1:?need a sequential executable}")"
PAR_EXE="$(readlink -f "${2:-$SEQ_EXE}")"
THREADS="${3:-4}"
AERR="${4:-1e-30}"
RERR="${5:-0}"

SCENARIO="${SCENARIO_DIR:-$REPO/workdata/Ames_sub1_144hru}"
YR_START="${YR_START:-2019}"
[ -d "$SCENARIO" ] || { echo "scenario not found: $SCENARIO" >&2; exit 2; }

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# Derive the short-window scenario into $1 (regular files only; skip subdirs like SWIFT/)
gen_fast() {
  local d="$1"
  find "$SCENARIO" -maxdepth 1 -type f -exec cp {} "$d"/ \;
  # Narrow time.sim + print.prt windows to [YR_START, end]. Line 3 holds yrc_start in
  # both files for this fixture family (day_start yrc_start day_end yrc_end ...).
  awk -v y="$YR_START" 'NR==3{sub(/[0-9]{4}/, y)}1' "$d"/time.sim  > "$d"/.t && mv "$d"/.t "$d"/time.sim
  awk -v y="$YR_START" 'NR==3{sub(/[0-9]{4}/, y)}1' "$d"/print.prt > "$d"/.t && mv "$d"/.t "$d"/print.prt
}

GOLD="$WORK/gold"; mkdir -p "$GOLD"; gen_fast "$GOLD"

echo "[oracle] scenario=$SCENARIO window=${YR_START}-end"
echo "[oracle] running sequential golden ($(basename "$SEQ_EXE"), 1 thread)"
# Marker written immediately before the run: any output file touched by the run (whether
# newly created OR an overwritten leftover from a prior run already in the fixture) ends
# up newer than this, which is how we detect the FULL set print.prt actually emits.
touch "$GOLD/.run_marker"; sleep 0.01
( cd "$GOLD" && OMP_NUM_THREADS=1 "$SEQ_EXE" >run.log 2>&1 ) \
  || { echo "sequential run failed; see $GOLD/run.log" >&2; tail -20 "$GOLD/run.log" >&2; exit 2; }

# Build .testfiles.tst from EVERY file the run wrote (mtime newer than the marker),
# restricted to numeric output extensions, minus files that legitimately differ
# run-to-run (timestamped progress log, run-metadata). This auto-covers whatever
# print.prt is configured to output -- the FULL output set (playbook Part 6), and
# adapts per-scenario without a hand-maintained list.
( cd "$GOLD" && find . -maxdepth 1 -type f -newer .run_marker \
      \( -name '*.txt' -o -name '*.out' -o -name '*.csv' \) -printf '%f\n' \
    | grep -vE '^(simulation\.out|files_out\.out|area_calc\.out|diagnostics\.out|success\.fin|mgt_out\.txt)$' \
    | sort -u > .testfiles.tst )
rm -f "$GOLD/.run_marker"
echo "[oracle] comparing $(wc -l < "$GOLD/.testfiles.tst") output files (abserr=$AERR relerr=$RERR, threads=$THREADS):"
( cd "$GOLD" && tr '\n' ' ' < .testfiles.tst; echo )

# spcheck ctest: copies GOLD to a temp dir, runs PAR_EXE there, diffs against GOLD.
OMP_NUM_THREADS="$THREADS" python3 "$REPO/test/spcheck.py" ctest \
  "$PAR_EXE" "$GOLD" "$WORK/run" --abserr "$AERR" --relerr "$RERR"
echo "[oracle] PASS -- outputs match within tolerance ($THREADS threads)"
