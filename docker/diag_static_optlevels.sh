#!/usr/bin/env bash
# Fully-static (SWAT_STATIC=full) runs at -O0 but crashes at -O3 on gfortran 13. Find the boundary:
# build fully-static at -O1 and -O2 (SWAT_EXTRA_FFLAGS overrides the -O3 in frelease; -march=native
# and -ffp-contract=off stay constant) and see which optimization level still runs racoon.
set -e
export HOME=/tmp
SRC=/src
DATASET="${1:-racoon_creek_120hru}"
git config --global --add safe.directory "$SRC" 2>/dev/null || true
gfortran --version | head -1
DS="$SRC/workdata/$DATASET"

build_and_run () {  # $1 = opt level (e.g. -O1)
    local opt="$1"
    local B="/src/build-static${opt}"
    echo "=== build fully-static ${opt} -march=native ==="
    cmake -S "$SRC" -B "$B" -DCMAKE_BUILD_TYPE=Release -DSWAT_OPENMP=ON -DSWAT_STATIC=full \
          -DSWAT_EXTRA_FFLAGS="$opt" >/tmp/c${opt}.log 2>&1
    cmake --build "$B" -j"$(nproc)" >/tmp/b${opt}.log 2>&1 \
        || { echo "  ${opt}: BUILD FAILED"; tail -8 /tmp/b${opt}.log; return; }
    local E=$(ls "$B"/swatplus-* | grep -v '\.o$' | head -1)
    local RUN="/tmp/run-static${opt}"; rm -rf "$RUN"; mkdir -p "$RUN"
    cp -a "$DS/." "$RUN/"; cp "$E" "$RUN/sp"; cd "$RUN"
    set +e; OMP_NUM_THREADS=1 ./sp >run.log 2>&1; local rc=$?; set -e
    cd /
    if [ $rc -eq 0 ] && grep -q "Execution successfully completed" "$RUN/run.log"; then
        echo "  ${opt}: rc=0  RAN CLEAN"
    else
        echo "  ${opt}: rc=$rc  CRASHED"
    fi
}

echo "=== run racoon ($DATASET), OMP_NUM_THREADS=1, fully-static ==="
build_and_run -O1
build_and_run -O2
echo
echo "Recall: -O0 runs clean, -O3 crashes. This locates the boundary."
