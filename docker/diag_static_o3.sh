#!/usr/bin/env bash
# Why does fully-static (SWAT_STATIC=full) segfault at -O3 but run clean at -O0 on gfortran 13?
# Hypothesis: -O3 -march=native auto-vectorizes SWAT+'s math loops into libmvec vector calls
# (_ZGV* symbols); statically linking libmvec + its IFUNC dispatch lands on a NULL pointer.
# Test: (1) look for libmvec vector symbols in a fully-static -O3 binary; (2) rebuild fully-static
# -O3 with -fno-tree-vectorize (no libmvec) and see whether racoon then runs.
set -e
export HOME=/tmp
SRC=/src
DATASET="${1:-racoon_creek_120hru}"
git config --global --add safe.directory "$SRC" 2>/dev/null || true
gfortran --version | head -1

# 1) fully-static -O3 as-is (the crashing config) + scan for libmvec vector symbols
B1=/src/build-o3-static
echo "=== build fully-static -O3 (as-is) ==="
cmake -S "$SRC" -B "$B1" -DCMAKE_BUILD_TYPE=Release -DSWAT_OPENMP=ON -DSWAT_STATIC=full >/tmp/c1.log 2>&1
cmake --build "$B1" -j"$(nproc)" >/tmp/b1.log 2>&1 || { echo "build failed"; tail /tmp/b1.log; exit 1; }
E1=$(ls "$B1"/swatplus-* | grep -v '\.o$' | head -1)
echo "libmvec vector symbols (_ZGV...) referenced by the binary:"
nm "$E1" 2>/dev/null | grep -Eo '_ZGV[a-zA-Z0-9_]*' | sort -u | head -20 | sed 's/^/  /'
n=$(nm "$E1" 2>/dev/null | grep -Ec '_ZGV[a-zA-Z0-9_]*' || true)
echo "  -> $n libmvec vector-symbol references"

# 2) fully-static -O3 with the vectorizer OFF (removes libmvec)
B2=/src/build-o3-novec-static
echo "=== build fully-static -O3 + -fno-tree-vectorize (no libmvec) ==="
cmake -S "$SRC" -B "$B2" -DCMAKE_BUILD_TYPE=Release -DSWAT_OPENMP=ON -DSWAT_STATIC=full \
      -DSWAT_EXTRA_FFLAGS=-fno-tree-vectorize >/tmp/c2.log 2>&1
cmake --build "$B2" -j"$(nproc)" >/tmp/b2.log 2>&1 || { echo "build failed"; tail /tmp/b2.log; exit 1; }
E2=$(ls "$B2"/swatplus-* | grep -v '\.o$' | head -1)
n2=$(nm "$E2" 2>/dev/null | grep -Ec '_ZGV[a-zA-Z0-9_]*' || true)
echo "  libmvec vector-symbol references now: $n2"

DS="$SRC/workdata/$DATASET"
run_it () {  # $1 = label, $2 = exe
    local RUN="/tmp/run-$1"; rm -rf "$RUN"; mkdir -p "$RUN"
    cp -a "$DS/." "$RUN/"; cp "$2" "$RUN/sp"; cd "$RUN"
    set +e; OMP_NUM_THREADS=1 ./sp >run.log 2>&1; local rc=$?; set -e
    if [ $rc -eq 0 ] && grep -q "Execution successfully completed" run.log; then
        echo "  $1: rc=0  RAN CLEAN"
    else
        echo "  $1: rc=$rc  CRASHED"
    fi
    cd /
}
echo "=== run racoon ($DATASET), OMP_NUM_THREADS=1 ==="
run_it "o3-static"        "$E1"
run_it "o3-novec-static"  "$E2"
echo
echo "If o3-static CRASHES and o3-novec-static RUNS CLEAN -> libmvec/vectorization is the cause."
