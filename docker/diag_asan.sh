#!/usr/bin/env bash
# Split the two remaining variables behind the static-gfortran-11 CLOSE crash: static linking vs
# the gfortran 11 toolchain. -DSWAT_ASAN=ON disables -static (CMakeLists line ~104) AND turns on
# AddressSanitizer, giving a DYNAMIC gfortran-11 build that also catches memory corruption.
#   ASan reports an error  -> real corruption, caught by gfortran 11 (Arch/gfortran-16 missed it)
#   runs clean, completes  -> dynamic gfortran 11 is fine -> the crash is STATIC-link-specific
set -e
export HOME=/tmp
SRC=/src
BLD=/src/build-ubuntu-asan
DATASET="${1:-racoon_creek_120hru}"
git config --global --add safe.directory "$SRC" 2>/dev/null || true
gfortran --version | head -1

echo "=== configure + build: dynamic gfortran-11 + AddressSanitizer ==="
cmake -S "$SRC" -B "$BLD" -DCMAKE_BUILD_TYPE=Debug -DSWAT_ASAN=ON -DSWAT_OPENMP=ON >/tmp/acfg.log 2>&1 \
    || { echo "CONFIGURE FAILED"; tail -20 /tmp/acfg.log; exit 1; }
cmake --build "$BLD" -j"$(nproc)" >/tmp/abuild.log 2>&1 \
    || { echo "BUILD FAILED"; tail -25 /tmp/abuild.log; exit 1; }
E=$(ls "$BLD"/swatplus-* 2>/dev/null | grep -v '\.o$' | head -1)
echo "built: $E"
file "$E" | grep -o "statically linked\|dynamically linked" | sed 's/^/  link: /'

DS="$SRC/workdata/$DATASET"
RUN="/tmp/run-asan-$DATASET"
rm -rf "$RUN"; mkdir -p "$RUN"; cp -a "$DS/." "$RUN/"; cp "$E" "$RUN/swatplus-asan"
cd "$RUN"
echo "=== run racoon ($DATASET) under ASan, OMP_NUM_THREADS=1 ==="
set +e
OMP_NUM_THREADS=1 ASAN_OPTIONS=halt_on_error=1:detect_leaks=0:abort_on_error=0 \
    ./swatplus-asan > asan.log 2>&1
rc=$?
set -e
echo "rc=$rc"
if grep -q "ERROR: AddressSanitizer" asan.log; then
    echo "=== ASan caught it -> ROOT CAUSE (dynamic gfortran-11) ==="
    grep -n "ERROR: AddressSanitizer\|#[0-9]\+ .*\.f90\|SUMMARY:" asan.log | head -30
elif grep -q "Execution successfully completed" asan.log; then
    echo "=== dynamic gfortran-11 COMPLETED clean -> crash is STATIC-LINK-SPECIFIC ==="
    tail -4 asan.log
else
    echo "=== ended without ASan report or completion banner (rc=$rc) ==="
    tail -20 asan.log
fi
