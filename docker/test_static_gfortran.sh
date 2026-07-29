#!/usr/bin/env bash
# Runs INSIDE the Ubuntu container (docker/Dockerfile.ubuntu-gfortran).
# 1. Builds SWAT+ (Release + OpenMP) and verifies the GNU Fortran/OpenMP runtimes are bundled into
#    the binary (SWAT_STATIC=libs, the default: static libgfortran/libgomp/libquadmath, dynamic
#    glibc) so it is portable to other Linux hosts -- the thing an Arch build cannot produce.
# 2. Runs a racoon dataset with that binary to prove it executes end to end. Fully static glibc
#    (SWAT_STATIC=full) links but segfaults here under gfortran 11; mostly-static runs clean.
#
# The build goes into /src/build-ubuntu, which is the mounted host tree (-v "$PWD":/src), so the
# executable persists on the host after the container exits. The racoon run happens on an ephemeral
# copy under /tmp so the host dataset stays clean.
#
# Usage (inside container):  bash docker/test_static_gfortran.sh [dataset_name]
#   dataset_name defaults to racoon_creek_120hru (smallest/fastest); other options live in
#   /src/workdata (racoon_creek_10pct, racoon_creek_mult-hru).
#   SWAT_STATIC env overrides the link mode (libs | full | off); default libs.
set -e
export HOME=/tmp
SRC=/src
BLD=/src/build-ubuntu                       # mounted host tree -> binary persists on the host
DATASET="${1:-racoon_creek_120hru}"
STATIC_MODE="${SWAT_STATIC:-libs}"
export OMP_NUM_THREADS="${OMP_NUM_THREADS:-4}"
git config --global --add safe.directory "$SRC" 2>/dev/null || true

echo "=== toolchain ==="
gfortran --version | head -1
cmake --version | head -1
echo "=== branch ==="
git -C "$SRC" rev-parse --abbrev-ref HEAD 2>/dev/null || echo "(no git)"

echo "=== static runtime libs present in the image ==="
for l in libgfortran.a libgomp.a libc.a libquadmath.a; do
    p=$(find /usr -name "$l" -print -quit 2>/dev/null)
    echo "  $l: ${p:-MISSING}"
done

echo "=== configure + build (Release, OpenMP, SWAT_STATIC=$STATIC_MODE) -> $BLD (persists on host) ==="
cmake -S "$SRC" -B "$BLD" -DCMAKE_BUILD_TYPE=Release -DSWAT_OPENMP=ON -DSWAT_STATIC="$STATIC_MODE"
cmake --build "$BLD" -j"$(nproc)"

E=$(ls "$BLD"/swatplus-* 2>/dev/null | grep -v '\.o$' | head -1)
echo "=== build result ==="
echo "binary: $E"
command -v file >/dev/null && file "$E"
echo "--- ldd (dynamic deps) ---"
ldd "$E" 2>&1 | sed 's/^/  /'

# A portable binary must NOT dynamically depend on the GNU Fortran/OpenMP runtimes -- those must be
# bundled. glibc (libc/libm/libpthread/libdl) staying dynamic is expected for mostly-static (libs)
# and is fine (present on every Linux); fully static (full) shows "not a dynamic executable".
lddall=$(ldd "$E" 2>&1)
leaked=$(echo "$lddall" | grep -Eo 'libgfortran|libgomp|libquadmath' | sort -u | paste -sd, -)
if echo "$lddall" | grep -qi "not a dynamic executable"; then
    echo "LINK CHECK: fully static (no dynamic deps) -- SUCCESS"
elif [ -z "$leaked" ]; then
    echo "LINK CHECK: Fortran/OpenMP runtimes bundled, only glibc dynamic -- SUCCESS (portable)"
else
    echo "LINK CHECK: runtime(s) still dynamic: $leaked -- NOT portable, investigate"
    exit 1
fi

# --- run the racoon dataset with the static binary -----------------------------------------------
DS="$SRC/workdata/$DATASET"
[ -d "$DS" ] || { echo "ERROR: dataset $DS not found"; exit 1; }
RUN="/tmp/run-$DATASET"                      # ephemeral copy so host workdata stays clean
echo "=== run $DATASET with the static binary (OMP_NUM_THREADS=$OMP_NUM_THREADS) ==="
rm -rf "$RUN"; mkdir -p "$RUN"
cp -a "$DS/." "$RUN/"
cp "$E" "$RUN/swatplus-static"
cd "$RUN"
t0=$(date +%s)
set +e
./swatplus-static > run.log 2>&1
rc=$?
set -e
echo "exit code: $rc   wall: $(( $(date +%s) - t0 ))s"
echo "--- last 8 lines of run.log ---"
tail -8 run.log

if [ "$rc" -eq 0 ] && grep -q "Execution successfully completed" run.log; then
    echo "RACOON RUN ($DATASET) ON STATIC UBUNTU BINARY: SUCCESS"
else
    echo "RACOON RUN ($DATASET) ON STATIC UBUNTU BINARY: FAILED (rc=$rc, no completion banner)"
    set +e
    # --- diagnostics ---------------------------------------------------------------------------
    # The -O3 backtrace mis-symbolizes to the nearest symbol. Build a -O0 -g -fcheck=all Debug
    # binary (accurate frames; bounds checks may pinpoint the line) and run that under gdb.
    # This crash is at startup (input reading), before any model FP, so Debug's -ffpe-trap
    # cannot fire first.
    echo "=== probe: OMP_NUM_THREADS=1 with the -O3 static binary ==="
    OMP_NUM_THREADS=1 ./swatplus-static > run1.log 2>&1
    echo "OMP=1 rc=$?"; tail -3 run1.log

    DBG=/src/build-ubuntu-dbg
    echo "=== building -O0 -g -fcheck=all static (Debug) into $DBG for an accurate backtrace ==="
    cmake -S "$SRC" -B "$DBG" -DCMAKE_BUILD_TYPE=Debug -DSWAT_OPENMP=ON >/tmp/dbgcfg.log 2>&1
    cmake --build "$DBG" -j"$(nproc)" >/tmp/dbgbuild.log 2>&1 \
        && echo "debug build OK" || { echo "debug build FAILED"; tail -15 /tmp/dbgbuild.log; }
    ED=$(ls "$DBG"/swatplus-* 2>/dev/null | grep -v '\.o$' | head -1)
    cp "$ED" ./swatplus-debug
    echo "=== run Debug binary (OMP_NUM_THREADS=1) — -fcheck=all may name the line ==="
    OMP_NUM_THREADS=1 ./swatplus-debug > rundbg.log 2>&1
    echo "debug rc=$?"; tail -8 rundbg.log
    if command -v gdb >/dev/null; then
        echo "=== gdb symbolic backtrace on Debug binary (OMP_NUM_THREADS=1) ==="
        OMP_NUM_THREADS=1 gdb -q -batch \
            -ex "set pagination off" -ex run -ex "bt 25" \
            ./swatplus-debug 2>&1 | tail -45
    fi
    set -e
    exit 1
fi
