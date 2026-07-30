#!/usr/bin/env bash
# Runs INSIDE the Ubuntu container (docker/Dockerfile.ubuntu-gfortran).
# 1. Builds SWAT+ (Release + OpenMP) and verifies the Fortran/OpenMP runtimes are bundled into the
#    binary (SWAT_STATIC=libs, the default: static libgfortran/libgomp/libquadmath for GNU, or
#    -static-intel/-qopenmp-link=static for ifx; glibc stays dynamic) so it is portable to other
#    Linux hosts -- the thing an Arch build cannot produce.
# 2. Runs a racoon dataset with that binary to prove it executes end to end. Fully static glibc
#    (SWAT_STATIC=full) links but segfaults here under gfortran 11; mostly-static runs clean.
#
# The build goes into /src/build-ubuntu[-ifx], which is the mounted host tree (-v "$PWD":/src), so
# the executable persists on the host after the container exits. The racoon run happens on an
# ephemeral copy under /tmp so the host dataset stays clean.
#
# Usage (inside container):  bash docker/test_static_gfortran.sh [dataset_name]
#   dataset_name defaults to racoon_creek_120hru (smallest/fastest); other options live in
#   /src/workdata (racoon_creek_10pct, racoon_creek_mult-hru).
#   SWAT_FC     env selects the compiler: gnu (default, gfortran) | ifx (Intel oneAPI). The two use
#               SEPARATE build dirs, so alternating between them does not force a full rebuild and
#               both binaries persist on the host for side-by-side timing.
#   SWAT_STATIC env overrides the link mode (libs | full | off); default libs.
#   SWAT_MARCH  env overrides the ISA target; default x86-64-v3 (portable AVX2/FMA baseline:
#               Intel Haswell+ 2013, AMD Excavator+ / all Zen 2015+). NOT native -- the whole point
#               of this build is a binary that runs on hosts other than the build machine. Use
#               x86-64-v2 for pre-2013 Intel / pre-2015 AMD targets, or native to benchmark locally.
#               CMakeLists maps native -> -xHost for ifx; other values pass through as -march=.
#
# Comparing compilers (same ISA + same FP policy -> a fair wall-clock comparison):
#   SWAT_FC=gnu bash docker/test_static_gfortran.sh racoon_creek_10pct
#   SWAT_FC=ifx bash docker/test_static_gfortran.sh racoon_creek_10pct
# Compare the reported wall times. Do NOT expect bitwise-identical model output between the two:
# they use different vectorizers and different math libraries (glibc libm vs Intel libimf), so
# transcendentals differ in the last bits. Validate results within tolerance, not bitwise.
# (Release-vs-Debug bit-reproducibility DOES hold within a single compiler -- that is what
# -ffp-contract=off / -fp-model=precise protect.)
set -e
export HOME=/tmp
SRC=/src
DATASET="${1:-racoon_creek_120hru}"
FC_KIND="${SWAT_FC:-gnu}"
STATIC_MODE="${SWAT_STATIC:-libs}"
MARCH="${SWAT_MARCH:-x86-64-v3}"
export OMP_NUM_THREADS="${OMP_NUM_THREADS:-4}"
git config --global --add safe.directory "$SRC" 2>/dev/null || true

# Per-compiler settings. RT_LIBS = static runtimes we expect the image to provide; LEAK_RE = the
# runtime .so names that must NOT remain dynamic deps (the portability check). glibc is excluded
# from LEAK_RE on purpose: it is expected to stay dynamic in the "libs" mode.
case "$FC_KIND" in
    gnu)
        FC_BIN=gfortran
        BLD=/src/build-ubuntu               # mounted host tree -> binary persists on the host
        CMAKE_FC_ARG=()                     # let CMake find its default (resolves to /usr/bin/f95)
        RT_LIBS="libgfortran.a libgomp.a libc.a libquadmath.a"
        LEAK_RE='libgfortran|libgomp|libquadmath'
        ;;
    ifx)
        FC_BIN=ifx
        BLD=/src/build-ubuntu-ifx
        CMAKE_FC_ARG=(-DCMAKE_Fortran_COMPILER=ifx)
        # oneAPI ships these under /opt/intel, not /usr, so the find below searches both roots.
        RT_LIBS="libifcore.a libiomp5.a libc.a"
        LEAK_RE='libifcore|libifport|libimf|libintlc|libsvml|libiomp5'
        ;;
    *)
        echo "ERROR: SWAT_FC must be 'gnu' or 'ifx' (got '$FC_KIND')"; exit 1 ;;
esac

command -v "$FC_BIN" >/dev/null || { echo "ERROR: $FC_BIN not found on PATH in this image"; exit 1; }

echo "=== toolchain (SWAT_FC=$FC_KIND) ==="
"$FC_BIN" --version | head -1
cmake --version | head -1
echo "=== branch ==="
git -C "$SRC" rev-parse --abbrev-ref HEAD 2>/dev/null || echo "(no git)"

echo "=== static runtime libs present in the image ==="
for l in $RT_LIBS; do
    p=$(find /usr /opt/intel -name "$l" -print -quit 2>/dev/null)
    echo "  $l: ${p:-MISSING}"
done

echo "=== configure + build ($FC_KIND, Release -O3, OpenMP, SWAT_STATIC=$STATIC_MODE, ISA=$MARCH) -> $BLD (persists on host) ==="
cmake -S "$SRC" -B "$BLD" "${CMAKE_FC_ARG[@]}" -DCMAKE_BUILD_TYPE=Release -DSWAT_OPENMP=ON \
      -DSWAT_STATIC="$STATIC_MODE" -DSWAT_MARCH="$MARCH"
cmake --build "$BLD" -j"$(nproc)"

E=$(ls "$BLD"/swatplus-* 2>/dev/null | grep -v '\.o$' | head -1)
echo "=== build result ==="
echo "binary: $E"
command -v file >/dev/null && file "$E"
echo "--- ldd (dynamic deps) ---"
ldd "$E" 2>&1 | sed 's/^/  /'

# A portable binary must NOT dynamically depend on the Fortran/OpenMP runtimes -- those must be
# bundled. Which names count as a leak is compiler-specific (LEAK_RE above): libgfortran/libgomp/
# libquadmath for GNU, the libifcore/libimf/libiomp5 family for ifx. glibc (libc/libm/libpthread/
# libdl) staying dynamic is expected for mostly-static (libs) and is fine (present on every Linux);
# libgcc_s may also appear under ifx and is likewise fine. Fully static (full) shows "not a dynamic
# executable". ldd exits non-zero on a fully static binary; || true so set -e does not abort the
# assignment before we reach the run.
lddall=$(ldd "$E" 2>&1 || true)
leaked=$(echo "$lddall" | grep -Eo "$LEAK_RE" | sort -u | paste -sd, - || true)
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
RUN="/tmp/run-$FC_KIND-$DATASET"             # ephemeral, per-compiler so the two never collide
echo "=== run $DATASET with the $FC_KIND static binary (OMP_NUM_THREADS=$OMP_NUM_THREADS) ==="
rm -rf "$RUN"; mkdir -p "$RUN"
cp -a "$DS/." "$RUN/"
cp "$E" "$RUN/swatplus-static"
cd "$RUN"
# Millisecond resolution: whole seconds are too coarse to compare compilers on the smaller
# datasets (racoon_creek_120hru finishes in ~1s).
t0=$(date +%s%N)
set +e
./swatplus-static > run.log 2>&1
rc=$?
set -e
wall_ms=$(( ( $(date +%s%N) - t0 ) / 1000000 ))
echo "exit code: $rc   wall: $(awk "BEGIN{printf \"%.3f\", $wall_ms/1000}")s"
# Single-line, greppable summary for scripted comparison across compilers/thread counts.
echo "TIMING: fc=$FC_KIND dataset=$DATASET threads=$OMP_NUM_THREADS march=$MARCH static=$STATIC_MODE wall_ms=$wall_ms"
echo "--- last 8 lines of run.log ---"
tail -8 run.log

if [ "$rc" -eq 0 ] && grep -q "Execution successfully completed" run.log; then
    echo "RACOON RUN ($DATASET) ON STATIC UBUNTU $FC_KIND BINARY: SUCCESS"
else
    echo "RACOON RUN ($DATASET) ON STATIC UBUNTU $FC_KIND BINARY: FAILED (rc=$rc, no completion banner)"
    set +e
    # --- diagnostics ---------------------------------------------------------------------------
    # The -O3 backtrace mis-symbolizes to the nearest symbol. Build a -O0 -g -fcheck=all Debug
    # binary (accurate frames; bounds checks may pinpoint the line) and run that under gdb.
    # This crash is at startup (input reading), before any model FP, so Debug's -ffpe-trap
    # cannot fire first.
    echo "=== probe: OMP_NUM_THREADS=1 with the -O3 static binary ==="
    OMP_NUM_THREADS=1 ./swatplus-static > run1.log 2>&1
    echo "OMP=1 rc=$?"; tail -3 run1.log

    DBG=/src/build-ubuntu-dbg-$FC_KIND
    echo "=== building -O0 -g (Debug, $FC_KIND) into $DBG for an accurate backtrace ==="
    cmake -S "$SRC" -B "$DBG" "${CMAKE_FC_ARG[@]}" -DCMAKE_BUILD_TYPE=Debug -DSWAT_OPENMP=ON >/tmp/dbgcfg.log 2>&1
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
