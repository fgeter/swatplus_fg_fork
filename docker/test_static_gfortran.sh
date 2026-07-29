#!/usr/bin/env bash
# Runs INSIDE the Ubuntu container (docker/Dockerfile.ubuntu-gfortran).
# Builds SWAT+ with gfortran (Release + OpenMP) and verifies the CMakeLists produces a
# STATICALLY linked binary on Ubuntu -- the thing Arch cannot do (no static libgfortran/libgomp).
set -e
export HOME=/tmp
SRC=/src
BLD=/tmp/build-ubuntu
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

echo "=== configure + build (Release, OpenMP) ==="
rm -rf "$BLD"
cmake -S "$SRC" -B "$BLD" -DCMAKE_BUILD_TYPE=Release -DSWAT_OPENMP=ON
cmake --build "$BLD" -j"$(nproc)"

E=$(ls "$BLD"/swatplus-* 2>/dev/null | grep -v '\.o$' | head -1)
echo "=== result ==="
echo "binary: $E"
command -v file >/dev/null && file "$E"
lddout=$(ldd "$E" 2>&1 | head -1)
echo "ldd: $lddout"

# Static-link detection. Prefer file(1) when present; otherwise fall back to ldd,
# which prints "not a dynamic executable" for a statically linked binary.
if command -v file >/dev/null && file "$E" | grep -q "statically linked"; then
    echo "STATIC gfortran BUILD ON UBUNTU: SUCCESS"
elif echo "$lddout" | grep -qi "not a dynamic executable"; then
    echo "STATIC gfortran BUILD ON UBUNTU: SUCCESS (via ldd)"
else
    echo "STATIC gfortran BUILD ON UBUNTU: NOT static -- investigate"
fi
