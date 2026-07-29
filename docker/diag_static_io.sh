#!/usr/bin/env bash
# Isolate the static-gfortran CLOSE segfault seen running racoon.
# SWAT+ crashes in libgfortran's close_unit (readcio_read.f90:109, a plain CLOSE) only when the
# binary is fully static AND built with OpenMP. Hypothesis: -fopenmp selects libgfortran's
# THREAD-SAFE I/O path, which locks the unit via pthread mutexes referenced as WEAK symbols; in a
# fully static link those weak refs resolve to NULL, so close_unit calls through a null pointer.
#
# Build a ~10-line open/read/close program in several link modes to pin it down, and test the fix
# (force-link the whole pthread archive so the weak refs resolve).
set -e
export HOME=/tmp
D=/tmp/iodiag
rm -rf "$D"; mkdir -p "$D"; cd "$D"
gfortran --version | head -1

cat > t.f90 <<'EOF'
program t
  implicit none
  character(len=80) :: l
  integer :: eof
  open(107, file="t.tmp", status="replace")
  write(107,*) "hello world"
  close(107)
  open(107, file="t.tmp")
  read(107,'(A)',iostat=eof) l
  close(107)                 ! <-- the CLOSE that segfaults in the static+OpenMP SWAT+ binary
  print *, "CLOSE OK: ", trim(l)
end program t
EOF

run () {  # $1 = label, rest = gfortran args
    local label="$1"; shift
    printf '%-34s ' "$label:"
    if gfortran -O0 -g "$@" t.f90 -o bin 2>/tmp/ld.err; then
        set +e; ./bin >/tmp/out 2>&1; local rc=$?; set -e
        if [ $rc -eq 0 ]; then echo "rc=0  OK  ($(tail -1 /tmp/out | tr -s ' '))"
        else echo "rc=$rc  <-- CRASH"; fi
    else
        echo "BUILD FAILED"; sed 's/^/    /' /tmp/ld.err | head -4
    fi
}

echo "=== link-mode matrix ==="
run "dynamic, no omp"
run "static,  no omp"                 -static
run "dynamic, -fopenmp"               -fopenmp
run "static,  -fopenmp"               -static -fopenmp
echo "=== candidate fixes for static + -fopenmp ==="
run "static -fopenmp whole-archive"   -static -fopenmp -Wl,--whole-archive -lpthread -Wl,--no-whole-archive
run "static -fopenmp -pthread"        -static -fopenmp -pthread

echo
echo "Expected: the 'static, -fopenmp' row CRASHES (rc=139); a fix row shows rc=0 OK."
