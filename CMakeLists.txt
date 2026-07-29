# SWAT+ cmake, req min v3.20 (ifx, etc. support)
###############################################################
if("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}" OR
   "${CMAKE_SOURCE_DIR}/src" STREQUAL "${CMAKE_BINARY_DIR}")
   message(FATAL_ERROR "'in-source' builds not permitted, e.g. use 'cmake -B build' instead.")
endif()

cmake_minimum_required(VERSION 3.22)

# Query the host system information
cmake_host_system_information(RESULT os_id QUERY DISTRIB_ID)
cmake_host_system_information(RESULT os_version QUERY DISTRIB_RELEASE)
cmake_host_system_information(RESULT os_like QUERY DISTRIB_ID_LIKE)
cmake_host_system_information(RESULT pretty_name QUERY DISTRIB_PRETTY_NAME)
cmake_host_system_information(RESULT n QUERY NUMBER_OF_LOGICAL_CORES)

# Inject a global compiler launcher prefix that forces the environment variable down to the build phase
set(CMAKE_CROSSCOMPILING_EMULATOR "env" "CMAKE_BUILD_PARALLEL_LEVEL=${n}" CACHE INTERNAL "" FORCE)

project(swatplus VERSION 0.2 LANGUAGES Fortran)

include(CTest)

if(NOT TAG)
    find_package(Git REQUIRED)
    execute_process(
      COMMAND ${GIT_EXECUTABLE} describe --tags
      WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
      OUTPUT_VARIABLE TAG
      OUTPUT_STRIP_TRAILING_WHITESPACE
    )

    if(TAG STREQUAL "")
    # message(FATAL_ERROR "No tag info found.")
    # this happens when git is not installed or not in the path
      set(TAG "unknown")
    endif()
endif()


# --- Threading build options (multi-threading effort) ------------------------
# All default OFF so normal builds are unaffected.
#   -DSWAT_OPENMP=ON : compile/link with OpenMP
#   -DSWAT_TSAN=ON   : ThreadSanitizer build (implies OpenMP, requires dynamic link)
#   -DSWAT_ASAN=ON   : AddressSanitizer build (implies OpenMP, requires dynamic link)
#   -DSWAT_PROF=ON   : gprof instrumentation (-pg); run with OMP_NUM_THREADS=1 to profile
#   -DSWAT_LTO=ON    : Link-Time Optimization (-flto); don't combine with SWAT_PROF
option(SWAT_OPENMP "Build with OpenMP threading" OFF)
option(SWAT_TSAN   "Build with ThreadSanitizer (implies OpenMP + dynamic link)" OFF)
option(SWAT_ASAN   "Build with AddressSanitizer (implies OpenMP + dynamic link)" OFF)
option(SWAT_PROF   "Build with gprof instrumentation (-pg)" OFF)
option(SWAT_LTO    "Build with Link-Time Optimization (-flto)" OFF)
if(SWAT_TSAN)
    set(SWAT_OPENMP ON)
endif()
if(SWAT_ASAN)
    set(SWAT_OPENMP ON)
endif()
if(SWAT_LTO AND SWAT_PROF)
    message(WARNING "SWAT_LTO+SWAT_PROF together measured no LTO benefit: -pg's per-function "
                     "mcount() hooks appear to inhibit LTO's cross-module inlining (confirmed by "
                     "profiling a combined build -- the hot derived-type operator functions showed "
                     "no shrinkage vs a non-LTO -pg build). Use SWAT_LTO alone to measure its real "
                     "effect, and SWAT_PROF alone to profile.")
endif()
# -----------------------------------------------------------------------------

if (UNIX)
    if(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
        set(fdialect        "-fpp -free -fpe0 -traceback -diag-disable=10448")
        set(fdebug          "-warn all")
        set(frelease        "-O")
        set(FFC             "ifo")
        link_libraries("-static")
    elseif(CMAKE_Fortran_COMPILER_ID STREQUAL IntelLLVM)
        # Mirror the gfortran split (tmp/recommended_compiler_flags.md): checking/FP-trapping in
        # fdebug only; optimization in frelease. -init=zero matches gfortran -finit-local-zero
        # (this codebase relies on zeroed locals). -fp-model precise keeps Release reproducible.
        set(fdialect        "-fpp -free -traceback -init=zero")
        set(fdebug          "-O0 -g -warn all -check all -fpe0")
        set(frelease        "-O3 -xHost -fp-model precise")
        set(FFC             "ifx")
        link_libraries("-static")
    elseif(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
        # fdialect = always-on flags (cheap; correctness/reproducibility). Runtime checking and
        # FP-trapping moved to fdebug (Debug only); optimization set in frelease (Release only).
        # See tmp/recommended_compiler_flags.md for the rationale and measured impact.
        set(fdialect        "-cpp -fimplicit-none -ffree-line-length-none -fbacktrace -finit-local-zero -fno-unsafe-math-optimizations")
        set(fdebug          "-O0 -g -Wall -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow -fsignaling-nans -frounding-math")
        # -O3 -march=native gives ~3x over the old -O (=-O1) on this codebase. -march=native builds
        # for THIS machine's ISA (AVX2/FMA); for a portable/redistributable binary use a baseline
        # (e.g. -march=x86-64-v2). -ffp-contract=off keeps results bit-reproducible with the Debug
        # build; do NOT add -flto alongside -ffp-contract=off (miscompiles -> segfault).
        set(frelease        "-O3 -march=native -ffp-contract=off")
        set(FFC             "gnu")
        
        if(NOT APPLE)
            # Check if the OS is not Arch Linux or is not based on Arch Linux and is not Fedpra based
            # For non-Arch and non-FedoraLinux based systems using gfortran (GNU), set the link_library static flag to build a static executable.")
            # For Arch Linux based and Fedora based systems using gfortran, omit the link_library static flagt to build a dynamic executable.")
            if (NOT ("${os_id}" STREQUAL "Arch" OR 
                     "${os_like}" MATCHES ".*arch.*" OR
                     "${os_id}" STREQUAL "fedora" ))
                if(NOT SWAT_TSAN AND NOT SWAT_ASAN)   # sanitizers are incompatible with static linking
                    link_libraries("-static")
                endif()
            endif()
        endif()
    endif()

elseif(WIN32)
    if(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
        set(fdialect        "/fpp /free /fpe0 /traceback /Qdiag-disable:all /Qdiag-disable:remarks")
        set(fdebug          "/warn:all /check:bounds")
        set(frelease        "/O")
        set(FFC             "ifo")
        link_libraries("-static")
    elseif(CMAKE_Fortran_COMPILER_ID STREQUAL IntelLLVM)
        set(fdialect        "/fpp /free /fpe0 /traceback")
        set(fdebug          "/warn:all /check:bounds")
        set(frelease        "/O")
        set(FFC             "ifx")
    elseif(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
        # See tmp/recommended_compiler_flags.md: checking/FP-trapping -> Debug only; -O3 in Release.
        set(fdialect        "-cpp -fimplicit-none -ffree-line-length-none -fbacktrace -finit-local-zero -fno-unsafe-math-optimizations")
        set(fdebug          "-O0 -g -Wall -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow -fsignaling-nans -frounding-math")
        set(FFC             "gnu")
        # -march=native = build for this machine's ISA; use a baseline for portable binaries.
        # -ffp-contract=off keeps Release bit-reproducible with Debug (do NOT add -flto with it).
        set(frelease        "-O3 -march=native -ffp-contract=off")
        link_libraries("-static")
    endif()
endif()

# Get linker properties for later output.
get_property(CURRENT_LINK_LIBS DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY LINK_LIBRARIES)


string(TOLOWER ${CMAKE_HOST_SYSTEM_PROCESSOR} ARCH)
if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    set(AR "lin_${ARCH}")
elseif(CMAKE_SYSTEM_NAME STREQUAL "Windows")
    set(AR "win_${ARCH}")
elseif(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    set(AR "mac_${ARCH}")
else()
    set(AR "unknown")
endif()

# Default to an optimized Release build when no build type is given. The flag logic below matches
# the full word "Release" (and "Debug") -- note that -DCMAKE_BUILD_TYPE=Rel does NOT match and
# yields an unoptimized (-O0) build. Configure with -DCMAKE_BUILD_TYPE=Release (or =Debug).
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Build type" FORCE)
    message(STATUS "No CMAKE_BUILD_TYPE set -- defaulting to Release")
endif()

if(CMAKE_BUILD_TYPE STREQUAL "Release")
    set(TY "-Rel")
elseif(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(TY "-Dbg")
else()
    set(TY "")
endif()

# SWAT Version number
set(SWAT_VERSION ${TAG})
set(SWATPLUS_EXE "swatplus-${SWAT_VERSION}-${FFC}-${AR}${TY}")

# Set this to 'TRUE' to see the fortran command on compile
set(CMAKE_VERBOSE_MAKEFILE FALSE)

set(CMAKE_Fortran_FLAGS         "${CMAKE_Fortran_FLAGS} ${fdialect}")
set(CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG} ${fdebug}")
# set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${frelease}")
set(CMAKE_Fortran_FLAGS_RELEASE "${frelease}")

# --- OpenMP / ThreadSanitizer flags (multi-threading effort) ------------------
if(SWAT_OPENMP)
    # Static link + OpenMP: by default find_package(OpenMP) resolves the DYNAMIC OpenMP runtime
    # (gfortran libgomp.so, Intel libiomp5.so), which a -static link cannot use ("attempted static
    # link of dynamic object"). Make the library search prefer the static .a (libgomp.a / libiomp5.a,
    # shipped on glibc distros like Ubuntu and in oneAPI) BEFORE find_package runs, keeping .so as a
    # fallback so dynamic-only systems (e.g. Arch, which ships no libgomp.a) still resolve. Applies
    # to the compilers this project static-links: GNU (gfortran) and Intel (ifort/ifx).
    if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU|Intel")
        set(_saved_suffixes "${CMAKE_FIND_LIBRARY_SUFFIXES}")
        set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" ${CMAKE_FIND_LIBRARY_SUFFIXES})
    endif()
    find_package(OpenMP REQUIRED COMPONENTS Fortran)
    if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU|Intel")
        set(CMAKE_FIND_LIBRARY_SUFFIXES "${_saved_suffixes}")
    endif()
    string(APPEND CMAKE_Fortran_FLAGS " ${OpenMP_Fortran_FLAGS}")
endif()
if(SWAT_TSAN)
    # -O1 with -g keeps TSan traces usable. The -ffpe-trap from fdialect stays on;
    # revisit only if underflow traps fire spuriously under the TSan/OpenMP runtime.
    string(APPEND CMAKE_Fortran_FLAGS " -fsanitize=thread -g -O1")
    string(APPEND CMAKE_EXE_LINKER_FLAGS " -fsanitize=thread")
endif()
if(SWAT_ASAN)
    # Precisely locates heap corruption (buffer overflows, use-after-free) at the point
    # of occurrence, unlike TSan which floods with false positives on this codebase's
    # threadprivate/automatic access patterns (see tmp/threading_playbook.md Part 4).
    string(APPEND CMAKE_Fortran_FLAGS " -fsanitize=address -g -O0")
    string(APPEND CMAKE_EXE_LINKER_FLAGS " -fsanitize=address")
endif()
if(SWAT_PROF)
    # gprof's sampling profiler only reliably attributes time on the main thread (it
    # doesn't automatically instrument pthread-created OpenMP worker threads) -- run
    # profiling builds with OMP_NUM_THREADS=1 so the whole call graph lands on one thread
    # and the resulting flat profile is actually interpretable.
    string(APPEND CMAKE_Fortran_FLAGS " -pg -O2 -g")
    string(APPEND CMAKE_EXE_LINKER_FLAGS " -pg")
endif()
if(SWAT_LTO)
    # Cross-module inlining for the codebase's pervasive small derived-type arithmetic
    # operators (hyd_output/organic_mineral_mass +, *, called 100M-5B times per run) --
    # measured ~2.6-2.7% wall-clock improvement, consistent at 1 and 8 threads, on the
    # full 7951-HRU/cswat=1 dataset. Binary size also drops measurably (~13% smaller in
    # testing), consistent with real cross-module optimization happening. See SWAT_PROF's
    # warning above -- don't combine the two if you want to see LTO's actual effect.
    string(APPEND CMAKE_Fortran_FLAGS " -flto")
    string(APPEND CMAKE_EXE_LINKER_FLAGS " -flto")
endif()
# -----------------------------------------------------------------------------

#############################################################################
# Build
# list (SORT _variableNames)
# foreach (_variableName ${_variableNames})
#     message(STATUS "${_variableName}=${${_variableName}}")
# endforeach()

if(EXISTS "${PROJECT_SOURCE_DIR}/src/main.f90.in")
    string(TIMESTAMP ISO   "%Y-%m-%d %H:%M:%S")
    string(TIMESTAMP TODAY "%Y-%m-%d")  # e.g. produces 2023-12-07
    string(TIMESTAMP YEAR  "%Y")

    configure_file(
        "${PROJECT_SOURCE_DIR}/src/main.f90.in"
        "${PROJECT_SOURCE_DIR}/src/main.f90"
    )
endif()

if(EXISTS "${PROJECT_SOURCE_DIR}/ford.md.in")
    configure_file(
        "${PROJECT_SOURCE_DIR}/ford.md.in"
        "${PROJECT_SOURCE_DIR}/ford.md"
    )   
endif()


file(GLOB sources src/*.f90)
add_executable(${SWATPLUS_EXE} ${sources})

if(SWAT_OPENMP)
    target_link_libraries(${SWATPLUS_EXE} OpenMP::OpenMP_Fortran)
endif()

#############################################################################
# Install
install(TARGETS ${SWATPLUS_EXE} DESTINATION RUNTIME DESTINATION ${PROJECT_SOURCE_DIR})


#############################################################################
# Testing
find_package(Python3 QUIET)

set(spcheck "${PROJECT_SOURCE_DIR}/test/spcheck.py")

set(exe_path "${PROJECT_BINARY_DIR}/${SWATPLUS_EXE}")
set(test_dir "${PROJECT_BINARY_DIR}/data")
set(ref_dir  "${PROJECT_SOURCE_DIR}/data")

# error tolerances
set(rerr "0.01")
set(aerr "1e-8")

add_test(Ames_sub1   ${Python3_EXECUTABLE} ${spcheck} ctest ${exe_path} ${ref_dir}/Ames_sub1 ${test_dir} --abserr ${aerr} --relerr ${rerr})
# add_test(Ithaca_sub6 python3 ${check_py} ${exe_path} ${ref_dir}/Ithaca_sub6 ${test_dir} ${aerr} ${rerr})


#############################################################################
# Summary
message( "-- Verbose Makefile      ${CMAKE_VERBOSE_MAKEFILE}")
message( "-- Generator             ${CMAKE_GENERATOR_TOOLSET}")
message( "-- VS Platform           ${CMAKE_VS_PLATFORM_TOOLSET}")
message( "-- Detected OS ID:       ${os_id}")
message( "-- Detected OS Version:  ${os_version}")
message( "-- Detected OS Like:     ${os_like}")
message( "-- Detected Pretty Name: ${pretty_name}")
message( "-- Build Type            ${CMAKE_BUILD_TYPE}")
message( "-- Compiler              ${CMAKE_Fortran_COMPILER} (${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION})")
message( "-- Number of cores       ${n}")
message( "-- FFLAGS                ${CMAKE_Fortran_FLAGS}")
message( "--  RELEASE              ${CMAKE_Fortran_FLAGS_RELEASE}")
message( "--  DEBUG                ${CMAKE_Fortran_FLAGS_DEBUG}")
message( "-- Link_libraries        ${CURRENT_LINK_LIBS}")
message( "-- PROJECT_DIR           ${PROJECT_SOURCE_DIR}")
message( "-- BINARY_DIR            ${PROJECT_BINARY_DIR}")
message( "-- Install Prefix        ${CMAKE_INSTALL_PREFIX}")
message( "-- Executable            ${SWATPLUS_EXE}, rev/tag: ${SWAT_VERSION}")
message( "-- FORD project          ${PROJECT_SOURCE_DIR}/ford.md")
message( "-- Test Data Dir         ${ref_dir}")
message( "--  Abs Error            ${aerr}")
message( "--  Rel Error            ${rerr}")
