      subroutine hru_output_accum (ihru)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Accumulate the daily HRU output structures into the monthly accumulators and
!!    derive the daily two-point averages. This is the ACCUMULATION half of what used
!!    to be the front of hru_output; hru_output itself now holds the WRITE half plus
!!    the month/year/simulation rollups.
!!
!!    Why the split: this routine runs once per HRU per DAY and is dominated by
!!    derived-type operator overloads (the "+" on the output structs), which profiling
!!    showed to be the single largest cost class in the model. It performs no file I/O
!!    and touches only element (ihru) of each per-HRU array, so it is safe to run
!!    inside the parallel HRU loop - unlike hru_output, which must stay sequential
!!    because it writes files (see tmp/threading_playbook.md Part 13).
!!
!!    The month/year/simulation rollups deliberately stay in hru_output: they run 12,
!!    1 and 1 times per year respectively, so their cost is negligible next to the
!!    daily path, and keeping them with the writes avoids splitting the
!!    scale-then-print-then-reset ordering across two routines.
!!
!!    NOTE: this routine must NOT advance hwb_d(j)%sw_init / %sno_init. That advance
!!    has to happen after the daily record is written, and so lives in hru_output.

      use time_module
      use basin_module
      use output_landscape_module
      use soil_module
      use hru_module, only : hru

      implicit none

      integer, intent (in) :: ihru             !            |HRU number
      integer :: j                             !            |local HRU index
      real :: bm_max_m                         !kg/ha       |monthly bm_max, saved across the add
      real :: bm_max_y                         !kg/ha       |yearly bm_max, saved across the add

      j = ihru

      hwb_m(j) = hwb_m(j) + hwb_d(j)
      hnb_m(j) = hnb_m(j) + hnb_d(j)
      hls_m(j) = hls_m(j) + hls_d(j)
      bm_max_m = hpw_m(j)%bm_max         ! save off monthly bm_max value
      bm_max_y = hpw_y(j)%bm_max         ! save off yearly bm_max value
      bm_max_a_sv(j) = hpw_a(j)%bm_max   ! save off annual bm_max value - consumed in hru_output
      hpw_m(j) = hpw_m(j) + hpw_d(j)
      hpw_m(j)%bm_max = bm_max_m         ! restore monthly bm_max value
      hpw_d(j)%bm_max = hpw_d(j)%bioms
      hpw_m(j)%bm_max = Max(hpw_d(j)%bioms, hpw_m(j)%bm_max)
      hpw_y(j)%bm_max = Max(hpw_d(j)%bioms, hpw_y(j)%bm_max)
      hpw_a(j)%bm_max = Max(hpw_d(j)%bioms, hpw_a(j)%bm_max)

      hwb_d(j)%sw_final = soil(j)%sw
      hwb_d(j)%sw = (hwb_d(j)%sw_init + hwb_d(j)%sw_final) / 2.
      hwb_d(j)%sno_final = hru(j)%sno_mm
      hwb_d(j)%snopack = (hwb_d(j)%sno_init + hwb_d(j)%sno_final) / 2.

      return
      end subroutine hru_output_accum
