      subroutine hru_carbon_output_accum (ihru)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Accumulate the daily HRU carbon output structures into the monthly accumulators.
!!    This is the ACCUMULATION half of what used to be the front of hru_carbon_output;
!!    hru_carbon_output itself now holds the WRITE half plus the month/year/simulation
!!    rollups.
!!
!!    Same split, and for the same reason, as hru_output_accum - see the comment there.
!!
!!    Measured on racoon_creek_mult-hru (7949 HRU, 2 yr) at 12 threads: removing the
!!    hru_carbon_output call outright takes ~3.4 s off a 28.2 s run, but only ~0.3-0.7 s
!!    of that is the four adds below. The rest - roughly 2.7-3.1 s, about 10% of the run -
!!    is the month / year / avann FILE WRITING that stays in hru_carbon_output: 24 months
!!    x 7949 HRUs x 4 structs x (txt + csv) is ~1.5 M list-directed write statements.
!!
!!    So this split is worth doing but is not where the remaining time is. The next real
!!    target in this path is the formatted output itself, not the accumulation.
!!
!!    These four adds are unconditional - they do not depend on any print flag, and they
!!    run whatever bsn_cc%cswat is set to. No file I/O, and only element (ihru) of each
!!    per-HRU array is touched, so this is safe inside the parallel HRU loop (unlike
!!    hru_carbon_output, which writes files - see tmp/threading_playbook.md Part 13).

      use carbon_module
      use time_module
      use basin_module
      use hydrograph_module, only : sp_ob1, ob

      implicit none

      integer, intent (in) :: ihru             !            |HRU number
      integer :: j                             !            |local HRU index
      integer :: iob                           !            |object number for this HRU

      j = ihru

      hsc_m(j)  = hsc_m(j)  + hsc_d(j)
      hrc_m(j)  = hrc_m(j)  + hrc_d(j)
      hpc_m(j)  = hpc_m(j)  + hpc_d(j)
      hscf_m(j) = hscf_m(j) + hscf_d(j)

      !! Pre-format this HRU's monthly output records into cb_mon_buf, so that
      !! hru_carbon_output only has to emit an already-built string.
      !!
      !! This is where the real time is. Measured on racoon_creek_mult-hru (7949 HRU,
      !! 2 yr) at 12 threads, the HRU output writes cost 13.3 s of a 27.5 s run, and ~83%
      !! of that is the number-to-text formatting rather than the write statement or the
      !! I/O. Formatting touches no shared state, so it belongs on this side of the split.
      !!
      !! Safe to do here because the four adds above are the LAST writes to hsc_m(j) etc.
      !! for the month - nothing between this parallel loop and hru_carbon_output's
      !! monthly block touches them, so the values formatted here are final.
      !!
      !! Only the monthly records are pre-formatted. Yearly and average-annual fire once
      !! and three times respectively over a whole run, against 12 times a year for
      !! monthly, so they are left in hru_carbon_output where they are easier to read.
      !!
      !! An internal list-directed write produces byte-identical text to the external
      !! list-directed write it replaces (verified), so output is unchanged.
      if (time%end_mo == 1 .and. pco%nb_hru%m == "y") then
        iob = sp_ob1%hru + j - 1
        write (cb_mon_buf(1,j),*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_m(j)
        write (cb_mon_buf(2,j),*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hrc_m(j)
        write (cb_mon_buf(3,j),*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpc_m(j)
        write (cb_mon_buf(4,j),*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_m(j)
        if (pco%csvout == "y") then
          write (cb_mon_buf(5,j),'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hsc_m(j)
          write (cb_mon_buf(6,j),'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hrc_m(j)
          write (cb_mon_buf(7,j),'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpc_m(j)
          write (cb_mon_buf(8,j),'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hscf_m(j)
        end if
      end if

      return
      end subroutine hru_carbon_output_accum
