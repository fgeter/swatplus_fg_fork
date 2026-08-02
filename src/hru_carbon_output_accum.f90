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

      implicit none

      integer, intent (in) :: ihru             !            |HRU number
      integer :: j                             !            |local HRU index

      j = ihru

      hsc_m(j)  = hsc_m(j)  + hsc_d(j)
      hrc_m(j)  = hrc_m(j)  + hrc_d(j)
      hpc_m(j)  = hpc_m(j)  + hpc_d(j)
      hscf_m(j) = hscf_m(j) + hscf_d(j)

      return
      end subroutine hru_carbon_output_accum
