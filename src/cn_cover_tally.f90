      subroutine cn_cover_tally (j)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    add today's surface runoff for one HRU to its frozen or unfrozen total,
!!    and on the last day of the run write the totals to cn_cover_sum.out.
!!    curve 2 is tuned so that unfrozen-day runoff matches the static run
!!    (curve 3): on frozen days sq_dailycn's cn_froz branch sets the curve
!!    number almost regardless of cover, so those days are not cover's to match.
!!
!!    frozen uses the same test sq_dailycn applies: soil layer 2 at or below 0 C.
!!    called from surface after surfq(j) is final.  each HRU writes its own
!!    line on the last day, so wetland HRUs (which skip surface) are simply
!!    absent rather than miscounted.

      use cn_cover_module
      use hru_module, only : hru, surfq
      use soil_module, only : soil
      use time_module, only : time

      implicit none

      integer, intent (in) :: j             !none  |HRU number

      if (.not. allocated (cn_cov_hru)) return

      if (soil(j)%phys(2)%tmp <= 0.) then
        cn_cov_hru(j)%q_frz = cn_cov_hru(j)%q_frz + surfq(j)
      else
        cn_cov_hru(j)%q_unf = cn_cov_hru(j)%q_unf + surfq(j)
      end if

      if (time%end_sim == 1 .and. time%day == time%day_end_yr) then
        write (cn_sum_unit,1000) j, cn_cov_hru(j)%wide, hru(j)%area_ha, cn_cov_hru(j)%cn_tbl,  &
                                 cn_cov_hru(j)%cn_hi, cn_cov_hru(j)%cn_mid, cn_cov_hru(j)%q_unf,  &
                                 cn_cov_hru(j)%q_frz
      end if

1000  format (i8,l6,f15.4,3f12.3,2f13.3)

      return
      end subroutine cn_cover_tally
