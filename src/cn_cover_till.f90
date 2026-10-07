      subroutine cn_cover_till (j, idtill)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    restart the stand clock of every plant on HRU j after a tillage pass whose
!!    mixing efficiency reaches till_reset.  a grass stand in a rotation ages
!!    from rotation meadow toward its mature row (PER-CROP FAMILY in
!!    cn_cover_module); tillage that breaks the sod starts it again.  called
!!    after the tillage operation in mgt_sched and the d-table till action.
!!    a no-op unless curve 2 runs with crop_fam 1.

      use cn_cover_module
      use tillage_data_module, only : tilldb
      use time_module, only : time

      implicit none

      integer, intent (in) :: j             !none  |HRU number
      integer, intent (in) :: idtill        !none  |tillage number in tillage.til

      if (.not. allocated (cn_cov_hru)) return
      if (cn_curve /= 2 .or. crop_fam /= 1) return
      if (idtill < 1) return
      if (.not. allocated (cn_cov_hru(j)%yr_p)) return
      if (tilldb(idtill)%effmix < till_reset) return

      cn_cov_hru(j)%yr_p = time%yrc
      cn_cov_hru(j)%day_p = time%day

      return
      end subroutine cn_cover_till
