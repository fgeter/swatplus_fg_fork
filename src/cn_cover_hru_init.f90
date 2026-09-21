      subroutine cn_cover_hru_init (j)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    cache the row set the cover method interpolates within for one HRU, and
!!    reset the external-offset bookkeeping.  called from cn2_init, so it runs
!!    once per HRU at startup and again whenever a lu_change d-table action
!!    re-seats the land use (actions.f90).
!!
!!    the HRU takes part only if the land use's own cntable.lum row has both a
!!    poor and a good variant for its own treatment.  that is what keeps urban,
!!    farm, pasth, the roads and fal_bare static: the land use, not the plant,
!!    decides whether this HRU's curve number is condition-dependent at all.
!!    within a participating HRU the growing plants may still pull the family
!!    sideways (a corn/soybean/wheat rotation moves rc -> rc -> sg).

      use basin_module, only : bsn_cc
      use hru_module, only : hru, cn2
      use soil_module, only : sol
      use landuse_data_module, only : lum_str
      use hydrograph_module, only : sp_ob
      use cn_cover_module

      implicit none

      integer, intent (in) :: j             !none  |HRU number
      integer :: ilum = 0                   !none  |land use number
      integer :: isol = 0                   !none  |soil number
      integer :: icn = 0                    !none  |cntable.lum row of this land use
      integer :: ifam = 0                   !none  |family index of that row
      integer :: itrt = 0                   !none  |treatment index of that row

      select case (bsn_cc%cn)
      case (0)
        return                              !! feature off - cn_cover_init allocated nothing
      end select

      if (.not. allocated (cn_cov_hru)) allocate (cn_cov_hru(sp_ob%hru))

      ilum = hru(j)%land_use_mgt
      isol = hru(j)%dbs%soil
      icn = lum_str(ilum)%cn_lu

      !! hydrologic soil group as a cn(:)%cn subscript - the same mapping
      !! cn2_init uses to pick the static curve number
      select case (sol(isol)%s%hydgrp)
      case ("A")
        cn_cov_hru(j)%hyd = 1
      case ("B")
        cn_cov_hru(j)%hyd = 2
      case ("C")
        cn_cov_hru(j)%hyd = 3
      case ("D")
        cn_cov_hru(j)%hyd = 4
      case default
        cn_cov_hru(j)%hyd = 2
      end select

      ifam = 0
      itrt = 0
      if (icn >= 1 .and. allocated (cn_key)) then
        ifam = cn_key(icn)%fam
        itrt = cn_key(icn)%trt
      end if
      cn_cov_hru(j)%fam_lum = ifam
      cn_cov_hru(j)%trt_lum = itrt

      cn_cov_hru(j)%active = .false.
      if (ifam >= 1 .and. itrt >= 1) then
        if (cn_row(ifam,itrt,cn_cond_poor) > 0 .and. cn_row(ifam,itrt,cn_cond_good) > 0) then
          cn_cov_hru(j)%active = .not. fam_is_static (cn_fam(ifam))
        end if
      end if

      !! cn2_init has just written the table value and called curno.  start the
      !! offset ledger from there: anything that moves cn2 afterwards
      !! (calibration.cal, the cnup management operation, the cn_update d-table
      !! action, pl_burnop) shows up as a difference from cn_last on the next
      !! daily pass and is carried forward rather than overwritten.  a land use
      !! change replaces the whole base, so the ledger restarts with it.
      cn_cov_hru(j)%cn_last = cn2(j)
      cn_cov_hru(j)%off = 0.
      cn_cov_hru(j)%cn_sel = cn2(j)
      cn_cov_hru(j)%c_rsd = 0.
      cn_cov_hru(j)%c_bio = 0.
      cn_cov_hru(j)%c_tot = 0.

      return
      end subroutine cn_cover_hru_init
