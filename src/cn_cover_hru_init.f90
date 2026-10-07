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
!!
!!    ~ ~ ~ MODULE VARIABLES (cn_cover_module) ~ ~ ~
!!    read:
!!      cn_key            each cntable.lum row's (fam,trt,cond) - cn_cover_init pass 1
!!      cn_row            (fam,trt,cond) -> cntable.lum row - cn_cover_init pass 2
!!      cn_fam            family names - cn_cover_init pass 1
!!    written:
!!      cn_cov_hru(j)     allocated on the first call.  hyd, fam_lum, trt_lum and
!!                        active are set here and only read by cn_cover_update.
!!                        cn_last and off are started here; cn_cover_update then
!!                        carries them from one day to the next.

      use basin_module, only : bsn_cc
      use hru_module, only : hru, cn2
      use soil_module, only : sol
      use landuse_data_module, only : lum_str, cn
      use hydrograph_module, only : sp_ob
      use plant_module, only : pcom
      use time_module, only : time
      use cn_cover_module

      implicit none

      integer, intent (in) :: j             !none  |HRU number
      integer :: ilum = 0                   !none  |land use number
      integer :: isol = 0                   !none  |soil number
      integer :: icn = 0                    !none  |cntable.lum row of this land use
      integer :: ifam = 0                   !none  |family index of that row
      integer :: itrt = 0                   !none  |treatment index of that row
      integer :: icond = 0                  !none  |hydrologic condition of that row
      integer :: iref = 0                   !none  |cntable.lum row of the straight-row equivalent
      integer :: npl = 0                    !none  |plants in the community

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
      cn_cov_hru(j)%wide = .false.
      cn_cov_hru(j)%cn_tbl = 0.
      cn_cov_hru(j)%cn_hi = 0.
      cn_cov_hru(j)%cn_mid = 0.
      if (icn >= 1) cn_cov_hru(j)%cn_tbl = cn(icn)%cn(cn_cov_hru(j)%hyd)

      select case (cn_curve)
      case (1)
        if (ifam >= 1 .and. itrt >= 1) then
          if (cn_row(ifam,itrt,cn_cond_poor) > 0 .and. cn_row(ifam,itrt,cn_cond_good) > 0) then
            cn_cov_hru(j)%active = .not. fam_is_static (cn_fam(ifam))
          end if
        end if

      case (2, 3)
        !! every HRU with a cntable.lum row is re-seated daily - at its table
        !! value unless curve 2 applies - so both runs take the same path
        cn_cov_hru(j)%active = icn >= 1
        select case (cn_curve)
        case (2)
          !! curve 2 needs a straight-row row of the same family and condition
          !! to measure the treatment offset against: row crops, small grains,
          !! legumes.  pasture, woods, urban and fallow stay at the table value
          icond = 0
          if (icn >= 1 .and. allocated (cn_key)) icond = cn_key(icn)%cond
          iref = 0
          if (ifam >= 1 .and. itrt >= 1 .and. icond >= cn_cond_poor .and. i_hi >= 1) then
            select case (off_ref)
            case (1)
              if (trt_sr(itrt) >= 1) iref = cn_row(ifam,trt_sr(itrt),icond)
            case (2)
              if (trt_sr0(itrt) >= 1) iref = cn_row(ifam,trt_sr0(itrt),icond)
            end select
          end if
          if (iref >= 1) then
            cn_cov_hru(j)%wide = .true.
            cn_cov_hru(j)%cn_hi = cn(i_hi)%cn(cn_cov_hru(j)%hyd)                  &
                                + (cn_cov_hru(j)%cn_tbl - cn(iref)%cn(cn_cov_hru(j)%hyd))
            if (i_mid >= 1) then
              cn_cov_hru(j)%cn_mid = cn(i_mid)%cn(cn_cov_hru(j)%hyd)              &
                                   + (cn_cov_hru(j)%cn_tbl - cn(iref)%cn(cn_cov_hru(j)%hyd))
            else
              cn_cov_hru(j)%cn_mid = cn_cov_hru(j)%cn_tbl
            end if
          end if

          !! per-crop family.  the land-use row is decomposed into layout,
          !! residue and condition, which every crop's row then shares; the
          !! anchors start as the land-use row's and hold until a crop is
          !! planted.  a land use changed mid-run starts its clocks again
          cn_cov_hru(j)%icn = icn
          cn_cov_hru(j)%b_tbl = cn_cov_hru(j)%cn_tbl
          cn_cov_hru(j)%b_hi = cn_cov_hru(j)%cn_hi
          cn_cov_hru(j)%b_mid = cn_cov_hru(j)%cn_mid
          cn_cov_hru(j)%lay = 0
          cn_cov_hru(j)%res = 0
          cn_cov_hru(j)%cond = icond
          select case (crop_fam)
          case (1)
            if (cn_cov_hru(j)%wide) then
              cn_cov_hru(j)%lay = trt_lay(itrt)
              cn_cov_hru(j)%res = trt_res(itrt)
              if (cn_cov_hru(j)%lay < 1 .and. .not. crop_noted(icn,0)) then
                crop_noted(icn,0) = .true.
                write (9001,*) "NOTE: cn_cover crop_fam: land use cn row ", trim (cn(icn)%name),     &
                  " has a treatment outside the editor vocabulary; its crops use this row"
              end if
              npl = pcom(j)%npl
              if (allocated (cn_cov_hru(j)%seen)) deallocate (cn_cov_hru(j)%seen)
              if (allocated (cn_cov_hru(j)%gro_prev)) deallocate (cn_cov_hru(j)%gro_prev)
              if (allocated (cn_cov_hru(j)%yr_p)) deallocate (cn_cov_hru(j)%yr_p)
              if (allocated (cn_cov_hru(j)%day_p)) deallocate (cn_cov_hru(j)%day_p)
              allocate (cn_cov_hru(j)%seen(npl))
              allocate (cn_cov_hru(j)%gro_prev(npl))
              allocate (cn_cov_hru(j)%yr_p(npl))
              allocate (cn_cov_hru(j)%day_p(npl))
              cn_cov_hru(j)%seen = .false.
              cn_cov_hru(j)%gro_prev = .false.
              cn_cov_hru(j)%yr_p = time%yrc
              cn_cov_hru(j)%day_p = time%day
            end if
          end select
        end select
      end select

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
      !! q_unf and q_frz are NOT reset: this routine also runs on a land use
      !! change, and the runoff tally covers the whole simulation

      return
      end subroutine cn_cover_hru_init
