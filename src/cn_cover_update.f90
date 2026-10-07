      subroutine cn_cover_update (j)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    re-select cn2 from surface cover for one HRU and rebuild the retention
!!    curve, once per day, immediately before sq_dailycn.
!!
!!    ~ ~ ~ THE FOUR STAGES ~ ~ ~
!!    1  residue cover      c_rsd = 1 - exp(-k_rsd * abg_rsd_tot)
!!    2  near-surface       bio_ns = sum over plants of ab_gr * exp(-k_ns * cht)
!!       living biomass     - exp(-k_ns*cht) is "what fraction of the canopy's
!!                            effect reaches the ground", the same form
!!                            ero_cfactor uses, so the runoff and erosion sides
!!                            tell the same story about cover
!!    3  combined cover     c_bio = B/(B + exp(1.175 - 1.748*B)), B in t/ha
!!                          c_tot = 1 - (1-c_rsd)*(1-c_bio)   (independent overlap)
!!    4  interpolation      cn2 <- between the poor and good (or poor/fair/good)
!!                          rows of the family, then + the external offset
!!
!!    ~ ~ ~ OUTGOING ~ ~ ~
!!    cn2(j), and through curno: smx(j), wrt(1:2,j)
!!
!!    ~ ~ ~ MODULE VARIABLES (cn_cover_module) ~ ~ ~
!!    cn_cov_hru(j)       per-HRU state.
!!                        hyd, fam_lum, trt_lum, active - set by cn_cover_hru_init,
!!                          only read here.
!!                        cn_last, off - READ AND WRITTEN here, and carried from
!!                          one day to the next: cn_last is the cn2 this routine
!!                          wrote yesterday, off the running total of what other
!!                          code has done to cn2 since.  both restart in
!!                          cn_cover_hru_init (startup and land use change).
!!                        c_rsd, c_bio, c_tot, cn_sel - today's values, written
!!                          here for the audit file only.
!!    pl_cov              plant -> family and k_rsd - filled by cn_cover_read
!!    cn_fam              family names - cn_cover_init pass 1
!!    k_rsd_row, k_rsd_grain, k_ns, cn_floor, cn_ceil
!!                        tunable; set where declared at the top of
!!                        cn_cover_module, never written at run time
!!    cn_cov_unit         cn_cover.out - opened by cn_cover_init, only at cn = 2
!!
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: curno
!!    utils: exp_w

      use basin_module, only : bsn_cc
      use hru_module, only : cn2
      use plant_module, only : pcom
      use organic_mineral_mass_module, only : pl_mass
      use time_module, only : time
      use soil_module, only : soil
      use landuse_data_module, only : cn
      use plant_data_module, only : pldb
      use utils, only : exp_w
      use cn_cover_module

      implicit none

      external :: curno

      integer, intent (in) :: j             !none   |HRU number
      integer :: ipl = 0                    !none   |sequential plant number in the community
      integer :: idp = 0                    !none   |plant number in plants.plt (pldb)
      integer :: ifam = 0                   !none   |family index of the current plant
      integer :: nfam_hit = 0               !none   |plants that resolved to an interpolatable family
      real :: rsd = 0.                      !kg/ha  |above-ground residue, whole community
      real :: k_rsd = 0.                    !ha/kg  |residue cover coefficient, residue-weighted
      real :: wk = 0.                       !kg/ha  |weight accumulator for k_rsd
      real :: kk = 0.                       !ha/kg  |this plant's residue cover coefficient
      real :: bio_ns = 0.                   !kg/ha  |near-surface living biomass
      real :: bmt = 0.                      !t/ha   |bio_ns in tonnes
      real :: c_rsd = 0.                    !frac   |residue cover
      real :: c_bio = 0.                    !frac   |near-surface biomass cover
      real :: c_tot = 0.                    !frac   |combined cover
      real :: cnv = 0.                      !none   |this plant's interpolated cn2
      real :: cn_first = 0.                 !none   |first interpolatable plant's cn2
      real :: cn_sum = 0.                   !none   |mass-weighted sum of cn2
      real :: w_sum = 0.                    !kg/ha  |sum of the weights
      real :: w = 0.                        !kg/ha  |this plant's weight
      real :: cn_new = 0.                   !none   |cn2 handed to curno
      real :: cn_t = 0.                     !none   |curve 2 table CN in effect today
      real :: cn_h = 0.                     !none   |curve 2 high end in effect today
      real :: cn_a = 0.                     !none   |curve 2 middle point in effect today
      logical :: gro = .false.              !none   |plant growing today
      logical :: ok = .false.               !none   |crop has a row of its own
      logical :: nores = .false.            !none   |crop's row is used without its residue treatment
      integer :: iage = 0                   !yr     |whole years since the stand clock started
      integer :: nhit = 0                   !none   |plants contributing to the blend
      real :: t_c = 0.                      !none   |one crop's table CN
      real :: h_c = 0.                      !none   |one crop's high end
      real :: m_c = 0.                      !none   |one crop's middle point
      real :: t_1 = 0.                      !none   |first contributing crop's table CN
      real :: h_1 = 0.                      !none   |first contributing crop's high end
      real :: m_1 = 0.                      !none   |first contributing crop's middle point
      real :: d_t = 0.                      !none   |weighted departures from the first crop
      real :: d_h = 0.                      !none   |
      real :: d_m = 0.                      !none   |

      if (.not. allocated (cn_cov_hru)) return
      if (.not. cn_cov_hru(j)%active) return

      !! -- stage 1: residue cover ---------------------------------------
      !! k_rsd differs by family: NRCS puts 20% cover at 750 lb/ac for row
      !! crops but 300 lb/ac for small grains, so weight by each plant's own
      !! residue.  a plant keeps its residue in the community after it is
      !! killed, which is exactly the window this method exists to represent
      rsd = pl_mass(j)%abg_rsd_tot%m
      k_rsd = 0.
      wk = 0.
      do ipl = 1, pcom(j)%npl
        idp = pcom(j)%plcur(ipl)%idplt
        kk = k_rsd_row
        if (idp >= 1 .and. allocated (pl_cov)) then
          if (pl_cov(idp)%k_rsd > 0.) then
            kk = pl_cov(idp)%k_rsd
          else if (pl_cov(idp)%fam >= 1) then
            if (trim(cn_fam(pl_cov(idp)%fam)) == "sg") kk = k_rsd_grain
          end if
        end if
        w = pl_mass(j)%abg_rsd(ipl)%m
        k_rsd = k_rsd + w * kk
        wk = wk + w
      end do
      if (wk > 1.e-6) then
        k_rsd = k_rsd / wk
      else
        k_rsd = k_rsd_row
      end if

      c_rsd = 1. - exp_w (-k_rsd * rsd)

      !! -- stage 2: near-surface living biomass -------------------------
      bio_ns = 0.
      do ipl = 1, pcom(j)%npl
        bio_ns = bio_ns + pl_mass(j)%ab_gr(ipl)%m * exp_w (-k_ns * pcom(j)%plg(ipl)%cht)
      end do

      !! -- stage 3: mass to cover fraction ------------------------------
      bmt = bio_ns / 1000.
      c_bio = bmt / (bmt + exp_w (1.175 - 1.748 * bmt))
      c_tot = 1. - (1. - c_rsd) * (1. - c_bio)
      if (c_tot < 0.) c_tot = 0.
      if (c_tot > 1.) c_tot = 1.

      !! table CN in effect today, for the audit file; curve 2 may replace it
      cn_t = cn_cov_hru(j)%cn_tbl

      select case (cn_curve)
      case (1)
        !! -- stage 4: condition index and interpolation -------------------
        !! blend the plants' families by above-ground mass.  a mixed community
        !! (corn under a rye cover crop) is rc and sg at once, and the cover that
        !! drives the condition is a property of the whole surface, so c_tot is
        !! computed once and only the family endpoints are weighted
        cn_sum = 0.
        w_sum = 0.
        cn_first = 0.
        nfam_hit = 0
        do ipl = 1, pcom(j)%npl
          idp = pcom(j)%plcur(ipl)%idplt
          if (idp < 1 .or. .not. allocated (pl_cov)) cycle
          ifam = pl_cov(idp)%fam
          if (ifam < 1) cycle                 !! plant not in plants.cov
          if (ifam == cn_cov_hru(j)%fam_lum) then
            !! same family as the land use - hold its treatment, never silently
            !! move the HRU onto a different cntable.lum treatment row
            cnv = cn_from_cover (ifam, cn_cov_hru(j)%trt_lum, cn_cov_hru(j)%hyd, c_tot, .false.)
          else
            cnv = cn_from_cover (ifam, cn_cov_hru(j)%trt_lum, cn_cov_hru(j)%hyd, c_tot, .true.)
          end if
          if (cnv < 1.e-6) cycle              !! static family (wood, woodgr)
          nfam_hit = nfam_hit + 1
          if (nfam_hit == 1) cn_first = cnv
          w = pl_mass(j)%ab_gr(ipl)%m + pl_mass(j)%abg_rsd(ipl)%m
          cn_sum = cn_sum + w * cnv
          w_sum = w_sum + w
        end do

        if (w_sum > 1.e-6) then
          cn_new = cn_sum / w_sum
        else if (nfam_hit > 0) then
          cn_new = cn_first                   !! community present but no mass yet
        else
          !! fallow, or no plant carries a family - fall back to the land use's
          !! own family.  residue alone then drives the condition, which is the
          !! right answer for a bare seedbed between tillage and emergence
          cn_new = cn_from_cover (cn_cov_hru(j)%fam_lum, cn_cov_hru(j)%trt_lum,  &
                                  cn_cov_hru(j)%hyd, c_tot, .false.)
        end if

      case (2)
        !! -- stage 4, curve 2: the table value is the period average and
        !! cover swings cn2 around it - see CURVE SHAPE in cn_cover_module
        cn_t = cn_cov_hru(j)%cn_tbl
        cn_h = cn_cov_hru(j)%cn_hi
        cn_a = cn_cov_hru(j)%cn_mid

        select case (crop_fam)
        case (1)
          !! per-crop family - see PER-CROP FAMILY in cn_cover_module.  blend
          !! the anchors of every plant planted this run, weighted by living
          !! biomass plus surface residue.  departures from the first plant are
          !! summed so a field of one family gets its anchors back exactly
          if (cn_cov_hru(j)%wide .and. cn_cov_hru(j)%lay >= 1 .and. allocated (cn_cov_hru(j)%seen)) then
            nhit = 0
            w_sum = 0.
            d_t = 0.
            d_h = 0.
            d_m = 0.
            do ipl = 1, pcom(j)%npl
              gro = pcom(j)%plcur(ipl)%gro == "y"
              if (gro .neqv. cn_cov_hru(j)%gro_prev(ipl)) then
                !! planted, or killed: either starts the stand clock again
                if (gro) cn_cov_hru(j)%seen(ipl) = .true.
                cn_cov_hru(j)%yr_p(ipl) = time%yrc
                cn_cov_hru(j)%day_p(ipl) = time%day
                cn_cov_hru(j)%gro_prev(ipl) = gro
              end if
              if (.not. cn_cov_hru(j)%seen(ipl)) cycle
              w = pl_mass(j)%ab_gr(ipl)%m + pl_mass(j)%abg_rsd(ipl)%m
              if (w <= 1.e-6) cycle

              iage = time%yrc - cn_cov_hru(j)%yr_p(ipl)
              if (time%day < cn_cov_hru(j)%day_p(ipl)) iage = iage - 1
              idp = pcom(j)%plcur(ipl)%idplt
              call crop_anchor (j, idp, iage, t_c, h_c, m_c, ok, nores)
              !! each fallback once per land-use row and plant
              if ((nores .or. .not. ok) .and. idp >= 1 .and. idp <= ubound (crop_noted, 2)) then
                if (pl_cov(idp)%fam >= 1 .and. .not. crop_noted(cn_cov_hru(j)%icn,idp)) then
                  crop_noted(cn_cov_hru(j)%icn,idp) = .true.
                  if (ok) then
                    write (9001,*) "NOTE: cn_cover crop_fam: ", trim (cn_fam(pl_cov(idp)%fam)),       &
                      " has no residue row; on land use cn row ", trim (cn(cn_cov_hru(j)%icn)%name),   &
                      " plant ", trim (pldb(idp)%plantnm), " uses the row without residue"
                  else
                    write (9001,*) "NOTE: cn_cover crop_fam: ", trim (cn_fam(pl_cov(idp)%fam)),       &
                      " has no row for the layout and condition of land use cn row ",                  &
                      trim (cn(cn_cov_hru(j)%icn)%name), "; plant ", trim (pldb(idp)%plantnm),         &
                      " uses the land-use row"
                  end if
                end if
              end if

              nhit = nhit + 1
              if (nhit == 1) then
                t_1 = t_c
                h_1 = h_c
                m_1 = m_c
              end if
              w_sum = w_sum + w
              d_t = d_t + w * (t_c - t_1)
              d_h = d_h + w * (h_c - h_1)
              d_m = d_m + w * (m_c - m_1)
            end do

            !! nothing with weight today (before the first planting, or all
            !! residue gone) - hold the anchors already in effect
            if (w_sum > 1.e-6) then
              cn_cov_hru(j)%b_tbl = t_1 + d_t / w_sum
              cn_cov_hru(j)%b_hi = h_1 + d_h / w_sum
              cn_cov_hru(j)%b_mid = m_1 + d_m / w_sum
            end if
            cn_t = cn_cov_hru(j)%b_tbl
            cn_h = cn_cov_hru(j)%b_hi
            cn_a = cn_cov_hru(j)%b_mid
          end if
        end select

        if (cn_cov_hru(j)%wide) then
          cn_new = cn_wide (cn_t, cn_h, cn_a, c_tot, c_bio)
        else
          cn_new = cn_cov_hru(j)%cn_tbl
        end if
        !! same frozen test as sq_dailycn.  with crop_fam 1 the hold is the
        !! current crop's table CN
        if (frz_hold == 1 .and. soil(j)%phys(2)%tmp <= 0.) cn_new = cn_t

      case (3)
        !! -- stage 4, curve 3: static, the baseline curve 2 is matched to
        cn_new = cn_cov_hru(j)%cn_tbl

      case default
        return
      end select

      if (cn_new < 1.e-6) return            !! nothing to re-seat

      !! -- carry whatever else moved cn2 since yesterday ----------------
      !! curno rebuilds the retention curve from cn2 wholesale, so writing a
      !! fresh cn2 every day would discard every calibration.cal entry, every
      !! cnup operation and pl_burnop's fire adjustment.  the difference
      !! between cn2 as we find it and cn2 as we left it is exactly what
      !! someone else did, and it accumulates - which matches the accumulate
      !! semantics the cn_update d-table action is built on
      cn_cov_hru(j)%off = cn_cov_hru(j)%off + (cn2(j) - cn_cov_hru(j)%cn_last)

      cn_cov_hru(j)%cn_sel = cn_new
      cn_new = cn_new + cn_cov_hru(j)%off
      if (cn_new < cn_floor) cn_new = cn_floor
      if (cn_new > cn_ceil) cn_new = cn_ceil
      cn_cov_hru(j)%cn_last = cn_new

      cn_cov_hru(j)%c_rsd = c_rsd
      cn_cov_hru(j)%c_bio = c_bio
      cn_cov_hru(j)%c_tot = c_tot

      !! rebuild smx and wrt from the new cn2 - the same entry point cnup,
      !! the cn_update d-table action and pl_burnop already use
      call curno (cn_new, j)

      select case (bsn_cc%cn)
      case (2)
        write (cn_cov_unit,1000) time%day, time%yrc, j, rsd, bio_ns, c_rsd, c_bio, c_tot,  &
                                 cn_cov_hru(j)%cn_sel, cn_cov_hru(j)%off, cn2(j), cn_t
      end select

1000  format (i6,i6,i7,2f11.2,3f11.4,4f11.3)

      return
      end subroutine cn_cover_update
