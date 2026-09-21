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
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: curno
!!    utils: exp_w

      use basin_module, only : bsn_cc
      use hru_module, only : cn2
      use plant_module, only : pcom
      use organic_mineral_mass_module, only : pl_mass
      use time_module, only : time
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
                                 cn_cov_hru(j)%cn_sel, cn_cov_hru(j)%off, cn2(j)
      end select

1000  format (i6,i6,i7,2f11.2,3f11.4,3f11.3)

      return
      end subroutine cn_cover_update
