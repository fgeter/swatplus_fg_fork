      module cn_cover_module

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    state and helper routines for the cover-driven daily curve number.
!!
!!    SWAT+ normally derives the daily curve number entirely from soil water:
!!
!!      cn2     <- cntable.lum row (fixed per land use) x hydgrp   cn2_init
!!      smx,wrt <- curno (cn2, cn3_swf, sumfc, sumul)              curno
!!      cnday   <- f(soil%sw, smx, wrt) [+ frozen-soil branch]     sq_dailycn
!!
!!    nothing in that chain sees residue or living biomass.  this module adds a
!!    daily re-selection of cn2 within the NRCS hydrologic-condition range of
!!    the HRU's own cover family, driven by above-ground residue plus the part
!!    of the living canopy that is near the ground surface.  the soil-water
!!    machinery downstream is untouched - cn_cover_update simply hands a new
!!    cn2 to curno before sq_dailycn runs.
!!
!!    the feature is off unless codes.bsn column "cn" (bsn_cc%cn) is set:
!!      0  current behaviour - this module allocates nothing and does nothing
!!      1  cover method
!!      2  cover method plus the daily audit file cn_cover.out
!!
!!    call sequence:
!!      proc_db   -> cn_cover_init      parse cntable.lum, read plants.cov
!!      cn2_init  -> cn_cover_hru_init  cache the HRU row set (also on lu_change)
!!      surface   -> cn_cover_update    daily re-seat of cn2, then curno
!!
!!    design note: tmp/CN_cover_design.md (section numbers are cited below).

      use landuse_data_module, only : cn

      implicit none

!!    ~ ~ ~ HYDROLOGIC CONDITION CODES ~ ~ ~
      integer, parameter :: cn_cond_none = 0   !none    |row carries no hydrologic condition
      integer, parameter :: cn_cond_poor = 1   !none    |"Poor"  column of cntable.lum
      integer, parameter :: cn_cond_fair = 2   !none    |"Fair"  column of cntable.lum
      integer, parameter :: cn_cond_good = 3   !none    |"Good"  column of cntable.lum

!!    ~ ~ ~ TUNABLE COEFFICIENTS (design note section 6) ~ ~ ~
!!    these are the calibration handles.  they are module variables rather than
!!    parameters so a future cn_cover.prm reader has somewhere to write, but
!!    nothing writes them today - change them here.
      real :: k_ns = 0.328          !1/m     |canopy-height decay for the near-surface
                                    !        |fraction of living biomass; same form and
                                    !        |prior as ero_cfactor.f90 line 73 (APEX)
      real :: k_rsd_row = 2.657e-4  !ha/kg   |residue mass -> cover, row crops.  NRCS puts
                                    !        |20% cover at 750 lb/ac = 840 kg/ha
      real :: k_rsd_grain = 6.64e-4 !ha/kg   |same for small grains: 300 lb/ac = 336 kg/ha
      real :: c_sat = 0.60          !frac    |cover at which the effect plateaus (Rawls 1980)
      real :: cn_floor = 30.        !none    |NEH table 650-2.15 footnote 4 - "actual curve
                                    !        |number is less than 30; use CN = 30"
      real :: cn_ceil = 98.         !none    |highest curve number in cntable.lum (urban)

!!    NRCS ground-cover breakpoints for the three-condition families
!!    (pasture/grassland footnote 2 and brush footnote 3, NEH 650-2.15).
!!    arid and semiarid rangeland uses 0.30/0.50/0.70 under a different
!!    footnote; the cntable.lum row set cannot tell the two apart, so the
!!    pasture breakpoints are used for both - see design note section 9.
      real :: cov_poor = 0.500      !frac    |at or below this cover the row is "Poor"
      real :: cov_fair = 0.625      !frac    |midpoint of the "Fair" band
      real :: cov_good = 0.750      !frac    |at or above this cover the row is "Good"

!!    ~ ~ ~ PARSED cntable.lum ROW KEYS ~ ~ ~
!!    every row name decomposes as <family>[_<treatment>][_<condition>], e.g.
!!    rc_strow_p -> (rc, strow, poor), pastg_g -> (pastg, "", good),
!!    pasth -> (pasth, "", none).  the name is parsed rather than the
!!    description/treat/cond_cov columns because those columns are absent from
!!    older cntable.lum files and a list-directed read that runs short of items
!!    silently continues onto the next record.
      type cn_row_key
        integer :: fam = 0                     !none  |index into cn_fam
        integer :: trt = 0                     !none  |index into cn_trt
        integer :: cond = cn_cond_none         !none  |cn_cond_* above
      end type cn_row_key
      type (cn_row_key), dimension(:), allocatable :: cn_key   !parallel to cn(:)

      integer :: n_fam = 0                     !none  |number of distinct families found
      integer :: n_trt = 0                     !none  |number of distinct treatments found
      character(len=16), dimension(:), allocatable :: cn_fam   !none |family tokens, file order
      character(len=16), dimension(:), allocatable :: cn_trt   !none |treatment tokens, file order
      integer, dimension(:,:,:), allocatable :: cn_row         !none |(fam,trt,cond) -> cn(:) row, 0 if absent
      integer, dimension(:), allocatable :: cn_trt_def         !none |(fam) -> default treatment index

!!    ~ ~ ~ PLANT -> FAMILY MAP (plants.cov) ~ ~ ~
      type plant_cover
        integer :: fam = 0                     !none  |index into cn_fam, 0 = plant not listed
        real :: k_rsd = 0.                     !ha/kg |residue cover coefficient from plants.cov;
                                               !      |0. means use the family default
      end type plant_cover
      type (plant_cover), dimension(:), allocatable :: pl_cov  !indexed like pldb

!!    ~ ~ ~ PER-HRU STATE ~ ~ ~
      type cn_cover_state
        logical :: active = .false.  !none  |.true. if the land use's own row has poor and good variants
        integer :: fam_lum = 0       !none  |family of the land use's cntable.lum row
        integer :: trt_lum = 0       !none  |treatment of that row
        integer :: hyd = 2           !none  |hydrologic soil group as a cn(:)%cn subscript
        real :: cn_last = 0.         !none  |the cn2 this module wrote yesterday
        real :: off = 0.             !none  |accumulated external cn2 offset - see below
        real :: c_rsd = 0.           !frac  |residue cover          (audit only)
        real :: c_bio = 0.           !frac  |near-surface biomass cover (audit only)
        real :: c_tot = 0.           !frac  |combined cover         (audit only)
        real :: cn_sel = 0.          !none  |interpolated cn2 before the offset (audit only)
      end type cn_cover_state
      type (cn_cover_state), dimension(:), allocatable :: cn_cov_hru

      integer, parameter :: cn_cov_unit = 4002 !none  |unit for cn_cover.out

      contains

!!    ---------------------------------------------------------------------
      subroutine cn_name_split (nm, fam, trt, cond)
!!    split a cntable.lum row name into family, treatment and condition.
!!    the condition is the trailing _p / _f / _g; everything between the first
!!    underscore and that suffix is the treatment.

      use utils, only : to_lower

      implicit none

      character(len=*), intent (in) :: nm      !none  |row name from cntable.lum
      character(len=16), intent (out) :: fam   !none  |family token
      character(len=16), intent (out) :: trt   !none  |treatment token ("" if none)
      integer, intent (out) :: cond            !none  |cn_cond_*

      character(len=40) :: work = ""           !none  |working copy of the name
      character(len=40) :: rest = ""           !none  |name with the family stripped
      integer :: iu = 0                        !none  |position of the first underscore
      integer :: nc = 0                        !none  |length of the remainder

      fam = ""
      trt = ""
      cond = cn_cond_none

      work = to_lower (adjustl (nm))
      if (len_trim (work) == 0) return

      iu = index (trim(work), "_")
      if (iu == 0) then
        fam = work(1:len_trim(work))
        return
      end if

      fam = work(1:iu-1)
      rest = work(iu+1:)
      nc = len_trim (rest)
      if (nc == 0) return

      !! a bare p/f/g remainder is the condition with no treatment
      select case (trim(rest))
      case ("p")
        cond = cn_cond_poor
        return
      case ("f")
        cond = cn_cond_fair
        return
      case ("g")
        cond = cn_cond_good
        return
      end select

      !! otherwise look for a _p / _f / _g suffix
      if (nc > 2) then
        select case (rest(nc-1:nc))
        case ("_p")
          cond = cn_cond_poor
        case ("_f")
          cond = cn_cond_fair
        case ("_g")
          cond = cn_cond_good
        end select
      end if

      select case (cond)
      case (cn_cond_none)
        trt = rest(1:nc)
      case default
        trt = rest(1:nc-2)
      end select

      return
      end subroutine cn_name_split

!!    ---------------------------------------------------------------------
      integer function cn_fam_index (nm) result (ifam)
!!    index of a family token in cn_fam, 0 if the token is not in cntable.lum

      use utils, only : to_lower

      implicit none

      character(len=*), intent (in) :: nm      !none  |family token
      integer :: i = 0                         !none  |counter
      character(len=16) :: key = ""            !none  |folded, trimmed token

      ifam = 0
      if (.not. allocated (cn_fam)) return
      key = to_lower (adjustl (nm))
      do i = 1, n_fam
        if (trim(cn_fam(i)) == trim(key)) then
          ifam = i
          exit
        end if
      end do

      return
      end function cn_fam_index

!!    ---------------------------------------------------------------------
      logical function fam_is_static (nm) result (is_static)
!!    families whose NRCS condition is defined by grazing and burning history
!!    rather than by a ground-cover percentage (NEH 650-2.15 footnote 6).
!!    driving these from a cover fraction is an extension the handbook does not
!!    support, and it walks cn2 down to where curno's Max(cn1, .4*cnn) clamp
!!    replaces the AMC I fit - so they are left static.  design note 9.3.
!!
!!    cntable.lum row names are not standardised: the SWAT+ editor writes
!!    wood_* and woodgr_*, while the HUC8 constructor datasets write frst_* and
!!    orch_*.  the list below therefore carries both vocabularies.  this is the
!!    one place to edit if woods should instead follow cover, or follow the
!!    graze and burn operations the model already simulates.

      use utils, only : to_lower

      implicit none

      character(len=*), intent (in) :: nm      !none  |family token

      select case (trim(to_lower(adjustl(nm))))
      case ("wood", "woodgr")                  !! cntable.lum from the SWAT+ editor
        is_static = .true.
      case ("frst", "frse", "frsd", "orch")    !! cntable.lum from the HUC8 constructor
        is_static = .true.
      case default
        is_static = .false.
      end select

      return
      end function fam_is_static

!!    ---------------------------------------------------------------------
      real function cn_from_cover (ifam, itrt, ihyd, cov, trt_fallback) result (cnv)
!!    interpolate cn2 between the hydrologic-condition rows of one family.
!!    returns 0. when the family has no condition rows to interpolate between,
!!    which the caller reads as "leave this HRU's cn2 alone".

      implicit none

      integer, intent (in) :: ifam             !none  |family index
      integer, intent (in) :: itrt             !none  |preferred treatment index
      integer, intent (in) :: ihyd             !none  |hydrologic soil group, 1-4
      real, intent (in) :: cov                 !frac  |combined ground cover
      logical, intent (in) :: trt_fallback     !none  |.true. permits dropping to the family default treatment

      integer :: it = 0                        !none  |treatment actually used
      integer :: ip = 0                        !none  |cn(:) row for "Poor"
      integer :: ifr = 0                       !none  |cn(:) row for "Fair"
      integer :: ig = 0                        !none  |cn(:) row for "Good"
      real :: cn_p = 0.                        !none  |curve number, poor condition
      real :: cn_f = 0.                        !none  |curve number, fair condition
      real :: cn_g = 0.                        !none  |curve number, good condition
      real :: idx = 0.                         !frac  |cover scaled by the plateau

      cnv = 0.
      if (ifam < 1 .or. ifam > n_fam) return
      if (fam_is_static (cn_fam(ifam))) return

      it = itrt
      if (it < 1) it = cn_trt_def(ifam)
      if (it >= 1) then
        if (cn_row(ifam,it,cn_cond_poor) == 0 .or. cn_row(ifam,it,cn_cond_good) == 0) it = 0
      end if
      if (it < 1 .and. trt_fallback) it = cn_trt_def(ifam)
      if (it < 1) return

      ip = cn_row(ifam,it,cn_cond_poor)
      ifr = cn_row(ifam,it,cn_cond_fair)
      ig = cn_row(ifam,it,cn_cond_good)
      if (ip == 0 .or. ig == 0) return

      cn_p = cn(ip)%cn(ihyd)
      cn_g = cn(ig)%cn(ihyd)

      select case (ifr)
      case (0)
        !! two-point family (rc_*, sg_*, legr_*, fal_res_*) - linear in cover
        !! up to the Rawls plateau
        idx = cov / c_sat
        if (idx > 1.) idx = 1.
        if (idx < 0.) idx = 0.
        cnv = cn_p - idx * (cn_p - cn_g)
      case default
        !! three-point family (pastg_*, brush_*) - piecewise linear through the
        !! NRCS ground-cover breakpoints themselves, not through the plateau
        cn_f = cn(ifr)%cn(ihyd)
        if (cov <= cov_poor) then
          cnv = cn_p
        else if (cov < cov_fair) then
          cnv = cn_p + (cov - cov_poor) / (cov_fair - cov_poor) * (cn_f - cn_p)
        else if (cov < cov_good) then
          cnv = cn_f + (cov - cov_fair) / (cov_good - cov_fair) * (cn_g - cn_f)
        else
          cnv = cn_g
        end if
      end select

      if (cnv < cn_floor) cnv = cn_floor

      return
      end function cn_from_cover

      end module cn_cover_module
