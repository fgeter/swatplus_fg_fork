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
      real :: c_min = 0.60          !frac    |minimum cover giving the full effect: the
                                    !        |plateau in Rawls (1980).  scales combined
                                    !        |cover in x_tot, living cover in x_bio
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

!!    ~ ~ ~ CURVE SHAPE (optional cn_cover.prm, read by cn_cover_prm_read) ~ ~ ~
!!    the land use's own cntable.lum value is treated as the AVERAGE over the
!!    simulation (NEH 630 ch9: each row is the median CN of a management system's
!!    annual-flood storms, not the state of the surface on a given day), and
!!    cover swings cn2 over a wider range around it, in two legs:
!!
!!      x_tot = c_tot / c_min, x_bio = c_bio / c_min, both capped at 1
!!      cn2 = cn_hi + x_tot * (cn_mid - cn_hi) + x_bio * (cn_lo - cn_mid)
!!
!!      cn_hi  = fallow-poor row + treatment offset              (bare)
!!      cn_mid = fallow-good-residue row + treatment offset + d_mid
!!                                        (full cover, but no canopy: residue only)
!!               or, with mid_row "none", table CN + d_mid
!!      cn_lo  = table CN * (1 - lo_pct) (full cover AND full canopy)
!!      treatment offset = table CN - the family's NON-residue straight row of the
!!               same condition (off_ref 2).  the fallow rows exist only for
!!               straight row, so a contoured, terraced or crop-residue row keeps
!!               its table advantage at both anchors.  measuring a _cr row against
!!               the _cr straight row instead (off_ref 1) drops its residue credit
!!               at the anchors while simulated residue cover credits it again -
!!               residue counted twice; kept only for comparison.
!!
!!    any cover - residue or canopy - moves cn2 from the high end to the middle;
!!    only living canopy carries it on to the low end.  residue alone reaches
!!    the cover cap for about half of all days, so without the canopy gate the
!!    low end was the typical state rather than the extreme.  x_bio <= x_tot
!!    because c_bio <= c_tot, so the curve is monotone in cover.
!!
!!    with both ends of the residue leg anchored in cntable.lum, lo_pct is the
!!    one fitted value: chosen so annual outlet flow matches the static curve
!!    number on a calibrated watershed.  d_mid is left at 0.  HRUs whose row has
!!    no straight-row equivalent (pasture, woods, urban, fallow) are held at
!!    their table CN.  cn_curve 3 holds every HRU at its table CN - the same cn2
!!    as cn = 0 - so a static baseline carries the same runoff tally
!!    (cn_cover_sum.out).  cn_curve 1 is the original v1 method (cn2 between the
!!    poor and good rows of the plant's family), kept for comparison.
      integer :: cn_curve = 2       !none    |1 condition rows (the original v1 method), 2 the cover
                                    !        |curve described above, 3 flat at table CN
      real :: lo_pct = 0.057        !frac    |full-cover reduction off the table CN.  fitted on the
                                    !        |calibrated full Raccoon run (off_ref 2): annual outlet
                                    !        |flow -0.01% vs the static curve number
      real :: d_mid = 0.            !none    |middle point offset from its anchor
      character(len=40) :: hi_nm = "fal_res_p" !none |cntable.lum row for the bare, high end
      integer :: frz_hold = 1       !none    |1 holds curve 2 at the table CN on frozen days: smx
                                    !        |still scales sq_dailycn's frozen branch, so without
                                    !        |this winter cn2 sits on the residue leg, above the
                                    !        |table, and frozen-day runoff rises (+30% on Raccoon)
      integer :: i_hi = 0           !none    |cn(:) row of hi_nm
      character(len=40) :: mid_nm = "fal_res_g" !none |cntable.lum row anchoring the middle point,
                                    !        |"none" anchors it on the table CN instead
      integer :: i_mid = 0          !none    |cn(:) row of mid_nm, 0 = table CN
      integer, dimension(:), allocatable :: trt_sr  !none |(trt) -> straight-row treatment with the
                                    !        |same residue status, 0 if none - cn_cover_init
      integer, dimension(:), allocatable :: trt_sr0 !none |(trt) -> straight-row treatment WITHOUT
                                    !        |residue credit (strow / sr), 0 if none - cn_cover_init
      integer :: off_ref = 2        !none    |treatment offset measured against: 1 the straight
                                    !        |row of the same residue status (the offset is the
                                    !        |contouring alone); 2 the non-residue straight row
                                    !        |(a _cr row also keeps its table residue credit at
                                    !        |both anchors, so curve 2 does not credit residue
                                    !        |twice - once in the table, once as simulated cover)

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
        logical :: wide = .false.    !none  |curve 2 applies (row has a straight-row equivalent)
        real :: cn_tbl = 0.          !none  |the land use's own cntable.lum value
        real :: cn_hi = 0.           !none  |curve 2 high end
        real :: cn_mid = 0.          !none  |curve 2 middle point before d_mid
        real :: q_unf = 0.           !mm    |surface runoff summed over unfrozen days
        real :: q_frz = 0.           !mm    |surface runoff summed over frozen days
      end type cn_cover_state
      type (cn_cover_state), dimension(:), allocatable :: cn_cov_hru

      integer, parameter :: cn_cov_unit = 4002 !none  |unit for cn_cover.out
      integer, parameter :: cn_sum_unit = 4003 !none  |unit for cn_cover_sum.out

      contains

!!    ---------------------------------------------------------------------
      subroutine cn_name_split (nm, fam, trt, cond)
!!    split a cntable.lum row name into family, treatment and condition.
!!    the condition is the trailing _p / _f / _g; everything between the first
!!    underscore and that suffix is the treatment.

      use utils, only : to_lower, split_line

      implicit none

      character(len=*), intent (in) :: nm      !none  |row name from cntable.lum
      character(len=16), intent (out) :: fam   !none  |family token
      character(len=16), intent (out) :: trt   !none  |treatment token ("" if none)
      integer, intent (out) :: cond            !none  |cn_cond_*

      character(len=16) :: fld(8) = ""         !none  |name split on "_"
      integer :: nf = 0                        !none  |number of pieces
      integer :: last = 0                      !none  |last piece that is treatment
      integer :: i = 0                         !none  |piece counter

      fam = ""
      trt = ""
      cond = cn_cond_none

      call split_line (to_lower (adjustl (nm)), fld, nf, delim="_")
      nf = min (nf, size (fld))
      if (nf == 0) return

      fam = fld(1)
      if (nf == 1) return

      !! a trailing p / f / g piece is the condition
      select case (trim(fld(nf)))
      case ("p")
        cond = cn_cond_poor
      case ("f")
        cond = cn_cond_fair
      case ("g")
        cond = cn_cond_good
      end select

      !! the treatment is every piece between the family and the condition,
      !! rejoined with "_" (Raccoon's cs_c_t_g has treatment c_t)
      last = nf
      if (cond /= cn_cond_none) last = nf - 1
      do i = 2, last
        if (i > 2) trt = trim(trt) // "_"
        trt = trim(trt) // fld(i)
      end do

      return
      end subroutine cn_name_split

!!    ---------------------------------------------------------------------
      function tok_register (list, n, tok) result (idx)
!!    index of tok in list(1:n); appended, and n bumped, if not already there.
!!    builds the cn_fam and cn_trt dictionaries in cn_cover_init.  no trim is
!!    needed: == pads the shorter operand with blanks before comparing.

      implicit none

      character(len=*), intent (inout) :: list(:) !none  |token dictionary
      integer, intent (inout) :: n             !none  |tokens registered so far
      character(len=*), intent (in) :: tok     !none  |token to look up
      integer :: idx                           !none  |index of tok in list

      do idx = 1, n
        if (list(idx) == tok) return
      end do

      n = n + 1
      list(n) = tok
      idx = n

      return
      end function tok_register

!!    ---------------------------------------------------------------------
      function cn_fam_index (nm) result (ifam)
!!    index of a family token in cn_fam, 0 if the token is not in cntable.lum

      use utils, only : to_lower

      implicit none

      character(len=*), intent (in) :: nm      !none  |family token
      integer :: ifam                          !none  |index into cn_fam, 0 if absent
      integer :: i = 0                         !none  |counter
      character(len=16) :: key = ""            !none  |folded, trimmed token

      ifam = 0
      if (.not. allocated (cn_fam)) return
      key = to_lower (adjustl (nm))
      !! cn_fam and n_fam are module variables, not arguments.  both are filled
      !! by cn_cover_init pass 1 (tok_register bumps n_fam once per new family)
      !! and are fixed from then on.
      do i = 1, n_fam
        if (trim(cn_fam(i)) == trim(key)) then
          ifam = i
          exit
        end if
      end do

      return
      end function cn_fam_index

!!    ---------------------------------------------------------------------
      function fam_is_static (nm) result (is_static)
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
      logical :: is_static                     !none  |.true. if the family stays at its cntable.lum cn2

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
      function cn_from_cover (ifam, itrt, ihyd, cov, trt_fallback) result (cnv)
!!    interpolate cn2 between the hydrologic-condition rows of one family.
!!    returns 0. when the family has no condition rows to interpolate between,
!!    which the caller reads as "leave this HRU's cn2 alone".
!!
!!    module variables read here, not passed in:
!!      n_fam, cn_fam     family count and names - cn_cover_init pass 1
!!      cn_row            (fam,trt,cond) -> cntable.lum row - cn_cover_init pass 2
!!      cn_trt_def        default treatment per family - cn_cover_init, after pass 2
!!      c_min, cov_poor, cov_fair, cov_good, cn_floor
!!                        tunable; set where declared at the top of this module,
!!                        never written at run time
!!      cn(:)             cntable.lum itself (landuse_data_module, cntbl_read)

      implicit none

      integer, intent (in) :: ifam             !none  |family index
      integer, intent (in) :: itrt             !none  |preferred treatment index
      integer, intent (in) :: ihyd             !none  |hydrologic soil group, 1-4
      real, intent (in) :: cov                 !frac  |combined ground cover
      logical, intent (in) :: trt_fallback     !none  |.true. permits dropping to the family default treatment
      real :: cnv                              !none  |interpolated cn2, 0. if nothing to interpolate

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
        idx = cov / c_min
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

!!    ---------------------------------------------------------------------
      function cn_wide (cn_t, cn_h, cn_a, c_tot, c_bio) result (cnv)
!!    curve 2: the high-to-middle leg follows all cover, the middle-to-low leg
!!    follows living canopy only.  see CURVE SHAPE at the top of the module.

      implicit none

      real, intent (in) :: cn_t                !none  |the land use's table CN
      real, intent (in) :: cn_h                !none  |high end, zero cover
      real, intent (in) :: cn_a                !none  |middle-point anchor, before d_mid
      real, intent (in) :: c_tot               !frac  |combined ground cover
      real, intent (in) :: c_bio               !frac  |near-surface living biomass cover
      real :: cnv                              !none  |cn2

      real :: x_tot = 0.                       !frac  |all-cover index
      real :: x_bio = 0.                       !frac  |canopy index
      real :: cn_l = 0.                        !none  |low end, full cover and canopy
      real :: cn_m = 0.                        !none  |middle point, full cover, no canopy

      x_tot = c_tot / c_min
      if (x_tot > 1.) x_tot = 1.
      if (x_tot < 0.) x_tot = 0.
      x_bio = c_bio / c_min
      if (x_bio > x_tot) x_bio = x_tot
      if (x_bio < 0.) x_bio = 0.

      cn_l = cn_t * (1. - lo_pct)
      cn_m = cn_a + d_mid
      if (cn_m > cn_h) cn_m = cn_h
      if (cn_m < cn_l) cn_m = cn_l

      cnv = cn_h + x_tot * (cn_m - cn_h) + x_bio * (cn_l - cn_m)

      if (cnv < cn_floor) cnv = cn_floor

      return
      end function cn_wide

      end module cn_cover_module
