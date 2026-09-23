      subroutine cn_cover_init

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    decompose every cntable.lum row into (family, treatment, hydrologic
!!    condition) and build the lookup the daily cover method walks, then read
!!    plants.cov.  called from proc_db immediately after cntbl_read, so cn(:)
!!    is populated and pldb(:) has already been read by plant_parm_read.
!!
!!    does nothing at all when bsn_cc%cn == 0 - no array in cn_cover_module is
!!    allocated, and cn_cover_hru_init/cn_cover_update return immediately.
!!
!!    ~ ~ ~ WHAT THIS SUBROUTINE PRODUCES ~ ~ ~
!!
!!    cn_row (family, treatment, condition) -> a row number in cn(:)
!!
!!    Three index spaces, none of them interchangeable:
!!
!!      family     1..n_fam   subscript of cn_fam(:)     "rc", "pastg", ...
!!      treatment  1..n_trt   subscript of cn_trt(:)     "strow", "", ...
!!      condition  1..3       cn_cond_poor/fair/good     compile-time constants
!!
!!    The VALUE stored is a subscript of cn(:), i.e. the Nth DATA row of
!!    cntable.lum - physical line N+2, after the title and header line.  0 means
!!    "no such row".  The curve numbers themselves are never copied here; the
!!    hydrologic soil group is applied only when cn_from_cover finally
!!    dereferences cn(row)%cn(ihyd).
!!
!!    Family and treatment need runtime dictionaries because their vocabularies
!!    are OPEN - the SWAT+ editor writes rc / pastg / wood / strow, the HUC8
!!    constructor writes rc / past / frst / sr_cr, and neither is known until the
!!    file is read.  Condition is CLOSED: NRCS defines exactly poor, fair and
!!    good, so it is a named constant and there is deliberately no cn_cond(:)
!!    dictionary.  cn_cond_none = 0 sits OUTSIDE the array bounds, which is what
!!    makes a row carrying no condition simply never get filed - see pass 2.
!!
!!    ~ ~ ~ WORKED EXAMPLE ~ ~ ~
!!
!!    The 49-row cntable.lum in workdata/IA-Ames_sp40_Clarion gives n_fam = 16,
!!    n_trt = 9.  Treatment dictionary cn_trt(1..9):
!!
!!      1 bare      2 res       3 strow     4 strowres  5 cont
!!      6 contres   7 contter   8 conterres 9 ""  (families with no treatment)
!!
!!    cn_row then holds ("-" is 0, i.e. no such row):
!!
!!      fam             trt=   1   2   3   4   5   6   7   8   9
!!       1 fal        P        -   2   -   -   -   -   -   -   -
!!                    F        -   -   -   -   -   -   -   -   -
!!                    G        -   3   -   -   -   -   -   -   -
!!       2 rc         P        -   -   4   6   8  10  12  14   -
!!                    F        -   -   -   -   -   -   -   -   -
!!                    G        -   -   5   7   9  11  13  15   -
!!       3 sg         P        -   -  16  18  20  22  24  26   -
!!                    G        -   -  17  19  21  23  25  27   -
!!       4 legr       P        -   -  28   -  30   -  32   -   -
!!                    G        -   -  29   -  31   -  33   -   -
!!       5 pastg      P/F/G    -   -   -   -   -   -   -   -  34/35/36
!!       7 brush      P/F/G    -   -   -   -   -   -   -   -  38/39/40
!!       8 woodgr     P/F/G    -   -   -   -   -   -   -   -  41/42/43
!!       9 wood       P/F/G    -   -   -   -   -   -   -   -  44/45/46
!!      11 open       P        -   -   -   -   -   -   -   -  48
!!
!!    Families 6 pasth, 10 farm, 12 urban, 13 paveroad, 14 gravroad,
!!    15 dirtroad and 16 "" have nothing filed at all - every one of their rows
!!    carries no hydrologic condition.  Everything else in the 16 x 9 x 3 array
!!    is 0.
!!
!!    Two rows of that table repay a second look:
!!
!!      fal has treatment "bare" registered in the dictionary but NOTHING filed
!!      under it, because fal_bare carries no condition.  The family is
!!      interpolatable (via "res") while that one treatment is not.  This is the
!!      case the trt_fallback = .false. guard in cn_cover_update protects: an HRU
!!      whose land use names fal_bare must not be walked onto fal_res.
!!
!!      open has exactly one row filed, open_p.  Interpolation needs BOTH
!!      endpoints, so one is no better than none and the family stays static.
!!
!!    wood and woodgr ARE fully populated.  What keeps them out of the daily
!!    calculation is fam_is_static short-circuiting inside cn_from_cover, not
!!    anything missing from this table - so turning woods back on really is the
!!    one-line change the design note claims.

      use basin_module, only : bsn_cc
      use maximum_data_module, only : db_mx
      use landuse_data_module, only : cn
      use cn_cover_module
      use output_path_module, only : open_output_file

      implicit none

      external :: cn_cover_read

      integer :: icno = 0                   !none  |cntable.lum row counter
      integer :: i = 0                      !none  |counter
      integer :: ifam = 0                   !none  |family index of the current row
      integer :: itrt = 0                   !none  |treatment index of the current row
      integer :: icond = 0                  !none  |hydrologic condition of the current row
      integer :: imax = 0                   !none  |number of cntable.lum rows
      character(len=16) :: fam = ""         !none  |family token parsed from the row name
      character(len=16) :: trt = ""         !none  |treatment token parsed from the row name
      logical :: found = .false.            !none  |token already registered

      !! the cover method is opt-in through codes.bsn column "cn"
      select case (bsn_cc%cn)
      case (0)
        return                              !! current behaviour - nothing allocated
      case (1, 2)
        continue                            !! cover method; 2 also writes cn_cover.out
      case default
        write (*,*)    "ERROR: codes.bsn cn must be 0, 1 or 2; got ", bsn_cc%cn
        write (9001,*) "ERROR: codes.bsn cn must be 0, 1 or 2; got ", bsn_cc%cn
        error stop
      end select

      !! the whole method interpolates between cntable.lum rows, so an empty
      !! or missing table is not something to limp along with
      imax = db_mx%cn_lu
      if (imax < 1 .or. .not. allocated (cn)) then
        write (*,*)    "ERROR: codes.bsn cn > 0 requires cntable.lum; no rows were read"
        write (9001,*) "ERROR: codes.bsn cn > 0 requires cntable.lum; no rows were read"
        error stop
      end if

      !! cn_key is parallel to cn(:).  cn_fam and cn_trt are sized to the row
      !! count because that is the most distinct tokens there could possibly be;
      !! n_fam and n_trt record how many are actually used.
      allocate (cn_key(0:imax))
      allocate (cn_fam(imax))
      allocate (cn_trt(imax))
      cn_fam = ""
      cn_trt = ""

      !! PASS 1 - decompose every row name into (family, treatment, condition)
      !! and build the two token dictionaries.  after this loop every row knows
      !! its own key, and cn_fam/cn_trt hold each distinct token exactly once.
      do icno = 1, imax
        call cn_name_split (cn(icno)%name, fam, trt, icond)
        !! a nameless row cannot be keyed - skip rather than register a blank
        if (len_trim(fam) == 0) cycle

        !! look the family token up in the dictionary; register it if new.
        !! ifam ends up holding this row's family index either way.
        found = .false.
        do i = 1, n_fam
          if (trim(cn_fam(i)) == trim(fam)) then
            ifam = i
            found = .true.
            exit
          end if
        end do
        if (.not. found) then
          n_fam = n_fam + 1
          cn_fam(n_fam) = fam
          ifam = n_fam
        end if

        !! same for the treatment token.  the empty string is a legitimate
        !! treatment - pastg_p and brush_g have no treatment segment - so it is
        !! registered like any other and gets its own index.
        found = .false.
        do i = 1, n_trt
          if (trim(cn_trt(i)) == trim(trt)) then
            itrt = i
            found = .true.
            exit
          end if
        end do
        if (.not. found) then
          n_trt = n_trt + 1
          cn_trt(n_trt) = trt
          itrt = n_trt
        end if

        !! record the decoded key for this row
        cn_key(icno)%fam = ifam
        cn_key(icno)%trt = itrt
        cn_key(icno)%cond = icond
      end do

      !! PASS 2 - invert pass 1.  cn_row answers the question the daily routine
      !! actually asks: "which cntable.lum row is this family, this treatment,
      !! this condition?"  dimensioned only now, because n_fam and n_trt were
      !! not known until pass 1 finished.
      allocate (cn_row(n_fam, n_trt, cn_cond_poor:cn_cond_good))
      allocate (cn_trt_def(n_fam))
      cn_row = 0
      cn_trt_def = 0

      !! file the row under its key.  rows that carry no hydrologic condition
      !! (pasth, farm, urban, the roads, fal_bare) are deliberately NOT filed -
      !! there is nothing to interpolate between, and their absence from cn_row
      !! is what makes cn_cover_hru_init mark those HRUs static.
      do icno = 1, imax
        ifam = cn_key(icno)%fam
        itrt = cn_key(icno)%trt
        icond = cn_key(icno)%cond
        if (ifam < 1 .or. itrt < 1) cycle
        select case (icond)
        case (cn_cond_poor, cn_cond_fair, cn_cond_good)
          cn_row(ifam,itrt,icond) = icno
        end select
      end do

      !! cn_trt_def is a SEPARATE 1-D array, not a fourth dimension of cn_row.
      !! It is derived from cn_row and can only be built now, once pass 2 has
      !! filled it: for each family, walk that family's treatments in file order
      !! and take the first that is actually interpolatable - meaning it has BOTH
      !! a poor and a good row.  A family with no such treatment keeps
      !! cn_trt_def = 0 and can never be selected.
      !!
      !! The value is a TREATMENT index, i.e. a subscript of cn_trt(:), which is
      !! fed back into cn_row's middle dimension.  On the example above:
      !!
      !!    cn_trt_def(1)  = 2   fal    -> cn_trt(2) = "res"
      !!    cn_trt_def(2)  = 3   rc     -> cn_trt(3) = "strow"
      !!    cn_trt_def(3)  = 3   sg     -> cn_trt(3) = "strow"
      !!    cn_trt_def(4)  = 3   legr   -> cn_trt(3) = "strow"
      !!    cn_trt_def(5)  = 9   pastg  -> cn_trt(9) = ""
      !!    cn_trt_def(7)  = 9   brush  -> cn_trt(9) = ""
      !!    cn_trt_def(11) = 0   open      none - only one endpoint exists
      !!    cn_trt_def(6)  = 0   pasth     none - nothing filed
      !!
      !! It answers one question, asked in cn_from_cover: "this family has to be
      !! used, but it does not carry the treatment the land use named - which of
      !! its own treatments should stand in?"  That substitution is permitted
      !! only when a PLANT has pulled the HRU into a different family; within the
      !! land use's own family the caller passes trt_fallback = .false. and no
      !! substitution happens.
      do ifam = 1, n_fam
        do itrt = 1, n_trt
          if (cn_row(ifam,itrt,cn_cond_poor) > 0 .and. cn_row(ifam,itrt,cn_cond_good) > 0) then
            cn_trt_def(ifam) = itrt
            exit
          end if
        end do
      end do

      !! plant -> family map.  must come AFTER the two passes: cn_cover_read
      !! validates each plants.cov cn_family token against cn_fam, so the
      !! dictionary has to exist first.
      call cn_cover_read

      !! daily audit file - only at cn = 2.  one line per participating HRU per
      !! day, so it is not something to leave on for a production run.
      select case (bsn_cc%cn)
      case (2)
        call open_output_file (cn_cov_unit, "cn_cover.out", 800)
        write (cn_cov_unit,1000)
        write (cn_cov_unit,1001)
        write (9000,*) "CN                        cn_cover.out"
      end select

1000  format (2x,"jday",4x,"yr",3x,"unit",8x,"rsd",5x,"bio_ns",6x,"c_rsd",6x,"c_bio",  &
              6x,"c_tot",5x,"cn2_cov",6x,"cn2_off",7x,"cn2")
1001  format (6x," ",5x," ",6x," ",5x,"kg/ha",6x,"kg/ha",5x,"frac",7x,"frac",          &
              7x,"frac",8x,"none",9x,"none",8x,"none")

      return
      end subroutine cn_cover_init
