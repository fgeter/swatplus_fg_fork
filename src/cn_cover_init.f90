      subroutine cn_cover_init

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    decompose every cntable.lum row into (family, treatment, hydrologic
!!    condition) and build the lookup the daily cover method walks, then read
!!    plants.cov.  called from proc_db immediately after cntbl_read, so cn(:)
!!    is populated and pldb(:) has already been read by plant_parm_read.
!!
!!    does nothing at all when bsn_cc%cn == 0 - no array in cn_cover_module is
!!    allocated, and cn_cover_hru_init/cn_cover_update return immediately.

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

      !! default treatment of a family - walk its treatments in file order and
      !! take the first that is actually interpolatable, i.e. has both a poor and
      !! a good row.  used only when a plant pulls the HRU into a family that
      !! does not carry the land use's own treatment.  a family with no such
      !! treatment keeps cn_trt_def = 0 and can never be selected.
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
