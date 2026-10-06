      subroutine cn_cover_prm_read

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    read the optional curve-shape file cn_cover.prm.  absent, every value
!!    keeps the default set where it is declared in cn_cover_module, which is
!!    curve 1 - the v1 method - so existing datasets are unaffected.
!!
!!    ~ ~ ~ FILE FORMAT ~ ~ ~
!!    title line, header line, one data row, all six fields required:
!!
!!      cn_curve  lo_pct  d_mid  hi_row     mid_row    frz_hold
!!             2    0.12    0.0  fal_res_p  fal_res_g         1
!!
!!    ~ ~ ~ OUTGOING (cn_cover_module) ~ ~ ~
!!    cn_curve, lo_pct, d_mid, hi_nm, mid_nm, frz_hold

      use cn_cover_module

      implicit none

      character(len=*), parameter :: prm_file = "cn_cover.prm"

      character(len=80) :: titldum = ""     !      |title of file
      character(len=80) :: header = ""      !      |header of file
      character(len=250) :: line = ""       !      |the data row
      integer :: eof = 0                    !none  |end of file / read status
      integer :: ios = 0                    !none  |internal read status
      logical :: i_exist = .false.          !none  |does cn_cover.prm exist

      inquire (file=prm_file, exist=i_exist)
      if (.not. i_exist) return

      open (107, file=prm_file, iostat=eof)
      if (eof /= 0) then
        write (*,*)    "ERROR: ", prm_file, " exists but could not be opened"
        write (9001,*) "ERROR: ", prm_file, " exists but could not be opened"
        error stop
      end if

      read (107,'(a)',iostat=eof) titldum
      read (107,'(a)',iostat=eof) header
      do
        read (107,'(a)',iostat=eof) line
        if (eof /= 0) exit
        if (len_trim(line) > 0) exit
      end do
      close (107)

      !! read from the line, not the unit, so a short row is an error instead
      !! of running on into the next record
      ios = 1
      if (eof == 0) read (line,*,iostat=ios) cn_curve, lo_pct, d_mid, hi_nm, mid_nm, frz_hold
      if (ios /= 0) then
        write (*,*)    "ERROR: ", prm_file, " data row is not <cn_curve> <lo_pct> <d_mid> <hi_row> <mid_row> <frz_hold>"
        write (9001,*) "ERROR: ", prm_file, " data row is not <cn_curve> <lo_pct> <d_mid> <hi_row> <mid_row> <frz_hold>"
        error stop
      end if

      select case (cn_curve)
      case (1, 2, 3)
        continue
      case default
        write (*,*)    "ERROR: ", prm_file, " cn_curve must be 1, 2 or 3; got ", cn_curve
        write (9001,*) "ERROR: ", prm_file, " cn_curve must be 1, 2 or 3; got ", cn_curve
        error stop
      end select

      if (lo_pct < 0. .or. lo_pct >= 1.) then
        write (*,*)    "ERROR: ", prm_file, " needs 0 <= lo_pct < 1"
        write (9001,*) "ERROR: ", prm_file, " needs 0 <= lo_pct < 1"
        error stop
      end if

      write (9001,*) "cn_cover.prm: cn_curve", cn_curve, " lo_pct", lo_pct,  &
                     " d_mid", d_mid, " hi_row ", trim (hi_nm), " mid_row ", trim (mid_nm), " frz_hold", frz_hold

      return
      end subroutine cn_cover_prm_read
