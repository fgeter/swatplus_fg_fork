      subroutine cn_cover_read

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    read plants.cov and resolve it into pl_cov, indexed like pldb so the
!!    daily lookup is pl_cov(pcom(j)%plcur(ipl)%idplt) with no name search.
!!
!!    plants.cov format (list-directed, one row per plant, any order, and a
!!    subset of plants.plt is fine):
!!
!!      plants.cov: <provenance line>
!!      name        cn_family   k_rsd
!!      corn        rc          0.
!!      wwht        sg          6.64e-4
!!
!!    every column carries a value on every row - there are no optional columns
!!    and no short rows.  k_rsd = 0. means "use the family default", which is
!!    k_rsd_grain for the sg family and k_rsd_row otherwise.
!!
!!    cn_family is the cntable.lum row-name prefix - fal, rc, sg, legr, pastg,
!!    pasth, brush, woodgr, wood.  it is validated against the families that
!!    cn_cover_init actually found in cntable.lum, so a typo stops the run on
!!    day one instead of silently leaving a plant static.
!!
!!    trailing text after the third field is ignored, as in every other SWAT+
!!    input file: the list-directed read stops once its io-list is satisfied.
!!
!!    called from cn_cover_init, which has already rejected bsn_cc%cn values
!!    other than 0, 1 and 2 and returned on 0.

      use maximum_data_module, only : db_mx
      use plant_data_module, only : pldb
      use cn_cover_module
      use utils, only : to_lower

      implicit none

      character(len=*), parameter :: cov_file = "plants.cov"

      character(len=80) :: titldum = ""     !      |title of file
      character(len=80) :: header = ""      !      |header of file
      character(len=250) :: line = ""       !      |one raw record
      character(len=40) :: nm = ""          !      |plant name from the file
      character(len=16) :: fam = ""         !      |family token from the file
      integer :: eof = 0                    !none  |end of file / read status
      integer :: ios = 0                    !none  |internal read status
      integer :: ic = 0                     !none  |pldb counter
      integer :: irow = 0                   !none  |data row counter
      integer :: ifam = 0                   !none  |resolved family index
      integer :: ipl = 0                    !none  |resolved pldb index
      integer :: nmiss = 0                  !none  |plants.plt entries with no plants.cov row
      real :: kk = 0.                       !ha/kg |residue cover coefficient from the file
      logical :: i_exist = .false.          !none  |does plants.cov exist

      if (db_mx%plantparm < 1) then
        write (*,*)    "ERROR: codes.bsn cn > 0 requires plants.plt; no plants were read"
        write (9001,*) "ERROR: codes.bsn cn > 0 requires plants.plt; no plants were read"
        error stop
      end if

      !! the cover method is meaningless without the plant -> family map, so
      !! fail loudly rather than silently reverting to the static curve number
      inquire (file=cov_file, exist=i_exist)
      if (.not. i_exist) then
        write (*,*)    "ERROR: codes.bsn cn > 0 requires ", cov_file, " and it was not found"
        write (9001,*) "ERROR: codes.bsn cn > 0 requires ", cov_file, " and it was not found"
        error stop
      end if

      allocate (pl_cov(0:db_mx%plantparm))

      open (107, file=cov_file, iostat=eof)
      if (eof /= 0) then
        write (*,*)    "ERROR: ", cov_file, " exists but could not be opened"
        write (9001,*) "ERROR: ", cov_file, " exists but could not be opened"
        error stop
      end if

      !! title and header are read with '(a)' so a blank line consumes exactly
      !! one record instead of being skipped by a list-directed read
      read (107,'(a)',iostat=eof) titldum
      read (107,'(a)',iostat=eof) header

      do
        read (107,'(a)',iostat=eof) line
        if (eof /= 0) exit

        !! skip the blank or all-blank records that editors leave at the end
        !! of a table - cntable.lum ships with one
        if (len_trim(line) == 0) cycle
        irow = irow + 1

        !! all three fields are required.  reading from the line rather than
        !! from the unit is what makes a short row an error: a list-directed
        !! read straight off the unit would run on into the next record to
        !! satisfy the missing item and silently consume the following plant
        kk = 0.
        read (line,*,iostat=ios) nm, fam, kk
        if (ios /= 0) then
          write (*,*)    "ERROR: ", cov_file, " row ", irow, " is not <name> <cn_family> <k_rsd>: ", trim(line)
          write (9001,*) "ERROR: ", cov_file, " row ", irow, " is not <name> <cn_family> <k_rsd>: ", trim(line)
          error stop
        end if

        ifam = cn_fam_index (fam)
        if (ifam < 1) then
          write (*,*)    "ERROR: ", cov_file, " plant ", trim(nm), " names cn_family ", trim(fam),  &
                         " which no cntable.lum row begins with"
          write (9001,*) "ERROR: ", cov_file, " plant ", trim(nm), " names cn_family ", trim(fam),  &
                         " which no cntable.lum row begins with"
          error stop
        end if

        !! resolve the plant name against plants.plt
        ipl = 0
        do ic = 1, db_mx%plantparm
          if (trim(to_lower(pldb(ic)%plantnm)) == trim(to_lower(nm))) then
            ipl = ic
            exit
          end if
        end do

        if (ipl < 1) then
          !! not fatal - this is what catches a rename, and naming it is the point
          write (9001,*) "WARNING: ", cov_file, " row ", irow, " plant ", trim(nm),  &
                         " matches no plants.plt entry - row ignored"
          cycle
        end if

        pl_cov(ipl)%fam = ifam
        pl_cov(ipl)%k_rsd = kk
      end do

      close (107)

      !! report the other direction of mismatch.  an unlisted plant is not an
      !! error - its HRUs simply keep the land use's own family - but a silent
      !! omission is exactly the failure this file exists to prevent
      do ic = 1, db_mx%plantparm
        if (pl_cov(ic)%fam < 1) nmiss = nmiss + 1
      end do
      if (nmiss > 0) then
        write (9001,*) "NOTE: ", cov_file, " has no cn_family for ", nmiss,  &
                       " of the ", db_mx%plantparm, " plants.plt entries:"
        do ic = 1, db_mx%plantparm
          if (pl_cov(ic)%fam < 1) write (9001,*) "      ", trim(pldb(ic)%plantnm)
        end do
      end if

      return
      end subroutine cn_cover_read
