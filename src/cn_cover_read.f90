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
!!    other than 0, 1 and 2 and returned on 0.  It must run AFTER both of that
!!    subroutine's passes, because the cn_family token on every row is validated
!!    against the family dictionary pass 1 builds.
!!
!!    ~ ~ ~ WHAT THIS SUBROUTINE PRODUCES ~ ~ ~
!!
!!    One array: pl_cov, holding (family index, k_rsd) for each plant.
!!
!!    It is indexed like pldb, NOT like the file, and that is the whole point.
!!    The daily routine can then ask
!!
!!      idp  = pcom(j)%plcur(ipl)%idplt      ! the plant growing here
!!      ifam = pl_cov(idp)%fam               ! one subscript, no search
!!
!!    Anything else would mean a name search per HRU per plant per day.  Reading
!!    the file into a pldb-shaped array once at startup pays for that here.
!!
!!    Worked example.  A plants.cov row
!!
!!      corn        rc          0.
!!
!!    resolves "rc" to a family index through cn_fam_index (say 2), finds "corn"
!!    at pldb(13), and stores pl_cov(13) = (fam 2, k_rsd 0.).  The 0. is not
!!    "no coefficient" - it means "use the family default", applied later in
!!    cn_cover_update, which is k_rsd_grain for the sg family and k_rsd_row
!!    otherwise.

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

      !! three fatal preconditions.  all of them write to stdout AND to
      !! diagnostics.out (unit 9001) before stopping, because a message that
      !! only reaches one of the two is a message somebody misses.

      !! (1) no plant database to index against
      if (db_mx%plantparm < 1) then
        write (*,*)    "ERROR: codes.bsn cn > 0 requires plants.plt; no plants were read"
        write (9001,*) "ERROR: codes.bsn cn > 0 requires plants.plt; no plants were read"
        error stop
      end if

      !! (2) the file itself.  the cover method is meaningless without the
      !! plant -> family map, and a missing file must NOT degrade quietly to the
      !! static curve number - that failure mode is indistinguishable from the
      !! feature working, which is the worst kind.  codes.bsn carries the intent;
      !! if cn > 0 says "do this", an absent plants.cov is an error, not a hint.
      inquire (file=cov_file, exist=i_exist)
      if (.not. i_exist) then
        write (*,*)    "ERROR: codes.bsn cn > 0 requires ", cov_file, " and it was not found"
        write (9001,*) "ERROR: codes.bsn cn > 0 requires ", cov_file, " and it was not found"
        error stop
      end if

      !! sized to the plant DATABASE, not to the file - see the header.  the 0:
      !! lower bound matches how pldb itself is allocated, so an unresolved
      !! idplt of 0 reads a zeroed entry instead of going out of bounds.
      allocate (pl_cov(0:db_mx%plantparm))

      !! (3) exists but will not open
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

      !! one pass over the data rows.  each row is read raw, parsed, validated
      !! in two directions, and turned into one pl_cov entry.
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

        !! FATAL: the family token must be one cn_cover_init found in
        !! cntable.lum.  a token that matches nothing is a typo, and the
        !! alternative to stopping is a plant that silently never participates -
        !! which looks exactly like the feature being off for that crop.
        ifam = cn_fam_index (fam)
        if (ifam < 1) then
          write (*,*)    "ERROR: ", cov_file, " plant ", trim(nm), " names cn_family ", trim(fam),  &
                         " which no cntable.lum row begins with"
          write (9001,*) "ERROR: ", cov_file, " plant ", trim(nm), " names cn_family ", trim(fam),  &
                         " which no cntable.lum row begins with"
          error stop
        end if

        !! resolve the plant name against plants.plt.  case-folded and trimmed
        !! on both sides, so "Corn" and "corn " both match.  a linear search, but
        !! it runs once per file row at startup, not per HRU per day.
        ipl = 0
        do ic = 1, db_mx%plantparm
          if (trim(to_lower(pldb(ic)%plantnm)) == trim(to_lower(nm))) then
            ipl = ic
            exit
          end if
        end do

        !! NOT fatal, unlike the family check above.  a plants.cov naming a
        !! plant this project does not grow is normal when the file is shared
        !! across projects; a typo'd family token never is.  the asymmetry is
        !! deliberate.
        if (ipl < 1) then
          !! this is what catches a rename, and naming it is the point
          write (9001,*) "WARNING: ", cov_file, " row ", irow, " plant ", trim(nm),  &
                         " matches no plants.plt entry - row ignored"
          cycle
        end if

        !! the entire product of this subroutine, two fields per plant
        pl_cov(ipl)%fam = ifam
        pl_cov(ipl)%k_rsd = kk
      end do

      close (107)

      !! report the other direction of mismatch: plants in plants.plt with no
      !! plants.cov row.  not an error - those HRUs simply keep the land use's
      !! own family - but a silent omission is exactly the failure this file
      !! exists to prevent, so every missing name is listed rather than counted.
      !! expect this to be noisy with a small hand-written plants.cov against a
      !! full 126-plant plants.plt; it earns its keep when somebody adds a crop
      !! to a rotation and forgets the sidecar.
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
