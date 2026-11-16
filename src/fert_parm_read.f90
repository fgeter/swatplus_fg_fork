      subroutine fert_parm_read
      
      use input_file_module
      use maximum_data_module
      use fertilizer_data_module
      
      implicit none
   
      integer :: it = 0               !none       |counter
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      integer :: mfrt = 0             !           |
      real :: frac_sum = 0.           !kg/kg      |sum of the mineral and organic N and P fractions
      logical :: i_exist              !none       |check to determine if file exists
      
      
      eof = 0
      imax = 0
      mfrt = 0
      
      inquire (file=in_parmdb%fert_frt, exist=i_exist)
      if (.not. i_exist .or. in_parmdb%fert_frt == "null") then
         allocate (fertdb(0:0))
      else
      do  
        open (107,file=in_parmdb%fert_frt)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
           do while (eof == 0) 
             read (107,*,iostat=eof) titldum
             if (eof < 0) exit
             imax = imax + 1
           end do
           
        allocate (fertdb(0:imax))
        
        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        
        do it = 1, imax
          read (107,*,iostat=eof) fertdb(it)
          if (eof < 0) exit

          !! fertilizer.frt has no carbon fraction, so pl_fert deliberately adds
          !! no carbon to the soil (see pl_fert.f90).  for a genuine mineral
          !! fertilizer that is correct - there is no organic matter in it.  for
          !! an entry that is really a manure it is an omission, and this is the
          !! only place it can be detected: an entry whose nutrient fractions sum
          !! to well under 1.0 has non-nutrient mass, which for something
          !! carrying organic N means organic matter.  a pure-N synthetic sums to
          !! exactly 1.0 and is silent here.  the test is deliberately limited to
          !! forgn > 0: ordinary mineral blends such as 00_06_00 sum to 0.026 and
          !! have nothing to say about carbon.
          frac_sum = fertdb(it)%fminn + fertdb(it)%fminp + fertdb(it)%forgn + fertdb(it)%forgp
          if (fertdb(it)%forgn > 0. .and. frac_sum < 0.999) then
            write (9001,*) "WARNING: fertilizer.frt entry ", trim(fertdb(it)%fertnm),         &
              " has organic N (forgn =", fertdb(it)%forgn, ") and its nutrient fractions",    &
              " sum to", frac_sum, "- the remaining mass is organic matter whose CARBON is",  &
              " not represented, because fertilizer.frt carries no carbon fraction.",         &
              " If this is a manure, move it to manure_om.frt (which has fcbn) and apply it", &
              " with a manu operation instead of fert."
          end if
        end do
       exit
      enddo
      endif
      
      db_mx%fertparm  = imax 
      
      close (107)
      return
      end subroutine fert_parm_read