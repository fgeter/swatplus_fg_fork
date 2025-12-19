      subroutine hydrol_read
      
      use input_file_module
      use maximum_data_module
      use hydrology_data_module
      use utils
      
      implicit none

      character (len=80) :: titldum = ""!         |first line in file that generally is the title and it will be ignored.
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists

      character(len=3100)         :: line
      integer, parameter :: max_cols = 100
      integer, parameter :: max_name_len = 30
      character(len=max_name_len) :: header_cols(max_cols)
      character(len=max_name_len) :: fields(max_cols)
      character(len=:), allocatable :: left_str
      integer                     :: i, num_data_row, num_header_cols, num_data_cols, num_skip_rows
      integer                     :: nfields
      logical                     :: found_header_row
      
      
      eof = 0
      imax = 0
      nfields = 0
      found_header_row = .false.
      
      !! read all data from hydrol.dat
      inquire (file=in_hyd%hydrol_hyd, exist=i_exist)
      if (.not. i_exist .or. in_hyd%hydrol_hyd == "null") then
        allocate (hyd_db(0:0))
      else
        do
          ! Read and ignore first line
          open (107,file=in_hyd%hydrol_hyd)
          read (107,*,iostat=eof) titldum
          if (eof < 0) then 
            db_mx%hyd = imax
            allocate (hyd_db(0:0))
            return
          else
            exit
          endif
        end do
          
        do
          ! Determine the size of hyd_db and allocate it. 
          read(107, '(A)', iostat=eof) line
          if (eof /= 0) exit  ! EOF
          line = adjustl(trim(line))
          call left_of_delim(line, '#', left_str)         ! remove comments
          if ( len(left_str) == 0) cycle                  ! skip empty lines
          line = left_str
          if (.not. found_header_row) then                 ! check to see if the header row has not yet been processed
            found_header_row = .true.
            call split_line(line, fields, num_header_cols) ! process header row into header columns
            cycle
          end if
          call split_line(line, fields, num_data_cols)     ! split data row into fields
          ! Ignore datarow if the number data columns does not match the number of header columns
          if (num_header_cols /= num_data_cols) then
              cycle
          end if
          imax = imax + 1
        end do

        allocate (hyd_db(imax))

        ! Rince and repeat to actually read in the data now that we know the size of the data object    
        rewind (107)
        
        read (107,*,iostat=eof) titldum ! Read the first line and ignore it 

        found_header_row = .false.
        num_data_row = 0
        num_skip_rows = 1
        do
          read(107, '(A)', iostat=eof) line
          if (eof /= 0) exit  ! EOF
          line = adjustl(trim(line))

          ! get portion of line left of comment delimiter '#'
          call left_of_delim(line, '#', left_str)
          line = left_str

          ! skip empty lines
          if (len(left_str) == 0 ) then
              num_skip_rows = num_skip_rows + 1
              cycle ! get next line
          endif
         
          ! check to if the header row has not yet been processed
          if (.not. found_header_row) then
            ! process header row into header columns
            call split_line(line, header_cols, num_header_cols)
            num_skip_rows = num_skip_rows + 1
            found_header_row = .true.
            cycle ! skip to next line
          end if

          ! split data row into fields
          call split_line(line, fields, num_data_cols)
          
          ! check for correct number of columns and if incorrect skip row with warning
          if (num_data_cols /= num_header_cols) then
            num_skip_rows = num_skip_rows + 1
            write(9001,'(A,I3, A)') 'Warning: Row ', num_data_row + num_skip_rows, ' in hydrology.hyd has the wrong number of columns, skipping'
            print('(A,I3, A)'), 'Warning: Row ', num_data_row + num_skip_rows, ' in hydrology.hyd has the wrong number of columns, skipping'
            cycle
          end if
          num_data_row = num_data_row + 1
            
          ! Assign data to hyd_db fields based on header column names
          do i = 1, num_data_cols
            select case (to_lower(header_cols(i)))
  
            case ("name")
                hyd_db(num_data_row)%name = fields(i)
            case ("lat_ttime")
                read(fields(i), *) hyd_db(num_data_row)%lat_ttime
            case ("lat_sed")
                read(fields(i), *) hyd_db(num_data_row)%lat_sed
            case ("canmx")
                read(fields(i), *) hyd_db(num_data_row)%canmx
            case ("esco")
                read(fields(i), *) hyd_db(num_data_row)%esco
            case ("epco")
                read(fields(i), *) hyd_db(num_data_row)%epco
            case ("erorgn")
                read(fields(i), *) hyd_db(num_data_row)%erorgn
            case ("erorgp")
                read(fields(i), *) hyd_db(num_data_row)%erorgp
            case ("cn3_swf")
                read(fields(i), *) hyd_db(num_data_row)%cn3_swf
            case ("biomix")
                read(fields(i), *) hyd_db(num_data_row)%biomix
            case ("perco")
                read(fields(i), *) hyd_db(num_data_row)%perco
            case ("lat_orgn")
                read(fields(i), *) hyd_db(num_data_row)%lat_orgn
            case ("lat_orgp")
                read(fields(i), *) hyd_db(num_data_row)%lat_orgp
            case ("pet_co")
                read(fields(i), *) hyd_db(num_data_row)%pet_co
            case ("latq_co")
                  read(fields(i), *) hyd_db(num_data_row)%latq_co
            case default
                write(9001,'(2A)') 'Warning: unknown column header in hydrology.hyd, skipping: ', to_lower(trim(header_cols(i)))
                print *, 'Warning: unknown column header in hydrology.hyd, skipping: ', to_lower(trim(header_cols(i)))
            end select

          end do
        enddo
      endif
      close (107)
 
      db_mx%hyd = imax
      
      return
      end subroutine hydrol_read