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

      character(MAX_NAME_LEN)       :: header_cols(MAX_TABLE_COLS) 
      character(MAX_NAME_LEN)       :: data_fields(MAX_TABLE_COLS)
      integer                       :: i, nrow, nheader_cols, ndata_cols, num_skip_rows
      integer                       :: nfields
      character(:), allocatable     :: sub_name !name of subroutine for data read warning messages       
      
      sub_name = "hydrol_read"

      eof = 0
      imax = 0
      nfields = 0
      num_skip_rows = 0
      
      !! read all data from hydrol.dat
      inquire (file=in_hyd%hydrol_hyd, exist=i_exist)
      if (.not. i_exist .or. in_hyd%hydrol_hyd == "null") then
        allocate (hyd_db(0:0))
      else
        open (107,file=in_hyd%hydrol_hyd)
        imax = num_data_lines_in_data_table(107)

        allocate (hyd_db(imax))
        if (imax == 0) then
          db_mx%hyd = imax
          close (107)
          return
        end if

        ! Rinse and repeat to actually read in the data now that we know the size of the data object    
        rewind (107)
        
        call get_data_table_header_columns(107, header_cols, nheader_cols, num_skip_rows, eof)
        if (eof == 0) then
          nrow = 0
          do
            call get_data_table_row_fields(107, data_fields, ndata_cols, num_skip_rows, eof)
            if (eof /= 0) exit  ! EOF
            
            ! check for correct number of columns and if incorrect skip row with warning
            if (ndata_cols /= nheader_cols) then
              num_skip_rows = num_skip_rows + 1
              write(9001,'(A,I3, 3A)') 'Warning: Row ', nrow + num_skip_rows, ' in ', sub_name, ' has the wrong number of columns, skipping'
              print('(A,I3, 3A)'), 'Warning: Row ', nrow + num_skip_rows, ' in ', sub_name, ' has the wrong number of columns, skipping'
              cycle
            end if

            nrow = nrow + 1
              
            ! Assign data to hyd_db fields based on header column names
            do i = 1, ndata_cols
              select case (to_lower(header_cols(i)))
    
              case ("name")
                  hyd_db(nrow)%name = data_fields(i)
              case ("lat_ttime")
                  read(data_fields(i), *) hyd_db(nrow)%lat_ttime
              case ("lat_sed")
                  read(data_fields(i), *) hyd_db(nrow)%lat_sed
              case ("canmx")
                  read(data_fields(i), *) hyd_db(nrow)%canmx
              case ("esco")
                  read(data_fields(i), *) hyd_db(nrow)%esco
              case ("epco")
                  read(data_fields(i), *) hyd_db(nrow)%epco
              case ("erorgn")
                  read(data_fields(i), *) hyd_db(nrow)%erorgn
              case ("erorgp")
                  read(data_fields(i), *) hyd_db(nrow)%erorgp
              case ("cn3_swf")
                  read(data_fields(i), *) hyd_db(nrow)%cn3_swf
              case ("biomix")
                  read(data_fields(i), *) hyd_db(nrow)%biomix
              case ("perco")
                  read(data_fields(i), *) hyd_db(nrow)%perco
              case ("lat_orgn")
                  read(data_fields(i), *) hyd_db(nrow)%lat_orgn
              case ("lat_orgp")
                  read(data_fields(i), *) hyd_db(nrow)%lat_orgp
              case ("pet_co")
                  read(data_fields(i), *) hyd_db(nrow)%pet_co
              case ("latq_co")
                    read(data_fields(i), *) hyd_db(nrow)%latq_co
              case default
                  write(9001,'(4A)') 'Warning: unknown column header in ', sub_name, ' skipping: ', to_lower(trim(header_cols(i)))
                  print('(4A)'), 'Warning: unknown column header in ', sub_name, ' skipping: ', to_lower(trim(header_cols(i)))
              end select

            end do
          enddo
        endif
      endif
      close (107)
 
      db_mx%hyd = imax
      
      return
      end subroutine hydrol_read