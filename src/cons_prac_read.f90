      subroutine cons_prac_read
      
      use input_file_module
      use maximum_data_module
      use landuse_data_module
      use utils
      
      implicit none
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists

      character(MAX_NAME_LEN)       :: header_cols(MAX_TABLE_COLS) 
      character(MAX_NAME_LEN)       :: data_fields(MAX_TABLE_COLS)
      integer                       :: i, nrow, nheader_cols, ndata_cols, num_skip_rows
      integer                       :: nfields
      character(:), allocatable     :: sub_name !name of subroutine for data read warning messages       
      
      sub_name = "cons_prac_read"
      
      eof = 0
      imax = 0
      nfields = 0
      num_skip_rows = 0
      
    !! read all curve number data from cn.tbl
      inquire (file=in_lum%cons_prac_lum, exist=i_exist)
      if (.not. i_exist .or. in_lum%cons_prac_lum == "null") then
        allocate (cons_prac(0:0))
      else
        open (107,file=in_lum%cons_prac_lum)
        imax = num_data_lines_in_data_table(107)  !determine number of actual valid data lines in the file

        allocate (cons_prac(0:imax))
        if (imax == 0) then
          db_mx%cons_prac = imax
          close (107)
          return
        end if

        ! Rinse and repeat to actually read the data in now that we know the size of the data object    
        rewind (107)
        
        ! get the column headers
        call get_data_table_header_columns(107, header_cols, nheader_cols, num_skip_rows, eof)

        if (eof == 0) then   ! proceed if not at the end of the file.
          nrow = 0
          do
            ! get a row of data
            call get_data_table_row_fields(107, data_fields, ndata_cols, num_skip_rows, eof)
            if (eof /= 0) exit  ! exit if at the end of the file.
            
            ! check for correct number of columns and if incorrect skip row with warning
            if (ndata_cols /= nheader_cols) then
              num_skip_rows = num_skip_rows + 1
              write(9001,'(A,I3, 3A)') 'Warning: Row ', nrow + num_skip_rows, ' in ', sub_name, ' has the wrong number of columns, skipping'
              print('(A,I3, 3A)'), 'Warning: Row ', nrow + num_skip_rows, ' in ', sub_name, ' has the wrong number of columns, skipping'
              cycle
            end if

            nrow = nrow + 1
              
            ! Assign data to cons_prac fields based on header column names
            do i = 1, ndata_cols
              select case (to_lower(header_cols(i)))
              case ("name")
                  cons_prac(nrow)%name = data_fields(i)
              case ("pfac")
                  read(data_fields(i), *) cons_prac(nrow)%pfac
              case ("sl_len_mx")
                  read(data_fields(i), *) cons_prac(nrow)%sl_len_mx
              case default
                  write(9001,'(4A)') 'Warning: unknown column header in ', sub_name, ' skipping: ', to_lower(trim(header_cols(i)))
                  print('(4A)'), 'Warning: unknown column header in ', sub_name, ' skipping: ', to_lower(trim(header_cols(i)))
              end select

            end do
          enddo
        endif
      endif
      
      db_mx%cons_prac = imax
      
      close(107)
      
      return 
      end subroutine cons_prac_read