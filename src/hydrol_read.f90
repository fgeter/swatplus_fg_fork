subroutine hydrol_read
      
use input_file_module
use maximum_data_module
use hydrology_data_module
use utils

implicit none

integer :: eof = 0        ! end of file
integer :: imax = 0       ! number of elements to be allocated
logical :: i_exist        ! true if file exists
integer :: i

call init_tblr_vars()  ! initialize tblr variables to zero and arrays to null

tblr%sub_name = "hyrol_read"
tblr%unit = 107

inquire (file=in_hyd%hydrol_hyd, exist=i_exist)
if (.not. i_exist .or. in_hyd%hydrol_hyd == "null") then
  allocate (hyd_db(0:0))
else
  open (tblr%unit,file=in_hyd%hydrol_hyd)
  imax = get_num_data_lines()  !get number of valid data lines

  allocate (hyd_db(imax))
  if (imax /= 0) then
    ! get the column headers
    call get_header_columns(eof)
    if (eof == 0) then
      allocate (tblr%col_okay(tblr%ncols))
      tblr%col_okay = .true.
      
      if (eof == 0) then   ! proceed if not at the end of the file.
        tblr%nrow = 0
        do
          ! get a row of data
          call get_data_fields(eof)
          if (eof /= 0) exit  ! exit if at the end of the file.
          
          tblr%nrow = tblr%nrow + 1
            
          ! Assign data to cons_prac fields based on header column names
          do i = 1, tblr%ncols
            select case (tblr%header_cols(i))
              case ("name")
                  hyd_db(tblr%nrow)%name = trim(tblr%data_fields(i))
              case ("lat_ttime")
                  read(tblr%data_fields(i), *) hyd_db(tblr%nrow)%lat_ttime
              case ("lat_sed")
                  read(tblr%data_fields(i), *) hyd_db(tblr%nrow)%lat_sed
              case ("canmx")
                  read(tblr%data_fields(i), *) hyd_db(tblr%nrow)%canmx
              case ("esco")
                  read(tblr%data_fields(i), *) hyd_db(tblr%nrow)%esco
              case ("epco")
                  read(tblr%data_fields(i), *) hyd_db(tblr%nrow)%epco
              case ("erorgn")
                  read(tblr%data_fields(i), *) hyd_db(tblr%nrow)%erorgn
              case ("erorgp")
                  read(tblr%data_fields(i), *) hyd_db(tblr%nrow)%erorgp
              case ("cn3_swf")
                  read(tblr%data_fields(i), *) hyd_db(tblr%nrow)%cn3_swf
              case ("biomix")
                  read(tblr%data_fields(i), *) hyd_db(tblr%nrow)%biomix
              case ("perco")
                  read(tblr%data_fields(i), *) hyd_db(tblr%nrow)%perco
              case ("lat_orgn")
                  read(tblr%data_fields(i), *) hyd_db(tblr%nrow)%lat_orgn
              case ("lat_orgp")
                  read(tblr%data_fields(i), *) hyd_db(tblr%nrow)%lat_orgp
              case ("pet_co")
                  read(tblr%data_fields(i), *) hyd_db(tblr%nrow)%pet_co
              case ("latq_co")
                    read(tblr%data_fields(i), *) hyd_db(tblr%nrow)%latq_co
              case default
                ! Output warning for unknown column header
                call output_column_warning(i)
            end select
          end do
        enddo
      endif
    endif
  endif
endif

db_mx%hyd = imax

close(tblr%unit)

return 
end subroutine hydrol_read