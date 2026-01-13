subroutine hydrol_read
      
use input_file_module
use maximum_data_module
use hydrology_data_module
use utils

implicit none

integer :: eof = 0     ! end of file
integer :: imax = 0    ! number of elements to be allocated
integer :: i

type(table_reader) :: hyd_tbl
call hyd_tbl%init(unit=107, file_name=in_hyd%hydrol_hyd)

if (hyd_tbl%file_exists .eqv. .false.) then
  allocate (hyd_db(imax))
else

  imax = hyd_tbl%get_num_data_lines()  !get number of valid data lines

  allocate (hyd_db(imax))
  if (imax /= 0) then
    ! get the column headers
    call hyd_tbl%get_header_columns(eof)
    if (eof == 0) then ! proceed if not at the end of the file.
      do
        ! get a row of data
        call hyd_tbl%get_row_fields(eof)
        if (eof /= 0) exit  ! exit if at the end of the file.
        
        ! Assign data to cons_prac fields based on header column names
        do i = 1, hyd_tbl%get_col_count()
          select case (hyd_tbl%header_cols(i))
            case ("name")
                hyd_db(hyd_tbl%get_row_idx())%name = trim(hyd_tbl%row_field(i))
            case ("lat_ttime")
                read(hyd_tbl%row_field(i), *) hyd_db(hyd_tbl%get_row_idx())%lat_ttime
            case ("lat_sed")
                read(hyd_tbl%row_field(i), *) hyd_db(hyd_tbl%get_row_idx())%lat_sed
            case ("canmx")
                read(hyd_tbl%row_field(i), *) hyd_db(hyd_tbl%get_row_idx())%canmx
            case ("esco")
                read(hyd_tbl%row_field(i), *) hyd_db(hyd_tbl%get_row_idx())%esco
            case ("epco")
                read(hyd_tbl%row_field(i), *) hyd_db(hyd_tbl%get_row_idx())%epco
            case ("erorgn")
                read(hyd_tbl%row_field(i), *) hyd_db(hyd_tbl%get_row_idx())%erorgn
            case ("erorgp")
                read(hyd_tbl%row_field(i), *) hyd_db(hyd_tbl%get_row_idx())%erorgp
            case ("cn3_swf")
                read(hyd_tbl%row_field(i), *) hyd_db(hyd_tbl%get_row_idx())%cn3_swf
            case ("biomix")
                read(hyd_tbl%row_field(i), *) hyd_db(hyd_tbl%get_row_idx())%biomix
            case ("perco")
                read(hyd_tbl%row_field(i), *) hyd_db(hyd_tbl%get_row_idx())%perco
            case ("lat_orgn")
                read(hyd_tbl%row_field(i), *) hyd_db(hyd_tbl%get_row_idx())%lat_orgn
            case ("lat_orgp")
                read(hyd_tbl%row_field(i), *) hyd_db(hyd_tbl%get_row_idx())%lat_orgp
            case ("pet_co")
                read(hyd_tbl%row_field(i), *) hyd_db(hyd_tbl%get_row_idx())%pet_co
            case ("latq_co")
                  read(hyd_tbl%row_field(i), *) hyd_db(hyd_tbl%get_row_idx())%latq_co
            case default
              ! Output warning for unknown column header
              call hyd_tbl%output_column_warning(i)
          end select
        end do
      enddo
    endif
  endif
endif

db_mx%hyd = imax

close(hyd_tbl%unit)

return 
end subroutine hydrol_read