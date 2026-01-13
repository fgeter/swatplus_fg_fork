subroutine cons_prac_read

use input_file_module
use maximum_data_module
use landuse_data_module
use utils

implicit none

integer :: eof = 0     ! end of file
integer :: imax = 0    ! number of elements to be allocated
integer :: i

type(table_reader) :: lu_tbl
call lu_tbl%init(unit=107, file_name=in_lum%cons_prac_lum, start_row_numbr=4)

!! read all curve number data from cn.tbl
if (lu_tbl%file_exists .eqv. .false.) then
  allocate (cons_prac(0:0))
else

  imax = lu_tbl%get_num_data_lines()  !get number of valid data lines
  allocate (cons_prac(0:imax))

  if (imax /= 0) then
    ! get the column headers
    call lu_tbl%get_header_columns(eof)
      
    if (eof == 0) then   ! proceed if not at the end of the file.
      lu_tbl%nrow = 0
      do
        ! get a row of data
        call lu_tbl%get_data_fields(eof)
        if (eof /= 0) exit  ! exit if at the end of the file.
        
        lu_tbl%nrow = lu_tbl%nrow + 1
          
        ! Assign data to cons_prac fields based on header column names
        do i = 1, lu_tbl%ncols
          select case (lu_tbl%header_cols(i))
            case ("name")
                cons_prac(lu_tbl%nrow)%name = trim(lu_tbl%data_fields(i))
            case ("pfac")
                read(lu_tbl%data_fields(i), *) cons_prac(lu_tbl%nrow)%pfac
            case ("sl_len_mx")
                read(lu_tbl%data_fields(i), *) cons_prac(lu_tbl%nrow)%sl_len_mx
            case default
              ! Output warning for unknown column header
              call lu_tbl%output_column_warning(i)
          end select
        end do
      enddo
    endif
  endif
endif

db_mx%cons_prac = imax

close(107)

return 
end subroutine cons_prac_read