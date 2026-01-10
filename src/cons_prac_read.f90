subroutine cons_prac_read

use input_file_module
use maximum_data_module
use landuse_data_module
use utils

implicit none

integer :: eof = 0        ! end of file
integer :: imax = 0       ! number of elements to be allocated
logical :: i_exist        ! true if file exists
integer :: i

tblr%sub_name = "cons_prac_read"
tblr%unit = 107

!! read all curve number data from cn.tbl
inquire (file=in_lum%cons_prac_lum, exist=i_exist)
if (.not. i_exist .or. in_lum%cons_prac_lum == "null") then
  allocate (cons_prac(0:0))
else
  open (tblr%unit,file=in_lum%cons_prac_lum)
  imax = get_num_data_lines()  !get number of valid data lines

  allocate (cons_prac(0:imax))
  if (imax /= 0) then
    ! get the column headers
    call get_header_columns(eof)
    if (eof == 0) then
      if (allocated(tblr%col_okay)) deallocate(tblr%col_okay)
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
                  cons_prac(tblr%nrow)%name = trim(tblr%data_fields(i))
              case ("pfac")
                  read(tblr%data_fields(i), *) cons_prac(tblr%nrow)%pfac
              case ("sl_len_mx")
                  read(tblr%data_fields(i), *) cons_prac(tblr%nrow)%sl_len_mx
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

db_mx%cons_prac = imax

close(107)

return 
end subroutine cons_prac_read