module utils
    IMPLICIT NONE

    integer, parameter :: MAX_TABLE_COLS = 100
    integer, parameter :: MAX_NAME_LEN = 50
    integer, parameter :: MAX_LINE_LEN = 2500

    type :: table_reader
        character(MAX_NAME_LEN)  :: header_cols(MAX_TABLE_COLS) !array of header column names
        character(MAX_NAME_LEN)  :: data_fields(MAX_TABLE_COLS) !array of data fields in a data row of data
        character(len=MAX_LINE_LEN)  :: line      ! character string used to read in lines from data table
        character (len=80)       :: titldum = ""  ! first in data file that that will be ignored 
        integer                  :: nrow          ! data row number
        integer                  :: ncols         ! number of header columns   
        integer                  :: nfields       ! number of data columns/fields in a data row
        integer                  :: skip_rows     ! number of rows skipped (empty or comment lines)
        integer                  :: unit          ! file unit number
        character(len=:), allocatable :: left_str ! portion of line left of comment delimiter '#'
        logical                  :: found_header_row ! flag to indicate if header row has been found
        logical, allocatable     :: col_okay(:)   ! array used to track if warning message has already
                                                  ! been printed out for unknown column headers
        character(len=:), allocatable :: sub_name ! name of the subroutine using the table reader routines
    end type table_reader
    type(table_reader), public  :: tblr

contains

real function exp_w(y)
      use iso_fortran_env
#ifdef __INTEL_COMPILER
      use ifcore, only: tracebackqq
#endif
    implicit none
    real, intent(in) :: y
    logical :: err_output
    
    ! err_output = .true.
    err_output = .false.
    ! err_output = .false.

    if (y < -80.) then
        exp_w = 0.
        if (err_output) then
            write(error_unit,'(A)') ""
            write(error_unit,'(A,F6.1,A)') "Warning: exp(", y, ") causes an underflow."
            write(error_unit,'(A)') "Setting exp_w result to zero"
#ifdef __INTEL_COMPILER
            write(error_unit,'(A)') "Intel Fortran compiler stack trace"
            call tracebackqq(USER_EXIT_CODE=-1)
#elif defined(__GFORTRAN__)
            write(error_unit,'(A)') "GNU Fortran compiler stack trace"
            call backtrace()
#else
            write(error_unit, *)  "No stack trace available: Unknown compiler"
#endif
        endif
    else  
        exp_w = exp(y)
    endif
end function exp_w

pure function to_lower(str) result(lower)
    character(len=*), intent(in) :: str
    character(len=len(str))      :: lower
    integer                      :: i, code

    do i = 1, len(str)
        code = iachar(str(i:i))
        if (code >= iachar('A') .and. code <= iachar('Z')) then
            lower(i:i) = achar(code + 32)
        else
            lower(i:i) = str(i:i)
        end if
    end do
end function to_lower

subroutine left_of_delim(input, delim, result)
    character(len=*), intent(in)               :: input
    character(len=1), intent(in)               :: delim
    character(len=:), allocatable, intent(out) :: result

    integer :: pos

    pos = index(input, delim)

    if (pos == 0) then
      ! Delimiter not found → return whole string, trimmed
      result = trim(input)
    else
      ! Return everything before the delimiter (exclude the delimiter itself)
      result = input(1:pos-1)
    end if

end subroutine left_of_delim

subroutine split_line(line2, fields2, nfields, delim, maxsplit)
    !===============================================================================
    ! SUBROUTINE: split_line
    ! PURPOSE:    Splits a string into fields using fixed-size arrays.
    ! AUTHOR:     Developed by user fgeter through many failed iterations 
    !             by Grok/xAI. Final corrections and working code was done by fgeter
    ! DATE:       December 19, 2025
    !
    ! DESCRIPTION:
    !   This subroutine splits an input line into individual fields and stores them
    !   in a fixed-size output array. It is designed to be robust and debugger-friendly
    !   with gfortran/VS Code by avoiding allocatable or deferred-length arrays.
    !
    !   Behaviour:
    !     * If an optional delimiter (delim) is provided:
    !         - Splits on that single character.
    !         - Preserves empty fields (leading, trailing, and consecutive delimiters
    !           all produce empty strings).
    !     * If no delimiter is provided:
    !         - Splits on whitespace (spaces and tabs).
    !         - Collapses consecutive whitespace.
    !         - Ignores leading and trailing whitespace (no empty fields created).
    !     * If an optional maxsplit is provided:
    !         - Performs at most maxsplit splits.
    !         - The remainder of the line becomes the last field.
    !
    ! PARAMETERS:
    !   line2     (in)  : character(len=*)          - Input string to split
    !   fields2   (out) : character(len=*) :: fields2(:) - Fixed-size array to receive fields
    !   nfields   (out) : integer                   - Number of fields found (size of result)
    !   delim     (in, optional) : character(len=1) - Single character delimiter
    !   maxsplit  (in, optional) : integer          - Maximum number of splits to perform
    !
    ! USAGE EXAMPLES:
    !
    !   character(len=1000) :: line
    !   character(len=50)   :: fields(100)
    !   integer             :: nf
    !
    !   ! 1. Default whitespace splitting (collapse whitespace)
    !   line = "  hello   world  example  "
    !   call split_line(line, fields, nf)
    !   ! nf = 3, fields = 'hello', 'world', 'example'
    !
    !   ! 2. Split on comma, preserve empty fields
    !   line = ",,  apple  ,,banana,"
    !   call split_line(line, fields, nf, delim=",")
    !   ! nf = 6, fields = '', '', 'apple', '', 'banana', ''
    !
    !   ! 3. Split on semicolon with maxsplit=1 (remainder as last field)
    !   line = "one;two;three;four"
    !   call split_line(line, fields, nf, delim=";", maxsplit=1)
    !   ! nf = 2, fields = 'one', 'two;three;four'
    !
    !   ! 4. Empty line
    !   line = ""
    !   call split_line(line, fields, nf)
    !   ! nf = 0
    !
    !   ! 5. Parsing line comma delimiter
    !   line = '43   # a,  b,c,  d,e,f'
    !   call split_line(line, fields, nf)
    !   ! nf = 6, fields = '43   # a', 'b', 'c', 'd', 'e', 'f'
    !
    ! NOTES:
    !   - Fields are left-justified with trailing blanks removed via adjustl(trim(...)).
    !   - If the number of fields exceeds the size of fields2, an error message is printed
    !     and the subroutine returns early.
    !   - Safe for gfortran debugging (no allocatables or deferred-length components).
    !===============================================================================

    character(len=*), intent(in)                 :: line2
    ! The following line uses deferred-length strings for fields2 array, however, gfortran debugger has issues with it.
    ! The gfortran debugging issue can be worked around by using fixed-length strings instead 
    ! (comment out the next line and uncomment the following line to get gfortran debugger to work).
    character(len=*), intent(out)                :: fields2(:)
    ! character(len=30), intent(out)                :: fields2(:)  
    integer,          intent(out)                :: nfields
    character(len=1), intent(in), optional       :: delim
    integer,          intent(in), optional       :: maxsplit

    integer :: pos1, pos2, len_line, splits_done
    character(len=1) :: current_delim
    logical :: use_custom_delim

    nfields = 0
    fields2 = ''  ! Clear all fields

    use_custom_delim = present(delim)
    if (use_custom_delim) then
        current_delim = delim
    end if

    splits_done = 0
    len_line = len(trim(line2))
    if (len_line == 0) return

    pos1 = 1

    if (use_custom_delim) then

        ! Leading empty fields
        do while (pos1 <= len_line .and. line2(pos1:pos1) == current_delim)
            nfields = nfields + 1
            if (nfields > size(fields2)) then
                print *, 'Error: too many fields'
                return
            end if
            fields2(nfields) = ''
            pos1 = pos1 + 1
            splits_done = splits_done + 1
            if (present(maxsplit)) then
                if (splits_done >= maxsplit) then
                if (pos1 <= len_line) then
                    nfields = nfields + 1
                    if (nfields > size(fields2)) return
                    fields2(nfields) = adjustl(trim(line2(pos1:)))
                end if
                end if
                return
            end if
        end do

        do while (pos1 <= len_line)
            pos2 = pos1

            do while (pos2 <= len_line .and. line2(pos2:pos2) /= current_delim)
                pos2 = pos2 + 1
            end do

            nfields = nfields + 1
            if (nfields > size(fields2)) then
                print *, 'Error: too many fields'
                return
            end if

            fields2(nfields) = adjustl(trim(line2(pos1:min(pos2-1, pos1 + len(fields2) - 1))))

            pos1 = pos2

            if (pos1 <= len_line .and. line2(pos1:pos1) == current_delim) then
                splits_done = splits_done + 1
                if (pos1 == len_line) then
                    nfields = nfields + 1
                    if (nfields > size(fields2)) then
                        print *, 'Error: too many fields'
                        return
                    end if
                    fields2(nfields) = ''
                    return
                end if
                pos1 = pos1 + 1
                if (present(maxsplit)) then
                    if (splits_done >= maxsplit) then
                        if (pos1 <= len_line) then
                            nfields = nfields + 1
                            if (nfields > size(fields2)) return
                            fields2(nfields) = trim(adjustl(line2(pos1:)))
                        end if
                    return
                    end if
                end if
            end if
        end do
    else
        ! Whitespace mode
        do while (pos1 <= len_line .and. (line2(pos1:pos1) == ' ' .or. line2(pos1:pos1) == char(9)))
            pos1 = pos1 + 1
        end do

        do while (pos1 <= len_line)
            pos2 = pos1

            do while (pos2 <= len_line .and. .not. (line2(pos2:pos2) == ' ' .or. line2(pos2:pos2) == char(9)))
                pos2 = pos2 + 1
            end do

            nfields = nfields + 1
            if (nfields > size(fields2)) then
                print *, 'Error: too many fields'
                return
            end if

            fields2(nfields) = line2(pos1:min(pos2-1, pos1 + len(fields2) - 1))

            pos1 = pos2

            do while (pos1 <= len_line .and. (line2(pos1:pos1) == ' ' .or. line2(pos1:pos1) == char(9)))
                pos1 = pos1 + 1
            end do

            splits_done = splits_done + 1
            if (present(maxsplit) ) then
                if (splits_done >= maxsplit) then
                if (pos1 <= len_line) then
                    nfields = nfields + 1
                    if (nfields > size(fields2)) return
                    fields2(nfields) = trim(adjustl(line2(pos1:)))
                end if
                endif
                return
            end if
        end do
    end if

end subroutine split_line

function get_num_data_lines() result(imax)
    integer :: imax
    integer :: eof = 0              !           |end of file
    ! integer :: num_header_cols, num_data_cols
    ! character(len=MAX_LINE_LEN)  :: line
    ! character (len=80) :: titldum = ""!           |title of file
    ! character(len=MAX_NAME_LEN) :: fields(MAX_TABLE_COLS)
    ! character(len=:), allocatable :: left_str
    ! logical :: found_header_row

    imax = 0
    tblr%found_header_row = .false.

    read (tblr%unit,*,iostat=eof) tblr%titldum
    if (eof == 0) then 
        do
            read(tblr%unit, '(A)', iostat=eof) tblr%line
            if (eof /= 0) exit  ! EOF
            tblr%line = adjustl(trim(tblr%line))
            call left_of_delim(tblr%line, '#', tblr%left_str)         ! remove comments
            if ( len(tblr%left_str) == 0) cycle                  ! skip empty lines
            tblr%line = tblr%left_str
            if (.not. tblr%found_header_row) then                 ! check to see if the header row has not yet been processed
                tblr%found_header_row = .true.
                call split_line(tblr%line, tblr%data_fields, tblr%ncols) ! process header row into header columns
                cycle
            end if
            call split_line(tblr%line, tblr%data_fields, tblr%nfields)     ! split data row into fields
            ! Ignore datarow if the number data columns does not match the number of header columns
            if (tblr%ncols /= tblr%nfields) then
                cycle
            end if
            imax = imax + 1
        end do
    endif
end function get_num_data_lines

! subroutine get_header_columns(unit, header_cols, nheader_cols, skip_rows, eof)
subroutine get_header_columns(eof)
    integer                     :: i
    integer                     :: eof
    ! integer, intent(in)         :: unit
    ! integer, intent(inout)      :: skip_rows
    ! integer, intent(out)        :: nheader_cols
    ! integer                     :: i
    ! character(len=:), allocatable :: left_str
    ! character(MAX_NAME_LEN), intent(out) :: header_cols(MAX_TABLE_COLS)

    ! character(len=MAX_LINE_LEN)  :: line
    ! character (len=80) :: titldum = ""!         |first line in file that generally is the title and it will be ignored.
    ! integer                         :: eof
    ! logical                         :: found_header_row

    eof = 0
    tblr%ncols = 0
    tblr%found_header_row = .false.

    rewind (tblr%unit)  ! reset file position to beginning
    read (tblr%unit,*,iostat=eof) tblr%titldum ! Read the first line and ignore it 
    tblr%skip_rows = tblr%skip_rows + 1

    if (eof == 0) then 
        do
            read(tblr%unit, '(A)', iostat=eof) tblr%line
            if (eof /= 0) exit  ! EOF
            tblr%line = adjustl(trim(tblr%line))
            call left_of_delim(tblr%line, '#', tblr%left_str)    ! remove comments
            if ( len(tblr%left_str) == 0) then              ! skip empty lines 
                tblr%skip_rows = tblr%skip_rows + 1
                cycle                  
            end if
            tblr%line = tblr%left_str
            if (.not. tblr%found_header_row) then                 ! check to see if the header row has not yet been processed
                tblr%found_header_row = .true.
                call split_line(tblr%line, tblr%header_cols, tblr%ncols) ! process header row into header columns
                do i = 1 , tblr%ncols
                    tblr%header_cols(i) = to_lower(trim(adjustl(tblr%header_cols(i))))
                end do
                tblr%skip_rows = tblr%skip_rows + 1
                exit
            end if
        end do
    end if

end subroutine get_header_columns

! subroutine get_data_table_row_fields(unit, fields, ndata_cols, nheader_cols, sub_name, nrow, skip_rows, eof)
subroutine get_data_fields(eof)
    integer, intent(out)          :: eof
    integer                       :: i
    ! integer, intent(in)         :: unit
    ! integer, intent(in)        d :: nheader_cols
    ! integer, intent(inout)      :: skip_rows
    ! integer, intent(out)        :: ndata_cols
    ! integer, intent(in)         :: nrow
    ! character(len=:), allocatable :: left_str
    ! character(MAX_NAME_LEN), intent(out) :: fields(MAX_TABLE_COLS)
    ! character(len=*), intent(in) :: sub_name
    ! character(MAX_LINE_LEN)     :: line

    do
        read(tblr%unit, '(A)', iostat=eof) tblr%line
        if (eof /= 0) exit
        tblr%line = adjustl(trim(tblr%line))

        ! get portion of line left of comment delimiter '#'
        call left_of_delim(tblr%line, '#', tblr%left_str)
        tblr%line = tblr%left_str

        ! skip empty lines
        if (len(tblr%left_str) == 0 ) then
            tblr%skip_rows = tblr%skip_rows + 1
            cycle ! get next line
        endif
        
        ! split data row into fields
        call split_line(tblr%line, tblr%data_fields, tblr%nfields)
        do i=1, tblr%nfields
            tblr%data_fields(i) = trim(adjustl(tblr%data_fields(i)))
        end do
        
        ! check for correct number of columns and if incorrect skip row with warning
        if (tblr%ncols /= tblr%nfields) then
            tblr%skip_rows = tblr%skip_rows + 1
            write(9001,'(A,I3, 3A)') 'Warning: Row ', tblr%nrow + tblr%skip_rows, ' in ', tblr%sub_name, ' has the wrong number of columns, skipping'
            print('(A,I3, 3A)'), 'Warning: Row ', tblr%nrow + tblr%skip_rows, ' in ', tblr%sub_name, ' has the wrong number of columns, skipping'
            cycle
        end if
        exit
    enddo
    
    return
end subroutine get_data_fields
end module utils





