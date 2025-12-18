module utils
    IMPLICIT NONE

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
    character(len=*), intent(in)                 :: line2
    ! The following line uses deferred-length strings for fields2 array, however, gfortran debugger has issues with it.
    ! The gfortran debugging issue can be worked around by using fixed-length strings instead 
    ! (uncomment the next line and comment out the following line to gfortran debugger to work).
    ! character(len=30), intent(out)                :: fields2(:)  
    character(len=*), intent(out)                :: fields2(:)
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
                            fields2(nfields) = adjustl(trim(line2(pos1:)))
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
                    fields2(nfields) = line2(pos1:)
                end if
                endif
                return
            end if
        end do
    end if

end subroutine split_line

    ! subroutine split(line, fields, sep, maxsplit)
    !---------------------------------------------------------------------------
    ! SUBROUTINE split
    !
    ! Purpase: Provides a routine that imics Python's str.split(sep=None, maxsplit=-1) function as closely as possible.
    !
    ! Author: Grok (xAI) after 12 iterations and extensive collaboration, testing, persistance and feedback from user fgeter
    ! Date:   December 16, 2025
    !
    ! Behaviour exactly matches Python's str.split(sep=None, maxsplit=-1)
    ! Fully compatible with gfortran 15.2
    ! The calling program/subroutine must contain the following declaration:
    !
    ! character(len=:), allocatable :: parts(:)
    !
    ! Usage
    !
    ! Parameters:
    !   line     (in)  : input string to split (character(len=*))
    !   fields   (out) : allocatable array of deferred-length strings containing
    !                    the split parts
    !   sep      (in, optional) : separator string
    !                             - If omitted: split on any whitespace and
    !                               collapse consecutive whitespace
    !                             - If present: split on exact sep, preserving
    !                               all empty fields (leading, trailing, consecutive)
    !   maxsplit (in, optional) : maximum number of splits to perform
    !                             - Default is -1 (no limit)
    !
    ! Usage examples:
    !   character(len=:), allocatable :: parts(:)
    !
    !   call split("  hello   world  ", parts)                 ! => ['hello', 'world']
    !   call split("apple,banana,cherry", parts, sep=",")      ! => ['apple', 'banana', 'cherry']
    !   call split(",,one,,two,", parts, sep=",")              ! => ['', '', 'one', '', 'two', '']
    !   call split("a b c d e", parts, maxsplit=2)             ! => ['a', 'b', 'c d e']
    !   call split("", parts)                                  ! => 0 elements (whitespace mode)
    !   call split("", parts, sep=",")                         ! => 1 empty string
    !
    ! Notes:
    !   - fields is automatically allocated and sized by the subroutine
    !   - When printing fields, use TRIM(fields(i)) to avoid trailing blanks
    !     (standard Fortran behaviour for fixed-length internal storage)
    !---------------------------------------------------------------------------

!     implicit none

!     character(len=*), intent(in)               :: line
!     character(len=:), allocatable, intent(out) :: fields(:)
!     character(len=*), optional, intent(in)     :: sep
!     integer, optional, intent(in)              :: maxsplit

!     integer                       :: max_splits, n, i, j, sep_len, capacity
!     character(len=:), allocatable :: s, temp(:)
!     logical                       :: ws_mode

!     max_splits = -1
!     if (present(maxsplit)) max_splits = maxsplit

!     ws_mode = .not. present(sep)

!     s = line

!     if (.not. ws_mode) then
!         sep_len = len(sep)
!         if (sep_len == 0) error stop "split(): empty separator"
!     end if

!     ! Empty/all-whitespace case
!     if (len_trim(s) == 0) then
!         if (ws_mode) then
!             allocate(character(len=0) :: fields(0))
!         else
!             allocate(character(len=0) :: fields(1))
!             fields(1) = ''
!         end if
!         return
!     end if

!     ! Allocate with sufficient length to avoid truncation
!     capacity = 16
!     allocate(character(len=len(line)) :: fields(capacity))
!     n = 0
!     i = 1

!     do
!         if (ws_mode) then
!             ! Skip whitespace
!             do while (i <= len(s) .and. verify(s(i:i), ' ') == 0)
!                 i = i + 1
!             end do
!             if (i > len(s)) exit

!             ! Find end of word
!             j = i
!             do while (j <= len(s) .and. verify(s(j:j), ' ') /= 0)
!                 j = j + 1
!             end do
!             j = j - 1
!         else
!             ! Find next separator
!             j = index(s(i:), sep)
!             if (j == 0) exit
!             j = i + j - 1
!         end if

!         n = n + 1
!         if (n > capacity) then
!             allocate(character(len=len(line)) :: temp(2*capacity))
!             temp(1:capacity) = fields(1:capacity)
!             call move_alloc(temp, fields)
!             capacity = 2 * capacity
!         end if

!         if (ws_mode) then
!             fields(n) = s(i:j)
!         else
!             fields(n) = s(i:j-1)  ! exclude separator
!         end if

!         if (ws_mode) then
!             i = j + 1
!         else
!             i = j + sep_len
!         end if

!         if (max_splits >= 0 .and. n == max_splits) exit
!     end do

!     ! Add remaining text
!     if (ws_mode) then
!         do while (i <= len(s) .and. verify(s(i:i), ' ') == 0)
!             i = i + 1
!         end do
!     end if

!     if (i <= len(s)) then
!         n = n + 1
!         if (n > capacity) then
!             allocate(character(len=len(line)) :: temp(2*capacity))
!             temp(1:capacity) = fields(1:capacity)
!             call move_alloc(temp, fields)
!             capacity = 2 * capacity
!         end if
!         fields(n) = s(i:)
!     end if

!     ! Trim to exact size
!     if (n < capacity) then
!         allocate(character(len=len(line)) :: temp(n))
!         temp(1:n) = fields(1:n)
!         call move_alloc(temp, fields)
!     end if

! end subroutine split

end module utils





