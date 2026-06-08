!!     ~ ~ ~ MODULE PURPOSE ~ ~ ~
!!     output_path_module centralises all output file path handling for SWAT+.
!!
!!     The module solves the problem that arises when the -i command-line flag
!!     changes the process working directory (CWD) to the input data folder:
!!     without an explicit output path, every output file would be written into
!!     that input folder rather than where the user expects.
!!
!!     How it works:
!!       1. Before chdir() is called in main, init_output_path() stores an
!!          absolute path in the module-level variable out_path and creates the
!!          directory if it does not already exist.
!!       2. Every output open() call goes through open_output_file(), which
!!          prepends out_path to the filename so the OS resolves the file in
!!          the correct directory regardless of the current CWD.
!!       3. The cmdline_outpath_set flag prevents readcio_read from overriding
!!          a path that was already set via the -o command-line argument.
!!
!!     Output path precedence (highest to lowest):
!!       -o command-line argument
!!       file.cio out_path setting
!!       current working directory (out_path = "")

      module output_path_module

      implicit none

      !! Absolute path prepended to every output filename; empty string means
      !! use the current working directory (no prefix added).
      character(len=256) :: out_path = ""

      !! Set to .true. when the output path was established from the command line
      !! (-o, or -i without -o).  Prevents readcio_read from overriding it with
      !! the out_path value read from file.cio.
      logical :: cmdline_outpath_set = .false.

      contains

!!    ~ ~ ~ SUBROUTINE: init_output_path ~ ~ ~
!!    PURPOSE: Set the module-level out_path variable and ensure the directory exists.
!!
!!    Called from two places:
!!      - main (before chdir) when -o or -i is given on the command line
!!      - readcio_read (after chdir) when file.cio specifies an out_path,
!!        unless cmdline_outpath_set is already .true.
!!
!!    How it works:
!!      1. Detects the OS at runtime by checking the "OS" environment variable.
!!      2. Normalises path separators for the detected OS and strips trailing
!!         separators for consistent processing.
!!      3. Runs mkdir (mkdir -p on Unix, cmd /c mkdir on Windows) to create the
!!         directory and any missing parent directories.  mkdir -p is idempotent
!!         so calling this on an existing directory is harmless.
!!      4. Verifies the directory exists with INQUIRE; falls back to a shell
!!         test if INQUIRE returns an unexpected result.
!!      5. Stores the path with a trailing separator in out_path so callers can
!!         simply concatenate a filename: trim(out_path) // "myfile.csv"

      subroutine init_output_path(path_in)

      implicit none

      character(len=*), intent(in) :: path_in
      character(len=256) :: path_work, path_mkdir
      character(len=512) :: cmd
      character(len=32) :: os_env
      integer :: i, path_len, stat
      logical :: is_windows
      logical :: dir_exists

      !! Detect OS - Runtime check is more robust if preprocessor fails
      is_windows = .false.
      call get_environment_variable("OS", os_env, status=stat)
      if (stat == 0 .and. index(os_env, "Windows") > 0) then
         is_windows = .true.
      end if

      !! If null or empty, use current directory (no prefix needed)
      if (trim(path_in) == "null" .or. trim(path_in) == "" .or. &
          trim(path_in) == "NULL" .or. trim(path_in) == " ") then
        out_path = ""
        return
      end if

      !! Copy and process the path
      path_work = trim(adjustl(path_in))
      path_len = len_trim(path_work)

      !! Handle path separators based on OS
      if (is_windows) then
        !! On Windows: convert forward slashes to backslashes
        do i = 1, path_len
          if (path_work(i:i) == '/') path_work(i:i) = '\'
        end do

        !! Check if this looks like a Unix absolute path on Windows (starts with /)
        !! If so, prepend C:
        if (path_in(1:1) == '/') then
          path_work = 'C:' // trim(path_work)
          path_len = len_trim(path_work)
        end if

        !! Remove trailing backslash(es) for consistent processing
        do while (path_len > 0 .and. path_work(path_len:path_len) == '\')
          path_work(path_len:path_len) = ' '
          path_len = path_len - 1
        end do
      else
        !! On Unix: convert backslashes to forward slashes
        do i = 1, path_len
          if (path_work(i:i) == '\') path_work(i:i) = '/'
        end do

        !! Check if this looks like a Windows path on Unix
        if (index(path_work, ':') > 0) then
          write (*,*) "! ERROR: cannot create path specified in file.cio (out_path)"
          write (*,*) "  > Windows-style path detected on Unix system: ", trim(path_in)
          stop 1
        end if

        !! Remove trailing slash(es) for consistent processing
        do while (path_len > 1 .and. path_work(path_len:path_len) == '/')
          path_work(path_len:path_len) = ' '
          path_len = path_len - 1
        end do
      end if

      !! Store path for mkdir (without trailing separator)
      path_mkdir = trim(path_work)

      !! Add trailing separator for out_path (used when building file paths)
      if (is_windows) then
        out_path = trim(path_work) // '\'
      else
        out_path = trim(path_work) // '/'
      end if

      !! Create the directory if it does not exist; mkdir -p / cmd mkdir are
      !! both idempotent so this is safe to call even when the directory exists
      if (is_windows) then
        !! Windows: use cmd /c to run mkdir and suppress output
        cmd = 'cmd /c mkdir "' // trim(path_mkdir) // '" 2>NUL'
      else
        !! Unix/macOS: mkdir -p creates parent directories and does not error
        !! if the directory already exists; output redirected to suppress noise
        cmd = 'mkdir -p "' // trim(path_mkdir) // '" 2>/dev/null'
      end if
      call execute_command_line(trim(cmd), wait=.true., exitstat=stat)

      !! Verify directory exists using INQUIRE
      inquire(file=trim(path_mkdir), exist=dir_exists)

      !! If INQUIRE did not confirm existence, fall back to a shell test
      if (.not. dir_exists) then
        if (is_windows) then
           cmd = 'cmd /c "if exist "' // trim(path_mkdir) // '" (exit 0) else (exit 1)"'
        else
           cmd = 'test -d "' // trim(path_mkdir) // '"'
        end if
        call execute_command_line(trim(cmd), wait=.true., exitstat=stat)

        if (stat /= 0) then
          write (*,*) "! ERROR: cannot create or access output path"
          write (*,*) "  > Path: ", trim(path_in)
          stop 1
        end if
      end if

      write (*,*) "Output path set to: ", trim(out_path)

      return
      end subroutine init_output_path

!!    ~ ~ ~ FUNCTION: get_output_filename ~ ~ ~
!!    PURPOSE: Return the full path for an output file by prepending out_path.
!!
!!    If out_path is empty (no -o given and file.cio has no out_path setting)
!!    the filename is returned unchanged and the OS resolves it against the
!!    current working directory.

      function get_output_filename(filename) result(full_path)

      implicit none

      character(len=*), intent(in) :: filename
      character(len=512) :: full_path

      if (len_trim(out_path) > 0) then
        full_path = trim(out_path) // trim(filename)
      else
        full_path = trim(filename)
      end if

      return
      end function get_output_filename

!!    ~ ~ ~ SUBROUTINE: open_output_file ~ ~ ~
!!    PURPOSE: Open an output file unit using the managed output path.
!!
!!    This is the standard wrapper used throughout the codebase instead of a
!!    direct OPEN statement for output files.  It calls get_output_filename to
!!    prepend out_path and then opens the unit, optionally with a record length.
!!
!!    Usage:
!!      call open_output_file(unit_number, 'myfile.csv')
!!      call open_output_file(unit_number, 'myfile.txt', recl_val=1500)

      subroutine open_output_file(iunit, filename, recl_val)

      implicit none

      integer, intent(in) :: iunit                   !! unit number
      character(len=*), intent(in) :: filename       !! output filename (no path)
      integer, intent(in), optional :: recl_val      !! record length (optional)
      character(len=512) :: full_path

      full_path = get_output_filename(filename)

      if (present(recl_val)) then
        open (iunit, file=trim(full_path), recl=recl_val)
      else
        open (iunit, file=trim(full_path))
      end if

      return
      end subroutine open_output_file

!!    ~ ~ ~ FUNCTION: is_absolute ~ ~ ~
!!    PURPOSE: Return .true. if filepath is an absolute (not relative) path.
!!
!!    Used in main before chdir() to determine whether a -i or -o argument
!!    needs exec_cwd prepended to make it absolute.  Relative paths must be
!!    resolved to absolute before chdir() changes the CWD, otherwise the OS
!!    would interpret them relative to the new (input) directory instead of
!!    the directory where the executable was launched.
!!
!!    Detection rules:
!!      '/'  as first character  ->  Unix/macOS absolute path
!!      '\'  as first character  ->  Windows UNC path (\\server\share)
!!      ':'  at character index 2 ->  Windows drive-letter path (C:\, D:/)

      logical function is_absolute(filepath)

      implicit none

      character(len=*), intent(in) :: filepath
      character(len=1) :: first_char

      first_char = filepath(1:1)
      if (first_char == '/' .or. first_char == '\' .or. &
          index(filepath, ':') == 2) then
        is_absolute = .true.
      else
        is_absolute = .false.
      end if

      end function is_absolute

      end module output_path_module
