!!     ~ ~ ~ SUBROUTINE PURPOSE ~ ~ ~
!!     readcio_read reads the file.cio control file and populates the input_file
!!     module variables that point to every other input file used by SWAT+.
!!
!!     Output path handling:
!!       file.cio contains an optional out_path field that historically set the
!!       directory where SWAT+ writes its output files.  This subroutine still
!!       reads that field, but only calls init_output_path() with the value if
!!       cmdline_outpath_set is .false.  When the user supplies -o (or -i without
!!       -o) on the command line, the path is established in main before this
!!       subroutine runs and cmdline_outpath_set is .true., so the file.cio
!!       value is read but silently ignored.  This preserves backward
!!       compatibility while letting command-line arguments take precedence.
!!
!!     Input path handling:
!!       file.cio is opened with a plain relative path ("file.cio").  By the
!!       time this subroutine is called from proc_bsn, main has already executed
!!       chdir() to the input directory (if -i was given), so the OS resolves
!!       "file.cio" against that directory automatically.

       subroutine readcio_read

       use input_file_module
       use output_path_module

       implicit none

       character (len=80) :: titldum = ""
       character (len=15) :: name = ""
       character (len=256) :: out_path_value = ""
       character (len=512) :: line_buffer
       integer :: eof = 0
       integer :: idx = 0
       logical :: i_exist              !none       |check to determine if file exists
       integer :: i = 0

       eof = 0

       !! read file.cio
       inquire (file="file.cio", exist=i_exist)
       if (i_exist ) then
         open (107,file="file.cio")
         read (107,*) titldum
      do i = 1, 31
         read (107,*,iostat=eof) name, in_sim
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_basin
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_cli
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_con
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_cha
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_res
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_ru
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_hru
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_exco
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_rec
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_delr
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_aqu
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_herd
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_watrts
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_link
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_hyd
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_str
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_parmdb
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_ops
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_lum
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_chg
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_init
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_sol
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_cond
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_regs
         if (eof < 0) exit
!!!!!weather path code
         read (107,*,iostat=eof) name, in_path_pcp
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_path_tmp
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_path_slr
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_path_hmd
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_path_wnd
         if (eof < 0) exit
!!!!!weather path code
!!!!!output path code
         !! Read the whole line to handle spaces; list-directed input would
         !! stop at '/' treating it as a separator in a path like /home/user/out
         read (107,'(A)',iostat=eof) line_buffer
         if (eof < 0) then
           out_path_value = ""
         else
           !! Parse the line: skip the label (first word), then get the rest
           line_buffer = adjustl(line_buffer)
           idx = index(line_buffer, ' ')
           if (idx > 0) then
             out_path_value = adjustl(line_buffer(idx+1:))
           else
             out_path_value = ""
           end if
         end if
!!!!!output path code
         exit
      enddo
      endif

       close (107)

       !! Apply file.cio out_path only when the command line did not already
       !! set the output path; cmdline_outpath_set is raised in main when
       !! -o or -i is supplied so that command-line arguments take precedence
       if (.not. cmdline_outpath_set) then
         call init_output_path(out_path_value)
       end if

       return
      end subroutine readcio_read
