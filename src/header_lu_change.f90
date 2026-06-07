     subroutine header_lu_change
    
     use basin_module
     use output_path_module
     
     implicit none 
!!   open lu_change output file 
        if (pco%csvout == "n") then
          call open_output_file(3612, "lu_change_out.txt", 800)
          write (3612,*) bsn%name, prog
          write (3612,100) 
        end if
100     format (1x,'         hru','       year','         mon','         day','     operation', &
        '   lu_before','         lu_after')  
        if (pco%csvout == "n") then
          write (9000,*) "DTBL                      lu_change_out.txt"
        end if
         
      return
      end subroutine header_lu_change  