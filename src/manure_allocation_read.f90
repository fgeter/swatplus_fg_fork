      subroutine manure_allocation_read
      
      use input_file_module
      use manure_allocation_module
      use mgt_operations_module
      use maximum_data_module
      use hydrograph_module
      use sd_channel_module
      use conditional_module
      use hru_module, only : hru
      
      implicit none 
      
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i = 0                !none       |counter
      integer :: k = 0                !none       |counter
      integer :: isrc = 0             !none       |counter
      integer :: imro = 0             !none       |number of manure allocation objects
      integer :: num_objs = 0
      integer :: itrn = 0
      integer :: idb = 0
      integer :: ihru = 0
      integer :: idb_man = 0
      
      eof = 0
      imax = 0
      
      !! read water allocation inputs

      inquire (file="manure_allo.mnu", exist=i_exist)
      if (.not. i_exist .or. "manure_allo.mnu" == "null") then
        allocate (mallo(0:0))
      else
      do 
        open (107,file="manure_allo.mnu")
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        db_mx%mallo_db = imax
        if (eof < 0) exit
        
        allocate (mallo(imax))
        
        do imro = 1, imax
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) mallo(imro)%name, mallo(imro)%rule_typ, mallo(imro)%src_obs, &
                                                    mallo(imro)%trn_obs
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          num_objs = mallo(imro)%src_obs
          allocate (mallo(imro)%src(num_objs))
          num_objs = mallo(imro)%trn_obs
          allocate (mallo(imro)%trn(num_objs))
                    
          !! read source object data
          do isrc = 1, mallo(imro)%src_obs
            read (107,*,iostat=eof) i
            mallo(imro)%src(i)%num = i
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) k, mallo(imro)%src(i)%mois_typ, mallo(imro)%src(i)%manure_typ,      &
                   mallo(imro)%src(i)%lat, mallo(imro)%src(i)%long, mallo(imro)%src(i)%stor_init,       & 
                   mallo(imro)%src(i)%stor_max, mallo(imro)%src(i)%prod_mon
            
            !xwalk fert name with fertilizer data base
            do idb = 1, db_mx%fertparm
              if (mallo(imro)%src(i)%manure_typ == fertdb(idb)%fertnm) then
                mallo(imro)%src(i)%fertdb = idb
                exit
              endif
            end do

            !! xwalk the same name to a manure_om.frt entry.  manure drawn from an
            !! allocation source is chemically manure, not fertilizer, so when this
            !! resolves the application goes through pl_manure (manure_om.frt) and
            !! gets the CENTURY pool partitioning, instead of pl_fert
            !! (fertilizer.frt).  manure_db already carries the crosswalk in
            !! iorg_min, so go through it rather than matching manure_om directly.
            do idb = 1, db_mx%manureparm
              if (mallo(imro)%src(i)%manure_typ == manure_db(idb)%name) then
                mallo(imro)%src(i)%iorg_min = manure_db(idb)%iorg_min
                exit
              endif
            end do

            !! a source may name a manure_om entry without a manure_db wrapper
            if (mallo(imro)%src(i)%iorg_min == 0) then
              do idb = 1, db_mx%manure_om
                if (mallo(imro)%src(i)%manure_typ == manure_om(idb)%name) then
                  mallo(imro)%src(i)%iorg_min = idb
                  exit
                endif
              end do
            end if

            !! fatal.  the old behaviour was to fall through to pl_fert, which
            !! applied manure as though it were fertilizer - the defect this
            !! crosswalk exists to remove.  there is no safe silent fallback:
            !! pl_fert has no manure composition to work from.
            if (mallo(imro)%src(i)%iorg_min == 0) then
              write (*,*)    "ERROR: manure allocation source ",                        &
                trim(mallo(imro)%src(i)%manure_typ), " matches no manure_db.frt or ",   &
                "manure_om.frt entry"
              write (9001,*) "ERROR: manure allocation source ",                        &
                trim(mallo(imro)%src(i)%manure_typ), " matches no manure_db.frt or ",   &
                "manure_om.frt entry - manure cannot be applied through pl_fert"
              error stop
            end if
          end do
          
          !! read demand object data
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          do itrn = 1, num_objs
            isrc = mallo(imro)%src_obs
            allocate (mallo(imro)%trn(itrn)%withdr(isrc), source = 0.)
            allocate (mallo(imro)%trn(itrn)%withdr_m(isrc), source = 0.)
            allocate (mallo(imro)%trn(itrn)%withdr_y(isrc), source = 0.)
            allocate (mallo(imro)%trn(itrn)%withdr_a(isrc), source = 0.)
            
            read (107,*,iostat=eof) i
            mallo(imro)%trn(i)%num = i
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) k, mallo(imro)%trn(i)%ob_typ, mallo(imro)%trn(i)%ob_num,            &
              mallo(imro)%trn(i)%dtbl, mallo(imro)%trn(i)%right
            
            !! for hru irrigtion, need to xwalk with irrigation demand decision table
            if (mallo(imro)%trn(i)%ob_typ == "hru") then
              ihru = mallo(imro)%trn(i)%ob_num
              !! xwalk with lum decision table
              do idb = 1, db_mx%dtbl_lum
                if (mallo(imro)%trn(i)%dtbl == dtbl_lum(idb)%name) then
                  mallo(imro)%trn(i)%dtbl_num = idb
                  hru(ihru)%man_trn_dtbl = idb
                  do idb_man = 1, db_mx%chemapp_db 
                    if (dtbl_lum(idb)%act(1)%option == chemapp_db(idb_man)%name) then
                      mallo(imro)%trn(itrn)%manure_amt%app_method = idb_man
                      exit
                    end if
                  end do
                end if
              end do
            end if

          end do
          
        end do      !loop for number of manure allocation objects

        exit
      end do    !loop for end of file exit
      end if    !if manure allocation file exists 
      close(107)

      return
      end subroutine manure_allocation_read