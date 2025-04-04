      subroutine wet_initial (iihru)
      
      use reservoir_module
      use reservoir_data_module
      use reservoir_data_module
      use hydrograph_module
      use hru_module, only : hru
      use maximum_data_module
      use water_body_module
      use soil_module
      use conditional_module
      use constituent_mass_module
      use res_salt_module
      use res_cs_module
      
      implicit none
      
      integer, intent (in) :: iihru     !none       |
      integer :: iprop = 0              !           |  
      integer :: init_om = 0
      integer :: init = 0               !           |
      integer :: iweir = 0              !           |
      integer :: icon = 0               !           |
      integer :: isalt = 0              !           |salt ion counter
      real :: x1 = 0.                   !           |
      real :: wet_h = 0.                !           |
      real :: wet_h1 = 0.               !           | 
      real :: wet_fr = 0.               !           |
      integer :: ihyd = 0                !none       |counter
      integer :: irel = 0                !none       |counter
      integer :: ised = 0                !none       |counter
      integer :: inut = 0                !none       |counter
      integer :: isp_ini = 0             !none       |counter
      integer :: ics = 0                 !none       |counter
      integer :: isstor = 0              !none       |counter
  
      iprop = hru(iihru)%dbs%surf_stor
      iweir = wet_ob(iihru)%iweir
        
      !! check if hru can store surface water
      if (iprop > 0) then
        !! crosswalk with wetland data files
        if (hru(iihru)%dbsc%surf_stor /= "null") then
          do isstor = 1, db_mx%wet_dat
            if (hru(iihru)%dbsc%surf_stor == wet_dat_c(isstor)%name) then
              hru(iihru)%dbs%surf_stor = isstor
              hru(iihru)%wet_db = isstor
                  
              !! initialize orgaincs and minerals in water
              do isp_ini = 1, db_mx%res_init
                if (wet_dat_c(isstor)%init == res_init_dat_c(isp_ini)%init) then
                  wet_dat(isstor)%init = isp_ini
                  !! initial organic mineral
                  do ics = 1, db_mx%om_water_init
                    if (res_init_dat_c(isp_ini)%org_min == om_init_name(ics)) then
                      wet_init(isp_ini)%org_min = ics
                      exit
                    end if
                  end do
                  !! initial pesticides
                  do ics = 1, db_mx%pestw_ini
                    if (res_init_dat_c(isp_ini)%pest == pest_init_name(ics)) then
                      wet_init(isp_ini)%pest = ics
                      exit
                    end if
                  end do
                  !! initial pathogens
                  do ics = 1, db_mx%pathw_ini
                    if (res_init_dat_c(isp_ini)%path == path_init_name(ics)) then
                      wet_init(isp_ini)%path = ics
                      exit
                    end if
                  end do
                  !! initial heavy metals
                  !! initial salts
                end if
              end do

              !! xwalk hydrology inputs
              do ihyd = 1, db_mx%wet_hyd
                if (wet_hyddb(ihyd)%name == wet_dat_c(isstor)%hyd) then
                  wet_hyd(iihru) = wet_hyddb(ihyd)
                  wet_dat(isstor)%hyd = ihyd
                  exit
                end if
              end do
       
              !! xwalk release decision table
              do irel = 1, db_mx%dtbl_res
                if (dtbl_res(irel)%name == wet_dat_c(isstor)%release) then
                  wet_dat(isstor)%release = irel
                  exit
                  end if
              end do      
   
              !! xwalk with sediment inputs
              do ised = 1, db_mx%res_sed
                if (res_sed(ised)%name == wet_dat_c(isstor)%sed) then
                  wet_prm(iihru)%sed = res_sed(ised)
                  !! d50 -micro meters
                  wet_prm(iihru)%sed_stlr_co = exp(-0.184 * wet_prm(iihru)%sed%d50)
                  wet_dat(isstor)%sed = ised
                  wet_prm(iihru)%soln_stl_fr = 0.2
                  wet_prm(iihru)%solp_stl_fr = 0.2
                  exit
                end if
              end do      
   
              !! xwalk with nutrient inputs
              do inut = 1, db_mx%res_nut
                if (res_nut(inut)%name == wet_dat_c(isstor)%nut) then
                  wet_prm(iihru)%nut = res_nut(inut)
                  wet_dat(isstor)%nut = inut
                  exit
                end if
              end do   
        
              if (wet_dat(isstor)%init == 0) write (9001,*) wet_dat_c(isstor)%init, " not found (wet-init)"
              if (wet_dat(isstor)%hyd == 0) write (9001,*) wet_dat_c(isstor)%hyd, " not found (wet-hyd)"
              if (wet_dat(isstor)%release == 0) write (9001,*) wet_dat_c(isstor)%release, " not found (wet-release)"
              if (wet_dat(isstor)%sed == 0) write (9001,*) wet_dat_c(isstor)%sed, " not found (wet-sed)"
              if (wet_dat(isstor)%nut == 0) write (9001,*) wet_dat_c(isstor)%nut, " not found (wet-nut)"

              exit
            end if
          end do
        end if
        if (hru(iihru)%dbs%surf_stor == 0 .and. hru(iihru)%dbsc%surf_stor /= 'null')       & 
              write (9001,*) hru(iihru)%dbsc%surf_stor,"not found (wetland.wet)"
       
            
        !! initialize parameters
        hru(iihru)%wet_hc = wet_hyd(iihru)%k  !mm/hr
        !! ha*mm*10. => m**3  - assume entire hru is wet and don't use fractional inputs (for simplicity)
        wet_ob(iihru)%evol = hru(iihru)%area_ha * wet_hyd(iihru)%edep * 10.  ! * wet_hyd(iihru)%esa
        wet_ob(iihru)%pvol = hru(iihru)%area_ha * wet_hyd(iihru)%pdep * 10.  ! * wet_hyd(iihru)%psa
        wet_ob(iihru)%psa = wet_hyd(iihru)%psa * hru(iihru)%area_ha 
        wet_ob(iihru)%esa = wet_hyd(iihru)%esa * hru(iihru)%area_ha 
        !! set initial weir height to principal depth - m
        if (db_mx%res_weir > 0.and.wet_ob(iihru)%iweir>0) then !if available, read from weir.res Jaehak 2022
          wet_ob(iihru)%weir_hgt = res_weir(iweir)%h  !m weir height
          wet_ob(iihru)%weir_wid = res_weir(iweir)%w  !m, weir width
          !update pvol/evol according to weir height for paddy weir discharge. Jaehak 2023
          wet_ob(iihru)%pvol = hru(iihru)%area_ha * res_weir(iweir)%h * 10.**4  ! m3
          if (wet_ob(iihru)%evol < wet_ob(iihru)%pvol*1.2) then
            wet_ob(iihru)%evol = wet_ob(iihru)%pvol * 1.2   
          endif
        else
          wet_ob(iihru)%weir_hgt = wet_hyd(iihru)%pdep / 1000.  !m
          wet_ob(iihru)%weir_wid = 2.5  !m
        endif

        !!set initial n and p concentrations --> (ppm) * (m^3) / 1000 = kg    !! ppm = t/m^3 * 10^6
        init = wet_dat(iprop)%init
        init_om = wet_init(init)%org_min
        wet(iihru) = om_init_water(init_om)
        call res_convert_mass (wet(iihru), wet_ob(iihru)%pvol)

        !!set initial salt ion concentrations and mass (rtb salt)
        if(cs_db%num_salts > 0) then
          icon = wet_dat(iprop)%salt !database to use (from salt_res file)
          if(icon > 0) then
            !initial concentrations (g/m3) and mass (kg)
            do isalt=1,cs_db%num_salts
            wet_water(iihru)%saltc(isalt) = res_salt_data(icon)%c_init(isalt) !g/m3
            wet_water(iihru)%salt(isalt) = (res_salt_data(icon)%c_init(isalt) / 1000.) * wet(iihru)%flo !kg
            enddo
          else
            !initial concentrations (g/m3) and mass (kg)
            do isalt=1,cs_db%num_salts
            wet_water(iihru)%saltc(isalt) = 0.
            wet_water(iihru)%salt(isalt) = 0.
            enddo
          endif
        endif
          
        !!set initial constituent concentrations and mass (rtb cs)
        if(cs_db%num_cs > 0) then
          icon = wet_dat(iprop)%cs !database to use (from cs_res file)
          if(icon > 0) then
            !initial concentrations (g/m3)
            wet_water(iihru)%csc(1) = res_cs_data(icon)%c_seo4
            wet_water(iihru)%csc(2) = res_cs_data(icon)%c_seo3
            wet_water(iihru)%csc(3) = res_cs_data(icon)%c_born
            !initial mass (kg)
            wet_water(iihru)%cs(1) = (res_cs_data(icon)%c_seo4 / 1000.) * wet(iihru)%flo !kg
            wet_water(iihru)%cs(2) = (res_cs_data(icon)%c_seo3 / 1000.) * wet(iihru)%flo !kg
            wet_water(iihru)%cs(3) = (res_cs_data(icon)%c_born / 1000.) * wet(iihru)%flo !kg
          else
            !initial concentrations (g/m3)
            wet_water(iihru)%csc(1) = 0.
            wet_water(iihru)%csc(2) = 0.
            wet_water(iihru)%csc(3) = 0.
            !initial mass (kg)
            wet_water(iihru)%cs(1) = 0.
            wet_water(iihru)%cs(2) = 0.
            wet_water(iihru)%cs(3) = 0.
          endif
        endif
          
        !wet(iihru)%flo = om_init_water(init_om)%flo * wet_ob(iihru)%pvol !Jaehak 2022
        wet_om_init(iihru) = wet(iihru)
  
        !! update surface area
        !! wetland on hru - solve quadratic to find new depth
        wet_wat_d(iihru)%area_ha = 0.
        if (wet(iihru)%flo > 0.) then
          x1 = wet_hyd(iihru)%bcoef ** 2 + 4. * wet_hyd(iihru)%ccoef * (1. - wet(iihru)%flo / wet_ob(iihru)%pvol)
          if (x1 < 1.e-6) then
            wet_h = 0.
          else
            wet_h1 = (-wet_hyd(iihru)%bcoef - sqrt(x1)) / (2. * wet_hyd(iihru)%ccoef)
            wet_h = wet_h1 + wet_hyd(iihru)%bcoef
          end if
          wet_fr = (1. + wet_hyd(iihru)%acoef * wet_h)
          wet_fr = min(wet_fr,1.)
          wet_wat_d(iihru)%area_ha = hru(iihru)%area_ha * wet_hyd(iihru)%psa * wet_fr
        end if 
  
      end if

      return
      end subroutine wet_initial