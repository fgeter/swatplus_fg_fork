      subroutine pl_waterup
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine distributes potential plant evaporation through
!!    the root zone and calculates actual plant water use based on soil
!!    water availability. Also estimates water stress factor.     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    epco(:)     |none          |plant water uptake compensation factor (0-1)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ep_day      |mm H2O        |actual amount of transpiration that occurs
!!                               |on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    gx          |
!!    ir          |
!!    j           |none          |HRU number
!!    k           |none          |counter (soil layer)
!!    sum         |
!!    sump        |
!!    wuse        |mm H2O        |water uptake by plants in each soil layer
!!    sum_wuse    |mm H2O        |water uptake by plants from all layers
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use plant_data_module
      use basin_module
      use hru_module, only : hru, ihru, epmax, ipl, ep_day, uptake
      use soil_module
      use plant_module
      use urban_data_module
      use constituent_mass_module !rtb salt
      use salt_data_module !rtb salt
      
      implicit none
      
      external :: salt_chem_soil_single
      
      integer :: j = 0       !none      |hru number
      integer :: k = 0       !none      |counter 
      integer :: ir = 0      !none      |flag to denote bottom of root zone reached
      integer :: idp = 0     !          | 
      integer :: ulu = 0     !          |urban land use from urban.urb
      integer :: isalt = 0   !          |salt ion counter (rtb salt)
      real :: sum = 0.       !          |
      real :: sum_wuse = 0.  !mm H2O    |water uptake by plants from all layers
      real :: sum_wusep = 0. !mm H2O    |previous water uptake by plants from all layers
      real :: sump = 0.      !          |
      real :: gx = 0.        !mm        |lowest depth in layer from which nitrogen
                             !          |may be removed
      real :: wuse = 0.      !mm H2O    |water uptake by plants in each soil layer
      real :: satco = 0.     !          | 
      real :: scparm = 0.    !          |  
      real :: rz_sw = 0.     !mm H2O    |soil water in the root zone
      real :: rz_fc = 0.     !mm H2O    |field capacity water of the root zone
      real :: rz_ul = 0.     !mm H2O    |saturated water of the root zone
      real :: frac = 0.      !none      |fraction of soil layer k inside the root zone
      real :: rz_tmp = 0.    !deg C*mm  |thickness-weighted soil temperature of the root zone
      real :: rz_thk = 0.    !mm        |root zone thickness used for rz_tmp
      real :: t_rz = 0.      !deg C     |root-zone soil temperature
      real :: tfac = 0.      !none      |fraction of the aeration stress applied (0-1)
      real, parameter :: aer_t50 = 10.  !deg C |root-zone soil temp at which half the aeration stress applies
      real, parameter :: aer_w = 2.     !deg C |width of the logistic temperature response
      real :: reduc_salt = 0.                                                !rtb salt
      real :: reduc_salt_min = 0.                                            !rtb salt
      real :: sw_decrease = 0.                                               !rtb salt
      real :: salt_decrease = 0.                                             !rtb salt
      real :: theta_w = 0.                                                   !rtb salt
      real :: soil_TDS_sat = 0.                                              !rtb salt
      real :: soil_ECe = 0.                                                  !rtb salt
      real :: a_val = 0.                                                     !rtb salt
      real :: b_val = 0.                                                     !rtb salt
      real :: depth = 0.                                                     !rtb salt

      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt
       
      !! compute aeration stress from the water in the root zone (not the whole profile)
      !! the layer that holds the root tip counts in proportion to the rooted thickness;
      !! layer 1 always counts, so a plant with no roots yet sees the surface layer
      rz_sw = 0.
      rz_fc = 0.
      rz_ul = 0.
      rz_tmp = 0.
      rz_thk = 0.
      do k = 1, soil(j)%nly
        if (k == 1) then
          frac = 1.
        else if (soil(j)%phys(k)%thick > 1.e-6) then
          frac = (pcom(j)%plg(ipl)%root_dep - soil(j)%phys(k-1)%d) / soil(j)%phys(k)%thick
          frac = Max (0., Min (1., frac))
        else
          frac = 0.
        end if
        if (frac <= 0.) exit
        rz_sw = rz_sw + frac * soil(j)%phys(k)%st
        rz_fc = rz_fc + frac * soil(j)%phys(k)%fc
        rz_ul = rz_ul + frac * soil(j)%phys(k)%ul
        rz_tmp = rz_tmp + frac * soil(j)%phys(k)%thick * soil(j)%phys(k)%tmp
        rz_thk = rz_thk + frac * soil(j)%phys(k)%thick
        if (frac < 1.) exit
      end do
      if (rz_sw > rz_fc .and. rz_ul - rz_fc > 1.e-6) then
        satco = (rz_sw - rz_fc) / (rz_ul - rz_fc)
        scparm = 100. * (satco - pldb(idp)%aeration) / (1.0001 - pldb(idp)%aeration)
        if (scparm > 0.) then
          pcom(j)%plstr(ipl)%strsa = 1. - (scparm / (scparm + Exp(2.9014 - .03867 * scparm)))
          !! scale aeration stress by root-zone soil temperature: oxygen demand of roots and
          !! soil microbes falls in cold soil, so waterlogging does little harm then.
          !! logistic in temperature, half strength at aer_t50; none at or below 0 C.
          !! aer_t50 and aer_w are first estimates (Goodwater Creek Field 1 winter wheat)
          !! to be refined with more cold-annual crop data.
          tfac = 1.
          if (rz_thk > 1.e-6) then
            t_rz = rz_tmp / rz_thk
            if (t_rz <= 0.) then
              tfac = 0.
            else
              tfac = 1. - 1. / (1. + Exp((t_rz - aer_t50) / aer_w))
            end if
          end if
          pcom(j)%plstr(ipl)%strsa = 1. - tfac * (1. - pcom(j)%plstr(ipl)%strsa)
        else
          pcom(j)%plstr(ipl)%strsa = 1.
        end if
      end if

      !! compute limiting water stress
      if (epmax(ipl) <= 1.e-6) then
        pcom(j)%plstr(ipl)%strsw = 1.
      else
        !! initialize variables
        gx = 0.
        ir = 0
        sump = 0.
        wuse = 0.
        sum_wuse = 0.
        sum_wusep = 0.
        
        sw_decrease = 0.
        salt_decrease = 0.
        reduc_salt = 1.
        reduc_salt_min = 1.
        depth = 0.
        pcom(j)%plcur(ipl)%uptake(:) = 0.

        do k = 1, soil(j)%nly
          
          if (ir > 0) exit

          if (pcom(j)%plg(ipl)%root_dep <= soil(j)%phys(k)%d) then
            gx = pcom(j)%plg(ipl)%root_dep
            ir = k
          else
            gx = soil(j)%phys(k)%d
          end if

          if (pcom(j)%plg(ipl)%root_dep <= 0.01) then
            sum = epmax(ipl) / uptake%water_norm
          else
            sum = epmax(ipl) * (1. - Exp(-uptake%water_dis * gx / pcom(j)%plg(ipl)%root_dep)) / uptake%water_norm
          end if

          !! let second layer compensate - 10 mm layer causes problems when root depth is shallow
          if (k == 2) then
            wuse = sum      !epco is always 1.0 for second layer
          else
            wuse = sum - sump * (1. - pcom(j)%plcur(ipl)%epco)
          end if
          
          ! adjust for impervious area
          ulu = hru(j)%luse%urb_lu
          !wuse = wuse * urbdb(ulu)%fcimp
          wuse = amin1 (wuse, soil(j)%phys(k)%st)
          
          sum_wuse = sum_wuse + wuse
          if (sum_wuse > epmax(ipl)) then
            wuse = epmax(ipl) - sum_wusep
            sum_wuse = epmax(ipl)
          end if
          sump = sum
          sum_wusep = sum_wuse

          if (soil(j)%phys(k)%st < wuse) then
            wuse = soil(j)%phys(k)%st
          end if

          !rtb salt
          !adjust uptake if soil salinity has reached threshold for crop 
          if(salt_tol_sim.eq.1) then
          depth = depth + soil(j)%phys(k)%thick !depth (mm) of soil layer below land surface
          !only proceed if salts are simulated and soil layer is within rooting depth
          if(cs_db%num_salts > 0 .and. depth < pcom(j)%plg(ipl)%root_dep) then
            reduc_salt = 1.
            
            !retrieve salt ion concentrations in soil water, based on additional salt mineral dissolution
            !(to mimic the procedure in the lab: saturated paste extract)
            !calculate salt ion concentration under saturated conditions (dilution)
            theta_w = soil(j)%phys(k)%st / soil(j)%phys(k)%ul !water content
            do isalt=1,cs_db%num_salts
              soil_salt_conc(isalt) = cs_soil(j)%ly(k)%saltc(isalt) * theta_w
            enddo
            
            !modify salt ion concentrations based on precipitation-dissolution reactions
            if(theta_w.gt.(1e-5)) then
              call salt_chem_soil_single(j,k,theta_w)
            endif
            
            !get TDS (mg/L) of the soil water, at saturation
            soil_TDS_sat = 0.
            do isalt=1,cs_db%num_salts
              soil_TDS_sat = soil_TDS_sat + soil_salt_conc(isalt)
            enddo
            
            !get the ECe value (dS/m)
            soil_ECe = soil_TDS_sat / salt_tds_ec
            
            !compute percent of potential soil water uptake, using yield-ECe relationship based on the
            !HRU crop type (idplt)
            a_val = salt_stress_a(idp) !threshold
            b_val = salt_stress_b(idp) !slope
            
            !only proceed if a and b values are provided (in "salt_plants" file)
            if(a_val.ne.0) then
              !if gypsiferous soil, increase a_val by 2 dS/m
              if(salt_soil_type.eq.1) then
                a_val = a_val + 2
              endif
              !calculate fraction of ET if soil_ECe is above the threshold value (a)
              if(soil_ECe.gt.a_val) then 
                reduc_salt = (100 - (b_val*(soil_ECe-a_val))) / 100
                if(reduc_salt.gt.1) reduc_salt = 1.
                if(reduc_salt.lt.0) reduc_salt = 0.
              endif
            endif

            !compute salinity stress on crop - take maximum from all soil layers
            if(reduc_salt .lt. reduc_salt_min) then
              reduc_salt_min = reduc_salt
            endif

          endif
          endif
          
          soil(j)%phys(k)%st = Max(1.e-6, soil(j)%phys(k)%st - wuse)
          pcom(j)%plcur(ipl)%uptake(k) = wuse
          
        end do      !! soil layer loop
        
        !! update total soil water in profile
        soil(j)%sw = 0.
        do k = 1, soil(j)%nly
          soil(j)%sw = soil(j)%sw + soil(j)%phys(k)%st
        end do

        !rtb salt - compute stress caused by high soil salinity
        pcom(j)%plstr(ipl)%strss = 1
        if(salt_tol_sim.eq.1) then
          if(cs_db%num_salts.gt.0) then
            pcom(j)%plstr(ipl)%strss = reduc_salt_min
          endif
        endif
        
        !new epco adjustment requires epmax adjustment of water stress is too high
        pcom(j)%plstr(ipl)%strsw = sum_wuse / epmax(ipl)
        
        ep_day = ep_day + sum_wuse
      end if

      return
      end subroutine pl_waterup