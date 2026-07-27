      subroutine res_sediment

      use reservoir_data_module
      use reservoir_module
      use conditional_module
      use climate_module
      use time_module
      use hydrograph_module
      use water_body_module
      
      implicit none
                   !              |  
      real :: sed_ppm
      real :: sil_ppm
      real :: cla_ppm

      if (wbody%flo < 1.e-6) then
        ! reservoir is empty
        wbody = hz
        wbody%sed = 0. !Jaehak 2025
        ht2%sed = 0.
        sed_ppm = 1.e-6
        sil_ppm = 1.e-6
        cla_ppm = 1.e-6
      else

        !! compute concentrations
        !! wbody%flo is guaranteed > 1.e-6 by the guard above, but a
        !! sediment mass that has shrunk to a denormal over many simulated
        !! years would still underflow on division under
        !! -ffpe-trap=underflow; such a value is physically zero (same
        !! class as organic_mineral_mass_module's fmul guard, but for a
        !! divide instead of a multiply).
        sed_ppm = 0.
        if (abs(wbody%sed) >= 1.e-30) sed_ppm = 1000000. * wbody%sed / wbody%flo
        sed_ppm = Max(1.e-6, sed_ppm)
        sil_ppm = 0.
        if (abs(wbody%sil) >= 1.e-30) sil_ppm = 1000000. * wbody%sil / wbody%flo
        sil_ppm = Max(1.e-6, sil_ppm)
        cla_ppm = 0.
        if (abs(wbody%cla) >= 1.e-30) cla_ppm = 1000000. * wbody%cla / wbody%flo
        cla_ppm = Max(1.e-6, cla_ppm)

        !! compute change in sediment concentration due to settling
        if (sed_ppm > wbody_prm%sed%nsed) then
          sed_ppm = (sed_ppm - wbody_prm%sed%nsed) * wbody_prm%sed_stlr_co + wbody_prm%sed%nsed
          sed_ppm = Max (sed_ppm, wbody_prm%sed%nsed)
          !! update wetland sediment after settling
          wbody%sed = sed_ppm * wbody%flo / 1000000.
          !! calculate sediment in the outflow and subtract from wetland
          !! sed_ppm is bounded away from 0 above, but ht2%flo (today's
          !! outflow) can legitimately be ~0/denormal.
          ht2%sed = 0.
          if (abs(ht2%flo) >= 1.e-30) ht2%sed = sed_ppm * ht2%flo / 1000000.
          wbody%sed = Max(0.,wbody%sed - ht2%sed)
          
          !! assume all sand aggregates and gravel settles
          wbody%sil = 0.
          wbody%cla = 0.
          wbody%san = 0.
          wbody%sag = 0.
          wbody%lag = 0.
          wbody%grv = 0.
        end if

        !! compute sediment leaving reservoir - ppm -> t
        !ht2%sed = sed_ppm * ht2%flo / 1000000.
        !wbody%sed = wbody%sed - ht2%sed

      end if

      return
      end subroutine res_sediment