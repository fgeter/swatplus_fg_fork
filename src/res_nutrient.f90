      subroutine res_nutrient (iob)

      use reservoir_data_module
      use time_module
      use reservoir_module
      use hydrograph_module, only : resz, ob, ht2, wbody
      use climate_module
      
      implicit none      
      
      real, external :: theta
      
      integer, intent (in) :: iob
      real :: nitrok             !              |
      real :: phosk              !              |
      real :: nitrosolk          !              |
      real :: phossolk           !              |
      real :: tpco               !              |
      real :: chlaco             !              |
      integer :: iwst            !none          |weather station number
      real :: nsetlr             !              |
      real :: psetlr             !              |
      real :: nsolr              !              |
      real :: psolr              !              |
      real :: conc_n             !              |
      real :: conc_p             !              |
      real :: conc_soln          !              |
      real :: conc_solp          !              |
      real :: flo_tot            !              |wbody%flo + ht2%flo, guarded against ~0


      !! if reservoir volume less than 1 m^3, set all nutrient levels to
      !! zero and perform no nutrient calculations
      if (wbody%flo < 1.e-6) then
        wbody = resz
        return
      end if

      !! if reservoir volume greater than 1 m^3, perform nutrient calculations
      if (time%mo >= wbody_prm%nut%ires1 .and. time%mo <= wbody_prm%nut%ires2) then
        nsetlr = wbody_prm%nut%nsetlr1
        psetlr = wbody_prm%nut%psetlr1
      else
        nsetlr = wbody_prm%nut%nsetlr2
        psetlr = wbody_prm%nut%psetlr2
      endif
      nsolr = wbody_prm%nut%nsolr
      psolr = wbody_prm%nut%psolr

      !! n and p concentrations kg/m3 * kg/1000 t * 1000000 ppp = 1000
      !! wbody%flo is guaranteed > 1.e-6 by the guard above, but a nutrient
      !! mass that has shrunk to a denormal over many simulated years would
      !! still underflow on division under -ffpe-trap=underflow; such a
      !! value is physically zero (same class as organic_mineral_mass_
      !! module's fmul guard, but for a divide instead of a multiply).
      conc_n = 0.
      if (abs(wbody%orgn) >= 1.e-30) conc_n = 1000. * wbody%orgn / wbody%flo
      conc_p = 0.
      if (abs(wbody%sedp) >= 1.e-30) conc_p = 1000. * wbody%sedp / wbody%flo
      conc_soln = 0.
      if (abs(wbody%no3 + wbody%nh3 + wbody%no2) >= 1.e-30) &
        conc_soln = 1000. * (wbody%no3 + wbody%nh3 + wbody%no2) / wbody%flo
      conc_solp = 0.
      if (abs(wbody%solp) >= 1.e-30) conc_solp = 1000. * wbody%solp / wbody%flo
      
      !! new inputs thetn, thetap, conc_pmin, conc_nmin
      !! Ikenberry wetland eqs modified - not function of area - fraction of difference in concentrations
      iwst = ob(iob)%wst
      nitrok = (conc_n - wbody_prm%nut%conc_nmin) * Theta(nsetlr, wbody_prm%nut%theta_n, wst(iwst)%weat%tave)
      nitrok = amin1 (nitrok, 1.)
      nitrok = max (nitrok, 0.)
      phosk = (conc_p - wbody_prm%nut%conc_pmin) * Theta(psetlr, wbody_prm%nut%theta_p, wst(iwst)%weat%tave)
      phosk = amin1 (phosk, 1.)
      phosk = max (phosk, 0.)
      nitrosolk = (conc_soln - wbody_prm%nut%conc_nmin) * Theta(nsolr, wbody_prm%nut%theta_n, wst(iwst)%weat%tave)
      nitrosolk = amin1 (nitrosolk, 1.)
      nitrosolk = max (nitrosolk, 0.)
      phossolk = (conc_solp - wbody_prm%nut%conc_pmin) * Theta(psolr, wbody_prm%nut%theta_p, wst(iwst)%weat%tave)
      phossolk = amin1 (phossolk, 1.)
      phossolk = max (phossolk, 0.)

      !! remove nutrients from reservoir by settling - exclude soluble nutrients
      !! other part of equation 29.1.3 in SWAT manual
      !! guard against denormal underflow: a nutrient mass that has shrunk
      !! to a denormal over many simulated years would underflow on this
      !! multiply under -ffpe-trap=underflow; such a value is physically
      !! zero (same pattern as organic_mineral_mass_module's fmul).
      if (abs(wbody%solp) < 1.e-30) then
        wbody%solp = 0.
      else
        wbody%solp = wbody%solp * (1. - phossolk * wbody_prm%solp_stl_fr)
      end if
      if (abs(wbody%sedp) < 1.e-30) then
        wbody%sedp = 0.
      else
        wbody%sedp = wbody%sedp * (1. - phosk)
      end if
      if (abs(wbody%orgn) < 1.e-30) then
        wbody%orgn = 0.
      else
        wbody%orgn = wbody%orgn * (1. - nitrok)
      end if
      if (abs(wbody%no3) < 1.e-30) then
        wbody%no3 = 0.
      else
        wbody%no3 = wbody%no3 * (1. - nitrosolk * wbody_prm%soln_stl_fr)
      end if
      if (abs(wbody%nh3) < 1.e-30) then
        wbody%nh3 = 0.
      else
        wbody%nh3 = wbody%nh3 * (1. - nitrosolk * wbody_prm%soln_stl_fr)
      end if
      if (abs(wbody%no2) < 1.e-30) then
        wbody%no2 = 0.
      else
        wbody%no2 = wbody%no2 * (1. - nitrosolk * wbody_prm%soln_stl_fr)
      end if

      !! calculate chlorophyll-a and water clarity
      chlaco = 0.
      wbody%chla = 0.
      !! wbody%flo alone is guaranteed > 1.e-6 by the guard above, but
      !! ht2%flo can be ~0 or slightly negative, so the sum can still
      !! collapse to a denormal/zero and blow up the divisions below.
      flo_tot = wbody%flo + ht2%flo
      if (flo_tot > 1.e-6 .and. abs(wbody%solp + wbody%sedp) >= 1.e-30) then
        tpco = 1.e+6 * (wbody%solp + wbody%sedp) / flo_tot
        if (tpco > 1.e-4) then
          !! equation 29.1.6 in SWAT manual
          !chlaco = wbody_prm%nut%chlar * 0.551 * (tpco**0.76)
          wbody%chla = flo_tot * 1.e-6
        endif
      endif

      !! check nutrient masses greater than zero
      wbody%no3 = max (wbody%no3, 0.0)
      wbody%orgn = max (wbody%orgn, 0.0)
      wbody%sedp = max (wbody%sedp, 0.0)
      wbody%solp = max (wbody%solp, 0.0)
      wbody%chla = max (wbody%chla, 0.0)
      wbody%nh3 = max (wbody%nh3, 0.0)
      wbody%no2 = max (wbody%no2, 0.0)

      !! calculate amount of nutrients leaving reservoir
      !! same denormal guard as above: either wbody%X or ht2%flo itself
      !! (today's outflow, which can be legitimately tiny) can make the
      !! multiply underflow even when the other operand is normal-sized.
      if (flo_tot > 1.e-6 .and. abs(ht2%flo) >= 1.e-30) then
        ht2%no3 = 0.
        if (abs(wbody%no3) >= 1.e-30) ht2%no3 = wbody%no3 * ht2%flo / flo_tot
        ht2%orgn = 0.
        if (abs(wbody%orgn) >= 1.e-30) ht2%orgn = wbody%orgn * ht2%flo / flo_tot
        ht2%sedp = 0.
        if (abs(wbody%sedp) >= 1.e-30) ht2%sedp = wbody%sedp * ht2%flo / flo_tot
        ht2%solp = 0.
        if (abs(wbody%solp) >= 1.e-30) ht2%solp = wbody%solp * ht2%flo / flo_tot
        ht2%chla = 0.
        if (abs(wbody%chla) >= 1.e-30) ht2%chla = wbody%chla * ht2%flo / flo_tot
        ht2%nh3 = 0.
        if (abs(wbody%nh3) >= 1.e-30) ht2%nh3 = wbody%nh3 * ht2%flo / flo_tot
        ht2%no2 = 0.
        if (abs(wbody%no2) >= 1.e-30) ht2%no2 = wbody%no2 * ht2%flo / flo_tot
      else
        ht2%no3 = 0.
        ht2%orgn = 0.
        ht2%sedp = 0.
        ht2%solp = 0.
        ht2%chla = 0.
        ht2%nh3 = 0.
        ht2%no2 = 0.
      endif
      
      !! remove nutrients leaving reservoir
      wbody%no3 = max(0.,wbody%no3 - ht2%no3) !No less than zero, Jaehak 2024
      wbody%orgn = max(0.,wbody%orgn - ht2%orgn)
      wbody%sedp = max(0.,wbody%sedp - ht2%sedp)
      wbody%solp = max(0.,wbody%solp - ht2%solp)
      wbody%chla = max(0.,wbody%chla - ht2%chla)
      wbody%nh3 = max(0.,wbody%nh3 - ht2%nh3)
      wbody%no2 = max(0.,wbody%no2 - ht2%no2)

      return
      end subroutine res_nutrient