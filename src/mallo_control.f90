      subroutine mallo_control (imallo)
      
      use manure_allocation_module
      use hru_module
      use basin_module
      use time_module
      use plant_module
      use soil_module
      use organic_mineral_mass_module
      use conditional_module
      use mgt_operations_module, only : chemapp_db
      
      implicit none 
      
      external :: actions, conditions, pl_manure

      integer, intent (in) :: imallo     !water allocation object number
      integer :: itrn = 0                   !water demand object number
      integer :: isrc = 0                   !source object number
      integer :: j = 0                      !hru number
      integer :: id = 0                     !decision table number
      integer :: ifrt = 0                   !number in fertilizer.frt
      integer :: ifertop = 0                !number in chem_app file
      integer :: imanure = 0                !number in manure_om.frt (0 = source is not manure)
      real :: surf_fr = 0.                  !frac   |fraction applied to the surface layer
      real :: frt_kg = 0.                   !m3     |demand

      isrc = 0
      frt_kg = 0.

      !! zero demand, withdrawal, and unmet for entire allocation object
      mallo(imallo)%tot = malloz
      
      !!add manure produced on the first day of the month
      if (time%day_mo == 1) then
        mallo(imallo)%src(isrc)%bal_d%stor = mallo(imallo)%src(isrc)%bal_d%stor +   &
                                      mallo(imallo)%src(isrc)%prod_mon(time%day_mo)
        mallo(imallo)%src(isrc)%bal_d%prod = mallo(imallo)%src(isrc)%prod_mon(time%day_mo)
      end if
      
      !!loop through each demand object for manure demand
      do itrn = 1, mallo(imallo)%trn_obs
        !! check decision table for manure application
        if (mallo(imallo)%trn(itrn)%dtbl /= "null" .and. mallo(imallo)%trn(itrn)%dtbl_num /= 0) then
          j = mallo(imallo)%trn(itrn)%ob_num
          id = mallo(imallo)%trn(itrn)%dtbl_num
          d_tbl => dtbl_lum(id)
          call conditions (j, id)
          call actions (j, itrn, id)
        end if
      end do
 
      !!loop through each demand object again and subtract from source if available
      do itrn = 1, mallo(imallo)%trn_obs
        if (mallo(imallo)%trn(itrn)%manure_amt%app_t_ha > 0. .and.      &
                    frt_kg > mallo(imallo)%src(isrc)%bal_d%stor) then
          isrc = mallo(imallo)%trn(itrn)%manure_amt%src_obj         !source object
          ifrt = mallo(imallo)%src(isrc)%fertdb                     !fertilizer type from fert data base
          frt_kg = mallo(imallo)%trn(itrn)%manure_amt%app_t_ha      !amount applied in kg/ha
          ifertop = mallo(imallo)%trn(itrn)%manure_amt%app_method   !surface application fraction from chem app data base
          ihru = mallo(imallo)%trn(itrn)%ob_num                        !hru number
          imanure = mallo(imallo)%src(isrc)%iorg_min   !manure_om.frt number, 0 if unresolved

          !! manure goes through pl_manure, never pl_fert: it is partitioned from
          !! its own manure_om.frt composition, the same as a "manu" operation or
          !! a graze.  manure_allocation_read has already error stopped on any
          !! source that does not resolve, so imanure is always valid here.
          !!
          !! app_method is not always set for an allocation transfer, and
          !! chemapp_db(0)%surf_frac defaults to 0., which would bury the whole
          !! application in layer 2.  default to the surface, as pl_graze does.
          if (ifertop > 0) then
            surf_fr = chemapp_db(ifertop)%surf_frac
          else
            surf_fr = 1.
          end if
          call pl_manure (imanure, frt_kg, surf_fr)
          mallo(imallo)%trn(itrn)%manure_amt = manure_amtz
          
          !! subtract manure from source
          mallo(imallo)%src(isrc)%bal_d%stor = mallo(imallo)%src(isrc)%bal_d%stor - frt_kg
          mallo(imallo)%src(isrc)%bal_d%withdr = frt_kg
          
          !! set daily withdrawal and source
          mallo(imallo)%trn(itrn)%withdr(isrc) = frt_kg

          if (pco%mgtout == "y") then
            !! name the source, which is correct on both the pl_manure and the
            !! pl_fert path (fertdb(ifrt) is meaningless when ifrt never resolved)
            write (2612, *) j, time%yrc, time%mo, time%day_mo,                        &
                  mallo(imallo)%src(isrc)%manure_typ, "    MANU",       &
                  phubase(j),pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pl_mass(j)%tot(ipl)%m,            &
                  pl_mass(j)%abg_rsd_tot%m, sol_sumno3(j), sol_sumsolp(j), frt_kg, fertno3, fertnh3,        &
                  fertorgn, fertsolp, fertorgp
          endif
        end if

      end do
        
      return
      end subroutine mallo_control