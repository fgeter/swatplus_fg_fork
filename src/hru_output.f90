      subroutine hru_output (ihru)

      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use hydrograph_module, only : sp_ob1, ob
      use organic_mineral_mass_module
      use soil_module
      use carbon_module
      use hru_module, only : hru
      use landuse_data_module
      
      implicit none
      
      external :: soil_nutcarb_write
      
      integer, intent (in) :: ihru             !            |
      integer :: idp                           !            |
      integer :: j
      integer :: iob
      integer :: ipl
	  integer :: ilu
      real :: bm_max_m
      real :: bm_max_y
      real :: const
      real :: sw_init
      real :: sno_init
      real :: percn_aa
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps

      j = ihru
      
      iob = sp_ob1%hru + j - 1   !!!!!! added for new output write
      ilu = hru(j)%land_use_mgt
          
        !! The daily accumulation and the daily two-point averages that used to sit
        !! here now live in hru_output_accum, which is called just before this routine
        !! and does no file I/O. What remains below is the WRITE half plus the month /
        !! year / simulation rollups.
             
      !! daily print
         if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%wb_hru%d == "y") then
             write (2000,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_d(j),      &
                                                                           lum(ilu)%plant_cov, lum(ilu)%mgt_ops     !! water bal day
             if (pco%csvout == "y") then
               !! changed write unit below (2004 to write file data)
               write (2004,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                                                                    hwb_d(j), lum(ilu)%plant_cov, lum(ilu)%mgt_ops  
             end if
          end if

          if (pco%nb_hru%d == "y") then
            write (2020,104) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_d(j),         &
                                                                          lum(ilu)%plant_cov, lum(ilu)%mgt_ops      !! nutrient bal day
            if (pco%csvout == "y") then
                write (2024,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                                                                    hnb_d(j), lum(ilu)%plant_cov, lum(ilu)%mgt_ops  
                end if
          end if
          if (pco%ls_hru%d == "y") then
            write (2030,108) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_d(j),         &
                                                                          lum(ilu)%plant_cov, lum(ilu)%mgt_ops, hpw_d(j)%percn       !! losses day
            if (pco%csvout == "y") then
                write (2034,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                                                                    hls_d(j), lum(ilu)%plant_cov, lum(ilu)%mgt_ops, hpw_d(j)%percn   
            end if
          end if
          if (pco%pw_hru%d == "y") then
            hpw_d(j)%bm_max = hpw_d(j)%bioms
            write (2040,101) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpw_d(j),                  & 
                                                                          lum(ilu)%plant_cov, lum(ilu)%mgt_ops  !! plant weather day 
              if (pco%csvout == "y") then 
                write (2044,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name,           &
                                                                hpw_d(j), lum(ilu)%plant_cov, lum(ilu)%mgt_ops  
              end if
          end if
        end if

        !! Advance the daily soil-water / snow carry-forward EVERY day.
        !! These are the only per-day writers of sw_init/sno_init anywhere
        !! (basin_sw_init runs once, gated by pco%sw_init), and hwb_d(j)%sw and
        !! %snopack above are the two-point averages (init + final) / 2 built from them.
        !!
        !! They used to sit inside the "daily print" guard, i.e. they advanced only on
        !! days that were both inside the pco%day_print window AND on an int_day
        !! interval boundary. That had two distinct consequences:
        !!
        !!  1. DEFECT. While outside the print window (yrc_start later than the sim
        !!     start), sw_init never advanced at all, so the FIRST printed day averaged
        !!     today's soil water against the value from simulation start. Measured on
        !!     racoon_creek_120hru with yrc_start one year in: HRU 36 reported
        !!     sw_ave = 155.9 when soil water was 238.9 - a 35% error. Self-corrected
        !!     from the second printed day, so it cost exactly one row per HRU.
        !!
        !!  2. CONVENTION. With int_day > 1, sw_init was the start of the reporting
        !!     interval, so sw_ave was an interval endpoint average - consistent with
        !!     how the monthly accumulator carries sw_init (see the end-of-month block
        !!     below), but inconsistent with this file being a DAILY file.
        !!
        !! Deliberate decision: advance unconditionally. sw_ave / snopack in
        !! hru_wb_day now always mean a true daily average, (yesterday + today) / 2,
        !! whatever int_day is set to. This intentionally CHANGES hru_wb_day output for
        !! runs with interval > 1 - a column named sw_ave in a file named hru_wb_day
        !! should not silently mean "n-day endpoint average".
        !!
        !! Scope: daily output only. hru_control.f90 sets hwb_d(j)%sw = soil(j)%sw and
        !! the monthly accumulation at the top of this routine banks THAT value before
        !! the line above overwrites %sw for the daily record - so monthly, yearly and
        !! average-annual output are untouched (verified bit-identical).
        !!
        !! Placed AFTER the print block so the printed record still carries the same
        !! sw_init it always did: bit-identical for interval = 1 inside the window.
        hwb_d(j)%sw_init = hwb_d(j)%sw_final
        hwb_d(j)%sno_init = hwb_d(j)%sno_final

        !! check end of month
        if (time%end_mo == 1) then
          bm_max_m = hpw_m(j)%bm_max
          hwb_y(j) = hwb_y(j) + hwb_m(j)
          hnb_y(j) = hnb_y(j) + hnb_m(j)
          hls_y(j) = hls_y(j) + hls_m(j)
          bm_max_y = hpw_y(j)%bm_max      ! save off yearly bm_max
          hpw_y(j) = hpw_y(j) + hpw_m(j)
          hpw_y(j)%bm_max = bm_max_y      ! restore yearly bm_max
          
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          hpw_m(j) = hpw_m(j) // const
          hwb_m(j) = hwb_m(j) // const
          
          hpw_m(j)%bm_max = bm_max_m     ! restore monthly bm_max value
          
          !! monthly print
           hwb_m(j)%sw_final = hwb_d(j)%sw_final
           hwb_m(j)%sno_final = hwb_d(j)%sno_final
           
           if (pco%wb_hru%m == "y") then
             write (2001,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_m(j),        &
                                                                          lum(ilu)%plant_cov, lum(ilu)%mgt_ops           !! water bal mon
               if (pco%csvout == "y") then
                 write (2005,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                                                                          hwb_m(j), lum(ilu)%plant_cov, lum(ilu)%mgt_ops 
               end if
           end if
           
           if (pco%nb_hru%m == "y") then
             write (2021,104) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_m(j),        &
                                                                          lum(ilu)%plant_cov, lum(ilu)%mgt_ops           !! nutrient bal mon
             if (pco%csvout == "y") then
                 write (2025,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                                                                          hnb_m(j), lum(ilu)%plant_cov, lum(ilu)%mgt_ops 
                 end if
           end if
           
           if (pco%ls_hru%m == "y") then
             write (2031,108) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_m(j),        &
                                                                          lum(ilu)%plant_cov, lum(ilu)%mgt_ops, hpw_m(j)%percn            !! losses mon
             if (pco%csvout == "y") then 
                 write (2035,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                                                                          hls_m(j), lum(ilu)%plant_cov, lum(ilu)%mgt_ops, hpw_m(j)%percn  
             end if
           end if
           
           if (pco%pw_hru%m == "y") then
             hpw_m(j)%nplnt = pl_mass(j)%tot_com%n
             hpw_m(j)%pplnt = pl_mass(j)%tot_com%p
             write (2041,101) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpw_m(j),         &
                                                                          lum(ilu)%plant_cov, lum(ilu)%mgt_ops  !! plant weather mon
               if (pco%csvout == "y") then 
                 write (2045,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name,  &
                                                                hpw_m(j), lum(ilu)%plant_cov, lum(ilu)%mgt_ops  
               end if 
           end if
           hpw_m(j)%bm_max = 0.0
          
          sw_init = hwb_m(j)%sw_final
          sno_init = hwb_m(j)%sno_final
          hwb_m(j) = hwbz
          hwb_m(j)%sw_init = sw_init
          hwb_m(j)%sno_init = sno_init
          hnb_m(j) = hnbz
          hpw_m(j) = hpwz
          hls_m(j) = hlsz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          bm_max_y = hpw_y(j)%bm_max
          hwb_a(j) = hwb_a(j) + hwb_y(j)
          hnb_a(j) = hnb_a(j) + hnb_y(j)
          hls_a(j) = hls_a(j) + hls_y(j)
          hpw_a(j) = hpw_a(j) + hpw_y(j)         
          
          const = time%day_end_yr
          hwb_y(j) = hwb_y(j) // const
          hpw_y(j) = hpw_y(j) // const
          
          hpw_y(j)%bm_max = bm_max_y   ! Restore bm_max_y
          hpw_a(j)%bm_max = bm_max_a_sv(j)   ! Restore bm_max_a (saved in hru_output_accum)

          !! yearly print
          hwb_y(j)%sw_final = hwb_d(j)%sw_final
          hwb_y(j)%sno_final = hwb_d(j)%sno_final
           !! if > 10mm irrigation, flag as irrigated for soft cal
           if (hwb_a(j)%irr > 10.) then
             hru(j)%irr = 1
           end if
          if (pco%wb_hru%y == "y") then
             write (2002,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_y(j),          &
                                                                          lum(ilu)%plant_cov, lum(ilu)%mgt_ops           !! water balance yr
               if (pco%csvout == "y") then
                 write (2006,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name,   &
                                                                          hwb_y(j), lum(ilu)%plant_cov, lum(ilu)%mgt_ops 
               end if
          end if
          
           if (pco%nb_hru%y == "y") then
             write (2022,104) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_y(j),          &
                                                                                lum(ilu)%plant_cov, lum(ilu)%mgt_ops     !! nutrient balance yr
             if (pco%csvout == "y") then
                 write (2026,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name,   &
                                                                          hnb_y(j), lum(ilu)%plant_cov, lum(ilu)%mgt_ops 
                 end if
           end if
           
           if (pco%ls_hru%y == "y") then
             write (2032,108) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_y(j),          &
                                                                           lum(ilu)%plant_cov, lum(ilu)%mgt_ops, hpw_y(j)%percn            !! losses yr
             if (pco%csvout == "y") then
                 write (2036,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name,   &
                                                                          hls_y(j), lum(ilu)%plant_cov, lum(ilu)%mgt_ops, hpw_y(j)%percn  
             end if
           end if
           
           if (pco%pw_hru%y == "y") then
             hpw_y(j)%nplnt = pl_mass(j)%tot_com%n
             hpw_y(j)%pplnt = pl_mass(j)%tot_com%p
             write (2042,101) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpw_y(j),        &
                                                                          lum(ilu)%plant_cov, lum(ilu)%mgt_ops  !! plant weather yr             
               if (pco%csvout == "y") then 
                 write (2046,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                                                                hpw_y(j), lum(ilu)%plant_cov, lum(ilu)%mgt_ops  
               end if 
           end if
          hpw_y(j)%bm_max = 0.0
           
          !reset yearly parameters in time_control - for calibration runs
        end if
        
!!!!! average annual print
         if (time%end_sim == 1) then
           sw_init = hwb_a(j)%sw_init
           sno_init = hwb_a(j)%sno_init
           hwb_a(j) = hwb_a(j) / time%yrs_prt
           hwb_a(j) = hwb_a(j) // time%days_prt
           hwb_a(j)%sw_init = sw_init
           hwb_a(j)%sw_final = hwb_d(j)%sw_final
           hwb_a(j)%sno_init = sno_init
           hwb_a(j)%sno_final = hwb_d(j)%sno_final
           if (pco%wb_hru%a == "y") then
             write (2003,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hwb_a(j),       &
                                                                          lum(ilu)%plant_cov, lum(ilu)%mgt_ops       !! water balance ann
             if (pco%csvout == "y") then
               write (2007,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name,  &
                                                                        hwb_a(j), lum(ilu)%plant_cov, lum(ilu)%mgt_ops
             end if
           end if
           sw_init = hwb_d(j)%sw_final
           sno_init = hwb_d(j)%sno_final
           hru(j)%precip_aa = hwb_a(j)%precip
           hru(j)%flow(1) = hwb_a(j)%wateryld
           hru(j)%flow(2) = hwb_a(j)%perc
           hru(j)%flow(3) = hwb_a(j)%surq_gen
           hru(j)%flow(4) = hwb_a(j)%latq
           hru(j)%flow(5) = hwb_a(j)%qtile
           hwb_a(j) = hwbz
           hwb_a(j)%sw_init = sw_init
           hwb_a(j)%sno_init = sno_init
         end if
        
         if (time%end_sim == 1 .and. pco%nb_hru%a == "y") then 
           hnb_a(j) = hnb_a(j) / time%yrs_prt
           write (2023,104) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hnb_a(j),        &
                                                                          lum(ilu)%plant_cov, lum(ilu)%mgt_ops      !! nutrient bal ann
           if (pco%csvout == "y") then 
               write (2027,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                                                                        hnb_a(j), lum(ilu)%plant_cov, lum(ilu)%mgt_ops
               end if
         end if
        
         if (time%end_sim == 1 .and. pco%ls_hru%a == "y") then
           hls_a(j) = hls_a(j) / time%yrs_prt 
           percn_aa = hpw_a(j)%percn / time%yrs_prt
           write (2033,107) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hls_a(j),        &
                                                                          lum(ilu)%plant_cov, lum(ilu)%mgt_ops, percn_aa       !! losses ann
             if (pco%csvout == "y") then 
               write (2037,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                                                                        hls_a(j), lum(ilu)%plant_cov, lum(ilu)%mgt_ops, percn_aa 
             end if
         end if
        
         !! hru(j)%strsa is MODEL state, not output: calsoft_plant branches on it
         !! (hru(iihru)%strsa > 50.) during soft calibration, and this is its only
         !! writer anywhere in the code. It used to be assigned inside the average-annual
         !! plant/weather print block below, which made soft calibration silently depend
         !! on the hru_pw avann print flag being "y". Set it unconditionally here.
         !! hpw_a(j) is still undivided at this point, so divide explicitly - the "//"
         !! (hruout_plantweather_ave) leaves strsa untouched, so the value below is
         !! identical to what the print block used to assign after its "/" and "//".
         if (time%end_sim == 1) then
           hru(j)%strsa = hpw_a(j)%strsa / time%yrs_prt
         end if

         if (time%end_sim == 1 .and. pco%pw_hru%a == "y") then
           hpw_a(j) = hpw_a(j) / time%yrs_prt
           hpw_a(j) = hpw_a(j) // time%days_prt
           hpw_a(j)%nplnt = pl_mass(j)%tot_com%n
           hpw_a(j)%pplnt = pl_mass(j)%tot_com%p
           hpw_a(j)%bm_max = bm_max_a_sv(j)   ! Restore bm_max_a (saved in hru_output_accum)
           write (2043,102) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpw_a(j),           &
                                                                        lum(ilu)%plant_cov, lum(ilu)%mgt_ops  !! plant weather ann
             if (pco%csvout == "y") then 
               write (2047,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name,    &
                                                              hpw_a(j), lum(ilu)%plant_cov, lum(ilu)%mgt_ops  
             end if
         end if

         !! Reset the AVERAGE-ANNUAL accumulators unconditionally.
         !! time_control zeroes the YEARLY accumulators (hwb_y/hnb_y/hpw_y/hls_y)
         !! between soft-calibration runs, but not these, and time_control is
         !! re-entered once per calibration iteration (calsoft_hyd, calsoft_plant,
         !! calsoft_sed, caltsoft_hyd, calhard_control - 44 call sites). An avann
         !! accumulator left unreset therefore compounds across iterations.
         !! hwb_a already reset outside its print guard; hnb_a/hls_a/hpw_a reset
         !! only when their own avann print flag was "y". That matters most for
         !! hpw_a, which the strsa assignment above reads on EVERY run regardless
         !! of the print flag - without this, hru(j)%strsa would grow every
         !! calibration iteration whenever hru_pw avann output is off.
         if (time%end_sim == 1) then
           hnb_a(j) = hnbz
           hls_a(j) = hlsz
           hpw_a(j) = hpwz
         end if

          if (time%end_sim == 1) then
            if (pco%cb_hru%d /= "n" .or. pco%cb_hru%m /= "n" .or. pco%cb_hru%y /= "n" .or. pco%cb_hru%a /= "n") then
              call soil_nutcarb_write(" e")    
            endif
          endif

         !! write average annual crop yields
         if (time%end_sim == 1) then
           if (pco%crop_yld == "a" .or. pco%crop_yld == "b") then
             do ipl = 1, pcom(j)%npl
               idp = pcom(j)%plcur(ipl)%idplt
               if (pcom(j)%plcur(ipl)%harv_num > 0) then 
                 pl_mass(j)%yield_tot(ipl) = pl_mass(j)%yield_tot(ipl) / float(pcom(j)%plcur(ipl)%harv_num)
              endif
              write (4008,103) time%day, time%mo, time%day_mo, time%yrc, j,pldb(idp)%plantnm, pl_mass(j)%yield_tot(ipl)
              if (pco%csvout == "y") then
                write (4009,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, j,pldb(idp)%plantnm, pl_mass(j)%yield_tot(ipl) 
              end if
            end do
           end if
         end if
      return
      
100   format (4i6,2i8,2x,a,42f12.3,3x,a16,a30)
101   format (4i6,2i8,2x,a,25f12.3,3x,a16,a30)
102   format (4i6,2i8,2x,a,25f12.3,3x,a16,a30)
103   format (4i6,i8,4x,a,5x,4f12.3)
104   format (4i6,2i8,2x,a8,4f12.3,15f17.3,7x,a16,a30)
107   format (4i6,2i8,2x,a,12f12.3,3x,a16,a30,f12.3)
108   format (4i6,2i8,2x,a,12(1x,f16.3),3x,a16,a30,1x,f16.3)
       
      end subroutine hru_output