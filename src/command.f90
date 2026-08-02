      subroutine command
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    for every day of simulation, this subroutine steps through the command
!!    lines in the watershed configuration (.fig) file. Depending on the 
!!    command code on the .fig file line, a command loop is accessed
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: subbasin, route, routres, transfer, recmon
!!    SWAT: recepic, save, recday, recyear

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use time_module
      use hydrograph_module
      use ru_module
      use channel_module
      use hru_lte_module
      use aquifer_module
      use sd_channel_module
      use reservoir_module
      use organic_mineral_mass_module
      use carbon_module
      use constituent_mass_module
      use hru_module, only : ihru, hru, enratio
      use basin_module
      use maximum_data_module
      use gwflow_module
      use soil_module
      use recall_module
      use water_allocation_module
      implicit none
      
      external :: aqu_1d_control, aqu_cs_output, aqu_pesticide_output, aqu_salt_output, aquifer_output, &
                  ch_cs_output, ch_salt_output, cha_pesticide_output, channel_output, constit_hyd_mult, &
                  cs_str_output, flow_dur_curve, gwflow_simulate, hru_carbon_output, hru_control, hru_output_accum, &
                  hru_cs_output, hru_lte_control, hru_lte_output, hru_output, hru_pathogen_output, &
                  hru_pesticide_output, hru_salt_output, hydin_output, hydout_output, manure_demand_output, &
                  manure_source_output, obj_output, recall_nut, recall_output, res_control, res_cs_output, &
                  res_pesticide_output, res_salt_output, reservoir_output, ru_control, ru_cs_output, &
                  ru_output, ru_salt_output, sd_chanbud_output, sd_chanmorph_output, sd_channel_control3, &
                  sd_channel_output, wallo_allo_output, wallo_treat_output, wallo_trn_output, &
                  wallo_use_output, wet_cs_output, wet_salt_output, wetland_output, basin_aqu_pest_output, &
                  basin_aquifer_output, basin_ch_pest_output, basin_chanbud_output, basin_chanmorph_output, &
                  basin_channel_output, basin_ls_pest_output, basin_output, basin_recall_output, &
                  basin_res_pest_output, basin_reservoir_output, basin_sdchannel_output, cs_balance, &
                  lsu_output, salt_balance, hyddep_output, recall_salt, recall_cs, soil_nutcarb_write, &
                  soil_carbvar_write

      real, dimension(time%step) :: hyd_flo     !flow hydrograph
      integer :: in                   !              | 
      integer :: iob                  !              |
      integer :: iday                 !              |
      integer :: isd                  !none          |counter
      integer :: ires                 !none          |reservoir number
      integer :: irec                 !              |
      integer :: iout                 !none          |counter
      integer :: ihtyp                !              |
      integer :: iaq                  !none          |counter
      integer :: j                    !none          |counter
      integer :: ihyd                 !              |
      integer :: idr                  !              |
      integer :: iwro                 !              |
      real :: conv                    !              |
      real :: frac_in                 !              |
      integer :: ts1
      integer :: ts2
      integer :: iw                   !              |counter for water allocation object
      integer :: iwallo               !              |variable to pass to wallo_control
      integer :: i_count              !rtb gwflow
      integer :: i_mfl                !rtb gwflow    |counter
      integer :: i_chan               !rtb gwflow    |counter
      integer :: iob_chan            !rtb gwflow    |ob index for channel
      real :: sumflo
      !! === OpenMP Stage 1: parallel pre-pass over independent (cmd_order==1) headwater HRUs ===
      integer, allocatable, save :: par_hru(:)   !! icmd indices of independent (cmd_order==1) HRUs
      integer, save :: n_par_hru = -1            !! size of par_hru (-1 = not yet built)
      logical :: use_par                         !! run the parallel HRU pre-pass this day?
      integer :: k                               !! pre-pass loop counter

      !! === OpenMP Stage 2: per-wavefront-level pre-pass over chandeg (SD channel) chains ===
      integer, allocatable, save :: par_cha(:)           !! icmd indices of eligible chandeg objs, grouped by cmd_order
      integer, allocatable, save :: par_cha_lvl_start(:) !! par_cha_lvl_start(L):par_cha_lvl_start(L+1)-1 = level L's objs
      integer, save :: max_cha_level = 0                 !! highest cmd_order among eligible chandeg objects
      integer, save :: n_par_cha = -1                    !! -1 = not yet built
      logical :: use_par2                                !! run the chandeg wavefront pre-pass this day?
      integer :: last_level_done                         !! per-day: highest cmd_order level already pre-passed
      integer :: lvl_lo, lvl_hi                          !! par_cha bounds for the level being pre-passed
      integer :: icmd_save                               !! preserves the sequential walk's position across the
                                                         !! parallel pre-pass (icmd is threadprivate, reassigned by workers)
      logical :: icmd_is_precomputed_cha                 !! true if THIS icmd was handled by the chandeg pre-pass

      !! === OpenMP Stage 3: per-wavefront-level pre-pass over reservoirs + aquifers ===
      integer, allocatable, save :: par_res(:), par_res_lvl_start(:)
      integer, save :: max_res_level = 0, n_par_res = -1
      logical :: use_par3
      integer :: lvl_lo_r, lvl_hi_r, icmd_save_r
      logical :: icmd_is_precomputed_res
      integer, allocatable, save :: par_aqu(:), par_aqu_lvl_start(:)
      integer, save :: max_aqu_level = 0, n_par_aqu = -1
      logical :: use_par4
      integer :: lvl_lo_a, lvl_hi_a, icmd_save_a
      logical :: icmd_is_precomputed_aqu

      !! === OpenMP Stage 4: per-wavefront-level pre-pass over routing units (ru) ===
      !! ru_control gathers via ru_def/ru_elem membership (not the standard hydrograph
      !! network), so ru_producers_all_earlier checks BOTH mechanisms. hyddep_output is
      !! deferred to the sequential walk (file I/O) with ht1 captured per-object in ru_ht1_save.
      integer, allocatable, save :: par_ru(:), par_ru_lvl_start(:)
      integer, save :: max_ru_level = 0, n_par_ru = -1
      logical :: use_par5
      integer :: lvl_lo_u, lvl_hi_u, icmd_save_u
      logical :: icmd_is_precomputed_ru
      type (hyd_output), allocatable, save :: ru_ht1_save(:)   !! per-object ht1 for deferred hyddep_output

      icmd = sp_ob1%objs
      wallo(:)%trn_cur = 1
      if (allocated(res_ob)) res_ob(:)%wallo_call = 0

      !! === OpenMP Stage 1: build the eligible independent-HRU list once (lazily), then
      !! compute those HRUs in parallel. Eligible = headwater HRU (cmd_order==1) with no
      !! incoming hydrograph (rcv_tot==0), so the gather part of the loop body is a no-op.
      !! Gated to simple runs (no water allocation / constituents). The sequential loop
      !! below skips their recompute but still runs their routing/output (kept sequential).
      use_par = (db_mx%wallo_db == 0 .and. cs_db%num_pests == 0 .and.  &
                 cs_db%num_salts == 0 .and. cs_db%num_cs == 0)
      if (n_par_hru < 0) then
        n_par_hru = 0
        iob = sp_ob1%objs
        do while (iob /= 0)
          if (ob(iob)%cmd_order == 1 .and. ob(iob)%typ == "hru" .and. ob(iob)%rcv_tot == 0)  &
            n_par_hru = n_par_hru + 1
          iob = ob(iob)%cmd_next
        end do
        allocate (par_hru(max(1, n_par_hru)))
        n_par_hru = 0
        iob = sp_ob1%objs
        do while (iob /= 0)
          if (ob(iob)%cmd_order == 1 .and. ob(iob)%typ == "hru" .and. ob(iob)%rcv_tot == 0) then
            n_par_hru = n_par_hru + 1
            par_hru(n_par_hru) = iob
          end if
          iob = ob(iob)%cmd_next
        end do
      end if

      !! === OpenMP Stage 2: build per-wavefront-level eligible chandeg (SD channel) lists ===
      !! Eligible = a real channel (chl > 1e-3; zero-length artificial channels take a trivial
      !! non-parallel path) with NO floodplain HRUs (those share per-HRU wetland state across
      !! channels) whose EVERY producer has a strictly lower cmd_order. Any object failing the
      !! per-producer check is left out and falls back to the always-correct sequential walk.
      use_par2 = (use_par .and. cs_db%num_tot == 0)
      if (n_par_cha < 0) then
        n_par_cha = 0
        max_cha_level = 0
        iob = sp_ob1%objs
        do while (iob /= 0)
          if (ob(iob)%typ == "chandeg") then
            if (sd_ch(ob(iob)%num)%chl > 1.e-3) then
              if (cha_producers_all_earlier(iob)) then
                n_par_cha = n_par_cha + 1
                max_cha_level = max(max_cha_level, ob(iob)%cmd_order)
              end if
            end if
          end if
          iob = ob(iob)%cmd_next
        end do
        allocate (par_cha(max(1, n_par_cha)))
        allocate (par_cha_lvl_start(max(1, max_cha_level) + 1))
        block
          integer, allocatable :: cha_lvl_cnt(:)
          allocate (cha_lvl_cnt(max(1, max_cha_level)))
          cha_lvl_cnt = 0
          n_par_cha = 0
          iob = sp_ob1%objs
          do while (iob /= 0)
            if (ob(iob)%typ == "chandeg") then
              if (sd_ch(ob(iob)%num)%chl > 1.e-3) then
                if (cha_producers_all_earlier(iob)) then
                  n_par_cha = n_par_cha + 1
                  par_cha(n_par_cha) = iob
                  cha_lvl_cnt(ob(iob)%cmd_order) = cha_lvl_cnt(ob(iob)%cmd_order) + 1
                end if
              end if
            end if
            iob = ob(iob)%cmd_next
          end do
          par_cha_lvl_start(1) = 1
          do k = 1, max_cha_level
            par_cha_lvl_start(k+1) = par_cha_lvl_start(k) + cha_lvl_cnt(k)
          end do
        end block
      end if

      !! === OpenMP Stage 3: build per-wavefront-level eligible reservoir lists ===
      !! Eligible = a reservoir with rcv_tot>0 (matching res_control's sequential gate) whose
      !! every producer has a strictly lower cmd_order.
      use_par3 = (use_par .and. cs_db%num_tot == 0)
      if (n_par_res < 0) then
        n_par_res = 0
        max_res_level = 0
        iob = sp_ob1%objs
        do while (iob /= 0)
          if (ob(iob)%typ == "res" .and. ob(iob)%rcv_tot > 0) then
            if (res_producers_all_earlier(iob)) then
              n_par_res = n_par_res + 1
              max_res_level = max(max_res_level, ob(iob)%cmd_order)
            end if
          end if
          iob = ob(iob)%cmd_next
        end do
        allocate (par_res(max(1, n_par_res)))
        allocate (par_res_lvl_start(max(1, max_res_level) + 1))
        block
          integer, allocatable :: res_lvl_cnt(:)
          allocate (res_lvl_cnt(max(1, max_res_level)))
          res_lvl_cnt = 0
          n_par_res = 0
          iob = sp_ob1%objs
          do while (iob /= 0)
            if (ob(iob)%typ == "res" .and. ob(iob)%rcv_tot > 0) then
              if (res_producers_all_earlier(iob)) then
                n_par_res = n_par_res + 1
                par_res(n_par_res) = iob
                res_lvl_cnt(ob(iob)%cmd_order) = res_lvl_cnt(ob(iob)%cmd_order) + 1
              end if
            end if
            iob = ob(iob)%cmd_next
          end do
          par_res_lvl_start(1) = 1
          do k = 1, max_res_level
            par_res_lvl_start(k+1) = par_res_lvl_start(k) + res_lvl_cnt(k)
          end do
        end block
      end if

      !! === OpenMP Stage 3b: build per-wavefront-level eligible aquifer lists ===
      !! Eligible = an aquifer with dfn_tot==0 (matching aqu_1d_control's sequential gate)
      !! whose every producer has a strictly lower cmd_order.
      use_par4 = (use_par .and. cs_db%num_tot == 0)
      if (n_par_aqu < 0) then
        n_par_aqu = 0
        max_aqu_level = 0
        iob = sp_ob1%objs
        do while (iob /= 0)
          if (ob(iob)%typ == "aqu" .and. ob(iob)%dfn_tot == 0) then
            if (aqu_producers_all_earlier(iob)) then
              n_par_aqu = n_par_aqu + 1
              max_aqu_level = max(max_aqu_level, ob(iob)%cmd_order)
            end if
          end if
          iob = ob(iob)%cmd_next
        end do
        allocate (par_aqu(max(1, n_par_aqu)))
        allocate (par_aqu_lvl_start(max(1, max_aqu_level) + 1))
        block
          integer, allocatable :: aqu_lvl_cnt(:)
          allocate (aqu_lvl_cnt(max(1, max_aqu_level)))
          aqu_lvl_cnt = 0
          n_par_aqu = 0
          iob = sp_ob1%objs
          do while (iob /= 0)
            if (ob(iob)%typ == "aqu" .and. ob(iob)%dfn_tot == 0) then
              if (aqu_producers_all_earlier(iob)) then
                n_par_aqu = n_par_aqu + 1
                par_aqu(n_par_aqu) = iob
                aqu_lvl_cnt(ob(iob)%cmd_order) = aqu_lvl_cnt(ob(iob)%cmd_order) + 1
              end if
            end if
            iob = ob(iob)%cmd_next
          end do
          par_aqu_lvl_start(1) = 1
          do k = 1, max_aqu_level
            par_aqu_lvl_start(k+1) = par_aqu_lvl_start(k) + aqu_lvl_cnt(k)
          end do
        end block
      end if

      !! === OpenMP Stage 4: build per-wavefront-level eligible routing-unit lists ===
      !! Eligible = a ru whose ru_def/ru_elem members AND (if rcv_tot>0) obj_in producers are
      !! all guaranteed computed -- ru_producers_all_earlier checks both. Restricted to daily
      !! timestep (time%step<=1); the subdaily ru path is left sequential (unaudited).
      use_par5 = (use_par .and. cs_db%num_tot == 0 .and. time%step <= 1)
      if (n_par_ru < 0) then
        n_par_ru = 0
        max_ru_level = 0
        allocate (ru_ht1_save(size(ob)))
        iob = sp_ob1%objs
        do while (iob /= 0)
          if (ob(iob)%typ == "ru") then
            if (ru_producers_all_earlier(iob)) then
              n_par_ru = n_par_ru + 1
              max_ru_level = max(max_ru_level, ob(iob)%cmd_order)
            end if
          end if
          iob = ob(iob)%cmd_next
        end do
        allocate (par_ru(max(1, n_par_ru)))
        allocate (par_ru_lvl_start(max(1, max_ru_level) + 1))
        block
          integer, allocatable :: ru_lvl_cnt(:)
          allocate (ru_lvl_cnt(max(1, max_ru_level)))
          ru_lvl_cnt = 0
          n_par_ru = 0
          iob = sp_ob1%objs
          do while (iob /= 0)
            if (ob(iob)%typ == "ru") then
              if (ru_producers_all_earlier(iob)) then
                n_par_ru = n_par_ru + 1
                par_ru(n_par_ru) = iob
                ru_lvl_cnt(ob(iob)%cmd_order) = ru_lvl_cnt(ob(iob)%cmd_order) + 1
              end if
            end if
            iob = ob(iob)%cmd_next
          end do
          par_ru_lvl_start(1) = 1
          do k = 1, max_ru_level
            par_ru_lvl_start(k+1) = par_ru_lvl_start(k) + ru_lvl_cnt(k)
          end do
        end block
      end if

      if (use_par .and. n_par_hru > 0) then
        !$omp parallel do schedule(dynamic)
        do k = 1, n_par_hru
          icmd = par_hru(k)                       !! threadprivate icmd
          !! per-object setup mirroring the loop body for an independent (rcv_tot==0) HRU
          ob(icmd)%day_cur = ob(icmd)%day_cur + 1
          if (ob(icmd)%day_cur > ob(icmd)%day_max) ob(icmd)%day_cur = 1
          ob(icmd)%hin     = hz
          ob(icmd)%hin_sur = hz
          ob(icmd)%hin_lat = hz
          ob(icmd)%hin_til = hz
          ob(icmd)%tsin     = 0.
          ob(icmd)%peakrate = 0.
          ht1 = hz                                !! threadprivate
          ihru = ob(icmd)%num                     !! threadprivate
          !! seed this thread's threadprivate carbon setup-config from the shared master
          !! snapshot (workers don't inherit master's threadprivate values; copyin is
          !! unreliable for derived-type arrays). Config fields persist; scratch fields are
          !! overwritten in cbn_zhang2, so a per-iteration assignment is safe and cheap.
          org_con   = org_con_hold
          org_allo  = org_allo_hold
          carbdb    = carbdb_hold
          org_tran  = org_tran_hold
          org_ratio = org_ratio_hold
          org_frac  = org_frac_hold
          !! also seed threadprivate residue/nut scratch that is read before write in the
          !! residue-decomp / organic-N routines (type-default init isn't applied to workers)
          decomp    = orgz
          transfer  = rsd_originz
          enratio   = 0.
          call hru_control
        end do
        !$omp end parallel do
      end if
      icmd = sp_ob1%objs                          !! reset for the sequential command loop
      last_level_done = 0

      do while (icmd /= 0)

        !! === OpenMP Stage 2: trigger this wavefront level's chandeg pre-pass ===
        !! The command list is sorted by non-decreasing cmd_order (hyd_connect), so the first
        !! time the walk sees a new cmd_order value, every object at every lower level is fully
        !! done (compute AND routing/output) -- upstream hydrographs for this level are current.
        if (ob(icmd)%cmd_order > last_level_done) then
          if (use_par2 .and. ob(icmd)%cmd_order <= max_cha_level) then
            lvl_lo = par_cha_lvl_start(ob(icmd)%cmd_order)
            lvl_hi = par_cha_lvl_start(ob(icmd)%cmd_order + 1) - 1
            if (lvl_hi >= lvl_lo) then
              icmd_save = icmd   !! icmd is threadprivate; every worker reassigns its own copy,
                                 !! so save/restore the sequential walk's position around the region
              !$omp parallel do schedule(dynamic)
              do k = lvl_lo, lvl_hi
                block
                  integer :: in, iob, ihyd, iday, irec
                  real :: frac_in, sumflo
                  real, dimension(time%step) :: hyd_flo
                  type (hyd_sep) :: hdsep1_local
                  !! hyd_rad/trav_time/flo_dep/timeint are threadprivate allocatable: OpenMP does
                  !! not propagate the master's allocation to workers, so each worker allocates its
                  !! own copy the first time it reaches this loop (sized as sd_hydsed_read does).
                  if (.not. allocated(hyd_rad)) then
                    allocate (hyd_rad(max(10, time%step)))
                    allocate (trav_time(max(10, time%step)))
                    allocate (flo_dep(max(10, time%step)))
                    allocate (timeint(max(10, time%step)))
                  end if
                  icmd = par_cha(k)                     !! threadprivate icmd
                  isdch = ob(icmd)%num                  !! threadprivate
                  isd_chsur = ob(icmd)%props2           !! threadprivate; read by sd_channel_control3
                  ob(icmd)%day_cur = 1
                  ob(icmd)%day_cur = ob(icmd)%day_cur + 1
                  if (ob(icmd)%day_cur > ob(icmd)%day_max) ob(icmd)%day_cur = 1
                  ob(icmd)%hin     = hz
                  ob(icmd)%hin_sur = hz
                  ob(icmd)%hin_lat = hz
                  ob(icmd)%hin_til = hz
                  ht1 = hz                              !! threadprivate
                  ob(icmd)%tsin     = 0.
                  ob(icmd)%peakrate = 0.
                  hyd_flo = 0.
                  if (ob(icmd)%rcv_tot > 0) then
                    do in = 1, ob(icmd)%rcv_tot
                      iob = ob(icmd)%obj_in(in)
                      ihyd = ob(icmd)%ihtyp_in(in)
                      frac_in = ob(icmd)%frac_in(in)
                      ob(icmd)%peakrate = ob(iob)%peakrate
                      !! chandeg is never hru/ru/hru_lte -> mirrors only the "else" (non-hru)
                      !! branch of the sequential gather loop below
                      ht1 = frac_in * ob(iob)%hd(ihyd)
                      ob(icmd)%hin = ob(icmd)%hin + ht1
                      hdsep1_local%flo_surq = frac_in * (ob(iob)%hdsep%flo_surq)
                      hdsep1_local%flo_latq = frac_in * (ob(iob)%hdsep%flo_latq)
                      hdsep1_local%flo_gwsw = frac_in * (ob(iob)%hdsep%flo_gwsw)
                      hdsep1_local%flo_swgw = frac_in * (ob(iob)%hdsep%flo_swgw)
                      hdsep1_local%flo_satex = frac_in * (ob(iob)%hdsep%flo_satex)
                      hdsep1_local%flo_satexsw = frac_in * (ob(iob)%hdsep%flo_satexsw)
                      hdsep1_local%flo_tile = frac_in * (ob(iob)%hdsep%flo_tile)
                      ob(icmd)%hdsep_in%flo_surq = ob(icmd)%hdsep_in%flo_surq + hdsep1_local%flo_surq
                      ob(icmd)%hdsep_in%flo_latq = ob(icmd)%hdsep_in%flo_latq + hdsep1_local%flo_latq
                      ob(icmd)%hdsep_in%flo_gwsw = ob(icmd)%hdsep_in%flo_gwsw + hdsep1_local%flo_gwsw
                      ob(icmd)%hdsep_in%flo_swgw = ob(icmd)%hdsep_in%flo_swgw + hdsep1_local%flo_swgw
                      ob(icmd)%hdsep_in%flo_satex = ob(icmd)%hdsep_in%flo_satex + hdsep1_local%flo_satex
                      ob(icmd)%hdsep_in%flo_satexsw = ob(icmd)%hdsep_in%flo_satexsw + hdsep1_local%flo_satexsw
                      ob(icmd)%hdsep_in%flo_tile = ob(icmd)%hdsep_in%flo_tile + hdsep1_local%flo_tile
                      ob(icmd)%hin_d(in) = ht1        !for hydrograph output
                      iday = ob(iob)%day_cur
                      if (ob(iob)%typ == "hru" .or. ob(iob)%typ == "ru") then
                        select case (ob(icmd)%htyp_in(in))
                        case ("tot")
                          hyd_flo = ob(iob)%hyd_flo(iday,:) + (ob(iob)%hd(4)%flo + ob(iob)%hd(5)%flo) / time%step
                        case ("sur")
                          hyd_flo(:) = ob(iob)%hyd_flo(iday,:)
                        case ("lat")
                          hyd_flo(:) = (ob(iob)%hd(4)%flo) / time%step
                        case ("til")
                          hyd_flo(:) = (ob(iob)%hd(5)%flo) / time%step
                        end select
                      else
                        select case (ob(icmd)%htyp_in(in))
                        case ("tot")
                          hyd_flo(:) = ob(iob)%hyd_flo(1,:)
                        case ("rhg")
                          hyd_flo(:) = ob(iob)%hd(2)%flo / time%step
                        case ("lat")
                          hyd_flo(:) = ob(iob)%hd(4)%flo / time%step
                        case ("til")
                          hyd_flo(:) = ob(iob)%hd(5)%flo / time%step
                        end select
                      end if
                      select case (ob(iob)%typ)
                      case ("aqu")
                        hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
                      case ("chandeg")
                        hyd_flo(:) = ob(iob)%hyd_flo(1,:)
                        sumflo = sum (hyd_flo(:))
                        sumflo = 1. * sumflo
                      case ("res")
                        hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
                      case ("outlet")
                        hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
                      case ("recall")
                        irec = ob(iob)%num
                        if (recall_db(irec)%org_min%tstep == "sub") then
                          hyd_flo(:) = ob(iob)%hyd_flo(ob(iob)%day_cur,:)
                        else
                          hyd_flo(:) = ob(iob)%hd(1)%flo / time%step
                        end if
                      end select
                      hyd_flo = frac_in * hyd_flo
                      ob(icmd)%tsin = ob(icmd)%tsin + hyd_flo
                    end do   ! in = 1, rcv_tot
                  end if
                  !! sd_channel_control3 is only reachable here for chl > 1.e-3 (gated at par_cha
                  !! build time); the zero-length artificial-channel branch never applies here
                  call sd_channel_control3
                end block
              end do
              !$omp end parallel do
              icmd = icmd_save   !! restore the walk's position
            end if
          end if

          !! === OpenMP Stage 3: this wavefront level's reservoir pre-pass ===
          if (use_par3 .and. ob(icmd)%cmd_order <= max_res_level) then
            lvl_lo_r = par_res_lvl_start(ob(icmd)%cmd_order)
            lvl_hi_r = par_res_lvl_start(ob(icmd)%cmd_order + 1) - 1
            if (lvl_hi_r >= lvl_lo_r) then
              icmd_save_r = icmd
              !$omp parallel do schedule(dynamic)
              do k = lvl_lo_r, lvl_hi_r
                block
                  integer :: in, iob, ihyd, iday, irec, ires
                  real :: frac_in, sumflo
                  real, dimension(time%step) :: hyd_flo
                  type (hyd_sep) :: hdsep1_local
                  icmd = par_res(k)                     !! threadprivate icmd
                  ob(icmd)%day_cur = 1
                  ob(icmd)%day_cur = ob(icmd)%day_cur + 1
                  if (ob(icmd)%day_cur > ob(icmd)%day_max) ob(icmd)%day_cur = 1
                  ob(icmd)%hin     = hz
                  ob(icmd)%hin_sur = hz
                  ob(icmd)%hin_lat = hz
                  ob(icmd)%hin_til = hz
                  ht1 = hz                              !! threadprivate
                  ob(icmd)%tsin     = 0.
                  ob(icmd)%peakrate = 0.
                  hyd_flo = 0.
                  do in = 1, ob(icmd)%rcv_tot           !! a reservoir is never hru/ru -> "else" branch only
                    iob = ob(icmd)%obj_in(in)
                    ihyd = ob(icmd)%ihtyp_in(in)
                    frac_in = ob(icmd)%frac_in(in)
                    ob(icmd)%peakrate = ob(iob)%peakrate
                    ht1 = frac_in * ob(iob)%hd(ihyd)
                    ob(icmd)%hin = ob(icmd)%hin + ht1
                    hdsep1_local%flo_surq = frac_in * (ob(iob)%hdsep%flo_surq)
                    hdsep1_local%flo_latq = frac_in * (ob(iob)%hdsep%flo_latq)
                    hdsep1_local%flo_gwsw = frac_in * (ob(iob)%hdsep%flo_gwsw)
                    hdsep1_local%flo_swgw = frac_in * (ob(iob)%hdsep%flo_swgw)
                    hdsep1_local%flo_satex = frac_in * (ob(iob)%hdsep%flo_satex)
                    hdsep1_local%flo_satexsw = frac_in * (ob(iob)%hdsep%flo_satexsw)
                    hdsep1_local%flo_tile = frac_in * (ob(iob)%hdsep%flo_tile)
                    ob(icmd)%hdsep_in%flo_surq = ob(icmd)%hdsep_in%flo_surq + hdsep1_local%flo_surq
                    ob(icmd)%hdsep_in%flo_latq = ob(icmd)%hdsep_in%flo_latq + hdsep1_local%flo_latq
                    ob(icmd)%hdsep_in%flo_gwsw = ob(icmd)%hdsep_in%flo_gwsw + hdsep1_local%flo_gwsw
                    ob(icmd)%hdsep_in%flo_swgw = ob(icmd)%hdsep_in%flo_swgw + hdsep1_local%flo_swgw
                    ob(icmd)%hdsep_in%flo_satex = ob(icmd)%hdsep_in%flo_satex + hdsep1_local%flo_satex
                    ob(icmd)%hdsep_in%flo_satexsw = ob(icmd)%hdsep_in%flo_satexsw + hdsep1_local%flo_satexsw
                    ob(icmd)%hdsep_in%flo_tile = ob(icmd)%hdsep_in%flo_tile + hdsep1_local%flo_tile
                    ob(icmd)%hin_d(in) = ht1
                    iday = ob(iob)%day_cur
                    if (ob(iob)%typ == "hru" .or. ob(iob)%typ == "ru") then
                      select case (ob(icmd)%htyp_in(in))
                      case ("tot")
                        hyd_flo = ob(iob)%hyd_flo(iday,:) + (ob(iob)%hd(4)%flo + ob(iob)%hd(5)%flo) / time%step
                      case ("sur")
                        hyd_flo(:) = ob(iob)%hyd_flo(iday,:)
                      case ("lat")
                        hyd_flo(:) = (ob(iob)%hd(4)%flo) / time%step
                      case ("til")
                        hyd_flo(:) = (ob(iob)%hd(5)%flo) / time%step
                      end select
                    else
                      select case (ob(icmd)%htyp_in(in))
                      case ("tot")
                        hyd_flo(:) = ob(iob)%hyd_flo(1,:)
                      case ("rhg")
                        hyd_flo(:) = ob(iob)%hd(2)%flo / time%step
                      case ("lat")
                        hyd_flo(:) = ob(iob)%hd(4)%flo / time%step
                      case ("til")
                        hyd_flo(:) = ob(iob)%hd(5)%flo / time%step
                      end select
                    end if
                    select case (ob(iob)%typ)
                    case ("aqu")
                      hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
                    case ("chandeg")
                      hyd_flo(:) = ob(iob)%hyd_flo(1,:)
                      sumflo = sum (hyd_flo(:))
                      sumflo = 1. * sumflo
                    case ("res")
                      hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
                    case ("outlet")
                      hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
                    case ("recall")
                      irec = ob(iob)%num
                      if (recall_db(irec)%org_min%tstep == "sub") then
                        hyd_flo(:) = ob(iob)%hyd_flo(ob(iob)%day_cur,:)
                      else
                        hyd_flo(:) = ob(iob)%hd(1)%flo / time%step
                      end if
                    end select
                    hyd_flo = frac_in * hyd_flo
                    ob(icmd)%tsin = ob(icmd)%tsin + hyd_flo
                  end do
                  ires = ob(icmd)%num
                  call res_control (ires)
                end block
              end do
              !$omp end parallel do
              icmd = icmd_save_r
            end if
          end if

          !! === OpenMP Stage 3b: this wavefront level's aquifer pre-pass ===
          if (use_par4 .and. ob(icmd)%cmd_order <= max_aqu_level) then
            lvl_lo_a = par_aqu_lvl_start(ob(icmd)%cmd_order)
            lvl_hi_a = par_aqu_lvl_start(ob(icmd)%cmd_order + 1) - 1
            if (lvl_hi_a >= lvl_lo_a) then
              icmd_save_a = icmd
              !$omp parallel do schedule(dynamic)
              do k = lvl_lo_a, lvl_hi_a
                block
                  integer :: in, iob, ihyd, iday, irec
                  real :: frac_in, sumflo
                  real, dimension(time%step) :: hyd_flo
                  type (hyd_sep) :: hdsep1_local
                  icmd = par_aqu(k)                     !! threadprivate icmd
                  ob(icmd)%day_cur = 1
                  ob(icmd)%day_cur = ob(icmd)%day_cur + 1
                  if (ob(icmd)%day_cur > ob(icmd)%day_max) ob(icmd)%day_cur = 1
                  ob(icmd)%hin     = hz
                  ob(icmd)%hin_sur = hz
                  ob(icmd)%hin_lat = hz
                  ob(icmd)%hin_til = hz
                  ht1 = hz                              !! threadprivate
                  ob(icmd)%tsin     = 0.
                  ob(icmd)%peakrate = 0.
                  hyd_flo = 0.
                  do in = 1, ob(icmd)%rcv_tot           !! an aquifer is never hru/ru -> "else" branch only
                    iob = ob(icmd)%obj_in(in)
                    ihyd = ob(icmd)%ihtyp_in(in)
                    frac_in = ob(icmd)%frac_in(in)
                    ob(icmd)%peakrate = ob(iob)%peakrate
                    ht1 = frac_in * ob(iob)%hd(ihyd)
                    ob(icmd)%hin = ob(icmd)%hin + ht1
                    hdsep1_local%flo_surq = frac_in * (ob(iob)%hdsep%flo_surq)
                    hdsep1_local%flo_latq = frac_in * (ob(iob)%hdsep%flo_latq)
                    hdsep1_local%flo_gwsw = frac_in * (ob(iob)%hdsep%flo_gwsw)
                    hdsep1_local%flo_swgw = frac_in * (ob(iob)%hdsep%flo_swgw)
                    hdsep1_local%flo_satex = frac_in * (ob(iob)%hdsep%flo_satex)
                    hdsep1_local%flo_satexsw = frac_in * (ob(iob)%hdsep%flo_satexsw)
                    hdsep1_local%flo_tile = frac_in * (ob(iob)%hdsep%flo_tile)
                    ob(icmd)%hdsep_in%flo_surq = ob(icmd)%hdsep_in%flo_surq + hdsep1_local%flo_surq
                    ob(icmd)%hdsep_in%flo_latq = ob(icmd)%hdsep_in%flo_latq + hdsep1_local%flo_latq
                    ob(icmd)%hdsep_in%flo_gwsw = ob(icmd)%hdsep_in%flo_gwsw + hdsep1_local%flo_gwsw
                    ob(icmd)%hdsep_in%flo_swgw = ob(icmd)%hdsep_in%flo_swgw + hdsep1_local%flo_swgw
                    ob(icmd)%hdsep_in%flo_satex = ob(icmd)%hdsep_in%flo_satex + hdsep1_local%flo_satex
                    ob(icmd)%hdsep_in%flo_satexsw = ob(icmd)%hdsep_in%flo_satexsw + hdsep1_local%flo_satexsw
                    ob(icmd)%hdsep_in%flo_tile = ob(icmd)%hdsep_in%flo_tile + hdsep1_local%flo_tile
                    ob(icmd)%hin_d(in) = ht1
                    iday = ob(iob)%day_cur
                    if (ob(iob)%typ == "hru" .or. ob(iob)%typ == "ru") then
                      select case (ob(icmd)%htyp_in(in))
                      case ("tot")
                        hyd_flo = ob(iob)%hyd_flo(iday,:) + (ob(iob)%hd(4)%flo + ob(iob)%hd(5)%flo) / time%step
                      case ("sur")
                        hyd_flo(:) = ob(iob)%hyd_flo(iday,:)
                      case ("lat")
                        hyd_flo(:) = (ob(iob)%hd(4)%flo) / time%step
                      case ("til")
                        hyd_flo(:) = (ob(iob)%hd(5)%flo) / time%step
                      end select
                    else
                      select case (ob(icmd)%htyp_in(in))
                      case ("tot")
                        hyd_flo(:) = ob(iob)%hyd_flo(1,:)
                      case ("rhg")
                        hyd_flo(:) = ob(iob)%hd(2)%flo / time%step
                      case ("lat")
                        hyd_flo(:) = ob(iob)%hd(4)%flo / time%step
                      case ("til")
                        hyd_flo(:) = ob(iob)%hd(5)%flo / time%step
                      end select
                    end if
                    select case (ob(iob)%typ)
                    case ("aqu")
                      hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
                    case ("chandeg")
                      hyd_flo(:) = ob(iob)%hyd_flo(1,:)
                      sumflo = sum (hyd_flo(:))
                      sumflo = 1. * sumflo
                    case ("res")
                      hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
                    case ("outlet")
                      hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
                    case ("recall")
                      irec = ob(iob)%num
                      if (recall_db(irec)%org_min%tstep == "sub") then
                        hyd_flo(:) = ob(iob)%hyd_flo(ob(iob)%day_cur,:)
                      else
                        hyd_flo(:) = ob(iob)%hd(1)%flo / time%step
                      end if
                    end select
                    hyd_flo = frac_in * hyd_flo
                    ob(icmd)%tsin = ob(icmd)%tsin + hyd_flo
                  end do
                  call aqu_1d_control
                end block
              end do
              !$omp end parallel do
              icmd = icmd_save_a
            end if
          end if

          !! === OpenMP Stage 4: this wavefront level's routing-unit pre-pass ===
          if (use_par5 .and. ob(icmd)%cmd_order <= max_ru_level) then
            lvl_lo_u = par_ru_lvl_start(ob(icmd)%cmd_order)
            lvl_hi_u = par_ru_lvl_start(ob(icmd)%cmd_order + 1) - 1
            if (lvl_hi_u >= lvl_lo_u) then
              icmd_save_u = icmd
              !$omp parallel do schedule(dynamic)
              do k = lvl_lo_u, lvl_hi_u
                block
                  integer :: in, iob, ihyd, iday, irec
                  real :: frac_in, sumflo, conv
                  real, dimension(time%step) :: hyd_flo
                  icmd = par_ru(k)                     !! threadprivate icmd
                  iru = ob(icmd)%num                    !! threadprivate iru
                  ob(icmd)%day_cur = ob(icmd)%day_cur + 1
                  if (ob(icmd)%day_cur > ob(icmd)%day_max) ob(icmd)%day_cur = 1
                  ob(icmd)%hin     = hz
                  ob(icmd)%hin_sur = hz
                  ob(icmd)%hin_lat = hz
                  ob(icmd)%hin_til = hz
                  ob(icmd)%tsin     = 0.
                  ob(icmd)%peakrate = 0.
                  hyd_flo = 0.
                  if (ob(icmd)%rcv_tot > 0) then
                    do in = 1, ob(icmd)%rcv_tot
                      iob = ob(icmd)%obj_in(in)
                      ihyd = ob(icmd)%ihtyp_in(in)
                      frac_in = ob(icmd)%frac_in(in)
                      ob(icmd)%peakrate = ob(iob)%peakrate
                      !! icmd is always "ru" here -> only the TRUE branch of the hru/ru/hru_lte check
                      if (ob(icmd)%obtyp_in(in) == "hru" .or. ob(icmd)%obtyp_in(in) == "ru" .or.  &
                                                               ob(icmd)%obtyp_in(in) == "hru_lte") then
                        if (ob(icmd)%htyp_in(in) == "tot") then
                          ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(3)
                          ob(icmd)%hin_til = ob(icmd)%hin_til + frac_in * ob(iob)%hd(5)
                          ob(icmd)%hin_lat = ob(icmd)%hin_lat + frac_in * ob(iob)%hd(4)
                        else
                          select case (ob(icmd)%htyp_in(in))
                          case ("tot")
                            ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(ihyd)
                          case ("sur")
                            ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(ihyd)
                          case ("lat")
                            ob(icmd)%hin_lat = ob(icmd)%hin_lat + frac_in * ob(iob)%hd(ihyd)
                          case ("til")
                            ob(icmd)%hin_til = ob(icmd)%hin_til + frac_in * ob(iob)%hd(ihyd)
                          case ("aqu")
                            ob(icmd)%hin_aqu = ob(icmd)%hin_aqu + frac_in * ob(iob)%hd(ihyd)
                          end select
                        end if
                      else
                        ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(1)
                      end if
                      iday = ob(iob)%day_cur
                      if (ob(iob)%typ == "hru" .or. ob(iob)%typ == "ru") then
                        select case (ob(icmd)%htyp_in(in))
                        case ("tot")
                          hyd_flo = ob(iob)%hyd_flo(iday,:) + (ob(iob)%hd(4)%flo + ob(iob)%hd(5)%flo) / time%step
                        case ("sur")
                          hyd_flo(:) = ob(iob)%hyd_flo(iday,:)
                        case ("rhg")
                          hyd_flo(:) = ob(iob)%hd(2)%flo / time%step
                        case ("lat")
                          hyd_flo(:) = ob(iob)%hd(4)%flo / time%step
                        case ("til")
                          hyd_flo(:) = ob(iob)%hd(5)%flo / time%step
                        end select
                      end if
                      select case (ob(iob)%typ)
                      case ("aqu")
                        hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
                      case ("chandeg")
                        hyd_flo(:) = ob(iob)%hyd_flo(1,:)
                        sumflo = sum (hyd_flo(:))
                        sumflo = 1. * sumflo
                      case ("res")
                        hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
                      case ("outlet")
                        hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
                      case ("recall")
                        irec = ob(iob)%num
                        if (recall_db(irec)%org_min%tstep == "sub") then
                          hyd_flo(:) = ob(iob)%hyd_flo(ob(iob)%day_cur,:)
                        else
                          hyd_flo(:) = ob(iob)%hd(1)%flo / time%step
                        end if
                      end select
                      hyd_flo = frac_in * hyd_flo
                      ob(icmd)%tsin = ob(icmd)%tsin + hyd_flo
                    end do
                    !! per-area conversion (inside the sequential rcv_tot>0 guard)
                    conv = ob(icmd)%area_ha
                    ob(icmd)%hin_sur = ob(icmd)%hin_sur / conv
                    ob(icmd)%hin_sur%flo = ob(icmd)%hin_sur%flo / 10.
                    ob(icmd)%hin_lat = ob(icmd)%hin_lat / conv
                    ob(icmd)%hin_lat%flo = ob(icmd)%hin_lat%flo / 10.
                    ob(icmd)%hin_til = ob(icmd)%hin_til / conv
                    ob(icmd)%hin_til%flo = ob(icmd)%hin_til%flo / 10.
                  end if
                  call ru_control
                  !! Part 13: hyddep_output does file I/O -> DEFER to the sequential walk; capture
                  !! this thread's threadprivate ht1 now for the sequential-side restore.
                  ru_ht1_save(icmd) = ht1
                end block
              end do
              !$omp end parallel do
              icmd = icmd_save_u
            end if
          end if
          last_level_done = ob(icmd)%cmd_order
        end if

        !! Skip the compute+gather+dispatch for objects already done by a pre-pass above
        !! (independent HRUs and pre-passed chandeg). Their routing/output below still runs. At
        !! 1 thread the pre-pass did them in command order, so this stays bit-for-bit. The
        !! chandeg skip-check must match par_cha's build-time eligibility EXACTLY (incl.
        !! cha_producers_all_earlier) so an object isn't computed by neither path or both.
        icmd_is_precomputed_cha = .false.
        if (use_par2 .and. ob(icmd)%typ == "chandeg") then
          if (sd_ch(ob(icmd)%num)%chl > 1.e-3) then
            if (cha_producers_all_earlier(icmd)) icmd_is_precomputed_cha = .true.
          end if
        end if
        icmd_is_precomputed_res = .false.
        if (use_par3 .and. ob(icmd)%typ == "res") then
          if (ob(icmd)%rcv_tot > 0) then
            if (res_producers_all_earlier(icmd)) icmd_is_precomputed_res = .true.
          end if
        end if
        icmd_is_precomputed_aqu = .false.
        if (use_par4 .and. ob(icmd)%typ == "aqu") then
          if (ob(icmd)%dfn_tot == 0) then
            if (aqu_producers_all_earlier(icmd)) icmd_is_precomputed_aqu = .true.
          end if
        end if
        icmd_is_precomputed_ru = .false.
        if (use_par5 .and. ob(icmd)%typ == "ru") then
          if (ru_producers_all_earlier(icmd)) icmd_is_precomputed_ru = .true.
        end if
        !! Part 13: hyddep_output (file I/O + required hdep_* state accumulation) was NOT run
        !! inside the ru pre-pass; run it here for rcv_tot>0 ru's, restoring the ht1 the worker
        !! computed (this main thread's threadprivate ht1 is unrelated).
        if (icmd_is_precomputed_ru .and. ob(icmd)%rcv_tot > 0) then
          ht1 = ru_ht1_save(icmd)
          call hyddep_output
        end if
        if (.not. ((use_par .and. ob(icmd)%cmd_order == 1 .and.  &
                   ob(icmd)%typ == "hru" .and. ob(icmd)%rcv_tot == 0) .or.  &
                   icmd_is_precomputed_cha .or. icmd_is_precomputed_res .or.  &
                   icmd_is_precomputed_aqu .or. icmd_is_precomputed_ru)) then

        !! allocate water for transfers that don't include a channel as a source
        !! check here in case channel is last object
        if (db_mx%wallo_db > 0) then
          do iwallo = 1, db_mx%wallo_db  
            do while (wallo(iwallo)%trn_cur > 0)
              if (wallo(iwallo)%trn(wallo(iwallo)%trn_cur)%ch_src > 0) exit
              iw = iwallo
              if (wallo(iwallo)%trn_cur <= wallo(iwallo)%trn_obs) call wallo_control (iw)
            end do
          end do
        end if
          
        if (ob(icmd)%typ == "hru" .or. ob(icmd)%typ == "ru") then
          !! hru and ru can have hyrdographs that lag into next day
          ob(icmd)%day_cur = ob(icmd)%day_cur + 1
          if (ob(icmd)%day_cur > ob(icmd)%day_max) ob(icmd)%day_cur = 1
        else
        !! assume only one day is saved for all other objects
          ob(icmd)%day_cur = 1
          !!update current day of hydrograph for the object
          ob(icmd)%day_cur = ob(icmd)%day_cur + 1
          if (ob(icmd)%day_cur > ob(icmd)%day_max) ob(icmd)%day_cur = 1
        end if
        
        !sum all receiving hydrographs
        !if (ob(icmd)%rcv_tot > 0) then
          ob(icmd)%hin = hz
          ob(icmd)%hin_sur = hz
          ob(icmd)%hin_lat = hz
          ob(icmd)%hin_til = hz
          ht1 = hz
          if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
            obcs(icmd)%hin = hin_csz
            obcs(icmd)%hin_sur = hin_csz
            obcs(icmd)%hin_lat = hin_csz
            obcs(icmd)%hin_til = hin_csz
          endif
          hcs1 = hin_csz
          hcs2 = hin_csz
          hcs3 = hin_csz
          ob(icmd)%tsin = 0.
          ob(icmd)%peakrate = 0.
          hyd_flo = 0.
          
          if (ob(icmd)%rcv_tot > 0) then
          do in = 1, ob(icmd)%rcv_tot
            iob = ob(icmd)%obj_in(in)
            ihyd = ob(icmd)%ihtyp_in(in)
            frac_in = ob(icmd)%frac_in(in)
            ob(icmd)%peakrate = ob(iob)%peakrate
            
            ! if object is not an hru, need ht1, don't need %hin_sur and %hin_lat
            ! don't have to check if it's in an ru - only hru's can be routed over
            if (ob(icmd)%typ == "hru" .or. ob(icmd)%typ == "ru" .or. ob(icmd)%typ == "hru_lte") then
                
              !! if incoming object is not an hru or ru, send total hyd to surface runoff
              if (ob(icmd)%obtyp_in(in) == "hru" .or. ob(icmd)%obtyp_in(in) == "ru" .or.          &
                                                       ob(icmd)%obtyp_in(in) == "hru_lte") then
                ! receiving hru, needs %hin_sur and %hin_lat and %hin_til to route separately in hru_control
                if (ob(icmd)%htyp_in(in) == "tot") then
                  ! if total hyd coming in from hru or ru -> add both surface and lateral flows
                  ! add to surface runon
                  ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(3)
                  if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                    obcs(icmd)%hin_sur(1) = obcs(icmd)%hin_sur(1) + frac_in * obcs(iob)%hd(3)
                  end if
                  ! add to tile flow
                  ob(icmd)%hin_til = ob(icmd)%hin_til + frac_in * ob(iob)%hd(5)
                  if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                    obcs(icmd)%hin_til(1) = obcs(icmd)%hin_til(1) + frac_in * obcs(iob)%hd(5)
                  end if
                  ! add to lateral soil runon
                  ob(icmd)%hin_lat = ob(icmd)%hin_lat + frac_in * ob(iob)%hd(4)
                  if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                    obcs(icmd)%hin_lat(1) = obcs(icmd)%hin_lat(1) + frac_in * obcs(iob)%hd(4)
                  end if
                else
                  ! if hyd in is not a total hyd from an hru or ru -> add the specified hyd typ 
                  select case (ob(icmd)%htyp_in(in))
                  case ("tot")   ! total flow
                    ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(ihyd)
                    !add constituents
                    if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                      obcs(icmd)%hin_til(1) = obcs(icmd)%hin_til(1) + frac_in * obcs(iob)%hd(ihyd)
                    end if
                  case ("sur")   ! surface runoff
                    ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(ihyd)
                    !add constituents
                    if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                      obcs(icmd)%hin_sur(1) = obcs(icmd)%hin_sur(1) + frac_in * obcs(iob)%hd(ihyd)
                    end if
                  case ("lat")   ! lateral soil flow
                    ob(icmd)%hin_lat = ob(icmd)%hin_lat + frac_in * ob(iob)%hd(ihyd)
                    !add constituents
                    if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                      obcs(icmd)%hin_lat(1) = obcs(icmd)%hin_lat(1) + frac_in * obcs(iob)%hd(ihyd)
                    end if
                  case ("til")   ! tile flow
                    ob(icmd)%hin_til = ob(icmd)%hin_til + frac_in * ob(iob)%hd(ihyd)
                    !add constituents
                    if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                      obcs(icmd)%hin_til(1) = obcs(icmd)%hin_til(1) + frac_in * obcs(iob)%hd(ihyd)
                    end if
                  case ("aqu")   ! aquifer inflow
                    ob(icmd)%hin_aqu = ob(icmd)%hin_aqu + frac_in * ob(iob)%hd(ihyd)
                    !add constituents
                    if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                      obcs(icmd)%hin_aqu(1) = obcs(icmd)%hin_aqu(1) + frac_in * obcs(iob)%hd(ihyd)
                    end if
                  end select
                end if  
              else
                ! add total inflow to surface runon if channel or recall
                ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(1)
                if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                  obcs(icmd)%hin_sur(1) = obcs(icmd)%hin_sur(1) + frac_in * obcs(iob)%hd(1)
                end if
              end if
              
            else
              ! all objects other than hru's
              ! fraction of organics
              ht1 = frac_in * ob(iob)%hd(ihyd)
              ob(icmd)%hin = ob(icmd)%hin + ht1

              !rtb hydrograph separation
              hdsep1%flo_surq = frac_in * (ob(iob)%hdsep%flo_surq)
              hdsep1%flo_latq = frac_in * (ob(iob)%hdsep%flo_latq)
              hdsep1%flo_gwsw = frac_in * (ob(iob)%hdsep%flo_gwsw)
              hdsep1%flo_swgw = frac_in * (ob(iob)%hdsep%flo_swgw)
              hdsep1%flo_satex = frac_in * (ob(iob)%hdsep%flo_satex)
              hdsep1%flo_satexsw = frac_in * (ob(iob)%hdsep%flo_satexsw)
              hdsep1%flo_tile = frac_in * (ob(iob)%hdsep%flo_tile)
              ob(icmd)%hdsep_in%flo_surq = ob(icmd)%hdsep_in%flo_surq + hdsep1%flo_surq
              ob(icmd)%hdsep_in%flo_latq = ob(icmd)%hdsep_in%flo_latq + hdsep1%flo_latq
              ob(icmd)%hdsep_in%flo_gwsw = ob(icmd)%hdsep_in%flo_gwsw + hdsep1%flo_gwsw
              ob(icmd)%hdsep_in%flo_swgw = ob(icmd)%hdsep_in%flo_swgw + hdsep1%flo_swgw
              ob(icmd)%hdsep_in%flo_satex = ob(icmd)%hdsep_in%flo_satex + hdsep1%flo_satex
              ob(icmd)%hdsep_in%flo_satexsw = ob(icmd)%hdsep_in%flo_satexsw + hdsep1%flo_satexsw
              ob(icmd)%hdsep_in%flo_tile = ob(icmd)%hdsep_in%flo_tile + hdsep1%flo_tile
              
              ! fraction of constituents
              if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                hcs1 = frac_in * obcs(iob)%hd(ihyd)
                obcs(icmd)%hin(1) = obcs(icmd)%hin(1) + hcs1
              end if
              ob(icmd)%hin_d(in) = ht1        !for hydrograph output
              if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                obcs(icmd)%hcsin_d(in) = hcs1   !for constituent hydrograph output
              endif
            end if
            
            !sum subdaily inflow hydrographs
            !if (time%step > 0) then
              iday = ob(iob)%day_cur
              if (ob(iob)%typ == "hru" .or. ob(iob)%typ == "ru") then
                select case (ob(icmd)%htyp_in(in))
                case ("tot")   ! total flow
                  hyd_flo = ob(iob)%hyd_flo(iday,:) + (ob(iob)%hd(4)%flo + ob(iob)%hd(5)%flo) / time%step
                case ("sur")   ! surface runoff
                  hyd_flo(:) = ob(iob)%hyd_flo(iday,:)
                case ("rhg")   ! recharge
                  hyd_flo(:) = ob(iob)%hd(2)%flo / time%step
                case ("lat")   ! lateral soil flow
                  hyd_flo(:) = ob(iob)%hd(4)%flo / time%step
                case ("til")   ! tile flow
                  hyd_flo(:) = ob(iob)%hd(5)%flo / time%step
                end select
              end if
              select case (ob(iob)%typ)
              case ("aqu")      ! aquifer inflow
                hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
              case ("chandeg")  ! channel inflow
                hyd_flo(:) = ob(iob)%hyd_flo(1,:)
                sumflo = sum (hyd_flo(:))
                sumflo = 1. * sumflo
              case ("res")      ! reservoir inflow
                hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
              case ("outlet")      ! outlet inflow
                hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
              case ("recall")   ! point source inflow
                irec = ob(iob)%num
                if (recall_db(irec)%org_min%tstep == "sub") then    !subdaily
                  hyd_flo(:) = ob(iob)%hyd_flo(ob(iob)%day_cur,:)
                else                                ! monthly, yearly, and ave annual
                  hyd_flo(:) = ob(iob)%hd(1)%flo / time%step
                end if
              end select
                
              !! multiply inflow hyd by the fraction of incoming
              hyd_flo = frac_in * hyd_flo
              !! add flow hydrographs for each incoming object
              ob(icmd)%tsin = ob(icmd)%tsin + hyd_flo
              
            !end if

          end do    ! in = 1, ob(icmd)%rcv_tot

          !convert to per area basis
          if (ob(icmd)%typ == "hru" .or. ob(icmd)%typ == "ru") then  !only convert hru and subbasin hyds for routing
            conv = ob(icmd)%area_ha
            ob(icmd)%hin_sur = ob(icmd)%hin_sur / conv
            ob(icmd)%hin_sur%flo = ob(icmd)%hin_sur%flo / 10.      ! m3/10*ha = mm
            ob(icmd)%hin_lat = ob(icmd)%hin_lat / conv
            ob(icmd)%hin_lat%flo = ob(icmd)%hin_lat%flo / 10.      ! m3/10*ha = mm
            ob(icmd)%hin_til = ob(icmd)%hin_til / conv
            ob(icmd)%hin_til%flo = ob(icmd)%hin_til%flo / 10.      ! m3/10*ha = mm
          end if
        end if

        ! select the next command type
        select case (ob(icmd)%typ)
            
          case ("hru")   ! hru
            ihru = ob(icmd)%num
            call hru_control
            if (ob(icmd)%rcv_tot > 0) call hyddep_output
                      
          case ("hru_lte")   ! hru_lte
            isd = ob(icmd)%num
            call hru_lte_control (isd)
            !if (ob(icmd)%rcv_tot > 0) call hyddep_output
            
          case ("ru")   ! subbasin
            iru = ob(icmd)%num
            call ru_control
            if (ob(icmd)%rcv_tot > 0) call hyddep_output

          case ("gwflow")   ! gwflow
            call gwflow_simulate
            do i_mfl = 1,sp_ob%gwflow
              icmd = icmd + 1
            enddo
            icmd = icmd - 1
            
          case ("aqu")   ! aquifer
            if (ob(icmd)%dfn_tot == 0) then   !1-D use old bf recession
              call aqu_1d_control
            end if
          
          !case ("chan")   ! channel
          !  jrch = ob(icmd)%num
          !  jrchq = ob(icmd)%props2
          !  if (ob(icmd)%rcv_tot > 0) then
          !    call channel_control
          ! end if

          case ("res")   ! reservoir
            ires = ob(icmd)%num
            if (ob(icmd)%rcv_tot > 0) then
              call res_control (ires)
            end if 
              
          case ("recall")   ! recall hydrograph
            irec = ob(icmd)%num
            select case (recall_db(irec)%org_min%tstep)
              case ("sub")    !subdaily
                ts1 = (time%day - 1) * time%step + 1
                ts2 = time%day * time%step
                ob(icmd)%hyd_flo(ob(icmd)%day_cur,:) = recall(irec)%hyd_flo(ts1:ts2,time%yrs)
                ob(icmd)%hd(1) = recall(irec)%hd(time%day,time%yrs)
              case ("day")    !daily
                if (time%yrc >= recall(irec)%start_yr .and. time%yrc <= recall(irec)%end_yr) then 
                    ob(icmd)%hd(1) = recall(irec)%hd(time%day,time%yrs)
                    !if negative flow (diversion), then remove nutrient mass
                    if(recall(irec)%hd(time%day,time%yrs)%flo < 0) then
                      call recall_nut(irec)
                    endif
                else
                    ob(icmd)%hd(1) = hz
                end if
              case ("mo")    !monthly
                if (time%yrc >= recall(irec)%start_yr .and. time%yrc <= recall(irec)%end_yr) then 
                    ob(icmd)%hd(1) = recall(irec)%hd(time%mo,time%yrs)
                else
                    ob(icmd)%hd(1) = hz
                end if
              case ("yr")    !yearly
                if (time%yrc >= recall(irec)%start_yr .or. time%yrc <= recall(irec)%end_yr) then
                  ob(icmd)%hd(1) = recall(irec)%hd(1,time%yrs)
                else
                  ob(icmd)%hd(1) = hz
                end if
              !case (4)    !average annual
              !  ob(icmd)%hd(1) = recall(irec)%hd(1,1)
              end select
              
              rec_d(irec) = ob(icmd)%hd(1)
              
              if (cs_db%num_tot > 0) obcs(icmd)%hd(1) = hin_csz
              if (cs_db%num_salts > 0) call recall_salt(irec) !rtb salt
              if (cs_db%num_cs > 0) call recall_cs(irec) !rtb cs
              
          !case ("exco")   ! export coefficient hyds are set at start

          case ("dr")   ! delivery ratios
            ob(icmd)%hd(1) = ob(icmd)%hin ** dr(ob(icmd)%props) ! ** is an intrinsic function to multiply 
            if (cs_db%num_tot > 0) then
              idr = ob(iob)%props
              
              call constit_hyd_mult (icmd, idr)
            end if
            
          case ("outlet")  !outlet
            ob(icmd)%hd(1) = ob(icmd)%hin
            if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
              obcs(icmd)%hd(1) = obcs(icmd)%hin(1) !rtb salt/cs
            endif
              
          case ("chandeg")  !swatdeg channel
            isdch = ob(icmd)%num
            isd_chsur = ob(icmd)%props2
            if (sd_ch(isdch)%chl > 1.e-3) then
              call sd_channel_control3
            else
                !! artificial channel - length=0 - no transformations
                ob(icmd)%hd(1) = ob(icmd)%hin
                
                ch_in_d(isdch) = ht1                        !set inflow om hydrograph
                chsd_d(isdch)%flo_in = ht1%flo / 86400.     !flow for morphology output
                ch_in_d(isdch)%flo = ht1%flo / 86400.       !flow for om output
                ch_out_d(isdch) = ht1                       !set inflow om hydrograph
                ch_out_d(isdch)%flo = ht1%flo / 86400.      !m3 -> m3/s
                !! output channel morphology
                chsd_d(isdch)%flo = ht1%flo / 86400.        !adjust if overbank flooding is moved to landscape
                chsd_d(isdch)%peakr = 0. 
                chsd_d(isdch)%sed_in = ob(icmd)%hin%sed
                chsd_d(isdch)%sed_out = ob(icmd)%hin%sed
                chsd_d(isdch)%washld = 0.
                chsd_d(isdch)%bedld = 0.
                chsd_d(isdch)%dep = 0.
                chsd_d(isdch)%deg_btm = .0
                chsd_d(isdch)%deg_bank = 0.
                chsd_d(isdch)%hc_sed = 0.
                chsd_d(isdch)%width = sd_ch(isdch)%chw
                chsd_d(isdch)%depth = sd_ch(isdch)%chd
                chsd_d(isdch)%slope = sd_ch(isdch)%chs
                chsd_d(isdch)%deg_btm_m = 0.
                chsd_d(isdch)%deg_bank_m = 0.
                chsd_d(isdch)%hc_m = 0.
                if (cs_db%num_tot > 0 .and. obcs_alloc(icmd).eq.1) then
                  obcs(icmd)%hd(1) = obcs(icmd)%hin(1)
                end if
            end if
            
          end select

        end if   !! end skip-compute wrapper for pre-pass HRUs

        !! allocate water for transfers that don't include a channel as a source
        !! check here in case channel is not the last object
        if (db_mx%wallo_db > 0) then
          do iwallo = 1, db_mx%wallo_db  
            do while (wallo(iwallo)%trn_cur > 0)
              if (wallo(iwallo)%trn(wallo(iwallo)%trn_cur)%ch_src > 0) exit
              iw = iwallo
              if (wallo(iwallo)%trn_cur <= wallo(iwallo)%trn_obs) call wallo_control (iw)
            end do
          end do
        end if
          
        !! compute flow duration curves for channels
        if (pco%fdcout == "y" .and. ob(icmd)%typ == "chandeg") then
          call flow_dur_curve
          !! compute flashiness index
          ob(icmd)%flash_idx%sum_q_q1 = ob(icmd)%flash_idx%sum_q_q1 + (ob(icmd)%hd(1)%flo - ob(icmd)%flash_idx%q_prev)
          ob(icmd)%flash_idx%q_prev = ob(icmd)%hd(1)%flo
          ob(icmd)%flash_idx%sum_q = ob(icmd)%flash_idx%sum_q + ob(icmd)%hd(1)%flo
        end if
  
        !print all outflow hydrographs
        if (time%yrs > pco%nyskip) then
          if (ob(icmd)%src_tot > 0) then
            do iout = 1, ob(icmd)%src_tot
              ihtyp = ob(icmd)%ihtyp_out(iout)
              ht1 = ob(icmd)%frac_out(iout) * ob(icmd)%hd(ihtyp)
              call hydout_output (iout)
            end do
          end if
        end if
        
        !set the next command
        icmd = ob(icmd)%cmd_next
        
      end do

      !! write object output for entire simulation
      call obj_output
      
      !! print all output files
      if (time%yrs > pco%nyskip) then
      
        !! print water allocation output
        do iwro =1, db_mx%wallo_db
          call wallo_allo_output (iwro)
          call wallo_trn_output (iwro)
          call wallo_treat_output (iwro)
          call wallo_use_output (iwro)
          !call wallo_osrc_output (iwro)
          !call wallo_odmd_output (iwro)
        end do
        
        !! print manure allocation output
        do iwro =1, db_mx%mallo_db
          call manure_source_output (iwro)
          call manure_demand_output (iwro)
        end do
        
        do isd = 1, sp_ob%hru_lte
          call hru_lte_output (isd)
        end do
        
        do ihru = 1, sp_ob%hru
          !! accumulation half - no file I/O, per-HRU only, safe to parallelize
          call hru_output_accum (ihru)
          call hru_output (ihru)
          call hru_carbon_output (ihru)
          if (hru(ihru)%dbs%surf_stor > 0) then
            call wetland_output(ihru)
            if (cs_db%num_salts > 0) then !rtb salt
              call wet_salt_output(ihru)
            endif
            if (cs_db%num_cs > 0) then !rtb cs
              call wet_cs_output(ihru)
            endif
          end if
          if (cs_db%num_tot > 0) then 
            call hru_pesticide_output (ihru)
            call hru_pathogen_output (ihru)
          end if
          if (cs_db%num_salts > 0) then !rtb salt
            call hru_salt_output(ihru)
          endif
          if (cs_db%num_cs > 0) then !rtb cs
            call hru_cs_output(ihru)
          endif
          !sum annual for SWIFT input
          if (bsn_cc%swift_out == 1) then
            icmd = hru(ihru)%obj_no
            do ihyd = 1, 5
              ob(icmd)%hd_aa(ihyd) = ob(icmd)%hd_aa(ihyd) + ob(icmd)%hd(ihyd)
            end do
          end if
                         
          ! Call soil_nutcarb_write for specified output for hru_cb in print.prt
          if (pco%cb_hru%d == "y") call soil_nutcarb_write(" d")
          if (pco%cb_hru%d == "l") call soil_nutcarb_write("dl")
          if (pco%cb_hru%m == "y" .and. time%end_mo == 1) call soil_nutcarb_write(" m")
          if (pco%cb_hru%m == "l" .and. time%end_mo == 1) call soil_nutcarb_write("ml")
          if (pco%cb_hru%y == "y" .and. time%end_yr == 1) call soil_nutcarb_write(" y") 
          if (pco%cb_hru%y == "l" .and. time%end_yr == 1) call soil_nutcarb_write("yl") 

          ! Call soil_carbvar_write for specified output for hru_cb_vars in print.prt
          if (bsn_cc%cswat == 1) then
            if (pco%cb_vars_hru%d == "y") call soil_carbvar_write(" d")
            if (pco%cb_vars_hru%d == "l") call soil_carbvar_write("dl")
            if (pco%cb_vars_hru%m == "y" .and. time%end_mo == 1) call soil_carbvar_write(" m")
            if (pco%cb_vars_hru%m == "l" .and. time%end_mo == 1) call soil_carbvar_write("ml")
            if (pco%cb_vars_hru%y == "y" .and. time%end_yr == 1) call soil_carbvar_write(" y")
            if (pco%cb_vars_hru%y == "l" .and. time%end_yr == 1) call soil_carbvar_write("yl")
          endif
        
        end do      ! hru loop  
        
        do iaq = 1, sp_ob%aqu
          call aquifer_output (iaq)
          if (cs_db%num_salts > 0) then !rtb salt
            call aqu_salt_output (iaq)
          endif
          if (cs_db%num_cs > 0) then !rtb cs
            call aqu_cs_output(iaq)
          endif  
          if (cs_db%num_tot > 0) then 
            call aqu_pesticide_output (iaq)
          end if       
        end do
        
        do jrch = 1, sp_ob%chan
          call channel_output (jrch)
        end do
                
        do jrch = 1, sp_ob%chandeg
          call sd_chanmorph_output (jrch)
          call sd_chanbud_output (jrch)
          call sd_channel_output (jrch)
          if (cs_db%num_tot > 0) then 
            call cha_pesticide_output (jrch)   
            !call ch_pathogen_output (jrch)
          end if   
          if (cs_db%num_salts > 0) then !rtb salt
            call ch_salt_output (jrch)
          endif
          if (cs_db%num_cs > 0) then
            call ch_cs_output (jrch) !rtb cs
          endif
        end do
        if(cs_db%num_cs > 0) then
          call cs_str_output !rtb cs
        endif
        

        do j = 1, sp_ob%res
          call reservoir_output(j)
         if (cs_db%num_tot > 0) then 
            call res_pesticide_output (j)
            if (cs_db%num_salts > 0) then !rtb salt
              call res_salt_output (j)
            endif
            if (cs_db%num_cs > 0) then !rtb cs
              call res_cs_output (j)
            endif
            !call res_pathogen_output (j)
          end if       
        end do 
        
        do j = 1, sp_ob%ru
          call ru_output(j)
          if(cs_db%num_salts > 0) then !rtb salt
            call ru_salt_output(j)
          endif
          if(cs_db%num_cs > 0) then !rtb cs
            call ru_cs_output(j)
          endif
        end do
        
        do j = 1, sp_ob%recall
          call recall_output (j)
        end do

        call hydin_output   !if all output is no, then don"t call
        !call hcsin_output  gives allocate error
        if (sp_ob%chandeg > 0 .and. cs_db%num_pests > 0) call basin_ch_pest_output  
        if (sp_ob%res > 0 .and. cs_db%num_pests > 0) call basin_res_pest_output     
        if (sp_ob%hru > 0 .and. cs_db%num_pests > 0) call basin_ls_pest_output
        if (sp_ob%aqu > 0 .and. cs_db%num_pests > 0) call basin_aqu_pest_output
        if (db_mx%lsu_elem > 0) call basin_output
        if (db_mx%lsu_out > 0) call lsu_output
        if (db_mx%aqu_elem > 0) call basin_aquifer_output
        !if (sp_ob%aqu > 0) call basin_aquifer_output !rtb - otherwise, aquifer output is not called
        if (sp_ob%res > 0) call basin_reservoir_output
        if (sp_ob%chan > 0) call basin_channel_output
        if (sp_ob%chandeg > 0) call basin_chanmorph_output
        if (sp_ob%chandeg > 0) call basin_chanbud_output
        if (sp_ob%chandeg > 0) call basin_sdchannel_output
        if (sp_ob%recall > 0) call basin_recall_output
        !call soil_nutcarb_output
        !call lsreg_output
        !call region_aquifer_output
        !call region_reservoir_output
        !call region_channel_output
        !call region_recall_output
        
        if(cs_db%num_salts > 0) call salt_balance !rtb salt
        if(cs_db%num_cs > 0) call cs_balance !rtb cs
        
      end if

      gw_daycount = gw_daycount + 1
      
      !rtb hydrograph separation
      !write out hydrograph components for all channels
      if (bsn_cc%gwflow == 1) then
      do i_chan=1,sp_ob%chandeg
        if(hydsep_flag(i_chan) == 1) then
          iob_chan = sp_ob1%chandeg + i_chan - 1
          write(out_hyd_sep,8102) time%day,time%mo,time%day_mo,time%yrc, &
            i_chan,ob(iob_chan)%gis_id,ob(iob_chan)%name, &
            (hyd_sep_array(i_chan,i_count),i_count=1,7)
        endif
      enddo
      endif
      !zero out arrays for next day
      icmd = sp_ob1%objs
      do while (icmd /= 0)
        ob(icmd)%hdsep%flo_surq = 0.
        ob(icmd)%hdsep%flo_latq = 0.
        ob(icmd)%hdsep%flo_gwsw = 0.
        ob(icmd)%hdsep%flo_swgw = 0.
        ob(icmd)%hdsep%flo_satex = 0.
        ob(icmd)%hdsep%flo_satexsw = 0.
        ob(icmd)%hdsep%flo_tile = 0.
        ob(icmd)%hdsep_in%flo_surq = 0.
        ob(icmd)%hdsep_in%flo_latq = 0.
        ob(icmd)%hdsep_in%flo_gwsw = 0.
        ob(icmd)%hdsep_in%flo_swgw = 0.
        ob(icmd)%hdsep_in%flo_satex = 0.
        ob(icmd)%hdsep_in%flo_satexsw = 0.
        ob(icmd)%hdsep_in%flo_tile = 0.  
        icmd = ob(icmd)%cmd_next
      enddo
      
102   format(i6,11x,i3,8x,i5,5x,1000(f16.4))
103   format(4i6,2i8,2x,a,35f12.3)
8102  format(4i6,2i8,a18,7e13.4)      

      return

      contains

      !! true only if EVERY producer of icmd_chk has a strictly lower cmd_order than icmd_chk
      !! itself. cmd_order is only an approximate depth diagnostic (hyd_connect pins "ru" objects
      !! to order 2 regardless of true depth), so a channel can legitimately share its producer's
      !! cmd_order despite a real dependency -- such objects fail this check and fall back to the
      !! always-correct sequential walk. Also excludes channels with floodplain HRUs, which update
      !! shared per-HRU wetland state (two channels can share a floodplain HRU and race on it).
      logical function cha_producers_all_earlier(icmd_chk)
        integer, intent(in) :: icmd_chk
        integer :: ii, iob_chk
        cha_producers_all_earlier = .true.
        if (sd_ch(ob(icmd_chk)%num)%fp%hru_tot > 0) then
          cha_producers_all_earlier = .false.
          return
        end if
        do ii = 1, ob(icmd_chk)%rcv_tot
          iob_chk = ob(icmd_chk)%obj_in(ii)
          if (ob(iob_chk)%cmd_order >= ob(icmd_chk)%cmd_order) then
            cha_producers_all_earlier = .false.
            return
          end if
        end do
      end function cha_producers_all_earlier

      !! reservoir analogue of cha_producers_all_earlier (no floodplain exclusion needed --
      !! reservoirs have no floodplain-HRU cross-object shared-state write).
      logical function res_producers_all_earlier(icmd_chk)
        integer, intent(in) :: icmd_chk
        integer :: ii, iob_chk
        res_producers_all_earlier = .true.
        do ii = 1, ob(icmd_chk)%rcv_tot
          iob_chk = ob(icmd_chk)%obj_in(ii)
          if (ob(iob_chk)%cmd_order >= ob(icmd_chk)%cmd_order) then
            res_producers_all_earlier = .false.
            return
          end if
        end do
      end function res_producers_all_earlier

      !! aquifer analogue -- recharge arrives via a single "ru" producer through the standard
      !! hydrograph network, so the same single-producer-order check applies.
      logical function aqu_producers_all_earlier(icmd_chk)
        integer, intent(in) :: icmd_chk
        integer :: ii, iob_chk
        aqu_producers_all_earlier = .true.
        do ii = 1, ob(icmd_chk)%rcv_tot
          iob_chk = ob(icmd_chk)%obj_in(ii)
          if (ob(iob_chk)%cmd_order >= ob(icmd_chk)%cmd_order) then
            aqu_producers_all_earlier = .false.
            return
          end if
        end do
      end function aqu_producers_all_earlier

      !! A ru has TWO independent dependency mechanisms, both checked here:
      !! (1) ru_def(iru)%num -> ru_elem(ise)%obj -- the ru's internal HRU-membership list
      !!     (NOT a hydrograph edge, so cmd_order alone isn't sufficient). A member with a
      !!     strictly lower cmd_order is fine; a member at the SAME cmd_order is also fine IF
      !!     it matches Stage 1's own eligibility (hru, cmd_order==1, rcv_tot==0), because the
      !!     Stage-1 HRU pre-pass runs unconditionally, in full, before the day-loop begins.
      !! (2) ob(icmd)%obj_in -- the standard hydrograph network (rcv_tot>0 ru's also receive
      !!     inflow this way), which requires strictly-lower cmd_order producers.
      logical function ru_producers_all_earlier(icmd_chk)
        integer, intent(in) :: icmd_chk
        integer :: iru_chk, ii, ise_chk, iob_chk
        logical :: elem_precomputed_by_stage1
        ru_producers_all_earlier = .true.
        iru_chk = ob(icmd_chk)%num
        do ii = 1, ru_def(iru_chk)%num_tot
          ise_chk = ru_def(iru_chk)%num(ii)
          iob_chk = ru_elem(ise_chk)%obj
          if (ob(iob_chk)%cmd_order < ob(icmd_chk)%cmd_order) cycle
          elem_precomputed_by_stage1 = (ob(iob_chk)%typ == "hru" .and. ob(iob_chk)%cmd_order == 1  &
                                         .and. ob(iob_chk)%rcv_tot == 0)
          if (.not. elem_precomputed_by_stage1) then
            ru_producers_all_earlier = .false.
            return
          end if
        end do
        do ii = 1, ob(icmd_chk)%rcv_tot
          iob_chk = ob(icmd_chk)%obj_in(ii)
          if (ob(iob_chk)%cmd_order >= ob(icmd_chk)%cmd_order) then
            ru_producers_all_earlier = .false.
            return
          end if
        end do
      end function ru_producers_all_earlier

      end subroutine command