      subroutine pl_fert (ifrt, frt_kg, fertop)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies N and P specified by date and
!!    amount in the management file (.mgt)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Erfc

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use mgt_operations_module
      use fertilizer_data_module
      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : ihru, fertn, fertp, fertnh3, fertno3, fertorgn, fertorgp, fertp,  &
        fertsolp  

      implicit none 
      
      real :: rtof             !none          |weighting factor used to partition the 
                                          !              |organic N & P concentration of septic effluent
                                          !              |between the fresh organic and the stable organic pools
      integer :: j = 0                    !none          |hru counter
      integer :: l = 0                    !none          |layer counter 
      integer, intent (in) :: ifrt        !              |fertilizer type from fert data base
      integer, intent (in) :: fertop      !              |fertilizer operation type
      real, intent (in) :: frt_kg         !kg/ha         |total mass of fertilizer applied
      real :: fr_ly = 0.                  !fraction      |fraction of fertilizer applied to layer
      logical :: organic_flag

      organic_flag = .false.
      org_frt%m = 0.
      org_frt%n = 0.
      org_frt%p = 0.

      j = ihru
      
      rtof = man_coef%rtof
      !! flag an entry that carries organic N or P.  no c:n ratio is calculated
      !! any more - see the note below on why fertilizer.frt cannot supply one.
      if (bsn_cc%cswat == 1 ) then
        if (fertdb(ifrt)%forgn > 0. .or. fertdb(ifrt)%forgp > 0. ) then
          organic_flag = .true.
        endif
      endif
        
      if (organic_flag) then
        !! NO CARBON IS CALCULATED HERE, deliberately.  fertilizer.frt carries
        !! fminn, fminp, forgn, forgp and fnh3n and nothing else - there is no
        !! carbon fraction in the file, so any carbon derived from it is invented.
        !! the previous form was
        !!    org_frt%c = forgn * frt_kg * 10.0    ! assume 10:1 C:N
        !! which is a fair central estimate for real manure (manure_om.frt has a
        !! median C:orgN of 11.5:1) but is a category error for the synthetic
        !! entries, where forgn is the slow-release share of a pure-N product and
        !! not organic matter at all: ceap_h_n is fminn 0.41 + forgn 0.59 = 1.00,
        !! and 10:1 hands it 5.9 kg C per kg applied - several times the mass
        !! actually spread on the field.  fert_parm_read names the suspect
        !! entries at startup.
        org_frt%m = frt_kg
        org_frt%n = fertdb(ifrt)%forgn * frt_kg
        org_frt%p = fertdb(ifrt)%forgp * frt_kg
      endif
      
      !! add fertilizer to first and/or second layer
      do l = 1, 2
        if (l == 1) then
          fr_ly = chemapp_db(fertop)%surf_frac
        else
          fr_ly = 1. - chemapp_db(fertop)%surf_frac                     
        endif

        !! add mineral n and p for all methods
        soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 + fr_ly * frt_kg *          &
                       (1. - fertdb(ifrt)%fnh3n) * fertdb(ifrt)%fminn
        soil1(j)%mn(l)%nh4 = soil1(j)%mn(l)%nh4 + fr_ly * frt_kg *          &
                       fertdb(ifrt)%fnh3n * fertdb(ifrt)%fminn
        soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab + fr_ly * frt_kg *          & 
                       fertdb(ifrt)%fminp

        !! add total organic n and p for all methods
        soil1(j)%tot(l)%n = soil1(j)%tot(l)%n + rtof * fr_ly * frt_kg *     &
                       fertdb(ifrt)%forgn
        soil1(j)%tot(l)%p = soil1(j)%tot(l)%p + rtof * fr_ly * frt_kg *     &
                       fertdb(ifrt)%forgp

        !! for stable carbon - add n and p to active humus pool
        !! NOTE: cswat==0 branch only; the abg/blg split is not used here (no CENTURY lignin
        !! partitioning on this path), so incorporated manure N/P is placed in abg purely for
        !! type-correctness. Origin is immaterial to cswat==0 results. Revisit if manure ever
        !! needs an origin under the CENTURY path (cswat==1).
        if (bsn_cc%cswat == 0) then
          soil1(j)%pl(1)%rsd(l)%abg%n = soil1(j)%pl(1)%rsd(l)%abg%n + rtof * fr_ly *            &
                       frt_kg * fertdb(ifrt)%forgn
          soil1(j)%pl(1)%rsd(l)%abg%p = soil1(j)%pl(1)%rsd(l)%abg%p + rtof * fr_ly * frt_kg *   &
                       fertdb(ifrt)%forgp
          soil1(j)%hact(l)%n = soil1(j)%hact(l)%n + (1. - rtof) * fr_ly *               &
                       frt_kg * fertdb(ifrt)%forgn
          soil1(j)%hact(l)%p = soil1(j)%hsta(l)%p + (1. - rtof) * fr_ly * frt_kg *      &
                       fertdb(ifrt)%forgp
        end if
        
        !! for SWAT-C put the organic N and P in the slow humus pool
        if ((bsn_cc%cswat == 1 ) .and. organic_flag) then
          
          !! all of it, not an rtof split: the metabolic and structural litter
          !! pools cannot be filled without a carbon amount to partition, and
          !! meta_fr was itself derived from the invented carbon.  slow humus is
          !! also the least distorting home for nitrogen that arrives with no
          !! carbon - on the IA-Ames fixture hs%n is ~4118 kg/ha, so a 60 kg/ha
          !! application moves it 1.5%, where the same amount into metabolic
          !! (0.57 kg/ha) would be four orders of magnitude.
          !!
          !! the residual cost is real and deliberate: humus C:N drifts down a
          !! little with each organic fertilizer application, because the N is
          !! represented and its carbon is not.  that is an honest omission
          !! rather than a fabricated number.  the fix for a dataset that means
          !! manure is to move those entries into manure_om.frt (which does
          !! carry fcbn) and apply them with a "manu" operation.
          soil1(j)%hs(l)%n = soil1(j)%hs(l)%n + fr_ly * org_frt%n
          soil1(j)%hs(l)%p = soil1(j)%hs(l)%p + fr_ly * org_frt%p
          
        end if
        
      end do 

      !! summary calculations
      fertno3 = frt_kg * fertdb(ifrt)%fminn * (1. - fertdb(ifrt)%fnh3n)
      fertnh3 = frt_kg * (fertdb(ifrt)%fminn * fertdb(ifrt)%fnh3n)
      fertorgn = frt_kg * fertdb(ifrt)%forgn
      fertsolp = frt_kg * fertdb(ifrt)%fminp
      fertorgp = frt_kg * fertdb(ifrt)%forgp  
      fertn = fertn + frt_kg * (fertdb(ifrt)%fminn + fertdb(ifrt)%forgn)
      fertp = fertp + frt_kg * (fertdb(ifrt)%fminp + fertdb(ifrt)%forgp)
      
      return
      end subroutine pl_fert