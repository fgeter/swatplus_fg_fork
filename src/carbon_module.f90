      module carbon_module
!< summary: Declares the parameters, state, and flux containers for the SWAT+ CENTURY-based carbon cycle.
!<
!< **Purpose:** `carbon_module` is the central declaration module for the SWAT+
!< carbon cycle. It holds every parameter, control factor, transformation rate,
!< and accounting container that the CENTURY-style soil organic matter routines
!< (`cbn_zhang2`) and the carbon output routines share. The module declares no
!< executable model logic of its own; the routines that read it
!< (`carbon_read`, `carbon_coef_read`), the routine that drives it (`cbn_zhang2`),
!< and the routines that report it (`hru_carbon_output`, `soil_nutcarb_write`)
!< supply the behaviour.
!<
!< **Summary of contents:**
!<
!< - **Input parameter sets** &mdash; `carbon_terrestrial_inputs` (`cbn_tes`, read
!<   from `basins_carbon.tes`), `carbon_inputs` (`carbdb`, the transformation
!<   rates actually used by `cbn_zhang2`), `manure_coef` (`man_coef`), and
!<   `carbon_water_coef` (`cb_wtr_coef`, carbon transport in runoff and sediment).
!< - **Process controls** &mdash; `organic_allocations` (`org_allo`, the fraction of
!<   each decomposed pool routed to CO2 or to another pool), `organic_controls`
!<   (`org_con`, the temperature / water / oxygen / tillage multipliers),
!<   `organic_fractions` (`org_frac`, litter and humus partitioning), and
!<   `organic_ratio` (`org_ratio`, N/C ratios of the humus pools).
!< - **Per-layer working variables** &mdash; `organic_transformations` (`org_tran`,
!<   the potential C and N transformation of each pool) and `organic_flux`
!<   (`org_flux`, the realised pool-to-pool C and N fluxes for one layer-day).
!< - **Output accounting containers** &mdash; `carbon_soil_transformations`,
!<   `carbon_soil_gain_losses`, `carbon_residue_gain_losses`, and
!<   `carbon_plant_gain_losses`, each instantiated at daily / monthly / yearly /
!<   average time steps (`_d`, `_m`, `_y`, `_a`) and at HRU (`h`), landscape unit
!<   (`l`), and basin (`b`) spatial scales.
!< - **Operator overloads** &mdash; `+`, `*`, and `/` are defined for the four
!<   accounting types so that output routines can aggregate them over time and
!<   space with ordinary arithmetic.
!<
!< **Pool naming.** The CENTURY pools are abbreviated consistently throughout:
!< S1 = microbial biomass, S2 = slow humus, S3 = passive humus, with metabolic
!< and structural litter feeding S1 and S2.
!<
!< **Layer classes.** The two-element arrays `carbdb` and `org_allo` are indexed
!< by layer class, selected as `cf_lyr` in `cbn_zhang2`: index 1 is soil layer 1,
!< the top 10 mm, and index 2 is every layer below 10 mm.
!<
!< **Activation.** This carbon path runs when `codes.bsn` `cswat = 1`;
!< `hru_control` gates the `cbn_surfrsd_decomp` / `cbn_rsd_transfer` /
!< `cbn_zhang2` sequence on it, and `carbon_coef_read` reads `carb_coefs.cbn`
!< under the same condition.
!<
!< @warning The meaning of `cswat = 1` versus `cswat = 2` is a **known open
!< issue** awaiting resolution between two development groups. The `codes.bsn`
!< documentation and the `cswat` comment in `basin_module` both label
!< `cswat = 1` as the "C-FARM one carbon pool model" and `cswat = 2` as the
!< "Century model", while this branch's code runs the CENTURY multi-pool path at
!< `cswat = 1` and the `carb_coefs.cbn` documentation also says "Applies when
!< codes.bsn carbon = 1". Do not change either the code or these comments to
!< "fix" the inconsistency on your own; it is a coordination question, not a
!< defect in this module.
!<
!< **Related published documentation.** The SWAT+ I/O documentation on GitBook
!< covers the input file that populates most of this module,
!< [carb_coefs.cbn](https://swatplus.gitbook.io/io-docs/introduction-1/carbon/carb_coefs.cbn),
!< and the files this module's accounting containers are written to,
!< [Carbon output files](https://swatplus.gitbook.io/io-docs/swat+-output-files/carbon).
!< It also documents parameters set by `carb_coefs.cbn` that live outside this
!< module: `till_eff_days`, `bio_consf`, `till_consf`, the `zz_bmix_coef_*`
!< tillage-factor coefficients, and `photo_degrade_factor`.
!<
!< @note Those pages are a useful cross-check but are **not** authoritative over
!< this branch. The carbon routines here lead `swat-model/swatplus` `main`, and
!< the published documentation tracks neither branch reliably. Where they
!< disagree, this source is what the model does; record the difference rather
!< than editing the code to match the documentation.
!<
!< **Units convention.** Every declaration below carries a trailing
!< `!< units | description` annotation. `none` marks a dimensionless quantity
!< (a counter, an index, a flag, or a pure coefficient); `frac` marks a
!< dimensionless quantity constrained to 0-1.
!<
!< @note Zhang, X., et al. implementation of Parton et al. (1993, 1994) CENTURY
!< soil organic matter dynamics.

      implicit none

      logical :: cbn_diagnostics = .false.   !< none | controls how many carbon output files `soil_nutcarb_write` produces. `.false.` writes the two SOC files
!<   `hru_cbn_lyr` and `hru_seq_lyr`; `.true.` adds `hru_cflux_stat`, `hru_cpool_stat`, `hru_n_p_pool_stat`,
!<   `hru_begin_soil_prop`, and `hru_end_soil_prop`, seven in total. Each is written as both `.txt` and `.csv`.

      type carbon_terrestrial_inputs
!< Terrestrial carbon-cycle input parameters read from `basins_carbon.tes`.
!<
!< Groups: enrichment and residue composition, dissolved carbon transport,
!< CO2 and inter-pool allocation fractions, optimal-condition decomposition
!< rates, soil-texture controls, and the oxygen-factor coefficients.
!<
!< @warning This type is populated by `carbon_read` but is not currently
!< consumed anywhere in the model. `cbn_zhang2` uses the equivalent values in
!< `carbon_inputs` (`carbdb`) and `organic_allocations` (`org_allo`) instead.
!< Treat the values here as a documented parameter reference, not as active
!< model input.
        real :: er_POC_para = 1.5       !< none | particulate organic carbon (POC) enrichment ratio; typical range 0-10, most sensitive over 0.0-5.0
        real :: CFB_para = 0.42         !< kg C kg-1 | carbon fraction of residue dry matter (0.42, from data of Pinck et al., 1950)
        real :: Sf_para_sur = 0.05      !< frac | fraction of mineral N sorbed to surface litter
        real :: Sf_para_sub = 0.10      !< frac | fraction of mineral N sorbed to below-ground litter
        !Dissolved carbon
        real :: ABL_para = 0.0          !< frac | carbon allocation from microbial biomass to leaching (calculated, not read)
        real :: peroc_DIC_para = 0.95   !< frac | dissolved inorganic carbon (DIC) percolation coefficient; range 0-1
        real :: peroc_DOC_para  = 0.70  !< frac | dissolved organic carbon (DOC) percolation coefficient; range 0-1
        real :: part_DOC_para = 4000.   !< m3 Mg-1 | organic carbon liquid-solid partition coefficient (KOC-equivalent, numerically the same as L kg-1); literature range 500-2000
        real :: hlife_doc_para = 50.    !< days | DOC half life in groundwater, used to compute DOC decay in groundwater; range 0-100
        !Allocation of CO2 and carbon transformation
        real :: ABCO2_para_sur = 0.6    !< frac | allocation from microbial biomass C to CO2, surface litter layer (Parton et al., 1993, 1994)
        real :: ABCO2_para_sub = 0.     !< frac | allocation from microbial biomass C to CO2, subsurface layers; calculated as 0.85 - 0.68 x (clay + silt)
        real :: ABP_para_sur = 0.0      !< frac | allocation from microbial biomass to passive humus, surface litter layer (Parton et al., 1993, 1994)
        real :: ABP_para_sub = 0.0      !< frac | allocation from microbial biomass to passive humus, subsurface layers; calculated as 0.003 + 0.032 x clay
        real :: ALMCO2_para_sur = 0.6   !< frac | allocation from metabolic litter to CO2, surface litter layer (Parton et al., 1993, 1994)
        real :: ALMCO2_para_sub = 0.55  !< frac | allocation from metabolic litter to CO2, subsurface layers (Parton et al., 1993, 1994)
        real :: ALSLNCO2_para_sur = 0.6 !< frac | allocation from the non-lignin part of structural litter to CO2, surface litter layer (Parton et al., 1993, 1994)
        real :: ALSLNCO2_para_sub =0.55 !< frac | allocation from the non-lignin part of structural litter to CO2, subsurface layers (Parton et al., 1993, 1994)
        real :: ASP_para_sur = 0.0      !< frac | allocation from slow humus to passive humus, surface litter layer (Parton et al., 1993, 1994)
        real :: ASP_para_sub = 0.0      !< frac | allocation from slow humus to passive humus, subsurface layers; calculated as 0.003 + 0.00009 x clay
        real :: ALSLCO2_para = 0.3      !< frac | allocation from the lignin part of structural litter to CO2 (Parton et al., 1993, 1994)
        real :: APCO2_para = 0.55       !< frac | allocation from passive humus to CO2 (Parton et al., 1993, 1994)
        real :: ASCO2_para = 0.55       !< frac | allocation from slow humus to CO2 (Parton et al., 1993, 1994)
        !decomposition rates
        real :: PRMT_51_para = 1.0      !< none | coefficient adjusting the microbial activity function in the top soil layer; range 0.1-1.0
        real :: PRMT_45_para = 0.003    !< none | CENTURY coefficient allocating slow humus to passive humus; range 0.001-0.05, original value 0.003
        real :: BMR_para_sur = 0.0164   !< day-1 | transformation rate of microbial biomass and associated products under optimal conditions, surface litter layer (Parton et al., 1993, 1994)
        real :: BMR_para_sub = 0.02     !< day-1 | transformation rate of microbial biomass and associated products under optimal conditions, subsurface layers (Parton et al., 1993, 1994)
        real :: HPR_para = 0.000012     !< day-1 | transformation rate of passive humus under optimal conditions (Parton et al., 1993, 1994)
        real :: HSR_para = 0.000548     !< day-1 | transformation rate of slow humus under optimal conditions (Parton et al., 1993, 1994; Vitousek et al., 1993)
        real :: LMR_para_sur = 0.0405   !< day-1 | transformation rate of metabolic litter under optimal conditions, surface litter layer (Parton et al., 1994)
        real :: LMR_para_sub = 0.0507   !< day-1 | transformation rate of metabolic litter under optimal conditions, subsurface layers (Parton et al., 1994)
        real :: LSR_para_sur = 0.0107   !< day-1 | potential transformation rate of structural litter under optimal conditions, surface litter layer (Parton et al., 1994)
        real :: LSR_para_sub = 0.0132   !< day-1 | potential transformation rate of structural litter under optimal conditions, subsurface layers (Parton et al., 1994)
        !Soil texture controls of microbial activity
        real :: XBM_para_sur = 1.0      !< none | soil texture and structure control on microbial biomass transformation, surface litter layer (Parton et al., 1993, 1994)
        real :: XBM_para_sub = 0.0      !< none | soil texture and structure control on microbial biomass transformation, subsurface layers; calculated as 1 - 0.75 x (silt + clay)
        real :: XLSLF_para = 0.0        !< none | lignin control on potential structural litter transformation; calculated as exp(-3 x lignin fraction) (Parton et al., 1993, 1994)
        !Oxygen factor control parameters
        real :: OX_aa_para = 10.0       !< none | numerator coefficient in the oxygen factor equation
        real :: OX_bb_para = 0.035      !< none | depth coefficient in the oxygen factor equation
      end type carbon_terrestrial_inputs
      type (carbon_terrestrial_inputs) :: cbn_tes   !< none | terrestrial carbon inputs as read from `basins_carbon.tes`

      type carbon_inputs
!< Optimal-condition transformation rates and partition coefficients used by
!< `cbn_zhang2`, read from `carb_coefs.cbn`. Instantiated as a two-element array
!< indexed by layer class: element 1 is the top 10 mm soil layer, element 2 is
!< every soil layer below 10 mm. The published documentation describes these as
!< "daily maximum potential transformation (decomposition) rate", one value per
!< layer class.
          real :: hp_rate = 0.          !< day-1 | transformation rate of passive humus under optimal conditions
          real :: hs_rate = 0.          !< day-1 | transformation rate of slow humus under optimal conditions
          real :: microb_rate = 0.      !< day-1 | transformation rate of microbial biomass and associated products under optimal conditions
          real :: meta_rate = 0.        !< day-1 | transformation rate of metabolic litter under optimal conditions
          real :: str_rate = 0.         !< day-1 | potential transformation rate of structural litter under optimal conditions
          real :: microb_top_rate = 0.  !< day-1 | transformation rate of microbial biomass, adjusting microbial activity in the top 10 mm layer
          real :: hs_hp = 0.            !< frac | fraction of the daily transformed slow pool allocated to the passive pool
          real :: microb_koc = 0.       !< 10^3 m3 Mg-1 | liquid-solid partition coefficient for microbial biomass
          real :: min_n_frac = 0.       !< frac | fraction of mineral N sorbed to litter
          real :: c_org_frac = 0.       !< kg C kg-1 | carbon fraction of organic materials
      end type carbon_inputs
      type (carbon_inputs), dimension(2) :: carbdb    !< none | carbon transformation rates; index 1 = top 10 mm soil layer, index 2 = layers below 10 mm
      type (carbon_inputs) :: carbz                   !< none | zeroed `carbon_inputs` used to reinitialise `carbdb`
      logical :: carbon_coef_file = .false.           !< none | `.true.` when the `carb_coefs.cbn` calibration file exists and has been read

      type manure_coef
!< Coefficients controlling how manure and septic organic matter are split
!< between the organic pools.
          real :: rtof = 0.5            !< frac | fraction used to partition C, N, and P between the fresh manure pool and the stable pool (slow humus, particulate organic matter); also applied to the organic N and P of septic effluent
          real :: man_to_c = 0.42       !< kg C kg-1 | conversion of manure solids to carbon
      end type manure_coef
      type (manure_coef) :: man_coef    !< none | manure and septic partitioning coefficients

      type organic_allocations
!< Fractions describing where the carbon leaving each decomposing pool goes,
!< read from `carb_coefs.cbn`. Instantiated as a two-element array indexed by
!< layer class: element 1 is the top 10 mm soil layer, element 2 is every soil
!< layer below 10 mm. `abp` and `asp` are not read but recomputed per layer from
!< soil clay content inside `cbn_zhang2`; `abco2` is likewise recomputed from
!< sand content for the subsurface layers.
          ! real :: abl = 0.        !               |Fraction of microbial biomass loss due to leaching
          real :: abp = 0.        !< frac | fraction of decomposed microbial biomass allocated to passive humus
          real :: asp = 0.        !< frac | fraction of decomposed slow humus allocated to passive humus
          ! real :: almco2 = 0.     !               |Fraction of decomposed metabolic litter allocated to CO2
          ! real :: alslco2 = 0.    !               |Fraction of decomposed lignin of structural litter allocated to CO2
          ! real :: alslnco2 = 0.   !               |Fraction of decomposed lignin of structural litter allocated to CO2
          real :: a1co2 =  0.     !< frac | fraction of daily transformed metabolic and non-lignin structural litter carbon emitted as CO2
          real :: asco2 = 0.      !< frac | fraction of daily transformed slow pool carbon emitted as CO2
          real :: apco2 = 0.      !< frac | fraction of daily transformed passive pool carbon emitted as CO2
          real :: abco2 = 0.      !< frac | fraction of daily transformed microbial pool carbon emitted as CO2
      end type organic_allocations
      type (organic_allocations), dimension(2) :: org_allo   !< none | carbon allocation fractions; index 1 = top 10 mm soil layer, index 2 = layers below 10 mm
      type (organic_allocations) :: org_alloz                !< none | zeroed `organic_allocations` used to reinitialise `org_allo`

      type organic_controls
!< Environmental multipliers that scale the optimal-condition transformation
!< rates down to actual conditions for one soil layer on one day, plus the
!< diagnostic values saved out of `cbn_zhang2`.
!<
!< The temperature and water factors are selectable formulations, following
!< Liang et al. (2022); `tmpf` and `watf` choose between them and are read from
!< `carb_coefs.cbn`.
          real :: sut = 0.           !< none | soil water control on biological processes; 0.05-1
          real :: cdg = 0.           !< none | soil temperature control on biological processes; 0-1
          real :: cs = 0.            !< none | combined control on biological processes, the product of the water, temperature, oxygen, and tillage factors; capped at 15
          real :: ox = 0.            !< none | oxygen (depth) control on biological processes; 0-1
          real :: till_eff           !< none | tillage effect multiplier; 1.0 when untilled, up to 1.6 in the tilled zone
          real :: x1 = 0.            !< day-1 | working decomposition rate for the pool currently being transformed, i.e. the optimal rate scaled by `cs`
          real :: no3 = 0.           !< kg N ha-1 | layer nitrate mass as adjusted by `cbn_zhang2`, saved for diagnostic output
          real :: nh4 = 0.           !< kg N ha-1 | layer ammonium mass as adjusted by `cbn_zhang2`, saved for diagnostic output
          real :: resp               !< kg C ha-1 day-1 | layer CO2 respiration, saved for diagnostic output
          ! real :: xbmt = 0.          !               |control on transformation of microbial biomass by soil texture and structure
          ! real :: xlslf = 0.         !               |control on potential transformation of structural litter by lignin fraction
          ! The following three parameters resolve the shape of the temperature effect equation (method 2 only):
          real :: tn = -5.           !< deg C | minimum soil temperature of the temperature response curve; applies to temperature factor method 2 (`tmpf = 2`)
          real :: top = 30.          !< deg C | optimum soil temperature of the temperature response curve; applies to temperature factor method 2 (`tmpf = 2`)
          real :: tx = 50.           !< deg C | maximum soil temperature of the temperature response curve; applies to temperature factor method 2 (`tmpf = 2`)
          integer :: tmpf = 2        !< none | soil temperature factor method: 1 = Izaurralde et al. (2006), 2 = Kemanian et al. (2011), 3 = Sharpley and Williams (1990)
          integer :: watf = 1        !< none | soil water factor method: 1 = Neitsch et al. (2011), 2 = Kemanian et al. (2011)
      end type organic_controls
      type (organic_controls) :: org_con   !< none | control factors for the layer currently being processed

      type organic_fractions
!< Fractions that split incoming residue into metabolic and structural litter
!< and that split the initial soil organic carbon from `soils.sol` among the
!< humus pools. The `frac_*` members are read from `carb_coefs.cbn`; `lmf`,
!< `lsf`, `lslf`, `lmnf`, and `lsnf` are recomputed per layer in `cbn_zhang2`.
!<
!< @note The published `carb_coefs.cbn` documentation gives the defaults for the
!< slow and passive pools the other way round from this branch: it lists
!< `frac_hum_slow` as 0.44 and `frac_hum_passive` as 0.54, whereas the
!< initialisers below are 0.54 and 0.44 respectively. The initialisers below are
!< what a run on this branch actually uses; the difference is recorded here, not
!< resolved.
          real :: lmf = 0.      !< frac | fraction of the litter that is metabolic
          real :: lmnf = 0.     !< kg N kg-1 C | nitrogen fraction of metabolic litter
          real :: lsf = 0.      !< frac | fraction of the litter that is structural
          real :: lslf = 0.     !< kg kg-1 | lignin fraction of structural litter
          real :: lsnf = 0.     !< kg N kg-1 C | nitrogen fraction of structural litter
          real :: frac_litter = .05          !< frac | fraction of the `soils.sol` carbon added as litter pool carbon at initialisation; the `soils.sol` value is sequestered carbon
          real :: frac_hum_microb = 0.02     !< frac | fraction of the `soils.sol` carbon allocated to the microbial pool at initialisation
          real :: frac_hum_slow = 0.54       !< frac | fraction of the `soils.sol` carbon allocated to the slow humus pool at initialisation (published documentation lists 0.44; see the type note)
          real :: frac_hum_passive = 0.44    !< frac | fraction of the `soils.sol` carbon allocated to the passive humus pool at initialisation (published documentation lists 0.54; see the type note)
          logical :: mathers_method = .false. !< none | `.true.` to use the Mathers et al. (2026) method to initialise the slow and passive humus pools instead of the fixed fractions above
      end type organic_fractions
      type (organic_fractions) :: org_frac   !< none | litter and humus partitioning fractions

      type organic_ratio
!< Nitrogen-to-carbon ratios of the humus pools, recomputed per layer in
!< `cbn_zhang2` from residue nitrogen content (surface) or mineral nitrogen
!< concentration (subsurface).
          ! real :: cnr = 0.         !                  |c/n ratio of standing dead
          real :: ncbm = 0.        !< kg N kg-1 C | N/C ratio of microbial biomass
          real :: nchp = 0.        !< kg N kg-1 C | N/C ratio of passive humus
          real :: nchs = 0.        !< kg N kg-1 C | N/C ratio of slow humus
      end type organic_ratio
      type (organic_ratio) :: org_ratio        !< none | N/C ratios for the layer currently being processed
      type (organic_ratio) :: org_ratio_zero   !< none | zeroed `organic_ratio` used to reinitialise `org_ratio`

      type carbon_water_coef
!< Coefficients controlling carbon loss to runoff, lateral flow, percolation,
!< and sediment, used by `nut_orgnc2`.
          real :: prmt_21 = 1000.   !< m3 Mg-1 | organic carbon-water partition coefficient, reflecting the strength of binding to organic matter versus remaining dissolved in water; KD = KOC x C; range 500-1500
          real :: prmt_44 = 0.5     !< none | ratio of surface runoff carbon concentration to percolate carbon concentration; range 0.1-1.0
      end type carbon_water_coef
      type (carbon_water_coef) :: cb_wtr_coef   !< none | carbon transport coefficients for water and sediment

      type organic_transformations
!< Potential (pre-allocation) transformation of carbon and nitrogen out of each
!< organic pool for one soil layer on one day. These are the rate x pool-mass
!< products before the CO2 and inter-pool allocation fractions are applied.
          real :: bmctp = 0.       !< kg C ha-1 day-1 | potential transformation of C in microbial biomass
          real :: bmntp = 0.       !< kg N ha-1 day-1 | potential transformation of N in microbial biomass
          real :: hsctp = 0.       !< kg C ha-1 day-1 | potential transformation of C in slow humus
          real :: hsntp = 0.       !< kg N ha-1 day-1 | potential transformation of N in slow humus
          real :: hpctp = 0.       !< kg C ha-1 day-1 | potential transformation of C in passive humus
          real :: hpntp = 0.       !< kg N ha-1 day-1 | potential transformation of N in passive humus
          real :: lmctp = 0.       !< kg C ha-1 day-1 | potential transformation of C in metabolic litter
          real :: lmntp = 0.       !< kg N ha-1 day-1 | potential transformation of N in metabolic litter
          real :: lsctp = 0.       !< kg C ha-1 day-1 | potential transformation of C in structural litter
          real :: lslctp = 0.      !< kg C ha-1 day-1 | potential transformation of C in the lignin part of structural litter
          real :: lslnctp = 0.     !< kg C ha-1 day-1 | potential transformation of C in the non-lignin part of structural litter
          real :: lsntp = 0.       !< kg N ha-1 day-1 | potential transformation of N in structural litter
      end type organic_transformations
      type (organic_transformations) :: org_tran        !< none | potential transformations for the layer currently being processed
      type (organic_transformations) :: org_tran_zero   !< none | zeroed `organic_transformations` used to reinitialise `org_tran`

      type organic_flux
!< Realised carbon and nitrogen fluxes between the CENTURY pools for one soil
!< layer on one day, after the allocation fractions have been applied.
!<
!< Member names encode source and destination: `cf`/`ef` are C and N transfers,
!< `imm` is nitrogen immobilisation and `mnr` nitrogen mineralisation
!< accompanying that transfer, and `co2f` is the CO2 released. The pool codes
!< are `met` (metabolic litter), `str` (structural litter), `s1` (microbial
!< biomass), `s2` (slow humus), and `s3` (passive humus).
          real :: cfmets1 = 0.           !< kg C ha-1 day-1 | C transformed from metabolic litter to S1 (microbial biomass)
          real :: cfstrs1 = 0.           !< kg C ha-1 day-1 | C transformed from structural litter to S1 (microbial biomass)
          real :: cfstrs2 = 0.           !< kg C ha-1 day-1 | C transformed from structural litter to S2 (slow humus)
          real :: efmets1 = 0.           !< kg N ha-1 day-1 | N transformed from metabolic litter to S1 (microbial biomass)
          real :: efstrs1 = 0.           !< kg N ha-1 day-1 | N transformed from structural litter to S1 (microbial biomass)
          real :: efstrs2 = 0.           !< kg N ha-1 day-1 | N transformed from structural litter to S2 (slow humus)
          real :: immmets1 = 0.          !< kg N ha-1 day-1 | N immobilisation resulting from transforming metabolic litter to S1 (microbial biomass)
          real :: immstrs1 = 0.          !< kg N ha-1 day-1 | N immobilisation resulting from transforming structural litter to S1 (microbial biomass)
          real :: immstrs2 = 0.          !< kg N ha-1 day-1 | N immobilisation resulting from transforming structural litter to S2 (slow humus)
          real :: mnrmets1 = 0.          !< kg N ha-1 day-1 | N mineralisation resulting from transforming metabolic litter to S1 (microbial biomass)
          real :: mnrstrs1 = 0.          !< kg N ha-1 day-1 | N mineralisation resulting from transforming structural litter to S1 (microbial biomass)
          real :: mnrstrs2 = 0.          !< kg N ha-1 day-1 | N mineralisation resulting from transforming structural litter to S2 (slow humus)
          real :: co2fmet = 0.           !< kg C ha-1 day-1 | CO2 production resulting from metabolic litter transformations
          real :: co2fstr = 0.           !< kg C ha-1 day-1 | CO2 production resulting from structural litter transformations
          real :: cfs1s2 = 0.            !< kg C ha-1 day-1 | C transformed from S1 (microbial biomass) to S2 (slow humus)
          real :: cfs1s3 = 0.            !< kg C ha-1 day-1 | C transformed from S1 (microbial biomass) to S3 (passive humus)
          real :: cfs2s1 = 0.            !< kg C ha-1 day-1 | C transformed from S2 (slow humus) to S1 (microbial biomass)
          real :: cfs2s3 = 0.            !< kg C ha-1 day-1 | C transformed from S2 (slow humus) to S3 (passive humus)
          real :: cfs3s1 = 0.            !< kg C ha-1 day-1 | C transformed from S3 (passive humus) to S1 (microbial biomass)
          real :: efs1s2 = 0.            !< kg N ha-1 day-1 | N transformed from S1 (microbial biomass) to S2 (slow humus)
          real :: efs1s3 = 0.            !< kg N ha-1 day-1 | N transformed from S1 (microbial biomass) to S3 (passive humus)
          real :: efs2s1 = 0.            !< kg N ha-1 day-1 | N transformed from S2 (slow humus) to S1 (microbial biomass)
          real :: efs2s3 = 0.            !< kg N ha-1 day-1 | N transformed from S2 (slow humus) to S3 (passive humus)
          real :: efs3s1 = 0.            !< kg N ha-1 day-1 | N transformed from S3 (passive humus) to S1 (microbial biomass)
          real :: imms1s2 = 0.           !< kg N ha-1 day-1 | N immobilisation resulting from transforming S1 (microbial biomass) to S2 (slow humus)
          real :: imms1s3 = 0.           !< kg N ha-1 day-1 | N immobilisation resulting from transforming S1 (microbial biomass) to S3 (passive humus)
          real :: imms2s1 = 0.           !< kg N ha-1 day-1 | N immobilisation resulting from transforming S2 (slow humus) to S1 (microbial biomass)
          real :: imms2s3 = 0.           !< kg N ha-1 day-1 | N immobilisation resulting from transforming S2 (slow humus) to S3 (passive humus)
          real :: imms3s1 = 0.           !< kg N ha-1 day-1 | N immobilisation resulting from transforming S3 (passive humus) to S1 (microbial biomass)
          real :: mnrs1s2 = 0.           !< kg N ha-1 day-1 | N mineralisation resulting from transforming S1 (microbial biomass) to S2 (slow humus)
          real :: mnrs1s3 = 0.           !< kg N ha-1 day-1 | N mineralisation resulting from transforming S1 (microbial biomass) to S3 (passive humus)
          real :: mnrs2s1 = 0.           !< kg N ha-1 day-1 | N mineralisation resulting from transforming S2 (slow humus) to S1 (microbial biomass)
          real :: mnrs2s3 = 0.           !< kg N ha-1 day-1 | N mineralisation resulting from transforming S2 (slow humus) to S3 (passive humus)
          real :: mnrs3s1 = 0.           !< kg N ha-1 day-1 | N mineralisation resulting from transforming S3 (passive humus) to S1 (microbial biomass)
          real :: co2fs1 = 0.            !< kg C ha-1 day-1 | CO2 production resulting from S1 (microbial biomass) transformations
          real :: co2fs2 = 0.            !< kg C ha-1 day-1 | CO2 production resulting from S2 (slow humus) transformations
          real :: co2fs3 = 0.            !< kg C ha-1 day-1 | CO2 production resulting from S3 (passive humus) transformations
      end type organic_flux
      type (organic_flux) :: org_flux        !< none | realised pool-to-pool fluxes for the layer currently being processed
      type (organic_flux) :: org_flux_zero   !< none | zeroed `organic_flux` used to reinitialise `org_flux`

      type carbon_soil_transformations
!< Soil carbon transformations aggregated over the soil profile for output.
!< This is the reporting counterpart of `organic_flux`: it carries only the
!< carbon terms, summed over layers, and is accumulated over time and space by
!< the overloaded `+`, `*`, and `/` operators defined below.
          real :: meta_micr = 0.        !< kg C ha-1 day-1 | C transformed from metabolic litter to S1 (microbial biomass)
          real :: str_micr = 0.         !< kg C ha-1 day-1 | C transformed from structural litter to S1 (microbial biomass)
          real :: str_hs = 0.           !< kg C ha-1 day-1 | C transformed from structural litter to S2 (slow humus)
          real :: co2_meta = 0.         !< kg C ha-1 day-1 | CO2 production resulting from metabolic litter transformations
          real :: co2_str = 0.          !< kg C ha-1 day-1 | CO2 production resulting from structural litter transformations
          real :: micr_hs = 0.          !< kg C ha-1 day-1 | C transformed from S1 (microbial biomass) to S2 (slow humus)
          real :: micr_hp = 0.          !< kg C ha-1 day-1 | C transformed from S1 (microbial biomass) to S3 (passive humus)
          real :: hs_micr = 0.          !< kg C ha-1 day-1 | C transformed from S2 (slow humus) to S1 (microbial biomass)
          real :: hs_hp = 0.            !< kg C ha-1 day-1 | C transformed from S2 (slow humus) to S3 (passive humus)
          real :: hp_micr = 0.          !< kg C ha-1 day-1 | C transformed from S3 (passive humus) to S1 (microbial biomass)
          real :: co2_micr = 0.         !< kg C ha-1 day-1 | CO2 production resulting from S1 (microbial biomass) transformations
          real :: co2_hs = 0.           !< kg C ha-1 day-1 | CO2 production resulting from S2 (slow humus) transformations
          real :: co2_hp = 0.           !< kg C ha-1 day-1 | CO2 production resulting from S3 (passive humus) transformations
      end type carbon_soil_transformations
      type (carbon_soil_transformations) :: hscfz   !< none | zeroed `carbon_soil_transformations` used to reinitialise the accumulators below

      !! hru soil carbon transformations
      type (carbon_soil_transformations), dimension (:), allocatable :: hscf_d   !< none | daily soil carbon transformations by HRU
      type (carbon_soil_transformations), dimension (:), allocatable :: hscf_m   !< none | monthly soil carbon transformations by HRU
      type (carbon_soil_transformations), dimension (:), allocatable :: hscf_y   !< none | yearly soil carbon transformations by HRU
      type (carbon_soil_transformations), dimension (:), allocatable :: hscf_a   !< none | average annual soil carbon transformations by HRU
      !! lsu soil carbon transformations
      type (carbon_soil_transformations), dimension (:), allocatable :: lscf_d   !< none | daily soil carbon transformations by landscape unit
      type (carbon_soil_transformations), dimension (:), allocatable :: lscf_m   !< none | monthly soil carbon transformations by landscape unit
      type (carbon_soil_transformations), dimension (:), allocatable :: lscf_y   !< none | yearly soil carbon transformations by landscape unit
      type (carbon_soil_transformations), dimension (:), allocatable :: lcsf_a   !< none | average annual soil carbon transformations by landscape unit
      !! basin soil carbon transformations
      type (carbon_soil_transformations) :: bscf_d   !< none | daily soil carbon transformations for the basin
      type (carbon_soil_transformations) :: bscf_m   !< none | monthly soil carbon transformations for the basin
      type (carbon_soil_transformations) :: bscf_y   !< none | yearly soil carbon transformations for the basin
      type (carbon_soil_transformations) :: bscf_a   !< none | average annual soil carbon transformations for the basin

      type carbon_soil_gain_losses
!< Carbon entering and leaving the soil profile: transport with water and
!< sediment, additions from residue decay and manure, and losses to
!< respiration and burning.
        real :: sed_c = 0.              !< kg C ha-1 | C transported with sediment yield
        real :: surq_c = 0.             !< kg C ha-1 | total dissolved C transported with surface runoff
        real :: surq_doc = 0.           !< kg C ha-1 | dissolved organic C transported with surface runoff
        real :: surq_dic = 0.           !< kg C ha-1 | dissolved inorganic C transported with surface runoff
        real :: latq_c = 0.             !< kg C ha-1 | total dissolved C transported with lateral flow, all layers
        real :: latq_doc= 0.            !< kg C ha-1 | dissolved organic C transported with lateral flow, all layers
        real :: latq_dic = 0.           !< kg C ha-1 | dissolved inorganic C transported with lateral flow, all layers
        real :: perc_c = 0.             !< kg C ha-1 | total dissolved C transported with percolate
        real :: perc_doc = 0.           !< kg C ha-1 | dissolved organic C transported with percolate
        real :: perc_dic = 0.           !< kg C ha-1 | dissolved inorganic C transported with percolate
        real :: rsd_decay_c = 0.        !< kg C ha-1 | C added to soil from residue decay
        real :: man_app_c = 0.          !< kg C ha-1 | C applied to soil from manure
        real :: man_graz_c = 0.         !< kg C ha-1 | C added to soil as manure from grazing animals
        real :: rsp_c = 0.              !< kg C ha-1 | CO2 production from soil respiration, summed over the profile
        real :: emit_c = 0.             !< kg C ha-1 | CO2 production from burning soil carbon
      end type carbon_soil_gain_losses
      type (carbon_soil_gain_losses) :: hscz   !< none | zeroed `carbon_soil_gain_losses` used to reinitialise the accumulators below

      !! hru soil carbon gains and losses
      type (carbon_soil_gain_losses), dimension (:), allocatable :: hsc_d   !< none | daily soil carbon gains and losses by HRU
      type (carbon_soil_gain_losses), dimension (:), allocatable :: hsc_m   !< none | monthly soil carbon gains and losses by HRU
      type (carbon_soil_gain_losses), dimension (:), allocatable :: hsc_y   !< none | yearly soil carbon gains and losses by HRU
      type (carbon_soil_gain_losses), dimension (:), allocatable :: hsc_a   !< none | average annual soil carbon gains and losses by HRU
      !! lsu soil carbon gains and losses
      type (carbon_soil_gain_losses), dimension (:), allocatable :: lsc_d   !< none | daily soil carbon gains and losses by landscape unit
      type (carbon_soil_gain_losses), dimension (:), allocatable :: lsc_m   !< none | monthly soil carbon gains and losses by landscape unit
      type (carbon_soil_gain_losses), dimension (:), allocatable :: lsc_y   !< none | yearly soil carbon gains and losses by landscape unit
      type (carbon_soil_gain_losses), dimension (:), allocatable :: lcs_a   !< none | average annual soil carbon gains and losses by landscape unit
      !! basin soil carbon gains and losses
      type (carbon_soil_gain_losses) :: bsc_d   !< none | daily soil carbon gains and losses for the basin
      type (carbon_soil_gain_losses) :: bsc_m   !< none | monthly soil carbon gains and losses for the basin
      type (carbon_soil_gain_losses) :: bsc_y   !< none | yearly soil carbon gains and losses for the basin
      type (carbon_soil_gain_losses) :: bsc_a   !< none | average annual soil carbon gains and losses for the basin

      type carbon_residue_gain_losses
!< Carbon entering and leaving the surface and below-ground residue pools.
        real :: plant_surf_c = 0.       !< kg C ha-1 | C added to surface residue from leaf drop and kill
        real :: plant_root_c = 0.       !< kg C ha-1 | C added to soil residue from root kill
        real :: rsd_surfdecay_c = 0.    !< kg C ha-1 | C lost from surface residue to soil by decay
        real :: rsd_rootdecay_c = 0.    !< kg C ha-1 | C lost from soil, root, and incorporated residue to soil by decay
        real :: harv_stov_c = 0.        !< kg C ha-1 | C removed during surface residue (stover) harvest
        real :: emit_c = 0.             !< kg C ha-1 | CO2 production from burning surface residue carbon
      end type carbon_residue_gain_losses
      type (carbon_residue_gain_losses) :: hrcz   !< none | zeroed `carbon_residue_gain_losses` used to reinitialise the accumulators below

      !! hru residue carbon gains and losses
      type (carbon_residue_gain_losses), dimension (:), allocatable :: hrc_d   !< none | daily residue carbon gains and losses by HRU
      type (carbon_residue_gain_losses), dimension (:), allocatable :: hrc_m   !< none | monthly residue carbon gains and losses by HRU
      type (carbon_residue_gain_losses), dimension (:), allocatable :: hrc_y   !< none | yearly residue carbon gains and losses by HRU
      type (carbon_residue_gain_losses), dimension (:), allocatable :: hrc_a   !< none | average annual residue carbon gains and losses by HRU
      !! lsu residue carbon gains and losses
      type (carbon_residue_gain_losses), dimension (:), allocatable :: lrc_d   !< none | daily residue carbon gains and losses by landscape unit
      type (carbon_residue_gain_losses), dimension (:), allocatable :: lrc_m   !< none | monthly residue carbon gains and losses by landscape unit
      type (carbon_residue_gain_losses), dimension (:), allocatable :: lrc_y   !< none | yearly residue carbon gains and losses by landscape unit
      type (carbon_residue_gain_losses), dimension (:), allocatable :: lrs_a   !< none | average annual residue carbon gains and losses by landscape unit
      !! basin residue carbon gains and losses
      type (carbon_residue_gain_losses) :: brc_d   !< none | daily residue carbon gains and losses for the basin
      type (carbon_residue_gain_losses) :: brc_m   !< none | monthly residue carbon gains and losses for the basin
      type (carbon_residue_gain_losses) :: brc_y   !< none | yearly residue carbon gains and losses for the basin
      type (carbon_residue_gain_losses) :: brc_a   !< none | average annual residue carbon gains and losses for the basin

      type carbon_plant_gain_losses
!< Carbon entering and leaving the live plant pool.
        real :: npp_c = 0.              !< kg C ha-1 | plant carbon growth from photosynthesis (net primary production)
        real :: harv_abgr_c = 0.        !< kg C ha-1 | C removed during grain or above-ground biomass harvest
        real :: harv_root_c = 0.        !< kg C ha-1 | C removed during tuber (root) harvest
        real :: drop_c = 0.             !< kg C ha-1 | C added to residue from leaf drop and kill
        real :: grazeat_c = 0.          !< kg C ha-1 | C eaten by animals during grazing
        real :: emit_c = 0.             !< kg C ha-1 | CO2 production from burning plant carbon
      end type carbon_plant_gain_losses
      type (carbon_plant_gain_losses) :: hpcz   !< none | zeroed `carbon_plant_gain_losses` used to reinitialise the accumulators below

      !! hru plant carbon gains and losses
      type (carbon_plant_gain_losses), dimension (:), allocatable :: hpc_d   !< none | daily plant carbon gains and losses by HRU
      type (carbon_plant_gain_losses), dimension (:), allocatable :: hpc_m   !< none | monthly plant carbon gains and losses by HRU
      type (carbon_plant_gain_losses), dimension (:), allocatable :: hpc_y   !< none | yearly plant carbon gains and losses by HRU
      type (carbon_plant_gain_losses), dimension (:), allocatable :: hpc_a   !< none | average annual plant carbon gains and losses by HRU
      !! lsu plant carbon gains and losses
      type (carbon_plant_gain_losses), dimension (:), allocatable :: lpc_d   !< none | daily plant carbon gains and losses by landscape unit
      type (carbon_plant_gain_losses), dimension (:), allocatable :: lpc_m   !< none | monthly plant carbon gains and losses by landscape unit
      type (carbon_plant_gain_losses), dimension (:), allocatable :: lpc_y   !< none | yearly plant carbon gains and losses by landscape unit
      type (carbon_plant_gain_losses), dimension (:), allocatable :: lps_a   !< none | average annual plant carbon gains and losses by landscape unit
      !! basin plant carbon gains and losses
      type (carbon_plant_gain_losses) :: bpc_d   !< none | daily plant carbon gains and losses for the basin
      type (carbon_plant_gain_losses) :: bpc_m   !< none | monthly plant carbon gains and losses for the basin
      type (carbon_plant_gain_losses) :: bpc_y   !< none | yearly plant carbon gains and losses for the basin
      type (carbon_plant_gain_losses) :: bpc_a   !< none | average annual plant carbon gains and losses for the basin

      interface operator (+)
!< Component-wise addition of two `carbon_soil_transformations` values, used to
!< accumulate daily values into monthly, yearly, and basin totals.
        module procedure carbon_soil_flux__add
      end interface

      interface operator (*)
!< Scaling of a `carbon_soil_transformations` value by a real constant, used to
!< area-weight HRU values when aggregating to landscape unit and basin.
        module procedure carbon_soil_flux_mult
      end interface

      interface operator (/)
!< Division of a `carbon_soil_transformations` value by a real constant, used to
!< convert accumulated totals into averages.
        module procedure carbon_soil_flux_div
      end interface

      interface operator (+)
!< Component-wise addition of two `carbon_soil_gain_losses` values.
        module procedure carbon_soil_gl__add
      end interface

      interface operator (*)
!< Scaling of a `carbon_soil_gain_losses` value by a real constant.
        module procedure carbon_soil_gl_mult
      end interface

      interface operator (/)
!< Division of a `carbon_soil_gain_losses` value by a real constant.
        module procedure carbon_soil_gl_div
      end interface

      interface operator (+)
!< Component-wise addition of two `carbon_residue_gain_losses` values.
        module procedure carbon_residue_gl__add
      end interface

      interface operator (*)
!< Scaling of a `carbon_residue_gain_losses` value by a real constant.
        module procedure carbon_residue_gl_mult
      end interface

      interface operator (/)
!< Division of a `carbon_residue_gain_losses` value by a real constant.
        module procedure carbon_residue_gl_div
      end interface

      interface operator (+)
!< Component-wise addition of two `carbon_plant_gain_losses` values.
        module procedure carbon_plant_gl__add
      end interface

      interface operator (*)
!< Scaling of a `carbon_plant_gain_losses` value by a real constant.
        module procedure carbon_plant_gl_mult
      end interface

      interface operator (/)
!< Division of a `carbon_plant_gain_losses` value by a real constant.
        module procedure carbon_plant_gl_div
      end interface

      contains

      function carbon_soil_flux__add (hru1, hru2) result (hru3)
!< summary: Adds two soil carbon transformation records component by component.
!<
!< **Purpose:** implements `+` for `carbon_soil_transformations` so that output
!< routines can accumulate daily soil carbon transformations into monthly,
!< yearly, and average-annual totals, and roll HRU values up to landscape unit
!< and basin scale, with ordinary arithmetic.
        type (carbon_soil_transformations), intent (in) :: hru1   !< kg C ha-1 day-1 | left operand
        type (carbon_soil_transformations), intent (in) :: hru2   !< kg C ha-1 day-1 | right operand
        type (carbon_soil_transformations) :: hru3                !< kg C ha-1 day-1 | component-wise sum of `hru1` and `hru2`
        hru3%meta_micr = hru1%meta_micr + hru2%meta_micr
        hru3%str_micr = hru1%str_micr + hru2%str_micr
        hru3%str_hs = hru1%str_hs + hru2%str_hs
        hru3%co2_meta = hru1%co2_meta + hru2%co2_meta
        hru3%co2_str = hru1%co2_str + hru2%co2_str
        hru3%micr_hs = hru1%micr_hs + hru2%micr_hs
        hru3%micr_hp = hru1%micr_hp + hru2%micr_hp
        hru3%hs_micr = hru1%hs_micr + hru2%hs_micr
        hru3%hs_hp = hru1%hs_hp + hru2%hs_hp
        hru3%hp_micr = hru1%hp_micr + hru2%hp_micr
        hru3%co2_micr = hru1%co2_micr + hru2%co2_micr
        hru3%co2_hs = hru1%co2_hs + hru2%co2_hs
        hru3%co2_hp = hru1%co2_hp + hru2%co2_hp
       end function carbon_soil_flux__add

      function carbon_soil_flux_mult (hru1,const) result (hru2)
!< summary: Scales a soil carbon transformation record by a constant.
!<
!< **Purpose:** implements `*` for `carbon_soil_transformations`, used mainly to
!< area-weight an HRU record before summing it into a landscape unit or basin
!< record.
        type (carbon_soil_transformations), intent (in) :: hru1   !< kg C ha-1 day-1 | record to be scaled
        real, intent (in) :: const                                !< none | scaling factor, typically an area fraction
        type (carbon_soil_transformations) :: hru2                !< kg C ha-1 day-1 | `hru1` with every component multiplied by `const`
        hru2%meta_micr = hru1%meta_micr * const
        hru2%str_micr = hru1%str_micr * const
        hru2%str_hs = hru1%str_hs * const
        hru2%co2_meta = hru1%co2_meta * const
        hru2%co2_str = hru1%co2_str * const
        hru2%micr_hs = hru1%micr_hs * const
        hru2%micr_hp = hru1%micr_hp * const
        hru2%hs_micr = hru1%hs_micr * const
        hru2%hs_hp = hru1%hs_hp * const
        hru2%hp_micr = hru1%hp_micr * const
        hru2%co2_micr = hru1%co2_micr * const
        hru2%co2_hs = hru1%co2_hs * const
        hru2%co2_hp = hru1%co2_hp * const
      end function carbon_soil_flux_mult

      function carbon_soil_flux_div (hru1,const) result (hru2)
!< summary: Divides a soil carbon transformation record by a constant.
!<
!< **Purpose:** implements `/` for `carbon_soil_transformations`, used to turn an
!< accumulated total into an average, for example dividing a multi-year sum by
!< the number of simulation years.
        type (carbon_soil_transformations), intent (in) :: hru1   !< kg C ha-1 day-1 | record to be divided
        real, intent (in) :: const                                !< none | divisor, typically a count of days or years
        type (carbon_soil_transformations) :: hru2                !< kg C ha-1 day-1 | `hru1` with every component divided by `const`
        hru2%meta_micr = hru1%meta_micr / const
        hru2%str_micr = hru1%str_micr / const
        hru2%str_hs = hru1%str_hs / const
        hru2%co2_meta = hru1%co2_meta / const
        hru2%co2_str = hru1%co2_str / const
        hru2%micr_hs = hru1%micr_hs / const
        hru2%micr_hp = hru1%micr_hp / const
        hru2%hs_micr = hru1%hs_micr / const
        hru2%hs_hp = hru1%hs_hp / const
        hru2%hp_micr = hru1%hp_micr / const
        hru2%co2_micr = hru1%co2_micr / const
        hru2%co2_hs = hru1%co2_hs / const
        hru2%co2_hp = hru1%co2_hp / const
      end function carbon_soil_flux_div

      function carbon_soil_gl__add (hru1, hru2) result (hru3)
!< summary: Adds two soil carbon gain/loss records component by component.
!<
!< **Purpose:** implements `+` for `carbon_soil_gain_losses` so that soil carbon
!< inputs and exports can be accumulated over time and aggregated over space.
        type (carbon_soil_gain_losses), intent (in) :: hru1   !< kg C ha-1 | left operand
        type (carbon_soil_gain_losses), intent (in) :: hru2   !< kg C ha-1 | right operand
        type (carbon_soil_gain_losses) :: hru3                !< kg C ha-1 | component-wise sum of `hru1` and `hru2`
        hru3%sed_c = hru1%sed_c + hru2%sed_c
        hru3%surq_c = hru1%surq_c + hru2%surq_c
        hru3%surq_doc = hru1%surq_doc + hru2%surq_doc
        hru3%surq_dic = hru1%surq_dic + hru2%surq_dic
        hru3%latq_c = hru1%latq_c + hru2%latq_c
        hru3%latq_doc = hru1%latq_doc + hru2%latq_doc
        hru3%latq_dic = hru1%latq_dic + hru2%latq_dic
        hru3%perc_c = hru1%perc_c + hru2%perc_c
        hru3%perc_doc = hru1%perc_doc + hru2%perc_doc
        hru3%perc_dic = hru1%perc_dic + hru2%perc_dic
        hru3%rsd_decay_c = hru1%rsd_decay_c + hru2%rsd_decay_c
        hru3%man_app_c = hru1%man_app_c + hru2%man_app_c
        hru3%man_graz_c = hru1%man_graz_c + hru2%man_graz_c
        hru3%rsp_c = hru1%rsp_c + hru2%rsp_c
        hru3%emit_c = hru1%emit_c + hru2%emit_c
       end function carbon_soil_gl__add

      function carbon_soil_gl_mult (hru1,const) result (hru2)
!< summary: Scales a soil carbon gain/loss record by a constant.
!<
!< **Purpose:** implements `*` for `carbon_soil_gain_losses`, used mainly to
!< area-weight an HRU record before aggregating it upward.
        type (carbon_soil_gain_losses), intent (in) :: hru1   !< kg C ha-1 | record to be scaled
        real, intent (in) :: const                            !< none | scaling factor, typically an area fraction
        type (carbon_soil_gain_losses) :: hru2                !< kg C ha-1 | `hru1` with every component multiplied by `const`
        hru2%sed_c = hru1%sed_c * const
        hru2%surq_c = hru1%surq_c * const
        hru2%surq_doc = hru1%surq_doc * const
        hru2%surq_dic = hru1%surq_dic * const
        hru2%latq_c = hru1%latq_c * const
        hru2%latq_doc = hru1%latq_doc * const
        hru2%latq_dic = hru1%latq_dic * const
        hru2%perc_c = hru1%perc_c * const
        hru2%perc_doc = hru1%perc_doc * const
        hru2%perc_dic = hru1%perc_dic * const
        hru2%rsd_decay_c = hru1%rsd_decay_c * const
        hru2%man_app_c = hru1%man_app_c * const
        hru2%man_graz_c = hru1%man_graz_c * const
        hru2%rsp_c = hru1%rsp_c * const
        hru2%emit_c = hru1%emit_c * const
      end function carbon_soil_gl_mult

      function carbon_soil_gl_div (hru1,const) result (hru2)
!< summary: Divides a soil carbon gain/loss record by a constant.
!<
!< **Purpose:** implements `/` for `carbon_soil_gain_losses`, used to turn an
!< accumulated total into an average.
        type (carbon_soil_gain_losses), intent (in) :: hru1   !< kg C ha-1 | record to be divided
        real, intent (in) :: const                            !< none | divisor, typically a count of days or years
        type (carbon_soil_gain_losses) :: hru2                !< kg C ha-1 | `hru1` with every component divided by `const`
        hru2%sed_c = hru1%sed_c / const
        hru2%surq_c = hru1%surq_c / const
        hru2%surq_doc = hru1%surq_doc / const
        hru2%surq_dic = hru1%surq_dic / const
        hru2%latq_c = hru1%latq_c / const
        hru2%latq_doc = hru1%latq_doc / const
        hru2%latq_dic = hru1%latq_dic / const
        hru2%perc_c = hru1%perc_c / const
        hru2%perc_doc = hru1%perc_doc / const
        hru2%perc_dic = hru1%perc_dic / const
        hru2%rsd_decay_c = hru1%rsd_decay_c / const
        hru2%man_app_c = hru1%man_app_c / const
        hru2%man_graz_c = hru1%man_graz_c / const
        hru2%rsp_c = hru1%rsp_c / const
        hru2%emit_c = hru1%emit_c / const
      end function carbon_soil_gl_div

      function carbon_residue_gl__add (hru1, hru2) result (hru3)
!< summary: Adds two residue carbon gain/loss records component by component.
!<
!< **Purpose:** implements `+` for `carbon_residue_gain_losses` so that residue
!< carbon inputs and losses can be accumulated over time and aggregated over
!< space.
        type (carbon_residue_gain_losses), intent (in) :: hru1   !< kg C ha-1 | left operand
        type (carbon_residue_gain_losses), intent (in) :: hru2   !< kg C ha-1 | right operand
        type (carbon_residue_gain_losses) :: hru3                !< kg C ha-1 | component-wise sum of `hru1` and `hru2`
        hru3%plant_surf_c = hru1%plant_surf_c + hru2%plant_surf_c
        hru3%plant_root_c = hru1%plant_root_c + hru2%plant_root_c
        hru3%rsd_surfdecay_c = hru1%rsd_surfdecay_c + hru2%rsd_surfdecay_c
        hru3%rsd_rootdecay_c = hru1%rsd_rootdecay_c + hru2%rsd_rootdecay_c
        hru3%harv_stov_c = hru1%harv_stov_c + hru2%harv_stov_c
        hru3%emit_c = hru1%emit_c + hru2%emit_c
       end function carbon_residue_gl__add

      function carbon_residue_gl_mult (hru1,const) result (hru2)
!< summary: Scales a residue carbon gain/loss record by a constant.
!<
!< **Purpose:** implements `*` for `carbon_residue_gain_losses`, used mainly to
!< area-weight an HRU record before aggregating it upward.
        type (carbon_residue_gain_losses), intent (in) :: hru1   !< kg C ha-1 | record to be scaled
        real, intent (in) :: const                               !< none | scaling factor, typically an area fraction
        type (carbon_residue_gain_losses) :: hru2                !< kg C ha-1 | `hru1` with every component multiplied by `const`
        hru2%plant_surf_c = hru1%plant_surf_c * const
        hru2%plant_root_c = hru1%plant_root_c * const
        hru2%rsd_surfdecay_c = hru1%rsd_surfdecay_c * const
        hru2%rsd_rootdecay_c = hru1%rsd_rootdecay_c * const
        hru2%harv_stov_c = hru1%harv_stov_c * const
        hru2%emit_c = hru1%emit_c * const
      end function carbon_residue_gl_mult

      function carbon_residue_gl_div (hru1,const) result (hru2)
!< summary: Divides a residue carbon gain/loss record by a constant.
!<
!< **Purpose:** implements `/` for `carbon_residue_gain_losses`, used to turn an
!< accumulated total into an average.
!<
!< @bug Two defects are present in the body of this function and are preserved
!< here only as documentation of current behaviour: `plant_root_c` is never
!< divided (`plant_surf_c` is assigned twice instead), and `emit_c` is
!< multiplied by `const` rather than divided by it.
        real, intent (in) :: const                               !< none | divisor, typically a count of days or years
        type (carbon_residue_gain_losses), intent (in) :: hru1   !< kg C ha-1 | record to be divided
        type (carbon_residue_gain_losses) :: hru2                !< kg C ha-1 | `hru1` with every component divided by `const`
        hru2%plant_surf_c = hru1%plant_surf_c / const
        hru2%plant_surf_c = hru1%plant_surf_c / const
        hru2%rsd_surfdecay_c = hru1%rsd_surfdecay_c / const
        hru2%rsd_rootdecay_c = hru1%rsd_rootdecay_c / const
        hru2%harv_stov_c = hru1%harv_stov_c / const
        hru2%emit_c = hru1%emit_c * const
      end function carbon_residue_gl_div

      function carbon_plant_gl__add (hru1, hru2) result (hru3)
!< summary: Adds two plant carbon gain/loss records component by component.
!<
!< **Purpose:** implements `+` for `carbon_plant_gain_losses` so that plant
!< carbon growth and removals can be accumulated over time and aggregated over
!< space.
        type (carbon_plant_gain_losses), intent (in) :: hru1   !< kg C ha-1 | left operand
        type (carbon_plant_gain_losses), intent (in) :: hru2   !< kg C ha-1 | right operand
        type (carbon_plant_gain_losses) :: hru3                !< kg C ha-1 | component-wise sum of `hru1` and `hru2`
        hru3%npp_c = hru1%npp_c + hru2%npp_c
        hru3%harv_abgr_c = hru1%harv_abgr_c + hru2%harv_abgr_c
        hru3%harv_root_c = hru1%harv_root_c + hru2%harv_root_c
        hru3%drop_c = hru1%drop_c + hru2%drop_c
        hru3%grazeat_c = hru1%grazeat_c + hru2%grazeat_c
        hru3%emit_c = hru1%emit_c + hru2%emit_c
       end function carbon_plant_gl__add

      function carbon_plant_gl_mult (hru1,const) result (hru2)
!< summary: Scales a plant carbon gain/loss record by a constant.
!<
!< **Purpose:** implements `*` for `carbon_plant_gain_losses`, used mainly to
!< area-weight an HRU record before aggregating it upward.
        type (carbon_plant_gain_losses), intent (in) :: hru1   !< kg C ha-1 | record to be scaled
        real, intent (in) :: const                             !< none | scaling factor, typically an area fraction
        type (carbon_plant_gain_losses) :: hru2                !< kg C ha-1 | `hru1` with every component multiplied by `const`
        hru2%npp_c = hru1%npp_c * const
        hru2%harv_abgr_c = hru1%harv_abgr_c * const
        hru2%harv_root_c = hru1%harv_root_c * const
        hru2%drop_c = hru1%drop_c * const
        hru2%grazeat_c = hru1%grazeat_c * const
        hru2%emit_c = hru1%emit_c * const
      end function carbon_plant_gl_mult

      function carbon_plant_gl_div (hru1,const) result (hru2)
!< summary: Divides a plant carbon gain/loss record by a constant.
!<
!< **Purpose:** implements `/` for `carbon_plant_gain_losses`, used to turn an
!< accumulated total into an average.
        type (carbon_plant_gain_losses), intent (in) :: hru1   !< kg C ha-1 | record to be divided
        real, intent (in) :: const                             !< none | divisor, typically a count of days or years
        type (carbon_plant_gain_losses) :: hru2                !< kg C ha-1 | `hru1` with every component divided by `const`
        hru2%npp_c = hru1%npp_c / const
        hru2%harv_abgr_c = hru1%harv_abgr_c / const
        hru2%harv_root_c = hru1%harv_root_c / const
        hru2%drop_c = hru1%drop_c / const
        hru2%grazeat_c = hru1%grazeat_c / const
        hru2%emit_c = hru1%emit_c / const
      end function carbon_plant_gl_div

     end module carbon_module
