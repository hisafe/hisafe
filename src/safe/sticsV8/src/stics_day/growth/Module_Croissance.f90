!> Module of growth
!! Description: growth, senescence, grain filling, ...)
!<
module Module_Croissance
    implicit none

interface
!> This module calculates the shoot biomass growth thanks to the radiation intercepted by foliage.
!> - Stics book paragraphe 3.3, page 54-57
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 3.3, page 54-57
!>
!! This module calculates the shoot biomass growth thanks to the radiation intercepted by foliage.
!!
!! The linear relationship between accumulated biomass in the plant and radiation intercepted by foliage, demonstrated by Monteith (1972), defines the
!! radiation use efficiency (or RUE ) as the slope of this relationship. synthesizes the processes of photosynthesis and respiration.
!! Its calculation (ratio between above-ground biomass and absorbed radiation) implies that this parameter also takes account of a carbon allocation coefficient
!! between above-ground and below-ground parts of the plant.  Obviously, because of underlying physiological processes this ratio varies somewhat, due to stresses,
!! temperature and phenology.  To take account of these effects, Sinclair (1986) proposed that RUE should be considered as a physiological function,
!! to which stress indices should be applied.
!! In STICS the calculation of the daily production of shoot biomass (dltams) relies on the RUE concept (though the relationship between dltams and raint
!! is slightly parabolic) taking into account various factors known to influence the elementary photosynthesis and respiration processes,
!! mostly as stresses (ftemp, swfac, inns and exobiom).  Dltams accumulated day by day gives the shoot biomass of the canopy, masec.
!!
!! Influence of radiation and phasic development:
!!
!! the accumulation of shoot biomass depends on the intercepted radiation (raint), and is almost linear but slightly asymptotic at high intercepted light values.
!! It is simulated in STICS by a parabolic function involving a maximum radiation use efficiency specific to each species, ebmax.
!! The parameter coefb stands for the radiation saturating effect. This effect is the result, even buffered, of the saturation occurring within a short time step
!! on the individual leaf scale and is easily observed when daily calculations are made with instantaneous formulae of canopy photosynthesis ;
!! such calculations lead to a value of 0.0815. The efficiency, ebmax, may differ during the juvenile (ILEV-IAMF), vegetative (IAMF-IDRP) and
!! reproductive (IDRP-IMAT) phases (corresponding respectively to the parameters efcroijuv, efcroiveg and efcroirepro).
!! Classically, efcroijuv=1/2 efcroiveg is used to take account of the preferential migration of assimilates towards the roots at the beginning of the cycle.
!! The difference between efcroiveg and efcroirepro arises from the biochemical composition of storage organs: e.g. for oil or protein crops efcroirepro is less
!! than efcroiveg because the respiratory cost to make oil and protein is higher than for glucose.
!!
!! Effect of atmospheric CO2 concentration:
!!
!> The CO2C parameter stands for the atmospheric CO2 concentration, which can be higher than the current value, assumed to be 350 ppm. The formalisation
!! chosen in STICS was adapted from Stockle et al. (1992): the effect of CO2C on the radiation use efficiency is expressed by an exponential relationship,
!! for which the parameter is calculated so that the curve passes through the point (600, alphaco2).
!!
!! The remobilisation of reserves:
!>   - Perennial reserve available from one cycle to the next: dltaremobil is obtained by the remobilisation of winter reserves in perennial plants.
!!   Each day the maximal proportion of the reserves that can be remobilised is remobres, until perennial stocks (parameter resperenne0 given as an initialisation
!!   at the beginning of the growth cycle) are exhausted.
!!   resperenne0 only represents carbon reserves, and nitrogen reserves can only be added through initiation of the QNplante0 parameter.
!!   The nitrogen remobilisation rate of the QNplante0 stock is assumed to equal the nitrogen demand until it is exhausted.  These reserves are only called upon
!!   if the newly formed assimilates (dltams) fail to satisfy the sink strength (fpv and fpft), which leads to a first calculation of
!!   the source/sinks variable (sourcepuits). These remobilisations contribute to increasing the source/sink ratio the following day because they are counted
!!   in the variable dltams.
!>   - Reserve built up and used during the cycle: reserves built up during the vegetative cycle (variable resperenne) and reused later on simply contribute
!!   to the estimation of the source/sink ratio for indeterminate crops.  The maximum quantity which can be remobilised per day (remobilj) is calculated
!!   similarly to dltaremobil. If the plant is both perennial and indeterminate, the reserves originating from the previous cycle are first used (dltaremobil)
!!   and when exhausted the current cycle’s reserves (remobilj) can be used.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine biomaer(n,ipl,ens,nlev,P_codeperenne,nplt,P_codehypo,P_codegermin,P_masecplantule,   &
                   P_adil,namf,P_adilmax,nrec,P_codcueille,P_codefauche,P_codelaitr,P_codetransrad, &
                   P_efcroijuv,P_efcroiveg,ndrp,P_efcroirepro,chargefruit,P_coefb,tcult,P_teopt,    &
                   P_teoptbis,P_temin,P_temax,P_codeclichange,P_alphaco2,co2,P_resplmax,densite,    &
                   P_codeh2oact,swfac,exobiom,P_codeinnact,dltaremobil,P_codeindetermin,fpv,        &
                   fpft,P_remobres,P_resperenne0,demande,P_QNplante0,P_msresiduel,P_nbcueille,      &
                   rdtint,CNgrain,P_codemontaison,nmontaison,                                       &
                   masec_veille,masec,QNplante_veille,QNplante,inns,inn,innlai,                     &
                   cumdltaremobilN,ebmax,ftemp,epsib,fco2,dltams,dltamsen,dltamstombe,              &
                   resperenne,dltamsN,photnet,sourcepuits,dltaremobilN,remobilj,                    &
                   cumdltares,magrain_veille,magrain,masecneo,surface,surfaceSous,                  &
                   P_nbplantes,P_extin,cumrg,cumraint,fapar,delta,P_adfol,lairognecum,              &
                   laieffcum,P_dfolbas,P_dfolhaut,dfol,rdif,parapluie,raint,P_parsurrg,P_forme,     &
                   lai,laisen,eai,P_interrang,nlax,nsen,P_codlainet,P_hautbase,P_codepalissage,     &
                   P_hautmaxtec,P_largtec,originehaut,hauteur,deltahauteur,P_hautmax,               &
                   varrapforme,largeur,jul,trg,P_latitude,rombre,rsoleil,P_orientrang,              &
                   P_ktrou,tauxcouv,P_khaut,surfAO,surfAS,fpari)

          implicit none

          integer, intent(IN)    :: ipl  
          integer, intent(IN)    :: ens  
          integer, intent(IN)    :: nlev  
          integer, intent(IN)    :: P_codeperenne  !< // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0
          integer, intent(IN)    :: nplt  
          integer, intent(IN)    :: n  
          integer, intent(IN)    :: P_codehypo  !< // PARAMETER // option of simulation of a  phase of hypocotyl growth (1) or planting of plantlets (2) // code 1/2 // PARPLT // 0
          integer, intent(IN)    :: P_codegermin  !< // PARAMETER // option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2) // code 1/2 // PARPLT // 0
          real,    intent(IN)    :: P_masecplantule  !< // PARAMETER // initial shoot biomass of plantlet // t ha-1 // PARPLT // 1
          real,    intent(IN)    :: P_adil  !< // PARAMETER // Parameter of the critical curve of nitrogen needs [Nplante]=P_adil MS^(-P_bdil) // N% MS // PARPLT // 1
          integer, intent(IN)    :: namf  
          real,    intent(IN)    :: P_adilmax  !< // PARAMETER // Parameter of the maximum curve of nitrogen needs [Nplante]=P_adilmax MS^(-P_bdilmax) // N% MS // PARPLT // 1
          integer, intent(IN)    :: nrec  
          integer, intent(IN)    :: P_codcueille  !< // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0
          integer, intent(IN)    :: P_codefauche  !< // PARAMETER // option of cut modes for forage crops: yes (1), no (2) // code 1/2 // PARTEC // 0
          integer, intent(IN)    :: P_codelaitr  !< // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0
          integer, intent(INOUT) :: P_codetransrad  !< // PARAMETER // simulation option of radiation 'interception: law Beer (1), radiation transfers (2) // code 1/2 // PARPLT // 0
          real,    intent(IN)    :: P_efcroijuv  !< // PARAMETER // Maximum radiation use efficiency during the juvenile phase(LEV-AMF) // g MJ-1 // PARPLT // 1
          real,    intent(IN)    :: P_efcroiveg  !< // PARAMETER // Maximum radiation use efficiency during the vegetative stage (AMF-DRP) // g MJ-1 // PARPLT // 1
          integer, intent(IN)    :: ndrp  
          real,    intent(IN)    :: P_efcroirepro  !< // PARAMETER // Maximum radiation use efficiency during the grain filling phase (DRP-MAT) // g MJ-1 // PARPLT // 1
          real,    intent(IN)    :: chargefruit   !< // OUTPUT // Amount of filling fruits per plant // nb fruits.plant-1
          real,    intent(IN)    :: P_coefb  !< // PARAMETER // parameter defining radiation effect on  conversion efficiency // SD // PARAM // 1
          real,    intent(IN)    :: tcult   !< // OUTPUT // Crop surface temperature (daily average) // degree C
          real,    intent(IN)    :: P_teopt  !< // PARAMETER // Optimal temperature for the biomass growth // degree C // PARPLT // 1
          real,    intent(IN)    :: P_teoptbis  !< // PARAMETER // optimal temperature for the biomass growth (if there is a plateau between P_teopt and P_teoptbis) // degree C // PARPLT // 1
          real,    intent(IN)    :: P_temin  !< // PARAMETER // Minimum threshold temperature for development // degree C // PARPLT // 1
          real,    intent(IN)    :: P_temax  !< // PARAMETER // Maximal threshold temperature for the biomass growth  // degree C // PARPLT // 1
          integer, intent(IN)    :: P_codeclichange  !< // PARAMETER // option for climatel change : yes (2), no (1)  // code 1/2 // STATION // 0
          real,    intent(IN)    :: P_alphaco2  !< // PARAMETER // coefficient allowing the modification of radiation use efficiency in case of  atmospheric CO2 increase // SD // PARPLT // 1
      ! dr 23/09/2014 co2 passe en reel pour eviter pb de divison de 2 entiers
      !    integer, intent(IN)    :: co2
          real,    intent(IN)    :: co2
          real,    intent(IN)    :: P_resplmax  !< // PARAMETER // maximal reserve biomass // t ha-1 // PARAMV6 // 1
          real,    intent(IN)    :: densite   !< // OUTPUT // Actual sowing density // plants.m-2
          integer, intent(IN)    :: P_codeh2oact  !< // PARAMETER // code to activate  water stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
          real,    intent(IN)    :: swfac   !< // OUTPUT // Index of stomatic water stress  // 0-1
          real,    intent(IN)    :: exobiom   !< // OUTPUT // Index of excess water active on surface growth // 0-1
          integer, intent(IN)    :: P_codeinnact  !< // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
          integer, intent(IN)    :: P_codeindetermin  !< // PARAMETER // option of  simulation of the leaf growth and fruit growth : indeterminate (2) or determinate (1) // code 1/2 // PARPLT // 0
          real,    intent(IN)    :: fpv   !< // OUTPUT // Sink strength of developing leaves // g.j-1.m-2
          real,    intent(IN)    :: fpft   !< // OUTPUT // Sink strength of fruits  // g.m-2 j-1
          real,    intent(IN)    :: P_remobres  !< // PARAMETER // proportion of daily remobilisable carbon reserve // SD // PARPLT // 1
          real,    intent(IN)    :: P_resperenne0  !< // PARAMETER // initial reserve biomass // t ha-1 // INIT // 1
          real,    intent(IN)    :: demande   !< // OUTPUT // Daily nitrogen need of the plant   // kgN.ha-1.j-1
          real,    intent(IN)    :: P_QNplante0  !< // PARAMETER // initial nitrogen amount in the plant // kg ha-1 // INIT // 1
          real,    intent(IN)    :: P_msresiduel  !< // PARAMETER // Residual dry matter after a cut // t ha-1 // PARTEC // 1
          integer, intent(IN)    :: P_nbcueille  !< // PARAMETER // number of fruit harvestings // code 1/2 // PARTEC // 0
          real,    intent(IN)    :: rdtint  
          real,    intent(IN)    :: CNgrain   !< // OUTPUT // Nitrogen concentration of grains  // %
          integer, intent(IN)    :: P_codemontaison  !< // PARAMETER // code to stop the reserve limitation from the stem elongation // code 1/2 // PARAMV6 // 0
          integer, intent(IN)    :: nmontaison  

        ! (IN)OUT
          real,    intent(INOUT) :: masec_veille    ! masec_veille => valeur précédente (valeur de la veille par ex.)  
          real,    intent(INOUT) :: masec           ! masec => valeur actuelle (valeur du jour courant par ex.)    // OUTPUT // Aboveground dry matter  // t.ha-1
          real,    intent(INOUT) :: QNplante_veille ! QNplante_veille => valeur précédente (valeur de la veille par ex.)  
          real,    intent(INOUT) :: QNplante        ! QNplante => valeur actuelle (valeur du jour courant par ex.)    // OUTPUT // Amount of nitrogen taken up by the plant  // kgN.ha-1
          real,    intent(INOUT) :: inns   !< // OUTPUT // Index of nitrogen stress active on growth in biomass // P_innmin to 1
          real,    intent(INOUT) :: inn   !< // OUTPUT // Nitrogen nutrition index (satisfaction of nitrogen needs ) // 0-1
          real,    intent(INOUT) :: innlai   !< // OUTPUT // Index of nitrogen stress active on leaf growth // P_innmin to 1
          real,    intent(INOUT) :: cumdltaremobilN  
          real,    intent(INOUT) :: ebmax  
          real,    intent(INOUT) :: ftemp   !< // OUTPUT // Temperature-related EPSIBMAX reduction factor // 0-1
          real,    intent(INOUT) :: epsib   !< // OUTPUT // Radiation use efficiency  // t ha-1.Mj-1. m-2
          real,    intent(INOUT) :: fco2  
          real,    intent(INOUT) :: dltams   !< // OUTPUT // Growth rate of the plant  // t ha-1.j-1
          real,    intent(INOUT) :: dltamsen   !< // OUTPUT // Senescence rate // t ha-1 j-1
          real,    intent(INOUT) :: dltamstombe  
          real,    intent(INOUT) :: resperenne   !< // OUTPUT // C crop reserve, during the cropping season, or during the intercrop period (for perenial crops) // t ha-1
          real,    intent(INOUT) :: dltamsN  
          real,    intent(INOUT) :: photnet   !< // OUTPUT // net photosynthesis // t ha-1.j-1
          real,    intent(INOUT) :: sourcepuits   !< // OUTPUT // Pool/sink ratio // 0-1
          real,    intent(INOUT) :: dltaremobil   !< // OUTPUT // Amount of perennial reserves remobilised // g.m-2.j-1
          real,    intent(INOUT) :: dltaremobilN  
          real,    intent(INOUT) :: remobilj   !< // OUTPUT // Amount of biomass remobilized on a daily basis for the fruits  // g.m-2 j-1
          real,    intent(INOUT) :: cumdltares  
          real,    intent(INOUT) :: magrain_veille  ! magrain_veille => valeur précédente (valeur de la veille par ex.)  
          real,    intent(INOUT) :: magrain         ! magrain => valeur actuelle (valeur du jour courant par ex.)  
          real,    intent(INOUT) :: masecneo   !< // OUTPUT // Newly-formed dry matter  // t.ha-1


        ! RAYTRANS
        !  integer, intent(IN)    :: P_codetransrad
          real,    intent(IN)    :: surface(2)  
          real,    intent(OUT)   :: surfaceSous(2)  
          integer, intent(IN)    :: P_nbplantes  !< // PARAMETER // number of simulated plants // SD // P_USM/USMXML // 0
          real,    intent(INOUT) :: P_extin(P_nbplantes)  !< // PARAMETER // extinction coefficient of photosynthetic active radiation canopy // SD // PARPLT // 1
          real,    intent(INOUT) :: cumrg   !< // OUTPUT // Sum of global radiation during the stage sowing-harvest   // Mj.m-2
          real,    intent(INOUT) :: cumraint   !< // OUTPUT // Sum of intercepted radiation  // Mj.m-2
          real,    intent(OUT)   :: fapar   !< // OUTPUT // Proportion of radiation intercepted // 0-1
          real,    intent(OUT)   :: delta  
        ! -- FIN RAYTRANS

        ! TRANSRAD
          real,    intent(IN)    :: P_adfol  !< // PARAMETER // parameter determining the leaf density evolution within the chosen shape // m-1 // PARPLT // 1
          real,    intent(IN)    :: lairognecum  
          real,    intent(IN)    :: laieffcum  
          real,    intent(IN)    :: P_dfolbas  !< // PARAMETER // minimal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1
          real,    intent(IN)    :: P_dfolhaut  !< // PARAMETER // maximal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1
          real,    intent(OUT)   :: dfol   !< // OUTPUT //  "Within the shape  leaf density" // m2 m-3
          real,    intent(OUT)   :: rdif   !< // OUTPUT // Ratio between diffuse radiation and global radiation  // 0-1
          integer, intent(OUT)   :: parapluie  
          real,    intent(OUT)   :: raint   !< // OUTPUT // Photosynthetic active radiation intercepted by the canopy  // Mj.m-2
          real,    intent(IN)    :: P_parsurrg  !< // PARAMETER // coefficient PAR/RG for the calculation of PAR  // * // STATION // 1
          integer, intent(IN)    :: P_forme  !< // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0
          real,    intent(IN)    :: lai   !< // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
          real,    intent(IN)    :: laisen   !< // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
          real,    intent(IN)    :: eai  
          real,    intent(IN)    :: P_interrang  !< // PARAMETER // Width of the P_interrang // m // PARTEC // 1
          integer, intent(IN)    :: nlax  
          integer, intent(IN)    :: nsen  
          integer, intent(IN)    :: P_codlainet  !< // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
          real,    intent(IN)    :: P_hautbase  !< // PARAMETER // Base height of crop // m // PARPLT // 1
          integer, intent(IN)    :: P_codepalissage  !< // PARAMETER // option: no (1),  yes2) // code 1/2 // PARTEC // 0
          real,    intent(IN)    :: P_hautmaxtec  !< // PARAMETER // maximal height of the plant allowed by the management // m // PARTEC // 1
          real,    intent(IN)    :: P_largtec  !< // PARAMETER // technical width // m // PARTEC // 1
          real,    intent(IN)    :: originehaut  
          real,    intent(INOUT) :: hauteur   !< // OUTPUT // Height of canopy // m
          real,    intent(INOUT) :: deltahauteur  
          real,    intent(INOUT) :: P_hautmax  !< // PARAMETER // Maximum height of crop // m // PARPLT // 1
          real,    intent(INOUT) :: varrapforme  
          real,    intent(OUT)   :: largeur   !< // OUTPUT // Width of the plant shape  // m
          integer, intent(IN)    :: jul  
          real,    intent(IN)    :: trg   !< // OUTPUT // Active radiation (entered or calculated) // MJ.m-2
          real,    intent(IN)    :: P_latitude  !< // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0
          real,    intent(OUT)   :: rombre   !< // OUTPUT // Radiation fraction in the shade // 0-1
          real,    intent(OUT)   :: rsoleil   !< // OUTPUT // Radiation fraction in the full sun // 0-1
          real,    intent(IN)    :: P_orientrang  !< // PARAMETER // Direction of ranks // rd (0=NS) // PARTEC // 1
          real,    intent(IN)    :: P_ktrou  !< // PARAMETER // Extinction Coefficient of PAR through the crop  (radiation transfer) // * // PARPLT // 1
        ! -- FIN TRANSRAD

        ! BEER
        !  integer, intent(IN)    :: P_codelaitr
          real,    intent(IN)    :: tauxcouv   !< // OUTPUT // Cover rate // SD
          real,    intent(IN)    :: P_khaut  !< // PARAMETER // Extinction Coefficient connecting leaf area index to height crop // * // PARAM // 1
        ! -- FIN BEER
          real,    intent(INOUT)   :: surfAO
          real,    intent(INOUT)   :: surfAS
          real,    intent(OUT)     :: fpari  ! DR 27/10/2015 ajout pour FR

end subroutine biomaer

!> This subroutine calculates the radiation interception.
!> - Stics book paragraphe 3,2, page 49
!>
!! Since most crop models are devoted to industrial crops the canopy is assumed to be a homogenous environment with leaves being randomly distributed over the area.
!! A consequence of this random, homogeneous representation is that it allows the use of an optical analogy (Beer's law) to estimate the interception of
!! photosynthetically active radiation. This law, having only one parameter (the extinction coefficient), has been thoroughly studied for many crops
!! (Varlet-Grancher et al., 1989): the more erect the plant, the smaller is the extinction coefficient. This approach is very successful for homogenous crops,
!! but poorly suited to canopies in rows or during the first stages of an annual crop because the homogeneity hypothesis cannot apply.
!! Consequently, like CROPGRO (Boote and Pickering, 1994) the STICS model can simulate canopies in rows, with prediction of light interception dependent
!! not only on LAI, but also on plant height and width, row spacing, plant spacing and direct and diffuse light absorption. Such capabilities are also required
!! to simulate intercropping.
!!
!! Thus in STICS two options are available to calculate radiation interception: a simple Beer's law, recommended for homogenous crops (see beer.f90),
!! and a more complex calculation for radiation transfers within the canopy, recommended for crops in rows (see transrad.f90). If the leaf status variable
!! is the ground cover and not the leaf area index, then only the Beer's law option is permitted.
subroutine raytrans(P_codetransrad,P_extin,cumrg,cumraint,fapar,delta,P_adfol,lairognecum,laieffcum,P_dfolbas,P_dfolhaut,     &
                    dfol,rdif,parapluie,raint,P_parsurrg,P_forme,lai,laisen,eai,P_interrang,nlax,nsen,P_codlainet,P_hautbase, &
                    P_codepalissage,P_hautmaxtec,P_largtec,originehaut,hauteur,deltahauteur,P_hautmax,varrapforme,largeur,    &
                    jul,trg,P_latitude, rombre,rsoleil,P_orientrang,P_ktrou,P_codelaitr,P_khaut,tauxcouv,surface,surfaceSous, &
                    ipl,P_nbplantes,ens, surfAO,surfAS)

  implicit none

!: Arguments

! RAYTRANS
  integer, intent(IN)    :: P_codetransrad  !< // PARAMETER // simulation option of radiation 'interception: law Beer (1), radiation transfers (2) // code 1/2 // PARPLT // 0
  real,    intent(IN)    :: surface(2)
  real,    intent(OUT)   :: surfaceSous(2)
  integer, intent(IN)    :: ipl
  integer, intent(IN)    :: P_nbplantes  !< // PARAMETER // number of simulated plants // SD // P_USM/USMXML // 0
  integer, intent(IN)    :: ens
  real,    intent(INOUT) :: P_extin(P_nbplantes)  !< // PARAMETER // extinction coefficient of photosynthetic active radiation canopy // SD // PARPLT // 1
  real,    intent(INOUT) :: cumrg   !< // OUTPUT // Sum of global radiation during the stage sowing-harvest   // Mj.m-2
  real,    intent(INOUT) :: cumraint   !< // OUTPUT // Sum of intercepted radiation  // Mj.m-2
  real,    intent(OUT)   :: fapar   !< // OUTPUT // Proportion of radiation intercepted // 0-1
  real,    intent(OUT)   :: delta

! TRANSRAD
  real,    intent(IN)    :: P_adfol  !< // PARAMETER // parameter determining the leaf density evolution within the chosen shape // m-1 // PARPLT // 1
  real,    intent(IN)    :: lairognecum
  real,    intent(IN)    :: laieffcum
  real,    intent(IN)    :: P_dfolbas  !< // PARAMETER // minimal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1
  real,    intent(IN)    :: P_dfolhaut  !< // PARAMETER // maximal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1
  real,    intent(OUT)   :: dfol   !< // OUTPUT //  "Within the shape  leaf density" // m2 m-3
  real,    intent(OUT)   :: rdif   !< // OUTPUT // Ratio between diffuse radiation and global radiation  // 0-1
  integer, intent(OUT)   :: parapluie
  real,    intent(OUT)   :: raint   !< // OUTPUT // Photosynthetic active radiation intercepted by the canopy  // Mj.m-2
  real,    intent(IN)    :: P_parsurrg  !< // PARAMETER // coefficient PAR/RG for the calculation of PAR  // * // STATION // 1

  integer, intent(IN)    :: P_forme  !< // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0
  real,    intent(IN)    :: lai   !< // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(IN)    :: laisen   !< // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
  real,    intent(IN)    :: eai
  real,    intent(IN)    :: P_interrang  !< // PARAMETER // Width of the P_interrang // m // PARTEC // 1
  real,    intent(IN)    :: nlax
  real,    intent(IN)    :: nsen
  real,    intent(IN)    :: P_codlainet  !< // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
  real,    intent(IN)    :: P_hautbase  !< // PARAMETER // Base height of crop // m // PARPLT // 1
  integer, intent(IN)    :: P_codepalissage  !< // PARAMETER // option: no (1),  yes2) // code 1/2 // PARTEC // 0
  real,    intent(IN)    :: P_hautmaxtec  !< // PARAMETER // maximal height of the plant allowed by the management // m // PARTEC // 1
  real,    intent(IN)    :: P_largtec  !< // PARAMETER // technical width // m // PARTEC // 1
  real,    intent(IN)    :: originehaut

  real,    intent(INOUT) :: hauteur   !< // OUTPUT // Height of canopy // m
  real,    intent(INOUT) :: deltahauteur
  real,    intent(INOUT) :: P_hautmax  !< // PARAMETER // Maximum height of crop // m // PARPLT // 1
  real,    intent(INOUT) :: varrapforme

  real,    intent(OUT)   :: largeur   !< // OUTPUT // Width of the plant shape  // m

  integer, intent(IN)    :: jul
  real,    intent(IN)    :: trg   !< // OUTPUT // Active radiation (entered or calculated) // MJ.m-2
  real,    intent(IN)    :: P_latitude  !< // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0

  real,    intent(OUT)   :: rombre   !< // OUTPUT // Radiation fraction in the shade // 0-1
  real,    intent(OUT)   :: rsoleil   !< // OUTPUT // Radiation fraction in the full sun // 0-1
  real,    intent(IN)    :: P_orientrang  !< // PARAMETER // Direction of ranks // rd (0=NS) // PARTEC // 1
  real,    intent(IN)    :: P_ktrou  !< // PARAMETER // Extinction Coefficient of PAR through the crop  (radiation transfer) // * // PARPLT // 1

!  real,    intent(INOUT) :: surf(2,2)
!  integer, intent(IN)    :: ipl
!  integer, intent(IN)    :: P_nbplantes
! -- FIN TRANSRAD

! BEER
  integer, intent(IN)    :: P_codelaitr  !< // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0
!  real,    intent(OUT)   :: raint
!  real,    intent(IN)    :: P_parsurrg
!  real,    intent(IN)    :: P_extin
!  real,    intent(IN)    :: lai
!  real,    intent(IN)    :: eai
!  real,    intent(IN)    :: trg
  real,    intent(IN)    :: tauxcouv   !< // OUTPUT // Cover rate // SD
!  real,    intent(OUT)   :: rombre
!  real,    intent(OUT)   :: rsoleil
!  integer, intent(OUT)   :: parapluie
!  integer, intent(IN)    :: nsen
!  integer, intent(IN)    :: nlax
!  integer, intent(IN)    :: P_codlainet
!  real,    intent(INOUT) :: hauteur
!  real,    intent(INOUT) :: deltahauteur
!  real,    intent(IN)    :: P_hautmax
!  real,    intent(IN)    :: P_hautbase
  real,    intent(IN)    :: P_khaut  !< // PARAMETER // Extinction Coefficient connecting leaf area index to height crop // * // PARAM // 1
!  real,    intent(IN)    :: laisen

!  integer, intent(IN)    :: ipl
!  integer, intent(OUT)   :: surf(2,2)
! -- FIN BEER
  real,    intent(INOUT)   :: surfAO
  real,    intent(INOUT)   :: surfAS

end subroutine raytrans


!> This subroutine calculates the radiation interception, with prediction of light interception dependent not only on LAI, but also on plant height and width,
!> row spacing, plant spacing and direct and diffuse light absorption.
!> - Stics book paragraphe 3.2.2, page 50-51
!>
!! A calculation of radiation transfer enables an estimate of the radiation intercepted by a crop in rows, taking account of its geometry in a simple fashion.
!! The objective is to estimate, on a daily time step, the fraction of radiation intercepted by the crop and fraction part transmitted to the layer below,
!! which can be either the soil or another crop (case of intercropping). To calculate those two components, the soil surface is split into a shaded part and
!! a sunlit part and by convention the shaded part corresponds to the vertical projection of the crop foliage onto the soil surface. The available daily variables
!! are the Leaf Area Index (LAI), calculated independently and the global radiation (trg)
!!
!! The simplest method of calculating the radiation received at a given point X (located on the soil in the inter-row) is to calculate angles H1 and H2
!! corresponding to the critical angles below which point X receives the total radiation directly.  At angles below H1 and above H2, point X receives an amount
!! of radiation below the total radiation value, due to absorption by the crop. Within those angle windows, Beer’s law is used to estimate the fraction of
!! transmitted radiation. It is assumed that a canopy can be represented by a simple geometric shape (rectangle or triangle) and that it is isotropically infinite.
!! We can therefore describe the daily radiation received at point X as the sum of the radiation not intercepted by the crop (rdroit)(sun at an angle between H1 and H2)
!! and the radiation transmitted (rtransmis).  The "infinite canopy" hypothesis allows us to assume that when the sun is at an angle below H1 and H2,
!! all the radiation passes through the crop. Each part of the radiation received at X includes a direct component and a diffuse component.
!! Let us assume that, for the transmitted part, the same extinction coefficient (ktrou) applies to both components (which is generally accepted to be the case
!! when the general Beer law is used with a daily time scale).
!!
!! In contrast, for rdroit, direct and diffuse components should be separated because of the directional character of the direct component, which requires
!! the calculation of separate proportions of radiation reaching the soil (kgdiffus and kgdirect are the proportions of diffuse radiation, rdiffus, and direct
!! radiation, rdirect, respectively, reaching the soil).
!---------------------------------------------------------------
subroutine transrad(P_adfol,lairognecum,laieffcum,P_dfolbas,P_dfolhaut,dfol,rdif,parapluie,raint,P_parsurrg,P_forme,lai,  &
                    laisen,eai, P_interrang,nlax,nsen,P_codlainet,P_hautbase,P_codepalissage,P_hautmaxtec,P_largtec,      &
                    originehaut,hauteur,deltahauteur, P_hautmax,varrapforme,largeur,jul,trg,P_latitude,rombre,rsoleil,    &
         !           P_orientrang,P_ktrou,surfAO,surfAS,ipl,P_nbplantes)
                    P_orientrang,P_ktrou,surfAO,surfAS,ipl)

  implicit none

  real,    intent(IN)    :: P_adfol  !< // PARAMETER // parameter determining the leaf density evolution within the chosen shape // m-1 // PARPLT // 1
  real,    intent(IN)    :: lairognecum
  real,    intent(IN)    :: laieffcum
  real,    intent(IN)    :: P_dfolbas  !< // PARAMETER // minimal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1
  real,    intent(IN)    :: P_dfolhaut  !< // PARAMETER // maximal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1
  real,    intent(OUT)   :: dfol   !< // OUTPUT //  "Within the shape  leaf density" // m2 m-3
  real,    intent(OUT)   :: rdif   !< // OUTPUT // Ratio between diffuse radiation and global radiation  // 0-1
  integer, intent(OUT)   :: parapluie
  real,    intent(OUT)   :: raint   !< // OUTPUT // Photosynthetic active radiation intercepted by the canopy  // Mj.m-2
  real,    intent(IN)    :: P_parsurrg  !< // PARAMETER // coefficient PAR/RG for the calculation of PAR  // * // STATION // 1

  integer, intent(IN)    :: P_forme  !< // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0
  real,    intent(IN)    :: lai   !< // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(IN)    :: laisen   !< // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
  real,    intent(IN)    :: eai
  real,    intent(IN)    :: P_interrang  !< // PARAMETER // Width of the P_interrang // m // PARTEC // 1
  real,    intent(IN)    :: nlax
  real,    intent(IN)    :: nsen
  real,    intent(IN)    :: P_codlainet  !< // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
  real,    intent(IN)    :: P_hautbase  !< // PARAMETER // Base height of crop // m // PARPLT // 1
  integer, intent(IN)    :: P_codepalissage  !< // PARAMETER // option: no (1),  yes2) // code 1/2 // PARTEC // 0
  real,    intent(IN)    :: P_hautmaxtec  !< // PARAMETER // maximal height of the plant allowed by the management // m // PARTEC // 1
  real,    intent(IN)    :: P_largtec  !< // PARAMETER // technical width // m // PARTEC // 1
  real,    intent(IN)    :: originehaut

  real,    intent(INOUT) :: hauteur   !< // OUTPUT // Height of canopy // m
  real,    intent(INOUT) :: deltahauteur
  real,    intent(INOUT) :: P_hautmax  !< // PARAMETER // Maximum height of crop // m // PARPLT // 1
  real,    intent(INOUT) :: varrapforme

  real,    intent(OUT)   :: largeur   !< // OUTPUT // Width of the plant shape  // m

  integer, intent(IN)    :: jul
  real,    intent(IN)    :: trg   !< // OUTPUT // Active radiation (entered or calculated) // MJ.m-2
  real,    intent(IN)    :: P_latitude  !< // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0

  real,    intent(OUT)   :: rombre   !< // OUTPUT // Radiation fraction in the shade // 0-1
  real,    intent(OUT)   :: rsoleil   !< // OUTPUT // Radiation fraction in the full sun // 0-1
  real,    intent(IN)    :: P_orientrang  !< // PARAMETER // Direction of ranks // rd (0=NS) // PARTEC // 1
  real,    intent(IN)    :: P_ktrou  !< // PARAMETER // Extinction Coefficient of PAR through the crop  (radiation transfer) // * // PARPLT // 1

  real,    intent(INOUT)   :: surfAO
  real,    intent(INOUT)   :: surfAS

  integer, intent(IN)    :: ipl
!  integer, intent(IN)    :: P_nbplantes  !< // PARAMETER // number of simulated plants // SD // P_USM/USMXML // 0



  real :: calcul_RDif

end subroutine transrad


! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 3.2.1, page 49
!>
!! This module calculates the radiation interception, assuming that the canopy is a homogenous environment with leaves being randomly distributed over the area.
!! A consequence of this random, homogeneous representation is that it allows the use of an optical analogy (Beer's law) to estimate the interception of
!! photosynthetically active radiation.
!!
!! Thus, the radiation intercepted by the crop (raint) is expressed according to a Beer's law function of LAI.  extin is a daily extinction coefficient and
!! parsurrg is a climatic parameter corresponding to the ratio of photosynthetically active radiation to the global radiation, trg
!! For homogenous crops, crop height is deduced from the leaf area index or the ground cover. It serves particularly in the calculation module for
!! water requirements via the resistive option. khaut is assumed to be plant-independent (a general value of 0.7 is proposed) while the potential height
!! of foliage growth is mostly plant-dependent and defined by the two limits hautbase and hautmax.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine beer(P_codelaitr,raint,P_parsurrg,P_extin,lai,eai,trg,tauxcouv,rombre,rsoleil,parapluie,nsen,  &
                nlax,P_codlainet,hauteur,deltahauteur,P_hautmax,P_hautbase,P_khaut,laisen,surface,ipl,    &
                surfaceSous)


  implicit none

!: Arguments
  integer, intent(IN)    :: P_codelaitr  !< // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0
  real,    intent(OUT)   :: raint   !< // OUTPUT // Photosynthetic active radiation intercepted by the canopy  // Mj.m-2
  real,    intent(IN)    :: P_parsurrg  !< // PARAMETER // coefficient PAR/RG for the calculation of PAR  // * // STATION // 1
  real,    intent(IN)    :: P_extin  !< // PARAMETER // extinction coefficient of photosynthetic active radiation canopy // SD // PARPLT // 1
  real,    intent(IN)    :: lai   !< // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(IN)    :: eai
  real,    intent(IN)    :: trg   !< // OUTPUT // Active radiation (entered or calculated) // MJ.m-2
  real,    intent(IN)    :: tauxcouv   !< // OUTPUT // Cover rate // SD
  real,    intent(OUT)   :: rombre   !< // OUTPUT // Radiation fraction in the shade // 0-1
  real,    intent(OUT)   :: rsoleil   !< // OUTPUT // Radiation fraction in the full sun // 0-1
  integer, intent(OUT)   :: parapluie
  integer, intent(IN)    :: nsen
  integer, intent(IN)    :: nlax
  integer, intent(IN)    :: P_codlainet  !< // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
  real,    intent(INOUT) :: hauteur   !< // OUTPUT // Height of canopy // m
  real,    intent(INOUT) :: deltahauteur
  real,    intent(IN)    :: P_hautmax  !< // PARAMETER // Maximum height of crop // m // PARPLT // 1
  real,    intent(IN)    :: P_hautbase  !< // PARAMETER // Base height of crop // m // PARPLT // 1
  real,    intent(IN)    :: P_khaut  !< // PARAMETER // Extinction Coefficient connecting leaf area index to height crop // * // PARAM // 1
  real,    intent(IN)    :: laisen   !< // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
  real,    intent(IN)    :: surface(2)

  integer, intent(IN)    :: ipl
  real,    intent(OUT)   :: surfaceSous(2)

 end subroutine beer


!> In STICS shoot senescence only concerns leaves: dry matter and LAI.  For cut crops, it also affects residual biomass after cutting.
!> - Stics book paragraphe 3.1.2, page 44-46
!> While in the first versions of the model senescence was implicit (Brisson et al., 1998a), it is now explicit, with a clear distinction between natural
!! senescence due to the natural ageing of leaves, and senescence accelerated by stresses (water, nitrogen, frost). The concept of leaf lifespan,
!! used for example by Maas (1993), is applied to the green leaf area and biomass produced. The leaf area and part of the leaf biomass produced on a given day
!! is therefore lost through senescence once the lifetime has elapsed (Duru et al., 1995). This part corresponds to the ratiosen parameter, taking into account
!! the part which was remobilised during its senescence.
!!
!! Calculation of lifespan:
!!
!! The natural lifespan of leaves (durage) is defined by two values: the  lifespan of early leaves, or durvieI (expressed as a proportion of durvieF ) and the
!! lifespan of the last leaves emitted, or durvieF (assumed genotype-dependent). Until the IAMF stage, the natural lifespan, calculated for the day when the
!! leaves are emitted (I0) is durvieI; from IAMF to ILAX, the natural lifespan increases between durvieI and durvieF as a function of the leaf development variable ULAI.
!! Because of water or nitrogen stress, the current lifespan may be shortened if the stress undergone is more intense than previous stresses.
!! Specific stress indices for senescence are introduced (senfac and innsenes).  Frost (fstressgel that can be either fgeljuv or fgelveg) may also reduce or
!! even cancel  lifespan.  In the event of over-fertilisation with nitrogen (inn >1), the foliage lifespan is increased from the IAMF stage up to a maximum
!! given by the durviesupmax parameter.
!! The lifespan of leaves is not expressed in degree.days (like phasic development), because this has the disadvantage of stopping any progression as soon as
!! the temperature is lower than the base temperature (tdmin).  To remedy this problem, the senescence course (somsen) is expressed by cumulated Q10 units
!! (with Q10=2), i.e. an exponential type function.
!! The senescence course between I0 and I is affected by the same cardinal temperatures as phasic development and can be slown down by stresses. The lifespan parameter
!! of the leaf (durvieF) expressed in Q10 units represents about 20% of the same  lifespan expressed in degree.days.
!!
!! Calculation of senescence:
!!
!! Material produced on day I0 disappears by senescence after a period corresponding to durvie(I0). Depending on the evolution of temperature and of lifespan
!! as a function of phenology and stresses, senescence can vary from one day to another and affect several days of production (J=I0, I0+1, …) or, on the contrary,
!! none (durvieE(I0)>somsen(I)).  This principle is applied to the biomass (dltamsen) and leaf area index (dltaisen). In general, the leaf biomass produced
!! does not completely disappear (remobilisation):  the ratiosen (<1) parameter enables the definition of the senescent proportion with reference to production.
!! It is the pfeuilverte ratio which defines the proportion of leaves in the biomass produced.
!! The cumulative senescent foliage is laisen. If the crop is a forage crop benefiting from residual dry matter from the previous cycle (msresiduel parameter),
!! the senescence of residual dry matter (deltamsresen) starts as from cutting.  It occurs at a rate estimated from the relative daily lifespan course and
!! weighted by the remobilisation (ratiosen).
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine senescen(nlev,n,nbjmax,lai,P_codeinnact,P_codeh2oact,senfac,innsenes,P_codlainet,  &
                    P_codeperenne,nbfeuille,P_nbfgellev,P_codgellev,tcultmin,P_tletale,P_tdebgel, &
                    P_tgellev90,P_tgellev10,ipl,densitelev,densiteassoc,P_codgeljuv,              &
                    P_tgeljuv90,P_tgeljuv10,P_codgelveg,P_tgelveg90,P_tgelveg10,masecveg,         &
                    nstopfeuille,somcour,resperenne,ndrp,nrec,P_QNpltminINN,numcult,              &
                    P_codeinitprec,ulai,P_vlaimax,durvieI,P_durvieF,inn,P_durviesupmax,           &
                    P_codestrphot,phoi,P_phobasesen,dltams,P_msresiduel,P_ratiosen,tdevelop,      &
                    somtemp,pfeuilverte,deltai,P_lai0,                                            &
                    dernier_n,nsencour,dltaisen,dltamsen,fstressgel,fgellev,gelee,                &
                    densite,laisen,nlan,P_stsenlan,nsen,P_stlaxsen,namf,nlax,P_stlevamf,          &
                    P_stamflax,nrecbutoir,mortplante,nst2,mortplanteN,durvie,strphot,             &
                    msres,dltamsres,ndebsen,somsenreste,msresjaune,mafeuiljaune,                  &
                    msneojaune,dltamstombe,QNplante,P_dltamsminsen,P_dltamsmaxsen,                &
                    P_alphaphot,strphotveille)

        USE Divers, only: GEL
        USE Messages

          implicit none

        !: Arguments

          integer, intent(IN)    :: nlev  
          integer, intent(IN)    :: n  
          integer, intent(IN)    :: nbjmax  
          real,    intent(IN)    :: lai   !< // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
          integer, intent(IN)    :: P_codeinnact  !< // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
          integer, intent(IN)    :: P_codeh2oact  !< // PARAMETER // code to activate  water stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
          real,    intent(IN)    :: senfac   !< // OUTPUT // Water stress index on senescence // 0-1
          real,    intent(IN)    :: innsenes   !< // OUTPUT // Index of nitrogen stress active on leaf death // P_innmin to 1
          integer, intent(IN)    :: P_codlainet  !< // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
          integer, intent(IN)    :: P_codeperenne  !< // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0
          integer, intent(IN)    :: nbfeuille   !< // OUTPUT // Number of leaves on main stem // SD
          integer, intent(IN)    :: P_nbfgellev  !< // PARAMETER // leaf number at the end of the juvenile phase (frost sensitivity)  // nb pl-1 // PARPLT // 1
          integer, intent(IN)    :: P_codgellev  !< // PARAMETER // activation of plantlet frost // code 1/2 // PARPLT // 0
          real,    intent(IN)    :: tcultmin  
          real,    intent(IN)    :: P_tletale  !< // PARAMETER // lethal temperature for the plant // degree C // PARPLT // 1
          real,    intent(IN)    :: P_tdebgel  !< // PARAMETER // temperature of frost beginning // degree C // PARPLT // 1
          real,    intent(IN)    :: P_tgellev90  !< // PARAMETER // temperature corresponding to 90% of frost damage on the plantlet  // degree C // PARPLT // 1
          real,    intent(IN)    :: P_tgellev10  !< // PARAMETER // temperature corresponding to 10% of frost damage on the plantlet  // degree C // PARPLT // 1
          integer, intent(IN)    :: ipl  
          real,    intent(IN)    :: densitelev  
          real,    intent(IN)    :: densiteassoc  
          integer, intent(IN)    :: P_codgeljuv  !< // PARAMETER // activation of LAI frost at the juvenile stadge // code 1/2 // PARPLT // 0
          real,    intent(IN)    :: P_tgeljuv90  !< // PARAMETER // temperature corresponding to 90 % of frost damage on the LAI (juvenile stage) // degree C // PARPLT // 1
          real,    intent(IN)    :: P_tgeljuv10  !< // PARAMETER // temperature corresponding to 10 % of frost damage on the LAI (juvenile stage) // degree C // PARPLT // 1
          integer, intent(IN)    :: P_codgelveg  !< // PARAMETER // activation of LAI frost at adult stage // code 1/2 // PARPLT // 0
          real,    intent(IN)    :: P_tgelveg90  !< // PARAMETER // temperature corresponding to 90 % of frost damage on the LAI (adult stage) // degree C // PARPLT // 1
          real,    intent(IN)    :: P_tgelveg10  !< // PARAMETER // temperature corresponding to 10 % of frost damage on the LAI (adult stage) // degree C // PARPLT // 1
          real,    intent(IN)    :: masecveg   !< // OUTPUT // Vegetative dry matter // t.ha-1
          integer, intent(IN)    :: nstopfeuille  
          real,    intent(IN)    :: somcour   !< // OUTPUT // Cumulated units of development between two stages // degree.days
          real,    intent(IN)    :: resperenne   !< // OUTPUT // C crop reserve, during the cropping season, or during the intercrop period (for perenial crops) // t ha-1
          integer, intent(IN)    :: ndrp  
          integer, intent(IN)    :: nrec  
          real,    intent(IN)    :: P_QNpltminINN  !< // PARAMETER // minimal amount of nitrogen in the plant allowing INN computing // kg ha-1 // PARAM // 1
          integer, intent(IN)    :: numcult  
          integer, intent(IN)    :: P_codeinitprec  !< // PARAMETER // reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0
          real,    intent(IN)    :: ulai(nbjmax)  ! dans l'idéal max(n,dernier_n) ! (dernier_n)    // OUTPUT // Daily relative development unit for LAI // 0-3
          real,    intent(IN)    :: P_vlaimax  !< // PARAMETER // ULAI  at inflection point of the function DELTAI=f(ULAI) // SD // PARPLT // 1
          real,    intent(IN)    :: durvieI  
          real,    intent(IN)    :: P_durvieF  !< // PARAMETER // maximal  lifespan of an adult leaf expressed in summation of P_Q10=2 (2**(T-Tbase)) // P_Q10 // PARPLT // 1
          real,    intent(IN)    :: inn   !< // OUTPUT // Nitrogen nutrition index (satisfaction of nitrogen needs ) // 0-1
          real,    intent(IN)    :: P_durviesupmax  !< // PARAMETER // proportion of additional lifespan due to an overfertilization // SD // PARPLT // 1
          integer, intent(IN)    :: P_codestrphot  !< // PARAMETER // activation of the photoperiodic stress on lifespan : yes (1), no (2) // code 1/2 // PARPLT // 0
          real,    intent(IN)    :: phoi   !< // OUTPUT // Photoperiod // hours
          real,    intent(IN)    :: P_phobasesen  !< // PARAMETER // photoperiod under which the photoperiodic stress is activated on the leaf lifespan // heures // PARPLT // 1
          real,    intent(IN)    :: dltams(nbjmax)!>   // OUTPUT // Growth rate of the plant  // t ha-1.j-1
          real,    intent(IN)    :: P_msresiduel  !< // PARAMETER // Residual dry matter after a cut // t ha-1 // PARTEC // 1
          real,    intent(IN)    :: P_ratiosen  !< // PARAMETER // fraction of senescent biomass (by ratio at the total biomass) // between 0 and 1 // PARPLT // 1
          real,    intent(IN)    :: tdevelop(n) ! 1 à n  
          real,    intent(IN)    :: somtemp   !< // OUTPUT // Sum of temperatures // degree C.j
          real,    intent(IN)    :: pfeuilverte(nbjmax) !>  // OUTPUT // Proportion of green leaves in total non-senescent biomass // 0-1
          real,    intent(IN)    :: deltai(nbjmax) !>    // OUTPUT // Daily increase of the green leaf index // m2 leafs.m-2 soil
          real,    intent(IN)    :: P_lai0  !< // PARAMETER // Initial leaf area index // m2 m-2 // INIT // 1

         ! dltams dans l'idéal max(n,dernier_n) ! (dernier_n) ! ndebsen à dernier_n, puis si n < ndebsen, 1 à n
         ! dans l'idéal max(n,dernier_n) ! (dernier_n) ! ndebsen à dernier_n, puis si n < ndebsen, 1 à n
         ! dans l'idéal max(n,dernier_n) ! (dernier_n) ! ndebsen à dernier_n, puis si n < ndebsen, 1 à n



        ! INOUT
          integer, intent(INOUT) :: dernier_n  
          integer, intent(INOUT) :: nsencour  
          real,    intent(INOUT) :: dltaisen   !< // OUTPUT // Daily increase of the senescent leaf index // m2.m-2 sol.j-1
          real,    intent(INOUT) :: dltamsen   !< // OUTPUT // Senescence rate // t ha-1 j-1
          real,    intent(INOUT) :: fstressgel   !< // OUTPUT // Frost index on the LAI // 0-1
          real,    intent(INOUT) :: fgellev  
          logical, intent(INOUT) :: gelee  
          real,    intent(INOUT) :: densite   !< // OUTPUT // Actual sowing density // plants.m-2
          real,    intent(INOUT) :: laisen(0:1) ! veille (0), aujourd'hui (1)    // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
          integer, intent(INOUT) :: nlan  
          real,    intent(INOUT) :: P_stsenlan  !< // PARAMETER // Sum of development units between the stages SEN et LAN // degree.days // PARPLT // 1
          integer, intent(INOUT) :: nsen  
          real,    intent(INOUT) :: P_stlaxsen  !< // PARAMETER // Sum of development units between the stages LAX and SEN // degree.days // PARPLT // 1
          integer, intent(INOUT) :: namf  
          integer, intent(INOUT) :: nlax  
          real,    intent(INOUT) :: P_stlevamf  !< // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1
          real,    intent(INOUT) :: P_stamflax  !< // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1
          integer, intent(INOUT) :: nrecbutoir  
          integer, intent(INOUT) :: mortplante  
          integer, intent(INOUT) :: nst2  
          integer, intent(INOUT) :: mortplanteN  
          real,    intent(INOUT) :: durvie(nbjmax)  ! dans l'idéal max(n,dernier_n) ! (dernier_n) ! ndebsen à dernier_n, puis si n < ndebsen, 1 à n    // OUTPUT // Actual life span of the leaf surface //  degree C
          real,    intent(OUT)   :: strphot  
          real,    intent(INOUT) :: msres  
          real,    intent(INOUT) :: dltamsres  
          integer, intent(INOUT) :: ndebsen  
          real,    intent(INOUT) :: somsenreste  
          real,    intent(INOUT) :: msresjaune   !< // OUTPUT // Senescent residual dry matter  // t.ha-1
          real,    intent(INOUT) :: mafeuiljaune   !< // OUTPUT // Dry matter of yellow leaves // t.ha-1
          real,    intent(INOUT) :: msneojaune   !< // OUTPUT // Newly-formed senescent dry matter  // t.ha-1
          real,    intent(IN)    :: dltamstombe  
          real,    intent(IN)    :: QNplante   !< // OUTPUT // Amount of nitrogen taken up by the plant  // kgN.ha-1

        ! STRESS PHOTOPERIODIQUE
          real,    intent(IN)    :: P_dltamsminsen  !< // PARAMETER // threshold value of deltams from which the photoperiodic effect on senescence is maximal // t ha-1j-1 // PARAM // 1
          real,    intent(IN)    :: P_dltamsmaxsen  !< // PARAMETER // threshold value of deltams from which there is no more photoperiodic effect on senescence // t ha-1j-1 // PARPLT // 1
          real,    intent(IN)    :: P_alphaphot  !< // PARAMETER // parameter of photoperiodic effect on leaf lifespan // P_Q10 // PARPLT // 1
          real,    intent(INOUT) :: strphotveille  
        ! -- FIN STRESS PHOTOPERIODIQUE

end subroutine senescen

subroutine ApportFeuillesMortes(fstressgel,CNplante,P_abscission,inn,P_parazofmorte,dltamsen, &
                               P_codedyntalle,mortmasec,mortreserve,surf,                     &
                               CNresmin,CNresmax,nbCouchesSol,nbResidus,Qmulchdec,      &
                               dltamstombe,mafeuiltombe,QNplante,QNplantetombe,nap,airg,      &
                               itrav1,itrav2,ires,Cres,Nres,Cnondec,Nnondec,                      &
                               resperenne,QCplantetombe,Cmulchnd,Nmulchnd,QCapp,QNapp,QCresorg,QNresorg, &
                               awb,bwb,cwb,CroCo,akres,bkres,ahres,bhres,Wb,kres,hres) ! param de decomposition du residu
! *---------------------------------------------------------------------------------------------------------------------------------------------------------
!> Addition of leaves falling from the plant onto the soil surface
!> - Stics book paragraphe 6.3.3, page 105
!>
!! Leaves falling onto the soil (the proportion of senescent leaves falling is ABSCISSIONP) during crop growth are taken into account by the model
!! as this phenomenon can be important (e.g. rapeseed due to winter frost). Their decomposition at the soil surface is simulated by the decomposition module
!! (see ResidusDecomposition.f90 and mineral.f90) as residues of young plants (category 2). The C/N ratio of leaves when they fall off is calculated from
!! the nitrogen nutrition index of the whole crop and relies on a plant parameter (PARAZOFMORTE), as proposed by Dorsainvil (2002).
! *---------------------------------------------------------------------------------------------------------------------------------------------------------
  implicit none

  real,    intent(IN)    :: fstressgel     !> // OUTPUT // Frost index on the LAI // 0-1
  real,    intent(IN)    :: CNplante       !> // OUTPUT // Nitrogen concentration of entire plant  // %
  real,    intent(IN)    :: P_abscission   !> // PARAMETER // senescent leaf proportion falling on the soil // SD // PARPLT // 1
  real,    intent(IN)    :: inn            !> // OUTPUT // Nitrogen nutrition index (satisfaction of nitrogen needs ) // 0-1
  real,    intent(IN)    :: P_parazofmorte !> // PARAMETER // parameter of proportionality between the C/N of dead leaves and the INN // SD // PARPLT // 1
  real,    intent(IN)    :: dltamsen       !> // OUTPUT // Senescence rate // t ha-1 d-1
  integer, intent(IN)    :: P_codedyntalle !> // PARAMETER // Activation of the module simulating tiller dynamic: yes (1), no (2) // code 1/2 // PARAMV6/PLT // 0
  real,    intent(IN)    :: mortmasec      !> // OUTPUT // Dead tiller biomass  // t.ha-1
  real,    intent(IN)    :: mortreserve    !> // OUTPUT // Reserve biomass corresponding to dead tillers // t.ha-1.d-1
  real,    intent(IN)    :: surf      !> // OUTPUT // Fraction of surface in the shade // 0-1
  real,    intent(IN)    :: CNresmin  !> // PARAMETER // minimum observed value of ratio C/N of organic residues  // g g-1 // PARAM // 1
  real,    intent(IN)    :: CNresmax  !> // PARAMETER // maximum observed value of ratio C/N of organic residues // g g-1 // PARAM // 1
  integer, intent(IN)    :: nbCouchesSol
  integer, intent(IN)    :: nbResidus
  real,    intent(IN)    :: Qmulchdec !> // PARAMETER // maximal amount of decomposing mulch // t C.ha-1 // PARAM // 1
  real,    intent(INOUT) :: dltamstombe
  real,    intent(INOUT) :: mafeuiltombe    !> // OUTPUT // Dry matter of fallen leaves // t.ha-1
  real,    intent(INOUT) :: QNplante        !> // OUTPUT // Amount of N taken up by the whole plant // kg.ha-1
  real,    intent(INOUT) :: QNplantetombe
  integer, intent(INOUT) :: nap !> nombre d'apports
  real,    intent(INOUT) :: airg ! !ATTENTION! n+1       // OUTPUT // Daily irrigation // mm
  integer, intent(INOUT) :: itrav1
  integer, intent(INOUT) :: itrav2
  integer, intent(INOUT) :: ires
  real,    intent(INOUT) :: Cres(nbCouchesSol,nbResidus) ! première couche, itrav1 = 1 & itrav2 = 1
  real,    intent(INOUT) :: Nres(nbCouchesSol,nbResidus) ! première couche, itrav1 = 1 & itrav2 = 1
  real,    intent(INOUT) :: QCapp      !> // OUTPUT // cumulative amount of organic C added to soil // kg.ha-1
  real,    intent(INOUT) :: QNapp      !> // OUTPUT // cumulative amount of organic N added to soil // kg.ha-1
  real,    intent(INOUT) :: Cnondec(10)!> // OUTPUT // undecomposable C stock of the type 10 residues on the surface // t.ha-1
  real,    intent(INOUT) :: Nnondec(10)!> // OUTPUT // undecomposable N stock of the type 10 residues on the surface // kg.ha-1
  real,    intent(INOUT) :: resperenne !> // OUTPUT // C crop reserve, during the cropping season, or during the intercrop period (for perenial crops) // t ha-1
! Ajout Bruno
  real,    intent(INOUT) :: QCplantetombe !> // OUTPUT // cumulative amount of C in fallen leaves // kg.ha-1
  real,    intent(INOUT) :: Cmulchnd
  real,    intent(INOUT) :: Nmulchnd
  real,    intent(INOUT) :: QCresorg
  real,    intent(INOUT) :: QNresorg
! ajout Bruno; paramètres de décomposition du residu ires (ici 'feuilles mortes')
  real,    intent(IN) ::awb
  real,    intent(IN) ::bwb
  real,    intent(IN) ::cwb
  real,    intent(IN) ::akres
  real,    intent(IN) ::bkres
  real,    intent(IN) ::ahres
  real,    intent(IN) ::bhres
  real,    intent(INOUT) ::Wb
  real,    intent(INOUT) ::kres
  real,    intent(INOUT) ::hres
  real,    intent(IN) ::CroCo

! Variables locales
  real :: Cfeupc       ! Cfeupc = C content (%) of falling leaves
  real :: CNresid
  real :: Cplantetombe ! ajout Bruno 22/05/2012
  real :: Wr           !ajout calcul des paramètres de décomposition du résidu "feuilles mortes"
  real :: Nplantetombe
  real :: eaures

 end subroutine ApportFeuillesMortes


!> This module simulates the yield formation for indeterminate growing plants
!> - Stics book paragraphe 3.1.2, page 44-46
!>
!> These species go on growing leaves while producing and growing harvested organs (fruits) during a period of time. There is thus a trophic interaction between
!! the growth of various groups of organs and among successive cohorts of harvested organs that is accounted for in STICS by the source/sink approach using
!! the notion of trophic stress. Both processes of organ setting and filling are concerned, assuming that abortion cannot occur during the filling phase.
!! The simulation technique adopted in STICS was inspired from the "boxcartrain" technique (Goudriaan, 1986) that is used in the TOMGRO model (Jones et al., 1991).
!! During growth, the fruits go through nboite compartments corresponding to increasing physiological ages.  The time fruits spend in a compartment depends on
!! temperature. In each compartment, fruit growth is equal to the product of a "sink strength" function and the source-sink ratio. The fruit sink strength
!! is the derivative of a logistic function that takes the genetic growth potential of a fruit into consideration (Bertin and Gary, 1993).
!>  - Fruit setting
!!   Fruits are set between the idrp stage and the inou stage (end of setting), defined by the stdrpnou phasic course. If this setting period lasts a long time,
!!   then the number of simultaneous compartments (i.e. fruits of different ages) is great which indicates that there must be agreement between the values
!!   of stdrpnou and nboite. During this setting period, on each day, the number of set fruits (nfruitnou) depends on afruitpot, a varietal parameter expressed
!!   as the potential number of set fruits per inflorescence and per degree.day, the daily development rate (UPVT), the number of inflorescences per plant(nbinflo),
!!   the plant density (densite), the trophic stress index (spfruit) and the frost stress index acting on fruits from flowering (fgelflo).
!!   The introduction of the notion of inflorescence (group of fruits) into the model is only useful when technical or trophic regulation occurs at the
!!   inflorescence level (in grapevines for example). If the number of inflorescences is more than 1 (in the case of vines, inflorescences=bunches),
!!   it can either be prescribed (nbinflo), or calculated as a function of the trophic status of the plant at an early stage (we have chosen iamf).
!!   In the latter case, nbinflo is calculated using the pentinflores  and inflomax parameters. Pruning is not accounted for in this calculation.
!>  - Fruit filling
!!   The time spent by each fruit in a given compartment is dureefruit/nboite, where dureefruit is the total duration of fruit growth expressed in
!!   developmental units.  In the last box (or age class), the fruits no longer grow and the final dry mass of the fruit has been reached: the fruit is assumed
!!   to have reached physiological maturity. Each day, in each growth compartment (K), the fruit growth (croifruit) depends on the number of fruits
!!   in the compartment (nfruit) multiplied by the growth of each fruit, i.e. the elementary fruit sink strength (fpft), the trophic stress index (sourcepuits)
!!   and the thermal stress index (ftempremp).
!!   There are two successive phases in fruit growth; the first corresponds to a cell division phase while the second is devoted to expansion of the cells already set.
!!   In order to account for this double dynamics, the fruit potential cumulative growth is defined as the summation of two functions:
!>      - an exponential type function describing the cell division phase (using the parameters CFPFP and DFPFP)
!>      - a logistic type function describing the cell elongation phase (using the parameters AFPFP and BFPFP)
!>
!!   pgrainmaxi is the genetic-dependent maximal weight of the fruit and dfr stands for the fruit development stage of each age class,
!!   varying between 0 and 1; it is calculated for each age class (K) in a discrete way.
!!   Then the daily fruit sink strength function (fpft) is calculated for each age class, accounting for the duration of fruit growth from setting to maturity,
!!   expressed in developmental units (dureefruit).
!!   If allocation to fruits (allocfruit variable) exceeds the allocfrmax threshold, the sourcepuits variable is reduced in proportion to the
!!   allocfruit/allocfrmax ratio. In the last box, the fruits are ripe and stop growing. The number of fruits present on the plant or fruit load is
!!   chargefruit (see cortie.f90). If the codefrmur is 1, then the chargefruit variable will take account of the fruits in the last box (ripe);
!!   if not, it will only take account of the (N-1) first boxes.
! ---------------------------------------------------------------------------
subroutine fruit(n,namf,P_codcalinflo,P_pentinflores,densite,P_resperenne0,cumdltares,P_inflomax,ndrp,  &
                 P_nbcueille,nrec,P_nboite,P_dureefruit,fpv,dltams,dltaremobil,remobilj,P_spfrmin,P_spfrmax,&
                 P_afruitpot,upvt,fgelflo,P_codazofruit,P_codeinnact,inns,nnou,P_codeclaircie,nb_eclair,    &
                 neclair, P_nbinfloecl,&
                 P_codetremp,tcultmin,tcultmax,P_tminremp,P_tmaxremp,nbrecolte,rdtint,         &
                 P_allocfrmax,nmat,P_afpf,P_bfpf,P_cfpf,P_dfpf,pfmax,                                       &
                 P_nbinflo,nfruit,pdsfruit,fpft,sourcepuits,spfruit,nbfruit,                                & !INOUT
                 nfruitnou,nbfrote,devjourfr,cumdevfr,pousfruit,ftempremp,nbj0remp,                         &
                 dltags,frplusp,ircarb,magrain,allocfruit,masec,compretarddrp,pdsfruittot)

          USE Messages

          implicit none

          integer, intent(IN)    :: n  
          integer, intent(IN)    :: namf  
          integer, intent(IN)    :: P_codcalinflo  !< // PARAMETER // option of the way of calculation of the inflorescences number  // code 1/2 // PARPLT // 0
          real,    intent(IN)    :: P_pentinflores  !< // PARAMETER // parameter of the calculation of the inflorescences number  // SD // PARPLT // 1
          real,    intent(IN)    :: densite   !< // OUTPUT // Actual sowing density // plants.m-2
          real,    intent(IN)    :: P_resperenne0  !< // PARAMETER // initial reserve biomass // t ha-1 // INIT // 1
          real,    intent(IN)    :: cumdltares  
          real,    intent(IN)    :: P_inflomax  !< // PARAMETER // maximal number of inflorescences per plant // nb pl-1 // PARPLT // 1
          integer, intent(IN)    :: ndrp  
          integer, intent(IN)    :: P_nbcueille  !< // PARAMETER // number of fruit harvestings // code 1/2 // PARTEC // 0
          integer, intent(IN)    :: nrec  
          integer, intent(IN)    :: P_nboite  !< // PARAMETER // "Number of  box  or  age class  of fruits for the fruit growth for the indeterminate crops " // SD // PARPLT // 1
          real,    intent(IN)    :: P_dureefruit  !< // PARAMETER // total growth period of a fruit at the setting stage to the physiological maturity // degree.days // PARPLT // 1
          real,    intent(IN)    :: fpv   !< // OUTPUT // Sink strength of developing leaves // g.j-1.m-2
          real,    intent(IN)    :: dltams   !< // OUTPUT // Growth rate of the plant  // t ha-1.j-1
          real,    intent(IN)    :: dltaremobil   !< // OUTPUT // Amount of perennial reserves remobilised // g.m-2.j-1
          real,    intent(IN)    :: remobilj   !< // OUTPUT // Amount of biomass remobilized on a daily basis for the fruits  // g.m-2 j-1
          real,    intent(IN)    :: P_spfrmin  !< // PARAMETER // minimal sources/sinks value allowing the trophic stress calculation for fruit onset // SD // PARPLT // 1
          real,    intent(IN)    :: P_spfrmax  !< // PARAMETER // maximal sources/sinks value allowing the trophic stress calculation for fruit onset // SD // PARPLT // 1
          real,    intent(IN)    :: P_afruitpot  !< // PARAMETER // maximal number of set fruits per degree.day (indeterminate growth) // nbfruits degree.day-1 // PARPLT // 1
          real,    intent(IN)    :: upvt   !< // OUTPUT // Daily development unit  // degree.days
          real,    intent(IN)    :: fgelflo   !< // OUTPUT // Frost index on the number of fruits // 0-1
          integer, intent(IN)    :: P_codazofruit  !< // PARAMETER // option of activation of the direct effect of the nitrogen plant status upon the fruit/grain number // code 1/2 // PARPLT // 0
          integer, intent(IN)    :: P_codeinnact  !< // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
          real,    intent(IN)    :: inns   !< // OUTPUT // Index of nitrogen stress active on growth in biomass // P_innmin to 1
          integer, intent(IN)    :: nnou  
          integer, intent(IN)    :: P_codeclaircie  !< // PARAMETER // option to activate techniques of fruit removal  // code 1/2 // PARTEC // 0
          integer, intent(IN)    :: nb_eclair
          integer, intent(IN)    :: neclair(1:nb_eclair)
          real,    intent(IN)    :: P_nbinfloecl(1:nb_eclair)  !< // PARAMETER // number of inflorescences or fruits removed at fruit removal // nb pl-1 // PARTEC // 1
          integer, intent(IN)    :: P_codetremp  !< // PARAMETER // option of heat effect on grain filling: yes (2), no (1) // code 1/2 // PARPLT // 0
          real,    intent(IN)    :: tcultmin  
          real,    intent(IN)    :: tcultmax   !< // OUTPUT // Crop surface temperature (daily maximum) // degree C
          real,    intent(IN)    :: P_tminremp  !< // PARAMETER // Minimal temperature for grain filling // degree C // PARPLT // 1
          real,    intent(IN)    :: P_tmaxremp  !< // PARAMETER // maximal temperature for grain filling // degree C // PARPLT // 1
          integer, intent(IN)    :: nbrecolte  
          real,    intent(IN)    :: rdtint(nbrecolte) !< 1 to nbrecolte
          real,    intent(IN)    :: P_allocfrmax  !< // PARAMETER // maximal daily allocation towards fruits // SD // PARPLT // 1
          integer, intent(IN)    :: nmat  
          real,    intent(IN)    :: P_afpf  !< // PARAMETER // parameter of the  function logistic defining sink strength of fruits (indeterminate growth) : relative fruit age at which growth is maximal // SD // PARPLT // 1
          real,    intent(IN)    :: P_bfpf  !< // PARAMETER // parameter of the logistic curve defining sink strength of fruits (indeterminate growth) :  rate of maximum growth proportionately to maximum weight of fruits // * // PARPLT // 1
          real,    intent(IN)    :: P_cfpf  !< // PARAMETER // parameter of the first potential growth phase of fruit, corresponding to an exponential type function describing the cell division phase. // SD // PARPLT // 1
          real,    intent(IN)    :: P_dfpf  !< // PARAMETER // parameter of the first potential growth phase of fruit, corresponding to an exponential type function describing the cell division phase. // SD // PARPLT // 1
          real,    intent(IN)    :: pfmax  

          real,    intent(INOUT) :: P_nbinflo  !< // PARAMETER // imposed number of inflorescences  // nb pl-1 // PARPLT // 1   // OUTPUT // Number of inflorescences // SD
          real,    intent(INOUT) :: nfruit(P_nboite)    !< 1 to P_nboite    // OUTPUT // Number of fruits in box 5 // nb fruits
          real,    intent(INOUT) :: pdsfruit(P_nboite)  !< 1 to P_nboite    // OUTPUT // Weight of fruits in box 3 // g m-2
          real,    intent(INOUT) :: fpft   !< // OUTPUT // Sink strength of fruits  // g.m-2 j-1
          real,    intent(INOUT) :: sourcepuits   !< // OUTPUT // Pool/sink ratio // 0-1
          real,    intent(INOUT) :: spfruit   !< // OUTPUT // Index of trophic stress applied to the number of fruits // 0 to 1
          real,    intent(INOUT) :: nbfruit  
          real,    intent(INOUT) :: nfruitnou   !< // OUTPUT // Number of set fruits  // nb set fruits.m-2
          real,    intent(INOUT) :: nbfrote  
          real,    intent(INOUT) :: devjourfr  
          real,    intent(INOUT) :: cumdevfr  
          real,    intent(INOUT) :: pousfruit   !< // OUTPUT // Number of fruits transferred from one box to the next  // nb fruits
          real,    intent(INOUT) :: ftempremp  
          integer, intent(INOUT) :: nbj0remp   !< // OUTPUT // Number of shrivelling days //
          real,    intent(INOUT) :: dltags   !< // OUTPUT // Growth rate of the grains  // t ha-1.j-1
          real,    intent(INOUT) :: frplusp  
          real,    intent(INOUT) :: ircarb   !< // OUTPUT // Carbon harvest index // g  grain g plant-1
          real,    intent(INOUT) :: magrain(0:1) !< n-1 & n
          real,    intent(INOUT) :: allocfruit   !< // OUTPUT // Allocation ratio of  assimilats to the  fruits 0 to 1  // 0-1
          real,    intent(INOUT) :: masec   !< // OUTPUT // Aboveground dry matter  // t.ha-1
          integer, intent(INOUT) :: compretarddrp  
          real,    intent(INOUT) :: pdsfruittot  

end subroutine fruit
!> Calculation of the number and filling of organs for harvest in the case of plants with determinate growth.
!> - Stics book paragraphe 4,1, page 74-76
!>
!> In the case of plants with determinate growth, the hypothesis is made that the number and filling of organs for harvest do not depend on the other
!! organs’ growth requirements. The number of grains is fixed during a phase of variable duration (nbjgrain in days), which precedes the onset of filling (IDRP).
!! This number depends on the mean growth rate of the canopy during this period (vitmoy in gm-2d-1), which in turns depends on dynamics specific to the
!! particular species.
!>
!> The number of grains per m2 (nbgrains) is defined at the IDRP stage. It depends on the growth variable (vitmoy in g m-2) that integrates the effect of the
!! prevailing stresses during the period preceding the IDRP stage, on two species-dependent parameters cgrain (in g-1 m2) and nbgrmin (grains m-2) and a
!! genetic-dependent parameter nbgrmax (grains m-2).  The last two parameters define the limits of variation of nbgrains.
!! After the IDRP stage, the grain number can be reduced in the event of frost and the daily proportion of grains affected is (1-fgelflo), whatever their
!! state of growth. The corresponding weight (pgraingel in gm-2) is deducted from the grain weight, using the elementary current grain weight (pgrain in g).
!>
!> The quantity of dry matter accumulated in grains is calculated by applying a progressive "harvest index" to the dry weight of the plant.
!! This ircarb index increases linearly with time (vitircarb in g grain g biomass 1 d-1), from the IDRP stage to the IMAT stage and the final harvest index
!! is restricted to the irmax parameter. Yet this dynamics may not be the actual grain filling dynamics since threshold translocation temperatures defining
!! the thermal stress ftempremp (tminremp and tmaxremp) may stop the carbon filling of harvested organs.
!! Consequently the grain filling is calculated daily (dltags in t ha-1) to allow the effect of the thermal stress and then accumulated within the
!! mafruit (in t ha-1) variable. The mass of each grain is then calculated as the ratio of the mass to the number of grains, although this cannot exceed
!! the genetic pgrainmaxi limit.
!-----------------------------------------------------------------------------------------------
subroutine grain(n,ndrp,nrec,nlev,nrecbutoir,P_nbjgrain,dltams,P_cgrain,P_cgrainv0,  & ! IN
                 P_nbgrmin,P_nbgrmax,P_codazofruit,P_codeinnact,inns,fgelflo,P_codeir,   & ! IN
                 P_vitircarb,P_irmax,P_vitircarbT,somcourdrp,nmat,masec,P_codetremp,     & ! IN
                 tcultmin,tcultmax,P_tminremp,P_tmaxremp,P_pgrainmaxi,                   & ! IN
                 ircarb,nbgrains,pgrain,CNgrain,vitmoy,nbgraingel,pgraingel,             & ! INOUT
                 dltags,ftempremp,magrain,nbj0remp,pdsfruittot)                            ! INOUT

          USE Messages

          implicit none

        !: Arguments

          integer, intent(IN)    :: n  
          integer, intent(IN)    :: ndrp  
          integer, intent(IN)    :: nrec  
          integer, intent(IN)    :: nlev  
          integer, intent(IN)    :: nrecbutoir  
          integer, intent(IN)    :: P_nbjgrain  !< // PARAMETER // Period to compute NBGRAIN // days // PARPLT // 1
          real,    intent(IN)    :: dltams(ndrp) ! ndrp-P_nbjgrain+1 à ndrp    // OUTPUT // Growth rate of the plant  // t ha-1.j-1
          real,    intent(IN)    :: P_cgrain  !< // PARAMETER // slope of the relationship between grain number and growth rate  // grains gMS -1 jour // PARPLT // 1
          real,    intent(IN)    :: P_cgrainv0  !< // PARAMETER // number of grains produced when growth rate is zero // grains m-2 // PARPLT // 1
          real,    intent(IN)    :: P_nbgrmin  !< // PARAMETER // Minimum number of grain // grains m-2  // PARPLT // 1
          real,    intent(IN)    :: P_nbgrmax  !< // PARAMETER // Maximum number of grain // grains m-2 // PARPLT // 1
          integer, intent(IN)    :: P_codazofruit  !< // PARAMETER // option of activation of the direct effect of the nitrogen plant status upon the fruit/grain number // code 1/2 // PARPLT // 0
          integer, intent(IN)    :: P_codeinnact  !< // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
          real,    intent(IN)    :: inns   !< // OUTPUT // Index of nitrogen stress active on growth in biomass // P_innmin to 1
          real,    intent(IN)    :: fgelflo   !< // OUTPUT // Frost index on the number of fruits // 0-1
          integer, intent(IN)    :: P_codeir  !< // PARAMETER // option of computing the ratio grain weight/total biomass: proportional to time(1), proportional to sum temperatures (2) // code 1/2 // PARPLT // 0
          real,    intent(IN)    :: P_vitircarb  !< // PARAMETER // Rate of increase of the carbon harvest index // g grain g plant -1 day-1 // PARPLT // 1
          real,    intent(IN)    :: P_irmax  !< // PARAMETER // Maximum harvest index // SD // PARPLT // 1
          real,    intent(IN)    :: P_vitircarbT  !< // PARAMETER // Heat rate of increase of the carbon harvest index  // g grain g plant-1 degree.day-1 // PARPLT // 1
          real,    intent(IN)    :: somcourdrp  
          integer, intent(IN)    :: nmat  
          real,    intent(IN)    :: masec(0:1)  !< n-1 (0) & n (1)    // OUTPUT // Aboveground dry matter  // t.ha-1
          integer, intent(IN)    :: P_codetremp  !< // PARAMETER // option of heat effect on grain filling: yes (2), no (1) // code 1/2 // PARPLT // 0
          real,    intent(IN)    :: tcultmin  
          real,    intent(IN)    :: tcultmax   !< // OUTPUT // Crop surface temperature (daily maximum) // degree C
          real,    intent(IN)    :: P_tminremp  !< // PARAMETER // Minimal temperature for grain filling // degree C // PARPLT // 1
          real,    intent(IN)    :: P_tmaxremp  !< // PARAMETER // maximal temperature for grain filling // degree C // PARPLT // 1
          real,    intent(IN)    :: P_pgrainmaxi  !< // PARAMETER // Maximum weight of one grain (à 0% water content) // g // PARPLT // 1

          real,    intent(INOUT) :: ircarb(0:1)   !< n-1 (0) & n (1)    // OUTPUT // Carbon harvest index // g  grain g plant-1
          real,    intent(INOUT) :: nbgrains  
          real,    intent(INOUT) :: pgrain  
          real,    intent(INOUT) :: CNgrain   !< // OUTPUT // Nitrogen concentration of grains  // %
          real,    intent(INOUT) :: vitmoy   !< // OUTPUT // mean growth rate of the canopy (dans le livre c'est en g mais ca colle pas)  // g.m-2.d-1
          real,    intent(INOUT) :: nbgraingel  
          real,    intent(INOUT) :: pgraingel  
          real,    intent(INOUT) :: dltags   !< // OUTPUT // Growth rate of the grains  // t ha-1.j-1
          real,    intent(INOUT) :: ftempremp  
          real,    intent(INOUT) :: magrain(0:1)  !< n-1 (0) & n (1)
          integer, intent(INOUT) :: nbj0remp   !< // OUTPUT // Number of shrivelling days //
          real,    intent(INOUT) :: pdsfruittot  

end subroutine grain
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module deals with the quality of harvested organs: water content and biochemical composition
!> - Stics book paragraphe 4.3, page 81-83
!>
!> Water content:
!>
!! For harvested organs, it is assumed that the water content is constant (h2ofrvert) up to the stage idebdes. This stage may occur before physiological maturity.
!! For indeterminate plants, it does not occur at the same time for all fruit cohorts but it corresponds to one of the age classes.
!! We shall call this stage "onset of fruit water dynamics" that can be hydration or dehydration which results from the concomitant water and dry matter influx
!! into the fruit or grain. As from this stage, we assume that there is a "programmed" time course in the water content of fruits, and this is expressed
!! using the deshydbase parameter (g water.g FM-1.d-1), which day after day will modify the fruit water content (teaugrain) from its initial value h2ofrvert.
!! For dehydration deshydbase is positive; if the programme evolution tends towards hydration, deshydbase is negative. Dehydration may be accelerated (or provoked)
!! by water stress, which is characterised by the difference between the crop and air temperatures. The proportionality coefficient is called tempdeshyd
!! in g water.g FM-1. degreesC-1.
!>
!> Biochemical composition:
!>
!! To complete the components of the quality of simulated harvested organs, we propose a very simple estimate of the sugar and oil contents.
!! From the beginning of fruit/grain filling until physiological maturity, we assume that there is a gradual increase in the proportions of these two types
!! of components in the dry matter of fruits. This increase is determined using the virpropsucre and vitprophuile parameters expressed in g.g DM-1.degree.day-1.
!! The combination of this evolution and the evolution in the water content in fruits produces contents based on fresh matter, which depends on the development
!! of each crop. For indeterminate crops, the calculation is made for each age category separately, and then combined for all age categories.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine eauqual(n,P_deshydbase,P_tempdeshyd,tcult,tairveille,ndrp,nrec,P_codeindetermin,P_nboite,    & !IN
                   P_stdrpdes,P_dureefruit,nfruit,pousfruit,pdsfruit,P_h2ofrvert,frplusp,P_vitpropsucre,    &
                   P_vitprophuile,magrain,somcourdrp,nmat,maenfruit,nlev,masec,mafeuilverte,P_h2ofeuilverte,&
                   mafeuiljaune,P_h2ofeuiljaune,matigestruc,P_h2otigestruc,resperenne,P_h2oreserve,         &
                   deshyd,eaufruit,ndebdes,teaugrain,h2orec,sucre,huile,sucreder,huileder,sucrems,huilems,  & !INOUT
                   pdsfruitfrais,mafraisrec,CNgrain,mafraisfeuille,mafraistige,mafraisres,mafrais)

        USE Messages

          implicit none

        !: Arguments

          integer, intent(IN)    :: n  
          real,    intent(IN)    :: P_deshydbase  !< // PARAMETER // phenological rate of evolution of fruit water content (>0 or <0) // g water.g MF-1.degree C-1 // PARPLT // 1
          real,    intent(IN)    :: P_tempdeshyd  !< // PARAMETER // increase in the fruit dehydration due to the increase of crop temperature (Tcult-Tair) // % water degree C-1 // PARPLT // 1
          real,    intent(IN)    :: tcult   !< // OUTPUT // Crop surface temperature (daily average) // degree C
          real,    intent(IN)    :: tairveille   !< // OUTPUT // Mean air temperature the previous day // degree C
          integer, intent(IN)    :: ndrp  
          integer, intent(IN)    :: nrec  
          integer, intent(IN)    :: P_codeindetermin  !< // PARAMETER // option of  simulation of the leaf growth and fruit growth : indeterminate (2) or determinate (1) // code 1/2 // PARPLT // 0
          integer, intent(IN)    :: P_nboite  !< // PARAMETER // "Number of  box  or  age class  of fruits for the fruit growth for the indeterminate crops " // SD // PARPLT // 1
          real,    intent(IN)    :: P_stdrpdes  !< // PARAMETER // phasic duration between the DRP stage and the beginning of the water fruit dynamics  // degree.days // PARPLT // 1
          real,    intent(IN)    :: P_dureefruit  !< // PARAMETER // total growth period of a fruit at the setting stage to the physiological maturity // degree.days // PARPLT // 1
          real,    intent(IN)    :: nfruit(P_nboite)        !< 1 to P_nboite    // OUTPUT // Number of fruits in box 5 // nb fruits
          real,    intent(IN)    :: pousfruit   !< // OUTPUT // Number of fruits transferred from one box to the next  // nb fruits
          real,    intent(IN)    :: pdsfruit(P_nboite)      !< 1 to P_nboite    // OUTPUT // Weight of fruits in box 3 // g m-2
          real,    intent(IN)    :: P_h2ofrvert  !< // PARAMETER // water content of fruits before the beginning of hydrous evolution (DEBDESHYD) // g water g-1 MF // PARPLT // 1
          real,    intent(IN)    :: frplusp  
          real,    intent(IN)    :: P_vitpropsucre  !< // PARAMETER // increase rate of sugar harvest index  // g sugar g MS-1  j-1 // PARPLT // 1
          real,    intent(IN)    :: P_vitprophuile  !< // PARAMETER // increase rate of oil harvest index  // g oil g MS-1 j-1 // PARPLT // 1
          real,    intent(IN)    :: magrain  
          real,    intent(IN)    :: somcourdrp  
          integer, intent(IN)    :: nmat  
          real,    intent(IN)    :: maenfruit   !< // OUTPUT // Dry matter of harvested organ envelopes // t.ha-1
          integer, intent(IN)    :: nlev  
          real,    intent(IN)    :: masec   !< // OUTPUT // Aboveground dry matter  // t.ha-1
          real,    intent(IN)    :: mafeuilverte   !< // OUTPUT // Dry matter of green leaves // t.ha-1
          real,    intent(IN)    :: P_h2ofeuilverte  !< // PARAMETER // water content of green leaves // g water g-1 MF // PARPLT // 1
          real,    intent(IN)    :: mafeuiljaune   !< // OUTPUT // Dry matter of yellow leaves // t.ha-1
          real,    intent(IN)    :: P_h2ofeuiljaune  !< // PARAMETER // water content of yellow leaves // g water g-1 MF // PARPLT // 1
          real,    intent(IN)    :: matigestruc   !< // OUTPUT // Dry matter of stems (only structural parts) // t.ha-1
          real,    intent(IN)    :: P_h2otigestruc  !< // PARAMETER // structural stem part water content // g eau g-1 MF // PARPLT // 1
          real,    intent(IN)    :: resperenne   !< // OUTPUT // C crop reserve, during the cropping season, or during the intercrop period (for perenial crops) // t ha-1
          real,    intent(IN)    :: P_h2oreserve  !< // PARAMETER // reserve water content // g eau g-1 MF // PARPLT // 1


          real,    intent(INOUT) :: deshyd(0:P_nboite)       !< 0 to P_nboite
          real,    intent(INOUT) :: eaufruit(0:P_nboite)     !< 0 to P_nboite
          integer, intent(INOUT) :: ndebdes  
          real,    intent(INOUT) :: teaugrain  
          real,    intent(INOUT) :: h2orec   !< // OUTPUT // Water content of harvested organs // %
          real,    intent(INOUT) :: sucre   !< // OUTPUT // Sugar content of fresh harvested organs // % (of fresh weight)
          real,    intent(INOUT) :: huile   !< // OUTPUT // Oil content of fresh harvested organs // % (of fresh weight)
          real,    intent(INOUT) :: sucreder  
          real,    intent(INOUT) :: huileder  
          real,    intent(INOUT) :: sucrems  
          real,    intent(INOUT) :: huilems  
          real,    intent(INOUT) :: pdsfruitfrais   !< // OUTPUT // Total weight of fresh fruits // g m-2
          real,    intent(INOUT) :: mafraisrec  
          real,    intent(INOUT) :: CNgrain   !< // OUTPUT // Nitrogen concentration of grains  // %
          real,    intent(INOUT) :: mafraisfeuille  
          real,    intent(INOUT) :: mafraistige  
          real,    intent(INOUT) :: mafraisres  
          real,    intent(INOUT) :: mafrais   !< // OUTPUT // Aboveground fresh matter // t.ha-1

end subroutine eauqual


subroutine croissanceFrontRacinaire(                                                                  &
             n,nh,nbCouches,hur,hucc,tcult,tsol,humin,dacouche,P_codesimul,P_epc,P_obstarac,              & ! IN
             P_daseuilbas,P_daseuilhaut,P_dacohes,P_zrac0,P_densinitial,P_coderacine,P_codeplante,        &
             P_codeperenne,P_codehypo,P_codegermin,P_zracplantule,P_stoprac,P_codetemprac,P_tcmin,P_tcmax,&
             P_tgmin,P_croirac,P_contrdamax,P_codeindetermin,P_sensanox,nplt,nrec,codeinstal,nger,nsen,   &
             nlax,nflo,nmat,nlev,namf,izrac,P_profsem,P_codcueille,deltai,lai,sioncoupe,P_sensrsec,       &
             P_codedyntalle,P_tcxstop,dltams,                                                             &
             nhe,nstoprac,zrac,znonli,rl_veille,deltaz,dtj,cumlracz,poussracmoy,lracsenz,tempeff,efda,humirac_mean)  ! INOUT

        USE Divers, only: F_humirac, escalin
        USE Messages

          implicit none


          integer,           intent(IN)    :: n  
          integer,           intent(IN)    :: nh  
          integer,           intent(IN)    :: nbCouches  
          real,              intent(IN)    :: hur(nbCouches)  
          real,              intent(IN)    :: hucc(nbCouches)  
          real,              intent(IN)    :: tcult   !< // OUTPUT // Crop surface temperature (daily average) // degree C
          real,              intent(IN)    :: tsol(nbCouches)  
          real,              intent(IN)    :: humin(nbCouches)  
          real,              intent(IN)    :: dacouche(nbCouches)  

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!       character(len=*),  intent(IN)    :: P_codesimul  !< // PARAMETER // simulation code (culture ou feuille=lai forcé) // SD // P_USM/USMXML // 0
!!!       character(len=3),  intent(IN)    :: P_codeplante  !< // PARAMETER // Name code of the plant in 3 letters // * // PARPLT // 0
!!!       character(len=3),  intent(IN)    :: P_stoprac  !> // PARAMETER // stage when root growth stops (LAX or SEN) // * // PARPLT // 0
          integer,           intent(IN)    :: P_codesimul  !< // PARAMETER // simulation code (culture ou feuille=lai forcé) // SD // P_USM/USMXML // 0
          integer,           intent(IN)    :: P_stoprac  !< // PARAMETER // stage when root growth stops (LAX or SEN) // * // PARPLT // 0
          integer,           intent(IN)    :: P_codeplante  !< // PARAMETER // Name code of the plant in 3 letters // * // PARPLT // 0



          real,              intent(IN)    :: P_epc(nh)  !< // PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm
          real,              intent(IN)    :: P_obstarac  !< // PARAMETER // Soil depth which will block the root growth  // cm // PARSOL // 1
          real,              intent(IN)    :: P_daseuilbas  !< // PARAMETER // Threshold of bulk density of soil below that the root growth is not limited // g cm-3 // PARAM // 1
          real,              intent(IN)    :: P_daseuilhaut  !< // PARAMETER // Threshold of bulk density of soil below that the root growth  no more possible // g cm-3 // PARAM // 1
          real,              intent(IN)    :: P_dacohes  !< // PARAMETER // bulk density under which root growth is reduced due to a lack of cohesion d // g cm-3 // PARAM // 1
          real,              intent(IN)    :: P_zrac0  !< // PARAMETER // initial depth of root front  // cm // INIT // 1
          real,              intent(IN)    :: P_densinitial(nh)  !< // PARAMETER // Table of initial root density of the 5 horizons of soil for fine earth // cm cm-3 // INIT // 1
          integer,           intent(IN)    :: P_coderacine  !< // PARAMETER // Choice of estimation module of growth root in volume: standard profile (1) or by actual density (2) // code 1/2 // PARPLT // 0

          integer,           intent(IN)    :: P_codeperenne  !< // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0
          integer,           intent(IN)    :: P_codehypo  !< // PARAMETER // option of simulation of a  phase of hypocotyl growth (1) or planting of plantlets (2) // code 1/2 // PARPLT // 0
          integer,           intent(IN)    :: P_codegermin  !< // PARAMETER // option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2) // code 1/2 // PARPLT // 0
          real,              intent(IN)    :: P_zracplantule  !< // PARAMETER // depth of the initial root front of the plantlet  // cm // PARPLT // 1
          integer,           intent(IN)    :: P_codetemprac  !< // PARAMETER // option calculation mode of heat time for the root: with crop temperature (1)  or with soil temperature (2) // code 1/2 // PARPLT // 0
          real,              intent(IN)    :: P_tcmin  !< // PARAMETER // Minimum temperature of growth // degree C // PARPLT // 1
          real,              intent(IN)    :: P_tcmax  !< // PARAMETER // Maximum temperature of growth // degree C // PARPLT // 1
          real,              intent(IN)    :: P_tgmin  !< // PARAMETER // Minimum threshold temperature used in emergence stage // degree C // PARPLT // 1
          real,              intent(IN)    :: P_croirac  !< // PARAMETER // Growth rate of the root front  // cm degree.days-1 // PARPLT // 1
          real,              intent(IN)    :: P_contrdamax  !< // PARAMETER // maximal root growth reduction due to soil strenghtness (high bulk density) // SD // PARPLT // 1
          integer,           intent(IN)    :: P_codeindetermin  !< // PARAMETER // option of  simulation of the leaf growth and fruit growth : indeterminate (2) or determinate (1) // code 1/2 // PARPLT // 0
          real,              intent(IN)    :: P_sensanox  !< // PARAMETER // anoxia sensitivity (0=insensitive) // SD // PARPLT // 1
          integer,           intent(IN)    :: nplt  
          integer,           intent(IN)    :: nrec  
          integer,           intent(IN)    :: codeinstal  
          integer,           intent(IN)    :: nger  
          integer,           intent(IN)    :: nsen  
          integer,           intent(IN)    :: nlax  
          integer,           intent(IN)    :: nflo  
          integer,           intent(IN)    :: nmat  
          integer,           intent(IN)    :: nlev  
          integer,           intent(IN)    :: namf  
          real,              intent(IN)    :: izrac   !< // OUTPUT // Index of excess water stress on roots  // 0-1
          real,              intent(IN)    :: P_profsem  !< // PARAMETER // Sowing depth // cm // PARTEC // 1
          integer,           intent(IN)    :: P_codcueille  !< // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0
          real,              intent(IN)    :: deltai   !< // OUTPUT // Daily increase of the green leaf index // m2 leafs.m-2 soil
          real,              intent(IN)    :: lai   !< // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
          logical,           intent(IN)    :: sioncoupe  
          real,              intent(IN)    :: P_sensrsec  !< // PARAMETER // root sensitivity to drought (1=insensitive) // SD // PARPLT // 1
          integer,           intent(IN)    :: P_codedyntalle  !< // PARAMETER // Activation of the module simulating tiller dynamic: yes (1), no (2) // code 1/2 // PARAMV6/PLT // 0
          real,              intent(IN)    :: P_tcxstop  !< // PARAMETER // threshold temperature beyond which the foliar growth stops // degree C // PARPLT // 1
          real,              intent(IN)    :: dltams   !< // OUTPUT // Growth rate of the plant  // t ha-1.j-1


          integer,           intent(INOUT) :: nhe  
          integer,           intent(INOUT) :: nstoprac  
          real,              intent(INOUT) :: zrac   !< // OUTPUT // Depth reached by root system // cm
          real,              intent(INOUT) :: znonli     ! dans le doute, INOUT  
          real,              intent(INOUT) :: rl_veille(nbCouches) ! nger-1 ou n-1  
          real,              intent(INOUT) :: deltaz   !< // OUTPUT // Deepening of the root front  // cm day-1
          real,              intent(INOUT) :: dtj(n) !< 1 to n    // OUTPUT // Daily efficient temperature for the root growing  // degree C.j-1
          real,              intent(OUT)   :: cumlracz   !< // OUTPUT // Sum of the effective root lengths  // cm root.cm -2 soil
          real,              intent(OUT)   :: poussracmoy   !< // OUTPUT // "Average index of the effect of soil constraints on the rooting profile (option  true density )" // 0-1
          real,              intent(INOUT) :: lracsenz(nbCouches)  
          real,              intent(INOUT) :: tempeff   !< // OUTPUT // Efficient temperature for growth // degree C
 ! 11/06/2013 la variable efda devient une varaible de sortie journaliere pour Simtraces
          real,              intent(INOUT)   :: efda      !> // OUTPUT // efda defines the effect of soil compaction through bulk density // 0-1
          real,              intent(INOUT)   :: humirac_mean !> // OUTPUT // soil dryness // 0-1

end subroutine croissanceFrontRacinaire

!> This module calculates the root density profile according to the ‘true density’ option.
!>- Stics book paragraphe 5.2.2, page 90-94
!>- With this option, growth in root length is first calculated, and then distributed to each layer of the soil profile. For sown crops, this calculation begins
!! at emergence: between germination and emergence, it is assumed that only the root front grows. For transplanted or perennial crops, the calculation is
!! initiated with an existing root density profile. After a lifetime characteristic of the species, the roots senesce and enter the mineralization process as
!! crop residue at the end of the crop cycle. Root density above 0.5 cm.cm-3 is not taken into account for water and nitrogen absorption.
!! - Growth in root length
!!   To ensure the robustness of the model, we have chosen to simulate the growth in root length directly, without passing through the root mass, because
!!   the specific length (root length/mass ratio) varies depending on the stresses suffered by the plant.  Two options are available to calculate the root length.
!!   With the first option, we have adopted a formulation similar to that used for the above-ground growth of leaves (Brisson et al., 1998a). With the second, a
!!   trophic link between shoot growth and root growth allows increase in root length to be calculated.
!!   -  Self-governing production : growth in root length is calculated using a logistic function that is analogous to that of leaves.
!!      A first calculation of the root length growth rate describes a logistic curve. This value is then multiplied by the effective crop temperature,
!!      the plant density combined with an inter-plant competition factor that is characteristic for the variety, and the water logging stress index.
!!      Then a second term is added corresponding to the growth at the root front (nouvrac), depending on the front growth rate (deltaz).
!!      The logistic curve describing the root length growth rate depends on the maximum root growth parameter draclong and on the normalized root development
!!      unit urac, ranging from 1 to 3 (such as ulai) and is thermally driven, even when the plant has vernalisation or photoperiod requirements.
!!      The plant parameters pentlaimax and vlaimax are the ones already used for the calculation of leaf growth rate.
!!      The thermal function rlj relies on crop temperature and cardinal temperatures (tcmin and tcmax) which are the same values as for the leaf area growth
!!      calculation. The inter-plant competition function is the same as the one calculated for the leaf area growth. Unlike the leaf area index,
!!      water and nitrogen deficiencies in the plant do not play any role in root growth, which results in the promotion of root growth relative to
!!      above-ground growth in the event of stress.  In contrast, anoxia acts via the water-logging stress index derived from the anox indicator.
!!   -  Trophic-linked production : the root length growth may rely on the daily production of shoot biomass (dltams) and on a dynamic underground/total biomass
!!      partitioning coefficient (reprac). The parameter longsperac is the specific root length/root mass ratio. The plant density effect is not taken
!!      into account because it is already integrated in the shoot biomass production. This value can replace calculation or just act as a threshold according
!!      to the chosing option.
!! - Distribution in the profile
!!   The new root length is then distributed in each layer of the soil profile in proportion to the roots present and as a function of the soil constraints.
!!   A "root sink strength" is defined by the proportion of roots present in the layer. This does not concern the root front, whose growth in density is
!!   defined by lvfront. This potential “root sink strength” is then reduced by the soil constraints in each layer. Each constraint is defined at the layer level,
!!   in the form of an index between 0 and 1, and assumed to be independent of the others. The resulting index poussrac is the product of elementary indices:
!!   humirac defines the effect of soil dryness, taking account of the plant's sensitivity to this effect. efda defines the effect of soil compaction through
!!   bulk density. The anoxia index of each soil layer anox(iz) is assigned the value of 1 if the horizon has reached saturation; it is associated with the
!!   sensitivity of the plant to water logging sensanox. efnrac defines the effect of mineral nitrogen, which contributes to the root distribution in the
!!   layers with high mineral nitrogen content. It depends on the specific parameters minazorac, maxazorac and minefnra which characterize the sensitivity
!!   of plant root growth to the mineral nitrogen content in the soil. This last constraint is optional and can be inactivated in the model.
!! - Senescence
!!   A thermal duration in degree days (stdebsenrac) defines the lifespan of roots. Thus, the history of root production per layer is memorized in order
!!   to make disappear by senescence the portion of roots stdebsenrac set earlier. The profile of dead roots is lracsenz while the corresponding total
!!   amount is lracsentot.
!! - Root density profiles
!!   The living root density profile is rl, while the total amount is rltot. For water and nitrogen absorption, an efficient root length density (lracz)
!!   is calculated by applying the threshold lvopt (by default equals 0.5 cm cm-3) to the total root length density, RL.
!-------------------------------------------------
subroutine densiteRacinaire(                                                                                &
             n,nbCouches,anox,hur,nstoprac,humin,dacouche,P_daseuilbas,P_daseuilhaut,P_dacohes,P_lvopt,        &
             P_coderacine,P_contrdamax,P_sensanox,zrac,P_zlabour,P_zpente,P_zprlim,znonli,difrac,lai,tauxcouv, &
             nrec,codeinstal,nger,rl,deltaz,dtj,nlev,cumlracz,poussracmoy,lracsenz,cumflrac,racnoy,            &
             flrac,lracz,cumlraczmaxi,zracmax,P_profsem,P_codazorac,P_minazorac,P_maxazorac,P_minefnra,        &
             P_codtrophrac,P_coefracoupe,nit,amm,profsol,msrac,nsencourprerac,somtemprac,idzrac,               &
             efdensite_rac,ndebsenrac,precrac,drl,lracsentot,masectot,rltot,sioncoupe,P_sensrsec,P_stlevamf,       &
             P_stamflax,P_lvfront,P_laicomp,P_adens,P_bdens,P_draclong,P_vlaimax,P_longsperac,P_debsenrac,     &
             P_codedyntalle,drlsenmortalle,densite,P_msresiduel,lai_veille,msrac_veille,rl_veille,dltams,      &
             repracmin,repracmax,kreprac,efda,efnrac_mean,humirac_mean,humirac_z,efnrac_z,rlj,masec,           &
             P_codemortalracine,dltmsrac_plante)


          implicit none

          integer,           intent(IN)    :: n  
          integer,           intent(IN)    :: nbCouches  
          real,              intent(IN)    :: anox(nbCouches)  
          real,              intent(IN)    :: hur(nbCouches)  
          integer,           intent(INOUT) :: nstoprac  
          real,              intent(IN)    :: humin(nbCouches)  
          real,              intent(IN)    :: dacouche(nbCouches)  
          real,              intent(IN)    :: P_daseuilbas  !< // PARAMETER // Threshold of bulk density of soil below that the root growth is not limited // g cm-3 // PARAM // 1
          real,              intent(IN)    :: P_daseuilhaut  !< // PARAMETER // Threshold of bulk density of soil below that the root growth  no more possible // g cm-3 // PARAM // 1
          real,              intent(IN)    :: P_dacohes  !< // PARAMETER // bulk density under which root growth is reduced due to a lack of cohesion d // g cm-3 // PARAM // 1
          real,              intent(IN)    :: P_lvopt  !< // PARAMETER // Optimum root density // cm root.cm-3 soil // PARAM // 1
          integer,           intent(IN)    :: P_coderacine  !< // PARAMETER // Choice of estimation module of growth root in volume: standard profile (1) or by actual density (2) // code 1/2 // PARPLT // 0
          real,              intent(IN)    :: P_contrdamax  !< // PARAMETER // maximal root growth reduction due to soil strenghtness (high bulk density) // SD // PARPLT // 1
          real,              intent(IN)    :: P_sensanox  !< // PARAMETER // anoxia sensitivity (0=insensitive) // SD // PARPLT // 1
          real,              intent(INOUT) :: zrac   !< // OUTPUT // Depth reached by root system // cm
          real,              intent(IN)    :: P_zlabour  !< // PARAMETER // Depth of ploughing  // cm // PARPLT // 1
          real,              intent(IN)    :: P_zpente  !< // PARAMETER // Depth where the root density is ½ of the surface root density for the reference profile // cm  // PARPLT // 1
          real,              intent(IN)    :: P_zprlim  !< // PARAMETER // Maximum depth of the root profile for the reference profile // cm  // PARPLT // 1
          real,              intent(INOUT) :: znonli            ! dans le doute, INOUT  
          real,              intent(OUT)   :: difrac  
          real,              intent(IN)    :: lai   !< // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
          real,              intent(IN)    :: tauxcouv   !< // OUTPUT // Cover rate // SD
          integer,           intent(IN)    :: nrec  
          integer,           intent(IN)    :: codeinstal  
          integer,           intent(IN)    :: nger  
          real,              intent(INOUT) :: rl(nbCouches)     !< n-1 (0) & n (1)
          real,              intent(INOUT) :: deltaz   !< // OUTPUT // Deepening of the root front  // cm day-1
          real,              intent(INOUT) :: dtj(n)            !< 1 to n    // OUTPUT // Daily efficient temperature for the root growing  // degree C.j-1
          integer,           intent(IN)    :: nlev  
          real,              intent(OUT)   :: cumlracz   !< // OUTPUT // Sum of the effective root lengths  // cm root.cm -2 soil
          real,              intent(OUT)   :: poussracmoy   !< // OUTPUT // "Average index of the effect of soil constraints on the rooting profile (option  true density )" // 0-1
          real,              intent(INOUT) :: lracsenz(nbCouches)  
          real,              intent(OUT)   :: cumflrac  
          real,              intent(INOUT) :: racnoy  
          real,              intent(INOUT) :: flrac(nbCouches)  
          real,              intent(INOUT) :: lracz(nbCouches)  
          real,              intent(OUT)   :: cumlraczmaxi  
          real,              intent(INOUT) :: zracmax  
          real,              intent(IN)    :: P_profsem  !< // PARAMETER // Sowing depth // cm // PARTEC // 1
          integer,           intent(IN)    :: P_codazorac  !< // PARAMETER // activation of the nitrogen influence on root partitionning within the soil profile  // code 1/2 // PARPLT // 0
          real,              intent(IN)    :: P_minazorac  !< // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning // kg N ha-1 mm-1 // PARPLT // 1
          real,              intent(IN)    :: P_maxazorac  !< // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning  // kg N ha-1 mm-1 // PARPLT // 1
          real,              intent(IN)    :: P_minefnra  !< // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning // SD // PARPLT // 1
          integer,           intent(IN)    :: P_codtrophrac  !< // PARAMETER // trophic effect on root partitioning within the soil // code 1/2/3 // PARPLT // 0
          real,              intent(IN)    :: P_coefracoupe  !< // PARAMETER // coefficient to define the proportion of dying roots after cut (grass) // SD // PARAMV6/PLT // 1
          real,              intent(IN)    :: nit(nbCouches)  
          real,              intent(IN)    :: amm(nbCouches)  
          real,              intent(IN)    :: profsol  
          real,              intent(INOUT) :: msrac                     !< n    // OUTPUT // Estimated dry matter of the roots // t.ha-1
          integer,           intent(INOUT) :: nsencourprerac  
          real,              intent(INOUT) :: somtemprac  
          real,              intent(IN)    :: idzrac  
          real,              intent(OUT)   :: efdensite_rac
          integer,           intent(INOUT) :: ndebsenrac  
          real,              intent(IN)    :: precrac(nbCouches)  
          real,              intent(INOUT) :: drl(n,nbCouches)  
          real,              intent(OUT)   :: lracsentot   !< // OUTPUT // Total length of senescent roots  // cm root.cm -2 soil
          real,              intent(IN)    :: masectot  
          real,              intent(IN)    :: masec
          real,              intent(OUT)   :: rltot   !< // OUTPUT // Total length of roots  // cm root.cm -2 soil
          logical,           intent(IN)    :: sioncoupe  
          real,              intent(IN)    :: P_sensrsec  !< // PARAMETER // root sensitivity to drought (1=insensitive) // SD // PARPLT // 1
          real,              intent(IN)    :: P_stlevamf  !< // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1
          real,              intent(IN)    :: P_stamflax  !< // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1
          real,              intent(IN)    :: P_lvfront  !< // PARAMETER // Root density at the root front // cm root.cm-3 soil // PARPLT // 1
          real,              intent(IN)    :: P_laicomp  !< // PARAMETER // LAI from which starts competition inbetween plants // m2 m-2 // PARPLT // 1
          real,              intent(IN)    :: P_adens  !< // PARAMETER // Interplant competition parameter // SD // PARPLT // 1
          real,              intent(IN)    :: P_bdens  !< // PARAMETER // minimal density from which interplant competition starts // plants m-2 // PARPLT // 1
          real,              intent(IN)    :: P_draclong  !< // PARAMETER // Maximum rate of root length production // cm root plant-1 degree.days-1 // PARPLT // 1
          real,              intent(IN)    :: P_vlaimax  !< // PARAMETER // ULAI  at inflection point of the function DELTAI=f(ULAI) // SD // PARPLT // 1
          real,              intent(IN)    :: P_longsperac  !< // PARAMETER // specific root length // cm g-1 // PARPLT // 1
          real,              intent(IN)    :: P_debsenrac  !< // PARAMETER // Sum of degrees.days defining the beginning of root senescence (life-time of a root) // degree.days // PARPLT // 1
          integer,           intent(IN)    :: P_codedyntalle  !< // PARAMETER // Activation of the module simulating tiller dynamic: yes (1), no (2) // code 1/2 // PARAMV6/PLT // 0
          real,              intent(IN)    :: drlsenmortalle   !< // OUTPUT // Root biomass corresponding to dead tillers // t ha-1.j-1
          real,              intent(IN)    :: densite   !< // OUTPUT // Actual sowing density // plants.m-2
          real,              intent(IN)    :: P_msresiduel  !< // PARAMETER // Residual dry matter after a cut // t ha-1 // PARTEC // 1
          real,              intent(IN)    :: lai_veille                ! <lai(n-1)
          real,              intent(IN)    :: msrac_veille              ! <msrac(n-1)
          real,              intent(IN)    :: rl_veille(nbCouches)      !< rl(n-1)
          real,              intent(IN)    :: dltams   !< // OUTPUT // Growth rate of the plant  // t ha-1.j-1
          real,              intent(IN)    :: repracmin  
          real,              intent(IN)    :: repracmax  
          real,              intent(IN)    :: kreprac  
 ! 11/06/2013 la variable efda devient une varaible de sortie journaliere pour Simtraces
          real,              intent(INOUT)   :: efda      !> // OUTPUT // efda defines the effect of soil compaction through bulk density // 0-1
          real,              intent(INOUT)   :: efnrac_mean    !> // OUTPUT // effect of mineral nitrogen, which contributes to the root distribution in the layers with high mineral nitrogen content. // 0-1
          real,              intent(INOUT)   :: humirac_mean !> // OUTPUT // soil dryness // 0-1
          real,              intent(INOUT)   :: humirac_z(nbcouches)
          real,              intent(INOUT)   :: efnrac_z(nbcouches)
!DR 24/07/2013 ajout de rlj en sortie
          real,              intent(OUT)     :: rlj !> // OUTPUT // roots length growth rate  // m.d-1
    ! DR 06/05/2015 je rajoute un code pouyr tester la mortalitéé des racines
       integer,              intent(IN)      :: P_codemortalracine !< // PARAMETER // masec servant a calculer les racines mortes a la coupe  : masec (1), masectot (2) // code 1/2 //PARAMv6 // 1
          real,              intent(OUT)     ::  dltmsrac_plante  !<// OUTPUT // pour sorties ArchiSTICS: biomasse journaliere allouee aux racines en g / m²sol / plante



end subroutine densiteRacinaire
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module calculates the root density profile according to the ‘true density’ option.
!!
!> - Stics book paragraphe 5.2.2, page 90-94
!>
!! With this option, growth in root length is first calculated, and then distributed to each layer of the soil profile. For sown crops, this calculation begins
!! at emergence: between germination and emergence, it is assumed that only the root front grows. For transplanted or perennial crops, the calculation is
!! initiated with an existing root density profile. After a lifetime characteristic of the species, the roots senesce and enter the mineralization process as
!! crop residue at the end of the crop cycle. Root density above 0.5 cm.cm-3 is not taken into account for water and nitrogen absorption.
!>  - Growth in root length
!!   To ensure the robustness of the model, we have chosen to simulate the growth in root length directly, without passing through the root mass, because
!!   the specific length (root length/mass ratio) varies depending on the stresses suffered by the plant.  Two options are available to calculate the root length.
!!   With the first option, we have adopted a formulation similar to that used for the above-ground growth of leaves (Brisson et al., 1998a). With the second, a
!!   trophic link between shoot growth and root growth allows increase in root length to be calculated.
!>       - Self-governing production : growth in root length is calculated using a logistic function that is analogous to that of leaves.
!!      A first calculation of the root length growth rate describes a logistic curve. This value is then multiplied by the effective crop temperature,
!!      the plant density combined with an inter-plant competition factor that is characteristic for the variety, and the water logging stress index.
!!      Then a second term is added corresponding to the growth at the root front (nouvrac), depending on the front growth rate (deltaz).
!!      The logistic curve describing the root length growth rate depends on the maximum root growth parameter draclong and on the normalized root development
!!      unit urac, ranging from 1 to 3 (such as ulai) and is thermally driven, even when the plant has vernalisation or photoperiod requirements.
!!      The plant parameters pentlaimax and vlaimax are the ones already used for the calculation of leaf growth rate.
!!      The thermal function rlj relies on crop temperature and cardinal temperatures (tcmin and tcmax) which are the same values as for the leaf area growth
!!      calculation. The inter-plant competition function is the same as the one calculated for the leaf area growth. Unlike the leaf area index,
!!      water and nitrogen deficiencies in the plant do not play any role in root growth, which results in the promotion of root growth relative to
!!      above-ground growth in the event of stress.  In contrast, anoxia acts via the water-logging stress index derived from the anox indicator.
!>       - Trophic-linked production : the root length growth may rely on the daily production of shoot biomass (dltams) and on a dynamic underground/total biomass
!!      partitioning coefficient (reprac). The parameter longsperac is the specific root length/root mass ratio. The plant density effect is not taken
!!      into account because it is already integrated in the shoot biomass production. This value can replace calculation or just act as a threshold according
!!      to the chosing option.
!>  - Distribution in the profile
!!   The new root length is then distributed in each layer of the soil profile in proportion to the roots present and as a function of the soil constraints.
!!   A "root sink strength" is defined by the proportion of roots present in the layer. This does not concern the root front, whose growth in density is
!!   defined by lvfront. This potential “root sink strength” is then reduced by the soil constraints in each layer. Each constraint is defined at the layer level,
!!   in the form of an index between 0 and 1, and assumed to be independent of the others. The resulting index poussrac is the product of elementary indices:
!!   humirac defines the effect of soil dryness, taking account of the plant's sensitivity to this effect. efda defines the effect of soil compaction through
!!   bulk density. The anoxia index of each soil layer anox(iz) is assigned the value of 1 if the horizon has reached saturation; it is associated with the
!!   sensitivity of the plant to water logging sensanox. efnrac defines the effect of mineral nitrogen, which contributes to the root distribution in the
!!   layers with high mineral nitrogen content. It depends on the specific parameters minazorac, maxazorac and minefnra which characterize the sensitivity
!!   of plant root growth to the mineral nitrogen content in the soil. This last constraint is optional and can be inactivated in the model.
!>  - Senescence
!!   A thermal duration in degree days (stdebsenrac) defines the lifespan of roots. Thus, the history of root production per layer is memorized in order
!!   to make disappear by senescence the portion of roots stdebsenrac set earlier. The profile of dead roots is lracsenz while the corresponding total
!!   amount is lracsentot.
!>  - Root density profiles
!!   The living root density profile is rl, while the total amount is rltot. For water and nitrogen absorption, an efficient root length density (lracz)
!!   is calculated by applying the threshold lvopt (by default equals 0.5 cm cm-3) to the total root length density, RL.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine densiteVraieRacinaire(n,nbCouches,dacouche,hur,humin,anox,nstoprac,P_codazorac,            &
                                 P_minazorac,P_maxazorac,P_minefnra,P_codtrophrac,P_coefracoupe,          &
                                 P_daseuilbas,P_daseuilhaut,P_dacohes,nit,amm,P_lvopt,profsol,nrec,       &
                                 msrac,nsencourprerac,nger,nlev,codeinstal,poussracmoy,zrac,              &
                                 rl,somtemprac,dtj,deltaz,idzrac,efdensite_rac,ndebsenrac,                    &
                                 precrac,drl,lracz,lracsenz,cumlracz,cumflrac,flrac,                      &
                                 cumlraczmaxi,racnoy,lracsentot,masectot,rltot,sioncoupe,                 &
                                 P_contrdamax,P_sensrsec,P_sensanox,P_stlevamf,P_stamflax,P_lvfront,      &
                                 P_laicomp,P_adens,P_bdens,P_draclong,P_vlaimax,P_longsperac,P_debsenrac, &
                                 P_codedyntalle,drlsenmortalle,P_profsem,densite,P_msresiduel,            &
                                 lai_veille,msrac_veille,rl_veille,dltams,repracmin,                      &
                                 repracmax,kreprac,efda,efnrac_mean,humirac_mean,humirac_z,efnrac_z,rlj,  &
                                 masec,P_codemortalracine,dltmsrac_plante)

USE Divers, only: F_humirac, escalin
USE Messages

  implicit none


  integer, intent(IN)    :: n
  integer, intent(IN)    :: nbCouches
  real,    intent(IN)    :: dacouche(nbCouches)
  real,    intent(IN)    :: hur(nbCouches)
  real,    intent(IN)    :: humin(nbCouches)
  real,    intent(IN)    :: anox(nbCouches)
  integer, intent(IN)    :: nstoprac

  integer, intent(IN)    :: P_codazorac  !< // PARAMETER // activation of the nitrogen influence on root partitionning within the soil profile  // code 1/2 // PARPLT // 0
  real,    intent(IN)    :: P_minazorac  !< // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning // kg N ha-1 mm-1 // PARPLT // 1
  real,    intent(IN)    :: P_maxazorac  !< // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning  // kg N ha-1 mm-1 // PARPLT // 1
  real,    intent(IN)    :: P_minefnra  !< // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning // SD // PARPLT // 1
  integer, intent(IN)    :: P_codtrophrac  !< // PARAMETER // trophic effect on root partitioning within the soil // code 1/2/3 // PARPLT // 0
  real,    intent(IN)    :: P_coefracoupe  !< // PARAMETER // coefficient to define the proportion of dying roots after cut (grass) // SD // PARAMV6/PLT // 1

  real,    intent(IN)    :: P_daseuilbas  !< // PARAMETER // Threshold of bulk density of soil below that the root growth is not limited // g cm-3 // PARAM // 1
  real,    intent(IN)    :: P_daseuilhaut  !< // PARAMETER // Threshold of bulk density of soil below that the root growth  no more possible // g cm-3 // PARAM // 1
  real,    intent(IN)    :: P_dacohes  !< // PARAMETER // bulk density under which root growth is reduced due to a lack of cohesion d // g cm-3 // PARAM // 1
  real,    intent(IN)    :: nit(nbCouches)
  real,    intent(IN)    :: amm(nbCouches)
  real,    intent(IN)    :: P_lvopt  !< // PARAMETER // Optimum root density // cm root.cm-3 soil // PARAM // 1
  real,    intent(IN)    :: profsol

  integer, intent(IN)    :: nrec
  real,    intent(INOUT) :: msrac                       !< n    // OUTPUT // Estimated dry matter of the roots // t.ha-1
  integer, intent(INOUT) :: nsencourprerac
  integer, intent(IN)    :: nger
  integer, intent(IN)    :: nlev
  integer, intent(IN)    :: codeinstal
  real,    intent(OUT)   :: poussracmoy   !< // OUTPUT // "Average index of the effect of soil constraints on the rooting profile (option  true density )" // 0-1
  real,    intent(INOUT) :: zrac   !< // OUTPUT // Depth reached by root system // cm
  real,    intent(INOUT) :: rl(nbCouches)               !< n
  real,    intent(INOUT) :: somtemprac
  real,    intent(IN)    :: dtj(n)                      !< 1 to n    // OUTPUT // Daily efficient temperature for the root growing  // degree C.j-1
  real,    intent(IN)    :: deltaz   !< // OUTPUT // Deepening of the root front  // cm day-1
  real,    intent(IN)    :: idzrac
  real,    intent(OUT)   :: efdensite_rac
  integer, intent(INOUT) :: ndebsenrac
  real,    intent(IN)    :: precrac(nbCouches)
  real,    intent(INOUT) :: drl(n,nbCouches)
  real,    intent(INOUT) :: lracz(nbCouches)
  real,    intent(INOUT) :: lracsenz(nbCouches)
  real,    intent(OUT)   :: cumlracz   !< // OUTPUT // Sum of the effective root lengths  // cm root.cm -2 soil
  real,    intent(OUT)   :: cumflrac
  real,    intent(INOUT) :: flrac(nbCouches)
  real,    intent(OUT)   :: cumlraczmaxi
  real,    intent(INOUT) :: racnoy
  real,    intent(OUT)   :: lracsentot   !< // OUTPUT // Total length of senescent roots  // cm root.cm -2 soil
  real,    intent(IN)    :: masectot
  real,    intent(IN)    :: masec

  real,    intent(OUT)   :: rltot   !< // OUTPUT // Total length of roots  // cm root.cm -2 soil
  logical, intent(IN)    :: sioncoupe

  real,    intent(IN)    :: P_contrdamax  !< // PARAMETER // maximal root growth reduction due to soil strenghtness (high bulk density) // SD // PARPLT // 1
  real,    intent(IN)    :: P_sensrsec  !< // PARAMETER // root sensitivity to drought (1=insensitive) // SD // PARPLT // 1
  real,    intent(IN)    :: P_sensanox  !< // PARAMETER // anoxia sensitivity (0=insensitive) // SD // PARPLT // 1
  real,    intent(IN)    :: P_stlevamf  !< // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1
  real,    intent(IN)    :: P_stamflax  !< // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1
  real,    intent(IN)    :: P_lvfront  !< // PARAMETER // Root density at the root front // cm root.cm-3 soil // PARPLT // 1
  real,    intent(IN)    :: P_laicomp  !< // PARAMETER // LAI from which starts competition inbetween plants // m2 m-2 // PARPLT // 1
  real,    intent(IN)    :: P_adens  !< // PARAMETER // Interplant competition parameter // SD // PARPLT // 1
  real,    intent(IN)    :: P_bdens  !< // PARAMETER // minimal density from which interplant competition starts // plants m-2 // PARPLT // 1
  real,    intent(IN)    :: P_draclong  !< // PARAMETER // Maximum rate of root length production // cm root plant-1 degree.days-1 // PARPLT // 1
  real,    intent(IN)    :: P_vlaimax  !< // PARAMETER // ULAI  at inflection point of the function DELTAI=f(ULAI) // SD // PARPLT // 1
  real,    intent(IN)    :: P_longsperac  !< // PARAMETER // specific root length // cm g-1 // PARPLT // 1
  real,    intent(IN)    :: P_debsenrac  !< // PARAMETER // Sum of degrees.days defining the beginning of root senescence (life-time of a root) // degree.days // PARPLT // 1
  integer, intent(IN)    :: P_codedyntalle  !< // PARAMETER // Activation of the module simulating tiller dynamic: yes (1), no (2) // code 1/2 // PARAMV6/PLT // 0
  real,    intent(IN)    :: drlsenmortalle   !< // OUTPUT // Root biomass corresponding to dead tillers // t ha-1.j-1

  real,    intent(IN)    :: P_profsem  !< // PARAMETER // Sowing depth // cm // PARTEC // 1
  real,    intent(IN)    :: densite   !< // OUTPUT // Actual sowing density // plants.m-2
  real,    intent(IN)    :: P_msresiduel  !< // PARAMETER // Residual dry matter after a cut // t ha-1 // PARTEC // 1

  real,    intent(IN)    :: lai_veille                  !< lai(n-1)
  real,    intent(IN)    :: msrac_veille                !< msrac(n-1)
  real,    intent(IN)    :: rl_veille(nbCouches)        !< n-1
  real,    intent(IN)    :: dltams   !< // OUTPUT // Growth rate of the plant  // t ha-1.j-1

  real,    intent(IN)    :: repracmin
  real,    intent(IN)    :: repracmax
  real,    intent(IN)    :: kreprac
! dr 11/06/2013 j'ajoute efda en variable de sortie
  real,    intent(INOUT)   :: efda      !> // OUTPUT // efda defines the effect of soil compaction through bulk density // 0-1
  real,    intent(INOUT)   :: efnrac_mean    !> // OUTPUT // effect of mineral nitrogen, which contributes to the root distribution in the layers with high mineral nitrogen content. // 0-1
  real,    intent(INOUT)   :: humirac_mean !> // OUTPUT // soil dryness // 0-1
  real,    intent(INOUT)   :: humirac_z(nbcouches)
  real,    intent(INOUT)   :: efnrac_z(nbcouches)
  real,    intent(OUT)     :: rlj !> // OUTPUT // roots length growth rate  // m.d-1
    ! DR 06/05/2015 je rajoute un code pouyr tester la mortalitéé des racines
  integer, intent(IN)      :: P_codemortalracine !< // PARAMETER // masec servant a calculer les racines mortes a la coupe  : masec (1), masectot (2) // code 1/2 //PARAMv6 // 1
  real,    intent(OUT)     ::  dltmsrac_plante  !<// OUTPUT // pour sorties ArchiSTICS: biomasse journaliere allouee aux racines en g / m²sol / plante


end subroutine densiteVraieRacinaire

!> There are models for which allocation of assimilates is critical to the operation of the model (e.g. SUCROS described by Van Ittersum et al., 2003).
!>- Stics book paragraphe 3,5, page 68-71
!>
!> In STICS this module was added at a late stage, mainly to help dimensioning the reserve pool. For annual plants with determinate growth, the partitioning
!! calculations simply allow the dimensioning of envelopes of harvested organs which may play a trophic role and ensure an input of information for the
!! senescence module. For perennial plants or those with indeterminate growth, those calculations enable the dimensioning of a compartment for reserves which
!! are recycled in the carbon balance. The calculation of root biomass is not directly connected to that of the above-ground biomass.
!> - Organs and compartments identified: the reasons for identifying an organ or a compartment are either its internal trophic role within the plant or an
!!   external role by participation in the nitrogen balance of the system (such as falling leaves and the recycling of roots). The reserve compartment is not
!!   located in a specific organ: it is just a certain quantity of carbon available for the plant growth.
!> - Dimensioning of organs:
!!   Green leaves: The biomass of green leaves is calculated without accounting for potential reserves that may be stored in the leaves and remobilized later on,
!!   which are accounted for in the resperenne non-located reserve pool. The mafeuilverte variable is deducted from the LAI, based on the
!!   maximum specific leaf area variable (slamax). We assume that the difference between the actual sla and slamax corresponds to remobilized leaf carbon
!!   Yellow leaves: The biomass of yellow leaves (mafeuiljaune) is calculated in the senescence module.  The proportion of leaves in the senescent biomass
!!   on a given day (dltamsen) is determined using the pfeuilverte ratio (proportion of green leaves in the non-senescent biomass) on the day of production
!!   of this senescent biomass. Some of these yellow leaves may fall to the ground depending on the abscission  parameter (between 0 and 1). The daily falling
!!   quantity (dltamstombe) is recycled in the nitrogen balance; its cumulative value is mafeuiltombe.
!!   Stems: this concerns only the structural component of stems (matigestruc). The non-structural component, if significant, can be included in the reserve
!!   compartment (e.g. for cereals) or in the harvested part (sugar cane).  The matigestruc variable is calculated as a constant proportion (tigefeuille of the
!!   total mass of foliage. For monocotyledonous plants, the stem is secondary and the matigestruc variable is only incremented from the time when accumulated
!!   biomass so allows.  It is thus assumed that the first organs to emerge are the leaves. For dicotyledonous plants, it is assumed that the tigefeuille
!!   proportionality is always respected.  Consequently, if the accumulated biomass and the foliage biomass (calculated from the LAI and SLA) are incompatible
!!   with this proportionality, then the SLA (or LAI if the SLA arises from fixed limits) is recalculated. The matigestruc variable cannot diminish,
!!   except in the case of cutting fodder crops.
!!   Harvested organs:
!>   - Fruits and grains: the calculation of the number and mass of fruits (indeterminate plants) or seeds (determinate plants) is achieved in modules
!!       fruit.f90 and grain.f90.
!>   - Envelops of harvested organs (pods, raches, etc.): the mass corresponding to the envelope is assumed to depend solely upon the number of organs.
!!       In any case, it cannot exceed the residual biomass (masecveg - mafeuil - matigestruc). The envfruit parameter corresponds to the proportion of
!!       membrane related to the maximum weight of the fruit. If the sea parameter is not zero, then this biomass is transformed into an equivalent
!!       leaf surface area, photosynthetically active from the IDRP stage to the IDEBDES stage.
!>
!!   Reserves (resperenne) are calculated as the difference between the total biomass and the accumulated biomass of leaves, stems and harvested organs.
!!   For perennial plants, at the beginning of the cropping season, the reserves (carbon) can be initialised at a non-zero value (resperenne0), so as to
!!   represent the role played by root reserves at the resumption of growth. Yet it is assumed that a limit exists to the size of the reserve compartment,
!!   parametrized at the plant level by resplmax. If this limlit is reached a “sink on source” effect is simulated. The use of reserves concerns perennial plants
!!   or indeterminate plants.  As for determinate annuals, the use of reserves for grain filling is not simulated as such, but taken globally into account
!!   when calculating the ercarb variable (index of progressive harvest).
!-----------------------------------------------------------------------------
subroutine repartir(n,nrec,P_codcueille,P_codeperenne,nlev,nlax,P_nbcueille,numcult,tustress,P_slamin, & !IN
                    P_slamax,P_codlainet,P_codemonocot,P_codesimul,dltaisen,P_tigefeuil,P_envfruit,        &
                    chargefruit,ndrp,ndebdes,P_sea,ntaille,P_codetaille,P_codeinitprec,dltams,lai_veille,  &
                    resperenne,masecveg,pdsfruittot,tursla,sla,mafeuilverte,mafeuil,mafeuilp,lai,deltai,   & !INOUT
                    maenfruit,eai,mareserve,deltares,mabois,P_resperenne0,masec,msresjaune,mafeuiljaune,   &
                    msneojaune,matigestruc,pfeuil,pfeuilverte,pfeuiljaune,ptigestruc,penfruit,preserve)

          USE Messages

          implicit none

        !: Arguments

          integer, intent(IN)    :: n  
          integer, intent(IN)    :: nrec  
          integer, intent(IN)    :: P_codcueille  !< // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0
          integer, intent(IN)    :: P_codeperenne  !< // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0
          integer, intent(IN)    :: nlev  
          integer, intent(IN)    :: nlax  
          integer, intent(IN)    :: P_nbcueille  !< // PARAMETER // number of fruit harvestings // code 1/2 // PARTEC // 0
          integer, intent(IN)    :: numcult  
          real,    intent(IN)    :: tustress   !< // OUTPUT // Stress index active on leaf growth (= minimum(turfac,innlai))  // 0-1
          real,    intent(IN)    :: P_slamin  !< // PARAMETER // minimal SLA of green leaves // cm2 g-1 // PARPLT // 1
          real,    intent(IN)    :: P_slamax  !< // PARAMETER // maximal SLA of green leaves // cm2 g-1 // PARPLT // 1
          integer, intent(IN)    :: P_codlainet  !< // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
          integer, intent(IN)    :: P_codemonocot  !< // PARAMETER // option plant monocot(1) or dicot(2) // code 1/2 // PARPLT // 0
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!          character(len=12), intent(IN) :: P_codesimul  !< // PARAMETER // simulation code (culture ou feuille=lai forcé) // SD // P_USM/USMXML // 0
          integer, intent(IN)    :: P_codesimul  !< // PARAMETER // simulation code (culture ou feuille=lai forcé) // SD // P_USM/USMXML // 0
          real,    intent(IN)    :: dltaisen   !< // OUTPUT // Daily increase of the senescent leaf index // m2.m-2 sol.j-1
          real,    intent(IN)    :: P_tigefeuil  !< // PARAMETER // stem (structural part)/leaf proportion // SD // PARPLT // 1
          real,    intent(IN)    :: P_envfruit  !< // PARAMETER // proportion envelop/P_pgrainmaxi in weight  // SD // PARPLT // 1
          real,    intent(IN)    :: chargefruit   !< // OUTPUT // Amount of filling fruits per plant // nb fruits.plant-1
          integer, intent(IN)    :: ndrp  
          integer, intent(IN)    :: ndebdes  
          real,    intent(IN)    :: P_sea  !< // PARAMETER // specifique surface of fruit envelops // cm2 g-1 // PARPLT // 1
          integer, intent(IN)    :: ntaille  
          integer, intent(IN)    :: P_codetaille  !< // PARAMETER // option of pruning // code 1/2 // PARTEC // 0
          integer, intent(IN)    :: P_codeinitprec  !< // PARAMETER // reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0
          real,    intent(IN)    :: dltams   !< // OUTPUT // Growth rate of the plant  // t ha-1.j-1
          real,    intent(IN)    :: lai_veille              ! n-1  

          real,    intent(INOUT) :: resperenne   !< // OUTPUT // C crop reserve, during the cropping season, or during the intercrop period (for perenial crops) // t ha-1
          real,    intent(INOUT) :: masecveg   !< // OUTPUT // Vegetative dry matter // t.ha-1
          real,    intent(INOUT) :: pdsfruittot  
          real,    intent(INOUT) :: tursla  
          real,    intent(INOUT) :: sla   !< // OUTPUT // Specific surface area // cm2 g-1
          real,    intent(INOUT) :: mafeuilverte   !< // OUTPUT // Dry matter of green leaves // t.ha-1
          real,    intent(INOUT) :: mafeuil   !< // OUTPUT // Dry matter of leaves // t.ha-1
          real,    intent(INOUT) :: mafeuilp  
          real,    intent(INOUT) :: matigestruc   !< // OUTPUT // Dry matter of stems (only structural parts) // t.ha-1
          real,    intent(INOUT) :: lai                     ! n    // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
          real,    intent(INOUT) :: deltai   !< // OUTPUT // Daily increase of the green leaf index // m2 leafs.m-2 soil
          real,    intent(INOUT) :: maenfruit   !< // OUTPUT // Dry matter of harvested organ envelopes // t.ha-1
          real,    intent(INOUT) :: eai  
          real,    intent(INOUT) :: mareserve  
          real,    intent(INOUT) :: deltares  
          real,    intent(INOUT) :: mabois   !< // OUTPUT // Prunning dry weight // t.ha-1
          real,    intent(INOUT) :: P_resperenne0  !< // PARAMETER // initial reserve biomass // t ha-1 // INIT // 1
          real,    intent(INOUT) :: masec   !< // OUTPUT // Aboveground dry matter  // t.ha-1
          real,    intent(INOUT) :: msresjaune   !< // OUTPUT // Senescent residual dry matter  // t.ha-1
          real,    intent(INOUT) :: mafeuiljaune   !< // OUTPUT // Dry matter of yellow leaves // t.ha-1
          real,    intent(INOUT) :: msneojaune   !< // OUTPUT // Newly-formed senescent dry matter  // t.ha-1
          real,    intent(INOUT) :: pfeuil   !< // OUTPUT // Proportion of leaves in total biomass // 0-1
          real,    intent(INOUT) :: pfeuilverte   !< // OUTPUT // Proportion of green leaves in total non-senescent biomass // 0-1
          real,    intent(INOUT) :: pfeuiljaune   !< // OUTPUT // Proportion of yellow leaves in total biomass // 0-1
          real,    intent(INOUT) :: ptigestruc   !< // OUTPUT // Proportion of structural stems in total biomass // 0-1
          real,    intent(INOUT) :: penfruit   !< // OUTPUT // Proportion of fruit envelopes in total biomass // 0-1
          real,    intent(INOUT) :: preserve   !< // OUTPUT // Proportion of reserve in the total biomass // 0-1

end subroutine repartir



end interface
end module Module_Croissance
 
 
