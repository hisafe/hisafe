! *******************
! *-  sous-programme de croissance
! *-  version 6.x   le 09/05/2005
! *-  intégration des réserves  hivernales
! *-  P_remobres = proportion de réserves mobilisables
! *-             par rapport à la croissance
! *-             à relier à la quantité totale de réserve
! *-  P_resperenne0 = réserves en t/ha
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
                   P_efcroijuv,P_efcroiveg,ndrp,P_efcroirepro,chargefruit,P_coefb,tcult,P_teopt,  &
                   P_teoptbis,P_temin,P_temax,P_codeclichange,P_alphaco2,co2,P_resplmax,densite,    &
                   P_codeh2oact,swfac,exobiom,P_codeinnact,dltaremobil,P_codeindetermin,fpv,  &
                   fpft,P_remobres,P_resperenne0,demande,P_QNplante0,P_msresiduel,P_nbcueille,    &
                   rdtint,CNgrain,P_codemontaison,nmontaison,                             &
                   masec_veille,masec,QNplante_veille,QNplante,inns,inn,innlai,         &
                   cumdltaremobilN,ebmax,ftemp,epsib,fco2,dltams,dltamsen,dltamstombe,  &
                   resperenne,dltamsN,photnet,sourcepuits,dltaremobilN,remobilj,        &
                   cumdltares,magrain_veille,magrain,masecneo,surface,surfaceSous,      &
                   P_nbplantes,P_extin,cumrg,cumraint,fapar,delta,P_adfol,lairognecum,        &
                   laieffcum,P_dfolbas,P_dfolhaut,dfol,rdif,parapluie,raint,P_parsurrg,P_forme, &
                   lai,laisen,eai,P_interrang,nlax,nsen,P_codlainet,P_hautbase,P_codepalissage, &
                   P_hautmaxtec,P_largtec,originehaut,hauteur,deltahauteur,P_hautmax,         &
                   varrapforme,largeur,jul,trg,P_latitude,rombre,rsoleil,P_orientrang,      &
                   P_ktrou,tauxcouv,P_khaut,surfAO,surfAS,fpari)

  implicit none

!: Arguments

! IN
  integer, intent(IN)    :: ipl  
  integer, intent(IN)    :: ens  
  integer, intent(IN)    :: nlev  
  integer, intent(IN)    :: P_codeperenne  !> // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0 
  integer, intent(IN)    :: nplt  
  integer, intent(IN)    :: n  
  integer, intent(IN)    :: P_codehypo  !> // PARAMETER // option of simulation of a  phase of hypocotyl growth (1) or planting of plantlets (2) // code 1/2 // PARPLT // 0 
  integer, intent(IN)    :: P_codegermin  !> // PARAMETER // option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: P_masecplantule  !> // PARAMETER // initial shoot biomass of plantlet // t ha-1 // PARPLT // 1 
  real,    intent(IN)    :: P_adil  !> // PARAMETER // Parameter of the critical curve of nitrogen needs [Nplante]=P_adil MS^(-P_bdil) // N% MS // PARPLT // 1 
  integer, intent(IN)    :: namf  
  real,    intent(IN)    :: P_adilmax  !> // PARAMETER // Parameter of the maximum curve of nitrogen needs [Nplante]=P_adilmax MS^(-P_bdilmax) // N% MS // PARPLT // 1 
  integer, intent(IN)    :: nrec  
  integer, intent(IN)    :: P_codcueille  !> // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0 
  integer, intent(IN)    :: P_codefauche  !> // PARAMETER // option of cut modes for forage crops: yes (1), no (2) // code 1/2 // PARTEC // 0 
  integer, intent(IN)    :: P_codelaitr  !> // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0 
  integer, intent(INOUT) :: P_codetransrad  !> // PARAMETER // simulation option of radiation 'interception: law Beer (1), radiation transfers (2) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: P_efcroijuv  !> // PARAMETER // Maximum radiation use efficiency during the juvenile phase(LEV-AMF) // g MJ-1 // PARPLT // 1 
  real,    intent(IN)    :: P_efcroiveg  !> // PARAMETER // Maximum radiation use efficiency during the vegetative stage (AMF-DRP) // g MJ-1 // PARPLT // 1 
  integer, intent(IN)    :: ndrp  
  real,    intent(IN)    :: P_efcroirepro  !> // PARAMETER // Maximum radiation use efficiency during the grain filling phase (DRP-MAT) // g MJ-1 // PARPLT // 1 
  real,    intent(IN)    :: chargefruit   !> // OUTPUT // Amount of filling fruits per m-2 // nb fruits.m-2
  real,    intent(IN)    :: P_coefb  !> // PARAMETER // parameter defining radiation effect on  conversion efficiency // SD // PARAM // 1 
  real,    intent(IN)    :: tcult   !> // OUTPUT // Crop surface temperature (daily average) // degree C
  real,    intent(IN)    :: P_teopt  !> // PARAMETER // Optimal temperature for the biomass growth // degree C // PARPLT // 1
  real,    intent(IN)    :: P_teoptbis  !> // PARAMETER // optimal temperature for the biomass growth (if there is a plateau between P_teopt and P_teoptbis) // degree C // PARPLT // 1
  real,    intent(IN)    :: P_temin  !> // PARAMETER // Minimum threshold temperature for development // degree C // PARPLT // 1
  real,    intent(IN)    :: P_temax  !> // PARAMETER // Maximal threshold temperature for the biomass growth  // degree C // PARPLT // 1
  integer, intent(IN)    :: P_codeclichange  !> // PARAMETER // option for climatel change : yes (2), no (1)  // code 1/2 // STATION // 0 
  real,    intent(IN)    :: P_alphaco2  !> // PARAMETER // coefficient allowing the modification of radiation use efficiency in case of  atmospheric CO2 increase // SD // PARPLT // 1 
 ! 23/09/2014je le passe en reel pour pb dans le calcul de fco2s
 ! integer, intent(IN)    :: co2
  real,    intent(IN)       :: co2
  real,    intent(IN)    :: P_resplmax  !> // PARAMETER // maximal reserve biomass // t ha-1 // PARAMV6 // 1 
  real,    intent(IN)    :: densite   !> // OUTPUT // Actual sowing density // plants.m-2
  integer, intent(IN)    :: P_codeh2oact  !> // PARAMETER // code to activate  water stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0 
  real,    intent(IN)    :: swfac   !> // OUTPUT // Index of stomatic water stress  // 0-1
  real,    intent(IN)    :: exobiom   !> // OUTPUT // Index of excess water active on surface growth // 0-1
  integer, intent(IN)    :: P_codeinnact  !> // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0 
  integer, intent(IN)    :: P_codeindetermin  !> // PARAMETER // option of  simulation of the leaf growth and fruit growth : indeterminate (2) or determinate (1) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: fpv   !> // OUTPUT // Sink strength of developing leaves // g.j-1.m-2
  real,    intent(IN)    :: fpft   !> // OUTPUT // Sink strength of fruits  // g.m-2 j-1
  real,    intent(IN)    :: P_remobres  !> // PARAMETER // proportion of daily remobilisable carbon reserve // SD // PARPLT // 1 
  real,    intent(IN)    :: P_resperenne0  !> // PARAMETER // initial reserve biomass // t ha-1 // INIT // 1 
  real,    intent(IN)    :: demande   !> // OUTPUT // Daily nitrogen need of the plant   // kgN.ha-1.j-1
  real,    intent(IN)    :: P_QNplante0  !> // PARAMETER // initial nitrogen amount in the plant // kg ha-1 // INIT // 1 
  real,    intent(IN)    :: P_msresiduel  !> // PARAMETER // Residual dry matter after a cut // t ha-1 // PARTEC // 1 
  integer, intent(IN)    :: P_nbcueille  !> // PARAMETER // number of fruit harvestings // code 1/2 // PARTEC // 0 
  real,    intent(IN)    :: rdtint  
  real,    intent(IN)    :: CNgrain   !> // OUTPUT // Nitrogen concentration of grains  // %
  integer, intent(IN)    :: P_codemontaison  !> // PARAMETER // code to stop the reserve limitation from the stem elongation // code 1/2 // PARAMV6 // 0 
  integer, intent(IN)    :: nmontaison  


! (IN)OUT
  real,    intent(INOUT) :: masec_veille    ! masec_veille => valeur précédente (valeur de la veille par ex.)  
  real,    intent(INOUT) :: masec           ! masec => valeur actuelle (valeur du jour courant par ex.)    // OUTPUT // Aboveground dry matter  // t.ha-1
  real,    intent(INOUT) :: QNplante_veille ! QNplante_veille => valeur précédente (valeur de la veille par ex.)  
  real,    intent(INOUT) :: QNplante        ! QNplante => valeur actuelle (valeur du jour courant par ex.)    // OUTPUT // Amount of nitrogen taken up by the plant  // kgN.ha-1
  real,    intent(INOUT) :: inns   !> // OUTPUT // Index of nitrogen stress active on growth in biomass // P_innmin to 1
  real,    intent(INOUT) :: inn   !> // OUTPUT // Nitrogen nutrition index (satisfaction of nitrogen needs ) // 0-1
  real,    intent(INOUT) :: innlai   !> // OUTPUT // Index of nitrogen stress active on leaf growth // P_innmin à 1
  real,    intent(INOUT) :: cumdltaremobilN  
  real,    intent(INOUT) :: ebmax  
  real,    intent(INOUT) :: ftemp   !> // OUTPUT // Temperature-related EPSIBMAX reduction factor // 0-1
  real,    intent(INOUT) :: epsib   !> // OUTPUT // Radiation use efficiency  // t ha-1.Mj-1. m-2
  real,    intent(INOUT) :: fco2  
  real,    intent(INOUT) :: dltams   !> // OUTPUT // Growth rate of the plant  // t ha-1.j-1
  real,    intent(INOUT) :: dltamsen   !> // OUTPUT // Senescence rate // t ha-1 j-1
  real,    intent(INOUT) :: dltamstombe  
  real,    intent(INOUT) :: resperenne   !> // OUTPUT // C crop reserve, during the cropping season, or during the intercrop period (for perenial crops) // t ha-1
  real,    intent(INOUT) :: dltamsN  
  real,    intent(INOUT) :: photnet   !> // OUTPUT // net photosynthesis // t ha-1.j-1
  real,    intent(INOUT) :: sourcepuits   !> // OUTPUT // Pool/sink ratio // 0-1
  real,    intent(INOUT) :: dltaremobil   !> // OUTPUT // Amount of perennial reserves remobilised // g.m-2.j-1
  real,    intent(INOUT) :: dltaremobilN  
  real,    intent(INOUT) :: remobilj   !> // OUTPUT // Amount of biomass remobilized on a daily basis for the fruits  // g.m-2 j-1
  real,    intent(INOUT) :: cumdltares  
  real,    intent(INOUT) :: magrain_veille  ! magrain_veille => valeur précédente (valeur de la veille par ex.)  
  real,    intent(INOUT) :: magrain         ! magrain => valeur actuelle (valeur du jour courant par ex.)  
  real,    intent(INOUT) :: masecneo   !> // OUTPUT // Newly-formed dry matter  // t.ha-1


! RAYTRANS
!  integer, intent(IN)    :: P_codetransrad
  real,    intent(IN)    :: surface(2)  
  real,    intent(OUT)   :: surfaceSous(2)  
  integer, intent(IN)    :: P_nbplantes  !> // PARAMETER // number of simulated plants // SD // P_USM/USMXML // 0 
  real,    intent(INOUT) :: P_extin(P_nbplantes)  !> // PARAMETER // extinction coefficient of photosynthetic active radiation canopy // SD // PARPLT // 1
  real,    intent(INOUT) :: cumrg   !> // OUTPUT // Sum of global radiation during the stage sowing-harvest   // Mj.m-2
  real,    intent(INOUT) :: cumraint   !> // OUTPUT // Sum of intercepted radiation  // Mj.m-2
  real,    intent(OUT)   :: fapar   !> // OUTPUT // Proportion of radiation intercepted // 0-1
  real,    intent(OUT)   :: delta  
! -- FIN RAYTRANS

! TRANSRAD
  real,    intent(IN)    :: P_adfol  !> // PARAMETER // parameter determining the leaf density evolution within the chosen shape // m-1 // PARPLT // 1 
  real,    intent(IN)    :: lairognecum  
  real,    intent(IN)    :: laieffcum  
  real,    intent(IN)    :: P_dfolbas  !> // PARAMETER // minimal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1 
  real,    intent(IN)    :: P_dfolhaut  !> // PARAMETER // maximal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1 
  real,    intent(OUT)   :: dfol   !> // OUTPUT //  "Within the shape  leaf density" // m2 m-3
  real,    intent(OUT)   :: rdif   !> // OUTPUT // Ratio between diffuse radiation and global radiation  // 0-1
  integer, intent(OUT)   :: parapluie  
  real,    intent(OUT)   :: raint   !> // OUTPUT // Photosynthetic active radiation intercepted by the canopy  // Mj.m-2
  real,    intent(IN)    :: P_parsurrg  !> // PARAMETER // coefficient PAR/RG for the calculation of PAR  // * // STATION // 1 
  integer, intent(IN)    :: P_forme  !> // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: lai   !> // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(IN)    :: laisen   !> // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
  real,    intent(IN)    :: eai  
  real,    intent(IN)    :: P_interrang  !> // PARAMETER // Width of the P_interrang // m // PARTEC // 1 
  integer, intent(IN)    :: nlax  
  integer, intent(IN)    :: nsen  
  integer, intent(IN)    :: P_codlainet  !> // PARAMETER // option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
  real,    intent(IN)    :: P_hautbase  !> // PARAMETER // Base height of crop // m // PARPLT // 1 
  integer, intent(IN)    :: P_codepalissage  !> // PARAMETER // option: no (1),  yes2) // code 1/2 // PARTEC // 0 
  real,    intent(IN)    :: P_hautmaxtec  !> // PARAMETER // maximal height of the plant allowed by the management // m // PARTEC // 1 
  real,    intent(IN)    :: P_largtec  !> // PARAMETER // technical width // m // PARTEC // 1 
  real,    intent(IN)    :: originehaut  
  real,    intent(INOUT) :: hauteur   !> // OUTPUT // Height of canopy // m
  real,    intent(INOUT) :: deltahauteur  
  real,    intent(INOUT) :: P_hautmax  !> // PARAMETER // Maximum height of crop // m // PARPLT // 1 
  real,    intent(INOUT) :: varrapforme  
  real,    intent(OUT)   :: largeur   !> // OUTPUT // Width of the plant shape  // m
  integer, intent(IN)    :: jul  
  real,    intent(IN)    :: trg   !> // OUTPUT // Active radiation (entered or calculated) // MJ.m-2
  real,    intent(IN)    :: P_latitude  !> // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0 
  real,    intent(OUT)   :: rombre   !> // OUTPUT // Radiation fraction in the shade // 0-1
  real,    intent(OUT)   :: rsoleil   !> // OUTPUT // Radiation fraction in the full sun // 0-1
  real,    intent(IN)    :: P_orientrang  !> // PARAMETER // Direction of ranks // rd (0=NS) // PARTEC // 1 
  real,    intent(IN)    :: P_ktrou  !> // PARAMETER // Extinction Coefficient of PAR through the crop  (radiation transfer) // * // PARPLT // 1 
! -- FIN TRANSRAD

! BEER
!  integer, intent(IN)    :: P_codelaitr
  real,    intent(IN)    :: tauxcouv   !> // OUTPUT // Cover rate // SD
  real,    intent(IN)    :: P_khaut  !> // PARAMETER // Extinction Coefficient connecting leaf area index to height crop // * // PARAM // 1 
! -- FIN BEER
  real,    intent(INOUT)   :: surfAO
  real,    intent(INOUT)   :: surfAS
  ! DR 27/10/2015 ajout pour FR
  real,    intent(OUT)     :: fpari  !> // OUTPUT // radiation factor on the calculation of conversion efficiency // g MJ-1



!: Variables locales
  real :: kco2  !>  
  real :: remob  !>  
!  real :: fpari  !>
  real :: rdtote  

! DR et ML et SYL 16/06/09 - NETTOYAGE
! ************************************
!   real vartalle,denstalle,densitetroph,bvartalle
!   integer pertalle
! DR et ML et SYL 15/06/09 Fin de l'epuration

! DR et ML et SYL 15/06/09
! ************************
! introduction de la fin des modifications de Sylvain (nadine et FR)
! dans le cadre du projet PERMED
! ####
! *- SYL 190209 - supression de la déclaration de P_resplmax qui a été introduit dans le paramV6
!   real P_resplmax
! ####
! DR et ML et SYL 15/06/09 FIN introduction de la fin des modifications de Sylvain

  real :: resperennemax  

!les FONCTIONs
  real :: calculerEffetTemperature ! à mettre dans le module Divers ?  

! 1.
      !: Pas de biomasse avant la levée

      ! ** AVANT LE STADE NLEV (LEVEE):
      ! *- dans le cas des cultures annuelles, deux cas:
      ! *-
      ! *- a) pour les cultures qui se plantent et admettent un
      ! *-    temps de latence avant le demarrage de la croissance:
      ! *-    initialisation de masec a P_masecplantule a partir de la plantation
      ! *-    et calcul de QNplante par la courbe de dilution.
      ! *-
      ! *- b) pour les cultures qui se sement:
      ! *-    initialisation de masec a 0 a partir du semis
      ! *-
      ! *- c) pour les cultures qui se plantent et demarrent directement
      ! *-    initialisation de masec a P_masecplantule a partir de la levee
      ! *-    qui est aussi la plantation
      ! *-    et calcul de QNplante par la courbe de dilution.
      if (nlev == 0) then
        !: nplt est gere differemment de nger, nlev, namf, etc.
        !- il est non nul des le depart de la simulation:
        !- on remplace donc le test precedent (nplt > 0)
        !- par le test (n >= nplt);
        if (P_codeperenne == 1 .and. n >= nplt) then

          if (P_codehypo == 2 .and. P_codegermin == 1) then
            masec = P_masecplantule
            ! NB le 13/07/05 remplacement de P_adilmax par P_adil pour homogénéité avec P_QNplante0
            ! DR le 10/11/05 du fait que dans stressN on calcule QNPlante à partir du QNPlante du jour -1 on a un souci
            !--QNplante = P_adil*P_masecplantule*10.
            QNplante_veille = P_adil * P_masecplantule * 10.
            inns   =  1.
            inn    = 1.
            innlai = 1.
            return
          else
            masec = 0.
            return
          endif
        else
          masec = 0.
        ! IGC le 19/10/2006 reinitialisation de la variable pour l'enchainement des perennes
          cumdltaremobilN = 0.
          return
        endif
      endif

      !: le jour de la levée
      if (n == nlev .and. P_codehypo == 2 .and. namf == 0) then
        masec_veille = P_masecplantule
        QNplante = P_adilmax * P_masecplantule * 10.
        inns   = 1.
        inn    = 1.
        innlai = 1.
      endif

      !: Après la récolte (nrec+1)
      if (nrec > 0 .and. n > nrec) then
        !: En cas de moisson, on enlève toute la biomasse aérienne
        !- (sauf si P_codefauche = 1) - PB & NB - 02/04/04
        if (P_codcueille == 1 .and. P_codefauche /= 1) then
          masec = 0.0
          ! DR et FR 30/05/2016
!  en fin de culture (apres recolte) on gardait la derniere valeur raint qui agit quand on
!   est en option shutwal (calcul de RnetS) sur le calcul de l'evopartion du sol rnetS=Rnet-rnetA-rnetP
          if((lai + eai).le.0.)then
                raint=0.
          endif
          return
        endif
      endif


! === CALCUL DE LA MATIERE SECHE ===

! pour le moment on interdit raytrans avec l'option taux de couverture
! A REGARDER PAR NADINE

      !: NB le 01/05 - il faut passer dans raytrans pour accèder à la routine "beer"
      !- TODO: rassembler dans initial tous les codes imposés quand P_codelaitr = 2
      if (P_codelaitr == 2) P_codetransrad = 1
      !TODO: il vaudrait mieux faire ce test dans une partie initialisation en amont et n'avoir ici qu'un test
      !      de vérification, qui enverrait un message à l'utilisateur pour le prévenir qu'en l'état actuel du code
      !      on force une loi de beer dans le cas du taux de couverture.
      !      Ce test pourrait être déporté dans raytrans d'ailleurs plutot qu'ici.

!      write(71,*)'dans biomaeravant raytrans',raint

! 2. le rayonnement intercepté
      call raytrans(P_codetransrad,P_extin,cumrg,cumraint,fapar,delta,P_adfol,lairognecum,laieffcum,P_dfolbas,P_dfolhaut,dfol,rdif,&
                    parapluie,raint,P_parsurrg,P_forme,lai,laisen,eai,P_interrang,nlax,nsen,P_codlainet,P_hautbase,P_codepalissage,&
                    P_hautmaxtec,P_largtec,originehaut,hauteur,deltahauteur,P_hautmax,varrapforme,largeur,jul,trg,P_latitude,     &
                    rombre,rsoleil,P_orientrang,P_ktrou,P_codelaitr,P_khaut,tauxcouv,surface,surfaceSous,ipl,P_nbplantes,ens,     &
                    surfAO,surfAS)


! 3. Machière sèche potentielle
! ** sa conversion
! *- ebmax varie avec le stade de croissance
      if (namf == 0) then
        ebmax = P_efcroijuv / 100.
      endif

      if (ndrp == 0 .and. namf > 0) then
        ebmax = P_efcroiveg / 100.
      endif

      if (ndrp /= 0) ebmax = P_efcroirepro / 100.

! NB le 23/06/06
! après récolte chez les pérennes on revient à une efficience végétative
      if (ndrp /= 0 .and. chargefruit <= 0.0) ebmax = P_efcroiveg / 100.


      !: Calcul de l'effet rayonnement
      fpari = ebmax - (P_coefb * raint)

! ** calcul de l'effet température ftemp
      ftemp = calculerEffetTemperature(tcult,P_teopt,P_teoptbis,P_temin,P_temax)

! ** calcul de epsilon b
      epsib = fpari*ftemp

! ** effet CO2 sur effcicience de conversion
! *- modifié le 19/01/02

      if (P_codeclichange == 2) then
        kco2 = -log(2-P_alphaco2)/(600-350)
        fco2 =  2-exp(-kco2*(co2-350))
        epsib = epsib*fco2
      endif


      dltams = epsib*raint


!    write(*,*)epsib,raint,dltams

! 4. Matière sèche réelle

! NB et IG le 22/09/06 limitation des réserves : effet puits sur les sources
! DR et ML et SYL 15/06/09
! **************************
! introduction de la fin des modifications de Sylvain (nadine et FR)
! dans le cadre du projet PERMED
! P_resplmax lu dans paramv6.par
!--      P_resplmax=0.66

! DR et ML et SYL 15/06/09
! **************************
! introduction de la fin des modifications de Sylvain (nadine et FR)
! dans le cadre du projet PERMED
! DR et ML et SYL 16/06/09
! on limite les reserves et on annule la croissance aerienne lorsque resperenne>resperennemax (P_resplmax*densite)
! dans tous les cas sauf si P_codemontaison=1 (actif) et qu'on est apres la montaison (nmontaison>0)
!--     resperennemax = P_resplmax*densite*10
!--     if(resperenne > resperennemax) dltams = 0.
      if (P_codemontaison == 2 .or. (P_codemontaison == 1 .and. nmontaison == 0)) then
        resperennemax = P_resplmax * densite * 10
        if (resperenne > resperennemax) dltams = 0.
      endif

      if (raint <= 0.0) dltams = 0.0

! ** effet des stress
! *- Nb - le 07/06 - introduction de l'excès d'eau
! *- les3 29/04/03  bug (exobiom jouait au carré)
      if (P_codeh2oact == 1) then
        dltams = dltams*swfac
      endif

      dltams = dltams*exobiom

      dltamsN =  dltams
! BM, NB le 11/04/05
! déplacement de l'influence du stress azoté pour le calcul de dltamsN

      if (P_codeinnact == 1) then
        dltams = dltams*inns
      endif
! ***faire une variable photnet
      photnet = dltams

! ** mobilisation des réserves
      dltams = dltams+dltaremobil

! ** recalcul de l'epsilon b et changement d'unité
! *- Nb - le 10/01/03 - bug calcul epsib
      if (raint > 0.0) then
        epsib = dltams/raint*100.0
      else
        epsib = 0.0
      endif
      epsib = min(epsib,ebmax*100)

! ** remobilisation des réserves chez les chez les pérennes
! *- ou chez les indéterminées (pour remplacer le paramétrage
! *- de remobilj

      if (P_codeperenne == 2 .or. P_codeindetermin == 2) then
! ** calcul du rapport source/puits
        if ((fpv+fpft) > 0.0) then
          sourcepuits = (dltams * 1e2) / (fpv + fpft)
        else
          sourcepuits = 1.0
        endif
        if (sourcepuits > 1.0) sourcepuits = 1.0
        !: Calcul des réserves mobilisables pour les pérennes
        !- avant l'effet des stress
        if (sourcepuits < 1.0) then
          !--remob = P_remobres*resperenne
          remob = (fpv + fpft) / 1e2 - dltams
!          write(73,*) remob,fpv,fpft,dltams
          remob = min(remob,P_remobres * resperenne)
!          write(73,*) remob,P_remobres,resperenne
          if (cumdltares < P_resperenne0) then
            dltaremobil = min(remob,resperenne)
            ! remobilisation concomittante de l'azote
            ! NB le 16/06/06
            !--dltaremobilN = P_QNplante0 * dltaremobil / P_resperenne0
            dltaremobilN = demande
            cumdltaremobilN = cumdltaremobilN + dltaremobilN
            if (cumdltaremobilN > P_QNplante0) dltaremobilN = 0.0
            resperenne = resperenne - dltaremobil
            remobilj = 0.0
          else
            !: Remobilisations déjà comptées dans l'accumulation de biomasse
            !- utilisée dans fruit.for pour calculer le rapport sourcepuits
            remobilj = min(remob,resperenne)

            !: NB et ML le 10/10/09: correction: on ne soustrait pas remobilj à resperenne
            !--resperenne = resperenne - remobilj
            dltaremobil = 0.0
            dltaremobilN = 0.0
          endif
        else
          dltaremobil = 0.0
          dltaremobilN = 0.0
          remobilj = 0.0
        endif
        !: Création d'une variable de cumul de remobilisation
        cumdltares = cumdltares + dltaremobil + remobilj
      endif

! DR et ML et SYL 16/06/09
! suppression d'un code test ancien sur le calcul de densite de talle (denstalle)
! NB  01/09/06
!      vartalle  = 1.5e4
!      bvartalle = 1000
!      P_maxtalle  = 4000
!      nspfourrage = 30
!      if (n == 1) then
!        spfourrage = 0.0
!        pertalle = 1
!        sptalle  = + dltams + remobilj + dltaremobil
!        denstalle = densite
!      endif
!
!      if (n <= pertalle*nspfourrage) then
!        if (fpv > 0.) then
!          spfourrage = spfourrage+dltams + remobilj + dltaremobil
!        endif
!      endif
!
!      if (n == pertalle * nspfourrage) then
!        sptalle = spfourrage / nspfourrage
!        densitetroph = (sptalle * vartalle) + bvartalle
!        if (denstalle <= densitetroph .and. namf <= 0) then
!          denstalle = min(densitetroph,P_maxtalle)
!        endif
!        if (denstalle > densitetroph) then
!          denstalle = densitetroph
!        endif
!        pertalle = pertalle + 1
!        spfourrage = 0.0
!      endif
! DR et ML et SYL 16/06/09 FIN DU NETTOYAGE

!  if (n == 1) denstalle = densite
!      if (sptalle < 0.2) then
!    denstalle = min(amortalle*sptalle,denstalle)
!  else
!    denstalle = min(amortalle*0.2,denstalle)
!  endif

      !: Pour les cultures fauchées demarrage
      !- à la matiere seche residuelle de la coupe precedente
      if (n == nlev .and. P_codefauche == 1) then
        masec_veille = P_msresiduel
      endif

      masec = masec_veille + dltams - dltamstombe

!   write(*,*)n,masec_veille, dltams , dltamstombe,masec
      !: Domi 06/06/01  matière sèche < 0
      if (masec < 0.) then
        dltamsen = 0.
        dltamstombe = 0.
        masec = 0.
        magrain = 0.
      endif
!      write(*,*)'P_codcueille',P_codcueille
      if (P_codcueille == 2) then
        if (n == nrec .and. nrec > 0) then
          if (P_nbcueille == 1) then
          ! DR 05/12/2014 pour la tomate semble bizarre ???
            rdtote = magrain_veille / 100.
          !  rdtote = rdtint / 100.
          endif
          if (P_nbcueille == 2) then
!            rdtote = rdtint / 100.
            rdtote = rdtint / 100.
          endif
          !--n = nrec
!          write(*,*)'nrec dans biomaer',masec,rdtote,P_nbcueille,rdtint,magrain_veille
!DR attention, 05/12/2014 on l'enleve apres dans cumaoas
          masec = masec - rdtote



!          write(*,*)'nrec dans biomaer',masec,rdtote,P_nbcueille,rdtint,magrain_veille
          !: NB&PB - 22/02/2005 - on enlève l'azote des fruits
          QNplante = QNplante - (CNgrain * rdtote * 10)

        endif
      endif


! ** TODO : voir si on peut supprimer ces commentaires.
! ** cumul réel de la matière sèche à partir du seuil MSAER0
! *- proposition de F. Ruget et X. Tayot pour tenir compte de
! *- l'allocation préférentielle aux racines en début de cycle
! *- ajout de la condition sur P_masec0 pour cultures plantées
! *- ajout du test sur codecroijuv NB le 14/11/00
! *-MSAER0 A SUPPRIMER
! --  if (codecroijuv == 1) then
! --    if (ms >= msaer0 .and. naer == 0) then
! --      if (P_codefauche == 2 .and. P_masec0 <= 0.0) ms = 0.0
! --      naer = n
! --    endif
! --
! --    if (naer == 0) then
! --      masec = 0.0
! -- ** si la biomasse est inférieure à MSAER0 à la récolte
! --      if (n == nrec .and. nrec > 0) masec = msaer0/10.0
! --    else
! --      masec = ms
! --    endif
! --  else
! --    masec = ms
! --  endif


      !: DR 05/03/08 - on ne fait ca que dans le cas de la prairie qui a des coupes
      !- sinon pb avec les autres cultures
      !- calcul de la matiere seche neoformee
      if (P_codefauche == 1) then
        masecneo = masec - P_msresiduel
      endif

return
end


!============================================
!============================================
!============================================

! ** calcul de l'effet température ftemp
real function calculerEffetTemperature(tcult,P_teopt,P_teoptbis,P_temin,P_temax)

  implicit none

  !: Arguments
  real :: tcult   !> // OUTPUT // Crop surface temperature (daily average) // degree C
  real :: P_teopt  !> // PARAMETER // Optimal temperature for the biomass growth // degree C // PARPLT // 1
  real :: P_teoptbis  !> // PARAMETER // optimal temperature for the biomass growth (if there is a plateau between P_teopt and P_teoptbis) // degree C // PARPLT // 1
  real :: P_temin  !> // PARAMETER // Minimum threshold temperature for development // degree C // PARPLT // 1
  real :: P_temax  !> // PARAMETER // Maximal threshold temperature for the biomass growth  // degree C // PARPLT // 1

  !: Variables locales
  real :: ftemp   !> // OUTPUT // Temperature-related EPSIBMAX reduction factor // 0-1

      !: Introduction d'un plateau entre P_teopt et P_teoptbis
      if (tcult < P_teopt) then
        ftemp = 1 - ((tcult - P_teopt) / (P_temin - P_teopt))**2
      endif

      if (tcult > P_teoptbis) then
        ftemp = 1 - ((tcult - P_teoptbis) / (P_temax - P_teoptbis))**2
      endif

      if (tcult >= P_teopt .and. tcult <= P_teoptbis) then
        ftemp = 1
      endif

      if (ftemp < 0.0) ftemp = 0.01

      calculerEffetTemperature = ftemp ! affectation de la variable de retour

return
end function calculerEffetTemperature
 
 
