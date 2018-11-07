!> Module Stics
!> - Description : Stics main module.
!! in this module are Described the structures
!! Stics_Communs_,Stics_Transit and the initialization subroutine which use it
!<

module Stics




USE iso_varying_string

implicit none

! 17/09/2012 DR pour record j'enleve l'allocatable et je mets une taille en dur "parametrable"
integer, parameter :: nb_residus_max = 21


! *********************************************** c
! * >>>>>>>                             <<<<<<< * c
! * >>>>>>>>>>> Variables communes <<<<<<<<<<<< * c
! * >>>>>>>                             <<<<<<< * c
! *********************************************** c

TYPE , BIND(C) :: Stics_Communs_


  logical :: flag_record = .false.
  integer :: ipl  
  integer :: P_nbplantes  !< // PARAMETER // number of simulated plants // SD // P_USM/USMXML // 0

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=3)  :: mois            !  variable pour le stockage du mois courant (jan à dec)
  integer           :: jour            !  variable pour le stockage du jour courant de la simulation (01 à 31)
  integer           :: nummois         !  variable pour le stockage du numéro de mois courant (01 à 12)
  !!!MODIF HISAFE 11 : Supression code inutile
  !!!integer           :: ancours         !  variable pour le stockage de l'année courante
  !!!integer :: P_ichsl  !< // PARAMETER // "soil number in the  param.soil  file" // SD // P_USM // 1

  integer :: P_culturean   ! // PARAMETER // crop status 1 = over 1 calendar year ,other than 1  = on two calendar years (winter crop in northern hemisphere) // code 0/1 // P_USM/USMXML // 0
  integer :: P_iwater  !< // PARAMETER // julian day of the beginning of the simulation // jour julien // P_USM // 1
  integer :: P_ifwater  !< // PARAMETER // julian day of the end of simulation // julian day // P_USM // 1
  integer :: ifwater_courant
  integer :: ifwater0
  integer :: dernier_n
  integer :: n
  integer :: nbjmax    = 366        ! taille des tableaux temporels (731 pour l'instant)
  integer :: jjul  
  integer :: jul  
  integer :: nbans  
  integer :: nbjanrec  
  integer :: nstoc  
  integer :: numcult  

  integer :: ens ! *** le code d'ensoleillement AO ou AS ***  

  integer :: fichist  
  integer :: ficsta  

  integer :: ficdbg ! ** PB - indice du fichier de debugging - 16/01/2004  
  integer :: ficdbg2 ! ** DR - indice du fichier de debugging number 2
  integer :: codoptim  
  integer :: P_codesuite   ! // PARAMETER // code for successive P_USM ( 1=yes, 0=no) // SD // P_USM/USMXML // 0



  integer :: nbjsemis  
  integer :: maxwth  
  integer :: nstoprac  

  integer :: numdate  
  integer :: bouchon      !< // OUTPUT // Index showing if the shrinkage slots are opened (0) or closed (1)  // 0-1
  integer :: nouvdensrac  

  integer :: nbCouchesSol  

  integer :: nappmulch  
  integer :: ires  
  integer :: itrav  

  integer :: ansemis  
  integer :: anrecol
  integer :: annee(732)

  integer :: NH

  integer :: codeprofil  

  integer :: nbjrecol  
  integer :: nbjrecol0  
  integer :: NHE  
  integer :: napini(2)  ! DR 10/06/2013 indexe sur les plantes
  integer :: napNini(2)    ! DR 10/06/2013 indexe sur les plantes
  integer :: nbjresini(2) ! DR 17/06/2016
! ** inicoupe DR le 7/11/05
  integer :: faucheannule  
!dr 19/09/2012 passe dans plante
!  integer :: nbjpourdecisemis(2)      !< // OUTPUT // "Number of days until sowing is launched when it's postponed by the   sowing decision  option activation" // days
!  integer :: nbjpourdecirecolte(2)      !< // OUTPUT // "Number of days until harvest is launched when it's postponed by the  harvest decision  option activation" // days

!!!MODIF HISAFE 5 : suppression variable inutile
!!!  real :: Ninitf(5)
!!!  real :: P_Hinitf(5)     !< // PARAMETER // Table of initial gravimetric water content of each soil layer (/fine earth) // % w // INIT // 1
  real :: delta  

  real :: devjourfr  

  real :: esz(1000)
  real :: fdens  
  real :: tustress      !< // OUTPUT // Stress index active on leaf growth (= minimum(turfac,innlai))  // 0-1

  real :: rdif      !< // OUTPUT // Ratio between diffuse radiation and global radiation  // 0-1
  real :: originehaut  
  real :: tairveille      !< // OUTPUT // Mean air temperature the previous day // degree C

  real :: coefbmsres  
  real :: coefaifres  
  real :: coefbifres  
  real :: coefcifres  

  real :: a  
  real :: effN ! TODO: variable locale. Pas d'interêt dans la version actuelle de la stocker  
  real :: hi  
  real :: ha  
  real :: hpf  
  real :: rglo  
  real :: hurlim  
  real :: rnetS      !< // OUTPUT // Net radiation in the soil // Mj.m-2
  real :: rnet      !< // OUTPUT // Net radiation  // MJ m-2
  real :: albedolai      !< // OUTPUT // P_Albedo of the crop cobining soil with vegetation // SD
  real :: resmes      !< // OUTPUT // Amount of soil water in the measurement depth // mm
  real :: dacouche(0:1000)  ! 0 à dacouche sinon pb le jour de la recolte dans densirac (si codeculture = feuille)
  real :: ruisselt      !< // OUTPUT // Total quantity of water in run-off (surface + overflow) // mm
  real :: infilj(0:5)  
  real :: exces(0:5)      !< // OUTPUT // Amount of water  present in the macroporosity of the horizon 1  // mm
  real :: anox(1000)
  real :: pluieruissel  
  real :: sat(1000)
  real :: cpreciptout      !< // OUTPUT // Water supply integrated over the simulation period  // mm
  real :: ruissel      !< // OUTPUT // Daily run-off // mm
  real :: QeauI  
  real :: QeauFS  
  real :: Qeau0  
  real :: doi  
  real :: Edirect      !< // OUTPUT // Water amount evaporated by the soil + intercepted by leafs + intercepted by the mulch  // mm
  real :: humidite      !< // OUTPUT // Moisture in the canopy // 0-1
  real :: mouillmulch  
  real :: Emulch      !< // OUTPUT // Direct evaporation of water intercepted by the mulch // mm
  real :: intermulch      !< // OUTPUT // Water intercepted by the mulch (vegetal) // mm
  real :: cintermulch      !< // OUTPUT // Amount of rain intercepted by the mulch // mm
  real :: ruisselsurf      !< // OUTPUT // Surface run-off // mm
  real :: ras      !< // OUTPUT // Aerodynamic resistance between the soil and the canopy   // s.m-1
  real :: Nvolat  
  real :: eptcult  
  real :: TcultMin  
  real :: TcultMax      !< // OUTPUT // Crop surface temperature (daily maximum) // degree C

  real :: dessecplt  

!DR 12/09/2012 devenu inutiles
!  real :: dassoiniteqv  ! calcul de la densité equivalente
!  real :: dassoinit

  real :: eo      !< // OUTPUT // Intermediary variable for the computation of evapotranspiration   // mm
  real :: eos      !< // OUTPUT // Maximum evaporation flux  // mm
  real :: Ratm      !< // OUTPUT // Atmospheric radiation  // Mj.m-2


! DR 08/07/2011 pb de dimensionnement des variables
!  real :: hres(21)
   real :: hres(nb_residus_max)

  real :: Wb(nb_residus_max)

  real :: kres(nb_residus_max)
  real :: NCbio      !< // OUTPUT // N/C ratio of biomass decomposing organic residues // gN.g-1C
  real :: saturation      !< // OUTPUT // Amount of water remaining in the soil macroporosity // mm
  real :: qmulch      !< // OUTPUT // Quantity of plant mulch // t.ha-1

!!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
!!!  real :: Ninit(5)
!!!  real :: Hinit(5)
!!!  real :: HR(5)      !< // OUTPUT // Water content of the horizon 5 (table)    // % pond.

  real :: azomes      !< // OUTPUT // Amount of  mineral nitrogen in the depth of measurement // kgN.ha-1
  real :: ammomes      !< // OUTPUT // Amount of ammonium in the depth of measurement // kgN.ha-1
  real :: FsNH3      !< // OUTPUT // Volatilisation of NH3  // µg.m-2.j-1
  real :: RsurRU      !< // OUTPUT // Fraction of available water reserve (R/RU) over the entire profile // 0 à 1
  real :: DRAT      !< // OUTPUT // Water flux drained at the base of the soil profile integrated over the simulation periodout of the soil   // mm
  real :: QNdrp  
  real :: esol      !< // OUTPUT // Actual soil evaporation flux  // mm day-1
  real :: et      !< // OUTPUT // Daily evapotranspiration (= es + et) // mm day-1
  real :: tnhc      !< // OUTPUT // "Cumulated  normalized   time  for the mineralisation of humus" // days
  real :: tnrc      !< // OUTPUT // "Cumulated  normalized  time for the mineralisation of organic residues" // days
  real :: pluieN  
  real :: irrigN  
  real :: precip      !< // OUTPUT // Daily amount of water (precipitation + irrigation)   // mm day-1
  real :: precipN  

  real :: cumoffrN  
  real :: cumoffrN0  
  real :: cumoffrN100  
  real :: azorac0  
  real :: azorac100  
  real :: demandebrute  
  real :: absodrp  
  real :: cpluie      !< // OUTPUT // Cumulative rainfall over the simulation period // mm
  real :: Chumt       !< // OUTPUT // Total amount of C humus (active + inert fractions) in the soil // kg.ha-1
  real :: Chumt0      !< // OUTPUT // Initial amount of C humus (active + inert fractions) in the soil // kg.ha-1
  real :: Nhuma       !< // OUTPUT // Amount of active nitrogen in the soil humus pool // kg.ha-1
  real :: Chuma
  real :: Nhuma0
  real :: Chuma0
  real :: Nhumi
  real :: Chumi
  real :: Nhumt      !< // OUTPUT // Total quantity of N humus (active + inert fractions) in the soil // kg.ha-1
  real :: Nhumt0
  real :: QCprimed
  real :: QNprimed
  real :: Cr      !< // OUTPUT // Amount of C in the soil organic residues // kg.ha-1
  real :: Nr      !< // OUTPUT // Amount of N remaining in the decaying organic residues in the soil  // kg.ha-1
  real :: Cb      !< // OUTPUT // amount of C in the microbial biomass decomposing organic residues mixed with soil // kg.ha-1
  real :: Nb      !< // OUTPUT // Amount of N remaining in the biomass decaying organic residues // kg.ha-1
  real :: Nb0
  real :: Nr0
  real :: Cb0
  real :: Cr0
  real :: Cbmulch0
  real :: Nbmulch0

  real :: etm      !< // OUTPUT // Maximum evapotranspiration ( = eop + es)  // mm
  real :: precipamm  
!!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
!!!  real :: P_NH4initf(5)    !< // PARAMETER // Amounts of initial mineral N in the 5 soil layers (fine earth) // kg.ha-1 // INIT // 1
!!!  real :: NH4init(5)

  real :: eaunoneffic  
  real :: toteaunoneffic  
  real :: raamax  
  real :: raamin  
  real :: laiTot                !  LAI total pour un jour de l'ensemble des plantes de la parcelle
  real :: stemflowTot  
  real :: EmdTot  
  real :: epTot  
  real :: hauteurMAX  

! DR on mets sur la profsol
! DR 13/11/06 on met sur 1000 comme le reste
  real :: Chum(1000)
  real :: Nhum(1000)
  real :: Cres(1000,nb_residus_max)
  real :: Nres(1000,nb_residus_max)
  real :: Cbio(1000,nb_residus_max)
  real :: Nbio(1000,nb_residus_max)

  real :: xmlch1      !< // OUTPUT // Thickness of mulch created by evaporation from the soil // cm
  real :: xmlch2  
  real :: supres  
  real :: stoc  
  real :: cestout      !< // OUTPUT // Evaporation integrated over the simulation period // mm
  real :: pfz(1000)
  real :: etz(1000)
  real :: parapluieetz  
  real :: totapN      !< // OUTPUT // Total amount of N inputs from fertiliser and residues // kg.ha-1
  real :: Qminh      !< // OUTPUT // Cumulative mineral N arising from humus // kg.ha-1
  real :: Qminr      !< // OUTPUT // Cumulative mineral N arising from organic residues // kg.ha-1
  real :: QLES      !< // OUTPUT // Cumulative NO3-N leached at the bottom of the soil profile // kg.ha-1
  !!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
  !!!real :: TS(5)      !< // OUTPUT // Mean soil temperature (mean of the 5 layers) // degree C
  real :: totapNres   !< // OUTPUT // Total amount of N in organic residues inputs  // kg.ha-1
  real :: Qnitrif      !< // OUTPUT // "cumulative nitrification of nitrogen (if option  nitrification  is activated)" // kg.ha-1
  real :: tcult      !< // OUTPUT // Crop surface temperature (daily average) // degree C
  real :: tcultveille  
  real :: tsol(0:1000)
  real :: tsolveille(1000)
  real :: HUR(1000)
  real :: hurmini(1000)
  real :: HUCC(1000)
  real :: HUMIN(1000)
!!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
 !!! real :: AZamm(5)      !< // OUTPUT // Amounts of NH4-N in the 5 soil horizons // kg.ha-1
  real :: effamm  

  !!!MODIF HISAFE 2 : reduction dimension temporelle
  !!!real :: tauxcouv(0:731)      !< // OUTPUT // Cover rate // SD
  real :: tauxcouv(0:366)      !< // OUTPUT // Cover rate // SD

! ** pour thomas - 27/01/2004 - on passe azsup dans le common pour le calcul de combithomas
  real :: azsup  

! * pour solnu
  real :: smes02  
  real :: sumes0  
  real :: sumes1  
  real :: sumes2  
  real :: sesj0  
  real :: ses2j0  
  real :: sum2  
  real :: esreste  
  real :: esreste2  


! * pour lixiv
  real :: drain      !< // OUTPUT // Water flux drained at the base of the soil profile // mm j-1
  real :: lessiv      !< // OUTPUT // daily N-NO3 leached at the base of the soil profile // kgN.ha-1

! * pour offrnodu et lecsorti
  real :: fxa      !< // OUTPUT // Anoxic effect on symbiotic uptake // 0-1
  real :: fxn      !< // OUTPUT // Nitrogen effect on symbiotic uptake // 0-1
  real :: fxt      !< // OUTPUT // Temperature effect on symbiotic uptake // 0-1
  real :: fxw      !< // OUTPUT // Water effect on symbiotic uptake // 0-1

! * tableau pour le cumul des absorptions (voir perteng.for)
  real :: absoTot(5)  

! * on garde les unites froids pour l'enchainement des perennes dans recup.tmp
  real :: cu0(2)  
  real :: somelong0(2)  
  integer :: nfindorm0(2)  
! 191206 Dr ET sAMUEL y'avait un soucis avec la valeur de vmax qu'on risque de perdre
  real :: vmax  
  real :: cumdltaremobilN  
! dr 05/09/2011 je rajoute qq variables de sortie pour AgMIP
!  real :: laimax(2)
! DR 06/09/2011 on ajoute des variable de sortie
  real :: cum_immob      !< // OUTPUT // cumulated mineral nitrogen arising from organic residues(immobilization) // kg.ha-1
  real :: QCapp     !> // OUTPUT // cumulative amount of organic C added to soil // kg.ha-1
  real :: QNapp     !> // OUTPUT // cumulative amount of organic N added to soil // kg.ha-1
  real :: QCresorg  !> // OUTPUT // cumulative amount of exogenous C added to soil // kg.ha-1
  real :: QNresorg  !> // OUTPUT // cumulative amount of exogenous N added to soil // kg.ha-1


  logical :: posibsw  
  logical :: posibpe  

 !!!MODIF HISAFE 6 : on remplace les tableaux de boolean
 !!! logical :: repoussesemis(2)
 !!! logical :: repousserecolte(2)
integer :: repoussesemis(2)
integer :: repousserecolte(2)
  logical :: recolte1  
  logical :: P_datefin  !< // PARAMETER // date of the end of the simulation // days // USMXML // 0


!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=50) :: codeversion    !  le numéro de version
!!!  character(len=12) :: P_codesimul      !  le type de simulation (1=culture ou 2=feuille)      // PARAMETER // simulation code (culture ou feuille=lai forcé) // SD // P_USM/USMXML // 0
  integer              :: P_codesimul !<   // PARAMETER // simulation code (1=culture ou 2=feuille=lai forcé) // SD // P_USM/USMXML // 0

!DR 19/07/2012 je mets la longueur de 25 à 50 pour les climats agmip
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=50) :: P_wdata1         !  nom du premier fichier de données climatiques      // PARAMETER // name of the beginning climate file // SD // P_USM // 0
!!!  character(len=50) :: P_wdata2         !  nom du deuxième fichier de données climatiques      // PARAMETER // name of the ending climate file // SD // P_USM // 0
!!!  character(len=40) :: P_usm            !  le nom du fichier P_usm      // PARAMETER // name of the P_USM // SD // USMXML // 0
!!!  character(len=7)  :: wlieu           !  le nom ou code du lieu des données climatiques

! ******************** c
! **>> CONSTANTES <<** c
! ******************** c
  integer :: AOAS = 0 ! Cumul des parties Au Soleil & A l'Ombre  
  integer :: AS = 1 ! Au Soleil  
  integer :: AO = 2 ! A l'Ombre  


! ** PARAMETRES TECHNIQUES AUTORISéS CULTURE PURE SEULEMENT
! *- culture sous abri

! DR 27/06/2013 ces parametres n'ont rien à faire la , ils sont deja declarés dans la structure itk
!  integer :: P_codabri  !< // PARAMETER // option of calculation of the climate under a shelter // code 1/2 // PARTEC // 0
!  integer :: P_julouvre2  !< // PARAMETER // day of opening of the shelter // julian day // PARTEC // 1
!  integer :: P_julouvre3  !< // PARAMETER // day of opening of the shelter // julian day // PARTEC // 1
  integer :: nouvre2
  integer :: nouvre3
!  real :: P_surfouvre1  !< // PARAMETER // surface proportion of the shelter opened the first day of opening // SD // PARTEC // 1
!  real :: P_surfouvre2  !< // PARAMETER // surface proportion of the shelter opened the second day of opening // SD // PARTEC // 1
!  real :: P_surfouvre3  !< // PARAMETER // surface proportion of the shelter opened the third day of opening // SD // PARTEC // 1
!  real :: P_transplastic  !< // PARAMETER // translission coefficient of the shelter plastic // SD // PARTEC // 1
! DR 28/06/2013 Ces parametres sont itk mais ne sont utilisés que dans le cas ou on a une seule plante , on ne prend en compte que la valeur de itk(1)


! *- code calcul automatique des fertilisations
! dr 04/05/2011 passé dans paramv6
!  integer :: P_codecalferti

! *- apports
  integer :: naptot  
  integer :: napNtot  

  !!!MODIF HISAFE 2 : reduction dimension temporelle
  !!!real :: anit(731)      !< // OUTPUT // Daily nitrogen provided  // kgN.ha-1 j-1
  !!!real :: anit_engrais(731)      !< // OUTPUT // Daily nitrogen provided by fertiliser // kgN.ha-1 j-1
  !!!real :: anit_uree(731)      !< // OUTPUT // Daily nitrogen provided  by pasture (uree) // kgN.ha-1 j-1
  !!!integer :: type_ferti(731) !< // OUTPUT // type of fertilizer // SD
  !!!real :: airg(731)      !< // OUTPUT // Daily irrigation // mm
  real :: anit(366)      !< // OUTPUT // Daily nitrogen provided  // kgN.ha-1 j-1
  ! DR 01/04/2016 je discretise anit en anit_uree et anit_engrais
  real :: anit_engrais(366)      !< // OUTPUT // Daily nitrogen provided by fertiliser // kgN.ha-1 j-1
  real :: anit_uree(366)      !< // OUTPUT // Daily nitrogen provided  by pasture (uree) // kgN.ha-1 j-1
  integer :: type_ferti(366) !< // OUTPUT // type of fertilizer // SD
  real :: airg(366)      !< // OUTPUT // Daily irrigation // mm


  real :: totir      !< // OUTPUT // Total amount of water inputs  // mm

! dr 05/04/2011 sont passés dans paramv6
! *- azote
!  integer :: P_codetesthumN
!  real :: P_dosimxN
!  real :: P_ratiolN

  real :: co2res      !< // OUTPUT // CO2 mass flow from the residues // kgC.ha-1.d-1
  real :: co2hum      !< // OUTPUT // CO2 mass flow from the soil humus // kgC.ha-1.d-1
  real :: CO2sol      !< // OUTPUT // CO2 mass flow from the soil // mgCO2.m-2.d-1
  real :: QCO2sol
  real :: QCO2res
  real :: QCO2hum
  real :: QCO2mul

! DR 26/11/07
  real :: tmoy_an(2,300)  
  real :: Tm_histo  
  real :: deltat_an(300)  
! DR 28/11/07
  real :: tm_glisse(200)  
  real :: deltaT_adaptCC(200)  


  real :: var_trefh(200)  
  real :: var_trefr(200)  
  real :: var_tnitmin(200)  
  real :: var_tnitmax(200)  
  real :: var_tnitopt(200)  
! Ajout Bruno juin 2012
  real :: var_tnitopt2(200)
  real :: var_TREFdenit1(200)  
  real :: var_TREFdenit2(200)  
  real :: var_TREFfhum(200)  
  real :: var_FTEM(200)  
  real :: var_FTEMr(200)  

!DR 19/07/2012 j'allonge le nom du fichier de 25 à 50
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=50) :: fplt_ori(2)
!!!  character(len=3) :: codeplante_ori(2)

!!!MODIF HISAFE 6 : on remplace les tableaux de boolean
!!!  logical :: plante_ori(2)
  logical :: plante_ori
  integer :: iplt_ori(2)  

  real :: Qem_N2O      !< // OUTPUT // cumulated N2O emission // kg.ha-1
  real :: em_N2O      !< // OUTPUT // daily N2O emission // kgN.ha-1.j-1
  real :: Qem_N2Onit      !< // OUTPUT // cumulated N2O-N emission due to nitrification// kg.ha-1
  real :: em_N2Onit      !< // OUTPUT // daily N2O-N emission due to nitrification // kg.ha-1.d-1
  real :: Qem_N2Oden      !< // OUTPUT // cumulated N2O-N emission due to denitrification// kg.ha-1
  real :: em_N2Oden      !< // OUTPUT // daily N2O-N emission due to denitrification // kgN.ha-1.d-1

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=60) :: sys
!!!  character(len=14) :: nomplante

  ! DR 19/09/2012 on les met dans la plante
!  real :: profextN(2)      !< // OUTPUT // Average depth of Nitrogen absorption // cm
!  real :: profexteau(2)      !< // OUTPUT // Average depth of water absorption // cm
!  integer :: age_prairie(2)      !< // OUTPUT // forage crop age from sowing // an
  integer :: nbcoupe_reel(2)
  logical :: onafaitunecoupedelanneedavant


! DR 26/02/08 pour climator prairie on ne gere pas idem l'annee de semis
  integer :: nbcoupe_an1(2)  
  integer :: julfauche_an1(2,20)  
  real :: lairesiduel_an1(2,20)  
  real :: hautcoupe_an1(2,20)  
  real :: msresiduel_an1(2,20)  
  real :: anitcoupe_an1(2,20)  
  real :: tempfauche_an1(2,20)  
  real :: tempfauche_ancours_an1(2,20)  

! DR 26/02/08  ... et les autres
  integer :: nbcoupe_an2(2)  
  integer :: julfauche_an2(2,20)  
  real :: lairesiduel_an2(2,20)  
  real :: hautcoupe_an2(2,20)  
  real :: msresiduel_an2(2,20)  
  real :: anitcoupe_an2(2,20)  
  real :: tempfauche_an2(2,20)  
  real :: tempfauche_ancours_an2(2,20)  

! DR 29/03/2016
  integer :: restit_an1(2,20)
  real :: mscoupemini_an1(2,20)
  integer :: restit_an2(2,20)
  real :: mscoupemini_an2(2,20)


! DR 03/03/02  sorties climator
  real :: irrigjN      !< // OUTPUT // mineral nitrogen from irrigation // kgN
  real :: precipjN      !< // OUTPUT // mineral nitrogen from rainfall // kgN
!  real :: Nexporte(2)      !< // OUTPUT // total of exported nitrogen // kgN.ha-1
!  real :: Nrecycle(2)      !< // OUTPUT // total of recycle nitrogen (unexported nitrogen at harvest + nitrogen from the fallen leaves) // kgN.ha-1
!  real :: MSexporte(2)      !< // OUTPUT // total of exported carbon // t.ha-1
!  real :: MSrecycle(2)      !< // OUTPUT // total of recycle carbon (unexported nitrogen at harvest + nitrogen from the fallen leaves) // t.ha-1
!  real :: p1000grain(2)      !< // OUTPUT // 1000 grain weight // g.m-2
  real :: apport_mini_semis  

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=15) :: nom_variete



!DR 05/03/08 iplt devient une variable car calculé par decision semis
! le P_iplt0 ets lu dans lectech
  integer :: iplt(2)  


!  real :: somudevair(2)      !< // OUTPUT // sum of air temperature from sowing // degree C
!  real :: somudevcult(2)      !< // OUTPUT // sum of crop temperature from sowing // degree C
!  real :: somupvtsem(2)      !< // OUTPUT // sum of development units from sowing // degree C

  integer :: iwater0
  integer :: ansemis0
  integer :: iwaterapres  
  integer :: ifwaterapres  
  integer :: nbjsemis0  

  integer :: iwater_cultsuiv  
  integer :: ifwater_cultsuiv  

  real :: beta_sol(2)  

  real :: offrN(1000)
  real :: absz(1000)

  real :: nodn      !< // OUTPUT // Nitrogen stress effect on nodosities establishment // 0 ou 1


  ! TODO: réfléchir quant à savoir si trosemax reste variable locale
  ! de humheure ou variable globale de Stics_Communs_ ou Climat_ ou autre.
  !!!MODIF HISAFE 2 : reduction dimension temporelle
  !!!real    :: trosemax(0:731)
  real    :: trosemax(0:366)


!: Les variables liées aux écritures/sorties
! DR 20/04/2012 verifier le dimensionnement des varaibles

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=20) :: valprof  !
!!!  character(len=20) :: valrap(100)
!!!  character(len=20) :: valpar(100)    !  stockage des noms des variables de sortie
!  character(len=10) :: nomvarprof(10)
! DR 27/062013 j'augmente le nombre de dates pour le fichier profil , je le passe de 20 à 60 dates
  integer  :: numdateprof(2), numdebprof(2),dateprof(2,600),nbvarsortie
  integer  :: nbvarrap  
  logical  :: ecritrap  
! DR 27/062013 j'augmente le nombre de dates pour le fichier profil , je le passe de 20 à 60 dates
! dr 07/02/2014 je passe de 60 dates à 400 pour Simtraces

!!!MODIF HISAFE 5 : suppression variable inutile
!!!  real     :: tabprof(2,600,1000), valsortie(100), valsortierap(100)
!!!  real     ::  valsortie_flo(100), valsortie_mat(100) , valsortie_iplt(100)

  !--variable locale de Ecriture_Rapport--real     :: nstt2
  real     :: QH2Of
  !!!MODIF HISAFE 1 : suppression des chaines de caractères
  !!!character(len=9) :: staderap(20)
  integer :: codeaucun  !
  integer :: codeenteterap  !
  integer :: codeenteterap_agmip ! dr 11/03/2013 ajout pour pouvoir avoir entete ou pas dans rappeort_agmip
  integer ::  codetyperap  
! DR 27/062013 j'augmente le nombre de dates pour le fichier rapport , je le passe de 20 à 366 dates
!!!MODIF HiSAFE 9 : suppression code inutile
!!!  integer :: daterap(366),nboccurrap, nboccurstd

  ! dr 11/03/2014 je stocke les dates du rapports puisque je les calcule avant l'appel à ecriture_rapport pour pouvoir recalculer les jours julien
  ! en cas de culture d'hiver
!!!  integer :: date_calend_rap(366,3) ! 1 an , 2 mois, 3 jour
  logical :: raplev  !
  logical :: rapamf  !
  logical :: raplax  !
  logical :: rapflo  !
  logical :: rapdrp  !
  logical :: raprec  !
  logical :: rapsen  !
  logical :: rapfin  
  logical :: rapplt  !
  logical :: rapger  !
  logical :: rapdebdes  !
  logical :: rapdebdebour  !
  logical :: rapmat  
  logical :: rapdebdorm  !
  logical :: rapfindorm  
  logical :: rapdeb
  logical :: start_rap



! DR et ML et BM et EJ 22/06/09
! *****************************
! introduction des modifications de BM et Eric Justes
! ** calcul de RsurRU
  real :: RU      !< // OUTPUT // maximum available water reserve over the entire profile // mm
! 19/09/2012 passe dans la plante
!  real :: RsurRUrac(2)      !< // OUTPUT // Fraction of available water reserve (R/RU) over the root profile // 0 à 1
! real :: RUrac(2)      !< // OUTPUT // maximum available water reserve over the root profile // mm
  real :: concNO3sol(5)      !< // OUTPUT // Nitrate concentration in the horizon 4 water // mg NO3 l-1
! fin DR et ML et BM 22/06/09

! DR et ML et BM et EJ 23/06/09
! *****************************
  real :: FTEMhb  
  real :: FTEMrb  

! DR et ML 16/10/09 ajout de variables relatives à la decomposition du mulch (BM)
! Bruno juin 2012 variables relatives à la decomposition du mulch
  real    :: Cresdec(11)   !< // OUTPUT // C in residue (i) decomposing mixed with soil // kg.ha-1
  real    :: Nresdec(11)   !< // OUTPUT // N in residue (i) decomposing mixed with soil // kg.ha-1
  real    :: Cnondec(10)   !< // OUTPUT // undecomposable C in residue i present in the mulch // kg.ha-1
  real    :: Nnondec(10)   !< // OUTPUT // undecomposable N in residue i present in the mulch // kg.ha-1
  real    :: Cmuldec(10)   !< // OUTPUT // C in residue (i) decomposing in mulch at soil surface // kg.ha-1
  real    :: Nmuldec(10)   !< // OUTPUT // N in residue (i) decomposing in mulch at soil surface // kg.ha-1
  real    :: Cmulch    !< // OUTPUT // Total C in mulch at soil surface // kg.ha-1
  real    :: Nmulch    !< // OUTPUT // Total N in mulch at soil surface // kg.ha-1
  real    :: Cmulchnd      !< // OUTPUT // total undecomposable C stock in mulch // kg.ha-1
  real    :: Nmulchnd      !< // OUTPUT // total undecomposable N stock in mulch // kg.ha-1
  real    :: Cmulchdec      !< // OUTPUT // total undecomposable C stock in mulch // kg.ha-1
  real    :: Nmulchdec      !< // OUTPUT // total undecomposable N stock in mulch // kg.ha-1
  real    :: Cbmulch      !< // OUTPUT // amount of C in the microbial biomass decomposing organic residues at soil surface (mulch) // kg.ha-1
  real    :: Nbmulch      !< // OUTPUT // amount of N in microbial biomass decomposing mulch // kg.ha-1
  real    :: Cmulch0     !< // OUTPUT // total undecomposable C stock in mulch at time 0 // kg.ha-1
  real    :: Nmulch0     !< // OUTPUT // total undecomposable N stock in mulch at time 0 // kg.ha-1
  real    :: couvermulch      !< // OUTPUT // Cover ratio of mulch  // 0-1

! DR 03/02/2011 on ajoute des varaibles de sorties sur les residus   !! voir sur quelle profondeur on dimensionne
! DR 04/12/2013 on avait une pb de dimensionnement sur cette varaible qui ne sert plus à rien arghhhhhhhhhhh !!!
!  real    :: Ctousresidusparcouche(nb_residus_max)
!  real    :: Ntousresidusparcouche(nb_residus_max)
  real    :: Ctousresidusprofil      !< // OUTPUT // total of carbon from the residues (all residues on P_profhum) // kg.ha-1
  real    :: Ntousresidusprofil      !< // OUTPUT // total of Nitrogen from residues (all residues on P_profhum) // kgN.ha-1
  real    :: Cresiduprofil(11)      !< // OUTPUT // total of Carbon from residues (ires) on P_profhum // kg.ha-1
  real    :: Nresiduprofil(11)      !< // OUTPUT // total of nitrogen from residues (ires) on P_profhum // kgN.ha-1
  real    :: qmulcht  
  integer :: irmulch  

!DR 16/12/2013 on ajoute les varaibles cumulés depuis le semis necessaires à Macsur
  real :: drain_from_plt       !< // OUTPUT // cumulative amount of water drained at the base of the soil profile over the crop period (planting-harvest)  // mm
  real :: leaching_from_plt    !< // OUTPUT //cumulative N-no3 leached at the base of the soil profile over the crop period (planting-harvest) // kg.ha-1
  real :: runoff_from_plt      !< // OUTPUT // cumulative Total quantity of water in run-off (surface + overflow) over the crop period (planting-harvest) // mm
  real :: Nmineral_from_plt !< // OUTPUT // mineral N from the mineralisation of humus and organic residues cumulated over the crop period (planting-harvest) // kg.ha-1
  real :: Nvolat_from_plt !< // OUTPUT // cumulative amount of N volatilised from fertiliser + organic inputs over the crop period (planting-harvest)// kg.ha-1
  real :: QNdenit_from_plt !< // OUTPUT // Cumulative denitrification of nitrogen from fertiliser or soil (if option  denitrification  is activated) over the crop period (planting-harvest)// kg.ha-1

! dr 16/12/2013 ajout de res_dispo_profmes par plante
  real  ::SoilAvW   !< //OUTPUT// variable eau dispo pour la plante  sur la profondeur profmes // mm
  real :: SoilWatM
  real :: SoilNM
! dr ajout des prof specifiques
  integer :: P_profmesW     !< // PARAMETER // depth of observed Water content  // cm //depths_paramv6.txt // 1
  integer :: P_profmesN     !< // PARAMETER // depth of observed Nitrogen content  // cm //depths_paramv6.txt // 1


! dr 19/09/2012 passe dans plante
! character (len=3) :: codebbch(2)      !< // OUTPUT // BBCH stage (see plant file) // SD


!  real    :: Cracine


! dr 07/09/2011 j'ahjoute une chaine de debut de ligne pour les fichiers st3
!!! MODIF HISAFE 11 : Supression code inutile
!!! character*40 :: chaine_debut_ligne



! PB - gestion des écritures

!!!  integer :: flagEcriture  !< // PARAMETER // option for writing the output files (1 = mod_history.sti, 2=daily outputs,4= report outut, 8=balance outputs,16 = profil outputs,  32= debug outputs, 64 = screen outputs, 128 = agmip outputs) add to have several types of outputs  code 1/2 // PARAM // 1

  integer :: ECRITURE_HISTORIQUE = 1  
  integer :: ECRITURE_SORTIESJOUR = 2  
  integer :: ECRITURE_RAPPORTS = 4  
  integer :: ECRITURE_BILAN = 8  
  integer :: ECRITURE_PROFIL = 16  
!   integer :: ECRITURE_DRAINAGE = 32  DR supprimé le 10/09/2012
  integer :: ECRITURE_DEBUG = 32
  ! 29/08/2012 DR j'ajoute sorties ecran et Agmip et je mets flagecriture en parametre des parametres generaux
  integer :: ECRITURE_ECRAN = 64
  integer :: ECRITURE_AGMIP = 128



! ---------------------------------------
! PB - les variables nécessaires au débug
  logical :: DEBUG = .FALSE.  

  integer :: DEVELOP = 0  
  integer :: CALAI = 0  
  integer :: BIOMAER = 0  
  integer :: SENESCEN = 0  
  integer :: FRUIT = 0  
  integer :: GRAIN = 0  
  integer :: EAUQUAL = 0  
  integer :: CROISSANCEFRONTRACINAIRE = 0  
  integer :: PROFILRACINAIRE = 0  
  integer :: DENSITEVRAIERACINAIRE = 0  
  integer :: CALPSIBASE = 0  
  integer :: REPARTIR = 0  
  integer :: IRRIG = 0  
  integer :: CALAPNENUPVT = 0  
  integer :: CALCULAUTOMATIQUEIRRIGATION = 0  
  integer :: APPORTSNPARPLUIEETIRRIGATION = 0  
  integer :: APPORTSNPARENGRAISMINERAUX = 0  
  integer :: APPORTSORGANIQUESETTRAVAILDUSOL = 0  
  integer :: RESIDUS = 0  
  integer :: ETATSURF = 0  
  integer :: MINERAL = 0  
  integer :: KETP = 0  
  integer :: SHUTWALL = 0  
  integer :: OFFRNODU = 0  
  integer :: BNPL = 0  
  integer :: LIXIV = 0  
  integer :: TRANSPI = 0  
  integer :: OFFREN = 0  
  integer :: ABSON = 0  
  integer :: MAJNSOL = 0  
  integer :: STRESSEAU = 0  
  integer :: STRESSN = 0  
  integer :: NGRAIN = 0  
  integer :: EXCESDEAU = 0  
  integer :: CALTCULT_SJ = 0  
  integer :: CALTCULT_SHUTWALL = 0  
  integer :: CALRNET_SHUTWALL = 0  
  integer :: TEMPSOL = 0  
  integer :: HUMCOUV_SJ = 0  
  integer :: HUMHEURE = 0  
  integer :: SOLNU = 0  
  integer :: DETASSEMENT = 0  
  integer :: TASSESEMISRECOLTE = 0  


! DR 05/09/2012 on passe ces 2 varaibles en commun sinon on perd sa valeur
  real  :: surfAO
  real  :: surfAS

! DR 29/04/2013 j'ajoute un compteur pour le calcul forcé de priestley taylor en cas de donnees manquantes
 integer ::  compt_calcul_taylor
! DR 10/20/2015 ajout des dates pour les irrigation JC
  integer :: n_datedeb_irrigauto
  integer :: n_datefin_irrigauto

  integer :: code_ecrit_nom_usm



!DR 14022016 pour AgMIP ET
  real :: HR_vol_1_30   !> // OUTPUT //water content of the horizon 1-30 cm  (table) //mm
  real :: HR_vol_31_60  !> // OUTPUT //water content of the horizon 31-60 cm  (table) //mm
  real :: HR_vol_61_90  !> // OUTPUT //water content of the horizon 61-90 cm  (table) //mm
  real :: HR_vol_91_120   !> // OUTPUT //water content of the horizon 91-120 cm  (table) //mm
  real :: HR_vol_121_150   !> // OUTPUT //water content of the horizon 121-150 cm  (table) //mm
  real :: HR_vol_151_180   !> // OUTPUT //water content of the horizon 151-180 cm  (table) //mm
! DR 31/05/2016 ajout du nombre de jours depuis le semis
  integer ::  day_after_sowing !> // OUTPUT //days after sowing //days

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character*20  :: info_level , model_name


! messages pour le fichier historique
!      type(varying_string), allocatable :: messages(:)
! desactiver pour Record la ligne qui suit
!      type(varying_string), dimension(nb_messages_max) :: messages


  real :: N_mineralisation
  real :: soilN
  real :: tcult_tairveille
  real :: humidite_percent  !< // OUTPUT // Moisture in the canopy // %
! pour le paturage des vaches
  logical :: flag_onacoupe
  ! DR 07/04/2016 j'ajoute les varaibles pour la pature
  real :: CsurNres_pature
  real :: qres_pature

 !!!MODIF HISAFE 7 : Déplacement de variables (de Climat.f90 dans Stics.f90)
integer :: nitetcult(0:366)    !< // OUTPUT // Number of iterations to calculate TCULT // SD

!!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
  real :: treeWaterUptake(1000)
  real :: treeNitrogenUptake(1000)

end type Stics_Communs_



! *********************************** c
! *     variables transitoires      * c
! *********************************** c

TYPE , BIND(C) ::  Stics_Transit_
!!!MODIF HISAFE 7 : déplacement de variables
!!!inutile cette variable est en fait dans le ITK
 !!! integer :: P_codlocirrig ! variable d'harmonisation des codlocirrg des différents fichiers techniques de la simulation (cultures associées) 	  // PARAMETER // code of irrigation localisation: 1= above the foliage, 2= below the foliage above the soil, 3 = in the soil // code 1/2/3 // PARTEC // 0

!: NB - le 07/06/2004 - calcul de psibase
! TODO: variable locale à calpsibase ? ou dans le commun pour sorties ?
!19/09/2012 va dans plante
!  real :: psibase(2) ! P_nbplantes  	  // OUTPUT // Predawn leaf water potential potentiel foliaire de base // Mpascal


!: NB le 11/02/05 nouveautés courbes de dilution N
! révisé le 11/09/05

!!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Plante.f90)
!!!  real :: bdilI(2)
!!!  real :: adilI(2)
!!!  real :: adilmaxI(2)
!!!  real :: bdilmaxI(2)
!!!  real :: inni(2,0:2)       ! pour chaque plante, AO/AS
!!!  real :: deltabso(2,0:2)   ! pour chaque plante, AO/AS
!!!  real :: dltamsN(2,0:2)    ! pour chaque plante, AO/AS
!!!  logical :: humectation
!!!  integer :: nbjhumec
!!!  real :: pfmax2
!!!  real :: pluiesemis
!!!  real :: QH2Oi
!!!  real :: QNO3i
!!!  real :: QNH4i

  integer :: P_codetempfauche  !< // PARAMETER // option of the reference temperature to compute cutting sum of temperatures : upvt (1), udevair (2) // code 1/2 // PARAMV6 // 0
  real :: P_coefracoupe(2)            ! 2 plantes 	  // PARAMETER // coefficient to define the proportion of dying roots after cut (grass) // SD // PARAMV6/PLT // 1 

!!!  real :: dltaremobilN(2,0:2)         ! 2 plantes, AO et AS

!!!  real :: spfourrage
!!!  real :: nspfourrage

  integer :: P_codepluiepoquet  !< // PARAMETER // option to replace rainfall by irrigation at poquet depth in the case of poquet sowing // code 1/2 // PARAMV6 // 0
  integer :: P_nbjoursrrversirrig  !< // PARAMETER // number of days during which rainfall is replaced by irrigation in the soil after a sowing poquet // jours // PARAMV6 // 1

!!!  real :: somtemphumec(2)


  real :: P_SurfApex(2)     !< // PARAMETER // equivalent surface of a transpiring apex // m² // PARAMV6/PLT // 1
  real :: P_SeuilMorTalle(2)     !< // PARAMETER // relative transpiring threshold to calculate tiller mortality // mm // PARAMV6/PLT // 1
  real :: P_SigmaDisTalle(2)     !< // PARAMETER // Coefficient used for the gamma law calculating tiller mortality //  // PARAMV6/PLT // 1
  real :: P_VitReconsPeupl(2)     !< // PARAMETER // thermal time for the regeneration of the tiller population // nb tillers/degree C/m² // PARAMV6 // 1
  real :: P_SeuilReconsPeupl(2)     !< // PARAMETER // tiller density threshold below which the entire population won't be regenerated // nb tillers/m² // PARAMV6/PLT // 1
  real :: P_MaxTalle(2)     !< // PARAMETER // maximal density of tillers/m² // Nb tillers/ // PARAMV6/PLT // 1
!!!  integer :: PerTalle(2)
!!!  real :: sptalle(2)
!!!  real :: denstalle(2)


!!!  integer :: dateirr(2,100)
!!!  integer :: nbapirr(2)

!!!  integer :: dateN(2,100)
!!!  integer :: nbapN(2)

  integer :: P_code_adapt_MO_CC  !< // PARAMETER // activation code for organic matter adaptation to climate change (1 = yes, 2 = no) // code1/2 // PARAMV6 // 0
  integer :: P_code_adaptCC_miner  !< // PARAMETER // activation code for the impact of climate change on mineralisation, parameter modification P_trefh and P_trefr (1 = yes, 2 = no) // code1/2 // PARAMV6 // 0
  integer :: P_code_adaptCC_nit  !< // PARAMETER // activation code for the impact of climate change on nitrification, parameter modification P_tnitmin, P_tnitmax, P_tnitopt and P_tnitopt2 (1 = yes, 2 = no) // code1/2 // PARAMV6 // 0
  integer :: P_code_adaptCC_denit  !< // PARAMETER // activation code for the impact of climate change on denitrification, parameter modification P_trefdenit1 and P_trefdenit2 (1 = yes, 2 = no) // code1/2 // PARAMV6 // 0
  integer :: P_periode_adapt_CC  !< // PARAMETER // year number to calculate moving temperature average // year // PARAMV6 // 1
  integer :: P_an_debut_serie_histo  !< // PARAMETER // beginning year for the calculation of moving average temperature on period_adapt_CC // year // PARAMV6 // 0
  integer :: P_an_fin_serie_histo  !< // PARAMETER // ending year for the calculation of moving average temperature on period_adapt_CC // year // PARAMV6 // 0
  real :: P_param_tmoy_histo  !< // PARAMETER // mean temperature over the period of adaptation to climate change // degree C // PARAMV6 // 1
  real :: P_TREFdenit1  !< // PARAMETER // temperature of reference for the soil denitrification parameters (11degree C for temperate soils and 20degree C for tropical soils) // degree C // PARAMV6 // 1
  real :: P_TREFdenit2  !< // PARAMETER // temperature of reference for the soil denitrification parameters (20degree C for temperate soils and 29degree C for tropical soils) // degree C // PARAMV6 // 1


  integer :: P_nbj_pr_apres_semis  !< // PARAMETER // days number to calculate rainfall need to start sowing (is codesemis is activated) // day // PARAMV6 // 1
  integer :: P_eau_mini_decisemis  !< // PARAMETER // minimum amount of rainfall to start sowing (when codesemis is activated) // mm // PARAMV6 // 1
  real :: P_humirac_decisemis  !< // PARAMETER // effect of soil moisture for sowing decision ( from 0 to 1 : 0 = no sensitivity to drought; 1 = very sensitive) // SD // PARAMV6 // 1

! DR et ML et SYL 15/06/09
! ************************
! introduction de la fin des modifications de Sylvain (nadine et FR)
! dans le cadre du projet PERMED
    real :: P_swfacmin  !< // PARAMETER // minimul value for drought stress index (turfac, swfac, senfac) // SD // PARAMV6 // 1
!!!    real :: LAIapex(2)
! DR 19/09/2012 passe dans plante
!    real :: mortreserve(2)   !< // OUTPUT // Reserve biomass corresponding to dead tillers // t.ha-1.d-1
    real :: P_SeuilLAIapex(2)  !< // PARAMETER // Maximal value of LAI+LAIapex when LAIapex isn't nil // m²/m² // PARAMV6/PLT // 1

! DR et ML et SYL 15/06/09
! ************************
! on indexe P_codedyntalle sur la plante
    integer :: P_codetranspitalle  !< // PARAMETER // Choice of the ratio used to calculate tiller mortality: et/etm (1) ou epc2 / eopC (2) // code 1/2 // PARAMV6 // 0
    integer :: P_codedyntalle(2)  !< // PARAMETER // Activation of the module simulating tiller dynamic: yes (1), no (2) // code 1/2 // PARAMV6/PLT // 0

! DR et ML et SYL 15/06/09
! ************************
! introduction de la fin des modifications de Sylvain (nadine et FR)
! dans le cadre du projet PERMED
! ####
! SYL 05/03/08 1 paramètre pour caractériser la composition de la biomasse résiduelle à la coupe
! NB  06/03/08 1 paramètre et 2 varibles pour le calcul du stade début montaison
! SYL 26/02/09 1 paramètre définissant la quantité de réserve par plante (ou par talle)
! DR et ML et SYL 16/06/09
! on supprime nvernal qui ne sert à rien

!!!    integer :: nmontaison(2)
    real :: P_tigefeuilcoupe(2)  !< // PARAMETER // stem (structural part)/leaf proportion the cutting day // SD // PARAMV6/PLT // 1
! DR 19/09/2012 passe dans plante
!    real :: somcourmont(2)   !< // OUTPUT // Cumulatied units of development from the start of vernalisation // degree.days
    real :: P_resplmax(2)  !< // PARAMETER // maximal reserve biomass // t ha-1 // PARAMV6 // 1

! ####
! DR et ML et SYL 15/06/09 FIN introduction de la fin des modifications de Sylvain


  integer :: P_codemontaison(2)     !< // PARAMETER // code to stop the reserve limitation from the stem elongation // code 1/2 // PARAMV6 // 0

!!!  integer :: onestan2

  integer :: P_codecalferti  !< // PARAMETER // automatic calculation of fertilisation requirements: yes (2), no (1) // code 1/2 // PARAMV6 // 0
  integer :: P_codetesthumN  !< // PARAMETER // automatic fertilisation calculations // code 1/2 // PARAMV6 // 0
  real :: P_dosimxN  !< // PARAMETER // maximum amount of fertilisation authorised at each time step (mode automatic fertilisation) // kg N ha-1 // PARAMV6 // 1
  real :: P_ratiolN  !< // PARAMETER // Nitrogen stress index below which we start an fertilisation in automatic mode (0 in manual mode) // between 0 and 1  // PARAMV6 // 1


! ajout param Bruno mai 2012
! Bruno: ajout de 5 nouveaux parametres relatifs a limitation decomposition par N min
  integer :: P_codeNmindec !< // PARAMETER // option to activate the available N :yes (1), no(2) // code 1/2 //PARAMv6 // 1
  real :: P_rapNmindec  !< // PARAMETER //slope of the linear relationship between the fraction of mineral N available for residue decomposition and the amount of C in decomposing residues (0.001)// g.g-1 // PARAMV6 //1
  real :: P_fNmindecmin !< // PARAMETER //minimal fraction of mineral N available for residues decomposition (if codeNmindec is activated) // SD // PARAMV6 //1
 ! DR 29/08/2012 ajout d'un code pour avoir ou non les sorties aGmip
!   integer :: P_Config_Output !> // PARAMETER // configuration optionnal Outputs  (1=no,2=screen,3=agmip) // SD // PARAMV6 // 1
! ML 29102012
  integer :: P_codetrosee   !< // PARAMETER // option to choose the way to calculate the hourly dew temperature : linear interpolation (1), sinusoidal interpolation (Debele Bekele et al.,2007)(2) // code 1/2 //PARAMv6 // 1
  integer :: P_codeSWDRH    !< // PARAMETER // option to activate the calculation of surface wetness duration : yes (1), no (2) // code 1/2 //PARAMv6 // 1
! 07/02/2014 supprimé du tronc
!  integer :: P_codepatho    !< // PARAMETER // option to couple Stics with Mila  : yes (1), no (2) // code 1/2 //PARAMv6 // 1
! DR 10/02/2015 ajout des paraemtre date irrig auto pour Julie
integer :: P_codedate_irrigauto !< // PARAMETER // option to activate the beginning and the ending date in case of automatic irrigation  : yes (1), no (2) // code 1/2 //PARAMv6 // 1
integer :: P_datedeb_irrigauto  !< // PARAMETER // date of beginning automatic irrigations  : yes (1), no (2) // code 1/2 //PARAMv6 // 1
integer :: P_datefin_irrigauto !< // PARAMETER // date of ending automatic irrigations  : yes (1), no (2) // code 1/2 //PARAMv6 // 1
! DR 06/05/2015 je rajoute un code pouyr tester la mortalitéé des racines
integer :: P_codemortalracine !< // PARAMETER // masec servant a calculer les racines mortes a la coupe  : masec (1), masectot (2) // code 1/2 //PARAMv6 // 1
! DR 05/02/2016 pour regles de semis agmipWheat
integer :: P_rules_sowing_AgMIP !< // PARAMETER // activation of the semis rules AgMIP wheat3 yes(1) no(2) // code 1/2 //PARAMv6 // 1
integer :: P_Flag_Agmip_rap  !< // PARAMETER // report specific outputs AgMIP nothing(1) AgMIP(2) Macsur(3) // code 1/2/3 //PARAMv6 // 1
integer :: P_type_project  !< // PARAMETER // activation des regles de semis AgMIP AgMIP Wheat(1) "AgMIP Wheat Giacomo (HSC)(2) wheat Canopy temp(3) face_maize(4) new wheat3(5) // code 1/2/3/4/5 //PARAMv6 // 1

integer :: P_option_thinning !< // PARAMETER // enabling of several thinning yes(1),(no) 2 //code 1/2 //PARAMv6 // 1
integer :: P_option_pature !< // PARAMETER // enabling of pasture of grassland yes(1),(no) 2 //code 1/2 //PARAMv6 // 1
integer :: P_option_engrais_multiple  !< // PARAMETER // enabling of using several kind of fertilizer yes(1),(no) 2 //code 1/2 //PARAMv6 // 1

integer :: P_coderes_pature !< // PARAMETER // residue type: 1=crop residues,  2=residues of CI,  3=manure,  4=compost OM,  5=mud SE,  6=vinasse,  7=corn,  8=other // code 1 to 10 // PARAMv6 // 0

   real :: P_pertes_restit_ext   !< // PARAMETER // dejections animales non restituees sur les parcelles //  // PARAMv6 // 1
   real :: P_Crespc_pature  !< // PARAMETER // carbon proportion in organic residue //  // PARAMv6 // 1
   real :: P_Nminres_pature    !< // PARAMETER // N mineral content of organic residues  // % fresh matter // PARAMv6 // 1
   real :: P_eaures_pature    !< // PARAMETER // Water amount of organic residues  // % fresh matter // PARAMv6 // 1
   real :: P_coef_calcul_qres   !< // PARAMETER // ?  // ? // PARAMv6 // 1
integer :: P_engrais_pature  !< // PARAMETER // fertilizer type  : 1 =Nitrate.of ammonium ,2=Solution,3=urea,4=Anhydrous ammoniac,5= Sulfate of ammonium,6=phosphate of ammonium,7=Nitrateof calcium,8= fixed efficiency    // * // PARAMv6 // 1
   real :: P_coef_calcul_doseN   !< // PARAMETER // ?  // ? // PARAMv6 // 1




end type Stics_Transit_


contains

!****f* Stics_initialisation
! NAME
!   Stics_initialisation
!
! DESCRIPTION
!   Routine d'initialisation de Stics
!
!***
subroutine Stics_initialisation(sc)

USE Messages

  type(Stics_Communs_), intent(INOUT) :: sc
! DR 17/01/2012 on fait tout in english
!  character(len=2) :: langue = 'FR'
  character(len=2) :: langue = 'EN'



! DR 20/06/2013 je lis la version dans un fichier qui me sera fourni par Ju.

    !!call call_num_version(sc)

    !: appel de la routine d'initialisation des messages
      call remplirMessages(langue)

end subroutine Stics_initialisation



subroutine Stics_Transit_Zero(t)

    type(Stics_Transit_), intent(INOUT) :: t  

!!!MODIF HISAFE 11 : Supression code inutile
 !!! t%P_codlocirrig = 0 ! variable d'harmonisation des codlocirrg des différents fichiers techniques de la simulation (cultures associées)

!: NB - le 07/06/2004 - calcul de psibase
! TODO: variable locale à calpsibase ? ou dans le commun pour sorties ?
!  t%psibase = 0.

!!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Plante.f90)
!!!  t%bdilI(:) = 0.
!!!  t%adilI(:) = 0.
!!!  t%adilmaxI(:) = 0.
!!!  t%bdilmaxI(:) = 0.
!!!  t%inni(:,:) = 0.       ! pour chaque plante, AO/AS
!!!  t%deltabso(:,:) = 0.   ! pour chaque plante, AO/AS
!!!  t%dltamsN(:,:) = 0.    ! pour chaque plante, AO/AS
!!!  t%humectation = .false.
!!!  t%nbjhumec = 0
!!!  t%pfmax2 = 0.
!!!  t%pluiesemis = 0.
!!!  t%QH2Oi = 0.
!!!  t%QNO3i = 0.
!!!  t%QNH4i = 0.

  t%P_codetempfauche = 0
  t%P_coefracoupe(:) = 0.            ! 2 plantes

 !!! t%dltaremobilN(:,:) = 0.         ! 2 plantes, AO et AS

!!!  t%denstalle(:) = 0.
!!!  t%sptalle(:) = 0.

 !!! t%spfourrage = 0.
 !!! t%nspfourrage = 0.

  t%P_codepluiepoquet = 0
  t%P_nbjoursrrversirrig = 0

!!!  t%somtemphumec(:) = 0.


  t%P_SurfApex(:) = 0.
  t%P_SeuilMorTalle(:) = 0.
  t%P_SigmaDisTalle(:) = 0.
  t%P_VitReconsPeupl(:) = 0.
  t%P_SeuilReconsPeupl(:) = 0.
  t%P_MaxTalle(:) = 0.
 !!! t%PerTalle(:) = 0
  t%P_swfacmin = 0.
!!!  t%LAIapex(:) = 0.
!  t%mortreserve(:) = 0.
  t%P_codetranspitalle = 0
  t%P_codedyntalle(:) = 0

! DR le 12/09/07 pour benjamin
! ITK =>  t%P_doseirrigmin(:) = 0
! dr 21/08/08 ne sert plus t%codeirrigmin
! dr 21/08/08 ne sert à rien  t% codeforcedrpdes

 !!! t%dateirr(:,:) = 0
!!!  t%nbapirr(:) = 0


! *- azote
! DR 05/04/2011 sont passés dans paramv6
! *- code calcul automatique des fertilisations
! dr 05/04/2011 mis dans paramv6
  t%P_codecalferti = 0
  t%P_codetesthumN = 0
  t%P_dosimxN = 0.0
  t%P_ratiolN = 0.0



  !!!t%dateN(:,:) = 0
  !!!t%nbapN(:) = 0

  t%P_resplmax(:) = 0.


! DR 23/11/07 parametres impact CC sur la MO
  t%P_code_adapt_MO_CC = 0
  t%P_code_adaptCC_miner = 0
  t%P_code_adaptCC_nit = 0
  t%P_code_adaptCC_denit = 0
  t%P_periode_adapt_CC = 0
  t%P_an_debut_serie_histo = 0
  t%P_an_fin_serie_histo = 0
  t%P_param_tmoy_histo = 0.
  t%P_TREFdenit1 = 0.
  t%P_TREFdenit2 = 0.


  t%P_nbj_pr_apres_semis = 0
  t%P_eau_mini_decisemis = 0
  t%P_humirac_decisemis = 0.


  t%P_codedate_irrigauto = 0
  t%P_datedeb_irrigauto = 0
  t%P_datefin_irrigauto = 0
  ! DR 06/05/2015 je rajoute un code pouyr tester la mortalitéé des racines
  t%P_codemortalracine = 0
! DR 05/02/2016 pour regles de semis agmipWheat
  t%P_rules_sowing_AgMIP = 0
  t%P_Flag_Agmip_rap = 0
  t%P_type_project = 0
  t%P_option_thinning = 0
  t%P_option_pature = 0

t%P_coderes_pature= 0
t%P_pertes_restit_ext= 0.
t%P_Crespc_pature= 0.
t%P_Nminres_pature= 0.
t%P_eaures_pature= 0.
t%P_coef_calcul_qres= 0.
t%P_engrais_pature= 0
t%P_coef_calcul_doseN= 0.


  return
end subroutine Stics_Transit_Zero



end module Stics
 
 
