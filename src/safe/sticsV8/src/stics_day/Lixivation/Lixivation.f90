!> Module lixivation : Module des fonctions et routines liees au phenomene de lixivation
!! - Description :
!! Dans ce module sont decrites toutes les interfaces des routines et fonctions attachees a la routine
!!   de simulation du phenomene de lixivation.
!!   Ainsi, une routine externe appelant une (ou plusieurs) des unites de programme du module Lixivation peut inclure
!!   la ou les interfaces correspondantes. La verification et la validation des arguments necessaires en est alors
!!   grandement facilite.
!<
module Lixivation

! - LES INTERFACES -
interface

!  subroutine lixiv(n,precip,hurlim,ruisselsurf,humin,nhe,nh,hucc,hr,P_codefente,ncel,icel,da,&  ! les entrées
!                   izcel,P_infil,P_concseuil,P_humcapil,izc,hcc,P_capiljour,P_epc,hmin,P_codemacropor, &
!                   P_profdrain,profsol,P_bformnappe,P_distdrain,P_codhnappe,ldrains,P_codrainage,    &
!                   P_rdrain,P_profimper,P_ksol,profcalc,epz,esz, zrac,irrigprof,                 &
!                   bouchon,hur,sat,azomes,anox,nit,pluieruissel,saturation,resmes,RsurRu,  &  ! les sorties
!                   concno3les,hnappe,hmax,hph,hpb,profnappe,qdrain,azlesd,azsup,QLES,DRAT, &
!                   drain,ruissel,remontee,qlesd,qdraincum,exces)
! DR 12/12/2011 pb dans la declaration manquait RU
!> This subroutine deals with water and nitrate transfers in soil.
!> - Stics book paragraphe 6.1.4.d., page 98
!!
!! -It calls the following functions and subroutines:
!!   - macroporosite : calculates the macroporosity of each layer
!!   - TransfertsDescendants : downward circulation of soil water
!!   - excesEau : circulation of soil water through the cracks, and upward circulation into macroporosity
!!   - nappe : calculates the water table caracteristics
!!   - drainageAgricole : deals with the case of artificially drained soils
!!   - calculRU : calculates the readily available water in the soil
!!   - calculRsurRU : calculates the soil water status as a proportion of readily available water
!!   - concNO3_EauxDrainage : calculates the NO3 concentration of deep water drainage
!!
!! -This subroutine make also:
!!   - initializes net evapotranspiration (etn) by elementary layer as the sum of water plant uptake (epz), evaporation (esz) and irrigation (irrigprof)
!!   - calculates nitrogen losses by leaching (QLES), runoff (DRAT) and deep drainage (drain)
!!   - calculates equivalent depth of the aquifer below the level of drains (de)
!!   - cumulates daily water outflow (qdraincum is the sum of qdrain calculated in the module drainageAgricole.f90)
!!   - calculates the anoxia index of each layer: if the layer's macroporosity has reached saturation, this anoxia index (anox) is allocated the value of 1 and can restrict
!!   root growth.
!!   - calculates the soil water reserve resmes and the nitrogen reserve (no3=azomes,nh3=ammomes) by integrating the elementary layer water and nitrogen contents, over the depth of soil.
subroutine lixiv(n,precip,hurlim,ruisselsurf,humin,nhe,nh,hucc,hr,P_codefente,ncel,icel,da,&  ! les entrées
                   izcel,P_infil,P_concseuil,P_humcapil,izc,hcc,P_capiljour,P_epc,hmin,P_codemacropor, &
                   P_profdrain,profsol,P_bformnappe,P_distdrain,P_codhnappe,ldrains,P_codrainage,    &
                   P_rdrain,P_profimper,P_ksol,profcalc,epz,esz, zrac,irrigprof,                 &
                   bouchon,hur,sat,azomes,anox,nit,pluieruissel,saturation,resmes,RU,  &  ! les sorties
                   RsurRu,concno3les,hnappe,hmax,hph,hpb,profnappe,qdrain,azlesd,azsup, &
                   QLES,DRAT,drain,ruissel,remontee,qlesd,qdraincum,exces,amm,ammomes,  &
                   profsem, P_profmesW, P_profmesN, SoilAvW, SoilWatM, SoilNM, lessiv, treeWaterUptake) ! ajout entrees et des sorties Macsur




!! 1- paramètres entrants
  integer, intent(IN)    :: n  
  integer, intent(IN)    :: nhe  
  integer, intent(IN)    :: nh  
  integer, intent(IN)    :: P_codefente  !> // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0 
  integer, intent(IN)    :: ncel  
  integer, intent(IN)    :: P_codemacropor  !> // PARAMETER // "Simulation option of water flux in the macroporosity of soils to estimate water excess and drip by  overflowing : yes(1), no (0)" // code 1/2 // PARSOL // 0
  integer, intent(IN)    :: P_codhnappe  !> // PARAMETER // mode of calculation of watertable level // code 1/2 // PARAM // 0 
  integer, intent(IN)    :: P_codrainage  !> // PARAMETER // artificial drainage (yes or no) // code 0/1 // PARSOL // 0 
  integer, intent(IN)    :: profcalc  
  integer, intent(IN)    :: izcel(5)                      ! dim = 5  
  integer, intent(IN)    :: izc(5)                        ! dim = 5.  
  integer, intent(IN)    :: icel(0:ncel)  
  real,    intent(IN)    :: precip   !> // OUTPUT // Daily amount of water (precipitation + irrigation)   // mm day-1
  real,    intent(IN)    :: hurlim  
  real,    intent(IN)    :: ruisselsurf   !> // OUTPUT // Surface run-off // mm
  real,    intent(IN)    :: zrac   !> // OUTPUT // Depth reached by root system // cm
  real,    intent(IN)    :: P_concseuil  !> // PARAMETER // Minimum Concentration of soil to NH4 // kgN ha-1 mm-1 // PARSOL // 1 
  real,    intent(IN)    :: P_humcapil  !> // PARAMETER // maximal water content to activate capillary rise // % pondéral // PARSOL // 1 
  real,    intent(IN)    :: P_capiljour  !> // PARAMETER // capillary rise upward water flux // mm j-1 // PARSOL // 1 
  real,    intent(IN)    :: P_profdrain  !> // PARAMETER // drain depth // cm // PARSOL // 1 
  real,    intent(IN)    :: profsol  
  real,    intent(IN)    :: P_bformnappe  !> // PARAMETER // coefficient of water table shape (artificial drained soil) // SD // PARAM // 1 
  real,    intent(IN)    :: P_distdrain  !> // PARAMETER // distance to the drain to calculate watertable height // cm // PARAM // 1 
  real,    intent(IN)    :: ldrains  
  real,    intent(IN)    :: P_rdrain  !> // PARAMETER // drain radius // cm // PARAM // 1 
  real,    intent(IN)    :: P_profimper  !> // PARAMETER // Upper depth of the impermeable layer (from the soil surface). May be greater than the soil depth // cm // PARSOL // 1
  real,    intent(IN)    :: P_ksol  !> // PARAMETER // hydraulic conductivity in the soil above and below the drains // SD // PARSOL // 1 
  real,    intent(IN)    :: hucc(int(profsol))                       ! nhe ou profsol. a priori profsol > nhe, donc profsol
  real,    intent(IN)    :: hr(nh)   !> // OUTPUT // Water content of the horizon 5 (table)    // % pond.
  real,    intent(IN)    :: humin(int(profsol))  
  real,    intent(IN)    :: epz(nhe)  
  real,    intent(IN)    :: esz(nhe)  
                            ! 0 to 5 pour transf, 0 to nh pour drainage, on va partir du principe que nh <= 5.
  real,    intent(IN)    :: P_infil(0:5)   !> // PARAMETER // infiltrability parameter at the base of each horizon  (if codemacropor = 1)// mm day-1 // PARSOL // 1   // OUTPUT // Infiltrability parameter at the base of the horizon 1 // mm day-1
  real,    intent(IN)    :: da(nh)   !> // OUTPUT // Bulk density in the horizon 1 // g cm-3
  real,    intent(IN)    :: hcc(nh)  
  real,    intent(IN)    :: hmin(nh)  
  real,    intent(IN)    :: P_epc(nh)  !< // PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm
  real,    intent(IN)    :: irrigprof(nhe)  

!! 2- paramètres sortants
  real,    intent(OUT)   :: azomes   !> // OUTPUT // daily amount of  no3-n integrated on the measurement depth // kgN.ha-1
  real,    intent(OUT)   :: ammomes   !> // OUTPUT // daily amount of  nh3-n integrated on the measurement depth // kgN.ha-1
  real,    intent(INOUT) :: hur(int(profsol))                        ! nhe, profsol, profcalc, a priori max = profsol
  real,    intent(INOUT) :: nit(int(profsol))                        ! nhe, profsol, profcalc, a priori max = profsol
  real,    intent(OUT)   :: sat(int(profsol))                        ! nhe, profsol, profcalc, a priori max = profsol
  real,    intent(OUT)   :: anox(nhe)  
  integer, intent(INOUT) :: bouchon   !> // OUTPUT // Index showing if the shrinkage slots are opened (0) or closed (1)  // 0-1

! variables calculées et utilisées par lixiv
! et ses sous-fonctions et globalisées
! uniquement pour les fonctions de sortie lecrap, lecsorti
  real,    intent(INOUT) :: DRAT   !> // OUTPUT // Water flux drained at the base of the soil profile integrated over the simulation periodout of the soil   // mm
  real,    intent(INOUT) :: QLES   !> // OUTPUT // Cumulated N-NO3 leached at the base of the soil profile // kgN.ha-1
  real,    intent(INOUT) :: qdraincum   !> // OUTPUT // Cumulated quantity of water evacuated towards drains // mm
  real,    intent(OUT)   :: pluieruissel  
  real,    intent(OUT)   :: saturation   !> // OUTPUT // Amount of water remaining in the soil macroporosity // mm
  real,    intent(OUT)   :: resmes   !> // OUTPUT // Amount of soil water in the measurement depth // mm
  real,    intent(OUT)   :: RsurRu   !> // OUTPUT // Fraction of available water reserve (R/RU) over the entire profile // 0 à 1
  real,    intent(OUT)   :: RU   !> // OUTPUT // maximum available water reserve over the entire profile // mm
  real,    intent(OUT)   :: concno3les   !> // OUTPUT // Nitrate concentration in drainage water // mg NO3 l-1
  real,    intent(OUT)   :: hnappe   !> // OUTPUT // Height of water table with active effects on the plant // cm
  real,    intent(OUT)   :: hmax   !> // OUTPUT // Maximum height of water table between drains // cm
  real,    intent(OUT)   :: hph   !> // OUTPUT // Maximum depth of perched water table // cm
  real,    intent(OUT)   :: hpb   !> // OUTPUT // Minimum depth of perched water table // cm
  real,    intent(OUT)   :: profnappe   !> // OUTPUT // Depth of water table // cm
  real,    intent(OUT)   :: qdrain   !> // OUTPUT // Flow rate towards drains // mm j-1
  real,    intent(OUT)   :: azlesd   !> // OUTPUT // Nitric nitrogen flow in drains // kgN.ha-1 j-1
  real,    intent(OUT)   :: azsup  
  real,    intent(OUT)   :: drain   !> // OUTPUT // Water flux drained at the base of the soil profile // mm j-1
  real,    intent(OUT)   :: ruissel   !> // OUTPUT // Daily run-off // mm
  real,    intent(OUT)   :: remontee   !> // OUTPUT // Capillary uptake in the base of the soil profile // mm j-1
  real,    intent(OUT)   :: qlesd   !> // OUTPUT // Cumulated N-NO3 leached into drains // kgN.ha-1
  real,    intent(OUT)   :: exces(0:5)   !> // OUTPUT // Amount of water  present in the macroporosity of the horizon 1  // mm

! dr 16/12/2013 ajout pour Macsur
  real,    intent(IN)    :: profsem
  integer,    intent(IN)    :: P_profmesW
  integer,    intent(IN)    :: P_profmesN
  real,    intent(OUT)   :: SoilAvW   !> // OUTPUT // Amount of soil available water in the measurement depth // mm
  real,    intent(OUT)   :: SoilWatM   !> // OUTPUT // Amount of soil available water in the observed measurement (macsur) // mm
  real,    intent(OUT)   :: SoilNM   !> // OUTPUT // daily amount of  no3-n integrated on the observed measurement depth // kgN.ha-1
  real,    intent(OUT)   :: lessiv   !> // OUTPUT // daily N-NO3 leached at the base of the soil profile // kgN.ha-1


end subroutine
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 9.2, page 169
!>
!> This module deals with downward circulation of soil water, and call the sub-module transf.f90.
!!
!! The nitrates circulate with water downwards through the soil. The way these transfers are accounted for in STICS relies on the soil compartmental description
!! and on the tipping bucket concept. The description of the soil can involve up to four compartments: microporosity, macroporosity,
!! cracks (the case of swelling clay soils) and pebbles.  However, only the description of microporosity is obligatory, the description of the other compartments
!! being optional. Whatever, soil microporosity is the basis for calculating water and nitrogen transfer values.
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine TransfertsDescendants(nit,hur,etn,hucc,ncel,icel,da,hr,nh,nhe,hurlim,izcel, &
                                   P_infil,P_concseuil,precip,husup,azsup, remontee,         &
                                   P_humcapil, izc, exces, azlesm, P_capiljour,P_codemacropor,P_epc)
! DR 06/09/2012 j'ajoute qq param pour Joel (codemacropor et epc)
! ENTREES
  integer, intent(IN)   :: ncel  
  integer, intent(IN)   :: nh  
  integer, intent(IN)   :: nhe  
  integer, intent(IN)   :: icel(0:ncel)  
  integer, intent(IN)   :: izcel(5)                         ! dim = 5. A faire varier ?  
  integer, intent(IN)   :: izc(5)                           ! dim = 5. A faire varier ?  
  real,    intent(IN)   :: hurlim  
  real,    intent(IN)   :: P_concseuil  !> // PARAMETER // Minimum Concentration of soil to NH4 // kgN ha-1 mm-1 // PARSOL // 1 
  real,    intent(IN)   :: precip   !> // OUTPUT // Daily amount of water (precipitation + irrigation)   // mm day-1
  real,    intent(IN)   :: P_humcapil  !> // PARAMETER // maximal water content to activate capillary rise // % pondéral // PARSOL // 1 
  real,    intent(IN)   :: P_capiljour  !> // PARAMETER // capillary rise upward water flux // mm j-1 // PARSOL // 1 
  real,    intent(IN)   :: etn(nhe)                           ! 1 to icel(ncel), 1 to nhe. Il faut prendre le max, a priori nhe >= icel(ncel)
  real,    intent(IN)   :: hucc(nhe)                          ! 1 to icel(ncel), 1 to nhe. Il faut prendre le max, a priori nhe >= icel(ncel)
  real,    intent(IN)   :: da(nh)                           ! (nh)    // OUTPUT // Bulk density in the horizon 1 // g cm-3
  real,    intent(IN)   :: hr(nh)                           ! (nh)    // OUTPUT // Water content of the horizon 5 (table)    // % pond.
                         ! 0 à 5 d'après transf
  real,    intent(IN)   :: P_infil(0:5) !> // PARAMETER // infiltrability parameter at the base of each horizon (if codemacropor=1) // mm day-1 // PARSOL // 1   // OUTPUT // Infiltrability parameter at the base of the horizon 1 // mm day-1

  integer, intent(IN)    :: P_codemacropor  !< // PARAMETER // "Simulation option of water flux in the macroporosity of soils to estimate water excess and drip by  overflowing : yes(1), no (0)" // code 1/2 // PARSOL // 0
  real,    intent(IN)    :: P_epc(5)  !< // PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm

! SORTIES
  real,    intent(OUT)   :: azlesm  
  real,    intent(OUT)   :: azsup  
  real,    intent(OUT)   :: husup  
  real,    intent(OUT)   :: remontee   !> // OUTPUT // Capillary uptake in the base of the soil profile // mm j-1
  real,    intent(INOUT) :: nit(nhe)                          ! 1 to icel(ncel), 1 to nhe. Il faut prendre le max, a priori nhe >= icel(ncel)
  real,    intent(INOUT) :: hur(nhe)                          ! 1 to icel(ncel), 1 to nhe. Il faut prendre le max, a priori nhe >= icel(ncel)
  real,    intent(INOUT) :: exces(0:5)                      ! 0 to 5 d'après transf    // OUTPUT // Amount of water  present in the macroporosity of the horizon 1  // mm

end subroutine TransfertsDescendants

!DR 23/07/2012 pas besoin de codefente
!subroutine nappe(P_codefente,macropor,P_epc,hnappe,hnapperch,hmax,       &
subroutine nappe(macropor,P_epc,hnappe,hnapperch,hmax,       &
                   P_profdrain,nh,profsol,P_bformnappe,exces,              &
                   nhe,hph,hpb,de,P_distdrain,profnappe,P_codhnappe,ldrains)
  ! 1- arguments entrants
!    integer, intent(IN)               ::  P_codefente  !>  // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0
    integer, intent(IN)               ::  nh  !>  
    integer, intent(IN)               ::  nhe  !>  
    integer, intent(IN)               ::  P_codhnappe  !>  // PARAMETER // mode of calculation of watertable level // code 1/2 // PARAM // 0 
    real, dimension(5), intent(IN)    ::  macropor  !>  
    real, dimension(5), intent(IN)    ::  P_epc  !>  // PARAMETER // Thickness of the horizon H   (table) // cm   // PARSOL // 1   // OUTPUT // Thickness of the horizon 2 // cm
    real, dimension(0:5), intent(IN)  ::  exces   !> // OUTPUT // Amount of water  present in the macroporosity of the horizon 1  // mm
    real, intent(IN)                  ::  hnapperch  !>  
    real, intent(IN)                  ::  P_profdrain  !>  // PARAMETER // drain depth // cm // PARSOL // 1 
    real, intent(IN)                  ::  profsol  
    real, intent(IN)                  ::  P_bformnappe  !>  // PARAMETER // coefficient of water table shape (artificial drained soil) // SD // PARAM // 1 
    real, intent(IN)                  ::  de  !>  
    real, intent(IN)                  ::  P_distdrain  !>  // PARAMETER // distance to the drain to calculate watertable height // cm // PARAM // 1 
    real, intent(IN)                  :: ldrains  
  ! 2- arguments sortants
    real, intent(OUT)                 ::  Hmax  !>    // OUTPUT // Maximum height of water table between drains // cm
    real, intent(OUT)                 ::  Hnappe  !>    // OUTPUT // Height of water table with active effects on the plant // cm
    real, intent(OUT)                 ::  Hpb  !>    // OUTPUT // Minimum depth of perched water table // cm
    real, intent(OUT)                 ::  Hph  !>    // OUTPUT // Maximum depth of perched water table // cm
    real, intent(OUT)                 ::  profnappe   !> // OUTPUT // Depth of water table // cm

end subroutine nappe

! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!>
!> - Stics book paragraphe 9.2.4, page 173
!>
!! This module deals with the circulation of soil water through the cracks, and the upward circulation into macroporosity.
!! In the case of swelling soils, the fissures, when open, are filled by overflow from the surface layer; water supply by rain interception at the surface is
!! not taken into consideration.  The opening of cracks (bouchon variable) depends on the combination of two factors in at least one of the layers:
!! empty macroporosity and a root front deeper than the base of the layer
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine excesEau(nh,nhe,P_codemacropor,P_codefente,pluiefente,zrac,macropor,P_epc,hur,hucc,bouchon,exces)
    ! ENTREES
      integer, intent(IN)    :: nh  
      integer, intent(IN)    :: nhe  
      integer, intent(IN)    :: P_codemacropor  !> // PARAMETER // "Simulation option of water flux in the macroporosity of soils to estimate water excess and drip by  overflowing : yes(1), no (0)" // code 1/2 // PARSOL // 0
      integer, intent(IN)    :: P_codefente  !> // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0 
      real,    intent(IN)    :: pluiefente  
      real,    intent(IN)    :: zrac   !> // OUTPUT // Depth reached by root system // cm
      real,    intent(IN)    :: P_epc(nh)  !< // PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm
      real,    intent(IN)    :: hucc(nhe)  
      real,    intent(IN)    :: macropor(nh)  
    ! SORTIES
      integer, intent(OUT)   :: bouchon   !> // OUTPUT // Index showing if the shrinkage slots are opened (0) or closed (1)  // 0-1
      real,    intent(OUT)   :: hur(nhe)  
      real,    intent(OUT)   :: exces(0:5)   !> // OUTPUT // Amount of water  present in the macroporosity of the horizon 1  // mm
  end subroutine excesEau

! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> calculation of agricultural drainage
!> - Stics book paragraphe 9.3, page 174-176
!>
!! The classic draining system uses the properties of symmetry arising from the presence of lines of drains with spacing (2 ldrains) which is generally constant
!! within a field.  Flow is assumed to occur from the space between drains towards the drain following a shape characterized by the parameter bformnappe
!! (in the module hauteurNappe.f90); it occurs within a water table based on an impermeable floor, the depth of which (profimper, in the module lixiv.f90) may be
!! greater than the soil depth considered in STICS.
!!
!! A simplification of the baseline Hooghoudt equation (1940) is used to simulate the daily water outflow (qdrain) at the drain level assuming a single hydraulic
!! conductivity above and below the drains (ksol). This equation relies on the estimation of de, the equivalent depth of the aquifer below the level of drains,
!! which first requires calculating hmax (de is calculated in the module lixiv.f90 and hmax is calculated in the module hauteurNappe.f90).
!! The Hooghoudt equation is normally valid under a permanent regime, but it was shown (Zimmer, 2001) that for sufficiently large time steps, it provides entirely
!! satisfactory predictions of the flows and water table heights in drainage systems.  The operating principle is as follows: when gravity flow begins following
!! saturation of the microporosity in the system, the macroporosity fills and creates a water table, whose level is at the top of the layer whose macroporosity
!! is saturated.  If we know the system parameters and the height of the previous table, a quantity of drained water is calculated, to which may be added,
!! if relevant, drainage linked to exchanges with deep layers of the soil.  The sum of these two drainage quantities is subtracted from the water contained in
!! the macroporosity, and a new water table height is calculated.
!!
!! Although it does not appear explicitly in the equations, the porosity of drainage plays an important role in the emptying and filling of soil macroporosity.
!! As a general rule, the simulations are correct only when the value of the soil macroporosity is equal to its drainage porosity.
!! In order to be able to account for the field heterogeneity due to the drainage system, it is possible to calculate the plant effects of the presence of a
!! water table either at the drain level or at the inter-drain level or for an average level.
!!
!! Nitrates can be leached through the drains and their amount is calculated assuming that nitrate concentration in the drained water is that of the hnappe level
!! (hnappe is calculated in the module hauteurNappe.f90).
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine drainageAgricole(n,nh,P_infil,hur,macropor,ldrains,P_profdrain,profsol,P_ksol,P_concseuil,de, &
                              exces,nit,hmax,qdrain,azlesd,qlesd,hnappe)
! ENTREES
  integer, intent(IN)    ::  nh  
  integer, intent(IN)    ::  n  
  real,    intent(IN)    ::  P_infil(0:nh)  !> // PARAMETER // infiltrability parameter at the base of each horizon // mm day-1 // PARSOL // 1   // OUTPUT // Infiltrability parameter at the base of the horizon 1 // mm day-1
  real,    intent(IN)    ::  hur(int(profsol))  
  real,    intent(IN)    ::  macropor(nh)  
  real,    intent(IN)    ::  ldrains  
  real,    intent(IN)    ::  P_profdrain  !> // PARAMETER // drain depth // cm // PARSOL // 1 
  real,    intent(IN)    ::  profsol  
  real,    intent(IN)    ::  P_ksol  !> // PARAMETER // hydraulic conductivity in the soil above and below the drains // SD // PARSOL // 1 
  real,    intent(IN)    ::  P_concseuil  !> // PARAMETER // Minimum Concentration of soil to NH4 // kgN ha-1 mm-1 // PARSOL // 1 
  real,    intent(IN)    ::  de  

! SORTIES
  real,    intent(INOUT) ::  exces(0:nh)   !> // OUTPUT // Amount of water  present in the macroporosity of the horizon 1  // mm
  real,    intent(INOUT) ::  nit(int(profsol))  
  real,    intent(INOUT) ::  hmax   !> // OUTPUT // Maximum height of water table between drains // cm
  real,    intent(INOUT) ::  qdrain   !> // OUTPUT // Flow rate towards drains // mm j-1
  real,    intent(INOUT) ::  azlesd   !> // OUTPUT // Nitric nitrogen flow in drains // kgN.ha-1 j-1
  real,    intent(INOUT) ::  qlesd   !> // OUTPUT // Cumulated N-NO3 leached into drains // kgN.ha-1
  real,    intent(INOUT) ::  hnappe   !> // OUTPUT // Height of water table with active effects on the plant // cm

end subroutine drainageAgricole

! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module deals with downward circulation of soil water
!> - Stics book paragraphe 9.2.2, page 169-170
!>
!! This module deals with downward circulation of soil water, and is called by the module transfertsDescendants
!! Water transfer in the soil microporosity is calculated per elementary 1 cm layer using a reservoir-type analogy.  Water fills the layers by downward flow,
!! assuming that the upper limit of each basic reservoir corresponds to the layer's field capacity.
!! If the flow is not obstructed, (see macroporosite.f90), the excess water above field capacity is drained downward.  The soil layers affected by evaporation,
!! can dry until they reach the residual soil water content. In deeper layers, the water is only extracted by the plant and therefore always remains above
!! the wilting point.
!!
!! The transfer of nitrates is also described using this reservoir-type analogy, according to the "mixing cells" principle.  Any nitrate arriving by convection
!! with water in the elementary layer mixes with the nitrate already present.  Excess water then leaves with the new concentration of the mixture.
!! This description produces results which are very similar to the convection-dispersion model, the thickness of layers being equal to twice the
!! dispersivity (Mary et al., 1999).
!!
!! A minimum concentration level may exist (concseuil), below which mineral nitrogen cannot be leached. This can be a simple way to simulate ammonia nitrogen
!! without using the simulation of the ammoniacal phase of mineralisation.
!!
!! The amounts of drained water and leached nitrogen, i.e. leaving via the base of the soil profile are not retrievable by another crop.
!! Upwards nitrate movements occur via plant uptake only. Capillary rises provided by humid subsoil can be taken into account. Indeed, if the basal soil layer
!! is dry enough (below the humcapil threshold), capillary rise can occur from the subsoil into the soil, at a constant rate (capiljour) until the basal layer
!! reaches a moist status (above humcapil). As, in the model, these upward transfers take place through the macroporosity (they are considered negative
!! infiltration), they require a zero value of infiltrability at the base of the soil to be active.
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c
! DR 06/09/2012 j'ajoute qq param pour Joel (codemacropor, daf et epc)
subroutine transf(nhe,hucc,hur,nit,etn,izc,hurlim,exces,P_infil,P_concseuil, &
                    precip,husup,azsup,remontee,nh,P_capiljour,P_humcapil,hr,P_codemacropor,da,P_epc)

      integer, intent(IN)    :: nhe  
      integer, intent(IN)    :: nh  
      integer, intent(IN)    :: izc(5)  
      real,    intent(IN)    :: hurlim  
      real,    intent(IN)    :: P_concseuil  !> // PARAMETER // Minimum Concentration of soil to NH4 // kgN ha-1 mm-1 // PARSOL // 1 
      real,    intent(IN)    :: precip   !> // OUTPUT // Daily amount of water (precipitation + irrigation)   // mm day-1
      real,    intent(IN)    :: P_capiljour  !> // PARAMETER // capillary rise upward water flux // mm j-1 // PARSOL // 1 
      real,    intent(IN)    :: P_humcapil  !> // PARAMETER // maximal water content to activate capillary rise // % pondéral // PARSOL // 1 
      real,    intent(IN)    :: hr   !> // OUTPUT // Water content of the horizon 5 (table)    // % pond.
      real,    intent(IN)    :: hucc(nhe)  
      real,    intent(IN)    :: etn(nhe)  
      real,    intent(IN)    :: P_infil(0:5)  !> // PARAMETER // infiltrability parameter at the base of each horizon // mm day-1 // PARSOL // 1   // OUTPUT // Infiltrability parameter at the base of the horizon 1 // mm day-1

      integer, intent(IN)    :: P_codemacropor
      real,    intent(IN)    :: da(5)
      real,    intent(IN)    :: P_epc(5)  !< // PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm

      real,    intent(OUT)   :: husup  
      real,    intent(OUT)   :: azsup  
      real,    intent(OUT)   :: remontee   !> // OUTPUT // Capillary uptake in the base of the soil profile // mm j-1
      real,    intent(INOUT) :: exces(0:5)   !> // OUTPUT // Amount of water  present in the macroporosity of the horizon 1  // mm
      real,    intent(OUT)   :: hur(nhe)  
      real,    intent(OUT)   :: nit(nhe)  
  end subroutine transf

  function sommeCouchesParCellule(nbCellules, iCellules, tabCouches)
    integer, intent(in) :: nbCellules  
    integer, intent(in) :: iCellules(0:nbCellules)  
    real,    intent(in) :: tabCouches(iCellules(nbCellules))  
    real, dimension(nbCellules) :: sommeCouchesParCellule  
  end function sommeCouchesParCellule


! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 9.4.1, page 176
!>
!! By integrating the difference between hur(iz)-humin(iz) over the soil depth, profsol, and weighted by the difference between hucc(iz)-humin(iz)
!! (corresponding to ru calculated by the function calculRU.f90) gives the soil water status as a proportion of readily available water (calculRsurRU).
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c
  real function calculRsurRU(RU, profsol, hur, sat, humin)
    real, intent(IN)  :: RU   !> // OUTPUT // maximum available water reserve over the entire profile // mm
    real, intent(IN)  :: profsol  
    real, intent(IN)  :: hur(int(profsol))  
    real, intent(IN)  :: humin(int(profsol))  
    real, intent(IN)  :: sat(int(profsol))  
  end function calculRsurRU

! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> calculation of the readily available water in the soil
!> - Stics book paragraphe 9.4.1, page 176
!!
!! This function calculates the readily available water in the soil as the difference between hucc(iz)-humin(iz) over the soil depth (profsol).
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c
  real function calculRU(profsol, hucc, humin)
    real, intent(IN)  :: profsol  
    real, intent(IN)  :: hucc(int(profsol))  
    real, intent(IN)  :: humin(int(profsol))  
  end function calculRU



  real function concNO3_EauxDrainage(husup, QLES, DRAT)
    real, intent(IN)  :: husup  !>  
    real, intent(IN)  ::  QLES  !>    // OUTPUT // Cumulated N-NO3 leached at the base of the soil profile // kgN.ha-1
    real, intent(IN)  ::  DRAT   !> // OUTPUT // Water flux drained at the base of the soil profile integrated over the simulation periodout of the soil   // mm
  end function concNO3_EauxDrainage

end interface

end module Lixivation
 
 
