!>
!!   lixiv - Routine principale du module lixivation
!!   Module : lixivation
!!   Typologie : scientifique
!!
!!   Simulation du transfert d'eau et de nitrate dans le sol
!!       Processus simulés:
!!         - ruissellement
!!         - infiltration dans la macroporosité
!!         - excès d'eau
!!         - ecoulement dans les drains
!!   Introduction de l'épaisseur de dispersion P_epd
!!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
subroutine lixiv(n,precip,hurlim,ruisselsurf,humin,nhe,nh,hucc,hr,P_codefente,ncel,icel,da,&  ! les entrées
                 izcel,P_infil,P_concseuil,P_humcapil,izc,hcc,P_capiljour,P_epc,hmin,P_codemacropor, &
                 P_profdrain,profsol,P_bformnappe,P_distdrain,P_codhnappe,ldrains,P_codrainage,    &
                 P_rdrain,P_profimper,P_ksol,profcalc,epz,esz, zrac,irrigprof,                 &
                 bouchon,hur,sat,azomes,anox,nit,pluieruissel,saturation,resmes,RU,      &  ! les sorties
                 RsurRU,concno3les,hnappe,hmax,hph,hpb,profnappe,qdrain,azlesd,azsup,    &
                 QLES,DRAT,drain,ruissel,remontee,qlesd,qdraincum,exces,amm,ammomes,     &
                 profsem,P_profmesW, P_profmesN, SoilAvW, SoilWatM, SoilNM, lessiv, treeWaterUptake) ! ajout entrees et des sorties Macsur

! On utilise le module Lixivation pour connaitre les interfaces d'appels des routines et fonctions suivantes
USE Lixivation, only: TransfertsDescendants, excesEau, nappe, drainageAgricole, calculRsurRU, calculRU, concNO3_EauxDrainage
USE Divers, only: macroporosite


! Tout explicite
implicit none

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
  real,    intent(IN)    :: hucc(int(profsol))                       ! nhe ou profsol. à priori profsol > nhe, donc profsol  
  real,    intent(IN)    :: hr(nh)   !> // OUTPUT // Water content of the horizon 5 (table)    // % pond.
  real,    intent(IN)    :: humin(int(profsol))  
  real,    intent(IN)    :: epz(nhe)  
  real,    intent(IN)    :: esz(nhe)  
 ! 0 à 5 pour transf, 0 à nh pour drainage, on va partir du principe que nh <= 5.
  real,    intent(IN)    :: P_infil(0:5) !> // PARAMETER // infiltrability parameter at the base of each horizon (if codemacropor = 1)// mm day-1 // PARSOL // 1   // OUTPUT // Infiltrability parameter at the base of the horizon 1 // mm day-1
  real,    intent(IN)    :: da(nh)   !> // OUTPUT // Bulk density in the horizon 1 // g cm-3
  real,    intent(IN)    :: hcc(nh)  
  real,    intent(IN)    :: hmin(nh)  
  real,    intent(IN)    :: P_epc(nh)  !< // PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm
  real,    intent(IN)    :: irrigprof(nhe)  


!! 2- paramètres sortants
  real,    intent(OUT)   :: azomes   !> // OUTPUT // daily amount of  no3-n integrated on the measurement depth // kgN.ha-1
  real,    intent(OUT)   :: ammomes   !> // OUTPUT // daily amount of  nh3-n integrated on the measurement depth // kgN.ha-1
  real,    intent(INOUT) :: hur(max(int(profsol),profcalc))                        ! nhe, profsol, profcalc, a priori max = profsol ! FAUX, profcalc peut être + grand que profsol
  real,    intent(INOUT) :: nit(max(int(profsol),profcalc))                        ! nhe, profsol, profcalc, a priori max = profsol
! DR 31/08/2012 ajout de amm pour pouvoir calculer ammmes
  real,    intent(IN)    :: amm(max(int(profsol),profcalc))
  real,    intent(OUT)   :: sat(max(int(profsol),profcalc))                        ! nhe, profsol, profcalc, a priori max = profsol
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
  real,    intent(OUT)   :: RU   !> // OUTPUT // maximum available water reserve over the entire profile // mm
  real,    intent(OUT)   :: RsurRU   !> // OUTPUT // Fraction of available water reserve (R/RU) over the entire profile // 0 to 1
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

!!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
 real,    intent(IN)     :: treeWaterUptake(1000)

!! 3- VARIABLES LOCALES
  integer               ::  profhoriz  !>  
  integer               :: ic  !>  
  integer               :: ii  !>  
  integer               :: iz  
  real                  ::  husup  !>  
  real                  ::  de  !>  
  real                  ::  pi  !>  
  real                  ::  delt  
  real, dimension(nh)   ::  macropor  
  !real, dimension(0:5)  ::  valeurs
  real, dimension(1000) ::  etn  
  real                  ::  hnapperch  
  real                  ::  pluiefente  !>  
  real                  ::  azlesm  



!==========================================================

    !! En absence de bouchon, le ruissellement déborde dans les fentes
    !- Dès qu'un horizon est bouché, la fente est fermée (bouchon=1)
      if (bouchon /= 1 .and. P_codefente == 1) then
        pluiefente = pluieruissel
      else
        pluiefente = 0. ! par précaution on affecte à zéro
      endif

    ! calcul de la macroporosité = f(ds,da,Hcc)
      do ic = 1,nh
        macropor(ic) = macroporosite(P_codefente,da(ic),hcc(ic),hmin(ic),P_epc(ic))
      end do


    !! initialisation anoxie et calcul evapotranspiration nette (etn)
      etn(:) = 0. ! Par défaut, on met tout à zéro dans le tableau

      do iz = 1,nhe
        anox(iz)= 0.
        etn(iz) = epz(iz) + esz(iz) - irrigprof(iz)
        !!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
        !!!Sera à vide si pas d'arbres
        etn(iz) = etn(iz) + treeWaterUptake(iz)

      end do


      call TransfertsDescendants(nit,hur,etn,hucc,ncel,icel,da,hr,nh,nhe,hurlim, &
                                 izcel, P_infil, P_concseuil, precip, husup,azsup,   &
                                 remontee, P_humcapil, izc, exces, azlesm, P_capiljour,P_codemacropor,P_epc)


    ! lessivage et drainage  a la base du profil
      QLES = QLES+azsup
      DRAT = DRAT+husup
      drain = husup



! DR_2010      write(*,*)'drain',drain
! dr 26/03/2014 reactivation , la variable n'etait plus calculée
     lessiv = azsup

    ! modif Bruno calcul cumulé 29/04/03
    !- calcul de la concentration cumulée en nitrate des eaux de drainage
      concno3les = concNO3_EauxDrainage(husup, QLES, DRAT)


    ! cas d'exces d'eau : circulation ascendante dans la macroporosité
    ! ----------------------------------------------------------------
      call excesEau(nh,nhe,P_codemacropor,P_codefente,pluiefente,zrac,macropor,P_epc,hur,hucc,bouchon,exces)


    ! calcul des hauteurs de nappe
    !-----------------------------
    !DR 23/07/2012 pas besoin de codefente
    !  call nappe(P_codefente,macropor,P_epc,hnappe,hnapperch,hmax,           &
      call nappe(macropor,P_epc,hnappe,hnapperch,hmax,           &
                 P_profdrain,nh,profsol,P_bformnappe,exces,                  &
                 nhe,hph,hpb,de,P_distdrain,profnappe,P_codhnappe,ldrains)


    ! cas du drainage agricole
    !-------------------------
      if(P_codrainage.eq.1) then

      ! si imperméable infini
        pi = 3.141592654
        delt = min(((hmax/ldrains)**0.36-hmax/ldrains)/2., pi/(4*log((2*ldrains)/(pi*P_rdrain))))
        if((P_profimper-P_profdrain).ge.(ldrains/2.)) then
          de = delt*ldrains
        else
          if((P_profimper-P_profdrain).le.0.) then
            de = 0.
          else
            de = (ldrains*(P_profimper-P_profdrain)*delt)/(ldrains*delt+(P_profimper-P_profdrain))
          endif
        endif

        call drainageAgricole(n,nh,P_infil,hur,macropor,ldrains,P_profdrain,profsol,P_ksol,P_concseuil,de,  &
                              exces,nit,hmax,qdrain,azlesd,qlesd,hnappe)



      ! mise à jour de la hauteur de la nappe
      !DR 23/07/2012 pas besoin de codefente
      !  call nappe(P_codefente,macropor,P_epc,hnappe,hnapperch,hmax,       &
        call nappe(macropor,P_epc,hnappe,hnapperch,hmax,       &
                   P_profdrain,nh,profsol,P_bformnappe,exces,              &
                   nhe,hph,hpb,de,P_distdrain,profnappe,P_codhnappe,ldrains)


      ! on cumule les quantités de drainage au fil du temps
        qdraincum = qdraincum + qdrain
      endif


    ! calcul du partage entre ruissellement et alimentation des fentes
      if ( bouchon == 1 .or. P_codefente /= 1 ) then
        ruissel = ruisselsurf + exces(0)
        pluieruissel = 0.
      else
        ruissel = ruisselsurf
        pluieruissel = exces(0)
      endif

    ! on vide le réservoir au dessus du sol s'il y a ruissellement
      if (pluieruissel.eq.0.) exces(0)=0.

      profhoriz = nhe
      saturation = 0.
      do ic = nh,1,-1
        do ii = 1,int(P_epc(ic))
          iz = profhoriz-ii+1
          sat(iz) = exces(ic)/P_epc(ic)
        ! recalcul du taux d'anoxie anox(iz) en fonction de la nappe
          if(iz >= Hph .and. iz <= Hpb) anox(iz) = 1.
          if(iz >= profnappe) anox(iz) = 1.
          saturation = saturation+sat(iz)
        end do
        profhoriz = profhoriz-int(P_epc(ic))
      end do

    ! calcul de la réserve jusqu'à profcalc
      resmes = SUM(hur(1:int(profcalc))+sat(1:int(profcalc)))
      azomes = SUM(nit(1:int(profcalc)))
      ammomes = SUM(amm(1:int(profcalc)))
    ! ajout de 3 sorties pour macsur de calcul de resreves sur des profondeurs specifiques
      SoilAvW = SUM(max(hur(1:int(profcalc)) + sat(1:int(profcalc)) - humin(1:int(profcalc)),0.))
! dr 14/11/2014 je corrige le calcul de soilWatM qui est la teneur totale sur la prof de mesure et no l'available
!      SoilWatM = SUM(max(hur(1:int(P_profmesW)) + sat(1:int(P_profmesW)) - humin(1:int(P_profmesW)),0.))
      SoilWatM = SUM(hur(1:int(P_profmesW)) + sat(1:int(P_profmesW)))

      SoilNM = SUM(nit(1:int(P_profmesN))) + SUM(amm(1:int(P_profmesN)))

    ! calcul de RsurRU
      RU = calculRU(profsol, hucc, humin)
      RsurRU = calculRsurRU(RU, profsol, hur, sat, humin)




end subroutine lixiv

!>
!! TODO: A TRANSFERER DANS UNE AUTRE UNITE DE PROGRAMME ?
!! Calcul de la concentration cumulée en nitrate des eaux de drainage
!!
!!       husup :
!!       QLES :
!!       DRAT :
!!
!! @return :
!!
real function concNO3_EauxDrainage(husup, QLES, DRAT)

  real, intent(IN)  :: husup  !>  
  real, intent(IN)  ::  QLES  !>    // OUTPUT // Cumulated N-NO3 leached at the base of the soil profile // kgN.ha-1
  real, intent(IN)  ::  DRAT   !> // OUTPUT // Water flux drained at the base of the soil profile integrated over the simulation periodout of the soil   // mm

  if (husup>0.) then
    concNO3_EauxDrainage = QLES/DRAT*6200./14.
  else
    concNO3_EauxDrainage = 0.
  endif

end function concNO3_EauxDrainage
 
 
