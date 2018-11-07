!*********************************************************************
!  version 5.0
! derniere modif NB 07/06/2001
!*********************************************************************
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module calculates root growth.
!> - Stics book paragraphe 5, 5.1, 5.2, page 85-90
!>
!! In the model, roots only act as water and mineral nitrogen absorbers, and are described by their front depth and density profile.
!! The root growth begins at germination (for sown plants) or at planting (for  transplanted crops, possibly after a latency phase), and it stops at a
!! given stage of development, depending on the species (stoprac which can be either LAX, FLO or MAT).
!>    - Root front growth (subroutine 'croissanceFrontRacinaire' in this module):
!!   A first calculation gives the depth of the root front (zrac) beginning at the sowing depth (profsem) for sown crops and at an initial value for
!!   transplanted crops (profsem + zracplantule) or perennial crops (zrac0). The root front growth stops when it reaches the depth of soil or an obstacle
!!   that can be physical or chemical (the obstacle depth is defined by the parameter obstarac) or when the phenological stopping stage has been reached.
!!   For indeterminate crops, when trophic competition prevents vegetative growth, the root front growth is stopped (except before the IAMF stage, when root growth
!!   is given priority).
!!   A first calculation of the front growth rate (deltaz in cm.d-1) is proportional to temperature with a coefficient depending on the variety (croirac).
!!   This value is then multiplied by the water and bulk density stress indices. The thermal function relies on crop or soil temperature according to
!!   the root growth dependence on the collar or apex temperature. If the driving temperature is that of the crop, the cardinal temperatures (tcmin and tcmax)
!!   are the same as those used for the thermal function of the leaf growth rate. If the driving temperature is that of the soil at level zrac (±1cm),
!!   the minimum temperature is the base temperature for germination (tgmin) but the maximum temperature does not change.
!!   The water and bulk density stress index is calculated as the product of 3 variables, depending on soil dryness (humirac),
!!   water logging (izrac), and bulk density (efda). The efda variable constitutes a constraint to penetration in the case of compacted soils,
!!   or more rarely a slowing of root penetration linked to a lack of soil cohesiveness. Root penetration is not constrained between the bulk density
!!   thresholds dacohes and daseuilbas. Above a bulk density threshold daseuilhaut the effect of bulk density (DA) on root penetration is constant
!!   and corresponds to the sensitivity of the plant to the penetration constraint; it is equal to contrdamax. daseuilbas and daseuilhaut values are
!!   1.4 and 2.0 respectively. The dacohes value is poorly understood and we only provide an order of magnitude. The bulk density is the effective one,
!!   taking into account fine earth and pebbles.
!>    - Growth in root density:
!!   The root density profile is calculated according to two possible options. The ‘standard profile’ option makes it possible to calculate the root profile
!!   that is effective with respect to absorption. The ‘true density’ option allows the actual root density profile to be estimated, which is more relevant
!!   in order to simulate low-density crops, for which root density is never optimal, or in order to take into consideration the effects of constraints imposed
!!   by the soil on root distribution. Whatever the chosen option, roots only play a role as absorbers of water and mineral nitrogen.  It is possible to estimate
!!   the root mass with the second option and to account for a direct link between shoot and root growing rates. However an indirect link exists in all
!!   calculations through temperature, which affects both levels.
!>         - Standard profile (subroutine 'profilracinaire' in this module): this option enables calculation of the root profile which is efficient in terms of absorption.
!!      It is defined by the maximum current depth, zrac, and a prescribed efficient root density profile, lracz (Z). This profile is calculated dynamically
!!      as a function of zrac and takes a sigmoidal form depending on the zlabour, zprlim and zpente parameters. These parameters define the form of the
!!      reference root profile and are of considerable importance in terms of their interrelationships, but they do not define the final shape of the root system.
!!      In this respect, it is the differences between zpente and zlabour, and particularly between zprlim and zpente which are determinant.
!!      zlabour corresponds to the depth of the tilled layer, where it is assumed that root proliferation is not limited with respect to water and mineral
!!      absorption: root density is optimum at this level (lvopt). zpente is the depth at which root uptake efficiency is reduced by half, and zprlim is the
!!      depth of the root front to which this reference profile can be attributed.
!!     The value used for the optimum root density threshold, LVOPTG, is 0.5 cm cm-3 soil (Brisson, 1998c).  In this way, it is possible to represent a
!!      root system for various species exhibiting fasciculate or pivotal type root systems.
!>         - True density (see the module 'densirac.f90')
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine densiteRacinaire(                                                                           &
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
  real,              intent(IN)    :: P_daseuilbas  !> // PARAMETER // Threshold of bulk density of soil below that the root growth is not limited // g cm-3 // PARAM // 1 
  real,              intent(IN)    :: P_daseuilhaut  !> // PARAMETER // Threshold of bulk density of soil below that the root growth  no more possible // g cm-3 // PARAM // 1 
  real,              intent(IN)    :: P_dacohes  !> // PARAMETER // bulk density under which root growth is reduced due to a lack of cohesion d // g cm-3 // PARAM // 1 
  real,              intent(IN)    :: P_lvopt  !> // PARAMETER // Optimum root density // cm root.cm-3 soil // PARAM // 1 
  integer,           intent(IN)    :: P_coderacine  !> // PARAMETER // Choice of estimation module of growth root in volume: standard profile (1) or by actual density (2) // code 1/2 // PARPLT // 0 
  real,              intent(IN)    :: P_contrdamax  !> // PARAMETER // maximal root growth reduction due to soil strenghtness (high bulk density) // SD // PARPLT // 1 
  real,              intent(IN)    :: P_sensanox  !> // PARAMETER // anoxia sensitivity (0=insensitive) // SD // PARPLT // 1 
  real,              intent(INOUT) :: zrac   !> // OUTPUT // Depth reached by root system // cm
  real,              intent(IN)    :: P_zlabour  !> // PARAMETER // Depth of ploughing  // cm // PARPLT // 1 
  real,              intent(IN)    :: P_zpente  !> // PARAMETER // Depth where the root density is ½ of the surface root density for the reference profile // cm  // PARPLT // 1 
  real,              intent(IN)    :: P_zprlim  !> // PARAMETER // Maximum depth of the root profile for the reference profile // cm  // PARPLT // 1 
  real,              intent(INOUT) :: znonli            ! dans le doute, INOUT  
  real,              intent(OUT)   :: difrac  
  real,              intent(IN)    :: lai   !> // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,              intent(IN)    :: tauxcouv   !> // OUTPUT // Cover rate // SD
  integer,           intent(IN)    :: nrec  
  integer,           intent(IN)    :: codeinstal  
  integer,           intent(IN)    :: nger  
  real,              intent(INOUT) :: rl(nbCouches)     ! n-1 (0) & n (1)  
  real,              intent(INOUT) :: deltaz   !> // OUTPUT // Deepening of the root front  // cm day-1
  real,              intent(INOUT) :: dtj(n)            ! 1 to n    // OUTPUT // Daily efficient temperature for the root growing  // degree C.j-1
  integer,           intent(IN)    :: nlev  
  real,              intent(OUT)   :: cumlracz   !> // OUTPUT // Sum of the effective root lengths  // cm root.cm -2 soil
  real,              intent(OUT)   :: poussracmoy   !> // OUTPUT // "Average index of the effect of soil constraints on the rooting profile (option  true density )" // 0-1
  real,              intent(INOUT) :: lracsenz(nbCouches)  
  real,              intent(OUT)   :: cumflrac  
  real,              intent(INOUT) :: racnoy  
  real,              intent(INOUT) :: flrac(nbCouches)  
  real,              intent(INOUT) :: lracz(nbCouches)  
  real,              intent(OUT)   :: cumlraczmaxi  
  real,              intent(INOUT) :: zracmax  
  real,              intent(IN)    :: P_profsem  !> // PARAMETER // Sowing depth // cm // PARTEC // 1 
  integer,           intent(IN)    :: P_codazorac  !> // PARAMETER // activation of the nitrogen influence on root partitionning within the soil profile  // code 1/2 // PARPLT // 0 
  real,              intent(IN)    :: P_minazorac  !> // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning // kg N ha-1 mm-1 // PARPLT // 1 
  real,              intent(IN)    :: P_maxazorac  !> // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning  // kg N ha-1 mm-1 // PARPLT // 1 
  real,              intent(IN)    :: P_minefnra  !> // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning // SD // PARPLT // 1 
  integer,           intent(IN)    :: P_codtrophrac  !> // PARAMETER // trophic effect on root partitioning within the soil // code 1/2/3 // PARPLT // 0 
  real,              intent(IN)    :: P_coefracoupe  !> // PARAMETER // coefficient to define the proportion of dying roots after cut (grass) // SD // PARAMV6/PLT // 1 
  real,              intent(IN)    :: nit(nbCouches)  
  real,              intent(IN)    :: amm(nbCouches)  
  real,              intent(IN)    :: profsol  
  real,              intent(INOUT) :: msrac                     ! n    // OUTPUT // Estimated dry matter of the roots // t.ha-1
  integer,           intent(INOUT) :: nsencourprerac  
  real,              intent(INOUT) :: somtemprac  
  real,              intent(IN)    :: idzrac  
  real,              intent(OUT)   :: efdensite_rac
  integer,           intent(INOUT) :: ndebsenrac  
  real,              intent(IN)    :: precrac(nbCouches)  
  real,              intent(INOUT) :: drl(n,nbCouches)  
  real,              intent(OUT)   :: lracsentot   !> // OUTPUT // Total length of senescent roots  // cm root.cm -2 soil
  real,              intent(IN)    :: masectot  
  real,              intent(IN)    :: masec
  real,              intent(OUT)   :: rltot   !> // OUTPUT // Total length of roots  // cm root.cm -2 soil
  logical,           intent(IN)    :: sioncoupe  
  real,              intent(IN)    :: P_sensrsec  !> // PARAMETER // root sensitivity to drought (1=insensitive) // SD // PARPLT // 1 
  real,              intent(IN)    :: P_stlevamf  !> // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1 
  real,              intent(IN)    :: P_stamflax  !> // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1 
  real,              intent(IN)    :: P_lvfront  !> // PARAMETER // Root density at the root front // cm root.cm-3 soil // PARPLT // 1 
  real,              intent(IN)    :: P_laicomp  !> // PARAMETER // LAI from which starts competition inbetween plants // m2 m-2 // PARPLT // 1 
  real,              intent(IN)    :: P_adens  !> // PARAMETER // Interplant competition parameter // SD // PARPLT // 1 
  real,              intent(IN)    :: P_bdens  !> // PARAMETER // minimal density from which interplant competition starts // plants m-2 // PARPLT // 1 
  real,              intent(IN)    :: P_draclong  !> // PARAMETER // Maximum rate of root length production // cm root plant-1 degree.days-1 // PARPLT // 1 
  real,              intent(IN)    :: P_vlaimax  !> // PARAMETER // ULAI  at inflection point of the function DELTAI=f(ULAI) // SD // PARPLT // 1
  real,              intent(IN)    :: P_longsperac  !> // PARAMETER // specific root length // cm g-1 // PARPLT // 1 
  real,              intent(IN)    :: P_debsenrac  !> // PARAMETER // Sum of degrees.days defining the beginning of root senescence (life-time of a root) // degree.days // PARPLT // 1 
  integer,           intent(IN)    :: P_codedyntalle  !> // PARAMETER // Activation of the module simulating tiller dynamic: yes (1), no (2) // code 1/2 // PARAMV6/PLT // 0 
  real,              intent(IN)    :: drlsenmortalle   !> // OUTPUT // Root biomass corresponding to dead tillers // t ha-1.j-1
  real,              intent(IN)    :: densite   !> // OUTPUT // Actual sowing density // plants.m-2
  real,              intent(IN)    :: P_msresiduel  !> // PARAMETER // Residual dry matter after a cut // t ha-1 // PARTEC // 1 
  real,              intent(IN)    :: lai_veille                ! lai(n-1)  
  real,              intent(IN)    :: msrac_veille              ! msrac(n-1)  
  real,              intent(IN)    :: rl_veille(nbCouches)      ! rl(n-1)  
  real,              intent(IN)    :: dltams   !> // OUTPUT // Growth rate of the plant  // t ha-1.j-1
  real,              intent(IN)    :: repracmin  
  real,              intent(IN)    :: repracmax  
  real,              intent(IN)    :: kreprac  
 ! 11/06/2013 la variable efda devient une varaible de sortie journaliere pour Simtraces
  real,              intent(INOUT)   :: efda      !> // OUTPUT // efda defines the effect of soil compaction through bulk density // 0-1
  real,              intent(INOUT)   :: efnrac_mean    !> // OUTPUT // effect of mineral nitrogen, which contributes to the root distribution in the layers with high mineral nitrogen content. // 0-1
  real,              intent(INOUT)   :: humirac_mean !> // OUTPUT // soil dryness // 0-1
  real,              intent(INOUT)   :: humirac_z(nbcouches)
  real,              intent(INOUT)   :: efnrac_z(nbcouches)
  real,              intent(OUT)     :: rlj
    ! DR 06/05/2015 je rajoute un code pouyr tester la mortalitéé des racines
  integer,           intent(IN)      :: P_codemortalracine !< // PARAMETER // masec servant a calculer les racines mortes a la coupe  : masec (1), masectot (2) // code 1/2 //PARAMv6 // 1
  real,              intent(OUT)     ::  dltmsrac_plante  !<// OUTPUT // pour sorties ArchiSTICS: biomasse journaliere allouee aux racines en g / m²sol / plante





! TODO : fonction densiteracinaire => II + III

! TODO : II. => subroutine profilracinaire

! II. CALCUL DE LA LONGUEUR RACINAIRE A PARTIR DE LA LEVEE
! ********************************************************
      if (P_coderacine == 1) then

!#if DEBUG == 1
!        call profilRacinaire_debug_write_input(1261,n,nbCouches,anox,hur,humin,tauxcouv,P_lvopt,P_zlabour,P_zpente,P_zprlim,    &
!                                               zrac,codeinstal,znonli,nger,cumlracz,difrac,cumflrac,racnoy,             &
!                                               flrac,lracz,cumlraczmaxi,P_profsem,lai)
!#endif

        call profilRacinaire(nbCouches,anox,hur,humin,tauxcouv,P_lvopt,P_zlabour, &   ! IN
                             P_zpente,P_zprlim,codeinstal,nger,P_profsem,lai,       &
                             zrac,znonli,cumlracz,difrac,cumflrac,racnoy,     &   ! INOUT
                             flrac,lracz,cumlraczmaxi,humirac_mean,humirac_z,efnrac_z)

!#if DEBUG == 1
!        call profilRacinaire_debug_write_output(1263,n,nbCouches,anox,hur,humin,tauxcouv,P_lvopt,P_zlabour,P_zpente,P_zprlim,   &
!                                                zrac,codeinstal,znonli,nger,cumlracz,difrac,cumflrac,racnoy,            &
!                                                flrac,lracz,cumlraczmaxi,P_profsem,lai)
!#endif

! TODO : subroutine densitevraieracinaire

! III. Nouveau modèle de croissance racinaire (Devienne et al., mars98)
! *********************************************************************
      else
        if (nger > 0) then


!#if DEBUG == 1
!          call densiteRacinaire_debug_write_input(1271,n,nbCouches,dacouche,hur,humin,anox,nstoprac,P_codazorac,P_minazorac,        &
!                    P_maxazorac,P_minefnra,                                                                                         &
!                    P_codtrophrac,P_coefracoupe,P_daseuilbas,P_daseuilhaut,P_dacohes,nit,amm,P_lvopt,profsol,nrec,msrac,nsencourprerac,     &
!                    nger,nlev,codeinstal,poussracmoy,zrac,rl,somtemprac,dtj,deltaz,idzrac,efdensite,ndebsenrac,precrac,drl,     &
!                    lracz,lracsenz,cumlracz,cumflrac,flrac,cumlraczmaxi,racnoy,lracsentot,masectot,rltot,sioncoupe,P_contrdamax,  &
!                    P_sensrsec,P_sensanox,P_stlevamf,P_stamflax,P_lvfront,P_laicomp,P_adens,P_bdens,P_draclong,P_vlaimax,P_longsperac,P_debsenrac,      &
!                    P_profsem,densite,P_msresiduel,lai_veille,msrac_veille,rl_veille,dltams,repracmin,repracmax,kreprac)
!#endif
!write(245,*)'croirac profsem',P_profsem
!write(245,*)n,nbCouches
!write(245,*)dacouche,hur,humin,anox,nstoprac
!write(245,*)P_codazorac,P_minazorac,P_maxazorac,P_minefnra,P_codtrophrac,P_coefracoupe,P_daseuilbas,P_daseuilhaut,P_dacohes
!write(245,*)nit
!write(45,*)amm,P_lvopt
!write(245,*)profsol,nrec
!write(245,*)msrac,nsencourprerac,nger,nlev,codeinstal,poussracmoy,zrac,rl,somtemprac,dtj,deltaz
!write(245,*)idzrac,efdensite,ndebsenrac,precrac,drl,lracz,lracsenz,cumlracz,cumflrac,flrac
!write(245,*)cumlraczmaxi,racnoy,lracsentot,masectot,rltot,sioncoupe,P_contrdamax,P_sensrsec,P_sensanox
!write(245,*)P_stlevamf,P_stamflax,P_lvfront,P_laicomp,P_adens,P_bdens,P_draclong,P_vlaimax,P_longsperac,P_debsenrac
!write(245,*)P_codedyntalle,drlsenmortalle,P_profsem,densite,P_msresiduel,lai_veille,msrac_veille
!write(245,*)rl_veille,dltams,repracmin,repracmax,kreprac


          call densiteVraieRacinaire(n,nbCouches,dacouche,hur,humin,anox,nstoprac,P_codazorac,P_minazorac,P_maxazorac,P_minefnra,&
                                     P_codtrophrac,P_coefracoupe,P_daseuilbas,P_daseuilhaut,P_dacohes,nit,amm,P_lvopt,profsol,   &
                                     nrec,msrac,nsencourprerac,nger,nlev,codeinstal,poussracmoy,zrac,rl,somtemprac,dtj,deltaz,   &
                                     idzrac,efdensite_rac,ndebsenrac,precrac,drl,lracz,lracsenz,cumlracz,cumflrac,flrac,         &
                                     cumlraczmaxi,racnoy,lracsentot,masectot,rltot,sioncoupe,P_contrdamax,P_sensrsec,P_sensanox, &
                                     P_stlevamf,P_stamflax,P_lvfront,P_laicomp,P_adens,P_bdens,P_draclong,P_vlaimax,P_longsperac,&
                                     P_debsenrac,P_codedyntalle,drlsenmortalle,P_profsem,densite,P_msresiduel,lai_veille,        &
                                     msrac_veille,rl_veille,dltams,repracmin,repracmax,kreprac,efda,efnrac_mean,humirac_mean,    &
                                     humirac_z,efnrac_z,rlj,masec,P_codemortalracine,dltmsrac_plante)

!#if DEBUG == 1
!          call densiteRacinaire_debug_write_output(1273,n,nbCouches,dacouche,hur,humin,anox,nstoprac,P_codazorac,P_minazorac,       &
!                    P_maxazorac,P_minefnra,                                                                                         &
!                    P_codtrophrac,P_coefracoupe,P_daseuilbas,P_daseuilhaut,P_dacohes,nit,amm,P_lvopt,profsol,nrec,msrac,nsencourprerac,     &
!                    nger,nlev,codeinstal,poussracmoy,zrac,rl,somtemprac,dtj,deltaz,idzrac,efdensite,ndebsenrac,precrac,drl,     &
!                    lracz,lracsenz,cumlracz,cumflrac,flrac,cumlraczmaxi,racnoy,lracsentot,masectot,rltot,sioncoupe,P_contrdamax,  &
!                    P_sensrsec,P_sensanox,P_stlevamf,P_stamflax,P_lvfront,P_laicomp,P_adens,P_bdens,P_draclong,P_vlaimax,P_longsperac,P_debsenrac,      &
!                    P_profsem,densite,P_msresiduel,lai_veille,msrac_veille,rl_veille,dltams,repracmin,repracmax,kreprac)
!#endif

        endif
      endif


! -- déplacé - PASTIS --   call excesdeau

    ! calcul de zracmax
    ! PB - 29/03/2004 - sorti de la boucle ci-dessus
      if (zrac > zracmax) zracmax = zrac

    ! dr 11/08/08 si on tasse et que le sol devient < zrac ca pose pb on borne
      if (zrac > profsol) zrac = profsol

return
end subroutine densiteRacinaire

subroutine croissanceFrontRacinaire(                                                             &
             n,nh,nbCouches,hur,hucc,tcult,tsol,humin,dacouche,P_codesimul,P_epc,P_obstarac,           & ! IN
             P_daseuilbas,P_daseuilhaut,P_dacohes,P_zrac0,P_densinitial,P_coderacine,P_codeplante,             &
             P_codeperenne,P_codehypo,P_codegermin,P_zracplantule,P_stoprac,P_codetemprac,P_tcmin,P_tcmax,       &
             P_tgmin,P_croirac,P_contrdamax,P_codeindetermin,P_sensanox,nplt,nrec,codeinstal,nger,nsen,    &
             nlax,nflo,nmat,nlev,namf,izrac,P_profsem,P_codcueille,deltai,lai,sioncoupe,P_sensrsec,    &
             P_codedyntalle,P_tcxstop,dltams,                                                        &
             nhe,nstoprac,zrac,znonli,rl_veille,deltaz,dtj,cumlracz,poussracmoy,lracsenz,tempeff,efda,humirac_mean)  ! INOUT

USE Divers, only: F_humirac, escalin
USE Messages

  implicit none


  integer,           intent(IN)    :: n  
  integer,           intent(IN)    :: nh  
  integer,           intent(IN)    :: nbCouches  
  real,              intent(IN)    :: hur(nbCouches)  
  real,              intent(IN)    :: hucc(nbCouches)  
  real,              intent(IN)    :: tcult   !> // OUTPUT // Crop surface temperature (daily average) // degree C
  real,              intent(IN)    :: tsol(nbCouches)  
  real,              intent(IN)    :: humin(nbCouches)  
  real,              intent(IN)    :: dacouche(nbCouches)  
  !!!MODIF HISAFE 1 : suppression des chaines de caractères
  !!!character(len=*),  intent(IN)    :: P_codesimul  !> // PARAMETER // simulation code (culture ou feuille=lai forcé) // SD // P_USM/USMXML // 0
  integer,  intent(IN)    :: P_codesimul  !> // PARAMETER // simulation code (culture ou feuille=lai forcé) // SD // P_USM/USMXML // 0
  real,              intent(IN)    :: P_epc(nh)  !< // PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm
  real,              intent(IN)    :: P_obstarac  !> // PARAMETER // Soil depth which will block the root growth  // cm // PARSOL // 1 
  real,              intent(IN)    :: P_daseuilbas  !> // PARAMETER // Threshold of bulk density of soil below that the root growth is not limited // g cm-3 // PARAM // 1 
  real,              intent(IN)    :: P_daseuilhaut  !> // PARAMETER // Threshold of bulk density of soil below that the root growth  no more possible // g cm-3 // PARAM // 1 
  real,              intent(IN)    :: P_dacohes  !> // PARAMETER // bulk density under which root growth is reduced due to a lack of cohesion d // g cm-3 // PARAM // 1 
  real,              intent(IN)    :: P_zrac0  !> // PARAMETER // initial depth of root front  // cm // INIT // 1 
  real,              intent(IN)    :: P_densinitial(nh)  !> // PARAMETER // Table of initial root density of the 5 horizons of soil for fine earth // cm cm-3 // INIT // 1
  integer,           intent(IN)    :: P_coderacine  !> // PARAMETER // Choice of estimation module of growth root in volume: standard profile (1) or by actual density (2) // code 1/2 // PARPLT // 0 
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=3),  intent(IN)    :: P_codeplante  !> // PARAMETER // Name code of the plant in 3 letters // * // PARPLT // 0
  integer,           intent(IN)    :: P_codeplante
  integer,           intent(IN)    :: P_codeperenne  !> // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0 
  integer,           intent(IN)    :: P_codehypo  !> // PARAMETER // option of simulation of a  phase of hypocotyl growth (1) or planting of plantlets (2) // code 1/2 // PARPLT // 0 
  integer,           intent(IN)    :: P_codegermin  !> // PARAMETER // option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2) // code 1/2 // PARPLT // 0 
  real,              intent(IN)    :: P_zracplantule  !> // PARAMETER // depth of the initial root front of the plantlet  // cm // PARPLT // 1 
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=3),  intent(IN)    :: P_stoprac  !> // PARAMETER // stage when root growth stops (LAX or SEN) // * // PARPLT // 0
  integer,  intent(IN)    :: P_stoprac  !> // PARAMETER // stage when root growth stops (LAX or SEN) // * // PARPLT // 0
  integer,           intent(IN)    :: P_codetemprac  !> // PARAMETER // option calculation mode of heat time for the root: with crop temperature (1)  or with soil temperature (2) // code 1/2 // PARPLT // 0 
  real,              intent(IN)    :: P_tcmin  !> // PARAMETER // Minimum temperature of growth // degree C // PARPLT // 1
  real,              intent(IN)    :: P_tcmax  !> // PARAMETER // Maximum temperature of growth // degree C // PARPLT // 1
  real,              intent(IN)    :: P_tgmin  !> // PARAMETER // Minimum threshold temperature used in emergence stage // degree C // PARPLT // 1
  real,              intent(IN)    :: P_croirac  !> // PARAMETER // Growth rate of the root front  // cm degree.days-1 // PARPLT // 1 
  real,              intent(IN)    :: P_contrdamax  !> // PARAMETER // maximal root growth reduction due to soil strenghtness (high bulk density) // SD // PARPLT // 1 
  integer,           intent(IN)    :: P_codeindetermin  !> // PARAMETER // option of  simulation of the leaf growth and fruit growth : indeterminate (2) or determinate (1) // code 1/2 // PARPLT // 0 
  real,              intent(IN)    :: P_sensanox  !> // PARAMETER // anoxia sensitivity (0=insensitive) // SD // PARPLT // 1 
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
  real,              intent(IN)    :: izrac   !> // OUTPUT // Index of excess water stress on roots  // 0-1
  real,              intent(IN)    :: P_profsem  !> // PARAMETER // Sowing depth // cm // PARTEC // 1 
  integer,           intent(IN)    :: P_codcueille  !> // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0 
  real,              intent(IN)    :: deltai   !> // OUTPUT // Daily increase of the green leaf index // m2 leafs.m-2 soil
  real,              intent(IN)    :: lai   !> // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  logical,           intent(IN)    :: sioncoupe  
  real,              intent(IN)    :: P_sensrsec  !> // PARAMETER // root sensitivity to drought (1=insensitive) // SD // PARPLT // 1 
  integer,           intent(IN)    :: P_codedyntalle  !> // PARAMETER // Activation of the module simulating tiller dynamic: yes (1), no (2) // code 1/2 // PARAMV6/PLT // 0 
  real,              intent(IN)    :: P_tcxstop  !> // PARAMETER // threshold temperature beyond which the foliar growth stops // degree C // PARPLT // 1
  real,              intent(IN)    :: dltams   !> // OUTPUT // Growth rate of the plant  // t ha-1.j-1


  integer,           intent(INOUT) :: nhe  
  integer,           intent(INOUT) :: nstoprac  
  real,              intent(INOUT) :: zrac   !> // OUTPUT // Depth reached by root system // cm
  real,              intent(INOUT) :: znonli     ! dans le doute, INOUT  
  real,              intent(INOUT) :: rl_veille(nbCouches) ! nger-1 ou n-1  
  real,              intent(INOUT) :: deltaz   !> // OUTPUT // Deepening of the root front  // cm day-1
  real,              intent(INOUT) :: dtj(n) ! 1 to n    // OUTPUT // Daily efficient temperature for the root growing  // degree C.j-1
  real,              intent(OUT)   :: cumlracz   !> // OUTPUT // Sum of the effective root lengths  // cm root.cm -2 soil
  real,              intent(OUT)   :: poussracmoy   !> // OUTPUT // "Average index of the effect of soil constraints on the rooting profile (option  true density )" // 0-1
  real,              intent(INOUT) :: lracsenz(nbCouches)  
  real,              intent(INOUT) :: tempeff   !> // OUTPUT // Efficient temperature for growth // degree C
 ! 11/06/2013 la variable efda devient une varaible de sortie journaliere pour Simtraces
  real,              intent(INOUT)   :: efda      !> // OUTPUT // efda defines the effect of soil compaction through bulk density // 0-1
  real,              intent(INOUT)   :: humirac_mean !> // OUTPUT // soil dryness // 0-1


!: Variables locales
  integer :: i  
  integer :: intzracbas  
  integer :: intzrachaut  
  integer :: iz  
  integer :: izmax  

  real    :: humsol  
  real    :: hn  
  real    :: hx  
  real    :: daz  
!  real    :: efda

!: I. CALCUL DU FRONT RACINAIRE
!******************************
! en journalier, initialisation de la croissance racinaire au semis
! en décadaire,  initialisation de la croissance racinaire à la levée
!
! cas d'une prairie : si la prairie est installee codeinstal = 1
! si on est à la deuxieme coupe, on ne passe pas dans croira
! domi - 19/09/97: pour toutes les plantes "plantees" on ne passe pas dans croira
! NB - 23/3/98: correction bug codeinstal


      !: initialisation si codeinstal = 1
      if (codeinstal == 1 .and. n == 1) then
        znonli = P_zrac0
        zrac = znonli
      endif

      !: initialisation pour P_coderacine = 2 si codeinstal = 1
      if (P_coderacine == 2 .and. codeinstal == 1 .and. n == nger) then
        nhe = 0
        do i = 1, nh
          izmax = int(P_epc(i))
          do iz = 1, izmax
            !rl(nger-1,nhe+iz) = P_densinitial(i)
            rl_veille(nhe+iz) = P_densinitial(i) ! nger-1 = n-1 puisque nger == n
            if ((nhe+iz) < P_profsem) rl_veille(nhe+iz) = 0.
            if ((nhe+iz) > P_obstarac) rl_veille(nhe+iz) = 0.
            if (rl_veille(iz+nhe) > 0.) zrac = iz + nhe
          end do
          nhe = nhe+izmax
        end do

        !: Tests sur l'initialisation racinaire
        !- priorité au profil racinaire
        if (P_profsem > izmax) then
          call EnvoyerMsgHistorique(23)
          !stop
          call exit(9)
        endif

        if (zrac /= P_zrac0) call EnvoyerMsgHistorique(24)
        znonli = zrac

      endif

      !: Si le sol est nu
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!      if (n < nplt .and. P_codeplante == 'snu') then
      if (n < nplt .and. P_codeplante == 1) then
        zrac = 0.
        znonli = 0.
        deltaz = 0.
        return
      endif

!: INITIALISATIONS AVANT LE STADE NLEV (LEVEE):
!-    Dans le cas des cultures annuelles, deux cas:
!- a) Pour les cultures qui se plantent et admettent un
!-    temps de latence avant le demarrage de la croissance:
!-    initialisation du zrac a P_zracplantule+P_profsem
!-    a partir de la plantation;
!- b) Pour les cultures qui se sement:
!-    initialisation du zrac a P_profsem a partir du semis
!- c) Pour les cultures qui se plantent et demarrent directement:
!-    initialisation du zrac a P_zracplantule+P_profsem
!-    a partir de la levee qui est aussi la plantation
!-    attention: P_zracplantule est la longueur de  la radicelle,
!-    et non la profondeur du front racinaire
!- NB et ML - le 23/02/2004

      if (nger == 0 .and. codeinstal == 0) then
        if (P_codeperenne == 1 .and. n >= nplt) then
          if (P_codehypo == 2 .and. P_codegermin == 1) then
            znonli = P_zracplantule + P_profsem
            zrac = znonli
            return
          else
            znonli = P_profsem
            zrac = znonli
            return
          endif
        else
          znonli = 0.
          zrac = znonli

          return
        endif
      endif

      if (n == nger .and. codeinstal == 0) then
        if (P_codehypo == 2) then
          znonli = P_zracplantule+P_profsem
          zrac = znonli
        else
          !: Initialisation de la croissance racinaire à la germination
          znonli = P_profsem
          zrac = znonli
        endif
      endif

! ** selon le type de plante on arrete la croissance racinaire
! *- au stade nlax ou au stade nsen
! --          if (P_codeindetermin /= 2) then
! --              nstoprac = nlax
! --         else
! --              nstoprac = nsen
! --         endif

      !: Bruno - 08/12/00: parametrage de P_stoprac
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!      if (P_stoprac == 'sen' .or. P_stoprac == 'SEN') nstoprac = nsen
!!!      if (P_stoprac == 'lax' .or. P_stoprac == 'LAX') nstoprac = nlax
!!!      if (P_stoprac == 'flo' .or. P_stoprac == 'FLO') nstoprac = nflo
!!!      if (P_stoprac == 'mat' .or. P_stoprac == 'MAT') nstoprac = nmat
!!!      if (P_stoprac == 'rec' .or. P_stoprac == 'REC') nstoprac = nrec
      if (P_stoprac == 12) nstoprac = nsen
      if (P_stoprac == 6) nstoprac = nlax
      if (P_stoprac == 7) nstoprac = nflo
      if (P_stoprac == 10) nstoprac = nmat
      if (P_stoprac == 11) nstoprac = nrec


! DR et ML et SYL 15/06/09
! ************************
! introduction de la fin des modifications de Sylvain (nadine et FR)
! dans le cadre du projet PERMED
! #### SYL
! NB le 20/02/2008
      if (P_codedyntalle == 1) then
        if (sioncoupe) deltaz = 0.0
      endif
! ####
! DR et ML et SYL 15/06/09 FIN introduction de la fin des modifications de Sylvain

    ! ML - 29/07/04: extraction du calcul de dtj de la condition
    ! (nstoprac == 0 .or. n == nstoprac) de façon à ce que l'activité des
    ! nodosités se poursuive après l'arrêt de la croissance racinaire
      if (n >= nger .and. nger > 0 .and. zrac > 0.) then
        if (P_codetemprac == 1) then
          dtj(n) = max(tcult - P_tcmin, 0.)
          dtj(n) = min(dtj(n), P_tcmax - P_tcmin)

        ! DR et ML et SYL 15/06/09
        ! ************************
        ! introduction de la fin des modifications de Sylvain (nadine et FR)
        ! dans le cadre du projet PERMED
          if (P_codedyntalle == 1) then
        ! ####
        ! NB le 06/03/2008 introduction de l'effet négatif des températures élevées
        ! sur les fonctions racinaires qui utilisent "dtj"
            if (tcult > P_tcmax .and. P_tcxstop < 100) then
              dtj(n) = (P_tcmax-P_tcmin)/(-P_tcxstop+P_tcmax)*(tcult-P_tcxstop)
              dtj(n) = max(dtj(n),0.)
            endif
          endif
        ! ####

        else
          dtj(n) = max(tsol(int(zrac)) - P_tgmin, 0.)
          dtj(n) = min(dtj(n) ,P_tcmax - P_tgmin)

        ! ####
        ! NB le 06/03/2008 introduction de l'effet négatif des températures élevées
        ! sur les fonctions racinaires qui utilisent "dtj"
          if(P_codedyntalle == 1)then
            if (tsol(int(zrac)) > P_tcmax .and. P_tcxstop < 100) then
              dtj(n) = (P_tcmax-P_tcmin)/(-P_tcxstop+P_tcmax)*(tsol(int(zrac))-P_tcxstop)
              dtj(n) = max(dtj(n),0.)
            endif
          endif
        ! ####

        endif
      endif

! 15/06/09 on introduit cette modif conditionnelle à P_codedyntalle
! on verifie plus tard le calcul est necessaire ici car fait dans calai
      if (P_codedyntalle == 1)then
        if (P_tcxstop >= 100.0) then
          if(tcult > P_tcmax) then
            tempeff = max(0.,(P_tcmax-P_tcmin))
          else
            tempeff = max(0.,(tcult-P_tcmin))
          endif
        else
          if(tcult > P_tcmax) then
            tempeff = (P_tcmax-P_tcmin) / (-P_tcxstop+P_tcmax)*(tcult-P_tcxstop)
            tempeff = max(0.,tempeff)
          else
            tempeff = max(0.,(tcult-P_tcmin))
          endif
        endif
      endif
! tester jusque la

! --
! DR et ML et SYL 15/06/09 FIN introduction de la fin des modifications de Sylvain


! ** croissance racinaire de la germination jusqu'à la floraison
! *- PB - 29/04/2004 - rajout d'un test si zrac plus grand que zéro
      if (n >= nger .and. (nstoprac == 0.or.n == nstoprac) .and. nger > 0 .and. zrac > 0.) then
! *- calcul de l'effet température pondéré par la sécheresse du sol
! *- définition de sommes de degrés.jours racines

! -- ML le 29/07/04: extraction du calcul de dtj (n) de la condition
! -- (nstoprac == 0.or.n == nstoprac) de façon à ce que l'activité des
! -- nodosités se poursuive après l'arrêt de la croissance racinaire
!        if (P_codetemprac == 1) then
!          dtj(n) = max(tcult-P_tcmin,0.)
!          dtj(n) = min(dtj(n),P_tcmax-P_tcmin)
!        else
!          dtj(n) = max(tsol(int(zrac))-P_tgmin,0.)
!          dtj(n) = min(dtj(n),P_tcmax-P_tgmin)
!        endif
! --

! **test sur int(zrac-1) et int(zrac+1) pour qu'ils restent dans l'intervalle [1,1000]
! *- ajout de l'effet d'anoxie
! *- introduction d'une sensibilité à l'anoxie : P_sensanox NB le 16/1/2001

        intzracbas = int(zrac-1)
        intzrachaut = int(zrac+1)
        if (intzracbas < 1)     intzracbas = 1
        if (intzracbas > 1000)  intzracbas = 1000
        if (intzrachaut < 1)    intzrachaut = 1
        if (intzrachaut > 1000) intzrachaut = 1000
        humsol = (hur(int(zrac))+hur(intzracbas)+hur(intzrachaut))/3.
        hn = (humin(int(zrac))+humin(intzracbas) + humin(intzrachaut))/3.
        hx = (hucc(int(zrac))+hucc(intzracbas) + hucc(intzrachaut))/3.

! ** fonction humidité en tout ou rien de part et d'autre du PF 4.2
! *- le front racinaire est davantage ralenti au stade radicelle (avant la levée)
! *- NB - le 18/12/01
        if (nlev == 0) then
          deltaz = dtj(n) * P_croirac * F_humirac(humsol,hn,hx,P_sensrsec)
        else
          deltaz = dtj(n) * P_croirac * F_humirac(humsol,hn,hn,P_sensrsec)
        endif
! DR 11/06/2013 on sort la variable de sortie de humirac apres levée
        humirac_mean = F_humirac(humsol,hn,hn,P_sensrsec)

! ** Ajout d'une contrainte à la pénétration  racinaire: efda
! -------------------------------------------------------
! *- Fonction Jones et al. 1991  + thèse de B. Rebière, adaptée avec la densité apparente
! *- P_daseuilbas et P_daseuilhaut : seuils min et max de densité apparente
! *- P_contrdamax : taux maximal de reduction de vitesse de croissance racinaire
        daz =   dacouche(int(zrac))
        efda = escalin(daz,P_daseuilbas,P_daseuilhaut,1.,P_contrdamax)
        if (daz < P_dacohes) efda = daz/P_dacohes

        deltaz = deltaz * efda

! ** ajout pour les indéterminées
        if (P_codeindetermin == 2 .and. namf > 0 .and. deltai <= 0.) then
          deltaz = 0.
        endif
        znonli = znonli + (deltaz * (1. - (1. - izrac) * P_sensanox * 0.8))

! DR et ML et SYL 15/06/09
! ************************
! introduction de la fin des modifications de Sylvain (nadine et FR)
! dans le cadre du projet PERMED
! #### SYL
! NB le 22/11/07 extension de l'arrêt de pousse des racines quand pas de
! croissance
        if (P_codedyntalle == 1) then
          if (nlev > 0 .and. dltams <= 0.) then
            deltaz = 0.
          endif
        endif
! ####
! DR et ML et SYL 15/06/09 FIN introduction de la fin des modifications de Sylvain

! **NB - le 07/06 - stress joue après calcul de znonli
        deltaz = deltaz*(1.-(1.-izrac)*P_sensanox)
        zrac = zrac+deltaz

! ** limitation de l'enracinement par la profondeur du sol
! --        if (znonli > P_obstarac) then
        if (zrac > P_obstarac) then
          zrac = P_obstarac
          deltaz = 0.

! ** NB - le 07/06
! --          zrac = znonli
! --          zrac = zrac+deltaz
        endif
      endif

! ** entre la floraison et la récolte, la profondeur du système racinaire n'évolue pas
! *- après la récolte, de nouveau situation de sol nu
! *- domi - 26/04/2002 - rajout d'un test pour qu'on remette zrac à 0 apres la recolte
! *- sauf quand cueillette - NB - le 08/05/02
! --       if (nrec > 0 .and. P_codesimul == 'culture') then
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!     if (nrec > 0 .and. n /= nrec .and. trim(P_codesimul) == 'culture' .and. P_codcueille == 1) then
      if (nrec > 0 .and. n /= nrec .and. P_codesimul == 1 .and. P_codcueille == 1) then
        zrac     = 0.
        znonli   = 0.
        deltaz   = 0.
        cumlracz = 0.
        poussracmoy = 0.
        lracsenz(:) = 0. ! on met à zéro tout le tableau plutot que lracsenz(n) = 0. qui n'a pas de sens
! *- 17/06/04 - emma
        dtj(n) = 0.
        return
      endif


!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!      if (nrec > 0 .and. trim(P_codesimul) == 'feuille') then
      if (nrec > 0 .and. P_codesimul == 2) then
        if (lai <= 0.) then
          zrac     = 0.
          znonli   = 0.
          deltaz   = 0.
          cumlracz = 0.
          lracsenz(:) = 0. ! on met à zéro tout le tableau plutot que lracsenz(n) = 0. qui n'a pas de sens
          return
        endif
      endif

!  fin du endif de la prairie
!      endif

return
end subroutine croissancefrontracinaire


subroutine profilracinaire(nbCouches,anox,hur,humin,tauxcouv,P_lvopt,P_zlabour, &   ! IN
                           P_zpente,P_zprlim,codeinstal,nger,P_profsem,lai,       &
                           zrac,znonli,cumlracz,difrac,cumflrac,racnoy,     &   ! INOUT
                           flrac,lracz,cumlraczmaxi,humirac_mean,humirac_z,efnrac_z)

USE Divers, only: F_humirac
USE Messages, only: EnvoyerMsgHistorique

  implicit none

  integer,           intent(IN)    :: nbCouches  
  real,              intent(IN)    :: anox(nbCouches)  
  real,              intent(IN)    :: hur(nbCouches)  
  real,              intent(IN)    :: humin(nbCouches)  
  real,              intent(IN)    :: tauxcouv   !> // OUTPUT // Cover rate // SD
  real,              intent(IN)    :: P_lvopt  !> // PARAMETER // Optimum root density // cm root.cm-3 soil // PARAM // 1 
  real,              intent(IN)    :: P_zlabour  !> // PARAMETER // Depth of ploughing  // cm // PARPLT // 1 
  real,              intent(IN)    :: P_zpente  !> // PARAMETER // Depth where the root density is ½ of the surface root density for the reference profile // cm  // PARPLT // 1 
  real,              intent(IN)    :: P_zprlim  !> // PARAMETER // Maximum depth of the root profile for the reference profile // cm  // PARPLT // 1 
  integer,           intent(IN)    :: codeinstal  
  integer,           intent(IN)    :: nger  
  real,              intent(IN)    :: P_profsem  !> // PARAMETER // Sowing depth // cm // PARTEC // 1 
  real,              intent(IN)    :: lai   !> // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil

  real,              intent(INOUT) :: zrac   !> // OUTPUT // Depth reached by root system // cm
  real,              intent(INOUT) :: znonli     ! dans le doute, INOUT  
  real,              intent(OUT)   :: cumlracz   !> // OUTPUT // Sum of the effective root lengths  // cm root.cm -2 soil
  real,              intent(OUT)   :: difrac  
  real,              intent(OUT)   :: cumflrac  
  real,              intent(INOUT) :: racnoy  
  real,              intent(INOUT) :: flrac(nbCouches)  
  real,              intent(INOUT) :: lracz(nbCouches)  
  real,              intent(OUT)   :: cumlraczmaxi  
  real,              intent(INOUT)   :: humirac_mean !> // OUTPUT // soil dryness // 0-1
  real,              intent(INOUT)   :: humirac_z(nbCouches)
  real,              intent(INOUT)   :: efnrac_z(nbCouches)



!: Variables locales

  integer :: iz  
  real    :: zdemi  
  real    :: S  

      ! NB - le 18/12/01
        !--if (nlev > 0 .or. P_codeperenne == 2) then
      ! les3 - 10/06/03 - nouveau test  pour le demarrage racinaire
        if (nger > 0 .or. codeinstal == 1) then
        ! forme du profil racinaire
          S = -log(1e-2)/(P_zlabour-P_zpente)
          difrac = P_zprlim - P_zpente
        ! profil de densité racinaire efficace : tableau lracz((200)
          cumlracz = 0.
        ! Nb - le 05/06 - cumflrac = cumul de racines / cumlracz =  cumul de racines fonctionnelles
          cumflrac = 0.
          zdemi = max((znonli - difrac),(log(4.)/S))
          racnoy = 0.
        ! domi - 14/12/00 -  sol 200 à 1000
        ! PB - 2009 - paramètre nbCouches à la place de la limite en dur
          humirac_mean = 0.
          do iz = 1, nbCouches
            flrac(iz) = 1/(1+exp(-S*(float(iz)-zdemi)))
            if (float(iz) > zrac) then
              lracz(iz) = 0.
              EXIT
            endif
          ! NB - le 08/05/02 - pas de racine au dessus de la profondeur de semis
            if (float(iz) < P_profsem) flrac(iz) = 0.
          ! NB - le 08/05/02 - P_sensrsec pour effet sécheresse
          ! NB - le 25/11/2002 - bug dans l'utilisation de la fonction humirac
          ! 17/06/04 - les 3 - Pour l'absorption d'eau la limite est toujours humin P_sensrsec ne joue pas
            !--if (nlev == 0) then
            lracz(iz) = flrac(iz) * P_lvopt * F_humirac(hur(iz),humin(iz),humin(iz),0.)
            ! dr 13/06/2013 j'ajoute humirac_mean en sortie
            humirac_mean = humirac_mean + F_humirac(hur(iz),humin(iz),humin(iz),0.)
            !DR 23/07/2013 j'ajoute le humriac par couche pour Simtraces
            humirac_z(iz) = F_humirac(hur(iz),humin(iz),humin(iz),0.)

            !--else
            !--  lracz(iz) = flrac(iz)*P_lvopt * HUMIRAC(hur(iz),humin(iz),humin(iz),P_sensrsec)
            !--endif
            cumlracz = cumlracz + lracz(iz)
            cumflrac = cumflrac + flrac(iz)
            cumlraczmaxi = cumlracz
          ! Ajout de l'effet anoxique
          ! variable BR pour exces d'eau
            racnoy = racnoy + flrac(iz) * anox(iz)
          end do
          humirac_mean = humirac_mean/nbCouches

        ! test sur cumlracz
          if ((lai > 0. .or. tauxcouv > 0.) .and. cumlracz <= 0.) then
            call EnvoyerMsgHistorique(20)
          endif

        endif

return
end subroutine profilracinaire

 
 
