! ************************************************************************ c
! * Etat de surface du sol en fonction de la présence d'un mulch végétal * c
! * Florent Maraux, E. Scopelet al.,N. Brisson                           * c
! * programmation N. Brisson                                             * c
! * le 30/12/98                                                          * c
! ************************************************************************ c

! ** paramètres :
! *- decomposmulch : taux de décomposition
! *- P_ruisolnu : proportion d'eau ruissellée en sol nu
! *- P_qmulchruis0 : quantité de mulch à partir de laquelle
! *-               le ruissellement est nul
! *- P_pminruis : pluie minimale pour ruissellement
! *- P_mouillabilmulch : réserve maxi du mulch en mm/(t.ha)
! *- P_kcouvmlch : coefficient d'extinction du mulch
! *- qmulch0 = quantité de mulch le jour de l'apport
! *- nappmulch = jour de l'apport du mulch

! ** variables :
! *- qmulch = quantité de mulch végétal en t/ha
! *- ruisselsurf : ruissellement du à l'état de surface
! *- mouillmulch  : eau stockée dans le mulch
! *- Emulch : évaporation de l'eau du mulch
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> Interception of water by mulch
!> - Stics book paragraphe 6.4.3.a, page 109-110
!>
!! This module calculates the interception of water by mulch
!! The maximum water reserve of the plant mulch (mouillmulch) is defined as being proportional to its quantity (qmulch), involving the mulch-dependent
!! parameter of wettability (mouillabilmulch) that can range between 0.22 and 0.38 mm t-1 ha (Scopel et al., 1998). The amount of water retained is limited
!! by the incident rainfall minus the surface run-off.
!! The amount of water directly evaporated from the mulch (Emulch) can be calculated in two ways, using either the reference evapotranspiration given in
!! the weather input file or the raw weather variables including wind speed and air humidity:
!>       -   In the first case, it is assumed that the water contained in mulch evaporates in the same way as from a grass canopy, according to a resistance/height
!!           compensation phenomenon. This last concept corresponds to the "extinction of energy at the soil level" by the vegetation (as is the case for soil).
!!           If the extin parameter is not active (because the radiation intercepted by the canopy is calculated with the radiation transfer model and not by the
!!           Beer law approach), the value is recalculated and varies depending on the crop geometry and the quality of radiation.
!>       -   In the second case the Shuttleworth and Wallace formalisation is applied (see module shutwall.f90), and Emulch evaporates in the same way as
!!           free water located at the soil level and receiving energy. It takes account of the proportion of soil covered by the mulch (couvermulch).
!! In both cases Emulch is limited by the amount of water intercepted by the vegetal mulch, mouillmulch.
! ------------------------------------------------------------------------------------------------------------------------------------------------------------* c
!subroutine etatsurf(n,jul,napS,P_jultrav,P_proftrav,julappmulch,nappmulch,P_codepaillage,decomposmulch, &   ! IN suppression de decomposmulch
!subroutine etatsurf(n,jul,napS,P_jultrav,P_proftrav,nappmulch,P_codepaillage,               &   ! IN
subroutine etatsurf(n,jul,             &   ! IN
                    stemflowTot,P_pminruis,P_ruisolnu,P_qmulchruis0,P_mouillabilmulch,P_kcouvmlch,P_codebeso, &
                    P_codelaitr,tetp,delta,laiTot,tauxcouv,                                         &
                    qmulch,ruisselsurf,precip,mouillmulch,intermulch,couvermulch,Emulch,P_penterui )      ! INOUT


  implicit none

!: Arguments
  integer, intent(IN)    :: n  
  integer, intent(IN)    :: jul  
!  integer, intent(IN)    :: napS
!  integer, intent(IN)    :: P_jultrav(napS)   ! 1 to napS   // PARAMETER // Day in year of the soil cultivation operations and/or apply of organic residues  // julian day // PARTEC // 1
!  real,    intent(IN)    :: P_proftrav(napS)  ! 1 to napS   // PARAMETER // Depth of residue incorporation  (max. 40 cm) // cm // PARTEC // 1
!  integer, intent(IN)    :: julappmulch
!  integer, intent(IN)    :: nappmulch
!  real,    intent(IN)    :: decomposmulch   ! (P_typemulch)
  real,    intent(IN)    :: stemflowTot     ! somme des stemflow des plantes  
  real,    intent(IN)    :: P_pminruis  !> // PARAMETER // Minimal amount of precipitation to start a drip  // mm day-1 // PARAM // 1 
  real,    intent(IN)    :: P_ruisolnu  !> // PARAMETER // fraction of drip rainfall (by ratio at the total rainfall) on a bare soil  // between 0 and 1 // PARSOL // 1 
  real,    intent(IN)    :: P_qmulchruis0     ! (P_typemulch)   // PARAMETER // Amount of mulch to annul the drip // t ha-1 // PARAM // 1
  real,    intent(IN)    :: P_mouillabilmulch ! (P_typemulch)   // PARAMETER // maximum wettability of crop mulch // mm t-1 ha // PARAM // 1
  real,    intent(IN)    :: P_kcouvmlch       ! (P_typemulch)   // PARAMETER // Extinction Coefficient reliant la quantité de paillis végétal au taux de couverture du sol // * // PARAM // 1
  integer, intent(IN)    :: P_codebeso        !  => de la plante principale   // PARAMETER // option computing of water needs by the k.ETP (1) approach  or resistive (2) approach // code 1/2 // PARPLT // 0
  integer, intent(IN)    :: P_codelaitr       !  => de la plante principale   // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0
  real,    intent(IN)    :: tetp            !    // OUTPUT // Efficient potential evapotranspiration (entered or calculated) // mm day-1
  real,    intent(IN)    :: delta  
  real,    intent(IN)    :: laiTot          ! somme des LAIs des plantes  
  real,    intent(IN)    :: tauxcouv        !    // OUTPUT // Cover rate // SD
  real,    intent(IN)    :: P_penterui      !< // PARAMETER // runoff coefficient taking account for plant mulch // SD // PARSOL // 1
!  real,    intent(INOUT) :: qmulch0
  real,    intent(INOUT) :: qmulch   !> // OUTPUT // Quantity of plant mulch // t.ha-1
  real,    intent(INOUT) :: ruisselsurf   !> // OUTPUT // Surface run-off // mm
  real,    intent(INOUT) :: precip   !> // OUTPUT // Daily amount of water (precipitation + irrigation)   // mm day-1
  real,    intent(INOUT) :: mouillmulch  
  real,    intent(INOUT) :: intermulch   !> // OUTPUT // Water intercepted by the mulch (vegetal) // mm
  real,    intent(INOUT) :: couvermulch   !> // OUTPUT // Cover ratio of mulch  // 0-1
  real,    intent(INOUT) :: Emulch   !> // OUTPUT // Direct evaporation of water intercepted by the mulch // mm



!: Variables locales
  real    :: fruis  !>  
  real    :: excesmouillmulch  

!  print *,'etatsurf: debut etatsurf '
!  print *,'etatsurf: debut etatsurf ',stemflowTot,P_pminruis,P_ruisolnu
!  print *,'etatsurf: P_qmulchruis0  ',P_qmulchruis0
  !
  !,P_mouillabilmulch,P_kcouvmlch,P_codebeso
! 08/02/2011 on a retiré la decomposition du mulch selon scopel puisque le mulch est maintenant considéré comme un residus
! dont la decomposition est calculee dans residusdecomposition
!print *,'etatsurf : debut etatsurf'


      ! ** estimation du ruissellement lié à l'état de surface
      ! *- partant d'une proportion de ruissellement sol nu (P_ruisolnu)
      ! *- une exponentielle négative jusqu'à qmulch = 0.1
      ! *- puis la droite proposée par Scopel et al., paramétrée par la quantité
      ! *- de mulch à partir de laquelle le ruissellement est négligeable
      ! *- si P_ruisolnu est inférieur à la pente de cette relation, alors
      ! *- on utilise cette valeur jusqu'à "rattraper" la droite de Scopel et al.
      if (precip - stemflowTot > P_pminruis) then
!        if (P_ruisolnu >= 0.33) then
!          if (qmulch <= 0.1) then
!            fruis = (0.33*(P_qmulchruis0-0.1)-P_ruisolnu) / 0.1*qmulch+P_ruisolnu
!          endif
!          if (qmulch > 0.1 .and. qmulch < P_qmulchruis0) fruis = 0.33*(P_qmulchruis0-qmulch)
!          if (qmulch > P_qmulchruis0)  fruis = 0.0
!        else
!          if (qmulch <= P_qmulchruis0-P_ruisolnu/0.33) then
!            fruis = P_ruisolnu
!          else
!            fruis = 0.33*(P_qmulchruis0-qmulch)
! ** NB - le 27/04 - seuillage de fruis à 0.0
!            fruis = max(fruis,0.0)
!          endif

        ! NB le 07/11/2005 simplification du ruissellement avec un mulch végétal
        if (qmulch <= 0.1) then
          fruis = P_ruisolnu
        else
        !  fruis = min(0.33 * (P_qmulchruis0 - qmulch), P_ruisolnu)! DR 27/07/2012 on externalise ce parametre dans le fichier sol
          fruis = min(P_penterui * (P_qmulchruis0 - qmulch), P_ruisolnu)
        endif

        fruis = max(fruis, 0.0)

        ! ** nb - le 30/01/02
        ! *- le ruissellement ne concerne pas la pluie qui s'écoule par stemflow
        ! ** NB - le 08/07/02 - ruissellement sans mulch
        if (qmulch <= 0.0) fruis = P_ruisolnu

        ruisselsurf = fruis * (precip - stemflowTot - P_pminruis)
      else
        ruisselsurf = 0.0
      endif
!print *,'Stics_etatsurf: avant precip'

      precip = precip - ruisselsurf
!write(71,*)'Stics_etatsurf: apres precip',precip,qmulch,mouillmulch
! DR 26/07/2011 le bilan mal equilibré si on a du mulch et si on incorpore des rexidus
      if(qmulch.eq.0)then
         couvermulch=0.
         intermulch=0.0
      endif



      if (qmulch <= 0.) return

      ! ** réservoir hydrique du mulch
      ! *- la quantité d'eau stockable dans le mulch est de
      ! *- P_mouillabilmulch mm/(t/ha)
!print *,'Stics_etatsurf: avant mouillmulch 1'
      mouillmulch = precip + mouillmulch
!print *,'Stics_etatsurf: apres mouillmulch 1'

      ! *- PB - 12/01/2005 - on calcul l'exces d'eau que ne peut retenir le paillage
      ! *- que l'on réaffectera à precip
!print *,'Stics_etatsurf: avant excesmouillmulch',mouillmulch,qmulch
      excesmouillmulch = max(0., mouillmulch - (P_mouillabilmulch * qmulch))
!print *,'Stics_etatsurf: apres excesmouillmulch'

      mouillmulch = min(mouillmulch, P_mouillabilmulch * qmulch)
!print *,'Stics_etatsurf: apres mouillmulch',precip,excesmouillmulch

      !: PB - 12/01/2005 : la variable intermulch est égale
      !-                   à l'interception le jour n, de la pluie par le mulch.
      intermulch = max(0., precip - excesmouillmulch)

!print *,'Stics_etatsurf: apres intermulch'

      !: La pluie qui arrive jusqu'au sol et n'est pas retenu par le mulch
      precip = excesmouillmulch
!    write(618,*)'exces',precip
!print *,'Stics_etatsurf: avant calcul couvermulch'

      !: Taux de couverture du mulch
      couvermulch = 1 - exp(-P_kcouvmlch * qmulch)
!   write(71,*)'dans etatsurf couvermulch',P_kcouvmlch , qmulch, couvermulch
      !: On fait évaporer l'eau interceptée par le mulch.
      !- On estime que le pouvoir évaporant de l'air pour une surface d'eau (Emd) libre
      !- est de l'ordre de l'ETP penman (dans le cas où codeso = 1, sinon, passage par S&W)
!print *,'Stics_etatsurf: avant P_codebeso'

      if (P_codebeso == 1) then
        !: Domi - 23/03/01 - taux de couverture
        if (P_codelaitr == 1) then
          Emulch = tetp * exp(-delta * laiTot) * couvermulch
        else
          Emulch = tetp * (1 - tauxcouv) * couvermulch
        endif
        Emulch = min(mouillmulch, Emulch)
        mouillmulch = max(mouillmulch - Emulch, 0.0)
      endif


return
end subroutine etatsurf
 
 
