! *************************************************************
!    modèle C.Hénault, programmation B.Gabrielle & N.Brisson
!       dans la couche allant de la surface à P_profdenit
!
! ------------------   Variables d'entrée   --------------------
!   P_vpotdenit =  dénitrification potentielle (kg N/ha/jour)
!              supposée constante
!   P_profdenit =  profondeur de dénitrification
!   wsat     = humidité volumique à saturation
!   sw       = humidité volumique totale du sol = hur+sat
!
! ------------------   Variables de sortie ---------------------
!   deniti   = N dénitrifié dans la couche i   au jour n
!   Ndenit   = N dénitrifié sur tout le profil au jour n
!   QNdenit = quantité cumulée de N dénitrifié (sol + P_engrais)
! **************************************************************
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
!> calculation of Denitrification and N2O emissions
!> - Stics book paragraphe 8.5, page 154-155
!>
!! model C.Henault, programmation B.Gabrielle & N.Brisson
!!       in the layer from the surface to P_profdenit
!!
!! Denitrification and N2O emissions are calculated according to the model proposed by Hénault et al. (2005).
!! The actual rate Ndenit (kg N-(N2O+N2) ha-1 day-1) is assumed to be affected by soil temperature (FT), nitrate content (FNO3) and water content (FW).
!! The denitrification rate increases with the nitrate content in soil and depends also on bulk density
!! PROFDENIT is the thickness (cm) of the denitrifying layer and VPOTDENIT is the total denitrification potential rate (kg N ha-1 day-1) of the soil,
!! assumed to be constant with time.
!!
!! Obviously the daily denitrification rate in each layer cannot exceed the amount of nitrate-N in that layer.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c

subroutine denit(P_profdenit,P_codefente,nbCouches,dacouche,hucc,humin,hur,sat,tsol,  &   ! IN
                 var_TREFh,var_TREFdenit1,var_TREFdenit2,P_vpotdenit,P_ratiodenit,    &
                 Ndenit,condenit,nit,QNdenit,em_N2Oden,Qem_N2Oden)                                       ! INOUT

  implicit none

  real, intent(IN)       :: P_profdenit  !> // PARAMETER // soil depth on which denitrification is active (with the appropriate option) // cm // PARSOL // 1
  integer, intent(IN)    :: P_codefente  !> // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0 
  integer, intent(IN)    :: nbCouches  
  real,    intent(IN)    :: dacouche(nbCouches)   ! 1 à P_profdenit  
  real,    intent(IN)    :: hucc(nbCouches)       ! 1 à P_profdenit  
  real,    intent(IN)    :: humin(nbCouches)      ! 1 à P_profdenit  
  real,    intent(IN)    :: hur(nbCouches)        ! 1 à P_profdenit  
  real,    intent(IN)    :: sat(nbCouches)        ! 1 à P_profdenit  
  real,    intent(IN)    :: tsol(nbCouches)       ! 1 à P_profdenit  
  real,    intent(IN)    :: var_TREFh             !  
  real,    intent(IN)    :: var_TREFdenit1        !  
  real,    intent(IN)    :: var_TREFdenit2        !  
  real,    intent(IN)    :: P_vpotdenit  !> // PARAMETER // potential rate of denitrification (per 1 cm soil layer) // kg ha-1 j-1 cm-1 // PARSOL // 1 
  real,    intent(IN)    :: P_ratiodenit  !> // PARAMETER // ratio between N2O emisson and total denitrification // kg.ha-1.j-1 // PARAM // 1 
  real,    intent(INOUT) :: Ndenit      !> // OUTPUT // "Daily denitrification of nitrogen from fertiliser or soil (if option  denitrification  is activated)" // kg.ha-1.j-1
  real,    intent(INOUT) :: condenit      !> // OUTPUT // Denitrifying condition rate regard to the potential // 0-1
  real,    intent(INOUT) :: nit(nbCouches)        ! quantité de nitrate par couche kg.ha-1
  real,    intent(INOUT) :: QNdenit      !> // OUTPUT // "Cumulated denitrification of nitrogen from fertiliser or soil (if option  denitrification  is activated)" // kgN.ha-1
  real,    intent(INOUT) :: em_N2Oden     !> // OUTPUT // "Daily N2O flux due to denitrification (if option denitrification is activated)" // kg.ha-1.d-1
  real,    intent(INOUT) :: Qem_N2Oden    !> // OUTPUT // "Cumulative N2O flux due to denitrification (if option denitrification is activated)" // kg.ha-1

!: Variables locales
  integer :: iz  
  real    :: wsat  !>  
  real    :: sw  !>  
  real    :: fw  !>  
  real    :: no3  !>  
  real    :: stt  !>  
  real    :: ft  !>  
  real    :: fno3  !>  
  real    :: fdenit  
  real    :: denitiz
  real    :: Potdeniz

! 18/08/09
! DR et ML modifs Jorge concernant l'adaptation des MO au changement climatique:
! pour le moment on garde ca en commentaire avant validation de BM et EJ
! attention désormais P_TREFh (temperature de reference pour la denitrification de l humus)
! a été remplacé par TREF (mêmes températures pour l'humus et les résidus)

     Ndenit = 0.
     condenit = 0.
     em_N2Oden = 0.

 ! Potentiel de denitrification par couche de sol
    Potdeniz = P_vpotdenit/P_profdenit

    do iz = 1,int(P_profdenit)
 ! 1) Effet teneur en eau (WFPS)
        if (P_codefente == 1) then
           wsat = (1.5 * hucc(iz) - 0.5 * humin(iz)) / 10.
        else
           wsat = 1. - dacouche(iz) / 2.66
        endif
        sw = (hur(iz)+sat(iz))/10.

      ! *******************************************************************************
      ! 18/08/09
      ! DR et ML modifs Jorge concernant l'adaptation des MO au changement climatique:
      ! pour le moment on garde ca en commentaire avant validation de BM et EJ
      ! attention désormais P_TREFh (temperature de reference pour la denitrification de l humus)
      ! a été remplacé par TREF (mêmes températures pour l'humus et les résidus)
      ! DR 27/12/07 TREFfhum est remplace par P_TREFh pour ne pas rajouter de parametre
!--      swrmin = 0.62 - ((tsol(iz) - var_TREFh) / 100)

!--      if (sw / wsat > swrmin) then

      ! introduction interaction mineralisation de la MOS
      ! les parametres TREFfh et TREFdenit1 TREFdenit2 sont variables
      ! on les a recalculé dans une variable var_nom du parametre
      ! Jorge Sierra 22/11/07
!--        fw = ((sw - swrmin * wsat) / (wsat * (1 - swrmin)))**1.74
        ! *******************************************************************************

        if (sw / wsat > 0.62) then
          fw = ((sw - 0.62 * wsat) / (wsat * 0.38))**1.74
          fw = min(fw, 1.)
        else
          fw = 0.
        endif
        stt = tsol(iz)

 ! 2) Effet température
      ! DR et ML le 18/08/09: P_TREFdenit1 et P_TREFdenit2 sont dans le fichier de paramètres de transition
      ! ils sont égaux par défaut à 11 et 20 degree C respectivement, et à 20 et 29 degree C dans le cas des sols tropicaux; cf bouquin p.155
        if (stt < var_TREFdenit1) then
          ft = exp((stt - var_TREFdenit1) * 0.4489 - 0.6677) ! ln(89) = 4.489  et 9*ln(2.1) = 6.677
        else
          ft = exp((stt - var_TREFdenit2) * 0.0742)          ! ln(2.1) = 0.74
        endif
        if (ft > 2.) ft = 2.

 ! 3) Effet nitrate
        no3 = nit(iz) / dacouche(iz) * 10.   ! concentration en mg N/kg sol
        fno3 =  no3 / (22. + no3)

 ! Bilan : flux de dénitrification
        fdenit = ft * fw * fno3
        denitiz = fdenit * Potdeniz
        denitiz  = min(nit(iz), denitiz)
        fdenit = denitiz/Potdeniz

  ! Actualisation du pool nitrate
        nit(iz) = nit(iz) - denitiz
        em_N2Oden = em_N2Oden + denitiz*P_ratiodenit
        Ndenit   = Ndenit   + denitiz
        condenit  = condenit  + fdenit

      end do

      QNdenit   = QNdenit + Ndenit
      condenit   = condenit / P_profdenit
      Qem_N2Oden = Qem_N2Oden + em_N2Oden

return
end subroutine denit
 
 
