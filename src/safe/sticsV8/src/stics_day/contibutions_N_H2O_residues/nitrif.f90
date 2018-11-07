!> Calculation of nitrification
!> **********************************************************************************
!> - Stics book paragraph 8, pages 148-151
!>
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine nitrif (nbCouches,numcult,P_profhum,P_codefente,P_codenitrif,P_rationit,&
                   P_hoptn,P_hminn,P_fnx,P_pH,P_pHmaxnit,P_pHminnit,var_tnitmin,var_tnitopt,var_tnitmax,var_tnitopt2, &
                   tsol,hucc,hur,dacouche,humin,sat,    & !variables d'entree
                   nitrifj,Qnitrif,amm,nit,em_N2Onit,Qem_N2Onit) !INOUT

  implicit none

  integer, intent(IN) :: nbCouches
  integer, intent(IN) :: P_codenitrif    !> // PARAMETER // option to activate nitrification calculation // code 1/2 // PARSOL // 0
  integer, intent(IN) :: P_codefente     !> // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0
  real,    intent(IN) :: P_fnx           !> // PARAMETER // maximum nitrification rate in the soil // day-1 // PARAM // 1
  real,    intent(IN) :: P_hminn         !> // PARAMETER // moisture (proportion of field capacity) below which nitrification rate is nil // g.g-1 // PARAM // 1
  real,    intent(IN) :: P_hoptn         !> // PARAMETER // moisture (proportion of field capacity) at which nitrification rate is maximum // g.g-1 // PARAM // 1
  integer, intent(IN) :: numcult
  real,    intent(IN) :: P_pH            !> // PARAMETER // Soil pH (basic value without addition of organic amendments)  // SD // PARSOL // 1
  real,    intent(IN) :: P_pHmaxnit      !> // PARAMETER // pH above which nitrification is maximal // P_pH // PARAM // 1
  real,    intent(IN) :: P_pHminnit      !> // PARAMETER // pH below which nitrification is nil // P_pH // PARAM // 1
  real,    intent(IN) :: P_profhum       !> // PARAMETER // soil depth below which biological activity is nil  (max.60 cm) // cm // PARSOL // 1
  real,    intent(IN) :: P_rationit      !> // PARAMETER // molar ratio of nitrification : N2O/N nitrified // SD // PARAM // 1
  real,    intent(IN) :: tsol(nbCouches)
  real,    intent(IN) :: hucc(nbCouches)
  real,    intent(IN) :: hur(nbCouches)
  real,    intent(IN) :: dacouche(nbCouches)
  real,    intent(IN) :: humin(nbCouches)
  real,    intent(IN) :: sat(nbCouches)
  real,    intent(IN) :: var_tnitmin(200)        ! dimension ?
  real,    intent(IN) :: var_tnitopt(200)        ! dimension ?
  real,    intent(IN) :: var_tnitmax(200)        ! dimension ?

  real,    intent(IN) :: var_tnitopt2(200)       ! Bruno : ajout plateau sur la courbe de nitrification     dimension ?

  real,    intent(INOUT) :: amm(nbCouches)  
  real,    intent(INOUT) :: nit(nbCouches)  
  real,    intent(INOUT) :: nitrifj    !> // OUTPUT // Daily N nitrified // kg.ha-1.d-1
  real,    intent(INOUT) :: Qnitrif    !> // OUTPUT // cumulative N nitrified // kg.ha-1
  real,    intent(INOUT) :: em_N2Onit !> // OUTPUT // daily N2O flux due to  nitrification// kg.ha-1.d-1
  real,    intent(INOUT) :: Qem_N2Onit !> // OUTPUT // cumulative N2O flux due to  nitrification // kg.ha-1

! Variables locales
  integer :: ihum
  integer :: iz
  real :: tnitrif          ! nitrification rate constant (relative to ammonium content)
  real :: fpHn
  real :: fhn
  real :: ftn
  real :: wsat
  real :: wfps
  real :: wfpscc
  real :: nitrifiz

! *- initialisation de la vitesse de nitrification
      nitrifj = 0.
! profondeur de minéralisation de l'humus
      ihum  = nint(P_profhum)

! Boucle sur les couches contribuant a la nitrification
! ------------------------------------------------------
      do iz = 1,ihum

    ! Vitesse de nitrification = f(pH, température, eau)
    ! tnitrif = taux de nitrification journalier du NH4 (Jorge Sierra)
        if (P_codenitrif == 2) then
           tnitrif = 1.
        else
    ! 1) effet pH : eq 8.13
           if (P_pH <= P_pHminnit)  fpHn = 0.
           if (P_pH >= P_pHmaxnit)  fpHn = 1.
           if (P_pH < P_pHmaxnit .and. P_pH > P_pHminnit) then
              fpHn = (P_pH-P_pHminnit) / (P_pHmaxnit-P_pHminnit)
           endif
    ! 2) effet temperature (fonction croissante puis decroissante) : eq 8.15 MODIFIEE pour avoir un plateau de Tnitopt a Tnitopt2 (Bruno)

          if (tsol(iz) <= var_tnitopt(numcult)) then
              ftn = (tsol(iz)-var_tnitmin(numcult)) / (var_tnitopt(numcult)-var_tnitmin(numcult))
          else
                if (tsol(iz) <= var_tnitopt2(numcult)) then
                    ftn = 1.
                else
                    ftn = (tsol(iz)-var_tnitmax(numcult))/ (var_tnitopt2(numcult)-var_tnitmax(numcult))
                endif
          endif
          ftn = max(ftn,0.)
          ftn = min(ftn,1.)

     ! 3) effet humidité (fonction lineaire entre Hminn et Hoptn) : eq 8.14
          fhn = (hur(iz)-P_hminn*hucc(iz))/((P_hoptn-P_hminn)*hucc(iz))
     ! diminution progressive de la nitrification en cas d'excès d'eau

          if (hur(iz) > hucc(iz)) then
             if (P_codefente == 1) then
                wsat = (1.5 * hucc(iz) - 0.5 * humin(iz)) / 10.
             else
                wsat = 1. - dacouche(iz) / 2.66
             endif
             wfps = (hur(iz)+sat(iz)) / 10. / wsat
             wfpscc = hucc(iz) / 10. / wsat
             fhn = (wfps-1.)/(wfpscc-1.) ! elsa correction fhn au lieu de fh
          endif

          fhn = max(fhn,0.)
          fhn = min(fhn,1.)

! Calcul de la vitesse de nitrification : Eq 8.12
          tnitrif = P_fnx * fpHn * ftn * fhn
          tnitrif = amin1(tnitrif,1.)
        endif

! ** Actualisation des pools NH4 et NO3
!      nitrification de l'ammonium (existant + minéralisé)
         nitrifiz = tnitrif*amm(iz)
         amm(iz) = amm(iz) - nitrifiz
         nit(iz) = nit(iz) + nitrifiz*(1-P_rationit)

! **  Sommation des flux  de nitrification sur tout le profil
         nitrifj = nitrifj + nitrifiz*(1-P_rationit)
      end do
! *- Fin de boucle sur les couches

!   Nitrification cumulée
      Qnitrif = Qnitrif + nitrifj
      em_N2Onit = P_rationit*nitrifj    ! Bruno: ajout nouvelle variable
      Qem_N2Onit = Qem_N2Onit + em_N2Onit

return
end subroutine nitrif

 
 
