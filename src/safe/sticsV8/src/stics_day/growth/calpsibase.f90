!*********************************************************************
!  version 6.0
!  calcul du potentiel de base
!  le 8/06/2004
!  ITV (JC Payan, I De Munter)
!  INRA (N Brisson, I Garcia de Cortazar)
! modif le 20/02/07
! mise en fonction du calcul du potentiel
! et de la fonction inverse
!*********************************************************************
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 9.4.3, page 177
!>
! At dawn, plant water potential is assumed to be in equilibrium with the soil water. Consequently this measurement is often used as a daily assessment of
! water stress and a relevant integrated measurement of soil behaviour. In order to be able to compare STICS simulations to this type of measurement,
! a simple calculation of predawn plant water potential is proposed, based on Brisson et al. (1993).  Predawn plant potential is calculated as the
! arithmetic mean over depth of soil water potential, weighted by root density. The soil potentials (psisol) are calculated using
! the Clapp and Hornberger (1978) formulae, using the points (HUCC, -0.03 MPa) and (HUMIN, -1.5MPa) to calculate the parameters bpsisol and psisols.
! The roots participating in predawn potential are the ones located in moist layers (psisol above -1.5 MPa)
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine calpsibase(zrac,nbCouches,hur,sat,humin,hucc,dacouche,P_psihumin,lracz,lai,P_psihucc,P_codefente,psibase)

USE Divers, only: F_humirac, escalin, humpotsol

  implicit none

!: Arguments
  real,    intent(IN)  :: zrac   !> // OUTPUT // Depth reached by root system // cm
  integer, intent(IN)  :: nbCouches  
  real,    intent(IN)  :: hur(nbCouches)  
  real,    intent(IN)  :: sat(nbCouches)  
  real,    intent(IN)  :: humin(nbCouches)  
  real,    intent(IN)  :: hucc(nbCouches)  
  real,    intent(IN)  :: dacouche(nbCouches)  
  real,    intent(IN)  :: P_psihumin  !> // PARAMETER // soil potential corresponding to wilting point // Mpa // PARAM // 1 
  real,    intent(IN)  :: lracz(nbCouches)  
  real,    intent(IN)  :: lai   !> // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(IN)  :: P_psihucc  !> // PARAMETER // soil potential corresponding to field capacity  // Mpa // PARAM // 1 
  integer, intent(IN)  :: P_codefente  !> // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0 
  real,    intent(OUT) :: psibase   !> // OUTPUT // Predawn leaf water potential potentiel foliaire de base // Mpascal

!: Variables locales
  integer :: iz  
  real    :: psisol(nbCouches)  
  real    :: sw  
  real    :: racinepsi  
  real    :: cumracinepsi  

!: Fonctions
  real :: potsol  

      !: Calcul du potentiel de sol en MPa
      !- Modèle Clapp et Hornberger (1978)
      !- NB - 08/07/2004: calcul de bpsisol et psisols en fonction de humin et hucc
      !- initialisation :
      racinepsi    = 0.0
      cumracinepsi = 0.0
      psibase      = 0.0
      sw = 0.0

      do iz = 1, int(zrac) ! TODO: vérifier que zrac < nbCouches
        sw = (hur(iz) + sat(iz)) / 10.
        psisol(iz) = potsol(P_psihucc,P_psihumin,humin(iz),hucc(iz),dacouche(iz),sw,P_codefente)

        !: Calcul du potentiel de la plante
        if (psisol(iz) < P_psihumin) then ! Test sur les potentiels des sols par sécurité
          racinepsi = 0.
        else
          racinepsi = lracz(iz)
        endif
        psibase = psibase + (racinepsi * psisol(iz))
        cumracinepsi = cumracinepsi + racinepsi
      end do

      if (lai <= 0.0) then
        psibase = 0.0
      else
        if (cumracinepsi > 0.) then
          psibase = psibase / cumracinepsi
        else
          psibase = P_psihumin
        endif
      endif

return
end subroutine calpsibase


!> Fonction de calcul du potentiel du sol

real function potsol(P_psihucc,P_psihumin,humin,hucc,dacouche,sw,P_codefente)

  implicit none

!: Arguments
  real,    intent(IN) :: P_psihucc  !> // PARAMETER // soil potential corresponding to field capacity  // Mpa // PARAM // 1 
  real,    intent(IN) :: P_psihumin  !> // PARAMETER // soil potential corresponding to wilting point // Mpa // PARAM // 1 
  real,    intent(IN) :: hucc  
  real,    intent(IN) :: humin  
  real,    intent(IN) :: dacouche  
  real,    intent(IN) :: sw  
  integer, intent(IN) :: P_codefente  !> // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0 

!: Variables locales
  real :: bpsisol  
  real :: psisols  
  real :: wsat  

    ! Calcul des paramètres de la courbe de rétention

      if (P_codefente == 1) then
        wsat = ((1.5 * hucc) - (0.5 * humin)) / 10.
      else
        wsat = 1. - (dacouche / 2.66)
      endif
      bpsisol = log(P_psihucc / P_psihumin) / log(humin / hucc)
      psisols = P_psihumin * (humin / (wsat * 10))**bpsisol

    ! Calcul du potentiel
      potsol = psisols * (sw / wsat)**(-bpsisol)

return
end function potsol
 
 
