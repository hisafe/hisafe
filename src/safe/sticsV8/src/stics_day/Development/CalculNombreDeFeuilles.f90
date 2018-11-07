! ************************************* c
! *  calcul du nombre de feuilles     * c
! *  version 5.0   29/06/01           * c
! ************************************* c
! ****************************************************************
!> Calculation of the number of leaves (nbfeuille) is mainly indicative.
!> - Stics book paragraphe 3.1.5, page 48
!!
!> Its only active role is to define the duration of the plantlet phase when calculating
!! frost risks. Indeed the plantlet stage is calculated as a leaf-number stage (2 or 3). nbfeuille is calculated up to the ILAX stage from the phyllotherm (phyllotherme)
!! (the thermal period separating the emission of two successive leaves) expressed in crop degree.days as  for the phasic development.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine CalculNombreDeFeuilles(P_phyllotherme,nlax,udev,somfeuille,nbfeuille)
      
  ! ARGUMENTS (IN)
  real,    intent(IN)    :: P_phyllotherme  !> // PARAMETER // thermal duration between the apparition of two successive leaves on the main stem // degree C day // PARPLT // 1
  integer, intent(IN)    :: nlax  
  real,    intent(IN)    :: udev  
  
  ! ARGUMENTS (INOUT)
  real,    intent(INOUT) :: somfeuille  
  integer, intent(INOUT) :: nbfeuille   !> // OUTPUT // Number of leaves on main stem // SD
         
    if (P_phyllotherme <= 0 .or. nlax > 0) return
    
    somfeuille = somfeuille + udev
          
    if (somfeuille > P_phyllotherme) then
      nbfeuille = nbfeuille + 1
      somfeuille = somfeuille - P_phyllotherme
    endif
      
return
end subroutine CalculNombreDeFeuilles
