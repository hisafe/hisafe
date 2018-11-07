
!>   calculRU
!!
!!   Module : lixivation
!!
!! retourne la valeur de RU (réel)
!!
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> calculation of the readily available water in the soil
!> - Stics book paragraphe 9.4.1, page 176
!>
!! This function calculates the readily available water in the soil as the difference between hucc(iz)-humin(iz) over the soil depth (profsol).
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c
real function calculRU(profsol, hucc, humin)

implicit none

! Argument(s)
  real, intent(IN)  :: profsol  
  real, intent(IN)  :: hucc(int(profsol))  
  real, intent(IN)  :: humin(int(profsol))  

! Variable(s) locale(s)
  integer :: iz  
  real    :: RU   !> // OUTPUT // maximum available water reserve over the entire profile // mm

    ! TODO : il faut vérifier que la taille des tableaux est cohérente avec profsol

      RU = SUM(hucc(1:int(profsol))) - SUM(humin(1:int(profsol)))
      RU = max(0., RU)

    !TODO: On pourra supprimer les lignes ci-dessous et garder la formule au-dessus à la place
    ! pour avoir l'exact même résultat que la version 6.4
      RU = 0.
      do iz = 1,int(profsol)
        RU = RU + hucc(iz)-humin(iz)
      end do

      calculRU = RU

end function calculRU
 
 
