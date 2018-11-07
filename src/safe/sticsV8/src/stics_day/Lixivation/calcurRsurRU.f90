
!>   calculRsurRU
!!
!!   Module : lixivation
!!
!! retourne la valeur de R/RU (réel)
!!
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 9.4.1, page 176
!>
!! By integrating the difference between hur(iz)-humin(iz) over the soil depth, profsol, and weighted by the difference between hucc(iz)-humin(iz)
!! (corresponding to ru calculated by the function calculRU.f90) gives the soil water status as a proportion of readily available water (calculRsurRU).
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c
real function calculRsurRU(RU,profsol,hur,sat,humin)

! Argument(s)
  real, intent(IN)  :: RU   !> // OUTPUT // maximum available water reserve over the entire profile // mm
  real, intent(IN)  :: profsol  
  real, intent(IN)  :: hur(int(profsol))  
  real, intent(IN)  :: humin(int(profsol))  
  real, intent(IN)  :: sat(int(profsol))  

! Variable(s) locale(s)
  integer :: iz  

    ! TODO : il faut vérifier que la taille des tableaux est cohérente avec profsol

      calculRsurRU = 0.

      do iz=1,int(profsol)
        calculRsurRU = calculRsurRU + max(hur(iz)+sat(iz)-humin(iz),0.)
      end do

      if (RU /= 0.) calculRsurRU = calculRsurRU/RU

end function
 
 
