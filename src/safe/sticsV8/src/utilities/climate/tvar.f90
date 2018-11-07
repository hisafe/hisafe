! ***************************************************************** c      
! * fonction de calcul de la tension de vapeur saturante          * c
! * a la temperature T                                            * c
! ***************************************************************** c
! Function calculates the vapor pressure at the T temperature
real function TVAR(T)

  implicit none

!: Arguments
  real, intent(IN) :: T  
  
!: Variables locales
  real :: X    

      X = T * 0.017453293
      
      TVAR = 6.1070 * (1 + SQRT(2.) * SIN(X/3))**8.827

return
end function TVAR 
 
