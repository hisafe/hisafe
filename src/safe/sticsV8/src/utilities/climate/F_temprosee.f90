!*** fonction de calcul de la température du point de rosée***********
!***** a la pression e *******************************************
!> Function for calculating the dew point temperature at the E pressure
real function F_temprosee(e)

  implicit none

!: Arguments
  real, intent(IN) :: e  
  
!: Variables locales
  real :: x  

!      TVAR    = 6.1070*(1+SQRT(2.)*SIN(X/3))**8.827

      x = 3 * asin(((e / 6.1070)**(1.0 / 8.827) - 1.0) / sqrt(2.0))
      F_temprosee = x / 0.017453293

return
end function F_temprosee
 
