!---------------------------------------------------------------------------
!  fonction déclinaison
real function decangle(j)
  
  integer, intent(IN) :: j    ! j est le jour julien  

  !: Variables locales
  real :: pi  
  real :: theta1  
  real :: theta2  
  real :: theta  
      

      pi = 4 * atan(1.0)
      theta1 = 2 * pi * (j - 80) / 365
      theta2 = 0.034 * (sin(2 * pi * j / 365) - sin(2 * pi * 80 / 365))
      theta = theta1 - theta2
      decangle = asin(0.3978 * sin(theta))

return
end function decangle 
 
