real function calculKH(ro, cp, phiv)

implicit none

  real, intent(IN) :: ro  
  real, intent(IN) :: cp  
  real, intent(IN) :: phiv  

  calculKH = ro * cp * phiv

return
end function calculKH

real function calculPHIV(surfouvre,P_cvent,tvent,P_phiv0)

implicit none

  real, intent(IN) :: surfouvre  
  real, intent(IN) :: P_cvent  !> // PARAMETER // parameter of the climate calculation under the shelter // SD // STATION // 1 
  real, intent(IN) :: tvent   !> // OUTPUT // Mean speed of B2vent // m.s-1
  real, intent(IN) :: P_phiv0  !> // PARAMETER // parameter allowing the calculation of the under shelter climate // * // STATION // 1 

  calculPHIV = ((surfouvre/2.) * P_cvent * tvent) + P_phiv0

return
end function calculPHIV
 
 
