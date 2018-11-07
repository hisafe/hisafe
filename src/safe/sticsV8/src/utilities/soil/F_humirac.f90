! *----------------------------------------------------------------* c
! *   introduction d'une fonction continue entre pf4.2 et CC pour  * c
! *   contraindre la germination et la levée                       * c
! *----------------------------------------------------------------* c
!> Introduction of a continuous function between pf4.2 and CC to Constrain germination and emergence
real function F_humirac(h,hmin,hmax,P_sensrsec)

  real, intent(IN) :: h  
  real, intent(IN) :: hmin  
  real, intent(IN) :: hmax      !> // OUTPUT // Maximum height of water table between drains // cm
  real, intent(IN) :: P_sensrsec  !> // PARAMETER // root sensitivity to drought (1=insensitive) // SD // PARPLT // 1 
  
  real :: x  

      if (hmax /= hmin) then  
        !: 10/06/03 - modif de fonction humirac avant pfp pour 
        !-            aboutir à une absorption nulle pour h = 0
        if (h > hmin) then
          x = (h - hmin) / (hmax - hmin)
          F_humirac = P_sensrsec + (1. - P_sensrsec) * x
        else
          F_humirac = P_sensrsec / hmin * h
        endif
      else
        !: NB - 10/06/03
        ! --if (h < hmin) humirac = P_sensrsec
        if (h >= hmin) then
          F_humirac = 1.0
        else
          F_humirac = P_sensrsec / hmin * h
        endif
      endif

      if (F_humirac > 1.)  F_humirac = 1.
      if (F_humirac < 0.)  F_humirac = 0.

return
end function F_humirac
 
