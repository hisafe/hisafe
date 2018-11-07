! *----------------------------------------------------* c
! * calcul du rayonnement extraterrestre en MJ/m2/jour * c
! * source :Saumane, 1993                              * c
! *----------------------------------------------------* c
!> Calculation of extraterrestrial radiation in MJ/m2/jour * c
!> Source: Saumane, 1993
real function RGEX(lat,t)

  USE Divers, only: decangle
  implicit none

!: Arguments
  real,    intent(IN) :: lat  !> P_latitude  
  integer, intent(IN) :: t    !> jour julien  
  
!: Variables locales
  real :: z  
  real :: x  
  real :: y  
  real :: solar  
  real :: a  
  real :: u  

! x =  sinus de l'angle de  déclinaison du soleil
! y  = sinus de la P_latitude

      ! 12/04/2012 je mets l'appel en minuscule pour que ce soit pareil que la declaration
      z = decangle(t)
      x = sin(z)
      y = sin(lat)

      solar = 1370 * 3600 * 24 / 3.14159
      a = -x * y / sqrt((1 - x**2) * (1 - y**2))
      RGEX = 0. * x
      if (a < 0.0) then
          RGEX = 3.14159
          if (a + 1.0 < 0.0) a = -1.0
          u = sqrt(1 - a**2) / a
          RGEX = x * y * (RGEX + atan(u) - u)
      endif

      if (a == 0.0) then
          if (x == 0.0) then
              RGEX = sqrt(1 - y**2)
          else
              RGEX = sqrt(1 - x**2)
          endif
      endif    

      if (a > 0.0) then
          u = sqrt(1 - a**2) / a
          RGEX = x * y * (RGEX + atan(u) - u)
      endif

      RGEX = solar * (1 + (0.033 * cos(0.0172 * t))) * RGEX * 1e-6

return
end function RGEX 
 
