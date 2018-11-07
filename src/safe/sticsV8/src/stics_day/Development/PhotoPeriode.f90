!     =================
!
! ****  SUBROUTINE TO CALCULATE DAILY DAYLENGTH AND PHOTOPERIOD.
!       DAYLENGTH IS CALCULATED FOLLOWING THE TREATMENT OF
!       Sellers, Physical Climatology,pp 15-16 and Appendix 2.
!       DAYLENGTH IS CALCULATED WITH A CORRECTION FOR ATMOSPHERIC
!       REFRACTION EQUIVALENT TO 50 MINUTES OF A DEGREE.
!       PHOTOPERIOD IS CALCULATED ASSUMING THAT LIGHT IS PERCEIVED
!       UNTIL THE CENTRE OF THE SUN IS 6 DEGREES BELOW THE HORIZON.
!
!
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module calculates daily daylength and photoperiod.
!> - Stics book paragraphe 2.3.2, page 32
!>
!! The current photoperiod (photop) is calculated on the basis of calendar days and latitude using classic astronomical functions (Sellers, 1965).
!! The photoperiod is calculated by assuming that light is perceptible until the sun is at 6° below the horizon, which corresponds to a duration 50 to 70 minutes
!! longer than the strictly defined daylength.
!!
!! Daylength is calculated with a correction for atmospheric refraction equivalent to 50 minutes of a degree.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine photpd(zlat,jday,daylen,photop)

!! ARGUMENTS
  real,    intent(IN)  :: zlat  
  integer, intent(IN)  :: jday  
  real,    intent(OUT) :: daylen  
  real,    intent(OUT) :: photop  

!! VARIABLES LOCALES
  real,dimension(3) :: days  !>  
  real,dimension(3) :: photp  !>  
  real,dimension(3) :: dl  
  real    :: alat  !>  
  real    :: zday  !>  
  real    :: pi  !>  
  real    :: b  !>  
  real    :: d  !>  
  real    :: h  !>  
  real    :: p  
  real    :: theta  !>  
  real    :: theta1  !>  
  real    :: theta2  !>  
  real    :: dec  !>  
  real    :: x1  !>  
  real    :: x2  
  integer :: maxjd  !>  
  integer :: i  
    
!TODO:   penser à modifier maxjd nombre de jours de l'annee
      
    maxjd=365        

    alat = zlat/57.296
    zday = float(jday)
    days(1) = zday-1.0
    days(2) = zday
    days(3) = zday+1.0
    pi = 3.14159

    if(days(3) > float(maxjd)) days(3) = 1.0
    if(days(1) < 1.0) days(1) = float(maxjd)

    do i = 1,3
      theta1 = 2.0 * pi * (days(i) - 80.0) / 365.0
      theta2 = 0.034 * (sin(2.0 * pi * days(i) / 365.0) - sin(2. * pi * 80.0 / 365.0))
      theta = theta1 + theta2
      dec = asin(0.3978 * sin(theta))
      x1 = cos(alat) * cos(dec)
      b = -0.01454 / x1
      x2 = tan(alat) * tan(dec)
      h = acos(b - x2)
      daylen = 24.0 * h / pi
      dl(i) = daylen
      d = -1.0 * 0.10453 / (cos(alat) * cos(dec))
      p = d - (tan(alat) * tan(dec))
      p = acos(p)
      photp(i) = 24.0 * p / pi
      if(photp(i) > 24.0) photp(i) = 24.0
    end do

    daylen = dl(2)
    photop = photp(2)

return
end subroutine photpd

 
