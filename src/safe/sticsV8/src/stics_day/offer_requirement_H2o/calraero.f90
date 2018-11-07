! raa et ras formule Shuttleworth & Wallace (1985)
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module calculates the resistances to diffusion (raa and ras).
!> - Stics book paragraphe 7.2.2a, page 132-134
!>
!! We have adopted the formalisations proposed by Shuttleworth and Wallace (1985) which are described in detail in Brisson et al. (1998b).
!! The calculations of diffusive resistances are different for bare soil and covering crops (LAI>=4), while for non-covering crops (LAI<4) a LAI-dependent
!! linear combination of the two first values is used. They all depend on wind speed (u).
! ------------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine calraero(P_zr,d,z0,z0s,karm,u,h,nconv,laidessus,raa,ras)

  implicit none
  
!: Arguments
  
  real, intent(IN)  :: P_zr  !> // PARAMETER // Reference height of meteorological data measurement // m // STATION // 0 
  real, intent(IN)  :: d  
  real, intent(IN)  :: z0  
  real, intent(IN)  :: z0s  
  real, intent(IN)  :: karm  
  real, intent(IN)  :: u  
  real, intent(IN)  :: h  
  real, intent(IN)  :: nconv  
  real, intent(IN)  :: laidessus  
  real, intent(OUT) :: raa  
  real, intent(OUT) :: ras   !> // OUTPUT // Aerodynamic resistance between the soil and the canopy   // s.m-1
  
!: Variables locales
  real :: raalim  !>  
  real :: raslim  !>  
  real :: raazero  !>  
  real :: raszero  

      raalim = log((P_zr - d) / z0) / (karm**2 * u) &
             * (log((P_zr - d) / (h - d)) + h / (nconv *(h - d)) * (exp(nconv * (1.0 - (d + z0) / h)) - 1.0))
     
      raslim = log((P_zr - d) / z0) / (karm**2 * u) * h  &
             / (nconv * (h - d)) * (exp(nconv) - exp(nconv * (1.0 - (d + z0) / h)))
     
      raszero = log(P_zr / z0s) * log((d + z0) / z0s) / (karm**2 * u)
      raazero = log(P_zr / z0s)**2 / (karm**2 * u) - raszero
      
      if (laidessus < 4.0) then
        raa = (0.25 * laidessus * raalim) + (0.25 * (4 - laidessus) * raazero)
        ras = (0.25 * laidessus * raslim) + (0.25 * (4 - laidessus) * raszero)
      else
        raa = raalim
        ras = raslim
      endif
! DR_2010
!write(71,*)P_zr,z0s,d,z0,karm,u
! DR_2010
!write(71,*)raszero

! DR_2010
!write(71,*)laidessus,raalim,raslim,raazero
! DR_2010
!write(71,*)'raa',raa,'ras',ras
return
end subroutine calraero
 
 
