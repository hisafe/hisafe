! *--------------------------------------------------* c
! * calcul du déficit de saturation interne vrai     * c
! * formule Shuttleworth & Wallace (1985)            * c
! *--------------------------------------------------* c
! NB correction P_patm le 26/01/05
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module calculates the daily average of the canopy moisture.
!> - Stics book paragraphe 6.6.3, page 119-121
!>
!! The calculation of the saturation deficit within the canopy (doivrai in mbars) is possible using the Shuttleworth and Wallace formula (1985),
!! using the sum of evaporation fluxes (evaporation from soil, mulch, free water on leaves and transpiration).
!!
!! deltat is the gradient of the relationship between saturation vapour pressure and temperature, tmoy (°C) is the average daily air temperature, rnet (MJ m-2) is
!! the net daily radiation, L is the latent heat of vaporisation (MJ kg-1), gamma is the psychometric constant (mbar °C-1) depending on atmospheric pressure,
!! dsat is the air saturation deficit (mbar), raa is the aerodynamic resistance between the canopy and the reference height of weather measurements (zr generally 2 m)
!! calculated from the canopy height and wind speed, the evaporation from soil, mulch and free water on leaves respectively and plant transpiration.
!! The average daily moisture (humidite) is then calculated with reference to the crop temperature (tcult).
!!
!! If the weather variable “wind speed” is not available, a default value of raa is used (ra). If air humidity is not available the same assumption is made as before,
!! using the parameter corecTrosee(see in calrnet.f90). In this way, the moisture variable can be calculated in the absence of actual weather data.
! ------------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine humcouv(tmoy,tcult,pm,P_ra,Edirect,EpA,EpP,rnet,P_patm,humidite,humair)
  
USE Divers, only: TVAR

  implicit none
  
!: Arguments
  
  real, intent(IN)    :: tmoy      !> // OUTPUT // Mean active temperature of air // degree C
  real, intent(IN)    :: tcult      !> // OUTPUT // Crop surface temperature (daily average) // degree C
  real, intent(IN)    :: pm  
  real, intent(IN)    :: P_ra  !> // PARAMETER // Aerodynamic resistance (used in volatilization  module when we use ETP approach) // s m-1 // STATION // 1      // OUTPUT // Aerodynamic resistance between the cover and the reference level P_zr // s.m-1
  real, intent(IN)    :: Edirect      !> // OUTPUT // Water amount evaporated by the soil + intercepted by leafs + intercepted by the mulch  // mm
  real, intent(IN)    :: EpA  
  real, intent(IN)    :: EpP  
  real, intent(IN)    :: rnet      !> // OUTPUT // Net radiation  // MJ m-2
  real, intent(IN)    :: P_patm  !> // PARAMETER // atmospheric pressure // mbars // STATION // 0 
  
  real, intent(INOUT) :: humidite      !> // OUTPUT // Moisture in the canopy // %
  real, intent(INOUT) :: humair      !> // OUTPUT // Air moisture // %
  
  
!: Variables locales
  real :: cp  
  real :: dsat  
  real :: deltat  
  real :: doivrai    
  real :: gamma   
  real :: L    
  real :: ro    
  real :: const_to  
  
  


      gamma = .65 * P_patm / 1000.0
      ro = 1.2
      cp = 1.013e-3
      const_to = 8.64e+4

      deltat = (TVAR(tmoy+.5) - TVAR(tmoy-.5))
      dsat = (TVAR(tmoy) - pm)
      L = (2500840 - (2358.6 * tmoy)) * 1e-6
     
      doivrai = dsat + (deltat * rnet - (deltat + gamma) * L * (Edirect + EpA + EpP)) * P_ra / (ro * cp * const_to)
   
      doivrai = max(doivrai, 0.0)
      humidite = (TVAR(tcult) - doivrai) / TVAR(tcult)
      
      ! ** NB - le 06/05/02
      humidite = max(humidite, 0.05)
      
      ! ** ajout - NB - le 16/10/01
      humair = pm / tvar(tmoy)

return
end subroutine humcouv 
 
