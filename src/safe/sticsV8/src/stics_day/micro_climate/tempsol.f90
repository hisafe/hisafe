! ************************************* c
! *-       version 3.3 18/03/98      -* c
! *-     derniere modif 02/02/2004   -* c
! ************************************* c

! ---------------------------------------------------------- c
! *                   TEMPERATURES SOL                     * c
! ---------------------------------------------------------- c
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module calculates the soil temperature.
!> - Stics book paragraphe 9.1, page 168
!>
!! Temperature variation in soil depends on the surface conditions which determine the daily thermal variation but also thermal inertia related to the environment.
!! This inertia is the cause of the lower daily average temperatures in deep layers compared to those at the surface: this is the annual thermal variation.
!! The temperature at the upper limit for calculating soil temperature is assumed to be tcult and the daily thermal amplitude at this upper limit is amplsurf.
!! The daily thermal amplitude, amplz, and the soil temperature, tsol, at depth Z in the soil are calculated using a formalisation suggested by McCann et al. (1991).
!! It is a recurrent calculation using the previous day’s values.
!!
!! The thermal diffusivity diftherm is assumed to be independent of soil water conditions and general throughout the various soil types.
!! A value of  5.37 10-3 cm2s-1  is proposed, based on the work by McCann et al. (1991).
! ------------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine tempsol(tcultmin,tcultmax,P_diftherm,profsol,tsolveille,tcultveille,tmin,tsol)

  implicit none

!: Arguments

  real,    intent(IN)    :: tcultmin  
  real,    intent(IN)    :: tcultmax   !> // OUTPUT // Crop surface temperature (daily maximum) // degree C
  real,    intent(IN)    :: P_diftherm  !> // PARAMETER // soil thermal diffusivity // cm2 s-1 // PARAM // 1 
  real,    intent(IN)    :: profsol  
  real,    intent(IN)    :: tsolveille(int(profsol))  
  real,    intent(IN)    :: tcultveille  
  real,    intent(IN)    :: tmin   !> // OUTPUT // Minimum active temperature of air // degree C

  real,    intent(INOUT) :: tsol(int(profsol))  

!: Variables locales
  integer :: iz  
  real    :: amplsurf  !>  
  real    :: amplz  !>  
  real    :: thermamp  


      amplsurf = tcultmax - tcultmin


      ! P_diftherm = 5.37e-3
      thermamp = sqrt(7.272e-5/2 / P_diftherm)
!        write(71,*)tmin,amplsurf,thermamp,tcultmin,tcultmax,P_diftherm,tcultveille
      do iz = 1,int(profsol)
!        write(71,*)iz,tsolveille(iz),tcultveille,tmin,amplsurf,thermamp
        tsol(iz) = tsolveille(iz) - exp(-iz * thermamp) * (tcultveille - tmin) &
                 + 0.1 * (tcultveille - tsolveille(iz))

!        write(71,*)iz,tsol(iz)
        amplz = amplsurf * exp(-iz * thermamp)
        tsol(iz) = tsol(iz) + (amplz / 2)


      end do

return
end subroutine tempsol
 
 
