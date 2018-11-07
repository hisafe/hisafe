

!! ****************************************************************
!> programme specifique vigne Inaki pour le calcul de la date
!! de debourrement avec des sommes de températures horaires
!! 20/09/2004
!< ****************************************************************

subroutine debour(P_codegdhdeb,P_codetemp,tmax,tmin,tmin_demain,P_tdmindeb,P_tdmaxdeb,rfvi,rfpi, &  ! IN
                  upvt,udevair,udevcult)                                                            ! OUT

USE Divers, only: calcul_TemperaturesHoraires,calcul_GDH
USE Messages

implicit none

!: ARGUMENTS
! IN
  integer, intent(IN)               :: P_codegdhdeb       !> 	  // PARAMETER // option of calculation of the bud break date in hourly or daily growing degrees  // code 1/2 // PARPLT // 0 
  integer, intent(IN)               :: P_codetemp         !> 	  // PARAMETER // option calculation mode of heat time for the plant : with air temperature (1)  or crop temperature (2) // code 1/2 // PARPLT // 0 
  real,    intent(IN)               :: tmax             !> T� Max  	  // OUTPUT // Maximum active temperature of air // degree C
  real,    intent(IN)               :: tmin             !> T� Min du jour  	  // OUTPUT // Minimum active temperature of air // degree C
  real,    intent(IN)               :: tmin_demain      !> T� Min du lendemain  
  real,    intent(IN)               :: P_tdmindeb         !> 	  // PARAMETER // minimal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
  real,    intent(IN)               :: P_tdmaxdeb         !> 	  // PARAMETER // maximal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
  real,    intent(IN)               :: rfvi             !>  	  // OUTPUT // Slowing effect of the vernalization on plant development // 0-1
  real,    intent(IN)               :: rfpi             !>  	  // OUTPUT // Slowing effect of the photoperiod on plant development  // 0-1
! OUT
  real,    intent(OUT)              :: upvt             !>  	  // OUTPUT // Daily development unit  // degree.days
  real,    intent(OUT)              :: udevair          !>  	  // OUTPUT // Effective temperature for the development, computed with TAIR // degree.days
  real,    intent(OUT)              :: udevcult         !>  	  // OUTPUT // Effective temperature for the development, computed with TCULT // degree.days


! VARIABLES LOCALES
  real,dimension(24) :: thor  
  !real :: udh ! never set, never used

  ! Unités horaires
  if (P_codegdhdeb == 2) then
    ! Pour l'instant que températures air autorisées
    if (P_codetemp == 2) then
      call EnvoyerMsgHistorique(49)
      ! Pour inaki j'enleve la condition car il veut utiliser debour pour les tp de l'air
      ! et les temperature de culture pour les stades
      !stop
    endif

    ! 1. Reconstitution des températures horaires (pour l'instant que températures air)
    thor = calcul_TemperaturesHoraires(tmin,tmin_demain,tmax)

    ! 2. Calcul des gdh
    udevair = calcul_GDH(thor,P_tdmindeb,P_tdmaxdeb)
    udevcult = udevair
  endif

  if (P_codetemp == 1) then
    upvt = udevcult * rfpi * rfvi
  endif

return
end subroutine debour
