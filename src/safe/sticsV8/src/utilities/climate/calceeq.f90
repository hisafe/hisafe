!  sous programme de calcul de ceeq (evaporation a l'equilibre) et
!   crnet (rayonnement net)
! NB le 26/01/05 introduction P_patm
!> subroutine for calculating ceeq (evaporation at equilibrium) and crnet (net radiation)
 real function calceeq(t,rg,P_patm)

implicit none

! Argument(s)
    real, intent(IN)  :: t          !> température  
    real, intent(IN)  :: rg         !> rayonnement global  
    real, intent(IN)  :: P_patm       !> ? 	  // PARAMETER // atmospheric pressure // mbars // STATION // 0 

! Variable(s) locale(s)
    real :: gamma  !>  
    real ::  rnet      !> // OUTPUT // Net radiation  // MJ m-2
    real :: alb  !>  
    real :: delt  

! Fonction(s) externe(s)
    real :: TVAR    ! TODO : module Divers, USE  


    ! initialisation des variables

    ! gamma
      gamma = 0.65*P_patm/1000.0

    ! P_albedo
      alb = 0.20

    ! calcul du deficit de saturation
      delt = TVAR(t+0.5) - TVAR(t-0.5)

    ! calcul du rayonnement net (en mm) selon Antonioletti & Brisson (chaleur latente = 2.5MJkg-1)
      rnet = (rg * (1 - alb) * 0.72 - 0.9504) / 2.5

    ! calcul de l'evaporation a l'equilibre en mm*decade
      calceeq = (delt / (delt + gamma)) * rnet


return
end function calceeq
 
 
