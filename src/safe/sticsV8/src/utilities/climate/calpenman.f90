! ************************************************ c
! *          calcul de etpp Penamn               * c
! * calcul du rayonnement net en MJm-2j-1 selon  * c
! * stefce (Brunt avec P_albedo = 0.2)             * c
! ************************************************ c
!> Calculate Penman evapotranspiration
!> Calculation of net radiation in MJm-2d-1 as Stefce (Brunt with P_albedo = 0.2)
subroutine calpenman(jul,P_patm,tmoy,tpm,trg,P_latitude,tvent2m,Rglo,etpp)


! Variable(s) locale(s)
    integer, intent(IN)    :: jul  
    real,    intent(IN)    :: P_patm  !> // PARAMETER // atmospheric pressure // mbars // STATION // 0 
    real,    intent(IN)    :: tmoy          ! (n)  	  // OUTPUT // Mean active temperature of air // degree C
    real,    intent(IN)    :: tpm           ! (n)  	  // OUTPUT // Vapour pressure in air // mbars
    real,    intent(IN)    :: trg           ! (n)  	  // OUTPUT // Active radiation (entered or calculated) // MJ.m-2
    real,    intent(IN)    :: P_latitude  !> // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0 
    real,    intent(IN)    :: tvent2m       !  


    real,    intent(OUT)    :: Rglo  
    real,    intent(OUT)    :: etpp         ! (n)  	  // OUTPUT // Potential evapotranspiration as given by Penman formula // mm day-1


    real    :: L  !>  
    real    :: deltat  !>  
    real    :: gamma  !>  
    real    :: dsat  !>  
    real    :: fracinsol  !>  
    real    :: rnetpen  

! Fonction(s)
    real    :: TVAR  !>  
    real    :: RGEX  

! ML le 28/02/08 creation de la variable locale tvent2m correspondant à la vitesse
! du vent mesurée à 2 mètres et utilisable plus bas dans le calcul de ETP Penman
! tmoy temperature moyenne en degree C
! tpm tension de vapeur en mb
! jul = numero du jour dans l'annee
! trg ray global mj/m2/j
! etpp etp en mm
! P_latitude : P_latitude en degres et dixieme
! P_patm = pression atmospherique à l'altitude de la simulation en mb (1000 en standard)
! tvent = vitesse moyenne du vent à 2m en m/s




      gamma = .65 * P_patm / 1000.

      deltat = TVAR(tmoy + .5) - TVAR(tmoy - .5)

      dsat = TVAR(tmoy) - tpm

      L = (2500840 - 2358.6 * tmoy) * 1e-6

      fracinsol = (trg / RGEX(P_latitude / 180 * 3.14, jul) - 0.18) / 0.62
      fracinsol = max(fracinsol,0.0)
      fracinsol = min(fracinsol,1.0)
      Rglo = 4.9e-9 * (((tmoy + 273.16)**4) * (0.1 + 0.9 * fracinsol) * (0.56- .08 * sqrt(tpm)))
      rnetpen = (1-0.2) * trg - Rglo

    ! ML le 28/02/08
    ! remplacement de la variable tvent par la variable locale tvent2m
    ! correspondant à la vitesse du vent mesurée à 2 mètres
      !--etpp(n) = rnetpen/L*deltat/(deltat+gamma)+(gamma/(deltat+gamma)) * (0.26*(1+0.54*tvent(n)))*dsat

      etpp = rnetpen / L * deltat / (deltat + gamma) + (gamma / (deltat + gamma)) * (0.26 * (1 + 0.54 * tvent2m)) * dsat

      etpp = max(etpp,0.0)



return
end subroutine calpenman
 
 
