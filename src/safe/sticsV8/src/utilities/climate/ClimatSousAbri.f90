!*************************************************!>
!    calcul du climat sous abri
!    Boulard & Wang, 2000
!    programmation Brisson le 28/06/01
!
! ** constantes: changement de dénomination par rapport au papier (référence ?)
! *- P_coefrnet= pi (0.59)
! *- kh=Kh (Wm-2K-1)
! *- ks=Ks (Wm-2K-1)
! *- P_aks et P_bks = A et B tels que Ks=A+B*Vent
! *- surfserre=Ag en m2
! *- P_cvent=C
! *- cventd=Cd
! *- surfouvre=proportion d'ouvrants/surface de la serre
! *- phiv= Ov flux de ventilation en ms-1
!**************************************************
! ************************************************* !>
!> calculate climate under shelter Boulard & Wang, 2000
!! programming Brisson 28/06/01
!
!>  Constants: change of name from the paper (reference?)
!>  - P_coefrnet = pi (0.59)
!>  - Kh = kh (Wm-2K-1)
!>  - Ks = Ks (Wm-2K-1)
!>  - P_aks and P_bks = A and B such that Ks = A + B * Wind
!>  - Surfserre Ag = m2
!>  - C = P_cvent
!>  - = Cd cventd
!>  - Surfouvre = proportion of opening / area of the greenhouse
!>  - = Ov phiv ventilation flows in ms-1
subroutine ClimatSousAbri( P_aks,P_bks,tvent,n,nouvre2,nouvre3,P_surfouvre1,P_surfouvre2,P_surfouvre3,P_cvent,P_phiv0, &
                           et,tetpN_1,tetpN,P_coefrnet,trgext,tmoy,tmoyext,tmin,tminext,tmax)

!: Arguments
  real,     intent(IN) :: P_aks  !> // PARAMETER // parameter of calculation of the energetic lost between the inside and the outside of a greenhouse  // Wm-2K-1 // STATION // 1 
  real,     intent(IN) :: P_bks  !> // PARAMETER // parameter of calculation of the energetic lost between the inside and the outside of a greenhouse  // Wm-2K-1 // STATION // 1 
  real,     intent(IN) :: tvent      !> // OUTPUT // Mean speed of B2vent // m.s-1
  integer,  intent(IN) :: n  
  integer,  intent(IN) :: nouvre2  
  integer,  intent(IN) :: nouvre3  
  real,     intent(IN) :: P_surfouvre1  !> // PARAMETER // surface proportion of the shelter opened the first day of opening // SD // PARTEC // 1 
  real,     intent(IN) :: P_surfouvre2  !> // PARAMETER // surface proportion of the shelter opened the second day of opening // SD // PARTEC // 1 
  real,     intent(IN) :: P_surfouvre3  !> // PARAMETER // surface proportion of the shelter opened the third day of opening // SD // PARTEC // 1 
  real,     intent(IN) :: P_cvent  !> // PARAMETER // parameter of the climate calculation under the shelter // SD // STATION // 1 
  real,     intent(IN) :: P_phiv0  !> // PARAMETER // parameter allowing the calculation of the under shelter climate // * // STATION // 1 
  real,     intent(IN) :: et      !> // OUTPUT // Daily evapotranspiration (= es + et) // mm day-1
  real,     intent(IN) :: tetpN_1    ! DR 13/06/2013 on n'est pas sur du fonctionnement du optionnal on test
  real,     intent(IN) :: tetpN  
  real,     intent(IN) :: P_coefrnet  !> // PARAMETER // coefficient of calculation of the net radiation under greenhouse // * // STATION // 1 
  real,     intent(IN) :: trgext      !> // OUTPUT // Exterior radiation // MJ.m-2
  real,     intent(IN) :: tmoyext      !> // OUTPUT // Mean temperature of external air // degree C
  real,     intent(IN) :: tminext      !> // OUTPUT // Minimum temperature of external air // degree C

  real,     intent(OUT) :: tmin      !> // OUTPUT // Minimum active temperature of air // degree C
  real,     intent(OUT) :: tmoy      !> // OUTPUT // Mean active temperature of air // degree C
  real,     intent(OUT) :: tmax      !> // OUTPUT // Maximum active temperature of air // degree C

!: Fonction(s)
  real :: calculKH  
  real :: calculPHIV  


!: Variables locales
  real :: phiv  !>  
  real :: L  !>  
  real :: ks  !>  
  real :: kh  !>  
  real :: surfouvre  !>  
  real :: ro  !>  
  real :: cp  !>  
  real :: estimet  !>  
  real :: deltatemp  


! ** 1/ Calcul de Ks
! *- -----------------
! *- voir comment calculer ou tabuler P_aks et P_bks en fonction
! *- de Ss/Sc (rapport surface de sol/surface de couverture)
! *- différence entre Ss et Ag ?
    ks = P_aks + (P_bks*tvent)

! ** 2/ Calcul de Kh
! *------------------
! *- affectation de surfouvre
    if(n.lt.nouvre2) surfouvre = P_surfouvre1
    if(n.lt.nouvre3.and.n.ge.nouvre2) surfouvre = P_surfouvre2
    if(n.ge.nouvre3) surfouvre = P_surfouvre3
    ro = 1.2
    cp = 1.013e3
    phiv = calculPHIV(surfouvre,P_cvent,tvent,P_phiv0)

    kh = calculKH(ro,cp,phiv)

! ** 3/ Calcul de la différence de température exterieur-interieur
! *---------------------------------------------------
! --  P_coefrnet=0.59
    L = 2.5
    if (n>1) then
    !if (present(tetpN_1) .and. tetpN_1 /= 0.) then ! TODO: A TESTER : si tetpN_1 n'est pas présent, risque de crash sur le test de différence
      estimet = et/tetpN_1*tetpN
    else
      estimet = tetpN
    endif
    deltatemp = 1 / ((kh+ks)*24*3600*1e-6) * (P_coefrnet*trgext-L*estimet)

! ** 4/ Calcul des températures tmin, tmoy et tmax. Par défaut, on garde la même tmin
! *-------------------------------------------------
    tmin = tminext
    tmoy = tmoyext + deltatemp
    tmax = (2 * tmoy) - tmin


return
end subroutine ClimatSousAbri
 
 
