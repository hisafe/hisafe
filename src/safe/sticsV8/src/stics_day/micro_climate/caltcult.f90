!---------------------------------------------------------- c
! *             TEMPERATURES CULTURES                     * c
!---------------------------------------------------------- c
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module calculates the crop temperature.
!> - Stics book paragraphe 6.6.2, page 116-119
!>
!! TCULT is assumed to be the arithmetic mean of the maximum crop temperature (tcultmax) and the minimum crop temperature (tcultmin).
!! Two calculation methods are proposed, depending on the availability of weather data, using either an empirical approach or the energy balance.
!!
!! Empirical approach:
!!
!! This method must be used when neither wind speed nor air humidity data are available. It is based on a relationship between midday surface temperature and
!! daily evaporation (Seguin and Itier, 1983), and allows the calculation of tcultmax taking in account the parameterization from Riou et al. (1988).
!! rnet is the net daily radiation in MJ m-2, et the daily evapotranspiration in mm and hauteur the canopy height. tcultmax cannot be lower than tmax.
!! In this approach, we assume that tcultmin=tmin.
!!
!! Energy balance:
!!
!! Two instantaneous energy balances are calculated to estimate tcultmax and tcultmin, assumed to occur at midday and at the end of the night, respectively.
!! This calculation involves the minimum and maximum values of the various fluxes: net radiation (rnetmin and tnetmax), soil heat (gmin and gmax) and
!! evapotranspiration (etmin and etmax) as well as the minimum and maximum values of the aerodynamic resistance (raamin and raamax).
!! To calculate long wave radiation i) atmospheric radiation is assumed to remain constant throughout the day, estimated using the Brutsaert formula,
!! ii) soil radiation is calculated using tcultmax and tcultmin, requiring the iterative convergence procedure. At the end of the night, etmin and rgmin are zero,
!! while rgmax and etmax are estimated assuming sinusoidal changes during the day. gmin is calculated as an empirical function of the wind speed under the cover
!! (Cellier et al.,1996). gmax is taken to be 25% of the maximum net radiation below the cover. In addition to the canopy height (hauteur) and the bare soil
!! roughness (z0solnu), the calculation of raamax and raamin requires wind speed values: the night-time wind speed is assumed to be equal to 50% of the daily
!! mean wind speed, and the daytime wind speed is assumed to be 150% of the daily mean wind speed.
!!
!! Iterative calculation of TCULT
!!
!! tcult is involved in the calculation of net radiation, which in turn is used to calculate energy balances. In the previous version of STICS, the air
!! temperature was used for calculation of long wave radiation to avoid numerical calculations. This hypothesis has demonstrated its limitations
!! (tcult sometimes greater than 60°C !); this led us to introduce an iterative calculation process based on a difference of 0.5° between two iterations.
!! In the option using Shuttleworth and Wallace the iteration also concerns estimates of water requirements, while in the option using the reference
!! evapotranspiration as an input, the iterative process is only used to calculate net radiation.
!!
!!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
!!!Ajout cellTrg et cellVisibleSky
! ------------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine caltcult(P_codabri,Emd,hauteur,eptcult,esol,Emulch,P_z0solnu,tmax,tvent,raamin,rnetS,P_codebeso,phoi,raamax, &
                    tcult,tcultmin,tcultmax,et,tutilrnet,numdate,daylen,difftcult,nitetcult,Ratm,nbCouches,jul,   &
                    hur,humin,hucc,lai,tauxcouv,posibsw,P_albedo,couvermulch,albedomulch,P_albveg,P_codernet,P_aangst,P_bangst,&
                    tmin,tmoy,P_corecTrosee,trg,P_latitude,P_codepaillage,P_codelaitr,tpm,P_codecaltemp,albedolai,rglo,rnet,&
                    cellTrg, cellVisibleSky)

  implicit none

!: Arguments

  integer, intent(IN)    :: P_codabri  !> // PARAMETER // option of calculation of the climate under a shelter // code 1/2 // PARTEC // 0 
  real,    intent(IN)    :: Emd      !> // OUTPUT // Direct evaporation of water intercepted by leafs  // mm
  real,    intent(IN)    :: hauteur      !> // OUTPUT // Height of canopy // m
  real,    intent(IN)    :: eptcult  
  real,    intent(IN)    :: esol      !> // OUTPUT // Actual soil evaporation flux  // mm day-1
  real,    intent(IN)    :: Emulch      !> // OUTPUT // Direct evaporation of water intercepted by the mulch // mm
  real,    intent(IN)    :: P_z0solnu  !> // PARAMETER // roughness length of bare soil // m // PARSOL // 1 
  real,    intent(IN)    :: tmax      !> // OUTPUT // Maximum active temperature of air // degree C
  real,    intent(IN)    :: tvent      !> // OUTPUT // Mean speed of B2vent // m.s-1
  real,    intent(IN)    :: raamin  
  real,    intent(IN)    :: rnetS      !> // OUTPUT // Net radiation in the soil // Mj.m-2
  integer, intent(IN)    :: P_codebeso  !> // PARAMETER // option computing of water needs by the k.ETP (1) approach  or resistive (2) approach // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: phoi      !> // OUTPUT // Photoperiod // hours
  real,    intent(IN)    :: raamax  

  real,    intent(INOUT) :: tcult      !> // OUTPUT // Crop surface temperature (daily average) // degree C
  real,    intent(INOUT) :: tcultmin  
  real,    intent(INOUT) :: tcultmax      !> // OUTPUT // Crop surface temperature (daily maximum) // degree C
  real,    intent(INOUT) :: et      !> // OUTPUT // Daily evapotranspiration (= es + et) // mm day-1
  real,    intent(INOUT) :: tutilrnet  
  integer, intent(INOUT) :: numdate  
  real,    intent(INOUT) :: daylen  
  real,    intent(INOUT) :: difftcult  
  integer, intent(INOUT) :: nitetcult      !> // OUTPUT // Number of iterations to calculate TCULT // SD
  real,    intent(INOUT) :: Ratm      !> // OUTPUT // Atmospheric radiation  // Mj.m-2

  integer, intent(IN)    :: nbCouches  
  integer, intent(IN)    :: jul  
  real,    intent(IN)    :: hur(nbCouches)  
  real,    intent(IN)    :: humin(nbCouches)  
  real,    intent(IN)    :: hucc(nbCouches)  
  real,    intent(IN)    :: lai                   ! laiTMP       // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(IN)    :: tauxcouv      !> // OUTPUT // Cover rate // SD
  logical, intent(IN)    :: posibsw  
  real,    intent(IN)    :: P_albedo  !> // PARAMETER // P_albedo of the bare dry soil // SD // PARSOL // 1 
  real,    intent(IN)    :: couvermulch      !> // OUTPUT // Cover ratio of mulch  // 0-1
  real,    intent(IN)    :: albedomulch  
  real,    intent(IN)    :: P_albveg  !> // PARAMETER // P_albedo of the vegetation // SD // STATION // 1 
  integer, intent(IN)    :: P_codernet  !> // PARAMETER // option of calculation of net radiation // code 1/2/3 // STATION // 0 
  real,    intent(IN)    :: P_aangst  !> // PARAMETER // coefficient of the Angstrom's relationship for extraterrestrial radiation // SD // STATION // 1 
  real,    intent(IN)    :: P_bangst  !> // PARAMETER // coefficient of the Angstrom's relationship for extraterrestrial radiation // SD // STATION // 1 
  real,    intent(IN)    :: tmin      !> // OUTPUT // Minimum active temperature of air // degree C
  real,    intent(IN)    :: tmoy      !> // OUTPUT // Mean active temperature of air // degree C
  real,    intent(IN)    :: P_corecTrosee  !> // PARAMETER // temperature to substract to Tmin to estimate dew point teñperature (in case of missing air humidity data) // degree C // STATION // 1
  real,    intent(IN)    :: trg      !> // OUTPUT // Active radiation (entered or calculated) // MJ.m-2
  real,    intent(IN)    :: P_latitude  !> // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0 
  integer, intent(IN)    :: P_codepaillage  !> // PARAMETER // option: 1 = no cover, 2 = plastic cover partly covering the soil   // code 1/2 // PARTEC // 0
  integer, intent(IN)    :: P_codelaitr  !> // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0 

  real,    intent(INOUT) :: tpm      !> // OUTPUT // Vapour pressure in air // mbars
  integer, intent(INOUT) :: P_codecaltemp  !> // PARAMETER // option of use of crop temperature for phasic development calculation : yes (2), no (1)  // code 1/2 // STATION // 0 
  real,    intent(INOUT) :: albedolai      !> // OUTPUT // P_Albedo of the crop cobining soil with vegetation // SD
  real,    intent(INOUT) :: rglo  
  real,    intent(INOUT) :: rnet      !> // OUTPUT // Net radiation  // MJ m-2

  real,     intent(IN)   :: cellTrg              !> // INPUT // Global radiation // %
  real,     intent(IN)   :: cellVisibleSky      !> // INPUT // Visible sky // %


!: Variables locales
  real    :: ab  !>  
  real    :: bb  !>  
  real    :: etmax  
  real    :: gmax  !>  
  real    :: gmin  !>  
  real    :: rgmax  !>  
  real    :: rnetmax  !>  
  real    :: rnetmin  !>  
  real    :: rnetSmax  
  real    :: Rsolglo  !>  
  real    :: sigma  !>  
  real    :: v  !>  
  real    :: z0  
  real    :: pi  !>  
  real    :: ratmh  

! fonction(s)
  real    :: gsol  




      sigma = 5.67e-8
      pi = atan(1.0) *4

! ** cas de la culture sous abri
      if (P_codabri == 2) then
        tcult = tmoy
        tcultmin = tmin
        tcultmax = tmax
        return
      endif



      et = eptcult + esol + Emd + Emulch

      if (nitetcult == 0) then
        tcult = tmoy
        tcultmin = tmin
        tcultmax = tmax
      endif


      !: Boucle de réitération rnet-tcult
B1:   do while(.TRUE.)

        ! ** recalcul du rayonnement net  uniquemlent si option ketp
        ! *-sinon la boucle se fait dans S & W
        if (P_codebeso == 1) then
          tutilrnet = tcult
          call calrnet(nbCouches,jul,hur,humin,hucc,lai,tauxcouv,posibsw,Ratm,              & ! IN
                       P_albedo,couvermulch,albedomulch,P_albveg,P_codernet,P_aangst,P_bangst,tmin,   &
                       tmoy,P_corecTrosee,trg,P_latitude,tutilrnet,P_codepaillage,P_codelaitr,      &
                       tpm,P_codecaltemp,albedolai,rglo,rnet, cellTrg, cellVisibleSky)
          P_codecaltemp = 1
        endif

        ! ** calcul de la température de surfacce par la relation simplifiée
        ! *- Riou et al., 1988. Int. J. Remote sensing 9-1529-1533
        if (P_codecaltemp == 1) then
          ! ** NB - le 18/04/02
          ! --        z0 = max(0.13*hauteur,0.001)
          z0 = max(0.13 * hauteur, P_z0solnu)
          bb = 1.68 / log(1.0 / z0)
          ab = 1.27
          tcultmax = tmax + (rnet / 2.46 - et - ab) / bb
!          write(71,*) 'tc.',tcultmax,tmax,rnet,et,bb,P_z0solnu,hauteur
          if (tcultmax < tmax) tcultmax = tmax
          tcultmin = tmin

        else
          ! ** calcul de la température de surface par bilan d'énergie (P. Cellier)
          ! *- on suppose que le rayonnement atmospherique est constant sur la journée
          call ratmo(trg,P_aangst,P_bangst,P_latitude,jul,tpm,tmoy,Ratm)
          Ratmh = Ratm / 24 * 1e6 / 3600

          ! au mini
          Rsolglo = sigma * (tcultmin + 273.15)**4
          Rglo = Ratmh - Rsolglo

          ! ** atténuation du vent sous le couvert
          v = max(tvent * exp(-0.96 * lai), 0.2)      ! lai => somme des lai de toutes les plantes
          rnetmin = Rglo
          gmin = gsol(6.0,v*0.5,rnetmin,0.0)
          tcultmin = (rnetmin - gmin) * raamin / 1200 + tmin

          ! ** au maxi
          ! *- calcul du flux de chaleur sol selon P. Cellier
          ! *- domi - 15/04/2002 - on ne connait pas numdate si il n'est pas calculé
          numdate = jul
          !if (numdate > nbjsemis) numdate = numdate - nbjsemis

          call photpd(P_latitude,numdate,daylen,phoi)

          etmax = et * 3.14 / 2 / daylen * 2.46 * 1e6 / 3600
          rgmax = trg * 3.14 / 2 / daylen * 1e6 / 3600

          Rsolglo = sigma * (tcultmax + 273.15)**4
          Rglo = Ratmh - Rsolglo
          rnetmax = (1 - albedolai) * rgmax + Rglo
          ! NB et DR 02/11/06 pb de calcul de rnet si trg tres petit
          if (rnetmax < rnetmin) rnetmax = rnetmin
          rnetSmax = rnetS / rnet * rnetmax
          gmax = 0.25 * rnetSmax
          tcultmax = (rnetmax - gmax - etmax) * raamax / 1200 + tmax

        endif

        ! ** Nb - le 11/06
        difftcult = tcult - ((tcultmin + tcultmax) / 2)
        tcult = (tcultmin + tcultmax) / 2

        ! ** réitération - nb - le 11/06
        nitetcult = nitetcult + 1
        if (abs(difftcult) > 0.5 .and. P_codebeso == 1) then
          if (nitetcult <= 5) then
            CYCLE
          else
            EXIT B1
          endif
        else
          EXIT B1
        endif

      end do B1

return
end



! ------------------------------------------------------------- c
! *        FUNCTION gsol(heure,v,rnet,hsens)                  * c
! ------------------------------------------------------------- c
real function gsol(heure,v,rnet,hsens)

  real, intent(IN) :: heure  
  real, intent(IN) :: rnet   !> // OUTPUT // Net radiation  // MJ m-2
  real, intent(IN) :: hsens  
  real, intent(IN) :: v  

  real :: omega  
  real :: Pgh  

      omega = 2*3.14/24.0
      if (heure <= 7.0.or.heure >= 18) then
        Pgh = 1
      else
        Pgh = cos(omega*(heure-12.0+1.5))/cos(omega*(heure-12.0-1.0))
      endif

      if (rnet > 0.0) then
        gsol = 1.36/sqrt(v)*hsens*Pgh
      else
        if (v < 1.0) then
          gsol = 0.9*rnet
        else
          gsol = max(0.3,(0.9+0.1*(v-1)))*rnet
        endif
      endif

return
end function gsol
! ------------------------------------------------------------------ c
 
 
