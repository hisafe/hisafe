!*********************************************************************
!         calcul les besoins en eau de la culture
!         version 5.0
!         approche k * etm
!   derniere modif 29/03/2001
!*********************************************************************
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> plant water requirements
!> - Stics book paragraphe 7.2.1, page 130-132
!>
!! This module calculates the plant water requirements (or maximum transpiration eop) thanks to the crop coefficient approach
!! In the crop coefficient approach, fully documented in Brisson et al. (1992b), plant water requirements (maximum transpiration) are calculated in several steps,
!! using the potential evapotranspiration as the driving variable.
!! First of all, calculation of what the crop evaporation value would be if none of the soil or plant surfaces had limited water (EO). This evaporation is a
!! logistic function of the LAI (or a linear function of the ground cover) which involves the KMAXP parameter, the maximum crop coefficient of the crop.
!! kmax is attained when the LAI is approximately 5 (or tauxcouv equals tauxrecouvkmax generally taken to 1) and depends on the reference evapotranspiration used
!! (Penman, Penman-Monteith or Priestley- Taylor : Penman, 1948, Monteith, 1965, Priestley and Taylor, 1972).
!!
!! If the leaves have intercepted water(mouill), then this water will evaporate depending on the reference evaporative demand (tetp): Emd for leaves
!! and Emulch for mulch.  Naturally, the Emd threshold is set by the amount of water retained on the foliage (mouill) while the Emulch threshold is set by
!! the amount of water retained in the mulch (see module etatsurf.f90). The evaporated water contributes to reducing evaporative demand at the plant level
!! Maximal transpiration depends on the available energy in plants, estimated by subtracting eos from eo but also on atmospheric conditions in the vegetation.
!! In order to take into consideration the increase in plant demand due to the dryness of the soil below the vegetation, we use the empirical relationship
!! based on the parameter beta deduced from work by Denmead (1973), Ritchie (1985) or Feddes (1987).
!! Edirectm corresponds to the maximum evaporation by soil+mulch+leaves together, and Edirect corresponds to the actual evaporation of the three together.
!! A value of 1.4 is taken for beta. It causes eop to increase by a maximum of 40 % when the soil is completely dry.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
subroutine ketp(n,P_codelaitr,lai,tauxcouv,tetp,P_beta,delta,P_codeplante,P_corecTrosee,    &
                Emulch,P_kmax,LAIapex,posibsw,P_tauxrecouvkmax,tmin,esol,               &
                eop,mouill,doi,Edirect,Emd,eo,etm,tpm)

USE Divers, only: TVAR

  implicit none

!: Arguments
  integer, intent(IN)    :: n  
  integer, intent(IN)    :: P_codelaitr  !> // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: lai                     ! (1,AS,n) = plante principale, soleil    // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(IN)    :: tauxcouv                ! (n)    // OUTPUT // Cover rate // SD
  real,    intent(IN)    :: tetp                    ! (n)    // OUTPUT // Efficient potential evapotranspiration (entered or calculated) // mm day-1
  real,    intent(IN)    :: P_beta  !> // PARAMETER // parameter of increase of maximal transpiration when occurs a water stress // SD // PARAM // 1 
  real,    intent(IN)    :: delta  

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=3), intent(IN)    :: P_codeplante  !> // PARAMETER // Name code of the plant in 3 letters // * // PARPLT // 0
  integer, intent(IN)    :: P_codeplante  !> // PARAMETER // Name code of the plant in 3 letters // * // PARPLT // 0

  real,    intent(IN)    :: P_corecTrosee  !> // PARAMETER // temperature to substract to Tmin to estimate dew point teñperature (in case of missing air humidity data) // degree C // STATION // 1
  real,    intent(IN)    :: Emulch   !> // OUTPUT // Direct evaporation of water intercepted by the mulch // mm
  real,    intent(IN)    :: P_kmax  !> // PARAMETER // Maximum crop coefficient for water requirements (= ETM/ETP) // SD // PARPLT // 1 
  real,    intent(IN)    :: LAIapex  
  logical, intent(IN)    :: posibsw  
  real,    intent(IN)    :: P_tauxrecouvkmax  !> // PARAMETER // soil cover rate corresponding to the maximal crop coefficient for water requirement  // m2 plante m-2 sol // PARPLT // 1 
  real,    intent(IN)    :: tmin                    ! (n)    // OUTPUT // Minimum active temperature of air // degree C
  real,    intent(IN)    :: esol   !> // OUTPUT // Actual soil evaporation flux  // mm day-1

  real,    intent(INOUT) :: eop                     ! (1,AS) = plante principale, soleil    // OUTPUT // Maximum transpiration flux  // mm
  real,    intent(INOUT) :: mouill                  ! (1,AS) = plante principale, soleil  
  real,    intent(INOUT) :: doi  
  real,    intent(INOUT) :: Edirect   !> // OUTPUT // Water amount evaporated by the soil + intercepted by leafs + intercepted by the mulch  // mm
  real,    intent(INOUT) :: Emd   !> // OUTPUT // Direct evaporation of water intercepted by leafs  // mm
  real,    intent(INOUT) :: eo   !> // OUTPUT // Intermediary variable for the computation of evapotranspiration   // mm
  real,    intent(INOUT) :: etm   !> // OUTPUT // Maximum evapotranspiration ( = eop + es)  // mm
  real,    intent(INOUT) :: tpm                     ! (n)    // OUTPUT // Vapour pressure in air // mbars

!: Variables locales
  real :: Edirectm  !>  
  real :: temp1  

    ! calcul de eo : augmentation du rapport eo/etpp
    ! en fonction du coefficient cultural maximal et du LAI
      if (P_codelaitr == 1) then
      ! 03/07 NB et Sylvain ajout de LAIapex dans le calcul de eo
      !-- eo = tetp*(1+(P_kmax-1)/(1+exp(-1.5*(lai-3))))
        eo = tetp * (1 + (P_kmax - 1) / (1 + exp(-1.5 * (lai + LAIapex - 3))))
      else
        if (tauxcouv > P_tauxrecouvkmax) then
          eo = tetp * P_kmax
        else
          eo = tetp * (tauxcouv * (P_kmax - 1) / P_tauxrecouvkmax + 1)
        endif
      endif

    ! on fait évaporer l'eau interceptée par les feuilles
    ! on estime que le pouvoir évaporant de l'air pour une surface d'eau (Emd) libre
    ! est de l'ordre de eo- évaporation potentielle sol
      if (P_codelaitr == 1) then
        Emd = eo - tetp * exp(-delta*lai)
      else
        Emd = eo - tetp * (1 - tauxcouv)
      endif

      Emd = min(mouill, Emd)
      mouill = max(mouill - Emd,0.0)

    ! Edirect pour calcul du déficit de saturation interne
      Edirect = esol + Emd + Emulch

! ** calcul de eop, effet de micro-convection du sol sec
! *- domi 23/03/01 taux de couverture

      if ((P_codelaitr == 1 .and. lai <= 0.0) .or. (P_codelaitr == 2 .and. tauxcouv <= 0.0)) then
        eop = 0.0
      else
      ! ajout de l'effet évaporation directe
        if (P_codelaitr == 1) then
          ! NB et Sylvain ajout de LAIapex dans le calcul de Edirectm 280807
          !-- Edirectm = tetp*exp(-delta*lai)+Emd
          Edirectm = tetp * exp(-delta*(lai+LAIapex))+Emd
!  write(ficdbg,'(i3,4f16.12)')n,tetp,delta,lai,Emd
        else
          Edirectm = tetp * (1 - tauxcouv)+Emd
        endif

        Edirectm = min(Edirectm, eo)

        if (Edirectm > 0.0) then
          temp1 = (1.-P_beta)*Edirect/Edirectm
          eop = (eo-Edirectm)*(P_beta+temp1)

!          write(70,'(i3,4f16.12)')n,eop,eo,P_beta,temp1

        else
          eop = eo
!          write(70,'(i3,2f16.12)')n,eop,eo
        endif
! --        eop = (eo-eos)*(P_beta+(1.-P_beta)*(es/eos))
      endif


    ! calcul de etm = eop+es
      if (eop < 0)eop = 0.0
      etm = eop + esol
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!      if (P_codeplante == 'snu') etm = tetp
      if (P_codeplante == 1) etm = tetp

    ! déficit de saturation interne doi = 0
    ! quand on ne passe pas dans S&W
      doi = 0.0

      ! Bug découvert avec Fred H sur le recalcul de Tpm quand données manquantes
      ! le 24/02/06
      !-- if (.not.posibsw)  tpm = TVAR(tmin)
      !!!MODIF HISAFE 12 : Modif après détection bug
      !!!MODIF HISAFE on change la façon de faire le test
      !!!if (.not.posibsw) tpm = TVAR(tmin - P_corecTrosee)
      if (posibsw) then
      else
        tpm = TVAR(tmin - P_corecTrosee)
      endif

!  write(ficdbg,'(i3,f16.12)')n,eop

return
end subroutine ketp
 
 
