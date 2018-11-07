! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
!> - Stics book paragraphe 6.3.2.b, page 104
!>
!! Similarly to water applications, fertilizer N applications can be either prescribed (option generally used) or calculated by the model.
!! This subroutine is activated when using the calculated fertilization option; the model calculates the N applications required to maintain
!! the nitrogen nutrition index (INN) above a given threshold (RATIOLNT). Two other conditions must be fulfilled:
!>   - 1.    The N uptake by the crop needs to be a limiting factor, i.e. the soil supply (CUMOFFRN) must be lower than the plant demand (DEMANDE).
!!       This condition is essential because INN characterizes a plant status which can remain N deficient for a long time even though root uptake is maximal.
!!       It is no use applying N that the plant cannot absorb when its uptake rate is maximal.
!>   - 2.    The soil must be wet enough in order to allow water and nitrate transport towards the roots.
!!       Two technical options are proposed to fulfil this condition: either a test on rainfall (PRECIP > PLNMING,)
!!       or a test on water availability in the upper soil layer (HUR (1) >= HUCC (1)).
!!
!! Since INN can be greater than 1, the threshold RATIOLNT can be set at a high value, for example 1.4 or 1.8, in order to mimic early applications
!! of fertilizer which occur in favourable conditions for plant uptake. The calculated N rate is the difference between the maximal amount of N
!! in the crop (QNPLMAX, calculated from the maximal dilution curve) and the actual amount of N, divided by the N use efficiency EFFN.
!! It is limited by a maximal N rate (DOSIMXNT).
!
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c

subroutine calculAutomatiqueFertilisation(P_voleng,P_deneng,P_orgeng,P_codetesthumN,inns,P_ratiolN,precip,P_plNmin, &
                                          cumoffrN,demande,masec,P_masecNmax,QNplante,P_dosimxN,hur,hucc,   &
                                          P_adilmax,P_bdilmax,effN,anit,napN)

  implicit none

!IN
    real,    intent(IN)    :: P_voleng  !> // PARAMETER // maximal fraction of mineral fertilizer that can be volatilized  // SD // PARAM // 1 
    real,    intent(IN)    :: P_deneng  !> // PARAMETER // proportion of the ñineral fertilizer that can be denitrified (useful if codenit not active) // SD // PARAM // 1 
    real,    intent(IN)    :: P_orgeng  !> // PARAMETER // maximal quantity of mineral fertilizer that can be organized in the soil (fraction for type 8) // kg ha-1 // PARAM // 1 
    integer, intent(IN)    :: P_codetesthumN  !> // PARAMETER // automatic fertilisation calculations // code 1/2 // PARAMV6 // 0 
    real,    intent(IN)    :: inns   !> // OUTPUT // Index of nitrogen stress active on growth in biomass // P_innmin to 1
    real,    intent(IN)    :: P_ratiolN  !> // PARAMETER // Nitrogen stress index below which we start an fertilisation in automatic mode (0 in manual mode) // between 0 and 1  // PARAMV6 // 1 
    real,    intent(IN)    :: precip   !> // OUTPUT // Daily amount of water (precipitation + irrigation)   // mm day-1
    real,    intent(IN)    :: P_plNmin  !> // PARAMETER // Minimal amount of precipitation to manage a fertilization // mm day-1 // PARAM // 1 
    real,    intent(IN)    :: cumoffrN  
    real,    intent(IN)    :: demande   !> // OUTPUT // Daily nitrogen need of the plant   // kgN.ha-1.j-1
    real,    intent(IN)    :: masec   !> // OUTPUT // Aboveground dry matter  // t.ha-1
    real,    intent(IN)    :: P_masecNmax  !> // PARAMETER // Aerial biomass  on and after there is nitrogen dilution (critical and maximal curves) // t ha-1 // PARPLT // 1 
    real,    intent(IN)    :: QNplante   !> // OUTPUT // Amount of nitrogen taken up by the plant  // kgN.ha-1
    real,    intent(IN)    :: P_dosimxN  !> // PARAMETER // maximum amount of fertilisation authorised at each time step (mode automatic fertilisation) // kg N ha-1 // PARAMV6 // 1 
    real,    intent(IN)    :: hur  
    real,    intent(IN)    :: hucc  
    real,    intent(IN)    :: P_adilmax  !> // PARAMETER // Parameter of the maximum curve of nitrogen needs [Nplante]=P_adilmax MS^(-P_bdilmax) // N% MS // PARPLT // 1 
    real,    intent(IN)    :: P_bdilmax  !> // PARAMETER // Parameter of the maximum curve of nitrogen needs [Nplante]=P_adilmax MS^(-P_bdilmax) // SD // PARPLT // 1 

!INOUT
    real,    intent(INOUT) :: effN  
    real,    intent(INOUT) :: anit   !> // OUTPUT // Daily nitrogen provided  // kgN.ha-1 j-1
    integer, intent(INOUT) :: napN  

!: Variables locales
  real    :: msmax  
  real    :: QNplmax  

    ! efficience approchée de l'P_engrais
    effN = 1. - P_voleng - P_deneng - P_orgeng

    ! bruno 06/2002
    effN = max(0.2, effN)

    if (P_codetesthumN == 1) then
      ! DR 10/03/08 on fertilise en comparant P_ratiolN à inns et plus à inn
      !-- if (inn(1,AS) < P_ratiolN .and. precip >= P_plNmin .and.
      if (inns < P_ratiolN .and. precip >= P_plNmin .and. cumoffrN < demande) then
        ! quantité maximale d'azote dans la plante
        msmax = max(masec, P_masecNmax)
        QNplmax = 10. * P_adilmax * msmax**(-P_bdilmax) * masec
        ! calcul de la dose
        anit = min( ((QNplmax - QNplante) / effN), P_dosimxN)
        napN = napN + 1
      else
        anit = 0.
      endif
    else
      ! DR 10/03/08 on fertilise en comparant P_ratiolN à inns et plus à inn
      !-- if (inn(1,AS) < P_ratiolN .and. hur >= hucc  .and.
      if (inns < P_ratiolN .and. hur >= hucc .and. cumoffrN < demande) then
        ! quantité maximale d'azote dans la plante
        msmax = max(masec, P_masecNmax)
        QNplmax  = 10. * P_adilmax * msmax**(-P_bdilmax) * masec
        ! calcul de la dose
        anit = min( ((QNplmax - QNplante) / effN), P_dosimxN)
        napN = napN + 1
      else
        anit = 0.
      endif
    endif

return
end subroutine calculAutomatiqueFertilisation
 
 
