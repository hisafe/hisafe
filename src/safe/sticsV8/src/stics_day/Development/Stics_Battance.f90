!> *******************************************************************
!! * Simulation de la formation d'une coûte de battance
!! * et de son impact sur le taux de levée des cultures annuelles
!! * semées par l'intermédiaire de la variable "BATTANCE"
!! * BATTANCE = 0.0 s'il y a obstacle à la levée
!! * P_PLUIEBAT et P_MULCHBAT sont des paramètres (paramv6.par)
!! * N. Brisson & G. Richard, sur la base de 
!! * Durr et al., 2001 (Soil Sci. Soc. Am. J.65:414-423)
!< *********************************************************************
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - This module estimates the soil crusting.
!> - Stics book paragraphe 2.2.1.d, page 24-27
!>
!! In the particular case of loamy soils, a crust may occur after sowing, creating a physical obstacle to emergence (Duval and Boiffin, 1990). In addition to
!! the textural characteristics of the surface soil layer, the development of such a crust depends on soil fragmentation following seedbed preparation and on
!! the weather at the time. Indeed, post-sowing rainfall may destroy soil fragments and then drought renders this layer almost impenetrable for the plantlets,
!! since the resistance to emergence depends on the weather through its evaporative demand and on the force exerted by the plantlet.
!! The formalisation of these processes in STICS relies partly on the work of Durr et al. (2001). Surface crusting is assumed to occur only after sowing once
!! a certain amount of rainfall (soil-dependent parameter pluiebat) has occurred. The crust is assumed to be dry when the natural mulch depth
!! (xmlch1: variable calculated from the soil evaporation formulations, see the module solnu.f90) is greater than the threshold parameter mulchbat,
!! in which case xmlch1 is considered as the thickness of the crusted layer.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
real function battance(n,nplt,nrec,P_codeperenne,pluiesemis,trr,P_pluiebat,P_mulchbat,xmlch1,elong,P_profsem)
  
!: ARGUMENTS
! IN
  integer, intent(IN)   :: n  
  integer, intent(IN)   :: nplt  
  integer, intent(IN)   :: P_codeperenne  !> // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0 
  integer, intent(IN)   :: nrec  
  real,    intent(IN)   :: trr   !> // OUTPUT // Rainfall  // mm.day-1
  real,    intent(IN)   :: P_pluiebat  !> // PARAMETER // minimal rain quantity for the crust occurrence // mm day-1 // PARSOL // 1 
  real,    intent(IN)   :: elong  
  real,    intent(IN)   :: P_profsem  !> // PARAMETER // Sowing depth // cm // PARTEC // 1 
  real,    intent(IN)   :: P_mulchbat  !> // PARAMETER // mulch depth from which a crust occurs // cm // PARSOL // 1 
  real,    intent(IN)   :: xmlch1   !> // OUTPUT // Thickness of mulch created by evaporation from the soil // cm
! OUT
  real,    intent(OUT)  :: pluiesemis  

!: Variables locales
  logical :: batpot  
  logical :: croutebattance  
  
  ! à initialiser pluiesemis,batpot,croutebattance
         
    ! variable logique "battance potentielle" après le semis
    if (n >= nplt .and. nrec == 0.and.P_codeperenne == 1) then
      batpot = .TRUE.
    else
      batpot = .FALSE.
    endif
    
    ! compteur de pluie après le semis
    if (n >= nplt) pluiesemis = pluiesemis + trr

    ! croute de battance humide
    if (pluiesemis >= P_pluiebat) croutebattance = .TRUE.

    ! état hydrique de la croûte de battance
    if (batpot .and. croutebattance) then
      if (xmlch1 >= P_mulchbat .and. elong >= (P_profsem - xmlch1)) then
        battance = 0.0
      else
        battance = 1.0
      endif
    else
      battance = 1.0
    endif


return
end 
