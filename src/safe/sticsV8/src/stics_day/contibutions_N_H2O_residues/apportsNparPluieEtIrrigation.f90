! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
!> N inputs by rainfall and irrigation
!!
!> - Stics book paragraphe 6.3.1, page 61
!>
!>    - The N inputs by rainfall (PLUIEN, in kg ha-1) are the product of the amount of rainfall (TRR, in mm) and
!! its mean concentration in mineral N (CONCRRG, in kg ha-1 mm-1). A mean concentration of 1 mg L-1 corresponds to 0.01 kg ha-1 mm-1.
!! The N input from rainfall occurs at the soil surface and is assumed to consist in 50% of NH4+ and 50% of NO3-.
!!
!>    - The N inputs due to irrigation water (IRRIGN, in kg ha-1) are also the product of the amounts of water (AIRG, in mm) and
!! its mean concentration, defined in the technical file (CONCIRRT, in kg ha-1 mm-1).
!! The N input is located either at the soil surface or at the depth LOCIRRIGT if the option 'localised irrigation' is activated (CODLOCIRRIGT = 3).
!! The mineral N in the irrigation water is assumed to be exclusively in the form of NO3- .
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine apportsNparPluieEtIrrigation(trr,P_concrr,airg,P_concirr,P_codlocirrig,P_locirrig,   &
                                        irrigN,nit_surf,nit_locirrig,amm,precipN,       &
                                        pluieN,precipjN,irrigjN)

  implicit none

  real,    intent(IN)    :: trr                       ! (n)  	  // OUTPUT // Rainfall  // mm.day-1
  real,    intent(IN)    :: P_concrr  !> // PARAMETER // Nitrogen concentration of the solute of the soil N-NH4 // kgN mm-1 // PARAM // 1 
  real,    intent(IN)    :: airg                      ! (n)  	  // OUTPUT // Daily irrigation // mm
  real,    intent(IN)    :: P_concirr                   ! (1) => plante principale 	  // PARAMETER // Nitrogen concentration of water // kgN mm-1 // PARTEC // 1 
  integer, intent(IN)    :: P_codlocirrig  !> // PARAMETER // code of irrigation localisation: 1= above the foliage, 2= below the foliage above the soil, 3 = in the soil // code 1/2/3 // PARTEC // 0 
  integer, intent(IN)    :: P_locirrig  !> // PARAMETER // Depth of water apply (when irrigation is applied in depth of soil) // cm // PARTEC // 1 

  real,    intent(INOUT) :: irrigN  
  real,    intent(INOUT) :: nit_surf            ! nit dans la première couche du sol  
  real,    intent(INOUT) :: nit_locirrig        ! nit à la profondeur de P_locirrig, pour l'irrigation en profondeur  
  real,    intent(INOUT) :: amm                 ! amm  dans la première couche du sol  
  real,    intent(INOUT) :: precipN  
  real,    intent(INOUT) :: pluieN  
  real,    intent(INOUT) :: precipjN      !> // OUTPUT // mineral nitrogen from rainfall // kgN
  real,    intent(INOUT) :: irrigjN      !> // OUTPUT // mineral nitrogen from irrigation // kgN

!: Variables locales

  real    :: ammsurf  !>  
  real    :: atmosph  
  real    :: nitsurf  !>  
  real    :: nitloci  

  ! a) apport par l'atmosphère (50% NH4 + 50% NO3)
      atmosph = trr * P_concrr
      nitsurf = 0.5 * atmosph
      ammsurf = nitsurf
  ! b) apport par irrigation (100% NO3)
      nitloci = airg * P_concirr

  ! répartition dans 2 couches (surf,loci)

      !- cas de l'irrigation en surface
      if (P_codlocirrig /= 3) then
        nitsurf = nitsurf + nitloci
        irrigN  = irrigN + nitloci
        nitloci = 0.
      else
        irrigN  = irrigN + nitloci
      endif

      ! mise à jour des profils d'azote
      nit_surf = nit_surf + nitsurf
      amm = amm + ammsurf

      ! domi - 21/07/03 - si on n'a pas d'irrigation en profondeur
      !-  alors P_locirrig n'a pas de raison d'etre et valeur souvent à zero
      !-  je rajoute un test pour voir
      if (P_codlocirrig == 3 .and. P_locirrig /= 0) then
        nit_locirrig = nit_locirrig + nitloci
      endif

      ! cumul des entrées d'azote
      precipN = nitsurf + nitloci
      pluieN  = pluieN + atmosph

      ! DR 03/03/08 on concerve atmosph pour climator
      precipjN = atmosph
      irrigjN = nitloci

return
end subroutine apportsNparPluieEtIrrigation
 
 
