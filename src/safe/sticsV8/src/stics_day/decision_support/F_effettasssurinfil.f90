! *------------------------------------------------------------------------------------------------------------------------------------------* c
!> compaction of the soil
!>- Stics book paragraphe 6.5.3, page 112
!>
!! Compaction (as influenced by sowing and harvesting machines) and soil tillage implements lead to a modification in soil bulk density (da)
!! This modification in bulk density affects infiltrability (infil) through the variable effettassurinfil
!! (effettasssurinfil modifies infil(i) in detassement.f90 and tassesemisrecolte.f90).
! *------------------------------------------------------------------------------------------------------------------------------------------* c

real function F_effettasssurinfil(da,beta_sol)

!: Arguments
  real, intent(IN) :: da   !> // OUTPUT // Bulk density in the horizon 1 // g cm-3
  real, intent(IN) :: beta_sol  
! *------------------------------------------------------------------------------------------------------------------------------------------* c
!>  Compaction (as influenced by sowing and harvesting machines) and soil tillage implements lead to a modification in soil bulk density (da)
!! This modification in bulk density affects infiltrability (infil) through the variable effettassurinfil
!! (effettasssurinfil modifies infil(i) in detassement.f90 and tassesemisrecolte.f90).
!>  Stics book paragraphe 6.5.3, page 112
! *------------------------------------------------------------------------------------------------------------------------------------------* c

      !  if (da <= 1.1)effettasssurinfil = 5.2
      !  if (da > 1.1 .and. da <= 1.2)effettasssurinfil = 4.5
      !  if (da > 1.2 .and. da <= 1.3)effettasssurinfil = 3.5
      !  if (da > 1.3 .and. da <= 1.4)effettasssurinfil = 2.8
      !  if (da > 1.4 .and. da <= 1.5)effettasssurinfil = 2.0
      !  if (da >= 1.5)effettasssurinfil = 1.3

      ! ******************************************
      ! 24/07/08 on recalcule l'infiltrabilite de l'horizon dans le cas de 
      ! tassement ou detassement  
      F_effettasssurinfil = exp(beta_sol * (da - 2.7) - 3.9)

return
end function F_effettasssurinfil
 
