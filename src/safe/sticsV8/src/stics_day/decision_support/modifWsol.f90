! NB et SL : modification de P_zesx et Qo après travaux du sol
! selon relations mises au point sur sol de Mons
! le 15/02/07
      
! da1 = densite apparente couche 1 modifiée par tassement ou detassement
! da01 = densite apparente couche 1 du fichier sol
! idem pour da2 et da02
! *------------------------------------------------------------------------------------------------------------------------------------------* c
!> Compaction (as influenced by sowing and harvesting machines) and soil tillage implements lead to a modification in soil structure.
!>- Stics book paragraphe 6.5.1 & 6.5.2 & 6.5.3, page 111-112
!> Compaction (as influenced by sowing and harvesting machines) and soil tillage implements lead to a modification in soil structure.
!!
!! The parameters accounting for the structure of each layer are the bulk density (da) and the infiltrability (infil) at the base of the layer.
!!
!! The modification of bulk density is handled by dettassement.f90 and tassement.f90, while the modification of infiltrability
!! is handled by effettassurinfil.f90.
!!
!>  Other parameters are not independent from those structural parameters. This is the case for parameters driving evaporation: zesx and q0.
!! The modification of those parameters is handled is this sub-program, modifWsol.
!> Stics book paragraphe 6.5.1 & 6.5.2 & 6.5.3, page 111-112
! *------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine modifWsol(P_epc,da1,da2,da01,da02,zesx0,q00,P_zesx,P_q0)


      
!: Arguments
  integer, intent(IN)  :: P_epc  !> // PARAMETER // Thickness of the horizon H   (table) // cm   // PARSOL // 1 	  // OUTPUT // Thickness of the horizon 2 // cm  
  real,    intent(IN)  :: da1  
  real,    intent(IN)  :: da2  
  real,    intent(IN)  :: da01  
  real,    intent(IN)  :: da02  
  real,    intent(IN)  :: zesx0  
  real,    intent(IN)  :: q00  
  real,    intent(OUT) :: P_zesx  !> // PARAMETER // maximal depth of soil affected by soil evaporation // cm // PARSOL // 1 
  real,    intent(OUT) :: P_q0  !> // PARAMETER // Parameter of the end of the maximum evaporation stage  // mm // PARSOL // 1 
  
  real :: daequi  
  real :: daequi0  
  
!    P_zesx = 32.0*daequi-16.2
!    if (P_zesx < 19.0) P_zesx = 19.0
!    if (P_zesx > 35.0) P_zesx = 35.0  
!         if (P_epc <= P_zesx) then
!      daequi = (da1*P_epc+da2*(P_zesx-P_epc))/P_zesx
!      else
!          daequi = da1
!    endif
!    P_zesx = 32.0*daequi-16.2
!    if (P_zesx < 19.0) P_zesx = 19.0
!    if (P_zesx > 35.0) P_zesx = 35.0
!    P_q0 = 13.98-7.8*daequi
!    if (P_q0 < 1.5) P_q0 = 1.5
!    if (P_q0 > 5.4) P_q0 = 5.4
! 24/07/08 recalcule de P_zesx et P_q0 si detassement ou tassement

      if (P_epc <= 20) then
        daequi  = ( da1 * P_epc +  da2 * (20 - P_epc)) / 20
        daequi0 = (da01 * P_epc + da02 * (20 - P_epc)) / 20
      else
        daequi  = da1
        daequi0 = da01
      endif
      
      P_zesx = zesx0 * exp( 0.77 * (daequi - daequi0))
      P_q0   =   q00 * exp(-2.28 * (daequi - daequi0)) 

return
end subroutine modifWsol
 
 
