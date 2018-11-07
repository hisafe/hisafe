
! **********************************************************************
! * Modification de la localisation des résidus de culture             *
! * Ss pg appelé après chaque opération de travail du sol              *
! **********************************************************************
! ----------------------- variables d'entree -------------------------
!
!   itrav1      cote superieure de l'operation de travail (j)     (cm)
!   itrav2      cote inferieure de l'operation de travail (j)     (cm)
!
! ------------------------ variables de sortie -----------------------
!
!   Cres(i,j)   stock C du pool résidu (j)  de la couche i       (kg/ha)
!   Nres(i,j)   stock N du pool résidu (j)  de la couche i       (kg/ha)
!   Cbio(i,j)   stock C du pool biomasse (j) de la couche i      (kg/ha)
!   Nbio(i,j)   stock N du pool biomasse (j) de la couche i      (kg/ha)
!   Chum(i)     stock C du pool humus de la couche i             (kg/ha)
!   Nhum(i)     stock N du pool humus de la couche i             (kg/ha)
!   Cnondec(j)    stock C non decomposable du résidu (j)en surface (t/ha)
!   Nnondec(j)    stock N non decomposable du résidu (j)en surface (kg/ha)
! --------------------------------------------------------------------
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 8.2, page 147
!>
!! The depth of residues incorporation in soil modifies their decomposition since water content and temperature vary with depth and their localization
!! determines the amount of mineral nitrogen available for the microbial biomass. Each tillage operation is assumed to mix the residues uniformly
!! with the soil over the depth defined by a minimal value (itrav1) and a maximal value (itrav2).
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine ResidusMelangeSol(itrav1,itrav2,nbCouches,nbResidus,  &        ! IN
                          Cres,Nres,Cbio,Nbio,Chum,Nhum,Cnondec,Nnondec,Cmulchnd,Nmulchnd)      ! INOUT
implicit none

  integer, intent(IN)    :: itrav1  
  integer, intent(IN)    :: itrav2  
  integer, intent(IN)    :: nbCouches
  integer, intent(IN)    :: nbResidus
  real,    intent(INOUT) :: Cres(nbCouches,nbResidus)
  real,    intent(INOUT) :: Nres(nbCouches,nbResidus)
  real,    intent(INOUT) :: Cbio(nbCouches,nbResidus)
  real,    intent(INOUT) :: Nbio(nbCouches,nbResidus)
  real,    intent(INOUT) :: Chum(nbCouches)
  real,    intent(INOUT) :: Nhum(nbCouches)
  real,    intent(INOUT) :: Cnondec(10)   !> // OUTPUT // undecomposable C stock of the type 10 residues on the surface // kg.ha-1
  real,    intent(INOUT) :: Nnondec(10)   !> // OUTPUT // undecomposable N stock of the type 5 residues on the surface // kg.ha-1
  real,    intent(INOUT) :: Cmulchnd
  real,    intent(INOUT) :: Nmulchnd

! VARIABLES LOCALES
  integer :: iz
  integer :: ir  
  integer :: itrav  
  real    :: Cresid(nbResidus)  
  real    :: Nresid(nbResidus)  
  real    :: Cbioma(nbResidus)  
  real    :: Nbioma(nbResidus)  
  real    :: Chumus
  real    :: Nhumus



! On sort du programme si la profondeur de travail maxi est <= 1 cm
      if(itrav2 <= 1) return
! Calcul des pools résidu, biomasse et humus sur la couche de mélange [itrav1,itrav2]
      Cresid(:) = 0.
      Nresid(:) = 0.
      Cbioma(:) = 0.
      Nbioma(:) = 0.
      Chumus    = 0.
      Nhumus    = 0.
      do ir = 11,nbResidus
  !       Cresid(ir) = SUM(Cres(itrav1:itrav2,ir))
  !       Nresid(ir) = SUM(Nres(itrav1:itrav2,ir))
  !       Cbioma(ir) = SUM(Cbio(itrav1:itrav2,ir))
  !       Nbioma(ir) = SUM(Nbio(itrav1:itrav2,ir))
          do iz=itrav1,itrav2
             Cresid(ir) = Cresid(ir) + Cres(iz,ir)
             Nresid(ir) = Nresid(ir) + Nres(iz,ir)
             Cbioma(ir) = Cbioma(ir) + Cbio(iz,ir)
             Nbioma(ir) = Nbioma(ir) + Nbio(iz,ir)
          end do
      end do
      Chumus = SUM(Chum(itrav1:itrav2))
      Nhumus = SUM(Nhum(itrav1:itrav2))

! Ajout des résidus de surface à la couche de mélange (sauf racines) et remise à 0 des résidus de surface
! Bruno : on n'ajoute le mulch à la couche de melange que si le travail du sol demarre depuis la surface
! Elsa 03/10/2012; même si itrav1>1, les résidus sont enfouis dans la couche de travail et le mulch détruit
!        if(itrav1==1) then
            do ir = 11,nbResidus-1
                 Cresid(ir) = Cresid(ir)+ Cres(1,ir-10) + Cnondec(ir-10) ! Amount of mulch-C of residue ir
                 Cres(1,ir-10)= 0.
                 Cnondec(ir-10) = 0.
                 Cbioma(ir) = Cbioma(ir)+ Cbio(1,ir-10)
                 Cbio(1,ir-10)= 0.
                 Nresid(ir) = Nresid(ir)+ Nres(1,ir-10) + Nnondec(ir-10)
                 Nres(1,ir-10)= 0.
                 Nnondec(ir-10) = 0.
                 Nbioma(ir) = Nbioma(ir)+ Nbio(1,ir-10)
                 Nbio(1,ir-10)= 0.
           end do
           Cmulchnd = 0.
           Nmulchnd = 0.
!        endif

! Répartition des résidus, biomasse & humus dans la couche de mélange
      itrav = itrav2 - itrav1 + 1
      do iz = itrav1,itrav2
          do ir = 11,nbResidus
              Cres(iz,ir) = Cresid(ir) / itrav
              Cbio(iz,ir) = Cbioma(ir) / itrav
              Nres(iz,ir) = Nresid(ir) / itrav
              Nbio(iz,ir) = Nbioma(ir) / itrav
          end do
          Chum(iz) = Chumus / itrav
          Nhum(iz) = Nhumus / itrav
      end do

return
end subroutine ResidusMelangeSol

 
 
