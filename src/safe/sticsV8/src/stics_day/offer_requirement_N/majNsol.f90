!   domi 01/12/2005 extration depuis offreN de la confrontation offre /demande
!   et de la maj du profil d'azote 
!   on le fait pour la plante à l'ombre et au soleil
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!! This module updates the elementary layer nitrogen and ammonium contents, nit(z) and amm(z), over the depth of soil.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
!!!                 on a dupliqué majNsol en majNsolCrop et majNsolTree
!!!                 pour séparer les extractions des cultures et celle des arbres
subroutine majNsolCrop(nbCouches,zrac,absz,nit,amm)


  implicit none
  
!: Arguments
  integer, intent(IN)    :: nbCouches  
  real,    intent(IN)    :: zrac      !> // OUTPUT // Depth reached by root system // cm
  real,    intent(IN)    :: absz(nbCouches)  
  real,    intent(INOUT) :: nit(nbCouches)  
  real,    intent(INOUT) :: amm(nbCouches)  

!: Variables locales
  integer :: iz  !>
  integer :: NRAC
  real    :: absamm  !>
  real    :: absnit  !>
  real    :: prop


  NRAC = int(zrac)

! *------------------------------------------* c
! * 3. Mise à jour des profils de NH4 et NO3 * c
! *------------------------------------------* c

!!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
!!!Because of hisafe voxels bigger than mini-couches we allow crop to extract water below NRAC
!!  do iz = 1,NRAC
  do iz = 1,nbCouches
    if (nit(iz) <= 0 .and. amm(iz) <= 0.) CYCLE

    ! ** l'absorption de NH4 et NO3 est répartie au prorata des quantités

    prop   = absz(iz) / (nit(iz) + amm(iz))
    absnit = prop * nit(iz)
    absamm = prop * amm(iz)
    nit(iz) =  nit(iz) - absnit
    amm(iz) =  amm(iz) - absamm

  end do



return
end subroutine majNsolCrop

subroutine majNsolTree(nbCouches,nit,amm, treeNitrogenUptake)


  implicit none

!: Arguments
  integer, intent(IN)    :: nbCouches
  real,    intent(INOUT) :: nit(nbCouches)
  real,    intent(INOUT) :: amm(nbCouches)

!!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
  real,    intent(IN)     :: treeNitrogenUptake(1000)

!: Variables locales
  integer :: iz  !>  
  real    :: absamm  !>  
  real    :: absnit  !>  
  real    :: prop  


! *------------------------------------------* c
! * 3. Mise à jour des profils de NH4 et NO3 * c
! *------------------------------------------* c

  do iz = 1,nbCouches
    if (nit(iz) <= 0 .and. amm(iz) <= 0.) CYCLE

    prop   = treeNitrogenUptake(iz) / (nit(iz) + amm(iz))
    absnit = prop * nit(iz)
    absamm = prop * amm(iz)
    nit(iz) =  nit(iz) - absnit
    amm(iz) =  amm(iz) - absamm

end do

return 
end subroutine majNsolTree

 
