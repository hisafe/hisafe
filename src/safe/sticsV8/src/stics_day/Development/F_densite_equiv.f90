! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 10.3.4, page 194
!>
!! This module calculates an equivalent plant density for the understorey crop which accounts for the presence of the dominant crop.
!! This equivalent plant density is calculated from the densities at emergence of the two associated crops (and not anymore from the density at sowing).
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!

subroutine F_densite_equiv(ipl,densitep1,densitep2,estDominante,P_bdensp1 ,P_bdensp2,densiteeqvp1,densiteeqvp2)
    implicit none

  real      :: P_bdensp1  !< // PARAMETER // minimal density from which interplant competition starts // plants m-2 // PARPLT // 1
  real      :: P_bdensp2  !< // PARAMETER // minimal density from which interplant competition starts // plants m-2 // PARPLT // 1
  real      :: densiteeqvp1  ! calcul de la densité equivalente de la plante 1 (dominante)
  real      :: densiteeqvp2  ! calcul de la densité equivalente de la plante 1 (dominée, celle qui est affectee  )
!DR 12/09/2012 devenu inutiles
!  real      :: dassoinit
  logical   :: estDominante
  integer   :: ipl
  real      :: densitep1
  real      :: densitep2

    ! DR 07/09/2012 je recualcule les densites en cas de cAS et quand les plantes ont leve

    !!!MODIF HISAFE 12 : Modif après détection bug
    !!!on change la façon de faire le test
        !!!if (ipl > 1 .and. .not. estDominante) then
        if (ipl > 1) then
            if (estDominante) then
            else
                !  dassoinit = densitep2 ! est inutile avce les structures
                ! densités pour le calcul de la compétition
                !  densitep2 = densitep2 + densitep1 / P_bdensp1 * P_bdensp2
                ! DR 12/09/2012 on conserve la desnite equivalente de la plante 2 dans dassoiniteqv
                ! la densite de la plante 2 reste elle la densite au semis reduite par les manque à la levee ou le gel
                  densiteeqvp1 = densitep1
                  densiteeqvp2 = densitep2 + densitep1 / P_bdensp1 * P_bdensp2
            end if
        endif
return
end subroutine F_densite_equiv
