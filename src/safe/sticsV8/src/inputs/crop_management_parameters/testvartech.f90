!> Here it is assumed that the main crop is that of ipl = 1
!! and therefore are forced to work the technical parameters of
!! soil plants ipl> 1 to be equal to those of the plant
!! of ipl = 1 ...
!
!  Ici on suppose que la culture principale est celle d'ipl = 1
! et donc on force les param�tres techniques de travail du
! sol des plantes d'ipl > 1 � �tre �gaux a ceux de la plante
! d'ipl = 1 ...
subroutine testvartech(P_nbplantes,itk)

USE Itineraire_Technique

implicit none

    integer,    intent(IN)    :: P_nbplantes  !> // PARAMETER // number of simulated plants // SD // P_USM/USMXML // 0 
    type(ITK_), intent(INOUT) :: itk(P_nbplantes)  

    integer :: i  

!--        call EnvoyerMsgHistorique('les param�tres P_jultrav, P_proftrav,P_qres &
!                                    &et P_CsurNres ne sont pas le memes pour &
!                                    &les deux cultures les param�tres de la&
!                                    & culture associ�e seront ignor�s')

      do i = 2,P_nbplantes
        itk(i)%P_jultrav(:)  = itk(1)%P_jultrav(:)
! DR 01/02/2011 on ajoute P_julres
        itk(i)%P_julres(:)  = itk(1)%P_julres(:)
        itk(i)%P_proftrav(:) = itk(1)%P_proftrav(:)
        itk(i)%P_profres(:)  = itk(1)%P_profres(:)
        itk(i)%P_qres(:)     = itk(1)%P_qres(:)
        itk(i)%P_CsurNres(:) = itk(1)%P_CsurNres(:)
        itk(i)%P_coderes(:)  = itk(1)%P_coderes(:)
        itk(i)%P_Nminres(:)  = itk(1)%P_Nminres(:)
        itk(i)%P_eaures(:)   = itk(1)%P_eaures(:)
        itk(i)%P_Crespc(:)   = itk(1)%P_Crespc(:)
! DR 01/02/2011 on a scind� naps en P_nbjres et P_nbjtrav
!        itk(i)%napS        = itk(1)%napS
        itk(i)%P_nbjres       = itk(1)%P_nbjres
        itk(i)%P_nbjtrav      = itk(1)%P_nbjtrav

    ! domi 22/05/200 forcage variables techniques plante
    ! principale vers plantes associ�es
    ! rajouter message(??)
        itk(i)%P_codlocirrig  = itk(1)%P_codlocirrig
        itk(i)%P_codecalirrig = itk(1)%P_codecalirrig
        itk(i)%P_codlocferti  = itk(1)%P_codlocferti
        itk(i)%P_locirrig     = itk(1)%P_locirrig
        itk(i)%P_locferti     = itk(1)%P_locferti
        itk(i)%P_codepaillage = itk(1)%P_codepaillage
      end do

return
end subroutine testvartech
 
 
