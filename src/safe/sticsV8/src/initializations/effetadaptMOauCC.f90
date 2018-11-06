
! *************************************************************************************
! DR 19/10/09
! sous-programme de recalcule des parametres quand on veut faire jouer l'effet de l'adpptation des
! Matières Organiques  au Changt clim.
! l'effet joue sur les parametres :
!     P_trefh et P_trefr pour la mineralisation
!     P_tnitmin tnitmat et P_tnitopt pour la nitrification
!     P_trefdenit1 et P_trefdenit2 pour la denitrification
! *************************************************************************************


subroutine effetadaptMOauCC(sc,t,pg)

USE Stics
USE Parametres_Generaux
USE Sol

implicit none

! Arguments
    type(Stics_Communs_),       intent(INOUT) :: sc  
    type(Stics_Transit_),       intent(IN)    :: t  
    type(Parametres_Generaux_), intent(IN)    :: pg  

! DR 21/09/09 on mets en commentaire en attendant la validation de BM
! **********************
! DR 23/11/07 adaptation des MO à l'effet du CC
! certains parametres sont augmentés du deltat
! on les a appelé autrement car c'est des param qui deviennent "variables"

    if (t%P_code_adaptCC_miner == 1) then
      sc%var_trefh(sc%numcult) = pg%P_trefh + sc%deltaT_adaptCC(sc%numcult)
      sc%var_trefr(sc%numcult) = pg%P_trefr + sc%deltaT_adaptCC(sc%numcult)
    else
      sc%var_trefh(sc%numcult) = pg%P_trefh
      sc%var_trefr(sc%numcult) = pg%P_trefr
    endif

    if (t%P_code_adaptCC_nit == 1) then
      sc%var_tnitmin(sc%numcult) = pg%P_tnitmin + sc%deltaT_adaptCC(sc%numcult)
      sc%var_tnitmax(sc%numcult) = pg%P_tnitmax + sc%deltaT_adaptCC(sc%numcult)
      sc%var_tnitopt(sc%numcult) = pg%P_tnitopt + sc%deltaT_adaptCC(sc%numcult)
      sc%var_tnitopt2(sc%numcult) = pg%P_tnitopt2 + sc%deltaT_adaptCC(sc%numcult)
    else
      sc%var_tnitmin(sc%numcult) = pg%P_tnitmin
      sc%var_tnitmax(sc%numcult) = pg%P_tnitmax
      sc%var_tnitopt(sc%numcult) = pg%P_tnitopt
      sc%var_tnitopt2(sc%numcult) = pg%P_tnitopt2
    endif

    if (t%P_code_adaptCC_denit == 1) then
      sc%var_TREFdenit1(sc%numcult) = t%P_TREFdenit1 + sc%deltaT_adaptCC(sc%numcult)
      sc%var_TREFdenit2(sc%numcult) = t%P_TREFdenit2 + sc%deltaT_adaptCC(sc%numcult)
    else
      sc%var_TREFdenit1(sc%numcult) = t%P_TREFdenit1
      sc%var_TREFdenit2(sc%numcult) = t%P_TREFdenit2
    endif

return
end subroutine effetadaptMOauCC
 
 
