!****f* Lixivation/sommeCouchesParCellule
! NAME
!   sommeCouchesParCellule
! DESCRIPTION
!   Module : lixivation
!   Typologie : utilitaire
!
!   Fonction g�n�rique de calcul de sommes de sections d'un tableau.
!
!   Le tableau des couches du sol avec les valeurs � sommer est le param�tre <i>tabCouches</i>.
!   Le nombre de cellules de m�lange est donn�e par le param�tre <i>nbCellules</i>
!   Pour chaque cellules de m�lange, le nombre de couches appartenant � la cellule est donn�e par le param�tre <i>iCellules</i>
!
!   On fait la somme, pour chaque cellule, de la section du tableau <i>tabCouches</i> correspondant au nombre de couches de la cellule.
!   Le r�sultat pour chaque cellule est stock�e et renvoy�e dans un tableau de taille <i>nbCellules</i>.
!
! INPUTS
!     tabCouches : tableau de r�els - l'ensemble des couches du sol avec
!                  pour chaque couche la valeur � additionner associ�e (ou nulle)
!     nbCellules : entier - le nombre de cellules de m�lange
!     iCellules : tableau d'entiers - pour chaque cellule, le nombre de couches appartenant � la cellule
!
!
! RETURN
!     tableau de r�els - tableau unidimensionnel de taille <i>nbCellules</i>. Pour chaque cellule, le tableau
!                        renvoi la somme des valeurs  correspondantes aux couches du sol appartenant � la cellule.
!
!***
function sommeCouchesParCellule(nbCellules, iCellules, tabCouches)

implicit none

! ARGUMENTS
  integer, intent(in) :: nbCellules  
  integer, intent(in) :: iCellules(0:nbCellules)  
  real,    intent(in) :: tabCouches(iCellules(nbCellules))                           ! (:)  

! TABLEAU DE RETOUR
  real, dimension(nbCellules) :: sommeCouchesParCellule  

! VARIABLES LOCALES
  integer :: ii  

!      print *, 'sommeCouchesParCellule : size(icel)=',size(iCellules)
!      print *, 'sommeCouchesParCellule : icel=',iCellules

    ! TODO : il faudrait peut �tre v�rifier qu'il n'y a pas possibilit� de d�bordement de tableau
    ! il ne doit pas y avoir + de cellules que d'indices dans le tableau iCellules
      if (nbCellules > SIZE(iCellules)) then


      endif

    ! on v�rifie que la valeur de couche max. est < ou = au nombres de couches (la taille du tableau tabCouches)
      ii = MAXVAL(iCellules) ! MAXVAL(iCellules) = iCellules(nbCellules)

      if (ii <= SIZE(tabCouches)) then
        sommeCouchesParCellule(:) = 0. ! on initialise le tableau � z�ro
        do ii = 1,nbCellules
          sommeCouchesParCellule(ii) = SUM(tabCouches((iCellules(ii-1)+1):iCellules(ii)))
        end do
      else
        ! Ici on est en situation d'erreur, comment g�re-t-on cela ?
        !call EnvoyerMsgHistorique(580)
        ! write(*,*) 'SUM(iCel)=',ii,' nbCel=',nbCellules
        !write(*,*) 'SIZE(tabCouches)', SIZE(tabCouches)
        sommeCouchesParCellule(:) = -1.
      endif

end function
 
 
