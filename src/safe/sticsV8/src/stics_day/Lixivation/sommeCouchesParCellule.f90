!****f* Lixivation/sommeCouchesParCellule
! NAME
!   sommeCouchesParCellule
! DESCRIPTION
!   Module : lixivation
!   Typologie : utilitaire
!
!   Fonction générique de calcul de sommes de sections d'un tableau.
!
!   Le tableau des couches du sol avec les valeurs à sommer est le paramètre <i>tabCouches</i>.
!   Le nombre de cellules de mélange est donnée par le paramètre <i>nbCellules</i>
!   Pour chaque cellules de mélange, le nombre de couches appartenant à la cellule est donnée par le paramètre <i>iCellules</i>
!
!   On fait la somme, pour chaque cellule, de la section du tableau <i>tabCouches</i> correspondant au nombre de couches de la cellule.
!   Le résultat pour chaque cellule est stockée et renvoyée dans un tableau de taille <i>nbCellules</i>.
!
! INPUTS
!     tabCouches : tableau de réels - l'ensemble des couches du sol avec
!                  pour chaque couche la valeur à additionner associée (ou nulle)
!     nbCellules : entier - le nombre de cellules de mélange
!     iCellules : tableau d'entiers - pour chaque cellule, le nombre de couches appartenant à la cellule
!
!
! RETURN
!     tableau de réels - tableau unidimensionnel de taille <i>nbCellules</i>. Pour chaque cellule, le tableau
!                        renvoi la somme des valeurs  correspondantes aux couches du sol appartenant à la cellule.
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

    ! TODO : il faudrait peut être vérifier qu'il n'y a pas possibilité de débordement de tableau
    ! il ne doit pas y avoir + de cellules que d'indices dans le tableau iCellules
      if (nbCellules > SIZE(iCellules)) then


      endif

    ! on vérifie que la valeur de couche max. est < ou = au nombres de couches (la taille du tableau tabCouches)
      ii = MAXVAL(iCellules) ! MAXVAL(iCellules) = iCellules(nbCellules)

      if (ii <= SIZE(tabCouches)) then
        sommeCouchesParCellule(:) = 0. ! on initialise le tableau à zéro
        do ii = 1,nbCellules
          sommeCouchesParCellule(ii) = SUM(tabCouches((iCellules(ii-1)+1):iCellules(ii)))
        end do
      else
        ! Ici on est en situation d'erreur, comment gère-t-on cela ?
        !call EnvoyerMsgHistorique(580)
        ! write(*,*) 'SUM(iCel)=',ii,' nbCel=',nbCellules
        !write(*,*) 'SIZE(tabCouches)', SIZE(tabCouches)
        sommeCouchesParCellule(:) = -1.
      endif

end function
 
 
