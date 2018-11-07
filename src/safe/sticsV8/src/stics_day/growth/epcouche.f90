! *- sb le 26/03/07
!    cette fonction renvoie l'epaisseur de la couche iz :
!        * int(debut)-debut+1,   si iz = int(debut)
!        * fin-int(fin),         si iz = int(fin)
!        * 1 sinon
! 07/02/08
!  epcouche correspond à l'epaisseur de la mini couche colonisée par les racines
!  lorsqu'il s'agit de la premiere mini couche (apres la profondeur de semis) ou de la
! derniere minicouche (à hauteur de zrac) : epcouche peut etre < 1


real function epcouche(iz, debut, fin)

  implicit none

!: Arguments
  integer, intent(IN) :: iz  
  real,    intent(IN) :: debut  
  real,    intent(IN) :: fin  
      
      if (iz == int(debut)) then
         epcouche = int(debut) - debut + 1.
      else if (iz == int(fin)) then
         epcouche = fin - int(fin)
      else
         epcouche = 1.
      endif
      
return
end function epcouche 
 
