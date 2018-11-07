!********************************************************************************
!  Calcule la fonction lineaire a seuil suivante
!
!     x1,x2,y1,y2 sont les parametres de la fonction
!     escalin est la valeur de la fonction pour la valeur de la variable x
! 
!
!    y1 .. --------
!               .\
!           . \     
!           .  \  
!           .   \
!           .    \
!             .     \
!           .      \
!           .       \
!           .        \
!    y2  ...................\-----------------
!             .         .
!           .       .
!           .       .
!           .       .
!           x1         x2
!               
!********************************************************************************

real function escalin(x,x1,x2,y1,y2)

  implicit none
  
!: Arguments
  real, intent(IN) :: x  
  real, intent(IN) :: x1  
  real, intent(IN) :: y1  
  real, intent(IN) :: x2  
  real, intent(IN) :: y2  
  
!: Variables locales
  real :: dx  
  real :: pente  


      if (x <= x1) then
        escalin = y1
        return
      endif
      
      if (x >= x2) then
        escalin = y2
        return
      endif
   
      dx = x2 - x1
      if (dx == 0.) dx = 1.e-10
      pente = (y2 - y1) / dx
      escalin = y1 + (pente * (x - x1))
      
return
end function escalin 
 
