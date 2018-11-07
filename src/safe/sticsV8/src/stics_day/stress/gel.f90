! ****************************************************************
!     Calcul du stress lié au GEL, en 3 segments de droite :
!     
!   1) entre P_tdebgel (0 degrees C) et tgel10 : GEL varie de 1.0 à 0.9
!   2) entre tgel10 et tgel90        : GEL varie de 0.9 à 0.1
!   3) entre tgel90 et P_tletale       : GEL varie de 0.1 à 0.0
! ****************************************************************
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!! This function calculates the frost index according to the temperatures
!> - tdebgel, corresponding to the beginning of frost damage
!> - tgel10, corresponding to 10 % of frost damages
!> - tgel90, corresponding to 90 % of frost damages
!> - tletale, corresponding to the lethal temperature for the plant
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
real function GEL(codegel,t,P_tletale,P_tdebgel,tgel90,tgel10)

  implicit none
  
!: Arguments
  integer, intent(IN) :: codegel  
  real,    intent(IN) :: t  
  real,    intent(IN) :: P_tletale  !> // PARAMETER // lethal temperature for the plant // degree C // PARPLT // 1
  real,    intent(IN) :: P_tdebgel  !> // PARAMETER // temperature of frost beginning // degree C // PARPLT // 1
  real,    intent(IN) :: tgel10  
  real,    intent(IN) :: tgel90  

!: Variables locales
  real :: a  !>  
  real :: b  
      
      !: Pas de stress si codegel = 1
      if (codegel == 1) then
        GEL = 1.
        return
      endif

      !: Si t > P_tdebgel : pas de GEL
      if (t >= P_tdebgel) then
        GEL = 1.
        return
      endif
      
      !: De P_tdebgel à tgel10
      if (t < P_tdebgel .and. t >= tgel10 .and. tgel10 < P_tdebgel) then
        a = (0.9 - 1.0) / (tgel10 - P_tdebgel)
        b = 1.0 - (a * P_tdebgel)
        GEL = (a * t) + b
        return
      endif

      !: De tgel10 à tgel90
      if (t < tgel10 .and. t >= tgel90 .and. tgel90 < tgel10) then
        a = (0.9 - 0.1) / (tgel10 - tgel90)
        b = 0.9 - (a * tgel10)
        GEL = (a * t) + b
        return
      endif
      
      !: De tgel90 à P_tletale
      if (t < tgel90 .and. t >= P_tletale .and. P_tletale < tgel90) then
        a = (0. - 0.1) / (P_tletale - tgel90)
        b = 0. - (a * P_tletale)
        GEL = (a * t) + b
        return
      endif
      
      !: En dessous de P_tletale
      if (t < P_tletale) then
        GEL = 0.
        return
      endif
      
return
end function GEL 
 
