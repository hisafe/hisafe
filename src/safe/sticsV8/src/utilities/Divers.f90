!> Module of the interface of utilities
!! Description : contains climatic, dates, astronomical and geometric functions
!!
!<
module Divers

! - LES INTERFACES -
interface

!======================================================================================!
!> calculation of the macroporosity
real function macroporosite(P_codefente,da,hcc,hmin,P_epc)
  ! les paramètres entrants
  integer, intent(IN) :: P_codefente  !> // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0 
  real,    intent(IN) :: da   !> // OUTPUT // Bulk density in the horizon 1 // g cm-3
  real,    intent(IN) :: hcc  
  real,    intent(IN) :: hmin  
  real,    intent(IN) :: P_epc  !> // PARAMETER // Thickness of the horizon H   (table) // cm   // PARSOL // 1 	  // OUTPUT // Thickness of the horizon 2 // cm  
end function macroporosite

!======================================================================================!
real function calcul_UDev(temp,P_tdmax,P_tdmin,P_tcxstop)
  real, intent(IN) :: temp  
  real, intent(IN) :: P_tdmax  !> // PARAMETER // Maximum threshold temperature for development // degree C // PARPLT // 1
  real, intent(IN) :: P_tdmin  !> // PARAMETER // Minimum threshold temperature for development // degree C // PARPLT // 1
  real, intent(IN) :: P_tcxstop  !> // PARAMETER // threshold temperature beyond which the foliar growth stops // degree C // PARPLT // 1
end function calcul_UDev

!======================================================================================!
!> calculating the hourly temperatures
function calcul_TemperaturesHoraires(tmin,tmin_demain,tmax)
  real, intent(IN)               :: tmin            !> arg. Température minimum  	  // OUTPUT // Minimum active temperature of air // degree C
  real, intent(IN)               :: tmin_demain     !> arg. Température minimum du lendemain  
  real, intent(IN)               :: tmax            !> arg. Température maximum  	  // OUTPUT // Maximum active temperature of air // degree C
  real, dimension(24)            :: calcul_TemperaturesHoraires ! variable de retour  
end function calcul_TemperaturesHoraires

!======================================================================================!
!> calculating the hourly temperatures
real function calcul_GDH(thor,P_tdmin,P_tdmax)

  real, intent(IN), dimension(24) :: thor  ! arg. Temperatures Horaires sur 24 heures  
  real, intent(IN)                :: P_tdmin ! arg. Température de développement minimum 	  // PARAMETER // Minimum threshold temperature for development // degree C // PARPLT // 1
  real, intent(IN)                :: P_tdmax ! arg. Température de développement maximum 	  // PARAMETER // Maximum threshold temperature for development // degree C // PARPLT // 1

end function calcul_GDH

!======================================================================================!
!> slowly effect of the photoperiod on plant development
real function cRFPI(P_sensiphot,P_phosat,P_phobase,phoi)
  real, intent(IN) :: P_sensiphot  !> // PARAMETER //  photoperiod sensitivity (1=insensitive) // SD // PARPLT // 1 
  real, intent(IN) :: P_phosat  !> // PARAMETER // saturating photoperiod // hours // PARPLT // 1 
  real, intent(IN) :: P_phobase  !> // PARAMETER // Base photoperiod  // hours // PARPLT // 1 
  real, intent(IN) :: phoi      !> // OUTPUT // Photoperiod // hours
end function cRFPI

!======================================================================================!

real function humpotsol(P_psihucc,P_psihumin,humin,hucc,dacouche,psiref,P_codefente)
!: ARGUMENTS
  real,    intent(IN) :: P_psihucc  !> // PARAMETER // soil potential corresponding to field capacity  // Mpa // PARAM // 1 
  real,    intent(IN) :: P_psihumin  !> // PARAMETER // soil potential corresponding to wilting point // Mpa // PARAM // 1 
  real,    intent(IN) :: hucc  
  real,    intent(IN) :: humin  
  real,    intent(IN) :: dacouche  
  real,    intent(IN) :: psiref  
  integer, intent(IN) :: P_codefente  !> // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0 
end function humpotsol


!======================================================================================!

!>introduction of a continuous function between CC and pf4.2 to constrain the germination and emergence
real function F_humirac(h,hmin,hmax,P_sensrsec)

  real, intent(IN) :: h  
  real, intent(IN) :: hmin  
  real, intent(IN) :: hmax      !> // OUTPUT // Maximum height of water table between drains // cm
  real, intent(IN) :: P_sensrsec  !> // PARAMETER // root sensitivity to drought (1=insensitive) // SD // PARPLT // 1 

end function F_humirac


!======================================================================================!
!> calculation of extraterrestrial radiation in MJ/m2/jour
!! source :Saumane, 1993
real function RGEX(lat,t)

!: Arguments
  real,    intent(IN) :: lat  !> P_latitude  
  integer, intent(IN) :: t    !> jour julien  

end function RGEX

!======================================================================================!
!------------------------!
!  fonction déclinaison  !
!------------------------!
real function decangle(j)

  integer, intent(IN) :: j    ! j est le jour julien  

end function decangle

!======================================================================================!
!>  Calculates the linear function has the following threshold
!!
!> - x1, x2, y1, y2 are the parameters of the function
!> - Escalin is the value of the function for the value of the variable x
!!    y1 .. --------
!!               .\
!!           . \
!!           .  \
!!           .   \
!!           .    \
!!             .     \
!!           .      \
!!           .       \
!!           .        \
!!    y2  ...................\-----------------
!!             .         .
!!           .       .
!!           .       .
!!           .       .
!!           x1         x2
!!
real function escalin(x,x1,x2,y1,y2)

  real, intent(IN) :: x  
  real, intent(IN) :: x1  
  real, intent(IN) :: y1  
  real, intent(IN) :: x2  
  real, intent(IN) :: y2  

end function escalin


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

end function GEL

!======================================================================================!
!>Function of calculating the saturation vapor pressure at temperature T
real function TVAR(T)
  implicit none
!: Arguments
  real, intent(IN) :: T  
end function TVAR


!======================================================================================!
!>Function for calculating the dew point temperature,  at pressure e
real function F_temprosee(e)
  implicit none
!: Arguments
  real, intent(IN) :: e  
end function F_temprosee

!======================================================================================!
!>Function for calculating the Julian day back to calendar
!! January 1 and for the current year.
!! The result is always between 1 and 365 (or 366 if leap is true).
!integer function jourJulienDepuis1erJanvier(j,bissextile)
!    implicit none
!: Arguments
!    integer, intent(IN) :: j ! le jour
!    logical, intent(IN) :: bissextile ! année bissextile ?
!end function jourJulienDepuis1erJanvier

!======================================================================================!
!> Function that returns. TRUE. if a is a leap year. FALSE. otherwise
logical function isBissextile(a)
    implicit none
    integer, intent(IN) :: a ! année dont on veut savoir si elle est bissextile  
end function isBissextile

!======================================================================================!
!> calculating the number of days in the year
integer function nbjParAnnee(annee)
    implicit none
    integer, intent(IN) :: annee  
end function nbjParAnnee

!======================================================================================!
!>function to transpose a date / day Stics
!! in the calendar since 1 January
!! Absolute version - since 1 January of the year of start of simulationinteger function tCal1JAbs(j,jDeb)
integer function tCal1JAbs(j,jdeb)
    implicit none
    integer, intent(IN) :: j ! la date Stics que l'on veut transposer  
    integer, intent(IN) :: jDeb ! le jour julien de début de la simulation  
end function tCal1JAbs

!======================================================================================!
!> Calendar of transposition since 1 January - Relating to release this year of simulation
integer function tCal1JRel(j,jAn,returnAn)
    implicit none
    integer, intent(IN)    :: j  
    integer, intent(INOUT) :: jAn  
    logical, intent(IN)    :: returnAn  
end function tCal1JRel

!======================================================================================!
!> calculation of the date format (year, month, day) from a day in the year
subroutine dates(j,jDeb,jAn,jt,anCours,mois,jour,numMois)
  implicit none
!: Arguments
  integer,          intent(IN)    :: j  
  integer,          intent(IN)    :: jDeb  
  integer,          intent(IN)    :: jAn  
  integer,          intent(INOUT) :: jt         ! jour transposé  
  integer,          intent(INOUT) :: anCours  
  character(len=3), intent(INOUT) :: mois  
  integer,          intent(INOUT) :: jour  
  integer,          intent(INOUT) :: nummois  
end subroutine dates

!======================================================================================!
!> subroutine for calculating the annual average temperature for decision
!! account of climate change on organic matter
real FUNCTION tmoy_histo(andeb,anfin,tmoy_an)
implicit none
!: Arguments
    integer, intent(IN) :: andeb  
    integer, intent(IN) :: anfin  
    real,    intent(IN) :: tmoy_an(2,300)  
 end function tmoy_histo

!======================================================================================!
end interface

end module Divers
 
 
