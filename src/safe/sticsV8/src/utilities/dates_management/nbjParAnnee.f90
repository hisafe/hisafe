!======================================================================================!
!> calculating the number of days in the year
integer function nbjParAnnee(annee)

    USE Divers, only: isBissextile

    implicit none

    integer, intent(IN) :: annee  

    if (isBissextile(annee)) then
        nbjParAnnee = 366
    else
        nbjParAnnee = 365
    endif

return
end function nbjParAnnee
 
 
