! calcul du nb de jours dans l'annee
!> calculating the number of days in the year
integer FUNCTION nbjan(nan)

! Argument(s)
    integer, intent(IN) :: nan  

! Variable(s) locale(s)
    integer :: months(12)  

! le nombre de jour des mois de l'année
    data months/31,28,31,30,31,30,31,31,30,31,30,31/

    ! on vérifie si l'année est bissextile (divisible par 4 mais pas par 100 ou divisible par 400)
      if ( (int(nan/4) * 4 == nan .and. int(nan/100) * 100 /= nan) .or. ((int(nan/400) * 400) == nan) ) then
        months(2) = 29
      endif

      nbjan = sum(months)
!
return
end function nbjan
 
 
