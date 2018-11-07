!======================================================================================!
!> Function that returns. TRUE. if a is a leap year. FALSE. otherwise
logical function isBissextile(a)

    implicit none

    integer, intent(IN) :: a ! année dont on veut savoir si elle est bissextile  

	! Une année bissextile est (divisible par 4 mais pas par 100) ou (divisible par 400)

    isBissextile = ((mod(a,4) == 0) .and. (mod(a,100) /= 0)) .or. (mod(a,400) == 0)

return
end function isBissextile
 
 
