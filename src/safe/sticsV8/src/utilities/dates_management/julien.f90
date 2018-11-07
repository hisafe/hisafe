!*********************************************************************
! sous programme de calcul de la date sous la forme jour/mois/an
!*********************************************************************
!> subroutine to calculate the date in the forme day / month / year
subroutine julien(jul,nan,rmon,nday,nummois)

  implicit none
  
!: Arguments

  integer,          intent(IN)    :: jul  
  integer,          intent(IN)    :: nan  

  character(len=3), intent(INOUT) :: rmon  
  integer,          intent(INOUT) :: nday  
  integer,          intent(INOUT) :: nummois    

!: Variables locales 
  character(len=3) :: rname(12)  
  integer :: jcount  !>  
  integer :: mon(12),ndif,nsum  

      mon = (/31,28,31,30,31,30,31,31,30,31,30,31/) ! on initialise le tableau SANS data qui
                                                    ! sinon sauvegarde l'�tat pr�c�dent,
                                                    ! ce qui peut poser pb pour le mois de f�vrier

      rname = (/'jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'/)
     
      ! ann�e bissextile ? attention, formule pas tout � fait exact, il faut pas que �a soit divisible par 100
      if ((nan / 4) * 4  ==  nan) then
        mon(2) = 29
      else
        mon(2) = 28 ! on force l'affectation, au cas o� (bug o� mon(2) est d�j� �gal � 29,
                    ! r�solu en virant le data, 2 pr�cautions valent mieux qu'une
      endif
      
      nsum = 0
B1:   do jcount = 1,12
        ndif = jul - nsum
        if (ndif <= mon(jcount)) then
          nday = ndif
          rmon = rname(jcount)
          nummois = jcount
          EXIT B1
        endif
        nsum = nsum+mon(jcount)
      end do B1
      
return
end subroutine julien
 
 
