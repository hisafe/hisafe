!======================================================================================!
!> subroutine for calculating the annual average temperature for decision
!! account of climate change on organic matter
real FUNCTION tmoy_histo(andeb,anfin,tmoy_an)

implicit none

    integer, intent(IN) :: andeb  
    integer, intent(IN) :: anfin  
    real,    intent(IN) :: tmoy_an(2,300)  

    integer :: l  

! calcul de la temperature moyenne historique sur la periode lue dans le paramv6.par
!  P_an_debut_serie_histo-P_an_fin_serie_histo

! l'annee est dans      tmoy_an(1,l)
! la temp moy de l'annee dans   tmoy_an(2,l)

    tmoy_histo = 0.

    do l = 1,200
      if (tmoy_an(1,l) >= andeb .and. tmoy_an(1,l) <= anfin) then
        tmoy_histo = tmoy_histo + tmoy_an(2,l)
      endif
    enddo

    tmoy_histo= tmoy_histo / (anfin-andeb+1)

return
end function tmoy_histo
 
 
