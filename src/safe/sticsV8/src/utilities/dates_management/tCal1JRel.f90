! Transposition du Calendrier depuis le 1er Janvier - version Relative à l'année en cours de simulation
integer function tCal1JRel(j,jAn,returnAn)

    USE Divers, only: nbjParAnnee

    implicit none

    integer, intent(IN)    :: j  
    integer, intent(INOUT) :: jAn  
    logical, intent(IN)    :: returnAn  

    integer :: anCours  

    anCours = jAn
    tCal1JRel = j

  ! on boucle tant que le jour julien calculé n'est pas inférieur à 365 (ou 366 si année bissextile)
  ! à chaque itération on enlève le nombre de jour de l'année correspondante
    do while( tCal1JRel > nbjParAnnee(anCours))
        tCal1JRel = tCal1JRel - nbjParAnnee(anCours)
        anCours = anCours + 1
    end do

    if (returnAn) jAn = anCours

end function tCal1JRel
 
 
