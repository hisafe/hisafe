! Transposition du Calendrier depuis le 1er Janvier - version Relative � l'ann�e en cours de simulation
integer function tCal1JRel(j,jAn,returnAn)

    USE Divers, only: nbjParAnnee

    implicit none

    integer, intent(IN)    :: j  
    integer, intent(INOUT) :: jAn  
    logical, intent(IN)    :: returnAn  

    integer :: anCours  

    anCours = jAn
    tCal1JRel = j

  ! on boucle tant que le jour julien calcul� n'est pas inf�rieur � 365 (ou 366 si ann�e bissextile)
  ! � chaque it�ration on enl�ve le nombre de jour de l'ann�e correspondante
    do while( tCal1JRel > nbjParAnnee(anCours))
        tCal1JRel = tCal1JRel - nbjParAnnee(anCours)
        anCours = anCours + 1
    end do

    if (returnAn) jAn = anCours

end function tCal1JRel
 
 
