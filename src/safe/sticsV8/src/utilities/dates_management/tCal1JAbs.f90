! Fonction de transposition d'une date/jour Stics
! dans le calendrier depuis le 1er janvier
! Version absolue - depuis le 1er janvier de l'ann�e de d�but de simulation
integer function tCal1JAbs(j,jDeb)
    implicit none

    integer, intent(IN) :: j ! la date Stics que l'on veut transposer  
    integer, intent(IN) :: jDeb ! le jour julien de d�but de la simulation  

    tCal1JAbs = j + jDeb - 1

return
end function tCal1JAbs
 
 
