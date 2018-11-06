! lecture des variables du fichier *.st2
!> Read variables daily file (mod_s*.sti)
!! We will read var.mod the list of variables which
!! one wishes to have the values ​​written to the file mod_s*.sti
subroutine Lecture_VariablesSortiesJournalieres(sc)

USE Stics

implicit none

    type(Stics_Communs_), intent(INOUT) :: sc  

! Variables locales
    integer :: ii  !>  
    integer ::  eof  

    ! DR - 21/07/08 - maintenant on a seulement un sti qu'on lit dans var.mod
        open(33,file='var.mod')

        sc%nbvarsortie = 0
        eof = 0
        ii = 0
        !!!MODIF HISAFE 1 : suppression des chaines de caract�res
        !!!sc%valpar(:) = ''   ! pour faire fonctionner le test sur la longueur de chaine plus bas,
                            ! sinon la chaine est remplie de caractères de terminaison par défaut
                            ! et donc retourne une longueur de 19 (puisque valpar est un tableau de chaines de 19 car.)

        !!!do
        !!!    ii = ii + 1
        !!!    read(33,'(a25)',iostat=eof) sc%valpar(ii)
        !!!    if (eof /= 0) EXIT
        !!!end do

      ! si on a terminé sur une ligne vide, on retranche 1
        !!!if (len(trim(sc%valpar(ii))) > 0) then
        !!!  sc%nbvarsortie = ii
        !!!else
        !!!  sc%nbvarsortie = ii - 1
        !!!endif

    ! on ferme le fichier
        close(33)

return
end subroutine Lecture_VariablesSortiesJournalieres
 
 
