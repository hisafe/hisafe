! Lecture des variables du fichier Rapport (rapport.sti)
! On va lire dans rap.mod la liste des variables dont
! on souhaite avoir les valeurs écrites dans le fichier rapport.sti
! D'abord on récupérer les dates et/ou les stades pour lesquels
! on veut ces valeurs, puis on lit le nom des variables souhaitées.
!> Read variables File Report (rapport.sti)
!! We will read rap.mod the list of variables which
!! one wishes to have the values ​​written to the file rapport.sti
!!
!! First we get the dates and / or stages for which
!! we want these values​​, then we read the name of the desired variables.
subroutine Lecture_VariablesRapport(sc)

    USE Stics

    implicit none

    type(Stics_Communs_), intent(INOUT) :: sc  


    integer :: ii  

! 17/02/2010 on ajoute la declaration de la date sous sa nouvelle forme
    integer :: jourdebutprof  !>  
    integer :: moisdebutprof  !>  
    integer :: andebutprof  





return
end subroutine Lecture_VariablesRapport
 
 
