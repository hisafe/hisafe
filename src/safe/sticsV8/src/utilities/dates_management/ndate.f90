!
!  *****************************************************************************
!
!    PARAMETRES EN ENTREE :
!   ***********************
!
!        JOUR : JOUR DU MOIS ( 0 < JOUR < 32 )
!
!        MOIS : MOIS DE L'ANNEE ( 0 < MOIS < 13 )
!
!        IAN : MILLESIME DE LA DATE ( NOMBRE DE 4 CHIFFRES )
!
!    PARAMETRE DE SORTIE :
!   ************************
!
!        NJ : N-IEME JOUR DE L'ANNEE ( 0 < NJ < 366 )
!
!  *****************************************************************************

subroutine NDATE (jour,mois,ian,nj)

implicit none

! Arguments
    integer, intent(IN)  :: ian  
    integer, intent(IN)  :: jour  
    integer, intent(IN)  :: mois  
    integer, intent(OUT) :: nj  

! Variables locales
    integer :: jo(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)  
    integer :: cmois  
    integer :: isBix  


    ! mois borné entre 1 et 12
        cmois = max(1,mois)
        cmois = min(12,cmois)


        isBix = ian - (4*IFIX(0.25*IAN)) + 1

    ! si année bissextile, février a 29 jours
        jo(2) = 28 ! forçage, juste au cas où
        if (isBix-1 == 0) jo(2) = 29

        nj = jour + sum(jo(1:cmois)) - jo(cmois)


return
end subroutine ndate
 
 
