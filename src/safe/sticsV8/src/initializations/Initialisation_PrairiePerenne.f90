!
! On initialise les variables fauche pour l'annee de semis
! (on saute la premiere coupe de regain d'octobre)
! et pour les autres annees

subroutine Initialisation_PrairiePerenne(sc,itk,ipl,stade0)

USE Stics
USE Plante
USE Itineraire_Technique

    implicit none

    type(Stics_Communs_),       intent(INOUT) :: sc  

    type(ITK_),                 intent(INOUT) :: itk  

    integer,                    intent(INOUT) :: ipl  

    integer :: k  

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!    character*3 :: stade0
    integer :: stade0

! DR 26/02/08 pour climator la premiere annee on zappe la premiere coupe
!             qui est la coupe de regain qui ne se fait pas en annee de semis

    sc%ipl = ipl

      !!!if(stade0.eq.'snu')then
      if(stade0 == 1) then
    ! DR 26/02/08 annee de semis
      !! dr et fr 13/02/2015
      !si on est en vrai semis 'snu'
        sc%nbcoupe_an1(sc%ipl) = itk%nbcoupe - 1
      else
        sc%nbcoupe_an1(sc%ipl) = itk%nbcoupe
     endif
     sc%nbcoupe_an2(sc%ipl) = itk%nbcoupe
  ! nbcoupe est le nombre de coupe de l'annee 2 lu



    do k = 1, sc%nbcoupe_an1(sc%ipl)
    ! ANNEE 1
    !=======================
    !!!  if(stade0.eq.'snu')then
      if(stade0 == 1)then
    ! DR 26/02/08 annee de semis
      !! dr et fr 13/02/2015
      !si on est en vrai semis 'snu'
        if(itk%P_codemodfauche == 3) then
          sc%tempfauche_an1(sc%ipl,k) = itk%P_tempfauche(k+1)
        endif
        if(itk%P_codemodfauche == 2) then
          sc%julfauche_an1(sc%ipl,k) = itk%P_julfauche(k+1)
        endif
        sc%hautcoupe_an1(sc%ipl,k)   = itk%P_hautcoupe(k+1)
        sc%lairesiduel_an1(sc%ipl,k) = itk%P_lairesiduel(k+1)
        sc%msresiduel_an1(sc%ipl,k)  = itk%P_msresiduel(k+1)
        sc%anitcoupe_an1(sc%ipl,k)   = itk%P_anitcoupe(k+1)
        sc%restit_an1(sc%ipl,k)  = itk%P_restit(k+1)
        sc%mscoupemini_an1(sc%ipl,k)   = itk%P_mscoupemini(k+1)

        if (itk%P_codemodfauche == 3) then
          if (k > 1) then
           sc%tempfauche_ancours_an1(sc%ipl,k) = sc%tempfauche_an1(sc%ipl,k)  &
                                               + sc%tempfauche_ancours_an1(sc%ipl,k-1)
          else
           sc%tempfauche_ancours_an1(sc%ipl,k) = sc%tempfauche_an1(sc%ipl,k)
          endif
        endif
      else
     ! annee 1 culture en place
     !! dr et fr 13/02/2015
     ! l'annee 1 n'est pas une annee de semis
        if(itk%P_codemodfauche == 3) then
          sc%tempfauche_an1(sc%ipl,k) = itk%P_tempfauche(k)
        endif

        if(itk%P_codemodfauche == 2) then
          sc%julfauche_an1(sc%ipl,k) = itk%P_julfauche(k)
        endif
        sc%hautcoupe_an1(sc%ipl,k)   = itk%P_hautcoupe(k)
        sc%lairesiduel_an1(sc%ipl,k) = itk%P_lairesiduel(k)
        sc%msresiduel_an1(sc%ipl,k)  = itk%P_msresiduel(k)
        sc%anitcoupe_an1(sc%ipl,k)   = itk%P_anitcoupe(k)
        sc%restit_an1(sc%ipl,k)  = itk%P_restit(k)
        sc%mscoupemini_an1(sc%ipl,k)   = itk%P_mscoupemini(k)

       if (itk%P_codemodfauche == 3) then
          if (k > 1) then
           sc%tempfauche_ancours_an1(sc%ipl,k) = sc%tempfauche_an1(sc%ipl,k)  &
                                               + sc%tempfauche_ancours_an1(sc%ipl,k-1)
          else
           sc%tempfauche_ancours_an1(sc%ipl,k) = sc%tempfauche_an1(sc%ipl,k)
          endif
        endif
      endif
    enddo
    ! les ANNEES suivantes
      do k = 1,sc%nbcoupe_an2(sc%ipl)
        if(itk%P_codemodfauche == 3) then
          sc%tempfauche_an2(sc%ipl,k)= itk%P_tempfauche(k)
        endif

        if(itk%P_codemodfauche == 2) then
          sc%julfauche_an2(sc%ipl,k)= itk%P_julfauche(k)
        endif
        sc%hautcoupe_an2(sc%ipl,k)   = itk%P_hautcoupe(k)
        sc%lairesiduel_an2(sc%ipl,k) = itk%P_lairesiduel(k)
        sc%msresiduel_an2(sc%ipl,k)  = itk%P_msresiduel(k)
        sc%anitcoupe_an2(sc%ipl,k)   = itk%P_anitcoupe(k)
        sc%restit_an2(sc%ipl,k)  = itk%P_restit(k)
        sc%mscoupemini_an2(sc%ipl,k)   = itk%P_mscoupemini(k)


        if(itk%P_codemodfauche == 3) then
          if(k > 1) then
           sc%tempfauche_ancours_an2(sc%ipl,k) = sc%tempfauche_an2(sc%ipl,k) &
                                               + sc%tempfauche_ancours_an2(sc%ipl,k-1)
          else
           sc%tempfauche_ancours_an2(sc%ipl,k) = sc%tempfauche_an2(sc%ipl,k)
          endif
        endif
      enddo


return
end subroutine Initialisation_PrairiePerenne
 
 
