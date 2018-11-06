subroutine init_prairie_avec_enchainementdescoupes(sc,p,itk,pg)

USE Stics
USE Plante
USE Itineraire_Technique
USE Parametres_Generaux
USE Messages
USE Divers, only: isBissextile

implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc
  type(Parametres_Generaux_), intent(INOUT) :: pg
  type(Plante_),              intent(INOUT) :: p (sc%P_nbplantes)
  type(ITK_),                 intent(INOUT) :: itk (sc%P_nbplantes)

! Variable(s) locale(s)
      integer :: i  !>
      integer :: ipl  !>
      integer :: k  !>
      character(len=500) :: tmp ! pour la consitution de messages complexes pour le fichier historique.
      real    :: difftemp(2)
      real :: som_temp_manquante


  do ipl = 1, sc%P_nbplantes
! == = == = == = == = == = == =  PRAIRIES CLIMATOR

    sc%onafaitunecoupedelanneedavant=.FALSE.
    write(619,*)'numcult',sc%numcult
    write(619,*)'**********'
      ! **** 22/06/2015 on est pas en prairies semees

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!    if ((p(ipl)%P_stade0 .ne. 'snu' .and. p(ipl)%P_stade0 .ne. 'plt')) then
    if ((p(ipl)%P_stade0 /= 1 .and. p(ipl)%P_stade0 /= 2)) then
        ! ** initialisations pour les cultures fauchées
        ! DR 07/11/05 on initialise faucheannule   ! DR 13/01/06 on initialise onarretesomcourdrp
          sc%faucheannule = 0
          p(ipl)%onarretesomcourdrp = .FALSE.
      if (itk(ipl)%P_codefauche == 1 .and. itk(ipl)%lecfauche ) then
          ! DR 26/02/08 on teste dans l'enchainement de perenne paririe de garder le reliquat
          !   de somcourfauche depuis la derniere coupe de l'annee d'avant
            if (p(ipl)%P_codeperenne == 2) then
            ! DR et FR il faut qu'on ajoute un cas annee 1 car dans le cas ou on est en somme de temp , on saute l'annee 1 car tempfauche(1) devient = 0
            ! **** cas annee 1 ****
            ! *********************
              if (sc%numcult==1) then
              write(619,*)'cas annee1'
              ! 19/02/2015 DR et FR on ne fait la boucle que sur les nbdecoupe
              !  do i = 1,20
                do i = 1,itk(ipl)%nbcoupe
                  if (itk(ipl)%P_codemodfauche == 3) then
                    p(ipl)%tempfauche_ancours(i) = sc%tempfauche_ancours_an1(ipl,i)
                    itk(ipl)%P_tempfauche(1)=p(ipl)%tempfauche_ancours(1)
                  endif
                  ! 16/02/2015 on teste le semis avec des coupes a des dates
                  if (itk(ipl)%P_codemodfauche == 2) then
                    itk(ipl)%P_julfauche(i) = sc%julfauche_an1(ipl,i)
                  endif
                  ! DR et Fr 22/06/2015 il ne faut pas oublier de recuperer les hauteur, msresiduel et le reste ...
                  itk(ipl)%P_hautcoupe(i) = sc%hautcoupe_an1(ipl,i)
                  itk(ipl)%P_lairesiduel(i) = sc%lairesiduel_an1(ipl,i)
                  itk(ipl)%P_msresiduel(i) = sc%msresiduel_an1(ipl,i)
                  itk(ipl)%P_anitcoupe(i) = sc%anitcoupe_an1(ipl,i)
                  itk(ipl)%P_restit(i) = sc%restit_an1(ipl,i)
                  itk(ipl)%P_mscoupemini(i) = sc%mscoupemini_an1(ipl,i)

                enddo
               ! dr ET fr 23/06/2015 on fait senescer le msresiduel des le debut de la premiere pousse (en cas de culture deja installée)
                p(ipl)%msres = itk(ipl)%P_msresiduel(1)
             ! DR et FR 24/06/2015 on met un else car on ne peut pas etre en annee 1 et 2 une meme fois
             ! endif
             else
            ! **** cas annee 2 ****
            ! *********************
               write(619,*)'cas annee2'
                ! DR et FR 13/02/2015 on fait les recalculs uniquement sur nbcoupe
!              do i = 1,20
              do i = 1,itk(ipl)%nbcoupe
              ! DR 26/02/08 pour climator la premiere annee on zappe la premiere coupe
              ! qui est la coupe de regain qui ne se fait pas en annee de semis
                if (itk(ipl)%P_codemodfauche == 3) then
                  p(ipl)%tempfauche_ancours(i) = sc%tempfauche_ancours_an2(ipl,i)
                  itk(ipl)%P_tempfauche(i) = sc%tempfauche_an2(ipl,i)
                endif
                if (itk(ipl)%P_codemodfauche == 2) then
                  itk(ipl)%P_julfauche(i) = sc%julfauche_an2(ipl,i)
                endif
                itk(ipl)%P_hautcoupe(i) = sc%hautcoupe_an2(ipl,i)
                itk(ipl)%P_lairesiduel(i) = sc%lairesiduel_an2(ipl,i)
                itk(ipl)%P_msresiduel(i) = sc%msresiduel_an2(ipl,i)
                itk(ipl)%P_anitcoupe(i) = sc%anitcoupe_an2(ipl,i)
                itk(ipl)%P_restit(i) = sc%restit_an2(ipl,i)
                itk(ipl)%P_mscoupemini(i) = sc%mscoupemini_an2(ipl,i)
              enddo
             endif
               if (.not.p(ipl)%fauchediff) then
              ! 1ER CAS ***********************************************************
              ! On est pas en fauche diff mais il peut rester des coupes à faire
              ! *******************************************************************
              ! on a pas cumulé les temperatures necessaires pour faire toutes les coupes de l'annee précédente
               if ((sc%nbcoupe_reel(ipl) .ne.0) .and. (sc%nbcoupe_reel(ipl) < itk(ipl)%nbcoupe)) then
                  ! DR et FR 13/02/2015 on garde le reliquat entre la derniere coupe effectuee nbcoupe_reel et la somme courante
                  ! on introduit une varaible pour tester sur ce qui reste et non sur ce qu'il manque
                  p(ipl)%reste_apres_derniere_coupe = p(ipl)%somcourfauche - p(ipl)%tempfauche_realise
                  write(619,*)'somcourfauche',p(ipl)%somcourfauche
                  write(619,*)'somme a la coupe avant',p(ipl)%tempfauche_realise
                  write(619,*)'il reste ', p(ipl)%reste_apres_derniere_coupe
                   p(ipl)%somcourfauche = p(ipl)%reste_apres_derniere_coupe
                  write(619,*) 'il aurait fallu',itk(ipl)%P_tempfauche(sc%nbcoupe_reel(ipl)+1)
                endif
              ! DR et FR 13/02/2015 On decide de changer les regles de decisions pour les coupes à venir pour se mettre tout de suite dans le rythme du debut d'annee
              !  on regarde plutot par rapport a ce qu'on a deja cumulé et qu'on a gardé de l'annee d'avant
 !               if (p(ipl)%somcourfauche < sc%tempfauche_ancours_an2(ipl,1)) then
                !if (p(ipl)%reste_apres_derniere_coupe > sc%tempfauche_ancours_an2(ipl,1)) then
                if (p(ipl)%reste_apres_derniere_coupe < sc%tempfauche_ancours_an2(ipl,1).and.sc%numcult.gt.1) then
                ! 1er sous cas
                ! ************
                ! ce qu'il manquait pour faire la derniere coupe est < à ce qu'il
                ! faudrait pour faire la premiere coupe de l'annee suivante
                ! --> on fait la premiere coupe de l'annee suivante lorsque qu'on a finit de cumuler
                ! ce qu'il manque et on décale les autres coupes en conservant
                ! le meme ecart entre coupes que l'ecart prescrit
                 ! --> on reinitialise somcourfauche à 0
                 ! --> on insere une coupe en debut d'annee (celle de la pousse de l'annee d'avant  non finie)
                 ! --> on recalcule les numeros de coupe et le calendrier des coupes
                  som_temp_manquante=p(ipl)%tempfauche_ancours(sc%nbcoupe_reel(ipl)+1)- &
                                          p(ipl)%tempfauche_ancours(sc%nbcoupe_reel(ipl))-p(ipl)%reste_apres_derniere_coupe
                  write(619,*)'1er sous cas, somtemmanq',som_temp_manquante
                  !DR 10/06/2016 je vais essayer de mettre la coupe de l'annee d'avant en indice 0 pour ne pas poser de pb

                  p(ipl)%somcourfauche = 0.
                  itk(ipl)%nbcoupe=itk(ipl)%nbcoupe+1
                  p(ipl)%numcoupe=0

                  p(ipl)%tempfauche_ancours(0)=som_temp_manquante
                  itk(ipl)%P_hautcoupe(0)   = itk(ipl)%P_hautcoupe(sc%nbcoupe_reel(ipl)+1)
                  itk(ipl)%P_lairesiduel(0) = itk(ipl)%P_lairesiduel(sc%nbcoupe_reel(ipl)+1)
                  itk(ipl)%P_msresiduel(0)  = itk(ipl)%P_msresiduel(sc%nbcoupe_reel(ipl)+1)
                  itk(ipl)%P_anitcoupe(0)   = itk(ipl)%P_anitcoupe(sc%nbcoupe_reel(ipl)+1)
                  itk(ipl)%P_restit(0)      = itk(ipl)%P_restit(sc%nbcoupe_reel(ipl)+1)
                  itk(ipl)%P_mscoupemini(0) = itk(ipl)%P_mscoupemini(sc%nbcoupe_reel(ipl)+1)

                  do i = 1,itk(ipl)%nbcoupe
                     p(ipl)%tempfauche_ancours(i)=sc%tempfauche_ancours_an2(ipl,i)+som_temp_manquante
                     itk(ipl)%P_hautcoupe(i) = sc%hautcoupe_an2(ipl,i)
                     itk(ipl)%P_lairesiduel(i) = sc%lairesiduel_an2(ipl,i)
                     itk(ipl)%P_msresiduel(i) = sc%msresiduel_an2(ipl,i)
                     itk(ipl)%P_anitcoupe(i) = sc%anitcoupe_an2(ipl,i)
                     itk(ipl)%P_restit(i) = sc%restit_an2(ipl,i)
                     itk(ipl)%P_mscoupemini(i) = sc%mscoupemini_an2(ipl,i)
                  enddo

                 write(619,*)'nouveau calendar',(p(ipl)%tempfauche_ancours(i),i=0,itk(ipl)%nbcoupe)
                 write(619,*)'on met onafaitunecoupedelanneedavant a', sc%onafaitunecoupedelanneedavant
                 sc%onafaitunecoupedelanneedavant=.TRUE.
                else
                ! 2eme sous cas
                ! *************
                ! ce qu'il manquait pour faire la derniere coupe est > à ce qu'il
                ! faudrait pour faire la premiere coupe de l'annee suivante
                ! --> on fait la premiere coupe de l'annee suivante
                ! selon les sommes de temperatures prescrites
                ! dr et FR On garde le reliquat et on coupe selon le calendrier de la nouvelle annee
                  itk(ipl)%P_tempfauche(1) = sc%tempfauche_an2(ipl,1)
                  difftemp(ipl) = 0.
                ! 19/02/2015 DR et FR on veut garder le calcul des somcourfauche meme si on est en dates
                ! donc il faut le reinitialiser en debut d'annee dans le cas ou on ets en dates
                ! 13/02/2015 on ne remet pas a zero puiqu'on garde le reliquat
                p(ipl)%somcourfauche = 0.
                   if(itk(ipl)%P_codemodfauche == 2) then
                       p(ipl)%somcourfauche = 0.
                   endif
                 write(619,*)'2eme sous cas'
                 write(619,*)'nouveau calendar',(p(ipl)%tempfauche_ancours(i),i=1,itk(ipl)%nbcoupe)
                endif
              else
            ! 2EME CAS FAUCHE DIFFEREE ***************************************************
            ! On a reporte car on avait pas assez de lai ou de matiere seche pour couper
            ! on repousse la coupe jusqu'a avoir le lai min
            ! on recalcule les temfauche_ancours à partir de la coupe 2
             ! ***************************************************************************
            ! on a repoussé la derniere coupe car lai et P_msresiduel insuffisants
            ! --> on supprime la 1ere coupe prevue
            !     on remet somcourfauche à 0 et on demarre à la 2eme coupe
            ! --> on reinitialise tempfauche_ancours de facon à ce que
            !     sa 1ere valeur soit celle necessaire à la deuxieme coupe
            ! --> on reinitialise nbcoupe à nbcoupe-1
            write(619,*)'2eme casfauche differee'
            !DR 26/06/2015 les prescriptions de la coupe 1 sont celles de la coupe d'avant ca plante !!!
                itk(ipl)%P_hautcoupe(1)=p(ipl)%hautcoupe_anterieure
                itk(ipl)%P_lairesiduel(1)=p(ipl)%lairesiduel_anterieure
                itk(ipl)%P_msresiduel(1) =p(ipl)%msresiduel_anterieure
                itk(ipl)%P_anitcoupe(1) =p(ipl)%anitcoupe_anterieure
                itk(ipl)%P_restit(1) = p(ipl)%restit_anterieure
                itk(ipl)%P_mscoupemini(1) = p(ipl)%mscoupemini_anterieure

! 15/04/2016 on tente de garder les variables de stades de la vegetaion à la fin de l'annee
               p(ipl)%ulai(1) = p(ipl)%ulai0
               p(ipl)%durvie(:,1) = p(ipl)%durvie0(:)
               if (p(ipl)%codebbch0.ge.9) p(ipl)%nlev=1
               if (p(ipl)%codebbch0.ge.35)then
                    p(ipl)%nlev=1
                    p(ipl)%namf=1
               endif
               if (p(ipl)%codebbch0.ge.55) then
                    p(ipl)%nlev=1
                    p(ipl)%namf=1
                    p(ipl)%nlax=1
                endif
! 15/04/2016
                do i = 1,9
                  if (itk(ipl)%P_codemodfauche == 3) then
                  ! DR et FR on ne supprime pas la premiere coupe finalement
                    itk(ipl)%P_tempfauche(i+1) = sc%tempfauche_an2(ipl,i)
                    itk(ipl)%P_hautcoupe(i+1) = sc%hautcoupe_an2(ipl,i)
                    itk(ipl)%P_lairesiduel(i+1) = sc%lairesiduel_an2(ipl,1)
                    itk(ipl)%P_msresiduel(i+1) = sc%msresiduel_an2(ipl,i)
                    itk(ipl)%P_anitcoupe(i+1) = sc%anitcoupe_an2(ipl,i)
                  endif
                  if (itk(ipl)%P_codemodfauche == 2) then
                    itk(ipl)%P_julfauche(i) = sc%julfauche_an2(ipl,i)
                    itk(ipl)%P_hautcoupe(i) = sc%hautcoupe_an2(ipl,i)
                    itk(ipl)%P_lairesiduel(i) = sc%lairesiduel_an2(ipl,i)
                    itk(ipl)%P_msresiduel(i) = sc%msresiduel_an2(ipl,i)
                    itk(ipl)%P_anitcoupe(i) = sc%anitcoupe_an2(ipl,i)
                    itk(ipl)%P_restit(i) = sc%restit_an2(ipl,i)
                    itk(ipl)%P_mscoupemini(i) = sc%mscoupemini_an2(ipl,i)
                  endif
                enddo

                if (itk(ipl)%P_codemodfauche == 3) then
                  do k = 1,19
                    if (k > 1) then
                      p(ipl)%tempfauche_ancours(k) = itk(ipl)%P_tempfauche(k) + p(ipl)%tempfauche_ancours(k-1)
                    else
                      p(ipl)%tempfauche_ancours(k) = itk(ipl)%P_tempfauche(k)
                    endif
                  enddo
                endif
                p(ipl)%somcourfauche = 0.
                itk(ipl)%nbcoupe = sc%nbcoupe_an2(ipl) - 1
              endif
              itk(ipl)%nbcoupe = sc%nbcoupe_an2(ipl)
              write(619,*)'nouveau calendar',(p(ipl)%tempfauche_ancours(i),i=1,itk(ipl)%nbcoupe)

            else
            ! les 4 coupes normales
            ! **** cas annee 1 semis ****
            !****************************
            ! DR cas annee 1
                        write(619,*)'les 4 coupes normales'
              do  i = 1,20
                if (itk(ipl)%P_codemodfauche == 3) then
                  itk(ipl)%P_tempfauche(i) = sc%tempfauche_an1(ipl,i)
                endif
                if (itk(ipl)%P_codemodfauche == 2) then
                  itk(ipl)%P_julfauche(i) = sc%julfauche_an1(ipl,i)
                endif
                itk(ipl)%P_hautcoupe(i) = sc%hautcoupe_an1(ipl,i)
                itk(ipl)%P_lairesiduel(i) = sc%lairesiduel_an1(ipl,i)
                itk(ipl)%P_msresiduel(i) = sc%msresiduel_an1(ipl,i)
                itk(ipl)%P_anitcoupe(i) = sc%anitcoupe_an1(ipl,i)
                itk(ipl)%P_restit(i) = sc%restit_an1(ipl,i)
                itk(ipl)%P_mscoupemini(i) = sc%mscoupemini_an1(ipl,i)

                p(ipl)%tempfauche_ancours(i) = sc%tempfauche_ancours_an1(ipl,i)
              enddo
              write(619,*)'nouveau calendar annee 1 (init_prairie, 368)',(p(ipl)%tempfauche_ancours(i),i=1,itk(ipl)%nbcoupe)
              itk(ipl)%nbcoupe = sc%nbcoupe_an1(ipl)
              p(ipl)%somcourfauche = 0.
            endif
          endif
!
          p(ipl)%numcoupe = 1

          ! TRAITEMENT DES HAUTEURS DE COUPE QUELQUE SOIT LE MODE DE COUPE (DATE,SOMME,STADE)
          !*********************************************************************************
          if (itk(ipl)%P_codefauche == 1) then
            if (itk(ipl)%lecfauche) then
             ! CAS FAUCHE A DATE OU SOMME  ***************


              do i = 1,itk(ipl)%nbcoupe
                p(ipl)%nfauche(i) = itk(ipl)%P_julfauche(i)- sc%P_iwater + 1


              ! initialisation pour la prairie calcul de lai et ms residuels f(hauteur de coupe)
                if (itk(ipl)%P_hautcoupe(i) < 999) then
                ! utilisation des relations suivantes : P_msresiduel = P_coefmshaut * P_hautcoupe
                ! et la relation de raytrans.for        lai = P_hautmax *(1-exp(-P_khaut * hauteur)) + P_hautbase
                  itk(ipl)%P_msresiduel(i) = p(ipl)%P_coefmshaut * (itk(ipl)%P_hautcoupe(i) - p(ipl)%P_hautbase)
                  itk(ipl)%P_lairesiduel(i) = -1. / pg%P_khaut *                                             &
                                                   log(1. - ((itk(ipl)%P_hautcoupe(i) - p(ipl)%P_hautbase) / p(ipl)%P_hautmax))
                  itk(ipl)%P_lairesiduel(i) = max(itk(ipl)%P_lairesiduel(i),0.)
                  itk(ipl)%P_msresiduel(i) = max(itk(ipl)%P_msresiduel(i),0.)
!                  P_mscoupemini = max(P_mscoupemini,itk(ipl)%P_msresiduel(1))
                endif
              end do
            else
            ! CAS FAUCHE A UN STADE ***************
              do i = 1,10
                p(ipl)%udevlaires(i)    = 0.
                itk(ipl)%P_hautcoupe(i)   = itk(ipl)%P_hautcoupedefaut
                itk(ipl)%P_msresiduel(i)  = p(ipl)%P_coefmshaut * (itk(ipl)%P_hautcoupe(i) - p(ipl)%P_hautbase)
                itk(ipl)%P_lairesiduel(i) = -1. / pg%P_khaut * log(1. -                                      &
                                           ((itk(ipl)%P_hautcoupe(i) - p(ipl)%P_hautbase) / p(ipl)%P_hautmax))
                itk(ipl)%P_lairesiduel(i) = max(itk(ipl)%P_lairesiduel(i),0.)
                itk(ipl)%P_msresiduel(i)  = max(itk(ipl)%P_msresiduel(i),0.)

              end do
              itk(ipl)%nbcoupe = 10
            endif
            itk(ipl)%P_msresiduel(0) =  p(ipl)%P_masec0
          ! PB - 27/07/2009 - obligé de rajouter l'indice -1 pour les cas où numcoupe = 0 et où on demande l'indice numcoupe-1...
            itk(ipl)%P_msresiduel(-1) =  p(ipl)%P_masec0
!            itk(ipl)%P_lairesiduel(0) = P_lai0
!            somcourfauche = 0.
          else
            itk(ipl)%P_msresiduel(0) = 0.
          ! PB - 27/07/2009 - obligé de rajouter l'indice -1 pour les cas où numcoupe = 0 et où on demande l'indice numcoupe-1...
            itk(ipl)%P_msresiduel(-1) = 0.
          endif

        ! DR 31/01/08 climator : on garde ilaxs pour les pariries
          do i = 1,10
            p(ipl)%ilaxs_prairie(i) = -999
          enddo
        endif
! == = == = == = == = == = fin  prairies climator / plutot prairies semees

! DR et FR 16/02/2015 on traite le cas des prairies en semis l'annee 1  codestade0=snu
!  on calcule le nfauche pour l'annee de semis
! 17/02/2015 on le fait dans les 2 cas coupes en dates et en sommes de temp
        if(sc%numcult.eq.1)then
           if (itk(ipl)%P_codefauche == 1 .and. itk(ipl)%lecfauche) then
              write(619,*)'apres fin prairies climator'
              p(ipl)%numcoupe = 1
              do i = 1,itk(ipl)%nbcoupe
                  if (itk(ipl)%P_codemodfauche == 2 )then
                     p(ipl)%nfauche(i) = sc%julfauche_an1(sc%ipl,i)- sc%P_iwater + 1
                  endif
                  if (itk(ipl)%P_codemodfauche == 3 )then
                     p(ipl)%tempfauche_ancours(i) = sc%tempfauche_ancours_an1(ipl,i)
                  endif
                ! DR et Fr 22/06/2015 il ne faut pas oublier de recuperer les hauteur, msresiduel et le reste ...
                itk(ipl)%P_hautcoupe(i) = sc%hautcoupe_an1(ipl,i)
                itk(ipl)%P_lairesiduel(i) = sc%lairesiduel_an1(ipl,i)
                itk(ipl)%P_msresiduel(i) = sc%msresiduel_an1(ipl,i)
                itk(ipl)%P_anitcoupe(i) = sc%anitcoupe_an1(ipl,i)
                itk(ipl)%P_restit(i) = sc%restit_an1(ipl,i)
                itk(ipl)%P_mscoupemini(i) = sc%mscoupemini_an1(ipl,i)
              enddo
              write(619,*)'nouveau calendar annee 1 (init_prairie, 351)',(p(ipl)%tempfauche_ancours(i),i=1,itk(ipl)%nbcoupe)
          endif
        endif
! DR et FR 24/06/2015 on deplace ici car valide pour toutes les situations
!DR et Fr 16/02/2016 le mscooupe mini est vraiment ce qu'on veut recolter au minimum , sinon on se fatigue pas à se deplacer !
!            itk(ipl)%mscoupemini_courant = max(itk(ipl)%P_mscoupemini,itk(ipl)%P_msresiduel(p(ipl)%numcoupe))
! voir si on peut supprimer mscoupemini_courant
!DR 29/03/2016 si on le mets en indice on peut plus s'en passer voir si i est correct
            itk(ipl)%mscoupemini_courant =itk(ipl)%P_mscoupemini(i)
            itk(ipl)%mscoupemini_courant =itk(ipl)%P_mscoupemini(i)
 !  *** ecritures dans history
          do i = 1,itk(ipl)%nbcoupe
            if (itk(ipl)%P_codemodfauche == 2) then
              write(tmp,*) 'P_julfauche,P_hautcoupe,P_lairesiduel,P_msresiduel,P_anitcoupe ',        &
                           itk%P_julfauche(i),itk(ipl)%P_hautcoupe(i),itk(ipl)%P_lairesiduel(i), &
                           itk(ipl)%P_msresiduel(i),itk(ipl)%P_anitcoupe(i)
              call EnvoyerMsgHistorique(tmp)
            ! NB - le 20/01/2004 - test de cohérence (avec FR)
              if (itk(ipl)%P_lairesiduel(i) > 0.0 .and. itk(ipl)%P_msresiduel(i) <= 0.0)   &
                call EnvoyerMsgHistorique(91)
            endif

            if (itk(ipl)%P_codemodfauche == 3) then
              write(tmp,*) 'P_tempfauche,P_hautcoupe,P_lairesiduel,P_msresiduel,P_anitcoupe',              &
                           itk(ipl)%P_tempfauche(i),itk(ipl)%P_hautcoupe(i),itk(ipl)%P_lairesiduel(i), &
                           itk(ipl)%P_msresiduel(i),itk(ipl)%P_anitcoupe(i)
              call EnvoyerMsgHistorique(tmp)
            ! NB - le 20/01/2004 - test de cohérence (avec FR)
              if (itk(ipl)%P_lairesiduel(i) > 0.0 .and. itk(ipl)%P_msresiduel(i) <= 0.0)   &
                call EnvoyerMsgHistorique(91)
            endif
          enddo
  enddo
         write(619,*)'fin de initprairie',(p(ipl)%tempfauche_ancours(i),i=1,itk(ipl)%nbcoupe)
end subroutine
