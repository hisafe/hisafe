!> annual loop
!> - initializations
!> - Call of the loop DAILY
!> - write the output files
! DR 17/10/2013 on ajoute le module patho pour couplage avce Mila
! DR 07/02/2014 on supprime tout ce qui a trait à Mila de la branche
!subroutine Stics_Boucle_Annees(sc,p,pg,itk,c,sta,soil,t,usma,patho,pis)

!!!MODIF HISAFE 8 : suppression objet USM
!!!subroutine Stics_Boucle_Annees(sc,p,pg,itk,c,sta,soil,t,usma)
subroutine Stics_Boucle_Annees(sc,p,pg,itk,c,sta,soil,t)

USE Stics
!!!MODIF HISAFE 8 : suppression de l'objet USM
!!!USE USM
USE Plante
USE Itineraire_Technique
USE Sol
USE Climat
USE Station
USE Parametres_Generaux
USE Divers
USE Bilans
! DR 17/10/2013 on ajoute le module patho pour couplage avce Mila
!USE mod_patho on enleve les appel à Mila geres sur la branche Stics_Mila

implicit none

    !type(USM_),                  intent(INOUT) :: P_usm


    type(Stics_Communs_),        intent(INOUT) :: sc  

    type(Plante_),               intent(INOUT) :: p(sc%P_nbplantes)

    type(Parametres_Generaux_),  intent(INOUT) :: pg    ! TODO pg devrait être en IN, tous les paramètres modifiés doivent être dupliqués dans sc  

    type(ITK_),                  intent(INOUT) :: itk(sc%P_nbplantes)

    type(Climat_),               intent(INOUT) :: c  

    type(Station_),              intent(INOUT) :: sta  

    type(Sol_),                  intent(INOUT) :: soil  

    type(Stics_Transit_),        intent(INOUT) :: t  

  !DR 17/07/2012 je rajoute le module usm pour les noms de fichiers pour optimistics
!!!MODIF HISAFE 8 : suppression de l'objet USM
!!!  type(USM_),                 intent(INOUT) :: usma  !> // PARAMETER // name of the P_USM // SD // USMXML // 0

  ! DR 17/10/2013 on ajoute le module patho pour couplage avce Mila
!    type (Patho_),               intent (INOUT) :: patho
!    type (Patho_inputs_Stics_),  intent (IN)    :: pis


    integer :: numcult  
   ! character(len=15) :: nom_variete
    integer :: i  !>  
    integer ::  n  

    integer :: jour_iwater  !>  
    integer :: nummois_iwater  
    character(len=3) :: mois_iwater  !>  
    character(len=3) :: mois_ifwater  
    integer :: jour_ifwater  !>  
    integer :: nummois_ifwater  !>  
    integer :: ancours  !>  
    integer :: ifin  !>  
    integer :: ifwater1an  
!    integer :: ibis  !>
!    integer :: cdts  !>
!    integer :: nbj_ansuiv
    integer totob,totoe,totor

    integer :: ficClimat = 153 ! numéro unique dans Stics pour le fichier climat.


!TODO    logical :: plante_ori(sc%P_nbplantes)
!TODO    type(Plante_) :: p_ori(sc%P_nbplantes)

!!!    character(len=2) :: langue ! TODO: cette variable devrait être stockée
                               ! dans la structure Stics et lue dans un des
                               ! fichiers de configuration de la simulation

character(len=3)  :: mois

            totob=iand(pg%P_flagEcriture,sc%ECRITURE_BILAN)
            totoE=iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)
            totor=iand(pg%P_flagEcriture,sc%ECRITURE_RAPPORTS)


if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )print *,'loop year : ',sc%nbans

    ! La boucle des années
      do numcult = 1, sc%nbans

        sc%numcult = numcult

!:: Diverses initialisation/sauvegardes/récupération en cas d'enchainement

!: On affiche le numéro de la culture courante (pour info uniquement)
if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )write(*,*) 'numcult = ',numcult
if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG) >0 )write(*,*) 'numcult = ',numcult

!: DR - 13/05/08 - Spécificités Climator
!- On conserve le nom de la P_variete car dans le cas où la plante meurt,
!- on lit solnu.plt et donc on perd le nom de la variété d'origine
!: PB - 23/10/08 - Modularisation : on ne perd plus le nom de la variété de la plante,
!- puisqu'en cas de mort de la plante, on échange sans pour autant effacer la plante contre du sol nu.
!    if (numcult == 1) nom_variete = p(1)%P_codevar(p(1)%P_variete)

    !: DR - 25/02/08 -On ajoute l'age de la prairie
        do i= 1, sc%P_nbplantes
        ! DR 09/10/09 on rajoute un test sur le P_stade0

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!          if (p(i)%P_codeplante == 'fou' .and. (p(i)%P_stade0 == 'snu' .or. p(i)%P_stade0 == 'plt')) then
          if (p(i)%P_codeplante == 2 .and. (p(i)%P_stade0 == 1 .or. p(i)%P_stade0 == 2)) then
            p(i)%age_prairie = numcult
          ! DR - 19/02/08
          ! DR et FR 17/02/2015 ce test ne permet pas de faire des enchainements de prairies semees
          ! on le change
          ! avant
          !  if (numcult > 1) then
          !    p(i)%P_codeperenne = 2
          !    p(i)%P_stade0 = 'amf'
          ! fin avant
          ! DR et FR 23/06/2015 dans le cas des prairies  semees avec reset il ne faut pas repartir à amf
          !  if (numcult > 1 .and. p(i)%codeperenne0==2) then
            if (numcult > 1 .and. p(i)%codeperenne0==2 .and. pg%P_codeinitprec==2 ) then
              p(i)%P_codeperenne = 2
              !!!MODIF HISAFE 1 : suppression des chaines de caractères
              !!!p(i)%P_stade0 = 'amf'
              p(i)%P_stade0 = 5
          ! DR - 28/02/08 - Il ne faut pas faire le travail du sol si on est en annee (semis+1)
! DR 01/02/2011 on a un P_nbjtrav à la place de napS
!              itk(i)%naps = 0
              itk(i)%P_nbjtrav = 0
            else
              p(i)%P_codeperenne = 1
              p(i)%numcoupe=1
              p(i)%fauchediff=.FALSE.
              ! DR et FR 17/02/2015 on remet a zero les varaibles servant a calculer mafruit
              ! dans cumaoas on recalucule un delta de msresjaune et msneojaune en ayant concerve la valeur du jour d'avant dans
              ! les vaariables suffixées en v qunad on change d'annee en mode semis c'est plus bon
!              p(i)%masecneov    = 0.0
!              p(i)%msneojaunev  = 0.0
!              p(i)%msresjaunev  = 0.0
!              p(i)%msresv       = 0.0

            endif
          else
            p(i)%age_prairie = -999
          endif

        enddo

    ! Si on enchaine plusieurs années climatiques => P_codesuite=1
        if (numcult > 1) sc%P_codesuite = 1

    ! Cas d'enchainement de 2 P_usm differentes : on force P_codeinitprec à 2
        if (sc%P_codesuite == 1 .and. sc%nbans == 1) pg%P_codeinitprec = 2


    ! DR 04/12/07 on finit en sol nu quand on a plus de racine mais si on est
    ! sur un enchainement d'annee, il faut retourner à la plante d'origine l'année suivante
!TODO        if (numcult > 1 .and. sc%nbans > 1) then
!          do i = 1,sc%P_nbplantes
!            if ( plante_ori(i) .eqv. .FALSE.) then
!              p(i) = p_ori(i)
!              plante_ori(i) = .TRUE.
!            ! DR 20/03/08 dans le cas ou on fait de l'optimisation pb il faut relire lecoptim
!              if(sc%codoptim /= 0)then
!                ! TODO: call lecoptim
!              endif
!            endif
!          enddo
!        endif



    ! Initialisation sols, climat et variables du cycle journalier
    if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )print *, 'Soil Initialisations'

    !!!MODIF HISAFE : on rajoute un test sur enchainement des années
    !!On initialise le SOL que la première année
    if (pg%P_codeinitprec == 1 .and. sc%P_codesuite == 0) then
        call initialisations_sol(sc,pg,t,soil,sta)
    endif


if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )print *, 'Initialisations'
        call initialisations(sc,pg,p,itk,soil,c,sta,t)


    ! DR - le 16/08/06 - pour Inaki (CC) on redonne la valeur du parametre co2 suivant
    !- la valeur initiale de param.par et l'année de la simulation
    ! IGC - le 11/09/06 - je modifie les valeurs de ansemis pour les études de
    !- changement climatique. Fred Huard fait les années à partir de valeurs réelles
    !- donc ansemis est égal à 1900. J'introduis un test sur l'utilisation des années
    !- climatiques en changement climatique.
    ! DR - 29/10/07 - maintenant on lit directement co2 dans le fichier climatique en journalier


     ! Domi - 29/08/03 - si lecture des apport N en upvt
        do i = 1, sc%P_nbplantes
          if(itk(i)%P_codedateappN == 1) itk(i)%numeroappN = 1
        ! DR 09/10/09 j'ajoute la possibilité de faire des irrigations à des sommes de temp
          if (itk(i)%P_codedateappH2O == 1) itk(i)%numeroappI = 1
        enddo





!:***************************************************************************
!:: Boucle Journalière                                                     !*
write(*,*) 'nbr jour=',sc%maxwth

        do n = 1, sc%maxwth



           print *,'n = ',n

           sc%n = n

         ! on transpose n en jour julien depuis le 1er janvier de l'année de début de simulation
           sc%jjul = tCal1JAbs(sc%n,sc%P_iwater)
         ! à partir de jjul, on calcule le jour julien relatif à l'année en cours de simulation
         ! TODO : vérifier que les nouvelles dates calculées correspondent aux valeurs attendues.
           sc%jul = tCal1JRel(sc%jjul,sc%annee(sc%P_iwater),.false.)
           sc%numdate = sc%jul


           !!!call julien(sc%jul,sc%annee(sc%jjul),sc%mois,sc%jour,sc%nummois)
           call julien(sc%jul,sc%annee(sc%jjul),mois,sc%jour,sc%nummois)
           !print *,'n = ',n,sc%jul,sc%annee(sc%jjul),sc%mois,sc%jour,sc%nummois


           if (iand(pg%P_flagEcriture,sc%ECRITURE_BILAN) > 0 .and. n == 1) then

           ! avant d'écrire, on initialise la liste des messages, ici FR
! DR 30/07/2012 maintenant on fait tout en anglais, je garde ca au cas ou on compile ca pour le chinois ...
!             select case(langue)
!                case('FR')
!                    call bilans_messages_FR
!                case default

                   call bilans_messages_ENG
!             end select

               ! on écrit l'entete du bilan, les données d'entrées et qq informations de configuration de la simulation
               do i = 1, sc%P_nbplantes

                   !!!call ecrireEnteteBilan(sc,pg,p(i),itk(i),soil,sta,t,usma)  !DR 19/07/2012 c n'est pas utilisé
                  call ecrireEnteteBilan(sc,pg,p(i),itk(i),soil,sta,t)  !DR 19/07/2012 c n'est pas utilisé

                 write(*,*) 'ecrireEnteteBilan'

               end do
           endif


if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )write(*,*)'calling of the daily loop Stics_Jour',n,sc%jjul
          ! On appelle la routine journalière de Stics

           write(*,*) 'appel Stics_Jour'

 !!!           call Stics_Jour(sc,pg,p,itk,soil,c,sta,t)


!DR 07/02/2014 on enleve l'appel à Mila qui est géré dans la branche stics_Mila
! DR 08/11/2013 j'essaie de sortie l'appel de la boucle journaliere pour le mettre ici pour couplage avce Mila
!if(t%P_codepatho.eq.1)then
! pour record j'ai ajouté les path , dans stics je les initialise ici
!       write (patho%path, '(A255)') ' '            ! enabling_RECORD
!       patho%path(1:255) = ' '                     ! enabling_RECORD
!       write (patho%pathmila, '(A255)') ' '        ! enabling_RECORD
!       patho%pathmila(1:255) = ' '                 ! enabling_RECORD
!       write (patho%pathoutput, '(A255)') ' '        ! enabling_RECORD
!       patho%pathoutput(1:255) = ' '                 ! enabling_RECORD
! fin
! fin patho
!endif


        end do
!:: Fin de la [Boucle Journalière]                                         !*
!:***************************************************************************




if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )write(*,*)'end annual loop iwater ',sc%P_iwater,'ifwater',sc%P_ifwater



    ! Recalcul des dates de début, dates de fin de simulation en cas d'enchainement

        sc%n = sc%maxwth
!        sc%P_ifwater = sc%n + sc%P_iwater - 1
!            write(*,*)'numcult',sc%ifwater_courant

!if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )write(*,*)'fin boucle annee iwater ',sc%P_iwater,'ifwater',sc%P_ifwater


      ! DR - 18/06/08 - j'essaie d'introduire un calcul coherent de P_ifwater et P_iwater en cas d'enchainement
        if (numcult == 1)then
        ! Calcul de P_iwater d'apres l'annee 1
              call julien(sc%P_iwater,sc%ansemis,mois_iwater,jour_iwater,nummois_iwater)
              ancours = sc%annee(sc%ifwater_courant)
        ! Calcul de P_ifwater d'apres l'annee 1
              ifin = sc%ifwater_courant
              if (sc%ifwater_courant > sc%nbjsemis) then
                ifwater1an = sc%ifwater_courant - sc%nbjsemis
              else
                ifwater1an = sc%ifwater_courant
              endif
              call julien(ifwater1an,ancours,mois_ifwater,jour_ifwater,nummois_ifwater)
        endif

        ! Calculs de P_iwater et P_ifwater pour annee suivante
!        call ndate(jour_iwater,nummois_iwater,sc%annee(sc%P_iwater)+1,sc%iwater_cultsuiv,IBIS,CDTS)
!        call ndate(jour_ifwater,nummois_iwater,sc%annee(sc%P_ifwater)+1,sc%ifwater_cultsuiv,IBIS,CDTS)
!        if (sc%P_culturean /= 1) then
!          nbj_ansuiv = nbjan(sc%annee(sc%P_iwater)+1)
!          sc%ifwater_cultsuiv = sc%ifwater_cultsuiv + nbj_ansuiv
!        endif
!  write(5588,*)'fin iwater',sc%P_iwater,'ifwater',sc%P_ifwater


    ! Rapport synthetique 1 ligne par annee
        do i = 1, sc%P_nbplantes

       !write(*,*)'codefauche',itk(i)%P_codefauche
          if(itk(i)%P_codefauche == 2) then

          ! DR 14/08/09
          ! integration de 7.1 dans 7.4 le calcul de densite etait activé
          ! DR 12/02/08 je comprends pas pourquoi on fait ca au moment du bilan
          ! à voir ensemble ca me fait une densite nulle pour la plante principale
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !--if (sc%P_nbplantes > 1) p(i)%densite = p(i)%densite - (p(1)%densite/ p(1)%P_bdens * p(i)%P_bdens)


            if (iand(pg%P_flagEcriture,sc%ECRITURE_BILAN) > 0) then
              if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )write(*,*)'before writing default balance'
          !!!!    call ecrireBilanDefaut(sc,pg,soil,p(i),itk(i))
            endif
          endif

!: PB - 13/10/2008 - Mise en commentaire du IF car rien à l'intérieur
!: DR - 200406 - On avait plus de recap de bilan pour la prairie
!--      if(P_codefauche(i).eq.1) then
!          nrecbutoir(i)=n
!          call bilcoupe
! DR 21/07/06 on enleve le bilan ici car il ne correspond en realité qu'a la derniere coupe
!          call bilan
!--      endif

!--      write(*,*) 'avant rapport',ipl
!--      if(ecritrap.eq..FALSE.) call Ecriture_Rapport(sc,pg,soil,c,sta,p(i),itk(i),t)
!--      if(codeaucun.eq.1) call Ecriture_Rapport(sc,pg,soil,c,sta,p(i),itk(i),t)

          sc%P_datefin = .TRUE.
          if (iand(pg%P_flagEcriture,sc%ECRITURE_RAPPORTS) >0 .or. iand(pg%P_flagEcriture,sc%ECRITURE_AGMIP) >0 ) then
            if (sc%rapfin)then
              if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )write(*,*)'report writing'
                ! DR 19/07/2012 on supprime itk qui ne sert pas
!DR 07/02/2014 on enleve patho du tronc
!               if (iand(pg%P_flagEcriture,sc%ECRITURE_RAPPORTS) >0)call Ecriture_Rapport(sc,pg,soil,c,sta,p(i),t,patho)
               if (iand(pg%P_flagEcriture,sc%ECRITURE_RAPPORTS) >0)call Ecriture_Rapport(sc,pg,soil,p(i),t)
! DR 29/08/2012 j'ajoute un code pour garder les sorties Agmip t%P_Config_Output : 1=rien , 2 sorties ecran , 3 sorties agmpi
               if (iand(pg%P_flagEcriture,sc%ECRITURE_AGMIP) >0 )call Ecriture_Rapport_AgMIP(sc,pg,soil,sta,p(i),t)
            endif
          endif
        end do

!         write(*,*)'numcult',sc%ifwater_courant

    ! Calculs de P_iwater et P_ifwater pour annee en cours

!        call ndate(jour_iwater,nummois_iwater,sc%annee(sc%P_iwater),sc%P_iwater,IBIS,CDTS)
!        call ndate(jour_ifwater,nummois_ifwater,sc%annee(sc%P_ifwater),sc%P_ifwater,IBIS,CDTS)

!        if (sc%P_culturean /= 1) then
!          nbj_ansuiv = nbjan(sc%annee(sc%P_iwater))
!          write(*,*)'numcult',sc%P_ifwater
!          !dr 12/09/2011 efface ca
!          sc%P_ifwater = sc%P_ifwater + nbj_ansuiv
!          write(*,*)'numcult',sc%P_ifwater
!        endif

!        if (numcult > 1 .and. sc%nbans > 1 .and. sc%P_culturean /= 2) sc%P_iwater = 1


    ! Récup(ération)
      !!!MODIF HISAFE 11 : Supression code inutile
      !!!  if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )write(*,*)'before Ecriture_DonneesFinDeCycle'
      !!!  call Ecriture_DonneesFinDeCycle(sc,pg,soil,p,itk)


!: PB - 13/10/08 - DO vide, on commente
!--    do ipl = 1, P_usm%P_nbplantes
!
!: PB - 13/10/08 - IF vide, on commente
!--      if (P_codeperenne(i).eq.2.and.P_codeinitprec.eq.2 .and. P_codebfroid(i).eq.2) then
!: DR - 20/02/08 - en enchainement ca merdait.
!- Pour le calcul des nombre de voir pour introduire ca à la place du calcul fait dans lecstat
!        call calc_nbjan(ansemis+1,nbjsemis,nbjrecol)
!        P_iwater = n + P_iwater -1
!        P_iwater = n + P_iwater-1
!        if (P_iwater.gt.nbjsemis) P_iwater = P_iwater - nbjsemis
!        P_ifwater = P_iwater + 365
!--      endif
!        write(ficdbg,'(4i4)')numcult,n,P_iwater,P_ifwater
!
!
!: DR - 11/03/08 - On a un pb avce la prairie
!: DR - 18/06/08 - J'enleve pour tester
!        if(P_codeplante(i).eq.'fou'.or.P_codeplante(i).eq.'vig')then
!          P_iwater=iwaterapres
!          P_ifwater=ifwaterapres
!        endif
! dr 12/06/08 pb enchainement prairie sur 2 ans
! DR c'est vraiment un petasse comme dirait marie faut trouver une solution pour
! les calculs de dates
! pour rotation bio : rot 1 et 2 on active ca
! pour rotation 3 on desactive
!        if(P_codeplante(i).eq.'fou'.and.nbans.gt.1)then
!          n=maxwth-1
!          P_ifwater = P_ifwater-1
!        endif
!
!--    end do



    ! Ecriture du profil
    ! DR - 17/01/08 - On ecrit les 2 plantes dans le profil (1 à la suite de l'autre)
        do i= 1, sc%P_nbplantes
          sc%ipl = i
          if (iand(pg%P_flagEcriture,sc%ECRITURE_PROFIL)>0 .and. sc%codeprofil /= 0)then
            if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0) print *,'writing profile'
            call Ecriture_Profil(sc,int(soil%profsol))
          endif
        end do





      end do
      ! Fin de la boucle des années

  ! DR 27/04/2011 on ferme le fichier climatique climat.txt
  !!!close(unit=ficClimat)





return
end subroutine Stics_Boucle_Annees
 
 
