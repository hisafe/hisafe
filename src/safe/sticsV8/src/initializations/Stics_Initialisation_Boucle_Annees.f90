
subroutine Stics_Initialisation_Boucle_Annees(sc,p,pg,itk,c,sta,soil,t)

USE Stics
!!!MODIF HISAFE 8 : suppression de l'objet USM
!!!USE P_USM
USE Plante
USE Itineraire_Technique
USE Sol
USE Climat
USE Station
USE Parametres_Generaux
USE Divers, only: tmoy_histo

implicit none

    !!!type(USM_),                  intent(INOUT) :: P_usm

    type(Stics_Communs_),        intent(INOUT) :: sc  

    type(Plante_),               intent(INOUT) :: p(sc%P_nbplantes)

    type(Parametres_Generaux_),  intent(INOUT) :: pg    ! TODO pg devrait �tre en IN, tous les param�tres modifi�s doivent �tre dupliqu�s dans sc  

    type(ITK_),                  intent(INOUT) :: itk(sc%P_nbplantes)

   type(Climat_),               intent(INOUT) :: c

   type(Station_),              intent(INOUT) :: sta

    type(Sol_),                  intent(INOUT) :: soil

    type(Stics_Transit_),        intent(INOUT) :: t  


!integer, intent(IN) :: P_culturean ! Code culture annuelle ou � cheval sur 2 ann�es
!character(len=12), intent(IN) :: P_codesimul ! Code type simulation
!integer, intent(INOUT) :: nbans ! Nombre d'ann�es de simulation

! Variables locales
    integer :: i  

      ! nbans est calcul� par rapport au nombre d'ann�es climatiques fournies en entr�es.
      ! Pour une culture � cheval sur deux ann�es, il faut enlever une ann�e de simulation.
     !!!MODIF HISAFE 11 : Supression code inutile
     !!!on est toujours sur une ann�e meme si on est � cheval
     !!!   if(sc%P_culturean /= 1) sc%nbans = sc%nbans-1

      ! Pour les versions -stress sans simulation de plante, on n'effectue
      ! la simulation que sur une seule ann�e.
      !!!MODIF HISAFE 1 : suppression des chaines de caract�res
      !!!  if (lge(sc%P_codesimul,'feuille') .eqv. .TRUE.) sc%nbans = 1
        if (sc%P_codesimul == 2) sc%nbans = 1


      ! initialisation de variables qu'on n'effectue qu'une fois en d�but de simulation.
        do i = 1, sc%P_nbplantes
        ! DR et FR 17/02/2015 on conserve le codeperenne de la plante qu'on change quand on fait un semis la premiere annee
          p(i)%codeperenne0 = p(i)%P_codeperenne

          call initsimul(sc,pg,p(i),itk(i),t)
        end do

! ************************* adaptation des Mo au CC ****************************
! DR 26/11/07 on calcule la temperature moyenne annuelle sur la serie clim dispo
! on a toutes les moyennes annuelles (1ier jour serie premiere annee � j-1 annee d'apres)
! dans tmoy_an(1,i)=an, tmoy(2,i)=tmoy annuelle i etant egal � numcult
!    if(P_code_adapt_MO_CC.eq.1.and.nbans.gt.P_periode_adapt_CC)then
!      call calc_tmoy_annuel
!    endif

      ! DR 19/10/09  on enleve le test sur codeoutsti
        if (t%P_code_adaptCC_miner == 1 .or. t%P_code_adaptCC_nit == 1 .or. t%P_code_adaptCC_denit == 1) then

          if (t%P_code_adapt_MO_CC == 1) then
            if (sc%nbans > t%P_periode_adapt_CC) then
              call calc_tmoy_annuel(sc%tmoy_an)
            else
              call EnvoyerMsgHistorique(452)
            endif
          endif
! DR et NB et JS 22/11/07
! calcul tmoy histo et deltaT_CC
          if (t%P_code_adapt_MO_CC == 1 .and. sc%nbans > (t%P_an_fin_serie_histo - t%P_an_debut_serie_histo)) then
            sc%Tm_histo = tmoy_histo(t%P_an_debut_serie_histo,t%P_an_fin_serie_histo,sc%tmoy_an)
          else
            sc%Tm_histo = t%P_param_tmoy_histo
          endif

          call calcDeltaTCC(sc,t)

        endif


! ********************** fin adaptation des Mo au CC ****************************


 sc%numcult = 1

        do i= 1, sc%P_nbplantes
        ! DR 09/10/09 on rajoute un test sur le P_stade0

!!!MODIF HISAFE 1 : suppression des chaines de caract�res
!!!          if (p(i)%P_codeplante == 'fou' .and. (p(i)%P_stade0 == 'snu' .or. p(i)%P_stade0 == 'plt')) then
          if (p(i)%P_codeplante == 2 .and. (p(i)%P_stade0 == 1 .or. p(i)%P_stade0 == 2)) then
            p(i)%age_prairie = sc%numcult
          ! DR - 19/02/08
          ! DR et FR 17/02/2015 ce test ne permet pas de faire des enchainements de prairies semees
          ! on le change
          ! avant
          !  if (numcult > 1) then
          !    p(i)%P_codeperenne = 2
          !    p(i)%P_stade0 = 'amf'
          ! fin avant
          ! DR et FR 23/06/2015 dans le cas des prairies  semees avec reset il ne faut pas repartir � amf
          !  if (numcult > 1 .and. p(i)%codeperenne0==2) then
            if (sc%numcult > 1 .and. p(i)%codeperenne0==2 .and. pg%P_codeinitprec==2 ) then
              p(i)%P_codeperenne = 2
              !!!MODIF HISAFE 1 : suppression des chaines de caract�res
              !!!p(i)%P_stade0 = 'amf'
              p(i)%P_stade0 = 5
          ! DR - 28/02/08 - Il ne faut pas faire le travail du sol si on est en annee (semis+1)
! DR 01/02/2011 on a un P_nbjtrav � la place de napS
!              itk(i)%naps = 0
              itk(i)%P_nbjtrav = 0
            else
              p(i)%P_codeperenne = 1
              p(i)%numcoupe=1
              p(i)%fauchediff=.FALSE.
              ! DR et FR 17/02/2015 on remet a zero les varaibles servant a calculer mafruit
              ! dans cumaoas on recalucule un delta de msresjaune et msneojaune en ayant concerve la valeur du jour d'avant dans
              ! les vaariables suffix�es en v qunad on change d'annee en mode semis c'est plus bon
!              p(i)%masecneov    = 0.0
!              p(i)%msneojaunev  = 0.0
!              p(i)%msresjaunev  = 0.0
!              p(i)%msresv       = 0.0

            endif
          else
            p(i)%age_prairie = -999
          endif

        enddo



    ! Initialisation sols, climat et variables du cycle journalier
    if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )print *, 'Soil Initialisations'
            call initialisations_sol(sc,pg,t,soil,sta)


    if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )print *, 'Initialisations'
            call initialisations(sc,pg,p,itk,soil,c,sta,t)


    ! DR - le 16/08/06 - pour Inaki (CC) on redonne la valeur du parametre co2 suivant
    !- la valeur initiale de param.par et l'ann�e de la simulation
    ! IGC - le 11/09/06 - je modifie les valeurs de ansemis pour les �tudes de
    !- changement climatique. Fred Huard fait les ann�es � partir de valeurs r�elles
    !- donc ansemis est �gal � 1900. J'introduis un test sur l'utilisation des ann�es
    !- climatiques en changement climatique.
    ! DR - 29/10/07 - maintenant on lit directement co2 dans le fichier climatique en journalier


     ! Domi - 29/08/03 - si lecture des apport N en upvt
        do i = 1, sc%P_nbplantes
          if(itk(i)%P_codedateappN == 1) itk(i)%numeroappN = 1
        ! DR 09/10/09 j'ajoute la possibilit� de faire des irrigations � des sommes de temp
          if (itk(i)%P_codedateappH2O == 1) itk(i)%numeroappI = 1
        enddo
return
end subroutine Stics_Initialisation_Boucle_Annees
 
 
