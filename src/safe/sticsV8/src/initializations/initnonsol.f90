!*********************************************************************
!  SUBROUTINE d'initialisation des infos SOL, CULTURE
!     culture : remise à 0 des stades calculés
!               reaffectation des sommes entre stades
!               remise à zero des variables plantes
!         sol :  Cas n°1:  P_codeinitprec = 2   (enchainement = lecture dans recup.tmp)
!                Cas n°2:  P_codeinitprec = 1   (reinitialisation)


!  derniere modif nadine 25/05/05
!*********************************************************************

subroutine initnonsol(sc,pg,p,itk,soil,sta,t)  ! c supprimé


USE Stics
USE Plante
USE Itineraire_Technique
USE Sol
! USE Climat
USE Station
USE Parametres_Generaux
USE Divers
USE Messages
USE Divers, only: isBissextile

  implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc  
  type(Parametres_Generaux_), intent(INOUT) :: pg
  type(Plante_),              intent(INOUT) :: p (sc%P_nbplantes)
  type(ITK_),                 intent(INOUT) :: itk (sc%P_nbplantes)
  type(Sol_),                 intent(INOUT) :: soil  
!  type(Climat_),              intent(INOUT) :: c
  type(Station_),             intent(INOUT) :: sta  
  type(Stics_Transit_),       intent(INOUT) :: t  

! Variable(s) locale(s)
      logical :: drpavantlax  
      real    :: Ch  !>  
      real    :: QNorg  !>  
      real    :: Nactif !>
      real    :: Cactif  !>  
      real    :: QEXC  
      integer :: izmax  !>  
      integer :: i  !>  
      integer :: iz  !>  
      integer :: ipl  
      integer :: k  
      real    :: HEXC  !>  
      real    :: Qeauinit(5)  


      integer :: AOAS  !>  
      integer ::  AS  !>  
      integer ::  AO  
      logical :: AgMIP,Macsur

      character(len=500) :: tmp ! pour la consitution de messages complexes pour le fichier historique.


!dr01/03/sert pas      integer :: ansemis_avant,anrecol_avant,P_iwater_avant,P_ifwater_avant
      real :: HR_dom(5),azo_dom(5),amm_dom(5)


  AgMIP= .FALSE.
  Macsur= .FALSE.
  if(t%P_type_project.eq.2) AgMIP= .TRUE.
  if(t%P_type_project.eq.3) Macsur= .TRUE.

      AOAS = sc%AOAS
      AO = sc%AO
      AS = sc%AS

! TODO: harmoniser les paramètres des itinéraires techniques des plantes (paillage, mulch, etc..)


!  write(5588,*)sc%numcult,'debut initnonsol  iwater',sc%P_iwater,'ifwater',sc%P_ifwater,sc%ifwater_courant,sc%maxwth



! DR 18/06/08 on integre les modifs enchainement (C)ultures (AS)sociées
! DR 030608 on essai de mettre la lecture de recup avant les initialisations
!!!MODIF HISAFE 11 : Supression code inutile
!!!      if (pg%P_codeinitprec == 2 .and. sc%P_codesuite /= 0) then
!!!      if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )print *,'Lecture du fichier recup'
!!!        call Lecture_DonneesCyclePrecedent(sc,pg,p,itk,soil) !,c,sta,t)
!!!      endif


! domi - 04/02/04
! On rajoute des tests pour la compatibilité entre initialisations et stade de demarrage
      do ipl = 1,sc%P_nbplantes
        !!!MODIF HISAFE 1 : suppression des chaines de caractères
        !!!if (p(ipl)%P_stade0 == 'snu') then
        if (p(ipl)%P_stade0 == 1) then
          if (p(ipl)%P_lai0 /= 0 .or. p(ipl)%P_masec0 /= 0 .or. p(ipl)%P_zrac0 /= 0 .or.  &
              p(ipl)%P_magrain0 /= 0 .or. p(ipl)%P_QNplante0 /= 0) then
            call EnvoyerMsgHistorique('(//)')
            call EnvoyerMsgHistorique(341)
            p(ipl)%P_lai0 = 0.0
            p(ipl)%P_masec0 = 0.0
            p(ipl)%P_zrac0 = 0.0
            p(ipl)%P_magrain0 = 0.0
            p(ipl)%P_QNplante0 = 0.0
            ! domi - demander à nadine pour P_resperenne0
          endif
        endif

        !!!MODIF HISAFE 1 : suppression des chaines de caractères
        !!!if (p(ipl)%P_stade0 == 'plt') then
        if (p(ipl)%P_stade0 == 2) then
        call EnvoyerMsgHistorique('(//)')
          if (p(ipl)%P_lai0 /= 0 .or. p(ipl)%P_masec0 /= 0 .or. p(ipl)%P_zrac0 /= 0 .or. &
              p(ipl)%P_magrain0 /= 0 .or. p(ipl)%P_QNplante0 /= 0) then
            call EnvoyerMsgHistorique(342)
          endif
        endif
      end do


! initialisations pour FERTIL
      soil%QNdenit = 0.
      soil%QNvolorg = 0.
      soil%QNvoleng = 0.
      soil%QNorgeng = 0.
      sc%totapN   = 0.
      sc%pluieN   = 0.
      sc%irrigN   = 0.
      sc%totapNres = 0.

! -------------------------------------------------------------
!   LES TECHNIQUES CULTURALES
! -------------------------------------------------------------
      do ipl = 1, sc%P_nbplantes

    ! azote
        p(ipl)%Qfix(:) = 0.

    ! DR 18/09/07 on initialise le tableau des dates d'apport N et H2o
    !!!    t%nbapN(ipl) = 0
    !!!    t%nbapirr(ipl) = 0
    !!!    t%dateN(ipl,:)   = 0
    !!!    t%dateirr(ipl,:) = 0

    ! test sur P_proftrav
        do i = 1, itk(ipl)%P_nbjtrav
          if (itk(ipl)%P_proftrav(i) > soil%profsol) then
            call EnvoyerMsgHistorique(82)
            !stop
            call exit(9)
          endif
        end do

    ! évaporation sol
        p(ipl)%parapluie = 1


      end do

    ! PB - par défaut, on se fie au P_codepaillage de la plante principale
    !   if (itk(1)%P_codepaillage /= 2) itk(1)%couvermulch = 0.

    ! PB - 29/07/2009 - D'une part, on lit des albedomulch dans les paramètres généraux. D'autre part, quand le P_codepaillage
    !                   est égal 2, on lit un albdeomulch dans le fichier technique.
    !                   J'ai fait le choix de toujours transmettre aux routines l'albedomulch associé à la structure ITK.
    !                   Quand le P_codepaillage n'est pas égal à 2, alors on affecte dans l'itk la valeur de l'albedomulch
    !                   des paramètres généraux correspondant au P_typemulch choisi dans i'itk.
    !  if (itk(1)%P_codepaillage /= 2) itk(1)%albedomulch = pg%albedomulch(itk(1)%P_typemulch)
    ! DR 24/02/2011 on initialise couvermuch à P_couvermulchplastique pour les paillage plastique
!      if (itk(ipl)%P_codepaillage == 2) sc%couvermulch = itk(ipl)%P_couvermulchplastique

      ! TODO: on utilise la variable couvermulch de itk plutot que celle de sol. A voir en cas d'inversion de dominance.
      sc%cintermulch = 0.

! DR 28/03/2013
!      itk(1)%nouvre2 = itk(1)%P_julouvre2 - sc%P_iwater + 1
!      itk(1)%nouvre3 = itk(1)%P_julouvre3 - sc%P_iwater + 1
      sc%nouvre2 = itk(1)%P_julouvre2 - sc%P_iwater + 1
      sc%nouvre3 = itk(1)%P_julouvre3 - sc%P_iwater + 1
!      sc%codabri = itk(1)%P_codabri
!      sc%surfouvre = itk(1)%P_surfouvre
!      sc%surfouvre2 = itk(1)%P_surfouvre2
!      sc%surfouvre3 = itk(1)%P_surfouvre3
!      sc%transplastic = itk(1)%P_transplastic

      do ipl = 1, sc%P_nbplantes
        if (itk(ipl)%P_codcueille == 1) itk(ipl)%P_nbcueille = 1
        if (itk(ipl)%P_nbcueille == 2 .and. p(ipl)%P_codeindetermin == 1) then
          call EnvoyerMsgHistorique(333)
          !stop
          call exit(9)
        endif
        p(ipl)%nbrecolte = 1
        ! DR 09/03/2016 on passe le nb d'eclaircissage possible à 10
        do k=1,itk(ipl)%P_nb_eclair
            p(ipl)%neclair(k) = itk(ipl)%P_juleclair(k) - sc%P_iwater + 1
        enddo
        if (itk(ipl)%P_codeclaircie == 2 .and. p(ipl)%P_codeindetermin == 1) then
          call EnvoyerMsgHistorique(334)
          !stop
          call exit(9)
        endif

        p(ipl)%biorognecum(:)  = 0.
        p(ipl)%lairognecum(:)  = 0.
        p(ipl)%laieffcum(:)    = 0.
        p(ipl)%bioeffcum(:)    = 0.

        p(ipl)%nrogne   = itk(ipl)%P_julrogne   - sc%P_iwater + 1
        p(ipl)%neffeuil = itk(ipl)%P_juleffeuil - sc%P_iwater + 1
        p(ipl)%ntaille  = itk(ipl)%P_jultaille  - sc%P_iwater + 1

      end do

! ----------------------------------------------------------
! LES DATES
! ----------------------------------------------------------

! ** changement de calendrier pour les stades lus
! -- variable introuvable -- ndebreserve  = 0
      do ipl =  1, sc%P_nbplantes
        p(ipl)%nplt         = sc%iplt(ipl) - sc%P_iwater + 1
        p(ipl)%nger         = 0
        p(ipl)%nlev         = 0
        p(ipl)%namf         = 0
        p(ipl)%nlax         = 0
        p(ipl)%ndrp         = 0
        p(ipl)%nsen         = 0
        p(ipl)%nlan         = 0
        p(ipl)%nmat         = 0
        p(ipl)%nrec         = 0
        p(ipl)%naer(:)      = 0
        p(ipl)%nnou         = 0
        p(ipl)%nflo         = 0
        p(ipl)%nstopfeuille = 0
        p(ipl)%ndebdes      = 0
        p(ipl)%nrecalpfmax  = 0

      ! cas des vernalisantes (herbacées)
        if (p(ipl)%P_codeperenne == 2) then
          if (p(ipl)%P_codebfroid == 1) p(ipl)%P_ifindorm = 1
          if (p(ipl)%P_codebfroid == 2 .and. sc%P_culturean == 1) p(ipl)%P_ifindorm = 1

        ! ML le 18/10/05 je propose de n empecher la dormance en debut de simulation
        ! -- dans le cas de simulations sur 1 seule année, QUE SI LA FIN DE DORMANCE
        ! -- EST CALCULEE PAR BIDABE OU RICHARDSON ET NON SI ELLE EST FORCEE
        ! DR et IGc ces 2 lignes se servent à rien les 2 varaibles sont recalculées en dessosu
        !  if (p(ipl)%P_codebfroid == 3 .and. sc%P_culturean == 1 .and. p(ipl)%P_codedormance > 1) p(ipl)%P_ifindorm = 1
        !  p(ipl)%nfindorm = p(ipl)%P_ifindorm - sc%P_iwater + 1
        endif

      ! cas de dormance Richardson
        if (p(ipl)%P_codebfroid == 3 .and. p(ipl)%P_codedormance == 2) then
          p(ipl)%ndebdorm = 0
          p(ipl)%nfindorm = 0
          p(ipl)%P_idebdorm = 0
          p(ipl)%P_ifindorm = 0
        endif

      ! cas Bidabe
        if (p(ipl)%P_codebfroid == 3 .and. p(ipl)%P_codedormance == 3) then
          p(ipl)%ndebdorm = p(ipl)%P_idebdorm - sc%P_iwater + 1
          p(ipl)%nfindorm = 0
          p(ipl)%P_ifindorm = 0
        endif

      ! cas forçage dormance
        if (p(ipl)%P_codebfroid == 3 .and. p(ipl)%P_codedormance == 1) then
          if (p(ipl)%P_ifindorm < sc%P_iwater) then
            call EnvoyerMsgHistorique(335)
            !stop
            call exit(9)
          endif
          p(ipl)%nfindorm = p(ipl)%P_ifindorm - sc%P_iwater + 1
          p(ipl)%nplt = p(ipl)%nfindorm
          sc%iplt(ipl) = p(ipl)%P_ifindorm
          p(ipl)%P_idebdorm = 0
        endif
        p(ipl)%nrecbutoir = itk(ipl)%P_irecbutoir - sc%P_iwater + 1
        p(ipl)%nlevobs = 999
        p(ipl)%namfobs = 999
        p(ipl)%nlaxobs = 999
        p(ipl)%ndrpobs = 999
        p(ipl)%nsenobs = 999
        p(ipl)%nlanobs = 999
        p(ipl)%nmatobs = 999
        p(ipl)%nrecobs = 999
        p(ipl)%nfloobs = 999
        p(ipl)%ndebdesobs = 999


        if (sc%numcult == 1 .and. p(ipl)%P_codetemp == 2) then
          p(ipl)%P_stlevamf(itk(ipl)%P_variete) = p(ipl)%P_stlevamf(itk(ipl)%P_variete) * p(ipl)%P_coeflevamf
          p(ipl)%P_stamflax(itk(ipl)%P_variete) = p(ipl)%P_stamflax(itk(ipl)%P_variete) * p(ipl)%P_coefamflax
          p(ipl)%P_stlaxsen(itk(ipl)%P_variete) = p(ipl)%P_stlaxsen(itk(ipl)%P_variete) * p(ipl)%P_coeflaxsen
          p(ipl)%P_stsenlan(itk(ipl)%P_variete) = p(ipl)%P_stsenlan(itk(ipl)%P_variete) * p(ipl)%P_coefsenlan
          p(ipl)%P_stlevdrp(itk(ipl)%P_variete) = p(ipl)%P_stlevdrp(itk(ipl)%P_variete) * p(ipl)%P_coeflevdrp
          p(ipl)%P_stdrpmat(itk(ipl)%P_variete) = p(ipl)%P_stdrpmat(itk(ipl)%P_variete) * p(ipl)%P_coefdrpmat
          p(ipl)%P_dureefruit(itk(ipl)%P_variete) = p(ipl)%P_dureefruit(itk(ipl)%P_variete) * p(ipl)%P_coefdrpmat
          p(ipl)%P_stflodrp(itk(ipl)%P_variete) = p(ipl)%P_stflodrp(itk(ipl)%P_variete) * p(ipl)%P_coefflodrp
          p(ipl)%P_stdordebour = p(ipl)%P_stdordebour * p(ipl)%P_coeflevamf
          p(ipl)%P_stdrpdes(itk(ipl)%P_variete) = p(ipl)%P_stdrpdes(itk(ipl)%P_variete) * p(ipl)%P_coefdrpmat
        endif

      ! affectation des dates de stades observés
      ! PB - 15/03/2005 - en enchainement de pérennes on ne force pas les stades
        if (pg%P_codeinitprec == 2 .and. p(ipl)%P_codeperenne == 2) then
            itk(ipl)%P_codestade = 0
            call EnvoyerMsgHistorique('')
            call EnvoyerMsgHistorique(443)
            call EnvoyerMsgHistorique('')
        endif


        if (itk(ipl)%P_codestade == 1) then
          if (itk(ipl)%P_ilev /= 999) p(ipl)%nlevobs = itk(ipl)%P_ilev - sc%P_iwater + 1
          if (itk(ipl)%P_iamf /= 999) p(ipl)%namfobs = itk(ipl)%P_iamf - sc%P_iwater + 1
          if (itk(ipl)%P_ilax /= 999) p(ipl)%nlaxobs = itk(ipl)%P_ilax - sc%P_iwater + 1
          if (itk(ipl)%P_isen /= 999) p(ipl)%nsenobs = itk(ipl)%P_isen - sc%P_iwater + 1
          if (itk(ipl)%P_ilan /= 999) p(ipl)%nlanobs = itk(ipl)%P_ilan - sc%P_iwater + 1
          if (itk(ipl)%P_idrp /= 999) p(ipl)%ndrpobs = itk(ipl)%P_idrp - sc%P_iwater + 1
          if (itk(ipl)%P_imat /= 999) p(ipl)%nmatobs = itk(ipl)%P_imat - sc%P_iwater + 1
          if (itk(ipl)%P_irec /= 999) p(ipl)%nrecobs = itk(ipl)%P_irec - sc%P_iwater + 1
          if (itk(ipl)%P_iflo /= 999) p(ipl)%nfloobs = itk(ipl)%P_iflo - sc%P_iwater + 1

        ! DR et ML le 29/01/08 (lendemain de l'anniversaire de notre président chéri)
        ! sachant qu'on a des problèmes quand on force DRP trop précocément sans forcer FLO
        ! c'est-à-dire avant que stlevflo soit accompli, alors on force FLO automatiquement
          if (itk(ipl)%P_idrp /= 999 .and. itk(ipl)%P_iflo == 999) p(ipl)%nfloobs = p(ipl)%ndrpobs
        endif

      end do



    ! initialisation climatique
      sc%posibsw = .true.
      sc%posibpe = .true.
      sc%Emulch      = 0.
      sc%mouillmulch = 0.

! ---------------------------------------------------------
! LA PLANTE
! ---------------------------------------------------------

      sc%albedolai     = 0.

      do ipl = 1, sc%P_nbplantes

        ! DR 22/02/08 plus necessaire il vaut mieux renseigner les 4 parametres
        !        if (codtefcroi == 1) then
        !          P_temin = P_tcmin
        !          P_temax = P_tcmax
        !        endif

! 25/04/2012 DR et IGC if faut tester si on a activé l'eclaicissage
!        if (sc%numcult > 1) p(ipl)%P_nbinflo = p(ipl)%P_nbinflo + itk(ipl)%P_nbinfloecl
        !if (sc%numcult > 1 .and. itk(ipl)%P_codeclaircie == 2) p(ipl)%P_nbinflo = p(ipl)%P_nbinflo + itk(ipl)%P_nbinfloecl
        if (sc%numcult > 1 .and. itk(ipl)%P_codeclaircie == 2) p(ipl)%P_nbinflo = p(ipl)%P_nbinflo + itk(ipl)%P_nbinfloecl(1)
                p(ipl)%durvieI = p(ipl)%P_ratiodurvieI * p(ipl)%P_durvieF(itk(ipl)%P_variete)

        p(ipl)%cumdevfr       = 0.
        p(ipl)%jdepuisrec     = 0
        p(ipl)%nbj0remp       = 0
        p(ipl)%nbfruit        = 0.
        p(ipl)%compretarddrp  = 0
        p(ipl)%QNplanteres    = 0.
        p(ipl)%masectot       = 0.
! DR 170106 prairie
        p(ipl)%QNplanteCtot   = 0.
! Modif Bruno juin 2014
        p(ipl)%rendementsec   = 0.
! EC 06/08/2012 ajout d'une variable quantité de N exportée pour les cultures fauchées
        p(ipl)%QNplantefauche = 0.

        p(ipl)%dltaisen(:)    = 0.
        p(ipl)%dltaisenat(:)  = 0.
        p(ipl)%dltamsen(:)    = 0.
        p(ipl)%sla(:)         = p(ipl)%P_slamax
        p(ipl)%deltares(:)    = 0.
        p(ipl)%masecveg(:)    = 0.
        p(ipl)%mafeuil(:)     = 0.

        p(ipl)%maenfruit(:)   = 0.
        p(ipl)%nbgrains(:)    = 0.
! DR 14/04/2016
        p(ipl)%msneojaune(:)  = 0.
        p(ipl)%somsenreste(:) = 0.
        p(ipl)%hauteur(:)     = 0.
        ! 12/02/2015 DR et VG for the vine we start the "hauteur" to hautbase
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!        if(p(ipl)%P_codeplante=='vig')then
        if(p(ipl)%P_codeplante==22)then
              p(ipl)%hauteur    =  p(ipl)%P_hautbase
        endif
        p(ipl)%pgraingel(:)   = 0.
        p(ipl)%fpft(:)        = 0.
        p(ipl)%mafeuiltombe(:)= 0.
        p(ipl)%teaugrain(:)   = p(ipl)%P_h2ofrvert

    ! NB le 07/07/2006 pour autoriser non effet stress hydrique sur senescence
    !        if (P_rapsenturg <= 0.) P_rapsenturg = 0.001

        p(ipl)%cu(:)          = 0.
        !!!MODIF HISAFE 4 : suppression dimension temporelle
        !!!p(ipl)%pfeuil(:,:)    = 0.
        p(ipl)%pfeuil(:)    = 0.

    ! DR 09/07/07 on iniitialise durvie pour la prairie à voir avec marie
        p(ipl)%durvie(:,:) = 0.
    ! DR 12/08/08 deplacé ici
        p(ipl)%irazo(:,:)  = 0.


        p(ipl)%nfruit(:,:)   = 0
        p(ipl)%nfruitv(:,:)  = 0
        p(ipl)%pdsfruit(:,:) = 0.

        p(ipl)%masecneo(:)    = 0.
      ! on remet masecneo à zéro, bonne ou mauvaise idée ?
      ! TODO: vérifier l'impact de cet ajout
        p(ipl)%masecneov(:)    = 0.

        p(ipl)%ndebsenrac     = 0
        p(ipl)%nsencourprerac = 0
        p(ipl)%somtemprac     = 0.
        p(ipl)%somfeuille     = 0.

        if (sc%numcult == 1) then
          if (p(ipl)%P_codeindetermin == 1) then
            p(ipl)%stdrpmatini = p(ipl)%P_stdrpmat(itk(ipl)%P_variete)
          else
            p(ipl)%P_stdrpmat(itk(ipl)%P_variete) = p(ipl)%P_dureefruit(itk(ipl)%P_variete)
            p(ipl)%stdrpmatini = p(ipl)%P_dureefruit(itk(ipl)%P_variete)
          endif
        else
          p(ipl)%P_stdrpmat(itk(ipl)%P_variete) = p(ipl)%stdrpmatini
        endif

        p(ipl)%dltags(:)   = 0.
! 12/08/08 les irazo on ete deplacé plus haut pour reinitialisation
!        irazo(:,n)    = 0.

      ! DR et ML et SYL 15/06/09
      ! ************************
      ! introduction de la fin des modifications de Sylvain (nadine et FR)
      ! dans le cadre du projet PERMED
      ! DR et ML et SYL 16/06/09
      ! on ajoute la condition sur P_codedyntalle
      ! ####
      ! SYL 06/09/07 initialisation de densitemax
        if (t%P_codedyntalle(ipl) == 1) then
          p(ipl)%densitemax = t%P_MaxTalle(ipl)
        endif
      ! ####
      ! DR et ML et SYL 15/06/09 FIN introduction de la fin des modifications de Sylvain


       p(ipl)%mortreserve=0   !DR 20/09/2012 TODO je l'initialise ici ?


      end do



      soil%itrav1   = 0
      soil%itrav2   = 0

    ! code pour action de la vernalisation dans le calcul de ulai
      pg%codeulaivernal = 1

      do ipl = 1, sc%P_nbplantes
        p(ipl)%cinterpluie = 0.

      ! recalcul des parcours (temporaire)
        p(ipl)%stlevflo = p(ipl)%P_stlevdrp(itk(ipl)%P_variete) - p(ipl)%P_stflodrp(itk(ipl)%P_variete)
        if (sc%numcult > 1) then
          p(ipl)%P_stlevamf(itk(ipl)%P_variete) = p(ipl)%stlevamf0
          p(ipl)%P_stamflax(itk(ipl)%P_variete) = p(ipl)%stamflax0
          p(ipl)%P_stlaxsen(itk(ipl)%P_variete) = p(ipl)%stlaxsen0
          p(ipl)%P_stsenlan(itk(ipl)%P_variete) = p(ipl)%stsenlan0
          p(ipl)%P_stlevdrp(itk(ipl)%P_variete) = p(ipl)%stlevdrp0
          p(ipl)%P_stflodrp(itk(ipl)%P_variete) = p(ipl)%stflodrp0
          p(ipl)%P_stdrpdes(itk(ipl)%P_variete) = p(ipl)%stdrpdes0
        ! DR et ML 05/05/08 on le reinitialise aussi sinon pb calcul de la date
        ! de floraison apres mort de la plante
          p(ipl)%stlevflo = p(ipl)%stlevdrp0 - p(ipl)%stflodrp0

        endif

    ! affectation des sommes
        p(ipl)%stlevamf0 = p(ipl)%P_stlevamf(itk(ipl)%P_variete)
        p(ipl)%stamflax0 = p(ipl)%P_stamflax(itk(ipl)%P_variete)
        p(ipl)%stlaxsen0 = p(ipl)%P_stlaxsen(itk(ipl)%P_variete)
        p(ipl)%stsenlan0 = p(ipl)%P_stsenlan(itk(ipl)%P_variete)
        p(ipl)%stlevdrp0 = p(ipl)%P_stlevdrp(itk(ipl)%P_variete)
        p(ipl)%stflodrp0 = p(ipl)%P_stflodrp(itk(ipl)%P_variete)
        p(ipl)%stdrpdes0 = p(ipl)%P_stdrpdes(itk(ipl)%P_variete)
        if (sc%numcult > 1) then
          p(ipl)%P_stlevamf(itk(ipl)%P_variete) = p(ipl)%stlevamf0
          p(ipl)%P_stamflax(itk(ipl)%P_variete) = p(ipl)%stamflax0
          p(ipl)%P_stlaxsen(itk(ipl)%P_variete) = p(ipl)%stlaxsen0
          p(ipl)%P_stsenlan(itk(ipl)%P_variete) = p(ipl)%stsenlan0
          p(ipl)%P_stlevdrp(itk(ipl)%P_variete) = p(ipl)%stlevdrp0
          p(ipl)%P_stflodrp(itk(ipl)%P_variete) = p(ipl)%stflodrp0
          p(ipl)%P_stdrpdes(itk(ipl)%P_variete) = p(ipl)%stdrpdes0
        endif

        p(ipl)%cumrg       = 0.
        p(ipl)%cumraint    = 0.

        p(ipl)%somcourdrp  = 0.
        p(ipl)%somcourno   = 0.

! cet indice n'existe pas :        p(ipl)%surf(AOAS)  = 1.
        p(ipl)%surf(AS)    = 1.
        p(ipl)%surf(AO)    = 0.

        p(ipl)%mafeuilverte(:) =  0.
        p(ipl)%pfeuiljaune(:) = 0.
        p(ipl)%ptigestruc(:)  = 0.
        p(ipl)%penfruit(:)    = 0.
        p(ipl)%preserve(:)    = 0.
    ! ML le 25/06/07 mafeuiljaune et consors ne sont pas initialisés à zero
    ! -- ce qui pose problème en cas d'enchaînement donc je les rajoute
        p(ipl)%mafeuiljaune(:) =  0.
        p(ipl)%matigestruc(:) = 0.
        p(ipl)%mareserve(:)   = 0.
        p(ipl)%pfeuilverte(:,0) = 0.

    ! ML le 06/07/07 mafeuilp doit également être réinitialisé
    ! -- sinon problème en cas d'enchaînement donc je le rajoute
        p(ipl)%mafeuilp(:) =  0.

    ! DR 10/07/07 on initialise tursla pour la prairie à voir avec marie
        p(ipl)%tursla(:) = 1.

        if (sc%numcult == 1) then
          itk(ipl)%P_orientrang = itk(ipl)%P_orientrang / 180. * 3.1416
          p(ipl)%P_envfruit = p(ipl)%P_envfruit * p(ipl)%P_pgrainmaxi(itk(ipl)%P_variete) * 1.e-2
        endif

        p(ipl)%coeflev = 1.

    ! DR 11/02/08 la densité
    ! dr 27/08/07 maintenant que P_densitesem est le parametre qui ne bouge plus
    ! on peut initialiser densite ici
        p(ipl)%densite = itk(ipl)%P_densitesem
        p(ipl)%densitelev = p(ipl)%densite
        p(ipl)%densiteger = p(ipl)%densite
    ! DR 12/09/2012 j'ajoute les densite equivalentes en initialisation
        p(ipl)%densiteequiv = p(ipl)%densite
!        p(ipl)%densiteequiv = p(ipl)%densite


        p(ipl)%pfmax = p(ipl)%P_pgrainmaxi(itk(ipl)%P_variete)

        p(ipl)%mouill(:)      = 0.
        p(ipl)%sourcepuits(:) = 1.
        p(ipl)%splai(:)       = 1.

      ! test sur les paramètres sourcepuits
        if (p(ipl)%P_splaimin == p(ipl)%P_splaimax .or. p(ipl)%P_spfrmin == p(ipl)%P_spfrmax) then
          call EnvoyerMsgHistorique(337)
          !stop
          call exit(9)
        endif

    ! offrnodu
        p(ipl)%ndno   = 0
        p(ipl)%nfno   = 0
        p(ipl)%nfvino = 0
        p(ipl)%fixpotfno = 0.0

      end do



      sc%anit(:) = 0.
      sc%airg(:) = 0.
      sc%tauxcouv(:) = 0.

!!!MODIF HISAFE 11 : Supression code inutile
!!!Fait deconner dès que l'on a plus de 2 cellules de culture
!!!      if (sc%numcult == 1) then
!!!        pg%P_coefb = pg%P_coefb / 100. ! TODO : on modifie un paramètre général qui normalement ne doit pas être modifié, à dupliquer !
!!!        soil%P_humcapil = soil%P_humcapil / 10.
!!!      endif

    ! initialisations stress
      if (sc%P_nbplantes > 1 .and. ipl > 1) then
        ! pour CAS, et on suppose que l'on passe en ipl = 2 APRES ipl = 1
        sc%delta = max(p(2)%P_extin - 0.2, 0.)
      else
        ! en culture pure
        sc%delta = max(p(1)%P_extin - 0.2, 0.)
      endif

      sc%xmlch1 = 0.
      sc%xmlch2 = 0.
      sc%supres = 0.
      sc%stoc   = 0.
      sc%nstoc  = 0

    ! variables cumulées
      sc%cestout         = 0.
      sc%cpreciptout     = 0.
      sc%cpluie          = 0.
      sc%toteaunoneffic  = 0.
      sc%totir           = 0.
! DR 26/01/2016 ces varaibles avaient ete cree pour macsur , elles etaient reiunitialisées implicitement par les usms en rotation
! pour Agmip en annees enchainées c'est plus bon
      sc%drain_from_plt = 0.
      sc%leaching_from_plt = 0.
      sc%runoff_from_plt = 0.
      sc%Nmineral_from_plt = 0.
      sc%Nvolat_from_plt = 0.
      sc%QNdenit_from_plt = 0.
! DR 26/01/20016

      sc%etz(:)  = 0.
      sc%anox(:) = 0.


      do ipl = 1, sc%P_nbplantes
        p(ipl)%swfac(:)  = 1.
        p(ipl)%turfac(:) = 1.
        p(ipl)%senfac(:) = 1.


        p(ipl)%totpl  = 0.
        p(ipl)%nst1   = 0
        p(ipl)%nst2   = 0
        p(ipl)%str1   = 0
        p(ipl)%str2   = 0
        p(ipl)%stu1   = 0
        p(ipl)%stu2   = 0
        p(ipl)%inn1   = 0.
        p(ipl)%inn2   = 0.
    ! DR 12/10/06 on initialise le cumul de exofac sinon pb
        p(ipl)%exofac1 = 0.
        p(ipl)%exofac2 = 0.

        p(ipl)%diftemp1 = 0.
        p(ipl)%diftemp2 = 0.

! DR 12/04/2011 on ajoute les stress etr/etm et tem/etr
        p(ipl)%etr_etm1 = 0.
        p(ipl)%etm_etr1 = 0.
        p(ipl)%etr_etm2 = 0.
        p(ipl)%etm_etr2 = 0.


        p(ipl)%gel1   = 1.
        p(ipl)%gel2   = 1.
        p(ipl)%gel3   = 1.
        p(ipl)%mortplante = 0
        p(ipl)%mortplanteN = 0

        p(ipl)%fstressgel = 1.
        p(ipl)%fgelflo  = 1.
        p(ipl)%fgellev  = 1.
        p(ipl)%gelee = .FALSE.
        p(ipl)%nbjgel = 0
        p(ipl)%znonli  = p(ipl)%zrac


      ! variables cumulées
        p(ipl)%cet       = 0.
        p(ipl)%ces       = 0.
        p(ipl)%cep       = 0.
        p(ipl)%cetm      = 0.
        p(ipl)%cprecip   = 0.
        p(ipl)%somger    = 0.
        p(ipl)%cdemande  = 0.
        p(ipl)%crg       = 0.
        p(ipl)%ctmoy     = 0.
        p(ipl)%ctcult    = 0.
   ! DR 29/12/2014 ajout de cumul de tcult max pour faire moyenne pour Giacomo Agmip Canopy temp
        p(ipl)%ctcultmax = 0.
        p(ipl)%cetp      = 0.

        p(ipl)%cum_et0   = 0.

        p(ipl)%qressuite = 0.
        p(ipl)%qressuite_tot = 0.
        p(ipl)%Nexporte = 0.      !< // OUTPUT // total of exported nitrogen // kgN.ha-1
        p(ipl)%Nrecycle = 0.      !< // OUTPUT // total of recycle nitrogen (unexported nitrogen at harvest + nitrogen from the fallen leaves) // kgN.ha-1
        p(ipl)%MSexporte = 0.      !< // OUTPUT // total of exported carbon // t.ha-1
        p(ipl)%MSrecycle = 0.     !< // OUTPUT // total of recycle carbon (unexported nitrogen at harvest + nitrogen from the fallen leaves) // t.ha-1
        p(ipl)%somudevair  = 0.
        p(ipl)%somudevcult = 0.
        p(ipl)%somupvtsem = 0.
        p(ipl)%p1000grain  = 0.

    ! DR 05/04/2006 initialisation pour repousse semis
     !!!MODIF HISAFE 6 : on remplace les tableaux de boolean
    !!!    sc%repoussesemis(ipl) = .FALSE.
    !!!    sc%repousserecolte(ipl) = .TRUE.
    sc%repoussesemis(ipl) = 0
    sc%repousserecolte(ipl) = 1

        p(ipl)%nbjpourdecisemis = 0
        p(ipl)%nbjpourdecirecolte = 0
    ! DR 05/03/08 on conserve la date de semis qu'on change si on est en decisionsemis
        sc%iplt(ipl) = itk(ipl)%P_iplt0

    ! DR 10/06/2013 en cas d'apport de residus avec beaucoup d'eau on a incrementé le nombre d'apports , faut le remettre à 0
        itk(ipl)%nap = sc%napini(ipl)
    ! DR 10/06/2013 je fais pareil pour les apports d'azote
        itk(ipl)%napN = sc%napNini(ipl)
    ! DR 17/06/2016 j'essaie  de faire pareil pour nbjres
        itk(ipl)%P_nbjres = sc%nbjresini(ipl)

    ! DR 20/04/06 initialisation des cumuls intercoupe
        p(ipl)%cescoupe = 0.0
        p(ipl)%cepcoupe = 0.0
        p(ipl)%cetmcoupe = 0.0
        p(ipl)%cetcoupe = 0.0
        p(ipl)%cprecipcoupe = 0.0


    ! pour l'enchainement des annees si on calcule les irrigations on doit
    ! remettre à zero les irrigations de la culture precedente
        if (itk(ipl)%P_codecalirrig == 1)  itk(ipl)%nap  = 0
        if (t%P_codecalferti == 1)  itk(ipl)%napN = 0


        p(ipl)%lracz(:)     = 0.
        p(ipl)%lracsenz(:)  = 0.
        p(ipl)%precrac(:)   = 0.
        p(ipl)%irrigprof(:) = 0.
        p(ipl)%racnoy(:)    = 0.

        if (p(ipl)%codeinstal <= 0) then
          !!!MODIF HISAFE 4 : suppression dimension temporelle
          !!!remplacé rl par rljour et rlveille
          !!!p(ipl)%rl(:,:)  = 0.
          p(ipl)%rljour(:)  = 0.
          p(ipl)%rlveille(:)  = 0.
          p(ipl)%drl(:,:) = 0.
        endif

        p(ipl)%izrac  = 1.
        p(ipl)%idzrac = 1.
        p(ipl)%exolai = 1.
        p(ipl)%exobiom =  1.

 ! 11/06/2013 les varaibles stress racinaire sont sorties pour Simtraces
        p(ipl)%efda  = 1.
        p(ipl)%efnrac_mean = 1.
        p(ipl)%humirac_mean = 1.

    ! domi - 11/04/03 - on initialise pas group et quand on enchaine ca pose pb
        p(ipl)%group  = 0

    ! initialisation de somme(upvt) pour le calcul des dates d'apport d'azote en upvt
        p(ipl)%somupvt = 0.0
    ! initialisation de somme(upvt) pour le calcul des dates d'apport d'eau en upvt
        p(ipl)%somupvtI = 0.0

    ! domi - 09/07/07 on reinitialise somtemp
        p(ipl)%somtemp = 0.0

! --------------------------------------------------------------
!  INITIALISATIONS
! --------------------------------------------------------------

    ! initialisation des stades pour les pérennes
        !!!MODIF HISAFE 1 : suppression des chaines de caractères
        !!!if  (p(ipl)%P_codeperenne == 2 .and. (p(ipl)%P_stade0 == 'snu' .or. p(ipl)%P_stade0 == 'plt')) then
        if  (p(ipl)%P_codeperenne == 2 .and. (p(ipl)%P_stade0 == 1 .or. p(ipl)%P_stade0 == 2)) then
          call EnvoyerMsgHistorique(338)
        !!!  p(ipl)%P_stade0 = 'dor'
          p(ipl)%P_stade0 = 3
        endif
        !!!if (p(ipl)%P_stade0 == 'plt') p(ipl)%P_stade0 = 'snu'
        if (p(ipl)%P_stade0 == 2) p(ipl)%P_stade0 = 1
        !!!if (p(ipl)%P_stade0 /= 'snu') then
        if (p(ipl)%P_stade0 /= 1) then
          p(ipl)%codeinstal = 1
        else
          p(ipl)%codeinstal = 0
        endif

    !!!MODIF HISAFE 1 : suppression des chaines de caractères
    !!!    if (p(ipl)%P_stade0 /= 'snu' .and. p(ipl)%P_stade0 /= 'lev'  .and.  &
    !!!        p(ipl)%P_stade0 /= 'amf' .and. p(ipl)%P_stade0 /= 'lax'  .and.  &
    !!!        p(ipl)%P_stade0 /= 'sen' .and. p(ipl)%P_stade0 /= 'drp'  .and.  &
    !!!        p(ipl)%P_stade0 /= 'dor') then
         if (p(ipl)%P_stade0 /= 1 .and. p(ipl)%P_stade0 /= 4  .and.  &
            p(ipl)%P_stade0 /= 5 .and. p(ipl)%P_stade0 /= 6  .and.  &
            p(ipl)%P_stade0 /= 12 .and. p(ipl)%P_stade0 /= 8  .and.  &
            p(ipl)%P_stade0 /= 3) then
          call EnvoyerMsgHistorique(83, p(ipl)%P_stade0)
          !stop
          call exit(9)
        endif

    ! etat trophique plante
        if (p(ipl)%codeinstal == 1 .and. p(ipl)%P_zrac0 == 0.) then
          call EnvoyerMsgHistorique(84)
          call EnvoyerMsgHistorique('P_zrac0 =  ',p(ipl)%P_zrac0)
          !stop
          call exit(9)
        endif
        if (p(ipl)%codeinstal == 1 .and. p(ipl)%P_coderacine == 2 .and.               &
            p(ipl)%P_densinitial(1) <= 0 .and. p(ipl)%P_densinitial(2) <= 0 .and.       &
            p(ipl)%P_densinitial(3) <= 0 .and. p(ipl)%P_densinitial(4) <= 0 .and.       &
            p(ipl)%P_densinitial(5) <= 0.) then
            call EnvoyerMsgHistorique(196)
            !stop
            call exit(9)
        endif

        if (p(ipl)%codeinstal == 1) then
        ! test si drp avant lax
          if (p(ipl)%P_stlevdrp(itk(ipl)%P_variete) <                                                        &
                       (p(ipl)%P_stlevamf(itk(ipl)%P_variete) + p(ipl)%P_stamflax(itk(ipl)%P_variete))) then
            drpavantlax = .true.
          else
            drpavantlax = .false.
          endif
          p(ipl)%nplt = 1
! DR 15/04/2016 pour les prairies qui sont des perennes herbacees on garde les stades au passage d'annees si on est en enchaine
! au contraire du cas des perennes ligneuses
!          if(p(ipl)%P_codeplante.ne.'fou')then
          p(ipl)%nlev = 1
!          else if(pg%P_codeinitprec.eq.1)then
!          p(ipl)%nlev = 1
!          endif
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!          if (p(ipl)%P_stade0 == 'dor') p(ipl)%nlev = 0
          if (p(ipl)%P_stade0 == 3) p(ipl)%nlev = 0
          p(ipl)%nger = 1
!          if (P_stade0 == 'dor') nger = 0

          p(ipl)%zrac = max(p(ipl)%P_zrac0,itk(ipl)%P_profsem)

          p(ipl)%fixpot(:) = 0.
          p(ipl)%fixreel(:) = 0.
          p(ipl)%offrenod(:) = 0.
          p(ipl)%fixmaxvar(:) = 0.

          p(ipl)%zracmax =  0.


! DR 05/01/2016 on teste la prise en compte des reserves
!!!!!!!!!!!!!!!!!!!!!!  bien voir ce que ca fait sur les autres plantes !!!!!!!!!!!!!!!!
!!!!  ACHTUNG !!!!
! 21/03/2016 pour la prairie cette modif pose pb, on la restreint à vigne et pommier
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!          if(p(ipl)%P_codeplante=='vig'.or.p(ipl)%P_codeplante=='pom')then
          if(p(ipl)%P_codeplante==22.or.p(ipl)%P_codeplante==23)then
              p(ipl)%P_masec0=p(ipl)%P_masec0 + p(ipl)%P_resperenne0
          endif

          p(ipl)%masec(:,0) = p(ipl)%P_masec0
          p(ipl)%magrain(:,0) = p(ipl)%P_magrain0

          if (p(ipl)%P_codelaitr == 1) then
            p(ipl)%lai(AS,0) = p(ipl)%P_lai0
            p(ipl)%lai(AO,0) = p(ipl)%P_lai0
            p(ipl)%lai(AOAS,0) = p(ipl)%P_lai0
          else
            sc%tauxcouv(0) = p(1)%P_lai0 ! TODO : à mettre ailleurs que dans une boule plante
          endif

          if (p(ipl)%P_codeperenne == 1) then
            p(ipl)%P_resperenne0 = 0.
          else
            p(ipl)%resperenne(:) = p(ipl)%P_resperenne0
            if (p(ipl)%P_QNplante0 <= 0) then
              call EnvoyerMsgHistorique(339)
            endif
          endif


!DR 14/04/2016 pour les pariries on stocke des varaibles plantes supplementaires dans le recup.tmp
! on reaffecte
!!!MODIF HISAFE 11 : Supression code inutile
!!!Ne nous sert à rien
!!!        if (pg%P_codeinitprec == 2 .and. sc%P_codesuite /= 0)then
!!!               p(ipl)%mafeuiltombe(:)=p(ipl)%mafeuiltombe0(:)
!!!               p(ipl)%msneojaune(:) =p(ipl)%msneojaune0(:)
!!!               p(ipl)%masecneo(:) =p(ipl)%masecneo0(:)
!!!               p(ipl)%mafeuiljaune(:)=p(ipl)%mafeuiljaune0(:)
!!!               p(ipl)%dltamsen(:) = p(ipl)%dltamsen0(:)
!!!        endif


        ! ajouts Bruno - 22/05/2012
        ! dr et ml 04/09/2014 on indexe sur ao et as pour les varaibles de l'abscission
          p(ipl)%QNplantetombe(:) = 0.
          p(ipl)%QCplantetombe(:) = 0.
!
          p(ipl)%QCrogne = 0.
          p(ipl)%QNrogne = 0.
          p(ipl)%QCplante = 0.



          p(ipl)%cumdltares(:) = 0.
          p(ipl)%QNplante(:,0) = p(ipl)%P_QNplante0


          if (p(ipl)%P_masec0 > 0) then
            if (p(ipl)%P_QNplante0 <= 0) then
              call EnvoyerMsgHistorique(85)
              p(ipl)%P_QNplante0 = p(ipl)%P_adil * p(ipl)%P_masec0 * 10.
              p(ipl)%QNplante(AS,0) = p(ipl)%P_QNplante0
              ! DR 10/01/2011 apres discussion avec paul
              ! pourrai introduire un bug ou des differneces de calcul car
              ! ici si QNplante = 0 et P_masec0 non nul on recalcule P_qnplante0 mais on l'affecte
              ! seulement à AO et donc quand apres on ralcule innlai on garde innlai =P_innmin
              ! comme Qnplante(0) est calculé sur les surfaces ( p%QNplante(0,n) = p%QNplante(AO,n) * p%surf(AO) + p%QNplante(AS,n) * p%surf(AS))
              ! on peut affecter la meme valeur à l'ombre et au soleil
              ! faudra enlever la mise à 0 et tester avec une cas
              p(ipl)%QNplante(AO,0) = 0.0
              !p(ipl)%QNplante(AO,0) = p(ipl)%QNplante(AS,0)
              ! TODO: quid de QNplante(AOAS) ?, DR 10/01/2011 il est recalculé dans cumaoas !
              ! DR 04/12/2014 il avait raison Paul , non dans cumaoas on le calcul pour n=1 !
              p%QNplante(0,0) = p%QNplante(AO,0) * p%surf(AO) + p%QNplante(AS,0) * p%surf(AS)
            endif

            p(ipl)%INN0(:) = p(ipl)%QNplante(:,0) / (p(ipl)%P_masec0 * 10. * (p(ipl)%P_adil * p(ipl)%P_masec0**(-p(ipl)%P_bdil)))
            p(ipl)%INN(:) = p(ipl)%INN0(:)
            p(ipl)%INNS(:) = max(p(ipl)%P_INNmin,p(ipl)%INN(:))
            p(ipl)%innlai(:) = max(p(ipl)%P_INNmin,p(ipl)%inn(:))
            p(ipl)%innsenes(:) = max(p(ipl)%P_INNmin,p(ipl)%inn(:))
            p(ipl)%innlai(AOAS) = min(p(ipl)%innlai(AO),p(ipl)%innlai(AS))

          else
            p(ipl)%inns(:)     = 1.
            p(ipl)%inn(:)      = 1.
            p(ipl)%innlai(:)   = 1.
            p(ipl)%innsenes(:) = 1.
          endif
        else
          p(ipl)%P_QNplante0    = 0.

          p(ipl)%lai(:,0)     = 0.
          p(ipl)%masec(:,0)   = 0.
          p(ipl)%QNplante(:,0) =  0

        ! dr 12/08/08 pas d'initialisationfrecup

          p(ipl)%QNgrain(:) = 0.

          p(ipl)%inns (:)     = 1.
          p(ipl)%innsenes(:)  = 1.
          p(ipl)%innlai(:)    = 1.
          p(ipl)%inn(:)       = 1.
        ! NB le 30/05/05
        ! dr et ml 04/09/2014 on indexe sur ao et as pour les varaibles de l'abscission
          p(ipl)%QCplantetombe(:) = 0.
          p(ipl)%QNplantetombe(:) = 0.
          p(ipl)%Crac = 0.
          p(ipl)%Nrac = 0.
          p(ipl)%QCrac = 0.
          p(ipl)%QNrac = 0.

        ! les3(emma) - 17/06/2004
          p(ipl)%somcourfauche = 0.
        !DR et FR 13/02/2015
          p(ipl)%reste_apres_derniere_coupe = 0.
          p(ipl)%dltamsen(:) = 0.

          p(ipl)%msneojaune(:) = 0.
!
        endif


      ! enregistrement dans le fichier history (TODO: on pourrait se passer du test sur le flag)
        if (iand(pg%P_flagEcriture,sc%ECRITURE_HISTORIQUE) >0 ) then
          call EnvoyerMsgHistorique('')
          call EnvoyerMsgHistorique(86)
          call EnvoyerMsgHistorique('codeinstal : ',p(ipl)%codeinstal)
          call EnvoyerMsgHistorique('masec(0) : ',p(ipl)%masec(:,0))
          call EnvoyerMsgHistorique('lai(0) : ',p(ipl)%lai(:,0))
          call EnvoyerMsgHistorique('P_QNplante0 : ',p(ipl)%QNplante(:,0))
          call EnvoyerMsgHistorique('P_zrac0 : ',p(ipl)%zrac)
          call EnvoyerMsgHistorique('P_stade0 : ',p(ipl)%P_stade0)
          call EnvoyerMsgHistorique('INN0 : ',p(ipl)%INN0(:))
          call EnvoyerMsgHistorique(5,'','(/,a,a,/)')
        end if

        !!!MODIF HISAFE 1 : suppression des chaines de caractères
        !!!if (p(ipl)%P_stade0 == 'amf') then
        if (p(ipl)%P_stade0 == 5) then
          p(ipl)%namf = 1
! ** domi 25/04/2002 compatibilite unix
          p(ipl)%ulai(1) = p(ipl)%P_vlaimax
          p(ipl)%P_stlevdrp(itk(ipl)%P_variete) = p(ipl)%P_stlevdrp(itk(ipl)%P_variete) - p(ipl)%P_stlevamf(itk(ipl)%P_variete)
          p(ipl)%P_stlevamf(itk(ipl)%P_variete) = 0.0
        endif

        !!!MODIF HISAFE 1 : suppression des chaines de caractères
        !!!if (p(ipl)%P_stade0 == 'lax') then
        if (p(ipl)%P_stade0 == 6) then
          p(ipl)%namf = 1
          p(ipl)%nlax = 1
        ! 25/04/2002 domi compatibilite unix (TODO: ???)
          p(ipl)%ulai(1) = 3.0
          if (drpavantlax) then
            p(ipl)%nflo = 1
            p(ipl)%ndrp = 1
            p(ipl)%P_stlevdrp(itk(ipl)%P_variete) = 0.
          else
            p(ipl)%P_stlevdrp(itk(ipl)%P_variete) = p(ipl)%P_stlevdrp(itk(ipl)%P_variete) &
                                              - p(ipl)%P_stlevamf(itk(ipl)%P_variete) &
                                              - p(ipl)%P_stamflax(itk(ipl)%P_variete)
          endif
          p(ipl)%P_stlevamf(itk(ipl)%P_variete) = 0.
          p(ipl)%P_stamflax(itk(ipl)%P_variete) = 0.
        endif

        !!!MODIF HISAFE 1 : suppression des chaines de caractères
        !!!if (p(ipl)%P_stade0 == 'sen') then
        if (p(ipl)%P_stade0 == 12) then
          p(ipl)%namf = 1
          p(ipl)%nlax = 1
          p(ipl)%nsen = 1
          p(ipl)%ndrp = 1
          p(ipl)%nflo = 1
          p(ipl)%P_nbjgrain = 1
        ! domi 25/04/2002 compatibilite unix (TODO: ???)
          p(ipl)%ulai(1)  = 3.
          p(ipl)%P_stdrpmat(itk(ipl)%P_variete) = p(ipl)%P_stlevdrp(itk(ipl)%P_variete) + p(ipl)%P_stdrpmat(itk(ipl)%P_variete) &
                                            - p(ipl)%P_stlevamf(itk(ipl)%P_variete) - p(ipl)%P_stamflax(itk(ipl)%P_variete) &
                                            - p(ipl)%P_stlaxsen(itk(ipl)%P_variete)
          p(ipl)%P_stlevamf(itk(ipl)%P_variete) = 0.
          p(ipl)%P_stamflax(itk(ipl)%P_variete) = 0.
          p(ipl)%P_stlaxsen(itk(ipl)%P_variete) = 0.
          p(ipl)%P_stlevdrp(itk(ipl)%P_variete) = 0.
          p(ipl)%P_stdrpdes(itk(ipl)%P_variete) = 0.
          if (p(ipl)%magrain(AS,0) <= 0 .or. p(ipl)%magrain(AO,0) <= 0) then
            call EnvoyerMsgHistorique(6)
            call EnvoyerMsgHistorique(87)
            !stop
            call exit(9)
          endif
        endif

        !!!MODIF HISAFE 1 : suppression des chaines de caractères
        !!!if (p(ipl)%P_stade0 == 'drp') then
        if (p(ipl)%P_stade0 == 8) then
          p(ipl)%namf = 1
          p(ipl)%nflo = 1
          p(ipl)%ndrp = 1
        ! domi 25/04/2002 compatibilite unix (TODO: ???)
          p(ipl)%ulai(1) = 3.0
          p(ipl)%P_nbjgrain = 1
          if (.not.drpavantlax) then
            p(ipl)%P_stlaxsen(itk(ipl)%P_variete) = p(ipl)%P_stlevamf(itk(ipl)%P_variete) + p(ipl)%P_stamflax(itk(ipl)%P_variete) &
                                              + p(ipl)%P_stlaxsen(itk(ipl)%P_variete) - p(ipl)%P_stlevdrp(itk(ipl)%P_variete)
            p(ipl)%P_stamflax(itk(ipl)%P_variete) = 0.
          else
            p(ipl)%P_stamflax(itk(ipl)%P_variete) = - p(ipl)%P_stlevdrp(itk(ipl)%P_variete) +                                     &
                                            p(ipl)%P_stlevamf(itk(ipl)%P_variete) + p(ipl)%P_stamflax(itk(ipl)%P_variete)
          endif
          p(ipl)%P_stlevamf(itk(ipl)%P_variete) = 0.
          p(ipl)%P_stlevdrp(itk(ipl)%P_variete) = 0.
          p(ipl)%P_stdrpdes(itk(ipl)%P_variete) = 0.
        endif


    ! initialisation des paramètres des courbes de dilution
    ! si option "plante isolée"
    ! NB le 11/09/05

        if (p(ipl)%P_codeplisoleN == 2) then

        !!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Plante.f90)
        !!!  t%bdilI(ipl) = (p(ipl)%P_bdil * log(p(ipl)%P_masecNmax) &
        !!!               - (log(p(ipl)%P_adil / p(ipl)%P_Nmeta))) / log(p(ipl)%P_masecNmax /  p(ipl)%P_masecmeta)
        !!!  t%adilI(ipl) = p(ipl)%P_Nmeta / ( p(ipl)%P_masecmeta**(-t%bdilI(ipl)))

          p(ipl)%bdilI = (p(ipl)%P_bdil * log(p(ipl)%P_masecNmax) &
                       - (log(p(ipl)%P_adil / p(ipl)%P_Nmeta))) / log(p(ipl)%P_masecNmax /  p(ipl)%P_masecmeta) 

          p(ipl)%adilI = p(ipl)%P_Nmeta / ( p(ipl)%P_masecmeta**(-p(ipl)%bdilI))

! DR 21/04/2011 P_Nres est renommé P_Nreserve car on garde Nres pour la quantité d'azote des residus au meme titre que Cres
          p(ipl)%P_adilmax =  p(ipl)%P_adil + p(ipl)%P_Nreserve / (p(ipl)%P_masecNmax**(-p(ipl)%P_bdil))

          !!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Plante.f90)
          !!!t%bdilmaxI(ipl) = log(p(ipl)%P_adilmax * p(ipl)%P_masecNmax**(-p(ipl)%P_bdil) / p(ipl)%P_Nmeta)
          !!!t%bdilmaxI(ipl) = - t%bdilmaxI(ipl) / log(p(ipl)%P_masecNmax /  p(ipl)%P_masecmeta)
          !!!t%adilmaxI(ipl) = p(ipl)%P_Nmeta / ( p(ipl)%P_masecmeta**(-t%bdilmaxI(ipl)))

          p(ipl)%bdilmaxI = log(p(ipl)%P_adilmax * p(ipl)%P_masecNmax**(-p(ipl)%P_bdil) / p(ipl)%P_Nmeta)
          p(ipl)%bdilmaxI = - p(ipl)%bdilmaxI / log(p(ipl)%P_masecNmax /  p(ipl)%P_masecmeta)
          p(ipl)%adilmaxI = p(ipl)%P_Nmeta / ( p(ipl)%P_masecmeta**(-p(ipl)%bdilmaxI))

        endif

      end do

! *----------------------------------------------* c
!            Initialisations sol
! *----------------------------------------------* c

      sc%ruisselt = 0.
      sc%exces(:) = 0.

    !  Cas n°1: P_codeinitprec = 2    (enchainement)


    if (pg%P_codeinitprec == 2 .and. sc%P_codesuite /= 0) then
    ! DR 03/06/08 pour l'enchainement des perennes on lit le recup.tmp
    ! au debut pour l'initialisation des variables plantes
        !--call Lecture_DonneesCyclePrecedent(sc,pg,p,itk,soil)  !,c,sta,t)

     else
    ! Cas n°2:   P_codeinitprec = 1   (reinitialisation)

      ! Variables AZOTE SOL
        Ch = 0.
        sc%Cr = 0.
        sc%Nr = 0.
        sc%Cb = 0.
        sc%Nb = 0.
        sc%Nb0 = 0.
        sc%Nr0 = 0.
        sc%Cb0 = 0.
        sc%Cr0 = 0.
        sc%Cbmulch0 = 0.
        sc%Nbmulch0 = 0.

        sc%Chum(1:sc%nbCouchesSol) = 0.
        sc%Chum(1:sc%nbCouchesSol) = 0.
        sc%Cres(1:sc%nbCouchesSol,1:pg%nbResidus) = 0.
        sc%Nres(1:sc%nbCouchesSol,1:pg%nbResidus) = 0.
        sc%Cbio(1:sc%nbCouchesSol,1:pg%nbResidus) = 0.
        sc%Nbio(1:sc%nbCouchesSol,1:pg%nbResidus) = 0.

! dr et ml 22/09/2011 pb de qressuite et qmulch non reinitialises à l'enchainement
        sc%qmulch   = 0.

        sc%hres(:) = 0.
        sc%Wb(:)   = 0.
        sc%kres(:) = 0.
        sc%NCbio   = 0.
! dr 21/04/2011 ajout de la mise à zero sinon soucis
        sc%Nnondec(:) = 0.
        sc%Cnondec(:) = 0.
        sc%Nmulchnd = 0. !Bruno- nouvelles variables-somme des quantités dans chaque résidu
        sc%Cmulchnd = 0.
        sc%Nmulchdec = 0.
        sc%Cbmulch = 0.
        sc%Nbmulch = 0.
        sc%Cmulchdec = 0.
        sc%Cbio(:,:) = 0.
        sc%Nbio(:,:) = 0.
        sc%Qminh = 0.
        sc%Qminr = 0.

      ! stock de N organique sur chaque cm (en kg/Ha)
        QNorg =  soil%P_Norg * soil%P_daf(1) * (1. - soil%P_cailloux(1) / 100.) * 1000.

      ! stock de N et C actif sur chaque cm (en kg/ha)
        Cactif = QNorg * (1. - pg%P_finert) / pg%P_Wh ! Bruno : on remplace Wh par CN ratio du sol
        Nactif = QNorg * (1. - pg%P_finert)
        ! Cactif = Nactif * soil%P_CsurNsol

        sc%Nhum(1:int(soil%P_profhum)) = Nactif
        sc%Chum(1:int(soil%P_profhum)) = Cactif
        Ch = sum(sc%Chum(1:int(soil%P_profhum)))

      ! stock de N actif  (Nhum, en kg/ha) sur tout le profil
      !  sc%Nhum = Ch * pg%P_Wh

      ! stock de N et C inerte (Nhumi, en kg/ha) sur tout le profil
        sc%Nhumi = QNorg * pg%P_finert * int(soil%P_profhum)
         ! Bruno   sc%Chumi = sc%Nhumi * soil%P_CsurNsol
         !sc%Chumi = sc%Nhumi / pg%P_Wh
        sc%Chumi = sc%Nhumi * soil%P_CsurNsol
      ! stock de N et C actif (Nhuma, en kg/ha) sur tout le profil
        sc%Nhuma = QNorg * (1-pg%P_finert )* int(soil%P_profhum)
         ! Bruno   * soil%P_CsurNsol
         !sc%Chuma = sc%Nhuma / pg%P_Wh
         sc%Chuma = sc%Nhuma * soil%P_CsurNsol

! ** stock de N (kg/ha) et C total (Chumt, en kg/ha) sur tout le profil
        sc%Nhumt = sc%Nhuma + sc%Nhumi
        sc%Chumt = sc%Chuma + sc%Chumi   ! modif Bruno on reste en kg/ha
!27/01/2016
    ! si on recalcule pour agmip , on teste pas
        AgMIP=.true.
        if (.not.AgMIP)then

        if (sc%P_codesuite == 1) then
          !ipl = 1
         !!! open(12,file = 'recup.tmp',status = 'unknown')
         !!! read(12,*) sc%nbjrecol0, sc%ifwater0, sc%nhe, sc%dernier_n
          if (sc%ifwater0 == sc%nbjrecol0) then
            if (sc%P_iwater /= 1) then
              call EnvoyerMsgHistorique(6)
              call EnvoyerMsgHistorique(3045)
              call EnvoyerMsgHistorique(3046)
              !stop
              call exit(9)
            endif
          else
            if (sc%P_iwater /= sc%ifwater0+1) then
              call EnvoyerMsgHistorique(6)
              call EnvoyerMsgHistorique(3045)
              call EnvoyerMsgHistorique(3046)
              !stop
              call exit(9)
            endif
          endif
         !!! close(12)
        endif
! 27/01/2016
        endif
   endif


!  write(5588,*)'dans initnonsol apres modif fin iwater',sc%P_iwater,'ifwater',sc%P_ifwater,sc%ifwater_courant,sc%maxwth

    ! mémorisation des stocks initiaux de MO du sol: C total, N total, Nactif
      sc%Chumt0 = sc%Chumt
      sc%Nhumt0 = sc%Nhumi + sc%Nhuma
      sc%Chuma0  = sc%Chuma
      sc%Nhuma0  = sc%Nhuma
    ! initialisation de la production de CO2
      sc%QCO2sol = 0. ! TODO: à mettre dans initsol !
      sc%QCapp = 0.
      sc%QNapp = 0.
      sc%QCresorg = 0.
      sc%QNresorg = 0.

    ! Initialisation des profils d'eau et d'azote





    !!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
     do i = 1, sc%NH
        !!!if (sc%Hinit(i) <= 0.) sc%Hinit(i) = soil%hcc(i)   ! TODO remplacer la boucle par une instruction WHERE sur tableau
        if (soil%Hinit(i) <= 0.) soil%Hinit(i) = soil%hcc(i)   ! TODO remplacer la boucle par une instruction WHERE sur tableau
      end do

    ! 1er cas : profil homogène dans chaque couche
      sc%nhe = 0
      do i = 1, sc%nh
        izmax = int(soil%P_epc(i))
        HR_dom(i)=0.
        azo_dom(i)=0.
        amm_dom(i)=0.
        do iz = 1,izmax
          sc%HUCC(sc%nhe+iz) = soil%HCC(i)  * soil%da(i) /10.
          sc%HUMIN(sc%nhe+iz) = soil%HMIN(i) * soil%da(i) /10.
          sc%dacouche(sc%nhe+iz) = soil%da(i)

        ! on n'initialise hur, nit et amm que pour la premiere culture et quand on n'enchaine pas

          !!!MODIF HISAFE 12 : Modif après détection bug
          !!!le test sc%numcult == 1 est toujours vrai donc on passait tjrs dans cette partie du code qui réinitialise le sol
          !!!alors qu'on ne veux pas puisque'on enchaine les années
          !!!if (pg%P_codeinitprec == 1 .or. sc%numcult == 1) then
          if (pg%P_codeinitprec == 1) then

            !!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
            !!!sc%hur(sc%nhe+iz)   = sc%Hinit(i)* soil%da(i) /10.
            sc%hur(sc%nhe+iz)   = soil%Hinit(i)* soil%da(i) /10.
            soil%nit(sc%nhe+iz) = soil%NO3init(i) / soil%P_epc(i)
            !!!soil%amm(sc%nhe+iz) = sc%NH4init(i) / soil%P_epc(i)
            soil%amm(sc%nhe+iz) = soil%NH4init(i) / soil%P_epc(i)
            HR_dom(i)=HR_dom(i)+sc%hur(sc%nhe+iz)
            azo_dom(i)=azo_dom(i)+soil%nit(sc%nhe+iz)
            amm_dom(i)=amm_dom(i)+soil%amm(sc%nhe+iz)

          endif
          sc%hurmini(sc%nhe+iz) = sc%hucc(sc%nhe+iz)
        end do

      ! write(71,*)'jour1 recup.tmp',i,soil%P_epc(i),HR_dom(i),azo_dom(i),amm_dom(i)


        sc%nhe = sc%nhe + izmax
      end do

    ! 2eme cas: profil non homogène lissé par une fonction spline
      if (sc%NH > 1) then
        if (pg%P_iniprofil == 1 .and. sc%numcult == 1 .and. (sc%P_codesuite == 0 .or. pg%P_codeinitprec == 1)) then
        ! Qeauinit = quantite d'eau dans chaque horizon (en mm)
          do i = 1,sc%NH
            !!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
            !!!Qeauinit(i) = sc%Hinit(i) * soil%da(i) / 10. * soil%P_epc(i)
            Qeauinit(i) = soil%Hinit(i) * soil%da(i) / 10. * soil%P_epc(i)
          end do

          call AJUPRO(sc%nh,soil%P_epc(1:sc%nh),soil%NO3init(1:sc%nh),sc%nhe,soil%nit(1:sc%nbCouchesSol),QEXC)

        ! pas d'ajustement si le profil initial est a la CC
          !!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
          !!!if (sc%Hinit(1) < soil%hcc(1)) then
          if (soil%Hinit(1) < soil%hcc(1)) then
            call AJUPRO(sc%NH,soil%P_epc(1:sc%nh),Qeauinit(1:sc%nh),sc%nhe,sc%hur(1:sc%nbCouchesSol),HEXC)
        ! NB 25/05/05 ajout pour eviter pb lissage profils
            do iz = 1,sc%nhe
              if (sc%hur(iz) <= 0.0) then
                call EnvoyerMsgHistorique(6)
                call EnvoyerMsgHistorique(520)
                !stop
                call exit(9)
              endif
            end do
          endif
        endif
      endif

      if (soil%P_humcapil >= sc%hucc(sc%nhe)) call EnvoyerMsgHistorique(336)

! *---------------------------------------------* c
! **         Fin des initialisations sol       ** c
! *********************************************** c



    ! pas de sol supérieur à 10 m, soit 1000 couches de 1 cm
      if (soil%profsol > 1000.) then
        call EnvoyerMsgHistorique(6)
        call EnvoyerMsgHistorique(180, soil%profsol)
        !stop
        call exit(9)
      endif


      soil%profcalc = 0

! DR 10/05/06
      sc%recolte1 = .TRUE.
      sc%P_datefin = .FALSE.

! DR 24/06/08
! dr 21/09/09 on supprime
!--      sc%apport_mini_semis = 0.

! DR 28/02/2011 initialisation de irmulch pour le premier jour passage de etatsurf avant mineral
      sc%irmulch=1

      do ipl = 1, sc%P_nbplantes
        sc%numdateprof = 1

        if (pg%P_codeprofmes == 1) then
          soil%profcalc = max(soil%profcalc,int(itk(ipl)%P_profmes))
        else
          soil%profcalc = int(soil%profsol)
        endif
      ! le calcul des reserves (resmes,azomes) doit etre fait
      ! sur une profondeur inferieure a la profondeur du sol
        if (itk(ipl)%P_profmes > soil%profsol) then
          call EnvoyerMsgHistorique(88,itk(ipl)%P_profmes)
          call EnvoyerMsgHistorique(89,soil%profsol)
          call EnvoyerMsgHistorique(181)
          itk(1)%P_profmes = soil%profsol
        endif

    ! DR et FR 23/06/2015 on nettoie initnonsol en isolant les initialisations prairies dans une subroutine independante
    ! DR et FR  24/06/2015 on conditionne l'appel sur l'initialisation des fauches aux cultures fauchees
!!!MODIF HISAFE 1 : suppression des chaines de caractères
 !!!  if(p(ipl)%P_codeplante == 'fou') call init_prairie(sc,p,itk,pg)
      if(p(ipl)%P_codeplante == 2) call init_prairie(sc,p,itk,pg)

      ! initialisation des plastochrones
      ! DR 22/08/06 pas plastochrones phyllothermes
        if (p(ipl)%P_nbfeuilplant <= 0 .or. p(ipl)%codeinstal /= 1) then
          p(ipl)%nbfeuille = 0
        else
          p(ipl)%nbfeuille = p(ipl)%P_nbfeuilplant
        endif
        p(ipl)%somfeuille = 0.

      ! si on est en taux de recouvrement on a P_codebeso = 1
      ! et pas d'interception de la pluie
        if (p(ipl)%P_codelaitr == 2) then
          if (p(ipl)%P_codebeso /= 1) then
            call EnvoyerMsgHistorique(191)
            call EnvoyerMsgHistorique(193)
            p(ipl)%P_codebeso = 1
          endif
          if (p(ipl)%P_codeintercept == 1) then
            call EnvoyerMsgHistorique(191)
            call EnvoyerMsgHistorique(192)
            p(ipl)%P_codeintercept = 2
          endif
          if (itk(ipl)%P_codefauche /= 2) then
            call EnvoyerMsgHistorique(191)
            call EnvoyerMsgHistorique(194)
            itk(ipl)%P_codefauche = 2
          endif

          if (sta%P_codecaltemp /= 1) then
            call EnvoyerMsgHistorique(191)
            call EnvoyerMsgHistorique(195)
            sta%P_codecaltemp = 1
          endif

          p(ipl)%P_codlainet = 1
        ! PB - 21/03/2005 - déplacé depuis biomaer
        ! On impose une loi de Beer quand P_codelaitr = 2 (option taux de couverture)
          p(ipl)%P_codetransrad = 1
        endif


      ! pour régler un pb avec les sorties
        p(ipl)%fapar(:) = 0.0

      ! DR le 12/07/06 on rajoute un test sur la valeur de tcxstp
        if (p(ipl)%P_tcxstop < 100 .and. p(ipl)%P_tdmax >= p(ipl)%P_tcxstop) then
          call EnvoyerMsgHistorique(6)
          call EnvoyerMsgHistorique(446)
          !stop
          call exit(9)
        endif


      ! ** DR et FR le 22/05/07 pour la prairie si on est en reinitialisation , on reinitialise
      ! aussi la repousse des fauches
      ! dr 11/02/08 on remonte ce test car fauchediff est indexé sur la plante
        if (pg%P_codeinitprec == 1 .or. sc%numcult == 1) then
          p(ipl)%fauchediff = .FALSE.
      ! DR et FR 23/06/2015 si on est en reset (codeinitprtec=1) et qu'on simule une serie climatique on doir repartir comme une annee independante
          p(ipl)%msresjaune(:) = 0.0
          p(ipl)%msneojaune(:) = 0.0
          p(ipl)%dltamstombe(:) = 0.0
          p(ipl)%dltaisen(:) = 0.0
        endif


      end do
! DR 29/04/2013 j'ajoute un compteur pour le calcul forcé de priestley taylor en cas de donnees manquantes
      sc%compt_calcul_taylor=0


    ! DR 31/08/07 pour qu'il n'y ai pas de problemes dans les enchainements
    ! une fois recup.tmp lu on l'efface comme ca si il n'est pas generé dans cette simul
    ! il ne peut pas etre reutilisé plus tard

    !!!  open(12,file = 'recup.tmp',status = 'unknown')
    !!!  write(12,*)
    !!!  close(12)

!write(444,*)'hur dans initsol',sc%hur



return
end subroutine initnonsol
 
 
