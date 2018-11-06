!  *************** Modif pour STICS **********************************************************
!  initialisation des variables climatiques lues dans lecstat
!  introduction de la modification des températures avec l'altitude
!  calcul de variables thermiques pour VAC
!  calcul des apports d'eau et d'azote avec cumul dew 2 plantes dans le cas de cultures associées
!  calcul des nouveaux parcours de develop en cas de stade observé
!  tests de coherence sur l'affectation de etatvernal
!
!     version 5.2
!     dernière modif : 19/09/06
!  *******************************************************************************************

!Initialisation variables climatique pour hisafe
subroutine iniclim1(c,sta)

USE Stics

USE Climat
USE Station
USE Divers
USE Besoins_en_froid
USE Messages
USE Divers, only: isBissextile

  implicit none

  type(Climat_),              intent(INOUT) :: c

  type(Station_),             intent(INOUT) :: sta


! Variable(s) locale(s)

      integer :: n
      real    :: difaltitude
      real    :: fracinsol

      c%nometp = sta%P_codeetp


! On initilisa toujours 366 jours
      do n = 1,366 ! TODO : mettre une étiquette sur la boucle pour mieux identifier la fin

        c%tmoy(n) = (c%tmin(n) + c%tmax(n)) / 2.

!!!MODIF HISAFE 7 : Déplacement de variables (de Climat.f90 dans Stics.f90)
!!!        c%nitetcult(n) = 0

! adéquation variables d'entrée-options des simulation des besoins en eau
!temporaire le 14/09
! domi 29/04/2002 on change  <= c%tvent sinon on ne calcule rien quand vent à 0
! temporaire pour Diane
! NB le 26/01/05 introduction d'un facteur de correction pour passer de c%tmin a trosee
!        if (c%tpm(n) <= 0.0) c%tpm(n) = TVAR(c%tmin(n)+c%P_corecTrosee)
! NB le 20/01/06 c%P_corecTrosee vient en soustraction à la temp de l'air
        if (c%tpm(n) <= 0.0)then
             c%tpm(n) = TVAR(c%tmin(n)-sta%P_corecTrosee)
             !!!MODIF HISAFE 1 : suppression des chaines de caractères
             !!! [pe/pc/sw/pt] => code 1/2/3/4
             !!!if(c%nometp == 'SW'.or.c%nometp == 'PC'.or.c%nometp == 'sw'.or.c%nometp == 'pc')call EnvoyerMsgHistorique(218)
             if(c%nometp == 3.or.c%nometp == 2)call EnvoyerMsgHistorique(218)
        endif

! bornage du vent
        if (c%tvent(n) < 1.0) c%tvent(n) = 1.0

! bornage du rayonnement (NB le 04/01/02)
        if (c%trg(n) <= 0.0) c%trg(n) = 1.0


! modification du vent si le niveau de
! référence est supérieur à 2 m NB le 24/4/98
        if (sta%P_zr > 2.0) then
          c%tvent(n) = c%tvent(n) * log(sta%P_zr/0.01) / log(2.0/0.01)
        endif

    ! POUR TEST VIGNE
    ! --       c%trr(n) = c%trr(n)/10.
    ! NB - le 05/04/01 - introduction de la modification
    ! des températures avec l'altitude
    ! modification le 20/04
        if (sta%P_codaltitude == 2 .and. sta%P_altistation /= sta%P_altisimul) then
          difaltitude = sta%P_altisimul - sta%P_altistation
          c%tmax(n) = c%tmax(n) + sta%P_gradtx * difaltitude / 100.0
        ! cas de l'inversion en cas de ciel clair
          fracinsol = (c%trg(n) / RGEX(sta%P_latitude/180*3.14,n)-0.18)/0.62
          fracinsol = max(fracinsol,0.0)
          fracinsol = min(fracinsol,1.0)


! ML et NB - le 31/08/06 reorganisation du codage de l inversion
! en cas de ciel clair
          if ((fracinsol < sta%P_cielclair) .or.                                                                 &
                      (sta%P_altinversion <= sta%P_altistation .and. sta%P_altinversion <= sta%P_altisimul)) then
             c%tmin(n) = c%tmin(n) + sta%P_gradtn * difaltitude / 100.0
          else
            if (sta%P_altinversion > sta%P_altistation .and. sta%P_altinversion > sta%P_altisimul) then
              c%tmin(n) = c%tmin(n) + sta%P_gradtninv * difaltitude / 100.0
            else
              if (difaltitude >= 0.0) then
                c%tmin(n) = c%tmin(n) + sta%P_gradtninv * (sta%P_altinversion - sta%P_altistation) / 100.0
                c%tmin(n) = c%tmin(n) + sta%P_gradtn * (sta%P_altisimul - sta%P_altinversion) / 100.0
              else
                c%tmin(n) = c%tmin(n) + sta%P_gradtn * (sta%P_altinversion - sta%P_altistation) / 100.0
                c%tmin(n) = c%tmin(n) + sta%P_gradtninv * (sta%P_altisimul - sta%P_altinversion) / 100.0
              endif
            endif
          endif

          if (sta%P_codadret == 2) then
            c%tmax(n) = c%tmax(n) + sta%P_ombragetx
          endif

          c%tmoy(n) = (c%tmin(n) + c%tmax(n)) / 2.


        endif


      end do

      c%tmoy(0) = c%tmoy(1)

    ! dr 09/01/06 initialisation des sommes de rapport
      c%Ctculttout = 0.
      c%Ctairtout = 0.
      c%somdifftculttair = 0.
      c%Ctetptout = 0.
      c%Cetmtout = 0.
      c%Crgtout = 0.
      c%amptcultmat = 0.
      c%tncultmat = 0.
      c%dureelaxrec = 0
      c%nbjechaudage = 0
      c%dureehumec = 0.0
      c%dureeRH1 = 0.0
      c%dureeRH2 = 0.0
      c%dureeRH = 0.0


return
end subroutine iniclim1

!Initialisation variables climatique pour hisafe
!On a laissé ici tout ce qu'on a pas fait dans iniclim1
subroutine iniclim2(sc,pg,p,itk,c,sta,t)

USE Stics
USE Plante
USE Itineraire_Technique
USE Sol
USE Climat
USE Station
USE Parametres_Generaux
USE Divers
USE Besoins_en_froid
USE Messages
USE Divers, only: isBissextile

  implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc

  type(Parametres_Generaux_), intent(IN)    :: pg

  type(Plante_),              intent(INOUT) :: p

  type(ITK_),                 intent(INOUT) :: itk

  type(Climat_),              intent(INOUT) :: c

  type(Station_),             intent(INOUT) :: sta

  type(Stics_Transit_),       intent(INOUT) :: t


! Variable(s) locale(s)
      integer :: i  !>
      integer :: n  !>
      integer :: jul  !>
      integer :: is  !>
      integer :: j  !>
      integer :: jour  !>
      integer :: phase  !>
      integer :: phasedrp  !>
      integer :: ipl
      integer :: n30juin  !>
      integer :: n30sept
      real    :: udevclim
      real    :: thor(24) ! températures horaires


      ipl = p%ipl

! ******* calcul de P_iwater et P_ifwater *******
!   nbans     = nombre total de cultures
!   P_iwater    = debut de la simulation lu dans travail.P_usm
!   P_ifwater   = fin de la simulation lue dans travail.P_usm
!   ifwater_courant = fin de simulation calculée chaque annee
!   numcult   = numero de la culture
!   julfin    = nombre de jours du fichier climatique
!   nbjsemis  = nombre de jours de l'annee de semis
!   P_culturean = code :1 si la culture est sur 1 annee civile*


      if (sc%nbans == 1) then
        if (sc%P_ifwater > c%julfin) then
          sc%ifwater_courant = c%julfin
        endif
        ! ce test est fait pour permettre a l'utilisateur de faire sa simulation meme si son semis est anterieur
        ! au debut de sa simulation et que la culture n'est pas en place (semée)
        if (sc%P_iwater > sc%iplt(ipl) .and. p%codeinstal == 0) then
          call EnvoyerMsgHistorique(61)
        endif
      endif


    ! attention pour enchainer les années, il faut que les fichiers climatiques soient complets
      if (sc%nbans > 1) then
        ! 13/09/2011 recalcule de ifwater seulement dans les cas ou  on est en enchainement et que l'annee de semis est bissextile
        if(isBissextile(sc%ansemis))then
           sc%ifwater_courant = sc%P_ifwater+1
        else
           sc%ifwater_courant = sc%P_ifwater
        endif

 ! test dr 27052013       if (c%julzero /= 1 .or. c%julfin < 365+sc%nbjrecol) then
 ! DR 10/06/2013 le pb d'enchainement de series si elles la premiere année ne commence pas en janvier semble resolu par ce test
        if ((sc%numcult.gt.1.and.c%julzero /= 1) .or. c%julfin < 365+sc%nbjrecol) then
              call EnvoyerMsgHistorique(63)
              call exit(9)
        endif
      endif


    ! si sc%P_iwater est antérieur à c%julzero, on jette
      if (sc%P_iwater < c%julzero) then
        call EnvoyerMsgHistorique(64)
         call exit(9)
      endif



      sc%maxwth = sc%ifwater_courant - sc%P_iwater + 1
    ! rajout de domi les dates dans le calendrier hydrique doivent etre recalculees là car c'est là qu'on a sc%P_iwater
    !  domi le 20/07/99 on doit voir ca avec nadine
      if (p%codeinstal == 0) then
        p%nplt = sc%iplt(ipl) - sc%P_iwater + 1
      else
        p%nplt = 1
      endif

      p%nrecbutoir = itk%P_irecbutoir - sc%P_iwater + 1



    ! dr 06/01/06 correction du bug sc%P_culturean == 1 au lieu de sc%P_culturean /= 1
    ! PB - 08/12/2004
    ! en cas d'enchainement sur une pérenne sur 2 années, on ne bloque pas p%nrecbutoir à sc%maxwth
      if (sc%maxwth < p%nrecbutoir  .and. (p%P_codeperenne /= 2 .or. sc%P_culturean == 1 .or. pg%P_codeinitprec /= 2)) then
        p%nrecbutoir = sc%maxwth

      endif


    !  domi 02/07/2001
    ! pour empecher les enchainement de saisons de plus de 365 jours
    ! domi 07/08/01 c'est pas sc%numcult mais sc%nbans
      if (sc%maxwth > 366 .and. sc%nbans > 1) then
        call EnvoyerMsgHistorique('***')
        call EnvoyerMsgHistorique(201)
        !stop
         call exit(9)
      endif
! write(5588,*)'lecture dans iniclim fin iwater',sc%P_iwater,'ifwater',sc%P_ifwater,sc%ifwater_courant,sc%maxwth
! DR 15/01/2016 correction bug sur les irrigations en upvt
! pb si on est en upvt faut pas passer j'ajoute un test
      if (itk%P_codedateappH2O /= 1) then
      do i = 1, itk%nap
        n = itk%P_julapI(i) - sc%P_iwater + 1
        if (n < 0) then
          call EnvoyerMsgHistorique(65,itk%P_julapI(i))
          call EnvoyerMsgHistorique(66,sc%P_iwater)
          call EnvoyerMsgHistorique(67)
          call EnvoyerMsgHistorique(68)
         ! stop
          call exit(9)
        endif
      ! si une dose a déja été apportée par une autre plante, alors on enleve 1 au nombre d'approt de la plante
        if (sc%P_nbplantes > 1 .and. sc%airg(n) /= 0.) itk%nap = itk%nap - 1
      ! on peut cumuler les doses apportées par plusieurs cultures
        sc%airg(n) = sc%airg(n) + itk%P_doseI(i)
      end do
      endif

    ! domi 29/08/03 pour caroline lecture des dates de fertil en upvtt
    !  cette affectation n'a pas lieu d'etre dans ce cas
    !  anit(n) est à recalculer plus loin
      if (itk%P_codedateappN /= 1) then
        do i = 1,itk%napN
          n = itk%P_julapN(i) - sc%P_iwater + 1
          ! DR 06/04/2016 possibilite de plusieurs engrais
          if(.not.itk%flag_plusieurs_engrais)itk%P_engrais(i)=itk%P_engrais(1)


          if (n < 0) then
          call EnvoyerMsgHistorique(69,itk%P_julapN(i))
          call EnvoyerMsgHistorique(66,sc%P_iwater)
          call EnvoyerMsgHistorique(67)
          call EnvoyerMsgHistorique(68)
          ! stop
           call exit(9)
          endif

        ! si une dose a déja été apportée par une autre plante, alors on enleve 1 au nombre d'approt de la plante
        ! domi 31/08/04 c'est napntot qu'on diminue d'un apport et un met un message
          if (sc%anit(n) /= 0.) sc%napNtot = sc%napNtot - 1

        ! on peut cumuler les doses apportées par plusieurs cultures
        ! DR 02/07/08 je rajoute le fractionnement de l'irrigation
          if (itk%P_codefracappN == 1) then
            sc%anit(n) = sc%anit(n) + itk%P_doseN(i)
            sc%type_ferti(n) = itk%P_engrais(i)
          else
            sc%anit(n) = sc%anit(n) + itk%P_fracN(i) / 100. * itk%P_Qtot_N
            sc%type_ferti(n) = itk%P_engrais(i)
          endif

        end do
      endif

! DR 01/02/2011 apport des residus
      do is = 1,itk%P_nbjres
        p%numjres(is) = itk%P_julres(is) - sc%P_iwater + 1
!     domi 19/09/97 test sur la date d'incorporation des residus
!     elle doit etre anterieure au semis ou superieure à la recolte
        if (itk%P_julres(1) /= 999 .and.  p%numjres(is) >= p%nplt  .and. p%numjres(is) <= p%nrecbutoir) then
          call EnvoyerMsgHistorique(60)
        endif
      end do
! DR 01/02/2011 travail du sol
      do is = 1,itk%P_nbjtrav
        p%numjtrav(is) = itk%P_jultrav(is) - sc%P_iwater + 1
!     domi 19/09/97 test sur la date d'incorporation des residus
!     elle doit etre anterieure au semis ou superieure à la recolte
        if (itk%P_jultrav(1) /= 999 .and.  p%numjtrav(is) >= p%nplt  .and. p%numjtrav(is) <= p%nrecbutoir) then
          call EnvoyerMsgHistorique(260)
        endif
      end do

! DR 010/02/2015 on change de referentiel pour les dates de debut et de fin d'irrigation
          sc%n_datedeb_irrigauto = t%P_datedeb_irrigauto- sc%P_iwater + 1
          sc%n_datefin_irrigauto = t%P_datefin_irrigauto- sc%P_iwater + 1

!***********************************************************
! passage a un referentiel temporel climatique depuis le debut
! du bilan hydrique : sc%P_iwater (indice n)

    ! NB - 10/06/2004
    ! initialisations des variables thermiques (VAC)
      p%TmoyIpltJuin = 0.0
      p%TmoyIpltSept = 0.0
      p%nbjTmoyIpltJuin = 0
      p%nbjTmoyIpltSept = 0
      if (sc%P_culturean  == 1) then
        n30juin = 181 - sc%P_iwater + 1
        n30sept = 273 - sc%P_iwater + 1
      else
        n30juin = 181 - sc%P_iwater + 1 + sc%nbjsemis
        n30sept = 273 - sc%P_iwater + 1 + sc%nbjsemis
      endif


! DR 29/10/07 plus de decadaire
      do j = c%julzero,c%julfin ! TODO : mettre une étiquette sur la boucle pour mieux identifier la fin
        n = j-sc%P_iwater+1
        if (j  <  sc%P_iwater) CYCLE
        jul = n + sc%P_iwater - 1
        if (jul > sc%nbjsemis) jul = jul-sc%nbjsemis

        jour = j


      ! NB - 10/06/2004
      ! calcul de variables thermiques pour VAC
        udevclim = c%tmoy(n) - p%P_tdmin
        if (c%tmoy(n) < p%P_tdmin) udevclim = 0.0
        if (c%tmoy(n) > p%P_tdmax) udevclim = p%P_tdmax-p%P_tdmin
        if (n >= p%nplt .and. n < n30juin) then
          p%TmoyIpltJuin = p%TmoyIpltJuin + udevclim
          p%nbjTmoyIpltJuin = p%nbjTmoyIpltJuin + 1
        endif
        if (n >= p%nplt .and. n < n30sept) then
          p%TmoyIpltSept = p%TmoyIpltSept + udevclim
          p%nbjTmoyIpltSept = p%nbjTmoyIpltSept + 1
        endif



! dr 03/05/2013 ajouter un message pour le recalcul de Tpm
        if (c%tpm(n) <= 0. .or. c%tvent(n) < 0.) sc%posibsw = .false.
        if (c%tetp(n) < 0.) sc%posibpe = .false.

! NB - le 28/06 - climat sous abri - culture pure seulement !!
        if (itk%P_codabri == 2) then
          p%P_codebeso = 1
          if (.not.sc%posibsw) then
            call EnvoyerMsgHistorique(204)
            !stop
            call exit(9)
          endif

          c%trrext(n) = c%trr(n)
          c%trr(n) = 0.
          c%tminext(n) = c%tmin(n)
          c%tmaxext(n) = c%tmax(n)
          c%tmoyext(n) = c%tmoy(n)
          c%trgext(n) = c%trg(n)
          c%trg(n) = itk%P_transplastic * c%trgext(n)
          c%tpmext(n) = c%tpm(n)
          c%tetp(n) = sta%P_coefdevil * c%trg(n)
        endif


       !!!MODIF HISAFE 7 : Déplacement de variables (de Climat.f90 dans Stics.f90)
       sc%nitetcult(n) = 0


      end do


    ! NB le 22/06/04 c'est des moyennes qui nous faut !
      p%TmoyIpltJuin = p%TmoyIpltJuin / p%nbjTmoyIpltJuin
      p%TmoyIpltSept = p%TmoyIpltSept / p%nbjTmoyIpltSept


    ! adéquation variables d'entrée-options des simulation des besoins en eau
      if (.not.sc%posibsw .and. p%P_codebeso == 2) then
        call EnvoyerMsgHistorique(71)
        !stop
        call exit(9)
      endif

      if (.not.sc%posibpe .and. p%P_codebeso == 1 .and. (.not.sc%posibsw)) then
        call EnvoyerMsgHistorique(72)
      endif

      sc%tairveille = c%tmoy(p%nplt-1)


    ! le 29/4/99 - etat de vernalisation
    ! NB - le 23/03
      if (p%P_codebfroid == 1) then

      ! ML - le 21/04/04 - impossibilite de demarrer au stade dor (debut de
      ! dormance) dans le cas d'une culture sans besoins en froid
      !!!MODIF HISAFE 1 : suppression des chaines de caractères
      !!!  if (p%P_stade0 == 'dor') then
        if (p%P_stade0 == 3) then
          call EnvoyerMsgHistorique(76)
          call exit(9)
        else
          p%etatvernal = .TRUE.
        endif
      endif

        ! herbacées
        ! NB le 18/12/01

      ! DR et ML et SYL 16/06/09
      ! on rajoute une condition sur P_codemontaison car ce test est tres specifique
      ! de cette option
      !!!MODIF HISAFE 1 : suppression des chaines de caractères
      !!!if (t%P_codemontaison(ipl) == 1 .and. p%P_stade0 /= 'lev')then
        if (t%P_codemontaison(ipl) == 1 .and. p%P_stade0 /= 4)then
          p%etatvernal = .TRUE.
        endif

        if (p%P_codebfroid == 2) then
          if (p%P_jvc(itk%P_variete) <= p%P_jvcmini) then
            p%etatvernal = .TRUE.
          else
          ! PB - 27/12/2004 - j'ai enlevé le test sur 'dor' et je l'ai remplacé par d'autres tests, plus bas
          !!!MODIF HISAFE 1 : suppression des chaines de caractères
          !!!  if (p%P_stade0 == 'snu' .or. p%P_stade0 == 'plt') then
            if (p%P_stade0 == 1 .or. p%P_stade0 == 2) then
              p%etatvernal = .FALSE.
            else
             !!!MODIF HISAFE 1 : suppression des chaines de caractères
             !!! if (p%P_codeperenne == 1 .and. p%P_stade0 == 'lev') then
              if (p%P_codeperenne == 1 .and. p%P_stade0 == 4) then
                p%etatvernal = .FALSE.
            ! ML - le 21/04/04 - ajout d'un message pour prévenir l'utilisateur que la vernalisation
            ! démarre avec la levée et non avec la germination dans ce cas.
                call EnvoyerMsgHistorique(70)
              endif

            ! PB - 27/12/2004 - ajout d'un test sur sc%numcult et P_codeinitprec
            !!!MODIF HISAFE 1 : suppression des chaines de caractères
            !!!  if (p%P_codeperenne == 2 .and. p%P_stade0 == 'lev' .and. (sc%numcult == 1 .or. pg%P_codeinitprec == 1)) then
              if (p%P_codeperenne == 2 .and. p%P_stade0 == 4 .and. (sc%numcult == 1 .or. pg%P_codeinitprec == 1)) then
                p%etatvernal = .TRUE.
              endif

             !!!MODIF HISAFE 1 : suppression des chaines de caractères
             !!! if (p%P_stade0 /= 'lev') p%etatvernal = .TRUE.
             if (p%P_stade0 /= 4) p%etatvernal = .TRUE.

            ! PB - 27/12/2004 - ajout d'un test sur sc%numcult et P_codeinitprec
            !!!MODIF HISAFE 1 : suppression des chaines de caractères
            !!!  if (p%P_codeperenne == 2 .and. p%P_stade0 == 'dor' .and. sc%numcult > 1 .and. pg%P_codeinitprec == 2) then
              if (p%P_codeperenne == 2 .and. p%P_stade0 == 3 .and. sc%numcult > 1 .and. pg%P_codeinitprec == 2) then
                p%etatvernal = .TRUE.
              endif
            endif
          endif
        endif !-- fin herbacées

      ! ligneux
        if (p%P_codebfroid == 3) then
        !  DR 18/08/06 seulement si on est pas en enchainement d'annee
        !!!MODIF HISAFE 1 : suppression des chaines de caractères
        !!!  if (p%P_stade0 == 'dor' .and. pg%P_codeinitprec /= 2) then
          if (p%P_stade0 == 3 .and. pg%P_codeinitprec /= 2) then
            p%etatvernal = .FALSE.
          else
            p%etatvernal = .TRUE.
          endif
        ! DR et IGC 18/08/06 on met les initialisations perennes vigne ici au lieu de initnonsol
          if (sc%cu0(ipl) /= 0) then
          ! on est dans un enchainement d'année , on a deja demarré la dormance
          ! mais on est pas arrivé à fin de dormance
          ! DR et IGC 02/10/2012 on elimine la ligne 631 car elle initialise la date d'entree en dormance de maniere inopinée
          !  p%ndebdorm = 1
            p%cu(0) = sc%cu0(ipl)
            p%etatvernal = .FALSE.
          else
          ! on a pas demarré la dormance ou on l'a finit
            if (pg%P_codeinitprec == 2) then
            ! si on est en enchainement d'année la dormance est fini et on calcule la nouvelle
            ! date d'entree en dormance et on recupere la valeur des actions chaud
            ! DR 22/01/08 y'avait un pb de calcul de date
              !--p%ndebdorm = p%P_idebdorm + sc%nbjsemis - sc%P_iwater - 1
              p%ndebdorm = p%P_idebdorm + sc%nbjsemis - sc%P_iwater + 1

              p%somelong = sc%somelong0(ipl)
            ! DR et IGC 17/03/08 on reinitialise nfindorm si il s'est passe l'annee avant
              p%nfindorm = sc%nfindorm0(ipl)

              p%rfvi = 1.0
              p%etatvernal = .TRUE.
            endif
          endif
        endif ! -- fin ligneux


! calcul des sommes de températures pour les phases avec stades observés
! on passe dans cette boucle uniquement si l'un eu moins des stades est observé
      if (p%nlevobs /= 999 .or. p%namfobs /= 999         &
          .or. p%ndrpobs /= 999 .or. p%nsenobs /= 999      &
          .or. p%nlanobs /= 999 .or. p%nmatobs /= 999      &
          .or. p%nrecobs /= 999 .or. p%nlaxobs /= 999) then
        phase = 0

        do n = 1,sc%maxwth
          if (n >= p%nplt .and. n <= p%nrecbutoir) then
! calcul de l'effet thermique
            p%udevair = sc%tairveille - p%P_tdmin
            if (sc%tairveille < p%P_tdmin) p%udevair = 0.0
            if (sc%tairveille > p%P_tdmax) then
              p%udevair = p%P_tdmax - p%P_tdmin
            endif
            p%udevcult = p%udevair
! calcul des somme de développement courantes depuis le stade précédent
! effet vernalisant uniquement si P_jvc>0
            if (.not.p%etatvernal  .and.  (phase >= 1 .and. phase <= 4)) then
              if (p%caljvc >= p%P_jvc(itk%P_variete)) then
                p%rfvi = 1.0
              else
                thor = calcul_TemperaturesHoraires(c%tmin(n),c%tmin(n+1),c%tmax(n))
                call bfroid(sc%tairveille,p%P_codebfroid,p%caljvc,p%P_jvc(itk%P_variete),p%P_jvcmini,p%rfvi,p%etatvernal,   &
                            p%P_codeperenne,sc%P_culturean,pg%P_codeinitprec,sc%maxwth,sc%nbjanrec,p%nrecbutoir,          &
                            p%P_tfroid,p%P_ampfroid,p%P_codedormance,thor,minval(p%cu),p%cu(n-1),p%cu(n),         &
                            p%ndebdorm,n,p%P_q10,c%tmin(n),c%tmax(n))
              endif
            else
              p%rfvi = 1.0
            endif

! calcul de l'effet photopériodique en prenant un P_phobase fixe
! (réajustement ensuite dans develop)
            if (p%P_codephot == 1 .and. (phase >= 2 .and. phase <= 4)) then
              sc%numdate = n+sc%P_iwater-1
              call photpd(sta%P_latitude,sc%numdate,c%daylen,c%phoi)
              p%rfpi = (c%phoi - p%P_phobase) / (p%P_phosat - p%P_phobase)
              p%rfpi = min(p%rfpi,1.0)
              p%rfpi = max(p%rfpi,0.0)
            else
              p%rfpi = 1.0
            endif

            p%upobs(n) = p%udevair * p%rfpi * p%rfvi
            p%somcour = p%somcour + p%upobs(n)


! version 4.0
             p%somcourdrp = p%somcourdrp + p%upobs(n)
            sc%tairveille = c%tmoy(n)


! calcul des nouveaux parcours de dl en cas de stade observé
! la germnination
            if (p%somcour >= p%P_stpltger .and. phase == 0) then
              p%somcour = 0.0
              phase = 1
            endif
! la levée
!-----------------------------------------------------
            if (n == p%nlevobs) then
              p%stpltlev = p%somcour
              p%somcour = 0.0
              p%somcourdrp = 0.0
              phase = 2
            endif
            if (p%nlevobs == 999 .and. p%somcour >= p%stpltlev .and. phase == 1) then
              p%somcour = 0.0
              p%somcourdrp = 0.0
              phase = 2
            endif
! amf
            if (n == p%namfobs) then
              p%P_stlevamf(itk%P_variete) = p%somcour

              p%somcour = 0.0
              phase = 3
              if (p%P_stlevamf(itk%P_variete) == 0) then
                call EnvoyerMsgHistorique(73)
              endif
            endif
            if (p%namfobs == 999 .and. p%somcour >= p%P_stlevamf(itk%P_variete) .and. phase == 2) then
              p%somcour = 0.0
              phase = 3
            endif
! laimax
            if (n == p%nlaxobs) then
              p%P_stamflax(itk%P_variete) = p%somcour
              p%somcour = 0.0
              phase = 4
            endif
            if (p%nlaxobs == 999 .and. p%somcour >= p%P_stamflax(itk%P_variete) .and. phase == 3) then
              p%somcour = 0.0
              phase = 4
            endif
! senescence rapide
            if (n == p%nsenobs) then
              p%P_stlaxsen(itk%P_variete) = p%somcour
              p%somcour = 0.0
              phase = 5
            endif
            if (p%nsenobs == 999 .and. p%somcour >= p%P_stlaxsen(itk%P_variete) .and. phase == 4) then
              p%somcour = 0.0
              phase = 5
            endif
! fin IR
            if (n == p%nlanobs) then
              p%P_stsenlan(itk%P_variete) = p%somcour
              p%somcour = 0.0
              phase = 6
            endif
            if (p%nlanobs == 999 .and. p%somcour >= p%P_stsenlan(itk%P_variete)  .and. phase == 5) then
              p%somcour = 0.0
              phase = 6
            endif
! dremp
            if (n == p%ndrpobs) then
              p%P_stlevdrp(itk%P_variete) = p%somcourdrp
              p%somcourdrp = 0.0
              phasedrp = 1
            endif
            if (p%ndrpobs == 999 .and. p%somcourdrp >= p%P_stlevdrp(itk%P_variete) .and. phase >= 2) then
              phasedrp = 1
            endif
! maturité physio
            if (n == p%nmatobs) then
              p%P_stdrpmat(itk%P_variete) = p%somcourdrp
              if (p%P_codeindetermin == 2) then
                p%P_dureefruit(itk%P_variete) = p%somcourdrp
              endif
              p%somcourdrp = 0.0
              phasedrp = 2
            endif
            if (p%nmatobs == 999 .and. p%somcour >= p%P_stdrpmat(itk%P_variete) .and. phasedrp == 1) then
              p%somcourdrp = 0.0
              phasedrp = 2
            endif
! récolte
            if (n == p%nrecobs) then
              p%stmatrec = p%somcourdrp
              p%somcourdrp = 0.0
            endif
!-----------------------------------------------------
          endif

        end do
      endif

    ! PB - 27/12/2004 - ajout d'un test sur numcult, P_codeperenne et P_codeinitprec
      if (sc%numcult == 1 .or. p%P_codeperenne /= 2 .or. pg%P_codeinitprec /= 2) then
      !!!MODIF HISAFE 1 : suppression des chaines de caractères
      !!!if (p%P_stade0 /= 'dor') p%caljvc = 0.0
        if (p%P_stade0 /= 3) p%caljvc = 0.0
        p%somcour = 0.0
        p%somcourdrp = 0.0
      endif

    ! initialisation des températures du sol uniquement pour les premières simulations
    ! domi - 14/12/00 - sol 200 à 1000
    ! domi - 24/04/01 - on initialise tsol si on enchaine sans lire le reste
      !--if (sc%numcult == 1 .and. P_codesuite == 0) then
      if ((sc%numcult == 1 .and. sc%P_codesuite == 0) .or. pg%P_codeinitprec == 1) then
        sc%tsolveille(:) = c%tmoy(1)
        sc%tsol(:) = c%tmoy(1)
        sc%tcult = c%tmoy(1)
        sc%tcultveille = c%tmoy(1)
        sc%tairveille = c%tmoy(1)

      endif


return
end subroutine iniclim2



 

