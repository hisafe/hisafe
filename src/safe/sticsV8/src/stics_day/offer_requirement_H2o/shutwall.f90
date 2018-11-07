! dernière modif le 11/06
! programme de calcul des besoins en eau pour une association de culture
! selon Shuttleworth et Wallace (1985), Wallace (1995) et Brisson et al. (1998)
!
! indice P = relatif à la culture principale
! indice A = relatif à la culture associée
! entree clim : vent(ms-1),tempe(degree C),rayonnement net(MJm-2j-1),
! pression de vapeur (mbars)
! test par rapport procédure Matlab (beaucas.m) le 16/3/98 OK

! function  [eop(1) = eopP,eopA,sc%Edirect] = beaucas(par)
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> Crop water requirements through a "resistance" approach
!> - Stics book paragraphe 7.2.2, page 132-134
!>
!! This module calculates the crop water requirements through a "resistance" approach.
!! This is an alternative approach to the "crop coefficient" approach, which consists of estimating plant water requirements and soil evaporation using the
!! Shuttleworth and Wallace daily time-step model (Brisson et al., 1998b). This has proved to be effective for explaining the energy budget of canopies (Sene, 1994)
!! provided that appropriate empirical resistance parameters are used (Fisher and Elliott, 1996).
!!
!! The calculations are based on the resistance diagram involving four flows (soil evaporation (es), maximum plant transpiration (eop), direct evaporation
!! of water intercepted by the foliage (Emd) or by mulch (Emulch) and two types of resistance (resistance to diffusion: ras and raa and surface resistance: rc and rac)
!! (see module calraero.f90 for details on ras and raa calculation).
!! In this case all the fluxes are actual ones except the plant transpiration flux, which is the maximal one.
!! Each flux is calculated using a formula of the same type as for the potential soil evaporation.
!! dos, the saturation deficit within the vegetation, is the variable linking all the fluxes.
!! dos is calculated assuming that, under soil conditions which are kept moist, total evapotranspiration (soil+canopy) can be written in the form of evaporation
!! according to Priestley and Taylor (Brisson et al., 1998c) : ePT. It is calculated by relationship of Shuttleworth and Wallace (1985).
! ------------------------------------------------------------------------------------------------------------------------------------------------------------* c
!subroutine shutwall(sc,pg,c,sta,soil,p,itk,t)
!!
subroutine shutwall(sc,pg,c,sta,soil,p,itk) ! DR 19/07/2012 t n'est pas utilisé


USE Stics
USE Plante
USE Itineraire_Technique
USE Parametres_Generaux
USE Climat
USE Station
USE Sol
USE Messages

  implicit none


  type(Stics_Communs_),       intent(INOUT) :: sc  

  type(Parametres_Generaux_), intent(IN)    :: pg  

  type(Climat_),              intent(INOUT) :: c  

  type(Plante_),              intent(INOUT) :: p(sc%P_nbplantes)

  type(ITK_),                 intent(INOUT) :: itk(sc%P_nbplantes)

!  type(Stics_Transit_),       intent(INOUT) :: t

  type(Sol_),                 intent(INOUT) :: soil  

  type(Station_),             intent(INOUT) :: sta  


!: Variables locales
  integer :: coderesist  !>  
  integer :: jul  !>  
  integer :: ii  !>  
  integer :: n  !>  
  integer :: AS  !>  
  integer :: AO  
! ** ?? -- en common ou en variable de fonction
  real    :: fAO  !>  
  real    :: fAS  !>  
  real    :: laidessus  
  real    :: rb  !>  
  real    :: nconv  !>  
  real    :: pi  
  real    :: karm  !>  
  real    :: alpha  !>  
  real    :: gamma  !>  
  real    :: ro  !>  
  real    :: cp  !>  
  real    :: const_to  
  ! dr 23/09/2014 je le passe dans les varaibles plantes
 ! real    :: fco2s  !>
  real    :: h  !>  
  real    :: z0s  !>  
  real    :: d  !>  
  real    :: z0  
  real    :: ePT  !>  
  real    :: dos  !>  
  real    :: EmdAO  !>  
  real    :: EmdAS  !>  
  real    :: mouillPP  !>  
  real    :: mouillA  
  real    :: L  !>  
  real    :: deltat  
  real    :: rnetP  !>  
  real    :: rnetA  !>  
  real    :: raintA  !>  
  real    :: eopA  !>  
  real    :: eopP  !>  
  real    :: sigma  

! domi 12/01/06 certaines varaibles ne sont plus utilisées je les met en com en attendant
!  real   :: largeurP,fracinsol,Rsolglo,rnetpen,RGEX,largeurA


! On fixe la dimension à 2 mais dans l'absolu il faudrait dimension(P_nbplantes) : ou pas ? on peut aussi dire que la routine ne marche que pour 1 à 2 plantes.
  real    :: hauteurMoy(2)  
  real    :: termrad(2)  
  real    :: termres(2)  
  real    :: rac(2)  


  real    :: dsat  !>  
  real    :: f1  !>  
  real    :: f2  !>  
  real    :: raa  !>  
  real    :: rasmax  !>  
  real    :: rasmin  

  logical :: deuxcult  

!  integer :: ficdbg = 71

!: Fonction(s)
  real    :: TVAR  

    ! pour faciliter l'écriture
      n = sc%n
      AS = sc%AS
      AO = sc%AO

    ! DR le 15/10/07 initialisation de mouillA qui peut etre le pb de Souverain
      mouillA = 0.0
    ! DR 28/04/2011 c'etait desactivé , je le reactive sinon pb
      rnetA = 0.0
      rnetP = 0.0


    ! Est-on en condition "deux cultures" ?
      deuxcult = .FALSE.
      if (sc%P_nbplantes > 1) then
      ! on fait le test en deux fois pour ne pas lire p(2) si p(2) n'existe pas. (out of bound)
        if (p(2)%lai(0,n-1) > 0.0) deuxcult = .TRUE.
      endif

    ! Mise a zero des tableaux locaux
      hauteurMoy(:) = 0.0
      termrad(:)    = 0.0
      termres(:)    = 0.0
      rac(:)        = 0.0


    ! Constantes
      gamma     = 0.65 * sta%P_patm / 1000.0   ! NB le 26/01/05 influence P_patm sur gamma
      ro        = 1.2
      cp        = 1.013e-3
      karm      = 0.41
      alpha     = 1.32
      const_to  = 8.64e+4
      pi        = atan(1.0)  * 4
      sigma     = 5.67e-8

    ! Constantes pour le calcul des résistances
      rb        = 50
      nconv     = 2.5

    ! calculs données intermédiaires
      deltat  =  TVAR(c%tmoy(n)+.5) - TVAR(c%tmoy(n)-.5)
      dsat    =  TVAR(c%tmoy(n)) - c%tpm(n)
      L       = (2500840 - (2358.6 * c%tmoy(n))) * 1e-6


    ! détermination du schéma résistif "haut" ou "bas"
      if (deuxcult) then
        do ii = 1,sc%P_nbplantes
          hauteurMoy(ii) = max(p(ii)%hauteur(AO), p(ii)%hauteur(AS))
        end do
        if (hauteurMoy(2) < 0.2) then
          coderesist = 2
          laidessus = p(1)%lai(AS,n)
        else
          coderesist = 1
          laidessus = p(2)%lai(0,n-1) + p(1)%lai(AS,n)
        endif
      else
        hauteurMoy(1) = p(1)%hauteur(AS)
        hauteurMoy(2) = 0.0
        coderesist = 2
        laidessus = p(1)%lai(AS,n)
      endif

    ! NB - le 11/06
      c%tutilrnet = c%tmoy(n)

12    continue

    ! somme des lai des plantes
  ! TODO : laiTot déjà calculé auparavant dans sc%laiTot
      sc%laiTot = 0.
      do ii = 1,sc%P_nbplantes
        sc%laiTot = sc%laiTot + p(ii)%lai(0,n)
      end do

!      write(618,*) 'laitot avant calrnet', sc%laitot

! TODO : rajouter une variable de controle de debug pour calrnet_shutwall
!#if DEBUG == 1
!      if (iand(sc%CALRNET_SHUTWALL,1) >0) &
!        call calrnet_shutwall_debug_read_input(1400,sc,pg,p,itk,soil,c,sta,t,1,sc%AS)
!      if (iand(sc%CALRNET_SHUTWALL,2) >0) &
!        call calrnet_shutwall_debug_write_input(1401,sc,pg,p,itk,soil,c,sta,t,1,sc%AS)
!#endif
! DR et ML 09/02/2011 on fait 2 appel suivant le type de mulch

!!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
!!!MODIF HISAFE : ici on met RG et Visible = 1 (100%)
      if(itk(1)%P_codepaillage == 2)then   !! paillage mulch plastique
          call calrnet(sc%nbCouchesSol,sc%jul,sc%hur,sc%humin,sc%hucc,sc%laiTot,sc%tauxcouv(n),sc%posibsw,  &   ! IN
                   sc%Ratm,soil%P_albedo,itk%P_couvermulchplastique,itk(1)%P_albedomulchplastique,             &
                   sta%P_albveg,sta%P_codernet,sta%P_aangst,sta%P_bangst,c%tmin(n),c%tmoy(n),sta%P_corecTrosee,   &
                   c%trg(n),sta%P_latitude,c%tutilrnet,itk(1)%P_codepaillage,p(1)%P_codelaitr,                &
                   c%tpm(n),sta%P_codecaltemp,sc%albedolai,sc%rglo,sc%rnet, 1, 1)
      else


          call calrnet(sc%nbCouchesSol,sc%jul,sc%hur,sc%humin,sc%hucc,sc%laiTot,sc%tauxcouv(n),sc%posibsw,  &   ! IN
                   sc%Ratm,soil%P_albedo,sc%couvermulch,pg%P_albedomulchresidus(sc%irmulch),             &
                   sta%P_albveg,sta%P_codernet,sta%P_aangst,sta%P_bangst,c%tmin(n),c%tmoy(n),sta%P_corecTrosee,   &
                   c%trg(n),sta%P_latitude,c%tutilrnet,itk(1)%P_codepaillage,p(1)%P_codelaitr,                &
                   c%tpm(n),sta%P_codecaltemp,sc%albedolai,sc%rglo,sc%rnet, 1, 1)

!      write(ficdbg,*)'1.a',n,p(1)%raint(AS),sc%rnet,sc%couvermulch,pg%P_albedomulchresidus(sc%irmulch),sc%irmulch


      endif
!#if DEBUG == 1
!      if (iand(sc%CALRNET_SHUTWALL,4) >0) &
!        call calrnet_shutwall_debug_read_output(1402,sc,pg,p,itk,soil,c,sta,t,1,sc%AS)
!      if (iand(sc%CALRNET_SHUTWALL,8) >0) &
!        call calrnet_shutwall_debug_write_output(1403,sc,pg,p,itk,soil,c,sta,t,1,sc%AS)
!      if (iand(sc%CALRNET_SHUTWALL,16) >0) &
!        call calrnet_shutwall_debug_test_output(1404,sc,pg,p,itk,soil,c,sta,t,1,sc%AS)
!#endif

    ! énergies disponibles au niveau des trois strates (Thornley, 1996)
    ! répartition des énergies entre AO et AS
      if (c%trg(n) <= 0.0) then
        call EnvoyerMsgHistorique(159, jul)
        !stop
        call exit(9)
      endif

      f1 = p(1)%raint(AS) / c%trg(n) / pg%P_parsurrg
      rnetP = 0.83 * f1 * sc%rnet
!      write(ficdbg,*)'1.n',n,p(1)%raint(AS),sc%rnet,rnetA,rnetP


      if (deuxcult .and. p(1)%P_codetransrad == 2) then
        if (p(1)%lai(AS,n) > 0.0) then
          fAO = p(1)%rombre * p(2)%surf(AO) / (p(1)%rombre*p(2)%surf(AO) + p(1)%rsoleil*p(2)%surf(AS))
        endif
        fAS = 1 - fAO
        raintA = p(2)%raint(AO) * p(2)%surf(AO) + p(2)%raint(AS) * p(2)%surf(AS)
!        write(ficdbg,*)'1.1',n,p(2)%raint(AO) , p(2)%surf(AO) , p(2)%raint(AS) , p(2)%surf(AS)
        f2 = raintA / c%trg(n) / pg%P_parsurrg
        rnetA = 0.83 * f2 * sc%rnet
!        write(ficdbg,*)'1.1',n,c%trg(n),sc%rnet,raintA,pg%P_parsurrg
      else
        raintA = 0.0
      endif

      sc%rnetS = sc%rnet - rnetA - rnetP
!      write(ficdbg,*)'2.',n,sc%rnet,rnetA,rnetP


    ! le mulch joue le rôle d'une culture associée
      if (itk(1)%P_codepaillage /= 1 .and. deuxcult) then
        call EnvoyerMsgHistorique(160)
                !stop
                call exit(9)
      endif

!      coefficient d'échange sol nu sous la culture, sc%ras et de raa
!      seuillages
      if (sc%P_nbplantes > 1) then
        if (hauteurMoy(2) > hauteurMoy(1)) then
          call EnvoyerMsgHistorique(161)
!          stop
        endif
      endif

      if (coderesist == 1) then
        h = (hauteurMoy(1)+hauteurMoy(2))/2
        z0s = soil%P_z0solnu
! DR_2010        write(*,*)'1',soil%P_z0solnu
      else
        h = hauteurMoy(1)
        z0s = 0.10 * hauteurMoy(2)
! DR_2010        write(*,*)'2',hauteurMoy(2),deuxcult,p(2)%lai(0,n-1)
        ! on découpe le test pour ne pas tester p(2) si p(2) n'existe pas.
! DR 06/10/2010 ca posait pb pour le calcul de z0s
!        if (deuxcult) then
        if (sc%P_nbplantes==2) then
          if (p(2)%lai(0,n-1) <= 0.0) then
            z0s = soil%P_z0solnu
! DR_2010        write(*,*)'3',soil%P_z0solnu
          endif
        endif

        if (sc%P_nbplantes == 1) then
          z0s = soil%P_z0solnu
! DR_2010          write(*,*),'4',soil%P_z0solnu
        endif
      endif



! *****************DR 18/08/2014 je teste le pbs de cas avec tcult devient NAN **************************************
!!              a cause de z0s qui est nulle et qui provoque une division par zero pour le calcul de raamin et raamax
!! essayer de voir avec Marie pourquoi
        if (sc%P_nbplantes == 2 .and.z0s==0) then
          z0s = soil%P_z0solnu
        endif


! modif NB le 20/07/00
      h = max(h, soil%P_z0solnu / 0.10)

      if (h >= sta%P_zr) then
        call EnvoyerMsgHistorique(162)
                !stop
                call exit(9)
      endif

      ! on remplace par max(c%tvent(n),0.02), on ne modifie pas tvent
      !-- if (c%tvent(n) <= 0.0) c%tvent(n) = 0.02

      d  = 0.66 * h
      z0 = 0.10 * h

!      write(ficdbg,*)'3.',n,sta%P_zr,d,z0,z0s,karm,c%tvent(n),h,nconv,laidessus
!      write(71,*)'3.',n,sta%P_zr,d,z0,z0s,karm,c%tvent(n),h,nconv,laidessus &
!      ,soil,hauteurmoy

    ! raa et sc%ras formule Shuttleworth & Wallace (1985)
      call calraero(sta%P_zr,d,z0,z0s,karm,max(0.02,c%tvent(n)),h,nconv,laidessus,raa,sc%ras)
    ! pour le calcul de la volatilisation et des températures de culture
      call calraero(sta%P_zr,d,z0,z0s,karm,max(0.02,c%tvent(n))*1.5,h,nconv,laidessus,sc%raamax,rasmax)
      call calraero(sta%P_zr,d,z0,z0s,karm,max(0.02,c%tvent(n))*0.5,h,nconv,laidessus,sc%raamin,rasmin)

! résistances de surface
! ** le sol


    ! la culture principale
      if (p(1)%lai(AS,n) > 0.0) then
        p(1)%rc = p(1)%P_rsmin * (0.5 * p(1)%lai(AS,n) + 1) / p(1)%lai(AS,n)
      ! effet dsat et rayonnement  d'après Stockle et Kjelgaard (1996)
        p(1)%rc = p(1)%rc * (dsat * 0.039 + 0.45) * 28 / (2.5 + c%trg(n))
      ! effet CO2 si changements climatiques  (Idso, 1991)
        if (sta%P_codeclichange == 2 .and. p(1)%fco2 > 0.) then
         ! dr 23/09/2014 je passe fco2s en variable plante pour pouvoir le sortir
         ! DR 23/09/2014  bug dans le calcul de fco2s : le deuxieme terme de l'equation etait la division d'un entier par un entier = un entier ...
          p(1)%fco2s = 1. / (1. + 0.77 * (1. - p(1)%fco2 / 2.5) * (1. - c%co2(n)/330.))
          p(1)%rc = p(1)%rc * p(1)%fco2s
        endif
        if (p(1)%lai(AS,n) < 4.0) then
          rac(1) = rb / (2 * p(1)%lai(AS,n))
        else
          rac(1) = rb / 8
        endif
      endif

    ! la culture associée
      if (deuxcult) then
        p(2)%rc = p(2)%P_rsmin * (0.5 * p(2)%lai(0,n-1) + 1) / p(2)%lai(0,n-1)
        p(2)%rc = p(2)%rc * (dsat * 0.039 + 0.45) * 28 / (2.5 + c%trg(n) * f1)
        if (sta%P_codeclichange == 2) p(2)%rc = p(2)%rc * (1.4 - 0.4 * c%CO2(n) / 330)
        if (p(2)%lai(0,n-1) < 4.0) then
          rac(2) = rb / (2 * p(2)%lai(0,n-1))
        else
          rac(2) = rb / 8
        endif
      ! résistance aérodynamique du niveau bas selon le schéma résistif
        if (coderesist == 2) rac(2) = rac(2) + sc%ras
      endif

    ! calcul des évaporations directes sol + surfaces feuilles
    ! mouillées en passant par Priestley-taylor
    ! LE SOL
      ePT = alpha * sc%rnet * deltat / (deltat + gamma)
      ePT = max(ePT, 0.0)
      dos = dsat + (deltat * sc%rnet - (deltat + gamma) * ePT) * raa / (ro * cp * const_to)
      dos = max(dos, 0.0)

!      write(ficdbg,*)'4.',n,dsat,sc%rnet,ePT,raa


    ! pour le sol en potentiel
    ! introduction du mulch

! DR et ml 25/02/2011 on test sur la valeur de P_codepaillage
      if (itk(1)%P_codepaillage == 2) then
         sc%eos = ((deltat * sc%rnetS) + (ro  *cp * dos * const_to / sc%ras)) * (1 - itk(1)%P_couvermulchplastique)
         sc%eos = sc%eos / (L * (deltat + gamma))
         sc%eos = min(sc%eos, sc%rnetS * (1 - itk(1)%P_couvermulchplastique))
         sc%eos = max(sc%eos, 0.0)
      else
         sc%eos = ((deltat * sc%rnetS) + (ro  *cp * dos * const_to / sc%ras)) * (1 - sc%couvermulch)
         sc%eos = sc%eos / (L * (deltat + gamma))
         sc%eos = min(sc%eos, sc%rnetS * (1 - sc%couvermulch))
         sc%eos = max(sc%eos, 0.0)
      endif

! Dr et FR le 30/05/2016 naissance de Clement
! On est surprise par le faut que tout se passe comme si le rnet et l'evap sol etaient dans la meme unité
! ligne 406 et 411
! on a des jours ou esol est negatif alors que eos est bien positif , serait-ce des pbs de valeurs tres faibles ?


! 08/07/2011 test couvermulch
!      write(71,*)'5.',n,sc%eos
!      write(71,*)'6.',n,deltat,sc%rnetS,ro,cp,dos,const_to,sc%ras,L
!      write(71,*)'7.',n,gamma, ePT,raa,alpha,sc%couvermulch


    ! pour le sol en réel

!#if DEBUG == 1
!      if (iand(sc%solnu,1) >0) call solnu_debug_read_input(1380,sc,pg,p,itk,soil,c,sta,t,1)
!      if (iand(sc%solnu,2) >0) call solnu_debug_write_input(1381,sc,pg,p,itk,soil,c,sta,t,1)
!#endif

!Elsa 01/08/2012 attention le call de solnu est incorrect!! des arguments semblent avoir été enlévés!
!      call solnu(sc%n,sc%nbCouchesSol,sc%precip,p(1)%P_codebeso,p(1)%P_codelaitr,c%tetp(sc%n),sc%delta,   &
!                sc%laiTot,sc%tauxcouv(sc%n),c%nitetcult(sc%n),soil%P_q0,soil%aevap, &
!                 soil%P_cfes,pg%P_codeactimulch,sc%ha,sc%hi,sc%hpf,sc%hurlim,soil%P_zesx,sc%hur,sc%hucc,    &
!                 sc%eos,soil%sumes00,soil%sumes10,soil%sumes20,soil%supres0,soil%ses2j00,soil%sesj00, &
!                 soil%smes020,soil%stoc0,soil%nstoc0,sc%sumes0,sc%sumes1,sc%sumes2,sc%supres,         &
!                 sc%ses2j0,sc%sesj0,sc%smes02,sc%stoc,sc%nstoc,sc%sum2,sc%esz,sc%esol,sc%esreste,sc%xmlch1,sc%xmlch2)

      call solnu(sc%n,sc%nbCouchesSol,sc%precip,sc%nitetcult(sc%n),soil%P_q0,soil%aevap, &
                 soil%P_cfes,pg%P_codeactimulch,sc%ha,sc%hi,sc%hpf,sc%hurlim,soil%P_zesx,sc%hur,sc%hucc,    &
                 sc%eos,soil%sumes00,soil%sumes10,soil%sumes20,soil%supres0,soil%ses2j00,soil%sesj00, &
                 soil%smes020,soil%stoc0,soil%nstoc0,sc%sumes0,sc%sumes1,sc%sumes2,sc%supres,         &
                 sc%ses2j0,sc%sesj0,sc%smes02,sc%stoc,sc%nstoc,sc%sum2,sc%esz,sc%esol,sc%esreste,sc%xmlch1,sc%xmlch2)

!#if DEBUG == 1
!      if (iand(sc%solnu,4) >0) call solnu_debug_read_output(1382,sc,pg,p,itk,soil,c,sta,t,1)
!      if (iand(sc%solnu,8) >0) call solnu_debug_write_output(1383,sc,pg,p,itk,soil,c,sta,t,1)
!      if (iand(sc%solnu,16) >0) call solnu_debug_test_output(1384,sc,pg,p,itk,soil,c,sta,t,1)
!#endif

! LES FEUILLES
! on estime que l'évaporation directe sur les feuilles
! consomme une partie du rnet de ces surfaces

      if (p(1)%lai(AS,n) > 0.0) then
        mouillPP = p(1)%mouill(AS)
        p(1)%Emd = (deltat * rnetP + ro * cp * dos * const_to / rac(1)) / (deltat+gamma)
        p(1)%Emd = min(p(1)%mouill(AS) * L, p(1)%Emd)
        p(1)%mouill(AS) = max(p(1)%mouill(AS) - p(1)%Emd / L,0.0)
        rnetP = max(rnetP - p(1)%Emd, 0.0)
      endif

      if (deuxcult) then
        mouillA = p(2)%mouill(AO) + p(2)%mouill(AS)
        p(2)%Emd = (deltat * rnetA + ro * cp * dos * const_to / rac(2)) / (deltat + gamma)
      ! répartition entre AO et AS en fonction de l'énergie reçue
        EmdAO = fAO * p(2)%Emd
        EmdAS = fAS * p(2)%Emd
        EmdAO = min(p(2)%mouill(AO) * L, EmdAO)
        p(2)%mouill(AO) = max(p(2)%mouill(AO) - EmdAO / L,0.0)
        EmdAS = min(p(2)%mouill(AS) * L, EmdAS)
        p(2)%Emd = EmdAO + EmdAS
        p(2)%mouill(AS) = max(p(2)%mouill(AS) - EmdAS / L,0.0)
        rnetA = max(rnetA - p(2)%Emd, 0.0)
      endif

! DR et ML 16/10/09 ajout de codepallage=3 (mulch se decomposant)
      if (itk(1)%P_codepaillage /= 2) then
        sc%Emulch = (deltat * sc%rnetS + ro * cp * dos * const_to / sc%ras) * sc%couvermulch / (deltat + gamma)
        sc%Emulch = min(sc%mouillmulch * L, sc%Emulch)
        sc%mouillmulch = max(sc%mouillmulch - (sc%Emulch / L), 0.0)
      endif



    ! évaporation directe = cumul sol+surface feuille
      sc%Edirect = sc%esol * L + sc%EmdTot + sc%Emulch


    ! calcul de doi connaissant sc%esol (venant de STICS(solnu)) et
    ! des autres évaporations directes
      if (p(1)%lai(AS,n) > 0.0) then
        termrad(1) = deltat * rnetP
        termrad(1) = termrad(1) / (deltat + gamma * (1 + p(1)%rc / rac(1)))
        termres(1) = 1 / rac(1) / (deltat + gamma * (1 + p(1)%rc / rac(1)))
      else
        termrad(1) = 0.0
        termres(1) = 0.0
      endif

      if (deuxcult) then
        termrad(2) = deltat * rnetA / (deltat + gamma * (1 + p(2)%rc / rac(2)))
        termres(2) = 1 / rac(2) / (deltat + gamma * (1 + p(2)%rc / rac(2)))
      else
        termrad(2) = 0.0
        termres(2) = 0.0
      endif

      !--termradTMP = termrad(1)+termrad(2)
      !--termresTMP = termres(1)+termres(2)

      sc%doi = ((ro * cp * const_to * dsat / raa) + (deltat * sc%rnet)      &
             - (deltat + gamma) * (termrad(1) + termrad(2) + sc%Edirect))   &
             / (ro * cp * const_to * (1 / raa + (deltat + gamma) * (termres(1) + termres(2))))

      sc%doi = max(0.0, sc%doi)

      eopP = termrad(1) + ro * cp * sc%doi * const_to * termres(1)
      eopA = termrad(2) + ro * cp * sc%doi * const_to * termres(2)

!      write(ficdbg,*)'7.',n,termrad(1),termres(1)
!      write(ficdbg,*)'8.',n,deltat,rnetP,p(1)%rc,rac(1)

    ! passage en mm
      eopA = min(eopA,rnetA)
      eopP = min(eopP,rnetP)

      eopA = eopA / L
      eopP = eopP / L
      do ii= 1, sc%P_nbplantes
        p(ii)%Emd = p(ii)%Emd / L
      end do
      sc%Edirect  = sc%Edirect  / L



      if (deuxcult) then
      ! on découpe le test pour ne pas lire p(2) si p(2) n'existe pas.
        if (p(2)%P_codetransrad == 2) then
          p(2)%largeur = (hauteurMoy(2) - p(2)%P_hautbase) / p(2)%varrapforme
        endif
      endif

      if (p(1)%P_codetransrad == 2 .and. p(1)%lai(AS,n) > 0.0) then
        p(1)%largeur = (hauteurMoy(1) - p(1)%P_hautbase) / p(1)%varrapforme
      else
        p(1)%largeur = 0.0
      endif

    ! Répartition entre AO et AS
      p(1)%eop(AS) = eopP
      p(1)%eop(AO) = 0.0

      if (deuxcult) then
        p(2)%eop(AS) = eopA * fAS
        p(2)%eop(AO) = eopA * fAO
      end if

 !     write(ficdbg,*)'9.',n,p(1)%eop(AS)


    ! on globalise sc%etm
      sc%etm = eopP + eopA + sc%esol

    ! réaffectation de la résistance aérodynamique pour le calcul de tcult
      sta%P_ra = raa

    ! affectation des variables en cas de culture pure
!      if (sc%P_nbplantes == 1) then
!        eop(1,AS) = eopP
!      endif

    ! Nb - le 11/06 - calcul de tcult temporaire pour modification de rnet

      sc%eptcult = 0.0
      do ii = 1,sc%P_nbplantes
        sc%eptcult = sc%eptcult + p(ii)%eop(AS) * p(ii)%swfac(AS) + p(ii)%eop(AO) * p(ii)%swfac(AO)
!        write(ficdbg,*)'10.',n,sc%eptcult,p(ii)%eop(1),p(ii)%swfac(1),p(ii)%eop(2),p(ii)%swfac(2)
      end do

!#if DEBUG == 1
!      if (iand(sc%caltcult_shutwall,1) >0) call caltcult_sj_debug_read_input(1560,sc,pg,p,itk,soil,c,sta,t,1,sc%AS)
!      if (iand(sc%caltcult_shutwall,2) >0) call caltcult_sj_debug_write_input(1561,sc,pg,p,itk,soil,c,sta,t,1,sc%AS)
!#endif
! DR et ML 09/02/2011 on fait 2 appel suivant le type de mulch
!!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
!!!MODIF HISAFE : ici on met RG et Visible = 1 (100%)
      if(itk(1)%P_codepaillage == 2)then   !! paillage mulch plastique
          call caltcult(itk(1)%P_codabri,sc%EmdTot,sc%hauteurMAX,sc%eptcult,sc%esol,sc%Emulch,soil%P_z0solnu,c%tmax(n),   &
                    c%tvent(n),sc%raamin,sc%rnetS,p(1)%P_codebeso,c%phoi,sc%raamax,                                 &
                    sc%tcult,sc%tcultmin,sc%tcultmax,sc%et,c%tutilrnet,sc%numdate,c%daylen,c%difftcult,           &
                    sc%nitetcult(n),sc%Ratm, &
                    sc%nbCouchesSol,sc%jul,sc%hur(1:sc%nbCouchesSol),sc%humin(1:sc%nbCouchesSol),                 &
                    sc%hucc(1:sc%nbCouchesSol),sc%laiTot,sc%tauxcouv(n),sc%posibsw,soil%P_albedo,                   &
                    itk%P_couvermulchplastique,itk(1)%P_albedomulchplastique,sta%P_albveg,    &
                    sta%P_codernet,sta%P_aangst,sta%P_bangst,c%tmin(n),c%tmoy(n),sta%P_corecTrosee,c%trg(n),        &
                    sta%P_latitude,itk(1)%P_codepaillage,p(1)%P_codelaitr, &
                    c%tpm(n),sta%P_codecaltemp,sc%albedolai,sc%rglo,sc%rnet, 1, 1)
      else
          call caltcult(itk(1)%P_codabri,sc%EmdTot,sc%hauteurMAX,sc%eptcult,sc%esol,sc%Emulch,soil%P_z0solnu,c%tmax(n),   &
                    c%tvent(n),sc%raamin,sc%rnetS,p(1)%P_codebeso,c%phoi,sc%raamax,                                 &
                    sc%tcult,sc%tcultmin,sc%tcultmax,sc%et,c%tutilrnet,sc%numdate,c%daylen,c%difftcult,           &
                    sc%nitetcult(n),sc%Ratm, &
                    sc%nbCouchesSol,sc%jul,sc%hur(1:sc%nbCouchesSol),sc%humin(1:sc%nbCouchesSol),                 &
                    sc%hucc(1:sc%nbCouchesSol),sc%laiTot,sc%tauxcouv(n),sc%posibsw,soil%P_albedo,                   &
                    sc%couvermulch,pg%P_albedomulchresidus(sc%irmulch),sta%P_albveg,sta%P_codernet,sta%P_aangst,sta%P_bangst, &
                    c%tmin(n),c%tmoy(n),sta%P_corecTrosee,c%trg(n),sta%P_latitude,itk(1)%P_codepaillage,p(1)%P_codelaitr, &
                    c%tpm(n),sta%P_codecaltemp,sc%albedolai,sc%rglo,sc%rnet, 1, 1)
       endif

!#if DEBUG == 1
!      if (iand(sc%caltcult_shutwall,4) >0) call caltcult_sj_debug_read_output(1562,sc,pg,p,itk,soil,c,sta,t,1,sc%AS)
!      if (iand(sc%caltcult_shutwall,8) >0) call caltcult_sj_debug_write_output(1563,sc,pg,p,itk,soil,c,sta,t,1,sc%AS)
!      if (iand(sc%caltcult_shutwall,16) >0) call caltcult_sj_debug_test_output(1564,sc,pg,p,itk,soil,c,sta,t,1,sc%AS)
!#endif

! --      nitetcult(n) = 1
!      write(ficdbg,*) '11.',n,c%difftcult,c%nitetcult(n)
      if (abs(c%difftcult) > 0.5) then
! --      if (abs(difftcult) >= 0.) then
! --      if (nitetcult(n) <= 1) then
        if (sc%nitetcult(n) <= 5) then
          c%tutilrnet = sc%tcult
          goto 12
        endif
      endif

! DR_2010 if(n.eq.1)pause
return
end subroutine shutwall

! déclinaison    (cf raytrans)
! pression de vapeur saturante (cf iniclim)

 
 
