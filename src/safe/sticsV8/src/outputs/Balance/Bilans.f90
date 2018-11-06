!> Module bilan lan calculated
!! Description :
!! writing of the balance file (description of the inputs, phenologicals stages, harvests components, water balance, Nitrogen balance, Carbon balance)
module Bilans

character(len=100) :: mes1008  
character(len=100) :: mes1009  
character(len=100) :: mes1010  
character(len=100) :: mes1011  
character(len=100) :: mes10111
character(len=100) :: mes1012  
character(len=100) :: mes1013  
character(len=150) :: mes1014  
character(len=100) :: mes1020  
character(len=100) :: mes1021  
character(len=100) :: mes1022  
character(len=100) :: mes1023  
character(len=100) :: mes1025  
character(len=100) :: mes1024  
character(len=100) :: mes1125  
character(len=100) :: mes1026  
character(len=100) :: mes1027  
character(len=100) :: mes1027b
character(len=100) :: mes1028  
character(len=100) :: mes1029  
character(len=100) :: mes1030  
character(len=100) :: mes1031  
character(len=100) :: mes1032  
character(len=100) :: mes1033  
character(len=100) :: mes1040  
character(len=100) :: mes1041  
character(len=100) :: mes1042  
character(len=100) :: mes1043  
character(len=100) :: mes1044  
character(len=100) :: mes1045  
character(len=100) :: mes1046  
character(len=100) :: mes1050  
!character(len=150) :: mes1051
character(len=100) :: mes1052  
character(len=100) :: mes1053  
character(len=100) :: mes1054  
character(len=100) :: mes1055  
character(len=250) :: mes1056  
character(len=100) :: mes1057  
character(len=100) :: mes1157  
character(len=300) :: mes1058  
character(len=150) :: mes1059  
character(len=100) :: mes1160  
character(len=100) :: mes1161  
character(len=100) :: mes1159  
character(len=100) :: mes1360  
character(len=100) :: mes1361  
character(len=100) :: mes2092  
character(len=100) :: mes2100  
character(len=100) :: mes1060  
character(len=250) :: mes1061  
character(len=150) :: mes1461  
character(len=100) :: mes1258  
character(len=100) :: mes1158  
character(len=250) :: mes1261  
character(len=100) :: mes1262  
character(len=100) :: mes1263  
character(len=100) :: mes1062  
character(len=100) :: mes1063  
character(len=200) :: mes1064  
character(len=150) :: mes1065  
character(len=300) :: mes1066  
character(len=100) :: mes1166  
character(len=100) :: mes1167  
character(len=100) :: mes1168  
character(len=100) :: mes1067  
character(len=100) :: mes1068  
character(len=100) :: mes1069  
character(len=100) :: mes1070  
character(len=100) :: mes4070  
character(len=100) :: mes1071  
character(len=100) :: mes1072  
character(len=1000) :: mes1073  
character(len=100) :: mes1074  
character(len=1000) :: mes1075  
character(len=100) :: mes1079
character(len=250) :: mes1080
character(len=250) :: mes1081  
character(len=1000) :: mes1082  
character(len=250) :: mes1083
character(len=250) :: mes1084
character(len=100) :: mes3181  
character(len=100) :: mes3008  
character(len=100) :: mes3009  
character(len=100) :: mes3110  
character(len=100) :: mes3111  
character(len=150) :: mes3101  
character(len=150) :: mes3011  
character(len=100) :: mes3015  
character(len=100) :: mes2001  
character(len=100) :: mes2012  
character(len=100) :: mes2014  
character(len=100) :: mes2017  
character(len=100) :: mes2018  
character(len=100) :: mes2019  
character(len=100) :: mes2031  
character(len=100) :: mes2032  
character(len=100) :: mes2020  
character(len=100) :: mes2021  
character(len=100) :: mes2022
character(len=100) :: mes20221
character(len=100) :: mes2023  
character(len=100) :: mes2024  
character(len=100) :: mes2025  
character(len=100) :: mes2026  
character(len=100) :: mes2029  
character(len=100) :: mes2030  
character(len=100) :: mes2033  
character(len=100) :: mes2034  
character(len=200) :: mes2035  
character(len=100) :: mes2036  
character(len=100) :: mes2038  
character(len=150) :: mes2039  
character(len=100) :: mes2040  
character(len=150) :: mes2041  
character(len=100) :: mes2064  
character(len=100) :: mes2065
character(len=100) :: mes2068
character(len=100) :: mes2069
character(len=100) :: mes2066  
character(len=100) :: mes2067  
character(len=150) :: mes2076  
character(len=100) :: mes2082  
character(len=100) :: mes2083  
character(len=150) :: mes2084  
character(len=100) :: mes2085  
character(len=100) :: mes2086  
character(len=100) :: mes2087  
character(len=100) :: mes2088  
character(len=150) :: mes2089  
character(len=100) :: mes2090  
character(len=100) :: mes2091  
character(len=100) :: mes2094  
character(len=100) :: mes2093  
character(len=100) :: mes2095  
character(len=100) :: mes2096  
character(len=100) :: mes2097  
character(len=100) :: mes2098  
character(len=100) :: mes2099  

character(len=60) :: mes445

character(len=250) :: mes3045  
character(len=100) :: mes4001  

character(len=60) :: mes410
character(len=60) :: mes411
character(len=60) :: mes412
character(len=60) :: mes414
character(len=100) :: mes415
character(len=60) :: mes417
character(len=60) :: mes418
character(len=60) :: mes419
character(len=60) :: mes420
character(len=60) :: mes421
character(len=60) :: mes422

character(len=60) :: mes48
character(len=60) :: mes447
character(len=100) :: mes448
character(len=100) :: mes449
!character(len=100) :: mes450

contains

!> subroutine bilanPourLesCulturesFauchees
!! Description :
!! part of balance for the cut crops

! ****
!EC et BM 02/08/2012 On pense que cette routine est inactive et pourrait être supprimées (aucun call)
! *********************************************************
subroutine bilanPourLesCulturesFauchees(sc,pg,p,itk,soil)
! *********************************************************
!!!MODIF HISAFE 8 : suppression de l'objet USM
!!!USE USM
USE Stics
USE Plante
USE Itineraire_Technique
USE Sol
USE Climat
USE Station
USE Parametres_Generaux
USE Divers

  implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc  

  type(Parametres_Generaux_), intent(IN)    :: pg  

  type(Plante_),              intent(INOUT) :: p  

  type(ITK_),                 intent(INOUT) :: itk  

  type(Sol_),                 intent(INOUT) :: soil  


  if (sc%n == p%nrecbutoir) then
!    call ecrireBilanRecolteButoir(sc,pg,soil,p,itk)
! E et B 02/08/2012 on change le nom de la routine qui sera commune à toutes les cultures DR 20/08/2012
    call ecrireBilanEauCN(sc,soil,p,itk)
  else
    if (p%sioncoupe)then
      call ecrireBilanIntercoupe(sc,pg,p,itk)
    endif
  endif



return
end subroutine bilanPourLesCulturesFauchees



!*************************************************************************************************
!> subroutine ecrireEnteteBilan
!! Description :
!! header part of the file balance
! *********************************************************
!subroutine ecrireEnteteBilan(sc,pg,p,itk,soil,c,sta,t,usma)

!!!MODIF HISAFE 8 : suppression de l'objet USM
!!!subroutine ecrireEnteteBilan(sc,pg,p,itk,soil,sta,t,usma)  !DR 19/07/2012 c n'est pas utilisé
subroutine ecrireEnteteBilan(sc,pg,p,itk,soil,sta,t)
! *********************************************************

USE Stics
USE Plante
USE Itineraire_Technique
USE Sol
!USE Climat
USE Station
USE Parametres_Generaux
USE Divers
!!!MODIF HISAFE 8 : suppression de l'objet USM
!!!USE USM

  implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc  

  type(Parametres_Generaux_), intent(IN)    :: pg  

  type(Plante_),              intent(INOUT) :: p  

  type(ITK_),                 intent(INOUT) :: itk  

  type(Sol_),                 intent(INOUT) :: soil  

!  type(Climat_),              intent(INOUT) :: c

  type(Station_),             intent(INOUT) :: sta  

  type(Stics_Transit_),       intent(IN)    :: t  

  !DR 18/07/2012 je rajoute le module usm pour les noms de fichiers pour optimistics
  !!!type(USM_),                 intent(INOUT) :: usma  !> // PARAMETER // name of the P_USM // SD // USMXML // 0

  integer :: fb  
  integer :: iz  !>  
  integer ::  jour  !>  
  integer ::  nummois  !>  
  integer ::  ancours  !>  
  integer ::  ifwater1an  

!!!MODIF HISAFE 7 : déplacement de variables
!!!supprimé de sc
 character(len=3)  :: mois            !  variable pour le stockage du mois courant (jan à dec)

! l'identifiant du fichier bilan
        fb = p%ficbil

! DONNEES d'ENTREES écrites en debut de fichier bilan INPUT DATA

    ! DR 20/07/06 on rajoute l'année sinon c'est le bazard
        if (sc%numcult > 1 .or. itk%P_codefauche == 1) &
          write(fb,mes3181) sc%annee(sc%ifwater_courant)

    ! on insère une ligne blanche
!        write(fb,'(/)')

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!        if (itk%P_codefauche == 1) then
!!!            write(fb,mes2001) sc%codeversion,sc%P_codesimul
!!!        else
!!!            write(fb,mes1008) sc%codeversion,sc%P_codesimul
!!!        endif

    ! DR 20/08/2012 en cas d'utilisation du forcage lai  on ecrit le nom du fichier lai
    !!!MODIF HISAFE 1 : suppression des chaines de caractères
    !!!    if (lge(sc%P_codesimul,'feuille') .eqv. .TRUE.) then
    !!!    if (sc%P_codesimul == 2) then
    !!!        write(fb,mes1013) p%P_flai
    !!!    endif

        write(fb,mes1009)
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!        write(fb,mes1010) sc%P_wdata1(1:index(sc%P_wdata1,'.')-1)

    ! NB - le 05/04 - ajout de l'info altitude
        if (sta%P_codaltitude == 2 .and. sta%P_altistation /= sta%P_altisimul) then
          write(fb,mes3008) sta%P_altistation
          write(fb,mes3009) sta%P_altisimul
          if (sta%P_codadret == 1) write(fb,mes3110)
          if (sta%P_codadret == 2) write(fb,mes3111)
        endif
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!       write(fb,mes1011) trim(adjustl(itk%P_ftec) )!sc%usma%P_ftec(p%ipl)
!!!        if (p%P_codeplante /= 'snu')then
!!!          write(fb,mes1012) trim(adjustl(p%P_fplt)), p%P_codevar(itk%P_variete) !sc%usma%P_fplt(p%ipl)
!!!          if (p%P_codeplante == 'stn') write(fb,mes1013) p%P_flai   !sc%usma%P_flai(p%ipl)
!!!          write(fb,mes2014) itk%P_variete
!--          write(fb,mes1012) usma%P_fplt(p%ipl),p%P_codevar(itk%P_variete)
!--          if (p%P_codeplante(ipl) == 'stn') write(fb,mes1013) usma%P_flai(p%ipl)
!!!        endif
! j'ajoute le nom des fichiers d'initialisation
!!!MODIF HISAFE 11 : Supression code inutile
!!!        write(fb,mes10111) trim(adjustl(usma%P_ficInit))!sc%usma%P_ftec(p%ipl)
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!        write(fb,mes1014) soil%P_typsol


        do iz = 1, sc%nh
!--          write(fb,50) sc%Hinit(iz), soil%NO3init(iz), sc%NH4init(iz)
!--50        format(9x,f6.1,f15.1,f15.1)
!!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
!!!          write(fb,50) soil%P_epc(iz),sc%Hinit(iz),soil%NO3init(iz),sc%NH4init(iz)
          write(fb,50) soil%P_epc(iz),soil%Hinit(iz),soil%NO3init(iz),soil%NH4init(iz)
50        format(f11.0,f12.1,2f14.1)
        end do

    ! domi : juste pour la valid solnu sinon pas d'ecriture de rmaxi
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!        if (p%P_codeplante == 'snu') write(fb,mes1065) p%rmaxi,0.0
        if (p%P_codeplante == 1) write(fb,mes1065) p%rmaxi,0.0

! DR 03/08/2015 je mets un test pour indiquer quand on est dans le cadre d'une rotation
 !!!      if(sc%P_codesuite == 1)then
  !!!         if(sc%nbans.eq.1)then
  !!!             write(fb,mes449)
  !!!         else
  !!!             if(pg%P_codeinitprec == 2.and. sc%numcult.gt.1 ) write(fb,mes449)
  !!!         endif
  !!!     endif
       ! if(pg%P_codeinitprec == 2.and. sc%numcult.gt.1) write(fb,mes449)
      ! if(pg%P_codeinitprec == 2) write(fb,mes450)


    !. Initialisations plante
        if (p%codeinstal == 1) then
          write(fb,mes1020)
          if (p%P_codelaitr == 1) then
            write(fb,47) p%P_stade0,p%lai(sc%AOAS,0),p%masec(sc%AOAS,0),p%P_zrac0,  &
                         p%magrain(sc%AOAS,0),p%P_QNplante0,p%inn0(sc%AOAS),p%P_resperenne0
          else
            write(fb,47) p%P_stade0,sc%tauxcouv(0),p%masec(sc%AOAS,0),p%P_zrac0,    &
                         p%magrain(sc%AOAS,0),p%P_QNplante0,p%inn0(sc%AOAS),p%P_resperenne0
          endif
47        format(5x,a3,7(1x,f6.2))
        endif


        call julien(sc%P_iwater,sc%ansemis,mois,jour,nummois)

        !!!write (fb,mes1021) jour,sc%mois,sc%annee(sc%P_iwater),sc%P_iwater
        write (fb,mes1021) jour,mois,sc%annee(sc%P_iwater),sc%P_iwater
        ancours = sc%annee(sc%ifwater_courant)
        if (sc%ifwater_courant  >  sc%nbjsemis) then
          ifwater1an = sc%ifwater_courant - sc%nbjsemis
        else
          ifwater1an = sc%ifwater_courant
        endif

        !!!call julien(ifwater1an,ancours,sc%mois,jour,nummois)
        call julien(ifwater1an,ancours,mois,jour,nummois)
        if (itk%P_codefauche == 1) then
          !!!write (fb,mes2017) jour,sc%mois,ancours,ifwater1an
          write (fb,mes2017) jour,mois,ancours,ifwater1an
        else
          !!!write (fb,mes1022) jour,sc%mois,ancours,ifwater1an,sc%ifwater_courant
          write (fb,mes1022) jour,mois,ancours,ifwater1an,sc%ifwater_courant
        endif

! on insère une ligne blanche
        write(fb,'(//)')

        if (itk%P_codefauche == 1) then
          if (itk%lecfauche) then
            if (itk%P_codemodfauche == 2) then
              write(fb,mes2018)
            else
              write(fb,mes2019)
              if (t%P_codetempfauche == 1) then
                write(fb,mes2031)
              else
                write(fb,mes2032)
              endif
            endif
          else
            write(fb,mes2020) itk%P_stadecoupedf
          endif
        endif

      ! domi 30/09/97 type d'irrigation
        if (itk%P_codecalirrig == 2) then
          write(fb,mes2021)
        else
          write(fb,mes2022) itk%P_ratiol
          if(t%P_codedate_irrigauto == 1 ) then
            write(fb,mes20221)t%P_datedeb_irrigauto,t%P_datefin_irrigauto
          endif
        endif
      ! domi 30/09/97 type de fertilisation
        if (t%P_codecalferti == 2) then
          write(fb,mes2023)
        else
          write(fb,mes2024) t%P_ratiolN
        endif

      ! cas d'un drainage agricole
        if (soil%P_codrainage == 1) then
          write(fb,mes3011) pg%P_Bformnappe,soil%Ldrains,soil%P_Ksol,   &
                            soil%P_profdrain,p%P_sensanox,pg%P_distdrain
          write(fb,mes3015) soil%P_Infil(sc%NH)
        endif


       ! DR 30/04/2013 je rajoute la methode de calcul de l'etp choisie dans le fichier station
      if(sta%P_codeetp.eq.3.or.p%P_codebeso.eq.2)then
         write(fb,mes448),' calcul of Shuttleworth and Wallace'
      else
         if(sta%P_codeetp.eq.1)write(fb,mes448),' Penman reading'
         if(sta%P_codeetp.eq.2)write(fb,mes448),' Penman calculated'
         if(sta%P_codeetp.eq.4)write(fb,mes448),' calcul of Priestley-Taylor'
      endif



return
end subroutine ecrireEnteteBilan




!> subroutine ecrireBilanDefaut
!! Description :
!! second part of the file balance (irrigations and fertilisations, phenologicals stages, harvests components, water balance, Nitrogen balance, Carbon balance)
!!  harvests components, water balance, Nitrogen balance, Carbon balance)
! **********************************************
subroutine ecrireBilanDefaut(sc,pg,soil,p,itk)
! **********************************************

USE Stics
USE Parametres_Generaux
USE Sol
USE Plante
USE Itineraire_Technique
USE Divers

    implicit none

    integer, parameter :: nb_stades_max = 30

    type(Stics_Communs_),       intent(IN)    :: sc  
    type(Parametres_Generaux_), intent(IN)    :: pg  
    type(Sol_),                 intent(IN)    :: soil  
    type(Plante_),              intent(INOUT) :: p  
    type(ITK_),                 intent(IN)    :: itk  


    integer           :: fb  
    integer           :: jour  !>  
    integer           :: numMois  !>  
    integer           ::  julrec  
    integer           :: dureecycle  !>  
    integer           ::  irecolte  !>  
    integer           ::  iz  
    character(len=3)  :: mois  
    real              :: stcum  !>  
    real              ::  stcumdrp  !>  
    real              ::  rdtote  !>  
    real              ::  effN  
    real              :: msrec  !>  
    real              ::  rendement  !>  
    ! 10/10/2014 dr integre ajout BM
    real              ::  nbdegrains
    !
    real              ::  pgrainfrais  !>  
    real              ::  qresMS  
    real              ::  cellulose
    real              ::  Cplante

    integer           :: nbstades  
!    character(len=3), allocatable :: stades(:)
    character(len=3), dimension(nb_stades_max) :: stades


  ! l'identifiant du fichier bilan
    fb = p%ficbil

! IRRIGATIONS
! écriture du bilan d'irrigation
!    call bilanIrrigation(sc,p%nplt,sc%n,fb,'final')
!! DR 22/10/2010 resoudre pb de debut de calcul de l'irrigation
!! pour le moment je mets 1 sinon on ne prend pas en compte les irrigations

    call bilanIrrigation(sc,1,sc%n,fb,'final')

! FERTILISATIONS
! écriture du bilan des fertilisations

    call bilanFertilisations(sc,itk,1,sc%n,fb,'final')

! ** APPORTS DE RESIDUS DE CULTURE
    write(fb,mes1029)
! Travail du sol / Apports de résidus organiques
!     1) pas de travail du sol ni d'incorporation de residus
    if(itk%P_nbjtrav == 0 .and. itk%P_nbjres == 0) then
         write(fb,mes1030)
    else
!     2)travail du sol
        do iz = 1,itk%P_nbjtrav
             write(fb,mes1031) itk%P_jultrav(iz), itk%P_proftrav(iz)
        enddo
! 3) incorporation de residus
        do iz = 1,itk%P_nbjres
     ! ** Bruno - impression de la quantité de MS et non de MF pour tester les erreurs d'entrée
             qresMS = itk%P_qres(iz) * (1. - itk%P_eaures(iz)/100.)
             write(fb,mes1032) itk%P_coderes(iz), itk%P_julres(iz),itk%P_profres(iz),qresMS, itk%P_CsurNres(iz)
        end do
! fin modif STICS-SIG
    endif

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!    if (p%P_codeplante /= 'snu') then
    if (p%P_codeplante /= 1) then

     ! Opérations particulières
      if(itk%P_codrognage == 2)  write(fb,*) mes410
      if(itk%P_codeffeuil == 2)  write(fb,*) mes411

    ! bilan DEVELOPPEMENT
      write (fb,mes1033)
      if (p%P_codephot == 1 .and. p%P_phobase < p%P_phosat)      write (fb,mes1040)
      if (p%P_codephot == 1 .and. p%P_phobase > p%P_phosat)      write (fb,mes1041)
      if (p%P_codephot == 1 .and. p%P_jvc(itk%P_variete) > 0.)   write (fb,mes1042)
      if (p%P_codephot == 1 .and. p%P_jvc(itk%P_variete) == 0.)  write (fb,mes1043)
      if (p%P_codephot == 2 .and. p%P_jvc(itk%P_variete) == 0.)  write (fb,mes1044)

      if (p%P_codetemp == 1) write (fb,mes1045)
      if (p%P_codetemp == 2) write (fb,mes1046)


!
! DR 17/08/06 on rajoute un test sur nlev
! pour que la premiere annee on ne calcule pas de levée (debourrement)
! ces 3 conditions servent à sortir du bilan pour ne pas ecrire des stades faux
! la premiere annee d'un enchainement de perenne avec bidabe
      if (p%P_codedormance == 3 .and. pg%P_codeinitprec == 2 .and. sc%numcult == 1) then
        p%nlev = 0
      endif

      if (p%rfvi == 0. .and. p%nlev == 0) then
        write(fb,mes1050)
        return
      endif

      if (p%ilevs == 0) then
        write(fb,mes1050)
        return
      endif



!

    ! parcours de développement de la culture
      !write(fb,mes1051)
      write(fb,mes2076)

      stcum = 0.
      stcumdrp = 0.

      !TODO: découper les appels à l'écriture des stades de développement par paquet
      !       pour intercaler des écritures. Ex : mes1053 = "('    Stades végétatifs')"
      !      Pour différencier les types de stades dans le bilan

    ! démarrage
      nbstades = 3
!      allocate(stades(nbstades))
      stades(1:nbstades) = (/'ddo','fdo','plt'/)
      call bilanStadesDeveloppement(sc,p,itk,nbstades,stades,stcum,stcumdrp)

!      deallocate(stades)

    ! stades végétatifs
       write(fb,mes1053)
     ! DR 19/07/2012 je rajoute le stade germination
    !DR 23/08/2012 pour la vigne je n'ecris pas le stade germination

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!      if(p%P_codeplante.eq.'vig')then
      if(p%P_codeplante == 22)then
         nbstades = 5
!         allocate(stades(nbstades))
         stades(1:nbstades) = (/'lev','amf','lax','sen','lan'/)
      else
         nbstades = 6
!         allocate(stades(nbstades))
         stades(1:nbstades) = (/'ger','lev','amf','lax','sen','lan'/)
      endif
      call bilanStadesDeveloppement(sc,p,itk,nbstades,stades,stcum,stcumdrp)

!      deallocate(stades)

    ! stades reproducteurs
      write(fb,mes1054)
      nbstades = 6
!      allocate(stades(nbstades))
      stades(1:nbstades) = (/'flo','drp','des','mat','fde','rec'/)
      call bilanStadesDeveloppement(sc,p,itk,nbstades,stades,stcum,stcumdrp)

!      deallocate(stades)

!        nbstades = 12
!        allocate(stades(nbstades))
!        stades = (/'ddo','fdo','plt','lev','amf','lax','sen','lan','drp','mat','rec','cut'/)
!        call bilanStadesDeveloppement(sc,p,itk,nbstades,stades,stcum)


    ! durée du cycle
      dureecycle = p%nrec - p%nplt
      write(fb,mes1055) dureecycle


    ! récolte
      call bilanRecolte(sc,p,itk,fb)

      if (p%group == -1)then
        write(fb,mes1056)
      endif


! -------------------------------------------------------
!  3. CROISSANCE & ABSORPTION d'AZOTE
! -------------------------------------------------------

    ! ajout NB le 7/4/98 pour fruit
      if (p%P_codeindetermin == 2) then
        p%nbgrains(sc%AOAS) = p%nbfruit + p%nfruit(sc%AOAS,p%P_nboite)

        if (p%nbgrains(sc%AOAS) == 0) then
          p%pgrain(sc%AOAS) = 0
          write(fb,mes2091)
        else
          p%pgrain(sc%AOAS) = p%magrain(sc%AOAS,p%nrec) / p%nbgrains(sc%AOAS)
        endif
      endif

      if(itk%P_codcueille == 2) then
        if(itk%P_nbcueille == 1) p%nbrecolte=2
        do irecolte = 1,p%nbrecolte-1
          if(itk%P_nbcueille == 1) rdtote = p%magrain(sc%AOAS,p%nrec)/100.
          if(itk%P_nbcueille == 2) rdtote = p%rdtint(sc%AOAS,irecolte)/100.
          p%masec(sc%AOAS,p%nrec) = p%masec(sc%AOAS,p%nrec) + rdtote
        end do
      endif


      write(fb,mes1057)


    ! jour de récolte
      julrec = p%nrec + sc%P_iwater - 1 ! tCal1JanAbs

      if (julrec > sc%nbjsemis) then
        call julien(julrec-sc%nbjsemis,sc%annee(julrec),mois,jour,nummois)
      else
        call julien(julrec,sc%annee(julrec),mois,jour,nummois)
      endif
      write(fb,mes1157) jour,mois,sc%annee(julrec)

      msrec = 1. - p%h2orec(sc%AOAS)

    ! DR 16/05/06 dans le cas ou y'a pas de grain ni de masec on a un rendt = 0.
      if(p%magrain(sc%AOAS,p%nrec) <= 0)then
        rendement=0.0
      else
        rendement = p%magrain(sc%AOAS,p%nrec) / 100 / msrec
      endif


      if(p%pgrain(sc%AOAS) <= 0)then
        pgrainfrais = 0.0
      else
        pgrainfrais = p%pgrain(sc%AOAS)/msrec
      endif



!  Modifs Bruno juin 2014
      p%rendementsec = p%magrain(sc%AOAS,p%nrec) / 100.
      nbdegrains = p%nbgrains(sc%AOAS)

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!      if(p%P_codeplante.eq.'bet') then
      if(p%P_codeplante == 18) then
         p%rendementsec = p%matuber_rec
         nbdegrains = p%P_nbinflo * p%densite
      endif

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!      if(p%P_codeplante.eq.'men') then
      if(p%P_codeplante ==24) then
         p%rendementsec = p%masec(sc%AOAS,p%nrec)
      endif

      rendement = 0.
      if(msrec > 0.) rendement = p%rendementsec / msrec

      write(fb,mes1058) p%masec(sc%AOAS,p%nrec), p%rendementsec ,     &
                        p%h2orec(sc%AOAS)*100., rendement, nbdegrains

      pgrainfrais = 0.
      if(msrec > 0.) pgrainfrais = p%pgrain(sc%AOAS)/msrec

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!      if(p%P_codeplante.eq.'bet') pgrainfrais = rendement / p%densite * 1000.
      if(p%P_codeplante ==18) pgrainfrais = rendement / p%densite * 1000.
!  Fin modifs Bruno


      write(fb,mes1059) p%densite,p%h2orec(sc%AOAS)*100.,pgrainfrais

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!      if(p%P_codeindetermin == 2.and.p%P_codeplante.ne.'bet' ) then
      if(p%P_codeindetermin == 2.and.p%P_codeplante /= 18) then
        write(fb,mes1160) p%P_nbinflo
        if(p%nbfruit > 0.) write(fb,mes1161) p%nbfruit,   &
                          (p%magrain(sc%AOAS,p%nrec)      &
                        -  p%pdsfruit(sc%AOAS,p%P_nboite))/100.
      endif

      if(p%P_codeindetermin == 1) write (fb,mes1159) p%vitmoy(sc%AOAS)

      if(p%P_phyllotherme > 0.) write (fb,mes2092) p%nbfeuille

    ! nombre de jours remplissage bloqué par la température
      if(p%P_codetremp == 1) write(fb,mes2100) p%nbj0remp

      write(fb,mes1060) p%msneojaune(sc%AOAS)

    ! NB le 03/05
      if(itk%P_codetaille == 2) write(fb,mes1360) p%mabois(sc%AOAS)
      if(p%P_codeperenne == 2) write(fb,mes1361) p%resperenne(sc%AOAS)



      write(fb,mes1061) p%QNplante(sc%AOAS,p%nrec),p%QNgrain_nrec,p%CNplante_nrec
     if(p%magrain(sc%AOAS,p%nrec) > 0.) write(fb,mes1461) p%CNgrain_nrec,p%CNgrain_nrec*5.70

    ! efficience de l'azote_engrais (CAU/CRU)
       effN = 1.
       if(sc%totapN > 0.) effN = 1.-(soil%QNvoleng + soil%QNdenit + soil%QNorgeng) / sc%totapN
       write(fb,mes1258) effN

    ! indice de récolte, sucre et huile
      p%QCplante = 0.
      if (p%masec(sc%AOAS,p%nrec) > 0 .and. p%magrain(sc%AOAS,p%nrec) > 0.) then
        write(fb,mes1158) p%magrain(sc%AOAS,p%nrec) / 100. /p%masec(sc%AOAS,p%nrec)
        write(fb,mes1261) msrec*100.,p%CNgrain_nrec * msrec
        if(p%sucre(sc%AOAS) > 0.) write(fb,mes1262) p%sucre(sc%AOAS)*100.
        if(p%huile(sc%AOAS) > 0.) write(fb,mes1263) p%huile(sc%AOAS)*100.
   ! modif Bruno mai 2012 : calcul de la quantité de C dans la plante (kg C/ha)
         cellulose = 1. - p%sucre(sc%AOAS) - p%huile(sc%AOAS)
         Cplante = 0.44*cellulose + 0.40*p%sucre(sc%AOAS) + 0.76*p%huile(sc%AOAS)
         p%QCplante = p%masec(sc%AOAS,p%nrec)*Cplante*1000.
      endif

      write(fb,'(//)')

! ---------------------------------------------------------------
!   4. Bilan eau, stress hydrique, azoté et GEL sur la culture
! ---------------------------------------------------------------
      write(fb,mes1063)
      write(fb,mes1064) p%cetm,p%cet,p%ces,p%cep,p%cprecip
      write(fb,mes1065) p%rmaxi,p%zracmax
      write(fb,mes1066) p%swfac1moy,p%turfac1moy,             &
                        p%inn1moy,p%diftemp1/p%nst1,          &
                        p%exofac1moy,p%etr_etm1moy,           &
                        p%etm_etr1moy,p%swfac2moy,            &
                        p%turfac2moy,p%inn2moy,               &
                        p%diftemp2/p%nst2,p%exofac2moy,       &
                        p%etr_etm2moy,p%etm_etr2moy
! 24/01/2011 DR pb sur le test des seuils je corrige
      if (p%gel1 < 1.or.p%gel2 < 1.) write(fb,mes1166) (1.-p%gel1)*100.,(1.-p%gel2)*100.
      if (p%mortplante == 1)    write(fb,mes1167)
      if (p%gel3 < 1.)          write(fb,mes1168) (1.-p%fgelflo)*100.
      if (p%mortplanteN == 1)   write(fb,mes2097)

      if (pg%P_codeinnact == 2) write(fb,mes1067)
      if (pg%P_codeh2oact == 2) write(fb,mes1068)

    endif ! fin des résultats culture


  ! Ecriture des bilans EAU, AZOTE, CARBONE sur toute la période de simulation
  ! E et B 02/08/2012 on cange le nom de la routine qui sera commune à toutes les cultures DR 20/08/2012
  !  call bilanSimulationDefaut(sc,soil,p,itk) ! DR 19/07/2012 pg non utilisé
  call ecrirebilanEauCN(sc,soil,p,itk) ! DR 19/07/2012 pg non utilisé

return
end subroutine ecrireBilanDefaut

!******************************************************************************************
! balance between two cuts for the cut crops
! *********************************************
!> subroutine ecrireBilanIntercoupe

subroutine ecrireBilanIntercoupe(sc,pg,p,itk)
! *********************************************
USE Stics
USE Parametres_Generaux
USE Plante
USE Itineraire_Technique
USE Divers

    implicit none
    integer, parameter :: nb_stades_max = 30

    type(Stics_Communs_),       intent(IN)    :: sc  
    type(Parametres_Generaux_), intent(IN)    :: pg  
    type(Plante_),              intent(INOUT) :: p  
    type(ITK_),                 intent(IN)    :: itk  


    integer           :: fb  
    integer           :: jul  !>  
    integer           :: ancours  !>  
    integer           :: jour  !>  
    integer           :: numMois  
    integer           :: idebsen  !>  
    integer           ::  dureecycle  
    character(len=3)  :: mois  
    real              :: stcum  !>  
    real              ::  stcumdrp  
    integer           :: nbstades  
!
!    character(len=3), allocatable :: stades(:)
    character(len=3), dimension(nb_stades_max) :: stades


  ! l'identifiant du fichier bilan
    fb = p%ficbil

    write(fb,mes2064) p%numcoupe

! DR 20/07/06
! on ecrit si on a repoussé la fauche
    if(p%nbrepoussefauche > 0)then
        write(fb,mes2096) p%nbrepoussefauche
    endif

    call dates(sc%n,sc%P_iwater,sc%annee(sc%P_iwater),jul,anCours,mois,jour,numMois)

    write (fb,mes2065) jour,mois,ancours,jul




    if (itk%P_hautcoupe(p%numcoupe) == 999)then
        write(fb,mes2069) itk%P_lairesiduel(p%numcoupe),itk%P_msresiduel(p%numcoupe)
    else
        write(fb,mes2068) itk%P_hautcoupe(p%numcoupe)
    endif
    if (p%fauchediff) write(fb,mes2066) itk%mscoupemini_courant

! IRRIGATIONS
! écriture du bilan d'irrigation pour la coupe
    call bilanIrrigation(sc,p%nplt,sc%n,fb,'coupe')

! FERTILISATIONS
! écriture du bilan des fertilisations pour la coupe
   ! write(*,*)'ferti coupe'
    call bilanFertilisations(sc,itk,p%nplt,sc%n,fb,'coupe')


! Pas de bilan des apports de résidus de culture dans l'inter-coupe


!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!    if (p%P_codeplante /= 'snu') then
    if (p%P_codeplante /= 1) then

     ! Opérations particulières
      if(itk%P_codrognage == 2)  write(fb,*) mes410
      if(itk%P_codeffeuil == 2)  write(fb,*) mes411

    ! bilan DEVELOPPEMENT
      write (fb,mes1033)
      if (p%P_codephot == 1 .and. p%P_phobase < p%P_phosat) then
        write (fb,mes1040)
      endif
      if (p%P_codephot == 1 .and. p%P_phobase > p%P_phosat) then
        write (fb,mes1041)
      endif

      if (p%P_codephot == 1 .and. p%P_jvc(itk%P_variete) > 0.0)   &
        write (fb,mes1042)

      if (p%P_codephot == 1 .and. p%P_jvc(itk%P_variete) == 0.0)  &
        write (fb,mes1043)

      if (p%P_codephot == 2 .and. p%P_jvc(itk%P_variete) == 0.0)  &
        write (fb,mes1044)

      if (p%P_codetemp == 1) write (fb,mes1045)
      if (p%P_codetemp == 2) write (fb,mes1046)

    ! recalcul des dates de stade dans le calendrier julien si elles sont simulees
      p%ilevs = tCal1JAbs(p%nlev,sc%P_iwater)
      p%iamfs = tCal1JAbs(p%namf,sc%P_iwater)
      p%ilaxs = tCal1JAbs(p%nlax,sc%P_iwater)
      p%idrps = tCal1JAbs(p%ndrp,sc%P_iwater)
      p%isens = tCal1JAbs(p%nsen,sc%P_iwater)
      idebsen = tCal1JAbs(p%ndebsen,sc%P_iwater)
      p%ilans = tCal1JAbs(p%nlan,sc%P_iwater)
      p%imats = tCal1JAbs(p%nmat,sc%P_iwater)
      p%irecs = tCal1JAbs(p%nrec,sc%P_iwater)

    ! parcours de développement de la culture
      write(fb,mes2076)

    ! premiere coupe
      if (p%numcoupe == 1) then

        stcum = 0.

        ! version 4.0
        stcumdrp = p%stpltlev

        nbstades = 9
!        allocate(stades(nbstades))
        stades(1:nbstades) = (/'lev','amf','lax','sen','lan','drp','mat','rec','cut'/)

        call bilanStadesDeveloppement(sc,p,itk,nbstades,stades,stcum,stcumdrp)

        dureecycle= sc%n - p%nlev  ! jul - ilevs

    ! les coupes suivantes
      else

      ! redemarrage
        stcum = p%udevlaires(p%numcoupe)

      ! version 4.0
        stcumdrp = p%udevlaires(p%numcoupe)

        nbstades = 9
!        allocate(stades(nbstades))
        stades(1:nbstades) = (/'lcu','amf','lax','sen','lan','drp','mat','rec','cut'/)

        call bilanStadesDeveloppement(sc,p,itk,nbstades,stades,stcum,stcumdrp)

        dureecycle = sc%n - p%nfauche(p%numcoupe-1)

      endif

    ! durée du cycle
      write(fb,mes1055) dureecycle

      if (p%group == -1)then
        write(fb,mes1056)
      endif


    ! ** CROISSANCE & ABSORPTION d'AZOTE
      write(fb,mes1057)
      write(fb,mes2082) p%masec(sc%AOAS,sc%n)
      write(fb,mes2083) p%masec(sc%AOAS,sc%n) - p%msresjaune(sc%AOAS) - p%msneojaune(sc%AOAS),  &
                        p%msresjaune(sc%AOAS)+p%msneojaune(sc%AOAS)


! dr 10/10/2014 je mets à la place les modifs de bruno
!      write(fb,mes2084) itk%P_msresiduel(p%numcoupe-1),   &
!                        p%masec(sc%AOAS,sc%n) - itk%P_msresiduel(p%numcoupe-1)
! Modif Bruno juin 2014 on calcule le rendement cumulé en fourrage
      if(p%numcoupe == 1) p%rendementsec = 0.
      p%rendementsec = p%rendementsec + p%masec(sc%AOAS,sc%n) - itk%P_msresiduel(p%numcoupe-1)
      write(fb,mes2084) itk%P_msresiduel(p%numcoupe-1), p%rendementsec
! Fin modif


      write(fb,mes2085) p%msres(sc%AOAS)
      write(fb,mes2086) itk%P_msresiduel(p%numcoupe)

          ! DR 08/11/05 on rajoute la matiere seche recoltable
      write(fb,mes2098) p%masec(sc%AOAS,sc%n) - itk%P_msresiduel(p%numcoupe-1)

      write(fb,mes2087) p%densite
      write(fb,mes2039) p%QNplante(sc%AOAS,sc%n),p%CNplante(sc%AOAS) !,sc%n)

          ! Bilan eau, stress hydrique et azoté
      write(fb,mes2088) p%numcoupe-1,p%numcoupe
      write(fb,mes1064) p%cetmcoupe,p%cetcoupe,p%cescoupe,   &
                        p%cepcoupe,p%cprecipcoupe

      write(fb,mes1065) p%rmaxi,p%zracmax
      write(fb,mes2089) p%str1coupe/p%nst1coupe,      &
                        p%stu1coupe/p%nst1coupe,      &
                        p%inn1coupe/p%nst1coupe,      &
                        p%diftemp1coupe/p%nst1coupe

      if (p%nst2coupe > 0) then
        write(fb,mes2090) p%str2coupe/p%nst2coupe,        &
                          p%stu2coupe/p%nst2coupe,        &
                          p%inn2coupe/p%nst2coupe,        &
                          p%diftemp2coupe/p%nst2coupe
      endif

      write(fb,mes1166) (1.-p%gel1)*100.,(1.-p%gel2)*100.

      if (p%mortplante == 1) write(fb,mes1167)

      if (p%gel3 < 1.) write(fb,mes1168) (1.-p%fgelflo)*100.
      if (pg%P_codeinnact == 0) write(fb,mes1067)
      if (pg%P_codeh2oact == 0) write(fb,mes1068)


          ! DR 17/01/06 on cumul pour le bilan
      p%QNplanteCtot = p%QNplanteCtot + p%QNplante(sc%AOAS,sc%n)

    endif ! fin des résultats culture

! ** BILANS EAU et AZOTE SUR L'ENSEMBLE DE LA SIMULATION
! - On le supprime pour l'intercoupe

return
end subroutine ecrireBilanIntercoupe

!*****************************************************************************************************************************
!EC et BM 08/2012 routine incative ; avant utilisée pour cultures fauchées; maitenant on utilise la même par défaut BilanEauCN
    ! 21/08/2012 DR je renomme ce n'est pas tres parlant
!subroutine ecrireBilanRecolteButoir(sc,pg,soil,p,itk)
subroutine ecrireBilanFinCultureFauchee(sc,p,itk)


USE Stics
USE Climat
USE Plante
USE Itineraire_Technique
USE Divers

    implicit none

    type(Stics_Communs_),       intent(IN)    :: sc  
    type(Plante_),              intent(INOUT) :: p  
    type(ITK_),                 intent(IN)    :: itk  


    integer           :: fb  
    integer           :: jul  !>  
    integer           :: ancours  !>  
    integer           :: jour  !>  
    integer           :: numMois  
    integer           :: k  
    character(len=3)  :: mois  


  ! l'identifiant du fichier bilan
    fb = p%ficbil

    write(fb,mes2025)

! On ne peut pas changer iplt-P_iwater+1 par nplt car nplt a été modifié en cours de simulation (jour de coupe).

! IRRIGATIONS
! écriture du bilan d'irrigation pour la coupe
   ! call bilanIrrigation(sc,sc%iplt(p%ipl)-sc%P_iwater+1,p%nrec,fb,'coupe')
   call bilanIrrigation(sc,1,sc%P_ifwater,fb,'coupe')

! FERTILISATIONS
! écriture du bilan des fertilisations pour la coupe
  !  write(*,*)'ferti fin coupes'
  !  call bilanFertilisations(sc,itk,sc%iplt(p%ipl)-sc%P_iwater+1,p%nrec,fb,'coupe')
   call bilanFertilisations(sc,itk,1,sc%P_ifwater,fb,'coupe')


! Apports des résidus
    if (itk%P_qres(1) == 0.) then
      write(fb,mes2034) ! pas d'apports
    else
      write(fb,mes2035) itk%P_qres(1),itk%P_CsurNres(1),itk%P_jultrav(1),itk%P_proftrav(1)
    endif



!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!    if (p%P_codeplante /= 'snu') then
    if (p%P_codeplante /= 1) then

!     ! Opérations particulières
!      if(itk%P_codrognage == 2)  write(fb,*) mes410
!      if(itk%P_codeffeuil == 2)  write(fb,*) mes411
!
!    ! bilan DEVELOPPEMENT
!      write (fb,mes1033)
!      if (p%P_codephot == 1 .and. p%P_phobase < p%P_phosat) then
!        write (fb,mes1040)
!      endif
!      if (p%P_codephot == 1 .and. p%P_phobase > p%P_phosat) then
!        write (fb,mes1041)
!      endif
!
!      if (p%P_codephot == 1 .and. p%P_jvc(itk%P_variete) > 0.0)   &
!        write (fb,mes1042)
!
!      if (p%P_codephot == 1 .and. p%P_jvc(itk%P_variete) == 0.0)  &
!        write (fb,mes1043)
!
!      if (p%P_codephot == 2 .and. p%P_jvc(itk%P_variete) == 0.0)  &
!        write (fb,mes1044)
!
!      if (p%P_codetemp == 1) write (fb,mes1045)
!      if (p%P_codetemp == 2) write (fb,mes1046)


    ! On va ecrire les dates de coupes  nfauche(numcoupe)
    ! parcours de développement de la culture
!      write(fb,mes2076)

    ! Les coupes

      write(fb,mes2036)
      do k = 1, p%numcoupe - 1

      ! domi le 23/01/98 : les dates de fauches sont dans le calendrier hydrique
        jul = p%nfauche(k) + sc%P_iwater - 1 ! TODO: tCal1J...
        ancours = sc%annee(jul)
        if (jul  >  sc%nbjsemis) jul = jul - sc%nbjsemis
        call julien(jul,ancours,mois,jour,nummois)
        write(fb,1300) k,jour,mois,ancours
1300    format(12x,i2,10x,I2,'-',A3,'-',I4,2F14.0)
      end do


!    ! durée du cycle
!      write(fb,mes1055) dureecycle
!
!      if (p%group == -1)then
!        write(fb,mes1056)
!      endif


    ! ** CROISSANCE & ABSORPTION d'AZOTE

    ! domi -- dans le bilan final la matiere seche est celle des 3 coupes + la ms residuelle

    ! DR 15/11/05 NON la ms final est vraiment la recoltée sans masecneo
    !-- p%masectot=p%masectot+p%masecneoC

!--       CNplanteC(ipl,n)=QNplanteC(ipl,n)/(p%masectot*10)

    ! DR 170106 le QNplante et le CNplante correspondent à la matiere seche reellement recoltée
      p%CNplante(sc%AOAS) = p%QNplanteCtot / (p%masectot * 10)

      p%magrain(sc%AOAS,p%nrec) = p%masectot

      write(fb,mes1057)

      write(fb,mes2038) p%masectot

!TODO : msg bilan intercoupe, à voir si on les conserve ou si on supprime
!      write(fb,mes2082) p%masec(sc%AOAS,sc%n)
!      write(fb,mes2083) p%masec(sc%AOAS,sc%n) - p%msresjaune(sc%AOAS) - p%msneojaune(sc%AOAS),  &
!                        p%msresjaune(sc%AOAS)+p%msneojaune(sc%AOAS)
!
!      write(fb,mes2084) itk%P_msresiduel(p%numcoupe-1),   &
!                        p%masec(sc%AOAS,sc%n) - itk%P_msresiduel(p%numcoupe-1)
!
!      write(fb,mes2085) p%msres(sc%AOAS)
!      write(fb,mes2086) itk%P_msresiduel(p%numcoupe)
!
!    ! DR 08/11/05 on rajoute la matiere seche recoltable
!      write(fb,mes2098) p%masec(sc%AOAS,sc%n) - itk%P_msresiduel(p%numcoupe-1)
!
!      write(fb,mes2087) p%densite

      write(fb,mes2039) p%QNplante(sc%AOAS,sc%n),p%CNplante(sc%AOAS) !,sc%n)
!      write(fb,mes2040) itk%P_ressuite,p%qressuite,p%CsurNressuite   DR et EC 25/07/2012 on prend les residus y compris les roots
      write(fb,mes2040) itk%P_ressuite,p%qressuite_tot,p%CsurNressuite_tot


    ! Bilan eau, stress hydrique et azoté

      write(fb,mes2041)

      write(fb,mes1064) p%cetm,p%cet,p%ces,p%cep,p%cprecip

      write(fb,mes1065) p%rmaxi,p%zracmax

! TODO: à voir si on affiche ou pas (issu de intercoupe)
!    ! gel
!      write(fb,mes1166) (1.-p%gel1)*100.,(1.-p%gel2)*100.
!
!      if (p%mortplante == 1) write(fb,mes1167)
!
!      write(fb,mes1168) (1.-p%fgelflo)*100.
!      if (pg%P_codeinnact == 0) write(fb,mes1067)
!      if (pg%P_codeh2oact == 0) write(fb,mes1068)

    endif ! fin des résultats culture

! ** BILANS EAU et AZOTE SUR L'ENSEMBLE DE LA SIMULATION

! DR 20/08/2012 on ecrit le bilan eau C N de Bruno et elsa
!    call bilanSimulationRecolteButoir(sc,pg,soil,p)

return
!end subroutine ecrireBilanRecolteButoir
end subroutine ecrireBilanFinCultureFauchee


! *****************************************************************************************
!
!> WATER, MINERAL NITROGEN AND ORGANIC C AND N BALANCE
!subroutine bilanSimulationDefaut(sc,pg,soil,p,itk)    ! DR 19/07/2012 pg n'est pas utilisé
!subroutine bilanSimulationDefaut(sc,soil,p,itk)
! E et B 02/08/2012 changement de nom de cette routine commune à toutes les cultures
subroutine ecrireBilanEauCN(sc,soil,p,itk)
! *****************************************************************************************

USE Stics
USE Parametres_Generaux
USE Sol
USE Plante
USE Itineraire_Technique

    implicit none

    type(Stics_Communs_),       intent(IN)    :: sc  
!    type(Parametres_Generaux_), intent(IN)    :: pg
    type(Sol_),                 intent(IN)    :: soil  
    type(Plante_),              intent(INOUT) :: p  
    type(ITK_),                 intent(IN)    :: itk  


    integer :: fb     ! descripteur du fichier bilan dans lequel écrire  
    real    :: QH2Oi  !>  
    real    ::  QH2Of  !>  
    real    ::  QNO3i  !>  
    real    ::  QNH4i  !>  
    real    ::  QNO3f  !>  
    real    ::  QNH4f  
    real    :: entree  !>  
    real    ::  sortie  
    real    :: Vp  !>  
    real    ::  Vpannuel  
    
!   Bruno: ajout mai 2012
    real    :: QNexport ! N exporté
    real    :: QNrest   ! N restitué
    real    :: Qmin     ! N mineral
    real    :: QCorgeng ! C associé au N organisé par l'engrais
    real    :: QNap     ! somme des apports N
    real    :: QCap     ! somme des apports C
    real    :: QNplantfinal   ! quantité de N dans la plante après récolte (résiduel)
    integer :: i
    character(len=45) :: typres

        fb = p%ficbil
        write(fb,mes1069) sc%maxwth
        ! DR et ML 18/08/09 modifs 7.4 vers 7.1 Java
        !--write(fb,mes1070) pg%P_TREFh, sc%tnhc, sc%tnrc


        write(fb,mes1070) sc%var_TREFh(sc%numcult),sc%tnhc,sc%tnrc
      ! vitesse de minéralisation potentielle
        Vp = 0.
        if (sc%tnhc > 0.) Vp = sc%Qminh / sc%tnhc
        Vpannuel = Vp *36525. / sc%Nhumt
        write(fb,mes4070) Vp,Vpannuel
    ! quantités d'eau et d'azote sur l'ensemble du profil
        QH2Oi = 0.0         ! TODO: certaines de ces variables sont dans la structure Transit
        QH2Of = 0.0         ! mais ne sont utilisées que pour les routines bilans,
        QNO3i = 0.0         ! donc replacées en locales pour l'instant
        QNH4i = 0.0
        QNO3f = 0.0
        QNH4f = 0.0
        do i = 1,5
!!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
 !!!         QH2Oi = QH2Oi + sc%Hinit(i) * soil%da(i) / 10. * soil%P_epc(i)
          QH2Oi = QH2Oi + soil%Hinit(i) * soil%da(i) / 10. * soil%P_epc(i)
          QNO3i = QNO3i + soil%NO3init(i)
          QNO3f = QNO3f + soil%AZnit(i)
!!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
!!!          QNH4i = QNH4i + sc%NH4init(i)
!!!          QNH4f = QNH4f + sc%AZamm(i)
          QNH4i = QNH4i + soil%NH4init(i)
          QNH4f = QNH4f + soil%AZamm(i)
        end do
        QH2Of = SUM(sc%Hur(1:int(soil%profsol))) + SUM(sc%sat(1:int(soil%profsol)))

! *************************************************************
! bilan EAU
! Pluies + Irrigations + Initialisations eaux du sol - Remontee
        entree =  QH2Oi + sc%cpluie + sc%totir - soil%remontee
        sortie =  QH2Of + sc%cestout + p%cep + sc%drat + sc%ruisselt     &
                  + p%cinterpluie + sc%cintermulch + sc%toteaunoneffic + soil%Qdraincum

        write(fb,mes1072)
        write(fb,mes1073) QH2Oi,QH2Of,sc%cpluie, sc%cestout, sc%totir, p%cep, -soil%remontee, sc%ruisselt, sc%drat, &
                          soil%Qdraincum, p%cinterpluie, sc%cintermulch, sc%toteaunoneffic, entree, sortie


! **********************************************
! bilan AZOTE MINERAL
! ------------------
! Bruno - ajout calcul du N exporte et restitue
! Elsa 30/07/2012 attention le N denitrifié est compris dans QNdenit donc nous ajoutons uniquement les emissions de N2O liées
! à la nitrification, soit Qem_N2O remplacé par Qem_N2Onit. Cette variable QNdeneng est renommée QNdenit = azote total denitrifié engrais + sol

! EC et BM 02/08/2012 les quantités de N restituées
! 23/08/2012 DR et IGC on le remonte pour avoir la valeur de QNrest
               QNrest = p%QNplantetombe(sc%AOAS) + p%QNrogne + p%QNressuite + p%QNrac

! EC et BM 02/08/2012 les quantités exportées et résiduelles dépendent du type de plante
     !*******************************
     ! cas 1 : les cultures fauchées
     if (itk%P_codefauche == 1) then
               QNexport = p%QNplantefauche
               QNplantfinal = p%QNplante(sc%AOAS,p%nrecbutoir)
!**********************************************
! Cas 2 : les cultures perennes avec cueillette
     else
        if (itk%P_codcueille == 2 .and. p%P_codeperenne == 2) then
               QNexport = p%QNgrain(sc%AOAS)
               if(p%ntaille.gt.p%nlev) then
                     QNplantfinal = p%QNplante(sc%AOAS,p%ntaille-1)-QNrest
               else
                     QNplantfinal = p%QNplante(sc%AOAS,p%nrec)-QNrest
               endif
        else
    !*******************************
    ! Cas 3 : les cultures annuelles
               QNexport = p%QNplante(sc%AOAS,p%nrec) - p%QNressuite - p%QNrac
               QNplantfinal = 0.
        endif
     endif

! Elsa 30/07/2012 on change l'ordre d'écriture des variables pour homogénéiser l'écriture des bilans N mineral et organique (1-pools, 2-entrees, 3-sorties)
        entree = p%P_QNplante0 + QNH4i + QNO3i + sc%pluieN + sc%irrigN &
                 + sc%totapN + p%Qfix(sc%AOAS) + sc%Qminh + sc%Qminr
        sortie = QNplantfinal + QNexport + QNrest + QNH4f + QNO3f &
                 + sc%Qles + soil%qlesd + soil%QNorgeng + soil%QNvoleng + soil%QNvolorg &
                 + soil%QNdenit+sc%Qem_N2Onit

        write(fb,mes1074)
        write(fb,mes1075) p%P_QNplante0, QNplantfinal, QNexport, QNrest, &
                          QNH4i, QNH4f, QNO3i, QNO3f, sc%pluieN, sc%Qles, sc%irrigN, soil%qlesd, &
                          sc%totapN, soil%QNorgeng, p%Qfix(sc%AOAS), soil%QNvoleng, sc%Qminh, soil%QNvolorg, &
                          sc%Qminr,soil%QNdenit+sc%Qem_N2Onit, &
                          entree, sortie

! **********************
! BILAN N et C ORGANIQUE
! ----------------------
! dr et ml 04/09/2014 on indexe sur ao et as pour les varaibles de l'abscission
! a faire egalement pour QCrac, QCressuite et QCrogne   et les QN itou

        Qmin = sc%Qminh + sc%Qminr - sc%QNprimed
        QNap = sc%QNresorg + p%QNressuite + p%QNrac + p%QNplantetombe(sc%AOAS) + p%QNrogne !equivalent à sc%QNapp

        entree = sc%Nhuma0 + sc%Nhumi + sc%Nb0 + sc%Nbmulch0 + sc%Nr0 + sc%Nmulch0 + soil%QNorgeng + QNap
        sortie = sc%Nhuma  + sc%Nhumi + sc%Nb  + sc%Nbmulch  + sc%Nr  + sc%Nmulch  + Qmin + sc%QNprimed



        write(fb,mes1080)
        write(fb,mes1082) sc%Nhuma0,sc%Nhuma,sc%Nhumi,sc%Nhumi,sc%Nb0+sc%Nbmulch0,sc%Nb+sc%Nbmulch,sc%Nr0,sc%Nr, &
                          sc%Nmulch0,sc%Nmulch,soil%QNorgeng,sc%QNresorg,p%QNressuite,p%QNrac,p%QNplantetombe(sc%AOAS), &
                          sc%QNprimed,p%QNrogne,Qmin,entree,sortie


        QCorgeng=0.
        Qmin =  sc%QCO2sol - sc%QCprimed
        QCap = sc%QCresorg + p%QCressuite + p%QCrac + p%QCplantetombe(sc%AOAS) + p%QCrogne !equivalent à sc%QCapp

        entree = sc%Chuma0 + sc%Chumi + sc%Cb0 + sc%Cbmulch0 + sc%Cr0 + sc%Cmulch0 + QCap
        sortie = sc%Chuma  + sc%Chumi + sc%Cb  + sc%Cbmulch  + sc%Cr  + sc%Cmulch  + sc%QCO2sol

        write(fb,mes1081)


        write(fb,mes1082) sc%Chuma0,sc%Chuma,sc%Chumi,sc%Chumi,sc%Cb0+sc%Cbmulch0,sc%Cb+sc%Cbmulch,sc%Cr0,sc%Cr, &
                          sc%Cmulch0,sc%Cmulch,QCorgeng,sc%QCresorg,p%QCressuite,p%QCrac,p%QCplantetombe(sc%AOAS),      &
                          sc%QCprimed,p%QCrogne,Qmin,entree,sortie
! FLUX CO2
        write(fb,mes1083) sc%QCO2sol-sc%QCO2hum, sc%QCO2hum, sc%QCO2sol

! FLUX N2O
        write(fb,mes1084) sc%Qem_N2Onit, sc%Qem_N2Oden, sc%Qem_N2O

! Residus de culture
! EC 06/08/2012 pour les cultures fauchées les résidus apportés au sol sont les racines mortes à la coupe qqsoit le code ressuite
      if (itk%P_codefauche == 1) then
                                              typres = "dead roots     "
      else
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!          if (p%P_codeplante == "snu") then
          if (p%P_codeplante == 1) then
                                              typres = "none           "
          else
          !!!MODIF HISAFE 1 : suppression des chaines de caractères
             if(itk%P_ressuite==1) typres = "roots          "
             if(itk%P_ressuite==2) typres = "whole crop     "
             if(itk%P_ressuite==3) typres = "straw   + roots"
             if(itk%P_ressuite==4) typres = "stubble + roots"
             if(itk%P_ressuite==5) typres = "stubble of residu type 9 + roots "
             if(itk%P_ressuite==6) typres = "stubble of residu type 10+ roots "
             if(itk%P_ressuite==7) typres = "prunings       "

          endif
      endif

  !      write(fb,mes1062) typres, p%qressuite, p%CsurNressuite ! DR et EC 25/07/2012 on prend les residus y compris les roots
        write(fb,mes1062) typres, p%qressuite_tot, p%CsurNressuite_tot


return
end subroutine ecrireBilanEauCN


! EC et BM 02/08/2012 les bilans sont déjà calculés dans subroutine bilanSimulationDefaut; donc on supprime cette partie et on change le call dans le bilan cultures fauchées
!> water and nitrogen balance
! *****************************************************
subroutine bilanSimulationRecolteButoir(sc,pg,soil,p)
! *****************************************************

USE Stics
USE Parametres_Generaux
USE Sol
USE Plante
USE Divers

    implicit none

    type(Stics_Communs_),       intent(IN)    :: sc  
    type(Parametres_Generaux_), intent(IN)    :: pg  
    type(Sol_),                 intent(IN)    :: soil  
    type(Plante_),              intent(INOUT) :: p  


    integer :: fb     ! descripteur du fichier bilan dans lequel écrire  
    real    :: QH2Oi  !>  
    real    ::  QH2Of  !>  
    real    ::  QNO3i  !>  
    real    ::  QNH4i  !>  
    real    ::  QNO3f  !>  
    real    ::  QNH4f  
    real    :: entree  !>  
    real    ::  sortie  
    integer :: i  


        fb = p%ficbil

    ! quantités d'eau et d'azote sur l'ensemble du profil
        QH2Oi = 0.0         ! TODO: certaines de ces variables sont dans la structure Transit
        QH2Of = 0.0         ! mais ne sont utilisées que pour les routines bilans,
        QNO3i = 0.0         ! donc replacées en locales pour l'instant
        QNH4i = 0.0
        QNO3f = 0.0
        QNH4f = 0.0

        do i = 1,5
!!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
          !!!QH2Oi = QH2Oi + sc%Hinit(i) * soil%da(i) / 10. * soil%P_epc(i)
          QH2Oi = QH2Oi + soil%Hinit(i) * soil%da(i) / 10. * soil%P_epc(i)
          QNO3i = QNO3i + soil%NO3init(i)
          QNO3f = QNO3f + soil%AZnit(i)
          !!!QNH4i = QNH4i + sc%NH4init(i)
          !!!QNH4f = QNH4f + sc%AZamm(i)
          QNH4i = QNH4i + soil%NH4init(i)
          QNH4f = QNH4f + soil%AZamm(i)
        end do

        QH2Of = SUM(sc%Hur(1:int(soil%profsol)))

        write(fb,mes1069) sc%maxwth
        write(fb,mes1070) pg%P_TREFh, sc%tnhc, sc%tnrc

! **
! bilan eau
! Pluies + Irrigations + Initialisations eaux du sol - Remontee
       entree = sc%cpluie + sc%totir + QH2Oi - soil%remontee

!--sortie = cestout + p%cep + drat + ruisselt + QH2Of    dans bilan
       sortie = sc%cestout + p%cep + sc%drat      &
              + sc%ruisselt + QH2Of - sc%et       &
              + p%cinterpluie + sc%cintermulch &
              + sc%toteaunoneffic + soil%Qdraincum

        write(fb,mes1072)
        write(fb,mes1073) sc%cpluie, sc%cestout, sc%totir, p%cep,           &
                         -soil%remontee, sc%ruisselt, sc%drat,              &
                          soil%Qdraincum, p%cinterpluie, sc%cintermulch,    &
                          sc%toteaunoneffic, QH2Oi, QH2Of, entree, sortie

! **
! bilan azote
        entree = sc%pluieN + sc%irrigN + sc%totapN       &
               + p%Qfix(sc%AOAS) + sc%Qminh + sc%Qminr     &
               + p%P_QNplante0 + QNO3i + QNH4i


        p%QNplanteCtot = p%QNplanteCtot + p%QNplante(sc%AOAS,p%nrec)

        sortie = sc%Qles + soil%qlesd + soil%QNorgeng + soil%QNvolorg     &
               + soil%QNvoleng + soil%QNdenit + p%QNplanteCtot + QNO3f + QNH4f


        write(fb,mes1074)

! DR 170106 on met le Qnplante cumulé des coupes
        write(fb,mes1075) sc%pluieN, p%QNplanteCtot - p%QNressuite,             &
                          sc%irrigN, p%QNressuite + soil%Qminrcult,             &
                          sc%totapN, sc%Qles, p%Qfix(sc%AOAS), soil%QNorgeng,   &
                          sc%Qminh, soil%QNvoleng, sc%Qminr, soil%QNvolorg,     &
                          soil%QNdenit, p%P_QNplante0, soil%qlesd, QNO3i, QNO3f, &
                          QNH4i, QNH4f, entree, sortie

return
end subroutine bilanSimulationRecolteButoir



!> IRRIGATION
subroutine bilanIrrigation(sc,debut,fin,fb,type)

USE Stics
USE Divers

    implicit none

!: Arguments
    type(Stics_Communs_), intent(IN)    :: sc  
    integer,              intent(IN)    :: debut    ! debut de periode de calcul des apports irrigation  
    integer,              intent(IN)    :: fin      !   fin de periode de calcul des apports irrigation  
    integer,              intent(IN)    :: fb       ! descripteur du fichier dans lequel écrire  
    character(len=5),     intent(IN)    :: type     ! type de bilan => coupe ou final  

!: Variables locales
    integer :: nApports  
    real    :: qApports  
    integer :: jul  !>  
    integer ::  ancours  !>  
    integer ::  jour  !>  
    integer ::  numMois  
    character(len=3) :: mois  
    integer :: k  


      write (fb,mes2026)
      !p%totircoupe(p%numcoupe)=0.0

      qApports = 0. ! quantité des apports  ! =   SUM(airg(debut:fin),msc%ASk=airg/=0.)
      nApports = 0  ! nombre d'apports      ! = COUNT(airg(debut:fin),msc%ASk=airg/=0.)

    ! s'il y a eu des apports
    ! (psc%AS terrible, parce qu'il peut y avoir eu des apports
    !  sur la simulation mais psc%AS pdt la coupe concernée,
    !  mais au moins, on ne psc%ASse psc%AS dans la boucle pour rien s'il n'y en a psc%AS eu.)
      if (sc%naptot  >  0) then

      ! ligne d'entete des arrosages
        write(fb,mes1024)


!        write(*,*)'bilans',debut,fin
        do k = debut, fin ! debut et fin de période

        ! s'il y a un apport ce jour là
          if (sc%airg(k) > 0.0) then

          ! on récupère le jour julien rapporté au calendrier réel
            jul = tCal1JAbs(k,sc%P_iwater)
            call dates(k,sc%P_iwater,sc%annee(sc%P_iwater),jul,anCours,mois,jour,numMois)

            write (fb,180) jour,mois,ancours,sc%airg(k)
180         format(7x,I2,'-',A3,'-',I4,15x,F7.1)

          ! on incrémente le nombre d'apports
            nApports = nApports + 1
          ! on cumule la quantité totale d'apports
            qApports =  qApports + sc%airg(k)
            !p%totircoupe(p%numcoupe) = p%totircoupe(p%numcoupe) + airg(k)
          endif
        end do
      endif

      write (fb,mes2029) nApports

      if (nApports > 0) then
        select case(trim(type))
            case('coupe')
                write (fb,mes2067) qApports        !p%totircoupe(p%numcoupe)
            case('final')
                write (fb,mes1125) qApports
        end select
      endif


return
end subroutine bilanIrrigation


! Fertilisations
subroutine bilanFertilisations(sc,itk,debut,fin,fb,type)

USE Stics
USE Itineraire_Technique
USE Divers

    implicit none

!: Arguments
    type(Stics_Communs_), intent(IN)    :: sc  
    type(ITK_),           intent(IN)    :: itk  
    integer,              intent(IN)    :: debut    ! debut de periode de calcul des apports irrigation  
    integer,              intent(IN)    :: fin      ! fin de periode de calcul des apports irrigation  
    integer,              intent(IN)    :: fb       ! descripteur du fichier dans lequel écrire  
    character(len=5),     intent(IN)    :: type     ! type de bilan => coupe ou final



!: Variables locales
    integer :: nApports  
    real    :: qApports  
    integer :: jul  !>  
    integer ::  ancours  !>  
    integer ::  jour  !>  
    integer ::  numMois  
    character(len=3) :: mois  
    integer :: k  


    qApports = 0. ! quantité des apports  ! =   SUM(airg(debut:fin),msc%ASk=anit/=0.)
    nApports = 0  ! nombre d'apports      ! = COUNT(airg(debut:fin),msc%ASk=anit/=0.)

    write (fb,mes2030)

    if (sc%napNtot == 0) then
      write (fb,mes1026) nApports
    endif

    if (sc%napNtot > 0) then

      nApports = COUNT(sc%anit(debut:fin)> 0.)
      if(nApports.gt.0)then
         !  write(*,*)'napNtotgt0'
! Dr 17/06/2016 le type d'engrais est pas celui al
!          write (fb,mes1026) nApports, itk%P_engrais(1)
          write (fb,mes1026) nApports
      else
          write (fb,*) 'no fertilisation on the period'
      endif
      if (itk%P_codedateappN == 1) write(fb,mes4001)

      write(fb,mes1027)
      write(fb,mes1027b)
      do k = debut,fin

        if (sc%anit(k) > 0.0) then

        ! on récupère le jour julien rapporté au calendrier réel
          jul = tCal1JAbs(k,sc%P_iwater)
          call dates(k,sc%P_iwater,sc%annee(sc%P_iwater),jul,anCours,mois,jour,numMois)

          write (fb,180) jour,mois,ancours,sc%anit(k), sc%type_ferti(k)
180       format(7x,I2,'-',A3,'-',I4,15x,F6.0,25x,i2)

! TODO: n'y aurait-il pas un problème en mode CAS ?
          qApports = qApports + sc%anit(k)
          ! DR 03/09/2012  voir si ca peut servir dans rapport
          !totapNcoupe(p%numcoupe)= totapNcoupe(p%numcoupe) + sc%anit(k)

        endif
      end do

    endif

    if (nApports > 0) then
      select case(trim(type))
        case('coupe')
          write(fb,mes2033) qApports
        case('final')
          write(fb,mes1028) qApports
      end select
    endif

return
end subroutine bilanFertilisations

!TODO: messages des stades externalisés pour traduction ! DR 03/09/2012 plus necessaire all in english
!> developments stages
subroutine bilanStadesDeveloppement(sc,p,itk,nbstades,stades,stcum,stcumdrp)

USE Stics
USE Plante
USE Itineraire_Technique
USE Divers
USE Messages

    implicit none

    type(Stics_Communs_),       intent(IN)    :: sc  
    type(Plante_),              intent(INOUT) :: p  
    type(ITK_),                 intent(IN)    :: itk  
    integer,                    intent(IN)    :: nbstades  
    character(len=3),           intent(IN)    :: stades(nbstades)  
    real,                       intent(INOUT) :: stcum
    real,                       intent(INOUT) :: stcumdrp  


    integer           :: fb  
    integer           :: jul  !>  
    integer           :: ancours  !>  
    integer           :: jour  !>  
    integer           :: numMois  
    integer           :: i  !>  
    integer           ::  k  !>  
    integer           ::  dureecycle  !>  
    integer           ::  idess  
    character(len=3)  :: mois  
    character(len=50) :: car
    real              :: stdrp  
    ! DR 19/07/2012 pour la germination
    real              :: stcumger
    real              :: stdrpdes, sominter
    logical :: flagdes

! format avec stade bbch
!!!300 format(5x,A32,a3,')',7x,I2,'-',A3,'-',I4,2F14.0,2F15.1)

! format sans stade bbch
!!!301 format(5x,A32,11x,I2,'-',A3,'-',I4,2F14.0,2F15.1)

    fb = p%ficbil

    stcum = 0.

    do k = 1,nbstades
      SELECT CASE(stades(k))

      ! semis
        case('plt')
          if(p%codeinstal == 0) then
            i = sc%iplt(p%ipl)
            if (sc%iplt(p%ipl) > sc%nbjsemis) i = sc%iplt(p%ipl) - sc%nbjsemis
            call julien(i,sc%annee(sc%iplt(p%ipl)),mois,jour,nummois)
            car = 'sowing   (stage BBCH = '
            write(fb,*) car,jour,mois,sc%annee(sc%iplt(p%ipl)),stcum,stcum

          ! DR 07/02/07 si on a activé decision semis , je l'indique
            if (itk%P_codedecisemis == 1) &
              write(fb,*) mes445,p%nbjpourdecisemis

          else
            write(fb,mes1052) p%P_stade0
          endif

! DR 18/07/2012 je rajoute la  date de germination
      ! germination
        case('ger')
          i = p%igers
          if (p%igers > sc%nbjsemis) i = p%igers - sc%nbjsemis
          call julien(i,sc%annee(p%igers),mois,jour,nummois)
          ! DR 19/07/2012 on cumule depuis le semis donc on cree une nouvelle variable
          stcumger = p%stpltger
          car = 'ger calculated (stage BBCH = '
          if (sc%iplt(p%ipl) /= 0) then
            write(fb,*) car,jour,mois,sc%annee(p%igers),      &
                          p%stpltger,stcumger,                     &
                          p%lai(sc%AO,p%nger)*p%surf(sc%AO)     &
                        + p%lai(sc%AS,p%nger)*p%surf(sc%AS)
          endif


      ! levée = démarrage
        case('lev')
          i = p%ilevs
          if (p%ilevs > sc%nbjsemis) i = p%ilevs - sc%nbjsemis
          call julien(i,sc%annee(p%ilevs),mois,jour,nummois)
          !01/12/2014 la somme intermediaire entre stade doit etre stgerlev
          stcum = stcum + p%stpltlev
          car = 'lev observed (stade BBCH = '
          if (p%nlevobs == 999) car = 'lev calculated (stage BBCH = '
          if (sc%iplt(p%ipl) /= 0) then
            write(fb,*) car,jour,mois,sc%annee(p%ilevs),      &
                          p%stpltlev-p%stpltger,stcum,                     &
                          p%lai(sc%AO,p%nlev)*p%surf(sc%AO)     &
                        + p%lai(sc%AS,p%nlev)*p%surf(sc%AS)
          endif


        ! amf
        case('amf')
          i = p%iamfs
          if (p%iamfs > sc%nbjsemis) i = p%iamfs - sc%nbjsemis
          call julien(i,sc%annee(p%iamfs),mois,jour,nummois)
          car = 'amf observed (stade BBCH = '
          if (p%namfobs == 999) car = 'amf calculated (stage BBCH = '
          if (p%namf /= 0)then
            stcum = stcum + p%P_stlevamf(itk%P_variete)
            write(fb,*) car,jour,mois,sc%annee(p%iamfs),          &
                          p%P_stlevamf(itk%P_variete),stcum,            &
                          p%lai(sc%AO,p%namf) * p%surf(sc%AO)       &
                        + p%lai(sc%AS,p%namf) * p%surf(sc%AS)
          endif

! ** senescence brute à partir d'une somme de tmp
! --            i=idebsen
! --            if (idebsen > sc%nbjsemis) i=idebsen-sc%nbjsemis+1
! --            call julien(i,sc%annee(p%ilevs),mois,jour,nummois)
! --            car = 'beg. senMS'
! --            if (p%ndebsen /= 0)then
! --              write(fb,300) car,p%,jour,mois,sc%annee(idebsen),
! --     s          p%stlevsenms-p%P_stlevamf,
! --     s          p%stlevsenms+p%stpltlev,
! --     s                      p%lai(sc%AO,p%ndebsen)*p%surf(sc%AO)
! --     s                     +p%lai(sc%AS,p%ndebsen)*p%surf(sc%AS)
! --            endif


        ! lax
        case('lax')
          i = p%ilaxs
          if (p%ilaxs > sc%nbjsemis) i = p%ilaxs - sc%nbjsemis
! DR 01/12/2014 je corrige les stades y'a des pbs de recalcule de dates , fallait prendre l'annee du stade
!          call julien(i,sc%annee(p%ilevs),mois,jour,nummois)
          call julien(i,sc%annee(p%ilaxs),mois,jour,nummois)
          car = 'lax observed (stade BBCH = '
          if (p%nlaxobs == 999) car = 'lax calculated (stage BBCH = '
          if (p%nlax /= 0)then
            stcum = stcum + p%P_stamflax(itk%P_variete)
            write(fb,*) car,jour,mois,sc%annee(p%ilaxs),     &
                            p%P_stamflax(itk%P_variete),stcum,    &
                            p%lai(sc%AO,p%nlax)*p%surf(sc%AO)       &
                          + p%lai(sc%AS,p%nlax)*p%surf(sc%AS)
          endif

        ! sen
        ! stade SEN   si P_codlainet = 1
        ! PB - 08/12/2004 - si P_codeperenne=2,P_culturean=2 et pas de récolte
        !                   lors de la première année alors isens = 0
        !                 --> plantage car annee([isens=]0) n'existe pas.
        case('sen')
          p%isens = max(1,p%isens)
          if (p%P_codlainet == 1) then
            i = p%isens
            if (p%isens > sc%nbjsemis) i = p%isens - sc%nbjsemis
! DR 01/12/2014 je corrige les stades y'a des pbs de recalcule de dates , fallait prendre l'annee du stade
            call julien(i,sc%annee(p%isens),mois,jour,nummois)
            car = 'sen observed (stade BBCH = '
            if (p%nsenobs == 999) car = 'sen calculated (stage BBCH = '
            if (p%nsen /= 0)then
              stcum = stcum + p%P_stlaxsen(itk%P_variete)
              write(fb,*) car,jour,mois,sc%annee(p%isens),     &
                            p%P_stlaxsen(itk%P_variete),stcum,    &
                            p%lai(sc%AO,p%nsen)*p%surf(sc%AO)       &
                          + p%lai(sc%AS,p%nsen)*p%surf(sc%AS)
            endif
          endif

        ! lan
        case('lan')
          i = p%ilans
          if (p%ilans > sc%nbjsemis) i = p%ilans - sc%nbjsemis
! DR 01/12/2014 je corrige les stades y'a des pbs de recalcule de dates , fallait prendre l'annee du stade
          call julien(i,sc%annee(p%ilans),mois,jour,nummois)

            car = 'lan observed (stade BBCH = '
          if (p%nlanobs == 999) car = 'lan calculated (stage BBCH = '
          if (p%nlan /= 0)then
            stcum = stcum + p%P_stsenlan(itk%P_variete)
            write(fb,*) car,jour,mois,sc%annee(p%ilans),      &
                          p%P_stsenlan(itk%P_variete),stcum,        &
                          p%lai(sc%AO,p%nlan)*p%surf(sc%AO)     &
                        + p%lai(sc%AS,p%nlan)*p%surf(sc%AS)
          endif

        ! flo
        case('flo')
          p%iflos = max(1,p%iflos)
          i = p%iflos
          if (p%iflos > sc%nbjsemis) i = p%iflos - sc%nbjsemis
! DR 01/12/2014 je corrige les stades y'a des pbs de recalcule de dates , fallait prendre l'annee du stade
          call julien(i,sc%annee(p%iflos),mois,jour,nummois)
          if (p%P_codeperenne == 1) then
            stcumdrp = stcumdrp + p%stlevflo + p%stpltlev
          endif



          car = 'flo observed (stade BBCH = '
          if (p%nfloobs == 999) car = 'flo calculated (stage BBCH = '

          if (p%P_codeperenne == 1) then
            write(fb,*) car,jour,mois,sc%annee(p%iflos),      &
                          p%stlevflo + p%stpltlev,stcumdrp
          else
            write(fb,*) car,jour,mois,sc%annee(p%iflos),      &
                          p%stlevflo,stcumdrp,                  &
                          p%lai(sc%AO,p%nflo) * p%surf(sc%AO)   &
                        + p%lai(sc%AS,p%nflo) * p%surf(sc%AS)
          endif

        ! drp
        case('drp')
          p%idrps = max(1,p%idrps)
          i = p%idrps
          if (p%idrps > sc%nbjsemis) i = p%idrps - sc%nbjsemis
! DR 01/12/2014 je corrige les stades y'a des pbs de recalcule de dates , fallait prendre l'annee du stade
          call julien(i,sc%annee(p%idrps),mois,jour,nummois)
          car = 'drp observed (stade BBCH = '
          if (p%ndrpobs == 999) car = 'drp calculated (stage BBCH = '
          if (p%ndrp /= 0)then
            if (itk%P_codefauche == 2) then
              stcumdrp = stcumdrp + p%P_stflodrp(itk%P_variete)
              stdrp = p%P_stflodrp(itk%P_variete)
            else
              stcumdrp = stcumdrp + p%P_stlevdrp(itk%P_variete)
              stdrp = p%P_stlevdrp(itk%P_variete)
            endif
            write(fb,*) car,jour,mois,sc%annee(p%idrps),   &
                          stdrp,stcumdrp,                                   &
                          p%lai(sc%AO,p%ndrp)*p%surf(sc%AO)                 &
                        + p%lai(sc%AS,p%ndrp)*p%surf(sc%AS)
          endif
   flagdes=.false.
        ! des avant mat
        case('des')
        ! domi 23/06/05 si le stade ndebdes n'a pas ete atteint pb
        ! DR 01/12/2014 ***  des est avant mat tout va bien ****
!          if (p%ndebdes /= 0 .and. p%ndebdes <= p%imats) then
          if (p%ndebdes /= 0 .and. p%ndebdes < p%nmat) then
            idess = max(1,p%ndebdes + sc%P_iwater -1)
            i = idess
        ! NB - le 5/10/03 - ajout d'un test sur idess pour éviter pb
            if (idess > 0) then
              if (idess > sc%nbjsemis) i = idess - sc%nbjsemis
! DR 01/12/2014 je corrige les stades y'a des pbs de recalcule de dates , fallait prendre l'annee du stade
              call julien(i,sc%annee(idess),mois,jour,nummois)
              stcumdrp = stcumdrp + p%P_stdrpdes(itk%P_variete)
              car = 'debdes observed (stade BBCH = '
        ! ML - le 24/08/06
        ! correction d une erreur liee a un copier-colle
        ! malencontreux. A terme lorsqu'on pourra forcer le stade debdes
        ! dans le fichier technique, ces lignes seront utiles
              if (p%ndebdesobs == 999) car = 'debdes calculated (stage BBCH = '
              write(fb,*) car,jour,mois,sc%annee(idess),    &
                            p%P_stdrpdes(itk%P_variete),stcumdrp,       &
                            p%lai(sc%AO,p%ndebdes) * p%surf(sc%AO)  &
                          + p%lai(sc%AS,p%ndebdes) * p%surf(sc%AS)
            endif
            flagdes=.true.
          endif

        ! mat
        case('mat')
          if (p%nmat > 0) then
            p%imats = max(1,p%imats)
            i = p%imats
            if (p%imats > sc%nbjsemis) i = p%imats - sc%nbjsemis
! DR 01/12/2014 je corrige les stades y'a des pbs de recalcule de dates , fallait prendre l'annee du stade
            call julien(i,sc%annee(p%imats),mois,jour,nummois)
        ! 23/06/05 pb sur le calcul de ndebdes
! DR 01/12/2014
!            if (p%ndebdes /= 0 .and. p%ndebdes < p%imats) then
            if (flagdes) then
              sominter=p%P_stdrpmat(itk%P_variete)- p%P_stdrpdes(itk%P_variete)
              if(sominter.lt.0)sominter=0.
            else
              sominter=p%P_stdrpmat(itk%P_variete)
            endif
            stcumdrp = stcumdrp + sominter
            car = 'mat observed (stade BBCH = '
            if (p%nmatobs == 999) car = 'mat calculated (stage BBCH = '
            write(fb,*) car,jour,mois,sc%annee(p%imats),      &
                            sominter,stcumdrp,               &
                            p%lai(sc%AO,p%nmat) * p%surf(sc%AO)   &
                          + p%lai(sc%AS,p%nmat) * p%surf(sc%AS)
          endif
        ! DR 02/12/2014 *** des est apres mat ***
        ! on a deja ecrit mat , on regarde si des est avant ou apres rec
        ! des après mat (23/05/06)
        case('fde')
        ! domi 23/06/05 si le stade ndebdes n'a pas ete atteint -> pb
! dr 01/12/2014
!          if (p%ndebdes  /= 0 .and. p%ndebdes > p%imats) then   ! TODO: ndebdes(1) <-//- pkoi (1) ?
          if(.not.flagdes)then
          if (p%ndebdes  /= 0 .and. p%ndebdes >= p%nmat .and.p%ndebdes < p%nrec ) then   ! TODO: ndebdes(1) <-//- pkoi (1) ?
          idess = max(1,p%ndebdes + sc%P_iwater - 1)
          i = idess
        ! NB - le 5/10/03 - ajout d'un test sur idess pour éviter pb
          if (idess > 0) then
            if (idess > sc%nbjsemis) i = idess - sc%nbjsemis
! DR 01/12/2014 je corrige les stades y'a des pbs de recalcule de dates , fallait prendre l'annee du stade
            call julien(i,sc%annee(idess),mois,jour,nummois)
            if(p%ndebdes .eq. p%nmat)then
                sominter=0.
            else
                sominter=p%P_stdrpdes(itk%P_variete)- p%P_stdrpmat(itk%P_variete)
            endif
            stcumdrp = stcumdrp + sominter
            car = 'debdes observed (stade BBCH = '
            if (p%ndebdesobs == 999) car = 'debdes calculated (stage BBCH = '
            write(fb,*) car,jour,mois,sc%annee(idess),            &
                          sominter,stcumdrp,         &
                          p%lai(sc%AO,p%ndebdes) * p%surf(sc%AO)    &
                        + p%lai(sc%AS,p%ndebdes) * p%surf(sc%AS)
          endif
          endif
          endif

        ! rec
        case('rec')
          p%irecs = max(1,p%irecs)  ! on veut éviter qu'il soit égal à zéro
          i = p%irecs
          if (p%irecs > sc%nbjsemis) i = p%irecs - sc%nbjsemis
! DR 01/12/2014 je corrige les stades y'a des pbs de recalcule de dates , fallait prendre l'annee du stade
          call julien(i,sc%annee(p%irecs),mois,jour,nummois)
          !04/12/2014 dr si des est entre mat et rec ca va pas
          if(p%ndebdes <= p%nmat)then
             sominter=p%stmatrec
          else
             sominter=p%stmatrec-(p%P_stdrpdes(itk%P_variete)- p%P_stdrpmat(itk%P_variete))
          endif
          stcumdrp = stcumdrp + sominter
          car = 'rec observed (stade BBCH = '
          if (p%nrecobs == 999) car = 'rec calculated (stage BBCH = '
          if (p%nrec == p%nrecbutoir) car = 'REC butoir (stage BBCH = '
          if (p%nrec /= 0)then
            write(fb,*) car,jour,mois,sc%annee(p%irecs),      &
                          p%stmatrec,stcumdrp,                  &
                          p%lai(sc%AO,p%nrec) * p%surf(sc%AO)     &
                        + p%lai(sc%AS,p%nrec) * p%surf(sc%AS)
          endif
        ! DR 07/02/07 si on a activé decision semis, on l'indique
          if (itk%P_codedecirecolte == 1) then
             !write(fb,*) p%nbjpourdecirecolte
             write(fb,mes447) p%nbjpourdecirecolte
          endif
        ! on ajoute le cas ou ***des est apres rec***
        if(.not.flagdes)then
         if (p%ndebdes  /= 0 .and. p%ndebdes >= p%nrec )then
         idess = max(1,p%ndebdes + sc%P_iwater - 1)
          i = idess
          stdrpdes=0.
        ! NB - le 5/10/03 - ajout d'un test sur idess pour éviter pb
          if (idess > 0) then
            if (idess > sc%nbjsemis) i = idess - sc%nbjsemis
! DR 01/12/2014 je corrige les stades y'a des pbs de recalcule de dates , fallait prendre l'annee du stade
            call julien(i,sc%annee(idess),mois,jour,nummois)
            car = 'debdes observed (stade BBCH = '
            if (p%ndebdesobs == 999) car = 'debdes calculated (stage BBCH = '
            write(fb,*) car,jour,mois,sc%annee(idess),            &
                          stdrpdes,stcumdrp,         &
                          p%lai(sc%AO,p%ndebdes) * p%surf(sc%AO)    &
                        + p%lai(sc%AS,p%ndebdes) * p%surf(sc%AS)
          endif
         endif
        endif

        ! jour de la coupe
        case('cut')
          jul = tCal1JAbs(sc%n,sc%P_iwater)
          ancours = sc%annee(jul)
          dureecycle = jul-p%ilevs
          if (jul  >  sc%nbjsemis) jul = jul - sc%nbjsemis
          call julien(jul,ancours,mois,jour,nummois)
          car = 'cut  day'
          write (fb,*) car,jour,mois,ancours,p%somcour,       &
                         stcum+p%somcour,                       &
                         p%lai(sc%AO,sc%n)*p%surf(sc%AO)        &
                       + p%lai(sc%AS,sc%n)*p%surf(sc%AS)

        ! dernière coupe
        case('lcu')
          jul = p%nfauche(p%numcoupe-1) + sc%P_iwater - 1
          ancours = sc%annee(jul)
          if (jul  >  sc%nbjsemis) jul = jul - sc%nbjsemis
          call julien(jul,ancours,mois,jour,nummois)
          car = 'cut    '
          write(fb,*) car,jour,mois,ancours,                  &
                        p%udevlaires(p%numcoupe),               &
                        stcum,itk%P_lairesiduel(p%numcoupe-1),    &
                        itk%P_msresiduel(p%numcoupe-1)

        ! entrée en dormance
        case('ddo')
          if (p%P_codebfroid == 3) then
            i = p%idebdorms
            if (p%idebdorms > sc%nbjsemis) i = p%idebdorms - sc%nbjsemis
        ! DR 050808 si on est en enchainement, on a la levée de dormance
        ! qui s'est passée l'année d'avant
            if (sc%P_iwater > p%P_idebdorm)then
! DR 01/12/2014 je corrige les stades y'a des pbs de recalcule de dates , fallait prendre l'annee du stade
              call julien(i,sc%annee(p%idebdorms),mois,jour,nummois)
              write(fb,mes2094)jour,mois,sc%annee(1)
            else
              call julien(i,sc%annee(p%ilevs),mois,jour,nummois)
              write(fb,mes2094)jour,mois,sc%annee(p%idebdorms)
            endif
          endif

        ! fin de dormance
        case('fdo')
          if (p%P_codebfroid == 3) then
            i = p%ifindorms
            if (p%ifindorms > sc%nbjsemis) i = p%ifindorms - sc%nbjsemis
! DR 01/12/2014 je corrige les stades y'a des pbs de recalcule de dates , fallait prendre l'annee du stade
            call julien(i,sc%annee(p%ifindorms),mois,jour,nummois)
            if(p%P_codedormance <= 2) then
              write(fb,mes2093) jour,mois,sc%annee(p%ifindorms),p%cu(p%nfindorm)
            else
              write(fb,mes2095) jour,mois,sc%annee(p%ifindorms),p%cu(p%nfindorm)
            endif
          endif


      end select
    end do

return
end subroutine bilanStadesDeveloppement



! ============================================

!> harvest components
subroutine bilanRecolte(sc,p,itk,fb)

USE Stics
USE Plante
USE Itineraire_Technique
USE Divers
USE Messages

    implicit none

    type(Stics_Communs_),       intent(IN)    :: sc  
    type(Plante_),              intent(INOUT) :: p  
    type(ITK_),                 intent(IN)    :: itk  
    integer,                    intent(IN)    :: fb  

    integer           :: irecolte  !>  
    integer           ::  julrec  !>  
    integer           ::  julan  
    integer           :: jour  !>  
    integer           :: numMois  
    character(len=3)  :: mois  
    real              :: tenMF  



    if (itk%P_codcueille == 1) then
      write(fb,mes412)
    else
      if(itk%P_nbcueille == 1) then
        write(fb,mes414)
      else
        write(fb,mes415) itk%P_cadencerec
      endif
    endif

! DR 11/04/06 je mets un message au cas où mat posterieur à rec
    if (p%nrec < p%nmat) then
       write(fb,mes48)
    endif

! calcul de la date de récolte
! ---------------------------------------------------------------
!  plusieurs critères pour la récolte  :
!     1- la maturité physiologique (p%P_codrecolte=1)
!     2- la teneur en eau          (p%P_codrecolte=2)
!     3- la teneur en sucre        (p%P_codrecolte=3)
!     4- la teneur en protéine     (p%P_codrecolte=4)
!     5- la teneur en huile        (p%P_codrecolte=5)
! ---------------------------------------------------------------
    if (itk%P_nbcueille == 1) then
      if (itk%P_codrecolte == 1) write(fb,mes417)
      if (itk%P_codrecolte == 2) write(fb,mes418)
      if (itk%P_codrecolte == 3) write(fb,mes419)
      if (itk%P_codrecolte == 4) write(fb,mes420)
      if (itk%P_codrecolte == 5) write(fb,mes421)
    endif

  ! étalement de la récolte
    if (itk%P_nbcueille == 2) then
      write(fb,mes422)
      do irecolte = 1,p%nbrecolte-1
        julrec = p%nrecint(irecolte) + sc%P_iwater - 1
        julan = julrec
        ! PB - teauint & nbfrint ne sont pas calculés pour les 2 composantes AO/AS
        !      dans la version modulaire on calcule directement le cumul dans 'recolte'
        !--p%teauint(sc%AOAS,irecolte) = p%teauint(sc%AS,irecolte) * p%surf(sc%AS) &
        !--                            + p%teauint(sc%AO,irecolte) * p%surf(sc%AO)
        !--p%nbfrint(sc%AOAS,irecolte) = p%nbfrint(sc%AS,irecolte) * p%surf(sc%AS) &
        !--                            + p%nbfrint(sc%AO,irecolte) * p%surf(sc%AO)

        if (julrec > sc%nbjsemis) julrec = julrec - sc%nbjsemis
 ! DR 05/12/2014 pb d'annee pour les bissextiles
 !       call julien(julrec,sc%annee(julan),mois,jour,nummois)
        call julien(julrec,sc%annee(julrec),mois,jour,nummois)

        tenMF = (1. - p%teauint(sc%AOAS,irecolte)) * 100.
 ! DR 05/12/2014 pb d'annee pour les bissextiles
!        write(fb,mes3101) irecolte,jour,mois,sc%annee(julan),   &
        write(fb,mes3101) irecolte,jour,mois,sc%annee(julrec),   &
                          p%rdtint(sc%AOAS,irecolte)/100.,      &
                          p%nbfrint(sc%AOAS,irecolte),tenMF

      ! on réintègre dans pdsfruit et nfruit ce qu'on avait enlevé lors des cueillettes
        p%pdsfruit(sc%AOAS,p%P_nboite) = p%pdsfruit(sc%AOAS,p%P_nboite) &
                                     + p%rdtint(sc%AOAS,irecolte)
        p%nfruit(sc%AOAS,p%P_nboite) = p%nfruit(sc%AOAS,p%P_nboite) &
                                   + p%nbfrint(sc%AOAS,irecolte)

      ! TODO : voir ce que ça change ça
        p%nrec = p%nrecint(irecolte)

      end do
    endif

return
end subroutine bilanRecolte

end module Bilans
 
 
