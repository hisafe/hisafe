! ************************************************
! sous_programme de calcul des initialisations sol :
!   calcul de la profondeur du sol
!   calcul pour lixiv des cellules de melange
!   initialisation pour minéralisation
!   prise en compte des P_cailloux dans le calcul pour
!    les caracteristiques du sol : hcc, hmin, da
!    les Humidités et quantités d'azote initiales : hinit, NO3init, NH4init
! DR 23/11/07 adaptation des MO à l'effet du CC : recalcul des parametres MO et res
!****************************************************************************


subroutine initialisations_sol(sc,pg,t,soil,st)

USE Stics
USE Parametres_Generaux
USE Sol
USE Climat
USE Station

implicit none

! Arguments
    type(Stics_Communs_),       intent(INOUT) :: sc  
    type(Stics_Transit_),       intent(IN)    :: t  
    type(Parametres_Generaux_), intent(IN)    :: pg  
    type(Sol_),                 intent(INOUT) :: soil  
    type(Station_),             intent(INOUT) :: st

! Variable(s) locale(s)
      integer :: i  !>  
      integer :: icou  !>  
      integer :: j  !>  
      integer :: nepc  !>  
      integer :: nhel  !>  
      integer :: npro  
      real    :: dax  !>  
      real    :: hccx  !>  
      real    :: hmincx  


! *---------------------------------------------------* c
! *                    LE SOL                         * c
! *---------------------------------------------------* c

! dr 20/09/2012 à remonter plus haut car on en a besoin avant
      sc%nbCouchesSol = soil%nbCouchesSol_max ! 60 ou 1000 : à rendre dynamique ?

    ! calcul de la profondeur de sol (profsol)
    ! calcul du numero des couches de transition (izc)

      soil%profsol = 0.
      do i = 1,5   ! on suppose qu'il y a max. 5 couches

        if (soil%P_epc(i) == 0.) EXIT ! épaisseur de la couche = zéro, on quitte la boucle
        soil%profsol = soil%profsol + soil%P_epc(i)
        soil%izc(i) = nint(soil%profsol)
        sc%NH = i

      ! attention si profsol est > profimperméable !....
        if (soil%P_codemacropor == 1) then

        ! DR 21/09/09 integration des modifs de Marie fdans version javastics 7.1
        ! modif ML 21/11/08 pb si P_infil à zero en dernier couche
          !--if (soil%P_infil(i) <= 0 .and. soil%P_epc(i+1) > 0.) then
! DR 6/10/2010 ca faisait des depassements de tableau , je teste en 2 fois
          if(sc%NH < 5)then
! 	          if(soil%P_infil(i) <= 0 .and. soil%P_epc(i+1) > 0 .and. sc%NH < 5) then !TODO: A terme, NH pourrait être variable en fonction de ce qui est lu dans le fichier sol ?
              if(soil%P_infil(i) <= 0 .and. soil%P_epc(i+1) > 0) then !TODO: A terme, NH pourrait être variable en fonction de ce qui est lu dans le fichier sol ?
                     call EnvoyerMsgHistorique(197)
                     if (soil%P_codrainage == 1) then
                        call EnvoyerMsgHistorique(5160)
                        !stop
                        call exit(9)
                     endif
              endif
          endif
        endif
      end do


    ! Calculs pour LIXIV
    ! ------------------
    ! ncel  = nombre de cellules de melange
    ! icel  = cote des cellules de melange
    ! izcel = numero des cellules de transition
    ! nhel  = nombre de cellules correspondant a P_epd
      soil%icel(0) = 0
      soil%ncel = 0
      npro = 0
      do i = 1, sc%NH
        nepc = nint(soil%P_epc(i))
        npro = npro + nepc
! **    si P_epd est nul, on prend 1 cm par défaut
        if (soil%P_epd(i) <= 0.) then
          call EnvoyerMsgHistorique(198)
          soil%P_epd(i) = 1
        endif

        nhel = int(nepc / soil%P_epd(i))
        if (nhel > nepc) nhel = nepc
        if (nhel < 1) nhel  = 1
        soil%P_epd(i) = int(nepc / nhel)
        do j = 1,nhel
          soil%ncel = soil%ncel +1
          soil%icel(soil%ncel) = soil%icel(soil%ncel-1) + soil%P_epd(i)
        end do
        if (soil%icel(soil%ncel) /= npro) then
            soil%ncel = soil%ncel + 1
            soil%icel(soil%ncel) = npro
        end if
        soil%izcel(i) = soil%ncel
      end do


    ! si P_codemacropor = 0 on annule les numeros de couche de transition
      if (soil%P_codemacropor == 2) then
        soil%P_codefente = 2
        soil%izc(:)   = 0
        soil%izcel(:) =  0
      endif

    ! drainage
      if (soil%P_codrainage == 2) soil%P_profdrain = 0.
      soil%ldrains = soil%P_ecartdrain / 2.

    ! PB - 15/03/2005 - déplacés. On réinitialise en début de cycle dans tous les cas.
      sc%QLES     = 0.  ! TODO : à déplacer dans initnonsol ?
      sc%DRAT     = 0.  ! TODO : à déplacer dans initnonsol ?
      soil%qlesd    = 0.
      soil%remontee = 0.

      if (soil%P_codrainage == 1) then
        if (soil%P_codemacropor == 2) then
          ! mise en place d'un retour code erreur pour la PIC
          call EnvoyerMsgHistorique(199)
          call exit(9)

          !return 2
        endif


! ** domi - 23/07/03 - test uniquement si codehnappe
        if (pg%P_codhnappe == 2) then
          if (pg%P_distdrain > soil%ldrains) then
            call EnvoyerMsgHistorique(330)
            !stop
            call exit(9)
          endif
        endif
      endif

      soil%qdraincum = 0.

! ** initialisation pour minéralisation
      if (soil%P_profhum > soil%profsol) then
        call EnvoyerMsgHistorique(171)
        soil%P_profhum = soil%profsol
      endif
      soil%nitrifj  = 0.
      sc%Qminh    = 0.
      sc%Qminr    = 0.
      soil%Qminrcult =  0.
      soil%cumvminr = 0.
      soil%cumvminh = 0.
      sc%tnhc     = 0.
      sc%tnrc     = 0.
      sc%Qnitrif  = 0.
      sc%Qem_N2O    = 0.
      sc%Qem_N2Onit = 0.
      sc%Qem_N2Oden = 0.


! DR 06/09/2011 on ajoute une varaible
      sc%cum_immob = 0.

! calcul des parametres sol équivalents lorsque le sol contient des P_cailloux
! --------------------------------------------------------------------------
      do icou = 1,5

        if (soil%P_epc(icou) <= 0.) CYCLE

        dax = pg%P_masvolcx(soil%P_typecailloux(icou))
        hccx = pg%P_hcccx(soil%P_typecailloux(icou))


    ! On suppose que les humidités mini sont proportionnelles entre CaillouX et Terre Fine
        hmincx = soil%P_hminf(icou) * hccx / soil%P_hccf(icou)

    ! DA
        soil%da(icou) = (soil%P_cailloux(icou) * dax + (100. - soil%P_cailloux(icou)) * soil%P_daf(icou)) / 100.

    ! HCC
        soil%hcc(icou) = soil%P_cailloux(icou) * hccx * dax + (100. - soil%P_cailloux(icou)) * soil%P_hccf(icou) * soil%P_daf(icou)
        soil%hcc(icou) = soil%hcc(icou) / soil%da(icou) / 100.

    ! HMIN
        soil%hmin(icou) = soil%P_cailloux(icou) * hmincx * dax +                                            &
                         (100. - soil%P_cailloux(icou)) * soil%P_hminf(icou) * soil%P_daf(icou)
        soil%hmin(icou) = soil%hmin(icou) / soil%da(icou)  / 100.

    ! Humidités et quantités d'azote initiales

    !!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
    !!!    sc%Hinit(icou) = sc%P_Hinitf(icou) * soil%hcc(icou) / soil%P_hccf(icou)
        soil%Hinit(icou) = soil%P_Hinitf(icou) * soil%hcc(icou) / soil%P_hccf(icou)
        soil%NO3init(icou) = soil%P_NO3initf(icou) * soil%hcc(icou) * soil%da(icou) / (soil%P_hccf(icou) * soil%P_daf(icou))

! domi 21/06/05 pour intercrop pour les sols à P_cailloux on calcule un coeff pour reajuster les observees
!    write(56,*)'eau couche ',icou, hcc(icou)/P_hccf(icou)
!    write(56,*)'No3 couche ',icou,
!     s    hcc(icou)*da(icou)/(P_hccf(icou)*P_daf(icou))
!      write(56,*)'da couche ',icou, da(icou)

        if (soil%P_codenitrif == 1) then
        !!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
        !!!  sc%NH4init(icou) = sc%P_NH4initf(icou) * soil%hcc(icou) * soil%da(icou) / (soil%P_hccf(icou) * soil%P_daf(icou))
          soil%NH4init(icou) = soil%P_NH4initf(icou) * soil%hcc(icou) * soil%da(icou) / (soil%P_hccf(icou) * soil%P_daf(icou))
        else
        !!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
        !!!  sc%NH4init(icou) = 0.
          soil%NH4init(icou) = 0.
        endif

      end do



! DR 21/09/09 on ajoute les modifs de BM et EJ
! DR et ML et BM 22/06/09
! **************************
! introduction des modifications de BM
! initialisation de pHvol
      soil%pHvol = soil%P_pH
      soil%dpH = 0.0
      soil%Nvolatorg = 0.0
! 22/06/09 FIN introduction des modifications de BM

! DR et ML et BM et EJ 23/06/09
! ******************************
! introduction des modifications de BM
!       FTEMhb=(P_FTEMha-1.0)*exp(P_FTEMh*TREF)
!       FTEMrb=(P_FTEMra-1.0)*exp(P_FTEMr*TREF)
! DR et ML 19/10/09 on reintroduit le distinguo entre P_trefh et P_trefr pour Jorge
! (P_code_adaptCC_miner=1)
! BM 19/05/2011 du fait de l'introduction de TREFh et TREFr c'est mal placé , à placer plus loin
!      sc%FTEMhb = (pg%P_FTEMha-1.0) * exp(pg%P_FTEMh * sc%var_TREFh(sc%numcult))
!      sc%FTEMrb = (pg%P_FTEMra-1.0) * exp(pg%P_FTEMr * sc%var_TREFr(sc%numcult))
! Fin DR et ML et BM et EJ 23/06/09
! DR 21/09/09 FIN on ajoute les modifs de BM et EJ


! Dr 19/10/09 je reactive les modifs de Jorge et je les mets dans une subroutine

! DR 21/09/09 on mets en commentaire en attendant la validation de BM
! **********************

! DR 23/11/07 adaptation des MO à l'effet du CC
! certains parametres sont augmentés du deltat
! on les a appelé autrement car c'est des param variables

!      if (t%P_code_adaptCC_miner == 1) then
!        sc%var_trefh(sc%numcult) = pg%P_trefh + sc%deltaT_adaptCC(sc%numcult)
!        sc%var_trefr(sc%numcult) = pg%P_trefr + sc%deltaT_adaptCC(sc%numcult)
!      else
!        sc%var_trefh(sc%numcult) = pg%P_trefh
!        sc%var_trefr(sc%numcult) = pg%P_trefr
!      endif
!      if (t%P_code_adaptCC_nit == 1) then
!        sc%var_tnitmin(sc%numcult) = pg%P_tnitmin + sc%deltaT_adaptCC(sc%numcult)
!        sc%var_tnitmax(sc%numcult) = pg%P_tnitmax + sc%deltaT_adaptCC(sc%numcult)
!        sc%var_tnitopt(sc%numcult) = pg%P_tnitopt + sc%deltaT_adaptCC(sc%numcult)
!      else
!        sc%var_tnitmin(sc%numcult) = pg%P_tnitmin
!        sc%var_tnitmax(sc%numcult) = pg%P_tnitmax
!        sc%var_tnitopt(sc%numcult) = pg%P_tnitopt
!      endif
!      if (t%P_code_adaptCC_denit == 1) then
!        sc%var_TREFdenit1(sc%numcult) = t%P_TREFdenit1 + sc%deltaT_adaptCC(sc%numcult)
!        sc%var_TREFdenit2(sc%numcult) = t%P_TREFdenit2 + sc%deltaT_adaptCC(sc%numcult)
!      else
!        sc%var_TREFdenit1(sc%numcult) = t%P_TREFdenit1
!        sc%var_TREFdenit2(sc%numcult) = t%P_TREFdenit2
!      endif
! DR 21/09/09 FIN on mets en commentaire

    if (t%P_code_adaptCC_miner == 1 .or. t%P_code_adaptCC_nit == 1 .or. t%P_code_adaptCC_denit == 1) then

    ! si on veut activer au moins un des effets adaptation MO au CC
      !TODO: call effetadaptMOauCC

    else

    ! si on veut rien activer
      sc%var_trefh(sc%numcult)      = pg%P_trefh
      sc%var_trefr(sc%numcult)      = pg%P_trefr
      sc%var_tnitmin(sc%numcult)    = pg%P_tnitmin
      sc%var_tnitmax(sc%numcult)    = pg%P_tnitmax
      sc%var_tnitopt(sc%numcult)    = pg%P_tnitopt
      sc%var_tnitopt2(sc%numcult)    = pg%P_tnitopt2
      sc%var_TREFdenit1(sc%numcult) = t%P_TREFdenit1
      sc%var_TREFdenit2(sc%numcult) = t%P_TREFdenit2

    endif
! BM 19/05/2011 deplacé ici pour pb d'initialisation plus haut
      sc%FTEMhb = (pg%P_FTEMha-1.0) * exp(pg%P_FTEMh * sc%var_TREFh(sc%numcult))
      sc%FTEMrb = (pg%P_FTEMra-1.0) * exp(pg%P_FTEMr * sc%var_TREFr(sc%numcult))


! initialisations pour CROIRA
    ! test sur P_obstarac
      if (soil%P_obstarac > soil%profsol) then
        call EnvoyerMsgHistorique(80)
        soil%P_obstarac = soil%profsol
      endif


    ! initialisation pour l'évaporation sol (SOLNU)
      sc%bouchon   = 1
      sc%sumes0    = 0.
      sc%sumes1    = 0.
      sc%sumes2    = 0.
      sc%esol      = 0.
      sc%hi = soil%hcc(1) * soil%da(1) / 100.


    if (sc%P_codesuite == 0) then
        sc%sum2     = 0.
        sc%smes02   = 0.
        sc%sesj0    = 0.
        sc%ses2j0   = 0.
        sc%esreste  = 0.
        sc%esreste2 = 0.
    endif

    ! Humidité résiduelle (Brisson et Perrier, 1991)
      sc%ha     = soil%P_argi / 100. / 15. * soil%da(1)
      sc%hurlim = sc%ha * 10.
      sc%hpf    = soil%hmin(1) * soil%da(1) / 100.
      soil%aevap  = st%P_aclim / 2. * (0.63 - sc%ha)**(5./3.) * (sc%hi - sc%ha)
    ! 17/06/04 on interdit P_zesx>profsol
      if (soil%P_zesx > soil%profsol) then
        soil%P_zesx = soil%profsol
        call EnvoyerMsgHistorique(182)
      endif


return
end subroutine initialisations_sol
 
 
