!>
!> daily loop part I
!>
!> The STICS model is organised into modules (fig. 1.1, page 18),
!> each module is composed of sub-modules dealing with specific mechanisms.
!> A first set of 3 modules deals with ecophysiology of above-ground plant parts (phenology, shoot growth, yield formation).
!> A second set of 4 modules deals with soil functions interacting with roots (root growth, water balance, nitrogen balance, soil transfers)
!> The crop management module deals with interactions between the applied techniques and the soil-crop system.
!> The microclimate module simulates the combined effects of climate and water balance on the temperature and air humidity within the canopy.
!! trrext
!> Each module includes options that can be used to extend the scope with which STICS can be applied to various crop systems.  These options relate to ecophysiology and to crop management, for example:
!> -   competition for assimilate between vegetative organs and reserve organs (hereafter referred to as trophic competition);
!> -   considering the geometry of the canopy when simulating radiation interception;
!> -   the description of the root density profile;
!> -   using a resistive approach to estimate the evaporative demand by plants;
!> -   the mowing of forage crops;
!> -   plant or plastic mulching under vegetation
!!
!> some options depend on data availability.  For example, the use of a resistive model is based on availability of additional climatic driving variables: wind and air humidity.
subroutine Stics_Jour1(sc,pg,p,itk,soil,c,sta,t)


USE Stics
USE Plante
USE Itineraire_Technique
USE Sol
USE Climat
USE Station
USE Parametres_Generaux
USE Divers
! ML je desactive pour compiler

  implicit none


  type(Stics_Communs_),       intent(INOUT) :: sc  

  type(Parametres_Generaux_), intent(IN)    :: pg  

  type(Plante_),              intent(INOUT) :: p (sc%P_nbplantes)

  type(ITK_),                 intent(INOUT) :: itk (sc%P_nbplantes)

  type(Sol_),                 intent(INOUT) :: soil  

  type(Climat_),              intent(INOUT) :: c  

  type(Station_),             intent(INOUT) :: sta  

  type(Stics_Transit_),       intent(INOUT) :: t  
! ML je desactive pour compiler
! 17/10/2013 à garder si on doit faire un module à part pour les varaibles issus de stics
!  type(Patho_inputs_Stics_),   intent(IN) :: pis

!! variables locales
  integer :: n  
  integer :: i  !>  
  integer ::  iz  !>  
  integer ::  ipl  !>  
  integer ::  ens  
  real    :: epzTot(sc%nbCouchesSol)      ! est-ce que la taille de epzTot peut varier au cours du temps (tassement/detassement) ?  
  real    :: zracMAXI  
  real    :: tmaxveille  
  real    :: troseh(24)
  real    :: tculth(24)
  real    :: humh(24)

  real    ::  cum_absz,cum_nitz
  integer ::  zz  !>
  n = sc%n


      if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0) print *,'Stics_Jour: climabri'

    !::: climabri
      if (itk(1)%P_codabri == 2) then
          if (n > 1) then
            call ClimatSousAbri(sta%P_aks,sta%P_bks,c%tvent(n),n,sc%nouvre2,sc%nouvre3,itk%P_surfouvre1,&
                                itk%P_surfouvre2,itk%P_surfouvre3,sta%P_cvent,sta%P_phiv0,              &
                                sc%et,c%tetp(n-1),c%tetp(n),sta%P_coefrnet,c%trgext(n),c%tmoy(n),                         &
                                c%tmoyext(n),c%tmin(n),c%tminext(n),c%tmax(n))
          else
            call ClimatSousAbri(sta%P_aks,sta%P_bks,c%tvent(n),n,sc%nouvre2,sc%nouvre3,itk%P_surfouvre1,&
                                itk%P_surfouvre2,itk%P_surfouvre3,sta%P_cvent,sta%P_phiv0,              &
                                sc%et,0,c%tetp(n),sta%P_coefrnet,c%trgext(n),c%tmoy(n),             &
                                c%tmoyext(n),c%tmin(n),c%tminext(n),c%tmax(n))
          endif
      endif


!::: Fonctions Physiologiques (call physio => dominver, laidev, croissance)

    !: Si la hauteur de la plante dominée dépasse celle de la plante dominante
    !- alors il doit y avoir INVERsion de DOMINance
      if (sc%P_nbplantes > 1) then
        if (p(2)%hauteur(0) > p(1)%hauteur(0)) then
           if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)  print *,'inversion de dominance'

          p(1:2) = p(2:1:-1) ! on inverse les 2 plantes dans le tableau plante avec une manipulation d'indices du tableau.
          ! DR 12/02/2015 on teste l'inversion des parametres techniques
          itk(1:2) = itk(2:1:-1) ! on inverse les 2 plantes dans les tableau management avec une manipulation d'indices du tableau.
          ! DR et ML 29/08/2014
          !  lorsqu'il y a inversion de dominance, afin d'eviter que le lai de la plante dominée qui passe à l'ombre ne soit nul ,
          !  on lui affecte la valeur du lai au soleil de la veille , sachant qu'il sera ensuite pondéré par la surface à l'ombre
          ! (dans croissance.f90 apres l'appel à la fonction repartir)
          !write(*,*)n,p(2)%lai(sc%ao,n),p(2)%lai(sc%as,n),p(2)%lai(sc%ao,n-1),p(2)%lai(sc%as,n-1)
          p(2)%lai(sc%ao,n)=p(2)%lai(sc%as,n-1)
          !write(*,*)n,p(2)%lai(sc%ao,n),p(2)%lai(sc%as,n),p(2)%lai(sc%ao,n-1),p(2)%lai(sc%as,n-1)
          !DR et ML 20/04/2016 on recalcule les surfaces de la plante qui devient dominante (pour la plante dominée c'est fait dans transrad)
          p(1)%surf(sc%as)=1.
          p(1)%surf(sc%ao)=0.
          !DR et ML 21/04/2016 on reaffecte les bonnes valeurs au logique est dominante
          p(1)%estDominante= .TRUE.
          p(2)%estDominante= .FALSE.

        endif
      endif



    ! Calcul de la photopériode
      sc%numdate = sc%jul
      call photpd(sta%P_latitude,sc%numdate,c%daylen,c%phoi)



      if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0) print *,'Stics_Jour: laidev'


    !: Lai - Developpement
    ! DR 23/07/2012 sta est inutilisé
    !  call laidev(sc,p,pg,itk,t,soil,c,sta)
      call laidev(sc,p,pg,itk,t,soil,c)

      if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0) print *,'Stics_Jour: croissance'

      call croissance(sc,pg,p,itk,soil,c,sta,t)


      sc%laiTot = 0.
      do i = 1, sc%P_nbPlantes
        sc%laiTot = sc%laiTot + p(i)%lai(0,n)
      end do




      if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0) print *,'Stics_Jour: detassement'

    !::: Détassement

    ! DR 10/11/06 je deplace tassement apres physio
    ! car dans le cas ou on n'active pas la decision de recolte , on a pas acces à
    ! la date de recolte et donc on ne tasse jamais à la recolte
    !        if (P_codeDST == 1.or.P_codeDSTtass == 1) call tassement

    ! DR le 06/02/07 decoupage de tassement en 3 sp
    !  1) on fait le agir detassement du au travail du sol
    !  2) on prend decide de la date semis ou recolte
    !  3) on fait agir le tassement du au passage des engins

      if (itk(1)%P_codeDST == 1) then


        call detassement(n,sc%NH,sc%nbCouchesSol,sc%sat(1:sc%nbCouchesSol),itk(1)%P_nbjtrav,          &
                         p(1)%numjtrav(1:itk(1)%P_nbjtrav),itk(1)%P_proftrav(1:itk(1)%P_nbjtrav),     &
                         pg%P_proflabour,                                                             &
                         pg%P_proftravmin,itk(1)%P_rugochisel,itk(1)%P_dachisel,soil%P_codefente,     &
                         soil%P_hccf(1:sc%NH),soil%P_hminf(1:sc%NH),sc%beta_sol(1:2),itk(1)%P_rugolabour, &
                         itk(1)%P_dalabour,soil%AZnit(1:sc%NH),soil%AZamm(1:sc%NH),p(1)%P_coderacine, &
                         p(1)%lrach(1:sc%NH),pg%P_lvopt,soil%da_ini(1:2),soil%zesx_ini,soil%q0_ini, &
                         pg%nbResidus,                                                              &
                         sc%nhe,soil%HR(1:sc%NH),soil%TS(1:sc%NH),soil%P_epc(1:sc%NH),soil%profsol,     &
                         soil%P_profhum,soil%P_z0solnu,soil%profhum_tass(1:2),soil%P_infil(1:2),soil%ncel,&
                         soil%da(1:sc%NH),soil%P_epd(1:sc%NH),soil%icel(0:sc%nbCouchesSol),         &
                         sc%hur(1:sc%nbCouchesSol),sc%hucc(1:sc%nbCouchesSol),                      &
                         sc%humin(1:sc%nbCouchesSol),sc%tsol(1:sc%nbCouchesSol),soil%izcel(1:sc%NH),&
                         soil%izc(1:sc%NH),soil%nit(1:sc%nbCouchesSol),soil%amm(1:sc%nbCouchesSol), &
                         p(1)%rljour(1:sc%nbCouchesSol),p(1)%flrac(1:sc%nbCouchesSol),                &
                         sc%Cbio(1:sc%nbCouchesSol,1:pg%nbResidus),sc%Nbio(1:sc%nbCouchesSol,1:pg%nbResidus),&
                         sc%Cres(1:sc%nbCouchesSol,1:pg%nbResidus),sc%Nres(1:sc%nbCouchesSol,1:pg%nbResidus),&
                         sc%Chum(1:sc%nbCouchesSol),sc%Nhum(1:sc%nbCouchesSol),soil%P_zesx,soil%P_q0)


      endif
      if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0) print *,'Stics_Jour: decisionsemis'

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!   if (itk(1)%P_codedecisemis == 1 .and. p(1)%P_codeplante /= 'snu') then
      if (itk(1)%P_codedecisemis == 1 .and. p(1)%P_codeplante /= 1) then
        call decisionsemis(n,sc%P_iwater,sc%NH,sc%nbCouchesSol,pg%P_prophumtasssem,soil%P_hccf(1:sc%NH),            &
                           soil%P_epc(1:sc%NH),sc%hur(1:sc%nbCouchesSol),soil%da(1:sc%NH),                          &
                           sc%sat(1:sc%nbCouchesSol),itk(1)%P_profhumsemoir,itk(1)%P_profsem,                       &
                           sc%hucc(1:sc%nbCouchesSol),sc%humin(1:sc%nbCouchesSol),p(1)%P_sensrsec,                  &
                           t%P_humirac_decisemis,itk(1)%P_nbjseuiltempref,c%tmin(n:n+itk(1)%P_nbjseuiltempref),     &
                           p(1)%P_tdebgel,c%tmoy(n:n+itk(1)%P_nbjseuiltempref),p(1)%P_tdmin,t%P_eau_mini_decisemis, &
                           t%P_nbj_pr_apres_semis,sc%esol,itk(1)%P_codecalirrig,pg%P_irrlev,                        &
                           sc%airg(n:n+t%P_nbj_pr_apres_semis),c%trr(n:n+t%P_nbj_pr_apres_semis),                   &
                           itk(1)%P_nbjmaxapressemis, &
                           p(1)%nbjpourdecisemis,sc%repoussesemis(1),p(1)%nplt,sc%iplt(1))
      endif
      if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0) print *,'Stics_Jour: decisionrecolte'

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!    if (itk(1)%P_codedecirecolte == 1 .and. p(1)%P_codeplante /= 'snu') then
       if (itk(1)%P_codedecirecolte == 1 .and. p(1)%P_codeplante /= 1) then
        call decisionrecolte(n,pg%P_prophumtassrec,soil%P_hccf(1:sc%NH),sc%NH,sc%nbCouchesSol,soil%P_epc(1:sc%NH),&
                             sc%hur(1:sc%nbCouchesSol),soil%da(1:sc%NH),sc%sat(1:sc%nbCouchesSol),                &   ! IN
                             itk(1)%P_profhumrecolteuse,p(1)%nrecbutoir,itk(1)%P_nbjmaxapresrecolte,              &
                             p(1)%nbjpourdecirecolte,sc%repousserecolte(1),p(1)%nrec)                          ! INOUT
      endif


    !::: Tassement semis & récolte
      if (itk(1)%P_codeDSTtass == 1) then


      if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0) print *,'Stics_Jour: tassesemisrecolte'

        call tassesemisrecolte(n,sc%NH,sc%nbCouchesSol,p(1)%P_codeplante,p(1)%nplt,p(1)%nrec,itk(1)%P_profhumsemoir,  &
                               pg%P_prophumtasssem,soil%P_hccf(1:sc%NH),itk(1)%P_dasemis,itk(1)%P_profhumrecolteuse,  &
                               pg%P_prophumtassrec,itk(1)%P_darecolte,sc%sat(1:sc%nbCouchesSol),sc%beta_sol,          &
                               itk(1)%P_codeDSTnbcouche,p(1)%P_coderacine,soil%da_ini(1:2),soil%zesx_ini,soil%q0_ini, &
                               pg%nbResidus,soil%Hinit(1:2),                                                          &
                               soil%profsol,soil%P_infil(1:sc%NH),soil%da(1:sc%NH),                                 &
                               soil%profhum_tass(1:2),soil%P_obstarac,soil%icel(0:sc%nbCouchesSol),soil%ncel,       &
                               soil%izcel(1:sc%NH),sc%nhe,soil%P_profhum,soil%izc(1:sc%NH),soil%P_epc(1:sc%NH),     &
                               soil%P_epd(1:sc%NH),sc%hur(1:sc%nbCouchesSol),soil%nit(1:sc%nbCouchesSol),           &
                               soil%amm(1:sc%nbCouchesSol),p(1)%flrac(1:sc%nbCouchesSol),p(1)%rljour(1:sc%nbCouchesSol),&
                               sc%Cbio(1:sc%nbCouchesSol,1:pg%nbResidus),sc%Cres(1:sc%nbCouchesSol,1:pg%nbResidus), &
                               sc%Nres(1:sc%nbCouchesSol,1:pg%nbResidus),sc%Nbio(1:sc%nbCouchesSol,1:pg%nbResidus), &
                               sc%Chum(1:sc%nbCouchesSol),sc%Nhum(1:sc%nbCouchesSol),sc%hucc(1:sc%nbCouchesSol),    &
                               sc%humin(1:sc%nbCouchesSol),sc%tsol(1:sc%nbCouchesSol),soil%P_zesx,soil%P_q0,soil%HR(1:sc%NH))



      endif


if (t%P_codeSWDRH.eq.1) then
!: ML - 29/10/12 - calcul durée d'humectation
!- appel prealable de shutwall et humheure
!- pour calculer tculthoraire et determiner s'il y a humectation ou non
      call shutwall(sc,pg,c,sta,soil,p,itk,t)
        if (n > 1) tmaxveille = sc%tcultveille * 2 - c%tmin(n-1)
        if (n == 1) tmaxveille = sc%tcultmax


      call humheure(n,sc%tcultmin,sc%tcultmax,tmaxveille,sc%humidite,c%daylen,c%tmin(n+1),sc%tcult,    &
           sc%trosemax(0:n),c%humimoy,c%tmin(n),t%P_codetrosee,tculth,troseh,humh)

      call surface_wetness_duration_relative_humidity(tculth,troseh,humh, &
           c%dureehumec,c%dureeRH,c%dureeRH1,c%dureeRH2,c%trr(n))

endif

!::: Apports

      if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: before apports '

      call apports(sc,pg,t,c,soil,p,itk)
      if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: after apports '
!::: Etatsurf

      sc%stemflowTot = 0.
      do i = 1, sc%P_nbPlantes
        sc%stemflowTot = sc%stemflowTot + p(i)%stemflow
      end do

      ipl = 1
      ens = sc%AS
! Ajout Bruno juin 2012 Cmulchnd en kg C/ha et qmulch en t MS/ha
!      sc%qmulch = sc%Cmulchnd / 420. correction bug Bruno 25/07/2014
! Modif Bruno avril 2013 Cmulchnd et Cmulchdec en kg C/ha et qmulch en t MS/ha
      sc%qmulch = (sc%Cmulchdec + sc%Cmulchnd)/ 420.

! DR 01/02/2011 on remplace naps pour P_nbjtrav
      if(itk(1)%P_codepaillage == 2)then   !! paillage mulch plastique

        call etatsurf(n,sc%jjul, &   ! IN
                    sc%stemflowTot,pg%P_pminruis,soil%P_ruisolnu,pg%P_qmulchruis0(sc%irmulch),   &
                    pg%P_mouillabilmulch(sc%irmulch),pg%P_kcouvmlch(sc%irmulch),p(1)%P_codebeso, &
                    p(1)%P_codelaitr,c%tetp(n),sc%delta,sc%laiTot,sc%tauxcouv(n),                &
                    sc%qmulch,sc%ruisselsurf,sc%precip,sc%mouillmulch,sc%intermulch,             &   ! INOUT
                    itk%P_couvermulchplastique,sc%Emulch,soil%P_penterui)
      else

        call etatsurf(n,sc%jjul,  &   ! IN
                    sc%stemflowTot,pg%P_pminruis,soil%P_ruisolnu,pg%P_qmulchruis0(sc%irmulch),   &
                    pg%P_mouillabilmulch(sc%irmulch),pg%P_kcouvmlch(sc%irmulch),p(1)%P_codebeso, &
                    p(1)%P_codelaitr,c%tetp(n),sc%delta,sc%laiTot,sc%tauxcouv(n),                &
                    sc%qmulch,sc%ruisselsurf,sc%precip,sc%mouillmulch,sc%intermulch,             &   ! INOUT
                    sc%couvermulch,sc%Emulch,soil%P_penterui)


      endif
      if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0) print *,'Stics_Jour: after etatsurf'


!  Calcul de la volatilisation liée aux apports de résidus organiques
      if (soil%Nvolatorg <= 0.1) then
          sc%FsNH3 = 0.
      else
          if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0) print *,'volatilisation'
          call volatorg(soil%da(1),sc%hur(1:2),soil%P_pH,sc%tcult,sta%P_NH3ref,sc%ras,sta%P_ra,soil%dpH,& ! IN
                        sc%FsNH3,soil%Nvolorg,soil%amm(1),soil%Nvolatorg,soil%QNvolorg,soil%pHvol)
      endif


      if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0) print *,'Stics_Jour: Mineralisation'
      if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0) print *,'Stics_Jour: Mineralisation'


        call mineral(sc%nbCouchesSol, pg%nbResidus,                                                        &
                   soil%P_profhum, soil%P_argi, soil%P_calc, soil%P_codefente, t%P_codeNmindec,            &  ! codes "activation"
                   pg%P_fhminsat, pg%P_fmin1, pg%P_fmin2, pg%P_fmin3, pg%P_FTEMh, pg%P_FTEMha,             &
                   pg%P_FTEMr, pg%P_FTEMra, sc%FTEMhb, sc%FTEMrb, pg%P_hminm, pg%P_hoptm, sc%var_TREFh,    &
                   pg%P_Wh,                                                                                &   !decomposition
                   sc%kres(1:pg%nbResidus), pg%P_kbio(1:pg%nbResidus), sc%Wb(1:pg%nbResidus),              &
                   pg%P_yres(1:pg%nbResidus), sc%hres(1:pg%nbResidus),                                     &
                   pg%P_fNCbiomin, pg%P_fredlN, pg%P_fredkN, t%P_rapNmindec, t%P_fNmindecmin,              &
                   pg%P_fredNsup, pg%P_Primingmax,                                                         &   !mineralisation
                   sc%tsol(1:sc%nbCouchesSol), sc%hucc(1:sc%nbCouchesSol), sc%hur(1:sc%nbCouchesSol),               &
                   sc%dacouche(1:sc%nbCouchesSol), sc%humin(1:sc%nbCouchesSol), sc%sat(1:sc%nbCouchesSol),          &
                   soil%itrav1, soil%itrav2, &                                     !variables d'entree IN
                   sc%Cres(1:sc%nbCouchesSol,1:pg%nbResidus),sc%Nres(1:sc%nbCouchesSol,1:pg%nbResidus),             &
                   sc%Cbio(1:sc%nbCouchesSol,1:pg%nbResidus),sc%Nbio(1:sc%nbCouchesSol,1:pg%nbResidus),             &
                   sc%Chum(1:sc%nbCouchesSol),sc%Nhum(1:sc%nbCouchesSol),sc%Chuma,sc%Nhuma,sc%Chumi,sc%Nhumi,       &
                   sc%Chumt, sc%Nhumt, sc%Cr, sc%Nr, sc%Cb, sc%Nb, sc%NCbio,                                        & !INOUT
                   soil%amm(1:sc%nbCouchesSol),soil%nit(1:sc%nbCouchesSol),sc%CO2hum,sc%CO2res,sc%CO2sol,soil%vminr,&
                   soil%cumvminh, soil%cumvminr, sc%QCO2sol, sc%QCO2res, sc%QCO2hum, sc%Qminh, sc%Qminr,            &
                   sc%QCprimed,sc%QNprimed,sc%tnhc,sc%tnrc,sc%Cnondec(1:10), sc%Nnondec(1:10),                      &
                   sc%Cmulchnd,sc%Nmulchnd,sc%Cmulchdec,sc%Nmulchdec,sc%Cmulch,sc%Nmulch,sc%Cbmulch,sc%Nbmulch,sc%cum_immob,&
                   sc%QCO2mul)
         if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0) print *,'Stics_Jour: nitrification'


         call nitrif(sc%nbCouchesSol,sc%numcult,soil%P_profhum,soil%P_codefente,soil%P_codenitrif,pg%P_rationit,     &
                   pg%P_hoptn,pg%P_hminn,pg%P_fnx,soil%P_pH,pg%P_pHmaxnit,pg%P_pHminnit,                             &
                   sc%var_tnitmin(sc%numcult),sc%var_tnitopt(sc%numcult),sc%var_tnitmax(sc%numcult),sc%var_tnitopt2, &
                   sc%tsol(1:sc%nbCouchesSol),sc%hucc(1:sc%nbCouchesSol),sc%hur(1:sc%nbCouchesSol),                  &
                   sc%dacouche(1:sc%nbCouchesSol),sc%humin(1:sc%nbCouchesSol),sc%sat(1:sc%nbCouchesSol),  & !variables d'entree
                   soil%nitrifj,sc%Qnitrif,soil%amm(1:sc%nbCouchesSol),soil%nit(1:sc%nbCouchesSol),       &  !INOUT
                   sc%em_N2Onit,sc%Qem_N2Onit)


!************
! Calcul de la dénitrification par modele NOE (conditions de sol)
        if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0) print *,'Stics_Jour: denitrification'

        if (soil%P_codedenit == 1)then
                   call denit(soil%P_profdenit,soil%P_codefente,sc%nbCouchesSol,sc%dacouche(1:sc%nbCouchesSol), &
                   sc%hucc(1:sc%nbCouchesSol),sc%humin(1:sc%nbCouchesSol),sc%hur(1:sc%nbCouchesSol),            &
                   sc%sat(1:sc%nbCouchesSol),sc%tsol(1:sc%nbCouchesSol),sc%var_TREFh,                           &   ! IN
                   sc%var_TREFdenit1,sc%var_TREFdenit2, soil%P_vpotdenit,pg%P_ratiodenit,                       &
                   soil%Ndenit,soil%condenit,soil%nit(1:sc%nbCouchesSol),soil%QNdenit,sc%em_N2Oden,sc%Qem_N2Oden)   ! INOUT
        endif
! Emissions totales de N2O
        sc%em_N2O  = sc%em_N2Onit  + sc%em_N2Oden
        sc%Qem_N2O = sc%Qem_N2Onit + sc%Qem_N2Oden



!*************

!::: Calcul des Besoins - Besoin en eau, nodules, besoin en azote


      if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)print *,'Stics_Jour: besoinsEnEauDeLaPlante'
      if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: besoinsEnEauDeLaPlante'



      call besoinsEnEauDeLaPlante(sc,pg,c,sta,soil,p,itk,t)




        if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0) print *,'Stics_Jour: besoins azote'


    ! Besoins en azote
      do i = 1,sc%P_nbplantes
        do ens = sc%AS, sc%AO
          if (p(i)%surf(ens) > 0.) then

          ! Nodosités
            if (p(i)%P_codelegume == 2 .and. pg%P_codesymbiose == 2) then


              if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)print *, 'offrnodu'



              call offrnodu(n,p(i)%nlev,p(i)%zrac,p(i)%dtj(n),itk(i)%P_profsem,p(i)%P_profnod,sc%nbCouchesSol,       & ! IN
                            soil%nit(1:sc%nbCouchesSol),sc%hur(1:sc%nbCouchesSol),p(i)%P_concNnodseuil,p(i)%P_vitno, &
                            p(i)%P_codefixpot,p(i)%P_fixmax,p(i)%P_fixmaxveg,p(i)%P_fixmaxgr,p(i)%dltams(ens,n),     &
                            p(i)%dltags(ens),p(i)%P_concNrac100,p(i)%P_concNrac0,pg%P_codefxn,                       &
                            sc%humin(1:sc%nbCouchesSol),sc%tcultmin,sc%tcultmax,sc%tsol(1:sc%nbCouchesSol),          &
                            p(i)%P_tempnod1,p(i)%P_tempnod2,p(i)%P_tempnod3,p(i)%P_tempnod4,sc%anox(1:sc%nbCouchesSol), &
                            p(i)%fixreel(ens),p(i)%somcourno,p(i)%P_stlevdno,p(i)%ndno,p(i)%P_stdnofno,p(i)%nfno,    & ! INOUT
                            p(i)%P_stfnofvino,p(i)%nfvino,sc%nodn,p(i)%propfixpot,p(i)%fixpotfno,                    &
                            p(i)%fixmaxvar(ens),p(i)%fixpot(ens),sc%fxn,sc%fxw,sc%fxt,sc%fxa)



            endif

            if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)print *, 'bNpl'
            call bNpl(p(i)%masec(ens,n),p(i)%nbrecolte,p(i)%rdtint(ens,1:max(1,p(i)%nbrecolte-1)),p(i)%surf(ens), & ! IN
                      p(i)%dltams(ens,n),p(i)%P_codeplisoleN,p(i)%P_masecNmax,p(i)%P_masecmeta,p(i)%adilmaxI,     &
                      p(i)%bdilmaxI,p(i)%adilI,p(i)%bdilI,p(i)%P_adilmax,p(i)%P_bdilmax,p(i)%P_bdil,p(i)%P_adil,  &
                      p(i)%dltags(ens),p(i)%dltamsN(ens),p(i)%P_inngrain1,p(i)%dltaremobilN(ens),p(i)%P_codelegume, &
                      pg%P_codesymbiose,p(i)%fixreel(ens),                                                        &
                      p(i)%masecdil(ens),p(i)%masecpartiel(ens),p(i)%inns(ens),p(i)%innlai(ens),p(i)%demande(ens),& ! INOUT
                      p(i)%abso(ens,n),p(i)%dNdWcrit,p(i)%deltabso(ens),sc%absodrp,p(i)%P_inngrain2,               &
                      p(i)%offrenod(ens),sc%demandebrute)


          endif
        end do
      end do






end subroutine Stics_Jour1

!>
!> daily loop part II
!!! MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
!!! Ajout cellTrg et cellVisibleSky
!>
subroutine Stics_Jour2(sc,pg,p,itk,soil,c,sta,t, hisafeInfluence, cellTrg, cellVisibleSky)


USE Stics
USE Plante
USE Itineraire_Technique
USE Sol
USE Climat
USE Station
USE Parametres_Generaux
USE Divers
! ML je desactive pour compiler

  implicit none


  type(Stics_Communs_),       intent(INOUT) :: sc

  type(Parametres_Generaux_), intent(IN)    :: pg

  type(Plante_),              intent(INOUT) :: p (sc%P_nbplantes)

  type(ITK_),                 intent(INOUT) :: itk (sc%P_nbplantes)

  type(Sol_),                 intent(INOUT) :: soil

  type(Climat_),              intent(INOUT) :: c

  type(Station_),             intent(INOUT) :: sta

  type(Stics_Transit_),       intent(INOUT) :: t

!!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
  real,     intent(IN)   :: cellTrg              !> // INPUT // Global radiation // %
  real,     intent(IN)   :: cellVisibleSky       !> // INPUT // Visible sky // %
!!!MODIF HISAFE 10 : Supression calcul déjà fait dans HISAFE (extaction eau, azote et stress)
  integer,  intent(IN)   :: hisafeInfluence      !>  //INPUT // Hisafe influence on water and nitrogen (1=yes 0=no)


! ML je desactive pour compiler
! 17/10/2013 à garder si on doit faire un module à part pour les varaibles issus de stics
!  type(Patho_inputs_Stics_),   intent(IN) :: pis

!! variables locales
  integer :: n
  integer :: i  !>
  integer ::  iz  !>
  integer ::  ipl  !>
  integer ::  ens
  real    :: epzTot(sc%nbCouchesSol)      ! est-ce que la taille de epzTot peut varier au cours du temps (tassement/detassement) ?
  real    :: zracMAXI
  real    :: tmaxveille
  real    :: troseh(24)
  real    :: tculth(24)
  real    :: humh(24)

  real    ::  cum_absz,cum_nitz

  integer ::  zz  !>

  n = sc%n


if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)print *, 'Stics_Jour: lixivation'

!::: Lixivation

    ! on calcul la somme des epz de toutes les cultures...
      epzTot(:) = 0.0
      do iz = 1,sc%nhe
        do i = 1,sc%P_nbplantes
            do ens = sc%AS, sc%AO
              epzTot(iz) = epzTot(iz) + p(i)%epz(ens,iz)
            end do
        end do
      end do

      zracMAXI = 0.0
      do i = 1,sc%P_nbplantes
          zracMAXI = max(zracMAXI,p(i)%zrac)
      end do

      call lixiv(sc%n,sc%precip,sc%hurlim,sc%ruisselsurf,sc%humin(1:int(soil%profsol)),sc%nhe,sc%nh,              & ! les entrées
                 sc%hucc(1:int(soil%profsol)),soil%hr(1:sc%NH),soil%P_codefente,soil%ncel,soil%icel(0:soil%ncel),   &
                 soil%da(1:sc%NH),soil%izcel(1:5),soil%P_infil(0:5),soil%P_concseuil,soil%P_humcapil,soil%izc(1:5), &
                 soil%hcc(1:sc%NH),soil%P_capiljour,soil%P_epc(1:sc%NH),soil%hmin(1:sc%NH),soil%P_codemacropor,     &
                 soil%P_profdrain,soil%profsol,pg%P_bformnappe,pg%P_distdrain,pg%P_codhnappe,soil%ldrains,soil%P_codrainage,&
                 pg%P_rdrain,soil%P_profimper,soil%P_ksol,soil%profcalc,epzTot(1:sc%nhe),sc%esz(1:sc%nhe),zracMAXI, &
                 p(1)%irrigprof(1:sc%nhe),                                                                        &
                 sc%bouchon,sc%hur(1:int(soil%profsol)),sc%sat(1:int(soil%profsol)),sc%azomes,sc%anox(1:sc%nhe),  & ! les sorties
                 soil%nit(1:int(soil%profsol)),sc%pluieruissel,sc%saturation,sc%resmes,sc%RU,sc%RsurRU,           &
                 soil%concno3les,soil%hnappe,soil%hmax,soil%hph,soil%hpb,soil%profnappe,soil%qdrain,soil%azlesd,  &
                 sc%azsup,sc%QLES,sc%DRAT,sc%drain,sc%ruissel,soil%remontee,soil%qlesd,soil%qdraincum,            &
                 sc%exces(0:5),soil%amm(1:int(soil%profsol)),sc%ammomes,                                          &
                 itk(1)%P_profsem,sc%P_profmesW, sc%P_profmesN, sc%SoilAvW, sc%SoilWatM, sc%SoilNM, sc%lessiv,    & ! ajout entrees et des sorties Macsur
                 sc%treeWaterUptake(1:1000)) ! !!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE


    ! PB - 03/08/2010
    ! J'extrait cette modif de lixiv et je mets dans une boucle P_nbplantes

    ! DR et ML et BM et EJ 22/06/09
    ! *****************************
    ! introduction des modifications de BM et Eric Justes
    ! ** calcul de RsurRU
      do i = 1,sc%P_nbplantes
        p(i)%RsurRUrac = 0.
        p(i)%RUrac     = 0.
        do iz = int(itk(i)%P_profsem),int(p(i)%zrac)
          p(i)%RsurRUrac = p(i)%RsurRUrac + max(sc%hur(iz) + sc%sat(iz) - sc%humin(iz),0.)
          p(i)%RUrac   = p(i)%RUrac    + sc%hucc(iz) - sc%humin(iz)
        end do

        if (p(i)%RUrac == 0) then
          p(i)%RsurRUrac = 1.0
        else
          p(i)%RsurRUrac = p(i)%RsurRUrac / p(i)%RUrac
        endif
      end do
    ! fin DR et ML et BM 22/06/09


    if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)print *,'Stics_Jour:  Transpiration'

      do i = 1,sc%P_nbplantes
        do ens = sc%AS, sc%AO

          if (p(i)%surf(ens) > 0.) then



            !!!MODIF HISAFE 10 : Supression calcul déjà fait dans HISAFE
            !!! On calcule epz seulement si HISAFE n'a pas calculé l'extraction d'eau et d'azote lui même
            !!! SINON epz a déjà été rempli dans HISAFE
            call transpi(sc%n,sc%nbCouchesSol,soil%profsol,sc%hur(1:sc%nbCouchesSol),sc%humin(1:sc%nbCouchesSol), &
                         p(i)%lracz(1:sc%nbCouchesSol),p(i)%surf(ens),p(i)%P_codelaitr,p(i)%lai(ens,n),                &
                         sc%tauxcouv(n),p(i)%nrec,itk(i)%P_codcueille,p(i)%eop(ens),p(i)%exobiom,                 &
                         p(i)%epz(ens,1:sc%nbCouchesSol),p(i)%ep(ens),p(i)%turfac(ens),p(i)%senfac(ens),          &
                         p(i)%swfac(ens),p(i)%profexteau, hisafeInfluence)

          endif
        end do

      ! Ombre/soleil => Tout
        p(i)%epz(sc%AOAS,:) = p(i)%epz(sc%AO,:) * p(i)%surf(sc%AO) + p(i)%epz(sc%AS,:) * p(i)%surf(sc%AS)
        p(i)%ep(sc%AOAS)    = p(i)%ep(sc%AO) * p(i)%surf(sc%AO) + p(i)%ep(sc%AS) * p(i)%surf(sc%AS)

        p(i)%turfac(sc%AOAS)   = min(p(i)%turfac(sc%AO), p(i)%turfac(sc%AS))
        p(i)%senfac(sc%AOAS)   = min(p(i)%senfac(sc%AO), p(i)%senfac(sc%AS))
        p(i)%swfac(sc%AOAS)    = min(p(i)%swfac(sc%AO), p(i)%swfac(sc%AS))
      end do


!::: Calcul des Offres - Offre en azote, absorption N, màj de l'azote du sol
      do i = 1,sc%P_nbplantes


        if (p(i)%masecdil(sc%AS) > 0.0) then


if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)print *,'Stics_Jour: offreN'


          call offreN(p(i)%zrac,soil%nit(1:int(p(i)%zrac)),soil%amm(1:int(p(i)%zrac)),sc%hur(1:int(p(i)%zrac)),   & ! les entrées
                      sc%humin(1:int(p(i)%zrac)),sc%hucc(1:int(p(i)%zrac)),p(i)%flrac(1:int(p(i)%zrac)),          &
                      pg%P_lvopt,pg%P_difN,p(i)%epz(sc%AOAS,1:int(p(i)%zrac)),p(i)%P_Vmax1,p(i)%P_Kmabs1,p(i)%P_Vmax2, &
                      p(i)%P_Kmabs2,                                                                              &
                      sc%cumoffrN,p(i)%flurac,p(i)%flusol,sc%offrN(1:int(p(i)%zrac)))                               ! les sorties






          do ens = sc%AS, sc%AO

            !if (p(i)%lai(ens,n) > 0.) then
            if (p(i)%surf(ens) > 0.) then

if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)print *,'Stics_Jour: absoN'

                cum_nitz=sum(soil%nit(1:sc%nbCouchesSol))

                !!!MODIF HISAFE 10 : Supression calcul déjà fait dans HISAFE
                !!! On calcule absz seulement si HISAFE n'a pas calculé l'extraction d'eau et d'azote lui même
                !!! SINON absz a déjà été rempli dans HISAFE
              call absoN(p(i)%zrac,sc%nbCouchesSol,sc%cumoffrN,p(i)%surf(ens),sc%offrN(1:sc%nbCouchesSol),  &
                     soil%nit(1:sc%nbCouchesSol),soil%amm(1:sc%nbCouchesSol),                           &
                     sc%absz(1:sc%nbCouchesSol),p(i)%demande(ens),p(i)%profextN,p(i)%abso(ens,n), hisafeInfluence)



                cum_absz=sum(sc%absz(1:sc%nbCouchesSol))
                cum_nitz=sum(soil%nit(1:sc%nbCouchesSol))



if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)print *,'Stics_Jour: majNsol'

! 02/09/2014 DR et ML bug du bilan d'N non bouclé pour les CAS
! il faut ponderer l'azote absorbé par les surfaces à l'ombre et au soleil de chaque plante
! sinon on enleve plus d'azote du sol que la plante n'en absorbe
              sc%absz(1:sc%nbCouchesSol)=sc%absz(1:sc%nbCouchesSol)*p(i)%surf(ens)

              !!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
              !!!Here crop nitrogen uptake ONLY will be extracted from soil
              call majNsolCrop(sc%nhe,p(i)%zrac,sc%absz(1:sc%nbCouchesSol),   &
                           soil%nit(1:sc%nbCouchesSol),soil%amm(1:sc%nbCouchesSol))

              cum_absz=sum(sc%absz(1:sc%nbCouchesSol))


            end if
          end do

        endif

      end do

      !!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
      !!!Here tree nitrogen uptake ONLY will be extracted from soil
      call majNsolTree(sc%nhe,   &
                   soil%nit(1:sc%nbCouchesSol),soil%amm(1:sc%nbCouchesSol), &
                   sc%treeNitrogenUptake(1:1000)) !

      cum_nitz=sum(soil%nit(1:sc%nbCouchesSol))

!::: Calcul des Stress - hydrique, azoté, azote des grains, excès d'eau
      do i = 1,sc%P_nbplantes

        do ens = sc%AS, sc%AO

          !if (p(i)%lai(sc%AOAS,n) > 0.) then
          if (p(i)%surf(ens) > 0.) then

          ! Stress Eau


            if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)print *, 'Stics_Jour: stressEau'

            !!!MODIF HISAFE 10 : Supression calcul déjà fait dans HISAFE
            call stressEau(n,itk(i)%P_profsem,p(i)%zrac,sc%nbCouchesSol,sc%hur(1:sc%nbCouchesSol),                  & ! IN
                           sc%humin(1:sc%nbCouchesSol),p(i)%cumlracz,p(i)%surf(ens),p(i)%P_codelaitr,               &
                           p(i)%lai(ens,n),sc%tauxcouv(n),p(i)%nrec,itk(i)%P_codcueille,p(i)%eop(ens),pg%P_rayon,   &
                           p(i)%P_psisto,p(i)%P_psiturg,p(i)%P_rapsenturg,sc%supres,t%P_swfacmin,                   &
                           p(i)%resrac,p(i)%swfac(ens),p(i)%turfac(ens),p(i)%tetstomate,p(i)%teturg,                &
                           p(i)%senfac(ens),hisafeInfluence)


          ! Stress Azote


            if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)print *, 'Stics_Jour: stressN'

            !!!MODIF HISAFE 10 : Supression calcul déjà fait dans HISAFE
            call stressN(p(i)%masecdil(ens),p(i)%abso(ens,n),p(i)%offrenod(ens),p(i)%QNplante(0,n-1),            & ! IN
                         p(i)%dltaremobilN(ens),p(i)%P_codelegume,p(i)%P_masec0,p(i)%P_adilmax,p(i)%P_masecNmax,    &
                         p(i)%P_bdilmax,p(i)%masecpartiel(ens),p(i)%magrain(ens,n),sc%absodrp,p(i)%P_codeplisoleN, &
                         p(i)%P_masecmeta,p(i)%adilI,p(i)%bdilI,p(i)%P_adil,p(i)%P_bdil,p(i)%adilmaxI,p(i)%bdilmaxI,    &
                         p(i)%dNdWcrit,p(i)%deltabso(ens),p(i)%P_codeINN,p(i)%P_INNmin,p(i)%P_INNimin,p(i)%P_innturgmin, &
                         p(i)%P_innsen,pg%P_QNpltminINN,                                                                &
                         p(i)%QNplante(ens,n),p(i)%Qfix(ens),p(i)%QNplanteres,p(i)%CNplante(ens),p(i)%inn(ens),         &
                         p(i)%inni(ens),p(i)%inns(ens),p(i)%innlai(ens),p(i)%innsenes(ens), hisafeInfluence)

          endif

        end do



      ! Cumuls AO/AS
        p(i)%QNplante(sc%aoas,sc%n) = p(i)%QNplante(sc%ao,sc%n) * p(i)%surf(sc%ao)  &
                                    + p(i)%QNplante(sc%as,sc%n) * p(i)%surf(sc%as)
        p(i)%CNplante(sc%aoas) = p(i)%CNplante(sc%ao) * p(i)%surf(sc%ao)  &
                               + p(i)%CNplante(sc%as) * p(i)%surf(sc%as)
! dr et ml 04/09/2014 on indexe sur ao et as pour les varaibles de l'abscission
        p(i)%QNplantetombe(sc%aoas) = p(i)%QNplantetombe(sc%ao) * p(i)%surf(sc%ao)  &
                               + p(i)%QNplantetombe(sc%as) * p(i)%surf(sc%as)
        p(i)%QCplantetombe(sc%aoas) = p(i)%QCplantetombe(sc%ao) * p(i)%surf(sc%ao)  &
                               + p(i)%QCplantetombe(sc%as) * p(i)%surf(sc%as)
!
        p(i)%Qfix(sc%aoas) = p(i)%Qfix(sc%ao) * p(i)%surf(sc%ao) + p(i)%Qfix(sc%as) * p(i)%surf(sc%as)

        p(i)%inn(sc%aoas)       = min(p(i)%inn(sc%ao), p(i)%inn(sc%as))
        p(i)%inns(sc%aoas)      = min(p(i)%inns(sc%ao), p(i)%inns(sc%as))
        p(i)%innsenes(sc%aoas)  = min(p(i)%innsenes(sc%ao), p(i)%innsenes(sc%as))
        p(i)%innlai(sc%aoas)    = min(p(i)%innlai(sc%ao), p(i)%innlai(sc%as))
        p(i)%inni(sc%aoas)      = min(p(i)%inni(sc%ao), p(i)%inni(sc%as))


if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)print *,'Stics_Jour: Ngrain'
if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: Ngrain'


        call Ngrain(n,p(i)%ndrp,p(i)%nrec,p(i)%P_vitirazo,p(i)%irazo(sc%AOAS,n-1),p(i)%nmat,p(i)%dltags(sc%AOAS), &
                    p(i)%QNplante(sc%AOAS,n-1),p(i)%QNplante(sc%AOAS,n),p(i)%pgrain(sc%AOAS),p(i)%nbgraingel,     &
                    p(i)%P_irmax,p(i)%P_vitircarb,p(i)%magrain(sc%AOAS,n),                                        &
                    p(i)%irazo(sc%AOAS,n),p(i)%QNgrain(sc%AOAS),p(i)%CNgrain(sc%AOAS))


        p(i)%CNgrain(sc%AO) = p(i)%CNgrain(sc%AOAS)
        p(i)%CNgrain(sc%AS) = p(i)%CNgrain(sc%AOAS)

        p(i)%irazo(sc%AO,sc%n) = p(i)%irazo(sc%AOAS,sc%n)
        p(i)%irazo(sc%AS,sc%n) = p(i)%irazo(sc%AOAS,sc%n)

        p(i)%QNgrain(sc%AO) = p(i)%QNgrain(sc%AOAS)
        p(i)%QNgrain(sc%AS) = p(i)%QNgrain(sc%AOAS)



      ! Stress Excès d'eau


        if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)print *,'Stics_Jour: excesdeau'


        call excesdeau(n,p(i)%cumflrac,p(i)%rltot,p(i)%P_sensanox,p(i)%racnoy(n-1),   & ! entrées
                       p(i)%racnoy(n),p(i)%exofac,p(i)%izrac,p(i)%idzrac,             & ! sorties
                       p(i)%exolai,p(i)%exobiom)

      end do

!::: MicroClimat
    ! le calcul de temperature s'effectue une seule fois pour l'ensemble des systemes plantes
    !--  ipl = 1

    ! *!* ATTENTION : ICI IL FAUT SOMMER LES EP DES PLANTES
    ! *!* TEMPSOL UNIQUE POUR TOUTES LES CULTURES
      sc%eptcult = 0.0
      do i = 1, sc%P_nbplantes
        sc%eptcult = sc%eptcult + p(i)%ep(sc%AS) + p(i)%ep(sc%AO)
      end do

      sc%hauteurMAX = 0.0
      do i = 1, sc%P_nbplantes
        sc%hauteurMAX = max(sc%hauteurMAX,p(i)%hauteur(sc%AS))
        sc%hauteurMAX = max(sc%hauteurMAX,p(i)%hauteur(sc%AO))
      end do

      sc%EmdTot = 0.0
      do i = 1, sc%P_nbplantes
        sc%EmdTot = sc%EmdTot + p(i)%Emd
      end do



    if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)print *,'Stics_Jour: caltcult'

!!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
!!!Ajout cellTrg et cellVisibleSky
      if(itk(1)%P_codepaillage == 2)then   !! paillage mulch plastique
        call caltcult(itk(1)%P_codabri,sc%EmdTot,sc%hauteurMAX,sc%eptcult,sc%esol,sc%Emulch,soil%P_z0solnu,c%tmax(n), &
                    c%tvent(n),sc%raamin,sc%rnetS,p(1)%P_codebeso,c%phoi,sc%raamax,                               &
                    sc%tcult,sc%tcultmin,sc%tcultmax,sc%et,c%tutilrnet,sc%numdate,c%daylen,c%difftcult,           &
                    sc%nitetcult(n),sc%Ratm,                                                                       &
                    sc%nbCouchesSol,sc%jul,sc%hur(1:sc%nbCouchesSol),sc%humin(1:sc%nbCouchesSol),                 &
                    sc%hucc(1:sc%nbCouchesSol),sc%laiTot,sc%tauxcouv(n),sc%posibsw,soil%P_albedo,                  &
                    itk%P_couvermulchplastique,itk(1)%P_albedomulchplastique,sta%P_albveg,sta%P_codernet,sta%P_aangst, &
                    sta%P_bangst,c%tmin(n),c%tmoy(n),sta%P_corecTrosee,c%trg(n),sta%P_latitude,itk(1)%P_codepaillage,  &
                    p(1)%P_codelaitr, c%tpm(n),sta%P_codecaltemp,sc%albedolai,sc%rglo,sc%rnet, &
                    cellTrg, cellVisibleSky)
      else
        call caltcult(itk(1)%P_codabri,sc%EmdTot,sc%hauteurMAX,sc%eptcult,sc%esol,sc%Emulch,soil%P_z0solnu,c%tmax(n), &
                    c%tvent(n),sc%raamin,sc%rnetS,p(1)%P_codebeso,c%phoi,sc%raamax,                                   &
                    sc%tcult,sc%tcultmin,sc%tcultmax,sc%et,c%tutilrnet,sc%numdate,c%daylen,c%difftcult,           &
                    sc%nitetcult(n),sc%Ratm,                                                                       &
                    sc%nbCouchesSol,sc%jul,sc%hur(1:sc%nbCouchesSol),sc%humin(1:sc%nbCouchesSol),                 &
                    sc%hucc(1:sc%nbCouchesSol),sc%laiTot,sc%tauxcouv(n),sc%posibsw,soil%P_albedo,                  &
                    sc%couvermulch,pg%P_albedomulchresidus(sc%irmulch),sta%P_albveg,sta%P_codernet,sta%P_aangst,sta%P_bangst, &
                    c%tmin(n),c%tmoy(n),sta%P_corecTrosee,c%trg(n),sta%P_latitude,itk(1)%P_codepaillage,p(1)%P_codelaitr,     &
                    c%tpm(n),sta%P_codecaltemp,sc%albedolai,sc%rglo,sc%rnet, &
                    cellTrg, cellVisibleSky)
      endif



    if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)print *,'Stics_Jour: tempsol'


      call tempsol(sc%tcultmin,sc%tcultmax,pg%P_diftherm,soil%profsol,          & ! entrées
                   sc%tsolveille(1:int(soil%profsol)),sc%tcultveille,c%tmin(n), &
                   sc%tsol(1:int(soil%profsol)))                                  ! sorties


      sc%tsolveille(1:int(soil%profsol)) = sc%tsol(1:int(soil%profsol))
    ! PB - 29/04/2004 - pour éviter des bugs qd zrac = 0
      sc%tsol(0) = sc%tsol(1)


      sc%tcultveille = sc%tcult

    ! rajout domi du 23/04/97 : pour homogeneiser avec la temp de surface on calcule
    ! les sommes de temp du developpement et le reste avec la temp air de la veille
      sc%tairveille = c%tmoy(n)

    ! inactivation de l'effet tcult sur  les plantes si P_codeh2oact = 0
      if (pg%P_codeh2oact == 0) sc%tcult = c%tmoy(n)

      sc%epTot = 0.0
      do i = 1,sc%P_nbplantes
        sc%epTot = sc%epTot + p(i)%ep(sc%AS) + p(i)%ep(sc%AO)
      end do

      if (itk(1)%P_codabri /= 2) then
        sc%et = sc%esol + sc%EmdTot + sc%epTot
      endif


if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: humcouv'


    ! NB le 26/01/05 intro de P_patm
      call humcouv(c%tmoy(n),sc%tcult,c%tpm(n),sta%P_ra,sc%Edirect,0,sc%epTot,sc%rnet,sta%P_patm,sc%humidite,c%humair)



      if (pg%P_codemicheur == 1) then

      ! NB le 08/08/05 oubli de calcul de numdate
      ! TODO : on calcule la photopériode systématiquement avant la phase de développement de la plante
      !        on doit pouvoir supprimer le calcul ici.
        sc%numdate = sc%jul


if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: photpd'


        call photpd(sta%P_latitude,sc%numdate,c%daylen,c%phoi)


if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: humheure'


      ! PB - 27/07/2009
      ! Quand n=1, on essaie de lire tmin(0) qui n'existe pas, pour éviter ça, on sort les deux lignes suivantes de humheure.
      ! TODO : attention, si n = nbjmax, on risque de lire tmin(nbjmax+1) => débordement !
        if (n > 1) tmaxveille = sc%tcultveille * 2 - c%tmin(n-1)
        if (n == 1) tmaxveille = sc%tcultmax

!: ML - 29/10/12 - calcul durée d'humectation: ajout de l'incrementation de compteurhumheure
!: et des arguments supplémentaires dans l'appel à la fonction humheure
!if (t%P_codeSWDRH.eq.2) c%compteurhumheure = 2

!        call humheure(n,sc%tcultmin,sc%tcultmax,tmaxveille,sc%humidite,c%daylen,c%tmin(n+1),  & ! entrées
!                      sc%tcult,sc%trosemax(0:n),c%humimoy,c%tmin(n),c%dureehumec,c%dureeRH,c%dureeRH1, &  ! sorties
!                   c%dureeRH2,c%compteurhumheure,c%trr(n))

        call humheure(n,sc%tcultmin,sc%tcultmax,tmaxveille,sc%humidite,c%daylen,c%tmin(n+1),sc%tcult,    &
           sc%trosemax(0:n),c%humimoy,c%tmin(n),t%P_codetrosee,tculth,troseh,humh)


!        c%compteurhumheure = 0

!: ML - fin


      endif


    if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: cumAOetAS'


!::: Cumul AO/AS

      do i = 1,sc%P_nbplantes
        call cumAOetAS(n,sc%P_codesimul,p(i),itk(i))
      end do

!::: Fonction d'écriture des sorties journalières
    ! on réinitialise le nombres des apports (irrig et fertil) pour les recalculer
      sc%naptot  = 0
      sc%napNtot = 0
    ! domi le 20/07/04 on calcul naptot et napNtot comme le cumul des 2 plantes
      sc%napNtot = SUM(itk(:)%napN)
      sc%naptot  = SUM(itk(:)%nap)


      call Stics_Jour_after(sc,pg,c,sta,soil,p,itk,t)
            if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0) print *,'apres stics_jour_after'

!write(*,*)'avant gestion',sc%n,p(1)%lai(0,sc%n)
!::: Gestion des coupes
if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: gestion des coupes'
      call GestionDesCoupes(sc,pg,c,sta,soil,p,itk,t)

!write(*,*)'apres gestion',sc%n,p(1)%lai(0,sc%n)

!::: Dynamique des talles
    ! introduction de la dynamique des talles
    ! NB le 08/03/07
    ! TODO: vérifier qu'il s'agit bien de passer deltai(n)
      do i = 1,sc%P_nbplantes

      ! DR et ML et SYL 15/06/09
      ! ************************
      ! introduction de la fin des modifications de Sylvain (nadine et FR)
      ! dans le cadre du projet PERMED
      ! on supprime la condition sur P_codeplante='fou' et on indexe P_codedyntalle sur la plante
        if (t%P_codedyntalle(i) == 1) then
      ! DR et ML et SYL 15/06/09 FIN introduction de la fin des modifications de Sylvain

if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)print *,'Stics_Jour: dynamique des talles'


          if (t%P_codetranspitalle == 1) then
            call dynamictalle(t%P_SurfApex(i),t%P_SeuilMorTalle(i),t%P_SigmaDisTalle(i),    &
                              t%P_VitReconsPeupl(i),t%P_SeuilReconsPeupl(i),t%P_MaxTalle(i),&
                              p(i)%densite,sc%et,sc%etm,p(i)%LAIapex,t%P_SeuilLAIapex(i),   &
                              p(i)%tempeff,p(i)%mortalle,                               &
                              p(i)%lai(sc%AOAS,n)-p(i)%lai(sc%AOAS,n-1),                &
                              p(i)%resperenne(sc%AOAS),p(i)%mortreserve,                &
                              p(i)%deltai(sc%AOAS,n),p(i)%lai(sc%AOAS,n),               &
                              p(i)%densitemax,p(i)%masec(0,n),p(i)%mortmasec,           &
                              p(i)%drlsenmortalle)
          else
            call dynamictalle(t%P_SurfApex(i),t%P_SeuilMorTalle(i),t%P_SigmaDisTalle(i),      &
                              t%P_VitReconsPeupl(i),t%P_SeuilReconsPeupl(i),t%P_MaxTalle(i),  &
                              p(i)%densite,p(i)%ep(sc%AOAS),p(i)%eop(sc%AOAS),          &
                              p(i)%LAIapex,t%P_SeuilLAIapex(i),p(i)%tempeff,p(i)%mortalle, &
                              p(i)%lai(sc%AOAS,n)-p(i)%lai(sc%AOAS,n-1),                &
                              p(i)%resperenne(sc%AOAS),p(i)%mortreserve,                &
                              p(i)%deltai(sc%AOAS,n),p(i)%lai(sc%AOAS,n),               &
                              p(i)%densitemax,p(i)%masec(0,n),p(i)%mortmasec,           &
                              p(i)%drlsenmortalle)
            if (p(i)%densite < 1.0) then
              call EnvoyerMsgHistorique(164,n)
            !--  stop
            endif
! DR et ML et SYL 15/06/09 FIN introduction de la fin des modifications de Sylvain
          endif
        endif
      end do

if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )print *,'end day ',n


end subroutine Stics_Jour2

 
