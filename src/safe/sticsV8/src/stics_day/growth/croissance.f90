! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This subroutine calls the main modules involved in crop growth simulation:
!>   - biomaer.f90 : shoot biomass growth
!>   - senescen.f90 : leaf senescence
!>   - ApportFeuillesMortes.f90 : decomposition of leaves fallen at soil surface
!>   - fruit.f90 : yield formation for indeterminate crops
!>   - grain.f90 : yield formation for determinate crops
!>   - eauqual.f90 : water content evolution in harvested organs
!>   - croissanceFrontRacinaire.f90 : root front growth
!>   - densiteRacinaire.f90 : root density profile
!>   - calpsibase.f90 : plant water potential
!>   - repartir.f90 : dimensioning of organs
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
subroutine croissance(sc,pg,p,itk,soil,c,sta,t)

  USE Stics
  USE Plante
  USE Itineraire_Technique
  USE Sol
  USE Climat
  USE Station
  USE Parametres_Generaux
  USE Divers
  USE Module_Croissance ! Pour les interfaces des routines

  implicit none


  type(Stics_Communs_),       intent(INOUT) :: sc  
  type(Parametres_Generaux_), intent(IN)    :: pg  
  type(Plante_),              intent(INOUT) :: p (sc%P_nbplantes)
  type(ITK_),                 intent(INOUT) :: itk (sc%P_nbplantes)
  type(Sol_),                 intent(INOUT) :: soil  
  type(Climat_),              intent(INOUT) :: c  
  type(Station_),             intent(INOUT) :: sta  
  type(Stics_Transit_),       intent(INOUT) :: t  

!: Variables locales
  integer :: ens  
  integer :: n  
  integer :: i ! indice plante  
  integer :: ic  !>  
  integer :: ii  !>
  integer :: iz
  real :: repracmin  !>  
  real :: repracmax  !>  
  real :: kreprac  
  real :: trg_bak  
  real :: densiteassoc  
  integer :: profhoriz  

      ! ** on distingue la culture principale et la culture associée car les traitements
      ! *- sont différents. Pour la culture principale on a notament besoin de calculer
      ! *- la variable originehaut et pour la culture associée d'affecter des trg(n) rapportés
      ! *- au rayonnement intercepté par la culture principale

      ! pour alléger l'écriture
      n = sc%n

    if (sc%P_nbplantes > 1) then
        densiteassoc = p(1)%densite / p(1)%P_bdens * p(2)%P_bdens
    else
        densiteassoc = 0.
    endif

    do i = 1, sc%P_nbplantes
       do ens = sc%AS,sc%AO
       ! DR et ML le 07/09/2012: suppression de la ligne en dessous car cela empêchait la croissance racinaire
       ! de la plante associee entre germination et levee de la plante principale
!         if (p(i)%surf(ens) > 0.) then ! on ne travaille que si la surface de la plante est supérieur à zéro.
! DR et ML 12/09/2012 on teste pour que ca focntionne avce plante puer et Cas (si on l'enlevait ca induisait des ecarts pour la vigne pure)
         if (p(i)%surf(ens) > 0..or.i>1) then ! on ne travaille que si la surface de la plante est supérieur à zéro.
!!!MODIF HISAFE 1 : suppression des chaines de caractères
            !!!if (p(i)%P_codeplante /= 'snu') then
            if (p(i)%P_codeplante /= 1) then
              if (i < sc%P_nbplantes) then
                sc%originehaut = max(p(i+1)%hauteur(sc%AO),p(i+1)%hauteur(sc%AS))
              else
                sc%originehaut = 0.0
              endif
              trg_bak = c%trg(n)
              if (i > 1) c%trg(n) = trg_bak * p(i-1)%rsoleil

 ! print *,'avant biomaer'
              call biomaer(n,i,ens,p(i)%nlev,p(i)%P_codeperenne,p(i)%nplt,p(i)%P_codehypo,p(i)%P_codegermin,                   &
                           p(i)%P_masecplantule,p(i)%P_adil,p(i)%namf,p(i)%P_adilmax,p(i)%nrec,itk(i)%P_codcueille,            &
                           itk(i)%P_codefauche,p(i)%P_codelaitr,p(i)%P_codetransrad,p(i)%P_efcroijuv,p(i)%P_efcroiveg,         &
                           p(i)%ndrp,p(i)%P_efcroirepro,p(i)%chargefruit,pg%P_coefb,sc%tcult,p(i)%P_teopt,p(i)%P_teoptbis,     &
                           p(i)%P_temin,p(i)%P_temax,sta%P_codeclichange,p(i)%P_alphaco2,c%co2(n),t%P_resplmax(i),p(i)%densite,&
                           pg%P_codeh2oact,p(i)%swfac(ens),p(i)%exobiom,pg%P_codeinnact,p(i)%dltaremobil(ens),                 &
                           p(i)%P_codeindetermin,p(i)%fpv(ens),p(i)%fpft(ens),p(i)%P_remobres,p(i)%P_resperenne0,            &
                           p(i)%demande(ens),p(i)%P_QNplante0,itk(i)%P_msresiduel(p(i)%numcoupe-1),itk(i)%P_nbcueille,         &
 ! dr 07/12/2010 c'est nbrecolte-1 et non nbrecolte si on se fie à 7.1
                           p(i)%rdtint(ens,p(i)%nbrecolte-1),p(i)%CNgrain(ens),t%P_codemontaison(i),p(i)%nmontaison,           &
! dr et ML 29/08/2014
! masec_veille doit etre masec global de la plante : masec(0,n-1) et non masec(ens, n-1) sachant que c'est ensuite pondéré
! par les surfaces à l'ombre et au soleil et que ces surfaces evoluent d'un jour à l'autre , notamment si il y a inversion de dominance
!                          p(i)%masec(ens,n-1),p(i)%masec(ens,n),p(i)%QNplante(ens,n-1),p(i)%QNplante(ens,n),                  &
! idem pour qnplante
!                           p(i)%masec(0,n-1),p(i)%masec(ens,n),p(i)%QNplante(ens,n-1),p(i)%QNplante(ens,n),                    &
                           p(i)%masec(0,n-1),p(i)%masec(ens,n),p(i)%QNplante(0,n-1),p(i)%QNplante(ens,n),                      &
                           p(i)%inns(ens),p(i)%inn(ens),p(i)%innlai(ens),sc%cumdltaremobilN,p(i)%ebmax,p(i)%ftemp,             &
                           p(i)%epsib(ens),p(i)%fco2,p(i)%dltams(ens,n),p(i)%dltamsen(ens),p(i)%dltamstombe(ens),              &
                           p(i)%resperenne(ens),p(i)%dltamsN(ens),                                                              &
                           p(i)%photnet(ens),p(i)%sourcepuits(ens),p(i)%dltaremobilN(ens),p(i)%remobilj(ens),                 &
! idem pour magrain
!                           p(i)%cumdltares(ens),p(i)%magrain(ens,n-1),p(i)%magrain(ens,n),p(i)%masecneo(ens),p(i)%surf,        &
                           p(i)%cumdltares(ens),p(i)%magrain(0,n-1),p(i)%magrain(ens,n),p(i)%masecneo(ens),p(i)%surf,          &
                           p(i)%surfSous,sc%P_nbplantes,p%P_extin,p(i)%cumrg,p(i)%cumraint,p(i)%fapar(ens),sc%delta,           &
                           p(i)%P_adfol,p(i)%lairognecum(ens),p(i)%laieffcum(ens),p(i)%P_dfolbas,p(i)%P_dfolhaut,p(i)%dfol,    &
                           sc%rdif,p(i)%parapluie,p(i)%raint(ens),pg%P_parsurrg,p(i)%P_forme,p(i)%lai(ens,n),                  &
                           p(i)%laisen(ens,n),p(i)%eai(ens),itk(i)%P_interrang,p(i)%nlax,p(i)%nsen,p(i)%P_codlainet,           &
                           p(i)%P_hautbase,itk(i)%P_codepalissage,itk(i)%P_hautmaxtec,itk(i)%P_largtec,sc%originehaut,         &
                           p(i)%hauteur(ens),p(i)%deltahauteur(ens),p(i)%P_hautmax,p(i)%varrapforme,p(i)%largeur,sc%jul,       &
                           c%trg(n),sta%P_latitude,p(i)%rombre,p(i)%rsoleil,itk(i)%P_orientrang,p(i)%P_ktrou,sc%tauxcouv(n),   &
                           pg%P_khaut,sc%surfAO,sc%surfAS,p(i)%fpari)

              ! on met à jour le cumul des parties AO/AS de masec, dltams calculés dans biomaer
              ! TODO: si on passe pour chaque partie AO/AS on effectue le calcul 2 fois... y'a pas mieux à faire ?
              p(i)%masec(0,n) =  p(i)%masec(sc%AS,n) * p(i)%surf(sc%AS) + p(i)%masec(sc%AO,n) * p(i)%surf(sc%AO)
              p(i)%dltams(0,n) = p(i)%dltams(sc%AS,n)* p(i)%surf(sc%AS) + p(i)%dltams(sc%AO,n)* p(i)%surf(sc%AO)

              !: On en profite pour attribuer les surfaces ombre/soleil aux plantes dominées
              !DR et ML 20/04/2016 on ne calcule les surfaces que si les 2 plantes ont levé (et pas uniquement la principale)
              ! on rajoute une condition sur la levée des 2 plantes
              !!!MODIF HISAFE 12 : Modif après détection bug
              !!!Test ajouté pour eviter plantage
              if (sc%P_nbplantes > 1) then
                  if(p(1)%nlev.gt.0.and.p(2)%nlev.gt.0)then
                     if (i < sc%P_nbplantes) then
                         p(i+1)%surf(sc%AS) = p(i)%surfSous(sc%AS)
                         p(i+1)%surf(sc%AO) = p(i)%surfSous(sc%AO)
                     endif
                  endif
              endif

              if (i > 1) c%trg(n) = trg_bak
              p(i)%gelee = .FALSE.

              ! DR et julie - 12/08/08 : le qnplante n'est pas encore calculé on prend celui de la veille
! le trio 26/07/2012 si on a un qressuite c'est qu'on a fait l'apport des residus à la recolte donc on passe pas dans senescen et apportfeuillemortes

!    avant 26/07/2012          if (p(i)%P_codelaitr == 1) then
!    apres 26/07/2012          if (p(i)%P_codelaitr == 1.and.p(i)%qressuite_tot == 0) then
! DR et ML le 05/04/2013 cas specifique des cultures dont on laisse une partie en place qui peut senescer (ex prairie apres une coupe : on veut calculer msresjaune)
! il faudra voir si c'est interessant aussi pour le miscanthus et la canne

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!                if ((p(i)%P_codelaitr == 1.and.p(i)%qressuite_tot == 0).or.(p(i)%P_codelaitr == 1.and.p(i)%P_codeplante=='fou')) then
              if ((p(i)%P_codelaitr == 1.and.p(i)%qressuite_tot == 0).or.(p(i)%P_codelaitr == 1.and.p(i)%P_codeplante==2)) then
!  print *,'avant senescen'


              call senescen(p(i)%nlev,n,sc%nbjmax,p(i)%lai(ens,n),pg%P_codeinnact,pg%P_codeh2oact,p(i)%senfac(ens),        &
                            p(i)%innsenes(ens),p(i)%P_codlainet,p(i)%P_codeperenne,p(i)%nbfeuille,p(i)%P_nbfgellev,        &
                            p(i)%P_codgellev,sc%tcultmin,p(i)%P_tletale,p(i)%P_tdebgel,p(i)%P_tgellev90,                   &
                            p(i)%P_tgellev10,i,p(i)%densitelev,densiteassoc,p(i)%P_codgeljuv,p(i)%P_tgeljuv90,             &
                            p(i)%P_tgeljuv10,p(i)%P_codgelveg,p(i)%P_tgelveg90,p(i)%P_tgelveg10,p(i)%masecveg(ens),        &
                            p(i)%nstopfeuille,p(i)%somcour,p(i)%resperenne(ens),p(i)%ndrp,p(i)%nrec,pg%P_QNpltminINN,      &
                            sc%numcult,pg%P_codeinitprec,p(i)%ulai(1:sc%nbjmax),p(i)%P_vlaimax,p(i)%durvieI,               &
                            p(i)%P_durvieF(itk(i)%P_variete),p(i)%inn(ens),p(i)%P_durviesupmax,p(i)%P_codestrphot,c%phoi,  &
                            p(i)%P_phobasesen,p(i)%dltams(ens,1:sc%nbjmax),itk(i)%P_msresiduel(p(i)%numcoupe-1),           &
                            p(i)%P_ratiosen,p(i)%tdevelop(1:n),p(i)%somtemp,p(i)%pfeuilverte(ens,1:sc%nbjmax),             &
                            p(i)%deltai(ens,1:sc%nbjmax),p(i)%P_lai0,                                                      &
                            sc%dernier_n,p(i)%nsencour,p(i)%dltaisen(ens),p(i)%dltamsen(ens),p(i)%fstressgel,              &
                            p(i)%fgellev,p(i)%gelee,p(i)%densite,p(i)%laisen(ens,n-1:n),p(i)%nlan,                         &
                            p(i)%P_stsenlan(itk(i)%P_variete),p(i)%nsen,p(i)%P_stlaxsen(itk(i)%P_variete),p(i)%namf,       &
                            p(i)%nlax,p(i)%P_stlevamf(itk(i)%P_variete),p(i)%P_stamflax(itk(i)%P_variete),p(i)%nrecbutoir, &
                            p(i)%mortplante,p(i)%nst2,p(i)%mortplanteN,p(i)%durvie(ens,1:sc%nbjmax),                       &
                            p(i)%strphot(ens),p(i)%msres(ens),p(i)%dltamsres(ens),p(i)%ndebsen,                            &
                            p(i)%somsenreste(ens),p(i)%msresjaune(ens),p(i)%mafeuiljaune(ens),p(i)%msneojaune(ens),        &
!02/09/2014 DR et ML
! QNplante_veille doit etre QNplante global de la plante : QNplante(0,n-1) et non QNplante(ens, n-1) sachant que c'est ensuite pondéré
! par les surfaces à l'ombre et au soleil et que ces surfaces evoluent d'un jour à l'autre , notamment si il y a inversion de dominance
!                            p(i)%dltamstombe(ens),p(i)%QNplante(ens,n-1),p(i)%P_dltamsminsen,p(i)%P_dltamsmaxsen,          &
                            p(i)%dltamstombe(ens),p(i)%QNplante(0,n-1),p(i)%P_dltamsminsen,p(i)%P_dltamsmaxsen,          &
                            p(i)%P_alphaphot,p(i)%strphotveille(ens))

! EC 06/08/2012 Ajout du code ires des feuilles mortes
              sc%ires = 2
!  print *,'avant apportfeuillemorte'
              call ApportFeuillesMortes(p(i)%fstressgel,p(i)%CNplante(ens),p(i)%P_abscission,p(i)%inn(ens),p(i)%P_parazofmorte, &
                                    p(i)%dltamsen(ens),t%P_codedyntalle(i),p(i)%mortmasec,p(i)%mortreserve,                 &
                                    p(i)%surf(ens),pg%P_CNresmin(sc%ires),pg%P_CNresmax(sc%ires),                           &
                                    sc%nbCouchesSol,pg%nbResidus,pg%P_Qmulchdec(sc%ires),                                   &
!02/09/2014 DR et ML
! idem au dessus ligne 157
!                                        p(i)%dltamstombe(ens),p(i)%mafeuiltombe(ens),p(i)%QNplante(ens,n-1),p(i)%QNplantetombe, &
                                p(i)%dltamstombe(ens),p(i)%mafeuiltombe(ens),p(i)%QNplante(0,n-1),p(i)%QNplantetombe(ens), &
                                   itk(i)%nap,sc%airg(sc%n+1),soil%itrav1,soil%itrav2,sc%ires,                             &
                                   sc%Cres(1:sc%nbCouchesSol,1:pg%nbResidus),sc%Nres(1:sc%nbCouchesSol,1:pg%nbResidus),    &
                                   sc%Cnondec(1:10),sc%Nnondec(1:10),p(i)%resperenne(ens),p(i)%QCplantetombe(ens),         &
                                   sc%Cmulchnd,sc%Nmulchnd,sc%QCapp,sc%QNapp,sc%QCresorg,sc%QNresorg,                      &
                                   pg%P_awb(sc%ires),pg%P_bwb(sc%ires),pg%P_cwb(sc%ires),pg%P_CroCo(sc%ires),              &
                                   pg%P_akres(sc%ires), pg%P_bkres(sc%ires),pg%P_ahres(sc%ires),pg%P_bhres(sc%ires),       &
                                    sc%Wb(sc%ires),sc%kres(sc%ires),sc%hres(sc%ires))


              !TODO: on passe airg(n+1) pour ApportsResidus. Mais attention si n == 731, débordement de tableau !
              !
              ! 02/09/2014 DR et ML pourquoi on fait ca !!!!
              ! on supprime les 2 lignes suivantes on n'a pas de raison de recalculer ce qui a ete fait le jour d'avant
              !  p(i)%QNplante(sc%aoas,sc%n-1) = p(i)%QNplante(sc%ao,sc%n-1) * p(i)%surf(sc%ao)    &
              !                                + p(i)%QNplante(sc%as,sc%n-1) * p(i)%surf(sc%as)

              endif

              !: Affectation des sensibilités au gel des fleurs et des fruits
              if (p(i)%nflo > 0 .and. p(i)%nrec == 0) then
                p(i)%fgelflo = GEL(p(i)%P_codgelflo,sc%tcultmin,p(i)%P_tletale,p(i)%P_tdebgel,p(i)%P_tgelflo90,p(i)%P_tgelflo10)
                if (p(i)%fgelflo < 1.) p(i)%gelee = .TRUE.
              endif
              if (p(i)%gelee .eqv. .TRUE.) p(i)%nbjgel = p(i)%nbjgel + 1
              if (p(i)%P_codeindetermin == 2) then

              ! on sauvegarde la valeur de nfruit
                p(i)%nfruitv(sc%AO,p(i)%P_nboite) = p(i)%nfruit(sc%AO,p(i)%P_nboite)
                p(i)%nfruitv(sc%AS,p(i)%P_nboite) = p(i)%nfruit(sc%AS,p(i)%P_nboite)
!  print *,'avant fruit'
                call fruit(n,p(i)%namf,p(i)%P_codcalinflo,p(i)%P_pentinflores,p(i)%densite,p(i)%P_resperenne0,      &
                           p(i)%cumdltares(ens),p(i)%P_inflomax,p(i)%ndrp,itk(i)%P_nbcueille,p(i)%nrec,             &
                           p(i)%P_nboite,p(i)%P_dureefruit(itk(i)%P_variete),p(i)%fpv(ens),p(i)%dltams(ens,n),    &
                           p(i)%dltaremobil(ens),p(i)%remobilj(ens),p(i)%P_spfrmin,p(i)%P_spfrmax,                  &
                           p(i)%P_afruitpot(itk(i)%P_variete),p(i)%upvt,p(i)%fgelflo,p(i)%P_codazofruit,         &
                           pg%P_codeinnact,p(i)%inns(ens),p(i)%nnou,itk(i)%P_codeclaircie,itk(i)%P_nb_eclair,       &
                           p(i)%neclair(1:itk(i)%P_nb_eclair), itk(i)%P_nbinfloecl(1:itk(i)%P_nb_eclair) ,          &
                           p(i)%P_codetremp,sc%tcultmin,sc%tcultmax,p(i)%P_tminremp,p(i)%P_tmaxremp,                &
                           p(i)%nbrecolte,p(i)%rdtint(ens,1:p(i)%nbrecolte),p(i)%P_allocfrmax,p(i)%nmat,            &
                           p(i)%P_afpf,p(i)%P_bfpf,p(i)%P_cfpf,p(i)%P_dfpf,p(i)%pfmax,                              &
                           p(i)%P_nbinflo,p(i)%nfruit(ens,1:p(i)%P_nboite),p(i)%pdsfruit(ens,1:p(i)%P_nboite),      & !INOUT
                           p(i)%fpft(ens),p(i)%sourcepuits(ens),                                                    &
                           p(i)%spfruit(ens),p(i)%nbfruit,p(i)%nfruitnou(ens),p(i)%nbfrote,                         &
                           sc%devjourfr,p(i)%cumdevfr,p(i)%pousfruit(ens),p(i)%ftempremp,p(i)%nbj0remp,             &
                           p(i)%dltags(ens),p(i)%frplusp(ens),p(i)%ircarb(ens,n),p(i)%magrain(ens,n-1:n),           &
                           p(i)%allocfruit(ens),p(i)%masec(ens,n),p(i)%compretarddrp,p(i)%pdsfruittot(ens))

                ! on met à jour le cumul des parties AO/AS de masec qui a pu être modifié dans la routine <fruit>
                ! TODO: si on passe pour chaque partie AO/AS on effectue le calcul 2 fois... y'a pas mieux à faire ?
                p(i)%masec(sc%aoas,sc%n) = p(i)%masec(sc%as,sc%n) * p(i)%surf(sc%as)      &
                                         + p(i)%masec(sc%ao,sc%n) * p(i)%surf(sc%ao)

              ! TODO: Attention, il y a un pb avec ce genre de cumul, car si on le fait plusieurs fois pour une même journée n
              !       alors on cumule plusieurs fois la même valeur ce qui fausse les résultats.
              !       Pour l'instant, je désactive le calcul dans cumAOetAS.f90
                p(i)%nfruit(sc%aoas,1:p(i)%P_nboite) = p(i)%nfruit(sc%aoas,1:p(i)%P_nboite)                             &
                       + (p(i)%nfruit(sc%ao,1:p(i)%P_nboite) - p(i)%nfruitv(sc%ao,1:p(i)%P_nboite)) * p(i)%surf(sc%ao)  &
                       + (p(i)%nfruit(sc%as,1:p(i)%P_nboite) - p(i)%nfruitv(sc%as,1:p(i)%P_nboite)) * p(i)%surf(sc%as)

                p(i)%pdsfruit(ens,1:p(i)%P_nboite) = p(i)%pdsfruit(sc%ao,1:p(i)%P_nboite) * p(i)%surf(sc%ao) &
                                                 + p(i)%pdsfruit(sc%as,1:p(i)%P_nboite) * p(i)%surf(sc%as)

                p(i)%fpft(sc%aoas) = p(i)%fpft(sc%as) * p(i)%surf(sc%as)                &
                                   + p(i)%fpft(sc%ao) * p(i)%surf(sc%ao)

                p(i)%sourcepuits(sc%aoas) = p(i)%sourcepuits(sc%ao) * p(i)%surf(sc%ao)  &
                                          + p(i)%sourcepuits(sc%as) * p(i)%surf(sc%as)

                p(i)%spfruit(sc%aoas) = p(i)%spfruit(sc%ao) * p(i)%surf(sc%ao)          &
                                      + p(i)%spfruit(sc%as) * p(i)%surf(sc%as)

                p(i)%nfruitnou(sc%aoas) = p(i)%nfruitnou(sc%ao) * p(i)%surf(sc%ao)      &
                                        + p(i)%nfruitnou(sc%as) * p(i)%surf(sc%as)

                p(i)%pousfruit(sc%aoas) = p(i)%pousfruit(sc%ao) * p(i)%surf(sc%ao)      &
                                        + p(i)%pousfruit(sc%as) * p(i)%surf(sc%as)

                p(i)%dltags(sc%aoas) = p(i)%dltags(sc%as) * p(i)%surf(sc%as)            &
                                     + p(i)%dltags(sc%ao) * p(i)%surf(sc%ao)

                 p(i)%ircarb(sc%aoas,n) = (p(i)%ircarb(sc%ao,n) + p(i)%ircarb(sc%as,n)) / sc%P_nbplantes

                p(i)%magrain(sc%aoas,n) = p(i)%magrain(sc%ao,n) * p(i)%surf(sc%ao)      &
                                        + p(i)%magrain(sc%as,n) * p(i)%surf(sc%as)


                p(i)%allocfruit(sc%aoas) = p(i)%allocfruit(sc%ao) * p(i)%surf(sc%ao)    &
                                         + p(i)%allocfruit(sc%as) * p(i)%surf(sc%as)

              else

 ! print *,'avant grain'
                call grain(n,p(i)%ndrp,p(i)%nrec,p(i)%nlev,p(i)%nrecbutoir,p(i)%P_nbjgrain,                                 &
                           p(i)%dltams(ens,1:p(i)%ndrp),p(i)%P_cgrain,p(i)%P_cgrainv0,p(i)%P_nbgrmin,                       &
                           p(i)%P_nbgrmax(itk(i)%P_variete),p(i)%P_codazofruit,pg%P_codeinnact,p(i)%inns(ens),p(i)%fgelflo, &
                           p(i)%P_codeir,p(i)%P_vitircarb,p(i)%P_irmax,p(i)%P_vitircarbT,p(i)%somcourdrp,p(i)%nmat,         &
                           p(i)%masec(ens,n-1:n),p(i)%P_codetremp,sc%tcultmin,sc%tcultmax,p(i)%P_tminremp,                  &
                           p(i)%P_tmaxremp,p(i)%P_pgrainmaxi(itk(i)%P_variete),                                             &
                           p(i)%ircarb(ens,n-1:n),p(i)%nbgrains(ens),p(i)%pgrain(ens),p(i)%CNgrain(ens),                    &
                           p(i)%vitmoy(ens),p(i)%nbgraingel,p(i)%pgraingel(ens),p(i)%dltags(ens),p(i)%ftempremp,            &
                           p(i)%magrain(ens,n-1:n),p(i)%nbj0remp,p(i)%pdsfruittot(ens))


              !DR 25/09/2012 tous ces cumuls sont à exteriorisés pourquoi sont t'ils la ???

                p(i)%dltags(sc%aoas) = p(i)%dltags(sc%as) * p(i)%surf(sc%as)  &
                                     + p(i)%dltags(sc%ao) * p(i)%surf(sc%ao)

                p(i)%ircarb(sc%aoas,n) = (p(i)%ircarb(sc%ao,n) + p(i)%ircarb(sc%as,n)) / min(2,sc%P_nbplantes) ! on triche pour avoir une division par 1 ou 2

! DR 25/09/2012 vraimenet bizarre ce truc on cumulait 2 fois le nbgrains pour les cas
!                p(i)%nbgrains(sc%aoas) = p(i)%nbgrains(sc%aoas)                                                   &
!                                         + (p(i)%nbgrains(sc%ao) - p(i)%nbgrainsv(sc%ao)) * p(i)%surf(sc%ao)      &
!                                         + (p(i)%nbgrains(sc%as) - p(i)%nbgrainsv(sc%as)) * p(i)%surf(sc%as)
                p(i)%nbgrains(sc%aoas) = (p(i)%nbgrains(sc%ao)  * p(i)%surf(sc%ao))      &
                                         + (p(i)%nbgrains(sc%as) * p(i)%surf(sc%as))

                p(i)%CNgrain(sc%aoas) = p(i)%CNgrain(sc%ao) * p(i)%surf(sc%ao)  &
                                      + p(i)%CNgrain(sc%as) * p(i)%surf(sc%as)



                p(i)%vitmoy(sc%aoas) = p(i)%vitmoy(sc%ao) * p(i)%surf(sc%ao)  &
                                     + p(i)%vitmoy(sc%as) * p(i)%surf(sc%as)

                p(i)%magrain(0,n) = p(i)%magrain(0,n-1)                                                     &
                                    + (p(i)%magrain(sc%ao,n) - p(i)%magrain(sc%ao,n-1)) * p(i)%surf(sc%ao)  &
                                    + (p(i)%magrain(sc%as,n) - p(i)%magrain(sc%as,n-1)) * p(i)%surf(sc%as)

                p(i)%pgrain(sc%aoas) = (p(i)%pgrain(sc%ao) + p(i)%pgrain(sc%as)) / min(2,sc%P_nbplantes) ! on triche pour avoir une division par 1 ou 2


              endif
!  print *,'avant eauqual'
              call eauqual(n,p(i)%P_deshydbase,p(i)%P_tempdeshyd,sc%tcult,sc%tairveille,p(i)%ndrp,                  &
                           p(i)%nrec,p(i)%P_codeindetermin,p(i)%P_nboite,p(i)%P_stdrpdes(itk(i)%P_variete),             &
                           p(i)%P_dureefruit(itk(i)%P_variete),p(i)%nfruit(ens,1:p(i)%P_nboite),p(i)%pousfruit(ens),  &
                           p(i)%pdsfruit(ens,1:p(i)%P_nboite),p(i)%P_h2ofrvert,p(i)%frplusp(ens),                   &
                           p(i)%P_vitpropsucre,p(i)%P_vitprophuile,p(i)%magrain(ens,n),p(i)%somcourdrp,p(i)%nmat,   &
                           p(i)%maenfruit(ens),p(i)%nlev,p(i)%masec(ens,n),p(i)%mafeuilverte(ens),              &
                           p(i)%P_h2ofeuilverte,p(i)%mafeuiljaune(ens),p(i)%P_h2ofeuiljaune,p(i)%matigestruc(ens),  &
                           p(i)%P_h2otigestruc,p(i)%resperenne(ens),p(i)%P_h2oreserve,                              &
                           p(i)%deshyd(ens,0:p(i)%P_nboite),p(i)%eaufruit(ens,0:p(i)%P_nboite),p(i)%ndebdes,        &
                           p(i)%teaugrain(ens),p(i)%h2orec(ens),p(i)%sucre(ens),p(i)%huile(ens),                &
                           p(i)%sucreder(ens),p(i)%huileder(ens),p(i)%sucrems(ens),p(i)%huilems(ens),           &
                           p(i)%pdsfruitfrais(ens),p(i)%mafraisrec(ens),p(i)%CNgrain(ens),                      &
                           p(i)%mafraisfeuille(ens),p(i)%mafraistige(ens),p(i)%mafraisres(ens),p(i)%mafrais(ens))

              p(i)%CNgrain(sc%aoas) = p(i)%CNgrain(sc%ao) * p(i)%surf(sc%ao)  &
                                    + p(i)%CNgrain(sc%as) * p(i)%surf(sc%as)

              p(i)%h2orec(sc%aoas) = p(i)%h2orec(sc%ao) * p(i)%surf(sc%ao)                    &
                                   + p(i)%h2orec(sc%as) * p(i)%surf(sc%as)

              p(i)%sucre(sc%aoas)  = p(i)%sucre(sc%ao)  * p(i)%surf(sc%ao)                    &
                                   + p(i)%sucre(sc%as)  * p(i)%surf(sc%as)

              p(i)%huile(sc%aoas)  = p(i)%huile(sc%ao)  * p(i)%surf(sc%ao)                    &
                                   + p(i)%huile(sc%as)  * p(i)%surf(sc%as)

              p(i)%pdsfruitfrais(sc%aoas) = p(i)%pdsfruitfrais(sc%ao) * p(i)%surf(sc%ao)      &
                                          + p(i)%pdsfruitfrais(sc%as) * p(i)%surf(sc%as)
              p(i)%mafraisrec(sc%aoas) = p(i)%mafraisrec(sc%as) * p(i)%surf(sc%as)            &
                                       + p(i)%mafraisrec(sc%ao) * p(i)%surf(sc%ao)

              p(i)%mafraisfeuille(sc%aoas) = p(i)%mafraisfeuille(sc%as) * p(i)%surf(sc%as)    &
                                           + p(i)%mafraisfeuille(sc%ao) * p(i)%surf(sc%ao)

              p(i)%mafraistige(sc%aoas) = p(i)%mafraistige(sc%as) * p(i)%surf(sc%as)          &
                                        + p(i)%mafraistige(sc%ao) * p(i)%surf(sc%ao)

              p(i)%mafraisres(sc%aoas) = p(i)%mafraisres(sc%as) * p(i)%surf(sc%as)            &
                                       + p(i)%mafraisres(sc%ao) * p(i)%surf(sc%ao)

              p(i)%mafrais(sc%aoas) = p(i)%mafrais(sc%ao) * p(i)%surf(sc%ao)                  &
                                    + p(i)%mafrais(sc%as) * p(i)%surf(sc%as)

            ! à défaut de mieux
              p(i)%sucreder(sc%aoas) = p(i)%sucreder(sc%as) ! p(i)%sucre(sc%ao) * p(i)%surf(sc%ao) + p(i)%sucre(sc%as) * p(i)%surf(sc%as)
              p(i)%huileder(sc%aoas) = p(i)%huileder(sc%as)
              p(i)%sucrems(sc%aoas)  = p(i)%sucrems(sc%as)
              p(i)%huilems(sc%aoas)  = p(i)%huilems(sc%as)

            ! TODO: il manque des cumuls ao/as

!#if DEBUG == 1
!              if (iand(sc%eauqual,8) >0) call eauqual_debug_write_output(1258,sc,p,itk,i,ens)
!#endif

            endif ! fin P_codeplante /= snu

            if (ens == sc%AS) then

              if (p(i)%P_codtrophrac == 1) then
                repracmax = p(i)%P_repracpermax
                repracmin = p(i)%P_repracpermin
                kreprac   = p(i)%P_krepracperm
              endif
              if (p(i)%P_codtrophrac == 2) then
                repracmax = p(i)%P_repracseumax
                repracmin = p(i)%P_repracseumin
                kreprac   = p(i)%P_krepracseu
              endif
              if (p(i)%P_codtrophrac == 3) then
                repracmax = 0.
                repracmin = 0.
                kreprac   = 0.
              endif

            ! Calcul d'anoxmoy
              profhoriz = sc%nhe
              p(i)%anoxmoy = 0.
              !!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Plante.f90)
              do ic = sc%nh, 1, -1
                do ii = 1, int(soil%P_epc(ic))
                  iz = profhoriz - ii + 1
                  if (iz <= int(p(i)%zrac)) p(i)%anoxmoy = p(i)%anoxmoy + sc%anox(iz)
                end do
                profhoriz = profhoriz - int(soil%P_epc(ic))
              end do
              p(i)%anoxmoy = p(i)%anoxmoy / max(p(i)%zrac,1.)

          ! Calcul de la réserve maximale utilisée
              if (sc%n > p(i)%nplt .and. p(i)%nrec == 0) then
                p(i)%rmaxi = 0.

              ! 17/06/04 - les3
                !do 188 iz = 1,int(zrac)+1
                do iz = int(itk(i)%P_profsem), int(p(i)%zrac)+1
                  if (sc%hur(iz) < sc%hurmini(iz)) then
                    sc%hurmini(iz) = sc%hur(iz)
                  endif
                  p(i)%rmaxi = p(i)%rmaxi + sc%hucc(iz) - sc%hurmini(iz)
                end do
              endif

!: FIN EXTRAIT LIXIV

! print *,'avant croissanceFrontRacinaire'
              call croissanceFrontRacinaire(                                                         &
                     n,sc%nh,sc%nbCouchesSol,sc%hur(1:sc%nbCouchesSol),sc%hucc(1:sc%nbCouchesSol),   & ! IN
                     sc%tcult,sc%tsol(1:sc%nbCouchesSol),sc%humin(1:sc%nbCouchesSol),                &
                     sc%dacouche(1:sc%nbCouchesSol),sc%P_codesimul,soil%P_epc(1:sc%NH),soil%P_obstarac,    &
                     pg%P_daseuilbas,pg%P_daseuilhaut,pg%P_dacohes,p(i)%P_zrac0,p(i)%P_densinitial(1:sc%NH),   &
                     p(i)%P_coderacine,p(i)%P_codeplante,p(i)%P_codeperenne,p(i)%P_codehypo,                 &
                     p(i)%P_codegermin,p(i)%P_zracplantule,p(i)%P_stoprac,p(i)%P_codetemprac,                &
                     p(i)%P_tcmin,p(i)%P_tcmax,p(i)%P_tgmin,p(i)%P_croirac(itk(i)%P_variete),                  &
                     p(i)%P_contrdamax,p(i)%P_codeindetermin,p(i)%P_sensanox,p(i)%nplt,p(i)%nrec,          &
                     p(i)%codeinstal,p(i)%nger,p(i)%nsen,p(i)%nlax,p(i)%nflo,p(i)%nmat,              &
                     p(i)%nlev,p(i)%namf,p(i)%izrac,itk(i)%P_profsem,itk(i)%P_codcueille,                &
                     p(i)%deltai(0,n),p(i)%lai(0,n),p(i)%sioncoupe,p(i)%P_sensrsec,t%P_codedyntalle(i),  &
                     p(i)%P_tcxstop,p(i)%dltams(0,sc%n),                                               &
                     sc%nhe,sc%nstoprac,p(i)%zrac,p(i)%znonli,                                       & ! INOUT
                     !!!MODIF HISAFE 4 : suppression dimension temporelle
                     !!!rl(p(i)%nger-1) = rlveille car dans ce cas nger=n (voir code croissanceFrontRacinaire)
                     !!!p(i)%rl(p(i)%nger-1,1:sc%nbCouchesSol),
                     p(i)%rlveille(1:sc%nbCouchesSol), &
                     p(i)%deltaz,p(i)%dtj(1:n),               &
                     p(i)%cumlracz,p(i)%poussracmoy,p(i)%lracsenz(1:sc%nbCouchesSol),p(i)%tempeff,p(i)%efda,p(i)%humirac_mean)

! print *,'avant densiteRacinaire'
              call densiteRacinaire(                                                                         &
                     sc%n,sc%nbCouchesSol,sc%anox(1:sc%nbCouchesSol),sc%hur(1:sc%nbCouchesSol),sc%nstoprac,  &
                     sc%humin(1:sc%nbCouchesSol),sc%dacouche(1:sc%nbCouchesSol),pg%P_daseuilbas,pg%P_daseuilhaut,&
                     pg%P_dacohes,pg%P_lvopt,p(i)%P_coderacine,p(i)%P_contrdamax,p(i)%P_sensanox,p(i)%zrac,            &
                     p(i)%P_zlabour,p(i)%P_zpente,p(i)%P_zprlim,p(i)%znonli,p(i)%difrac,p(i)%lai(0,n),             &
                     sc%tauxcouv(n),p(i)%nrec,p(i)%codeinstal,p(i)%nger,p(i)%rljour(1:sc%nbCouchesSol),        &
                     p(i)%deltaz,p(i)%dtj(1:n),p(i)%nlev,p(i)%cumlracz,p(i)%poussracmoy,                     &
                     p(i)%lracsenz(1:sc%nbCouchesSol),p(i)%cumflrac,p(i)%racnoy(n),                          &
                     p(i)%flrac(1:sc%nbCouchesSol),p(i)%lracz(1:sc%nbCouchesSol),p(i)%cumlraczmaxi,          &
                     p(i)%zracmax,itk(i)%P_profsem,p(i)%P_codazorac,p(i)%P_minazorac,p(i)%P_maxazorac,               &
                     p(i)%P_minefnra,p(i)%P_codtrophrac,t%P_coefracoupe(i),soil%nit(1:sc%nbCouchesSol),            &
                     soil%amm(1:sc%nbCouchesSol),soil%profsol,p(i)%msrac(n),p(i)%nsencourprerac,             &
                     p(i)%somtemprac,p(i)%idzrac,p(i)%efdensite_rac,p(i)%ndebsenrac,                             &
                     p(i)%precrac(1:sc%nbCouchesSol),p(i)%drl(1:n,1:sc%nbCouchesSol),p(i)%lracsentot,        &
                     p(i)%masectot,p(i)%rltot,p(i)%sioncoupe,p(i)%P_sensrsec,                                  &
                     p(i)%P_stlevamf(itk(i)%P_variete),p(i)%P_stamflax(itk(i)%P_variete),p(i)%P_lvfront,               &
                     p(i)%P_laicomp,p(i)%P_adens(itk(i)%P_variete),p(i)%P_bdens,p(i)%P_draclong,p(i)%P_vlaimax,          &
                     p(i)%P_longsperac,p(i)%P_debsenrac,t%P_codedyntalle(i),p(i)%drlsenmortalle,p(i)%densite,      &
                     itk(i)%P_msresiduel(p(i)%numcoupe),p(i)%lai(0,n-1),p(i)%msrac(n-1),                       &
                     p(i)%rlveille(1:sc%nbCouchesSol),p(i)%dltams(0,n),repracmin,repracmax,kreprac,             &
                     p(i)%efda,p(i)%efnrac_mean,p(i)%humirac_mean,p(i)%humirac_z(1:sc%nbCouchesSol),          &
                     p(i)%efnrac_z(1:sc%nbCouchesSol),p(i)%rlj,p(i)%masec(sc%aoas,n), t%P_codemortalracine,   &
                     p(i)%dltmsrac_plante)




!#if DEBUG == 1
!              if (iand(sc%calpsibase,2) >0) call calpsibase_debug_write_input(1281,sc,pg,p,itk,soil,c,sta,t,i,ens)
!#endif

! print *,'avant calpsibase'
            ! NB - le 07/06/2004 - introduction du calcul de psibase
              call calpsibase(p(i)%zrac,sc%nbCouchesSol,sc%hur(1:sc%nbCouchesSol),      &
                              sc%sat(1:sc%nbCouchesSol),sc%humin(1:sc%nbCouchesSol),    &
                              sc%hucc(1:sc%nbCouchesSol),sc%dacouche(1:sc%nbCouchesSol),&
                              pg%P_psihumin,p(i)%lracz(1:sc%nbCouchesSol),                &
                              p(i)%lai(sc%aoas,n),pg%P_psihucc,soil%P_codefente,p(i)%psibase)


!#if DEBUG == 1
!              if (iand(sc%calpsibase,8) >0) call calpsibase_debug_write_output(1283,sc,pg,p,itk,soil,c,sta,t,i,ens)
!#endif
            endif

            if (p(i)%P_codelaitr == 1 .and. p(i)%nlev > 0) then

!#if DEBUG == 1
!              if (iand(sc%repartir,2) >0) call repartir_debug_write_input(1291,sc,pg,p,itk,soil,c,sta,t,i,ens)
!#endif

! print *,'avant repartir'
              call repartir(n,p(i)%nrec,itk(i)%P_codcueille,p(i)%P_codeperenne,p(i)%nlev,p(i)%nlax,itk(i)%P_nbcueille,    &
                            sc%numcult,sc%tustress,p(i)%P_slamin,p(i)%P_slamax,p(i)%P_codlainet,p(i)%P_codemonocot,         &
                            sc%P_codesimul,p(i)%dltaisen(ens),p(i)%P_tigefeuil,p(i)%P_envfruit,p(i)%chargefruit,          &
                            p(i)%ndrp,p(i)%ndebdes,p(i)%P_sea,p(i)%ntaille,itk(i)%P_codetaille,pg%P_codeinitprec,         &
                            p(i)%dltams(ens,n),p(i)%lai(ens,n-1),                                                   &
                            p(i)%resperenne(ens),p(i)%masecveg(ens),p(i)%pdsfruittot(ens),p(i)%tursla(ens),         &
                            p(i)%sla(ens),p(i)%mafeuilverte(ens),p(i)%mafeuil(ens),p(i)%mafeuilp(ens),              &
                            p(i)%lai(ens,n),p(i)%deltai(ens,n),p(i)%maenfruit(ens),p(i)%eai(ens),                   &
                            p(i)%mareserve(ens),p(i)%deltares(ens),p(i)%mabois(ens),p(i)%P_resperenne0,               &
                            p(i)%masec(ens,n),p(i)%msresjaune(ens),p(i)%mafeuiljaune(ens),p(i)%msneojaune(ens),     &
                            p(i)%matigestruc(ens),p(i)%pfeuil(ens),p(i)%pfeuilverte(ens,n),p(i)%pfeuiljaune(ens), &
                            p(i)%ptigestruc(ens),p(i)%penfruit(ens),p(i)%preserve(ens))

            ! on recalcul les sommes pondérées par les surfaces ombre/soleil des variables AO/AS
              p(i)%lai(sc%aoas,n) = p(i)%lai(sc%as,n) * p(i)%surf(sc%as)                  &
                                  + p(i)%lai(sc%ao,n) * p(i)%surf(sc%ao)
              p(i)%resperenne(sc%aoas) = p(i)%resperenne(sc%ao) * p(i)%surf(sc%ao)        &
                                       + p(i)%resperenne(sc%as) * p(i)%surf(sc%as)
              p(i)%masecveg(sc%aoas) = p(i)%masecveg(sc%ao) * p(i)%surf(sc%ao)            &
                                     + p(i)%masecveg(sc%as) * p(i)%surf(sc%as)
              p(i)%pdsfruittot(sc%aoas) = p(i)%pdsfruittot(sc%ao) * p(i)%surf(sc%ao)      &
                                        + p(i)%pdsfruittot(sc%as) * p(i)%surf(sc%as)
              p(i)%tursla(sc%aoas) = p(i)%tursla(sc%ao) * p(i)%surf(sc%ao)                &
                                   + p(i)%tursla(sc%as) * p(i)%surf(sc%as)
              p(i)%sla(sc%aoas) = p(i)%sla(sc%ao) * p(i)%surf(sc%ao)                      &
                                + p(i)%sla(sc%as) * p(i)%surf(sc%as)
              p(i)%mafeuilverte(sc%aoas) = p(i)%mafeuilverte(sc%ao) * p(i)%surf(sc%ao)    &
                                         + p(i)%mafeuilverte(sc%as) * p(i)%surf(sc%as)
              p(i)%mafeuil(sc%aoas) = p(i)%mafeuil(sc%ao) * p(i)%surf(sc%ao)              &
                                    + p(i)%mafeuil(sc%as) * p(i)%surf(sc%as)
              p(i)%mafeuilp(sc%aoas) = p(i)%mafeuilp(sc%ao) * p(i)%surf(sc%ao)            &
                                     + p(i)%mafeuilp(sc%as) * p(i)%surf(sc%as)
              p(i)%deltai(sc%aoas,sc%n) = p(i)%deltai(sc%ao,sc%n) * p(i)%surf(sc%ao)      &
                                        + p(i)%deltai(sc%as,sc%n) * p(i)%surf(sc%as)
              p(i)%maenfruit(sc%aoas) = p(i)%maenfruit(sc%ao) * p(i)%surf(sc%ao)          &
                                      + p(i)%maenfruit(sc%as) * p(i)%surf(sc%as)
              p(i)%eai(sc%aoas) = p(i)%eai(sc%ao) * p(i)%surf(sc%ao)                      &
                                + p(i)%eai(sc%as) * p(i)%surf(sc%as)
              p(i)%mareserve(sc%aoas) = p(i)%mareserve(sc%ao) * p(i)%surf(sc%ao)          &
                                      + p(i)%mareserve(sc%as) * p(i)%surf(sc%as)
              p(i)%deltares(sc%aoas) = p(i)%deltares(sc%ao) * p(i)%surf(sc%ao)            &
                                     + p(i)%deltares(sc%as) * p(i)%surf(sc%as)
              p(i)%mabois(sc%aoas) = p(i)%mabois(sc%ao) * p(i)%surf(sc%ao)                &
                                   + p(i)%mabois(sc%as) * p(i)%surf(sc%as)
              p(i)%masec(sc%aoas,sc%n) = p(i)%masec(sc%ao,sc%n) * p(i)%surf(sc%ao)        &
                                       + p(i)%masec(sc%as,sc%n) * p(i)%surf(sc%as)
              p(i)%msresjaune(sc%aoas) = p(i)%msresjaune(sc%ao) * p(i)%surf(sc%ao)        &
                                       + p(i)%msresjaune(sc%as) * p(i)%surf(sc%as)
              p(i)%mafeuiljaune(sc%aoas) = p(i)%mafeuiljaune(sc%ao) * p(i)%surf(sc%ao)    &
                                         + p(i)%mafeuiljaune(sc%as) * p(i)%surf(sc%as)
! DR et Fr 15/04/2016 on a enleve les cumuls dans cimAOAS avce les deltas (jour - veille) et on les fait la
              p(i)%msneojaune(sc%aoas) = p(i)%msneojaune(sc%ao) * p(i)%surf(sc%ao)        &
                                       + p(i)%msneojaune(sc%as) * p(i)%surf(sc%as)
              p(i)%masecneo(sc%aoas)   = p(i)%masecneo(sc%ao) * p(i)%surf(sc%ao)        &
                                       + p(i)%masecneo(sc%as) * p(i)%surf(sc%as)
              p(i)%msres(sc%aoas)   = p(i)%msres(sc%ao) * p(i)%surf(sc%ao)        &
                                       + p(i)%msres(sc%as) * p(i)%surf(sc%as)
              p(i)%mafeuiltombe(sc%aoas)   = p(i)%mafeuiltombe(sc%ao) * p(i)%surf(sc%ao)    &
                                       + p(i)%mafeuiltombe(sc%as) * p(i)%surf(sc%as)
!

              p(i)%matigestruc(sc%aoas) = p(i)%matigestruc(sc%ao) * p(i)%surf(sc%ao)      &
                                        + p(i)%matigestruc(sc%as) * p(i)%surf(sc%as)


              if (p(i)%masec(sc%aoas,sc%n) > 0.) then
                p(i)%ptigestruc(sc%aoas)       = p(i)%matigestruc(sc%aoas)  / p(i)%masec(sc%aoas,sc%n)
                p(i)%penfruit(sc%aoas)         = p(i)%maenfruit(sc%aoas)    / p(i)%masec(sc%aoas,sc%n)
                p(i)%preserve(sc%aoas)         = p(i)%resperenne(sc%aoas)   / p(i)%masec(sc%aoas,sc%n)
                p(i)%pfeuil(sc%aoas)           = p(i)%mafeuil(sc%aoas)      / p(i)%masec(sc%aoas,sc%n)
                p(i)%pfeuiljaune(sc%aoas)      = p(i)%mafeuiljaune(sc%aoas) / p(i)%masec(sc%aoas,sc%n)
                p(i)%pfeuilverte(sc%aoas,sc%n) = p(i)%mafeuilverte(sc%aoas) / p(i)%masec(sc%aoas,sc%n)
              else
                p(i)%ptigestruc(sc%aoas)       = 0.
                p(i)%penfruit(sc%aoas)         = 0.
                p(i)%preserve(sc%aoas)         = 0.
                p(i)%pfeuil(sc%aoas)           = 0.
                p(i)%pfeuiljaune(sc%aoas)      = 0.
                p(i)%pfeuilverte(sc%aoas,sc%n) = 0.
              endif

!#if DEBUG == 1
!              if (iand(sc%repartir,8) >0) call repartir_debug_write_output(1293,sc,pg,p,itk,soil,c,sta,t,i,ens)
!#endif

            endif
          endif
        end do
      end do

return
end subroutine croissance
 
 
