! *********************************************************
!   sous-programme de calcul du taux de recouvrement
!   version 5.0
!   N.Brisson, P.Bertuzzi
!   dernieres modifs le 13/06/2001
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> calculation of the ground cover
!> - Stics book paragraphe3.1.4, page 47-48
!>
!!This module allows the use of ground cover instead of the leaf area index.
!! Given the complexity and the numerous parameters required for LAI calculation, de Tourdonnet (1999) proposed a simple alternative by the direct calculation
!! of the ground cover, which can be used as a state variable in calculations for radiation interception and water requirements.  This can be particularly useful
!! for plants with a complex foliage structure such as lettuce, or for a first modelling approach. It is programmed in STICS as an alternative option to all
!! the previous calculations. This formalisation is particularly interesting when leaves have complex spatial arrangement or when the individual plant foliage
!! is abundant.
!!
!! To calculate the ground cover (tauxcouv), a temporal scale similar to that of LAI is used, and called ULAI; this varies from 0 to 2, depending on the
!! phenological time.  At the IAMF stage, ULAI is equal to infrecouv. tauxcouv is calculated by a logistic curve, using trecouvmax as the asymptote, which
!! represents the proportion of the soil covered by an isolated plant, infrecouv as the abscissa of the inflexion point and pentrecouv as the slope at the
!! inflexion point. The competitive effect linked to population growth is simulated in the same way as for the leaf area index and uses the same parameters,
!! adens, bdens and laicomp (expressed as ground cover). In this case, laiplantule is the ground cover of plants at planting if the crop is  transplanted
!! rather than sown. Water and nitrogen shortage and waterlogging stresses are applied to the rate of growth of ground cover, and the method of combining
!! stresses is the same as for the leaf area index.
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine TauxRecouvrement(sc,pg,p,itk)

USE Stics
USE Plante
USE Parametres_Generaux
USE Itineraire_Technique

    implicit none

    type(Stics_Communs_),       intent(INOUT) :: sc  

    type(Parametres_Generaux_), intent(IN)    :: pg  

    type(Plante_),              intent(INOUT) :: p  

    type(ITK_),                 intent(IN)    :: itk  


    call TauxRecouvrement_(sc%n,pg%P_codeinnact,pg%P_codeh2oact,p%turfac(sc%ens),p%innlai(sc%ens),          &
                           p%nlev,p%P_codeperenne,p%nplt,p%ndrp,p%P_codehypo,p%P_codegermin,                &
                           p%P_laiplantule,p%namf,p%nrec,itk%P_codcueille,p%nlax,p%nsen,                    &
                           p%upvt,p%upvtutil,p%P_infrecouv,p%densite,p%somcour,                       &
                           p%P_stamflax(itk%P_variete),p%P_stlevamf(itk%P_variete),p%P_codeindetermin,      &
                           p%P_laicomp,p%P_adens(itk%P_variete),p%P_bdens,p%P_tauxrecouvmax,p%P_pentrecouv, &
                           p%exolai,p%P_stsenlan(itk%P_variete),p%ulai(sc%n-1),sc%tauxcouv(p%nsen),         &
                           p%P_phyllotherme,p%udevcult,                                                     &
                           sc%tustress,sc%tauxcouv(sc%n),sc%tauxcouv(sc%n-1),p%somfeuille,                  &
                           p%nbfeuille,p%reajust,sc%fdens,p%ulai(sc%n),p%efdensite,p%nlan,                  &
                           sc%tauxcouv(p%nlan))

end subroutine TauxRecouvrement


subroutine TauxRecouvrement_(n,P_codeinnact,P_codeh2oact,turfac,innlai,nlev,P_codeperenne,nplt,ndrp,        & ! IN
                             P_codehypo,P_codegermin,P_laiplantule,namf,nrec,P_codcueille,nlax,nsen,        &
                             upvt,upvtutil,P_infrecouv,densite,somcour,P_stamflax,P_stlevamf,               &
                             P_codeindetermin,P_laicomp,P_adens,P_bdens,P_tauxrecouvmax,P_pentrecouv,       &
                             exolai,P_stsenlan,ulai_veille,tauxcouv_nsen,P_phyllotherme,udevcult,           &
                             tustress,tauxcouv,tauxcouv_veille,somfeuille,nbfeuille,reajust,                & ! INOUT
                             fdens,ulai,efdensite,nlan,tauxcouv_nlan)

    implicit none

    integer, intent(IN) :: n  
    integer, intent(IN) :: P_codeinnact  !> // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0 
    integer, intent(IN) :: P_codeh2oact  !> // PARAMETER // code to activate  water stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0 
    real,    intent(IN) :: turfac   !> // OUTPUT // Index of turgescence water stress  // 0-1
    real,    intent(IN) :: innlai   !> // OUTPUT // Index of nitrogen stress active on leaf growth // P_innmin to 1
    integer, intent(IN) :: nlev  
    integer, intent(IN) :: P_codeperenne  !> // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0 
    integer, intent(IN) :: nplt  
    integer, intent(IN) :: ndrp  
    integer, intent(IN) :: P_codehypo  !> // PARAMETER // option of simulation of a  phase of hypocotyl growth (1) or planting of plantlets (2) // code 1/2 // PARPLT // 0 
    integer, intent(IN) :: P_codegermin  !> // PARAMETER // option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2) // code 1/2 // PARPLT // 0 
    real,    intent(IN) :: P_laiplantule  !> // PARAMETER // Plantlet Leaf index at the plantation // m2 leaf  m-2 soil // PARPLT // 1 
    integer, intent(IN) :: namf  
    integer, intent(IN) :: nrec  
    integer, intent(IN) :: P_codcueille  !> // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0 
    integer, intent(IN) :: nlax  
    integer, intent(IN) :: nsen  
    real,    intent(IN) :: upvt   !> // OUTPUT // Daily development unit  // degree.days
    real,    intent(IN) :: upvtutil  
    real,    intent(IN) :: P_infrecouv  !> // PARAMETER // ulai at the stage AMF (inflection point of the soil cover rate increase) // SD // PARPLT // 1
    real,    intent(IN) :: densite   !> // OUTPUT // Actual sowing density // plants.m-2
    real,    intent(IN) :: somcour   !> // OUTPUT // Cumulated units of development between two stages // degree.days
    real,    intent(IN) :: P_stamflax  !> // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1 
    real,    intent(IN) :: P_stlevamf  !> // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1 
    integer, intent(IN) :: P_codeindetermin  !> // PARAMETER // option of  simulation of the leaf growth and fruit growth : indeterminate (2) or determinate (1) // code 1/2 // PARPLT // 0 
    real,    intent(IN) :: P_laicomp  !> // PARAMETER // LAI from which starts competition inbetween plants // m2 m-2 // PARPLT // 1 
    real,    intent(IN) :: P_adens  !> // PARAMETER // Interplant competition parameter // SD // PARPLT // 1 
    real,    intent(IN) :: P_bdens  !> // PARAMETER // minimal density from which interplant competition starts // plants m-2 // PARPLT // 1 
    real,    intent(IN) :: P_tauxrecouvmax  !> // PARAMETER // maximal soil cover rate // m2 plante m-2 sol // PARPLT // 1 
    real,    intent(IN) :: P_pentrecouv  !> // PARAMETER // parameter of the logistic curve of the soil cover rate increase // * // PARPLT // 1 
    real,    intent(IN) :: exolai   !> // OUTPUT // Index for excess water active on growth in biomass // 0-1
    real,    intent(IN) :: P_stsenlan  !> // PARAMETER // Sum of development units between the stages SEN et LAN // degree.days // PARPLT // 1 
    real,    intent(IN) :: ulai_veille  
    real,    intent(IN) :: tauxcouv_nsen  
    real,    intent(IN) :: P_phyllotherme  !> // PARAMETER // thermal duration between the apparition of two successive leaves on the main stem // degree C day // PARPLT // 1
    real,    intent(IN) :: udevcult   !> // OUTPUT // Effective temperature for the development, computed with TCULT // degree.days

! INOUT
    real,    intent(INOUT) :: tustress   !> // OUTPUT // Stress index active on leaf growth (= minimum(turfac,innlai))  // 0-1
    real,    intent(INOUT) :: tauxcouv   !> // OUTPUT // Cover rate // SD
    real,    intent(INOUT) :: tauxcouv_veille  
    real,    intent(INOUT) :: somfeuille  
    integer, intent(INOUT) :: nbfeuille   !> // OUTPUT // Number of leaves on main stem // SD
    real,    intent(INOUT) :: reajust  
    real,    intent(INOUT) :: fdens  
    real,    intent(INOUT) :: ulai   !> // OUTPUT // Daily relative development unit for LAI // 0-3
    real,    intent(INOUT) :: efdensite  
    integer, intent(INOUT) :: nlan  
    real,    intent(INOUT) :: tauxcouv_nlan  



! Variables locales
    real :: infrecouv1  !>  
    real :: ar  !>  
    real :: br  !>  
    real :: abr  !>  
    real :: dulai  !>  
    real :: dtauxrec  




    ! 1. CALCUL DU FACTEUR STRESS EAU ET N
      if (P_codeinnact == 1 .and. P_codeh2oact == 1) then
        tustress = min(turfac,innlai)
      endif

      if (P_codeinnact == 2 .and. P_codeh2oact == 1) then
        tustress = min(turfac,1.)
      endif

      if (P_codeinnact == 1 .and. P_codeh2oact == 2) then
        tustress = min(1.,innlai)
      endif

      if (P_codeinnact == 2 .and. P_codeh2oact == 2) tustress=1.


    ! 2. INITIALISATIONS

! ** AVANT LE STADE NLEV (LEVEE):
! *- dans le cas des cultures annuelles, deux cas:
! *- a) pour les cultures qui se plantent et admettent un
! *-    temps de latence avant le demarrage de la croissance:
! *-    initialisation du tauxcouv a P_laiplantule a partir de la plantation
! *- b) pour les cultures qui se sement:
! *-    initialisation du tauxcouv a 0 a partir du semis
! *- c) pour les cultures qui se plantent et demarrent directement:
! *-    initialisation de tauxcouv a P_laiplantule a partir de la levee
! *-    qui est aussi la plantation
! *- NB et ML - le 23/02/2004

      if (nlev == 0) then
! --        if (P_codeperenne == 1 .and. nplt > 0) then

    ! nplt est gere differemment de nger, nlev, namf, etc.
    ! il est non nul des le depart de la simulation:
    ! on remplace donc le test precedent (nplt > 0)
    ! par le test (n >= nplt);
    ! ML - le 24/02/04
        if (P_codeperenne == 1 .and. n >= nplt) then
          if (P_codehypo == 2 .and. P_codegermin == 1) then
            tauxcouv = P_laiplantule
            return
          else
            tauxcouv = 0.
            return
          endif
        else
          tauxcouv = 0.
          return
        ! PB - 29/03/2004 - somfeuille apres return ???
          somfeuille = 0.
        endif
      endif

      if (n == nlev .and. P_codehypo == 2 .and. namf == 0) then
        tauxcouv_veille = P_laiplantule
      endif

    ! à la récolte (nrec+1)
      if (nrec > 0 .and. n > nrec) then
    ! EN CAS DE MOISSON, ON N'ENLEVE LES FEUILLES
        if (P_codcueille == 1) then
          tauxcouv = 0.
          somfeuille = 0.
          return
        endif
      endif

    ! domi - 20/12/00
    ! calcul du nombre de feuilles
    ! si code - NB - le 23/03
    ! --  if (codenbfeuil == 2) call feuille

    ! NB - le 03/12/01
      call CalculNombreDeFeuilles(P_phyllotherme,nlax,udevcult,somfeuille,nbfeuille)

    ! 2a) calcul de reajust : distance de développement pour reajuster
    !     les parcours observés
      if (n == nlev) reajust = 0.
      if (n == namf) reajust = 0.
      if (n == nlax) reajust = 0.
      if (n == nsen) reajust = 0.
      if (n == nlan) reajust = 0.
      reajust = reajust + upvt - upvtutil


! ** 4a) calcul du facteur densité fdens qui joue sur la
! *-     position du point d inflexion
      if (n == nlev) then
        fdens = -log(P_infrecouv/5.) * 1. / 14.
        infrecouv1 = 5. * exp(-fdens * densite)
      endif


    ! 3. CALCUL DE ULAI


    ! 3a) calcul de ulai entre lev et amf
      if (nlev > 0 .and. namf == 0) then
        ulai = P_infrecouv * (somcour + reajust)       &
             / (P_stlevamf + reajust)
      endif

    ! 3b) calcul de ulai entre amf et lax
      if (namf > 0 .and. nlax == 0) then
        ulai = P_infrecouv + (2. - P_infrecouv)        &
             * (somcour + reajust)                 &
             / (P_stamflax + reajust)
      endif

    ! NB - le 13/12/01
      if (n >= nlax .and. nlax > 0 .and. nsen == 0) then
        ulai = 2.
      endif

    ! 3c) ulai maximal atteint à drp pour les indéterminées
      if (P_codeindetermin == 2 .and. ndrp > 0) ulai = 2

    ! entre ulai=0 (lev)  et ulai=2. (lax), tauxcouv=logistique
      if (ulai < 2.) then

! ** ancienne formulation avant 01/01/02
! --  fdens = -log(P_infrecouv/5.)*1./14.
! --  infrecouv1 = 5.*exp(-fdens*densite)
! --  tauxcouv = P_laiplantule+P_tauxrecouvmax / (1+exp(P_pentrecouv*(infrecouv1-ulai)))

        ! calcul de l'effet densité sur le taux de recouvrement
        efdensite = 1.

        ! NB le 22/08/07 bug n-1 au lieu de n car n'était pas calculé
        if (tauxcouv_veille < P_laicomp) then
          efdensite = 1.
        else
          if (densite == 0)then
            efdensite = 0.
          else
            efdensite = min( exp(P_adens * (log(densite/P_bdens))), 1. )
          endif
        endif
        tauxcouv = P_laiplantule + efdensite * densite    &
                       * (P_tauxrecouvmax / (1 + exp(P_pentrecouv * (P_infrecouv - ulai))))

        ! calcul de la dérivée de la logistique
        ar = P_pentrecouv
        br = P_infrecouv
        abr = ar * br
        dulai = ulai - ulai_veille
        dtauxrec = ar * dulai * exp(ar * ulai + abr)       &
                 / (exp(ar * ulai) + exp(abr))**2

        if (ulai == 0.) then
          dtauxrec = 1 / (1 + exp(P_pentrecouv * P_infrecouv))
        endif

        dtauxrec = dtauxrec * efdensite * densite * P_tauxrecouvmax

        tauxcouv = tauxcouv_veille + dtauxrec * tustress * exolai
        tauxcouv = min(tauxcouv,1.)

      else
    ! plateau entre lax et sen
        tauxcouv = tauxcouv_veille
      endif
! --   endif

! Effet des stress
! ajout du stress exces d'eau (NB le 7/1/02)
! --deltatauxcouv = (tauxcouv-tauxcouv_veille)*tustress*exolai
! --if (n == nlev)deltatauxcouv = deltatauxcouv+tauxcouv_veille
! --deltatauxcouv = amax1(0.0,deltatauxcouv)
! --tauxcouv = tauxcouv_veille+deltatauxcouv


    ! 6. Calcul de la senescence rapide par ajustement a la recolte
    !    entre sen et lan  pour les options 1 et 2

      if (n == nsen) tauxcouv = tauxcouv_veille

      if (nsen > 0 .and. nlan == 0.) then
        tauxcouv = tauxcouv_nsen * (1 - (somcour + reajust) / (P_stsenlan + reajust))
        if (tauxcouv <= 0 .and. nlan == 0)  nlan = n
      endif

      if (n == nlan) tauxcouv_nlan = 0.

return
end subroutine TauxRecouvrement_
