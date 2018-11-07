!    *****************************
!     calcul de l'indice foliaire
!           N.Brisson, R. Roche
!    *****************************
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> calculation of the LAI
!> - Stics book paragraphe 3.1.1, page 40-44
!>
!! In STICS, leaf area growth is driven by phasic development, temperature and stresses. An empirical plant density-dependent function represents inter-plant
!! competition. For indeterminate plants, trophic competition is taken into account through a trophic stress index, while for determinate plants a maximal
!! expansion rate threshold is calculated to avoid unrealistic leaf expansion.
!! The calculation of leaf growth rate (deltai in m2m-2 d-1) is broken down: a first calculation of the LAI growth rate (in m2 plant-1 degree-day-1) describes
!! a logistic curve, related to the ILEV, IAMF and ILAX phenological stages. This value is then multiplied by the effective crop temperature, the plant density
!! combined with a density factor, supposed to stand for inter-plant competition, that is characteristic for the variety, and the water and nitrogen stress indices.
!!
!! The phasic development function is comparable to that of the model PUTU (Singels and Jagger, 1991), i.e. a logistic function with dlaimaxbrut as asymptote
!! and pentlaimax as the slope at the inflection point. It is driven by a normalized leaf development unit (ULAI) equal to 1 at ILEV and 3 at ILAX.
!! At the end of the juvenile stage (IAMF), it is equal to vlaimax when the inflection of the dynamics (point of maximal rate) also occurs.
!! Between the stages ILEV, IAMF and ILAX, the model performs linear interpolation based on development units (upvt) which include all the environmental effects
!! on phasic development. As the ILAX stage approaches, it is possible to introduce a gradual decline in growth rate using the udlaimax parameter
!!- the ULAI value beyond which there is a decline in the leaf growth rate. If udlaimax=3 it has no effect and the leaf stops growing at once at ILAX.
!!
!! The thermal function relies on crop temperature and cardinal temperatures (tcmin and tcmax) which differ from the ones used for the phasic development.
!! The extreme threshold tcxstop is the same as for development.
!!
!! The density function is active solely after a given LAI threshold occurs (laicomp parameter) if the plant density (densite in plant m-2 possibly decreased
!! by early frost) is greater than the bdens threshold, below which plant leaf area is assumed independent of density.  Beyond this density value, leaf area
!! per plant decreases exponentially.  The adens parameter represents the ability of a plant to withstand increasing densities.  It depends on the species
!! and may depend on the variety.  For branching or tillering plants, adens represents the plant’s branching or tillering ability (e. g. wheat or pea).
!! For single-stem plants, adens represents competition between plant leaves within a given stand (e.g. maize or sunflower).
!!
!! Water and nitrogen affect leaf growth as limiting factors, i.e. stress indices whose values vary between 0 and 1. Water (turfac) and nitrogen deficits (innlai)
!! are assumed to interact, justifying the use of the more severe of the two stresses. Meanwhile at the whole plant level the water-logging stress index is assumed
!! to act independently
!!
!!
!!
!> -    Features of determinate crops
!!   Failure to account for trophic aspects in the calculation of leaf growth may cause problems when the radiation intercepted by the crop is insufficient to
!!   ensure leaf expansion (e.g. for crops under a tree canopy or crops growing in winter).  Consequently, from the IAMF stage, we have introduced a trophic effect
!!  to calculate the definitive LAI growth rate in the form of a maximum threshold for leaf expansion (deltaimaxi in m2m-2d-1) using the notion of the maximum
!!   leaf expansion allowed per unit of biomass accumulated in the plant (sbvmax in cm2 g-1) and the daily biomass accumulation (dltams in t.ha-1day-1 possibly
!!   complemented by remobilized reserve remobilj). sbvmax is calculated using the slamax and tigefeuil parameters.
!!
!> -    Features of indeterminate crops
!!   It has been possible to test the robustness of the above formalisation on a variety of crops, including crops where there is an overlap between the
!!   vegetative phase and the reproductive phase (soybean and flax for example).  However, when trophic competition between leaves and fruits is a driving force
!!   for the production and management of the crop (for example tomato, sugarbeet), this formalisation is unsuitable.  We therefore calculate the deltai variable
!!   so as to take trophic monitoring into consideration in the case of crops described as ‘indeterminate’, by introducing a trophic stress index (splai).
!!   As a consequence, the LAI can decrease markedly during the theoretical growth phase if the crop is experiencing severe stresses during the harvested
!!   organs filling phase.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
!subroutine calai(sc,pg,p,itk,t)  !DR 19/07/2012 t n'est pas utile car codedlaimin a ete supprimé
subroutine calai(sc,pg,p,itk)
USE Stics
USE Plante
USE Itineraire_Technique
USE Parametres_Generaux

implicit none

!: Arguments
    type(Stics_Communs_),           intent(INOUT) :: sc  

    type(Parametres_Generaux_),     intent(INOUT) :: pg  

    type(Plante_),                  intent(INOUT) :: p  

    type(ITK_),                     intent(INOUT) :: itk  

!    type(Stics_Transit_),           intent(INOUT) :: t

!: variables locales
    integer :: ens  !>  
    integer ::  n  



    !pour faciliter l'écriture
    ens = sc%ens
    n = sc%n



!write(618,*)'jour',n,'ens',ens

    call calai_(pg%P_codeinnact,pg%P_codeh2oact,p%P_codehypo,p%P_codegermin,itk%P_codcueille, & ! IN
                p%P_codlainet,pg%codeulaivernal,p%P_codeindetermin,                           &
                pg%P_codeinitprec,p%P_codeperenne,p%P_codetemp,p%turfac(ens),                 &
                p%innlai(ens),p%P_laiplantule,p%P_phyllotherme,p%udevair,p%udevcult,          &
                p%P_dlaimaxbrut,p%P_udlaimax,sc%n,sc%numcult,sc%nbjmax,p%nplt,p%nlev,         &
                p%namf,p%nlax,p%ndrp,p%nrec,p%upvt,p%upvtutil,p%P_vlaimax,                 &
                p%P_laicomp,p%somcour,p%somcourutp,p%P_stlevamf(itk%P_variete),               &
                p%P_stamflax(itk%P_variete),p%densite,p%P_adens(itk%P_variete),p%P_bdens,     &
                p%P_tcxstop,sc%tcult,p%P_tcmin,p%P_tcmax,p%P_pentlaimax,p%P_dlaimin,          &
                p%exolai,p%sourcepuits(ens),p%P_splaimin,p%P_splaimax,p%P_slamin,p%P_slamax,  &
                p%P_tigefeuil,p%P_tustressmin,p%fstressgel,p%dltamsen(ens),p%sla(ens),        &
                p%remobilj(ens),p%dltams(ens,n-1),                                            &
                sc%tustress,p%efdensite,p%nstopfeuille,p%deltai(ens,n),p%splai(ens),          & ! OUT & INOUT
                p%fpv(ens),p%P_stlaxsen(itk%P_variete),p%tempeff,p%lai(ens,:),              &
                p%somfeuille,p%nbfeuille,p%nsen,p%nlan,p%P_dlaimax,p%reajust,p%ulai,          &
                sc%vmax,p%dltaisenat(ens),p%laisen(ens,:),p%dltaisen(ens),                    &
                p%P_stsenlan(itk%P_variete),p%densiteequiv)
!                p%P_codlainet,pg%codeulaivernal,p%P_codeindetermin,t%P_codedlaimin,          &

!: cumuls AO/AS


    p%splai(sc%AOAS) = p%splai(sc%AS) * p%surf(sc%AS) + p%splai(sc%AO) * p%surf(sc%AO)
    p%fpv(sc%AOAS) = p%fpv(sc%AS) * p%surf(sc%AS)+ p%fpv(sc%AO) * p%surf(sc%AO)

    !p%lai(sc%AOAS,n) = p%lai(sc%AS,n) * p%surf(sc%AS) + p%lai(sc%AO,n) * p%surf(sc%AO)
    ! remplacé par : (extrait de cumAOetAS.f90)

    !!!MODIF HISAFE 1 : suppression des chaines de caractères
    !!!if (lge(sc%P_codesimul,'feuille') .eqv. .FALSE.) then
    if (sc%P_codesimul == 1)  then
        if (p%P_codelaitr == 1) then
          p%lai(sc%AOAS,n) = p%lai(sc%AS,n) * p%surf(sc%AS) + p%lai(sc%AO,n) * p%surf(sc%AO)
        endif

        p%deltai(sc%AOAS,n) = p%deltai(sc%AS,n) * p%surf(sc%AS) + p%deltai(sc%AO,n) * p%surf(sc%AO)

        p%laisen(sc%AOAS,n) = p%laisen(sc%AOAS,n-1)                                             &
                            + (p%laisen(sc%AO,n) - p%laisen(sc%AO,n-1)) * p%surf(sc%AO)         &
                            + (p%laisen(sc%AS,n) - p%laisen(sc%AS,n-1)) * p%surf(sc%AS)

      else
        p%lai(sc%AOAS,n)    = p%lai(sc%AS,n)
        p%laisen(sc%AOAS,n) = p%laisen(sc%AS,n)
        p%deltai(sc%AOAS,n) = max(p%lai(sc%AS,n) - p%lai(sc%AS,n-1),0.0)
      endif

      p%dltaisen(sc%AOAS) = p%dltaisen(sc%AS) * p%surf(sc%AS) + p%dltaisen(sc%AO) * p%surf(sc%AO)

    ! pas de cumul AO/AS pour tempeff
    ! pas de cumul AO/AS pour dltaisenat


return
end subroutine calai

subroutine calai_(P_codeinnact,P_codeh2oact,P_codehypo,P_codegermin,P_codcueille,P_codlainet,codeulaivernal,      & !IN
                  P_codeindetermin,P_codeinitprec,P_codeperenne,P_codetemp,turfac,innlai,                         &
                  P_laiplantule,P_phyllotherme,udevair,udevcult,P_dlaimaxbrut,P_udlaimax,n,numcult,nbjmax,        &
                  nplt,nlev,namf,nlax,ndrp,nrec,upvt,upvtutil,P_vlaimax,P_laicomp,somcour,somcourutp,             &
                  P_stlevamf,P_stamflax,densite,P_adens,P_bdens,P_tcxstop,tcult,P_tcmin,P_tcmax,P_pentlaimax,     &
                  P_dlaimin,exolai,sourcepuits,P_splaimin,P_splaimax,P_slamin,P_slamax,P_tigefeuil,P_tustressmin, &
                  fstressgel,dltamsen,sla,remobilj,dltams,                                                        & !IN
                  tustress,efdensite,nstopfeuille,deltai,splai,fpv,P_stlaxsen,tempeff,                            & !OUT
                  lai,somfeuille,nbfeuille,nsen,nlan,P_dlaimax,reajust,ulai,vmax,dltaisenat,laisen,               & !INOUT
                  dltaisen,P_stsenlan,densiteequiv)
!                  P_codeindetermin,P_codedlaimin,P_codeinitprec,P_codeperenne,P_codetemp,turfac,innlai,          &

implicit none

integer, intent(IN)    :: nbjmax !> la taille des tableaux journaliers  

integer, intent(IN)    :: P_codeinnact  !> // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0 
integer, intent(IN)    :: P_codeh2oact  !> // PARAMETER // code to activate  water stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0 
integer, intent(IN)    :: P_codehypo  !> // PARAMETER // option of simulation of a  phase of hypocotyl growth (1) or planting of plantlets (2) // code 1/2 // PARPLT // 0 
integer, intent(IN)    :: P_codegermin  !> // PARAMETER // option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2) // code 1/2 // PARPLT // 0 
integer, intent(IN)    :: P_codcueille  !> // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0 
integer, intent(IN)    :: P_codlainet  !> // PARAMETER // option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
integer, intent(IN)    :: codeulaivernal  
integer, intent(IN)    :: P_codeindetermin  !> // PARAMETER // option of  simulation of the leaf growth and fruit growth : indeterminate (2) or determinate (1) // code 1/2 // PARPLT // 0 
!integer, intent(IN)    :: P_codedlaimin
integer, intent(IN)    :: P_codeinitprec  !> // PARAMETER // reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0 
integer, intent(IN)    :: P_codeperenne  !> // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0 
integer, intent(IN)    :: P_codetemp  !> // PARAMETER // option calculation mode of heat time for the plant : with air temperature (1)  or crop temperature (2) // code 1/2 // PARPLT // 0 
real,    intent(IN)    :: turfac   !> // OUTPUT // Index of turgescence water stress  // 0-1
real,    intent(IN)    :: innlai   !> // OUTPUT // Index of nitrogen stress active on leaf growth // P_innmin à 1
real,    intent(IN)    :: P_laiplantule  !> // PARAMETER // Plantlet Leaf index at the plantation // m2 leaf  m-2 soil // PARPLT // 1 
real,    intent(IN)    :: P_phyllotherme  !> // PARAMETER // thermal duration between the apparition of two successive leaves on the main stem // degree C day // PARPLT // 1
real,    intent(IN)    :: udevair   !> // OUTPUT // Effective temperature for the development, computed with TAIR // degree.days
real,    intent(IN)    :: udevcult   !> // OUTPUT // Effective temperature for the development, computed with TCULT // degree.days
real,    intent(IN)    :: P_dlaimaxbrut  !> // PARAMETER // Maximum rate of the setting up of LAI // m2 leaf plant-1 degree d-1 // PARPLT // 1 
real,    intent(IN)    :: P_udlaimax  !> // PARAMETER // ulai from which the rate of leaf growth decreases  // SD // PARPLT // 1 
integer, intent(IN)    :: n  
integer, intent(IN)    :: numcult  
integer, intent(IN)    :: nplt  
integer, intent(IN)    :: nlev  
integer, intent(IN)    :: namf  
integer, intent(IN)    :: nlax  
integer, intent(IN)    :: ndrp  
integer, intent(IN)    :: nrec  
real,    intent(IN)    :: upvt   !> // OUTPUT // Daily development unit  // degree.days
real,    intent(IN)    :: upvtutil  
real,    intent(IN)    :: P_vlaimax  !> // PARAMETER // ULAI  at inflection point of the function DELTAI=f(ULAI) // SD // PARPLT // 1
real,    intent(IN)    :: P_laicomp  !> // PARAMETER // LAI from which starts competition inbetween plants // m2 m-2 // PARPLT // 1 
real,    intent(IN)    :: somcour   !> // OUTPUT // Cumulated units of development between two stages // degree.days
real,    intent(IN)    :: somcourutp  
real,    intent(IN)    :: P_stlevamf  !> // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1 
real,    intent(IN)    :: P_stamflax  !> // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1 
real,    intent(IN)    :: densite   !> // OUTPUT // Actual sowing density // plants.m-2
real,    intent(IN)    :: P_adens  !> // PARAMETER // Interplant competition parameter // SD // PARPLT // 1 
real,    intent(IN)    :: P_bdens  !> // PARAMETER // minimal density from which interplant competition starts // plants m-2 // PARPLT // 1 
real,    intent(IN)    :: P_tcxstop  !> // PARAMETER // threshold temperature beyond which the foliar growth stops // degree C // PARPLT // 1
real,    intent(IN)    :: tcult   !> // OUTPUT // Crop surface temperature (daily average) // degree C
real,    intent(IN)    :: P_tcmin  !> // PARAMETER // Minimum temperature of growth // degree C // PARPLT // 1
real,    intent(IN)    :: P_tcmax  !> // PARAMETER // Maximum temperature of growth // degree C // PARPLT // 1
real,    intent(IN)    :: P_pentlaimax  !> // PARAMETER // parameter of the logistic curve of LAI growth  // SD // PARPLT // 1 
real,    intent(IN)    :: P_dlaimin  !> // PARAMETER // accelerating parameter for the lai growth rate // SD // PARAMV6/PLT // 1 
real,    intent(IN)    :: exolai   !> // OUTPUT // Index for excess water active on growth in biomass // 0-1
real,    intent(IN)    :: sourcepuits   !> // OUTPUT // Pool/sink ratio // 0-1
real,    intent(IN)    :: P_splaimin  !> // PARAMETER // Minimal value of ratio sources/sinks for the leaf growth  // between 0 and 1 // PARPLT // 1 
real,    intent(IN)    :: P_splaimax  !> // PARAMETER // maximal sources/sinks value allowing the trophic stress calculation for leaf growing // SD // PARPLT // 1 
real,    intent(IN)    :: P_slamin  !> // PARAMETER // minimal SLA of green leaves // cm2 g-1 // PARPLT // 1 
real,    intent(IN)    :: P_slamax  !> // PARAMETER // maximal SLA of green leaves // cm2 g-1 // PARPLT // 1 
real,    intent(IN)    :: P_tigefeuil  !> // PARAMETER // stem (structural part)/leaf proportion // SD // PARPLT // 1 
real,    intent(IN)    :: P_tustressmin  !> // PARAMETER //  threshold stress (min(turfac,inns)) under which there is an effect on the LAI (supplementary senescence by ratio at the natural senescence) // SD // PARPLT // 1 
real,    intent(IN)    :: fstressgel   !> // OUTPUT // Frost index on the LAI // 0-1
real,    intent(IN)    :: dltamsen   !> // OUTPUT // Senescence rate // t ha-1 j-1
real,    intent(IN)    :: sla   !> // OUTPUT // Specific surface area // cm2 g-1
real,    intent(IN)    :: remobilj   !> // OUTPUT // Amount of biomass remobilized on a daily basis for the fruits  // g.m-2 j-1
real,    intent(IN)    :: dltams                ! (n-1)    // OUTPUT // Growth rate of the plant  // t ha-1.j-1
real,    intent(IN)    :: densiteequiv  !densite equivalente calculée chaque jour

real,    intent(OUT)   :: tustress   !> // OUTPUT // Stress index active on leaf growth (= minimum(turfac,innlai))  // 0-1
real,    intent(OUT)   :: efdensite  
integer, intent(OUT)   :: nstopfeuille  
real,    intent(OUT)   :: deltai                ! (n)    --(0:nbjmax)    // OUTPUT // Daily increase of the green leaf index // m2 leafs.m-2 soil
real,    intent(OUT)   :: splai   !> // OUTPUT // Pool/sink ratio applied to leaves // 0-1
real,    intent(OUT)   :: fpv   !> // OUTPUT // Sink strength of developing leaves // g.j-1.m-2
real,    intent(OUT)   :: P_stlaxsen  !> // PARAMETER // Sum of development units between the stages LAX and SEN // degree.days // PARPLT // 1 
real,    intent(OUT)   :: tempeff   !> // OUTPUT // Efficient temperature for growth // degree C

real,    intent(INOUT) :: lai(0:nbjmax)            ! (n), (n-1), (nsen), (nlan)    // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
real,    intent(INOUT) :: somfeuille  
integer, intent(INOUT) :: nbfeuille   !> // OUTPUT // Number of leaves on main stem // SD
integer, intent(INOUT) :: nsen  
integer, intent(INOUT) :: nlan  
real,    intent(INOUT) :: P_dlaimax  !> // PARAMETER // Maximum rate of the setting up of LAI // m2 leaf plant-1 degree d-1 // PARPLT // 1 
real,    intent(INOUT) :: reajust  
real,    intent(INOUT) :: ulai(0:nbjmax)        ! (n), (n-1)    // OUTPUT // Daily relative development unit for LAI // 0-3
real,    intent(INOUT) :: vmax  
real,    intent(INOUT) :: dltaisenat  
real,    intent(INOUT) :: laisen(0:nbjmax)        ! (n), (n-1)    // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
real,    intent(INOUT) :: dltaisen   !> // OUTPUT // Daily increase of the senescent leaf index // m2.m-2 sol.j-1
real,    intent(INOUT) :: P_stsenlan  !> // PARAMETER // Sum of development units between the stages SEN et LAN // degree.days // PARPLT // 1 



!: VARIABLES LOCALES
real :: deltaimaxi  
real :: sbvmin  
real :: sbvmax  


!write(618,*)'debut calai', lai(n),lai(n-1),P_codeinnact,P_codeh2oact,turfac,innlai


!: 1. CALCUL DU FACTEUR STRESS EAU ET N
!--------------------------------------

    if (P_codeinnact == 1 .and. P_codeh2oact == 1) then
      tustress = min(turfac,innlai)
    endif
    if (P_codeinnact == 2 .and. P_codeh2oact == 1) then
      tustress = min(turfac,1.0)
    endif
    if (P_codeinnact == 1 .and. P_codeh2oact == 2) then
      tustress = min(1.0,innlai)
    endif
    if (P_codeinnact == 2 .and. P_codeh2oact == 2) then
      tustress = 1.
    endif


!: 2. INITIALISATIONS AVANT LE STADE NLEV (LEVEE):
!-------------------------------------------------

!: Dans le cas des cultures annuelles (P_codeperenne=1), trois cas:
!
!- a) pour les cultures qui se plantent (P_codehypo=2) et admettent un
!-    temps de latence avant le demarrage de la croissance (P_codegermin=1):
!-    initialisation du lai a P_laiplantule a partir de la plantation;
!-    et calcul de QNPlante par courbe de dilution
!-
!- b) pour les cultures qui se sement (P_codehypo=1):
!-    initialisation du lai a 0 a partir du semis
!-
!- c) pour les cultures qui se plantent (P_codehypo=2) et demarrent directement (P_codegermin=2):
!-    initialisation du lai a P_laiplantule a partir de la levee qui est aussi la plantation
!-------------------------------------------------
    if (nlev == 0) then

      if (P_codeperenne == 1 .and. n >= nplt) then  !- NPLT étant non nul dès le depart de la simulation, on test n>=nplt
        if (P_codehypo == 2 .and. P_codegermin == 1) then ! a)
          lai(n) = P_laiplantule
          return
        else ! b)
          lai(n) = 0.
          return
        endif
      else
        lai(n) = 0.0
        return
        !: TODO - le somfeuille=0 après le return est inutile, il n'est jamais exécuté. BUG ?
        somfeuille = 0.
      endif
    endif



    ! le jour de la levée, si plantation par plantule, on force le lai du jour précédent
    if (n == nlev .and. P_codehypo == 2 .and. namf == 0) then
      lai(n-1) = P_laiplantule
    endif

    !: à la récolte (nrec+1)
    if (nrec > 0 .and. n > nrec) then
      ! En cas de moisson, on enlève les feuilles.
      if (P_codcueille == 1) then
        lai(n) = 0.
        somfeuille = 0.
!        write(618,*)'apres recolte on sort de calai'
        return
      endif
    endif



! write(618,*)'lai avant deltai',n , lai(n-1), lai(n)
    !: Calcul du nombre de feuilles
    if (P_codetemp == 2) then
      call CalculNombreDeFeuilles(P_phyllotherme,nlax,udevcult,somfeuille,nbfeuille)
    else
      call CalculNombreDeFeuilles(P_phyllotherme,nlax,udevair,somfeuille,nbfeuille)
    endif

    !: si option LAI brut+ sénescence
    if (P_codlainet == 2) P_dlaimax = P_dlaimaxbrut

    !: 2a) calcul de reajust : distance de développement pour réajuster
    !-     les parcours observés
    if (n == nlev) reajust = 0.0
    if (n == namf) reajust = 0.0
    if (n == nlax) reajust = 0.0
    if (n == nsen) reajust = 0.0
    if (n == nlan) reajust = 0.0

    reajust = reajust + upvt - upvtutil


    !: 2b) lai de départ non nul pour les cultures qui se plantent
    !--      if (n == nlev .and. P_codehypo == 2 .and. namf == 0) then
    !--        lai(n-1) = P_laiplantule
    !--!: NB - le 21/04 - sbv remplacé par slavert
    !--        ms(n-1) = lai(n-1)/P_slamin*100.0
    !--!: NB - ajout de P_QNplante0 - le 21/04
    !--        QNplante = (P_adilmax * ms(n-1) * 10) + P_QNplante0
    !--        QNplantetot = QNplante
    !--        inns =  1.0
    !--        inn  =  1.0
    !--        innlai  =  1.0
    !--      endif

!: 3. CALCUL DE ULAI
!-------------------
    !: 3a) calcul de ulai entre lev et amf
    !- domi - 29/03 la vernalisation ne joue pas sur ulai si codeulaivernal = 1
    if (nlev > 0 .and. namf == 0) then
      if (codeulaivernal == 1) then
        ulai(n) = 1 + (P_vlaimax - 1) * (somcour + reajust) / (P_stlevamf + reajust)


!write(618,*)'ulai 1',n,nlev,namf,P_vlaimax,somcour,reajust,P_stlevamf,ulai(n)
      else
        if (somcourutp <= P_stlevamf) then
          ulai(n) = 1 + (P_vlaimax - 1) * (somcourutp + reajust) / (P_stlevamf + reajust)
!write(618,*)'ulai 2',n,nlev,namf,P_vlaimax,somcour,reajust,P_stlevamf,ulai(n)
        else
          ulai(n) = ulai(n-1)
!write(618,*)'ulai 3',n,nlev,namf,P_vlaimax,somcour,reajust,P_stlevamf,ulai(n)
        endif
      endif
    endif

    !: 3b) calcul de ulai entre amf et lax
    if (namf > 0 .and. nlax == 0) then
! --         ulai(n) = P_vlaimax+(3-P_vlaimax)*(somcour+reajust)/(P_stamflax+reajust)
      if (codeulaivernal == 1) then
        ulai(n) = P_vlaimax + (3 - P_vlaimax) * (somcour + reajust) / (P_stamflax + reajust)
!write(618,*)'ulai 4',n,nlev,namf,P_vlaimax,somcour,reajust,P_stlevamf,ulai(n)
      else
        if (somcourutp <= P_stamflax) then
          ulai(n) = P_vlaimax + (3 - P_vlaimax) * (somcourutp + reajust) / (P_stamflax + reajust)
!write(618,*)'ulai 5',n,nlev,namf,P_vlaimax,somcour,reajust,P_stlevamf,ulai(n)
        else
          ulai(n) = ulai(n-1)
!write(618,*)'ulai 6',n,nlev,namf,P_vlaimax,somcour,reajust,P_stlevamf,ulai(n)
        endif
      endif

    endif

    !: 3c) ulai maximal atteint à drp pour les indéterminées
    if (P_codeindetermin == 2 .and. ndrp > 0) then
      ulai(n) = P_udlaimax
    else
      if (nlax > 0) ulai(n) = P_udlaimax
    endif

!write(618,*)'ulai',n,P_stlevamf,ulai(n)

!: 4. CALCUL du DELTAI BRUT
!--------------------------

    !: 4a) calcul du facteur densité efdensite actif à partir de P_laicomp
    !- calcul de l'effet densité sur la mise en place du LAI pour les stics-plante
    efdensite = 1.
    if (ulai(n) > 1.) then
      if (lai(n-1) < P_laicomp) then
        efdensite = 1.
      else
        if (densite == 0) then
          efdensite = 0.0
        else
          ! domi - 02/07/2002 - pb si gel total densite  = 0 et pb de log(0)
!          efdensite = min(1.0,(exp(P_adens * (log(densite / P_bdens)))))
! DR 12/09/2012 on prend la densite equivalente pour calculer l'effet densite et uniquement pour ca !
          efdensite = min(1.0,(exp(P_adens * (log(densiteequiv / P_bdens)))))
        endif
      endif
    else
      ! domi - 02/07/2002 - pb si gel total densite  = 0 et pb de log(0)
      if (densite == 0) then
        efdensite = 0.0
      else
!        efdensite = min(1.0,(exp(P_adens * log(densite / P_bdens))))
! DR 12/09/2012 on prend la densite equivalente pour calculer l'effet densite et uniquement pour ca !
        efdensite = min(1.0,(exp(P_adens * log(densiteequiv / P_bdens))))
      endif
    endif

    if (ulai(n) == 1.0) efdensite = 1


    ! ** 4b) température efficace de croissance: limitation par P_tcmax
    ! NB le 13/06/06
    ! introduction d'une température maximale d'arrêt de croissance
    ! foliaire P_Tcxstop à l'occasion étude Françoise sur fourrages méditerranéens
    !      P_tcxstop = 30.0
    if (P_tcxstop >= 100.0) then
      if (tcult > P_tcmax) then
        tempeff = max(0.,(P_tcmax - P_tcmin))
      else
        tempeff = max(0.,(tcult - P_tcmin))
      endif
    else
      if (tcult > P_tcmax) then
        tempeff = (P_tcmax - P_tcmin) / (-P_tcxstop + P_tcmax) * (tcult - P_tcxstop)
        tempeff = max(0.,tempeff)
      else
        tempeff = max(0.,(tcult - P_tcmin))
      endif
    endif

    !: 4c) calcul du deltai
    !- quelle que soit l'option LAI choisie, LAI net ou LAI brut :
    !-  * la croissance s'arrête à LAX pour les déterminées
    !-  * la croissance s'arrête à SEN pour les indéterminées
    if (P_codeindetermin == 1) nstopfeuille = nlax
    if (P_codeindetermin == 2) nstopfeuille = nsen

    if (P_codlainet == 2) nstopfeuille = nlax

    if (nstopfeuille == 0) then
      !--If (P_codeindetermin == 2) P_udlaimax = 3.0

      !: modif le 23/07/01
      !- entre ulai = 1 et ulai = P_udlaimax, la vitesse est croissante
      !- NB - le 13/12

      if (ulai(n) <= P_udlaimax .or. P_udlaimax == 3.0) then
        ! DR 07/03/08 integration des modifs du 28/02/07 P_dlaimax n'etait actif
        ! et introduction de P_codedlaimin
        ! NB et FR le 28/02/07
        ! introduction d'une ordonnée à l'origine pour la croissance des feuilles
! DR on vire le P_codedlaimin et on met P_dlaimin dans le fichier plante
!        if (P_codedlaimin == 2) then
!          deltai = P_dlaimax * efdensite * densite &
!                    / (1 + exp(P_pentlaimax * (P_vlaimax - ulai(n))))
!          write(*,*) P_dlaimax, efdensite, densite, P_pentlaimax, P_vlaimax, ulai(n), deltai
!        else
          deltai = P_dlaimax * efdensite * densite &
                    * (P_dlaimin + (1.0 - P_dlaimin) / (1 + exp(P_pentlaimax * (P_vlaimax - ulai(n)))))


!DR 14/09/2012
         ! write(618,*)'1, P_dlaimax,P_dlaimin, efdensite, densite, P_pentlaimax, P_vlaimax, ulai(n)',n, deltai, &
         ! & P_dlaimax,P_dlaimin, efdensite, densite, P_pentlaimax, P_vlaimax, ulai(n)

!        endif
        vmax = deltai

      else
        !: entre P_udlaimax = 1 et ulai = 3(lax), la vitesse est décroissante
        !- sauf pour les indéterminées
! DR 24/01/2011 on a un pb dans l'equation la puissance multiplie tout (eq 3.2 bouquin)
!        deltai = vmax * (1 - (ulai(n) - P_udlaimax) / ((3.0 - P_udlaimax))**2)
        deltai = vmax * (1 - (ulai(n) - P_udlaimax) / (3.0 - P_udlaimax))**2

       ! write(618,*)'2',deltai,vmax,ulai(n),P_udlaimax
      endif


      !: NB - le 28/05 - on lève toujours
      !- 10/01 - ajout d'un test sur l'enchainement des perennes
      ! TODO: voir si on pourrait remplacer ce test par un booléen qui dirait si la plante a levé ou pas. ex: *if (isLevee) then*
      if (n > nlev .or. (P_codeperenne == 2 .and. P_codeinitprec == 2 .and. numcult > 1)) then
        !: NB le 07/06 introduction de exolai
! DR 22/09/2015 pour etre en coherence avce la version modularisation LAI et pouvoir continuer les comparaison je decoupe le calcul de deltai en 2 parties
       ! deltai = deltai * tempeff * tustress * exolai
        deltai = deltai * tempeff
        deltai = deltai * tustress * exolai


!DR 14/09/2012
        !write(618,*)'2,deltai,  tempeff , tustress , exolai',deltai,  tempeff , tustress , exolai
      endif
!     write(*,*)P_codedlaimin, P_codeindetermin

      !: la croissance foliaire des plantes indéterminées dépend
      !- du rapport source/puits des organes végétatifs
      if (P_codeindetermin == 2) then

        splai = (sourcepuits - P_splaimin) / (P_splaimax - P_splaimin)

        splai = max(splai,0.0)
        splai = min(splai,1.0)

        ! NB le 21/04 recalcul d'un sbv approprié
        sbvmin = P_slamin / (1.0 + P_tigefeuil)
        fpv = deltai * splai * 1e4 / sbvmin
        deltai  =  deltai*splai


        !DR 14/09/2012 nettoyage
       ! write(618,*)'3deltai,  splai',deltai,  splai
      else
        !: limitation si le rayonnement est vraiment insuffisant
        !- après AMF (cas des cultures associées)
        !- NB le 21/04 recalcul d'un sbv approprié
        !- NB le 15/05/02 calcul de fpv pour les pérennes
        sbvmin = P_slamin / (1.0 + P_tigefeuil)
        fpv = deltai * 1e4 / sbvmin
        sbvmax = P_slamax / (1.0 + P_tigefeuil)
        !: ajout des remobilisations possible pour faire redémarrer le
        !- lai en cas de couvert complètement sec
        !- NB le 13/06/06
        !- NB le 27/06/06 attention delatremobil était déjà compté dans dltams
        deltaimaxi = (dltams + remobilj) * sbvmax / 100.

        if (namf > 0) then
          deltai = min(deltai,deltaimaxi)

         ! write(618,*)'3 bis deltai,  deltaimaxi',deltai,  deltaimaxi
        endif
      endif
      ! fin nstopfeuille = 0
    else
      deltai = 0.0

    endif

!write(618,*)'avant phase plateau deltai,lai(n)',deltai,lai(n)
!: 5. PHASE DE PLATEAU  (si option lainet et culture determinée)
!---------------------------------------------------------------

    if (P_codlainet == 1 .and. P_codeindetermin == 1) then
      if (nlax > 0 .and. nsen == 0) deltai = 0.0
    endif


!: 6. Calcul de la senescence rapide par ajustement a la récolte
!- entre sen et lan  pour les options 1 et 2
!---------------------------------------------------------------
    if (P_codlainet <= 1) then
      lai(n) =  lai(n-1) + deltai - dltaisenat
    !  write(618,*)'4 lai(n),lai(n-1) , deltai , dltaisenat',lai(n),lai(n-1) , deltai , dltaisenat

      if (n == nsen) lai(n) = lai(n-1)

      if (nsen > 0 .and. nlan == 0.) then
        lai(n) = lai(nsen) * (1 - ((somcour+reajust) / (P_stsenlan+reajust)))
        dltaisenat = lai(n-1) - lai(n)
        if (lai(n) <= 0.0 .and. nlan == 0)  nlan = n
        if (n == nlan) lai(nlan) = 0.0
      endif
    else
      !: option LAI brut la sénescence (dltaisen) est calculé dans le spg senescen
      lai(n) = lai(n-1) + deltai - dltaisen
    endif

    if (lai(n) < 0.) then
      lai(n) = 0.
      ! DR 06/09/06 y'a un soucis car alors laisen ne prend pas en compte la derniere valeur de lai
      !--dltaisen = 0.0
    endif

    ! NB - le 22/04 - à cause précisison dans les soustractions
    if (lai(n) <= 1e-4 .and. nstopfeuille > 0) then
      lai(n) = 0.
    endif

!write(618,*)'apres phase plateau P_codlainet,deltai,dltaisenat,lai(n)',P_codlainet,deltai,dltaisenat,lai(n)

!: 7. senescence foliaire due à des stress  pour l'option LAInet
!---------------------------------------------------------------

    if (P_codlainet == 1 .and. lai(n) > 0.0) then
      !: NB le 08/04 ajout du stress gel
      if (tustress < P_tustressmin .or. tcult < P_tcmin .or. fstressgel < 1.0) then
        ! bug dans l'opération NB le 08/05
        dltaisen = dltamsen / 100 * sla
        lai(n) = lai(n) - dltaisen
        dltaisen = dltaisen + dltaisenat
        lai(n) = max(lai(n),0.0)
!        write(618,*)'5',dltamsen,dltaisen,sla,lai(n)
      else
        dltaisen = dltaisenat
      endif
    endif
!write(618,*)'1', lai(n),lai(n-1),dltaisen,tustress, P_tustressmin,tcult , P_tcmin,fstressgel
    laisen(n) = laisen(n-1) + dltaisen

    !: NB le 22/04: calcul du stade lan pour P_codlainet = 2
    if (lai(n) <= 0 .and. nlan == 0 .and. nlax > 0) then
      if (nsen == 0) then
        nsen = n
        P_stlaxsen = somcour
      endif
      nlan = n

      P_stsenlan = somcour

      lai(n) = 0.0      ! 07/06
      dltaisen = 0.0
      dltaisenat = 0.0 ! NB le 11/06
    endif
!write(618,*)'apres phase scenes P_codlainet,deltai,dltaisen,lai(n)',P_codlainet,deltai,dltaisen,lai(n)
     ! write(618,*)'fin calai',n, lai(n),deltai,dltaisen


return
end subroutine calai_
