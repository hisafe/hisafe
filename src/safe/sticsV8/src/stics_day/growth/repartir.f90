! *----------------------------------------------* c
! * répartition de la biomasse entre les organes * c
! * programmation N. Brisson le 14/11/2000       * c
! * modif le 18/04 NB                            * c
! *----------------------------------------------* c
!> There are models for which allocation of assimilates is critical to the operation of the model (e.g. SUCROS described by Van Ittersum et al., 2003).
!>- Stics book paragraphe 3,5, page 68-71
!>
!> In STICS this module was added at a late stage, mainly to help dimensioning the reserve pool. For annual plants with determinate growth, the partitioning
!! calculations simply allow the dimensioning of envelopes of harvested organs which may play a trophic role and ensure an input of information for the
!! senescence module. For perennial plants or those with indeterminate growth, those calculations enable the dimensioning of a compartment for reserves which
!! are recycled in the carbon balance. The calculation of root biomass is not directly connected to that of the above-ground biomass.
!> - Organs and compartments identified: the reasons for identifying an organ or a compartment are either its internal trophic role within the plant or an
!!   external role by participation in the nitrogen balance of the system (such as falling leaves and the recycling of roots). The reserve compartment is not
!!   located in a specific organ: it is just a certain quantity of carbon available for the plant growth.
!> - Dimensioning of organs:
!!   Green leaves: The biomass of green leaves is calculated without accounting for potential reserves that may be stored in the leaves and remobilized later on,
!!   which are accounted for in the resperenne non-located reserve pool. The mafeuilverte variable is deducted from the LAI, based on the
!!   maximum specific leaf area variable (slamax). We assume that the difference between the actual sla and slamax corresponds to remobilized leaf carbon
!!   Yellow leaves: The biomass of yellow leaves (mafeuiljaune) is calculated in the senescence module.  The proportion of leaves in the senescent biomass
!!   on a given day (dltamsen) is determined using the pfeuilverte ratio (proportion of green leaves in the non-senescent biomass) on the day of production
!!   of this senescent biomass. Some of these yellow leaves may fall to the ground depending on the abscission  parameter (between 0 and 1). The daily falling
!!   quantity (dltamstombe) is recycled in the nitrogen balance; its cumulative value is mafeuiltombe.
!!   Stems: this concerns only the structural component of stems (matigestruc). The non-structural component, if significant, can be included in the reserve
!!   compartment (e.g. for cereals) or in the harvested part (sugar cane).  The matigestruc variable is calculated as a constant proportion (tigefeuille of the
!!   total mass of foliage. For monocotyledonous plants, the stem is secondary and the matigestruc variable is only incremented from the time when accumulated
!!   biomass so allows.  It is thus assumed that the first organs to emerge are the leaves. For dicotyledonous plants, it is assumed that the tigefeuille
!!   proportionality is always respected.  Consequently, if the accumulated biomass and the foliage biomass (calculated from the LAI and SLA) are incompatible
!!   with this proportionality, then the SLA (or LAI if the SLA arises from fixed limits) is recalculated. The matigestruc variable cannot diminish,
!!   except in the case of cutting fodder crops.
!!   Harvested organs:
!>   - Fruits and grains: the calculation of the number and mass of fruits (indeterminate plants) or seeds (determinate plants) is achieved in modules
!!       fruit.f90 and grain.f90.
!>   - Envelops of harvested organs (pods, raches, etc.): the mass corresponding to the envelope is assumed to depend solely upon the number of organs.
!!       In any case, it cannot exceed the residual biomass (masecveg - mafeuil - matigestruc). The envfruit parameter corresponds to the proportion of
!!       membrane related to the maximum weight of the fruit. If the sea parameter is not zero, then this biomass is transformed into an equivalent
!!       leaf surface area, photosynthetically active from the IDRP stage to the IDEBDES stage.
!>
!!   Reserves (resperenne) are calculated as the difference between the total biomass and the accumulated biomass of leaves, stems and harvested organs.
!!   For perennial plants, at the beginning of the cropping season, the reserves (carbon) can be initialised at a non-zero value (resperenne0), so as to
!!   represent the role played by root reserves at the resumption of growth. Yet it is assumed that a limit exists to the size of the reserve compartment,
!!   parametrized at the plant level by resplmax. If this limlit is reached a “sink on source” effect is simulated. The use of reserves concerns perennial plants
!!   or indeterminate plants.  As for determinate annuals, the use of reserves for grain filling is not simulated as such, but taken globally into account
!!   when calculating the ercarb variable (index of progressive harvest).
!-----------------------------------------------------------------------------
subroutine repartir(n,nrec,P_codcueille,P_codeperenne,nlev,nlax,P_nbcueille,numcult,tustress,P_slamin,P_slamax,   &
                    P_codlainet,P_codemonocot,P_codesimul,dltaisen,P_tigefeuil,P_envfruit,chargefruit,ndrp,       &
                    ndebdes,P_sea,ntaille,P_codetaille,P_codeinitprec,dltams,lai_veille,                      &
                    resperenne,masecveg,pdsfruittot,tursla,sla,mafeuilverte,mafeuil,mafeuilp,lai,deltai,&
                    maenfruit,eai,mareserve,deltares,mabois,P_resperenne0,masec,msresjaune,mafeuiljaune,  &
                    msneojaune,matigestruc,pfeuil,pfeuilverte,pfeuiljaune,ptigestruc,penfruit,preserve)

  USE Messages

  implicit none

!: Arguments

  integer, intent(IN)    :: n  
  integer, intent(IN)    :: nrec  
  integer, intent(IN)    :: P_codcueille  !> // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0 
  integer, intent(IN)    :: P_codeperenne  !> // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0 
  integer, intent(IN)    :: nlev  
  integer, intent(IN)    :: nlax  
  integer, intent(IN)    :: P_nbcueille  !> // PARAMETER // number of fruit harvestings // code 1/2 // PARTEC // 0 
  integer, intent(IN)    :: numcult  
  real,    intent(IN)    :: tustress   !> // OUTPUT // Stress index active on leaf growth (= minimum(turfac,innlai))  // 0-1
  real,    intent(IN)    :: P_slamin  !> // PARAMETER // minimal SLA of green leaves // cm2 g-1 // PARPLT // 1 
  real,    intent(IN)    :: P_slamax  !> // PARAMETER // maximal SLA of green leaves // cm2 g-1 // PARPLT // 1 
  integer, intent(IN)    :: P_codlainet  !> // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
  integer, intent(IN)    :: P_codemonocot  !> // PARAMETER // option plant monocot(1) or dicot(2) // code 1/2 // PARPLT // 0 
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=12), intent(IN) :: P_codesimul  !> // PARAMETER // simulation code (culture ou feuille=lai forcé) // SD // P_USM/USMXML // 0
  integer, intent(IN)    :: P_codesimul  !> // PARAMETER // simulation code (culture ou feuille=lai forcé) // SD // P_USM/USMXML // 0
  real,    intent(IN)    :: dltaisen   !> // OUTPUT // Daily increase of the senescent leaf index // m2.m-2 sol.j-1
  real,    intent(IN)    :: P_tigefeuil  !> // PARAMETER // stem (structural part)/leaf proportion // SD // PARPLT // 1 
  real,    intent(IN)    :: P_envfruit  !> // PARAMETER // proportion envelop/P_pgrainmaxi in weight  // SD // PARPLT // 1 
  real,    intent(IN)    :: chargefruit   !> // OUTPUT // Amount of filling fruits per m-2 // nb fruits.m-2
  integer, intent(IN)    :: ndrp  
  integer, intent(IN)    :: ndebdes  
  real,    intent(IN)    :: P_sea  !> // PARAMETER // specifique surface of fruit envelops // cm2 g-1 // PARPLT // 1 
  integer, intent(IN)    :: ntaille  
  integer, intent(IN)    :: P_codetaille  !> // PARAMETER // option of pruning // code 1/2 // PARTEC // 0 
  integer, intent(IN)    :: P_codeinitprec  !> // PARAMETER // reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0 
  real,    intent(IN)    :: dltams   !> // OUTPUT // Growth rate of the plant  // t ha-1.j-1
  real,    intent(IN)    :: lai_veille              ! n-1  

  real,    intent(INOUT) :: resperenne   !> // OUTPUT // C crop reserve, during the cropping season, or during the intercrop period (for perenial crops) // t ha-1
  real,    intent(INOUT) :: masecveg   !> // OUTPUT // Vegetative dry matter // t.ha-1
  real,    intent(INOUT) :: pdsfruittot  
  real,    intent(INOUT) :: tursla  
  real,    intent(INOUT) :: sla   !> // OUTPUT // Specific surface area // cm2 g-1
  real,    intent(INOUT) :: mafeuilverte   !> // OUTPUT // Dry matter of green leaves // t.ha-1
  real,    intent(INOUT) :: mafeuil   !> // OUTPUT // Dry matter of leaves // t.ha-1
  real,    intent(INOUT) :: mafeuilp  
  real,    intent(INOUT) :: matigestruc   !> // OUTPUT // Dry matter of stems (only structural parts) // t.ha-1
  real,    intent(INOUT) :: lai                     ! n    // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(INOUT) :: deltai   !> // OUTPUT // Daily increase of the green leaf index // m2 leafs.m-2 soil
  real,    intent(INOUT) :: maenfruit   !> // OUTPUT // Dry matter of harvested organ envelopes // t.ha-1
  real,    intent(INOUT) :: eai  
  real,    intent(INOUT) :: mareserve  
  real,    intent(INOUT) :: deltares  
  real,    intent(INOUT) :: mabois   !> // OUTPUT // Prunning dry weight // t.ha-1
  real,    intent(INOUT) :: P_resperenne0  !> // PARAMETER // initial reserve biomass // t ha-1 // INIT // 1 
  real,    intent(INOUT) :: masec   !> // OUTPUT // Aboveground dry matter  // t.ha-1
  real,    intent(INOUT) :: msresjaune   !> // OUTPUT // Senescent residual dry matter  // t.ha-1
  real,    intent(INOUT) :: mafeuiljaune   !> // OUTPUT // Dry matter of yellow leaves // t.ha-1
  real,    intent(INOUT) :: msneojaune   !> // OUTPUT // Newly-formed senescent dry matter  // t.ha-1
  real,    intent(INOUT) :: pfeuil   !> // OUTPUT // Proportion of leaves in total biomass // 0-1
  real,    intent(INOUT) :: pfeuilverte   !> // OUTPUT // Proportion of green leaves in total non-senescent biomass // 0-1
  real,    intent(INOUT) :: pfeuiljaune   !> // OUTPUT // Proportion of yellow leaves in total biomass // 0-1
  real,    intent(INOUT) :: ptigestruc   !> // OUTPUT // Proportion of structural stems in total biomass // 0-1
  real,    intent(INOUT) :: penfruit   !> // OUTPUT // Proportion of fruit envelopes in total biomass // 0-1
  real,    intent(INOUT) :: preserve   !> // OUTPUT // Proportion of reserve in the total biomass // 0-1

!: Variables locales
  real :: mareservep  !>  
  real :: ptigestrucveg  !>  
  real :: matigestruc1  
  real :: laiajour  

! ** paramètres
! *-    slavert est la surface spécifique du feuillage "sans stress"
! *-    slavertmin est le minimum de slavert après application des stress
! *-    tigefeuille est le rapport tigestruc/feuilles totales
! *-    P_envfruit correspond à la proportion enveloppe/P_pgrainmaxi
! *-    photores est la photopériode seuil en jour décroissant
! *-      qui déclenche la mise en réserve

      ! ** après la récolte (nrec+1)
      ! ** en cas de moisson,on shunte ce module après récolte

      if (n >= nrec+1 .and. nrec > 0 .and. P_codcueille == 1 .and. P_codeperenne == 1) then
          resperenne = 0.
      else

          ! ** la biomasse non récoltable est masecveg
          ! --     if (nrec == 0.or.n == nrec) masecveg = masec-pdsfruittot/100.
          ! -- ML le 10/10/09 calcul de masecveg avant annulation de pdsfruittot le jour de recolte
          ! 11/07/2013 DR et ML on corrige un bug suite à un pb pour la betterave (matuber incluait 2 fois mafruit) , (c'etait le cas pour toutes les plantes )
          !  dans biomaer la biomasse des fruits est retirée de masec uniquement si codcueille=2 : dans ce cas il ne faut pas la retirer une 2ieme fois dans repartir
!          if (P_nbcueille == 1 .and. n >= nrec .and. nrec > 0) then
          if (P_codcueille == 2 .and. n >= nrec .and. nrec > 0) then
            ! !!!!!!!!!DR et Ml le 31/01/06 on se pose des questions sur la ligne mise en commantaire
            ! et on la reactive car masec est la matiere seche avce les fruits d'ou pb
            !  a voir
            !    masecveg = masec-pdsfruittot/100.
            ! NB le 09/07/06
            ! les fruits récoltés ont déjà été ôtés dans le sspg biomaer.for
            ! donc il ne faut pas les enlever 2 fois....
            masecveg = masec
            pdsfruittot = 0.
          else
            masecveg = masec - pdsfruittot / 100.
          endif


          ! ** conservation de la réserve du jour précédent
          mareservep = mareserve

          ! ** initialisation de la réserve pérenne
          if (n == nlev .and. numcult == 1) then
            if (P_codeperenne == 2) then
              resperenne = P_resperenne0
            else
              resperenne = 0.
            endif
          endif

          ! ** les feuilles vertes
          ! *- calcul d'un stress moyen pour module slavert
          !-- if (n == nlev) tursla = 1.


          tursla = (tursla+tustress)/2.
          sla = max(tursla*P_slamax,P_slamin)


          ! le 09/07/06 mise à jour du LAI pour adéquation avec sénescence
          if (P_codlainet == 2) then
            laiajour = lai-dltaisen
          else
            laiajour = lai
          endif

          ! NB le 09/07/06 pour permettre le stockage de réserve dans les feuilles
          ! mafeuilleverte correspond à de la matière structurale et donc n'est pas
          ! affectée par les stress, ce qui se traduit par l'utilisation de P_slamax et non sla
          !      mafeuilverte = min(lai/sla * 1e2,masecveg)
          ! loic 20/05/2016 il faut retirer de masecveg ce qui est senecent mafeuiljaune , sinon mafeuilverte n'estr plus verte
          ! mafeuilverte = min(laiajour/P_slamax * 1e2, masecveg)
          mafeuilverte = min(laiajour/P_slamax * 1e2, (masecveg-mafeuiljaune))


          ! ** les feuilles jaunes sont calculées dans senescen.for
          ! *- variable mafeuiljaune

          ! ** cumul ensemble des feuilles
          mafeuil = mafeuilverte + mafeuiljaune

          ! ** test sur mafeuil >masecveg NB le 22/04
          if (masecveg < mafeuil) then
            mafeuilverte = masecveg - mafeuiljaune
            mafeuil = masecveg
          endif

          if (mafeuilverte <= 0 .and. nlax == 0) then
            call EnvoyerMsgHistorique(403)
          endif

          ! ** la masse structurale des tiges est une proportion de la masse de feuille
          ! *- matigestruc = min(P_tigefeuil*mafeuil,masecveg-mafeuil)

          ! ** pour les monocotylédones : priorité aux feuilles
          if (P_codemonocot == 1) then
            matigestruc = matigestruc + max(P_tigefeuil * (mafeuil - mafeuilp),0.)
            matigestruc = min(matigestruc, masecveg-mafeuil)

          ! ** pour les dicotylédones : priorité aux tiges
          else
            matigestruc1 = P_tigefeuil * mafeuil
            if (matigestruc1 > (masecveg - mafeuil)) then
              ! ** on recalcule mafeuil tel que masecveg = (tigfeuil+1)mafeuil
              mafeuil = masecveg/(P_tigefeuil+1)
              mafeuilverte = mafeuil - mafeuiljaune

              if (mafeuilverte > 0.) then
                ! NB le 09/07/06 mise à jour LAI
                !-- sla = lai/mafeuilverte*100.
                sla = laiajour/mafeuilverte*100.
              endif

              if (sla < P_slamin) then
                ! NB le 09/07/06 mise à jour LAI
                !-- mafeuilverte = lai/P_slamin*100.
                mafeuilverte = laiajour/P_slamin*100.
                mafeuil = mafeuilverte + mafeuiljaune
                sla = P_slamin
              endif
              ! ** domi - 29/04/03 - pour stics-feuille incompatibilite avce le forcage du lai
              ! DR 15/10/2010 y'avait un pb dans les tests --> incompatibilité avec le version 7.1 , j'ajoute des parentheses
!              if (sla > P_slamax  .and. lge(P_codesimul,'feuille') .eqv. .FALSE.) then
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!              if ((lge(P_codesimul,'feuille') .eqv. .FALSE.).and.(sla > P_slamax  )) then
              if ((P_codesimul == 1).and.(sla > P_slamax  )) then
                sla = P_slamax
                lai = sla * (mafeuil - mafeuiljaune)/100.
                ! DR et IGC le 07/09/06
                deltai = lai - lai_veille
                !write(618,*)'repartir',sla,lai,mafeuil,mafeuiljaune
              endif
            endif

            matigestruc = matigestruc + max(P_tigefeuil * (mafeuil-mafeuilp),0.)
            ! loic 20/05/2016 il faut ajouter le test pour eviter  que la somme des composants
            ! de la biomasse aerienne soient plus grands que masecveg.
            matigestruc = min(matigestruc, masecveg-mafeuil)
          endif

          mafeuilp = mafeuil

          ! ** NB - le 12/04 - pour éviter plantade liée au GEL
          if (masecveg > 0.0) then
            ptigestrucveg = matigestruc / masecveg
          else
            ptigestrucveg = 0.0
          endif

          ! ** les enveloppes des organes récolté sont un % du
          ! *- nombre de fruits à condition qu'il y ait assez de biomasse
          maenfruit = P_envfruit * chargefruit

          ! ** seuillage de maenfruit - NB - le 21/04
          maenfruit = min(maenfruit, 0.99*(masecveg-mafeuil - matigestruc))

          maenfruit = max(maenfruit,0.)

          if ((masecveg-mafeuil-matigestruc-maenfruit) < 0.) then
            call EnvoyerMsgHistorique(404)
          !--     stop
          endif

          ! ** NB - le 28/03/02 - estimation d'une surface photosynthétique pour le fruit
          ! *- entre les stades DRP et DEBDES
          if (ndrp > 0 .and. ndebdes == 0) then
            eai = P_sea * maenfruit/100.
          else
            eai = 0.
          endif
          if (maenfruit <= 0.) eai = 0.

          ! ** les réserves sont le complémentaire
          mareserve = masecveg-mafeuil - matigestruc - maenfruit
          if (mareserve < 0.) mareserve = 0.

          ! ** pour les pérennes les réserves migrent vers des
          ! *- organes de stockage (racines ou bois)

          ! *- un seul compartiment de réserve
          ! *- NB - calcul des réserves en deux étapes
          ! *- pour éviter de consommer des réserves
          ! *- qui n'existe pas

          deltares = mareserve-mareservep
          resperenne = max(resperenne + deltares,0.0)

    ! TODO : fonction taille
          ! ** taille
          if (n == ntaille .and. P_codetaille == 2) then
            call repartir_taille(mafeuil,P_codeperenne,P_codeinitprec,        & ! IN
                                 mabois,lai,P_resperenne0,masec,msresjaune, & ! INOUT
                                 mafeuiljaune,msneojaune,matigestruc)
          endif

      endif



      ! ** arrêt à la récolte
      ! *- PB - 10/12/2004 - pour l'enchainement des ligneux (vigne),
      ! *- on ne remet pas à zéro ce qui n'a pas lieu d'etre
      ! *- TODO : voir avec nadine s'il ne faut pas qd meme remettre certaines choses à zéro.
      if (masec <= 0.0) then
        mafeuil = 0.0
        mafeuilverte = 0.0
        mafeuiljaune = 0.0
        matigestruc = 0.0
        masecveg = 0.0
        maenfruit = 0.0
        mareserve = 0.0
        pfeuil = 0.0
        pfeuilverte = 0.0
        pfeuiljaune = 0.0
        ptigestruc = 0.0
        penfruit = 0.0
        preserve = 0.0
      else
        ! ** mémoire de pfeuil pour la répartition
        ! *- de la biomasse sénéscente dans senescen.for
        pfeuil = mafeuil / masec

        ! *- Nb & PB - le 23/02/05
        ! *- pfeuilverte calculé en fonction des deltas et non des masses cumulées
        !-- pfeuilverte = mafeuilverte / (masec-mafeuiljaune)
        if (dltams > 0.) then
          ! Nb le 09/07/06 mafeuilleverte et jaune sont en structural (P_slamax au lieu de sla)
          !-- pfeuilverte = (deltai/sla*1e2) / dltams
          pfeuilverte = (deltai/P_slamax*1e2) / dltams
          pfeuilverte = min(1.0,pfeuilverte)
        else
          pfeuilverte = 0.0
        endif

        pfeuiljaune =  mafeuiljaune / masec

        ptigestruc = matigestruc/masec
        penfruit = maenfruit/masec
        preserve =  resperenne/masec
      endif


return
end subroutine repartir


subroutine repartir_taille(mafeuil,P_codeperenne,P_codeinitprec,        & ! IN
                           mabois,lai,P_resperenne0,masec,msresjaune, & ! INOUT
                           mafeuiljaune,msneojaune,matigestruc)


    real,    intent(IN)    :: mafeuil   !> // OUTPUT // Dry matter of leaves // t.ha-1
    integer, intent(IN)    :: P_codeperenne  !> // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0 
    integer, intent(IN)    :: P_codeinitprec  !> // PARAMETER // reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0 

    real,    intent(INOUT) :: mabois   !> // OUTPUT // Prunning dry weight // t.ha-1
    real,    intent(INOUT) :: lai   !> // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
    real,    intent(INOUT) :: P_resperenne0  !> // PARAMETER // initial reserve biomass // t ha-1 // INIT // 1 
    real,    intent(INOUT) :: masec   !> // OUTPUT // Aboveground dry matter  // t.ha-1
    real,    intent(OUT)   :: msresjaune   !> // OUTPUT // Senescent residual dry matter  // t.ha-1
    real,    intent(OUT)   :: mafeuiljaune   !> // OUTPUT // Dry matter of yellow leaves // t.ha-1
    real,    intent(OUT)   :: msneojaune   !> // OUTPUT // Newly-formed senescent dry matter  // t.ha-1
    real,    intent(OUT)   :: matigestruc   !> // OUTPUT // Dry matter of stems (only structural parts) // t.ha-1

    mabois = matigestruc + mafeuil

	! *- PB&Inaki - 08/03/2005 - on enlève plus les feuilles tombées de mabois... absurde !!
	!- mabois = matigestruc + mafeuil - mafeuiltombe

	! *- PB & inaki - 08/12/2004
	! *- à la taille, il faut réinitialiser les variables plantes
    lai = 0.0

    if (P_codeperenne == 2 .and. P_codeinitprec == 2) then
	  ! 07/09/06 DR et IGC on rajoute un then et on garde reserveN
      P_resperenne0 = masec - mabois
	  !-- P_QNplante0 = QNplante(1,n)+QNplante(2,n)
    endif

	! *- INAKI - 08/03/2005 - on change le mode de calcul du masec à la taille.
	!-- masec = masec - mabois
    masec = 0.
    msresjaune = 0.0
    mafeuiljaune = 0.0
    msneojaune = 0.0
    matigestruc = 0.0
	!-- sometemp = 0.

end subroutine
 
 
