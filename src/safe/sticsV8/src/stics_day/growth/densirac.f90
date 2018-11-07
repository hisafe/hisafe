! ************************************************************************* c
! * NB - le 30/5/98                                                       * c
! * croissance racinaire selon groupe racine (17/03/98)                   * c
! * compte-rendu F. Devienne                                              * c
! * rlj : longueur racinaire fournie par la plante en m jour-1            * c
! * P_draclong : paramètre spécifique donnant la vitesse de croissance      * c
! * racinaire par plante et par degré.jour                                * c
! * drliz : répartition de la longueur dans chaque couche                 * c
! * poussrac(z) : indice entre 0 et 1 définissant les obstacles physiques * c
! * à la croissance                                                       * c
! * P_croirac : croissance du front racinaire en cm par degré.jour          * c
! * debsen : jour définissant l'entré en sénescence                       * c
! * ratio de sénescence                                                   * c
! * prop = 1 répartition proportionnelle aux racines présentes            * c
! * prop = 0 répartition équitable                                        * c
! *                                                                       * c
! * derniere modif PB 08/03/04 version 5.1                                * c
! ************************************************************************* c
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module calculates the root density profile according to the ‘true density’ option.
!!
!> - Stics book paragraphe 5.2.2, page 90-94
!>
!! With this option, growth in root length is first calculated, and then distributed to each layer of the soil profile. For sown crops, this calculation begins
!! at emergence: between germination and emergence, it is assumed that only the root front grows. For transplanted or perennial crops, the calculation is
!! initiated with an existing root density profile. After a lifetime characteristic of the species, the roots senesce and enter the mineralization process as
!! crop residue at the end of the crop cycle. Root density above 0.5 cm.cm-3 is not taken into account for water and nitrogen absorption.
!>  - Growth in root length
!!   To ensure the robustness of the model, we have chosen to simulate the growth in root length directly, without passing through the root mass, because
!!   the specific length (root length/mass ratio) varies depending on the stresses suffered by the plant.  Two options are available to calculate the root length.
!!   With the first option, we have adopted a formulation similar to that used for the above-ground growth of leaves (Brisson et al., 1998a). With the second, a
!!   trophic link between shoot growth and root growth allows increase in root length to be calculated.
!>       - Self-governing production : growth in root length is calculated using a logistic function that is analogous to that of leaves.
!!      A first calculation of the root length growth rate describes a logistic curve. This value is then multiplied by the effective crop temperature,
!!      the plant density combined with an inter-plant competition factor that is characteristic for the variety, and the water logging stress index.
!!      Then a second term is added corresponding to the growth at the root front (nouvrac), depending on the front growth rate (deltaz).
!!      The logistic curve describing the root length growth rate depends on the maximum root growth parameter draclong and on the normalized root development
!!      unit urac, ranging from 1 to 3 (such as ulai) and is thermally driven, even when the plant has vernalisation or photoperiod requirements.
!!      The plant parameters pentlaimax and vlaimax are the ones already used for the calculation of leaf growth rate.
!!      The thermal function rlj relies on crop temperature and cardinal temperatures (tcmin and tcmax) which are the same values as for the leaf area growth
!!      calculation. The inter-plant competition function is the same as the one calculated for the leaf area growth. Unlike the leaf area index,
!!      water and nitrogen deficiencies in the plant do not play any role in root growth, which results in the promotion of root growth relative to
!!      above-ground growth in the event of stress.  In contrast, anoxia acts via the water-logging stress index derived from the anox indicator.
!>       - Trophic-linked production : the root length growth may rely on the daily production of shoot biomass (dltams) and on a dynamic underground/total biomass
!!      partitioning coefficient (reprac). The parameter longsperac is the specific root length/root mass ratio. The plant density effect is not taken
!!      into account because it is already integrated in the shoot biomass production. This value can replace calculation or just act as a threshold according
!!      to the chosing option.
!>  - Distribution in the profile
!!   The new root length is then distributed in each layer of the soil profile in proportion to the roots present and as a function of the soil constraints.
!!   A "root sink strength" is defined by the proportion of roots present in the layer. This does not concern the root front, whose growth in density is
!!   defined by lvfront. This potential “root sink strength” is then reduced by the soil constraints in each layer. Each constraint is defined at the layer level,
!!   in the form of an index between 0 and 1, and assumed to be independent of the others. The resulting index poussrac is the product of elementary indices:
!!   humirac defines the effect of soil dryness, taking account of the plant's sensitivity to this effect. efda defines the effect of soil compaction through
!!   bulk density. The anoxia index of each soil layer anox(iz) is assigned the value of 1 if the horizon has reached saturation; it is associated with the
!!   sensitivity of the plant to water logging sensanox. efnrac defines the effect of mineral nitrogen, which contributes to the root distribution in the
!!   layers with high mineral nitrogen content. It depends on the specific parameters minazorac, maxazorac and minefnra which characterize the sensitivity
!!   of plant root growth to the mineral nitrogen content in the soil. This last constraint is optional and can be inactivated in the model.
!>  - Senescence
!!   A thermal duration in degree days (stdebsenrac) defines the lifespan of roots. Thus, the history of root production per layer is memorized in order
!!   to make disappear by senescence the portion of roots stdebsenrac set earlier. The profile of dead roots is lracsenz while the corresponding total
!!   amount is lracsentot.
!>  - Root density profiles
!!   The living root density profile is rl, while the total amount is rltot. For water and nitrogen absorption, an efficient root length density (lracz)
!!   is calculated by applying the threshold lvopt (by default equals 0.5 cm cm-3) to the total root length density, RL.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine densiteVraieRacinaire(n,nbCouches,dacouche,hur,humin,anox,nstoprac,P_codazorac,                &
                                 P_minazorac,P_maxazorac,P_minefnra,P_codtrophrac,P_coefracoupe,          &
                                 P_daseuilbas,P_daseuilhaut,P_dacohes,nit,amm,P_lvopt,profsol,nrec,       &
                                 msrac,nsencourprerac,nger,nlev,codeinstal,poussracmoy,zrac,              &
                                 rl,somtemprac,dtj,deltaz,idzrac,efdensite_rac,ndebsenrac,                &
                                 precrac,drl,lracz,lracsenz,cumlracz,cumflrac,flrac,                      &
                                 cumlraczmaxi,racnoy,lracsentot,masectot,rltot,sioncoupe,                 &
                                 P_contrdamax,P_sensrsec,P_sensanox,P_stlevamf,P_stamflax,P_lvfront,      &
                                 P_laicomp,P_adens,P_bdens,P_draclong,P_vlaimax,P_longsperac,P_debsenrac, &
                                 P_codedyntalle,drlsenmortalle,P_profsem,densite,P_msresiduel,            &
                                 lai_veille,msrac_veille,rl_veille,dltams,repracmin,                      &
                                 repracmax,kreprac,efda,efnrac_mean,humirac_mean,humirac_z,efnrac_z,rlj   &
                                 ,masec,P_codemortalracine,dltmsrac_plante)

USE Divers, only: F_humirac, escalin
USE Messages

  implicit none


  integer, intent(IN)    :: n  
  integer, intent(IN)    :: nbCouches  
  real,    intent(IN)    :: dacouche(nbCouches)  
  real,    intent(IN)    :: hur(nbCouches)  
  real,    intent(IN)    :: humin(nbCouches)  
  real,    intent(IN)    :: anox(nbCouches)  
  integer, intent(IN)    :: nstoprac  

  integer, intent(IN)    :: P_codazorac  !> // PARAMETER // activation of the nitrogen influence on root partitionning within the soil profile  // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: P_minazorac  !> // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning // kg N ha-1 mm-1 // PARPLT // 1 
  real,    intent(IN)    :: P_maxazorac  !> // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning  // kg N ha-1 mm-1 // PARPLT // 1 
  real,    intent(IN)    :: P_minefnra  !> // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning // SD // PARPLT // 1 
  integer, intent(IN)    :: P_codtrophrac  !> // PARAMETER // trophic effect on root partitioning within the soil // code 1/2/3 // PARPLT // 0 
  real,    intent(IN)    :: P_coefracoupe  !> // PARAMETER // coefficient to define the proportion of dying roots after cut (grass) // SD // PARAMV6/PLT // 1 

  real,    intent(IN)    :: P_daseuilbas  !> // PARAMETER // Threshold of bulk density of soil below that the root growth is not limited // g cm-3 // PARAM // 1 
  real,    intent(IN)    :: P_daseuilhaut  !> // PARAMETER // Threshold of bulk density of soil below that the root growth  no more possible // g cm-3 // PARAM // 1 
  real,    intent(IN)    :: P_dacohes  !> // PARAMETER // bulk density under which root growth is reduced due to a lack of cohesion d // g cm-3 // PARAM // 1 
  real,    intent(IN)    :: nit(nbCouches)  
  real,    intent(IN)    :: amm(nbCouches)  
  real,    intent(IN)    :: P_lvopt  !> // PARAMETER // Optimum root density // cm root.cm-3 soil // PARAM // 1 
  real,    intent(IN)    :: profsol  

  integer, intent(IN)    :: nrec  
  real,    intent(INOUT) :: msrac                       ! n    // OUTPUT // Estimated dry matter of the roots // t.ha-1
  integer, intent(INOUT) :: nsencourprerac  
  integer, intent(IN)    :: nger  
  integer, intent(IN)    :: nlev  
  integer, intent(IN)    :: codeinstal  
  real,    intent(OUT)   :: poussracmoy   !> // OUTPUT // "Average index of the effect of soil constraints on the rooting profile (option  true density )" // 0-1
  real,    intent(INOUT) :: zrac   !> // OUTPUT // Depth reached by root system // cm
  real,    intent(INOUT) :: rl(nbCouches)               ! n  
  real,    intent(INOUT) :: somtemprac  
  real,    intent(IN)    :: dtj(n)                      ! 1 à n    // OUTPUT // Daily efficient temperature for the root growing  // degree C.j-1
  real,    intent(IN)    :: deltaz   !> // OUTPUT // Deepening of the root front  // cm day-1
  real,    intent(IN)    :: idzrac  
  real,    intent(OUT)   :: efdensite_rac
  integer, intent(INOUT) :: ndebsenrac  
  real,    intent(IN)    :: precrac(nbCouches)  
  real,    intent(INOUT) :: drl(n,nbCouches)  
  real,    intent(INOUT) :: lracz(nbCouches)  
  real,    intent(INOUT) :: lracsenz(nbCouches)  
  real,    intent(OUT)   :: cumlracz   !> // OUTPUT // Sum of the effective root lengths  // cm root.cm -2 soil
  real,    intent(OUT)   :: cumflrac  
  real,    intent(INOUT) :: flrac(nbCouches)  
  real,    intent(OUT)   :: cumlraczmaxi  
  real,    intent(INOUT) :: racnoy  
  real,    intent(OUT)   :: lracsentot   !> // OUTPUT // Total length of senescent roots  // cm root.cm -2 soil
  real,    intent(IN)    :: masectot
  real,    intent(IN)    :: masec
  real,    intent(OUT)   :: rltot   !> // OUTPUT // Total length of roots  // cm root.cm -2 soil
  logical, intent(IN)    :: sioncoupe  

  real,    intent(IN)    :: P_contrdamax  !> // PARAMETER // maximal root growth reduction due to soil strenghtness (high bulk density) // SD // PARPLT // 1 
  real,    intent(IN)    :: P_sensrsec  !> // PARAMETER // root sensitivity to drought (1=insensitive) // SD // PARPLT // 1 
  real,    intent(IN)    :: P_sensanox  !> // PARAMETER // anoxia sensitivity (0=insensitive) // SD // PARPLT // 1 
  real,    intent(IN)    :: P_stlevamf  !> // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1 
  real,    intent(IN)    :: P_stamflax  !> // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1 
  real,    intent(IN)    :: P_lvfront  !> // PARAMETER // Root density at the root front // cm root.cm-3 soil // PARPLT // 1 
  real,    intent(IN)    :: P_laicomp  !> // PARAMETER // LAI from which starts competition inbetween plants // m2 m-2 // PARPLT // 1 
  real,    intent(IN)    :: P_adens  !> // PARAMETER // Interplant competition parameter // SD // PARPLT // 1 
  real,    intent(IN)    :: P_bdens  !> // PARAMETER // minimal density from which interplant competition starts // plants m-2 // PARPLT // 1 
  real,    intent(IN)    :: P_draclong  !> // PARAMETER // Maximum rate of root length production // cm root plant-1 degree.days-1 // PARPLT // 1 
  real,    intent(IN)    :: P_vlaimax  !> // PARAMETER // ULAI  at inflection point of the function DELTAI=f(ULAI) // SD // PARPLT // 1
  real,    intent(IN)    :: P_longsperac  !> // PARAMETER // specific root length // cm g-1 // PARPLT // 1 
  real,    intent(IN)    :: P_debsenrac  !> // PARAMETER // Sum of degrees.days defining the beginning of root senescence (life-time of a root) // degree.days // PARPLT // 1 
  integer, intent(IN)    :: P_codedyntalle  !> // PARAMETER // Activation of the module simulating tiller dynamic: yes (1), no (2) // code 1/2 // PARAMV6/PLT // 0 
  real,    intent(IN)    :: drlsenmortalle   !> // OUTPUT // Root biomass corresponding to dead tillers // t ha-1.j-1

  real,    intent(IN)    :: P_profsem  !> // PARAMETER // Sowing depth // cm // PARTEC // 1 
  real,    intent(IN)    :: densite   !> // OUTPUT // Actual sowing density // plants.m-2
  real,    intent(IN)    :: P_msresiduel  !> // PARAMETER // Residual dry matter after a cut // t ha-1 // PARTEC // 1 

  real,    intent(IN)    :: lai_veille                  ! lai(n-1)  
  real,    intent(IN)    :: msrac_veille                ! msrac(n-1)  
  real,    intent(IN)    :: rl_veille(nbCouches)        ! n-1  
  real,    intent(IN)    :: dltams   !> // OUTPUT // Growth rate of the plant  // t ha-1.j-1

  real,    intent(IN)    :: repracmin  
  real,    intent(IN)    :: repracmax  
  real,    intent(IN)    :: kreprac  
  real,    intent(INOUT)   :: efda      !> // OUTPUT // effect of soil compaction through bulk density // 0-1
  real,    intent(INOUT)   :: efnrac_mean    !> // OUTPUT // effect of mineral nitrogen, which contributes to the root distribution in the layers with high mineral nitrogen content. // 0-1
  real,    intent(INOUT)   :: humirac_mean !> // OUTPUT // soil dryness // 0-1
  real,    intent(INOUT)   :: humirac_z(nbcouches)
  real,    intent(INOUT)   :: efnrac_z(nbcouches)
  !DR 24/07/2013 ajout de rlj en sortie
  real,    intent(OUT)     :: rlj !> // OUTPUT // roots length growth rate  // m.d-1
  ! DR 06/05/2015 je rajoute un code pouyr tester la mortalitéé des racines
  integer, intent(IN)      :: P_codemortalracine !< // PARAMETER // masec servant a calculer les racines mortes a la coupe  : masec (1), masectot (2) // code 1/2 //PARAMv6 // 1

  real,    intent(OUT)     ::  dltmsrac_plante  !<// OUTPUT // pour sorties ArchiSTICS: biomasse journaliere allouee aux racines en g / m²sol / plante




!: Variables locales
  real :: poussrac(nbCouches)  
  real :: pondetot(nbCouches)  
  real :: azo  
  real :: daz  
  real :: drliz  
  real :: drlsen  
  real :: efanoxd  
  real :: efnrac
!  real :: efda
  real :: nouvrac  
  real :: reprac  
!  real :: rlj
  real :: rlj1  
  real :: sumrl  
  real :: stsen  
  real :: ponderation  
  real :: urac  
  integer :: prop  
  integer :: nsencourac  
  integer :: iz  
  integer :: i  
  real ::tot


! NB le 19/02/08
  real :: durvieracine  

!: les FONCTIONS
  real :: epcouche  

!write(245,*)'densirac profsem',drlsenmortalle,P_profsem,densite



      !: msrac au jour de recolte
      if (n == nrec) msrac = msrac_veille

      !: 17/06/04 - Les3 - (emma): recuperation de nsencourac
      nsencourac = nsencourprerac

      !: NB & PB - le 25/05/2004: On continue la croissance racinaire après la récolte
      !-                          si autorisé par nstoprac.
      !--if (nger == 0.or.nrec > 0) return
      if (nger == 0) return
      if (nlev == 0) prop = 0
      if (nlev > 0) prop = 1
      if (nlev == nger .and. nger > 0 .and. n == nger) prop = 0
      if (codeinstal == 1) prop = 1

      !: Calcul de poussrac
      ponderation = 0.
      poussracmoy = 0.
      sumrl = 0.

      humirac_mean = 0.
      efnrac_mean = 0.
      do iz = 1, int(zrac)+1

        !: poussrac = effet Da x effet H2O x effet N x effet T° x effet anoxie


        !: 1. Effet Da (contrainte à la pénétration racinaire) : efda
        !------------------------------------------------------------
        !- Fonction Jones et al. 1991  + thèse de B. Rebière, adaptée avec la densité apparente
        !- P_daseuilbas et P_daseuilhaut : seuils min et max de densité apparente
        !- P_contrdamax : taux maximal de reduction de vitesse de croissance racinaire
        ! TODO : que faire qd zrac = 0 ? j'ai mis un bornage pour éviter les débordements de tableaux
        daz = dacouche(max(1,int(zrac)))
        efda = ESCALIN(daz,P_daseuilbas,P_daseuilhaut,1.,P_contrdamax)
        if (daz < P_dacohes) efda = daz / P_dacohes

        !: 2. Effet H2O (sensibilité à l'anoxie) : P_sensanox
        !--------------------------------------------------
        !- NB - le 18/12/01: efpfz remplacé par humirac
        poussrac(iz) = F_humirac(hur(iz),humin(iz),humin(iz),P_sensrsec) * efda * (1. - (anox(iz) * P_sensanox))
        humirac_mean = humirac_mean + F_humirac(hur(iz),humin(iz),humin(iz),P_sensrsec)
            !DR 23/07/2013 j'ajoute le humriac par couche pour Simtraces
        humirac_z(iz)=F_humirac(hur(iz),humin(iz),humin(iz),P_sensrsec)

        !: 3. Effet N sur la répartition des racines (option non activée) : efnrac
        !-------------------------------------------------------------------------
        if (P_codazorac == 1) then
          azo = nit(iz) + amm(iz)
          efnrac = ESCALIN(azo,P_minazorac,P_maxazorac,P_minefnra,1.)
          ! dr 23/07/2013 ajout d'une varaible profil pour simtraces
          efnrac_z(iz)=efnrac
          efnrac_mean= efnrac_mean + efnrac
          poussrac(iz) = poussrac(iz) * efnrac
        endif

        if (iz >= int(P_profsem) .and. iz <= int(zrac)+1) then
          poussracmoy = poussracmoy + poussrac(iz)
        endif

        !: Cumul de rl
        sumrl = sumrl + rl_veille(iz)
      end do
      humirac_mean = humirac_mean/(int(zrac)+1)
      if (P_codazorac == 1) then
        efnrac_mean = efnrac_mean/(int(zrac)+1)
      else
        efnrac_mean = 1.0  ! dr 14/06/2013 cca ne joue pas
      endif

      !: Nb - le 13/02/2003:
      !- Calcul d'une pondération incluant présence de racine et contraintes sol

      do iz = 1, int(zrac)+1


        !: par défaut pondetot(iz) = 1.
        pondetot(iz) = 1.
        !: PB - 24/12/2004: Attention, si poussracmoy = 0, division par zéro !
        !-                  ajout d'un test sur poussracmoy = 0.
        if (sumrl > 0 .and. poussracmoy /= 0.0) then
          pondetot(iz) = rl_veille(iz) / sumrl * poussrac(iz) / poussracmoy
        endif
        !: PB - 24/12/2004: pondetot = 0 si poussracmoy = 0
        if (poussracmoy <= 0.) pondetot(iz) = 0.
        ponderation = ponderation + pondetot(iz)
      end do

      if (poussracmoy <= 0 .and. zrac == P_profsem) then
        call EnvoyerMsgHistorique(30)

! TODO: gestion de l'échange entre plante et sol nu...
!!!    ! DR 26/09/07 bon maintenant quand le sol est trop sec faut qu'on fasse du sol nu
!!!    !        stop
!!!            codeplante_ori = P_codeplante
!!!            P_codeplante = 'snu'
!!!            fplt_ori = P_fplt
!!!            P_fplt = 'solnu.plt'
!!!    !    iplt_ori = iplt
!!!            call lecplant
!!!            densite = 0.
!!!            P_vlaimax = 0.
!!!            group = -2
!!!            plante_ori = .FALSE.
      endif

      !: Pondération de poussrac pour respecter un cumul de 1
      !- pour l'ensemble du profil
      poussracmoy = max(1.,poussracmoy)


      !: NB - 19/02/2008: mort d'une partie des racines lors de la fauche des cultures fourragères
      !- P_coefracoupe permet de pondérer la mortalité des racines par espèce (entre 0 et 1)
      !- Dr - 22/02/08: passé en parametre dans paramv6.par
      !--P_coefracoupe = 0.5
      if (sioncoupe) then
        !--durvieracine = 0.0
        !--durvieracine = P_debsenrac*P_msresiduel(numcoupe)/masectot*P_coefracoupe
        !: NB - 13/03/08: On met une courbe à la place de la droite P_coefracoupe est la courbure .
        !- mettre à 0.1 pour le moment
        ! DR 19/02/215 pour FR on test en faisant jouer masec au lieu de masectot pour etre moins severe sur la proportion de
        ! racines à faire mourir à la coupe
        ! voir ce qu'on garde et enlever l'autre
        ! DR 06/05/2015 j'ajoute n code pour tester masec ou masectot pour la mort des racines à la coupe
        if(P_codemortalracine.eq.1)then
          durvieracine = P_debsenrac * (1 - exp(-P_coefracoupe * P_msresiduel / masec))
        else
          durvieracine = P_debsenrac * (1 - exp(-P_coefracoupe * P_msresiduel / masectot))
        endif

      else
        durvieracine = P_debsenrac
      endif


      !: NB - le 13/02/2003
      !- GROS BUG : pourquoi diviser par poussracmoy ?
      ! DR et ml 24/07/2013 On reactive sinon ce n'est plus une moyenne et il n'est pas utilisé apres
      poussracmoy = poussracmoy/(int(zrac)-int(P_profsem)+2)

      !: Calcul de la croissance totale de racines (rlj en cm/m2/jour)
      !- entre la germination et le stade nstoprac.

      !- définition de sommes de degrés.jours racines
      somtemprac = somtemprac + dtj(n)
      if (n == nlev) somtemprac = 0.
      if (nstoprac == 0) then
        !: Définition d'une unité de dl racinaire urac
        !- somme de degré.jours racine
        urac = min(1. + (2. * somtemprac / (P_stlevamf + P_stamflax)), 3.)
        if (nlev == 0) urac = 1.
        !: la longueur totale de racine émise par jour (rlj)
        !- suit une logistique de façon similaire au LAI
        !- NB - le 28/10/01:
        !- 07/02/08: on integre ENFIN les modifs de Samuel (vive lui)
        !- essai de rendre nouvrac continu pour Samuel
        !- 20/02/07:
        !-if (int(zrac)-int(zrac-deltaz) <= int(deltaz)) then
        !-  multlvfront = int(deltaz)
        !-else
        !-  multlvfront = int(deltaz)+1
        !-endif
        !-if (nlev == 0) multlvfront = max0(1,multlvfront)
        !-nouvrac = P_lvfront*multlvfront
        nouvrac = P_lvfront*deltaz


        !: Introduction de l'indice de stress de densité racinaire idzrac / NB - le 06/06
        efanoxd = 1.-(1.-idzrac)*P_sensanox

        !: NB - 10/03/02:
        !- Calcul du facteur densité efdensite_rac actif à partir de P_laicomp
        !- Calcul de l'effet densité sur la longueur racinaire
        ! DR  et ML 20/04/2016 on rajoute une variable specifique pour les racines car on a deja un efdensite pour les feuilles
        !               qui peut être calculee en fonstion de la densite equivalente dans le cas des cultures associees
        efdensite_rac = 1.
        if (urac >= 1.) then
          if (lai_veille < P_laicomp) then
            efdensite_rac = 1.
          else
            !: domi - 02/07/2002: pb si GEL total densite = 0 et pb de log(0)
            if (densite == 0) then
              efdensite_rac = 0.
            else
              efdensite_rac = min(exp(P_adens * (log(densite / P_bdens))), 1.)
            endif
          endif
        else
          efdensite_rac = min(exp(P_adens * (log(densite / P_bdens))),1.)
        endif

        rlj = (P_draclong / (1. + exp(5.5 * (P_vlaimax - urac))) * efdensite_rac * densite *dtj(n) * efanoxd) + (nouvrac * 1.e4)


        !: De la germination à la levée, il n'y a qu'une croissance du front racinaire
        !- Nb - le 17/02/2003: suppression de coderac

        if (P_codtrophrac /= 3) then
          !: Option trophique
          ! *- par calcul d'une fonction de répartition reprac = souterrain/total
          ! *- la longueur de racine au niveau du front est soustraite de la
          ! *- longueur produite
          ! ** ML le 25/05/2007 - les paramètres initiaux repracmin, repracmax et kreprac
          ! *- ont changé de noms dans le fichier plante: P_repracpermin, P_repracpermax et P_krepracperm
          ! *- si la liaison trophique est permanente (P_codtrophrac = 1) et
          ! *- P_repracseumin, P_repracseumax et P_krepracseu si la liaison est par seuils (P_codtrophrac = 2)
!!!          if (P_codtrophrac == 1) then
!!!            repracmax = P_repracpermax
!!!            repracmin = P_repracpermin
!!!            kreprac = P_krepracperm
!!!          endif
!!!
!!!          if (P_codtrophrac == 2) then
!!!            repracmax = P_repracseumax
!!!            repracmin = P_repracseumin
!!!            kreprac = P_krepracseu
!!!          endif

          reprac = (repracmax-repracmin) * (exp(-kreprac * (urac - 1.))) + repracmin

          !: PB - 06/01/2005: On multiplie par P_longsperac plutot que diviser (modifs Nadine du 30/12/2004)
          rlj1 = reprac/(1.-reprac) * P_longsperac * 1.e2 * dltams



      ! DR et ML le 12/10/2015 d apres nos calculs rlj1 est en 1.e4 cm racine/m² sol (et non en cm racine/cm sol)


!write(1236,*)'1',n,rlj1,nouvrac

          if (rlj1 < nouvrac * 1.e4) nouvrac = rlj1 * 1.e-4
          if (P_codtrophrac == 1) then
            rlj = rlj1
          else
            if (rlj >= rlj1) rlj = rlj1
          endif
        endif


        if (nlev == 0) rlj = nouvrac*1.e4

      else
        rlj = 0.
      endif



      ! DR et ML le 12/10/2015 pour sorties ArchiSTICS: biomasse journaliere allouee aux racines en g / m²sol / plante
      dltmsrac_plante=rlj/P_longsperac/densite

      !: Changement d'unité de rlj en cm racine/cm2 sol/jour
      rlj = rlj * 1.e-4


     ! write(1236,*)'2',n,rlj,nouvrac
      ! DR et ML le 12/10/2015 d apres nos calculs rlj est en cm racine/m² sol (et non en cm racine/cm2 sol!)

      !: Jour du début de sénéscence racinaire
      !- NB - 19/02/08: durvieracine remplace P_debsenrac
      !--if (somtemprac > P_debsenrac .and. ndebsenrac == 0) then
      if (somtemprac > durvieracine .and. ndebsenrac == 0) then
        ndebsenrac = n
      endif

      !: Hypothèse de longueur spécifique de 18e3 cmg-1 pour le blé
      !- et 9e3 cmg-1 pour le maïs (Gregory et al., 1997)
      !- pour calculer une allocation racinaire en sortie
      !--xlinmas = 0.9
      !--xlinmas = 1.8
      !--if (dltams(ens,n) /= 0.0) then
      !--  allocrac = rlj/xlinmas/(rlj/xlinmas+dltams(ens,n)*100.)
      !--endif

      !: Début de la boucle profondeur
      !--do 60 iz = int(P_profsem),int(zrac)+1
      !- nb &sb - 26/03/07 : ATTENTION, l'indice de fin de boucle a été modifié,
      !- cela est susceptible de générer des effets de bords non désirés et +/- génants !
      ! A VALIDER !
      tot=0


      do iz = int(P_profsem), int(zrac)

        !: Si on est après le stade nstoprac alors drliz = 0.
        if (nstoprac > 0) then
          drliz = 0.
        else

          !: Distribution de la longueur racinaire au prorata de poussrac
          !- (option prop = 0)
          if (prop == 0) then
            !: NB - 13/02/2003 : Répartition équitable sans tenir compte des contraintes sol
            !--drliz = rlj/(int(zrac)-int(P_profsem)+2)
            !- SB - 02/03/2007 : Sur les couches limites, on calcule drliz au pro-rata de la longueur de racine par
            !                    rapport à l'épaisseur de la couche, pour rendre ce calcul continu.
            drliz = rlj * epcouche(iz,P_profsem,zrac) / (zrac - P_profsem + 1)


          endif

          !: Distribution de la longueur racinaire au prorata de poussrac
          !- et des racines présentes (option prop = 1)
          if (prop == 1) then
            !: Calcul de la proportion de racines présentes
            if (n > nger.or.codeinstal == 1) then
              if (precrac(iz) <= 0 .and. iz > (zrac - deltaz)) then
                !: SB - 02/03/2007 : Modification en accord avec ce qui est fait plus haut pour le calcul de nouvrac
                !-if (multlvfront > 0.) then
                if (deltaz > 0.) then
                  !-drliz = nouvrac/multlvfront
                  drliz = nouvrac / deltaz


                  ! dr 24/07/2013 on avait un petit pb (on divise par deltaz reel et on affecte ca sur iz entier), une partie des racines nouvelles nouvrac n'etaient pas prise en compte
                  ! dr 24/07/2013 j'affecte à la dernier couche (celle de zrac) le reliquat de nouvrac moins la somme des couches d'avant (tot) pour que le bilan soit bon
                 if(iz==int(zrac))drliz = nouvrac- tot
                  tot=tot+drliz
                  !write(1236,*)n,iz,zrac,'rliz',drliz,nouvrac,deltaz,'nouvrac',nouvrac,'tot',tot
                else
                  drliz = 0.
                endif
              else
                if (ponderation /= 0.) then
                  drliz = (rlj - nouvrac) * pondetot(iz) / ponderation
                else
                  drliz = 0.
                endif
              endif
            endif
          endif
          ! fin du if 'croissance'
        endif

        !: Calcul de la sénescence racinaire : P_debsenrac * racines émises  au jour j-debsen
        if (ndebsenrac == 0) then
          drlsen = 0
        else
          !: Recherche du jour de production de ce qui doit disparaitre
          !- NB - 19/02/08 : durvieracine remplace P_debsenrac
          stsen = 0.
          do i = 1, n
            stsen = stsen + dtj(n-i)
            !--if (stsen >= P_debsenrac) then
            if (stsen >= durvieracine) then
              nsencourac = n-i
              EXIT ! on sort de la boucle
            endif
          end do

          !: Cumul de biomasse entre nsencourpre et nsencourac
          if (nsencourprerac < nsencourac) then
            drlsen = 0.
            do i = nsencourprerac+1, nsencourac
              drlsen = drlsen + drl(i,iz)
            end do
          else
            drlsen = 0.
          endif
        endif

        drl(n,iz) = drliz

      ! Cumul de racine dans chaque couche
        if (n == nger .and. codeinstal == 0) rl(iz) = drliz

        if (n > nger .or. codeinstal == 1) then


      ! DR et ML et SYL 15/06/09
      ! ************************
      ! introduction de la fin des modifications de Sylvain (nadine et FR)
      ! dans le cadre du projet PERMED
      ! ####
      ! SYL 120907 Mortalité d'une proportion de racine en lien avec la mortalité de talles
      ! DR et ML et SYL 16/06/09
      ! on ajoute la condition sur P_codedyntalle
        if (P_codedyntalle == 1)then
          drlsen = drlsen + rl_veille(iz) * drlsenmortalle
        endif
      ! ####
      ! DR et ML et SYL 15/06/09 FIN introduction de la fin des modifications de Sylvain

          rl(iz) = rl_veille(iz) + drliz - drlsen



        !: sb - 06/03/2007 :
        !- Lorsque la quantité de racine devient très faible, c'est que les racines ont disparu.
        !- En pratique, dans ce cas, la longueur de racine ne devient jamais exactement égale à 0
        !- en raison de la façon de calculer la senescence et des erreurs d'arrondis. Cette longueur est
        !- mise à 0 seulement dans les cas où le calcul la donne négative. Pour éviter les imprécisions
        !- et instabilité numériques que cela génère, on a choisi de fixer la longueur de racine à 0 dès
        !- qu'elle est inférieure à une quantité très faible.
        !--if (rl(n,iz) < 0.) then
          !-- prob. avec 1.d -- if (rl(n,iz) < 1.d - 10) then
          if (rl(iz) < 1.d-10) then
            rl(iz) = 0.
            drlsen = rl_veille(iz) + drliz
          endif
          lracsenz(iz) = lracsenz(iz) + drlsen
        endif

        !: Passage du tableau 2 au tableau 1
        lracz(iz) = rl(iz)

      end do ! fin boucle profondeur


      cumlracz = 0.
      cumflrac = 0.
!      write(245,*)'densirac',P_profsem,zrac
      do iz = int(P_profsem), int(zrac)+1


        flrac(iz) = min(lracz(iz),P_lvopt) / P_lvopt
        ! 17/06/04 - les 3 : Pour l'absorption d'eau la limite est tj humin
        !--lracz(iz) = min(lracz(iz),P_lvopt) * HUMIRAC(hur(iz),humin(iz),humin(iz),P_sensrsec)
        lracz(iz) = min(lracz(iz),P_lvopt) * F_humirac(hur(iz),humin(iz),humin(iz),0.)
        cumlracz = cumlracz+lracz(iz)
        ! NB - le 05/06
        ! DR et marie : On a un pb sur le caulcul de cumflrac du coup on a des racnoy > 1
        ! et des exofac > 1 il faut supprimer P_lvopt
        !--cumflrac = cumflrac + flrac(iz) * P_lvopt
        cumflrac = cumflrac + flrac(iz)
        cumlraczmaxi = cumlracz
        !: variable BR pour exces d'eau
        racnoy = racnoy + (flrac(iz) * anox(iz))
      end do
!      write(245,*)'densirac',flrac

      !: Test sur la densité racinaire
      if (cumlracz <= 0 .and. nrec == 0) call EnvoyerMsgHistorique(31,n)
      nsencourprerac = nsencourac
      lracsentot = 0.
      rltot = 0.
      do iz = 1, int(profsol)
        lracsentot = lracsentot + lracsenz(iz)
        !: NB - 20/02/2008
        if (sioncoupe) lracsenz(iz) = 0.0
        rltot = rltot + rl(iz)
      end do

!     write(1236,*)n,' fin rlj',rlj,'rltot',rltot
      !: 30/12/04 : lonsperac cm g-1, rltot en cm racine cm-2 sol
      !- msrac comprend à la fois la matière racinaire vivante et morte
      msrac = (rltot + lracsentot) / P_longsperac * 100


      ! DR et ML le 12/10/2015 d apres nos calculs msrac est bien en t/ha sachant que,
      ! toujours d apres nos calculs rltot est en cm racine/m² sol (et non en cm racine/cm2 sol!)
      ! Dans le code et dans le bouquin on annoce rltot (et donc rlj) en cm racine /cm² sol, alors que dans le code on calcule d'autres unités. Fort heureusement on retombe sur nos pattes pour msrac qui est bien en t/ha (mais grâce à la compensation de deux erreurs).
      ! Il faudrait faire vérifier cela par d'autres référents.

      !: NB - 20/02/08 : recalcul de zrac
      if (sioncoupe) then
        do iz = int(zrac), 10, -1
          if (rl(iz) < 0.1 * P_lvfront) then
            zrac = iz
          endif
        end do
      endif

!      if (sioncoupe) write(*,*) 'coupe ', rltot,lracsentot,zrac,deltaz
!      if (.not.sioncoupe) write(*,*) 'not coupe ', rltot,lracsentot,zrac,deltaz

return
end subroutine densiteVraieRacinaire
 
 
