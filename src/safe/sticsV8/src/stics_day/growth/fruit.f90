!> This module simulates the yield formation for indeterminate growing plants
!> - Stics book paragraphe 3.1.2, page 44-46
!>
!> These species go on growing leaves while producing and growing harvested organs (fruits) during a period of time. There is thus a trophic interaction between
!! the growth of various groups of organs and among successive cohorts of harvested organs that is accounted for in STICS by the source/sink approach using
!! the notion of trophic stress. Both processes of organ setting and filling are concerned, assuming that abortion cannot occur during the filling phase.
!! The simulation technique adopted in STICS was inspired from the "boxcartrain" technique (Goudriaan, 1986) that is used in the TOMGRO model (Jones et al., 1991).
!! During growth, the fruits go through nboite compartments corresponding to increasing physiological ages.  The time fruits spend in a compartment depends on
!! temperature. In each compartment, fruit growth is equal to the product of a "sink strength" function and the source-sink ratio. The fruit sink strength
!! is the derivative of a logistic function that takes the genetic growth potential of a fruit into consideration (Bertin and Gary, 1993).
!>  - Fruit setting
!!   Fruits are set between the idrp stage and the inou stage (end of setting), defined by the stdrpnou phasic course. If this setting period lasts a long time,
!!   then the number of simultaneous compartments (i.e. fruits of different ages) is great which indicates that there must be agreement between the values
!!   of stdrpnou and nboite. During this setting period, on each day, the number of set fruits (nfruitnou) depends on afruitpot, a varietal parameter expressed
!!   as the potential number of set fruits per inflorescence and per degree.day, the daily development rate (UPVT), the number of inflorescences per plant(nbinflo),
!!   the plant density (densite), the trophic stress index (spfruit) and the frost stress index acting on fruits from flowering (fgelflo).
!!   The introduction of the notion of inflorescence (group of fruits) into the model is only useful when technical or trophic regulation occurs at the
!!   inflorescence level (in grapevines for example). If the number of inflorescences is more than 1 (in the case of vines, inflorescences=bunches),
!!   it can either be prescribed (nbinflo), or calculated as a function of the trophic status of the plant at an early stage (we have chosen iamf).
!!   In the latter case, nbinflo is calculated using the pentinflores  and inflomax parameters. Pruning is not accounted for in this calculation.
!>  - Fruit filling
!!   The time spent by each fruit in a given compartment is dureefruit/nboite, where dureefruit is the total duration of fruit growth expressed in
!!   developmental units.  In the last box (or age class), the fruits no longer grow and the final dry mass of the fruit has been reached: the fruit is assumed
!!   to have reached physiological maturity. Each day, in each growth compartment (K), the fruit growth (croifruit) depends on the number of fruits
!!   in the compartment (nfruit) multiplied by the growth of each fruit, i.e. the elementary fruit sink strength (fpft), the trophic stress index (sourcepuits)
!!   and the thermal stress index (ftempremp).
!!   There are two successive phases in fruit growth; the first corresponds to a cell division phase while the second is devoted to expansion of the cells already set.
!!   In order to account for this double dynamics, the fruit potential cumulative growth is defined as the summation of two functions:
!>      - an exponential type function describing the cell division phase (using the parameters CFPFP and DFPFP)
!>      - a logistic type function describing the cell elongation phase (using the parameters AFPFP and BFPFP)
!>
!!   pgrainmaxi is the genetic-dependent maximal weight of the fruit and dfr stands for the fruit development stage of each age class,
!!   varying between 0 and 1; it is calculated for each age class (K) in a discrete way.
!!   Then the daily fruit sink strength function (fpft) is calculated for each age class, accounting for the duration of fruit growth from setting to maturity,
!!   expressed in developmental units (dureefruit).
!!   If allocation to fruits (allocfruit variable) exceeds the allocfrmax threshold, the sourcepuits variable is reduced in proportion to the
!!   allocfruit/allocfrmax ratio. In the last box, the fruits are ripe and stop growing. The number of fruits present on the plant or fruit load is
!!   chargefruit (see sortie.f90). If the codefrmur is 1, then the chargefruit variable will take account of the fruits in the last box (ripe);
!!   if not, it will only take account of the (N-1) first boxes.
subroutine fruit(n,namf,P_codcalinflo,P_pentinflores,densite,P_resperenne0,cumdltares,P_inflomax,ndrp,P_nbcueille,nrec,       &
                 P_nboite,P_dureefruit,fpv,dltams,dltaremobil,remobilj,P_spfrmin,P_spfrmax,P_afruitpot,upvt,fgelflo,          &
                 P_codazofruit,P_codeinnact,inns,nnou,P_codeclaircie,nb_eclair,neclair,P_nbinfloecl, &
                 P_codetremp,tcultmin,     &
                 tcultmax,P_tminremp,P_tmaxremp,nbrecolte,rdtint,P_allocfrmax,nmat,P_afpf,P_bfpf,P_cfpf,P_dfpf,pfmax,         &
                 P_nbinflo,nfruit,pdsfruit,fpft,sourcepuits,spfruit,nbfruit,            & !INOUT
                 nfruitnou,nbfrote,devjourfr,cumdevfr,pousfruit,ftempremp,nbj0remp,                                 &
                 dltags,frplusp,ircarb,magrain,allocfruit,masec,compretarddrp,pdsfruittot)

  USE Messages

  implicit none

  integer, intent(IN)    :: n  
  integer, intent(IN)    :: namf  
  integer, intent(IN)    :: P_codcalinflo  !> // PARAMETER // option of the way of calculation of the inflorescences number  // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: P_pentinflores  !> // PARAMETER // parameter of the calculation of the inflorescences number  // SD // PARPLT // 1 
  real,    intent(IN)    :: densite   !> // OUTPUT // Actual sowing density // plants.m-2
  real,    intent(IN)    :: P_resperenne0  !> // PARAMETER // initial reserve biomass // t ha-1 // INIT // 1 
  real,    intent(IN)    :: cumdltares  
  real,    intent(IN)    :: P_inflomax  !> // PARAMETER // maximal number of inflorescences per plant // nb pl-1 // PARPLT // 1 
  integer, intent(IN)    :: ndrp  
  integer, intent(IN)    :: P_nbcueille  !> // PARAMETER // number of fruit harvestings // code 1/2 // PARTEC // 0 
  integer, intent(IN)    :: nrec  
  integer, intent(IN)    :: P_nboite  !> // PARAMETER // "Number of  box  or  age class  of fruits for the fruit growth for the indeterminate crops " // SD // PARPLT // 1
  real,    intent(IN)    :: P_dureefruit  !> // PARAMETER // total growth period of a fruit at the setting stage to the physiological maturity // degree.days // PARPLT // 1 
  real,    intent(IN)    :: fpv   !> // OUTPUT // Sink strength of developing leaves // g.j-1.m-2
  real,    intent(IN)    :: dltams   !> // OUTPUT // Growth rate of the plant  // t ha-1.j-1
  real,    intent(IN)    :: dltaremobil   !> // OUTPUT // Amount of perennial reserves remobilised // g.m-2.j-1
  real,    intent(IN)    :: remobilj   !> // OUTPUT // Amount of biomass remobilized on a daily basis for the fruits  // g.m-2 j-1
  real,    intent(IN)    :: P_spfrmin  !> // PARAMETER // minimal sources/sinks value allowing the trophic stress calculation for fruit onset // SD // PARPLT // 1 
  real,    intent(IN)    :: P_spfrmax  !> // PARAMETER // maximal sources/sinks value allowing the trophic stress calculation for fruit onset // SD // PARPLT // 1 
  real,    intent(IN)    :: P_afruitpot  !> // PARAMETER // maximal number of set fruits per degree.day (indeterminate growth) // nbfruits degree.day-1 // PARPLT // 1 
  real,    intent(IN)    :: upvt   !> // OUTPUT // Daily development unit  // degree.days
  real,    intent(IN)    :: fgelflo   !> // OUTPUT // Frost index on the number of fruits // 0-1
  integer, intent(IN)    :: P_codazofruit  !> // PARAMETER // option of activation of the direct effect of the nitrogen plant status upon the fruit/grain number // code 1/2 // PARPLT // 0 
  integer, intent(IN)    :: P_codeinnact  !> // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0 
  real,    intent(IN)    :: inns   !> // OUTPUT // Index of nitrogen stress active on growth in biomass // P_innmin to 1
  integer, intent(IN)    :: nnou
  integer, intent(IN)    :: nb_eclair
  integer, intent(IN)    :: P_codeclaircie  !> // PARAMETER // option to activate techniques of fruit removal  // code 1/2 // PARTEC // 0 
  integer, intent(IN)    :: neclair(1:nb_eclair)
  real,    intent(IN)    :: P_nbinfloecl(1:nb_eclair)  !> // PARAMETER // "number of inflorescences or fruits removed at  fruit removal  " // nb pl-1 // PARTEC // 1
  integer, intent(IN)    :: P_codetremp  !> // PARAMETER // option of heat effect on grain filling: yes (2), no (1) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: tcultmin  
  real,    intent(IN)    :: tcultmax   !> // OUTPUT // Crop surface temperature (daily maximum) // degree C
  real,    intent(IN)    :: P_tminremp  !> // PARAMETER // Minimal temperature for grain filling // degree C // PARPLT // 1
  real,    intent(IN)    :: P_tmaxremp  !> // PARAMETER // maximal temperature for grain filling // degree C // PARPLT // 1
  integer, intent(IN)    :: nbrecolte  
  real,    intent(IN)    :: rdtint(nbrecolte) ! 1 to nbrecolte
  real,    intent(IN)    :: P_allocfrmax  !> // PARAMETER // maximal daily allocation towards fruits // SD // PARPLT // 1 
  integer, intent(IN)    :: nmat  
  real,    intent(IN)    :: P_afpf  !> // PARAMETER // parameter of the  function logistic defining sink strength of fruits (indeterminate growth) : relative fruit age at which growth is maximal // SD // PARPLT // 1 
  real,    intent(IN)    :: P_bfpf  !> // PARAMETER // parameter of the logistic curve defining sink strength of fruits (indeterminate growth) :  rate of maximum growth proportionately to maximum weight of fruits // * // PARPLT // 1 
  real,    intent(IN)    :: P_cfpf  !> // PARAMETER // parameter of the first potential growth phase of fruit, corresponding to an exponential type function describing the cell division phase. // SD // PARPLT // 1 
  real,    intent(IN)    :: P_dfpf  !> // PARAMETER // parameter of the first potential growth phase of fruit, corresponding to an exponential type function describing the cell division phase. // SD // PARPLT // 1 
  real,    intent(IN)    :: pfmax  

  real,    intent(INOUT) :: P_nbinflo  !> // PARAMETER // imposed number of inflorescences  // nb pl-1 // PARPLT // 1   // OUTPUT // Number of inflorescences // SD
  real,    intent(INOUT) :: nfruit(P_nboite)    ! 1 to P_nboite    // OUTPUT // Number of fruits in box 5 // nb fruits
  real,    intent(INOUT) :: pdsfruit(P_nboite)  ! 1 to P_nboite    // OUTPUT // Weight of fruits in box 3 // g m-2
  real,    intent(INOUT) :: fpft   !> // OUTPUT // Sink strength of fruits  // g.m-2 j-1
  real,    intent(INOUT) :: sourcepuits   !> // OUTPUT // Pool/sink ratio // 0-1
  real,    intent(INOUT) :: spfruit   !> // OUTPUT // Index of trophic stress applied to the number of fruits // 0 to 1
  real,    intent(INOUT) :: nbfruit  
  real,    intent(INOUT) :: nfruitnou   !> // OUTPUT // Number of set fruits  // nb set fruits.m-2
  real,    intent(INOUT) :: nbfrote  
  real,    intent(INOUT) :: devjourfr  
  real,    intent(INOUT) :: cumdevfr  
  real,    intent(INOUT) :: pousfruit   !> // OUTPUT // Number of fruits transferred from one box to the next  // nb fruits
  real,    intent(INOUT) :: ftempremp  
  integer, intent(INOUT) :: nbj0remp   !> // OUTPUT // Number of shrivelling days //
  real,    intent(INOUT) :: dltags   !> // OUTPUT // Growth rate of the grains  // t ha-1.j-1
  real,    intent(INOUT) :: frplusp  
  real,    intent(INOUT) :: ircarb   !> // OUTPUT // Carbon harvest index // g  grain g plant-1
  real,    intent(INOUT) :: magrain(0:1) ! n-1 & n  
  real,    intent(INOUT) :: allocfruit   !> // OUTPUT // Allocation ratio of  assimilats to the  fruits 0 to 1  // 0-1
  real,    intent(INOUT) :: masec   !> // OUTPUT // Aboveground dry matter  // t.ha-1
  integer, intent(INOUT) :: compretarddrp  
  real,    intent(INOUT) :: pdsfruittot  

!: VARIABLES LOCALES
  integer :: nboiteremp  !>  
  integer :: i , k !>
  integer :: irecolte  
  real :: pdevfruit(P_nboite),nfruitp(P_nboite),pdsfruitp(P_nboite)  
  real :: croifruit  !>  
  real :: frmoins  !>  
  real :: frmoinsp  !>  
  real :: frplus  !>  
  real :: tefficace  

!: Pour la fonction FPF
  real :: FPF  
! dr 16/01/2015 correction du bug des fruits eclaircies qui passent dans les reserves
  real :: poids_tot_avant_eclai
  real :: poids_tot_apres_eclai
  logical :: flag_oneclairci
  real :: nbinfloecl


! NB le 30/05/06 on supprime la double croissance
! ** ML le 29/05/07 suppression de l'option de calcul de la
! *- croissance potentielle en 2 phases (maintenant obligatoire)
!  if (codefruitcroi /= 2) P_cfpf = 0.0



      ! calcul du nombre d'inflorescence au stade AMF
      if (n == namf) then
        if (P_codcalinflo == 2) then
        P_nbinflo = P_pentinflores / densite * (masec + max(P_resperenne0 - cumdltares, 0.0))
        P_nbinflo = min(P_nbinflo, P_inflomax)
        P_nbinflo = max(P_nbinflo, 1.0)
        endif
      endif


      !: On ne passe dans ce sousprg qu'en période de fructification
      if (ndrp == 0) return
      if (P_nbcueille == 1 .and. (nrec > 0 .and. n > nrec)) return

      !: On ne peut pas faire de fruits sans biomasse
      !- (pb quand parcours de développement incohérent)!
      if (masec <= 0.) then
        call EnvoyerMsgHistorique(350)
        !: Ajout compteur des jours de retard sur drp
        masec = 0.001
        compretarddrp = compretarddrp + 1
        !: Pb de marjorie on ecrit un compteur du nombre de jours de retard pour drp
        !call EnvoyerMsgHistorique(annee(ifwater__courant)) ! TODO: revoir ce message, affichage de l'année..
        call EnvoyerMsgHistorique(351,compretarddrp)
      endif

      !: Initialisation à drp
      if (n == ndrp .and. ndrp > 0) then
        do i = 1, P_nboite
          nfruit(i) = 0
          pdsfruit(i) = 0.0
        end do
      endif

! ** transfert dans CALAI
! *- la force de puits des organes végétatifs : fpv en g jour-1
! --          fpv = deltai*splai*1e4 / sbv

      !: Calcul du rapport source/puits
      !-la force de puits des fruits : fpft en g jour-1
      fpft = 0.0
      do i = 1,P_nboite-1
        pdevfruit(i) = float(i) / P_nboite
        fpft = fpft + nfruit(i) * FPF(pdevfruit(i),P_afpf,P_bfpf,P_cfpf,P_dfpf,pfmax,devjourfr)
      end do

      !: Ajout d'un pool remobilisable
      if (P_dureefruit <= 0.0) return

      if ((fpv+fpft) > 0.0) then
        !: NB - 13/06/06 : bug dans les unités g/m2 t/ha y compris
        ! tests betteraves
        sourcepuits = (dltams + dltaremobil + remobilj) * 1e2 / (fpv + fpft)
      else
        sourcepuits = 1.0
      endif
      if (sourcepuits > 1.0) sourcepuits = 1.0
      if (n == ndrp) sourcepuits = 1.0

      !: Autre fonction sourcepuits pour nfruitnou
      spfruit = (sourcepuits - P_spfrmin) / (P_spfrmax - P_spfrmin)
      spfruit = max(spfruit, 0.0)
      spfruit = min(spfruit, 1.0)

      !: Calcul du nombre de fruits noués /m2
      !- dégats de GEL
      !- P_afruitpot est en nb fruit/inflo/degré.jour
      nfruitnou = P_afruitpot * upvt * P_nbinflo * densite * spfruit * fgelflo

      !: NB - 06/05/02
      if (P_codazofruit == 2 .and. P_codeinnact == 1) then
        nfruitnou = nfruitnou * inns
      endif


      if (nnou > 0) nfruitnou = 0.0

      !: Effet du GEL sur les fruits en croissance
      if (fgelflo < 1.0) then
        do i = 1, P_nboite-1
          if (nfruit(i) <= 0.0) CYCLE
          pdsfruit(i) = pdsfruit(i) / nfruit(i)
          nfruit(i)   = nfruit(i) * fgelflo
          pdsfruit(i) = pdsfruit(i) * nfruit(i)
        end do
      endif
      flag_oneclairci=.FALSE.
      do k=1,nb_eclair
          if (n == neclair(k))then
               flag_oneclairci=.TRUE.
               nbinfloecl=P_nbinfloecl(k)
          endif
      enddo
      !: éclaircissage : on supprime les plus petits fruits
!      if (P_codeclaircie == 2 .and. n == neclair) then
      if (P_codeclaircie == 2 .and. flag_oneclairci) then
        if (P_nbinflo <= 1.0) then
!          nbfrote = 2.0*densite
          nbfrote = nbinfloecl * densite
        else
!          nbfrote = 1.0*densite*nbfruit /(P_nbinfloecl*densite)
          nbfrote = nbinfloecl / P_nbinflo * nbfruit
          nboiteremp = 0
          do i = 1,P_nboite-1
             if (nfruit(i) > 0.0) nboiteremp = nboiteremp + 1
          end do
          nbfrote = nbfrote / nboiteremp
          P_nbinflo = P_nbinflo - nbinfloecl
        endif

! dr 16/01/2015 correction du bug des fruits eclaircies qui passent dans les reserves
        poids_tot_avant_eclai=0.0
        poids_tot_apres_eclai=0.0

        do i = 1, P_nboite-1
      ! DR 15/01/15 je calcule le poids de fruit avant eclaicissage pour pour pouvoir retrancher le poids de fuits enlavé de masec !!!!
          poids_tot_avant_eclai = poids_tot_avant_eclai + pdsfruit(i)

      !
          if (nfruit(i) <= 0.0) CYCLE
          pdsfruit(i) = pdsfruit(i) / nfruit(i)
          !: Si P_nbinflo = 1 on enlève les fruits les plus jeunes
          if (P_nbinflo <= 1.0) then
            nfruit(i) = nfruit(i) - nbfrote
            if (nfruit(i) < 0.0) then
              nbfrote = -nfruit(i)
              nfruit(i) = 0.0
            endif
          else
            !: Si P_nbinflo>1 on enlève les grappes entières donc des fruits d'age différents
            nfruit(i) = nfruit(i) - nbfrote
            if (nfruit(i) < 0.0) then
              nbfrote = -nfruit(i) + nbfrote
              nfruit(i) = 0.0
            endif
          endif
          pdsfruit(i) = pdsfruit(i) * nfruit(i)

           poids_tot_apres_eclai = poids_tot_apres_eclai + pdsfruit(i)

        end do
      endif



      !: Calcul du nombre et poids (g) des fruits journaliers par "petit train" de boites de même âge
      devjourfr = upvt / P_dureefruit
      cumdevfr = cumdevfr + devjourfr - (pousfruit / P_nboite)
      if (cumdevfr < 1.0/P_nboite) then
        pousfruit = 0.0
      else
        pousfruit = 1.0
      endif

      !: NB - 18/07/06 : Arrêt dynamique des boites quand tous les fruits dans la dernière boite.
      !-                 Arrêt du calcul de la teneur en eau quand on arrive à la dernier boite.
      if (nbfruit <= 0.0) pousfruit = 0.0


      ! boucle sur allocation maxi trop forte

100   continue

      !: On recalcule les composantes du rendement en cas de dépassement
      !- du seuil d'allocation vers les fruits
      !- affectation des valeurs du premier passage
      do i = 1, P_nboite
        nfruitp(i) = nfruit(i)
        pdsfruitp(i) = pdsfruit(i)
      end do

      !: Effet température
      ftempremp = 1.0
      if (P_codetremp == 1) then
        ! pour les mini
        tefficace = tcultmin
        if (tefficace <= P_tminremp) then
          ftempremp = 0.0
          nbj0remp = nbj0remp + 1
        endif
        ! pour les maxi
        tefficace = tcultmax
        if (tefficace >= P_tmaxremp) then
          ftempremp = 0.0
          nbj0remp = nbj0remp + 1
        endif
      endif

      !: Premiere boite
      frmoins = pousfruit * nfruit(1)
      frmoinsp = pousfruit * pdsfruit(1)
      nfruit(1) = nfruitnou + nfruit(1) - frmoins
      croifruit = FPF(pdevfruit(1),P_afpf,P_bfpf,P_cfpf,P_dfpf,pfmax,devjourfr) * nfruit(1) * sourcepuits * ftempremp
      pdsfruit(1) = pdsfruit(1) + croifruit - frmoinsp
      dltags = croifruit / 100.
!      write(*,*)dltags,P_nboite
      !: Boites de 2 à P_nboite-1
      do i = 2,P_nboite-1
        frplus = frmoins
        frplusp = frmoinsp
        frmoins = pousfruit * nfruit(i)
        frmoinsp = pousfruit * pdsfruit(i)
        nfruit(i) = nfruit(i) + frplus - frmoins
        croifruit = FPF(pdevfruit(i),P_afpf,P_bfpf,P_cfpf,P_dfpf,pfmax,devjourfr) * nfruit(i) * sourcepuits * ftempremp
        pdsfruit(i) = pdsfruit(i) + croifruit + frplusp - frmoinsp
        dltags = dltags + (croifruit / 100.)
!        write(*,*)dltags
      end do

      !: Charge en fruits non murs
      pdsfruittot = 0.0
      nbfruit = 0.0
      do i = 1, P_nboite-1
        pdsfruittot = pdsfruittot + pdsfruit(i)
        nbfruit = nbfruit + nfruit(i)
      end do

      !: La maturité correspond à tous les fruits dans la dernière boite
      !- derniere boite : il n'y a plus de croissance
      frplus = frmoins
      frplusp = frmoinsp
      nfruit(P_nboite) = nfruit(P_nboite) + frplus
      pdsfruit(P_nboite) = pdsfruit(P_nboite) + frplusp

      !: Calcul de l'indice de récolte
      pdsfruittot = pdsfruittot + pdsfruit(P_nboite)

      ircarb = pdsfruittot / masec / 100.

      !: NB - 5/09/05 : accumulation d'azote journalier
      magrain(1) = pdsfruittot

      !: domi - 28/10/05 : On deplace tout les calculs d'azote des fruits.
      !-                   On calcule l'azote des fruits de la meme maniere
      !-                   que pour les grains dans Ngrain.
      if (P_nbcueille == 2) then
        do irecolte = 1, nbrecolte-1
          magrain(1) = magrain(1) + rdtint(irecolte)
        end do
      endif

      !: Calcul de l'allocation de biomasse aux fruits:
      if (dltams > 0.) then
        allocfruit = dltags / dltams
      else
        allocfruit = 0.
      endif

      !: Seuillage de l'allocation vers les fruits
!      if(n.gt.67)then
!      write(*,*)n,'allocfruit',allocfruit, 'P_allocfrmax',P_allocfrmax,'P_nboite',P_nboite
!           call flush(6)
!      pause
!      endif
!   dr 24/01/2011 y'a un blocage dans la boucle car la valeur est tres eau differente mais on passe dans la boucle
!  je change le test A VOIR plus tard
      if((allocfruit - P_allocfrmax) > 0.0000001) then
!      if (allocfruit > P_allocfrmax) then
        sourcepuits = sourcepuits * P_allocfrmax / allocfruit
        do i = 1, P_nboite
          nfruit(i) = nfruitp(i)
          pdsfruit(i) = pdsfruitp(i)
        end do
        goto 100
      endif

      !: Recalcul de sourcepuits (pour les feuilles) si ftempremp = 0.0
      if (ftempremp <= 0.0 .and. fpft > 0.0) then
        sourcepuits = dltams * 1e2 / fpft
        if (sourcepuits > 1.0) sourcepuits = 1.0
      endif


      if (nrec == 0 .and. nmat > 0) then
          magrain(1) = magrain(0)
      endif
      if (n == nrec) magrain(1) = magrain(0)
      pdsfruittot = magrain(1)


      ! DR 15/01/2015 quand on fait de l'eclaicissage il faut enlever le poids de fruits otés de masec
            !: éclaircissage : on supprime les plus petits fruits
      ! on divise par 100 pour la conversion de g/m2 en t/ha
      !if (P_codeclaircie == 2 .and. n == neclair) then
      if (P_codeclaircie == 2 .and. flag_oneclairci) then
          masec = masec - (poids_tot_avant_eclai - poids_tot_apres_eclai)/100.
      endif

return
end subroutine fruit



! *************************************************************************
! * dérivée d'une courbe de croissance de type Gompertz (Bertin, 1993)    *
! * fpf est la pente d'une loi de croissance en fonction de son stade     *
! * exprimé en fraction entre 0 et 1                                      *
! * pour ramener cette pente en g par jour, il est nécessaire de          *
! * multiplier par devjourfr : la fraction de developpement journalière   *
! *************************************************************************
! NB - 14/12/04 : Modification pour normaliser la courbe de
!                 croissance entre 0 et 1
! NB & IG : Modification pour introduire une croissance en 2 phases.
!           Une phase exponentielle pour la multiplication cellulaire
!           (qu'on peut annuler avec P_cfpf = 0.0)
!           et une phase logistique pour l'élongation cellulaire.
!           Le tout normalisé entre 0 et 1 (calcul complexe de afx)
! *************************************************************************

real FUNCTION FPF(X,P_afpf,P_bfpf,P_cfpf,P_dfpf,pfmax,devjourfr)

  implicit none

!: Arguments
  real, intent(IN) :: X  
  real, intent(IN) :: P_afpf  !> // PARAMETER // parameter of the  function logistic defining sink strength of fruits (indeterminate growth) : relative fruit age at which growth is maximal // SD // PARPLT // 1 
  real, intent(IN) :: P_bfpf  !> // PARAMETER // parameter of the logistic curve defining sink strength of fruits (indeterminate growth) :  rate of maximum growth proportionately to maximum weight of fruits // * // PARPLT // 1 
  real, intent(IN) :: P_cfpf  !> // PARAMETER // parameter of the first potential growth phase of fruit, corresponding to an exponential type function describing the cell division phase. // SD // PARPLT // 1 
  real, intent(IN) :: P_dfpf  !> // PARAMETER // parameter of the first potential growth phase of fruit, corresponding to an exponential type function describing the cell division phase. // SD // PARPLT // 1 
  real, intent(IN) :: pfmax  
  real, intent(IN) :: devjourfr  

!: VARIABLES LOCALES
  real :: Y  !>  
  real :: afx  

      Y = exp(-P_bfpf*(X-P_afpf))

! ancien calcul
!      FPF =  (pfmax*P_bfpf*Y)/((1.0+Y)**2)*devjourfr*a
! calcul du poids potentiel potcroifruit = pfmax (1/(1+Y) * a +b)
!      afx = 1/(1/(1+exp(-P_bfpf*(1-P_afpf)))-1/
!    s     (1+exp(P_afpf*P_bfpf)))
!      FPF =  (pfmax*P_bfpf*Y)/((1.0+Y)**2)*devjourfr*afx

! nouveau calcul du 30/05/06
      afx = 1-P_dfpf*(1-exp(-P_cfpf))
      afx = afx / (1 / (1 + exp(-P_bfpf * (1-P_afpf))) - 1 / (1 + exp(P_bfpf*P_afpf)))
      FPF = (P_dfpf * P_cfpf * exp(-P_cfpf * X)) + (P_bfpf * afx * Y / (1 + Y)**2)
      FPF = FPF * pfmax * devjourfr

! ** ancienne version changée le 7/4/98 NB et CL
! *- remplacement de la Gompertz par une logistique
! --     FPF =  pfmax * P_bfpf * exp(-P_bfpf * (X * 60.0 - P_afpf)) * exp(-exp(-P_bfpf * (X * 60.0 - P_afpf)))

return
end function FPF
 
 
