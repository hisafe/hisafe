! ml_com !
! *----------------------------------------------------------------------------------------------------------------------------------------------------------------------------* c
!> calculation of the sowing date (IPLT) from rules
!> - Stics book paragraphe 6.1.2, page96
!>
!! It is possible either to prescribe the sowing date (IPLTT) or to calculate it (IPLT) from rules to do with the weather and soil water status.
!! In the case of calculation, a period when sowing is allowed is defined as the interval [IPLTT, IPLTT + NBJMAXAPRESSEMIST].
!! Three criteria are then taken into account to postpone sowing within the previously defined sowing period.
!>- The soil must be wet (above wilting point in the seedbed = PROSEMT +/- 2 cm) and warm enough (TAIR above TDMINP for several days to allow significant growth: NBJSEUILTEMPREFT) to avoid germination delays or failure of plant emergence (see § 2.2.1)
!>  - The risk of freezing must be low: TMIN above TDEBGELPfor NBJSEUILTEMPREFT days
!>  - The soil must be dry enough to avoid compaction risks: the soil water status is considered as damaging if it is above HUMSEUILTASSSEMT x HUCC
!!     in the zone between the surface and PROFHUMSEMT (see § 6.5.2).

!*----------------------------------------------------------------------------------------------------------------------------------------------------------------------------* c
! Permet de decider a partir de la date de semis lue dans le fichier tec si on seme ou pas .
! possibilité de repousser le semis au plus du parametre nbjapressemis
! *-----------------------------------------------------------------------* c
subroutine decisionsemis(n,P_iwater,nh,nbCouches,P_prophumtasssem,P_hccf,P_epc,hur,da,sat,P_profhumsemoir,P_profsem,hucc,humin, &
                         P_sensrsec,P_humirac_decisemis,P_nbjseuiltempref,tmin,P_tdebgel,tmoy,P_tdmin,P_eau_mini_decisemis,     &
                         P_nbj_pr_apres_semis,esol,P_codecalirrig,P_irrlev,airg,trr,P_nbjmaxapressemis,                     &
                         nbjpourdecisemis,repoussesemis,nplt,iplt)

USE Messages

  implicit none
  
!: Arguments

  integer, intent(IN)    :: n  
  integer, intent(IN)    :: P_iwater  !> // PARAMETER // julian day of the beginning of the simulation // jour julien // P_USM // 1 
  integer, intent(IN)    :: nh  
  integer, intent(IN)    :: nbCouches  
  real,    intent(IN)    :: P_prophumtasssem  !> // PARAMETER // field capacity proportion above witch compaction may occur (to delay sowing) // SD // PARAM // 1 
  real,    intent(IN)    :: P_hccf(nh)  !> // PARAMETER // gravimetric water content at field capacity of each soil layer (/fine earth) (table) // % w // PARSOL // 1
  real,    intent(IN)    :: P_epc(nh)  !< // PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm
  real,    intent(IN)    :: hur(nbCouches)  
  real,    intent(IN)    :: da(nh)   !> // OUTPUT // Bulk density in the horizon 1 // g cm-3
  real,    intent(IN)    :: sat(nbCouches)  
  real,    intent(IN)    :: P_profhumsemoir  !> // PARAMETER // soil depth for which moisture is considered as a reference to allow or not sowing in the case of soil compaction activation // cm // PARTEC // 1 
  real,    intent(IN)    :: P_profsem  !> // PARAMETER // Sowing depth // cm // PARTEC // 1 
  real,    intent(IN)    :: hucc(nbCouches)  
  real,    intent(IN)    :: humin(nbCouches)  
  real,    intent(IN)    :: P_sensrsec  !> // PARAMETER // root sensitivity to drought (1=insensitive) // SD // PARPLT // 1 
  real,    intent(IN)    :: P_humirac_decisemis  !> // PARAMETER // parameter to start sowing according to the soil moisture (0: no sensitivity to drought, 1: very sensitive) // SD // PARAMV6 // 1 
  integer, intent(IN)    :: P_nbjseuiltempref  !> // PARAMETER // number of days for which we check if there is no frost sowing conditions when decision to sow is activated // jours // PARTEC // 1 
  real,    intent(IN)    :: tmin(n:n+P_nbjseuiltempref)                   ! n à n+P_nbjseuiltempref    // OUTPUT // Minimum active temperature of air // degree C
  real,    intent(IN)    :: P_tdebgel  !> // PARAMETER // temperature of frost beginning // degree C // PARPLT // 1
  real,    intent(IN)    :: tmoy(n:n+P_nbjseuiltempref)   !> // OUTPUT // Mean active temperature of air // degree C
  real,    intent(IN)    :: P_tdmin  !> // PARAMETER // Minimum threshold temperature for development // degree C // PARPLT // 1
  integer, intent(IN)    :: P_eau_mini_decisemis  !> // PARAMETER // minimum amount of rainfall to start sowing (when codesemis is activated) // mm // PARAMV6 // 1 
  integer, intent(IN)    :: P_nbj_pr_apres_semis  !> // PARAMETER // days number to calculate rainfall need to start sowing (is codesemis is activated) // day // PARAMV6 // 1 
  real,    intent(IN)    :: esol   !> // OUTPUT // Actual soil evaporation flux  // mm day-1
  integer, intent(IN)    :: P_codecalirrig  !> // PARAMETER // automatic calculation of irrigation requirements: yes (2), no (1) // code 1/2 // PARTEC // 1 
  real,    intent(IN)    :: P_irrlev  !> // PARAMETER // amount of irrigation applied automatically on the sowing day when the model calculates irrigation, to allow germination // mm // PARAM // 1 
  real,    intent(IN)    :: airg(n:n+P_nbj_pr_apres_semis)   !> // OUTPUT // Daily irrigation // mm
  real,    intent(IN)    :: trr(n:n+P_nbj_pr_apres_semis)   !> // OUTPUT // Rainfall  // mm.day-1
  integer, intent(IN)    :: P_nbjmaxapressemis  !> // PARAMETER // maximal delay (number of days) of sowing date in case of soil compaction activation // jours // PARPLT // 1 
    
  integer, intent(INOUT) :: nbjpourdecisemis   !> // OUTPUT // "Number of days until sowing is launched when it's postponed by the   sowing decision  option activation" // days
  !!!MODIF HISAFE 6 : on remplace les tableaux de boolean
  !!!logical, intent(INOUT) :: repoussesemis
  integer, intent(INOUT) :: repoussesemis
  integer, intent(INOUT) :: nplt  
  integer, intent(INOUT) :: iplt  
  

!: Variables locales
  real    :: hummoytass  
  real    :: humminsemis  !>  
  real    :: hummoylev  !>  
  real    :: hummaxsemis  
  integer :: k  !>  
  integer :: numcouche  !>  
  integer :: i  !>  
  integer :: jdeb  !>  
  integer :: jfin  
  logical :: yagel1  !>  
  logical :: yagel2  !>  
  logical :: yagel3  
  real    :: humseuiltasssem  
  real    :: apport_eau_mini  !>  
  real    :: cumul_pluie_5j  
!  integer :: l
  real    :: humisow  
  real    :: apportH2o  

! fonction(s)  
  real    :: hoptsemis  

      ! DR 27/05/08 toutes les humidités calculées et seuils sont en % ponderal

      ! 07/03/08 mis en parametre 
      !  humseuiltasssem = 1.2*P_hccf(1)
      humseuiltasssem = P_prophumtasssem * P_hccf(1)

      ! DR 21/05/08 initialisations
      cumul_pluie_5j = 0.0
      yagel1 = .FALSE.
      yagel2 = .FALSE.
      yagel3 = .FALSE.

      
      ! DR 11/08/06 on introduit un seuil d'humidité minimale pour ne pas semer 
      ! sur du sol trop sec qui empecherait la levée
      ! pour l'instant on regarde par rapport au pfp (humin)
      ! on va de la meme manière que hummoy le calculer sur la couche 
      hummoytass = 0.
      jdeb = 1
      jfin = 0
      numcouche = 0
B1:   do i = 1,nh     ! on utilise une étiquette nommée pour pouvoir sortir des 2 boucles avec le EXIT, afin de remplacer le GOTO original
        jfin = jfin + int(P_epc(i))
        do k = jdeb,jfin
          numcouche = numcouche + 1
          ! domi 090606 on divise sat par da pour passer en ponderal
          ! domi 13/11/06 y'avait un bug (meaculpa sur le calcul de hummoy
          !        hummoy = hummoy+hur(i)/da(i)+sat(i)/da(i)
          hummoytass = hummoytass + (hur(numcouche) / da(i)) + (sat(i) / da(i))

! DR 15/01/07 on le calcule maintenant autour de la profondeur de semis
!         humminsemis = humminsemis+humin(numcouche)/da(i)

          if (numcouche == P_profhumsemoir) EXIT B1 ! on sort des 2 boucles
        end do
        jdeb = jdeb + int(P_epc(i))
      end do B1
      
      hummoytass = hummoytass * 10. / P_profhumsemoir

      ! DR 15/01/07 ca c'est pour le calcul de l'humidite mini au dela de laquelle
      !  y'a pas de germination possible
      ! on le fait sur la couche -2 +2 cm autour de la profondeur de semis 
      ! je laisse le 2 au cas ou on voudrait le parametrer un de ces 4
      hummoylev   = 0.
      humminsemis = 0.
      hummaxsemis = 0.

      jdeb = 1
      jfin = 0
      numcouche = 0
B2:   do i = 1,nh
        jfin = jfin + int(P_epc(i))
        do k = jdeb,jfin
          numcouche = numcouche + 1
          if (numcouche >= (P_profsem - 2) .and. numcouche <= (P_profsem + 2)) then
            hummoylev   = hummoylev + (hur(numcouche) / da(i)) + (sat(i) / da(i))
            humminsemis = humminsemis + humin(numcouche) / da(i)

            ! DR 27/05/08 faut calculer une humdité maxi sur le lit de semence pour la fonction
            hummaxsemis = hummaxsemis + hucc(numcouche) / da(i)
          endif
          if (numcouche == P_profsem + 2) EXIT B2 ! on sort des 2 boucles
        end do
        jdeb = jdeb + int(P_epc(i))
      end do B2
    
      humminsemis = humminsemis * 10. / ((P_profsem + 2) - (P_profsem - 2))
      hummaxsemis = hummaxsemis * 10. / ((P_profsem + 2) - (P_profsem - 2))
      hummoylev   = hummoylev   * 10. / ((P_profsem + 2) - (P_profsem - 2))
      
       !!!MODIF HISAFE 6 : on remplace les tableaux de boolean
       !!!if (n == nplt .or. repoussesemis .eqv. .TRUE.) then
       if (n == nplt .or. repoussesemis == 1) then

        ! DR on teste les conditions pour avoir une levee optimale
        ! = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = 
        ! DR 11/08/06 on ajoute le test sur l'humidite mini
        !        if (hummoy < humseuiltasssem) then
        !        if (hummoytass < humseuiltasssem .and. 
        !     s          hummoylev > humminsemis) then
        !    yagel = .FALSE.
        ! **********
        !  calcul de l'humidité minimale 
        !  autorisant le semis pour une valeur de humirac        
        ! Warning voir si il faut pas passer en humidités volumiques
        humisow = hoptsemis(humminsemis,hummaxsemis,P_sensrsec,P_humirac_decisemis)
        ! 28/05/08 DR et JC on modifie les conditions de semis sur l'humidité

        if (hummoytass < humseuiltasssem .and. hummoylev > humisow) then
          yagel1 = .FALSE.
        else
          yagel1 = .TRUE.
          !--write(128,*) n,'condition humisow mauvaises'
        endif

        ! condition 2 sur les P_nbjseuiltempref jours suivants il fait pas trop froid
        !---------------------------------------------------------------------------
        do k = 0,P_nbjseuiltempref
          ! on teste si on seme ou pas
          if (tmin(n+k) < P_tdebgel .or. tmoy(n+k) < P_tdmin) then
            yagel2 = .TRUE.
          endif
        end do
        
        !--if (yagel2) write(128,*) n,'condition temperatures mauvaises'

        ! condition 3 on a de l'eau dans le sol dans les P_nbj_pr_apres_semis pour la levee
        !------------------------------------------------------------------------------  
        ! DR 21/05/08 on rajoute une condition sur l'humidite du sol suffisante pour la levée
        !   Condition sur la pluie
        apport_eau_mini = P_eau_mini_decisemis + (P_nbj_pr_apres_semis * esol)
        
        ! DR 24/06/08 calcul des apports le jour du semis
        if (P_codecalirrig == 1) then
          ! ** calcul automatique des irrigations entre semis et recolte
          ! ** 20 mm minimum au semis pour permettre la germination              
          !--if (trr(n) < P_irrlev) then
          !--  apportH2o = P_irrlev
          !--else
          !--  apportH2o = trr(n)
          !--endif
          apportH2o = max( P_irrlev, trr(n) )
        else
          apportH2o = airg(n) + trr(n)
        endif
        
        ! DR calcul des apports les P_nbj_pr_apres_semis apres le semis
        cumul_pluie_5j = apportH2o + SUM(airg(n+1:n+P_nbj_pr_apres_semis)) + SUM(trr(n+1:n+P_nbj_pr_apres_semis))


        ! 27/05/08 on integre les modifs pour permettre une levée dans de bonnes condition
        ! condition sur l'humidité (pluie ou humidité suffisante
        if (cumul_pluie_5j > apport_eau_mini) then
          yagel3 = .FALSE.
        else
          yagel3 = .TRUE.
          !-- write(128,*)n,'condition apport eau mauvaises'
        endif
        !--write(130,*)n,cumul_pluie_5j,apport_eau_mini                  

        !  les conditions de temp ne sont pas respectees      
        if (yagel1 .or. yagel2 .or. yagel3) then
          nbjpourdecisemis = nbjpourdecisemis + 1
          if (nbjpourdecisemis <= P_nbjmaxapressemis) then
            !!!MODIF HISAFE 6 : on remplace les tableaux de boolean
            !!!repoussesemis = .TRUE.
            repoussesemis = 1
            nplt = -999
          else
            !!!MODIF HISAFE 6 : on remplace les tableaux de boolean
            !!!repoussesemis = .FALSE.
            repoussesemis = 0
            nplt = n
            iplt = nplt + P_iwater - 1
            call EnvoyerMsgHistorique(444)
          endif
        else
          !!!MODIF HISAFE 6 : on remplace les tableaux de boolean
          !!!repoussesemis = .FALSE.
          repoussesemis = 0
          nplt = n
          iplt = nplt + P_iwater - 1
          call EnvoyerMsgHistorique(445,nbjpourdecisemis)
        endif
      endif

return        
end subroutine decisionsemis

! *------------------------------------------------------------------* c
! *   introduction d'une fonction pour obtenir une humidité minimale * c
! *   autorisant le semis pour une valeur de humirac                 * c
! *------------------------------------------------------------------* c
! ** 27/05/08 on a inversé la fonction humirac 
real function hoptsemis(hmin,hmax,P_sensrsec,H)

  implicit none 
  
!: Arguments
  real, intent(IN)    :: hmin  
  real, intent(IN)    :: hmax   !> // OUTPUT // Maximum height of water table between drains // cm
  real, intent(IN)    :: P_sensrsec  !> // PARAMETER // root sensitivity to drought (1=insensitive) // SD // PARPLT // 1 
  real, intent(INOUT) :: H          ! TODO: INOUT ? est-ce vraiment utile de renvoyer la valeur modifiée de H ?  
  
!: Variables locales
  real :: x  

      if (H == 0.) H = 0.05
      if (H == 1.) H = 0.95

      if (H > P_sensrsec) then  
          x = (H-P_sensrsec)/(1-P_sensrsec)
          hoptsemis  = (x*(hmax-hmin))+hmin
      else
          hoptsemis = H * hmin /P_sensrsec
      endif
      
return
end function hoptsemis
 
 
