!*********************************************************************
!     Calcul des apports d'eau
!  version 3.3 18/03/98
! MODIF le 4/6/98 bug interception
! derniere modif 14/11/00
!*********************************************************************
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
!> This program manages irrigation.
!> - Stics book paragraphe 6.2, page 99
!>
!! The amounts of water applied can be entered from an irrigation calendar or calculated by the model.
!!
!! In the latter case, the model automatically calculates water inputs so as to satisfy water requirements at the level of the RATIOL parameter:
!! the model triggers irrigation each time the stomatal stress index (SWFAC is less than RATIOL). Irrigation amounts (AIRG) are then calculated so as
!! to replenish the soil water reserve (HUR) to field capacity (HUCC) down to the rooting front (ZRAC) without exceeding the maximum dose authorised
!! by the irrigation system (DOSIMX). At the time of sowing, whatever the soil reserve status, a fixed value of about 20 mm (IRRLEV parameter) is supplied
!! to the crop if it has not rained, to enable germination.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c

!: ML - 29/10/12 - ajout variable dureehumec dans les arguments

subroutine irrig(n,nplt,nrecbutoir,P_codecalirrig,P_irrlev,swfac,P_ratiol,cumlracz,   &
                 zrac,hucc,hur,P_dosimx,P_doseirrigmin,exofac,P_codlocirrig,P_effirr,   &
                 lai,P_codeintercept,P_stemflowmax,P_kstemflow,parapluie,ipl,         &
!    P_nbplantes,surfSous,P_mouillabil,P_locirrig,P_codeplante,P_codeSIG,     &  23/07/2012 on utilise pas plocirrig
                 P_nbplantes,surfSous,P_mouillabil,P_codeplante,flagEcriture,     &
                 nap,airg,preciptmp,stem,precipEns,mouill,interpluie,           &
                 irrigprof,eaunoneffic,totpl,codeSWDRH,dureehumec,P_codedate_irrigauto ,n_datedeb_irrigauto,n_datefin_irrigauto)

  USE Messages

  implicit none

!: Arguments

  integer, intent(IN)    :: n  
  integer, intent(IN)    :: nplt  
  integer, intent(IN)    :: nrecbutoir  
  integer, intent(IN)    :: P_codecalirrig  !> // PARAMETER // automatic calculation of irrigation requirements: yes (2), no (1) // code 1/2 // PARTEC // 1 
! DR 21/09/09 on supprime
!--  integer, intent(IN)    :: P_eau_mini_decisemis
!--  integer, intent(IN)    :: P_nbj_pr_apres_semis
!--  real,    intent(IN)    :: esol
  real,    intent(IN)    :: P_irrlev  !> // PARAMETER // amount of irrigation applied automatically on the sowing day when the model calculates irrigation, to allow germination // mm // PARAM // 1 
  real,    intent(IN)    :: swfac   !> // OUTPUT // Index of stomatic water stress  // 0-1
  real,    intent(IN)    :: P_ratiol  !> // PARAMETER // Water stress index below which we start an irrigation in automatic mode (0 in manual mode) // between 0 and 1 // PARTEC // 1 
  real,    intent(IN)    :: cumlracz   !> // OUTPUT // Sum of the effective root lengths  // cm root.cm -2 soil
  real,    intent(IN)    :: zrac   !> // OUTPUT // Depth reached by root system // cm
  real,    intent(IN)    :: hucc(int(zrac))  
  real,    intent(IN)    :: hur(int(zrac))  
  real,    intent(IN)    :: P_dosimx  !> // PARAMETER // maximum water amount of irrigation authorised at each time step (mode automatic irrigation) // mm day-1 // PARTEC // 1 
  real,    intent(IN)    :: P_doseirrigmin  !> // PARAMETER // minimal amount of irrigation // mm // PARTEC // 1 
  real,    intent(IN)    :: exofac   !> // OUTPUT // Variable for excess water // 0-1
  integer, intent(IN)    :: P_codlocirrig  !> // PARAMETER // code of irrigation localisation: 1= above the foliage, 2= below the foliage above the soil, 3 = in the soil // code 1/2/3 // PARTEC // 0 
  real,    intent(IN)    :: P_effirr  !> // PARAMETER // irrigation efficiency // SD // PARTEC // 1 
  real,    intent(IN)    :: lai                             ! (n)    // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  integer, intent(IN)    :: P_codeintercept  !> // PARAMETER // option of simulation rainfall interception by leafs: yes (1) or no (2) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: P_stemflowmax  !> // PARAMETER // Maximal fraction of rainfall which flows out along the stems  // between 0 and 1 // PARPLT // 1 
  real,    intent(IN)    :: P_kstemflow  !> // PARAMETER // Extinction Coefficient connecting leaf area index to stemflow // * // PARPLT // 1 
  integer, intent(IN)    :: parapluie  
  integer, intent(IN)    :: ipl  
  integer, intent(IN)    :: P_nbplantes  !> // PARAMETER // number of simulated plants // SD // P_USM/USMXML // 0 
  real,    intent(IN)    :: surfSous(2)  
  real,    intent(IN)    :: P_mouillabil  !> // PARAMETER // maximum wettability of leaves // mm LAI-1 // PARPLT // 1 
!  integer, intent(IN)    :: P_locirrig  !> // PARAMETER // Depth of water apply (when irrigation is applied in depth of soil) // cm // PARTEC // 1
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!! 1=snu     11=poi      21=tom
!!! 2=fou     12=pdt      22=vig
!!! 3=ban     13=col      23=pom
!!! 4=esc     14=rgi      24=men
!!! 5=mai     15=sor      25=qui
!!! 6=ble     16=soj
!!! 7=fet     17=fra
!!! 8=lin     18=bet
!!! 9=sal     19=can
!!! 10=mou    20=tou
!!!  character(len=3), intent(IN) :: P_codeplante  !> // PARAMETER // Name code of the plant in 3 letters // * // PARPLT // 0
  integer, intent(IN) :: P_codeplante  !> // PARAMETER // Name code of the plant in 3 letters // * // PARPLT // 0

  integer, intent(IN)    :: flagEcriture  !> // PARAMETER // code of writing: all the output files (0), the files bilan and rapport (1), or the file rapport only (2) // code 0/1/2 // PARAM // 0
!: ML 29/10/12 - ajout duree d'humectation
  integer, intent(IN)    :: codeSWDRH
  real,    intent(IN)    :: dureehumec
!: ML fin
! DR 10/02/2015 ajout des parametrs de dates
integer,    intent(IN) :: P_codedate_irrigauto !< // PARAMETER // option to activate the beginning and the ending date in case of automatic irrigation  : yes (1), no (2) // code 1/2 //PARAMv6 // 1
integer,    intent(IN) :: n_datedeb_irrigauto  !< // PARAMETER // date of beginning automatic irrigations  : yes (1), no (2) // code 1/2 //PARAMv6 // 1
integer,    intent(IN) :: n_datefin_irrigauto !< // PARAMETER // date of ending automatic irrigations  : yes (1), no (2) // code 1/2 //PARAMv6 // 1



! DR 21/09/09 on supprime
!--  real,    intent(INOUT) :: apport_mini_semis
  integer, intent(INOUT) :: nap  
  real,    intent(INOUT) :: airg                            ! (n)    // OUTPUT // Daily irrigation // mm
  real,    intent(INOUT) :: preciptmp  
  real,    intent(INOUT) :: stem  
  real,    intent(INOUT) :: precipEns(2)  
  real,    intent(INOUT) :: mouill  
  real,    intent(INOUT) :: interpluie   !> // OUTPUT // Water intercepted by leaves // mm
  real,    intent(INOUT) :: irrigprof                       ! (P_locirrig)  
  real,    intent(INOUT) :: eaunoneffic  
  real,    intent(INOUT) :: totpl  

!: Variables locales
  real    :: mouillprec  !>  
  real    :: mouillmax  
  integer :: iz  

  integer :: AS = 1  
  integer :: AO = 2  

!      write(618,*)'codes', P_codecalirrig,P_codlocirrig,airg


      ! **
      ! *- P_codecalirrig = 2 --> on lit les données dans le fichier technique
      ! *- utilisation des irrigations lues dans le fichier des techniques

      !: P_codecalirrig = 1 --> on recalcule les apports

      if (P_codecalirrig == 1) then

    !TODO : fonction calculAutomatiqueDesIrrigations
        !: Calcul automatique des irrigations entre semis et recolte
        if (n >= nplt .and. n < nrecbutoir) then

        ! DR 21/09/09 on supprime
        ! DR 24/06/08 pour climator on met dans rapport l'apport d'eau mini pour le semis
        !--apport_mini_semis = P_eau_mini_decisemis + (P_nbj_pr_apres_semis * esol)

          if (n == nplt) nap = 0

          ! ** 20 mm minimum au semis pour permettre la germination
          if (n == nplt .and. preciptmp < P_irrlev) then
            airg = P_irrlev - preciptmp
            nap = nap + 1
          else

          ! 10/02/2015 prise en compte de dates de debut et de fin
!            if(P_codedate_irrigauto == 1 .and. (n >= n_datedeb_irrigauto .and. n <= n_datefin_irrigauto)) then
! DR 24/02/2015 si on est sans dates faut quand meme qu'on irrig
            if(P_codedate_irrigauto == 2 .or. &
                     (P_codedate_irrigauto == 1 .and. (n >= n_datedeb_irrigauto .and. n <= n_datefin_irrigauto))) then

            ! ** NB - le 30/12/01 - turfac remplacé par swfac
              if ((swfac >= P_ratiol .and. cumlracz > 0.0) .or. preciptmp > 0. .or. cumlracz <= 0.) then
                airg = 0.
              else
                !airg = airg + SUM(hucc(1:int(zrac))) - SUM(hur(1:int(zrac)))
                !TODO: décommenter la ligne ci-dessus et supprimer la boucle ci-dessous.
                do iz = 1,int(zrac)
                  airg = airg + hucc(iz) - hur(iz)
                end do
                airg = min(airg, P_dosimx)
                ! domi 11/09/07 introduction d'une dose minimal apportée
                if (airg < P_doseirrigmin) airg = 0.
                ! DR 27/03/08  si on a de l'exces d'eau on irrig pas !!!!
                if (airg /= 0.0 .and. exofac > 0.0) then
                  airg = 0.0
                endif
              ! domi 11/09/07 si on est < dose minimal apportée alors pas d'apport
                if (airg >= P_doseirrigmin) then
                  nap = nap + 1
                endif
              endif

            endif
          endif

        endif

      endif




! ** calcul des apports d'eau (preciptmp) et cumul des irrigations
! *- introduction de l'interception de la pluie NB et MD le 27/9/97
! *- P_codlocirrig = 1 : irrigation sur-frondaison
! *- P_codlocirrig = 2 : irrigation sous-frondaison
! *- P_codlocirrig = 3 : irrigation en profondeur
! *- valeur de P_locirrig = profondeur de l'apport en entier (-10, -20)


      !: Les précipitations soumises à l'interception comprennent
      !- les irrigations pour P_locirrig = 1
      if (P_codlocirrig == 1) then
      ! DR et ML 28/08/2014
      ! on ajoute airg à preciptmp qu'une seule fois (des la premiere plante), sinon on comptabilise 2 fois chaque irrigation
        if(ipl.lt.2)then
          preciptmp = preciptmp + (airg * P_effirr)
        endif
      endif

      !: Interception de la pluie
      !- (P_codeintercept = 1 pour interception par le feuillage)

      if (lai > 0.0 .and. P_codeintercept == 1) then
       ! write(618,*)'feuillage'

        !: Calcul du stemflow
        stem = preciptmp * P_stemflowmax * (1.0 - exp(-P_kstemflow * lai))


        !: Interception
        !--if (P_codeintercept == 2) P_mouillabil = 0.0



        !: Effet parapluie pour cultures en rangs
        !- defaut: parapluie = 1 si culture principale
        if (parapluie == 1 .and. ipl < P_nbplantes) then
          precipEns(AS) = preciptmp * surfSous(AS)
          precipEns(AO) = preciptmp * surfSous(AO) - stem
        else
          precipEns(AS) = 0.0
          precipEns(AO) = preciptmp - stem
        endif


        !: Le mouillage (du jour) précédent (non évaporé ?)
        mouillprec = mouill
        mouillmax = P_mouillabil * lai

        !: On ne peut pas retenir + que la capacité des feuilles (mouillmax)
        mouill = min(mouillmax, precipEns(AO))

        !: ML - 29/10/12 - ajout de la prise en compte de la duree d'humectation-
        !- si elle est positive on remplit les feuilles d'eau
! DR et ML 16/10/2013 on change la valeur du code pour etre coherent avce la syntaxe classique (1 devinet 2 et 2 devient 1)
!        if(codeSWDRH.eq.2) then
        if(codeSWDRH.eq.1) then
            if (dureehumec > 0) then
                mouill = mouillmax
            endif
        endif
        !: ML fin

        !: Est-ce que ca ne devrait pas etre mis avant le calcul des retombées
        !- oui effectivement : debug le 10/06/05 (NB)
        mouill = mouill + mouillprec
        if (mouill > mouillmax) then
          mouillprec = mouill - mouillmax
          mouill = mouillmax
        else
          mouillprec = 0.0
        endif

        !: Les retombées qui ne sont pas retenues par les feuilles ?
        precipEns(AO) = precipEns(AO) - mouill

        ! debug NB 10/06/05 soustraction de mouillprec
        ! domi 16/09/05 on le conditionne à P_codeintercept = 1
        interpluie =  mouill - mouillprec

        ! ** reconstitution de la pluie sous la culture

      else
        precipEns(AO) = 0.0
        precipEns(AS) = preciptmp
        stem = 0.0
      endif

      preciptmp = precipEns(AO) + precipEns(AS) + stem
!      write(618,*)'preciptmp',preciptmp,airg,P_effirr

      if (P_codlocirrig == 2) then
      ! DR et ML 28/08/2014
      ! on ajoute airg à preciptmp qu'une seule fois (des la premiere plante), sinon on comptabilise 2 fois chaque irrigation
          if(ipl.lt.2)then
            preciptmp = preciptmp + (airg * P_effirr)
         endif
      endif

      !: Irrigation en profondeur
      if (P_codlocirrig == 3) then
        irrigprof = airg * P_effirr
      endif

! debug NB 10/06/05 soustraction de mouillprec
! domi 16/09/05 on le conditionne à P_codeintercept = 1
!      interpluie =  mouill-mouillprec

      !: Domi - 14/11/00: Calcul des irrigations non efficaces
      eaunoneffic = airg * (1.0 - P_effirr)

    ! Deplacement du test aprés le calcul de preciptmp
    !TODO: c'est quoi ce test sur P_codeSIG ??
! DR 29/08/2012 strange      if (P_codeplante /= 'snu' .and. P_codeSIG == 0) then
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!      if (P_codeplante /= 'snu') then
      if (P_codeplante /= 1) then
        if (n == nplt .or. n == nplt+1) then
          totpl = totpl + preciptmp
        endif
        if (n == nplt+1 .and. totpl == 0.0) then
          call EnvoyerMsgHistorique(90)
        endif
      endif
!      write(618,*)'preciptmp fin irrig',preciptmp


return
end subroutine irrig
 
 
