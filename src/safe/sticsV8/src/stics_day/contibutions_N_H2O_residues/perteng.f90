!*********************************************************************************   
!  Calcul du devenir des engrais min�raux (forme ur�e, NH4 ou NO3)
!       par l'approche empirique "comp�tition sol-plante"
!       inspir� des travaux sur bl� de Limaux et al.(2000)
!
!  La proportion d'azote perdu par volatilisation et d�nitrification d�pend:
!      - du type d'engrais
!      - de la vitesse d'absorption du couvert au moment de l'apport
!      - du pH du sol (pour la volatilisation)
!
!  La quantit� d'azote organis� dans le sol d�pend :
!      - de la quantit� d'engrais
!      - du type d'engrais
! 
!  La quantit� d'azote lessiv� de l'engrais est simul�e avec le reste du N min�ral
!
!  -------------------------- Variables d'entr�e --------------------------------
!  P_Vabs2:  vitesse d'absorption pour laquelle les pertes sont diminu�es par 2 (kg/ha/jour)
!  P_voleng: fraction maximale de l'engrais volatilis�e  (% apport)
!  P_deneng: fraction maximale de l'engrais denitrifi�e  (% apport)

!  -------------------------- Variables de sortie --------------------------------
!  Nvoleng: quantit� d'azote de l'engrais volatilis� (kg/ha)
!  Ndenit: quantit� d'azote de l'engrais denitrifi� (kg/ha)
!  Norgeng: quantit� d'azote de l'engrais organis�   (kg/ha)
!**********************************************************************************
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> calculation of the N loss
!> - Stics book paragraphe 6.3.2.a, page 101-104
!>
!! The (potential) nitrogen use efficiency (EFFN), i.e. the fraction of fertilizer N available for plant uptake, can be either imposed or calculated by the model.
!! If EFFN is fixed, it must be defined in the technical file. Part of the fertilizer is considered to be unavailable for the plant because it is either
!! immobilized in soil by microbial activity, denitrified or volatilized. The efficiency EFFN is the complement of these 'losses' to 100%.
!! It must be noticed that nitrate leaching is not included in these losses since it is simulated directly by the nitrate transfer module.
!!
!! In the STICS model the calculation of losses is based on the concept of competition between the soil and the crop. Indeed Limaux et al. (1999) have shown
!! that the nitrogen use efficiency depends on the crop growth rate at the time of fertilizer application. The greater the growth rate,
!! the higher is the N use efficiency. Since nitrate leaching from fertilizer is usually negligible, the higher efficiency is attributed to smaller gaseous losses
!! (denitrification and volatilization) from the fertilizer.
!! In STICS these losses are assumed to depend on nitrogen uptake rate immediately before fertilizer application (Vabsmoy, in kg N ha-1 day-1).
!! The parameters "deneng(f)" and "voleng(f)" characterize the maximal amounts of N losses for each fertilizer type �f�, by denitrification and volatilization,
!! respectively. The potential gaseous losses (denitrification and volatilization) are assumed to be proportional to the N fertilizer rate anit (kg N ha-1).
!! The actual losses depend on the nitrogen uptake rate (Vabsmoy) recorded in the five days before fertilizer application, through a hyperbolic relationship.
!! The N loss through denitrification (N2+N2O) is Ndenit.
!! The parameter Vabs2 corresponds to the crop uptake rate (kg N ha-1 day-1) at which losses reach 50% of their maximum.
!!
!! The N loss through NH3 volatilization (Nvoleng) is calculated similarly, but it also depends on soil pH: it increases linearly
!! when the pH increases from pHminvol to pHmaxvol.
!! Concerning N immobilization, studies made with 15N-labelled fertilizers have shown that the microbial immobilization of N derived from fertilizer
!! depends mainly on the N rate and the type of fertilizer. We have derived a quadratic relationship between the amount of N immobilized (Norgeng, in kg N ha-1)
!! and the fertilizer N rate.The parameter orgeng (f) represents the maximal amount of microbial immobilized N from the fertilizer type f and Xorgmax
!! is the N rate at which this maximum is reached. Both are expressed in kg N ha-1.
! The fertilizer N losses through immobilization and volatilisation are always calculated as indicated above.
!!! However the N losses through denitrification (from soil and fertilizer) can be calculated more mechanistically by activating the option "codedenit".
!! In this case, denitrification is calculated according to NEMIS model.
!! Finally, it is also possible to impose a fixed efficiency by choosing fertilizer type 8. In that case, the microbial immobilization, the volatilization
!! and the denitrification are fixed and expressed in % of fertilizer-N. The efficiency is the complement of these values to 1.
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c

subroutine perteng(P_orgeng, P_voleng, P_deneng, anit, n, absoTot, P_Vabs2, P_codlocferti, P_pHminvol, &
                   P_pHmaxvol, P_pH, P_Xorgmax, P_codedenit,                                           &  ! IN
                   Nvoleng, Ndenit, Norgeng, QNvoleng, QNorgeng, QNdenit)                               ! INOUT

  implicit none
  
  real,    intent(IN)    :: P_orgeng    !  // PARAMETER // maximal quantity of mineral fertilizer that can be organized in the soil (fraction for type 8) // kg ha-1 // PARAM // 1
  real,    intent(IN)    :: P_voleng    !> // PARAMETER // maximal fraction of mineral fertilizer that can be volatilized  // SD // PARAM // 1
  real,    intent(IN)    :: P_deneng    !> // PARAMETER // proportion of the �ineral fertilizer that can be denitrified (useful if codenit not active) // SD // PARAM // 1
  real,    intent(IN)    :: anit        !  // OUTPUT // Daily N added as fertiliser  // kg.ha-1.d-1
  integer, intent(IN)    :: n  
  real,    intent(IN)    :: absoTot(5)  ! 5 derniers jours (max.)  
  real,    intent(IN)    :: P_Vabs2     !> // PARAMETER // N uptake rate for which  fertilizer losts of  are divided by 2 // kg/ha/jour // PARAM // 1
  integer, intent(IN)    :: P_codlocferti !// PARAMETER // code of fertilisation localisation:  1 = at soil surface, 2 = in the soil // code 1/2 // PARTEC // 0
  real,    intent(IN)    :: P_pHminvol  !> // PARAMETER // pH above which the fertilizer volatilisation is null // P_pH // PARAM // 1
  real,    intent(IN)    :: P_pHmaxvol  !> // PARAMETER // pH beyond which the fertilizer volatilisation is maximum // P_pH // PARAM // 1
  real,    intent(IN)    :: P_pH        !> // PARAMETER // pH of mixing soil + organic amendments  // SD // PARSOL // 1
  real,    intent(IN)    :: P_Xorgmax   !> // PARAMETER // maximal amount of immobilised N coming from the mineral fertilizer  // kg.ha-1 // PARAM // 1
  integer, intent(IN)    :: P_codedenit !> // PARAMETER // option to allow the calculation of denitrification :yes (1), no(2) // code 1/2 // PARSOL // 0
  
  ! ici, ces variables de sortie pourraient �tre mises en OUT plutot que INOUT
  real,    intent(INOUT) :: Nvoleng     !> // OUTPUT // Daily volatilisation of N from fertiliser // kgN.ha-1.j-1
  real,    intent(INOUT) :: Ndenit     !> // OUTPUT // Daily denitrification of N from fertiliser or soil (if option  denitrification  is activated)" // kg.ha-1.j-1
  real,    intent(INOUT) :: Norgeng     !> // OUTPUT // Daily organisation of N from fertiliser // kgN.ha-1.j-1
  real,    intent(INOUT) :: QNvoleng    !> // OUTPUT // Cumulative volatilisation of N from fertiliser // kgN.ha-1
  real,    intent(INOUT) :: QNorgeng    !> // OUTPUT // Cumulative organisation of N from fertiliser // kgN.ha-1
  real,    intent(INOUT) :: QNdenit    !> // OUTPUT // Cumulative denitrification of N from fertiliser or soil (if option  denitrification  is activated)" // kgN.ha-1

!: Variables locales
  integer :: i  !>  
  integer :: jour  
  real    :: pHmin  !>  
  real    :: tdenit  
  real    :: tvolat  !>  
  real    :: Vabsmoy  !>  
  real    :: Vabsrel  !>  
  real    :: volmax  

 ! 1er Cas : l'efficience de l'engrais est impos�e
      !-  le taux de volatilisation  est constant et egal a P_voleng
      !-  le taux de denitrification est constant et egal a P_deneng
      !-  le taux d'organisation     est constant et egal a P_orgeng
      if (P_orgeng + P_voleng + P_deneng <= 1.) then 
         Nvoleng = P_voleng * anit
         Ndenit = P_deneng * anit
         Norgeng = P_orgeng * anit
      else  
! 2eme Cas : l'efficience de l'engrais est calcul�e
   ! Vitesse moyenne d'absorption d'azote dans les 5 jours pr�c�dant l'apport d'engrais
         Vabsmoy = 0.
         jour = 5
         if (n < 5) jour = n
         do i = 1,jour
            Vabsmoy = Vabsmoy + absoTot(6-i)
         end do
         if (jour > 0) Vabsmoy = Vabsmoy / jour

        !: effet de la vitesse d'absorption
        Vabsrel = P_Vabs2 / (P_Vabs2 + Vabsmoy)

  ! Volatilisation de N engrais
        !- 1) le taux maxi de volatilisation d�pend du type d'engrais
        !- 2) il d�pend aussi du pH du sol, selon une fonction lin�aire
        !- 3) il est r�duit lorsque l'engrais est inject� en profondeur
        if (P_codlocferti == 2) then
           pHmin = (P_pHminvol + P_pHmaxvol) / 2.
        else
           pHmin = P_pHminvol
        endif

        if (P_pH < pHmin) then
           volmax = 0.
        else
           volmax = (P_pH - pHmin) / (P_pHmaxvol - pHmin)
           volmax = P_voleng * min(volmax, 1.)
        endif
        
        ! 4) La volatilisation effective d�cro�t avec la vitesse d'absorption N selon une fonction hyperbole
        tvolat = volmax * Vabsrel
        Nvoleng = tvolat * anit

  ! Denitrification
        !: Taux maxi de denitrification = P_deneng (d�pend du type d'engrais)
        !: La denitrification effective d�cro�t avec la vitesse d'absorption N (fonction hyperbole)
        tdenit  = P_deneng * Vabsrel
        Ndenit = tdenit * anit

  ! Organisation
        !: 1) La quantit� maximale de N organis� = P_orgeng (d�pend du type d'engrais)
        !: 2) Elle est obtenue pour une dose d'azote P_Xorgmax
        !: 3) L'organisation effective cro�t avec la dose d'engrais apport� selon une fonction quadratique avec plateau
        if (anit >= P_Xorgmax) then 
           Norgeng = P_orgeng
        else
           Norgeng = P_orgeng / (P_Xorgmax**2) * anit * (2. * P_Xorgmax - anit)
        endif
      endif

    !- cumul des pertes d'azote
      QNvoleng = QNvoleng + Nvoleng
      QNorgeng = QNorgeng + Norgeng
      if (P_codedenit == 2) QNdenit = QNdenit + Ndenit

return
end subroutine perteng 
 
