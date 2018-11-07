! ******************************************************** c
! * - derniere modif le 28/06/01                         * c
! * - version 3.3 18/03/98                               * c
! * - dernière modif le 22/5/98  paramétrage de P_irmax    * c
! * - derniere modif le 17/11/00     magrain constant    * c
! * - derniere modif le 08/04/01     GEL                 * c
! * - derniere modif le 02/05        sucre,huile         * c
! * - derniere modif dr 25/10/05 pb proteines            * c
! ******************************************************** c
!> Calculation of the number and filling of organs for harvest in the case of plants with determinate growth.
!> - Stics book paragraphe 4,1, page 74-76
!>
!> In the case of plants with determinate growth, the hypothesis is made that the number and filling of organs for harvest do not depend on the other
!! organs’ growth requirements. The number of grains is fixed during a phase of variable duration (nbjgrain in days), which precedes the onset of filling (IDRP).
!! This number depends on the mean growth rate of the canopy during this period (vitmoy in gm-2d-1), which in turns depends on dynamics specific to the
!! particular species.
!>
!> The number of grains per m2 (nbgrains) is defined at the IDRP stage. It depends on the growth variable (vitmoy in g m-2) that integrates the effect of the
!! prevailing stresses during the period preceding the IDRP stage, on two species-dependent parameters cgrain (in g-1 m2) and nbgrmin (grains m-2) and a
!! genetic-dependent parameter nbgrmax (grains m-2).  The last two parameters define the limits of variation of nbgrains.
!! After the IDRP stage, the grain number can be reduced in the event of frost and the daily proportion of grains affected is (1-fgelflo), whatever their
!! state of growth. The corresponding weight (pgraingel in gm-2) is deducted from the grain weight, using the elementary current grain weight (pgrain in g).
!>
!> The quantity of dry matter accumulated in grains is calculated by applying a progressive "harvest index" to the dry weight of the plant.
!! This ircarb index increases linearly with time (vitircarb in g grain g biomass 1 d-1), from the IDRP stage to the IMAT stage and the final harvest index
!! is restricted to the irmax parameter. Yet this dynamics may not be the actual grain filling dynamics since threshold translocation temperatures defining
!! the thermal stress ftempremp (tminremp and tmaxremp) may stop the carbon filling of harvested organs.
!! Consequently the grain filling is calculated daily (dltags in t ha-1) to allow the effect of the thermal stress and then accumulated within the
!! mafruit (in t ha-1) variable. The mass of each grain is then calculated as the ratio of the mass to the number of grains, although this cannot exceed
!! the genetic pgrainmaxi limit.
!-----------------------------------------------------------------------------------------------
subroutine grain(n,ndrp,nrec,nlev,nrecbutoir,P_nbjgrain,dltams,P_cgrain,P_cgrainv0, & ! IN
                 P_nbgrmin,P_nbgrmax,P_codazofruit,P_codeinnact,inns,fgelflo,P_codeir,  & ! IN
                 P_vitircarb,P_irmax,P_vitircarbT,somcourdrp,nmat,masec,P_codetremp,  & ! IN
                 tcultmin,tcultmax,P_tminremp,P_tmaxremp,P_pgrainmaxi,              & ! IN
                 ircarb,nbgrains,pgrain,CNgrain,vitmoy,nbgraingel,pgraingel,  & ! INOUT
                 dltags,ftempremp,magrain,nbj0remp,pdsfruittot)                 ! INOUT

  USE Messages

  implicit none

!: Arguments

  integer, intent(IN)    :: n  
  integer, intent(IN)    :: ndrp  
  integer, intent(IN)    :: nrec  
  integer, intent(IN)    :: nlev  
  integer, intent(IN)    :: nrecbutoir  
  integer, intent(IN)    :: P_nbjgrain  !> // PARAMETER // Period to compute NBGRAIN // days // PARPLT // 1 
  real,    intent(IN)    :: dltams(ndrp) ! ndrp-P_nbjgrain+1 to ndrp    // OUTPUT // Growth rate of the plant  // t ha-1.j-1
  real,    intent(IN)    :: P_cgrain  !> // PARAMETER // slope of the relationship between grain number and growth rate  // grains gMS -1 jour // PARPLT // 1 
  real,    intent(IN)    :: P_cgrainv0  !> // PARAMETER // number of grains produced when growth rate is zero // grains m-2 // PARPLT // 1 
  real,    intent(IN)    :: P_nbgrmin  !> // PARAMETER // Minimum number of grain // grains m-2  // PARPLT // 1 
  real,    intent(IN)    :: P_nbgrmax  !> // PARAMETER // Maximum number of grain // grains m-2 // PARPLT // 1 
  integer, intent(IN)    :: P_codazofruit  !> // PARAMETER // option of activation of the direct effect of the nitrogen plant status upon the fruit/grain number // code 1/2 // PARPLT // 0 
  integer, intent(IN)    :: P_codeinnact  !> // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0 
  real,    intent(IN)    :: inns   !> // OUTPUT // Index of nitrogen stress active on growth in biomass // P_innmin to 1
  real,    intent(IN)    :: fgelflo   !> // OUTPUT // Frost index on the number of fruits // 0-1
  integer, intent(IN)    :: P_codeir  !> // PARAMETER // option of computing the ratio grain weight/total biomass: proportional to time(1), proportional to sum temperatures (2) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: P_vitircarb  !> // PARAMETER // Rate of increase of the carbon harvest index // g grain g plant -1 day-1 // PARPLT // 1 
  real,    intent(IN)    :: P_irmax  !> // PARAMETER // Maximum harvest index // SD // PARPLT // 1 
  real,    intent(IN)    :: P_vitircarbT  !> // PARAMETER // Heat rate of increase of the carbon harvest index  // g grain g plant-1 degree.day-1 // PARPLT // 1 
  real,    intent(IN)    :: somcourdrp  
  integer, intent(IN)    :: nmat  
  real,    intent(IN)    :: masec(0:1)  ! n-1 (0) & n (1)    // OUTPUT // Aboveground dry matter  // t.ha-1
  integer, intent(IN)    :: P_codetremp  !> // PARAMETER // option of heat effect on grain filling: yes (2), no (1) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: tcultmin  
  real,    intent(IN)    :: tcultmax   !> // OUTPUT // Crop surface temperature (daily maximum) // degree C
  real,    intent(IN)    :: P_tminremp  !> // PARAMETER // Minimal temperature for grain filling // degree C // PARPLT // 1
  real,    intent(IN)    :: P_tmaxremp  !> // PARAMETER // maximal temperature for grain filling // degree C // PARPLT // 1
  real,    intent(IN)    :: P_pgrainmaxi  !> // PARAMETER // Maximum weight of one grain (at 0% water content) // g // PARPLT // 1

  real,    intent(INOUT) :: ircarb(0:1)   ! n-1 (0) & n (1)    // OUTPUT // Carbon harvest index // g  grain g plant-1
  real,    intent(INOUT) :: nbgrains  
  real,    intent(INOUT) :: pgrain  
  real,    intent(INOUT) :: CNgrain   !> // OUTPUT // Nitrogen concentration of grains  // %
  real,    intent(INOUT) :: vitmoy   !> // OUTPUT // mean growth rate of the canopy (dans le livre c'est en g mais ca colle pas)  // g.m-2.d-1
  real,    intent(INOUT) :: nbgraingel  
  real,    intent(INOUT) :: pgraingel  
  real,    intent(INOUT) :: dltags   !> // OUTPUT // Growth rate of the grains  // t ha-1.j-1
  real,    intent(INOUT) :: ftempremp  
  real,    intent(INOUT) :: magrain(0:1)  ! n-1 (0) & n (1)  
  integer, intent(INOUT) :: nbj0remp   !> // OUTPUT // Number of shrivelling days //
  real,    intent(INOUT) :: pdsfruittot  

!: Variables locales
  integer :: ng  
  real    :: tefficace  




      !: magrain reste constant après nrec si P_codemsfinal = 1 (pour optimisation)
!      if (nrec > 0 .and. n > nrec .and. p%P_codemsfinal == 1) then
      !if (nrec > 0 .and. n > nrec ) then
      ! magrain(1) = magrain(0)
      !   return
      !endif
      !- fin modif


      if (ndrp == 0 .or. (nrec > 0 .and. n > nrec)) then
        ircarb(1) = 0.0
        return
      endif


      !: Attention incohérence entre tsol qui interdit la levée et tair qui amène à drp

      if (ndrp > 0 .and. nlev == 0) then
        call EnvoyerMsgHistorique(51)
        !stop
        call exit(9)
      endif

      !: Modif NB - 20/08/97
      !- Si la date de récolte butoir intervient avant le début remplissage,
      !- pas de grain.
      if (ndrp == nrecbutoir) then
        nbgrains = 0.0
        pgrain   = 0.0
        CNgrain  = 0.0
        return
      endif


      !: Calcul du nombre de grain
      if (n == ndrp) then

        !: Calcul de la vitesse de croissance moyenne P_nbjgrain avant ndrp
        !- NB - 08/04 : nbgrains remplacé par nbgrainpot
        vitmoy = 0.0
        ng = ndrp - P_nbjgrain + 1
        vitmoy = SUM(dltams(ng:ndrp))
        vitmoy = vitmoy / P_nbjgrain * 100

        !: nbgrains devient variétal avec P_nbgrmax
        nbgrains = ((P_cgrain * vitmoy) + P_cgrainv0) * P_nbgrmax

        !: Seuillage de nbgrains
        nbgrains = max(nbgrains, P_nbgrmin)
        nbgrains = min(nbgrains, P_nbgrmax)

        !: Option P_codazofruit
        if (P_codazofruit == 2 .and. P_codeinnact == 1) then
          nbgrains = nbgrains * inns
        endif
      else
        !: Dégats de GEL
        if (fgelflo < 1.0) then
          nbgraingel = nbgrains * (1.0 - fgelflo)
        else
          nbgraingel = 0.0
        endif
        pgraingel = (pgrain * nbgraingel) + pgraingel
        nbgrains = nbgrains - nbgraingel

        !: vitmoy est en g:m2:j
        if (P_codeir == 1) then
          ircarb(1) = P_vitircarb * (n - ndrp + 1)
          ircarb(1) = min(ircarb(1), P_irmax)
        endif

        if (P_codeir == 2) then
          ircarb(1) = P_vitircarbT * somcourdrp
        endif


        !: domi - 29/10/05: irazo deplacé dans Ngrain sinon on a pas la valeur
        !-                  de qnplante qui est calculée dans stressn

        if (nmat == 0 .or. n == nmat) then
          !: Vitesse de remplissage du grain
          dltags = ircarb(1) * masec(1)      &
                 - ircarb(0) * masec(0)


          !: Effet température
          ftempremp = 1.0
          if (P_codetremp == 1) then
            !: NB - Ajout d'un compteur jours sans remplissage

            !: pour les mini
            tefficace = tcultmin
            if (tefficace <= P_tminremp) then
              magrain(1) = magrain(0)
              ftempremp = 0.0
              dltags = 0.0
              nbj0remp = nbj0remp + 1
            endif

            !: pour les maxi
            tefficace = tcultmax
            if (tefficace >= P_tmaxremp) then
              magrain(1) = magrain(0)
              ftempremp = 0.0
              dltags = 0.0
              nbj0remp = nbj0remp + 1
            endif
          endif


          !: Le carbone des grains
          !- NB - le 23/05 : suppression des grains gelés
          !- NB - passage par dltags pour echaudage actif - le 11/01/02
          !-- magrain(1) = ircarb(1)*masec(n)*100 - pgraingel

          !- DR et AIG, on a trouvé un bug :
          !- On enlevait chaque jour le cumul du poids de grain gelé
          !- au lieu du poids de grain gelé du jour et ce meme apres
          !- que le gel soit fini d'ou diminution
          !- notable du rdt (dixit aig)
          magrain(1) = magrain(0) + (dltags * 100) - (pgrain * nbgraingel)
          magrain(1) = max(magrain(1), 0.0)
          if (magrain(1) > P_pgrainmaxi * nbgrains) then
            magrain(1) = P_pgrainmaxi * nbgrains
            dltags = 0.0
          endif
          if (nbgrains > 0.0) then
            pgrain = magrain(1) / nbgrains
          else
            pgrain = 0.0
          endif


          ! les calculs d'azote dans les grains ont ete deplacé dans Ngrain
        endif

        !: Entre maturité et récolte
        !- DR - 050506 : Dans le cas où on repousse la recolte,
        !-               on doit pourvoir garder masec.
        !-- if (nrec == 0 .and. nmat > 0) then
        if ((nrec == 0 .or. nrec == -999) .and. nmat > 0) then
           magrain(1) = magrain(0)
        endif
        if (n == nrec) magrain(1) = magrain(0)

        pdsfruittot = magrain(1)

      endif


return
end
 
 
