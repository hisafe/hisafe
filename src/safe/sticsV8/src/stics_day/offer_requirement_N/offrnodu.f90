! ******************************************************************************
! auourd'hui 07/02/08 aurelien a 2 ans ,  bon anniversaire
!  derniere modif DR le 28/06/05
! en test pour Eric justes nouveau mode de calcul de
!   azonod et hurnod et azorac et hurrac
!
!  Calcul de l'offre des nodules en azote  par fixation symbiotique
!              P. Burger, P. Debaeke, N. Brisson
!
! 1) Calcul d'une fixation potentielle fixpot (kg/ha/j), résultant de
!     la mise en place des nodules entre les stades début nodulation (DNO)
!     et fin nodulation (FNO), faisant intervenir  :
!   * une vitesse d'installation P_vitno (ratio de P_fixmax degré.jour-1)
!      P_fixmax = 6 kg/ha/j (Bouniols et al., 1991)
!   * une durée de vie de la population DVIENO (degrés.jours)
!   *  un effet inhibiteur du N minéral à partir du seuil de concentration
!      NNODMAX dans la zone de nodulation (P_profsem à P_profnod)
!      avec  - retard du stade DNO
!            - annulation de la vitesse P_vitno
!
! 2) Calcul de la fixation effective (FIXREEL = OFFRENOD) obtenue en
!     faisant agir les contraintes du milieu sur fixpot :
!   * contrainte azote minéral (FXN) faisant intervenir la variable
!      CUMOFFRN et les seuils FXN0 et FXN100
!   * contrainte sécheresse (FXW) calculé comme la proportion des
!      couches de sol ayant une humidité supérieure au seuil HURNOD
!      entre P_profsem et P_profnod
!   * contrainte thermique (FXT) faisant intervenir 4 températures
!     cardinales P_TEMPNOD1, P_TEMPNOD2, P_TEMPNOD3 et P_TEMPNOD4 (Tmoyenne
!     ou maxi) calculées au niveau de chaque couche
!   * contrainte anoxie (FXA)
!     hyp 1 : FIXREEL = fixpot* min(FXW,FXN,FXT) * FXA
!     hyp 2 : FIXREEL = fixpot* FXW * FXN * FXT * FXA
!     hyp 3 : FIXREEL = fixpot* min(FXW,FXN) * FXT * FXA
! ******************************************************************************
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!! This module calculates the nitrogen fixation by legumes.
!> - Stics book paragraphe 8.7, page 100-102
!>
!! In STICS, symbiotic N2 fixation by legumes is simulated considering three criteria. The first of these is the presence of nodules, depending on their own
!! phenology and lifespan and also on an inhibiting effect of excessive nitrate in the soil. The second is the capacity of the plant to feed these supplementary
!! symbiotic organs depending on plant growth rate, and the third is the soil-dependent physicochemical conditions allowing optimal nodule activity:
!! soil nitrate level, water deficit, anoxia and temperature (Debaeke et al., 2001; Voisin et al., 2003). The first two criteria define the potential N2 fixation
!! while the third defines the actual N2 fixation.
!> - The potential N2 fixation (fixpot in kg N ha-1 day-1) is calculated as the product of a phenology-dependent coefficient propfixpot (between 0 and 1) and
!!   the maximal fixation capacity of the crop (fixmax in kg N ha-1 day-1).
!!   The propfixpot coefficient varies according to growing degree-days calculated as for root growth. The fixation process begins at the idno stage (defined by
!!   the thermal duration stlevdno) and stops at the ifno stage (defined by the thermal duration stdnofno). The potential curve then decreases until the death
!!   of nodules, corresponding to the ifvino stage, during the stfnofvino thermal duration. The establishment rate of nodules between idno and ifno stages depends
!!   on the potential rate vitno and on growing degree-days.
!!   It may be inhibited by high mineral nitrogen levels, under the control of nodn which is nil when soil mineral nitrate concentration exceeds the threshold
!!   concnodseuil (in kg N ha-1 mm-1 water), and otherwise is equal to 1.0.
!!   The maximal fixation capacity of the crop fixmax is calculated from above-ground biomass growth rate. The fixmaxveg parameter defines the N
!!   fixed per ton of produced vegetative dry matter and the fixmaxgr parameter defines the amount of N fixed per ton of grain dry matter produced.
!> - The actual N2 fixation
!!   To calculate the actual N2 fixation (fixreel, in kg N ha-1 day-1), the potential N2 fixation fixpot is multiplied by indices (varying between 0 and 1)
!!   corresponding to constraints due to anoxia (fxa), temperature (fxt), water content (fxw) and soil mineral nitrogen (fxn).
!!   Limitation by temperature (fxt) uses the soil temperature in the nodulation zone and is a trapezoidal function defined by four cardinal temperatures
!!   (tempnod1 to tempnod4).
!!   The water stress factor (fxw) is estimated from the proportion of elementary soil layers in the nodulation area whose water content hur is at least
!!   as high as the permanent wilting point humin.
!!   Limitation by anoxia (fxa) is calculated according to the same principle, as the proportion of elementary soil layers which are in aerobic conditions
!!   using the anox variable.
!!   Finally the fixation is partially inhibited when the mean amount of mineral nitrogen in the rooting zone (azorac/zrac, in kg N ha-1 cm-1 soil) exceeds
!!   the threshold concNrac100, and is fully inhibited when it exceeds the threshold concNrac0.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
subroutine offrnodu(n,nlev,zrac,dtj,P_profsem,P_profnod,nbCouches,nit,hur,P_concNnodseuil,P_vitno,P_codefixpot, &
                    P_fixmax,P_fixmaxveg,P_fixmaxgr,dltams,dltags,P_concNrac100,P_concNrac0,P_codefxn,humin,      &
                    tcultmin,tcultmax,tsol,P_tempnod1,P_tempnod2,P_tempnod3,P_tempnod4,anox,                  &
                    fixreel,somcourno,P_stlevdno,ndno,P_stdnofno,nfno,P_stfnofvino,nfvino,nodn,             &
                    propfixpot,fixpotfno,fixmaxvar,fixpot,fxn,fxw,fxt,fxa)

  implicit none

!: Arguments
  integer, intent(IN)    :: n  
  integer, intent(IN)    :: nlev  
  real,    intent(IN)    :: zrac      !> // OUTPUT // Depth reached by root system // cm
  real,    intent(IN)    :: dtj                     ! (n)       // OUTPUT // Daily efficient temperature for the root growing  // degree C.j-1
  real,    intent(IN)    :: P_profsem  !> // PARAMETER // Sowing depth // cm // PARTEC // 1 
  real,    intent(IN)    :: P_profnod  !> // PARAMETER // nodulation depth // cm // PARPLT // 1 
  integer, intent(IN)    :: nbCouches  
  real,    intent(IN)    :: nit(nbCouches)          ! P_profsem to P_profnod, P_profsem to zrac
  real,    intent(IN)    :: hur(nbCouches)          ! P_profsem to P_profnod, P_profsem to zrac
  real,    intent(IN)    :: P_concNnodseuil  !> // PARAMETER // maximal soil nitrogen threshold for nodule onset  // kg.ha-1.mm-1 // PARPLT // 1 
  real,    intent(IN)    :: P_vitno  !> // PARAMETER // "rate of nodule onset expressed as a proportion of  P_fixmax  per degree day " // nb degrés.jour-1 // PARPLT // 1
  integer, intent(IN)    :: P_codefixpot  !> // PARAMETER // option of calculation of the maximal symbiotic fixation // code 1/2/3 // PARPLT // 0 
  real,    intent(IN)    :: P_fixmax  !> // PARAMETER // maximal symbiotic fixation // kg ha-1 j-1 // PARPLT // 1      // OUTPUT // Maximal symbiotic uptake // kg ha-1 j-1
  real,    intent(IN)    :: P_fixmaxveg  !> // PARAMETER // parameter to calculate symbiotic fixation as a function of the plant growth   // kg N  (t MS)-1 // PARPLT // 1 
  real,    intent(IN)    :: P_fixmaxgr  !> // PARAMETER // parameter to calculate symbiotic fixation as a function of the plant growth   // kg N  (t MS)-1 // PARPLT // 1 
  real,    intent(IN)    :: dltams                  ! (n)       // OUTPUT // Growth rate of the plant  // t ha-1.j-1
  real,    intent(IN)    :: dltags      !> // OUTPUT // Growth rate of the grains  // t ha-1.j-1
  real,    intent(IN)    :: P_concNrac100  !> // PARAMETER // soil nitrogen threshold for full nodule activity  // kg.ha-1.mm-1 // PARPLT // 1 
  real,    intent(IN)    :: P_concNrac0  !> // PARAMETER // soil nitrogen threshold forbiding nodule activity // kg.ha-1.mm-1 // PARPLT // 1 
  integer, intent(IN)    :: P_codefxn  !> // PARAMETER // option to activate the chosen way to compute fxN // code 1/2 // PARAM // 0 
  real,    intent(IN)    :: humin(nbCouches)        ! P_profsem to P_profnod
  real,    intent(IN)    :: tcultmin  
  real,    intent(IN)    :: tcultmax      !> // OUTPUT // Crop surface temperature (daily maximum) // degree C
  real,    intent(IN)    :: tsol(nbCouches)         ! P_profsem to P_profnod
  real,    intent(IN)    :: P_tempnod1  !> // PARAMETER // cardinal temperature for nodule activity   // degree C // PARPLT // 1
  real,    intent(IN)    :: P_tempnod2  !> // PARAMETER // cardinal temperature for nodule activity   // degree C // PARPLT // 1
  real,    intent(IN)    :: P_tempnod3  !> // PARAMETER // cardinal temperature for nodule activity   // degree C // PARPLT // 1
  real,    intent(IN)    :: P_tempnod4  !> // PARAMETER // cardinal temperature for nodule activity   // degree C // PARPLT // 1
  real,    intent(IN)    :: anox(nbCouches)         ! P_profsem to P_profnod

  real,    intent(INOUT) :: fixreel      !> // OUTPUT // Actual rate of symbiotic uptake // kg ha-1 j-1
  real,    intent(INOUT) :: somcourno  
  real,    intent(INOUT) :: P_stlevdno  !> // PARAMETER // phasic duration between emergence and the beginning of nodulation  // degrés.jours // PARPLT // 1 
  integer, intent(INOUT) :: ndno  
  real,    intent(INOUT) :: P_stdnofno  !> // PARAMETER // phasic duration between the beginning and the end of nodulation // degree.days // PARPLT // 1 
  integer, intent(INOUT) :: nfno  
  real,    intent(INOUT) :: P_stfnofvino  !> // PARAMETER // phasic duration between the end of the nodulation and the end of the nodule life   // degrés.jours // PARPLT // 1 
  integer, intent(INOUT) :: nfvino  
  real,    intent(INOUT) :: nodn      !> // OUTPUT // Nitrogen stress effect on nodosities establishment // 0 ou 1
  real,    intent(INOUT) :: propfixpot  
  real,    intent(INOUT) :: fixpotfno  
  real,    intent(INOUT) :: fixmaxvar  
  real,    intent(INOUT) :: fixpot      !> // OUTPUT // Potential rate of symbiotic uptake // kg ha-1 j-1
  real,    intent(INOUT) :: fxn      !> // OUTPUT // Nitrogen effect on symbiotic uptake // 0-1
  real,    intent(INOUT) :: fxw      !> // OUTPUT // Water effect on symbiotic uptake // 0-1
  real,    intent(INOUT) :: fxt      !> // OUTPUT // Temperature effect on symbiotic uptake // 0-1
  real,    intent(INOUT) :: fxa      !> // OUTPUT // Anoxic effect on symbiotic uptake // 0-1


!: Variables locales
  real    :: azonod  !>  
  real    :: hurnod  !>  
  real    :: concNnod  
  real    :: amplsurf  !>  
  real    :: amplz  !>  
  real    :: azorac  !>  
  real    :: concNrac  !>  
  real    :: hurrac  
  real    :: fxn3  !>  
  real    :: fxtmoy  !>  
  real    :: fxn2  
  integer :: iz  

!: Fonction(s)
  real    :: FXTFONC  
  real    :: epcouche  


      if (nlev == 0) return

      if (zrac <= 0.0) then
        fixreel = 0.0
        return
      endif

! paramètres
!     P_stlevdno = 150.0
!     P_stdnofno = 500.0
!     P_stfnofvino = 300.0
!     P_fixmax = 6
!     P_vitno = 0.003
!     P_profnod = 40.0
!     P_concNnodseuil = 13.0e-1
!     P_codefxn = 2
!     cumoffrN0 = 3.0
!     cumoffrN100 = 1.0
! * azorac en kg ha-1 cm-1
!     azorac0 = 3.0
!     azorac100 = 1.0
! * conNrac en kg ha-1 mm-1
!     P_concNrac0 = 1.2
!     P_concNrac100 = 0.4
! * P_hunod en mm cm-1
!     P_hunod = 1.5
!     P_tempnod1 = 0.0
!     P_tempnod2 = 30.0
!     P_tempnod3 = 36.0
!     P_tempnod4 = 50.0
!     codefxt = 2
!     codecombi = 1


      ! --------------------------------------!
      !  1. Calcul des sommes de température  !
      ! --------------------------------------!
      somcourno = somcourno + dtj
      if (somcourno >= P_stlevdno .and. ndno == 0 .and. nlev > 0) then
        ndno = n
        P_stlevdno = somcourno
        somcourno = 0.0
!        write(ficdbg,*) 'dno',n,somcourno,P_stlevdno,dtj
      endif

      if (somcourno >= P_stdnofno .and. nfno == 0 .and. ndno > 0) then
        nfno = n
        P_stdnofno = somcourno
        somcourno = 0.0
!        write(ficdbg,*) 'fno',n,somcourno,P_stdnofno,dtj
      endif

      if (somcourno >= P_stfnofvino .and. nfvino == 0 .and. nfno > 0) then
        nfvino = n
        P_stfnofvino = somcourno
        somcourno = 0.0
!        write(ficdbg,*) 'fvino',n,somcourno,P_stfnofvino,dtj
      endif


      ! ----------------------------------------------------!
      ! 2. Calcul de la contrainte azotée sur fixpot: nodn  !
      ! ----------------------------------------------------!
      azonod = 0.0
      hurnod = 0.0

      do iz = int(P_profsem), int(P_profnod)
        azonod = azonod + nit(iz)
        hurnod = hurnod + hur(iz)
      end do



      ! concentration en azote (kg.ha-1.mm-1)
      concNnod = azonod / hurnod

      ! contrainte  = NODN
      if (concNnod < P_concNnodseuil) then
        nodn = 1.0
      else
        nodn = 0.0
      endif

      ! ---------------------------------------------!
      ! 3. Calcul de la fixation potentielle fixpot  !
      ! ---------------------------------------------!
      !- 30/04/04 - fixpot entre 0 et 1 (P_fixmax remplacé par 1.0)
      !- 10/06/2004 - pb avec l'initialisation de fixpotfno - il faut le mettre a zéro !
      !- on introdiut une variable intermediaire propfixpot sinon pb apres nfno
      if (n < ndno .and. ndno > 0) propfixpot = 0.0
      if (n > nfvino .and. nfvino > 0) propfixpot = 0.0
      if (n >= ndno .and. ndno > 0 .and. nfno == 0) then
        propfixpot = propfixpot + (1.0 * P_vitno * dtj * nodn)
        propfixpot = min(propfixpot, 1.0)
!        write(ficdbg,'(i4,a15,f16.12)')n,'propfixpot = ',propfixpot
      endif
      if (n == nfno) then
        ! 27/07/04 ML et DR j'avais vraisemblament un pb a fno on a pas tenu compte a cet endroit de la modif
        !                   fixpot compris entre 0 et 1
        fixpotfno = propfixpot
!        write(ficdbg,'(a15,i4,f16.12)')'fixpotfno',n,fixpotfno
      endif
      if (n >= nfno .and. nfvino == 0 .and. nfno > 0) then
        propfixpot = propfixpot - (fixpotfno / P_stfnofvino * dtj)
        propfixpot = max(propfixpot, 0.0)
      endif

!      write(ficdbg,'(i4,a15,f16.12)')n,'propfixpot = ',propfixpot
!      write(ficdbg,'(4i4)')n,ndno,nfvino,nfno
!      write(ficdbg,'(i4,a15,f16.12)')n,'fixpotfno = ',fixpotfno

      !: Option 1 : P_fixmax fixé
      if (P_codefixpot == 1) then
        fixmaxvar = P_fixmax
        fixpot = propfixpot * fixmaxvar
      endif

      !: Option 2 : P_fixmax piloté par la croissance
      !- ML le 25/05/07: l'option 2 (en fonction de la demande en azote)
      !                  a été supprimée: l'option 3 devient donc l'option 2
      if (P_codefixpot == 2) then
        fixmaxvar = P_fixmaxveg * max(dltams - dltags,0.) + (P_fixmaxgr * dltags)
        fixpot = fixmaxvar * propfixpot
      endif




      ! ----------------------------------!
      !  4. Calcul de l'effet azote: FXN  !
      ! ----------------------------------!

      ! Choix final FXN option 3 (P. Burger 08/01)
      ! mais on laisse la programmation de FX1 et FX2 en commentaires
      ! *- FXN1 : FXN en fonction de cumoffrN
      ! --      if (cumoffrN < cumoffrN100) then
      ! --        fxn1 = 1.0
      ! --      else
      ! --        fxn1 = 1.0/(cumoffrN0-cumoffrN100)*(cumoffrN0-cumoffrN)
      ! --      endif
      ! --      fxn1 = max(fxn1,0.0)

      ! FXN2 : FXN en fonction de azorac
      azorac = 0.0
      hurrac = 0.0
      do iz = int(P_profsem),int(zrac)
        azorac = azorac + nit(iz)
        hurrac = hurrac + hur(iz)
      end do
      concNrac = azorac / hurrac


      if (azorac < P_concNrac100 * zrac) then
        fxn2 = 1.0
      else
        fxn2 = 1.0 / (P_concNrac0 - P_concNrac100) * (P_concNrac0 - (azorac / zrac))
      endif
      fxn2 = max(fxn2, 0.0)

      ! FXN3 : FXN en fonction de concNrac
      if (concNrac < P_concNrac100) then
        fxn3 = 1.0
      else
        fxn3 = 1.0 / (P_concNrac0 - P_concNrac100) * (P_concNrac0 - concNrac)
      endif
      fxn3 = max(fxn3, 0.0)


! --      if (P_codefxn == 1) fxn = fxn1
!   domi 23/06/05   pb dans le cas ou P_codefxn = 1 dans ce cas fxn = 0 d'ou pas
!   de fixation symbiotique
      fxn = 1.0
      if (P_codefxn == 2) fxn = fxn2
      if (P_codefxn == 3) fxn = fxn3



      ! ------------------------------------------ !
      !  5. Calcul de l'effet teneur en eau: FXW   !
      ! ------------------------------------------ !
      fxw = 0.0
      do iz = int(P_profsem),int(P_profnod)
        ! sb le 05/03/07 : on essaie de rendre ça un peu plus continue ...
        if (hur(iz) > humin(iz)) fxw = fxw + epcouche(iz, P_profsem, P_profnod)
      end do
      fxw = fxw / (P_profnod - P_profsem)


      ! ---------------------------------------- !
      !  6. Calcul de l'effet température: FXT   !
      ! ---------------------------------------- !
      !- Choix final FXT = FXTMOY (P. Burger 08/01)
      !- mais on laisse la programmation de FXTMAX en commentaires

      ! recalcul d'une température maxi de sol
      amplsurf = tcultmax - tcultmin
      fxtmoy = 0.0
      !-- fxtmax = 0.0
      do iz = int(P_profsem), int(P_profnod)
        amplz = amplsurf * exp(-8.23 * iz * 1e-2)
        !-- tsolmax = tsol(iz) + (amplz / 2)

        !- sb le 05/03/07 : on essaie de rendre ça un peu plus continue ...
        !-- fxtmoy = fxtmoy + FXTFONC(tsol(iz),P_tempnod1,P_tempnod2,P_tempnod3, P_tempnod4)
        fxtmoy = fxtmoy + FXTFONC(tsol(iz),P_tempnod1,P_tempnod2,P_tempnod3, P_tempnod4) * epcouche(iz, P_profsem, P_profnod)

        !-- fxtmax = fxtmax+fxtfonc(tsolmax,P_tempnod1,P_tempnod2,P_tempnod3,P_tempnod4)
      end do

      fxtmoy = fxtmoy / (P_profnod-P_profsem)
      !-- fxtmax = fxtmax/(int(P_profnod)-int(P_profsem)+1)

! --      if (codefxt == 1) then
! --        fxt = fxtmoy
! --      else
! --        fxt = fxtmax
! --      endif

      fxt = fxtmoy


      ! ----------------------------------- !
      !  7. Calcul de l'effet anoxie: FXA   !
      ! ----------------------------------- !
      fxa = 0.0
      do iz = int(P_profsem), int(P_profnod)
        !- sb le 05/03/07 : on essaie de rendre ça un peu plus continue ...
        !-- fxa = fxa+anox(iz)
        fxa = fxa + anox(iz) * epcouche(iz, P_profsem, P_profnod)
      end do
      fxa = 1. - (fxa / (P_profnod-P_profsem))


      ! --------------------------------------------------------------- !
      !  8. Calcul de l'effet combiné température, eau, azote et anoxie !
      ! --------------------------------------------------------------- !
      !    Choix 3 P_Ph. Burger
      ! --      if (codecombi == 1) fixreel = min(fxt,fxw,fxn)*fxa*fixpot
      ! --      if (codecombi == 2) fixreel = fxt*fxw*fxn*fxa*fixpot
      ! --      if (codecombi == 3) fixreel = fxt*min(fxw,fxn)*fxa*fixpot

      fixreel = fxt * min(fxw,fxn) * fxa * fixpot


return
end subroutine offrnodu



! -------------------------------------------- !
!             FONCTION FXTFONC                 !
! -------------------------------------------- !
real function FXTFONC(t,t1,t2,t3,t4)

  implicit none

!: Arguments
  real, intent(IN) :: t  
  real, intent(IN) :: t1  
  real, intent(IN) :: t2  
  real, intent(IN) :: t3  
  real, intent(IN) :: t4  

      if (t <= t1) fxtfonc = 0.0
      if (t > t1 .and. t <= t2) fxtfonc = (t-t1) / (t2-t1)
      if (t > t2 .and. t <= t3) fxtfonc = 1.0
      if (t > t3 .and. t <= t4) fxtfonc = (t-t4) / (t3-t4)
      if (t > t4) fxtfonc = 0.0

return
end function FXTFONC
 
 
