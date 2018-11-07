!> Calculation of nitrification, OM decomposition and N mineralization-immobilization
!> **********************************************************************************
!> - Stics book paragraph 8, pages 141-147
!>
!! The net N mineralization, i.e. the net production of mineral nitrogen by the soil, is the sum of two components:
!>- Humus mineralization, which results from the decomposition of stabilized organic matter in soil. This is a permanent process which always leads to a
!!   release of mineral N, i.e. a positive net mineralization, called 'basal' mineralization.
!>- The mineralization of organic residues, which is associated with the decomposition of crop residues (straw, roots, etc.) or organic wastes added to the soil.
!!  It is a process which is very variable in time, linked to the application of organic residues. During a first phase after the addition of residues,
!!   the mineralization can be positive or negative (immobilization of soil mineral N). During the second phase, it is positive through the
!!   're-mineralization' process which releases N coming from either the residue or the microbial biomass which has decomposed it.
!!
!! In STICS we use a different function for decomposition of humus and (fresh) organic residues.
!>- Mineralization of soil organic matter (humus):
!!   Although the soil below the plough depth and the subsoil may contain important reserves of organic C and N, their decomposition rate appears to be slow or
!!   negligible compared to the upper soil layer (e.g. Fontaine et al., 2007). In STICS, mineralization is assumed to occur up to a maximum depth (PROFHUMS, in cm)
!!   and be negligible below that depth. The basal mineralization rate (VMINH, in kg N ha-1 day-1) depends on the amount of
!!   active soil organic nitrogen (NHUM, in t ha-1), the soil type (its clay and calcium carbonate contents) and environmental factors,
!!   namely the water content and temperature in each soil layer (ftr and fth).
!>- Mineralization of organic residues:
!!   STICS simulates the decomposition of various organic residues and their humification due to microbial activity, as described by Nicolardot et al. (2001)
!!   for crop residues. Nitrogen mineralization depends on the decomposition rate of organic residues (i.e. carbon fluxes), their C/N ratio (CSURNREST),
!!   the C/N ratio of the zymogeneous biomass (CNBIO) and of the newly formed humified matter (CNHUM).
!!   Ten categories of organic residues have been defined:
!>         - 1) crop residues from mature crops (e.g. straw),
!>         - 2) crop residues from young plants (e.g. catch crops),
!>         - 3) farmyard manures,
!>         - 4) composts,
!>         - 5) sewage sludges,
!>         - 6) vinasses,
!>         - 7) animal horn
!>         - 8, 9, 10) others (any other residue).
!!
!!   The fate of residues in each category (r) is followed separately.
!!   The model is defined by 6 parameters, most of them being residue-dependent: two decomposition rate constants (KRES and KBIOG, in day-1),
!!   two partition parameters (YRESG and HRES) and two C/N ratios (CNBIO and CNHUM). For a given category, the parameters are either constant
!!   (KBIOG, YRESG, and CNHUM=1/WHG) or dependent upon the C/N ratio of the organic residue (CSURNREST).
!!   The decomposition rate of organic residues (DCRES, in kg C ha-1 day-1) is assumed to follow first order kinetics against the amount of
!!   decomposable carbon (CRES) and depends on their nature (kres), on soil temperature (fth), water content (ftr) and the available soil nitrogen
!!   in the vicinity of residues (fn).
!!   The soil moisture content influences decomposition similarly to the decomposition of humified materials whereas the soil temperature has a specific effect
!!   on the decomposition rate of organic residues. The thermal effect on residue mineralization is similar to the logistic function defined for humus decomposition
!!   but with different parameters.
!!   The net N mineralization rate (DN, in kg N ha-1 day-1, positive or negative) resulting from residue decomposition is calculated as the complement of
!!   the variation in the three organic pools (dN = -(dNrest+dNbiot+dNhum)).
!!
!!   STICS simulates the effects of N when mineral N is limiting. A lack of mineral nitrogen may successively:
!!      S1) reduces the decomposition rate
!!      S2) reduces the N/C ratio of microbial biomass
!!      S3) reduces the N/C ratio of newly formed humus
!!      S4) increases the C and N mineralization from humus through the "priming effect".
!!   At each step (S1-S4), the new available mineral N is caculated. If this amount is positive, then program skips the following steps.
!!   remineralization of nitrogen.
!!   The available mineral N available for residue decomposers in the layer [proftrav, profres] may be a fraction of the mineral N if
!!   codeNmindec is activated. In this case, the fraction of available Nmin is equal to rapNmindec*Cres+fNmindecmin.
!!

! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine mineral(nbCouches, nbResidus, P_profhum, P_argi, P_calc, P_codefente, P_codeNmindec,  &
                   P_fhminsat,P_fmin1,P_fmin2,P_fmin3,P_FTEMh,P_FTEMha,P_FTEMr,P_FTEMra,FTEMhb,FTEMrb,P_hminm,  &
                   P_hoptm, var_TREFh, P_Wh, kres, P_kbio, Wb, P_yres, hres, P_fNCbiomin, P_fredlN, &
                   P_fredkN, P_rapNmindec, P_fNmindecmin, P_fredNsup, P_Primingmax,                     &
                   tsol, hucc, hur, dacouche, humin, sat, itrav1, itrav2,    & ! IN
                   Cres,Nres,Cbio,Nbio,      & !INOUT
                   Chum,Nhum,Chuma,Nhuma,Chumi,Nhumi,Chumt,Nhumt,Cr,Nr,Cb,Nb,NCbio,    &
                   amm,nit,CO2hum,CO2res,CO2sol,vminr,cumvminh,cumvminr,&
                   QCO2sol,QCO2res,QCO2hum,Qminh,Qminr,QCprimed,QNprimed,tnhc,tnrc,&
                   Cnondec,Nnondec,Cmulchnd,Nmulchnd,Cmulchdec,Nmulchdec,Cmulch,Nmulch,Cbmulch,Nbmulch,cum_immob,QCO2mul)

  implicit none

  integer, intent(IN) :: nbCouches
  integer, intent(IN) :: nbResidus
  real,    intent(IN) :: P_argi          !> // PARAMETER // clay content in the first layer  // % // PARSOL // 1
  real,    intent(IN) :: P_calc          !> // PARAMETER // calcium carbonate content in the surface layer // % // PARSOL // 1
  integer, intent(IN) :: P_codefente     !> // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0
  real,    intent(IN) :: P_fhminsat      !> // PARAMETER // soil mineralisation rate at water saturation // SD // PARAM // 1
  real,    intent(IN) :: P_fmin1         !> // PARAMETER // constant of mineralization potential rate // 10000.day-1 // PARAM // 1
  real,    intent(IN) :: P_fmin2         !> // PARAMETER // constant of mineralization potential rate // % // PARAM // 1
  real,    intent(IN) :: P_fmin3         !> // PARAMETER // constant of mineralization potential rate // % // PARAM // 1
  real,    intent(IN) :: P_FTEMh         !> // PARAMETER // constant of the temperature function for humus decomposition // °K-1 // PARAM // 1
  real,    intent(IN) :: P_FTEMha        !> // PARAMETER // constant of the temperature function for humus decomposition // * // PARAM // 1
  real,    intent(IN) :: P_FTEMr         !> // PARAMETER // constant of the temperature function for residue decomposition // °K-1 // PARAM // 1
  real,    intent(IN) :: P_FTEMra        !> // PARAMETER // constant of the temperature function for residue decomposition // * // PARAM // 1
  real,    intent(IN) :: P_hminm         !> // PARAMETER // moisture (proportion of field capacity) below which mineralisation rate is nil // g.g-1 // PARAM // 1
  real,    intent(IN) :: P_hoptm         !> // PARAMETER // moisture (proportion of field capacity) above which mineralisation rate is maximum // g.g-1 // PARAM // 1
  real,    intent(IN) :: P_profhum       !> // PARAMETER // soil depth below which biological activity is nil  (max.60 cm) // cm // PARSOL // 1
  real,    intent(IN) :: P_Wh            !> // PARAMETER // ratio N/C of humus // g.g–1 // PARAM // 1
  real,    intent(IN) :: tsol(nbCouches)
  real,    intent(IN) :: var_TREFh(200)     ! dimension ?
  real,    intent(IN) :: hucc(nbCouches)
  real,    intent(IN) :: hur(nbCouches)
  real,    intent(IN) :: dacouche(nbCouches)
  real,    intent(IN) :: humin(nbCouches)
  real,    intent(IN) :: sat(nbCouches)
  real,    intent(IN) :: kres(nbResidus)
  real,    intent(IN) :: P_kbio(nbResidus) !> // PARAMETER // decay rate of microbial biomass // d-1 // PARAM // 1
  real,    intent(IN) :: Wb(nbResidus)
  real,    intent(IN) :: P_yres(nbResidus) !> // PARAMETER // C assimilation yield of residue by microbial biomass // g.g-1 // PARAM // 1
  real,    intent(IN) :: hres(nbResidus)
  integer, intent(IN) :: itrav1
  integer, intent(IN) :: itrav2
  real,    intent(IN) :: FTEMhb
  real,    intent(IN) :: FTEMrb
  real,    intent(IN) :: P_fNCbiomin     !> // PARAMETER // maximal reduction factor of the ratio N/C of the microbial biomass when nitrogen limits decomposition (between 0 and 1) // SD // PARAM // 1
  real,    intent(IN) :: P_fredlN        !> // PARAMETER // reduction factor of decomposition rate of biomass when mineral N is limiting // SD // PARAM // 1
  real,    intent(IN) :: P_fredkN        !> // PARAMETER // reduction factor of decomposition rate of residues when mineral N is limiting // SD // PARAM // 1
!  real,    intent(IN) :: P_CroCo(nbResidus) !> // PARAMETER // parameter of decomposition rate of residues  // SD // PARAM // 1

! Nouveaux parametres Elsa
  integer, intent(IN) :: P_codeNmindec   !> // PARAMETER // option to activate the available N :yes (1), no(2) // code 1/2 // PARSOL // 0
  real,    intent(IN) :: P_fredNsup      !> // PARAMETER // additional reduction factor of residues decomposition rate when mineral N is very limited in soil // SD // PARAM // 1
  real,    intent(IN) :: P_rapNmindec    !> // PARAMETER // slope of the linear relationship between the fraction of mineral N available for residue decomposition and the amount of C in decomposing residues (0.001) // g.g-1 // PARAMV6 // 1
  real,    intent(IN) :: P_fNmindecmin   !> // PARAMETER // minimal fraction of mineral N available for residues decomposition (if codeNmindec is activated) // SD // PARAM //1
  real,    intent(IN) :: P_Primingmax    !> // PARAMETER // maximum priming ratio (relative to SOM decomposition rate) // SD //PARAM // 1

  real,    intent(INOUT) :: Cres(nbCouches,nbResidus)               ! 1 to int(P_profhum), 1:8
  real,    intent(INOUT) :: Nres(nbCouches,nbResidus)               ! 1 to int(P_profhum), 1:8
  real,    intent(INOUT) :: Cbio(nbCouches,nbResidus)
  real,    intent(INOUT) :: Nbio(nbCouches,nbResidus)
  real,    intent(INOUT) :: Chum(nbCouches)   !> // OUTPUT // Quantity of C in humus in each layer // kg.ha-1
  real,    intent(INOUT) :: Nhum(nbCouches) !> // OUTPUT // Quantity of N in humus in each layer // kg.ha-1
  real,    intent(INOUT) :: amm(nbCouches)  
  real,    intent(INOUT) :: nit(nbCouches)  
  real,    intent(INOUT) :: Cb         !> // OUTPUT // Quantity of C in the  biomass decomposing organic residues // kg.ha-1
  real,    intent(INOUT) :: Nb         !> // OUTPUT // Amount of N in the biomass decaying organic residues // kg.ha-1
  real,    intent(INOUT) :: Chumt      !> // OUTPUT // Total amount of C in soil humus (active + inert fractions) // t.ha-1
  real,    intent(INOUT) :: Nhumt      !> // OUTPUT // Total quantity of N humus (active + inert fractions) in the soil // kg.ha-1
  real,    intent(INOUT) :: CO2hum     !> // OUTPUT // daily C mineralized from soil humus as CO2// kg.ha-1.d-1
  real,    intent(INOUT) :: CO2res     !> // OUTPUT // daily C mineralized from residues as CO2 // kg.ha-1.d-1
  real,    intent(INOUT) :: CO2sol     !> // OUTPUT // daily C mineralized from soil (heterotrophic respiration) // kg.ha-1.d-1
  real,    intent(INOUT) :: Cr         !> // OUTPUT // Amount of C in soil organic residues // kg.ha-1
  real,    intent(INOUT) :: Nr         !> // OUTPUT // Amount of N remaining in the decaying organic residues in the soil  // kg.ha-1
  real,    intent(INOUT) :: cumvminh   !> // OUTPUT // daily N mineralized from humus // kg.ha-1.d-1
  real,    intent(INOUT) :: cumvminr   !> // OUTPUT // daily N mineralized from organic residues // kg.ha-1.d-1
  real,    intent(INOUT) :: NCbio      !> // OUTPUT // N/C ratio of biomass decomposing organic residues // g.g-1
  real,    intent(INOUT) :: Chuma         !> // OUTPUT //  Amount of C in actif pool of humus // kg.ha-1
  real,    intent(INOUT) :: Nhuma      !> // OUTPUT // Amount of active N in the humus pool  // kg.ha-1
  real,    intent(INOUT) :: QCO2res       !> // OUTPUT //  cumulative quantity of C mineralised from residues // kg.ha-1
  real,    intent(INOUT) :: QCO2hum       !> // OUTPUT //  cumulative quantity of C mineralised from humus // kg.ha-1
  real,    intent(INOUT) :: QCO2sol    !> // OUTPUT // cumulative C mineralized from soil (heterotrophic respiration) // kg.ha-1
  real,    intent(INOUT) :: Qminh      !> // OUTPUT // cumulative N mineralized during decomposition of humus // kg.ha-1
  real,    intent(INOUT) :: Qminr      !> // OUTPUT // cumulative N mineralized during decomposition of organic residues // kg.ha-1
  real,    intent(INOUT) :: tnhc       !> // OUTPUT // cumulative normalized time for the mineralisation of humus // d
  real,    intent(INOUT) :: tnrc       !> // OUTPUT // cumulative normalized time for the mineralisation of organic residues // d
  real,    intent(INOUT) :: vminr
  real,    intent(INOUT) :: Cnondec(10)   !> // OUTPUT // undecomposable C stock of residue ir at soil surface //  t.ha-1
  real,    intent(INOUT) :: Nnondec(10)   !> // OUTPUT // undecomposable N stock of residue ir at soil surface // kg.ha-1
  real,    intent(INOUT) :: Cmulchnd     !> // OUTPUT // quantity of C in undecomposable plant mulch // kg.ha-1
  real,    intent(INOUT) :: Nmulchnd     !> // OUTPUT // quantity of N in undecomposable plant mulch // kg.ha-1
  real,    intent(INOUT) :: Cmulchdec     !> // OUTPUT // quantity of C in undecomposable plant mulch // kg.ha-1
  real,    intent(INOUT) :: Nmulchdec     !> // OUTPUT // quantity of N in undecomposable plant mulch // kg.ha-1
  real,    intent(INOUT) :: Cmulch     !> // OUTPUT // quantity of C in total mulch // kg.ha-1
  real,    intent(INOUT) :: Nmulch     !> // OUTPUT // quantity of N in total mulch // kg.ha-1
  real,    intent(INOUT) :: Cbmulch     !> // OUTPUT // quantity of C in microbial biomass decomposing mulch // kg.ha-1
  real,    intent(INOUT) :: Nbmulch     !> // OUTPUT // quantity of N in microbial biomass decomposing mulch // kg.ha-1
  real,    intent(INOUT) :: cum_immob  !> // OUTPUT // cumulative N immobilized during decomposition of organic residues // kg.ha-1

! Nouveaux parametres Elsa
  real,    intent(INOUT) :: QCprimed      !> // OUTPUT //  cumulative quantity of C derived from humus and mineralised by priming // kg.ha-1
  real,    intent(INOUT) :: QNprimed      !> // OUTPUT //  cumulative quantity of N derived from humus and mineralised by priming // kg.ha-1

  real,    intent(INOUT) :: Chumi         !> // OUTPUT //  Amount of C in inert pool of humus // kg.ha-1
  real,    intent(INOUT) :: Nhumi         !> // OUTPUT //  Amount of N in inert pool of humus // kg.ha-1
  real,    intent(INOUT) :: QCO2mul         !> // OUTPUT //  Amount of N in inert pool of humus // kg.ha-1

! Variables locales
  integer :: ihum
  integer :: iz
  integer :: ir
!  integer :: kk
  integer :: itrav
  real :: K2hum            ! potential mineralisation rate of humus
  real :: kr               ! decomposition rate of residue (ir)
  real :: kb               ! decay rate of the microbial biomass associated with residue (ir)
  real :: K2               ! actual mineralisation rate of humus
  real :: fth              ! temperature factor on humus mineralisation rate
  real :: ftr              ! temperature factor on residue decomposition rate
  real :: fh               ! moisture factor on humus and residue decomposition rates
  real :: dCres(nbResidus) ! daily variation of C in residue ir
  real :: dNres(nbResidus) ! daily variation of N in residue ir
  real :: dCbio(nbResidus) ! daily variation of C in biomass decomposing residue ir
  real :: dNbio(nbResidus) ! daily variation of N in biomass decomposing residue ir
  real :: dChumres(nbResidus)

  real :: dCO2hum
  real :: dCrest  !
  real :: dCbiot            ! daily variation of C in microbial biomass
  real :: dChumrest         ! daily amount of humified C due to decomposition of all residues
  real :: dNrest  !
  real :: dNbiot            ! daily variation of N in microbial biomass
  real :: dNhumrest         ! daily amount of humified N due to decomposition of all residues
  real :: azomin
  real :: azonew
  real :: tnr
  real :: tnh  
  real :: dChum
  real :: dN
  real :: dNhum
  real :: dCO2res  
  real :: fredk
  real :: fredl
  real :: vminh
  real :: wsat
  real :: wfps
  real :: wfpscc
  real :: fNCbio0
  real :: dNbiot0
  real :: dCnondec
  real :: dNnondec
  real :: fNCbio
  real :: CO2mul

! Nouveaux parametres
  real :: fredsup           ! facteur de réduction supplémentaire de la vitesse de décomposition
  real :: Priming           ! ratio C minéralisé avec priming / C minéralisé sans priming
  real :: PrimedC           ! quantité de C minéralisé par priming
  real :: PrimedN           ! quantité de N minéralisé par priming
  real :: Cresiz            ! quantité de C des résidus dans la couche iz
  real :: dChumt            ! quantité de C humus minéralisé pendant le pas de temps (jour)
  real :: dNhumt            ! quantité de N humus minéralisé pendant le pas de temps (jour)
  real :: availN            ! quantité de N minéral disponible pour la décomposition des résidus
  real :: fNmindec          ! proportion de N minéral disponible pour la décomposition des résidus
  real :: dNbiomin          ! Valeur minimale de dNbio pour que le rapport C/N de la biomasse soit < CNbiomax
  integer :: nbresid        ! nombre de types de résidus en mulch



  nbresid = (nbResidus-1)/2

! Parametres de mineralisation
! ----------------------------
! profondeur de minéralisation de l'humus
      ihum  = nint(P_profhum)
! quantité minimale d'azote minéral par couche de 1 cm (kg/ha)
      azomin = 0.01
! taux potentiel de mineralisation de l'humus : eq. 8.5
      K2hum = P_fmin1*exp(-P_fmin2*P_argi) / (1.0 + P_fmin3 * P_calc)
! initialisation des temps normalisés
      tnh = 0.
      tnr = 0.
! initialisation des stocks C et N sur tout le profil
      Cr = 0.
      Cb = 0.
    ! Ch = 0. Elsa
      Nb = 0.
      Nr = 0.
! *- initialisation des flux de CO2
      CO2res = 0.
      CO2hum = 0.

! *- initialisation du cumul sur vminr et vminh
      cumvminr = 0.
      cumvminh = 0.

! Boucle sur les couches contribuant a la mineralisation
! ------------------------------------------------------
      do iz = 1,ihum

! MINERALISATION C et N ORGANIQUES
! Modulation de la minéralisation par la température : eq 8.3
        if (tsol(iz) <= 0.) then
          fth = 0.
          ftr = 0.
        else
          fth = P_FTEMha/(1.0+FTEMhb*exp(-P_FTEMh*tsol(iz)))
          ftr = P_FTEMra/(1.0+FTEMrb*exp(-P_FTEMr*tsol(iz)))
        endif

! Modulation de la minéralisation par l'humidité : eq 8.2
          fh = (hur(iz) - P_hminm * hucc(iz)) / ((P_hoptm - P_hminm) * hucc(iz))

! diminution progressive de la minéralisation en cas d'excès d'eau : eq. 9.11
          if (hur(iz) > hucc(iz)) then
             if (P_codefente == 1) then
                wsat = (1.5 * hucc(iz) - 0.5 * humin(iz)) / 10.
             else
                wsat = 1. - dacouche(iz) / 2.66
             endif
             wfps = (hur(iz)+sat(iz)) / 10. / wsat
             wfpscc = hucc(iz) / 10. / wsat
             fh = ((1-P_fhminsat) * wfps + wfpscc * P_fhminsat - 1) / (wfpscc-1)

          endif
          if (fh > 1.) fh = 1.
          if (fh < 0.) fh = 0.

! ** temps normalisé humus
        tnh = tnh + fth*fh

! ** temps normalisé résidus
        if (iz >= itrav1 .and. iz <= itrav2)  then
            tnr = tnr + ftr*fh
        endif

! ** Minéralisation de l'humus
! ----------------------------
        K2 = K2hum*fth*fh       ! eq 8.1
        dChum = -K2*Chum(iz)
        dNhum = -K2*Nhum(iz)

! ** Minéralisation des résidus organiques
! *---------------------------------------
        fredk = 1.
        fredl = 1.
        fredsup = 1.
   15   continue
        dCrest = 0.
        dNrest = 0.
        dCbiot = 0.
        dNbiot = 0.
        dChumrest = 0.
        dNhumrest = 0.
        fNCbio = 1.
        Priming = 1.
        PrimedC = 0.
        PrimedN = 0.
        Cresiz = 0.

! ** boucle sur tous les types de résidus
        do ir = 1,nbResidus
! on ne calcule pas Cres(iz,ir) lorsque ir<11 et iz>1  car le mulch est seulement dans la couche 1
            if(ir>10 .or. iz==1) then
            dCres(ir) = 0.
            dNres(ir) = 0.
            dCbio(ir) = 0.
            dNbio(ir) = 0.
            dChumres(ir) = 0.
            Cresiz = Cresiz + Cres(iz,ir)
            if (Cres(iz,ir)+Cbio(iz,ir) <= 1.e-10) CYCLE
! ** taux de décomposition effectif : introduction des facteurs de limitation en azote (fredk et fredl)
            kr = kres(ir) *fredk *ftr*fh


            kb = P_kbio(ir) *fredl *ftr*fh
! ** Variation des stocks C et N des pools residu, biomasse & humus
            dCres(ir) = -kr*Cres(iz,ir)
            if (Cres(iz,ir)+dCres(ir) < 0.) dCres(ir) = -Cres(iz,ir)
            dNres(ir) = -kr*Nres(iz,ir)

            if (Nres(iz,ir)+dNres(ir) < 0.) dNres(ir) = -Nres(iz,ir)
            dCbio(ir) = -kb*Cbio(iz,ir) - P_yres(ir)*dCres(ir)
            if (Cbio(iz,ir)+dCbio(ir) < 0.) dCbio(ir) = -Cbio(iz,ir)
            dNbio(ir) = -kb*Nbio(iz,ir) - P_yres(ir)*Wb(ir)*dCres(ir)
            if (Nbio(iz,ir)+dNbio(ir) < 0.) dNbio(ir) = -Nbio(iz,ir)
            dChumres(ir) = kb*hres(ir)*Cbio(iz,ir)



! * Cumul des variations de C et N pour les différents types de résidus
            dCrest = dCrest + dCres(ir)
            dNrest = dNrest + dNres(ir)
            dCbiot = dCbiot + dCbio(ir)
            dNbiot = dNbiot + dNbio(ir)



            dChumrest = dChumrest + dChumres(ir)
            dNhumrest = dNhumrest + P_Wh*dChumres(ir)
              if(ir<11) CO2mul = - (dCres(ir) + dCbio(ir) + dChumres(ir)) ! calcul de la mineralisation du mulch (une fraction du QCO2res)

            endif
        end do


! fin de boucle sur le type de residus

       dChumt = dChum + dChumrest
       dNhumt = dNhum + dNhumrest
! ** Variation du stock N mineral en absence de limitation N
       dN = -(dNrest+dNbiot+dNhumt)

! ** Calcul de la quantite N mineral disponible pour la decomposition
       if(P_codeNmindec == 1) then
             fNmindec = P_rapNmindec*Cresiz + P_fNmindecmin
       else
             fNmindec = 1.
       endif
       availN = (nit(iz)+ amm(iz))*fNmindec - azomin
       azonew = availN + dN

! ** Test T1
       if(azonew < 0.) then
! ** LIMITATION DE LA DECOMPOSITION PAR N MINERAL
! -----------------------------------------------
! ** Test T2
          if(fredk == 1.) then
! ** Effet E1 : la vitesse de décomposition est réduite
               fredk = P_fredkN*fredsup
               fredl = P_fredlN*fredsup
               goto 15
          endif
! **  Test T3
          dNbiomin = -1.e10
          if(dNbiot > 1.e-6) then
! **  Effet E2 : le rapport N/C de la biomasse diminue (multiplie par fNCbio)
               dNbiot0 = dNbiot
               fNCbio0 = 1. + azonew/dNbiot
               if(P_fNCbiomin < 1.) then
                  fNCbio = amax1(P_fNCbiomin, fNCbio0)
                  dNbiot = fNCbio*dNbiot
               else
                  dNbiomin = dCbiot/P_fNCbiomin ! à remplacer par P_CNbiomax
                  dNbiot = amax1(fNCbio0*dNbiot0,dNbiomin)
               endif
               fNCbio = dNbiot/dNbiot0
 ! fin modif
               do 25 ir=1,nbResidus
                  dNbio(ir) = fNCbio*dNbio(ir)
   25          continue
               azonew=azonew-dNbiot+dNbiot0
           endif
! **  Test T4
!           if(dNbiot <= 1.e-6 .or. fNCbio == P_fNCbiomin) then
           if(dNbiot <= 1.e-6 .or. dNbiot == dNbiomin) then
! **  Effet E3 : la quantite d'azote associée à l'humification du résidu diminue (de azonew)
                dNhumrest = dNhumrest+azonew
! **  Test T5
                if(dNhumrest < 0.) then
! **  Effet E4 : priming effect positif sur N et C humifies actifs (K2 accru)
                   Priming=1.
                   if(dNhum < -1.e-10) Priming=(dNhum + dNhumrest)/dNhum
           ! on reduit a nouveau la vitesse de decomposition si le taux de priming > Primingmax
                   if(Priming > P_Primingmax .and. fredsup == 1.) then
                      fredsup = P_fredNsup
                      fredk = P_fredkN*fredsup
                      fredl = P_fredlN*fredsup
                      goto 15
                   else
                      PrimedC = -dChum*(Priming-1.)
                      dChum = dChum*Priming
                      PrimedN = -dNhum*(Priming-1.)
                      dNhum = dNhum + dNhumrest
                      dNhumrest = 0.
                   endif
                endif
           endif
        endif
! fin de limitation N mineral

        dChumt = dChum + dChumrest
        dNhumt = dNhum + dNhumrest
! **  Minéralisation du carbone provenant de l'humus ou des résidus
        dCO2hum = -dChum
        dCO2res = -(dCrest+dCbiot+dChumrest)
        QCprimed = QCprimed + PrimedC
! **  Minéralisation d'azote provenant de l'humus ou des résidus
        dN = -(dNrest+dNbiot+dNhumt)
        vminh = -dNhum
        vminr = dN - vminh
        Qminh = Qminh + vminh

        Qminr = Qminr + vminr
        QNprimed = QNprimed + PrimedN
! DR 28/07/08 vminh doit etre cumulé sur les couches P_profhum pour les sorties st2
! cumvminh est la minéralisation journaliere de l'humus² (cumulée sur les couches et les résidus)
        cumvminr = cumvminr + vminr
        cumvminh = cumvminh + vminh
! dr 06/09/2011 pour AgMIP j'ajoute la variable immobilisation
        if(vminr < 0) cum_immob = cum_immob  + vminr

! Actualisation du stock C des pools de C et N des residus mélangés au sol et de la biomasse microbienne associée
        do ir = nbresid+1,nbResidus
          if (Cres(iz,ir)+Cbio(iz,ir) > 1.e-10) then
              Cres(iz,ir) = Cres(iz,ir) + dCres(ir)
              Nres(iz,ir) = Nres(iz,ir) + dNres(ir)
              Cbio(iz,ir) = Cbio(iz,ir) + dCbio(ir)
              Nbio(iz,ir) = Nbio(iz,ir) + dNbio(ir)
              Cr = Cr + Cres(iz,ir)
              Nr = Nr + Nres(iz,ir)
              Cb = Cb + Cbio(iz,ir)
              Nb = Nb + Nbio(iz,ir)
          endif
        end do

! Mise à jour du mulch decomposable et non decomposable dans la 1ere couche
        if (iz == 1) then
             Cmulchdec = 0.
             Nmulchdec = 0.
             Cmulchnd = 0.
             Nmulchnd = 0.
             Cbmulch = 0.
             Nbmulch = 0.
             do ir = 1,nbresid
                 Cres(iz,ir) = Cres(iz,ir) + dCres(ir)
                 Nres(iz,ir) = Nres(iz,ir) + dNres(ir)
                 Cbio(iz,ir) = Cbio(iz,ir) + dCbio(ir)
                 Nbio(iz,ir) = Nbio(iz,ir) + dNbio(ir)
                 Cbmulch = Cbmulch + Cbio(iz,ir)
                 Nbmulch = Nbmulch + Nbio(iz,ir)
     ! passage de la fraction non décomposable du mulch (Cnondec) a la fraction decomposable (Cres(1,ir)) jusqu'a epuisement de Cnondec
                 dCnondec    = min(-dCres(ir),Cnondec(ir))
                 dNnondec    = min(-dNres(ir),Nnondec(ir))
                 Cres(1,ir) = Cres(1,ir) + dCnondec
                 Cnondec(ir)  = Cnondec(ir)  - dCnondec
                 Nres(1,ir) = Nres(1,ir) + dNnondec
                 Nnondec(ir)  = Nnondec(ir)  - dNnondec
                 Cmulchdec = Cmulchdec + Cres(iz,ir)
                 Nmulchdec = Nmulchdec + Nres(iz,ir)
                 Cmulchnd  = Cmulchnd  + Cnondec(ir)
                 Nmulchnd  = Nmulchnd  + Nnondec(ir)
             end do
             Cmulch = Cmulchdec + Cmulchnd    ! total C in mulch
             Nmulch = Nmulchdec + Nmulchnd
        endif

! ** Actualisation du stock C et N humus actif (kg/ha)
         Nhum(iz) = Nhum(iz) + dNhumt
         Chum(iz) = Chum(iz) + dChumt
         Nhuma = Nhuma + dNhumt
         Chuma = Chuma + dChumt
! ** Actualisation des pools NH4 et NO3
         amm(iz)= amm(iz) + dN

!      si le NH4 ne suffit pas, alors le NO3 est organisé
         if(amm(iz) < 0.) then
            nit(iz) = amax1(0.,nit(iz)+amm(iz))
            amm(iz) = 0.
         endif
! **  Sommation des flux  de CO2 sur tout le profil
         CO2hum = CO2hum + dCO2hum
         CO2res = CO2res + dCO2res
      end do
! *- Fin de boucle sur les couches

! print *, 'Cmulchnd Cnondec Cr Cres Couche 1:',Cmulchnd,Cnondec(1),Cr,(Cres(1,ir),ir=1,nbresidus)

! ** Actualisation des pools N total (kg/ha) et C total (kg/ha)
      Nhumt = Nhuma + Nhumi
      Chumt = Chuma + Chumi   ! modif Bruno on reste en kg/ha
! ** Emission totale de CO2
      CO2sol  = CO2hum  + CO2res
      QCO2hum = QCO2hum + CO2hum
      QCO2mul = QCO2mul + CO2mul
      QCO2sol = QCO2sol + CO2sol
      QCO2res = QCO2res + CO2res

! ** Rapport N/C moyen de la biomasse
      NCbio = 0.
      if(Cb > 0.) NCbio=Nb/Cb
! ** calcul du temps normalisé cumulé
      tnh = tnh/ihum


      itrav = itrav2-itrav1+1
      if(itrav > 0) tnr = tnr/itrav


      tnhc = tnhc + tnh
      tnrc = tnrc + tnr


return
end subroutine mineral

 
 
