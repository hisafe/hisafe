! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
!> - Stics book paragraphe 6.3.3, page 104-105
!>
!! The N inputs from organic residues arrive onto the soil either under mineral form (mainly as NH4+) or under organic form.
!! The mineral fraction enters the soil mineral pool and is submitted to NH3 volatilization, nitrification, leaching and plant uptake.
!! The organic fraction decomposes more or less rapidly and mineralizes its C and N according to the decomposition module (see ResidusDecomposition.f90).
!!
!! The module is generic and can simulate most types of organic residues.
!! 10 categories are considered:
!>    - 1) mature crop residues (straw, roots),
!>    - 2) catch crop residues (young plants),
!>    - 3) farmyard manure,
!>    - 4) compost,
!>    - 5) sewage sludge,
!>    - 6) distillery vinasse,
!>    - 7) animal horn ,
!>    - 8,9,10) others.
!!
!! The net mineralization (positive or negative) due to the addition of these residues depends on the category and the C/N ratio of the residue.
!! The characteristics of each organic residue are defined in the technical file: category, depth of incorporation in soil, amount of fresh matter added,
!! carbon content, C/N ratio, water content and mineral N content. .
!!
!! In the case of chained simulations (§ 10.2), the characteristics of the crop residues returning to the soil are simulated by the model and
!! are taken into account automatically in the next simulation.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c

subroutine apportsOrganiquesEtTravailDuSol(n,nbCouches,nbResidus,P_nbjres,P_nbjtrav,numjtrav,numjres,P_coderes,P_proftrav,  & ! IN
                                           P_profres,P_qres,P_Crespc,P_Nminres,P_eaures,P_CNresmin,P_CNresmax,P_awb,P_bwb,  &
                                           P_cwb,P_CroCo,P_akres,P_bkres,P_ahres,P_bhres,P_Qmulchdec,P_alphapH,P_dpHvolmax,   &
                                           P_pHvols, P_pH,P_profhum,P_codeNmindec,zrac,         &
                                           Nvolatorg,amm,totapN,totapNres,P_CsurNres,nap,airg,Cres,Nres,Cbio,Nbio,Chum, & ! INOUT
                                           Nhum,Wb,kres,hres,itrav1,itrav2,ires,Cmulchnd,Nmulchnd,Cnondec,Nnondec,dpH,pHvol, &
                                           QCapp,QNapp,QCresorg,QNresorg,irmulch)
  implicit none

  integer, intent(IN)    :: n
  integer, intent(IN)    :: nbCouches  
  integer, intent(IN)    :: nbResidus  
  integer, intent(IN)    :: P_nbjres  !> // PARAMETER // number of residue addition // days // PARTEC // 1 
  integer, intent(IN)    :: P_nbjtrav  !> // PARAMETER // number of cultivations operations // days // PARTEC // 1
  integer, intent(IN)    :: numjtrav(P_nbjtrav)  
  integer, intent(IN)    :: numjres(P_nbjres)  

  integer, intent(IN)    :: P_coderes(P_nbjres)    !> // PARAMETER // residue type: 1=crop residues,  2=residues of CI,  3=manure,  4=compost OM,  5=mud SE,  6=vinasse,  7=corn,  8=other // code 1 to 10 // PARTEC // 0
  real,    intent(IN)    :: P_proftrav(P_nbjtrav)  !> // PARAMETER // Depth of residue incorporation  (max. 40) // cm // PARTEC // 1
  real,    intent(IN)    :: P_profres(P_nbjtrav)   !> // PARAMETER // minimal depth of organic residue incorporation  // cm // PARTEC // 1
  real,    intent(IN)    :: P_qres(P_nbjres)       !> // PARAMETER // amount of crop residue or organic amendments applied to the soil (fresh weight) // t.ha-1 // PARTEC // 1
  real,    intent(IN)    :: P_Crespc(P_nbjres)     !> // PARAMETER // carbon content of organic residue //  // PARTEC // 1
  real,    intent(IN)    :: P_Nminres(P_nbjres)    !> // PARAMETER // N mineral content of organic residues  // % fresh matter // PARTEC // 1
  real,    intent(IN)    :: P_eaures(P_nbjres)     !> // PARAMETER // Water amount of organic residues  // % fresh matter // PARTEC // 1
  real,    intent(IN)    :: P_CNresmin(nbResidus)  !> // PARAMETER // minimum observed value of ratio C/N of organic residues  // g g-1 // PARAM // 1
  real,    intent(IN)    :: P_CNresmax(nbResidus)  !(:)  NE PAS UTILISER LE DIMENSIONNEMENT IMPLICITE : , ERREUR d'EXECUTION   // PARAMETER // maximum observed value of ratio C/N of organic residues // g g-1 // PARAM // 1
  real,    intent(IN)    :: P_awb(nbResidus)       !(:)   // PARAMETER // parameter  of organic residues decomposition: CsurNbio=P_awb+P_bwb/P_CsurNres // SD // PARAM // 1
  real,    intent(IN)    :: P_bwb(nbResidus)       !(:)   // PARAMETER // parameter of organic residues decomposition: CsurNbio=P_awb+P_bwb/P_CsurNres // g g-1 // PARAM // 1
  real,    intent(IN)    :: P_cwb(nbResidus)       !(:)   // PARAMETER // Minimum ratio C/N of microbial biomass in the relationship: CsurNbio=P_awb+P_bwb/P_CsurNres // g g-1 // PARAM // 1
  real,    intent(IN)    :: P_akres(nbResidus)     !(:)   // PARAMETER // parameter of organic residues decomposition: kres=P_akres+P_bkres/P_CsurNres // day-1 // PARAM // 1
  real,    intent(IN)    :: P_bkres(nbResidus)     !(:)   // PARAMETER // parameter of organic residues decomposition: kres=P_akres+P_bkres/P_CsurNres // g g-1 // PARAM // 1
  real,    intent(IN)    :: P_ahres(nbResidus)     !(:)   // PARAMETER // parameter of organic residues humification: hres=1-P_ahres*P_CsurNres/(P_bhres+P_CsurNres) // g g-1 // PARAM // 1
  real,    intent(IN)    :: P_bhres(nbResidus)     !(:)   // PARAMETER // parameter of organic residues humification: hres=1-P_ahres*P_CsurNres/(P_bhres+P_CsurNres) // g g-1 // PARAM // 1
  real,    intent(IN)    :: P_Qmulchdec(nbResidus) !> // PARAMETER // maximal amount of decomposing mulch // t C.ha-1 // PARAM // 1
  real,    intent(IN)    :: P_alphapH        !< // PARAMETER // maximal soil pH variation per unit of inorganic N added with slurry // kg-1 ha //PARAM //1
  real,    intent(IN)    :: P_dpHvolmax    !> // PARAMETER // maximal pH increase following the application of organic residue such as slurry // SD // PARAM // 1
  real,    intent(IN)    :: P_pHvols       !> // PARAMETER // soil pH above which soil pH is not affected by addition of organic residue (such as slurry)  // SD // PARAM // 1
  real,    intent(IN)    :: P_pH           !> // PARAMETER // basic soil pH // SD // PARSOL // 1
  real,    intent(IN)    :: P_profhum      !> // PARAMETER // Humification depth  (max.60) // cm // PARSOL // 1
! Elsa 03/10/2012 P_codeNmindec mal déclaré
! real,    intent(IN)    :: P_codeNmindec  !< // PARAMETER // option to activate the available N :yes (1), no(2) // code 1/2
  integer, intent(IN)    :: P_codeNmindec  !< // PARAMETER // option to activate the available N :yes (1), no(2) // code 1/2
  real,    intent(IN)    :: zrac           !> // OUTPUT // Depth reached by root system // cm
  real,    intent(INOUT) :: Nvolatorg      ! Nvolatorg = NH4 volatilisable  (thèse de T. MORVAN 1999)
  real,    intent(INOUT) :: amm(2)         ! première couche du sol (surface)
  real,    intent(INOUT) :: totapN         !> // OUTPUT // Total amount of mineral N inputs through fertiliser and organic residues  // kg.ha-1
  real,    intent(INOUT) :: totapNres      !> // OUTPUT // Total amount of organic N inputs through organic products  // kg.ha-1
  real,    intent(INOUT) :: P_CsurNres(P_nbjres)  !> // PARAMETER // C/N ratio of residues // g g-1 // PARTEC // 1
  integer, intent(INOUT) :: nap            !> nombre d'apports
  real,    intent(INOUT) :: airg           !! DR 22/10/2010 attention ic c'est airg de n+1    // OUTPUT // Daily irrigation // mm
  real,    intent(INOUT) :: Cres(nbCouches,nbResidus)  
  real,    intent(INOUT) :: Nres(nbCouches,nbResidus)
  real,    intent(INOUT) :: Cbio(nbCouches,nbResidus)  
  real,    intent(INOUT) :: Nbio(nbCouches,nbResidus)  
  real,    intent(INOUT) :: Chum(nbCouches)  
  real,    intent(INOUT) :: Nhum(nbCouches)
  real,    intent(INOUT) :: Wb(nbResidus)  
  real,    intent(INOUT) :: kres(nbResidus)  
  real,    intent(INOUT) :: hres(nbResidus)  
  integer, intent(INOUT) :: itrav1  
  integer, intent(INOUT) :: itrav2  
  integer, intent(INOUT) :: ires  
  real,    intent(INOUT) :: Cmulchnd
  real,    intent(INOUT) :: Nmulchnd
  real,    intent(INOUT) :: Cnondec(10)   !> // OUTPUT // undecomposable C stock of the type 10 residues on the surface //  t.ha-1
  real,    intent(INOUT) :: Nnondec(10)   !> // OUTPUT // undecomposable N stock of the type 5 residues on the surface // kgN.ha-1
  real,    intent(INOUT) :: dpH  
  real,    intent(INOUT) :: pHvol   !> // OUTPUT // soil surface P_pH varying after organic residue application (sutch as slurry) // SD
  real,    intent(INOUT) :: QCapp
  real,    intent(INOUT) :: QNapp
  real,    intent(INOUT) :: QCresorg
  real,    intent(INOUT) :: QNresorg
  real,    intent(IN)    :: P_CroCo(nbResidus)     !(:)   // PARAMETER // parameter of organic residues decomposition  // SD // PARAM // 1
  integer, intent(INOUT) :: irmulch

!: Variables locales
  integer :: is  
  real    :: propvolat  !>  
  real    :: travsol    ! code de travail du sol
  real    :: QNminres   ! Quantité de NH4 contenu dans l'amendement (kg N/ha)
  real    :: dpHvolm  !>  
  real    :: dpHvol  
  real    :: Wr

!* P_nbjres est le nombre de dates d'apport de MO; numjres est le numéro de jour de chaque apport

  do is = 1,P_nbjres


      if (n == numjres(is)) then
    ! calcul de l'apport de NH4 et du NH4 volatilisable
          QNminres = P_qres(is)*P_Nminres(is)*10.
          travsol = +1.
          if (is > 1) then
            if (P_proftrav(is-1) > 1 .and. (n - numjtrav(is-1)) <= 7) then
              travsol = -1.
            endif
          endif

        ! Quantite de N volatilisable au jour de l'apport
          propvolat = min(1., 0.37 + 0.029 *(100. - P_eaures(is)) + 0.117 * travsol)
          Nvolatorg = propvolat * QNminres

        ! Ccalcul de la variation de P_pH liée à l'apport de NH4 de l'amendement (eq 8.18)
        !    Morvan:    variation maximale = 0.94  pour un apport de 200 kg N/ha (74 m3/ha)
        !    Chantigny: variation maximale = 2.80  pour un apport de 500 kg N/ha (90 m3/ha)
          dpHvolm = min(P_alphapH*QNminres,P_dpHvolmax)
          if(Nvolatorg == 0. .or. P_pHvols == 7.) then
            dpH = 0.
          else
            dpHvol = 0.
            if (P_pH <= 7.) dpHvol = dpHvolm
            if (P_pH > 7. .and. P_pH <= P_pHvols) dpHvol = dpHvolm * max(0.,(P_pHvols-P_pH)/(P_pHvols-7.))
            dpH = dpH + (dpHvol / Nvolatorg)
            pHvol = P_pH + dpHvol
          endif

    ! b) actualisation des stocks

    !-    le NH3 volatilisé sera déduit du stock NH4 dans le ss-pg volatorg
    !-    le NH4 non volatilisable est mis dans la 2eme couche de sol (1-2 cm)
          amm(1) = amm(1) + Nvolatorg
          amm(2) = amm(2) + QNminres - Nvolatorg
          totapN  = totapN + QNminres
          totapNres = totapNres + QNminres
    !-    type de résidu organique apporté
          ires = P_coderes(is)
!Elsa 01/08/2012 il faut attribuer également le irmulch
          irmulch = P_coderes(is)

!          print *,'numjres(is),is,Capp,ires',numjres(is),is,P_qres(is)*P_Crespc(is)*10.,ires
          call ResidusApportSurfaceSol(P_qres(is),P_Crespc(is),P_eaures(is),ires,P_CNresmin(ires),P_CNresmax(ires), & ! IN
                         P_profhum, zrac, P_qmulchdec(ires), nbCouches, nbResidus, nap, airg, P_CsurNres(is),       &
                         Cnondec(1:10),Nnondec(1:10),Cres(1:nbCouches,1:nbResidus),Nres(1:nbCouches,1:nbResidus),   &
                         QCapp, QNapp, QCresorg, QNresorg) ! INOUT
          Wr=1./P_CsurNres(ires)
          call ResiduParamDec(P_awb(ires),P_bwb(ires),P_cwb(ires),P_CroCo(ires),P_akres(ires),P_bkres(ires), &
                             P_ahres(ires),P_bhres(ires),Wr,P_CNresmin(ires),P_CNresmax(ires),Wb(ires),kres(ires),hres(ires))
  endif
  end do


! Recherche du travail du sol
! ---------------------------
!             P_nbjtrav est le nombre de dates de travail du sol; numjtrav est le numéro de jour de chaque travail)
      do is = 1,P_nbjtrav


        if (n == numjtrav(is)) then
          itrav1 = nint(P_profres(is))
          itrav2 = nint(P_proftrav(is))


          if(P_codeNmindec==1) itrav1=1
          if (itrav2 < itrav1) itrav2 = itrav1

!          print *,'numjtrav(is),is,itrav1,itrav2, P_codeNmindec',numjtrav(is),is,itrav1,itrav2,P_codeNmindec
          call ResidusMelangeSol(itrav1,itrav2,nbCouches,nbResidus,  & !IN
                      Cres(1:nbCouches,1:nbResidus),Nres(1:nbCouches,1:nbResidus),Cbio(1:nbCouches,1:nbResidus), & ! INOUT
                      Nbio(1:nbCouches,1:nbResidus),Chum(1:nbCouches),Nhum(1:nbCouches), &
                      Cnondec(1:10),Nnondec(1:10),Cmulchnd,Nmulchnd)

          call ResidusParamDecMelange(nbCouches,nbResidus,P_awb(1:nbResidus),P_bwb(1:nbResidus),P_cwb(1:nbResidus),&
                     P_CroCo(1:nbResidus),P_akres(1:nbResidus),P_bkres(1:nbResidus),P_ahres(1:nbResidus),P_bhres(1:nbResidus),  &
                     Cres(1:nbCouches,1:nbResidus),Nres(1:nbCouches,1:nbResidus),P_profhum, &
                     P_CNresmin(1:nbResidus),P_CNresmax(1:nbResidus), &   ! IN
                     Wb(1:nbResidus),kres(1:nbResidus),hres(1:nbResidus)) ! INOUT

        endif


      end do

return
end subroutine apportsOrganiquesEtTravailDuSol
 
 
