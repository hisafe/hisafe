!> Module of apports
!<
module Module_Apports
    implicit none

interface

subroutine apports(sc,pg,t,c,soil,p,itk)
USE Stics
USE Plante
USE Itineraire_Technique
USE Sol
USE Climat
USE Parametres_Generaux

  implicit none


  type(Stics_Communs_),       intent(INOUT) :: sc

  type(Parametres_Generaux_), intent(IN)    :: pg

  type(Plante_),              intent(INOUT) :: p(sc%P_nbplantes)    ! Toutes les plantes du système, pour somme des LAI et des ABSO

  type(ITK_),                 intent(INOUT) :: itk(sc%P_nbplantes)

  type(Sol_),                 intent(INOUT) :: soil

  type(Climat_),              intent(IN)    :: c

  type(Stics_Transit_),       intent(INOUT) :: t
end subroutine Apports

subroutine apportsNparEngraisMineraux(P_orgeng, P_voleng, P_deneng, anit, n, absoTot, P_Vabs2, P_codlocferti,           & ! IN
!                                      P_locferti, P_pHminvol, P_pHmaxvol, P_pH, P_Xorgmax, P_codedenit, P_Wh, P_engamm, &
                                      P_locferti, P_pHminvol, P_pHmaxvol, P_pH, P_Xorgmax, P_codedenit,  P_engamm, &
                                      Nvoleng, Ndenit, Norgeng, QNvoleng, QNorgeng, QNdenit, Nhum,Nhuma,              & ! INOUT
                                      amm, nit, precipN, totapN)
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
!> fertilization
!> - Stics paragraphe 6.3, 6.3.1, 6.3.2, page 100-101
!!
!! The N inputs from mineral fertilizers can be applied either at the soil surface or at a given depth (LOCFERTIT)
!! if the option 'localized fertilization' is activated (CODLOCFERTIT =2).
!! We consider 8 different types of mineral fertilizers. As a simplification, urea is treated as an ammonium fertilizer
!! since its hydrolysis to ammonium carbonate is a very fast process (e.g. Recous et al, 1988; Hah, 2000).
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
  implicit none

  real,    intent(IN)    :: P_orgeng      ! (P_engrais)   // PARAMETER // maximal quantity of mineral fertilizer that can be organized in the soil (fraction for type 8) // kg ha-1 // PARAM // 1
  real,    intent(IN)    :: P_voleng      ! (P_engrais)   // PARAMETER // maximal fraction of mineral fertilizer that can be volatilized  // SD // PARAM // 1
  real,    intent(IN)    :: P_deneng      ! (P_engrais)   // PARAMETER // proportion of the mineral fertilizer that can be denitrified (useful if codenit not active) // SD // PARAM // 1
  real,    intent(IN)    :: anit          ! (n)           // OUTPUT // Daily nitrogen provided  // kgN.ha-1 j-1
  integer, intent(IN)    :: n
  real,    intent(IN)    :: absoTot(5)    ! 5 derniers jours (max.)
  real,    intent(IN)    :: P_Vabs2       !> // PARAMETER // N uptake rate for which fertilizer losses are divided by 2 // kg.ha-1.d-1 // PARAM // 1
  integer, intent(IN)    :: P_codlocferti !> // PARAMETER // code of fertilisation localisation:  1: at soil surface, 2 = in the soil // code 1/2 // PARTEC // 0
  integer, intent(IN)    :: P_locferti    !> // PARAMETER // Depth of nitrogen apply (when fertiliser is applied in depth of soil) // cm // PARTEC // 1
  real,    intent(IN)    :: P_pHminvol    !> // PARAMETER // pH above which the fertilizer volatilisation is null // P_pH // PARAM // 1
  real,    intent(IN)    :: P_pHmaxvol    !> // PARAMETER // pH beyond which the fertilizer volatilisation is maximum // P_pH // PARAM // 1
  real,    intent(IN)    :: P_pH          !> // PARAMETER // P_pH of mixing soil + organic amendments  // SD // PARSOL // 1
  real,    intent(IN)    :: P_Xorgmax     !> // PARAMETER // maximal amount of immobilised N coming from the mineral fertilizer  // kg.ha-1 // PARAM // 1
  integer, intent(IN)    :: P_codedenit   !> // PARAMETER // option to allow the calculation of denitrification :yes (1), no(2) // code 1/2 // PARSOL // 0
!  real,    intent(IN)    :: P_Wh          !> // PARAMETER // ratio N/C of humus // g g–1 // PARAM // 1
  real,    intent(IN)    :: P_engamm      ! (P_engrais)   // PARAMETER // proportion of ammonium in the fertilizer // SD // PARAM // 1

  ! ici, ces variables de sortie pourraient être mises en OUT plutot que INOUT
  real,    intent(INOUT) :: Nvoleng      !> // OUTPUT // Daily volatilisation of NH3-N from fertiliser // kg.ha-1.d-1
  real,    intent(INOUT) :: Ndenit      !> // OUTPUT // "Daily denitrification of N from fertiliser or soil (if option  denitrification  is activated)" // kg.ha-1.d-1
  real,    intent(INOUT) :: Norgeng      !> // OUTPUT // Daily organisation of N from fertiliser // kg.ha-1.d-1
  real,    intent(INOUT) :: QNvoleng     !> // OUTPUT // Cumulative volatilisation of NH3-N  from fertiliser // kg.ha-1
  real,    intent(INOUT) :: QNorgeng     !> // OUTPUT // Cumulative organisation of N from fertiliser // kg.ha-1
  real,    intent(INOUT) :: QNdenit     !> // OUTPUT // "Cumulative denitrification of N from fertiliser or soil (if option  denitrification  is activated)" // kg.ha-1
  real,    intent(INOUT) :: Nhum(1:10)   ! avant Nbio(1:10,1:1) >// OUTPUT immobilised N from fertilizers by soil microbial biomass -part of humus - in the 10 first cm  // kg.ha-1
  real,    intent(INOUT) :: Nhuma      !> // OUTPUT // Amount of active N in the humus pool  // kg.ha-1
  real,    intent(INOUT) :: amm(1:max(1,P_locferti))  ! (1:max(1,itk%P_locferti))
  real,    intent(INOUT) :: nit(1:max(1,P_locferti))
  real,    intent(INOUT) :: precipN
  real,    intent(INOUT) :: totapN       !> // OUTPUT // Total amount of N inputs coming from fertiliser and organic residues // kg.ha-1
end subroutine apportsNparEngraisMineraux



subroutine ResidusApportSurfaceSol(qresidu,Crespc,eauresidu,ires,CNresmin,CNresmax, &       ! IN
                          P_profhum,zrac,P_qmulchdec,nbCouchesSol,nbResidus,nap,airg,CNresid, &
                          Cnondec,Nnondec,Cres,Nres,Cmulchnd,Nmulchnd,QCapp,QNapp,QCresorg,QNresorg) ! INOUT
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 6.3.3, page 104
!>
!! This module updates the C and N stock (as well as water) at surface when applying organic residues
!! Plant residues (which belong to possible organic residues) are assumed to remain at the soil surface until being buried by the following soil tillage,
!! except for pure roots which are assumed to be located between the surface and the PROFHUMS depth.
!! Those residues may have mulch properties.
!!
!! The N inputs from organic residues arrive onto the soil either under mineral form (mainly as NH4+) or under organic form. The mineral fraction enters
!! the soil mineral pool and is submitted to NH3 volatilization, nitrification, leaching and plant uptake. The organic fraction decomposes more or less rapidly
!! and mineralizes its C and N according to the decomposition module (see mineral.f90). The module is generic and can simulate most types of organic residues.
!! Eight categories are considered:
!> -  1) mature crop residues (straw, roots),
!> -  2) catch crop residues (young plants),
!> -  3) farmyard manures,
!> -  4) composts,
!> -  5) sewage sludges,
!> -  6) distillery vinasses,
!> -  7) animal horn,
!> -  8) others.
!!
!! The characteristics of each organic residue are defined in the technical file: category, depth of incorporation in soil, amount of fresh matter added,
!! carbon content, C/N ratio, water content and mineral N content
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c

  implicit none

  real,    intent(IN)    :: qresidu   ! Fresh matter (FM) added from residue ires  t.ha-1
  real,    intent(IN)    :: Crespc   ! C content of residue ires  %DM
  real,    intent(IN)    :: eauresidu ! Water content of residue ires  %FM
  integer, intent(IN)    :: ires      ! Number of residue
  real,    intent(IN)    :: CNresmin  !> // PARAMETER // minimum observed value of ratio C/N of residue ires // g g-1 // PARAM // 1
  real,    intent(IN)    :: CNresmax  !> // PARAMETER // maximum observed value of ratio C/N of residue ires // g g-1 // PARAM // 1
  real,    intent(IN)    :: P_profhum  !> // PARAMETER // Humification depth  (max.60) // cm // PARSOL // 1
  real,    intent(IN)    :: zrac      !> // OUTPUT // Depth reached by root system // cm
  real,    intent(IN)    :: P_qmulchdec  !> // PARAMETER // maximal amount of decomposing mulch // t.ha-1 // PARAM // 1
  integer, intent(IN)    :: nbCouchesSol
  integer, intent(IN)    :: nbResidus
  integer, intent(INOUT) :: nap       !> nombre d'apports de residus (que l'on cumule)
  real,    intent(INOUT) :: airg      !> // OUTPUT // Daily irrigation // mm
  real,    intent(INOUT) :: CNresid
  real,    intent(INOUT) :: Cnondec(10) !> // OUTPUT // undecomposable C stock of the residue ires at soil surface //  t.ha-1
  real,    intent(INOUT) :: Nnondec(10) !> // OUTPUT // undecomposable N stock of the residue ires at soil surface // kg.ha-1
  real,    intent(INOUT) :: Cres(nbCouchesSol,nbResidus)
  real,    intent(INOUT) :: Nres(nbCouchesSol,nbResidus)
  real,    intent(INOUT) :: Cmulchnd
  real,    intent(INOUT) :: Nmulchnd
  real,    intent(INOUT) :: QCapp      !> // OUTPUT // cumulative amount of organic C added to soil // kg.ha-1
  real,    intent(INOUT) :: QNapp      !> // OUTPUT // cumulative amount of organic N added to soil // kg.ha-1
  real,    intent(INOUT) :: QCresorg   !> // OUTPUT // cumulative amount of exogenous C added to soil // kg.ha-1
  real,    intent(INOUT) :: QNresorg   !> // OUTPUT // cumulative amount of exogenous N added to soil // kg.ha-1

!: VARIABLES LOCALES
  real    :: Qeaures
  real    :: Capp
  real    :: Napp
  real    :: NCresmoy
  real    :: Csup
  real    :: Nsup
  real    :: Cmulchndecmax
  integer :: irac
  integer :: iz

end subroutine ResidusApportSurfaceSol

subroutine ResiduParamDec(awb,bwb,cwb,CroCo,akres,bkres,ahres,bhres,Wr,CNresmin,CNresmax,Wb,kres,hres)
! *---------------------------------------------------------------------------
!>
!! This program updates the parameter values for organic residue mineralization, when applying new organic residue:
!>  -  Wr: N/C of the residue
!>  -  Wr: N/C of the biomass
!>  -  kres: decomposition rate for organic residues
!>  -  hres: humification rate for organic residues
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
! *---------------------------------------------------------------------------
implicit none

  real,    intent(IN)    :: awb   !> // PARAMETER // decomposition parameter : CsurNbio=awb+bwb.Wr // SD // PARAM // 1
  real,    intent(IN)    :: bwb   !> // PARAMETER // decomposition parameter : CsurNbio=awb+bwb.Wr // g g-1 // PARAM // 1
  real,    intent(IN)    :: cwb   !> // PARAMETER // decomposition parameter : CsurNbio=awb+bwb.Wr // g g-1 // PARAM // 1
  real,    intent(IN)    :: CroCo !> // PARAMETER // decomposition parameter  // SD // PARAM // 1
  real,    intent(IN)    :: akres !> // PARAMETER // decomposition parameter : kres=akres+bkres.Wr // d-1 // PARAM // 1
  real,    intent(IN)    :: bkres !> // PARAMETER // decomposition parameter : kres=akres+bkres.Wr // g g-1 // PARAM // 1
  real,    intent(IN)    :: ahres !> // PARAMETER // decomposition parameter : hres=1-ahres/(bhres.Wr+1) // g g-1 // PARAM // 1
  real,    intent(IN)    :: bhres !> // PARAMETER // decomposition parameter : hres=1-ahres/(bhres.Wr+1) // g g-1 // PARAM // 1
  real,    intent(IN)    :: Wr    ! N/C of the residue    g.g-1 IN
  ! EC et BM 04/10/2012: modifs des param de decomposition pour le mulch
  real,    intent(IN)    :: CNresmin  !> // PARAMETER // minimum observed value of ratio C/N of residue ires // g g-1 // PARAM // 1
  real,    intent(IN)    :: CNresmax  !> // PARAMETER // maximum observed value of ratio C/N of residue ires // g g-1 // PARAM // 1
  real,    intent(INOUT) :: Wb    ! N/C of the biomass    g.g-1 INOUT
  real,    intent(INOUT) :: kres  ! decomposition rate of the organic residue  d-1   INOUT
  real,    intent(INOUT) :: hres  ! humification rate of the organic residue   g.g-1 INOUT

! EC et BM 04/10/2012: modifs des param de decomposition pour le mulch
  !: VARIABLES LOCALES
  real    :: CsurNbio
  real    :: NsurCres

end subroutine ResiduParamDec

subroutine ResidusMelangeSol(itrav1,itrav2,nbCouches,nbResidus,  &        ! IN
                          Cres,Nres,Cbio,Nbio,Chum,Nhum,Cnondec,Nnondec,Cmulchnd,Nmulchnd)      ! INOUT
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 8.2, page 147
!>
!! The depth of residues incorporation in soil modifies their decomposition since water content and temperature vary with depth and their localization
!! determines the amount of mineral nitrogen available for the microbial biomass. Each tillage operation is assumed to mix the residues uniformly
!! with the soil over the depth defined by a minimal value (itrav1) and a maximal value (itrav2).
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c

implicit none

  integer, intent(IN)    :: itrav1
  integer, intent(IN)    :: itrav2
  integer, intent(IN)    :: nbCouches
  integer, intent(IN)    :: nbResidus
  real,    intent(INOUT) :: Cres(nbCouches,nbResidus)
  real,    intent(INOUT) :: Nres(nbCouches,nbResidus)
  real,    intent(INOUT) :: Cbio(nbCouches,nbResidus)
  real,    intent(INOUT) :: Nbio(nbCouches,nbResidus)
  real,    intent(INOUT) :: Chum(nbCouches)
  real,    intent(INOUT) :: Nhum(nbCouches)
  real,    intent(INOUT) :: Cnondec(10)   !> // OUTPUT // undecomposable C stock of the type 10 residues on the surface // kg.ha-1
  real,    intent(INOUT) :: Nnondec(10)   !> // OUTPUT // undecomposable N stock of the type 5 residues on the surface // kg.ha-1
  real,    intent(INOUT) :: Cmulchnd
  real,    intent(INOUT) :: Nmulchnd

! VARIABLES LOCALES
  integer :: iz
  integer :: ir
  integer :: itrav
  real    :: Cresid(nbResidus)
  real    :: Nresid(nbResidus)
  real    :: Cbioma(nbResidus)
  real    :: Nbioma(nbResidus)
  real    :: Chumus
  real    :: Nhumus


end subroutine ResidusMelangeSol

subroutine ResidusParamDecMelange(nbCouches,nbResidus,awb,bwb,cwb,CroCo,akres,bkres,ahres,bhres,Cres,Nres,profhum,  & ! IN
                                  Wb,kres,hres)                                                    ! INOUT
! --------------------------------------------------------------------------------------------------------
!! This program updates the decomposition parameters of organic residue
!!      following addition of organic residue and soil tillage
!> Stics book  page 265-287
! *-------------------------------------------------------------------------------------------------------
  implicit none

  integer, intent(IN)    :: nbCouches
  integer, intent(IN)    :: nbResidus
  real,    intent(IN)    :: awb(nbResidus)     !> // PARAMETER // decomposition parameter : CsurNbio=awb+bwb.Wr // SD // PARAM // 1
  real,    intent(IN)    :: bwb(nbResidus)     !> // PARAMETER // decomposition parameter : CsurNbio=awb+bwb.Wr // SD // PARAM // 1
  real,    intent(IN)    :: cwb(nbResidus)     !> // PARAMETER // decomposition parameter : CsurNbio=awb+bwb.Wr // SD // PARAM // 1
  real,    intent(IN)    :: akres(nbResidus)   !> // PARAMETER // decomposition parameter : kres=akres+bkres.Wr // d-1 // PARAM // 1
  real,    intent(IN)    :: bkres(nbResidus)   !> // PARAMETER // decomposition parameter : kres=akres+bkres.Wr // d-1 // PARAM // 1
  real,    intent(IN)    :: ahres(nbResidus)   !> // PARAMETER // decomposition parameter : hres=1-ahres/(bhres.Wr+1) // g g-1 // PARAM // 1
  real,    intent(IN)    :: bhres(nbResidus)   !> // PARAMETER // decomposition parameter : hres=1-ahres/(bhres.Wr+1) // g g-1 // PARAM // 1
  real,    intent(IN)    :: Cres(nbCouches,nbResidus)
  real,    intent(IN)    :: Nres(nbCouches,nbResidus)
  real,    intent(IN)    :: profhum            !> // PARAMETER // Humification depth (max.60) // cm // PARSOL // 1
  real,    intent(INOUT) :: Wb(nbResidus)      !> N/C of the biomass
  real,    intent(INOUT) :: kres(nbResidus)    !> decomposition rate of the organic residues
  real,    intent(INOUT) :: hres(nbResidus)    !> humification rate of the organic residues
  real,    intent(IN)    :: CroCo(nbResidus)   !> // PARAMETER // decomposition parameter // SD // PARAM // 1

  !: VARIABLES LOCALES
  integer :: iz
  integer :: ir
  integer :: izmax
  real    :: Cresid
  real    :: Nresid
  real    :: CsurNbio
  real    :: Wr

end subroutine ResidusParamDecMelange


end interface
end module Module_Apports
