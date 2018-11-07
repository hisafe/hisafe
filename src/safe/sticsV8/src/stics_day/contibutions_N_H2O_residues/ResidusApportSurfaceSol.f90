
! **********************************************************************
! * Apport de Matière Organique à la surface du sol                    *
! **********************************************************************
! -------------------- variables d'entree -------------------------
!
!   ires       type de residu apporté (1 à nbResidus)
!   qresidu    quantite de MF de résidu apporté              (t/ha)
!   Crespc    teneur en C du residu                         (% MF)
!   CNresid   rapport C/N du résidu apporté                  (g/g)
!   eauresidu  teneur en eau du résidu organique             (% MF)
!   P_qmulchdec  quantite maximale de mulch decomposable      (t/ha)
! --------------------- variables de sortie -----------------------
!
!   Cres      stock C du résidu ires                        (kg/ha)
!   Nres      stock N du résidu ires                        (kg/ha)
!   Cnondec     stock C non decomposable                      (kg/ha)
!   Nnondec     stock N non decomposable                      (kg/ha)
! ------------------------------------------------------------------
! ml_com !
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
!! 10 categories are considered:
!> -  1) mature crop residues (straw, roots),
!> -  2) catch crop residues (young plants),
!> -  3) farmyard manures,
!> -  4) composts,
!> -  5) sewage sludges,
!> -  6) distillery vinasses,
!> -  7) animal horn,
!> -  8,9,10) others.
!!
!! The characteristics of each organic residue are defined in the technical file: category, depth of incorporation in soil, amount of fresh matter added,
!! carbon content, C/N ratio, water content and mineral N content
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c

subroutine ResidusApportSurfaceSol(qresidu,Crespc,eauresidu,ires,CNresmin,CNresmax, &       ! IN
                          P_profhum,zrac,Qmulchdec,nbCouchesSol,nbResidus,nap,airg,CNresid, &
                          Cnondec,Nnondec,Cres,Nres,QCapp,QNapp,QCresorg,QNresorg) ! INOUT

  implicit none

  real,    intent(IN)    :: qresidu   ! Fresh matter (FM) added from residue ires  t.ha-1
  real,    intent(IN)    :: Crespc   ! C content of residue ires  %DM
  real,    intent(IN)    :: eauresidu ! Water content of residue ires  %FM
  integer, intent(IN)    :: ires      ! Number of residue
  real,    intent(IN)    :: CNresmin  !> // PARAMETER // minimum observed value of ratio C/N of residue ires // g g-1 // PARAM // 1
  real,    intent(IN)    :: CNresmax  !> // PARAMETER // maximum observed value of ratio C/N of residue ires // g g-1 // PARAM // 1
  real,    intent(IN)    :: P_profhum  !> // PARAMETER // Humification depth  (max.60) // cm // PARSOL // 1
  real,    intent(IN)    :: zrac      !> // OUTPUT // Depth reached by root system // cm
  real,    intent(IN)    :: Qmulchdec  !> // PARAMETER // maximal amount of decomposing mulch of residue ires // t.ha-1 // PARAM // 1
  integer, intent(IN)    :: nbCouchesSol
  integer, intent(IN)    :: nbResidus
  integer, intent(INOUT) :: nap       !> nombre d'apports de residus (que l'on cumule)
  real,    intent(INOUT) :: airg      !> // OUTPUT // Daily irrigation // mm
  real,    intent(INOUT) :: CNresid  
  real,    intent(INOUT) :: Cnondec(10)  !> // OUTPUT // undecomposable C stock of the residue ires at soil surface //  t.ha-1
  real,    intent(INOUT) :: Nnondec(10)  !> // OUTPUT // undecomposable N stock of the residue ires at soil surface // kg.ha-1
  real,    intent(INOUT) :: Cres(nbCouchesSol,nbResidus)
  real,    intent(INOUT) :: Nres(nbCouchesSol,nbResidus)
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
! Elsa 03/10/2012; on renomme cette variable puisqu'elle se réfère au mulch décomposable
!  real    :: Cmulchndmax
  real    :: Cmulchdecmax
  integer :: irac  
  integer :: iz  


! Apport d'eau par les résidus organiques (1 mm eau = 10 t eau /ha)
! -----------------------------------------------------------------
      Qeaures = qresidu * eauresidu / 1000.
    ! on ne considère l'apport que si l'eau apportée est > 0.2 mm
      if (Qeaures >= 0.2) then
        airg = airg + Qeaures ! airg(n+1)  =  airg(n+1) + Qeaures
        nap  =  nap + 1
      endif

!       Apport de C et N par le résidu organique
! L'apport se fait en surface du sol, sauf pour les racines
! ---------------------------------------------------------
!    Recalcul eventuel du rapport C/N du residu
!      1) Si CNresid = 0 on prend la moyenne des valeurs de calibration
        if(CNresid == 0.) then
          NCresmoy  =  1. / CNresmin + 1. / CNresmax
          CNresid  =  2. / NCresmoy
!        else
!      2) Si CNresid est en dehors des limites de calibration, on borne
!          CNresid = min(CNresid,CNresmax)
!          CNresid = max(CNresid,CNresmin)
        endif
!    Apport de C et N par le residu (kg/ha)
        Capp = qresidu * Crespc * 10. * (1. - eauresidu/100.)
        Napp = Capp / CNresid
!
!   Bruno mai 2012  Cumul des apports C et N provenant des apports de MO exogènes
       if(eauresidu >= 0.) then
           QCresorg = QCresorg + Capp
           QNresorg = QNresorg + Napp
        endif
!    Cumul des apports totaux de C et N organiques au sol
        QCapp = QCapp + Capp
        QNapp = QNapp + Napp
        if (Capp <= 0.) return

! Mise a jour des stocks de carbone
!    1) Cas des racines: apport de racines dans toutes les couches jusqu'a irac = min(P_profhum,zrac)
!    ---------------------------------------------------------------------------------------------
!!!!!
!!!MODIF HISAFE on rajoute les feuilles en profondeur (ires=20)
!!!!!
!!!!!   if (ires == nbResidus) then

        if ((ires == nbResidus) .OR. (ires == 20)) then
           irac = int(min(P_profhum, zrac))
           Capp = Capp/irac
           Napp = Napp/irac

           do iz=1,irac
             Cres(iz,ires)= Cres(iz,ires)+ Capp
             Nres(iz,ires)= Nres(iz,ires)+ Napp
            end do
        else
!    2) Cas des autres résidus: ils arrivent en surface et forment un mulch: modif Bruno juin 2012
!    ----------------------------------------------------------------------
            Cmulchdecmax = Qmulchdec * Crespc * 10.  ! maximal amount of decomposable C of residue ires

            Cres(1,ires)= Cres(1,ires) + Capp
            Nres(1,ires)= Nres(1,ires) + Napp

            Csup = Cres(1,ires) - Cmulchdecmax    ! extra C undecomposable
            if(Csup > 0.) then
               Cnondec(ires) = Cnondec(ires)  + Csup
               Cres(1,ires)  = Cres(1,ires)   - Csup
               Nsup          = Napp * Csup / Capp
               Nnondec(ires) = Nnondec(ires)  + Nsup
               Nres(1,ires)  = Nres(1,ires)   - Nsup
            endif



        endif

return
end subroutine ResidusApportSurfaceSol

 
 
