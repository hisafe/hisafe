!*****************************************************************
! sous-programme de calcul
! de suppression des feuilles
! par effeuillage ou rognage
! NB le 13/01/2003
!*****************************************************************
! EFFEUILLAGE
!-------------
! par le haut : codeffhaut = 1
! par le bas  : codeffhaut = 2
! effeuillage automatique : P_codcaleffeuil = 1
!            * à partir d'un niveau d'indice foliaire : laidebeffeuil
!            * on supprime un % du deltai produit  : P_effeuil
! effeuillage forcé : P_codcaleffeuil = 2
!            * date d'effeuillage : P_juleffeuil
!            * quantité supprimée : P_laieffeuil
! pas d'effeuillage P_codeffeuil = 1
!! ****************************************************************
!> This module deals with foliage regulation by leaf removal.
!> - Stics book paragraphe6.1.3.a, page 97
!>
!! Leaf removal (laieffcum) is expressed directly by reducing the leaf area index, also according to two possible methods.
!! With the automatic calculation, a constant proportion (effeuil) of the new foliage generated each day is removed as soon as the LAI reaches a threshold
!! value (laidebeff). The other possible calculation is done on only one occasion, on day juleffeuil and the quantity laieffeuil is removed.
!!
!! The corresponding biomass is calculated from the specific leaf area (sla) and deducted from the biomass of the plant. Another option concerns the location
!! of leaf removal: the top or bottom of the canopy, which affects the radiation and water balances of crops in rows.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------*
subroutine effeuill(P_codcaleffeuil,P_laidebeff,deltai,P_effeuil,sla,n,neffeuil,P_laieffeuil,   & ! IN
                    P_codetransrad,P_hautmax,P_khaut,dfol,P_largrogne,P_codhauteff,largeur,     & ! IN
                    lai,masec,P_hautbase,varrapforme,bioeffcum,laieffcum)                         ! INOUT

USE Messages

implicit none

!: ARGUMENTS
  integer, intent(IN)    :: P_codcaleffeuil  !> // PARAMETER // option of the way of caluclation of leaf removal // code 1/2 // PARTEC // 0 
  real,    intent(IN)    :: P_laidebeff  !> // PARAMETER // LAI of the beginning of leaf removal // m2 m-2 // PARTEC // 1 
  real,    intent(IN)    :: deltai   !> // OUTPUT // Daily increase of the green leaf index // m2 leafs.m-2 soil
  real,    intent(IN)    :: P_effeuil  !> // PARAMETER // proportion of daily leaf removed at thinning // 0-1  // PARTEC // 1 
  real,    intent(IN)    :: sla   !> // OUTPUT // Specific surface area // cm2 g-1
  integer, intent(IN)    :: n  
  integer, intent(IN)    :: neffeuil  
  real,    intent(IN)    :: P_laieffeuil  !> // PARAMETER // LAI of the end of leaf removal // m2 m-2 // PARTEC // 1 
  integer, intent(IN)    :: P_codetransrad  !> // PARAMETER // simulation option of radiation interception: law Beer (1), radiation transfers (2) // code 1/2 // PARPLT // 0
  real,    intent(IN)    :: P_hautmax  !> // PARAMETER // Maximum height of crop // m // PARPLT // 1 
  real,    intent(IN)    :: P_khaut  !> // PARAMETER // Extinction Coefficient connecting leaf area index to height crop // * // PARAM // 1 
  real,    intent(IN)    :: dfol   !> // OUTPUT // (Within the shape) leaf density // m2 m-3
  real,    intent(IN)    :: P_largrogne  !> // PARAMETER // width of shapening // m // PARTEC // 1 
  integer, intent(IN)    :: P_codhauteff  !> // PARAMETER // leaf removal heitgh // code 1/2 // PARTEC // 0 
  real,    intent(IN)    :: largeur   !> // OUTPUT // Width of the plant shape  // m

  real,    intent(INOUT) :: lai   !> // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(INOUT) :: masec   !> // OUTPUT // Aboveground dry matter  // t.ha-1
  real,    intent(INOUT) :: P_hautbase  !> // PARAMETER // Base height of crop // m // PARPLT // 1 
  real,    intent(INOUT) :: varrapforme  
  real,    intent(INOUT) :: bioeffcum  
  real,    intent(INOUT) :: laieffcum  

! VARIABLES LOCALES
  real :: bioeff  
  real :: laieff  
  real :: reducep  

      if (lai <= 0.0) return

      laieff = 0.0
      if (P_codcaleffeuil == 1) then
        if (lai > P_laidebeff) then
          laieff = deltai * P_effeuil
          bioeff = laieff / sla * 100.
          lai = lai - laieff
          masec = masec - bioeff
        endif
      endif

      if (P_codcaleffeuil == 2) then
        if (n == neffeuil) then
          laieff = min(P_laieffeuil,lai)
          bioeff = laieff / sla * 100.
          lai = lai - laieff
        !: modif Marie le 06/09/05: désormais, on stoppe le programme que si le lai est
        !- strictement inférieur à 0, et non s'il est simplement égal à 0; cela permet d'enlever toutes
        !- les feuilles si on le souhaite (cf manip betterave)
        if (lai < 0.) then
            call EnvoyerMsgHistorique(321)
            !stop
            call exit(9)
          endif
          masec = masec - bioeff
        endif
      endif

! ** calcul de la hauteur
      if (laieff > 0.0) then
        if (P_codetransrad == 1) then
          reducep = P_hautmax * (1 - exp(-P_khaut * laieff))
        else
          reducep = laieff / (dfol * P_largrogne)
        endif
      if (P_codhauteff == 1) P_hautbase = P_hautbase - reducep
        if (P_codhauteff == 2 .and. P_codetransrad == 2) then
          varrapforme = varrapforme - reducep / largeur
        endif
      endif

      ! cumul des "effeuillages"
      bioeffcum = bioeffcum + bioeff
      laieffcum = laieffcum + laieff

return
end subroutine effeuill
