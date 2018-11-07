! /***************************************************************************
!  * Calcule le rayonnement intercepté par une loi de Beer utilisant le
!  * paramètre P_extin  (recommandé pour la plupart des cultures herbacées)
!  */
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 3.2.1, page 49
!>
!! This module calculates the radiation interception, assuming that the canopy is a homogenous environment with leaves being randomly distributed over the area.
!! A consequence of this random, homogeneous representation is that it allows the use of an optical analogy (Beer's law) to estimate the interception of
!! photosynthetically active radiation.
!!
!! Thus, the radiation intercepted by the crop (raint) is expressed according to a Beer's law function of LAI.  extin is a daily extinction coefficient and
!! parsurrg is a climatic parameter corresponding to the ratio of photosynthetically active radiation to the global radiation, trg
!! For homogenous crops, crop height is deduced from the leaf area index or the ground cover. It serves particularly in the calculation module for
!! water requirements via the resistive option. khaut is assumed to be plant-independent (a general value of 0.7 is proposed) while the potential height
!! of foliage growth is mostly plant-dependent and defined by the two limits hautbase and hautmax.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine beer(P_codelaitr,raint,P_parsurrg,P_extin,lai,eai,trg,tauxcouv,rombre,rsoleil,parapluie,nsen,  &
                nlax,P_codlainet,hauteur,deltahauteur,P_hautmax,P_hautbase,P_khaut,laisen,surface,ipl,    &
                surfaceSous)

  implicit none

!: Arguments
  integer, intent(IN)    :: P_codelaitr  !< // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0
  real,    intent(OUT)   :: raint   !< // OUTPUT // Photosynthetic active radiation intercepted by the canopy  // Mj.m-2
  real,    intent(IN)    :: P_parsurrg  !< // PARAMETER // coefficient PAR/RG for the calculation of PAR  // * // STATION // 1
  real,    intent(IN)    :: P_extin  !< // PARAMETER // extinction coefficient of photosynthetic active radiation canopy // SD // PARPLT // 1
  real,    intent(IN)    :: lai   !< // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(IN)    :: eai  
  real,    intent(IN)    :: trg   !< // OUTPUT // Active radiation (entered or calculated) // MJ.m-2
  real,    intent(IN)    :: tauxcouv   !< // OUTPUT // Cover rate // SD
  real,    intent(OUT)   :: rombre   !< // OUTPUT // Radiation fraction in the shade // 0-1
  real,    intent(OUT)   :: rsoleil   !< // OUTPUT // Radiation fraction in the full sun // 0-1
  integer, intent(OUT)   :: parapluie  
  integer, intent(IN)    :: nsen  
  integer, intent(IN)    :: nlax  
  integer, intent(IN)    :: P_codlainet  !< // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
  real,    intent(INOUT) :: hauteur   !< // OUTPUT // Height of canopy // m
  real,    intent(INOUT) :: deltahauteur  
  real,    intent(IN)    :: P_hautmax  !< // PARAMETER // Maximum height of crop // m // PARPLT // 1
  real,    intent(IN)    :: P_hautbase  !< // PARAMETER // Base height of crop // m // PARPLT // 1
  real,    intent(IN)    :: P_khaut  !< // PARAMETER // Extinction Coefficient connecting leaf area index to height crop // * // PARAM // 1
  real,    intent(IN)    :: laisen   !< // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
  real,    intent(IN)    :: surface(2)  

  integer, intent(IN)    :: ipl  
  real,    intent(OUT)   :: surfaceSous(2)  


!: Variables locales
  real :: hauteurjour  


      if (P_codelaitr == 1) then
        !: NB le 28/03/02 ajout de eai
        raint = 0.95 * P_parsurrg * (1.0 - (exp(-P_extin * (lai + eai)))) * trg

      else
        !: Nb le 15/06
        raint = 0.95 * tauxcouv * trg * P_parsurrg

      endif


      rombre = 0.0
      rsoleil = 0.0

      !: NB le 26/3/98 pour irrig
      if (ipl == 1) then
        if (P_codelaitr == 1) then
          surfaceSous(1) = exp(-P_extin * lai)
        else
          surfaceSous(1) = 1 - tauxcouv
        endif
        surfaceSous(2) = 1 - surface(1)
        ! Seuillage des surfaces
        if (surfaceSous(2) <= 0.0) then
          surfaceSous(2) = 0.001
          surfaceSous(1) = 1 - surfaceSous(2)
        endif
      endif

      !: Pas d'effet parapluie quand on utilise une loi de Beer
      parapluie = 0

      !: Calcul de la hauteur (modif le 19/02/03)
      !- NB - 23/11 : introduction du supplément de hauteur entre lax et sen
      if (P_codelaitr == 1) then
        if (nsen == 0) then
          if (nlax > 0 .and. nsen == 0) then
            if (P_codlainet == 1) hauteur = hauteur + deltahauteur
          else
            hauteurjour =  (P_hautmax - P_hautbase) * (1 - exp(-P_khaut * (lai + laisen))) + P_hautbase
            !write(*,*) '1. hauteurjour=',hauteurjour,' <= ',P_hautmax,P_hautbase,P_khaut,lai,laisen
            deltahauteur = hauteurjour - hauteur
            !write(*,*) '1. deltahauteur=',deltahauteur,' <= ',hauteurjour,hauteur
            hauteur = hauteurjour
          endif
! ML le 19/06/06 je supprime l erreur qui consistait à poser hauteur = P_hautmax
! après le début de la sénescence: en fait la hauteur ne change plus
!        else
!          hauteur = P_hautmax
        endif
      else
        if (nsen == 0) then
          if (nlax > 0 .and. nsen == 0) then
            hauteur = hauteur + deltahauteur
          else
            hauteurjour = P_hautbase + (P_hautmax - P_hautbase) * tauxcouv
            deltahauteur = hauteurjour - hauteur
            hauteur = hauteurjour
          endif
! ML le 19/06/06 je supprime l erreur qui consistait à poser hauteur = P_hautmax
! après le début de la sénescence: en fait la hauteur ne change plus
!        else
!          hauteur = P_hautmax
        endif
      endif

      if (hauteur > P_hautmax) hauteur = P_hautmax

return
end subroutine beer
 
 
