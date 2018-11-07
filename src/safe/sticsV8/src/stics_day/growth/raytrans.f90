! **************************************************************** c
! * sous-programme de transfert radiatif                         * c
! * selon la valeur de P_codetransrad, ce sous-programme           * c
! * calcule le rayonnement intercepté par                        * c
! * 1 - une loi de Beer utilisant le paramètre P_EXTIN             * c
! * (recommandé pour la plupart des cultures herbacées)          * c
! * 2-  un modèle de transfert radiatif prenant en compte        * c
! * la géométrie du couvert (recommandé pour arbres et           * c
! * cultures principales dans les associations)                  * c
! **************************************************************** c
!> This subroutine calculates the radiation interception.
!> - Stics book paragraphe 3,2, page 49
!>
!! Since most crop models are devoted to industrial crops the canopy is assumed to be a homogenous environment with leaves being randomly distributed over the area.
!! A consequence of this random, homogeneous representation is that it allows the use of an optical analogy (Beer's law) to estimate the interception of
!! photosynthetically active radiation. This law, having only one parameter (the extinction coefficient), has been thoroughly studied for many crops
!! (Varlet-Grancher et al., 1989): the more erect the plant, the smaller is the extinction coefficient. This approach is very successful for homogenous crops,
!! but poorly suited to canopies in rows or during the first stages of an annual crop because the homogeneity hypothesis cannot apply.
!! Consequently, like CROPGRO (Boote and Pickering, 1994) the STICS model can simulate canopies in rows, with prediction of light interception dependent
!! not only on LAI, but also on plant height and width, row spacing, plant spacing and direct and diffuse light absorption. Such capabilities are also required
!! to simulate intercropping.
!!
!! Thus in STICS two options are available to calculate radiation interception: a simple Beer's law, recommended for homogenous crops (see beer.f90),
!! and a more complex calculation for radiation transfers within the canopy, recommended for crops in rows (see transrad.f90). If the leaf status variable
!! is the ground cover and not the leaf area index, then only the Beer's law option is permitted.
!-----------------------------------------------------------
subroutine raytrans(P_codetransrad,P_extin,cumrg,cumraint,fapar,delta,P_adfol,lairognecum,laieffcum,P_dfolbas,P_dfolhaut,     &
                    dfol,rdif,parapluie,raint,P_parsurrg,P_forme,lai,laisen,eai,P_interrang,nlax,nsen,P_codlainet,P_hautbase, &
                    P_codepalissage,P_hautmaxtec,P_largtec,originehaut,hauteur,deltahauteur,P_hautmax,varrapforme,largeur,    &
                    jul,trg,P_latitude, rombre,rsoleil,P_orientrang,P_ktrou,P_codelaitr,P_khaut,tauxcouv,surface,surfaceSous, &
                    ipl,P_nbplantes,ens,surfAO,surfAS)

  implicit none

!: Arguments

! RAYTRANS
  integer, intent(IN)    :: P_codetransrad  !> // PARAMETER // simulation option of radiation 'interception: law Beer (1), radiation transfers (2) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: surface(2)  
  real,    intent(OUT)   :: surfaceSous(2)  
  integer, intent(IN)    :: ipl  
  integer, intent(IN)    :: P_nbplantes  !> // PARAMETER // number of simulated plants // SD // P_USM/USMXML // 0 
  integer, intent(IN)    :: ens  
  real,    intent(INOUT) :: P_extin(P_nbplantes)  !> // PARAMETER // extinction coefficient of photosynthetic active radiation canopy // SD // PARPLT // 1
  real,    intent(INOUT) :: cumrg   !> // OUTPUT // Sum of global radiation during the stage sowing-harvest   // Mj.m-2
  real,    intent(INOUT) :: cumraint   !> // OUTPUT // Sum of intercepted radiation  // Mj.m-2
  real,    intent(OUT)   :: fapar   !> // OUTPUT // Proportion of radiation intercepted // 0-1
  real,    intent(OUT)   :: delta  

! TRANSRAD
  real,    intent(IN)    :: P_adfol  !> // PARAMETER // parameter determining the leaf density evolution within the chosen shape // m-1 // PARPLT // 1 
  real,    intent(IN)    :: lairognecum  
  real,    intent(IN)    :: laieffcum  
  real,    intent(IN)    :: P_dfolbas  !> // PARAMETER // minimal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1 
  real,    intent(IN)    :: P_dfolhaut  !> // PARAMETER // maximal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1 
  real,    intent(OUT)   :: dfol   !> // OUTPUT //  "Within the shape  leaf density" // m2 m-3
  real,    intent(OUT)   :: rdif   !> // OUTPUT // Ratio between diffuse radiation and global radiation  // 0-1
  integer, intent(OUT)   :: parapluie  
  real,    intent(OUT)   :: raint   !> // OUTPUT // Photosynthetic active radiation intercepted by the canopy  // Mj.m-2
  real,    intent(IN)    :: P_parsurrg  !> // PARAMETER // coefficient PAR/RG for the calculation of PAR  // * // STATION // 1 

  integer, intent(IN)    :: P_forme  !> // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: lai   !> // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(IN)    :: laisen   !> // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
  real,    intent(IN)    :: eai  
  real,    intent(IN)    :: P_interrang  !> // PARAMETER // Width of the P_interrang // m // PARTEC // 1 
  real,    intent(IN)    :: nlax  
  real,    intent(IN)    :: nsen  
  real,    intent(IN)    :: P_codlainet  !> // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
  real,    intent(IN)    :: P_hautbase  !> // PARAMETER // Base height of crop // m // PARPLT // 1 
  integer, intent(IN)    :: P_codepalissage  !> // PARAMETER // option: no (1),  yes2) // code 1/2 // PARTEC // 0 
  real,    intent(IN)    :: P_hautmaxtec  !> // PARAMETER // maximal height of the plant allowed by the management // m // PARTEC // 1 
  real,    intent(IN)    :: P_largtec  !> // PARAMETER // technical width // m // PARTEC // 1 
  real,    intent(IN)    :: originehaut  

  real,    intent(INOUT) :: hauteur   !> // OUTPUT // Height of canopy // m
  real,    intent(INOUT) :: deltahauteur  
  real,    intent(INOUT) :: P_hautmax  !> // PARAMETER // Maximum height of crop // m // PARPLT // 1 
  real,    intent(INOUT) :: varrapforme  

  real,    intent(OUT)   :: largeur   !> // OUTPUT // Width of the plant shape  // m

  integer, intent(IN)    :: jul  
  real,    intent(IN)    :: trg   !> // OUTPUT // Active radiation (entered or calculated) // MJ.m-2
  real,    intent(IN)    :: P_latitude  !> // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0 

  real,    intent(OUT)   :: rombre   !> // OUTPUT // Radiation fraction in the shade // 0-1
  real,    intent(OUT)   :: rsoleil   !> // OUTPUT // Radiation fraction in the full sun // 0-1
  real,    intent(IN)    :: P_orientrang  !> // PARAMETER // Direction of ranks // rd (0=NS) // PARTEC // 1 
  real,    intent(IN)    :: P_ktrou  !> // PARAMETER // Extinction Coefficient of PAR through the crop  (radiation transfer) // * // PARPLT // 1 

!  real,    intent(INOUT) :: surf(2,2)
!  integer, intent(IN)    :: ipl
!  integer, intent(IN)    :: P_nbplantes
! -- FIN TRANSRAD

! BEER
  integer, intent(IN)    :: P_codelaitr  !> // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0 
!  real,    intent(OUT)   :: raint
!  real,    intent(IN)    :: P_parsurrg
!  real,    intent(IN)    :: P_extin
!  real,    intent(IN)    :: lai
!  real,    intent(IN)    :: eai
!  real,    intent(IN)    :: trg
  real,    intent(IN)    :: tauxcouv   !> // OUTPUT // Cover rate // SD
!  real,    intent(OUT)   :: rombre
!  real,    intent(OUT)   :: rsoleil
!  integer, intent(OUT)   :: parapluie
!  integer, intent(IN)    :: nsen
!  integer, intent(IN)    :: nlax
!  integer, intent(IN)    :: P_codlainet
!  real,    intent(INOUT) :: hauteur
!  real,    intent(INOUT) :: deltahauteur
!  real,    intent(IN)    :: P_hautmax
!  real,    intent(IN)    :: P_hautbase
  real,    intent(IN)    :: P_khaut  !> // PARAMETER // Extinction Coefficient connecting leaf area index to height crop // * // PARAM // 1 
!  real,    intent(IN)    :: laisen

!  integer, intent(IN)    :: ipl
!  integer, intent(OUT)   :: surf(2,2)
! -- FIN BEER
  real,    intent(INOUT)   :: surfAO
  real,    intent(INOUT)   :: surfAS


! variables locales passé en commun
!    real :: surfAO  !>
!    real ::  surfAS


      !: loi de Beer
      if (P_codetransrad /= 2) then
          call beer(P_codelaitr,raint,P_parsurrg,P_extin(ipl),lai,eai,trg,tauxcouv,rombre,rsoleil,parapluie,nsen,nlax,P_codlainet, &
                    hauteur,deltahauteur,P_hautmax,P_hautbase,P_khaut,laisen,surface,ipl,surfaceSous)

      else


        ! 18/03/08 DR et ML
        !*****************************************
        ! pour la vigne il arrive qu'apres lev on est encore un lai nul si par exemple
        ! il fait trop froid (tempeff = 0) donc quand on rentre dans raytrans on a des pbs
        ! avec largeur = 0 division par zero et tout le binz
        ! on a mis un test qui nous semble logique mais qu'il faut discuter avce le chef

        if (lai  + eai > 0.0) then
            call transrad(P_adfol,lairognecum,laieffcum,P_dfolbas,P_dfolhaut,dfol,rdif,parapluie,raint,P_parsurrg,P_forme,lai,   &
                    laisen,eai, P_interrang,nlax,nsen,P_codlainet,P_hautbase,P_codepalissage,P_hautmaxtec,P_largtec,originehaut, &
                    hauteur,deltahauteur, P_hautmax,varrapforme,largeur,jul,trg,P_latitude,rombre,rsoleil,P_orientrang,P_ktrou,  &
  !                  surfAO,surfAS,ipl,P_nbplantes)
                    surfAO,surfAS,ipl)



          ! pour harmoniser avec la v6.4, je rajoute un test surle nombre de plantes
          ! pour ne pas affecter automatiquement la surface sous.
            if (P_nbplantes > 1) then
                surfaceSous(1) = surfAS
                surfaceSous(2) = surfAO
            endif
        endif
      endif
!write(71,*)'raytrans',raint

      !: cumul de rayonnement
      cumrg = cumrg + (trg * surface(ens))

      !: cumul du rayonnement intercepté
      cumraint = cumraint + (raint * surface(ens))

      !: calcul fapar
      if (trg > 0.) then
        fapar = raint / trg / P_parsurrg
      else
        fapar = 0.0
      endif

      !: Recalcul d'P_extin et delta en cas de transferts radiatif
      !- NB le 06/05/02
      if (P_codetransrad == 2) then
        if (lai > 0.0) then
          !: PB - 26/05/2004 - si fapar > 0.95 --> bug de log(x< = 0)
          if (fapar >= 0.95) then
            P_extin(ipl) = 0.0
            P_extin(ipl) = 0.0
          else
            P_extin(ipl) = -log(1 - fapar / 0.95) / lai
            P_extin(ipl) = -log(1 - fapar / 0.95) / lai
          endif
          delta = max(P_extin(ipl) - 0.2,0.0)
          !: PB & NB - 15/06/2004
          if (P_nbplantes > 1) delta = max(P_extin(2) - 0.2,0.0)
        endif
      endif

return
end subroutine raytrans
 
 
