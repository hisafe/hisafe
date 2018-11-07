! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module deals with foliage regulation by topping.
!> - Stics book paragraphe 6.1.3.a, page 96-97
!>
!! If the plant exhibits indeterminate growth, a trellis system is required, which can be simulated by imposing a maximal height and width: hautmaxtec and largtec.
!! Topping only concerns crops having a row structure and consists in restricting growth in terms of height (hautrogne) and width (largrogne) of the structure.
!! In order to ensure the efficiency of this technique, a minimum topped shoot biomass threshold must be observed (biorognem). The topped biomass and the
!! corresponding LAI are subtracted from the biomass and LAI of the plant. The calculation of this topped LAI (lairognecum) and biomass relies on the foliage
!! density dfol, the specific surface area of biomass, sbv using the variable sla.
!!
!! Topped biomass is recycled in the soil nitrogen balance. Two topping calculations may be employed. With automatic calculation, topping occurs as soon as the
!! plant height exceeds hautrogne+margerogne. The other possible calculation is done at an imposed date, nrogne (first read as "julrogne").
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine rognage(P_codcalrogne,hauteur,largeur,P_hautrogne,P_margerogne,P_largrogne,dfol,P_hautbase,P_interrang,sla,  & !IN
! IN DR 20/07/2012 on a plus besoin du Cnplante il est calculé apres
!                   P_tigefeuil,P_biorognem,n,nrogne,CNplante,                          &
                   P_tigefeuil,P_biorognem,n,nrogne,                                    & !IN
                   lairogne,biorogne,lai,masec,varrapforme,P_forme,biorognecum,lairognecum)                      !INOUT & OUT


USE Messages

implicit none

  integer, intent(IN)    :: P_codcalrogne  !> // PARAMETER // option of the way of calculation of tipping // code 1/2 // PARTEC // 0 
  integer, intent(IN)    :: n  
  integer, intent(IN)    :: nrogne  

  real,    intent(IN)    :: hauteur   !> // OUTPUT // Height of canopy // m
  real,    intent(IN)    :: largeur   !> // OUTPUT // Width of the plant shape  // m
  real,    intent(IN)    :: P_hautrogne  !> // PARAMETER // cutting height // m // PARTEC // 1 
  real,    intent(IN)    :: P_margerogne  !> // PARAMETER // allowed quantity of biomass inbetween two shapenings when asking automatic shapening  // t ha-1 // PARTEC // 1 
  real,    intent(IN)    :: P_largrogne  !> // PARAMETER // width of shapening // m // PARTEC // 1 
  real,    intent(IN)    :: dfol   !> // OUTPUT //  "Within the shape  leaf density" // m2 m-3
  real,    intent(IN)    :: P_hautbase  !> // PARAMETER // Base height of crop // m // PARPLT // 1 
  real,    intent(IN)    :: P_interrang  !> // PARAMETER // Width of the P_interrang // m // PARTEC // 1 
  real,    intent(IN)    :: sla   !> // OUTPUT // Specific surface area // cm2 g-1
  real,    intent(IN)    :: P_tigefeuil  !> // PARAMETER // stem (structural part)/leaf proportion // SD // PARPLT // 1 
  real,    intent(IN)    :: P_biorognem  !> // PARAMETER // minimal biomass to be removed when tipping (automatic calculation) // t ha-1 // PARTEC // 1 
! DR 20/07/2012 on a plus besoin du Cnplante il est calculé apres
! real,    intent(IN)    :: CNplante   !> // OUTPUT // Nitrogen concentration of entire plant  // %

  real,    intent(OUT)   :: lairogne  
  real,    intent(OUT)   :: biorogne  
  real,    intent(OUT)   :: lai   !> // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(OUT)   :: masec   !> // OUTPUT // Aboveground dry matter  // t.ha-1
  real,    intent(OUT)   :: varrapforme  
  integer, intent(OUT)   :: P_forme  !> // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0
  real,    intent(OUT)   :: biorognecum  
  real,    intent(OUT)   :: lairognecum  

!: VARIABLES LOCALES
  real :: hautro  
  real :: largro  

      !: ROGNAGE
      !- on ne peut pas "rogner" sans décrire la structure

      hautro = P_hautrogne
      largro = P_largrogne
      biorogne = 0.0
      lairogne = 0.0

      if (P_codcalrogne == 2) then !=> calcul automatique
        if (hauteur >= (P_hautrogne + P_margerogne) .or. largeur >= (P_largrogne + P_margerogne)) then
          !: Calcul du LAI rogné
          if (hauteur < P_hautrogne) hautro = hauteur
          if (largeur < P_largrogne) largro = largeur
          ! - ML le 15/01/08 correction d'une erreur: division du 2ème terme par P_interrang
          if (P_forme == 1) then
            lairogne = lai - dfol * (hautro - P_hautbase) * largro / P_interrang
          else
            lairogne = lai - 0.5 * dfol * (hautro - P_hautbase) * largro / P_interrang
          endif
          ! test sur la biomasse rognée
          biorogne = lairogne / (sla / (1.0 + P_tigefeuil)) * 100.
          if (biorogne < P_biorognem) then
            lairogne = 0.0
            biorogne = 0.0
            hautro = hauteur
            largro = largeur
          endif
          lai = lai - lairogne
          masec = masec - biorogne
          varrapforme = (hautro - P_hautbase) / largro
        endif
      endif

      !: Jour de rognage forcé
      if (P_codcalrogne == 1 .and. n == nrogne) then
        if (hauteur < P_hautrogne .and. largeur < P_largrogne) then
          call EnvoyerMsgHistorique(320)
        else
          !: calcul du LAI rogné
          if (hauteur < P_hautrogne) hautro = hauteur
          if (largeur < P_largrogne) largro = largeur
          if (P_forme == 1) then
            lairogne = lai - dfol * (hautro - P_hautbase) * largro / P_interrang
          else
            lairogne = lai - 0.5 * dfol * (hautro - P_hautbase) * largro / P_interrang
          endif
          !: test sur la biomasse rognée
          biorogne = lairogne / (sla / (1.0 + P_tigefeuil)) * 100.
          if (biorogne < P_biorognem) then
            call EnvoyerMsgHistorique(320)
            lairogne = 0.0
            biorogne = 0.0
            hautro = hauteur
            largro = largeur
          endif
          lai = lai - lairogne
          masec = masec - biorogne
          varrapforme = (hautro - P_hautbase) / largro

          P_forme = 1

          endif
      endif

      ! cumuls des "rognages"
      biorognecum = biorognecum + biorogne
      lairognecum = lairognecum + lairogne


end subroutine rognage
