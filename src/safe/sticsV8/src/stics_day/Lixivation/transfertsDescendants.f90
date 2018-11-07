!****f* Lixivation/TransfersDescendants
! NAME
!   TransfertsDescendants -
! DESCRIPTION
!   Module : Lixivation
!   Typologie : scientifique
!
!
! INPUTS
!     etn
!     hucc
!     ncel
!     icel
!     da
!     hr
!     nh
!     nhe
!     hurlim
!     izcel
!     P_infil
!     P_concseuil
!     precip
!     husup
!     P_humcapil
!     izc
!     P_capiljour
!
! OUTPUTS
!     azlesm
!     azsup
!     remontee
!
! INPUT & OUTPUTS
!     nit
!     hur
!     exces
!***
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 9.2, page 169
!>
!> This module deals with downward circulation of soil water, and call the sub-module transf.f90.
!!
!! The nitrates circulate with water downwards through the soil. The way these transfers are accounted for in STICS relies on the soil compartmental description
!! and on the tipping bucket concept. The description of the soil can involve up to four compartments: microporosity, macroporosity,
!! cracks (the case of swelling clay soils) and pebbles.  However, only the description of microporosity is obligatory, the description of the other compartments
!! being optional. Whatever, soil microporosity is the basis for calculating water and nitrogen transfer values.
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine TransfertsDescendants(nit,hur,etn,hucc,ncel,icel,da,hr,nh,nhe,hurlim,izcel, P_infil, P_concseuil, precip, husup, &
                                 azsup, remontee, P_humcapil, izc, exces, azlesm, P_capiljour,P_codemacropor,P_epc)
! DR 06/09/2012 j'ajoute qq param pour Joel (codemacropor et epc)

! On utilise le module Lixivation pour connaitre les interfaces d'appels des routines et fonctions suivantes
USE Lixivation, only: transf, sommeCouchesParCellule

! Tout explicite
implicit none

! Déclaration d'interface obligatoire étant donné que la fonction sommeCouchesParCellule retourne un tableau.
! C'est une contrainte du fortran 90
!interface
!  function sommeCouchesParCellule(nbCellules, iCellules, tabCouches)
!    integer, intent(in) :: nbCellules
!    integer, intent(in) :: iCellules(0:nbCellules)
!    real,    intent(in) :: tabCouches(iCellules(nbCellules))
!    !real, dimension(nbCellules) :: sommeCouchesParCellule
!  end function sommeCouchesParCellule
!end interface

! ENTREES
  integer, intent(IN)   :: ncel  
  integer, intent(IN)   :: nh  
  integer, intent(IN)   :: nhe  
  integer, intent(IN)   :: icel(0:ncel)  
  integer, intent(IN)   :: izcel(5)                         ! dim = 5. A faire varier ?  
  integer, intent(IN)   :: izc(5)                           ! dim = 5. A faire varier ?  
  real,    intent(IN)   :: hurlim  
  real,    intent(IN)   :: P_concseuil  !> // PARAMETER // Minimum Concentration of soil to NH4 // kgN ha-1 mm-1 // PARSOL // 1 
  real,    intent(IN)   :: precip   !> // OUTPUT // Daily amount of water (precipitation + irrigation)   // mm day-1
  real,    intent(IN)   :: P_humcapil  !> // PARAMETER // maximal water content to activate capillary rise // % pondéral // PARSOL // 1 
  real,    intent(IN)   :: P_capiljour  !> // PARAMETER // capillary rise upward water flux // mm j-1 // PARSOL // 1 
  real,    intent(IN)   :: etn(nhe)                           ! 1 to icel(ncel), 1 to nhe. Il faut prendre le max, a priori nhe >= icel(ncel)
  real,    intent(IN)   :: hucc(nhe)                          ! 1 to icel(ncel), 1 to nhe. Il faut prendre le max, a priori nhe >= icel(ncel)
  real,    intent(IN)   :: da(nh)                           ! (nh)    // OUTPUT // Bulk density in the horizon 1 // g cm-3
  real,    intent(IN)   :: hr(nh)                           ! (nh)    // OUTPUT // Water content of the horizon 5 (table)    // % pond.
                       ! 0 to 5 d'après transf
  real,    intent(IN)   :: P_infil(0:5) !< // PARAMETER // infiltrability parameter at the base of each horizon (if codemacropor=1)// mm day-1 // PARSOL // 1   // OUTPUT // Infiltrability parameter at the base of the horizon 1 // mm day-1

  integer, intent(IN)    :: P_codemacropor  !< // PARAMETER // "Simulation option of water flux in the macroporosity of soils to estimate water excess and drip by  overflowing : yes(1), no (0)" // code 1/2 // PARSOL // 0
  real,    intent(IN)    :: P_epc(nh)  !< // PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm

! SORTIES
  real,    intent(OUT)   :: azlesm  
  real,    intent(OUT)   :: azsup  
  real,    intent(OUT)   :: husup  
  real,    intent(OUT)   :: remontee   !> // OUTPUT // Capillary uptake in the base of the soil profile // mm j-1
  real,    intent(INOUT) :: nit(nhe)                          ! 1 to icel(ncel), 1 to nhe. Il faut prendre le max, a priori nhe >= icel(ncel)
  real,    intent(INOUT) :: hur(nhe)                          ! 1 to icel(ncel), 1 to nhe. Il faut prendre le max, a priori nhe >= icel(ncel)
  real,    intent(INOUT) :: exces(0:5)                      ! 0 to 5 d'après transf    // OUTPUT // Amount of water  present in the macroporosity of the horizon 1  // mm

! VARIABLES LOCALES
  integer               ::  ii  !>  
  integer               ::  iz  
  real                  ::  hrtest  
  real                  ::  hurlimcel  
  real                  ::  azot  
   ! concel obsolète ? inutile ?
  real, dimension(ncel) ::  nitcel  
  real, dimension(ncel) ::  hurcel  
  real, dimension(ncel) ::  etncel  
  real, dimension(ncel) ::  huccel  
  real, dimension(0:5)  ::  excel  


    ! on initialise les tableaux locaux à 0
      nitcel = 0.
      hurcel = 0.
      etncel = 0.
      huccel = 0.
      excel = 0.

    ! ---------------------------------------------------------------------
    ! 1) Calcul des stocks d'eau et d'azote par cellule de mélange
      nitcel = sommeCouchesParCellule(ncel,icel(0:ncel),nit(1:icel(ncel)))
      hurcel = sommeCouchesParCellule(ncel,icel,hur(1:icel(ncel)))
      etncel = sommeCouchesParCellule(ncel,icel,etn(1:icel(ncel)))
      huccel = sommeCouchesParCellule(ncel,icel,hucc(1:icel(ncel)))

    ! ---------------------------------------------------------------------
    ! 2) Calcul des transferts entre les cellules de mélange

!        write(1752,*) '===========',ncel
!        write(1752,*) huccel
!        write(1752,*) hurcel
!        write(1752,*) nitcel
!        write(1752,*) etncel
!        write(1752,*) izcel
!        write(1752,*) hurlimcel
!        write(1752,*) excel
!        write(1752,*) P_infil
!        write(1752,*) P_concseuil
!        write(1752,*) precip
!        write(1752,*) husup
!        write(1752,*) azsup
!        write(1752,*) remontee
!        write(1752,*) nh
!        write(1752,*) 0.
!        write(1752,*) P_humcapil
!        write(1752,*) hrtest

    ! *- humidite minimale (mm)
      hrtest = hr(nh) * da(nh) / 10.

    ! *- le 17/06/04 - les 3 - on met en commentaire (Hurlim est une constante et n'est pas fonction des cellules de melange)
    ! --      hurlim=ha*10.*nhe/ncel
    ! -- le 21/07/04 - domi - je cree une variable intermediaire car en fait Bruno
    ! -- se sert de hurlim avec une autre signification (quantité d'eau et non humidité)
      hurlimcel=hurlim*nhe/ncel
      call transf(ncel,huccel,hurcel,nitcel,etncel,izcel,hurlimcel,           &
                  excel,P_infil,P_concseuil,precip,husup,azsup,remontee,nh,       &
                  0.,P_humcapil,hrtest,P_codemacropor,da,P_epc)

!        write(1753,*) '===========',ncel
!        write(1753,*) huccel
!        write(1753,*) hurcel
!        write(1753,*) nitcel
!        write(1753,*) etncel
!        write(1753,*) izcel
!        write(1753,*) hurlimcel
!        write(1753,*) excel
!        write(1753,*) P_infil
!        write(1753,*) P_concseuil
!        write(1753,*) precip
!        write(1753,*) husup
!        write(1753,*) azsup
!        write(1753,*) remontee
!        write(1753,*) nh
!        write(1753,*) 0.
!        write(1753,*) P_humcapil
!        write(1753,*) hrtest

    ! ---------------------------------------------------------------------
    ! 3) Calcul des transferts entre les couches elementaires

!        write(1762,*) '===========',nhe
!        write(1762,*) hucc
!        write(1762,*) hur
!        write(1762,*) nit
!        write(1762,*) etn
!        write(1762,*) izc
!        write(1762,*) hurlim
!        write(1762,*) exces
!        write(1762,*) P_infil
!        write(1762,*) P_concseuil
!        write(1762,*) precip
!        write(1762,*) husup
!        write(1762,*) azlesm
!        write(1762,*) remontee
!        write(1762,*) nh
!        write(1762,*) P_capiljour
!        write(1762,*) P_humcapil
!        write(1762,*) hrtest

    ! *!* PB - 16/03/2004 - azles -> azlesd ou azlesm ?
      call transf(nhe,hucc,hur,nit,etn,izc,hurlim,exces,P_infil,                &
                  P_concseuil,precip,husup,azlesm,remontee,nh,P_capiljour,        &
                  P_humcapil,hrtest,P_codemacropor,da,P_epc)

!        write(1763,*) '===========',nhe
!        write(1763,*) hucc
!        write(1763,*) hur
!        write(1763,*) nit
!        write(1763,*) etn
!        write(1763,*) izc
!        write(1763,*) hurlim
!        write(1763,*) exces
!        write(1763,*) P_infil
!        write(1763,*) P_concseuil
!        write(1763,*) precip
!        write(1763,*) husup
!        write(1763,*) azlesm
!        write(1763,*) remontee
!        write(1763,*) nh
!        write(1763,*) P_capiljour
!        write(1763,*) P_humcapil
!        write(1763,*) hrtest

    ! ---------------------------------------------------------------------
    ! 4)  Calcul des nouvelles quantites d'azote par couche
      do ii = 1,ncel
        !concel = nitcel(ii)/hurcel(ii)
        azot = 0. ! par défaut et pour éviter les erreurs
        azot = SUM(nit(icel(ii-1)+1:icel(ii)))
      ! ** la quantite d'azote est proportionnelle au cas dispersion 1 cm
        if (azot > 0.) then
          do iz = icel(ii-1)+1,icel(ii)
            nit(iz) = nit(iz)*nitcel(ii)/azot
          end do
        endif
      end do

end subroutine
 
 
