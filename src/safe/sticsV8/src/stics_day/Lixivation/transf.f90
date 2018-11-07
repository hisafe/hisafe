!****f* Lixivation/transf
! NAME
!   transf - ???
! DESCRIPTION
!   Module : Lixivation
!   Typologie : scientifique
! 
!   Calcul du transfert de l'eau et de l'azote dans le profil
!   Modèle réservoir: principe des "cellules de mélange" 

!
! INPUTS
!     nhe
!     hucc
!     etn
!     izc
!     hurlim
!     P_infil
!     P_concseuil
!     precip
!     nh
!     P_capiljour
!     P_humcapil
!     hr
!
! OUTPUTS
!     husup
!     azsup
!     remontee
!     hur
!     nit
!
! INPUTS & OUTPUTS
!     exces
!
!
!
!***
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module deals with downward circulation of soil water
!> - Stics book paragraphe 9.2.2, page 169-170
!>
!! This module deals with downward circulation of soil water, and is called by the module transfertsDescendants
!! Water transfer in the soil microporosity is calculated per elementary 1 cm layer using a reservoir-type analogy.  Water fills the layers by downward flow,
!! assuming that the upper limit of each basic reservoir corresponds to the layer's field capacity.
!! If the flow is not obstructed, (see macroporosite.f90), the excess water above field capacity is drained downward.  The soil layers affected by evaporation,
!! can dry until they reach the residual soil water content. In deeper layers, the water is only extracted by the plant and therefore always remains above
!! the wilting point.
!!
!! The transfer of nitrates is also described using this reservoir-type analogy, according to the "mixing cells" principle.  Any nitrate arriving by convection
!! with water in the elementary layer mixes with the nitrate already present.  Excess water then leaves with the new concentration of the mixture.
!! This description produces results which are very similar to the convection-dispersion model, the thickness of layers being equal to twice the
!! dispersivity (Mary et al., 1999).
!!
!! A minimum concentration level may exist (concseuil), below which mineral nitrogen cannot be leached. This can be a simple way to simulate ammonia nitrogen
!! without using the simulation of the ammoniacal phase of mineralisation.
!!
!! The amounts of drained water and leached nitrogen, i.e. leaving via the base of the soil profile are not retrievable by another crop.
!! Upwards nitrate movements occur via plant uptake only. Capillary rises provided by humid subsoil can be taken into account. Indeed, if the basal soil layer
!! is dry enough (below the humcapil threshold), capillary rise can occur from the subsoil into the soil, at a constant rate (capiljour) until the basal layer
!! reaches a moist status (above humcapil). As, in the model, these upward transfers take place through the macroporosity (they are considered negative
!! infiltration), they require a zero value of infiltrability at the base of the soil to be active.
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c
! DR 06/09/2012 j'ajoute qq param pour Joel (codemacropor, daf et epc)
subroutine transf(nhe,hucc,hur,nit,etn,izc,hurlim,exces,P_infil,P_concseuil,precip,husup,azsup,remontee,nh,P_capiljour,    &
                  P_humcapil,hr,P_codemacropor,da,P_epc)

  integer, intent(IN)    :: nhe  
  integer, intent(IN)    :: nh  
  integer, intent(IN)    :: izc(5)  
  real,    intent(IN)    :: hurlim  
  real,    intent(IN)    :: P_concseuil  !> // PARAMETER // Minimum Concentration of soil to NH4 // kgN ha-1 mm-1 // PARSOL // 1 
  real,    intent(IN)    :: precip   !> // OUTPUT // Daily amount of water (precipitation + irrigation)   // mm day-1
  real,    intent(IN)    :: P_capiljour  !> // PARAMETER // capillary rise upward water flux // mm j-1 // PARSOL // 1 
  real,    intent(IN)    :: P_humcapil  !> // PARAMETER // maximal water content to activate capillary rise // % pondéral // PARSOL // 1 
  real,    intent(IN)    :: hr   !> // OUTPUT // Water content of the horizon 5 (table)    // % pond.
  real,    intent(IN)    :: hucc(nhe)  
  real,    intent(IN)    :: etn(nhe)  
  real,    intent(IN)    :: P_infil(0:5)  !> // PARAMETER // infiltrability parameter at the base of each horizon (if codemacropor=1)// mm day-1 // PARSOL // 1   // OUTPUT // Infiltrability parameter at the base of the horizon 1 // mm day-1

  integer, intent(IN)    :: P_codemacropor  !< // PARAMETER // "Simulation option of water flux in the macroporosity of soils to estimate water excess and drip by  overflowing : yes(1), no (0)" // code 1/2 // PARSOL // 0
  real,    intent(IN)    :: da(5)
  real,    intent(IN)    :: P_epc(5)  !< // PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm




  
  real,    intent(OUT)   :: husup  
  real,    intent(OUT)   :: azsup  
  real,    intent(OUT)   :: remontee   !> // OUTPUT // Capillary uptake in the base of the soil profile // mm j-1
  real,    intent(INOUT) :: exces(0:5)   !> // OUTPUT // Amount of water  present in the macroporosity of the horizon 1  // mm
  real,    intent(INOUT) :: hur(nhe)  
  real,    intent(OUT)   :: nit(nhe)  

! VARIABLES LOCALES
  real     ::  alpha  !>  
  real     ::  conc0  !>  
  real     ::  conc1  !>  
  real     ::  ex  !>  
  real     ::  eaudrp  !>  
  real     ::  azlesp  !>  
  real     ::  eaudrm  !>  
  real     ::  azlesm  
  integer  ::  i  !>  
  integer  ::  icou  !>  
  integer  ::  iz  

    ! alpha: proportion d'effet piston  (0 a 1)
      alpha = 0.
      
    ! Transfert d'eau et d'azote d'une couche à l'autre: initialisation      
      husup = precip


      azsup = 0.
    ! boucle sur les couches elementaires
      do iz = 1,nhe
      
        icou = 0
        i = 1
      ! recherche des horizons de transition s'il y en a
        do while(i <= 5 .and. icou == 0)
          if (iz == izc(i)) icou = i
          i= i+1
        end do  
      
      !** ML - le 29/06/07 - pour ne pas diviser par une valeur nulle on ajoute
      !*- un test avant le calcul de conc0: si hur(iz) est nul on ne divise plus
      !*- par hur(iz) mais on pose conc0=0
        if (hur(iz) > 0.) then
          conc0 = nit(iz) / hur(iz)
        else 
          conc0 = 0
        endif
        
        nit(iz) = nit(iz) + azsup
        hur(iz) = hur(iz) + husup - etn(iz)

        if (hur(iz) < hurlim) then
          hur(iz) = hurlim  
          husup = 0.
          azsup = 0.
        endif
      
      ! circulation dans la microporosité
        husup = hur(iz) - hucc(iz)

      ! circulation dans la macroporosité
        if (icou /= 0) then
        ! remontée capillaires dans la dernière couche
        ! -- if(icou.eq.nh.and.hur(nhe).le.P_humcapil) then
          if(icou == nh .and. hr <= P_humcapil) then
            exces(icou) = exces(icou) + P_capiljour
            remontee = remontee - P_capiljour
          endif
          husup = max(husup,0.) + exces(icou)

          exces(icou) = max(husup - P_infil(icou),0.)
          ex = husup
        ! l'humidité est bornée au niveau de la discontinuité
          if (exces(icou) > 0.) hur(iz) = hucc(iz)

          husup = min(husup,P_infil(icou))
      
      !-----possible de faire remonter de l'eau de la base du profil
      !-----avec une infiltrabilité négative
      !             write(1,*)'Ce qui passe dans la couche inf.',husup,'mm'
      ! NB le 29/06 introduction d'une condition sur l'humidité du sol pour
      ! les remontées capillaires
      !            P_humcapil=10.0
      !        if(icou.eq.nh.and.husup.lt.0.0) then
      !                if(hur(nhe).ge.P_humcapil) husup=0.
      !            remontee=remontee+husup
      !            endif
      !------------------------------------------------------------------------
        endif
      
        if (husup <= 0.) then
          husup = 0.
          azsup = 0.
        else   
        ! -------- infiltration vers l'horizon iz+1
          eaudrp  = alpha * husup 
          hur(iz) = hur(iz) - eaudrp
          azlesp  = eaudrp * conc0
          nit(iz) = nit(iz) - azlesp
          if (hur(iz) > 0) then
            conc1  = nit(iz) / hur(iz)
          else 
            conc1 = 0
          endif
        ! à la base d'un horizon : correction par l'eau de la macroporosité
          if(icou /= 0) conc1 = nit(iz) / (hur(iz)+ex) 
          eaudrm  = (1. - alpha) * husup
          azlesm  = eaudrm * amax1(0.,conc1-P_concseuil)
          hur(iz) = hucc(iz)

          nit(iz) = nit(iz) - azlesm
          azsup   = azlesp + azlesm 
        endif

      end do

return
end 
 
