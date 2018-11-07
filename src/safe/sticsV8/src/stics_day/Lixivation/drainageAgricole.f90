!subroutine drainageAgricole(exces,nit,hmax,hpb,hph,profnappe,qdrain,   &
!                            azlesd,qlesd,hnappe,nh,n,P_codefente,P_infil,  &
!                            hur,macropor,ldrains,P_profimper,        &
!                            P_profdrain,profsol,P_ksol,P_concseuil,de)
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> calculation of agricultural drainage
!> - Stics book paragraphe 9.3, page 174-176
!>
!! The classic draining system uses the properties of symmetry arising from the presence of lines of drains with spacing (2 ldrains) which is generally constant
!! within a field.  Flow is assumed to occur from the space between drains towards the drain following a shape characterized by the parameter bformnappe
!! (in the module hauteurNappe.f90); it occurs within a water table based on an impermeable floor, the depth of which (profimper, in the module lixiv.f90) may be
!! greater than the soil depth considered in STICS.
!!
!! A simplification of the baseline Hooghoudt equation (1940) is used to simulate the daily water outflow (qdrain) at the drain level assuming a single hydraulic
!! conductivity above and below the drains (ksol). This equation relies on the estimation of de, the equivalent depth of the aquifer below the level of drains,
!! which first requires calculating hmax (de is calculated in the module lixiv.f90 and hmax is calculated in the module hauteurNappe.f90).
!! The Hooghoudt equation is normally valid under a permanent regime, but it was shown (Zimmer, 2001) that for sufficiently large time steps, it provides entirely
!! satisfactory predictions of the flows and water table heights in drainage systems.  The operating principle is as follows: when gravity flow begins following
!! saturation of the microporosity in the system, the macroporosity fills and creates a water table, whose level is at the top of the layer whose macroporosity
!! is saturated.  If we know the system parameters and the height of the previous table, a quantity of drained water is calculated, to which may be added,
!! if relevant, drainage linked to exchanges with deep layers of the soil.  The sum of these two drainage quantities is subtracted from the water contained in
!! the macroporosity, and a new water table height is calculated.
!!
!! Although it does not appear explicitly in the equations, the porosity of drainage plays an important role in the emptying and filling of soil macroporosity.
!! As a general rule, the simulations are correct only when the value of the soil macroporosity is equal to its drainage porosity.
!! In order to be able to account for the field heterogeneity due to the drainage system, it is possible to calculate the plant effects of the presence of a
!! water table either at the drain level or at the inter-drain level or for an average level.
!!
!! Nitrates can be leached through the drains and their amount is calculated assuming that nitrate concentration in the drained water is that of the hnappe level
!! (hnappe is calculated in the module hauteurNappe.f90).
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine drainageAgricole(n,nh,P_infil,hur,macropor,ldrains,P_profdrain,profsol,P_ksol,P_concseuil,de, &
                            exces,nit,hmax,qdrain,azlesd,qlesd,hnappe)

! On utilise le module Sorties pour connaitre les interfaces d'appels des routines et fonctions suivantes
USE Messages, only: EnvoyerMsgHistorique

!: Arguments
  integer, intent(IN)    ::  nh  
  integer, intent(IN)    ::  n  
  real,    intent(IN)    ::  P_infil(0:nh)  !> // PARAMETER // infiltrability parameter at the base of each horizon (if codemacropor = 1)// mm day-1 // PARSOL // 1   // OUTPUT // Infiltrability parameter at the base of the horizon 1 // mm day-1
  real,    intent(IN)    ::  hur(int(profsol))  
  real,    intent(IN)    ::  macropor(nh)  
  real,    intent(IN)    ::  ldrains  
  real,    intent(IN)    ::  P_profdrain  !> // PARAMETER // drain depth // cm // PARSOL // 1 
  real,    intent(IN)    ::  profsol  
  real,    intent(IN)    ::  P_ksol  !> // PARAMETER // hydraulic conductivity in the soil above and below the drains // SD // PARSOL // 1 
  real,    intent(IN)    ::  P_concseuil  !> // PARAMETER // Minimum Concentration of soil to NH4 // kgN ha-1 mm-1 // PARSOL // 1 
  real,    intent(IN)    ::  de  

  real,    intent(INOUT) ::  exces(0:nh)   !> // OUTPUT // Amount of water  present in the macroporosity of the horizon 1  // mm
  real,    intent(INOUT) ::  nit(int(profsol))  
  real,    intent(INOUT) ::  hmax   !> // OUTPUT // Maximum height of water table between drains // cm
  real,    intent(INOUT) ::  qdrain   !> // OUTPUT // Flow rate towards drains // mm j-1
  real,    intent(INOUT) ::  azlesd   !> // OUTPUT // Nitric nitrogen flow in drains // kgN.ha-1 j-1
  real,    intent(INOUT) ::  qlesd   !> // OUTPUT // Cumulated N-NO3 leached into drains // kgN.ha-1
  real,    intent(INOUT) ::  hnappe   !> // OUTPUT // Height of water table with active effects on the plant // cm


!: Variables locales
  integer ::  iazodrain  !>  
  integer ::  icou  
  real    ::  sommacropor  !>  
  real    ::  ajout  !>  
  real    ::  cotedrain  



      if (P_infil(nh) /= 0 .and. n == 1) then
        call envoyerMsgHistorique(666)
      endif

      ! somme des exces
      sommacropor = SUM(exces(1:nh))

      ! ((P_ksol*hmax² + 2*P_ksol*de*hmax) / ldrains² ) * 10
      qdrain = max(0., ((P_ksol * hmax**2 + 2. * P_ksol * de * hmax) / ldrains**2) * 10.) ! borné min. à 0
      if (qdrain > sommacropor) qdrain = sommacropor   ! borné max à sommacropor

      if (qdrain > 0.) then

        ! ** mise à jour de l'état de la macroporosité après drainage
        ! ** on calcule le lessivage dans les drains à partir de la concentration au niveau de nappe
        cotedrain = profsol - P_profdrain
        iazodrain = int(P_profdrain - hnappe + cotedrain) ! on cast en integer car iazodrain est un entier calculé à partir d'une somme de reels
! dr et ml 30/03/2012 si la nappe est remontée à la surface du profil on stoppe et on met un warning dans history.sti
        if(hnappe.eq.(P_profdrain+cotedrain))then
           call EnvoyerMsgHistorique(570)
           !stop
           call exit(9)

        endif
        azlesd = qdrain * max(0., nit(iazodrain)/hur(iazodrain)-P_concseuil)
        qlesd = qlesd + azlesd
        nit(iazodrain) = nit(iazodrain) - azlesd

        exces(nh) = exces(nh) - qdrain
        do while(exces(nh) < macropor(nh) .and. exces(nh-1) > 0.)
          do icou = nh,1,-1
            if (exces(icou) < macropor(icou) .and. exces(icou-1)> 0.) then
              exces(icou) = exces(icou) + min(exces(icou-1),P_infil(icou-1))
              if (exces(icou) > macropor(icou)) then
                ajout = exces(icou)-macropor(icou)
                exces(icou) = macropor(icou)
              else
                ajout = 0.
              endif
              exces(icou-1) = exces(icou-1) - min(exces(icou-1), P_infil(icou-1)) + ajout
            endif
          end do
        end do

      endif

end subroutine drainageAgricole
 
 
