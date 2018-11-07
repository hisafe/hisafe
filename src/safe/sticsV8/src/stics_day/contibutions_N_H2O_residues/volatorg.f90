! **************************************************************
!   Calcul de la volatilisation d'ammoniac 
!     issue des amendements organiques (type lisier)
!
! ------------------   Variables d'entrée   --------------------
! Caractéristiques du produit épandu
!  - teneur en azote organique                      (%MS)
!  - teneur en azote minéral      P_Nminres(i)      (%MS)
!  - matière sèche              P_eaures(i)    (%)
!  - quantité apportée          qresS(i)    (t/ha)
!  - rapport C/N            P_CsurNres(i)
!  - taux de rétention d'azote ammoniacal du produit(%)
! Techniques culturales 
!  - date d'apport              jultravS (i)
!  - profondeur d'enfouissement    proftravS (i)    (m)
! Paramètres sol
!  - P_pH   
!  - teneur en argile               P_argi         (%)
!  - porosité totale = 1-da1/2.66
!  - teneur en eau de surface (moy. journalière): hur(1)
!  - température de surface         tcult             (C)
!  - raa (min et max)
!  - ras (moyen)
! Potentiel de volatilisation (cf.thèse T. Morvan, 1999)
!   Nvolatorg = N volatilisable
!   Nnonvolat = N non volatilisable (rejoint le NH4 de la couche 1)
!
! Compartiments
!  AAT = azote ammoniacal total
!  AAs = azote ammoniacal adsorbé sur les fractions solides
!  AAaq =    "    "     en solution
!  NH3aq = concentration en ammoniac en phase aqueuse (mol/l) 
!  NH3g  = concentration en ammoniac en phase gazeuse (atm) 
!      
! ------------------   Variables de sortie ---------------------
!    - FsNH3     = flux de volatilisation (kg N/ha/j)
!    - Nvolorg   = quantité volatilisée cumulée (kg N/ha)
!    - Nvolatorg = potentiel de N volatilisable (kg N/ha)
! ***************************************************************
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 8.4, page 152-154
!>
!! Ammonia volatilization is a purely chemical process which operates on the soil ammonium pool (NH4+) and converts it into gaseous ammonia (NH3).
!! It affects the ammonium derived from mineral fertilizers or from organic fertilizers which contain large amounts of ammonium (such as liquid manure) and/or
!! which have rapid mineralizing potentials (e.g. vinasses). The current STICS version only simulates explicitly the volatilization following an application
!! of liquid organic manure.
!!
!! In order to simulate volatilization, it is necessary to consider four forms of ammonia compounds which are in equilibrium (Génermont and Cellier, 1997):
!>  - NH4s : ammonium ions (NH4+) adsorbed onto the mineral or organic soil fractions
!>  - NH4l : ammonium ions in solution in the liquid soil phase
!>  - NH3l : ammonia molecules (NH3) in solution in the liquid soil phase
!>  - NH3g: ammonia molecules in the gaseous soil phase.
!! All conditions which move these equilibrium towards the last form (e.g. high pH and temperature) stimulate volatilization.
!! Volatilization occurs at the soil surface and depends on the NH4+ concentration there: therefore it is affected by fertilizer type, fertilizer rate,
!! soil water content and NH4+ movement in soil. The equilibrium between NH4s and NH4l forms can be characterized by an adsorption isotherm which depends
!! on soil CEC (itself linked to clay and organic matter contents). NH4l and NH3l are linked through a chemical equilibrium which is pH- and temperature- dependent.
!! The solubility equilibrium between NH3l and NH3g forms mainly depends on temperature.
!!
!! The first step consists of defining the volatilizable NH4+ immediately after the application. The exchangeable NH4+ (Nminres, in kg N ha-1) is split
!! into two pools: a pool which remains at the soil surface and which can be volatilized (Nvolatorg, in kg N ha-1) and a pool which infiltrates and
!! is not volatilizable. The proportion of the volatilizable fraction (propvolat) increases with the dry matter content of the manure
!! or its water content (eaures)(see apportsOrganiquesEtTravailDuSol.f90).
!!
!! Furthermore, the addition of manure (containing urea type compounds and bicarbonates) is accompanied by a pH increase which is considered in the calculations.
!! Immediately after the manure application, the soil pH at soil surface (pHvol) increases by a value dpHvol, which varies with the mineral N level.
!! During the following days, the pH at the soil surface (pHvol) returns to the soil pH value (pH), at a rate proportional to the decrease in the volatilizable pool.
!! The model then calculates the amounts of the four forms: NH4s (equivalent to Aag in subroutine eqammo), NH4l(equivalent to Aaq in subroutine eqammo),
!! NH3l (equivalent to NH3aq in subroutine eqammo) and NH3g (equivalent to NH3g in subroutine eqammo, in mol m-2), using the acido-basic equilibria equations,
!! Henry solubility equation the transfer equations of Beutier and Renon (1978). These amounts depend on soil temperature, water content, soil porosity,
!! pH at soil surface and wind speed.
!!
!! The actual ammonia volatilization rate (Nvolorg, in kg N ha-1 day-1) is proportional to FsNH3 through a coefficient 0.036 which is a
!! unit conversion factor (µg m-2 s-1 into kg ha-1 day-1). However it can exceed neither the amount of ammonium at the soil surface (amm(1), in kg N ha-1)
!! nor the volatilizable pool (Nvolatorg). Finally, the volatilizable pool is updated daily: Nvolatorg = Nvolatorg - Nvolorg.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c

subroutine volatorg(da1,hur,P_pH,tcult,P_NH3ref,ras,P_ra,dpH,          & ! IN
                    FsNH3,Nvolorg,amm,Nvolatorg,QNvolorg,pHvol)   ! INOUT

  implicit none

!: Arguments
  real, intent(IN)    :: da1      !> // OUTPUT // Bulk density in the horizon 1 // g cm-3
  real, intent(IN)    :: hur(2)  
  real, intent(IN)    :: P_pH  !> // PARAMETER // P_pH of mixing soil + organic amendments  // SD // PARSOL // 1 
  real, intent(IN)    :: tcult      !> // OUTPUT // Crop surface temperature (daily average) // degree C
  real, intent(IN)    :: P_NH3ref  !> // PARAMETER // NH3 concentration in the atmosphere // ug.m-3 // STATION // 1
  real, intent(IN)    :: ras      !> // OUTPUT // Aerodynamic resistance between the soil and the canopy   // s.m-1
  real, intent(IN)    :: P_ra  !> // PARAMETER // Aerodynamic resistance (used in volatilization  module when we use ETP approach) // s m-1 // STATION // 1      // OUTPUT // Aerodynamic resistance between the cover and the reference level P_zr // s.m-1
  real, intent(IN)    :: dpH  

  real, intent(INOUT) :: FsNH3      !> // OUTPUT // Volatilisation of NH3  // µg.m-2.j-1
  real, intent(INOUT) :: Nvolorg  
  real, intent(INOUT) :: amm(1)  
  real, intent(INOUT) :: Nvolatorg  
  real, intent(INOUT) :: QNvolorg      !> // OUTPUT // Cumulated volatilisation of nitrogen from organic inputs // kgN.ha-1
  real, intent(INOUT) :: pHvol      !> // OUTPUT // soil surface P_pH varying after organic residue application (sutch as slurry) // SD

!: Variables locales
  integer :: heure  
  real    :: AAT  !>  
  real    :: AAg  
  real    :: NH3g  !>  
  real    :: NH3surf  
  real    :: cf  !>  
  real    :: H3O  !>  
  real    :: porosite  !>  
  real    :: RoAir  !>  
  real    :: tempsurf  !>  
  real    :: w  !>  
  real    :: zs  
      
      
      
      RoAir    = 1.2
      porosite = 1.0 - (da1 / 2.66)


! DR et ML et BM 22/06/09
! ***********************
! introduction des modifications de BM
! modif Bruno2004 :on prend l'humidité moyenne sur 0-2 cm et non 0-1 cm
!      w       = hur(1)/10.0
      w = SUM(hur(1:2)) / 20
! 22/06/09 FIN introduction des modifications de BM

      zs       = 0.01

      ! Stock de NH4 volatilisable: passage kg N/ha -->  mol/m2
      AAT = Nvolatorg / 140.

      ! concentration en protons
      H3O = 10.0**(-P_pH)
       
      ! temperature de surface (K)
      tempsurf = tcult + 273.16

      ! FsNH3: passage  µg/m2/s  --> mol/m2/jour
      cf = 1e-6 / 14. * 3600.

      do heure = 1,24
        call eqammo(tempsurf,AAT,zs,w,H3O,porosite,NH3g,AAg)
        ! NH3Surf concentration en surface (µg m-3 ou 1e9 ppb/atm) 
        NH3Surf = RoAir * NH3g * 1e9
        ! FsNH3 en µg/m2/s
        ! P_NH3ref =  concentration dans l'atmosphère (µg/m3)
        FsNH3 = (NH3surf - P_NH3ref) / (ras + P_ra)       
      end do

      ! passage µg/m2/s --> mol/m2/jour
      FsNH3 = FsNH3 * cf

      ! la quantité volatilisée ne peut être supérieure au N volatilisable
      FsNH3 = min(AAT, FsNH3)

      ! passage mol/m2/jour --> kg N/ha/jour
      FsNH3 = FsNH3 * 140.
      
      ! la quantité volatilisée ne peut être supérieure au NH4 disponible
      Nvolorg = min(amm(1), FsNH3)
      
      ! actualisation du stock de NH4, du N volatilisable et du N volatilisé
      amm(1)    = amm(1)    - Nvolorg

! DR et ML et BM 22/06/09
! ***********************
! introduction des modifications de BM
! modif Bruno2004   : le P_pH varie après l'apport de NH4 lisier
! eq 8.22
!      Nvolatorg = Nvolatorg - Nvolorg
! modif Bruno2004 :le N volatilisable est borné par la quantité de NH4 de la couche 1
      Nvolatorg = min(Nvolatorg - Nvolorg,amm(1))
! 22/06/09 FIN introduction des modifications de BM

      QNvolorg  = QNvolorg  + Nvolorg

! DR et ML et BM 22/06/09
! ***********************
! introduction des modifications de BM
! modif Bruno2004   : le P_pH varie après l'apport de NH4 lisier
! eq 8.19
      pHvol = P_pH + dpH * Nvolatorg
! 22/06/09 FIN introduction des modifications de BM


return
end subroutine volatorg


! *----------------------------------------------------------------* c
! *   Equilibres entre les differentes formes d'azote ammoniacal   * c
! *   dans la couche de surface                                    * c
! *----------------------------------------------------------------* c
subroutine eqammo(tempsurf,AAT,zs,w,H3O,porosite,NH3g,AAg)

  implicit none

!: Arguments
  real, intent(IN)  :: tempsurf  
  real, intent(IN)  :: AAT  
  real, intent(IN)  :: zs  
  real, intent(IN)  :: w  
  real, intent(IN)  :: H3O  
  real, intent(IN)  :: porosite  
  
  real, intent(OUT) :: NH3g    
  real, intent(OUT) :: AAg  
  
! Variables locales
  real    :: AAaq0  !>  
  real    :: CAaq  !>  
  real    :: NH3aq  !>  
  real    :: AAaq  !>  
  real    :: AAs  
  real    :: cf1  !>  
  real    :: cf2  !>  
  real    :: cf3  !>  
  real    :: cf4  !>  
  real    :: AAaq1  
  integer :: i  
      
!: fonction(s)
  real :: KaBR  !>  
  real :: KhBR  


      AAaq = AAT
      cf1 = 1. / (1. + H3O / KaBR(tempsurf))
      cf2 = KhBR(tempsurf)
      cf3 =  1./ (zs * 1000. * w)
      cf4 = (porosite - w) * 1000. * zs
  
      do i = 1,1000
        AAaq0 = AAaq
        ! concentration en AAaq (NH4aq + NH3aq)      (mol/L)  
        CAaq  = AAaq * cf3  
        ! concentration en ammoniac en phase aqueuse (mol/L)
        NH3aq = CAaq * cf1
        ! concentration en ammoniac en phase aqueuse (mol/L) d'apres Beutier et Renon, 1978
        NH3g  = NH3aq* cf2
        ! concentration en ammoniac en phase gazeuse (atm) d'apres Beutier et Renon, 1978
        AAg   = NH3g * cf4
        ! azote ammoniacal absorbé sur le complexe argilo-humique
        AAs  = 0.
      
        AAaq = AAT - AAs - AAg
        AAaq1 = 1.e-6 * abs(AAaq)
        if (abs(AAaq-AAaq0) < AAaq1 .or. AAaq1 < 1.) EXIT
      end do
   
return
end subroutine eqammo

! *---------------------------------------------------------------* c
! * constante d'equilibre acido basique NH3-NH4+ en phase liquide * c
! *       T = temperature (K)                                     * c
! *---------------------------------------------------------------* c
real FUNCTION KaBR(T)

!: Arguments
  real, intent(IN) :: T  

      KaBR = exp(-177.953 - (1843.22 / T) + (31.4335 * LOG(T)) - (0.0544943 * T))      

return
end function KaBR

! *----------------------------------------------------------------* c
! * constante de la loi d'Henry: formule de Beutier et Renon, 1978 * c
! * equilibre NH3(phase liquide, mol/L) - NH3(phase gazeuse, atm)  * c
! *----------------------------------------------------------------* c
real function KhBR(T)

!: Arguments
  real, intent(IN) :: T  

      KhBR = exp(160.559 - (8621.06 / T) - (25.6767 * LOG(T)) + (0.035388 * T))      

return
end function KhBR
 
 
