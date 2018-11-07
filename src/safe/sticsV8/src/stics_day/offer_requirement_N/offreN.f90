! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 8.6.2, 8.6.3., 8.6.4, page 98-100
!>
!! The soil supply is the maximum amount of mineral N that the soil can deliver to the surface of roots, for a given status of soil and plant root system.
!! It is calculated for each elementary layer (1 cm thick) from the surface to the maximum rooting depth (zrac, in cm). It does not account for possible
!! nitrate upflow by capillary rise (this would require a knowledge of the nitrate concentration in the soil below the rooting depth).
!! The soil N supply in each soil layer (fluxsol, in kg N ha-1 day-1) is determined by the transport of mineral N from a given soil location to the nearest root
!! by convection and diffusion.
!!
!! The potential uptake rate in each soil layer is fluxrax (kg N ha-1 day-1). It is proportional to the effective root density which is limited by the
!! threshold lvopt above which uptake is no longer limited by root density.
!!
!! The mineral N available for root uptake in each layer (offrN) is equal to the smallest of the three terms: soil supply, uptake capacity and available mineral N.
!! The integration of offrN over the whole profile yields cumoffrN.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
subroutine offreN(zrac,nit,amm,hur,humin,hucc,flrac,P_lvopt,P_difN,epz,P_Vmax1,P_Kmabs1,P_Vmax2,P_Kmabs2, &
                  cumoffrN,flurac,flusol,offrN)

  implicit none
  
!: Arguments

real, intent(IN)    :: zrac      !> // OUTPUT // Depth reached by root system // cm
real, intent(IN)    :: nit(int(zrac))  
real, intent(IN)    :: amm(int(zrac))  
real, intent(IN)    :: hur(int(zrac))  
real, intent(IN)    :: humin(int(zrac))  
real, intent(IN)    :: hucc(int(zrac))  
real, intent(IN)    :: flrac(int(zrac))  
real, intent(IN)    :: P_lvopt  !> // PARAMETER // Optimum root density // cm root.cm-3 soil // PARAM // 1 
real, intent(IN)    :: P_difN  !> // PARAMETER // coefficient de diffusion apparente du nitrate dans le sol humide // cm2 jour-1 // PARAM // 1 
real, intent(IN)    :: epz(int(zrac))  
real, intent(IN)    :: P_Vmax1  !> // PARAMETER // Rate of slow nitrogen absorption (high affinity system) // µmole cm-1 h-1 // PARPLT // 1 
real, intent(IN)    :: P_Kmabs1  !> // PARAMETER // Constant of nitrogen uptake by roots for the high affinity system // µmole. cm root-1 // PARPLT // 1 
real, intent(IN)    :: P_Vmax2  !> // PARAMETER // Rate of rapid nitrogen absorption (high affinity system) // µmole cm-1 h-1 // PARPLT // 1 
real, intent(IN)    :: P_Kmabs2  !> // PARAMETER // Constant of nitrogen uptake by roots for the low affinity system // µmole. cm root-1 // PARPLT // 1 

real, intent(INOUT) :: cumoffrN  
real, intent(INOUT) :: flurac      !> // OUTPUT // Nitrogen absorption flow associated with the limiting absorption capacity of the plant // kgN ha-1 j-1
real, intent(INOUT) :: flusol      !> // OUTPUT // Nitrogen absorption flux associated with limiting transfer soil  --> root  // kgN ha-1 j-1
real, intent(INOUT) :: offrN(int(zrac))  
  
!: Variables locales
  integer :: iz  !>  
  integer :: NRAC  
  real    :: concN  !>  
  real    :: conv  !>  
  real    :: diff  !>  
  real    :: fluxrac  !>  
  real    :: fluxsol  !>  
  real    :: rh  !>  
  real    :: Vabs  
  real    :: lrac  

!write(245,*)zrac
!write(245,*)nit
!write(245,*)amm
!write(245,*)hur
!write(245,*)humin
!write(245,*)hucc
!write(245,*)'flrac',flrac
!write(245,*)'epz',P_lvopt,P_difN,epz,P_Vmax1,P_Kmabs1,P_Vmax2,P_Kmabs2
!write(245,*)cumoffrN,flurac,flusol,offrN


      NRAC = int(zrac)
      cumoffrN = 0.0
      flurac = 0.0
      flusol = 0.0

      offrN(:) = 0.


!   dr et nb le 17/11/05
!***************************************
!  on s'est rendu compte qu'on avait un pb :
!  il faudrait sortir tout ce qui concerne le calcul de l'offre cumoffrn dans un sp
!  et ne l'appeler que 1 fois pour chaque plante sinon on refait le calcul avec un
!  profil remis à jour (juste apres) donc on fait le calcul de cumoffrn avec 
!    2 profils differents
!**************************************


  
! *-------------------------------* c
! * Forçage des besoins en azote  * c
! * T. Nesme                      * c
! * le 04/07/02                   * c
! *-------------------------------* c
!   domi  01/12/05 je le transfere dans majprofilN 
!   puisque demande et masecpartiel ne servent pas ici

      ! *-----------------------------------------------------------------* c
      ! * 1. Offre du sol (flux d'arrivée à la racine, flux d'absorption) * c
      ! *-----------------------------------------------------------------* c
      ! * les calculs sont faits à partir de la surface du sol mais comme il 
      ! *  n'y a pas de racines de la surface à la profondeur de semis , cette
      ! *  tranche de sol est inoperante sur l'absorption d'azote
      !
      do iz = 1,Nrac
        ! concentration en azote (kg.ha-1.mm-1)
        concN = (nit(iz) + amm(iz)) / hur(iz)

        ! longueur racinaire efficace
        lrac = flrac(iz) * P_lvopt

        ! ** test d'arrêt d'absorption à partir d'un stade NB le 25/05/2000
        !-- if (ndrp > 0) lrac = absodrp*lrac
                 
        ! ** flux de nitrate sol --> racine (kg.ha-1.jour-1)
        ! *- 1) flux diffusif             
        rh = (hur(iz) - humin(iz))
        rh = rh / (hucc(iz) - humin(iz))
        if (rh < 0.) rh = 0.

        !--diff = P_difN * rh * sqrt(lrac)
        !--diff = diff * 70.9 * concN

        diff = P_difN * rh

      ! *********************************************************************
      ! ** DR et Bruno Mary le 051208
      !  version modifiée (equation 8.35 du bouquin)
        diff = diff * 7.09 * (nit(iz) + amm(iz)) * sqrt(lrac)
      !  ancienne version
      !--diff = diff * 70.9 * concN * sqrt(lrac)
      ! *********************************************************************
      ! unités :  P_difN: cm2.jour-1 ; lrac: cm.cm-3 ; concN: kg.ha-1.mm-1

        
        !: 2) flux convectif 
        conv = epz(iz) * concN    ! cumul epz => epzC => epz(0)

        ! unités :  epz(iz): mm.jour-1 ; concN: kg.ha-1.mm-1

        !: 3) flux total              
        fluxsol = conv + diff


        ! Ce flux peut être limité par le stock N disponible                       
        fluxsol = min(fluxsol,(nit(iz)+amm(iz)))


        !: Flux "actif" d'absorption racinaire
        !- passage de kg.ha-1.mm-1 à µmol.l-1
        concN = concN / 140. * 1e6         
  
        !: Hypothèse : même cinétique MM pour NH4 et NO3      
        Vabs = P_Vmax1 / (P_Kmabs1 + concN)
        Vabs = Vabs + P_Vmax2 / (P_Kmabs2 + concN)


        fluxrac = Vabs * concN * lrac
        !: Passage de µmol.cm-2.h-1 à kg.ha-1.j-1     
        fluxrac = fluxrac * 33.6


          
        !: L'absorption est limitée par le plus petit des 2 flux
        offrN(iz) =  min(fluxsol, fluxrac)

        cumoffrN = cumoffrN + offrN(iz)
        ! 06/01/06 flusol n'est qu'une variable de sortie aidant à l'interpretation 
        ! des flux d'azote à l'entree de la plante
        if (fluxsol  <  fluxrac) then
          flusol = flusol + fluxsol
        endif
        
        if (fluxsol  >=   fluxrac) then
          flurac = flurac + fluxrac
        endif
      end do


! domi 01/12/2005 on sort la confrontation offre/demande et la
! maj des profils qu'on fera sur la partie ombre et soleil
! == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == =  = 

      return
      end
 
 
