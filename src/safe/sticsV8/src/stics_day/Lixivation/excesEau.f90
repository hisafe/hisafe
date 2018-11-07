!****f* Lixivation/excesEau
! NAME
!   excesEau - 
! DESCRIPTION
!   Module : Lixivation
!   Typologie : scientifique
! 
!
! INPUTS 
!     nh
!     nhe
!     P_codemacropor
!     P_codefente
!     pluiefente
!     zrac
!     macropor
!     P_epc
!     hucc
!
! OUTPUTS
!     bouchon
!     hur
!     exces
!
!
!
!***
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!>
!> - Stics book paragraphe 9.2.4, page 173
!>
!! This module deals with the circulation of soil water through the cracks, and the upward circulation into macroporosity.
!! In the case of swelling soils, the fissures, when open, are filled by overflow from the surface layer; water supply by rain interception at the surface is
!! not taken into consideration.  The opening of cracks (bouchon variable) depends on the combination of two factors in at least one of the layers:
!! empty macroporosity and a root front deeper than the base of the layer
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine excesEau(nh,nhe,P_codemacropor,P_codefente,pluiefente,zrac,macropor,P_epc,hur,hucc,bouchon,exces)

! On utilise le module Sorties pour connaitre les interfaces d'appels des routines et fonctions suivantes
USE Messages, only: EnvoyerMsgHistorique

  integer, intent(IN)    :: nh  
  integer, intent(IN)    :: nhe  
  integer, intent(IN)    :: P_codemacropor  !> // PARAMETER // "Simulation option of water flux in the macroporosity of soils to estimate water excess and drip by  overflowing : yes(1), no (0)" // code 1/2 // PARSOL // 0
  integer, intent(IN)    :: P_codefente  !> // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0 
  real,    intent(IN)    :: pluiefente  
  real,    intent(IN)    :: zrac   !> // OUTPUT // Depth reached by root system // cm
  real,    intent(IN)    :: P_epc(nh)  !< // PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm
  real,    intent(IN)    :: hucc(nhe)  
  real,    intent(IN)    :: macropor(nh)  
  
  integer, intent(OUT)   :: bouchon   !> // OUTPUT // Index showing if the shrinkage slots are opened (0) or closed (1)  // 0-1
  real,    intent(OUT)   :: hur(nhe)  
  real,    intent(OUT)   :: exces(0:5)   !> // OUTPUT // Amount of water  present in the macroporosity of the horizon 1  // mm

! VARIABLES LOCALES
  integer :: profhoriz  !>  
  integer ::  ic  !>  
  integer ::  ii  !>  
  integer ::  iz  
  real :: remonte  !>  
  real ::  l_macropor  


      exces(nh) = exces(nh)+pluiefente
      !write(670,*) '1. ',exces(nh),nh,pluiefente
      profhoriz = nhe
      bouchon = 1 
      exces(0) = 0.
      
      do ic = nh,1,-1
      !  write(670,*) '1.1 ',macropor(ic)
      ! test sur macropor
        if (macropor(ic) <= 2.) then
          if (P_codemacropor == 1) then
            call EnvoyerMsgHistorique(153, 0.)
            call EnvoyerMsgHistorique(154, 0.)
            ! 02/07/2013 je corrige le message qui n'etait pas bon et j'en rajoute un pour codefente=1 (non actif)
            if(P_codefente == 2 )then
                call EnvoyerMsgHistorique(155, 0.)
                call EnvoyerMsgHistorique(256, 0.)
                !stop
                call exit(9)
            else
                call EnvoyerMsgHistorique(255, 0.)  ! ne doit jamais arriver
                call EnvoyerMsgHistorique(256, 0.)
                !stop
                call exit(9)
            endif
          else
             l_macropor = 0.
          endif
        else
          l_macropor = macropor(ic)
        endif
      
      !* exces(ic) alimente la microporosité de l'horizon ic si celui-ci 
      !- s'est vidé par absorption d'eau
        do ii = 1,int(P_epc(ic))
          iz = profhoriz-ii+1
          if (hur(iz)+exces(ic) > hucc(iz)) then
      !      write(670,*) '2. ',exces(ic),ic,iz,hucc(iz),hur(iz)
            exces(ic) = exces(ic)-(hucc(iz)-hur(iz))
            hur(iz) = hucc(iz)
          endif
        end do
      
      !* ouverture des fissures
        if (exces(ic) <= 0 .and. zrac >= profhoriz .and. P_codefente == 1) bouchon = 0
      
        if (exces(ic) > l_macropor) then
          remonte = exces(ic) - l_macropor
      !    write(670,*) '2.1 ',remonte,exces(ic),ic,l_macropor
          if(exces(ic-1) <= 0. .and. ic /= 1) then
      ! réalimentation de la microporosité de l'horizon si celui-ci est en dessous de la CC
            do ii = 1,int(P_epc(ic-1))
              iz = profhoriz-int(P_epc(ic))-ii+1
              if (hur(iz)+remonte >= hucc(iz)) then
                remonte = remonte-(hucc(iz)-hur(iz))
                hur(iz) = hucc(iz)
              endif
            end do
          endif
      
      !    write(670,*) '3. ',exces(ic-1),ic,remonte
          exces(ic-1) = exces(ic-1)+remonte
          remonte = 0.
      !* l'eau ne peut plus passer entre ic-1 et ic
          exces(ic) = l_macropor
        endif
        profhoriz = profhoriz-int(P_epc(ic))
      
      end do

end subroutine excesEau 
 
