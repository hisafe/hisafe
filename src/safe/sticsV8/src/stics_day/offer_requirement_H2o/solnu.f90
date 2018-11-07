! ***************************************************************** c
! *           calcul de l'évaporation du sol nu                   * c
! ***************************************************************** c
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> evaporation of the soil

!! This module calculates the actual evaporation of the soil (esol), and the distribution in the soil profile.
!> - Stics book paragraphe 7.1.2, 7.1.3, page 128-129
!>
!! The calculation of actual evaporation relies on a semi-empirical model fully developed and justified in Brisson and Perrier (1991). Following a rain event,
!! soil evaporation is assumed to follow two successive phases, as in Ritchie’s (1972) approach improved by Boesten and Stroosnijder (1986).
!! During the first phase evaporation is potential until the accumulation of daily evaporation reaches the q0 threshold.
!! During the second phase evaporation decreases and this decrease depends on the weather and soil type, through parameter aevap (calculated in the module init_sol.f90).
!! The parameter q0 depends on the soil texture and structure: it is difficult to infer it from soil particle size distribution or bulk density.
!! It generally varies between 0 to 30 mm.
!!
!! This module also provides an estimate of the thickness of the dry layer in the surface (or natural mulch : xmlch1 and xmlch2) which is taken into account
!! in the water profile in the soil, in the sense that this layer is supposed not to participate in evaporation.
!!
!! The method of calculating the distribution of evaporation resembles that of the LIXIM model (Mary et al., 1999). The daily evaporation value esol,
!! calculated above, is assumed to affect the layers of soil up from the base of the natural mulch xmlch1-xmlch2 (if present) to a maximum depth of zesx.
!! Below this depth, there is no evaporation.  The contribution of each basic soil layer to evaporation esz decreases with depth.
!! cfes is a slope coefficient. By varying parameters zesx and cfes, it is possible to take account of differences in hydraulic conductivity from one soil to another.
!! A very high surface moisture gradient during soil drying is correctly represented by a high cfes value.  The sensitivity of the soil evaporation depth partitioning
!! If nothing is known about the soil one can use the standard values proposed: cfes=5 and zesx=60 cm.
! ------------------------------------------------------------------------------------------------------------------------------------------------------------* c
!subroutine solnu(n,nbCouches,precip,P_codebeso,P_codelaitr,tetp,delta,laiTot,tauxcouv,  &
subroutine solnu(n,nbCouches,precip,  &
                 nitetcult,P_q0,aevap,P_cfes,P_codeactimulch,ha,hi,hpf,hurlim,P_zesx,hur,hucc,          &
                 eos,sumes00,sumes10,sumes20,supres0,ses2j00,sesj00,smes020,stoc0,nstoc0,       &
                 sumes0,sumes1,sumes2,supres,ses2j0,sesj0,smes02,stoc,nstoc,sum2,esz,esol,      &
                 esreste,xmlch1,xmlch2)

USE Messages

  implicit none

!: Arguments

  integer, intent(IN)    :: n  
  integer, intent(IN)    :: nbCouches  
  real,    intent(IN)    :: precip      !> // OUTPUT // Daily amount of water (precipitation + irrigation)   // mm day-1
  real,    intent(IN)    :: nitetcult                     ! (n)       // OUTPUT // Number of iterations to calculate TCULT // SD
  real,    intent(IN)    :: P_q0  !> // PARAMETER // Parameter of the end of the maximum evaporation stage  // mm // PARSOL // 1 
  real,    intent(IN)    :: aevap  
  real,    intent(IN)    :: P_cfes  !> // PARAMETER // parameter defining the soil contribution to evaporation as a function of depth  // SD // PARSOL // 1 
  integer, intent(IN)    :: P_codeactimulch  !> // PARAMETER // activation of the accounting for natural mulching in the partitioning of soil evaporation within the soil profile // code 1/2 // PARAM // 0 
  real,    intent(IN)    :: ha  
  real,    intent(IN)    :: hi  
  real,    intent(IN)    :: hpf  
  real,    intent(IN)    :: hurlim  
  real,    intent(IN)    :: P_zesx  !> // PARAMETER // maximal depth of soil affected by soil evaporation // cm // PARSOL // 1 
  real,    intent(IN)    :: hur(nbCouches)  
  real,    intent(IN)    :: hucc(nbCouches)  



  real,    intent(INOUT) :: eos      !> // OUTPUT // Maximum evaporation flux  // mm
  real,    intent(INOUT) :: sumes00           ! spécifique solnu, peut être argument SAVE plutot que mettre en sortie ?  
  real,    intent(INOUT) :: sumes10           ! spécifique solnu, peut être argument SAVE plutot que mettre en sortie ?  
  real,    intent(INOUT) :: sumes20           ! spécifique solnu, peut être argument SAVE plutot que mettre en sortie ?  
  real,    intent(INOUT) :: supres0           ! spécifique solnu, peut être argument SAVE plutot que mettre en sortie ?  
  real,    intent(INOUT) :: ses2j00           ! spécifique solnu, peut être argument SAVE plutot que mettre en sortie ?  
  real,    intent(INOUT) :: sesj00            ! spécifique solnu, peut être argument SAVE plutot que mettre en sortie ?  
  real,    intent(INOUT) :: smes020           ! spécifique solnu, peut être argument SAVE plutot que mettre en sortie ?  
  real,    intent(INOUT) :: stoc0             ! spécifique solnu, peut être argument SAVE plutot que mettre en sortie ?  
  integer, intent(INOUT) :: nstoc0            ! spécifique solnu, peut être argument SAVE plutot que mettre en sortie ?  

  real,    intent(INOUT) :: sumes0  
  real,    intent(INOUT) :: sumes1  
  real,    intent(INOUT) :: sumes2  
  real,    intent(INOUT) :: supres  
  real,    intent(INOUT) :: ses2j0  
  real,    intent(INOUT) :: sesj0  
  real,    intent(INOUT) :: smes02  
  real,    intent(INOUT) :: stoc  
  integer, intent(INOUT) :: nstoc  
  real,    intent(INOUT) :: sum2  
  real,    intent(INOUT) :: esz(nbCouches)  
  real,    intent(INOUT) :: esol      !> // OUTPUT // Actual soil evaporation flux  // mm day-1
  real,    intent(INOUT) :: esreste  
  real,    intent(INOUT) :: xmlch1      !> // OUTPUT // Thickness of mulch created by evaporation from the soil // cm
  real,    intent(INOUT) :: xmlch2  


!: Variables locales
  integer :: codesolnu  !>  
  integer :: i  !>  
  integer :: iz  !>  
  integer :: z  !>  
  integer :: izx  !>  
  integer :: izbase  !>  
  integer :: nbres  
  real    :: cumules  !>  
  real    :: dispomlch  !>  
  real    :: dz  !>  
  real    :: precipsol  
  real    :: rol  !>  
  real    :: sumrepares  !>  
  real    :: Wi  !>  
  real    :: xmstoc  
  real    :: repares(nbCouches)  

!: Fonction(s)
  real :: fper  !>  
  real :: finv  



     !if (n.eq.1)write(618,*)n,nbCouches,precip,P_codebeso,P_codelaitr,tetp
      !if (n.eq.1)write(618,*)delta
       !if (n.eq.1)write(618,*)laiTot
!       !if (n.eq.1)write(618,*)couvermulch
       !if (n.eq.1)write(618,*)tauxcouv
       !if (n.eq.1)write(618,*)nitetcult
       !if (n.eq.1)write(618,*)P_q0
       !if (n.eq.1)write(618,*)aevap
       !if (n.eq.1)write(618,*)'P_cfes',P_cfes
       !if (n.eq.1)write(618,*)P_codeactimulch
       !if (n.eq.1)write(618,*)ha
       !if (n.eq.1)write(618,*)hi
       !if (n.eq.1)write(618,*)'hpf',hpf
       !if (n.eq.1)write(618,*)hurlim
       !if (n.eq.1)write(618,*)P_zesx
       !if (n.eq.1)write(618,*)'hur',hur
       !if (n.eq.1)write(618,*)hucc
       !if (n.eq.1)write(618,*)eos
       !if (n.eq.1)write(618,*)'sumes00',sumes00
       !if (n.eq.1)write(618,*)sumes10
       !if (n.eq.1)write(618,*)sumes20
       !if (n.eq.1)write(618,*)supres0
       !if (n.eq.1)write(618,*)'ses2j00',ses2j00
       !if (n.eq.1)write(618,*)'sesj00',sesj00
       !if (n.eq.1)write(618,*)smes020
       !if (n.eq.1)write(618,*)stoc0
       !if (n.eq.1)write(618,*)nstoc0
       !if (n.eq.1)write(618,*)'sumes0',sumes0
       !if (n.eq.1)write(618,*)sumes1
       !if (n.eq.1)write(618,*)sumes2
       !if (n.eq.1)write(618,*)supres
       !if (n.eq.1)write(618,*)ses2j0
       !if (n.eq.1)write(618,*)sesj0
       !if (n.eq.1)write(618,*)smes02
       !if (n.eq.1)write(618,*)stoc
       !if (n.eq.1)write(618,*)nstoc
       !if (n.eq.1)write(618,*)    sum2
       !if (n.eq.1)write(618,*)esz
       !if (n.eq.1)write(618,*)esol
       !if (n.eq.1)write(618,*)esreste
       !if (n.eq.1)write(618,*)xmlch1
       !if (n.eq.1)write(618,*)xmlch2

! write(618,*)'precip',n,precip

      precipsol = precip
      rol = 9.45



      codesolnu = 1
      if (codesolnu == 2) then

! ** calcul alternatif de l'évaporation réelle par modèle A. Chanzy
! *-
! *- variables d'état
! *- hur(iz) =  humidité volumique de la couche centimétrique iz en mm cm-1
! *- dacouche(iz) = densité apparente de la couche centimétrique iz
! *- eos = demande climatique appliquée au sol (mm)
! *- esol = variable de sortie évaporation en mm
! *- paramètres


! A TOI DE JOUER !....

      else

        !: Calcul de l'évaporation réelle
        !- NB le 13/06/02 affectation des cumuls en cas de
        !- calcul itératif de rnet (passages multiples dans solnu)
        if (nitetcult == 0) then
          sumes00 = sumes0
          sumes10 = sumes1
          sumes20 = sumes2
          supres0 = supres
          ses2j00 = ses2j0
          sesj00  = sesj0
          smes020 = smes02
          stoc0   = stoc
          nstoc0  = nstoc
        else
          sumes0 = sumes00
          sumes1 = sumes10
          sumes2 = sumes20
          supres = supres0
          ses2j0 = ses2j00
          sesj0 = sesj00
          smes02 = smes020
          stoc = stoc0
          nstoc = nstoc0
        endif
!       write(618,*)'sumes1 debut',sumes1

        !: Cumul du déssèchement des deux réservoirs superficiels
        sum2 = sumes2 + ses2j0

        !- phase I
        if (sumes1 >= P_q0 .and. precipsol >= sum2) goto 1000

        !- phase II
        if (sumes1 >= P_q0 .and. precipsol < sum2) goto 1100

        !- phase I(II?)
        if (precipsol >= sumes1) goto 1400
!            write(618,*)'precipsol',sumes1,precipsol

        sumes1 = sumes1 - precipsol
        goto 1500


! --------------- c
! *   phase I   * c
! --------------- c
1000    continue
!        write(618,*) '1000'
        if (precipsol < sumes2) goto 1100

        sumes1 = P_q0-(precipsol-sumes2)
        sumes0 =  0.
        sumes2 =  0.
        !write(*,*) sumes1, sumes0, sumes2
        if (precipsol > P_q0) goto 1400
        goto 1500


! ----------------
! *   phase II   *
! ----------------

      ! a) poursuite du dessèchement sans réservoir secondaire (nstoc = 0)
1100    continue
!        write(618,*) '1100'
        if (precipsol <= 0 .and. nstoc == 0) then
          sumes0 =  sumes0+eos
          esol  = fper(sumes0,aevap)-sumes2
          sumes2 =  fper(sumes0,aevap)
          goto 1700
        endif

      ! b) phase II avec réservoir secondaire (nstoc>0)
        if (precipsol > 0 .or. nstoc > 0) then
          stoc = stoc+precipsol
          if (nstoc == 0) then
          ! initialisation des variables pour le second réservoir
            sesj0 = sumes0
            ses2j0 = sumes2
          endif
          if (precipsol > 0.) then
          ! remise à zéro des variables après une pluie faible
            sumes2 = 0.
            smes02 = 0.
          endif
          if (nstoc > 0 .and. stoc >= ses2j0) then
          ! l'apport est suffisamment important pour revenir en phase I
            precipsol = stoc-ses2j0
            ses2j0 = 0.
            sumes2 = 0.
            nstoc = 0
            stoc = 0.
            smes02 = 0.
            goto 1000
          endif
          smes02 = smes02+eos
          esol   = fper(smes02,aevap)-sumes2
          sumes2 =  fper(smes02,aevap)
          if (sumes2 < stoc) then
          ! création d'un réservoir secondaire
            nstoc = nstoc+1
          else
          ! annulation du réservoir secondaire
            nstoc  = 0
            stoc   = 0.
            smes02 = 0.
            sumes0 = sesj0+finv(sumes2-stoc,aevap)
            sumes2 = fper(sumes0,aevap)
            ses2j0 = 0.
          endif
        endif
        goto 1700


! ----------------------- c
! *       phase I       * c
! ----------------------- c
1400    continue
!        write(618,*) '1400'
        sumes1 = 0.

1500    continue
!        write(618,*) '1500',sumes1
        sumes1 = sumes1 + eos
!      write(618,*) n,'sumes1',sumes1, eos, P_q0

        if (sumes1 > P_q0) goto 1600
        esol = eos

!      write(618,*)n,'P_q0',esol,P_q0
        goto 1700

! ------------------------- c
! *   passage phase II    * c
! ------------------------- c
1600    continue
        !write(*,*) '1600'
        sumes0 = sumes1-P_q0
        sumes2 = fper(sumes0,aevap)
        esol = eos - (sumes0 - sumes2)

!        write(618,*)'1600',n,esol,eos,sumes0,sumes2
1700    continue
        !write(*,*) '1700'

      ! calcul de l'épaisseur du mulch
        if (nstoc == 0) then
          nbres = 1
        else
          nbres = 2
        endif
        xmlch2 = 0.
        if (nbres == 1) xmlch1 = sumes2/(rol*(hi-ha))
        if (nbres == 2) then
          xmlch1 = ses2j0/(rol*(hi-ha))
          xmlch2 = sumes2/(rol*(hi-ha))
        endif
      ! calcul d'une reserve supplementaire liée a la presence des mulchs
        xmstoc = stoc/(rol*(hi-ha))
        supres = (xmlch1-(xmstoc-xmlch2))*(hpf-ha)*10.
! *- sb le 02/03/07 : pourquoi donc cette perte de précision ?
!        esol = aint(esol*1000)/1000.

! *- fin alternative calcul esol
      endif



  !: Evaporation répartie dans le mulch jusqu'à dessèchement hurlim
  !- puis avec une fonction linéaire jusqu'à une profondeur fixée P_zesx

      ! init. à zéro
      esz(:) = 0.
      repares(:) = 0.
      dispomlch = 0.

      ! DR 15/05/06 on a pn ob xmlch1 dejante et va jusqu'a 5000
      ! car sumes à des valeurs astronomiques
      ! pour depanner celia on prend l'option de le borner à P_zesx
      ! on voit ce qui se passe
      if (xmlch1+1 > P_zesx) xmlch1 = P_zesx-1

      do iz = 1, ifix(xmlch1+1)
        dispomlch = dispomlch+hur(iz)-hurlim
      end do

      !: On peut ne pas tenir compte du mulch
      if (P_codeactimulch == 2) xmlch1 = 0.

      ! 17/06/04 - EMMA bug sur reservoir mulch
      if (xmlch1 <= 0.) then
        dispomlch = 0.
        supres = 0.
        xmlch1 = 0.
      endif

      !: Si la quantité d'eau dans le mulch ne suffit pas à satisfaire l'évaporation
      esreste = esol

!  write(618,*)'1*',n,iz,esreste,esol
      if (xmlch1 > 0.) then
        do iz = 1,int(xmlch1)+1
          if (esol > dispomlch .and. dispomlch > 0.) then
            esz(iz) = hur(iz) - hurlim
          else
            esz(iz) = min(esreste, hur(iz)-hurlim)

          endif
          esreste = max(esreste-esz(iz), 0.)


!   write(618,*)'2*',n,iz,esreste,esz(iz)
        end do
      endif
      ! répartition sous le mulch
      if (esreste > 0.) then
        izbase = int(xmlch1)+2
        if (xmlch1 == 0.) izbase = 1
        sumrepares = 0.

        izx = int(P_zesx)
        dz = izx-izbase+1.
        do iz = izbase,izx
          z = iz-izbase
          repares(iz) = (1.-z/dz)**(abs(P_cfes))
          if (P_cfes > 0.) then
            Wi = (hur(iz)-hurlim)/(hucc(iz)-hurlim)
!  write(618,'(2i3,3f16.12)')n,iz,Wi,repares(iz),hur(iz)

            repares(iz) = Wi*repares(iz)
          endif
          sumrepares = sumrepares+repares(iz)
        end do

!**
        if (sumrepares <= 0.) then
          call EnvoyerMsgHistorique(163)
!          call EnvoyerMsgHistorique(164)
        ! NB - le 02/07/02
        ! si on ne peut pas dessecher le sol au delà de P_zesx alors l'évaporation est nulle
          sumrepares = 1.0
          esol = 0.0
!        write(618,*)'sumrepares',n,esol
! --       stop
        endif
!       write(618,*)n,esreste,sumrepares,hurlim

      ! 17/06/04 - les 3 - (emma) recalcul de l'evaporation
        do iz = izbase,izx
          esz(iz)  = min(esreste*repares(iz)/sumrepares, max(hur(iz)-hurlim, 0.))

          if (esz(iz) < 0.) esz(iz) = 0.

        end do
      endif

    ! cumul de esz
      cumules = 0.
      do i = 1,int(P_zesx)
        cumules = cumules + esz(i)
      end do
      esol = cumules
!    write(618 ,*)n,'cumules',esol,cumules
! DR_2010      write(*,*)n,esol,hur
return
end subroutine solnu


! ******************************************* c
! -    calcul de l'évaporation en phase II  - c
! ******************************************* c
real function fper(x,a)

!: Arguments
  real, intent(IN) :: x  
  real, intent(IN) :: a  

        fper = sqrt(a**2+2.*a*x)-a

return
end function fper


! ******************************************* c
! -       fonction inverse de fper          - c
! ******************************************* c
real function finv(x,a)

!: Arguments
  real, intent(IN) :: x  
  real, intent(IN) :: a  

        finv = ((x+a)**2-a**2)/(2.*a)

return
end function finv
 
 
