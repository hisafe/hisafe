! = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == =
! DR le 28/10/05 on sort le calucul de l'azote grain et fruit qui etaient dans
!    grain.for et fruit.for
! ** calcul de l'azote dans les grains
! = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == = == =  =
! domi 27/10/05 on deplace ca dans stressN sinon on a pas la valuer de qnplante qui est calculée dans stressn
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module calculates the biochemical composition of the harvested organ
!> - Stics book paragraphe 4.3.2, page 48-49
!>
!! The quantity of nitrogen in harvested organs, both for determinate and indeterminate species (QNgrain), is an increasing proportion (irazo) of the
!! quantity of nitrogen in the biomass (QNplante): the concept of the harvest index is extended to nitrogen, using the parameter vitirazo.
!!
!! Obviously, as for carbon, the grain/fruit nitrogen filling can be affected by thermal stress which requires a daily calculation. The temperature effect
!! on nitrogen grain filling is assumed to be the same as for carbon. The nitrogen harvest index is assumed to be limited to a value calculated using the
!! carbon parameters (irmax and viticarb).
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine Ngrain(n,ndrp,nrec,P_vitirazo,irazo_veille,nmat,dltags,QNplante_veille,    &
                  QNplante,pgrain,nbgraingel,P_irmax,P_vitircarb,magrain,               &
                  irazo,QNgrain,CNgrain)

  implicit none

!: Arguments

  integer, intent(IN)    :: n  
  integer, intent(IN)    :: ndrp  
  integer, intent(IN)    :: nrec  
  real,    intent(IN)    :: P_vitirazo  !> // PARAMETER // Rate of increase of the nitrogen harvest index // g grain g plant -1 day-1 // PARPLT // 1 
  real,    intent(IN)    :: irazo_veille  
  integer, intent(IN)    :: nmat  
  real,    intent(IN)    :: dltags      !> // OUTPUT // Growth rate of the grains  // t ha-1.j-1
  real,    intent(IN)    :: QNplante_veille  
  real,    intent(IN)    :: QNplante      !> // OUTPUT // Amount of nitrogen taken up by the plant  // kgN.ha-1
  real,    intent(IN)    :: pgrain  
  real,    intent(IN)    :: nbgraingel  
  real,    intent(IN)    :: P_irmax  !> // PARAMETER // Maximum harvest index // SD // PARPLT // 1 
  real,    intent(IN)    :: P_vitircarb  !> // PARAMETER // Rate of increase of the carbon harvest index // g grain g plant -1 day-1 // PARPLT // 1 
  real,    intent(IN)    :: magrain  

  real,    intent(INOUT) :: irazo      !> // OUTPUT // Nitrogen harvest index  // gN grain gN plant-1
  real,    intent(INOUT) :: QNgrain      !> // OUTPUT // Amount of nitrogen in harvested organs (grains / fruits) // kg ha-1
  real,    intent(INOUT) :: CNgrain      !> // OUTPUT // Nitrogen concentration of grains  // %

!: Variables locales
  real :: dltazo  


      if (ndrp == 0 .or. (nrec > 0 .and. n > nrec)) then
        irazo = 0.0
        return
      endif

      if (n > ndrp) then



        ! ** domi 25/10/05
        ! domi 08/09/05 on reactive pour un test pour Martine
        irazo = P_vitirazo * (n - ndrp + 1)

        ! NB le 2/09/05
        ! ** vitesse de remplissage du grain  en azote limitée par protlim
        if (nmat == 0 .or. n == nmat) then




        if (dltags > 0.0) then
          ! domi 25/10/2005 je fais les modifs comme dans la version vianney
          ! en incluant la mise en tableau de qnplante et irazo
          ! domi 29/10/05 on a pas encore la valeur de qnplante(n) donc on decale le
          ! delta de 1 jour
          !-- dltazo = P_vitirazo*QNplante(n-1)
          dltazo = irazo        * QNplante              &
                 - irazo_veille * QNplante_veille

          !-- protgraindyn = dltazo/(dltags*1000.0)*5.7*100.0
        else
          dltazo = 0.0
          !-- protgraindyn = 0.0
        endif

        ! essai de limitation de l'accumulation de N par rapport à C dans le grain
        !-- if (protgraindyn > protlim) then
        !--     dltazo = protlim*(dltags*1000.0)/5.7/100.0
        !-- endif

        ! nb le 14022006 on eleve l'azote des grains geles
        !-- QNgrain = QNgrain+dltazo
        !-- QNgrain = QNgrain+dltazo-(pgraingel*cngrain*0.1)
        QNgrain = QNgrain + dltazo - ((pgrain * nbgraingel) * CNgrain * 0.1)

        irazo = QNgrain / QNplante


        if ( irazo > P_irmax / P_vitircarb * P_vitirazo) then
          irazo = P_irmax / P_vitircarb * P_vitirazo
          QNgrain  = QNplante * irazo
        endif

          ! ** CNgrain en %
          ! *- utilisation de masecpartiel pour cohérence NB le 03/04/98
          ! --       CNgrain =  QNgrain / (masecpartiel*ircarb(n)*10.0)
          ! *- changement car mauvais calcul de CNgrai NB le 27/07/02
          if (magrain > 0.) then
            CNgrain =  QNgrain / magrain * 10
          endif
        endif

      endif


return
end subroutine Ngrain
 
 
