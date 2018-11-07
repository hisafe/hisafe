! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module calculates waterlogging and corresponding stress indices.
!> - Stics book paragraphe 3.4.5, page 65-66
!>
!! The waterlogging variable is called exofac and corresponds to the proportion of roots flooded, i.e. roots in saturated layers (anox=1).
!!
!! The calculation of stress indices is based on the experimental work by Rebière (1996), reviewed in Brisson et al. (2002b).
!! izrac is the root stress index which limits root growth at an efficient depth and density.  The index which affects the leaf area index is called exolai and
!! the index affecting radiation use efficiency and transpiration (stomatal effect) is called exobiom.
!!
!! If the species (or variety) develops resistance mechanisms (e.g. aerenchyma) the effects of excess water will be less pronounced, thanks to the
!! sensanox sensitivity parameter. If sensanox=1, the sensitivity is maximal and if sensanox=0, the plant is indifferent to excess water.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
subroutine excesdeau(n,cumflrac,rltot,P_sensanox,racnoy_veille,racnoy,exofac,izrac,idzrac,exolai,exobiom)

  implicit none

!: Arguments

  integer, intent(IN)    :: n  
  real,    intent(IN)    :: cumflrac  
  real,    intent(IN)    :: rltot      !> // OUTPUT // Total length of roots  // cm root.cm -2 soil
  real,    intent(IN)    :: P_sensanox  !> // PARAMETER // anoxia sensitivity (0=insensitive) // SD // PARPLT // 1 
  real,    intent(IN)    :: racnoy_veille  

  real,    intent(INOUT) :: racnoy  
  real,    intent(INOUT) :: exofac      !> // OUTPUT // Variable for excess water // 0-1
  real,    intent(INOUT) :: izrac      !> // OUTPUT // Index of excess water stress on roots  // 0-1
  real,    intent(INOUT) :: idzrac  
  real,    intent(INOUT) :: exolai      !> // OUTPUT // Index for excess water active on growth in biomass // 0-1
  real,    intent(INOUT) :: exobiom      !> // OUTPUT // Index of excess water active on surface growth // 0-1

      !: Calcul de l'indice d'excès d'eau (thèse B.R.) et Brisson et al., 2001
      if (cumflrac > 0.) then
        racnoy = racnoy / cumflrac
      else
        racnoy = 0.
      endif

      if (n == 1) then
        exofac = racnoy
      else
        exofac = (racnoy_veille + racnoy) / 2.
      endif

      !: Calcul des indices de stress exces d'eau
      !- sur les racines
      izrac = 1.60 * exp(-0.27 * exofac * 100.) - 0.60
      !- pas de régression possible
      izrac = max(izrac,0.)

      !: Calcul de l'indice de densité qui joue dans l'option
      !- densité vraie
      if (rltot > 0 .and. izrac < 1.) then
        idzrac = izrac / rltot * cumflrac
      else
        idzrac = 1.
      endif

      !: Sur le LAI
      exolai = exp(-0.055 * exofac * 100.)
      exolai = 1. - ((1. - exolai) * P_sensanox)

      !: Sur l'epsilonb
      if (exofac == 0.) then
        exobiom = 1.
      else
        exobiom = 1. - (1. / (1. + exp(0.14 * (28.25 - exofac *100.))))
      endif
      exobiom = 1.- ((1. - exobiom) * P_sensanox)

return
end subroutine excesdeau
 
 
