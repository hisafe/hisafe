subroutine ResiduParamDec(awb,bwb,cwb,CroCo,akres,bkres,ahres,bhres,Wr,CNresmin,CNresmax,Wb,kres,hres)
! *---------------------------------------------------------------------------
!>
!! This program updates the decomposition parameters of an organic residue
!> Stics book  page 265-287
! *---------------------------------------------------------------------------
  implicit none

  real,    intent(IN)    :: awb   !> // PARAMETER // decomposition parameter : CsurNbio=awb+bwb.Wr // SD // PARAM // 1
  real,    intent(IN)    :: bwb   !> // PARAMETER // decomposition parameter : CsurNbio=awb+bwb.Wr // g g-1 // PARAM // 1
  real,    intent(IN)    :: cwb   !> // PARAMETER // decomposition parameter : CsurNbio=awb+bwb.Wr // g g-1 // PARAM // 1
  real,    intent(IN)    :: CroCo !> // PARAMETER // decomposition parameter  // SD // PARAM // 1
  real,    intent(IN)    :: akres !> // PARAMETER // decomposition parameter : kres=akres+bkres.Wr // d-1 // PARAM // 1
  real,    intent(IN)    :: bkres !> // PARAMETER // decomposition parameter : kres=akres+bkres.Wr // g g-1 // PARAM // 1
  real,    intent(IN)    :: ahres !> // PARAMETER // decomposition parameter : hres=1-ahres/(bhres.Wr+1) // g g-1 // PARAM // 1
  real,    intent(IN)    :: bhres !> // PARAMETER // decomposition parameter : hres=1-ahres/(bhres.Wr+1) // g g-1 // PARAM // 1
  real,    intent(IN)    :: Wr    ! N/C of the residue    g.g-1 IN
  ! EC et BM 04/10/2012: modifs des param de decomposition pour le mulch
  real,    intent(IN)    :: CNresmin  !> // PARAMETER // minimum observed value of ratio C/N of residue ires // g g-1 // PARAM // 1
  real,    intent(IN)    :: CNresmax  !> // PARAMETER // maximum observed value of ratio C/N of residue ires // g g-1 // PARAM // 1
  real,    intent(INOUT) :: Wb    ! N/C of the biomass    g.g-1 INOUT
  real,    intent(INOUT) :: kres  ! decomposition rate of the organic residue  d-1   INOUT
  real,    intent(INOUT) :: hres  ! humification rate of the organic residue   g.g-1 INOUT

! EC et BM 04/10/2012: modifs des param de decomposition pour le mulch
  !: VARIABLES LOCALES
  real    :: CsurNbio  
  real    :: NsurCres

       NsurCres = Wr
       if(CNresmin > 0.) NsurCres = min(NsurCres, 1. / CNresmin)
       if(CNresmax > 0.) NsurCres = max(NsurCres, 1. / CNresmax)

       CsurNbio   =  max(awb+bwb*NsurCres,cwb)
       CsurNbio   =  min(CsurNbio,25.)
       if(CsurNbio > 0.) then
           Wb = 1./CsurNbio
       else
           Wb = 0.
       endif
       kres =  akres + bkres*NsurCres
       hres =  1. - ahres /(bhres*NsurCres + 1.)
return

end subroutine ResiduParamDec
