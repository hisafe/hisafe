subroutine ResidusParamDecMelange(nbCouches,nbResidus,awb,bwb,cwb,CroCo,akres,bkres,ahres,bhres,Cres,Nres,profhum,  & ! IN
                                  CNresmin,CNresmax, Wb,kres,hres)                                                    ! INOUT
! --------------------------------------------------------------------------------------------------------
!! This program updates the decomposition parameters of organic residue
!!      following addition of organic residue and soil tillage
!> Stics book  page 265-287
! *-------------------------------------------------------------------------------------------------------
  implicit none

  integer, intent(IN)    :: nbCouches
  integer, intent(IN)    :: nbResidus
  real,    intent(IN)    :: awb(nbResidus)     !> // PARAMETER // decomposition parameter : CsurNbio=awb+bwb.Wr // SD // PARAM // 1
  real,    intent(IN)    :: bwb(nbResidus)     !> // PARAMETER // decomposition parameter : CsurNbio=awb+bwb.Wr // SD // PARAM // 1
  real,    intent(IN)    :: cwb(nbResidus)     !> // PARAMETER // decomposition parameter : CsurNbio=awb+bwb.Wr // SD // PARAM // 1
  real,    intent(IN)    :: CroCo(nbResidus)   !> // PARAMETER // decomposition parameter  // SD // PARAM // 1
  real,    intent(IN)    :: akres(nbResidus)   !> // PARAMETER // decomposition parameter : kres=akres+bkres.Wr // d-1 // PARAM // 1
  real,    intent(IN)    :: bkres(nbResidus)   !> // PARAMETER // decomposition parameter : kres=akres+bkres.Wr // d-1 // PARAM // 1
  real,    intent(IN)    :: ahres(nbResidus)   !> // PARAMETER // decomposition parameter : hres=1-ahres/(bhres.Wr+1) // g g-1 // PARAM // 1
  real,    intent(IN)    :: bhres(nbResidus)   !> // PARAMETER // decomposition parameter : hres=1-ahres/(bhres.Wr+1) // g g-1 // PARAM // 1
  real,    intent(IN)    :: Cres(nbCouches,nbResidus)
  real,    intent(IN)    :: Nres(nbCouches,nbResidus)
  real,    intent(IN)    :: profhum            !> // PARAMETER // Humification depth (max.60) // cm // PARSOL // 1
! EC et BM 04/10/2012: modifs des param de decomposition pour le mulch
  real,    intent(IN)    :: CNresmin(nbResidus)  !> // PARAMETER // minimum observed value of ratio C/N of all residues // g g-1 // PARAM // 1
  real,    intent(IN)    :: CNresmax(nbResidus)  !> // PARAMETER // maximum observed value of ratio C/N of all residues // g g-1 // PARAM // 1
  real,    intent(INOUT) :: Wb(nbResidus)      !> N/C of the biomass
  real,    intent(INOUT) :: kres(nbResidus)    !> decomposition rate of the organic residues
  real,    intent(INOUT) :: hres(nbResidus)    !> humification rate of the organic residues

  !: VARIABLES LOCALES
  integer :: iz  
  integer :: ir  
  integer :: izmax  
  real    :: Cresid  
  real    :: Nresid  
  real    :: CsurNbio
  real    :: Wr
  ! EC et BM 04/10/2012: modifs des param de decomposition pour le mulch
  real    :: NsurCres 

! Calcul de Wr (rapport N/C) moyen de chaque résidu sur la profondeur izmax
      do ir=1,nbResidus
          Cresid = 0.
          Nresid = 0.
          izmax = int(profhum)
          if(ir < 11) izmax=1
          do iz=1,izmax
            Cresid = Cresid + Cres(iz,ir)
            Nresid = Nresid + Nres(iz,ir)
          end do
          if (Cresid > 0.) then
            Wr = Nresid/Cresid
          else
            Wr = 0.
          endif

! Actualisation des paramètres de décomposition de chaque résidu en fonction de Wr
          NsurCres = Wr
          if(CNresmin(ir) > 0.) NsurCres = min(NsurCres, 1. / CNresmin(ir))
          if(CNresmax(ir) > 0.) NsurCres = max(NsurCres, 1. / CNresmax(ir))
   
          CsurNbio   =  max(awb(ir)+bwb(ir)*NsurCres,cwb(ir))
          CsurNbio   =  min(CsurNbio,25.)
          if(CsurNbio > 0.) then
            Wb(ir) = 1./CsurNbio
          else
            Wb(ir) = 0.
          endif
          kres(ir) =  akres(ir) + bkres(ir)*NsurCres
          hres(ir) =  1. - ahres(ir) /(bhres(ir)*NsurCres + 1.)
      end do
return

end subroutine ResidusParamDecMelange
 
 
