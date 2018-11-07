subroutine apports(sc,pg,t,c,soil,p,itk)

! APPORTS eau/fertilisants/residus

USE Stics
USE Plante
USE Itineraire_Technique
USE Sol
USE Climat
USE Parametres_Generaux

  implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc  
  type(Parametres_Generaux_), intent(IN)    :: pg  
  type(Plante_),              intent(INOUT) :: p(sc%P_nbplantes)   ! Toutes les plantes du système, pour somme LAI et ABSO
  type(ITK_),                 intent(INOUT) :: itk(sc%P_nbplantes)
  type(Sol_),                 intent(INOUT) :: soil  
  type(Climat_),              intent(IN)    :: c  
  type(Stics_Transit_),       intent(INOUT) :: t  

!  Precipitations & irrigations
   if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: before eauEntrantSysteme '
      call eauEntrantSysteme(sc,pg,t,c,p,itk)
 if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: after eauEntrantSysteme '
! Fertilisations (engrais minéraux et apports organiques exogènes)

   if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: before Fertilisations '
      call Fertilisations(sc,pg,t,c,soil,p,itk(1))
       if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: after Fertilisations '

! Résidus de culture
    ! QNplante(n-1) et QNgrain(n-1) car les valeurs pour le jour n n'ont pas encore été calculées

       if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: before ApportResidusCulture '


      call ApportResidusCulture(sc%n,p(1)%nrec,soil%cumvminr,itk(1)%P_codcueille,p(1)%ntaille,p(1)%P_coderacine, &
                      p(1)%masec(0,sc%n), pg%P_proprac, pg%P_y0msrac, p(1)%msrac(sc%n-1), p(1)%magrain(0,sc%n),    & ! IN
                      p(1)%QNplante(0,sc%n-1), p(1)%QNgrain(0), itk(1)%P_ressuite, p(1)%mabois(0),      & ! p(1)%QNgrain_nrec = 0.
                      p(1)%sioncoupe, p(1)%lracsentot,p(1)%zrac,p(1)%P_longsperac,soil%P_profhum,       & ! IN
                      soil%Qminrcult, p(1)%msrac(sc%n),p(1)%qressuite,p(1)%QNressuite,p(1)%QCressuite,  & ! INOUT
                      p(1)%CsurNressuite, p(1)%CNplante(sc%AOAS), soil%itrav1, soil%itrav2, sc%ires,    &
                      sc%nbCouchesSol, pg%P_CNresmin(1:pg%nbResidus), pg%P_CNresmax(1:pg%nbResidus),    & ! APORES IN
                      pg%nbResidus, pg%P_Qmulchdec(1:pg%nbResidus),                                     &
                      itk(1)%nap, sc%airg(sc%n+1), sc%Cres(1:sc%nbCouchesSol,1:pg%nbResidus),           & ! APORES INOUT
                      sc%Nres(1:sc%nbCouchesSol,1:pg%nbResidus), sc%Cnondec(1:10),sc%Nnondec(1:10), &
                      sc%Cmulchnd,sc%Nmulchnd,p%Crac,p%Nrac, sc%QCapp, sc%QNapp,sc%QCresorg, sc%QNresorg,            &
                      pg%P_awb(1:pg%nbResidus),pg%P_bwb(1:pg%nbResidus),pg%P_cwb(1:pg%nbResidus),pg%P_CroCo(1:pg%nbResidus),&
                      pg%P_akres(1:pg%nbResidus),pg%P_bkres(1:pg%nbResidus),pg%P_ahres(1:pg%nbResidus),pg%P_bhres(1:pg%nbResidus),&
                      sc%Wb(1:pg%nbResidus),sc%kres(1:pg%nbResidus),sc%hres(1:pg%nbResidus),p(1)%qressuite_tot,       &
                      p(1)%CsurNressuite_tot,p%QCrac,p%QNrac,p%QNplantefauche)


       if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: after ApportResidusCulture '

return
end subroutine apports
 
