!         subroutine beaupl
! ****************************************************************************** c
! * calcul les besoins en eau de la culture                                    * c
! * version 4.0 selon deux modalités possibles                                 * c
! * P_codebeso = 1 : approche k * etm                                            * c
! * P_codebeso = 2 : approche résistive (Penman-Monteith,Shuttleworth & Wallace) * c
! ****************************************************************************** c
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 7.1.1, page 126-127
!>
!! This subroutine calculates the potential evaporation of the soil (eos), related to the energy available at the soil level.
!! Then it calls the modules
!>    - solnu.f90, calculating the actual evaporation (esol), related to water availability;
!>    - ketp.f90, calculating the maximum transpiration (eop).
!>    - or the module shutwall.f90.
!!
!! Indeed, there are two methods for calculating potential evaporation related to plant cover above the soil, using either LAI or fractional ground cover,
!! and the possible presence of an inert cover placed on the soil (Brisson et al., 1998b).
!!
!! The first relies on a Beer’s Law equivalent (module ketp.f90) and is linked with the “crop coefficient approach” for the estimation of plant requirements;
!! it uses the reference potential evapotranspiration (tetp). When using the radiation transfer option the value of delta is dynamically recalculated as a function
!! of the canopy geometry and the quality of radiation (direct/diffusive radiation). However for row crops, justifying the use of the radiation transfer calculations,
!! it is highly recommended to use the following energy balance approach.
!!
!! The second is an energy balance approach (module shutwall.f90) and is available only if the LAI is explicitly calculated.
! ------------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine besoinsEnEauDeLaPlante(sc,pg,c,sta,soil,p,itk,t)


USE Stics
USE Plante
USE Itineraire_Technique
USE Parametres_Generaux
USE Climat
USE Station
USE Sol

  implicit none



  type(Stics_Communs_),       intent(INOUT) :: sc  

  type(Parametres_Generaux_), intent(IN)    :: pg  

  type(Climat_),              intent(INOUT) :: c  

  type(Plante_),              intent(INOUT) :: p(sc%P_nbplantes)

  type(ITK_),                 intent(INOUT) :: itk(sc%P_nbplantes)

  type(Stics_Transit_),       intent(INOUT) :: t  

  type(Sol_),                 intent(INOUT) :: soil  

  type(Station_),             intent(INOUT) :: sta  


  integer :: i  


    ! calcul des besoins en eau affectant les diverses strates

      if (p(1)%P_codebeso == 1) then

    !: Calcul de l'évaporation potentielle
        if (p(1)%P_codelaitr == 1) then
          if(itk(1)%P_codepaillage == 2)then   !! paillage mulch plastique
            sc%eos = c%tetp(sc%n) * exp(-sc%delta * sc%laiTot) * &
                      (1 - itk(1)%P_couvermulchplastique)

          else
            sc%eos = c%tetp(sc%n) * exp(-sc%delta * sc%laiTot) * (1 - sc%couvermulch)

          endif
        else
          if(itk(1)%P_codepaillage == 2)then   !! paillage mulch plastique
             sc%eos = c%tetp(sc%n) * (1 - sc%tauxcouv(sc%n)) * &
                      (1 - itk(1)%P_couvermulchplastique)
          else
             sc%eos = c%tetp(sc%n) * (1 - sc%tauxcouv(sc%n)) * (1 - sc%couvermulch)
          endif
        endif


! DR_2010	write(168,*)'beso',sc%eos
!#if DEBUG == 1
!        if (iand(sc%solnu,1) >0) call solnu_debug_read_input(1380,sc,pg,p,itk,soil,c,sta,t,1)
!        if (iand(sc%solnu,2) >0) call solnu_debug_write_input(1381,sc,pg,p,itk,soil,c,sta,t,1)
!#endif


!subroutine solnu(n,nbCouches,precip,P_codebeso,P_codelaitr,tetp,delta,laiTot,tauxcouv,  &
!                 nitetcult,P_q0,aevap,P_cfes,P_codeactimulch,ha,hi,hpf,hurlim,P_zesx,hur,hucc,          &
!                 eos,sumes00,sumes10,sumes20,supres0,ses2j00,sesj00,smes020,stoc0,nstoc0,       &
!                 sumes0,sumes1,sumes2,supres,ses2j0,sesj0,smes02,stoc,nstoc,sum2,esz,esol,      &
!                 esreste,xmlch1,xmlch2)



 !       call solnu(sc%n,sc%nbCouchesSol,sc%precip,p(1)%P_codebeso,p(1)%P_codelaitr,c%tetp(sc%n),sc%delta,   &   ! IN
 !                  sc%laiTot,sc%tauxcouv(sc%n),c%nitetcult(sc%n),soil%P_q0,soil%aevap, &

        call solnu(sc%n,sc%nbCouchesSol,sc%precip,  &   ! IN
                   sc%nitetcult(sc%n),soil%P_q0,soil%aevap, &
                   soil%P_cfes,pg%P_codeactimulch,sc%ha,sc%hi,sc%hpf,sc%hurlim,soil%P_zesx,                   &
                   sc%hur(1:sc%nbCouchesSol),sc%hucc(1:sc%nbCouchesSol),                                &
                   sc%eos,soil%sumes00,soil%sumes10,soil%sumes20,soil%supres0,soil%ses2j00,soil%sesj00, &   ! INOUT
                   soil%smes020,soil%stoc0,soil%nstoc0,sc%sumes0,sc%sumes1,sc%sumes2,sc%supres,         &
                   sc%ses2j0,sc%sesj0,sc%smes02,sc%stoc,sc%nstoc,sc%sum2,sc%esz,sc%esol,sc%esreste,     &
                   sc%xmlch1,sc%xmlch2)


!#if DEBUG == 1
!        if (iand(sc%solnu,4) >0) call solnu_debug_read_output(1382,sc,pg,p,itk,soil,c,sta,t,1)
!        if (iand(sc%solnu,8) >0) call solnu_debug_write_output(1383,sc,pg,p,itk,soil,c,sta,t,1)
!        if (iand(sc%solnu,16) >0) call solnu_debug_test_output(1384,sc,pg,p,itk,soil,c,sta,t,1)
!#endif

!#if DEBUG == 1
!        if (iand(sc%ketp,1) >0) call ketp_debug_read_input(1390,sc,pg,p,itk,soil,c,sta,t,1)
!        if (iand(sc%ketp,2) >0) call ketp_debug_write_input(1391,sc,pg,p,itk,soil,c,sta,t,1)
!#endif

        call ketp(sc%n,p(1)%P_codelaitr,p(1)%lai(sc%AOAS,sc%n),sc%tauxcouv(sc%n),c%tetp(sc%n),pg%P_beta,    &
                  sc%delta,p(1)%P_codeplante,sta%P_corecTrosee,sc%Emulch,p(1)%P_kmax,p(1)%LAIapex,sc%posibsw, &
                  p(1)%P_tauxrecouvkmax,c%tmin(sc%n),sc%esol,                                             &
                  p(1)%eop(sc%AOAS),p(1)%mouill(sc%AOAS),sc%doi,sc%Edirect,p(1)%Emd,sc%eo,sc%etm,c%tpm(sc%n))

        p(1)%eop(sc%AS) = p(1)%eop(sc%AOAS)
        p(1)%mouill(sc%AS) = p(1)%mouill(sc%AOAS)
       ! TODO: et pour la partie AO ?

!#if DEBUG == 1
!        if (iand(sc%ketp,4) >0) call ketp_debug_read_output(1392,sc,pg,p,itk,soil,c,sta,t,1)
!        if (iand(sc%ketp,8) >0) call ketp_debug_write_output(1393,sc,pg,p,itk,soil,c,sta,t,1)
!        if (iand(sc%ketp,16) >0) call ketp_debug_test_output(1394,sc,pg,p,itk,soil,c,sta,t,1)
!#endif

      else
      ! si nometp = sw

      sc%hauteurMAX = 0.0
      do i = 1, sc%P_nbplantes
        sc%hauteurMAX = max(sc%hauteurMAX,p(i)%hauteur(sc%AS))
        sc%hauteurMAX = max(sc%hauteurMAX,p(i)%hauteur(sc%AO))
      end do

        call shutwall(sc,pg,c,sta,soil,p,itk)  ! DR 19/07/2012 t n'est pas utilisé

      endif

      return
      end
 
 
