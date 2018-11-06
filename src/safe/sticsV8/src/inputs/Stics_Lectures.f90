!> subroutine to read all files parameters
subroutine Stics_Lectures(sc,pg,p,itk,soil,c,sta,t)

USE iso_varying_string

USE Stics
!!!MODIF HiSAFE 8 : suppression de l'objet USM
!!!USE USM
USE Plante
USE Itineraire_Technique
USE Sol
USE Climat
USE Station
USE Parametres_Generaux
USE Divers

  implicit none


  type(Stics_Communs_),       intent(INOUT) :: sc  

  type(Parametres_Generaux_), intent(INOUT) :: pg  

  type(Plante_),              intent(INOUT) :: p(sc%P_nbplantes)

  type(ITK_),                 intent(INOUT) :: itk(sc%P_nbplantes)

  type(Sol_),                 intent(INOUT) :: soil  

  type(Climat_),              intent(INOUT) :: c  

  type(Station_),             intent(INOUT) :: sta  

  type(Stics_Transit_),       intent(INOUT) :: t  

  !DR 17/07/2012 je rajoute le module usm pour les noms de fichiers pour optimistics
  !!!type(USM_),                 intent(INOUT) :: usma  !> // PARAMETER // name of the P_USM // SD // USMXML // 0



! Variables locales
    integer :: i  !>  










    ! Pour chaque plante de la culture, on va lire un fichier technique et un fichier plante
      do i = 1, sc%P_nbplantes


          if(t%P_option_thinning.eq.1)then
              itk(i)%flag_eclairmult=.TRUE.
          else
              itk(i)%flag_eclairmult=.FALSE.
          endif

          if(t%P_option_engrais_multiple.eq.1)then
              itk(i)%flag_plusieurs_engrais=.TRUE.
          else
              itk(i)%flag_plusieurs_engrais=.FALSE.
          endif

          if(t%P_option_pature.eq.1)then
               itk(i)%flag_pature=.TRUE.
!               itk(i)%flag_plusieurs_engrais=.TRUE.
          else
               itk(i)%flag_pature=.FALSE.
          endif






         !!!MODIF HISAFE 6 : on remplace les tableaux de boolean
        !!!sc%plante_ori(i)=.TRUE.
        sc%plante_ori=.TRUE.

      ! PB - 23/07/2009 - On a introduit une variable logique qui détermine si une plante est dominante ou non.
      !                   Par défaut, la plante lue en premier est considérée comme dominante et les suivantes comme dominées.
      ! TODO : voir si on peut mettre les affectations de estDominante ailleurs
        p(i)%estDominante = .FALSE.

        ! si sol nu pas de test/écriture
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!        if (p(i)%P_codeplante == 'snu') then
        if (p(i)%P_codeplante == 1) then
          !--iplt = P_iwater
          itk(i)%P_iplt0 = sc%P_iwater
        else
            !call Plante_Ecriture
        endif

      end do


    ! dr 31/10/07 pour le moment on remet le nometp qu'il faudra changer apres

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!MODIF HISAFE code déplacé dans main.f90
!!!if (sta%P_codeetp == 1) c%nometp = 'pe'
!!!if (sta%P_codeetp == 2) c%nometp = 'pc'
!!!if (sta%P_codeetp == 3) c%nometp = 'sw'
!!!if (sta%P_codeetp == 4) c%nometp = 'pt'




    ! pour les cultures associées, il faut SW, sinon stop
    ! - P_codeetp = 1 --> Penman forcé
    ! - P_codeetp = 2 --> Penman calculé
    ! - P_codeetp = 3 --> Shutwall & Wallace
    ! - P_codeetp = 4 --> Prestley - Taylor
      if (sc%P_nbplantes > 1 .and. sta%P_codeetp /= 3) then
        call EnvoyerMsgHistorique(6)
        call EnvoyerMsgHistorique(433)
          !stop
          call exit(9)
      endif

    ! Pour les cultures associées (+ d'1 plante), il faut Shutwall & Wallace
      if (sc%P_nbplantes > 1 .and. sta%P_codeetp /= 3) then
        call EnvoyerMsgHistorique(433)
        call EnvoyerMsgHistorique(434)
        sta%P_codeetp = 3
    ! 16/10/06 - DR et ML - en culture associé on ne sort plus mais on force le P_codeetp=3 (Shutwall & Wallace)
    ! On teste dans une routine d'initialisation du climat (iniclim) si les calculs sont possibles
        !--stop
      endif

    ! On effectue un test de cohérence des paramètres pour le transfert radiatif
      if (sc%P_nbplantes > 1) then
        if (itk(1)%P_codetradtec == 2) then
          call EnvoyerMsgHistorique(6)
          call EnvoyerMsgHistorique(430)
          !stop
          call exit(9)
        endif
        p(1)%codetransradb = p(1)%P_codetransrad
        p(1)%P_codetransrad = 2
      endif

      do i = 1, sc%P_nbplantes
        if (p(i)%P_codetransrad == 2) then
          if (  itk(i)%P_interrang >= 999    .or.   &
                itk(i)%P_interrang <= 0.     .or.   &
                itk(i)%P_orientrang >= 999.  .or.   &
                p(i)%P_ktrou >= 999.         .or.   &
                p(i)%P_forme >= 999.         .or.   &
                p(i)%P_rapforme >= 999.      .or.   &
                p(i)%P_hautbase >= 999.      .or.   &
                p(i)%dfol >= 999.) then
            call EnvoyerMsgHistorique(6)
            call EnvoyerMsgHistorique(432)
            !stop
            call EnvoyerMsgHistorique(6)
            call exit(9)
          end if
        endif
      end do

! DR 11/12/2014 on verifie la coherence des parametres pour l'etp SW et le codebeso
     if(sta%P_codeetp == 3 .and. p(1)%P_codebeso==1 ) then
           call EnvoyerMsgHistorique(6)
           call EnvoyerMsgHistorique(203)
           call EnvoyerMsgHistorique(62)
          !stop
          call exit(9)
     endif

! DR  et Fr 30/05/2016 on verifie la coherence des parametres pour l'etp PC ou PE et le codebeso
     if(sta%P_codeetp .ne.3  .and. p(1)%P_codebeso==2 ) then
           call EnvoyerMsgHistorique(6)
           call EnvoyerMsgHistorique(1203)
           call EnvoyerMsgHistorique(1062)
!           call exit(9)
     endif


      if(soil%P_CsurNsol==0.) soil%P_CsurNsol = 1./.105   !pg%P_Wh ! Bruno - initialisation du nouveau paramètre rapport C/N du sol
      if(soil%P_penterui==0.) soil%P_penterui = 0.33   !DR 27/07/2012 - externalisation du parametre penterui
      sc%nbCouchesSol =  soil%nbCouchesSol_max ! je le rajoute la car il n'est pas encore connu

    ! Pour tester les paramètres du sol
      call Sol_Tests(soil,sc%nbCouchesSol,pg%P_masvolcx,sc%beta_sol)
      do i = 1, sc%P_nbplantes
           call sol_test_itk(itk(i),soil%P_profhum)
      enddo





return
end subroutine Stics_Lectures
 
 
