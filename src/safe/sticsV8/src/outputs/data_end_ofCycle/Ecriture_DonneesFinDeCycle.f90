! ---------------------------------------------------------
!     calculs de fin de simulation
!     + écriture du fichier recup.tmp
! ---------------------------------------------------------
!> Write in the file recup.tmp for the next crop
subroutine Ecriture_DonneesFinDeCycle(sc,pg,soil,p,itk)

USE Stics
USE Plante
USE Itineraire_Technique
USE Parametres_Generaux
USE Sol
USE Divers, only: isBissextile

  implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc  
  type(Parametres_Generaux_), intent(IN)    :: pg  
  type(Plante_),              intent(INOUT) :: p(sc%P_nbplantes)
  type(ITK_),                 intent(INOUT) :: itk(sc%P_nbplantes)
  type(Sol_),                 intent(INOUT) :: soil  

! les VARIABLES LOCALES
  integer :: ifwateravant  !>  
  integer :: i  !>  
  integer :: ir  !>  
  integer :: idx  !>  
  integer :: icou  !>  
  integer :: iz  
  integer :: nbresid        ! nombre de types de résidus en mulch
  !DR 03/08/2015
  real    :: QNplante_fin

! pour record
  integer ib1, ib2                                        ! enabling_record
  character(len=300) :: filepluspath                      ! enabling_record

   character(len=12) :: P_typsol  !< // PARAMETER // Soil type // * // PARSOL // 1
   character(len=255) :: datapath ! enabling_record
   character(len=255) :: sticsid

  ! to get the full path
  ib1 = len_trim(datapath)                             ! enabling_record
  ib2 = len_trim(sticsid)                              ! enabling_record
  if (ib1 .eq. 0 ) then                                   ! enabling_record
     filepluspath = 'recupiza.tmp'
  else
     if (ib2 .eq. 0 ) then
        filepluspath = datapath(1:ib1) // '/tmp/' // 'recupiza.tmp'  ! enabling_record
     else
        filepluspath = datapath(1:ib1) // '/tmp/' // 'recupiza' // sticsid(1:ib2) // '.tmp'  ! enabling_record
     endif
  endif                                                   ! enabling_record
!      write(*,*), 'write :', filepluspath
! fin record
      nbresid = (pg%nbResidus-1)/2

!      open(12,file = 'recup.tmp',status = 'unknown')
      open(12,file = filepluspath,status = 'unknown')

      if (sc%ifwater_courant > sc%nbjsemis) then
        ifwateravant = sc%ifwater_courant - sc%nbjsemis
      else
        ifwateravant = sc%ifwater_courant
      endif


! DR 24/09/2012 j'esaie d'ecrire sans format
!      write(12,610) sc%nbjanrec,ifwateravant,sc%nhe,sc%n
!      write(12,620) (sc%HR(i),i = 1,5)
!      write(12,620) (soil%AZnit(i),i = 1,5)
!      write(12,620) (sc%AZamm(i),i = 1,5)
!      write(12,620) (sc%TS(i),i = 1,5)

      write(12,*) sc%nbjanrec,ifwateravant,sc%nhe,sc%n
      write(12,*) (soil%HR(i),i = 1,5)
      write(12,*) (soil%AZnit(i),i = 1,5)
      write(12,*) (soil%AZamm(i),i = 1,5)
      write(12,*) (soil%TS(i),i = 1,5)
      write(12,*)   sc%tcultveille, sc%tairveille

!!!610   format(4i10)
!!!620   format(25(e10.4,2x))

!  Bruno mai 2012 : prise en compte de tous les pools C et N organiques
!      write(12,620) sc%Chumt,sc%Chuma,sc%Chumi,sc%Nhumt,sc%Nhuma,sc%Nhumi,sc%Cr,sc%Nr,sc%Cb,sc%Nb,sc%Cmulchnd,sc%Nmulchnd, &
!                    sc%Cmulchdec,sc%Nmulchdec,sc%Cbmulch,sc%Nbmulch
      write(12,*) sc%Chumt,sc%Chuma,sc%Chumi,sc%Nhumt,sc%Nhuma,sc%Nhumi,sc%Cr,sc%Nr,sc%Cb,sc%Nb,sc%Cmulchnd,sc%Nmulchnd, &
                    sc%Cmulchdec,sc%Nmulchdec,sc%Cbmulch,sc%Nbmulch
!  Bruno aout 2012 : ajout de Cnondec et Nnondec
      do ir = 1,pg%nbResidus
!         write(12,620) sc%kres(ir),pg%P_kbio(ir),sc%hres(ir),sc%Wb(ir)
         write(12,*) sc%kres(ir),pg%P_kbio(ir),sc%hres(ir),sc%Wb(ir)
      end do
      do ir = 1,nbResid
!         write(12,620) sc%Cnondec(ir),sc%Nnondec(ir)
         write(12,*) sc%Cnondec(ir),sc%Nnondec(ir)
      end do
!  Bruno mai 2012 : ajout de Nhum(i)
      do i = 1,40
!          write(12,620) sc%Cres(i,1:pg%nbResidus)
!          write(12,620) sc%Nres(i,1:pg%nbResidus)
!          write(12,620) sc%Cbio(i,1:pg%nbResidus)
!          write(12,620) sc%Nbio(i,1:pg%nbResidus)
!          write(12,620) sc%Chum(i), sc%Nhum(i)

          write(12,*) sc%Cres(i,1:pg%nbResidus)
          write(12,*) sc%Nres(i,1:pg%nbResidus)
          write(12,*) sc%Cbio(i,1:pg%nbResidus)
          write(12,*) sc%Nbio(i,1:pg%nbResidus)
          write(12,*) sc%Chum(i), sc%Nhum(i)
      end do
!  Bruno mai 2012 : separation de hur(i) et sat(i)
      do i = 1,sc%nhe
!          write(12,620) sc%hur(i), sc%sat(i), soil%nit(i), soil%amm(i), sc%tsol(i)
          write(12,*) sc%hur(i), sc%sat(i), soil%nit(i), soil%amm(i), sc%tsol(i)
      end do

! DR 14/10/2011 mise en conformité param.sol
!      write(12,700) soil%P_numsol,soil%P_typsol,soil%P_argi,soil%P_Norg,soil%P_profhum,soil%P_calc, &
!                    soil%P_pH,soil%P_concseuil,soil%P_albedo,soil%P_q0,soil%P_ruisolnu,soil%P_obstarac
!      write(12,700) soil%P_numsol,soil%P_typsol,soil%P_argi,soil%P_Norg,soil%P_profhum,soil%P_calc,soil%P_pH,    &
!                    soil%P_concseuil,soil%P_albedo,soil%P_q0,soil%P_ruisolnu,soil%P_obstarac,soil%P_pluiebat, &
!                    soil%P_mulchbat,soil%P_zesx,soil%P_cfes,soil%P_z0solnu ,soil%P_CsurNsol, soil%P_penterui
                    ! DR 22/08/2012 j'ajoute les 2 parametres  ,soil%P_CsurNsol, soil%P_penterui
      write(12,*) soil%P_numsol,P_typsol,soil%P_argi,soil%P_Norg,soil%P_profhum,soil%P_calc,soil%P_pH,    &
                    soil%P_concseuil,soil%P_albedo,soil%P_q0,soil%P_ruisolnu,soil%P_obstarac,soil%P_pluiebat, &
                    soil%P_mulchbat,soil%P_zesx,soil%P_cfes,soil%P_z0solnu ,soil%P_CsurNsol, soil%P_penterui





! DR 14/10/2011 mise en conformité param.sol
!      write(12,701) soil%P_numsol,soil%P_codecailloux,soil%P_codemacropor,soil%P_codefente,     &
!                    soil%P_codrainage,soil%P_coderemontcap,soil%P_codenitrif

!      write(12,701) soil%P_numsol,soil%P_codecailloux,soil%P_codemacropor,soil%P_codefente,   &
!                    soil%P_codrainage,soil%P_coderemontcap,soil%P_codenitrif,soil%P_codedenit
      write(12,*) soil%P_numsol,soil%P_codecailloux,soil%P_codemacropor,soil%P_codefente,   &
                    soil%P_codrainage,soil%P_coderemontcap,soil%P_codenitrif,soil%P_codedenit

! DR 14/10/2011 mise en conformité param.sol
!      write(12,702) soil%P_numsol,soil%P_capiljour,soil%P_humcapil,soil%P_profimper,            &
!                    soil%P_ecartdrain,soil%P_ksol,soil%P_profdrain

!      write(12,702) soil%P_numsol,soil%P_profimper,soil%P_ecartdrain,soil%P_ksol,soil%P_profdrain,     &
!                    soil%P_capiljour,soil%P_humcapil,soil%P_profdenit,soil%P_vpotdenit
      write(12,702) soil%P_numsol,soil%P_profimper,soil%P_ecartdrain,soil%P_ksol,soil%P_profdrain,     &
                    soil%P_capiljour,soil%P_humcapil,soil%P_profdenit,soil%P_vpotdenit

      do icou = 1,5
!        write(12,703) soil%P_numsol,soil%P_epc(icou),soil%P_hccf(icou),soil%P_hminf(icou),  &
      ! domi 22/03/06 j'ecris da et non P_daf
      ! DR 10/04/2012 voir avce Marie pour savoir comment on gere le tassement et detassement et les cailloux
!                      soil%P_DAF(icou),soil%P_cailloux(icou),soil%P_typecailloux(icou),    &
!                      soil%P_infil(icou),soil%P_epd(icou)

        write(12,*) soil%P_numsol,soil%P_epc(icou),soil%P_hccf(icou),soil%P_hminf(icou),  &
                      soil%P_DAF(icou),soil%P_cailloux(icou),soil%P_typecailloux(icou),    &
                      soil%P_infil(icou),soil%P_epd(icou)

      end do

!!!700   format(i5,1x,a12,1x,f5.1,8(1x,f6.2),1x,f7.2,7f10.5)
!!!701   format(8i5)
702   format(i5,8f10.2)
!!!703   format(i5,5f10.2,i3,f10.2,i3)

      if (sc%n >= sc%maxwth) then
        do idx = 1, sc%P_nbplantes
        ! 27/07/2015 DR et PL dans le cas de rotation de prairies on veut ecrire l'etat de fin de culture dans tout les cas .
        ! on ne le relira que si on est en rotation ou si codeinitprec = 2
        write(fichist,*)'&& sans test sur codeinitprec'
!          if (pg%P_codeinitprec == 2) then
            write(12,*) 'Plante : ',idx
            write(12,'(a8,2x,i3)') p(idx)%P_codeplante, sc%n
            write(12,*) itk(idx)%P_ressuite
            ! DR 03/08/0215 je deplace les ecritures
            ! dr 03/08/2015 je recalcule un qnplante_fin pour pouvoir l'ecrire et ne l'affecter dans P_QNplante0 que si on est en enchainement
            if (p(idx)%P_codebfroid == 3 .and. p(idx)%P_codedormance == 3) then

            !TODO: remplacer les mabois AO & AS par leur équivalent AOAS ?
              QNplante_fin = p(idx)%QNplante(sc%AOAS,p(idx)%ntaille-1)    &
                               - p(idx)%mabois(sc%AS) * 0.5 * 10.               &
                               - p(idx)%mabois(sc%AO) * 0.5 * 10.               &
                               - p(idx)%Qngrain_ntailleveille
              if (QNplante_fin <= 0.) QNplante_fin = 0.
            else
             QNplante_fin = p(idx)%QNplante(sc%AOAS,sc%n)
            endif

            write(12,*) p(idx)%lai(sc%AOAS,sc%n), p(idx)%masec(sc%AOAS,sc%n),  &
                                 p(idx)%magrain(sc%AOAS,sc%n),p(idx)%zrac,              &
                                 QNplante_fin,p(idx)%resperenne(sc%AOAS)

            write(12,*) p(idx)%cu(sc%n)
            write(12,*)(p(idx)%LRACH(i),i = 1,5)

! DR le 14/04/2016 on va tenter de garder pour la prairie les varaibles mafeuiltombe, msneojaune et masecneo
            write(12,*) p(idx)%mafeuiltombe(0:2),p(idx)%msneojaune(0:2),p(idx)%masecneo(0:2),&
                      p(idx)%mafeuiljaune(0:2),p(idx)%dltamsen(0:2)



! DR 03/08/2015 c'etait les affectations qu'il faut conditionner sur codeinitprec
          if (pg%P_codeinitprec == 2) then

            p(idx)%P_lai0   = p(idx)%lai(sc%AOAS,sc%n-1)
            p(idx)%P_masec0 = p(idx)%masec(sc%AOAS,sc%n-1)
            p(idx)%P_zrac0  = p(idx)%zrac
            p(idx)%P_QNplante0 = QNplante_fin


            do iz = 1,5
              p(idx)%P_densinitial(iz) = p(idx)%LRACH(iz)
            enddo

    ! NB le 11/09/05 à cause pb de cumul intenpestif azote des bois taillés
    !        P_QNplante0(idx) = QNplanteC(idx,n)

    ! 07/09/06 IGC seul devant le danger!!! Voici que je vais mettre un petit test
    ! pour laisser le calcul de P_QNplante0 égal à la valeur qui a été observé dans la
    ! plante le derniere jour avant la taille (car le jour de taille QNplante devient
    ! égal à 0). Le test est basé sur la caracteristique de la dormance de ligneuses
    !     if(P_codedormance(idx) == 3)then
    !        P_QNplante0(idx) = QNplanteC(idx,n)-mabois(idx,1)*
    !    s     CNplante(idx,1)*10 -mabois(idx,2)* CNplante(idx,2)*10
    !     endif
    ! DR 31/01/07 IGC seul devant le danger ca merde !
    ! il faut tester aussi sur le P_codebfroid sinon on peut aller dans ce test pour du ble ou de la parierie
    !     if(P_codedormance(idx) == 3)then
    !     if(P_codebfroid(idx) == 3.and.P_codedormance(idx) == 3)then
    !        P_QNplante0(idx) = QNplanteC(idx,ntaille(idx)-1)
    !    s     -mabois(idx,1)*0.5*10 -mabois(idx,2)* 0.5*10
    !    s       - QngrainC(idx,ntaille(idx)-1)
    !     endif

            !   if(P_QNplante0(idx).le.0.0) P_QNplante0(idx)=0.0

            p(idx)%P_resperenne0 = p(idx)%resperenne(sc%AOAS)

    !       p(idx)%P_stade0 = 'dor'
            p(idx)%P_magrain0 = 0.0

        ! DR 16/08/06 recuperer l'etat de la dormance si on est en enchainement de perenne
        !  besoins en froid et besoins en chaud
        !   cu0 = p(idx)%cu(sc%n-1)
        !   cu0 = p(idx)%cu(sc%n)
!DR 09/02/2015 je reactive ca
            sc%cu0(idx) = p(idx)%cu(sc%n)
            sc%somelong0(idx) = p(idx)%somelong

          ! DR et IGC 17/03/08 on garde nfindorm si il se passe l'annee en cours
            if (sc%cu0(idx) == 0) then
              sc%nfindorm0(idx) = p(idx)%nfindorm
            else
              sc%nfindorm0(idx) = 0
!              etatvernal=.FALSE.
            endif
          endif
        end do
      endif

    ! on ferme le fichier
      close(12)

return
end subroutine Ecriture_DonneesFinDeCycle
 
 
