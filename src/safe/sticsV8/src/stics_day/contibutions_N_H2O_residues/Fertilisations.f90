subroutine Fertilisations(sc,pg,t,c,soil,p,itk)

! *-----------------------------------------------------------------------------------------------------------------------------------
!> This program manages mineral and organic nitrogen supplies.
!> - Stics book paragraphe 6.3, page 100
!>
!! The inorganic N pool in soil can be replenished by the addition of synthetic fertilizers (called 'mineral fertilizers'),
!! by organic fertilizers which contain significant amounts of mineral N (for example: pig slurry, distillery vinasse, etc.),
!! by rainfall or irrigation water.
!!
!! The N inputs derived from rain and irrigation are sumed up in the variables AMMSURF (inputs of NH4-N) and
!! PRECIPN (inputs of NO3-N)(see apportsNparPluieEtIrrigation.f90).
!! The N inputs derived from mineral fertilizers (NH4 + NO3)-N (see apportsNparEngraisMineraux.f90) and from the inorganic fraction of
!! organic fertilizers (see apportsOrganiquesEtTravailDuSol.f90) are sumed up in the variable TOTAPN.
! *------------------------------------------------------------------------------------------------------------------------------------

USE Stics
USE Plante
USE Itineraire_Technique
USE Sol
USE Climat
USE Parametres_Generaux

  implicit none


  type(Stics_Communs_),       intent(INOUT) :: sc  
  type(Parametres_Generaux_), intent(IN)    :: pg  
  type(Plante_),              intent(INOUT) :: p(sc%P_nbplantes)    ! Toutes les plantes du système, pour somme des LAI et des ABSO
  type(ITK_),                 intent(INOUT) :: itk  
  type(Sol_),                 intent(INOUT) :: soil  
  type(Climat_),              intent(IN)    :: c  
  type(Stics_Transit_),       intent(INOUT) :: t  

!: Variables locales
  integer :: nd  !>  
  integer :: ii  
  integer :: AO  !>  
  integer ::  AS  



  AS = sc%AS
  AO = sc%AO
      !- le calcul est global aux cultures, et pour simuler cela, on se place
      !- par defaut sur la plante dominante (ipl = 1) avec ensoleillement (ens = AS)
      !- puis on calcule une valeur de Lai global à la culture (i.e : pour toutes les plantes)
    !TODO: déplacer le calcul de laiTot qui n'est pas utilisé par fertil, le mettre là où il est utilisé.
      sc%laiTot = 0.
      do ii = 1, sc%P_nbplantes
        sc%laiTot = sc%laiTot + p(ii)%lai(0,sc%n) ! TODO, il faut calculer le laiC(n)(=lai(0,n)) avant ici.
      end do

      do nd = 1,min(5, sc%n)
        sc%absoTot(nd) = 0.0
        do ii = 1, sc%P_nbplantes
          sc%absoTot(nd) = sc%absoTot(nd)+ p(ii)%abso(AS,sc%n-nd)+ p(ii)%abso(AO,sc%n-nd)
        end do
      end do


      !- pour pouvoir enfin appeler la routine de fertilisations
      if (itk%P_codedateappn == 1 .and. p(1)%nplt > 0) then
        call calapNenUpvt(sc%n,p(1)%nplt,itk%P_upvttapN(1:50),itk%P_codefracappN,itk%P_doseN(1:50), &
                          itk%P_fracN(1:50),itk%P_Qtot_N,p(1)%nlev,p(1)%upvt,sc%jjul,        &
                          sc%anit(sc%n),itk%numeroappN,itk%napN,p(1)%somupvt,itk%P_julapN(1:50))
      end if

! 1. calcul automatique de la fertilisation (NH4+NO3)
! ---------------------------------------------------
! - (uniquement pour le mode de simulation monoculture donc ipl = 1 et ens = AS)

      if (t%P_codecalferti == 1) then
        itk%P_engrais = 8 ! P_engrais type 8 : données de pertes forcées (cf. doc)

        ! le jour du semis, on initialise à zéro le nombre d'apport en N
        ! TODO: extrait du calcul automatique, à voir où le mettre
        if (sc%n == p(1)%nplt) itk%napN = 0

        ! Partie ensoleillée
        call calculAutomatiqueFertilisation(pg%P_voleng(itk%P_engrais),pg%P_deneng(itk%P_engrais),pg%P_orgeng(itk%P_engrais), &
                                            t%P_codetesthumN,p(1)%inns(AS),t%P_ratiolN,sc%precip,pg%P_plNmin,           &
                                            sc%cumoffrN,p(1)%demande(AS),p(1)%masec(AS,sc%n),p(1)%P_masecNmax,        &
                                            p(1)%QNplante(AS,sc%n),t%P_dosimxN,sc%hur(1),sc%hucc(1),p(1)%P_adilmax,    &
                                            p(1)%P_bdilmax,sc%effN,sc%anit(sc%n),itk%napN)
      endif

! 2. Apports d'azote (NH4 et NO3) par les pluies et l'irrigation
! --------------------------------------------------------------
        ! TODO : P_locirrig peut être égal à zéro, l'indice zéro de nit n'existe pas, bug ?!


      call apportsNparPluieEtIrrigation(c%trr(sc%n),pg%P_concrr,sc%airg(sc%n),itk%P_concirr,itk%P_codlocirrig,itk%P_locirrig,  &
                                        sc%irrigN,soil%nit(1),soil%nit(itk%P_locirrig),soil%amm(1),sc%precipN,            &
                                        sc%pluieN,sc%precipjN,sc%irrigjN)

! 3. Apports d'azote (NH4 et NO3) par les engrais mineraux
! ----------------------------------------------------------
      ! DR 30/03/2016 on apporte plusieurs types d'engaris
      ! DR on les apporte pas le meme jour obligatoirement


!      call apportsNparEngraisMineraux(pg%P_orgeng(sc%type_ferti(sc%n)),pg%P_voleng(sc%type_ferti(sc%n)),  &
!                                      pg%P_deneng(sc%type_ferti(sc%n)), &
!                                      sc%anit(sc%n),sc%n,sc%absoTot,pg%P_Vabs2,itk%P_codlocferti,           & ! IN
!                                      itk%P_locferti,pg%P_pHminvol,pg%P_pHmaxvol,soil%P_pH,pg%P_Xorgmax,    &
!                       !               soil%P_codedenit,pg%P_Wh,pg%P_engamm(itk%P_engrais),                  &
!                                      soil%P_codedenit,pg%P_engamm(sc%type_ferti(sc%n)),                  &
!                                      soil%Nvoleng,soil%Ndenit,soil%Norgeng,soil%QNvoleng,soil%QNorgeng,   & ! INOUT
!                                      soil%QNdenit,sc%Nhum(1:10),sc%Nhuma,soil%amm(1:max(1,itk%P_locferti)), &
!                                      soil%nit(1:max(1,itk%P_locferti)), sc%precipN,sc%totapN)


! DR 30/03/2016 apport par les vaches
! 3. Apports d'azote (type 3 ) par les vaches: uree
! ----------------------------------------------------------

      if(sc%flag_onacoupe .and. itk%P_restit(p(1)%numcoupe).eq.1)then
      ! 14/04/2016 fertilisation par les pissats
            sc%type_ferti(sc%n) = t%P_engrais_pature
            call apportsNparEngraisMineraux(pg%P_orgeng(t%P_engrais_pature),pg%P_voleng(t%P_engrais_pature), &
                                      pg%P_deneng(t%P_engrais_pature), &
                                      sc%anit_uree(sc%n),sc%n,sc%absoTot,pg%P_Vabs2,itk%P_codlocferti,       & ! IN
                                      itk%P_locferti,pg%P_pHminvol,pg%P_pHmaxvol,soil%P_pH,pg%P_Xorgmax,     &
                                      soil%P_codedenit,pg%P_engamm(t%P_engrais_pature),                                       &
                                      soil%Nvoleng,soil%Ndenit,soil%Norgeng,soil%QNvoleng,soil%QNorgeng,     & ! INOUT
                                      soil%QNdenit,sc%Nhum(1:10),sc%Nhuma,soil%amm(1:max(1,itk%P_locferti)), &
                                      soil%nit(1:max(1,itk%P_locferti)), sc%precipN,sc%totapN)

            ! DR 06/04/2016 on remet le flag à faux
            sc%flag_onacoupe=.FALSE.
      else
! 14/04/2016 fertilisation classique
            call apportsNparEngraisMineraux(pg%P_orgeng(sc%type_ferti(sc%n)),pg%P_voleng(sc%type_ferti(sc%n)),  &
                                      pg%P_deneng(sc%type_ferti(sc%n)), &
                                      sc%anit(sc%n),sc%n,sc%absoTot,pg%P_Vabs2,itk%P_codlocferti,           & ! IN
                                      itk%P_locferti,pg%P_pHminvol,pg%P_pHmaxvol,soil%P_pH,pg%P_Xorgmax,    &
                       !               soil%P_codedenit,pg%P_Wh,pg%P_engamm(itk%P_engrais),                  &
                                      soil%P_codedenit,pg%P_engamm(sc%type_ferti(sc%n)),                  &
                                      soil%Nvoleng,soil%Ndenit,soil%Norgeng,soil%QNvoleng,soil%QNorgeng,   & ! INOUT
                                      soil%QNdenit,sc%Nhum(1:10),sc%Nhuma,soil%amm(1:max(1,itk%P_locferti)), &
                                      soil%nit(1:max(1,itk%P_locferti)), sc%precipN,sc%totapN)

      endif

! *---------------------------------------------------------------
! 4. Apport d'amendements organiques  et/ou travail du sol
! *-    Conséquences: - apport d'azote minéral sous forme NH4
! *-                  - apport de différentes formes de MO
! *-                  - mélange de ces MO par le travail du sol
! *-  P_profres  = cote supérieure d'incorporation des résidus
! *-  P_proftrav = profondeur de travail du sol = cote inférieure
! *---------------------------------------------------------------

      ! TODO : que se passe-t-il si napS = zéro ??
! DR 01/02/2011 on remplace napS par P_nbjtrav ou P_nbjres
! DR 26/07/2011 pb sur l'appel de fonction mauvaise prise en compte de numjtrav

      call apportsOrganiquesEtTravailDuSol(sc%n,sc%nbCouchesSol,pg%nbResidus,itk%P_nbjres,  &
             itk%P_nbjtrav,p(1)%numjtrav(1:itk%P_nbjtrav),p(1)%numjres(1:itk%P_nbjres),  &
             itk%P_coderes(1:itk%P_nbjres),itk%P_proftrav(1:itk%P_nbjtrav), &
             itk%P_profres(1:itk%P_nbjtrav),itk%P_qres(1:itk%P_nbjres),itk%P_Crespc(1:itk%P_nbjres),      &
             itk%P_Nminres(1:itk%P_nbjres),itk%P_eaures(1:itk%P_nbjres),pg%P_CNresmin(1:pg%nbResidus),  &
             pg%P_CNresmax(1:pg%nbResidus),pg%P_awb(1:pg%nbResidus),pg%P_bwb(1:pg%nbResidus),       &
             pg%P_cwb(1:pg%nbResidus),pg%P_CroCo(1:pg%nbResidus),pg%P_akres(1:pg%nbResidus),pg%P_bkres(1:pg%nbResidus),        &
             pg%P_ahres(1:pg%nbResidus),pg%P_bhres(1:pg%nbResidus),                               &
             pg%P_Qmulchdec(1:pg%nbResidus),pg%P_alphapH,pg%P_dpHvolmax,pg%P_pHvols,soil%P_pH,soil%P_profhum, &
             t%P_codeNmindec,p%zrac,soil%Nvolatorg,soil%amm(1:2),sc%totapN,sc%totapNres,                      &
             itk%P_CsurNres(1:itk%P_nbjres),itk%nap,sc%airg(sc%n+1),                              &
             sc%Cres(1:sc%nbCouchesSol,1:pg%nbResidus),sc%Nres(1:sc%nbCouchesSol,1:pg%nbResidus),  &
             sc%Cbio(1:sc%nbCouchesSol,1:pg%nbResidus),sc%Nbio(1:sc%nbCouchesSol,1:pg%nbResidus),  &
             sc%Chum(1:sc%nbCouchesSol),sc%Nhum(1:sc%nbCouchesSol),          &
             sc%Wb(1:pg%nbResidus),sc%kres(1:pg%nbResidus),             &
             sc%hres(1:pg%nbResidus),soil%itrav1,soil%itrav2,sc%ires,sc%Cmulchnd,sc%Nmulchnd,          &
             sc%Cnondec(1:10),sc%Nnondec(1:10),soil%dpH,soil%pHvol, &
             sc%QCapp,sc%QNapp,sc%QCresorg,sc%QNresorg,sc%irmulch)


    !!!MODIF HISAFE 11 : Supression code inutile
    !!!  if (sc%anit(sc%n) > 0) then
    !!!     t%nbapN(1) = t%nbapN(1) + 1
    !!!     t%dateN(1,t%nbapN(1)) = sc%jjul
    !!!  endif

      ! Si on simule + d'1 plante, alors on recalcule un precipN (à préciser)
      if (sc%P_nbplantes > 1) sc%precipN = p(1)%stemflow * pg%P_concrr + sc%precipN

end subroutine Fertilisations
 
 
