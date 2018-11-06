! *********************************************************************************
!  sous programme general d'appel des sous_programmes d'initialisation
!  dans l'ordre
!      initsol2
!     initnonsol
!     initcycle
!      testvartech
!      lecstat
!      iniclim
! *********************************************************************************

subroutine initialisations(sc,pg,p,itk,soil,c,sta,t)

USE Stics
USE Plante
USE Itineraire_Technique
USE Sol
USE Climat
USE Station
USE Parametres_Generaux
USE Divers

  implicit none


  type(Stics_Communs_),       intent(INOUT) :: sc  

  type(Parametres_Generaux_), intent(IN)    :: pg  

  type(Plante_),              intent(INOUT) :: p(sc%P_nbplantes)

  type(ITK_),                 intent(INOUT) :: itk(sc%P_nbplantes)

  type(Sol_),                 intent(INOUT) :: soil  

  type(Climat_),              intent(INOUT) :: c  

  type(Station_),             intent(INOUT) :: sta  

  type(Stics_Transit_),       intent(INOUT) :: t  

! Variable(s) locale(s)
  integer :: i  !>  
  integer :: j  !>  
  integer :: codeRetour  
  character(len=60) :: nomFicClimat  
! record
  integer ib0                                              ! enabling_record
  integer ib1                                              ! enabling_record
  character(len=300) :: filepluspath                       ! enabling_record
! fin record

  character(len=50) :: laifile
   logical :: AgMIP,Macsur


  AgMIP= .FALSE.
  Macsur= .FALSE.
  if(t%P_type_project.eq.2) AgMIP= .TRUE.
  if(t%P_type_project.eq.3) Macsur= .TRUE.


! DR 28/08/07
! DR 06/02/08 dans la version 6.4 P_stdrpdes devient varietal
!   do i = 1,P_nbplantes
!     if (codeforcedrpdes == 1) then
!       P_stdrpdes = p(i)%P_stdrpmat(P_variete)
!       call EnvoyerMsgHistorique(449)
!     endif
!   enddo

! DR 12/02/08 passé dans initnonsol
!      if (P_nbplantes > 1) then
!        dassoinit = densite(2)
! ** densités pour le calcul de la compétition
!          densite(2) = densite(2)+densite(1)/P_bdens(1)*P_bdens(2)
!          dassoiniteqv = densite(2)
!      endif



if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )print *, 'Initialisations non sol'
! ***
! Initialisations des variables de cultures
! On ne passe plus qu'une fois dans initial
! pour l'ensemble de la culture
!      call initnonsol(sc,pg,p,itk,soil,c,sta,t)  ! DR 20/07/2012 c n'est pas utilisé
      call initnonsol(sc,pg,p,itk,soil,sta,t)

      do i = 1, sc%P_nbplantes

if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )print *, 'Initialisations du cycle, plante :', i
        call initcycle(sc,pg,p(i),itk(i),t)

	! Pour initialiser les cumuls (notamment pour les enchainements d'année)
        call cumAOetAS(1,sc%P_codesimul,p(i),itk(i))

      end do



! print *, 'Testvartech'
! ***
! Controle de cohérence des paramètres
! un seul jour de travail du sol (celui de la principale)
      call testvartech(sc%P_nbplantes,itk)

!  dr 14/09/2011 dans le cas de serie on force la fin à P_iwater-1 mais il faut qu'on soit sur 2 ans
    ! dr 14/09/2011 on calcule automatiquement le ifwater si on est en enchainement sur plusieurs annees (nbans>1), on fait en sorte de commencer tj le meme jour (P_iwater)
     if (sc%nbans > 1.and.sc%numcult==1) then
!  dr 21/01/2015 si c'est une culture annuelle qui commence au premier janvier y'a pas de raison de la passer en 2
!!!MODIF HISAFE 11 : Supression code inutile
!!!        if (sc%P_culturean==1 .and.sc%P_iwater==1) then
!!!            sc%P_culturean = 1
!!!        else
!!!            sc%P_culturean = 2
!!!        endif
        if(isBissextile(sc%ansemis))then
           sc%P_ifwater = sc%P_iwater-1 + 366-1
        else
           sc%P_ifwater = sc%P_iwater-1 + 365-1
        endif
     endif



      sc%ansemis = c%anneezero
      sc%annee(1:732) = c%anneezero

    ! calcul du nombre de jours de l'annee de semis et de l'année de récolte
      sc%nbjsemis = nbjParAnnee(c%anneezero)



      sc%annee(c%julzero:sc%nbjsemis) = sc%ansemis

        if (sc%P_culturean  == 1) then
            sc%nbjrecol = 0
        else
            sc%nbjrecol = nbjParAnnee(c%anneezero+1)
            sc%annee(sc%nbjsemis+1:sc%nbjsemis+sc%nbjrecol) = sc%ansemis + 1
        endif


    ! DR 13/01/06 dans le cas ou on a coché 2 ans et que la simulation est sur 1 an
 !     if (sc%P_culturean /= 1 .and. sc%P_ifwater <= sc%nbjsemis) then   ! 14/09/2011
      if (sc%P_culturean > 1 .and. sc%P_ifwater < sc%nbjsemis) then

        call EnvoyerMsgHistorique(442)
!        stop
! dr 04/08/2014 je reactive le stop parait coherent de faire corriger ca par l'utilisateur
! dr 08/09/2014 voir comment corriger ca dans le cas de series ca a l'air de merder
!        call exit(9)
      endif

    ! affectation nbjanrec pour recup.tmp
    !14/09/2011 correction si on demarre un 01/01 on a qu'une annee
    !  if (sc%P_culturean == 1) then
      if (sc%P_culturean == 1 .or. sc%P_iwater==1) then
        sc%nbjanrec = sc%nbjsemis
      else
        sc%nbjanrec = sc%nbjrecol
      endif


    ! initialisation des variables climatiques
      do i = 1, sc%P_nbplantes
if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )print *, 'Initialisations climatique, plante :', i
        !!!MODIF HISAFE 11 : Supression code inutile
        !!!call iniclim(sc,pg,p(i),itk(i),c,sta,t) !DR 19/07/2012 soil n'est pas utilisé
        call iniclim2(sc,pg,p(i),itk(i),c,sta,t) !DR 19/07/2012 soil n'est pas utilisé
    ! DR 13/07/06 essaie d'initialisation de sioncoupe
        p(i)%sioncoupe = .FALSE.
    ! DR 20/07/06 on initialise le nb de jours de repousse de la fauche
    ! DR et FR 22/06/2015 on garde cette valeur entre 2 annees au cas ou on repousse les fauches
    !    p(i)%nbrepoussefauche = 0
      end do

      !!!MODIF HISAFE 11 : Supression code inutile
      ! DR 26012016 j'implemente les regles de decision pour Agmip, on a lu la date de semis
      ! rules=.TRUE. passé dans param_new_form
      !!!if(AgMIP.and.t%P_rules_sowing_AgMIP.eq.1)then
      !!!    call determinate_sowing_Agmip(c,sc%P_usm,sc%ansemis,sc%P_iwater,p(1)%nplt,sc%iplt(1),ferti_semis)
          !DR 05/02/2015 pour le moment Giacomo precnise de demarrer 10 jours avant la premiere date de semis de la fenetre
          !sc%P_ifwater= sc%P_iwater-1
      !!!    sc%anit(p(1)%nplt)=ferti_semis
      !!!endif


      if (sc%P_nbplantes > 1 .and. .not.sc%posibsw) then
          call EnvoyerMsgHistorique(433)
          !stop
          call exit(9)
      endif

    ! TODO : Lecture des lai
    !!!MODIF HISAFE 1 : suppression des chaines de caractères
    !!!  if (lge(sc%P_codesimul,'feuille') .eqv. .TRUE.) then
    !!!if (sc%P_codesimul == 2)  then
    !!!   do i = 1, sc%P_nbplantes
    !!!      laifile='durum-wheat-allur.obs'
    !!!      call Lecture_Lai(sc,p(i),laifile)
    !!!    end do
    !!! endif

    ! DR 05/07/07 on reinitailise chaque annee tempfauche_ancours à P_tempfauche
      do i = 1,sc%P_nbplantes
        if (itk(i)%P_codemodfauche == 3) then
          do j = 1, itk(i)%nbcoupe
         !11/02/2015 on supprime cette reinitialisation qui la premiere annee ne nous fait jamais couper
         ! ca empeche la premiere coupe et donc toutes les autres
         !   p(i)%tempfauche_ancours(j) = itk(i)%P_tempfauche(j)
          !  if (j > 1) p(i)%tempfauche_ancours(j) = itk(i)%P_tempfauche(j) + p(i)%tempfauche_ancours(j-1)
          end do
        endif



      enddo

return
end subroutine initialisations
 
 
