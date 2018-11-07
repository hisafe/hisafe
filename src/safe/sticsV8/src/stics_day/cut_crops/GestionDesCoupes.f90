! ****************************************************** c
! * version 5.1 18/03/98 - dernière modif : 03/02/2004 * c
! *-  sous programme d'initialisation des departs de   * c
! *-  coupe pour la prairie                            * c
! ****************************************************** c

subroutine GestionDesCoupes(sc,pg,c,sta,soil,p,itk,t)


USE Stics
USE Plante
USE Itineraire_Technique
USE Parametres_Generaux
USE Climat
USE Station
USE Sol
USE Bilans

  implicit none


  type(Stics_Communs_),       intent(INOUT) :: sc  

  type(Parametres_Generaux_), intent(IN)    :: pg  

  type(Climat_),              intent(INOUT) :: c  

  type(Plante_),              intent(INOUT) :: p(sc%P_nbplantes)

  type(ITK_),                 intent(INOUT) :: itk(sc%P_nbplantes)

  type(Stics_Transit_),       intent(INOUT) :: t  

  type(Sol_),                 intent(INOUT) :: soil  

  type(Station_),             intent(INOUT) :: sta  


!: Variables locales
  integer :: ncoupedf  !>  
  integer ::  i  !>  
  integer ::  k  
  integer :: compteur


  do i = 1,sc%P_nbplantes
  ! domi 02/10/00 si on est en fauche P_codefauche = 1


    if (itk(i)%P_codefauche == 1) then

    ! 05/05/2015 DR et FR on ajoute la variable mafruit récolté
    ! 23/06/2015 on le deplace car on veut avoir sa valeur dans les variables de sorties journaleres qui sont ecrites avant
    ! p(i)%msrec_fou= p(i)%mafruit- itk(i)%P_msresiduel(p(i)%numcoupe)

    ! On détermine si on est un jour de coupe,
    ! si oui, on met sioncoupe à true
    ! si non, on met sioncoupe à false

      ! DR 09/06/2016 on essaie d'eviter les coupes de fin d'annee quand on a depassé le nb de coupes du tec
        if(sc%onafaitunecoupedelanneedavant)then
                    compteur=1
!            write(619,*)' onafaitunecoupedelanneedavant ', sc%onafaitunecoupedelanneedavant, compteur

        else
                    compteur=0
!            write(619,*)' onafaitunecoupedelanneedavant ', sc%onafaitunecoupedelanneedavant, compteur

        endif
        if(p(i)%numcoupe .gt. itk(i)%nbcoupe+compteur)then
        ! if( sc%nbcoupe_reel(p(i)%ipl)+compteur .gt. itk(i)%nbcoupe)then
            ! DR 20062016 en attente de reprise des modifs enchain'on a depasse le nb de coupes du fichier tec, on repart sur une annee normale'
             p(i)%somcourfauche = 0.
        endif


      if (itk(i)%lecfauche) then   ! on dispose des dates de fauche

          if ((     sc%n == p(i)%nfauche(p(i)%numcoupe)                                  &
!            .or. sc%n == sc%maxwth                                                     &
            .or. p(i)%fauchediff                                                       &
            .or. (      p(i)%somcourfauche >= p(i)%tempfauche_ancours(p(i)%numcoupe)   &
                  .and. p(i)%tempfauche_ancours(p(i)%numcoupe) /= 0.0)))               &
!                  .and. (sc%nbcoupe_reel(p(i)%ipl)<=itk(i)%nbcoupe) )               &
          then
            p(i)%sioncoupe = .TRUE.
          else
            p(i)%sioncoupe = .FALSE.
          endif
      else

      ! paramétrage du stade de coupe automatique NB le 20/08/97
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!          if (itk(i)%P_stadecoupedf == 'lax') ncoupedf = p(i)%nlax
!!!          if (itk(i)%P_stadecoupedf == 'drp') ncoupedf = p(i)%ndrp
!!!          if (itk(i)%P_stadecoupedf == 'sen') ncoupedf = p(i)%nsen
!!!          if (itk(i)%P_stadecoupedf == 'lan') ncoupedf = p(i)%nlan
!!!          if (itk(i)%P_stadecoupedf == 'mat') ncoupedf = p(i)%nmat
!!!          if (itk(i)%P_stadecoupedf == 'rec') ncoupedf = p(i)%nrec
!!!          if (itk(i)%P_stadecoupedf == 'amf') ncoupedf = p(i)%namf

          if (itk(i)%P_stadecoupedf == 6) ncoupedf = p(i)%nlax
          if (itk(i)%P_stadecoupedf == 8) ncoupedf = p(i)%ndrp
          if (itk(i)%P_stadecoupedf == 12) ncoupedf = p(i)%nsen
          if (itk(i)%P_stadecoupedf == 13) ncoupedf = p(i)%nlan
          if (itk(i)%P_stadecoupedf == 10) ncoupedf = p(i)%nmat
          if (itk(i)%P_stadecoupedf == 11) ncoupedf = p(i)%nrec
          if (itk(i)%P_stadecoupedf == 5) ncoupedf = p(i)%namf


          if ((sc%n /= 1 .and. sc%n == ncoupedf) .or. p(i)%fauchediff) then
              p(i)%sioncoupe = .TRUE.
          !goto 100
          else
              p(i)%sioncoupe = .FALSE.
          !goto 200
          endif
      endif

      if (p(i)%sioncoupe) then

      ! DR 09/06/2016 on essaie d'eviter les coupes de fin d'annee quand on a depassé le nb de coupes du tec
       ! if(sc%onafaitunecoupedelanneedavant)then
       !             compteur=1
       ! else
       !             compteur=0
       ! endif
      !  if(p(i)%numcoupe .gt. itk(i)%nbcoupe+compteur)then
        ! if( sc%nbcoupe_reel(p(i)%ipl)+compteur .gt. itk(i)%nbcoupe)then
       !      write(619,*)'on a depasse le nb de coupes du fichier tec, on repart sur une annee normale'
       !      p(i)%somcourfauche = 0.
       ! endif

! 10/06/2016 DR et FR on veut pouvoir couper le dernier jour aussi si les conditions sont atteintes
!        if (sc%n < sc%maxwth) then

      ! domi - le 02/09/97 - si la ms le jour de la coupe est inferieure a 1,5t, on ne coupe pas
      ! le seuil de coupe devient paramètre => P_mscoupemini
      ! DR le 19/07/06 Fr veut que P_mscoupemini s'applique à mafruit et non à masec
      !        if (masec(0,n) < P_mscoupemini) then
      ! DR le 20/07/06 on repousse si une des 2 conditions n'est pas respecté :
      ! mafruit<P_mscoupemini ou lai<leiresiduel
      ! ce qui revient à dire qu'on coupe si les 2 sont respectees ...
      !--       if (mafruit(0) < P_mscoupemini) then
      ! DR et FR 11/02/2015 on a cree une varaible pour pas ecraser la valeur
!          if (p(i)%mafruit < itk(i)%P_mscoupemini .or. p(i)%lai(0,sc%n) < itk(i)%P_lairesiduel(p(i)%numcoupe)) then
        ! 05/05/2015 DR et FR on change le critere de coupe , le recoltable est compare au mscoupemini
        !  if (p(i)%mafruit < itk(i)%mscoupemini_courant .or. p(i)%lai(0,sc%n) < itk(i)%P_lairesiduel(p(i)%numcoupe)) then
       ! dr et fr 16/02/2016le teste est mauvais on ne recolte que si on a au moins mscoupe_mini à recolter
!          if (p(i)%msrec_fou < 0. .or. p(i)%lai(0,sc%n) < itk(i)%P_lairesiduel(p(i)%numcoupe) .or. &

          if (p(i)%msrec_fou < itk(i)%mscoupemini_courant .or.    &
          &   p(i)%lai(0,sc%n) < itk(i)%P_lairesiduel(p(i)%numcoupe) .or. &
          &   p(i)%masec(0,sc%n) < itk(i)%P_msresiduel(p(i)%numcoupe) ) then
               p(i)%sioncoupe = .FALSE.
               p(i)%fauchediff = .TRUE.
               p(i)%nbrepoussefauche = p(i)%nbrepoussefauche+1
               call EnvoyerMsgHistorique(75,sc%n)  ! ajout dr 13/08/2014
!            return
          else
        ! *- fin modif domi
        ! pour les apports d'uerre par les vaches
             sc%flag_onacoupe=.FALSE.
             call JourDeCoupe(i,sc,pg,c,sta,soil,p(i),itk(i),t)

    ! écriture de recup.tmp et calcul de msrac dans recup.for
    ! PB - 07/12/2004 - déplacé de bilcoupe pour l'enchainement des pérennes.
            !!!MODIF HISAFE 11 : Supression code inutile
            !!!call Ecriture_DonneesFinDeCycle(sc,pg,soil,p,itk)
          endif
! 10/06/2016 DR et FR on veut pouvoir couper le dernier jour aussi si les conditions sont atteintes
!        else
        if (sc%n == sc%maxwth) then
          p(i)%P_stlevamf(itk(i)%P_variete) = p(i)%stlevamf0
          p(i)%P_stamflax(itk(i)%P_variete) = p(i)%stamflax0
          p(i)%P_stlaxsen(itk(i)%P_variete) = p(i)%stlaxsen0
          p(i)%P_stsenlan(itk(i)%P_variete) = p(i)%stsenlan0
          p(i)%P_stlevdrp(itk(i)%P_variete) = p(i)%stlevdrp0

          ! copié et collé ici pour correspondre au comportement avant modification pour modularisation
          !  22/06/2015 on reinitialise le nb de jours de repousse de fauche alors que c'est deja fait dqns jourdecoupe
          !  p(i)%nbrepoussefauche = 0.
          !DR 26/06/2015 on est en fin d'annee et on a pas coupe on stocke les prescriptions de fauche pour l'annee d'apres
            p(i)%hautcoupe_anterieure = itk(i)%P_hautcoupe(p(i)%numcoupe)
            p(i)%lairesiduel_anterieure = itk(i)%P_lairesiduel(p(i)%numcoupe)
            p(i)%msresiduel_anterieure = itk(i)%P_msresiduel(p(i)%numcoupe)
            p(i)%anitcoupe_anterieure = itk(i)%P_anitcoupe(p(i)%numcoupe)

            p(i)%tempfauche_realise = p(i)%tempfauche_ancours(p(i)%numcoupe-1)
            !p(i)%tempfauche_realise = p(i)%tempfauche_ancours(p(i)%numcoupe)
            p(i)%restit_anterieure = itk(i)%P_restit(p(i)%numcoupe)
            p(i)%mscoupemini_anterieure = itk(i)%P_mscoupemini(p(i)%numcoupe)

! 15/04/2016 on tente de garder les variables de stades de la vegetaion à la fin de l'annee
! faudrait qu'on le fasse dans tous les cas a la fin de l'année meme si on est pas en fauche repossée, cas ou on a pas atteint la somme
! ce cas la n'arrive que quand on est en somme
! TODO
            p(i)%ulai0 = p(i)%ulai(sc%n)
            p(i)%durvie0(0:2) = p(i)%durvie(0:2,sc%n)
            p(i)%codebbch0 =  p(i)%codebbch_output
! 15/04/2016
           ! DR 20062016 en attente de reprise des modifs enchain'on est en fin d annee avec une somme a ',p(i)%somcourfauche,'et on veut coupe a ' &
           ! &,p(i)%tempfauche_ancours(p(i)%numcoupe)
           ! DR 20062016 en attente de reprise des modifs enchain'derniere fauche realisee a ', p(i)%tempfauche_realise
           ! DR 20062016 en attente de reprise des modifs enchain'on garde les prescriptions pour l annee d apres : lairesiduel ',&
           ! & p(i)%lairesiduel_anterieure,'msresiduel ',p(i)%msresiduel_anterieure, 'haucoupe ',p(i)%hautcoupe_anterieure

        endif
        if(p(i)%fauchediff) return
      end if ! fin du sioncoupe

      call entreLesCoupes(sc%n,p(i)%numcoupe,itk(i)%nbcoupe,p(i)%lai(sc%aoas,sc%n),itk(i)%P_lairesiduel(1:itk(i)%nbcoupe),   &
                          p(i)%somcour,p(i)%nlev,p(i)%nlax,p(i)%swfac(sc%aoas),p(i)%turfac(sc%aoas),p(i)%inns(sc%aoas),    &
                          sc%tcult,c%tmoy(sc%n),p(i)%nmat,                                                                 &
                          p(i)%udevlaires(1:itk(i)%nbcoupe),p(i)%nst1coupe,p(i)%str1coupe,p(i)%stu1coupe,p(i)%inn1coupe,   &
                          p(i)%diftemp1coupe,p(i)%str1intercoupe,p(i)%stu1intercoupe,p(i)%inn1intercoupe,                  &
                          p(i)%diftemp1intercoupe,p(i)%nst2coupe,p(i)%str2coupe,p(i)%stu2coupe,p(i)%inn2coupe,             &
                          p(i)%diftemp2coupe,p(i)%str2intercoupe,p(i)%stu2intercoupe,p(i)%inn2intercoupe,                  &
                          p(i)%diftemp2intercoupe)


        if (sc%n == sc%maxwth) then
          p(i)%P_stlevamf(itk(i)%P_variete) = p(i)%stlevamf0
          p(i)%P_stamflax(itk(i)%P_variete) = p(i)%stamflax0
          p(i)%P_stlaxsen(itk(i)%P_variete) = p(i)%stlaxsen0
          p(i)%P_stsenlan(itk(i)%P_variete) = p(i)%stsenlan0
          p(i)%P_stlevdrp(itk(i)%P_variete) = p(i)%stlevdrp0

          ! copié et collé ici pour correspondre au comportement avant modification pour modularisation
          !  22/06/2015 on reinitialise le nb de jours de repousse de fauche alors que c'est deja fait dqns jourdecoupe
          !  p(i)%nbrepoussefauche = 0.
          !DR 26/06/2015 on est en fin d'annee et on a pas coupe on stocke les prescriptions de fauche pour l'annee d'apres
            p(i)%hautcoupe_anterieure = itk(i)%P_hautcoupe(p(i)%numcoupe)
            p(i)%lairesiduel_anterieure = itk(i)%P_lairesiduel(p(i)%numcoupe)
            p(i)%msresiduel_anterieure = itk(i)%P_msresiduel(p(i)%numcoupe)
            p(i)%anitcoupe_anterieure = itk(i)%P_anitcoupe(p(i)%numcoupe)

            p(i)%tempfauche_realise = p(i)%tempfauche_ancours(p(i)%numcoupe-1)
            !p(i)%tempfauche_realise = p(i)%tempfauche_ancours(p(i)%numcoupe)
            p(i)%restit_anterieure = itk(i)%P_restit(p(i)%numcoupe)
            p(i)%mscoupemini_anterieure = itk(i)%P_mscoupemini(p(i)%numcoupe)

! 15/04/2016 on tente de garder les variables de stades de la vegetaion à la fin de l'annee
! faudrait qu'on le fasse dans tous les cas a la fin de l'année meme si on est pas en fauche repossée, cas ou on a pas atteint la somme
! ce cas la n'arrive que quand on est en somme
! TODO
            p(i)%ulai0 = p(i)%ulai(sc%n)
            p(i)%durvie0(0:2) = p(i)%durvie(0:2,sc%n)
            p(i)%codebbch0 =  p(i)%codebbch_output
! 15/04/2016
           ! DR 20062016 en attente de reprise des modifs enchain'on est en fin d annee avec une somme a ',p(i)%somcourfauche,'et on veut coupe a ' &
            !&,p(i)%tempfauche_ancours(p(i)%numcoupe)
           ! DR 20062016 en attente de reprise des modifs enchain'derniere fauche realisee a ', p(i)%tempfauche_realise
           ! DR 20062016 en attente de reprise des modifs enchain'on garde les prescriptions pour l annee d apres : lairesiduel ',&
           ! & p(i)%lairesiduel_anterieure,'msresiduel ',p(i)%msresiduel_anterieure, 'haucoupe ',p(i)%hautcoupe_anterieure

        endif



    ! le jour de la récolte butoir
      if (sc%n == p(i)%nrecbutoir) then

        if (iand(pg%P_flagEcriture,sc%ECRITURE_BILAN) > 0) then
	! 20/08/2012 DR je laisse la recap de fin de saison pour avoir la synthese des coupes 
	! 20/08/2012 DR et j'ai modifié ecrirebilanrecoltebutoir pour n'avoir que le bilan de BM
	! 21/08/2012 DR je renomme ce n'est pas tres parlant
    !       call ecrireBilanRecolteButoir(sc,pg,soil,p(i),itk(i))
           call ecrireBilanFinCultureFauchee(sc,p(i),itk(i))

! E et B 02/08/2012 on change le nom de la routine qui sera commune à toutes les cultures
           call ecrireBilanEauCN(sc,soil,p(i),itk(i))
        endif


      ! DR 26/02/08 on essai de garder le nb de coupe reellement fait
        sc%nbcoupe_reel(p(i)%ipl) = 0
        do k = 1, itk(i)%nbcoupe
          if (p(i)%nfauche(k) > 0) then
            sc%nbcoupe_reel(p(i)%ipl) = sc%nbcoupe_reel(p(i)%ipl) + 1
          endif
        enddo
      endif

    endif

  end do

return
end subroutine GestionDesCoupes

subroutine entreLesCoupes(n,numcoupe,nbcoupe,lai,P_lairesiduel,somcour,nlev,nlax,swfac,turfac,inns,tcult,tmoy,nmat,   &
                          udevlaires,nst1coupe,str1coupe,stu1coupe,inn1coupe,diftemp1coupe,str1intercoupe,          &
                          stu1intercoupe,inn1intercoupe,diftemp1intercoupe,nst2coupe,str2coupe,stu2coupe,inn2coupe, &
                          diftemp2coupe,str2intercoupe,stu2intercoupe,inn2intercoupe,diftemp2intercoupe)

  implicit none

  integer, intent(IN)    :: numcoupe      !> // OUTPUT // Cut number // SD
  integer, intent(IN)    :: nbcoupe  
  real,    intent(IN)    :: lai                         ! (aoas,n)  	  // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(IN)    :: P_lairesiduel(1:nbcoupe)     !> // PARAMETER // Residual leaf index after each cut (table) // m2 leaf  m-2 soil // PARTEC // 1
  real,    intent(IN)    :: somcour      !> // OUTPUT // Cumulated units of development between two stages // degree.days
  integer, intent(IN)    :: nlev  
  integer, intent(IN)    :: nlax  
  integer, intent(IN)    :: n  
  real,    intent(IN)    :: swfac                       ! (aoas)  	  // OUTPUT // Index of stomatic water stress  // 0-1
  real,    intent(IN)    :: turfac                      ! (aoas)  	  // OUTPUT // Index of turgescence water stress  // 0-1
  real,    intent(IN)    :: inns                        ! (aoas)  	  // OUTPUT // Index of nitrogen stress active on growth in biomass // P_innmin to 1
  real,    intent(IN)    :: tcult      !> // OUTPUT // Crop surface temperature (daily average) // degree C
  real,    intent(IN)    :: tmoy                        ! (n)  	  // OUTPUT // Mean active temperature of air // degree C
  integer, intent(IN)    :: nmat  


  real,    intent(INOUT) :: udevlaires(1:nbcoupe)  
  integer, intent(INOUT) :: nst1coupe  
  real,    intent(INOUT) :: str1coupe  
  real,    intent(INOUT) :: stu1coupe  
  real,    intent(INOUT) :: inn1coupe  
  real,    intent(INOUT) :: diftemp1coupe  
  real,    intent(INOUT) :: str1intercoupe      !> // OUTPUT // stomatal water stress average during the cut (cut crop vegetative phase)  // 0-1
  real,    intent(INOUT) :: stu1intercoupe      !> // OUTPUT // turgescence water stress average during the cut (cut crop vegetative phase)  // 0-1
  real,    intent(INOUT) :: inn1intercoupe      !> // OUTPUT // nitrogen stress (inn) average during the cut (cut crop vegetative phase)  // 0-1
  real,    intent(INOUT) :: diftemp1intercoupe      !> // OUTPUT // mean difference between crop surface temperature and air temperature during the cut (cut crop vegetative phase) // degree C
  integer, intent(INOUT) :: nst2coupe  
  real,    intent(INOUT) :: str2coupe  
  real,    intent(INOUT) :: stu2coupe  
  real,    intent(INOUT) :: inn2coupe  
  real,    intent(INOUT) :: diftemp2coupe  
  real,    intent(INOUT) :: str2intercoupe      !> // OUTPUT // stomatal water stress average during the cut (cut crop reproductive phase)  // 0-1
  real,    intent(INOUT) :: stu2intercoupe      !> // OUTPUT // turgescence water stress average during the cut (cut crop reproductive phase)  // 0-1
  real,    intent(INOUT) :: inn2intercoupe      !> // OUTPUT // nitrogen stress (inn) average during the cut (cut crop reproductive phase)  // 0-1
  real,    intent(INOUT) :: diftemp2intercoupe      !> // OUTPUT // mean difference between crop surface temperature and air temperature during the cut (cut cropreproductive phase) // degree C


! variables locales
  integer :: kcoupe  


    ! on est un jour quelconque
    !---------------------------
    ! modif domi - 13/11/97 - à la premiere coupe on stocke les valeurs de sommes de Temp
    ! pour repartir à un stade de developpement de croissance des feuilles, ulai equivalent
    ! par papport aux les lairesiduels des differentes coupes
      if (numcoupe == 1) then
        do kcoupe = 1,nbcoupe
          if (udevlaires(kcoupe) == 0 .and. lai >= P_lairesiduel(kcoupe)) then
            udevlaires(kcoupe) = somcour
          endif
        end do
      endif

! *- pour l'instant lai = lai+P_lairesiduel
! *- PB - 06/01/2005 - mise en commentaire dans le cadre de l'enchainement des pérennes
! --      if (codeinstal == 1.or.numcoupe > 1) then
! --        if (laiC(n) < P_lairesiduel(numcoupe-1)) then
! --          lai(as,n) = P_lairesiduel(numcoupe-1)
! --          lai(ao,n) = P_lairesiduel(numcoupe-1)
! --          laiC(n) = P_lairesiduel(numcoupe-1)
! --        endif
! --      endif


    ! modifs domi - 25-08-97 :
    ! reinitialisation des stress sur les periodes, a voir avec francoise pour la
    ! determination de la periode
      if (nlev > 0 .and. (nlax == 0 .or. n == nlax)) then
        nst1coupe = nst1coupe + 1
        str1coupe = str1coupe + swfac
        stu1coupe = stu1coupe + turfac
        inn1coupe = inn1coupe + inns
        diftemp1coupe = diftemp1coupe + tcult - tmoy
      ! DR 19/09/07  on garde les varaibles pour les ecrire dans rapport pour FR
        str1intercoupe = str1coupe / nst1coupe
        stu1intercoupe = stu1coupe / nst1coupe
        inn1intercoupe = inn1coupe / nst1coupe
        diftemp1intercoupe = diftemp1coupe / nst1coupe
      endif

      if (nlax > 0 .and. (nmat == 0.or.n == nmat)) then
        nst2coupe = nst2coupe + 1
        str2coupe = str2coupe + swfac
        stu2coupe = stu2coupe + turfac
        inn2coupe = inn2coupe + inns
        diftemp2coupe = diftemp2coupe + tcult - tmoy
      ! DR 19/09/07  on garde les varaibles pour les ecrire dans rapport pour FR
        str2intercoupe = str2coupe / nst2coupe
        stu2intercoupe = stu2coupe / nst2coupe
        inn2intercoupe = inn2coupe / nst2coupe
        diftemp2intercoupe = diftemp2coupe / nst2coupe
      endif

return
end subroutine entreLesCoupes

 
 
