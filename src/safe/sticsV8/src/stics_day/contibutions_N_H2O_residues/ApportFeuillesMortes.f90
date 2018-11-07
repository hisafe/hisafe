subroutine ApportFeuillesMortes(fstressgel,CNplante,P_abscission,inn,P_parazofmorte,dltamsen, &
                               P_codedyntalle,mortmasec,mortreserve,surf,                     &
                               CNresmin,CNresmax,nbCouchesSol,nbResidus,Qmulchdec,      &
                               dltamstombe,mafeuiltombe,QNplante,QNplantetombe,nap,airg,      &
                               itrav1,itrav2,ires,Cres,Nres,Cnondec,Nnondec,                      &
                               resperenne,QCplantetombe,Cmulchnd,Nmulchnd,QCapp,QNapp,QCresorg,QNresorg, &
                               awb,bwb,cwb, CroCo,akres,bkres,ahres,bhres,Wb,kres,hres) ! param de decomposition du residu
! *---------------------------------------------------------------------------------------------------------------------------------------------------------
!> Addition of leaves falling from the plant onto the soil surface
!> - Stics book paragraphe 6.3.3, page 105
!>
!! Leaves falling onto the soil (the proportion of senescent leaves falling is ABSCISSIONP) during crop growth are taken into account by the model
!! as this phenomenon can be important (e.g. rapeseed due to winter frost). Their decomposition at the soil surface is simulated by the decomposition module
!! (see ResidusDecomposition.f90 and mineral.f90) as residues of young plants (category 2). The C/N ratio of leaves when they fall off is calculated from
!! the nitrogen nutrition index of the whole crop and relies on a plant parameter (PARAZOFMORTE), as proposed by Dorsainvil (2002).
! *---------------------------------------------------------------------------------------------------------------------------------------------------------
  implicit none

  real,    intent(IN)    :: fstressgel     !> // OUTPUT // Frost index on the LAI // 0-1
  real,    intent(IN)    :: CNplante       !> // OUTPUT // Nitrogen concentration of entire plant  // %
  real,    intent(IN)    :: P_abscission   !> // PARAMETER // senescent leaf proportion falling on the soil // SD // PARPLT // 1
  real,    intent(IN)    :: inn            !> // OUTPUT // Nitrogen nutrition index (satisfaction of nitrogen needs ) // 0-1
  real,    intent(IN)    :: P_parazofmorte !> // PARAMETER // parameter of proportionality between the C/N of dead leaves and the INN // SD // PARPLT // 1
  real,    intent(IN)    :: dltamsen       !> // OUTPUT // Senescence rate // t ha-1 d-1
  integer, intent(IN)    :: P_codedyntalle !> // PARAMETER // Activation of the module simulating tiller dynamic: yes (1), no (2) // code 1/2 // PARAMV6/PLT // 0
  real,    intent(IN)    :: mortmasec      !> // OUTPUT // Dead tiller biomass  // t.ha-1
  real,    intent(IN)    :: mortreserve    !> // OUTPUT // Reserve biomass corresponding to dead tillers // t.ha-1.d-1
  real,    intent(IN)    :: surf      !> // OUTPUT // Fraction of surface in the shade // 0-1
  real,    intent(IN)    :: CNresmin  !> // PARAMETER // minimum observed value of ratio C/N of organic residue ires  // g g-1 // PARAM // 1
  real,    intent(IN)    :: CNresmax  !> // PARAMETER // maximum observed value of ratio C/N of organic residue ires // g g-1 // PARAM // 1
  integer, intent(IN)    :: nbCouchesSol
  integer, intent(IN)    :: nbResidus
  real,    intent(IN)    :: Qmulchdec !> // PARAMETER // maximal amount of decomposing mulch of residue ires// t C.ha-1 // PARAM // 1
  real,    intent(INOUT) :: dltamstombe  
  real,    intent(INOUT) :: mafeuiltombe    !> // OUTPUT // Dry matter of fallen leaves // t.ha-1
  real,    intent(INOUT) :: QNplante        !> // OUTPUT // Amount of N taken up by the whole plant // kg.ha-1
  real,    intent(INOUT) :: QCplantetombe !> // OUTPUT // cumulative amount of C in fallen leaves // kg.ha-1
  real,    intent(INOUT) :: QNplantetombe  
  integer, intent(INOUT) :: nap !> nombre d'apports  
  real,    intent(INOUT) :: airg ! !ATTENTION! n+1       // OUTPUT // Daily irrigation // mm
  integer, intent(INOUT) :: itrav1  
  integer, intent(INOUT) :: itrav2  
  integer, intent(INOUT) :: ires  
  real,    intent(INOUT) :: Cres(nbCouchesSol,nbResidus) ! première couche, itrav1 = 1 & itrav2 = 1
  real,    intent(INOUT) :: Nres(nbCouchesSol,nbResidus) ! première couche, itrav1 = 1 & itrav2 = 1
  real,    intent(INOUT) :: QCapp      !> // OUTPUT // cumulative amount of organic C added to soil // kg.ha-1
  real,    intent(INOUT) :: QNapp      !> // OUTPUT // cumulative amount of organic N added to soil // kg.ha-1
  real,    intent(INOUT) :: Cnondec(10)!> // OUTPUT // undecomposable C stock of the type 10 residues on the surface // t.ha-1
  real,    intent(INOUT) :: Nnondec(10)!> // OUTPUT // undecomposable N stock of the type 10 residues on the surface // kg.ha-1
  real,    intent(INOUT) :: resperenne !> // OUTPUT // C crop reserve, during the cropping season, or during the intercrop period (for perenial crops) // t ha-1
! Ajout Bruno
  real,    intent(INOUT) :: Cmulchnd
  real,    intent(INOUT) :: Nmulchnd
  real,    intent(INOUT) :: QCresorg
  real,    intent(INOUT) :: QNresorg
! ajout Bruno; paramètres de décomposition du residu ires (ici 'feuilles mortes')
  real,    intent(IN) ::awb
  real,    intent(IN) ::bwb
  real,    intent(IN) ::cwb
  real,    intent(IN) ::akres
  real,    intent(IN) ::bkres
  real,    intent(IN) ::ahres
  real,    intent(IN) ::bhres
  real,    intent(INOUT) ::Wb
  real,    intent(INOUT) ::kres
  real,    intent(INOUT) ::hres
  real,    intent(IN) ::Croco


! Variables locales
  real :: Cfeupc       ! Cfeupc = C content (%) of falling leaves
  real :: CNresid  
  real :: Cplantetombe ! ajout Bruno 22/05/2012
  real :: Wr           !ajout calcul des paramètres de décomposition du résidu "feuilles mortes"
  real :: Nplantetombe
  real :: eaures

  ! ** Décomposition des parties mortes de la plante en surface du sol
  ! *----------------------------------------------------------------
  ! *-     soit toute la plante si elle est entièrement gelée (fstressgel = 0)
  ! *-     soit la partie des feuilles sénescentes qui tombent (P_abscission)

  ! ** Attention: IL FAUDRA RETIRER LES FEUILLES TOMBEES DU BILAN
  ! *-  CNresid = rapport C/N des residus morts
  ! *-  CNresid = C/N plante entière si toute la plante est gelée
  ! *-  CNresid = C/N feuilles mortes si une partie seulement de la plante est gelée
  ! *-  Thèse J.F.Dejoux    (p.75):   CNresid = -48.4*inn + 86
  ! *-  Thèse F. Dorsainvil (p.66) :  CNresid =  13.3/inn    : on prend cette equation

     Cfeupc = 42.
     if (fstressgel <= 0.) then
        dltamstombe  = dltamsen
        CNresid = Cfeupc / CNplante
     else

    ! DR et ML et SYL 15/06/09
    ! ************************
    ! introduction de la fin des modifications de Sylvain (nadine et FR)
    ! dans le cadre du projet PERMED
    ! DR et ML et SYL 16/06/09
    ! on ajoute la condition sur P_codedyntalle
        if (P_codedyntalle == 1) then
          dltamstombe = dltamsen * P_abscission + (mortmasec * surf)
        else
          dltamstombe = dltamsen * P_abscission
        endif
        CNresid = P_parazofmorte / inn
     endif

     if (dltamstombe > 0.) then
        mafeuiltombe = mafeuiltombe + dltamstombe
        itrav1 = 1
        itrav2 = 1

! DR et ML et SYL 15/06/09
! ************************
! introduction de la fin des modifications de Sylvain (nadine et FR)
! dans le cadre du projet PERMED
! ####
! SYL et NB le 22/11/07 pour faire en sorte que les talles (structurelles) plus les
! réserves comprises dans les talles mortes soient minéralisées en profondeur
! DR et ML et SYL 15/06/09
! on ajoute la condition sur P_codedyntalle

! ******************ATTENTION on ne mineralise que dans le premier cm de sol  DR et BM 02/02/2011 *************************
!        if (P_codedyntalle == 1 .and. P_codeplante == 'fou') itrav2=5
! DR et ML et SYL 15/06/09 FIN introduction de la fin des modifications de Sylvain

        ires = 2
        eaures = -1.e-10
        call ResidusApportSurfaceSol(dltamstombe,Cfeupc,eaures,ires,CNresmin,CNresmax,                         & ! IN
                          1.,1.,Qmulchdec,nbCouchesSol,nbResidus,nap,airg,CNresid,Cnondec(1:10),Nnondec(1:10), &
                          Cres(1:nbCouchesSol,1:nbResidus),Nres(1:nbCouchesSol,1:nbResidus),                   &
                          QCapp,QNapp,QCresorg,QNresorg) ! INOUT

              Wr=1./CNresid
              call ResiduParamDec(awb,bwb,cwb,CroCo,akres,bkres,ahres,bhres,Wr,CNresmin,CNresmax,Wb,kres,hres)

        Cplantetombe  = dltamstombe * Cfeupc * 10.   ! ajout Bruno 22/05/2012
        Nplantetombe  = Cplantetombe * Wr
        QCplantetombe = QCplantetombe + Cplantetombe ! ajout Bruno 22/05/2012
        QNplantetombe = QNplantetombe + Nplantetombe
        QNplante = QNplante - Nplantetombe

        ! Dans le cas où QNplantetombe est plus grand que QNplante ; correction -> QNplante=0
!        if (QNplante < 0.) then
!          QNplantetombe = QNplantetombe + QNplante
!          QNplante = 0
!       endif
      endif

! DR et ML et SYL 15/06/09
! **************************
! introduction de la fin des modifications de Sylvain (nadine et FR)
! dans le cadre du projet PERMED
! ####
! SYL réactivation 280207... plus de mortalité de réserves!
! NB et SYL le 22/11/07
! normalemnt les réserves seront mises à jour automatiquement
! dans repartir
! Nb le 09/03/07 mortallité des talles entraine disparition de réserves
! et minéralisation comme feuilles mortes entre 1 et 5 cm
! DR et ML et SYL 15/06/09
! on ajoute la condition sur P_codedyntalle
    if (P_codedyntalle == 1) then
      resperenne = resperenne - (mortreserve * surf)
      if (mortreserve*surf > 0.) then
         itrav1 = 1
         itrav2 = 5
         ires = 2
         call ResidusApportSurfaceSol(mortreserve*surf,Cfeupc,-1.e-10,ires,CNresmin,CNresmax,                  & ! IN
                          1.,1.,Qmulchdec,nbCouchesSol,nbResidus,nap,airg, CNresid,Cnondec(1:10),Nnondec(1:10),&
                          Cres(1:nbCouchesSol,1:nbResidus),Nres(1:nbCouchesSol,1:nbResidus),                   &
                          QCapp,QNapp,QCresorg,QNresorg) ! INOUT

         Wr=0.
           if (CNresid > 0.) Wr=1./CNresid
           call ResiduParamDec(awb,bwb,cwb,CroCo,akres,bkres,ahres,bhres,Wr,CNresmin,CNresmax,Wb,kres,hres)
       endif
    endif

return
end subroutine ApportFeuillesMortes
 
 
