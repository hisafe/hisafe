! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 6.3.4, page 105
!>
!! The calculation of crop residues returning to the soil for the following crop, in terms of quantity (QRESSUITE) and quality (CSURNRESSUITE),
!! relies on the parameter "ressuite", defining four possible management practices according to the plant fraction remaining in the field and then incorporated
!! into the soil:
!> - roots (ressuite=”roots”) when harvesting, for example, lettuce or textile flax,
!> - trimmed woody material (ressuite=”prunings”) for vineyards'fan club
!> - straw and fine roots (ressuite=”straw+roots”) when harvesting cereal grains or sugar-beet taproots or potatoes,
!> - stubble and fine roots (ressuite=”stubble+roots”) when harvesting cereal grains and straw together or silage maize or cutting meadow,
!> - whole crop (ressuite=”whole_crop”) corresponding to catch crops, green manure or crop volunteers.
!! Plant residues are assumed to remain at the soil surface until being buriedfP by the following soil tillage, except for pure roots which are assumed
!! to be located between the surface and the PROFHUM depth.
!!
!! In all cases the fine root biomass (MSRAC) calculation is required, which is done differently according to the option chosen for root profile.
!! For the standard profile, root biomass is assumed to be a fixed proportion of shoot biomass.
!!
!! The quantities of residues (QRESSUITE, in t DM ha-1) left on the soil at harvest are calculated.
!! The nitrogen content of the returned residues is CsurNressuite
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine ApportResidusCulture(n,nrec,cumvminr,P_codcueille,ntaille,P_coderacine,masec,P_proprac,P_y0msrac,msrac_prec,  & ! IN
                               magrain,QNplante,QNgrain,P_ressuite,mabois,sioncoupe,lracsentot,zrac, & ! IN
                               P_longsperac,P_profhum,Qminrcult,msrac,qressuite,QNressuite,QCressuite,CsurNressuite, &
                               CNplante,itrav1,itrav2,ires,nbCouches,P_CNresmin,P_CNresmax,nbResidus,P_Qmulchdec,nap,airg, & ! IN
                               Cres,Nres,Cnondec,Nnondec,Cmulchnd,Nmulchnd,Crac,Nrac,QCapp,QNapp,QCresorg,QNresorg, & !INOUT
                               P_awb,P_bwb,P_cwb,P_CroCo,P_akres,P_bkres,P_ahres,P_bhres,Wb,kres,hres,qressuite_tot,  &
                               CsurNressuite_tot,QCrac,QNrac,QNplantefauche)

  implicit none

  integer, intent(IN)    :: n
  integer, intent(IN)    :: nrec  
  real,    intent(IN)    :: cumvminr      !> // OUTPUT // daily mineral nitrogen arising from humus // kgN.ha-1.j-1
  integer, intent(IN)    :: P_codcueille  !> // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0 
  integer, intent(IN)    :: ntaille  
  integer, intent(IN)    :: P_coderacine  !> // PARAMETER // Choice of estimation module of growth root in volume: standard profile (1) or by actual density (2) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: masec         ! masec(n)       // OUTPUT // Aboveground dry matter  // t.ha-1
  real,    intent(IN)    :: P_proprac     !> // PARAMETER // Slope of the relationship (root mass vs shoot mass) at harvest  // g.g-1 // PARAM // 0.20
  real,    intent(IN)    :: P_y0msrac      !> // PARAMETER // minimal amount of root mass at harvest (when aerial biomass is nil) // t.ha-1 // PARAM // 0.7
  real,    intent(IN)    :: msrac_prec    ! msrac(n-1)
  real,    intent(IN)    :: magrain       ! magrain(n)
  real,    intent(IN)    :: QNplante      ! QNplante(n-1)       // OUTPUT // Amount of nitrogen taken up by the plant  // kgN.ha-1
  real,    intent(IN)    :: QNgrain       !> // OUTPUT // Amount of nitrogen in harvested organs (grains / fruits) // kg ha-1
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!! 1 = roots
!!! 2 = whole_crop
!!! 3 = straw+roots
!!! 4 = stubble+roots
!!! 5 = stubble_of_residu_type_9+roots
!!! 6 = stubble_of_residu_type_10+roots
!!! 7 = prunings
  !!!character(len=45), intent(IN)    :: P_ressuite  !> // PARAMETER // Name of residue type (for the next crop) // straw, roots, crops, nothing // PARTEC // 0
  integer, intent(IN)    :: P_ressuite  !> // PARAMETER // Name of residue type (for the next crop) // straw, roots, crops, nothing // PARTEC // 0
  real,    intent(IN)    :: mabois        ! maboisC       // OUTPUT // Prunning dry weight // t.ha-1
  logical, intent(IN)    :: sioncoupe  
  real,    intent(IN)    :: lracsentot    !> // OUTPUT // Total length of senescent roots  // cm root.cm -2 soil
  real,    intent(IN)    :: zrac          !> maximum rooting depth
  real,    intent(IN)    :: P_longsperac  !> // PARAMETER // specific root length // cm g-1 // PARPLT // 1
  real,    intent(IN)    :: P_profhum     !> // PARAMETER // Humification depth  (max.60) // cm // PARSOL // 1
  real,    intent(INOUT) :: Qminrcult  
  real,    intent(INOUT) :: msrac         ! msrac(n)       // OUTPUT // Estimated dry matter of the roots // t.ha-1
  real,    intent(INOUT) :: qressuite     !> // OUTPUT // quantity of residues from the previous crop // kg.ha-1
  real,    intent(INOUT) :: QNressuite    !> // OUTPUT // quantity of N in residues from the previous crop // kg.ha-1
  real,    intent(INOUT) :: QCressuite    !> // OUTPUT // quantity of C in residues from the previous crop // kg.ha-1
  real,    intent(INOUT) :: CsurNressuite
  real,    intent(INOUT) :: CNplante      !> // OUTPUT // Nitrogen concentration of entire plant  // %
  integer, intent(INOUT) :: itrav1  
  integer, intent(INOUT) :: itrav2  
  integer, intent(INOUT) :: ires  
  integer, intent(IN)    :: nbCouches  
  real,    intent(IN)    :: P_CNresmin(nbResidus)     !> // PARAMETER // minimum observed value of ratio C/N of organic residues  // g g-1 // PARAM // 1
  real,    intent(IN)    :: P_CNresmax(nbResidus)     !> // PARAMETER // maximum observed value of ratio C/N of organic residues // g g-1 // PARAM // 1
  integer, intent(IN)    :: nbResidus  
  real,    intent(IN)    :: P_Qmulchdec(nbResidus)     !> // PARAMETER // maximal amount of decomposing mulch // t.ha-1 // PARAM // 1
  integer, intent(INOUT) :: nap                        !> nombre d'apports
  real,    intent(INOUT) :: airg                       ! (n+1)       // OUTPUT // Daily irrigation // mm
  real,    intent(INOUT) :: Cres(nbCouches,nbResidus)
  real,    intent(INOUT) :: Nres(nbCouches,nbResidus)
  real,    intent(INOUT) :: Cnondec(10)      !> // OUTPUT // undecomposable C stock of the type 10 residues on the surface // t.ha-1
  real,    intent(INOUT) :: Nnondec(10)      !> // OUTPUT // undecomposable N stock of the type 10 residues on the surface // kg.ha-1
  real,    intent(INOUT) :: Cmulchnd
  real,    intent(INOUT) :: Nmulchnd
  real,    intent(INOUT) :: Crac      !> // OUTPUT // amount of C in roots at harvest//  kg.ha-1
  real,    intent(INOUT) :: Nrac      !> // OUTPUT // amount of N in roots at harvest//  kg.ha-1
  real,    intent(INOUT) :: QCapp      !> // OUTPUT // cumulative amount of organic C added to soil // kg.ha-1
  real,    intent(INOUT) :: QNapp      !> // OUTPUT // cumulative amount of organic N added to soil // kg.ha-1
  real,    intent(INOUT) :: QCresorg
  real,    intent(INOUT) :: QNresorg
  real,    intent(INOUT) :: QCrac  !> // OUTPUT // cumulative amount of C in dead roots added to soil //  kg.ha-1
  real,    intent(INOUT) :: QNrac  !> // OUTPUT // cumulative amount of N in dead roots added to soil //  kg.ha-1
  real,    intent(INOUT) :: QNplantefauche !> // OUTPUT // cumulative amount of N exported at the cut //  kg.ha-1

  ! ajout Bruno; paramètres de décomposition du residu ires (ici 'feuilles mortes')
!  real,    intent(IN) ::awb
! real,    intent(IN) ::bwb
!  real,    intent(IN) ::cwb
!  real,    intent(IN) ::akres
!  real,    intent(IN) ::bkres
!  real,    intent(IN) ::ahres
!  real,    intent(IN) ::bhres
!  real,    intent(INOUT) ::Wb
!  real,    intent(INOUT) ::kres
!  real,    intent(INOUT) ::hres

  real,    intent(IN)    :: P_awb(nbResidus)       !(:)   // PARAMETER // parameter  of organic residues decomposition: CsurNbio=P_awb+P_bwb/P_CsurNres // SD // PARAM // 1
  real,    intent(IN)    :: P_bwb(nbResidus)       !(:)   // PARAMETER // parameter of organic residues decomposition: CsurNbio=P_awb+P_bwb/P_CsurNres // g g-1 // PARAM // 1
  real,    intent(IN)    :: P_cwb(nbResidus)       !(:)   // PARAMETER // Minimum ratio C/N of microbial biomass in the relationship: CsurNbio=P_awb+P_bwb/P_CsurNres // g g-1 // PARAM // 1
  real,    intent(IN)    :: P_akres(nbResidus)     !(:)   // PARAMETER // parameter of organic residues decomposition: kres=P_akres+P_bkres/P_CsurNres // day-1 // PARAM // 1
  real,    intent(IN)    :: P_bkres(nbResidus)     !(:)   // PARAMETER // parameter of organic residues decomposition: kres=P_akres+P_bkres/P_CsurNres // g g-1 // PARAM // 1
  real,    intent(IN)    :: P_ahres(nbResidus)     !(:)   // PARAMETER // parameter of organic residues humification: hres=1-P_ahres*P_CsurNres/(P_bhres+P_CsurNres) // g g-1 // PARAM // 1
  real,    intent(IN)    :: P_bhres(nbResidus)     !(:)   // PARAMETER // parameter of organic residues humification: hres=1-P_ahres*P_CsurNres/(P_bhres+P_CsurNres) // g g-1 // PARAM // 1
  real,    intent(INOUT) :: Wb(nbResidus)
  real,    intent(INOUT) :: kres(nbResidus)
  real,    intent(INOUT) :: hres(nbResidus)
  real,    intent(IN) :: P_CroCo(nbResidus)     !(:)   // PARAMETER // parameter of organic residues decomposition // SD // PARAM // 1


  real,    intent(OUT)   :: qressuite_tot           !(:)   // bilan // amount of total harvest residues (aerials + roots) // t.ha-1
  real,    intent(OUT)   :: CsurNressuite_tot       !(:)   // bilan // Carbon to Nitrogen ratio of total harvest residues (aerials + roots) // t.ha-1

!: Variables locales
  real    :: Cressuite
  real    :: mspaille  
  real    :: Cracpc
  real    :: CsurNrac  
!  integer :: ihum
!  integer :: iz
!  real    :: Cracine
!!!  real    :: CsurNplante
  real    :: MSveget     ! Bruno 22/05/2012
  real    :: QNveget
  real    :: CNveget
  real    :: Wr
  real    :: msracmorte ! Bruno 08/2012 matière seche de racines mortes à la date de récolte, de taille ou de fauche/ t.ha-1
  real    :: Cracmorte !
  real    :: Nracmorte


 !     ihum  = nint(P_profhum)
 !  Minéralisation cumulée des résidus de la culture après récolte
      if (nrec > 0) then
         Qminrcult = Qminrcult + cumvminr
      endif

   if ((P_codcueille == 1 .and. n == nrec) .or. (P_codcueille == 2 .and. n == ntaille) .or. sioncoupe) then
! BM et EC 08/2012 Calcul des pools de racines totales (mortes et vivantes) pour toutes les plantes mais selon le coderacine
      if (P_coderacine == 1) then
           msrac = masec * P_proprac + P_y0msrac !/profil type
           msracmorte = 0.
      else
           msrac = msrac_prec !/ densite vraie
           msracmorte = lracsentot / P_longsperac * 100.
      endif
!    Biomasse aerienne restituable
        mspaille = masec - (magrain / 100.)
!    MS et N contenus dans les parties vegetatives (pailles + racines) au jour n-1
        MSveget = mspaille+msrac
        QNveget = QNplante-QNgrain
!    Concentration en N des parties végétatives (pailles + racines). On suppose que :
!      1) QNplante inclut l'azote des racines
!      2) la teneur en N des racines = celle des pailles
!      3) les racines sont toutes dans la couche [1,min(profhum,zrac)]
        if(MSveget==0.) then
            CNveget= 0.
        else
            CNveget=QNveget/MSveget/10.
        endif
!    C et N contenus dans la biomasse racinaire
        Cracpc=38.
        Crac = msrac * Cracpc * 10.
        Nrac = msrac * CNveget * 10.
        if(Nrac > QNveget) Nrac = QNveget
!BM et EC 04/10/2012 ajout d'une valeur par défaut du CsurNrac dans le cas où Nrac =0
           CsurNrac = 10000.
!BM et EC 04/10/2012 pour éviter les NaN
!  if(Nrac >  0.) CsurNrac = Crac/Nrac
        if(Nrac >  1.e-10) CsurNrac = Crac/Nrac

!: Apport au sol des résidus de récolte et de taille et de coupe (cultures fauchées)
!----------------------------------------------------------------------------------
!   - on distingue residus de racines (msrac à la récolte ou msracmorte à la fauche) et de parties aeriennes (qressuite)
!   - on considere que les residus de parties aeriennes apres recolte sont d'abord en mulch (dans le 1er cm de sol)
!      25/08/08 DR et NB on a ajouté l'incorporation des bois de taille le jour de la taille
!   - et les residus racinaires sont directement incorporés au sol

!!!MODIF HISAFE 11 : Modif après détection bug
!!!if (sioncoupe) then
   if (sioncoupe .eqv. .TRUE.) then
        qressuite = 0.
   else
        qressuite     = 0.
        QNressuite    = 0.
        CsurNressuite = 0.
        Cressuite    = 42.         !- Hypothèse: teneur en carbone des residus de culture = 42% MS
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!! 1 = roots
!!! 2 = whole_crop
!!! 3 = straw+roots
!!! 4 = stubble+roots
!!! 5 = stubble_of_residu_type_9+roots
!!! 6 = stubble_of_residu_type_10+roots
!!! 7 = prunings
 !  1er cas:  restitution de la plante entiere  (plante jeune type P_engrais vert)
!!!        if (index(P_ressuite , 'whole_crop')>0 ) then


        if (P_ressuite == 2) then
           qressuite = masec
           QNressuite = QNplante-Nrac
           ires = 2
        endif
 !  2eme cas : restitution des pailles
 !!!       if (index(P_ressuite , 'straw+roots')>0) then
        if (P_ressuite == 3) then
           qressuite = mspaille
           QNressuite = QNveget-Nrac
           ires = 1
        endif
 !  2eme cas bis : restitution des pailles d'un residu de type 9
 !!!       if (index(P_ressuite , 'stubble_of_residu_type_9+roots')>0) then
        if (P_ressuite == 5) then
           qressuite = mspaille
           QNressuite = QNveget-Nrac
           ires = 9
        endif
 !  2eme cas ter : restitution des pailles d'un residu de type 10
 !!!       if (index(P_ressuite , 'stubble_of_residu_type_10+roots')>0) then
        if (P_ressuite == 6) then
           qressuite = mspaille
           QNressuite = QNveget-Nrac
           ires = 10
        endif
!   3eme cas : restitution des bois !
!!!        if (index(P_ressuite , 'prunings')>0) then
        if (P_ressuite == 7) then
           qressuite = mabois
           QNressuite = mabois * 0.5 * 10.
           ires = 8  ! code res à verifier
        endif
!   4eme cas : restitution des chaumes = 35% des pailles
!!!        if (index(P_ressuite , 'stubble+roots')>0) then
        if (P_ressuite == 4) then
           qressuite = 0.35 * mspaille
           QNressuite = 0.35*(QNveget-Nrac)
           ires = 1
        endif

!  **** Restitution des parties aeriennes (en surface du sol)
!!!        if (P_ressuite /= 'roots') then

        if (P_ressuite > 1) then
           Cressuite = 42.    ! Hypothèse: teneur en carbone des parties aériennes = 42% MS
           QCressuite = qressuite * Cressuite * 10.
!BM et EC 04/10/2012 ajout d'une valeur par défaut du CsurNressuite dans le cas où QNressuite =0
           CsurNressuite = 10000.
!BM et EC 04/10/2012 pour éviter les NaN
!           if (QNressuite > 0.) CsurNressuite = qressuite * Cressuite / QNressuite * 10.
           if (QNressuite > 1.e-10) CsurNressuite = qressuite * Cressuite / QNressuite * 10.
!           itrav1=1
!           itrav2=1
           call ResidusApportSurfaceSol(qressuite,Cressuite,-1.e-10,ires,P_CNresmin(ires),P_CNresmax(ires),    & ! IN
                          1.,1.,P_qmulchdec(ires),nbCouches,nbResidus,nap,airg,CsurNressuite, Cnondec(1:10),Nnondec(1:10), &
                          Cres(1:nbCouches,1:nbResidus),Nres(1:nbCouches,1:nbResidus),                                     &
                          QCapp,QNapp,QCresorg,QNresorg) ! INOUT
           Wr=0.
               if (CsurNressuite > 0.) Wr=1./CsurNressuite
               call ResiduParamDec(P_awb(ires),P_bwb(ires),P_cwb(ires),P_CroCo(ires),P_akres(ires),P_bkres(ires), &
                             P_ahres(ires),P_bhres(ires),Wr,P_CNresmin(ires),P_CNresmax(ires),Wb(ires),kres(ires),hres(ires))
        endif

!  Mise a jour des pools (pailles + racines)
!        qressuite  = qressuite + msrac
!        QNressuite = QNressuite + Nrac
!EC et BM 02/08/2012 pool déjà calculé == Cres(i,21)- pas besoin
!        Cracine = 0.
!        do iz = 1,ihum
!           Cracine = Cracine + Cres(iz,nbResidus) ! Bruno type residus "racine" = nbResidus et pas 10
!        end do
!DR et EC 25/07/2012 Mise a jour des pools (residus aeriens + racines)
!*** Cas 1 : la taille ! BM et EC 08/2012 dans le cas d'une taille on n'apporte pas de racines
!-------------------------------------------
        if (n == ntaille) then
          qressuite_tot = qressuite
          CsurNressuite_tot = CsurNressuite
        else
!*** Cas 2 : la récolte ! restitution obligatoire des racines (sur la profondeur P_profhum) et mise à jour des pools
!-------------------------------------------
        ires = nbResidus
!        itrav1 = 1
!        itrav2 = ihum
           call ResidusApportSurfaceSol(msrac,Cracpc,-1.e-10,ires,P_CNresmin(ires),P_CNresmax(ires),              & ! IN
              P_profhum,zrac,P_qmulchdec(ires),nbCouches,nbResidus,nap,airg,CsurNrac,Cnondec(1:10),Nnondec(1:10), &
              Cres(1:nbCouches,1:nbResidus),Nres(1:nbCouches,1:nbResidus),                                        &
                          QCapp,QNapp,QCresorg,QNresorg)  ! INOUT
           Wr=0.
               if (CsurNrac > 0.) Wr=1./CsurNrac
              call ResiduParamDec(P_awb(ires),P_bwb(ires),P_cwb(ires),P_CroCo(ires),P_akres(ires),P_bkres(ires), &
                             P_ahres(ires),P_bhres(ires),Wr,P_CNresmin(ires),P_CNresmax(ires),Wb(ires),kres(ires),hres(ires))

          qressuite_tot  = qressuite + msrac
          CsurNressuite_tot = (QCressuite+Crac)/(QNressuite+Nrac)
          QCrac = QCrac + Crac
          QNrac = QNrac + Nrac
          msrac = 0.
          Crac = 0.
          Nrac = 0.
        endif
   endif

   endif

!*** Cas 3 : Apport des résidus racinaire suite à la fauche
! --------------------------------------------------------------
! BM et EC 02/08/2012 la fauche a lieu quelquesoit le coderacine
!   if(sioncoupe .and. P_coderacine == 2) then
!!!MODIF HISAFE 11 : Modif après détection bug
!!!if (sioncoupe) then
    if (sioncoupe .eqv. .TRUE.) then
!        Cressuite = 38.    ! Hypothèse: teneur en carbone des racines = 38% MS Cracpc déjà défini plus haut
        ires = nbResidus
!        itrav1 = 1 EC pas besoin?
!        itrav2 = ihum EC pas besoin?
!        if (CNplante > 0.) CsurNplante =  Cressuite / CNplante  ! on suppose C/N racines = C/N plante entiere EC pourquoi ne pas prendre CsurNrac?
        call ResidusApportSurfaceSol(msracmorte,Cracpc,-1.e-10,ires,P_CNresmin(ires),P_CNresmax(ires),                       & ! IN
                         P_profhum,zrac,P_qmulchdec(ires),nbCouches,nbResidus,nap,airg,CsurNrac,Cnondec(1:10),Nnondec(1:10), &
                         Cres(1:nbCouches,1:nbResidus),Nres(1:nbCouches,1:nbResidus),                                        &
                         QCapp,QNapp,QCresorg,QNresorg) ! INOUT

        Wr=1./CsurNrac
        call ResiduParamDec(P_awb(ires),P_bwb(ires),P_cwb(ires),P_CroCo(ires),P_akres(ires),P_bkres(ires), &
                             P_ahres(ires),P_bhres(ires),Wr,P_CNresmin(ires),P_CNresmax(ires),Wb(ires),kres(ires),hres(ires))

! BM et EC 02/08/2012 mise à jour des pools racinaires des cultures fauchées
         msrac = msrac - msracmorte
         Cracmorte = msracmorte * Cracpc * 10.
         Nracmorte = Cracmorte / CsurNrac
         Crac = Crac - Cracmorte
         Nrac = Nrac - Nracmorte
         QCrac = QCrac + Cracmorte
         QNrac = QNrac + Nracmorte
         qressuite_tot = qressuite_tot + msracmorte
         CsurNressuite_tot = QCrac / QNrac
! EC 07/08/2012 mise à jour du pool de N exporté à la fauche - ce qui est racinaire est restitué donc doit être déduit du QNplantefauche
         QNplantefauche = QNplantefauche - Nracmorte

   endif


return
end subroutine ApportResidusCulture
 
 
