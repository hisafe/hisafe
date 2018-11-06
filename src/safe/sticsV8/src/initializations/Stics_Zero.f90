!!!MODIF HISAFE 8 : suppression de l'objet USM
!!!subroutine Stics_Zero(sc,usma)
subroutine Stics_Zero(sc)

USE Stics
!!!USE USM

    implicit none

    type(Stics_Communs_), intent(INOUT) :: sc  

    ! dr 17/07/21012 ajourt de usma pour lecture fichiers pour optimistisc
    !!!type(USM_), intent(INOUT) :: usma


  sc%ipl = 0


  !!!MODIF HISAFE 1 : suppression des chaines de caractères
  !!!sc%mois = '***'
  sc%jour = 0
  sc%nummois = 0
  !!!sc%ancours = 0
  !!!sc%P_ichsl = 0
  sc%P_iwater = 0
  sc%P_ifwater = 0
  sc%ifwater_courant = 0
  sc%ifwater0 = 0
  sc%dernier_n = 0
  sc%n = 0
  !nbjmax = 731        ! taille des tableaux temporels (731 pour l'instant)
  sc%jjul = 0
  sc%jul = 0
  sc%nbans = 0
  sc%nbjanrec = 0
  sc%nstoc = 0
  sc%numcult = 0
  sc%ens = 0
  sc%fichist = 0
  sc%ficsta = 0
  sc%ficdbg = 0
  !!!sc%P_culturean = 0
  sc%codoptim = 0
 !!! sc%P_codesuite = 0
  sc%nbjsemis = 0
  sc%maxwth = 0
  sc%nstoprac = 0

  sc%numdate = 0
  sc%bouchon = 0
  sc%nouvdensrac = 0
  sc%nbCouchesSol = 0

  sc%nappmulch = 0
  sc%ires = 0
  sc%itrav = 0

  sc%ansemis = 0
  sc%anrecol = 0
  sc%annee(:) = 0
  sc%NH = 0

  sc%codeprofil = 0

  sc%nbjrecol = 0
  sc%nbjrecol0 = 0
  sc%NHE = 0
  sc%napini = 0
  sc%napNini = 0
  sc%nbjresini = 0
  sc%faucheannule = 0
!  sc%nbjpourdecisemis(:) = 0
!  sc%nbjpourdecirecolte(:) = 0

!!!MODIF HISAFE 5 : suppression variable inutile
!!!  sc%Ninitf(:) = 0.0
!!!  sc%P_Hinitf(:) = 0.0
  sc%delta = 0.0

  sc%devjourfr = 0.0

  sc%esz(:) = 0.0
  sc%fdens = 0.0
  sc%tustress = 0.0

  sc%rdif = 0.0
  sc%originehaut = 0.0
  sc%tairveille = 0.0

!  declaration pour sp senescen  18/03/98
!  sc%    coefamsres = 0.0
  sc%coefbmsres = 0.0
  sc%coefaifres = 0.0
  sc%coefbifres = 0.0
  sc%coefcifres = 0.0
  sc%a = 0.0
  sc%effN = 0.0
  sc%hi = 0.0
  sc%ha = 0.0
  sc%hpf = 0.0
  sc%rglo = 0.0
  sc%hurlim = 0.0
  sc%rnetS = 0.0
  sc%rnet = 0.0
  sc%albedolai = 0.0
  sc%resmes = 0.0
! ** Domi - 25/10/2004 - j'ai mis 0 à dacouche sinon pb le jour de la recolte
! *- dans densirac (si codeculture = feuille)
  sc%dacouche(:) = 0.0
  sc%ruisselt = 0.0
  sc%infilj(:) = 0.0
  sc%exces(:) = 0.0
  sc%anox(:) = 0.0
  sc%pluieruissel = 0.0
  sc%sat(:) = 0.0
  sc%cpreciptout = 0.0
  sc%ruissel = 0.0
  sc%QeauI = 0.0
  sc%QeauFS = 0.0
  sc%Qeau0 = 0.0
  sc%doi = 0.0
  sc%Edirect = 0.0
  sc%humidite = 0.0
  sc%mouillmulch = 0.0
  sc%Emulch = 0.0
  sc%intermulch = 0.0
  sc%cintermulch = 0.0
  sc%ruisselsurf = 0.0
  sc%ras = 0.0
  sc%Nvolat = 0.0
  sc%eptcult = 0.0
  sc%TcultMin = 0.0
  sc%TcultMax = 0.0
  sc%dessecplt = 0.0

! *- pour le calcul de la densité equivalente
!DR 12/09/2012 devenu inutiles les densite equivalenets sont dans la structure plante
!  sc%dassoiniteqv = 0.0
!  sc%dassoinit = 0.0

  sc%eo = 0.0
  sc%eos = 0.0
  sc%Ratm = 0.0
  sc%hres(:) = 0.0
  sc%Wb(:) = 0.0
  sc%kres(:) = 0.0
  sc%NCbio = 0.0
  sc%saturation = 0.0
  sc%qmulch = 0.0
  sc%couvermulch = 0.0
!  sc%albedomulch = 0.0
!!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
!!!  sc%Ninit(:) = 0.0
!!!  sc%Hinit(:) = 0.0
!!!  sc%HR(:) = 0.0

  sc%azomes = 0.0
  sc%ammomes = 0.0
  sc%FsNH3 = 0.0
  sc%RsurRU = 0.0
  sc%DRAT = 0.0
  sc%QNdrp = 0.0
  sc%esol = 0.0
  sc%et = 0.0
  sc%tnhc = 0.0
  sc%tnrc = 0.0
  sc%pluieN = 0.0
  sc%irrigN = 0.0
  sc%precip = 0.0
  sc%precipN = 0.0
  sc%cumoffrN = 0.0
  sc%cumoffrN0 = 0.0
  sc%cumoffrN100 = 0.0
  sc%azorac0 = 0.0
  sc%azorac100 = 0.0
  sc%demandebrute = 0.0
  sc%absodrp = 0.0
  sc%cpluie = 0.0
  sc%Chumt = 0.0
  sc%Chumt0 = 0.0
  sc%Nhuma = 0.0
  sc%Nhuma0 = 0.0
  sc%Nhumi = 0.0
  sc%Nhumt = 0.0
  sc%Nhumt0 = 0.0
  sc%Cr = 0.0
  sc%Nr = 0.0
  sc%Cb = 0.0
  sc%Nb = 0.0
  sc%etm = 0.0
  sc%precipamm = 0.0
 !!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
 !!! sc%P_NH4initf(:) = 0.0
 !!! sc%NH4init(:) = 0.0
  sc%eaunoneffic = 0.0
  sc%toteaunoneffic = 0.0
  sc%raamax = 0.0
  sc%raamin = 0.0
  sc%laiTot = 0.0
  sc%stemflowTot = 0.0
  sc%EmdTot = 0.0
  sc%epTot = 0.0
  sc%hauteurMAX = 0.0

! DR 13/11/06 on met sur 1000 comme le reste
  sc%Chum(:) = 0.0
  sc%Nhum(:)   = 0.
  sc%Cres(:,:) = 0.0
  sc%Nres(:,:) = 0.0
  sc%Cnondec (:) = 0.
  sc%Nnondec(:)  = 0.
  sc%Cmulchnd    = 0.    ! Bruno- nouvelles variables
  sc%Nmulchnd    = 0.
  sc%Cmulch0   = 0.
  sc%Nmulch0   = 0.
  sc%Cbio(:,:) = 0.0
  sc%Nbio(:,:) = 0.0
  sc%xmlch1 = 0.0
  sc%xmlch2 = 0.0
  sc%supres = 0.0
  sc%stoc = 0.0
  sc%cestout = 0.0
  sc%pfz(:) = 0.0
  sc%etz(:) = 0.0
  sc%parapluieetz = 0.0
  sc%totapN = 0.0
  sc%Qminh = 0.0
  sc%Qminr = 0.0
  ! DR 06/09/2011 on ajoute une varaible
  sc%cum_immob = 0.

  sc%QLES = 0.0
!!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
!!!  sc%TS(:) = 0.0
  sc%totapNres = 0.0
  sc%Qnitrif = 0.0
  sc%tcult = 0.0
  sc%tcultveille = 0.0
  sc%tsol(:) = 0.0
  sc%tsolveille(:) = 0.0
  sc%HUR(:) = 0.0
  sc%hurmini(:) = 0.0
  sc%HUCC(:) = 0.0
  sc%HUMIN(:) = 0.0
!!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
!!!  sc%AZamm(:) = 0.0
  sc%effamm = 0.0

  sc%tauxcouv(:) = 0.0
! ** pour thomas - 27/01/2004 - on passe azsup dans le common pour le calcul de combithomas
  sc%azsup = 0.0
! * pour solnu
  sc%smes02 = 0.0
  sc%sumes0 = 0.0
  sc%sumes1 = 0.0
  sc%sumes2 = 0.0
  sc%sesj0 = 0.0
  sc%ses2j0 = 0.0
  sc%sum2 = 0.0
  sc%esreste = 0.0
  sc%esreste2 = 0.0

! * pour lixiv
  sc%drain = 0.0
  sc%lessiv = 0.0

! * pour offrnodu et lecsorti
  sc%fxa = 0.0
  sc%fxn = 0.0
  sc%fxt = 0.0
  sc%fxw = 0.0

! * tableau pour le cumul des absorptions (voir perteng.for)
  sc%absoTot(:) = 0.0

! * on garde les unites froids pour l'enchainement des perennes dans recup.tmp
  sc%cu0(:) = 0.0
  sc%somelong0(:) = 0.0
  sc%nfindorm0(:) = 0
! 191206 Dr ET sAMUEL y'avait un soucis avec la valeur de vmax qu'on risque de perdre
  sc%vmax = 0.0
  sc%cumdltaremobilN = 0.0

  sc%posibsw = .FALSE.
  sc%posibpe = .FALSE.
  !!!MODIF HISAFE 6 : on remplace les tableaux de boolean
 !!! sc%repoussesemis(:) = .FALSE.
 !!! sc%repousserecolte(:) = .FALSE.
sc%repoussesemis(:) = 0
sc%repousserecolte(:) = 0
  sc%recolte1 = .FALSE.
  sc%P_datefin = .FALSE.

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  sc%codeversion = '*******'
!!!  sc%P_codesimul = '*******'
!!!  sc%P_wdata1 = '*******'
!!!  sc%P_wdata2 = '*******'
!!!  sc%P_usm = '*******'
!!!  sc%wlieu = '*******'


! ** PARAMETRES TECHNIQUES AUTORISéS CULTURE PURE SEULEMENT
! *- culture sous abri

! DR 27/06/2013 ces parametres n'ont rien à faire la , ils sont deja declarés dans la structure itk
!  sc%P_codabri = 0
!  sc%P_julouvre2 = 0
!  sc%P_julouvre3 = 0
  sc%nouvre2 = 0
  sc%nouvre3 = 0
!  sc%P_surfouvre1 = 0.0
!  sc%P_surfouvre2 = 0.0
!  sc%P_surfouvre3 = 0.0
!  sc%P_transplastic = 0.0


! *- apports
  sc%naptot = 0
  sc%napNtot = 0
  sc%anit(:) = 0.0
  ! DR 06/04/2016 pour plusieurs ferti
  sc%anit_uree(:)=0.0
  sc%type_ferti(:)=0
  sc%airg(:) = 0.0
  sc%totir = 0.0

  sc%CO2res = 0.0
  sc%CO2hum = 0.0
  sc%CO2sol = 0. ! Bruno déplacés depuis Sol
  sc%QCO2sol = 0. !Bruno- initialisations ; ajout 22/05/2012
  sc%QCO2hum = 0.
  sc%QCO2res = 0.
  sc%QCO2mul = 0.
  sc%QCprimed = 0.
  sc%QNprimed = 0.

! DR 26/11/07
  sc%tmoy_an(:,:) = 0.0
  sc%Tm_histo = 0.0
  sc%deltat_an(:) = 0.0
! DR 28/11/07
  sc%tm_glisse(:) = 0.0
  sc%deltaT_adaptCC(:) = 0.0

  sc%var_trefh(:) = 0.0
  sc%var_trefr(:) = 0.0
  sc%var_tnitmin(:) = 0.0
  sc%var_tnitmax(:) = 0.0
  sc%var_tnitopt(:) = 0.0
  sc%var_tnitopt2(:) = 0.0
  sc%var_TREFdenit1(:) = 0.0
  sc%var_TREFdenit2(:) = 0.0
  sc%var_TREFfhum(:) = 0.0
  sc%var_FTEM(:) = 0.0
  sc%var_FTEMr(:) = 0.0

!!!MODIF HISAFE 1 : suppression des chaines de caractères
 !!! sc%fplt_ori(:) = '*******'
 !!! sc%codeplante_ori(:) = '***'

 !!!MODIF HISAFE 6 : on supprime les tableaux de booléen
 !!! sc%plante_ori(:) = .FALSE.
  sc%plante_ori = .FALSE.
  sc%iplt_ori(:) = 0

  sc%Qem_N2O = 0.0
  sc%em_N2O = 0.0
  sc%Qem_N2Onit = 0.0
  sc%em_N2Onit = 0.0
  sc%Qem_N2Oden = 0.0
  sc%em_N2Oden = 0.0

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  sc%sys = '******************'
!!!  sc%nomplante = '**********'

!  sc%profextN(:) = 0.0
!  sc%profexteau(:) = 0.0
!  sc%age_prairie(:) = 0
  sc%nbcoupe_reel(:) = 0

! DR 26/02/08 pour climator prairie on ne gere pas idem l'annee de semis
  sc%nbcoupe_an1(:) = 0
  sc%julfauche_an1(:,:) = 0
  sc%lairesiduel_an1(:,:) = 0.0
  sc%hautcoupe_an1(:,:) = 0.0
  sc%msresiduel_an1(:,:) = 0.0
  sc%anitcoupe_an1(:,:) = 0.0
  sc%tempfauche_an1(:,:) = 0.0
  sc%tempfauche_ancours_an1(:,:) = 0.0
! DR 26/02/08  ... et les autres
  sc%nbcoupe_an2(:) = 0
  sc%julfauche_an2(:,:) = 0
  sc%lairesiduel_an2(:,:) = 0.0
  sc%hautcoupe_an2(:,:) = 0.0
  sc%msresiduel_an2(:,:) = 0.0
  sc%anitcoupe_an2(:,:) = 0.0
  sc%tempfauche_an2(:,:) = 0.0
  sc%tempfauche_ancours_an2(:,:) = 0.0
! 29/03/2016
  sc%restit_an1(:,:) = 0
  sc%mscoupemini_an1(:,:) = 0.0
  sc%restit_an2(:,:) = 0
  sc%mscoupemini_an2(:,:) = 0.0

! DR 03/03/02  sorties climator
  sc%irrigjN = 0.0
  sc%precipjN = 0.0
!  sc%Nexporte(:) = 0.0
!  sc%Nrecycle(:) = 0.0
!  sc%MSexporte(:) = 0.0
!  sc%MSrecycle(:) = 0.0
!  sc%p1000grain(:) = 0.0
  sc%apport_mini_semis = 0.0

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  sc%nom_variete = '********'


!DR 05/03/08 iplt devient une variable car calculé par decision semis
! le P_iplt0 ets lu dans lectech
  sc%iplt(:) = 0


!  sc%somudevair(:) = 0.0
!  sc%somudevcult(:) = 0.0
!  sc%somupvtsem(:) = 0.0

  sc%iwater0 = 0
  sc%ansemis0=0
  sc%iwaterapres = 0
  sc%ifwaterapres = 0
  sc%nbjsemis0 = 0

  sc%iwater_cultsuiv = 0
  sc%ifwater_cultsuiv = 0

  sc%beta_sol(:) = 0.0

!  sc%offrN(1000) = 0.0
!  sc%absz(1000) = 0.0
  sc%offrN(:) = 0.0
  sc%absz(:) = 0.0

  sc%nodn = 0.0
  ! TODO: réfléchir quant à savoir si trosemax reste variable locale
  ! de humheure ou variable globale de Stics_Communs_ ou Climat_ ou autre.
  !!!MODIF HISAFE 2 : reduction dimension temporelle
  !!!sc%trosemax(0:731) = 0.0
  sc%trosemax(:) = 0.0


!: Les variables liées aux écritures/sorties
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  sc%valprof = '*******'
!!!  sc%valrap(:) = '********'
!!!  sc%valpar(:) = '********'    !> stockage des noms des variables de sortie

  sc%numdateprof(:) = 0
  sc%numdebprof(:) = 0
  sc%dateprof(:,:) = 0
  sc%nbvarsortie = 0
  sc%nbvarrap = 0

  sc%ecritrap = .FALSE.

!!!MODIF HISAFE 5 : suppression variable inutile
 !!! sc%tabprof(:,:,:) = 0.0
 !!! sc%valsortie(:) = 0.0
 !!! sc%valsortierap(:) = 0.0
! 14/09/2011 pour AgMIP un rapport special
!!!  sc%valsortie_flo(:) = 0.0
 !!! sc%valsortie_mat(:) = 0.0
! 14/09/2011 pour macsur un rapport special
 !!! sc%valsortie_iplt(:) = 0.0

  sc%QH2Of = 0.0

  !!!MODIF HISAFE 1 : suppression des chaines de caractères
  !!!sc%staderap(:) = '********'

  sc%codeaucun = 0
  sc%codeenteterap = 0
  sc%codeenteterap_agmip = 0
  sc%codetyperap = 0
! DR 27/062013 j'augmente le nombre de dates pour le fichier rapport , je le passe de 20 à 366 dates
!!!MODIF HiSAFE 9 : suppression code inutile
!!!  sc%daterap(366) = 0
  ! DR 11/03/2014 ajout pour les cultures sur 2 ans
!!!  sc%date_calend_rap(366,3) = 0
!!!  sc%nboccurrap = 0
!!!  sc%nboccurstd = 0

  sc%raplev = .FALSE.
  sc%rapamf = .FALSE.
  sc%raplax = .FALSE.
  sc%rapflo = .FALSE.
  sc%rapdrp = .FALSE.
  sc%raprec = .FALSE.
  sc%rapsen = .FALSE.
  sc%rapfin = .FALSE.
  sc%rapplt = .FALSE.
  sc%rapger = .FALSE.
  sc%rapdebdes = .FALSE.
  sc%rapdebdebour = .FALSE.
  sc%rapmat = .FALSE.
  sc%rapdebdorm = .FALSE.
  sc%rapfindorm = .FALSE.
  sc%rapdeb = .FALSE.
  ! DR 25/07/2013 ajout de flag pour ecriture le premier jour
  sc%start_rap=.FALSE.

! DR et ML et BM et EJ 22/06/09
! *****************************
! introduction des modifications de BM et Eric Justes
! ** calcul de RsurRU
  sc%RU = 0.0
!  sc%RsurRUrac(:) = 0.0
!  sc%RUrac(:) = 0.0
  sc%concNO3sol(:) = 0.0
! fin DR et ML et BM 22/06/09

! DR et ML et BM et EJ 23/06/09
! *****************************
  sc%FTEMhb = 0.0
  sc%FTEMrb = 0.0

! DR et ML 16/10/09 ajout de variables relatives à la decomposition du mulch (BM)
! DR + Bruno on ajoute des variables de sorties sur les residus
  sc%Cresdec(:)= 0.
  sc%Nresdec(:)= 0.
  sc%Cmuldec(:)= 0.
  sc%Nmuldec(:)= 0.
  sc%Cmulch   = 0.
  sc%Nmulch   = 0.
 ! DR 11/12/2013 ajout des varaibles cumulées depuis le semis (pour Macsur)
  sc%drain_from_plt = 0.
  sc%runoff_from_plt = 0.
  sc%leaching_from_plt = 0.
  sc%Nmineral_from_plt =0.
  sc%Nvolat_from_plt = 0.
  sc%QNdenit_from_plt = 0.

! dr 16/12/2013 ajout de res_dispo_profmes par plante
  sc%SoilAvW = 0
  sc%SoilWatM = 0
  sc%SoilNM = 0
!DR 14022016 pour AgMIP ET
  sc%HR_vol_1_30 = 0.
  sc%HR_vol_31_60 = 0.
  sc%HR_vol_61_90 = 0.
  sc%HR_vol_91_120 = 0.
  sc%HR_vol_121_150 = 0.
  sc%HR_vol_151_180 = 0.
  sc%day_after_sowing = 0
!  sc%Cracine = 0.0

! DR 03/02/2011 on ajoute des varaibles de sorties sur les residus
! DR 04/12/2013 on avait une pb de dimensionnement sur cette varaible qui ne sert plus à rien arghhhhhhhhhhh !!!
!  sc%Ctousresidusparcouche(:)= 0.
!  sc%Ntousresidusparcouche(:)= 0.
  sc%Ctousresidusprofil = 0.
  sc%Ntousresidusprofil = 0.
  sc%Cresiduprofil(:)= 0.
  sc%Nresiduprofil(:)= 0.

! dr 07/09/2011
!!!  sc%chaine_debut_ligne= '    '
  ! DR 30/03/2016   pour les vaches
  sc%flag_onacoupe=.FALSE.
  ! DR 07/04/2016 nouvealles varaibles pour pature
  sc%CsurNres_pature =0.
  sc%qres_pature=0.

! DR 17/07/2012 on initialise les methode pour lecture des noms de fichiers pour Optimistics dans travail.usm

!!!MODIF HISAFE 8 : suppression de l'objet USM
!!!  usma%file_Method_Optimistics_Station = 0
!!!  usma%file_Method_Optimistics_Init = 0
!!!  usma%file_Method_Optimistics_Tec(:) = 0
!!!  usma%file_Method_Optimistics_Plt(:) = 0
!!!  usma%file_Method_Optimistics_Sol = 0
!!!  usma%file_Method_Optimistics_lai(:) = 0

end subroutine Stics_Zero


 
