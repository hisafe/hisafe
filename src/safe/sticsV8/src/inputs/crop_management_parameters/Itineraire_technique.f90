!> Module of crop management
!>- Description of the structure ITK_
!>- reading of ITK parameters
module Itineraire_Technique



TYPE , BIND(C) :: ITK_


  integer :: ipl    ! indice associé  
  !DR 19/07/2012 j'allonge le nom du fichier de 25 à 50
  !!!MODIF HISAFE 1 : suppression des chaines de caractères
  !!!character(len=50) :: P_ftec  !< // PARAMETER // name of the technique file // SD // P_USM/USMXML // 0

!  parametres techniques
!***********************
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!! 1 = roots
!!! 2 = whole_crop
!!! 3 = straw+roots
!!! 4 = stubble+roots
!!! 5 = stubble_of_residu_type_9+roots
!!! 6 = stubble_of_residu_type_10+roots
!!! 7 = prunings
!!! character(len=45) :: P_ressuite  !< // PARAMETER // Name of residue type (for the next crop) // straw, roots, crops, nothing // PARTEC // 0
  integer :: P_ressuite  !< // PARAMETER // Name of residue type (for the next crop) // straw, roots, crops, nothing // PARTEC // 0


!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!! 1=snu    7=flo
!!! 2=plt    8=drp
!!! 3=dor    9=des
!!! 4=lev    10=mat
!!! 5=amf    11=rec
!!! 6=lax    12=sen
!!! 13=lan
 !!!  character(len=3) :: P_stadecoupedf  !< // PARAMETER // Stage of automatic cut // * // PARTEC // 0
  integer :: P_stadecoupedf  !< // PARAMETER // Stage of automatic cut // * // PARTEC // 0

  logical :: lecfauche  

  integer :: P_codefauche  !< // PARAMETER // option of cut modes for forage crops: yes (1), no (2) // code 1/2 // PARTEC // 0
  integer :: P_codeaumin  !< // PARAMETER // harvest as a function of grain/fruit water content // code 1/2 // PARTEC // 0
  integer :: P_codetradtec  !< // PARAMETER // description of crop structure with use of radiation transfers: yes (2), non(1) // code1/2 // PARTEC // 0
  integer :: P_codlocirrig  !< // PARAMETER // code of irrigation localisation: 1= above the foliage, 2= below the foliage above the soil, 3 = in the soil // code 1/2/3 // PARTEC // 0
  integer :: P_codlocferti  !< // PARAMETER // code of fertilisation localisation:  1: at the soil surface, 2 = in the soil // code 1/2 // PARTEC // 0
  integer :: P_codemodfauche  !< // PARAMETER // option defining the cut mode: (1) automatic calculation depending on phenologic and trophic state crop,  (2)  calendar pre-established in days,  (3) calendar pre-established in degrees.days // code 1/2/3 // PARTEC // 0
  integer :: P_codeffeuil  !< // PARAMETER // option technique of thinning: yes (1), no (2) // code 1/2 // PARTEC // 0
  integer :: P_codecalirrig  !< // PARAMETER // automatic calculation of irrigation requirements: yes (2), no (1) // code 1/2 // PARTEC // 1
  integer :: P_codestade  !< // PARAMETER // option of forcing for one or several development stages: yes (2), no (1) // code 1/2 // PARTEC // 0
  integer :: P_codcalrogne  !< // PARAMETER // option of the way of calculation of tipping // code 1/2 // PARTEC // 0
  integer :: P_codepaillage  !< // PARAMETER // option: option: 1 = no cover, 2 = plastic cover partly covering the soil   // code 1/2 // PARTEC // 0
  integer :: P_codeclaircie  !< // PARAMETER // option to activate techniques of fruit removal  // code 1/2 // PARTEC // 0
  integer :: P_codcaleffeuil  !< // PARAMETER // option of the way of caluclation of leaf removal // code 1/2 // PARTEC // 0
  integer :: P_codrognage  !< // PARAMETER // option of foliage control // code 1/2 // PARTEC // 0
  integer :: P_codhauteff  !< // PARAMETER // leaf removal heitgh // code 1/2 // PARTEC // 0
  integer :: P_codetaille  !< // PARAMETER // option of pruning // code 1/2 // PARTEC // 0
  integer :: P_codrecolte  !< // PARAMETER // harvest mode : all the plant (1) or just the fruits (2) // code 1/2 // PARTEC // 0
  integer :: P_codcueille  !< // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0

  integer :: P_iplt0  !< // PARAMETER // Date of sowing // julian day // PARTEC // 1
  integer :: P_ilev  !< // PARAMETER // Day of the stage LEV (emergence) when the stage is observed (else 999) // julian day // PARTEC // 1
  integer :: P_iamf  !< // PARAMETER // Day of the stage AMF (maximal of leaf growth , end of juvenile phase ) when the stage is observed (else 999) // * // PARTEC // 1
  integer :: P_ilax  !< // PARAMETER // Day of the stage LAX(maximal leaf area index) when the stage is observed (else 999) // julian day // PARTEC // 1
  integer :: P_idrp  !< // PARAMETER // Day of the stage DRP (beginning of grain filling) when the stage is observed (else 999) // julian day // PARTEC // 1
  integer :: P_isen  !< // PARAMETER // Day of the stage SEN (beginning of net senescence) when the stage is observed (else 999) // julian day // PARTEC // 1
  integer :: P_imat  !< // PARAMETER // Day of the stage MAT (physiological maturity) when the stage is observed (else 999) // julian day // PARTEC // 1
  integer :: P_irec  !< // PARAMETER // Day of the stage REC (harvest) when the stage is observed (else 999) // julian day // PARTEC // 1
  integer :: P_irecbutoir  !< // PARAMETER // Date of harvest  butoir (if the crop has not finished his cycle at this date, the harvest is imposed) // julian day // PARTEC // 1
  integer :: P_variete  !< // PARAMETER // variety number in the technical file // SD // PARTEC // 1
  integer :: nap  
  integer :: napN  
  ! DR 01/04/2016 je discretise les apports uree due au pature et engrais classique
  integer :: napN_uree
  integer :: napN_engrais


!  integer :: napS DR 01/02/2011 on supprime naps pour le remplacer par P_nbjtrav et P_nbjres
  integer :: P_nbjres  !< // PARAMETER // number of residue addition // days // PARTEC // 1
  integer :: P_nbjtrav  !< // PARAMETER // number of cultivations oprerations // days // PARTEC // 1
  integer :: nbcoupe  
  integer :: P_julfauche(20)     !< // PARAMETER // Day in year of each cut (table)  // julian day // PARTEC // 1
  integer :: P_nbcueille  !< // PARAMETER // number of fruit harvestings // code 1/2 // PARTEC // 0
  integer :: P_ilan  !< // PARAMETER // Day of the stage LAN () if the stage is observed (else 999) // julian day // PARTEC // 1
  integer :: P_iflo  !< // PARAMETER // flowering day // julian day // PARTEC // 1
  integer :: P_locirrig  !< // PARAMETER // Depth of water apply (when irrigation is applied in depth of soil) // cm // PARTEC // 1
!  integer :: P_engrais  !< // PARAMETER // fertilizer type  : 1 =Nitrate.of ammonium ,2=Solution,3=urea,4=Anhydrous ammoniac,5= Sulfate of ammonium,6=phosphate of ammonium,7=Nitrateof calcium,8= fixed efficiency    // * // PARTEC // 1
  integer :: P_engrais(10)  !< // PARAMETER // fertilizer type  : 1 =Nitrate.of ammonium ,2=Solution,3=urea,4=Anhydrous ammoniac,5= Sulfate of ammonium,6=phosphate of ammonium,7=Nitrateof calcium,8= fixed efficiency    // * // PARTEC // 1
  integer :: P_locferti  !< // PARAMETER // Depth of nitrogen apply (when fertiliser is applied in depth of soil) // cm // PARTEC // 1
  integer :: P_cadencerec  !< // PARAMETER // number of days between two harvests // day // PARTEC // 1
  integer :: P_julrogne  !< // PARAMETER // day of plant shapening // julian day // PARTEC // 1
! DR je passe à 10 les interventions d'eclaircissage
  integer :: P_juleclair(10)  !< // PARAMETER // julian day of fruits removal // julian day // PARTEC // 1
  integer :: P_juleffeuil  !< // PARAMETER // julian day of leaf removal // julian day // PARTEC // 1
  integer :: P_jultaille  !< // PARAMETER // pruning day // julian day // PARTEC // 1
!  integer :: P_jultrav(11)  DR 01/02/2011 on supprime naps pour le remplacer par P_nbjtrav et P_nbjres
  integer :: P_jultrav(11)     !< // PARAMETER // Day in year of the soil cultivation operations and/or apply of organic residues  // julian day // PARTEC // 1
  integer :: P_julres(11)     !< // PARAMETER // Day in year of the residue apply  // julian day // PARTEC // 1
  integer :: P_coderes(11)     !< // PARAMETER // residue type: 1=crop residues,  2=residues of CI,  3=manure,  4=compost OM,  5=mud SE,  6=vinasse,  7=corn,  8=other // code 1 to 10 // PARTEC // 0

  real :: P_profsem  !< // PARAMETER // Sowing depth // cm // PARTEC // 1
  real :: P_ratiol  !< // PARAMETER // Water stress index below which we start an irrigation in automatic mode (0 in manual mode) // between 0 and 1 // PARTEC // 1
  real :: P_dosimx  !< // PARAMETER // maximum water amount of irrigation authorised at each time step (mode automatic irrigation) // mm day-1 // PARTEC // 1
  real :: P_doseirrigmin      ! DR le 12/09/07 pour benjamin      // PARAMETER // minimal amount of irrigation // mm // PARTEC // 1
  real :: P_profmes  !< // PARAMETER // measure depth of soil water reserve // cm // PARTEC // 1
  real :: P_densitesem  !< // PARAMETER // Sowing density  // plants.m-2 // PARTEC // 1
  real :: P_concirr  !< // PARAMETER // Nitrogen concentration of water // kgN mm-1 // PARTEC // 1
  real :: P_effirr  !< // PARAMETER // irrigation efficiency // SD // PARTEC // 1
  real :: P_lairesiduel(0:20)     !< // PARAMETER // Residual leaf index after each cut (table) // m2 leaf  m-2 soil // PARTEC // 1
  real :: P_hautcoupedefaut  !< // PARAMETER // Cut height for forage crops (calendar calculated) // m // PARTEC // 1
!  real :: P_hautcoupe(20)     !< // PARAMETER // Cut height for forage crops (calendar fixed) // m // PARTEC // 1
  real :: P_hautcoupe(0:20)     !< // PARAMETER // Cut height for forage crops (calendar fixed) // m // PARTEC // 1
  real :: P_msresiduel(-1:20)     !< // PARAMETER // Residual dry matter after a cut // t ha-1 // PARTEC // 1
!  real :: P_anitcoupe(20)     !< // PARAMETER // amount of mineral fertilizer applications at each cut (forage crop) // kg N ha-1 // PARTEC // 1
  real :: P_anitcoupe(0:20)     !< // PARAMETER // amount of mineral fertilizer applications at each cut (forage crop) // kg N ha-1 // PARTEC // 1

!  real :: P_tempfauche(20)     !< // PARAMETER // Sum of development units between 2 cuts  // degree.days // PARTEC // 1
  real :: P_tempfauche(0:20)     !< // PARAMETER // Sum of development units between 2 cuts  // degree.days // PARTEC // 1
  ! 29/03/2016 prise en compte du paturage
!  integer :: P_restit(20)   !< // PARAMETER // option of restitution in case of pasture yes (1), no (2) // 1-2 // PARTEC // 1
  integer :: P_restit(0:20)   !< // PARAMETER // option of restitution in case of pasture yes (1), no (2) // 1-2 // PARTEC // 1
!  real :: P_mscoupemini(20)   !< // PARAMETER // Minimum threshold value of dry matter to make a cut // t ha-1 // PARTEC // 1
  real :: P_mscoupemini(0:20)   !< // PARAMETER // Minimum threshold value of dry matter to make a cut // t ha-1 // PARTEC // 1


  real :: P_largrogne  !< // PARAMETER // width of shapening // m // PARTEC // 1
  real :: P_margerogne  !< // PARAMETER // allowed quantity of biomass inbetween two shapenings when asking automatic shapening  // t ha-1 // PARTEC // 1
  real :: P_hautrogne  !< // PARAMETER // cutting height // m // PARTEC // 1
  real :: P_effeuil  !< // PARAMETER // proportion of daily leaf removed at thinning // 0-1  // PARTEC // 1
  real :: P_interrang  !< // PARAMETER // Width of the P_interrang // m // PARTEC // 1
  real :: P_orientrang  !< // PARAMETER // Direction of ranks // rd (0=NS) // PARTEC // 1
  real :: P_Crespc(11)     !< // PARAMETER // carbon proportion in organic residue //  // PARTEC // 1
  real :: P_proftrav(11)     !< // PARAMETER // Depth of residue incorporation  (max. 40 cm) // cm // PARTEC // 1
  real :: P_profres(11)     !< // PARAMETER // minimal depth of organic residue incorporation  // cm // PARTEC // 1
  real :: P_CsurNres(11)     !< // PARAMETER // C/N ratio of residues // g g-1 // PARTEC // 1
  real :: P_Nminres(11)     !< // PARAMETER // N mineral content of organic residues  // % fresh matter // PARTEC // 1
  real :: P_eaures(11)     !< // PARAMETER // Water amount of organic residues  // % fresh matter // PARTEC // 1
  real :: P_qres(11)     !< // PARAMETER // amount of crop residue or organic amendments applied to the soil (fresh weight) // t MF ha-1 // PARTEC // 1

!!!MODIF HISAFE 3 : reduction dimension interventions
!!!  real :: P_doseI(300)     !< // PARAMETER // irrigation amount // mm.jour-1 // PARTEC // 1
!!!  real :: P_doseN(300)     !< // PARAMETER // fertilizer amount // kgN.jour-1 // PARTEC // 1
!!!  integer :: P_upvttapN(300)     !< // PARAMETER // thermal time from emergence (UPVT units) driving fertilization // degree C // PARTEC // 1
!!!  integer :: P_julapN(300)     !< // PARAMETER // dates in julian days of fertilizer application // julian day // PARTEC // 1
!!!  integer :: P_julapI(300)     !< // PARAMETER // dates in julian days of irrigation  // julian day // PARTEC // 1
! Dr 09/10/09 j'ajoute les parametres irrigation avce des sommes en upvt
!!!  integer :: P_upvttapI(300)     !< // PARAMETER // thermal time from emergence (UPVT units) driving irrigation // degree C // PARTEC // 1
! DR 02/07/08 fractionnement de l'azote
!!!  real :: P_fracN(731)     !< // PARAMETER // percentage of P_Qtot_N applied // % // PARTEC // 1
  integer :: P_julapI(100)     !< // PARAMETER // dates in julian days of irrigation  // julian day // PARTEC // 1
  integer :: P_julapN(100)     !< // PARAMETER // dates in julian days of fertilizer application // julian day // PARTEC // 1
  real    :: P_doseI(100)     !< // PARAMETER // irrigation amount // mm.jour-1 // PARTEC // 1
  real    :: P_doseN(100)     !< // PARAMETER // fertilizer amount // kgN.jour-1 // PARTEC // 1
  integer :: P_upvttapI(100)     !< // PARAMETER // thermal time from emergence (UPVT units) driving irrigation // degree C // PARTEC // 1
  integer :: P_upvttapN(100)     !< // PARAMETER // thermal time from emergence (UPVT units) driving fertilization // degree C // PARTEC // 1
  real    :: P_fracN(100)     !< // PARAMETER // percentage of P_Qtot_N applied // % // PARTEC // 1


  real :: P_h2ograinmin  !< // PARAMETER // minimal water content allowed at harvest // g eau g-1 MF // PARTEC // 1
  real :: P_h2ograinmax  !< // PARAMETER // maximal water content allowed at harvest // g water g-1 MF // PARTEC // 1
  real :: P_CNgrainrec  !< // PARAMETER // minimal grain nitrogen content for harvest  // 0-1 // PARTEC // 1
  real :: P_huilerec  !< // PARAMETER // minimal oil content allowed for harvest // g huile g-1 MF // PARTEC // 1
  real :: P_sucrerec  !< // PARAMETER // minimal sugar rate at harvest // g sucre g-1 MF // PARTEC // 1
! DR 29/03/2016 passé en tableau
! real :: P_mscoupemini  !< // PARAMETER // Minimum threshold value of dry matter to make a cut // t ha-1 // PARTEC // 1
  real :: P_nbinfloecl(10)  !< // PARAMETER // "number of inflorescences or fruits removed at  fruit removal   // nb pl-1 // PARTEC // 1
  real :: P_laidebeff  !< // PARAMETER // LAI of the beginning of leaf removal // m2 m-2 // PARTEC // 1
  real :: P_laieffeuil  !< // PARAMETER // LAI of the end of leaf removal // m2 m-2 // PARTEC // 1
  real :: P_biorognem  !< // PARAMETER // minimal biomass to be removed when tipping (automatic calculation) // t ha-1 // PARTEC // 1
  integer :: P_nb_eclair  !< // PARAMETER // removal number // 1-10 // PARTEC // 1


! -- inutilisés
!      integer :: modexploit

! DR et celia 31/03/06
  integer :: P_codeDSTtass  !< // PARAMETER // activation of compaction at sowing and harvest : yes (1), no (2) // code1/2 // PARTEC // 0
  integer :: P_codedecisemis  !< // PARAMETER // activation of moisture effect on the decision to harvest // code 1/2 // PARTEC // 0
  integer :: P_codedecirecolte  !< // PARAMETER // activation of moisture and frost effects on the decision to harvest // code 1/2 // PARTEC // 0
  integer :: P_nbjmaxapressemis  !< // PARAMETER // maximal delay (number of days) of sowing date in case of soil compaction activation // jours // PARPLT // 1
  integer :: P_nbjseuiltempref  !< // PARAMETER // number of days for which we check if there is no frost sowing conditions when decision to sow is activated // jours // PARTEC // 1
  integer :: P_nbjmaxapresrecolte  !< // PARAMETER // maximal delay (number of days) of harvest date in case of soil compaction activation // jours // PARTEC // 1
  integer :: P_codeDSTnbcouche  !< // PARAMETER // activation of the number of compacted soil layers: one layer (1), two layers (2) // code 1/2 // PARTEC // 0
  real :: P_profhumsemoir  !< // PARAMETER // soil depth for which moisture is considered as a reference to allow or not sowing in the case of soil compaction activation // cm // PARTEC // 1
  real :: P_profhumrecolteuse  !< // PARAMETER // soil depth for which moisture is considered as a reference to allow or not harvesting in the case of soil compaction activation // cm // PARTEC // 1
  real :: P_dasemis  !< // PARAMETER // bulk density modification as a result of sowing // g.cm-3 // PARTEC // 1
  real :: P_darecolte  !< // PARAMETER // bulk density modification as a result of harvest // g.cm-3 // PARSOL // 1



  integer :: P_codefracappN  !< // PARAMETER // option of cut modes for forage crops: yes (1), no (2) // code 1/2 // PARTEC // 0
  integer :: P_Qtot_N  !< // PARAMETER // amount of mineral fertilizer applications  // kg N ha-1 // PARTEC // 1

! NB le 15/02/06 parametres DST
  integer ::  P_codeDST  !< // PARAMETER // activation of modifications of physical soil deep conditions through cropping management : yes (1), no (2) // code 1/2 // PARTEC // 0
  real :: P_dachisel  !< // PARAMETER // bulk density modification as a result of soil management (Chisel) // g.cm-3 // PARTEC // 1
  real :: P_dalabour  !< // PARAMETER // bulk density modification as a result of soil management (Plough) // g.cm-3 // PARTEC // 1
  real :: P_rugochisel  !< // PARAMETER // bare soil rugosity lenght (P_z0solnu) for a chisel when soil compaction is activated // m // PARTEC // 1
  real :: P_rugolabour  !< // PARAMETER // bare soil rugosity lenght (P_z0solnu) for a plough when soil compaction is activated // m // PARPLT // 1



!=========
!

! VARIABLES TECH DANS LES COMMUNES (culture sous abri)
  integer :: P_codabri  !< // PARAMETER // option of calculation of the climate under a shelter // code 1/2 // PARTEC // 0
  integer :: P_julouvre2  !< // PARAMETER // day of opening of the shelter // julian day // PARTEC // 1
  integer :: P_julouvre3  !< // PARAMETER // day of opening of the shelter // julian day // PARTEC // 1
!  integer :: nouvre2
!  integer :: nouvre3
  real :: P_transplastic  !< // PARAMETER // translission coefficient of the shelter plastic // SD // PARTEC // 1
  real :: P_surfouvre1  !< // PARAMETER // surface proportion of the shelter opened the first day of opening // SD // PARTEC // 1
  real :: P_surfouvre2  !< // PARAMETER // surface proportion of the shelter opened the second day of opening // SD // PARTEC // 1
  real :: P_surfouvre3  !< // PARAMETER // surface proportion of the shelter opened the third day of opening // SD // PARTEC // 1

! VARIABLES TECH DANS SOL (mulch)
!  real :: qmulch0
!  integer :: julappmulch
!  integer :: P_typemulch
!  real :: couvermulch
!  real :: albedomulch
  real :: P_couvermulchplastique  !< // PARAMETER // proportion of soil covered by the cover  // SD // PARTEC // 1
  real :: P_albedomulchplastique  !< // PARAMETER // P_albedo of plastic cover // SD // PARTEC // 1

! NB le 01/07/05 hauteur et largeur techniques
  real :: P_hautmaxtec  !< // PARAMETER // maximal height of the plant allowed by the management // m // PARTEC // 1
  real :: P_largtec  !< // PARAMETER // technical width // m // PARTEC // 1

! DR le 23/03/06 un code pour lire P_hautmaxtec et P_largtec dans le fichier technique pour inaki
  integer :: P_codepalissage  !< // PARAMETER // option: no (1),  yes2) // code 1/2 // PARTEC // 0

  integer :: P_coderecolteassoc  !< // PARAMETER // option to harvest intercrop species simultaneously, at the physiological maturity date of the earliest one: yes(1), no (2) // code 1/2 // PARTEC // 0

! PB - 02/08/2010 - on déplace depuis transit
  integer :: P_codedateappH2O  !< // PARAMETER // irrigation application dates given as sum of temperatures / yes (1), no (2) // code 1/2 // PARTEC // 0
  integer :: numeroappI  

! PB - 03/08/2010 - on déplace depuis stics_commmuns_ car ce sont des variables lues dans le fichier technique
  integer :: numeroappN  
  integer :: P_codedateappN  !< // PARAMETER // mineral fertilizer application dates given as sum of temperatures / yes (1), no (2) // code 1/2 // PARTEC // 0
! DR et FR 11/02/2015 on ne modifie pas le parametre donc on fabrique une variable
   real :: mscoupemini_courant  !< // variable // Minimum threshold value of dry matter to make a cut // t ha-1 //

   logical  :: flag_eclairmult  !< // variable // flag for activation or not of the option several thinning // 1-2 //
   logical  :: flag_pature  !< // variable // flag for activation or not of pasture and restitution// 1-2 //
   logical  :: flag_plusieurs_engrais  !< // variable // flag for activation or not of several fertilisations type// 1-2 //

end type ITK_

contains

!*********************************************************************
!>   reading crop management parameters
!!     in the file fic1_tec.txt
!*********************************************************************
! TODO : se débarrasser de sc et t, par exemple en transférant/dupliquant les paramètres incriminés
!subroutine ITK_Lecture_V6(P_ftec, itk, sc) ! DR 19/07/2012 sc n'est pas utilisé
subroutine ITK_Lecture_V6(P_ftec, itk, path, pathtec)

USE Stics
USE Messages

implicit none


! ARGUMENTS
! DR 17/07/2012 je rallonge pour optimistics
  character(len=50), intent(IN)    :: P_ftec  !> // PARAMETER // name of the technique file // SD // P_USM/USMXML // 0

  type(ITK_),        intent(INOUT) :: itk  

!  type(Stics_Communs_), intent(INOUT) :: sc

   ! enabling_record :le chemin pour accéder à la config
   character(len=255), intent(IN) :: path ! enabling_record
   ! enabling_record :le chemin pour accéder directement à la config
   character(len=255), intent(IN) :: pathtec ! enabling_record
!   logical  :: flag_eclairmult


! DR 05/03/08 iplt à ete renomme P_iplt0 car il est possiblement modifiable par decisionsemis

! Variables locales
  integer :: nbcoupe2  !>  
  integer ::  nbcoupe3  
  integer :: i  
  integer :: eof !> variable de gestion des erreurs d'ouverture/lecture du fichier  
!  integer :: napS ! DR 01/02/2011 je le laisse declare en attendant de le supprimer definitement

!---------------------------------------------------------------------!
! DR 19/11/2013 pour record
      integer ib0                                             ! enabling_record
      integer ib1                                             ! enabling_record
      character(len=300) :: filepluspath                      ! enabling_record
      ib0 = len_trim(pathtec)                              ! enabling_record
      if (ib0 .ne. 0 ) then                                   ! enabling_record
         filepluspath =  pathtec                           ! enabling_record
      else
         ib1 = len_trim(path)                                 ! enabling_record
         if (ib1 .eq. 0 ) then                                   ! enabling_record
            filepluspath = p_ftec
         else
            filepluspath = path(1:ib1) // '/' // p_ftec         ! enabling_record
         endif                                                   ! enabling_record
      endif
! fin record

    ! Ouverture du fichier pour lecture

      open (56,file = filepluspath,status = 'old',iostat=eof) ! enabling_record

      if (eof /= 0) then ! erreur
        !TODO: assigner un code erreur et arrêter la routine
        call EnvoyerMsgHistorique(3274,P_ftec,'a15')
        !stop
        !return
        call exit(9)
      endif
! DR 01/02/2011 on scinde le tableau apport de residus et travail du sol en 2 tableaux
    ! lecture des apports de residus
!supply of organic residus
!***************************

      read (56,*)
      read (56,*,iostat=eof) itk%P_nbjres

      if (itk%P_nbjres > 0) then
        do i = 1,itk%P_nbjres
          read (56,*)
          read (56,*,iostat=eof) itk%P_julres(i),itk%P_coderes(i), &
                                 itk%P_qres(i),itk%P_Crespc(i),itk%P_CsurNres(i),itk%P_Nminres(i),itk%P_eaures(i)
        end do
      endif
    ! lecture des opérations de travail du sol
!soil tillage
!******************
      read (56,*)
      read (56,*,iostat=eof) itk%P_nbjtrav
      if (itk%P_nbjtrav > 0) then
        do i = 1,itk%P_nbjtrav
          read (56,*)
          read (56,*,iostat=eof) itk%P_jultrav(i),itk%P_profres(i),itk%P_proftrav(i)
        end do
      endif
!sowing
!*****************
      read (56,*)
      read (56,*,iostat=eof) itk%P_iplt0
      read (56,*)
      read (56,*,iostat=eof) itk%P_profsem
      read (56,*)
    ! DR 27/08/07 on renomme densite P_densitesem car densite varie
      read (56,*,iostat=eof) itk%P_densitesem
      read (56,*)
      read (56,*,iostat=eof) itk%P_variete
      read (56,*)
      read (56,*,iostat=eof) itk%P_codetradtec
      read (56,*)
      read (56,*,iostat=eof) itk%P_interrang
      read (56,*)
      read (56,*,iostat=eof) itk%P_orientrang
    ! DR 23/10/07 lecture des options de semis
      read (56,*)
      read (56,*,iostat=eof) itk%P_codedecisemis
      read (56,*)
      read (56,*,iostat=eof) itk%P_nbjmaxapressemis
      read (56,*)
      read (56,*,iostat=eof) itk%P_nbjseuiltempref
!phenological stages
      read (56,*)
      read (56,*,iostat=eof) itk%P_codestade
      read (56,*)
      read (56,*,iostat=eof) itk%P_ilev
      read (56,*)
      read (56,*,iostat=eof) itk%P_iamf
      read (56,*)
      read (56,*,iostat=eof) itk%P_ilax
      read (56,*)
      read (56,*,iostat=eof) itk%P_isen
      read (56,*)
      read (56,*,iostat=eof) itk%P_ilan
      read (56,*)
      read (56,*,iostat=eof) itk%P_iflo
      read (56,*)
      read (56,*,iostat=eof) itk%P_idrp
      read (56,*)
      read (56,*,iostat=eof) itk%P_imat
      read (56,*)
      read (56,*,iostat=eof) itk%P_irec
      read (56,*)
      read (56,*,iostat=eof) itk%P_irecbutoir
! irrigation
!***************************
      read (56,*)
      read (56,*,iostat=eof) itk%P_effirr
      read (56,*)
      read (56,*,iostat=eof) itk%P_codecalirrig
      read (56,*)
      read (56,*,iostat=eof) itk%P_ratiol
      read (56,*)
      read (56,*,iostat=eof) itk%P_dosimx
    ! DR 23/10/07
      read (56,*)
      read (56,*,iostat=eof) itk%P_doseirrigmin
    !
    ! DR 23/10/07
      read (56,*)
      read (56,*,iostat=eof) itk%P_codedateappH2O
    !
      read (56,*)
      read (56,*,iostat=eof) itk%nap

    ! *- lecture des irrigations
      if (itk%nap > 0) then
        do i = 1,itk%nap
          if (itk%P_codedateappH2O /= 1) then
            read (56,*)
            read (56,*,iostat=eof) itk%P_julapI(i), itk%P_doseI(i)
          else
            read (56,*)
            read (56,*,iostat=eof) itk%P_upvttapI(i),itk%P_doseI(i)
          endif
        end do
      endif

      read (56,*)
      read (56,*,iostat=eof) itk%P_codlocirrig
      read (56,*)
      read (56,*,iostat=eof) itk%P_locirrig
      read (56,*)
      read (56,*,iostat=eof) itk%P_profmes
      !fertilisation
!************************
      read (56,*)
      if(itk%flag_plusieurs_engrais)then
          read (56,*,iostat=eof)
      else
          read (56,*,iostat=eof) itk%P_engrais(1)
      endif
      read (56,*)
      read (56,*,iostat=eof) itk%P_concirr
    ! DR 23/10/07 on deplace P_codedateappn ici pour la lecture suivant code
      read (56,*)
      read (56,*,iostat=eof) itk%P_codedateappN
    ! DR 2/07/08 on rajoute 2 parametres pour permettre le fractionnent de l'azote
      read (56,*)
      read (56,*,iostat=eof) itk%P_codefracappN
      read (56,*)
      read (56,*,iostat=eof) itk%P_Qtot_N
    !
      read (56,*)
      read (56,*,iostat=eof) itk%napN


    ! *- lecture des fertilisations

      if (itk%napN > 0) then
        do i = 1,itk%napN
          if (itk%P_codedateappN /= 1) then
            if (itk%P_codefracappN == 1) then
              read (56,*)
              if(itk%flag_plusieurs_engrais)then
                  read (56,*,iostat=eof) itk%P_julapN(i), itk%P_doseN(i),itk%P_engrais(i)
              else
                  read (56,*,iostat=eof) itk%P_julapN(i), itk%P_doseN(i)
              endif
            else
              read (56,*)
              if(itk%flag_plusieurs_engrais)then
                  read (56,*,iostat=eof) itk%P_julapN(i), itk%P_fracN(i),itk%P_engrais(i)
              else
                  read (56,*,iostat=eof) itk%P_julapN(i), itk%P_fracN(i)
              endif
            endif
          else
            if (itk%P_codefracappN == 1) then
              read (56,*)
              if(itk%flag_plusieurs_engrais)then
                  read (56,*,iostat=eof) itk%P_upvttapN(i), itk%P_doseN(i),itk%P_engrais(i)
              else
                  read (56,*,iostat=eof) itk%P_upvttapN(i), itk%P_doseN(i)
              endif
            else
              read (56,*)
              if(itk%flag_plusieurs_engrais)then
                  read (56,*,iostat=eof) itk%P_upvttapN(i), itk%P_fracN(i),itk%P_engrais(i)
              else
                  read (56,*,iostat=eof) itk%P_upvttapN(i), itk%P_fracN(i)
              endif
            endif
          endif
        end do
      endif

      read (56,*)
      read (56,*,iostat=eof) itk%P_codlocferti
      read (56,*)
      read (56,*,iostat=eof) itk%P_locferti



! DR 05/04/2011 on les passe dans paramv6 : sc%P_codecalferti,sc%P_ratiolN,sc%P_dosimxN
!      read (56,*)
!      read (56,*,iostat=eof) sc%P_codetesthumN      ! => Stics_Communs ?? A classer ou dupliquer
      read (56,*)
      read (56,*,iostat=eof) itk%P_ressuite


! harvest
!***************************
      read (56,*)
      read (56,*,iostat=eof) itk%P_codcueille
      read (56,*)
      read (56,*,iostat=eof) itk%P_nbcueille
      read (56,*)
      read (56,*,iostat=eof) itk%P_cadencerec
      read (56,*)
      read (56,*,iostat=eof) itk%P_codrecolte
      read (56,*)
      read (56,*,iostat=eof) itk%P_codeaumin
      read (56,*)
      read (56,*,iostat=eof) itk%P_h2ograinmin
      read (56,*)
      read (56,*,iostat=eof) itk%P_h2ograinmax
      read (56,*)
      read (56,*,iostat=eof) itk%P_sucrerec
      read (56,*)
      read (56,*,iostat=eof) itk%P_CNgrainrec
      read (56,*)
      read (56,*,iostat=eof) itk%P_huilerec
    ! DR 23/10/07
      read (56,*)
      read (56,*,iostat=eof) itk%P_coderecolteassoc
      read (56,*)
      read (56,*,iostat=eof) itk%P_codedecirecolte
      read (56,*)
      read (56,*,iostat=eof) itk%P_nbjmaxapresrecolte
!special techniques
!*****************************
      read (56,*)
      read (56,*,iostat=eof) itk%P_codefauche
  ! *- domi - 10/01/2012 - P_mscoupemini deplace

! DR 29/03/2016 pour efese on ajoute 2 variables correspondant au type d'intervention fauche ou paturage , on indice le mscoupe mini specifique à la coupe
!    if(.not.itk%flag_pature)then
      read (56,*)
      read (56,*,iostat=eof) itk%P_mscoupemini(1)
!    endif
      read (56,*)
      read (56,*,iostat=eof) itk%P_codemodfauche
      read (56,*)

      if(itk%P_codemodfauche == 1) then
        itk%lecfauche=.false.
      else
        itk%lecfauche=.true.
      endif

      read (56,*,iostat=eof) itk%P_hautcoupedefaut
      read (56,*)
      read (56,*,iostat=eof) itk%P_stadecoupedf
      read (56,*)
      read (56,*,iostat=eof) nbcoupe2
      if (itk%P_codemodfauche == 2) then
    ! *- on lit des jours
        do i = 1,nbcoupe2
          read (56,*)
          if(itk%flag_pature)then
            read (56,*,iostat=eof) itk%P_julfauche(i),itk%P_hautcoupe(i),itk%P_lairesiduel(i),itk%P_msresiduel(i), &
                                   itk%P_anitcoupe(i), itk%P_restit(i),itk%P_mscoupemini(i)
          else
            read (56,*,iostat=eof) itk%P_julfauche(i),itk%P_hautcoupe(i),itk%P_lairesiduel(i),itk%P_msresiduel(i),itk%P_anitcoupe(i)
          endif
        end do
        itk%nbcoupe = nbcoupe2
      else
        do i = 1,nbcoupe2
          read (56,*)
          read (56,*)
        end do
      endif

      read (56,*)
      read (56,*,iostat=eof) nbcoupe3
      if(itk%P_codemodfauche == 3) then
    ! *- on lit des degrés.jours
        do i = 1,nbcoupe3
          read (56,*)
          if(itk%flag_pature)then
          read (56,*,iostat=eof) itk%P_tempfauche(i),itk%P_hautcoupe(i),itk%P_lairesiduel(i),itk%P_msresiduel(i),  &
                                 itk%P_anitcoupe(i),itk%P_restit(i),itk%P_mscoupemini(i)
          else
          read (56,*,iostat=eof) itk%P_tempfauche(i),itk%P_hautcoupe(i),itk%P_lairesiduel(i),itk%P_msresiduel(i),itk%P_anitcoupe(i)
          endif
        end do
        itk%nbcoupe=nbcoupe3
      else
        do i = 1,nbcoupe3
          read (56,*)
          read (56,*)
        end do
      endif

    ! ** version <5.0
    ! --     read (56,*)
    ! --     read (56,*,iostat=eof) itk%P_codeffeuil
    ! --     read (56,*)
    ! --     read (56,*,iostat=eof) itk%P_effeuil
      read (56,*)
      read (56,*,iostat=eof) itk%P_codepaillage
    ! TODO: ici, on testait le code paillage de la plante principale uniquement. Pb ?
      read (56,*)
      read (56,*,iostat=eof) itk%P_couvermulchplastique
      read (56,*)
      read (56,*,iostat=eof) itk%P_albedomulchplastique
      read (56,*)

    ! -- déplacé -- read (56,*,iostat=eof) itk%P_ressuite

    ! ** version 5.0
      read (56,*,iostat=eof) itk%P_codrognage
      read (56,*)
      read (56,*,iostat=eof) itk%P_largrogne
      read (56,*)
      read (56,*,iostat=eof) itk%P_hautrogne
      read (56,*)
      read (56,*,iostat=eof) itk%P_biorognem
      read (56,*)
      read (56,*,iostat=eof) itk%P_codcalrogne
      read (56,*)
      read (56,*,iostat=eof) itk%P_julrogne
      read (56,*)
      read (56,*,iostat=eof) itk%P_margerogne
      read (56,*)
      read (56,*,iostat=eof) itk%P_codeclaircie
      read (56,*)
      ! DR 09/03/2014 pour Constance et pour la preochaine version on peut faire plusieurs eclaircissages
      ! DR 23/03/2016 pour le rendre generique je le sort dans param_newform
      !flag_eclairmult=.TRUE.

      if(itk%flag_eclairmult)then
      read (56,*,iostat=eof) itk%P_nb_eclair
         do i = 1,itk%P_nb_eclair
         read (56,*)
         read (56,*,iostat=eof) itk%P_juleclair(i), itk%P_nbinfloecl(i)
         enddo
      else
         itk%P_nb_eclair=1
         read (56,*,iostat=eof) itk%P_juleclair(1)
         read (56,*)
         read (56,*,iostat=eof) itk%P_nbinfloecl(1)
      endif
      read (56,*)
      read (56,*,iostat=eof) itk%P_codeffeuil
      read (56,*)
      read (56,*,iostat=eof) itk%P_codhauteff
      read (56,*)
      read (56,*,iostat=eof) itk%P_codcaleffeuil
      read (56,*)
      read (56,*,iostat=eof) itk%P_laidebeff
      read (56,*)
      read (56,*,iostat=eof) itk%P_effeuil
      read (56,*)
      read (56,*,iostat=eof) itk%P_juleffeuil
      read (56,*)
      read (56,*,iostat=eof) itk%P_laieffeuil
      read (56,*)
      read (56,*,iostat=eof) itk%P_codetaille
      read (56,*)
      read (56,*,iostat=eof) itk%P_jultaille
    ! DR 23/10/07
      read (56,*)
      read (56,*,iostat=eof) itk%P_codepalissage
      read (56,*)
      read (56,*,iostat=eof) itk%P_hautmaxtec
      read (56,*)
      read (56,*,iostat=eof) itk%P_largtec
    !
      read (56,*)
      read (56,*,iostat=eof) itk%P_codabri
      read (56,*)
      read (56,*,iostat=eof) itk%P_transplastic
      read (56,*)
      read (56,*,iostat=eof) itk%P_surfouvre1
      read (56,*)
      read (56,*,iostat=eof) itk%P_julouvre2
      read (56,*)
      read (56,*,iostat=eof) itk%P_surfouvre2
      read (56,*)
      read (56,*,iostat=eof) itk%P_julouvre3
      read (56,*)
      read (56,*,iostat=eof) itk%P_surfouvre3
    ! DR 23/10/07
!soil modification by techniques (compaction-fragmentation)
!************************************************************
      read (56,*)
      read (56,*,iostat=eof) itk%P_codeDST
      read (56,*)
      read (56,*,iostat=eof) itk%P_dachisel
      read (56,*)
      read (56,*,iostat=eof) itk%P_dalabour
      read (56,*)
      read (56,*,iostat=eof) itk%P_rugochisel
      read (56,*)
      read (56,*,iostat=eof) itk%P_rugolabour
      read (56,*)
      read (56,*,iostat=eof) itk%P_codeDSTtass
      read (56,*)
      read (56,*,iostat=eof) itk%P_profhumsemoir
      read (56,*)
      read (56,*,iostat=eof) itk%P_dasemis
      read (56,*)
      read (56,*,iostat=eof) itk%P_profhumrecolteuse
      read (56,*)
      read (56,*,iostat=eof) itk%P_darecolte
      read (56,*)
      read (56,*,iostat=eof) itk%P_codeDSTnbcouche

    ! fermeture du fichier
      close(56)
      return

! TODO : gérer les erreurs
!251   call EnvoyerMsgHistorique(3274,P_ftec)
!      stop

return
end subroutine ITK_Lecture_V6


!*********************************************************************
!>   writing in the cookie
!!     in the file history.sti
!*********************************************************************
! TODO : se débarrasser de sc et t, par exemple en transférant/dupliquant les paramètres incriminés
subroutine ITK_Ecriture_Tests(itk,pg,sc)

USE Stics
USE Parametres_Generaux
USE Messages

    implicit none

    type(ITK_),                 intent(INOUT) :: itk  
    type(Stics_Communs_),       intent(INOUT) :: sc
    type(Parametres_Generaux_), intent(IN)    :: pg  



    integer :: is  !>  
    integer ::  i  
    character(len=500) :: tmp  

! dr 01/02/2011 on laisse napS en attendant
!    integer :: napS


! écriture dans le mouchard
! DR 21/03/2014 TODO mettre le mouchard dans outputs/history
!!!      call EnvoyerMsgHistorique('  ')
!!!      call EnvoyerMsgHistorique(152,itk%P_ftec)
!!!      call EnvoyerMsgHistorique(5005)

! ** test sur les valeurs de P_ressuite
! dr 25/08/08 pour la vigne on enleve pour incorporer les bois de taille
! DR 10/09/2012 on a changé les noms des ressuite suivant BM

! DR 10/09/2012 je garde ca pour faire la correspondance avce 6.9 qui normalement est geree par le maf6versmodulo

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!      if (index(itk%P_ressuite,'acine') > 0) itk%P_ressuite='roots'
!!!      if (index(itk%P_ressuite,'aille') > 0) itk%P_ressuite='straw+roots'
!!!      if (index(itk%P_ressuite,'haume') > 0) itk%P_ressuite='stubble+roots'
!!!      if (index(itk%P_ressuite,'ulture') > 0) itk%P_ressuite='whole_crop'
!!!      if (index(itk%P_ressuite,'ois') > 0) itk%P_ressuite='prunings'
!!!      if (index(itk%P_ressuite,'ucun') > 0.or.index(itk%P_ressuite,'ien') > 0) itk%P_ressuite='roots'


!!!      if (index(itk%P_ressuite,'roots') == 0 .and. &
!!!          index(itk%P_ressuite,'whole_crop') == 0 .and. &
!!!          index(itk%P_ressuite,'straw+roots') == 0 .and. &
!!!         index(itk%P_ressuite,'stubble+roots') == 0 .and. &
!!!          index(itk%P_ressuite,'stubble_of_residu_type_9+roots') == 0 .and. &
!!!          index(itk%P_ressuite,'stubble_of_residu_type_10+roots') == 0 .and. &
!!!          index(itk%P_ressuite,'prunings')    == 0) then
!!!       itk%P_ressuite = 'roots'
!!!      end if

        if (itk%P_ressuite == 0) itk%P_ressuite = 1

! j'ajoute un test sur l'utilisation des residus type 9 ou 10 si non parametres
! DR 05/08/2014 y'avait un pb avec le type 9 qui n'etait pas pris en compte
!      if (index(itk%P_ressuite,'stubble_of_residu_type_10+roots') > 0 )then
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!! 1 = roots
!!! 2 = whole_crop
!!! 3 = straw+roots
!!! 4 = stubble+roots
!!! 5 = stubble_of_residu_type_9+roots
!!! 6 = stubble_of_residu_type_10+roots
!!! 7 = prunings
!!!      if (index(itk%P_ressuite,'stubble_of_residu_type_9+roots') > 0 )then
      if (itk%P_ressuite == 5) then
        if(pg%P_kbio(9).eq.0.or.pg%P_yres(9).eq.0.or.pg%P_akres(9).eq.0.or.pg%P_bkres(9).eq.0.or.pg%P_awb(9).eq.0&
         .or.pg%P_bwb(9).eq.0.or.pg%P_cwb(9).eq.0.or.pg%P_ahres(9).eq.0.or.pg%P_bhres(9).eq.0&
         .or.pg%P_CNresmin(9).eq.0.or.pg%P_CNresmax(9).eq.0.or.pg%P_qmulchruis0(9).eq.0&
         .or.pg%P_mouillabilmulch(9).eq.0.or.pg%P_kcouvmlch(9).eq.0.or.pg%P_albedomulchresidus(9).eq.0&
         .or.pg%P_Qmulchdec(9).eq.0) then
           call EnvoyerMsgHistorique(600,itk%P_ressuite)
           !stop
           call exit(9)
         endif
       endif
!!!      if (index(itk%P_ressuite,'stubble_of_residu_type_10+roots') > 0 )then
       if (itk%P_ressuite == 6) then
        if(pg%P_kbio(10).eq.0.or.pg%P_yres(10).eq.0.or.pg%P_akres(10).eq.0.or.pg%P_bkres(10).eq.0.or.pg%P_awb(10).eq.0&
         .or.pg%P_bwb(10).eq.0.or.pg%P_cwb(10).eq.0.or.pg%P_ahres(10).eq.0.or.pg%P_bhres(10).eq.0&
         .or.pg%P_CNresmin(10).eq.0.or.pg%P_CNresmax(10).eq.0.or.pg%P_qmulchruis0(10).eq.0&
         .or.pg%P_mouillabilmulch(10).eq.0.or.pg%P_kcouvmlch(10).eq.0.or.pg%P_albedomulchresidus(10).eq.0&
         .or.pg%P_Qmulchdec(10).eq.0) then
           call EnvoyerMsgHistorique(600,itk%P_ressuite)
           !stop
           call exit(9)
         endif
       endif

    ! si P_iplt0 different de 999 et que P_fplt correspond a un fichier solnu.plt
    ! TODO : possibilité de tester plutot le P_codeplante ? (= snu)
    ! TODO : décommenter et corriger
!      if (itk%P_iplt0 /= 999 .and. index(P_fplt(ipl),'solnu.plt') > 0)  then
!        itk%P_iplt0=999
!      endif

    ! test sur la date de semis : si 999 alors solnu
    !      if (itk%P_iplt0.eq.999) then
    ! domi 25/10/05 dans le cas d'une plante deja en place (demarrage à un stade>P_iplt0 on ne doit pas y mettre solnu)
    ! TODO : décommenter et corriger
!      if (itk%P_iplt0 == 999 .and. P_stade0 == 'plt') then
!        P_fplt='solnu.plt'
!        itk%P_ressuite='aucun  '
!        call EnvoyerMsgHistorique(144)
!      endif
! DR 01/02/2011 on fait les tests sur travail du sol et apports de residus
! tests sur les interventions travail du sol
      do is = 1, itk%P_nbjtrav
        if (itk%P_jultrav(is) < sc%P_iwater .and. itk%P_jultrav(is) /= 0) then
          call EnvoyerMsgHistorique(146)
          call EnvoyerMsgHistorique(147)
          !stop
          call exit(9)
        endif
        if (itk%P_jultrav(is) <= 0) then
!          itk%P_qres(is) = 0.0
          call EnvoyerMsgHistorique(149)
        endif
        if (itk%P_proftrav(is) <= 0.0) itk%P_proftrav(is)=1.0
        if (itk%P_profres(is) < 1.) itk%P_profres(is)=1.
        if (itk%P_profres(is) > itk%P_proftrav(is)) then
          itk%P_profres(is)=itk%P_proftrav(is)
        endif
      end do
! tests sur les apports de residus
      do is = 1, itk%P_nbjres
        if (itk%P_coderes(is) <= 0) then
          call EnvoyerMsgHistorique(150)
          !stop
          call exit(9)
        endif
      enddo

    ! le 22/07/99 test sur P_locirrig=0
      if (itk%P_codlocirrig == 3 .and. itk%P_locirrig <= 0) then
        call EnvoyerMsgHistorique(151)
        !stop
        call exit(9)
      endif

    ! domi - 24/06/03 - j'ai diminue le tableau locirrg de 1000 à 50 et donc je test sur P_locirrig
      if (itk%P_codlocirrig == 3 .and. itk%P_locirrig > 50) then
        call EnvoyerMsgHistorique(393)
        itk%P_locirrig = 50
      endif

    ! domi - 27/01/04 - je rajoute un test sur P_ratiol>1
      if (itk%P_ratiol > 1.0 .or. itk%P_ratiol < 0.0)then
        call EnvoyerMsgHistorique('(/)')
        call EnvoyerMsgHistorique(394)
        itk%P_ratiol=1.0
      endif
    ! domi 15/11/05 je rajoute un test si P_profsem=0 je le mets à 1
      if (itk%P_profsem == 0)then
        call EnvoyerMsgHistorique('(/)')
        call EnvoyerMsgHistorique(396)
        itk%P_profsem=1
      endif



      call EnvoyerMsgHistorique('P_nbjtrav ',itk%P_nbjtrav)

!DR 01/02/2011 on teste sur les apports de residus et les travaux du sol
       if (itk%P_nbjtrav > 0) then
        call EnvoyerMsgHistorique('P_jultrav P_profres P_proftrav ')
      endif

      do i=1,itk%P_nbjtrav
        write(tmp,'(i6,2f9.0)')                                      &
               itk%P_jultrav(i),itk%P_profres(i),itk%P_proftrav(i)
        call EnvoyerMsgHistorique(tmp)
      end do


      call EnvoyerMsgHistorique('P_nbjres ',itk%P_nbjres)

      if (itk%P_nbjres > 0) then
        call EnvoyerMsgHistorique(510)
      endif

      do i=1,itk%P_nbjres
        write(tmp,'(i6,i7,5f8.1)')                                      &
               itk%P_julres(i),itk%P_coderes(i),  &
               itk%P_qres(i),itk%P_Crespc(i),itk%P_CsurNres(i),itk%P_Nminres(i),itk%P_eaures(i)
        call EnvoyerMsgHistorique(tmp)
      end do









      call EnvoyerMsgHistorique('P_iplt0 ',itk%P_iplt0)
      call EnvoyerMsgHistorique('P_profsem ',itk%P_profsem)
      call EnvoyerMsgHistorique('P_densitesem ',itk%P_densitesem)
      call EnvoyerMsgHistorique('P_variete ',itk%P_variete)

      if (itk%P_codetradtec == 1) then
        call EnvoyerMsgHistorique('P_interrang ',itk%P_interrang)
        call EnvoyerMsgHistorique('P_orientrang ',itk%P_orientrang)
      endif

      if (itk%P_codestade == 1) then
        call EnvoyerMsgHistorique('P_ilev ',itk%P_ilev)
        call EnvoyerMsgHistorique('P_iamf ',itk%P_iamf)
        call EnvoyerMsgHistorique('P_ilax ',itk%P_ilax)
        call EnvoyerMsgHistorique('P_iflo ',itk%P_iflo)

      ! domi 16/11/05 si flo forcée on met un message
        if (itk%P_iflo /= 999)then
          call EnvoyerMsgHistorique('')
          call EnvoyerMsgHistorique(398)
          call EnvoyerMsgHistorique('')
        endif
        call EnvoyerMsgHistorique('P_idrp ',itk%P_idrp)
        call EnvoyerMsgHistorique('P_imat ',itk%P_imat)
        call EnvoyerMsgHistorique('P_isen ',itk%P_isen)
        call EnvoyerMsgHistorique('P_irec ',itk%P_irec)
        call EnvoyerMsgHistorique('P_ilan ',itk%P_ilan)
      endif

      call EnvoyerMsgHistorique('P_irecbutoir ',itk%P_irecbutoir)
      call EnvoyerMsgHistorique('P_effirr ',itk%P_effirr)
      call EnvoyerMsgHistorique('nap ',itk%nap)

      if (itk%nap > 0) then
        do i = 1,itk%nap
          write(tmp,*) 'P_julapI,P_doseI ',itk%P_julapI(i),itk%P_doseI(i)
          call EnvoyerMsgHistorique(tmp)
        end do
      endif

      call EnvoyerMsgHistorique('P_codlocirrig ',itk%P_codlocirrig)

      if (itk%P_codlocirrig == 3) then
        call EnvoyerMsgHistorique('P_locirrig ',itk%P_locirrig)
      endif

      if (itk%P_codecalirrig == 1) then
        call EnvoyerMsgHistorique('P_ratiol ',itk%P_ratiol)
        call EnvoyerMsgHistorique('P_dosimx ',itk%P_dosimx)
      endif

      call EnvoyerMsgHistorique('P_profmes',itk%P_profmes)
      call EnvoyerMsgHistorique('P_engrais',itk%P_engrais(1))
      call EnvoyerMsgHistorique('P_concirr ',itk%P_concirr)

    ! Domi 21/10/2004 P_engrais est lu dans lectech donc je depace l'ecriture dans lectech
      call EnvoyerMsgHistorique('P_engamm',  pg%P_engamm(itk%P_engrais(1)))
      call EnvoyerMsgHistorique('P_orgeng',  pg%P_orgeng(itk%P_engrais(1)))
      call EnvoyerMsgHistorique('P_deneng',  pg%P_deneng(itk%P_engrais(1)))
      call EnvoyerMsgHistorique('P_voleng',  pg%P_voleng(itk%P_engrais(1)))

      call EnvoyerMsgHistorique('napN ',itk%napN)
      if (itk%napN > 0) then
        do i = 1,itk%napN
        ! Domi - 29/08/2003 - introduction de P_codedateappN pour Caroline Godard,
        ! si P_codedateappN = 1 on lit les dates d'apport en upvtt
          if (itk%P_codedateappN /= 1) then
            write(tmp,*) 'P_julapN,P_doseN ',itk%P_julapN(i),itk%P_doseN(i)
            call EnvoyerMsgHistorique(tmp)
          else
            write(tmp,*) 'upvttN,P_doseN ',itk%P_upvttapN(i),itk%P_doseN(i)
            call EnvoyerMsgHistorique(tmp)
          endif
        end do
      endif

      call EnvoyerMsgHistorique('P_codlocferti ',itk%P_codlocferti)
      if (itk%P_codlocirrig == 3) then
        call EnvoyerMsgHistorique('P_locferti ',itk%P_locferti)
      endif


      call EnvoyerMsgHistorique('P_ressuite ',itk%P_ressuite)

      if (itk%P_codefauche == 1 .and. itk%P_codcueille /= 2) then
        call EnvoyerMsgHistorique(379)
        itk%P_codcueille = 2
      endif

      if (itk%P_codcueille == 1) then
        call EnvoyerMsgHistorique(382)
      else
        call EnvoyerMsgHistorique(383)
        if (itk%P_nbcueille == 1) then
          call EnvoyerMsgHistorique(384)
        else
          call EnvoyerMsgHistorique(385,itk%P_cadencerec)
          call EnvoyerMsgHistorique(386)
        endif
      endif

      if (itk%P_codrecolte == 1) call EnvoyerMsgHistorique(387)
      if (itk%P_codrecolte == 2) then
        if (itk%P_codeaumin == 1)then
          call EnvoyerMsgHistorique(388,itk%P_h2ograinmin)
        else
          call EnvoyerMsgHistorique(389,itk%P_h2ograinmax)
        endif
      endif
      if (itk%P_codrecolte == 3) call EnvoyerMsgHistorique(390,itk%P_sucrerec)
      if (itk%P_codrecolte == 4) call EnvoyerMsgHistorique(391,itk%P_CNgrainrec)
      if (itk%P_codrecolte == 5) call EnvoyerMsgHistorique(392,itk%P_huilerec)

      if (itk%P_codefauche == 1) then
        call EnvoyerMsgHistorique('P_mscoupemini ',itk%P_mscoupemini(1))
        if (itk%P_codemodfauche == 1) then
          call EnvoyerMsgHistorique('P_hautcoupedefaut ',itk%P_hautcoupedefaut)
          call EnvoyerMsgHistorique('P_stadecoupedf ',itk%P_stadecoupedf)
! DR 15/11/05 on interdit la coupe à un stade reproducteur
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!          if (itk%P_stadecoupedf == 'FLO' .or. itk%P_stadecoupedf == 'DRP' .or.     &
!!!              itk%P_stadecoupedf == 'DES' .or. itk%P_stadecoupedf == 'MAT' .or.     &
!!!              itk%P_stadecoupedf == 'REC' .or. itk%P_stadecoupedf == 'flo' .or.     &
!!!             itk%P_stadecoupedf == 'drp' .or. itk%P_stadecoupedf == 'des' .or.     &
!!!              itk%P_stadecoupedf == 'mat' .or. itk%P_stadecoupedf == 'rec') then

           if (itk%P_stadecoupedf == 7 .or. itk%P_stadecoupedf == 8 .or.     &
              itk%P_stadecoupedf == 9 .or. itk%P_stadecoupedf == 10 .or.     &
              itk%P_stadecoupedf == 11 ) then
            call EnvoyerMsgHistorique(397)
            !stop
            call exit(9)
          endif
! dr 14/08/2014 j'ajoute un test pour ne pas perdre un message
        ! if (itk(i)%P_stadecoupedf.ne.'lax'.and.itk(i)%P_stadecoupedf.ne.'LAX'.and. &
        !     itk(i)%P_stadecoupedf.ne.'sen'.and.itk(i)%P_stadecoupedf.ne.'SEN'.and. &
        !     itk(i)%P_stadecoupedf.ne.'amf'.and.itk(i)%P_stadecoupedf.ne.'AMF'.and. &
        !     itk(i)%P_stadecoupedf.ne.'lan'.and.itk(i)%P_stadecoupedf.ne.'LAN') then
        !     call EnvoyerMsgHistorique(140)
        ! endif

        else
          call EnvoyerMsgHistorique('nbcoupe ',itk%nbcoupe)
          if (itk%P_codemodfauche == 2) then
            do i = 1,itk%nbcoupe
              write(tmp,*) 'P_julfauche,P_hautcoupe,P_lairesiduel,P_msresiduel,P_anitcoupe ',  &
                           itk%P_julfauche(i),itk%P_hautcoupe(i),itk%P_lairesiduel(i),     &
                           itk%P_msresiduel(i),itk%P_anitcoupe(i)
              call EnvoyerMsgHistorique(tmp)

            ! NB - le 20/01/2004 - test de cohérence (avec FR)
              if (itk%P_lairesiduel(i) > 0.0 .and. itk%P_msresiduel(i) <= 0.0) &
                call EnvoyerMsgHistorique('P_lairesiduel >0. et P_msresiduel <=0.0')
            end do
          endif

! debut suppression test initialisation 18/06/08 l'ini se fait dans init_paririeperenne
          if (itk%P_codemodfauche == 3) then
            do i = 1,itk%nbcoupe
              write(tmp,*) 'P_tempfauche,P_hautcoupe,P_lairesiduel,P_msresiduel,P_anitcoupe', &
                           itk%P_tempfauche(i),itk%P_hautcoupe(i),itk%P_lairesiduel(i),   &
                           itk%P_msresiduel(i),itk%P_anitcoupe(i)
              call EnvoyerMsgHistorique(tmp)
            ! NB - le 20/01/2004 - test de cohérence (avec FR)
              if (itk%P_lairesiduel(i) > 0.0 .and. itk%P_msresiduel(i) <= 0.0)    &
                call EnvoyerMsgHistorique('P_lairesiduel>0. et P_msresiduel<=0.0')
            end do
          endif
        endif
      endif
! === fin test suppression initialisation 18/06/08


    ! dr et ml 08/02/2011
      if (itk%P_codepaillage == 2) then
        call EnvoyerMsgHistorique('P_couvermulchplastique',  itk%P_couvermulchplastique)
        call EnvoyerMsgHistorique('P_albedomulchplastique',  itk%P_albedomulchplastique)
      endif
! 08/02/2011
      if(itk%P_codepaillage == 2 ) then
        if(itk%P_nbjres > 0) then
          call EnvoyerMsgHistorique(5600)
          !stop
          call exit(9)
        endif
      endif
! 08/02/2011
      if(itk%P_codepaillage == 2 )then
!        if( (itk%P_ressuite /= 'racines'.or.itk%P_ressuite /= 'rien')) then
 !       if( index(itk%P_ressuite,'racines')>0.and.index(itk%P_ressuite,'aucun')>0) then
 ! DR 03/04/2014 on avait introduit une erreur au recodage de P_ressuite  : on ne peut faire du plaiallge plastique que si
 ! on me laisse pas de resuuite (ressuite different de roots)
 !!!MODIF HISAFE 1 : suppression des chaines de caractères
 !!!      if( index(itk%P_ressuite,'root') == 0) then
        if(itk%P_ressuite == 1) then
          call EnvoyerMsgHistorique(601)
          !stop
          call exit(9)
        endif
      endif


      if (itk%P_codrognage == 2) then
        call EnvoyerMsgHistorique('P_largrogne',itk%P_largrogne)
        call EnvoyerMsgHistorique('P_hautrogne',itk%P_hautrogne)
        call EnvoyerMsgHistorique('P_biorognem',itk%P_biorognem)
        if (itk%P_codcalrogne == 1) then
          call EnvoyerMsgHistorique('P_julrogne ',itk%P_julrogne)
        endif
        if (itk%P_codcalrogne == 2) then
          call EnvoyerMsgHistorique('P_margerogne ',itk%P_margerogne)
        endif
      endif

      if (itk%P_codeclaircie == 2) then
        do i=1,itk%P_nb_eclair
           call EnvoyerMsgHistorique('P_juleclair',itk%P_juleclair(i))
! PB - 09/03/2004 - a mon avis ici, on ne doit pas sortir nbfrote mais plutot P_nbinfloecl
! --        write(fichist,*) 'nbfrote',itk%nbfrote
           call EnvoyerMsgHistorique('P_nbinfloecl',itk%P_nbinfloecl(i))
        enddo
      endif

      if (itk%P_codeffeuil == 2) then
        if (itk%P_codhauteff == 1) call EnvoyerMsgHistorique(380)
        if (itk%P_codhauteff == 2) call EnvoyerMsgHistorique(381)
        if (itk%P_codcaleffeuil == 1) then
          call EnvoyerMsgHistorique('P_laidebeff ',itk%P_laidebeff)
          call EnvoyerMsgHistorique('P_effeuil ',itk%P_effeuil)
        else
          call EnvoyerMsgHistorique('P_juleffeuil ',itk%P_juleffeuil)
          call EnvoyerMsgHistorique('P_laieffeuil ',itk%P_laieffeuil)
        endif
      endif

      if (itk%P_codabri == 2) then

        call EnvoyerMsgHistorique('P_transplastic',itk%P_transplastic)
        call EnvoyerMsgHistorique('P_surfouvre1',itk%P_surfouvre1)
        call EnvoyerMsgHistorique('P_julouvre2',itk%P_julouvre2)
        call EnvoyerMsgHistorique('P_surfouvre2',itk%P_surfouvre2)
        call EnvoyerMsgHistorique('P_julouvre3',itk%P_julouvre3)
        call EnvoyerMsgHistorique('P_surfouvre3',itk%P_surfouvre3)
      endif


    ! le 24/09/2012  test sur P_codecueille=0
      if (itk%P_codcueille == 0) then
        call EnvoyerMsgHistorique(169)
        !stop
        call exit(9)
      endif


! fin mouchard


! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!  domi 07/04/2000 pb si irrigations imposées et rajout d'une irrigation par apores
!   nap n'est plus egal à 0  on concerve nap dans napini
      sc%napini(itk%ipl) = itk%nap
      sc%napNini(itk%ipl) = itk%napN
! dr 17/06/2016 idem pour nbjres
      sc%nbjresini(itk%ipl) = itk%P_nbjres
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*


      if (pg%P_codemsfinal == 1 .and. itk%P_codcueille == 2)then
        call EnvoyerMsgHistorique(399)
      endif


return
end subroutine ITK_Ecriture_Tests


subroutine ITK_Zero(itk)

type(ITK_), intent(INOUT) :: itk  

  itk%ipl = 0
  !!!MODIF HISAFE 1 : suppression des chaines de caractères
  !!!itk%P_ressuite = ''
  itk%P_ressuite = 0
  itk%P_stadecoupedf = 0

  itk%lecfauche = .false.

  itk%P_codefauche = 0
  itk%P_codeaumin = 0
  itk%P_codetradtec = 0
  itk%P_codlocirrig = 0
  itk%P_codlocferti = 0
  itk%P_codemodfauche = 0
  itk%P_codeffeuil = 0
  itk%P_codecalirrig = 0
  itk%P_codestade = 0
  itk%P_codcalrogne = 0
  itk%P_codepaillage = 0
  itk%P_codeclaircie = 0
  itk%P_codcaleffeuil = 0
  itk%P_codrognage = 0
  itk%P_codhauteff = 0
  itk%P_codetaille = 0
  itk%P_codrecolte = 0
  itk%P_codcueille = 0

  itk%P_iplt0 = 0
  itk%P_ilev = 0
  itk%P_iamf = 0
  itk%P_ilax = 0
  itk%P_idrp = 0
  itk%P_isen = 0
  itk%P_imat = 0
  itk%P_irec = 0
  itk%P_irecbutoir = 0
  itk%P_variete = 0
  itk%nap = 0
  itk%napN = 0
! DR 01/02/2011 on a scindé le tableau en 2
!  itk%napS = 0
  itk%P_nbjres = 0
  itk%P_nbjtrav = 0
  itk%nbcoupe = 0
  itk%P_julfauche(:) = 0
  itk%P_nbcueille = 0
  itk%P_ilan = 0
  itk%P_iflo = 0
  itk%P_locirrig = 0
  itk%P_engrais = 0
  itk%P_locferti = 0
  itk%P_cadencerec = 0
  itk%P_julrogne = 0
  itk%P_juleclair(:) = 0
  itk%P_juleffeuil = 0
  itk%P_jultaille = 0
  itk%P_jultrav(:) = 0
  itk%P_julres(:) = 0 !DR 01/02/2011 on a scindé le tableau en 2
  itk%P_coderes(:) = 0
  itk%P_upvttapN(:) = 0
  itk%P_julapI(:) = 0
  itk%P_julapN(:) = 0

  itk%P_profsem = 0.
  itk%P_ratiol = 0.
  itk%P_dosimx = 0.
  itk%P_profmes = 0.
  itk%P_densitesem = 0.
  itk%P_concirr = 0.
  itk%P_effirr = 0.
  itk%P_lairesiduel(:) = 0.
  itk%P_hautcoupedefaut = 0.
  itk%P_hautcoupe(:) = 0.
  itk%P_msresiduel(:) = 0.
  itk%P_anitcoupe(:) = 0.
  itk%P_tempfauche(:) = 0.
  itk%P_largrogne = 0.
  itk%P_margerogne = 0.
  itk%P_hautrogne = 0.
  itk%P_effeuil = 0.
  itk%P_interrang = 0.
  itk%P_orientrang = 0.
  itk%P_Crespc(:) = 0.
  itk%P_proftrav(:) = 0.
  itk%P_profres(:) = 0.
  itk%P_CsurNres(:) = 0.
  itk%P_Nminres(:) = 0.
  itk%P_eaures(:) = 0.
  itk%P_qres(:) = 0.
  itk%P_doseI(:) = 0.
  itk%P_doseN(:) = 0.
  itk%P_h2ograinmin = 0.
  itk%P_h2ograinmax = 0.
  itk%P_CNgrainrec = 0.
  itk%P_huilerec = 0.
  itk%P_sucrerec = 0.
  itk%P_mscoupemini = 0.
  itk%P_nbinfloecl(:) = 0.
  itk%P_laidebeff = 0.
  itk%P_laieffeuil = 0.
  itk%P_biorognem = 0.

  itk%P_codeDSTtass = 0
  itk%P_codedecisemis = 0
  itk%P_codedecirecolte = 0
  itk%P_nbjmaxapressemis = 0
  itk%P_nbjseuiltempref = 0
  itk%P_nbjmaxapresrecolte = 0
  itk%P_codeDSTnbcouche = 0
  itk%P_profhumsemoir = 0.
  itk%P_profhumrecolteuse = 0.
  itk%P_dasemis = 0.
  itk%P_darecolte = 0.

  itk%P_fracN(:) = 0.
  itk%P_codefracappN = 0
  itk%P_Qtot_N = 0

  itk%P_codeDST = 0
  itk%P_dachisel = 0.
  itk%P_dalabour = 0.
  itk%P_rugochisel = 0.
  itk%P_rugolabour = 0.

  itk%P_codabri = 0
  itk%P_julouvre2 = 0
  itk%P_julouvre3 = 0
  itk%P_transplastic = 0.
  itk%P_surfouvre1 = 0.
  itk%P_surfouvre2 = 0.
  itk%P_surfouvre3 = 0.

!  itk%qmulch0 = 0.
!  itk%julappmulch = 0
!  itk%P_typemulch = 0
  itk%P_couvermulchplastique = 0.
  itk%P_albedomulchplastique = 0.

  itk%P_hautmaxtec = 0.
  itk%P_largtec = 0.

  itk%P_codepalissage = 0

  itk%P_coderecolteassoc = 0

  itk%flag_eclairmult = .false.
  itk%flag_pature = .false.
  itk%P_restit(:) = 0
  itk%P_mscoupemini(:) = 0.

return
end subroutine ITK_Zero

end module Itineraire_Technique
 
 
