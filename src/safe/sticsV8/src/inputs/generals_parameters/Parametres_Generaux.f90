!> Module of general parameters
!> - Description of the structure Parametres_Generaux_
!> - reading of the generals parameters codetypeng
module Parametres_Generaux

USE Stics , only: nb_residus_max

! Les codes symboliques du module Plante
integer, parameter :: METHOD_STICS_V6 = 6         !< Code symbolique. define the use herited reading method of Stics V6
integer, parameter :: METHOD_XML_V7 = 7           !< Code symbolique.  define the use herited reading method XML / Javastics
integer, parameter :: PG_LECTURE_OK = 1       !< Code de retour. the reading of the plant file made without error.
integer, parameter :: PG_LECTURE_ERREUR_NO_METHOD = -1      !< Code retour. Error : the method choose is unknown.
! 17/09/2012 DR pour record j'enleve l'allocatable et je mets une taille en dur "parametrable"
! DR 15/11/2013 integer, parameter :: nb_residus_max = 21 remplac� par le use stics plus haut pour Record


TYPE , BIND(C) :: Parametres_Generaux_

  integer :: P_codeh2oact  !< // PARAMETER // code to activate  water stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
  integer :: P_codeinnact  !< // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
  integer :: P_codhnappe  !< // PARAMETER // mode of calculation of watertable level // code 1/2 // PARAM // 0
  integer :: P_codeminopt  !< // PARAMETER // option to maintain a constant water content in bare soil rainfall and PET ni): yes (1), no (2)  // code 1/2 // PARAM // 0
  integer :: P_codeprofmes  !< // PARAMETER // option of depth of calculation for water and nitrogen stocks (1=P_profmes or 2=soil depth) // code 1/2 // PARAM // 0
  integer :: P_codeactimulch  !< // PARAMETER // activation of the accounting for natural mulching in the partitioning of soil evaporation within the soil profile: yes (1), no (2) // code 1/2 // PARAM // 0
  integer :: codeulaivernal  
  integer :: P_codetycailloux  !< // PARAMETER // stones type code // code 1 to 10 // PARAM // 0
  integer :: P_codetypeng  !< // PARAMETER // fertiliser type code // code 1 to 8 // PARAM // 0
  integer :: P_codetypres  !< // PARAMETER // organic residue tyope code // code 1 to 8 // PARAM // 0
!  integer :: P_codemulch  !< // PARAMETER // Typology of crop mulchs: maize cane leafs of sugar cane,… // code 1 to 10 // PARAM // 0
  integer :: P_iniprofil  !< // PARAMETER // Option of smoothing out (function spline) the initial nitrogen and water profile: yes (1), no (0) // code 0/1 // PARAM // 0

!  real :: P_aclim  !< // PARAMETER // climatic component of A // mm // PARAM // 1
  real :: P_beta  !< // PARAMETER // parameter of increase of maximal transpiration when occurs a water stress // SD // PARAM // 1
  real :: P_lvopt  !< // PARAMETER // Optimum root density // cm root.cm-3 soil // PARAM // 1
  real :: P_rayon  !< // PARAMETER // Average radius of roots // cm  // PARAM // 1
  real :: P_fmin1  !< // PARAMETER // Parameter of the constant of mineralization potential rate: K2=P_fmin1/(P_fmin2+P_argi)/(P_fmin3+P_calc) // 10000.day-1 // PARAM // 1
  real :: P_fmin2  !< // PARAMETER // Parameter of the constant of mineralization potential rate: K2=P_fmin1/(P_fmin2+P_argi)/(P_fmin3+P_calc) // % // PARAM // 1
  real :: P_fmin3  !< // PARAMETER // Parameter of the constant of mineralization potential rate: K2=P_fmin1/(P_fmin2+P_argi)/(P_fmin3+P_calc) // % // PARAM // 1
  real :: P_Wh  !< // PARAMETER // ratio N/C of humus // g g–1 // PARAM // 1
  real :: P_concrr  !< // PARAMETER // inorganic N concentration (NH4+NO3-N) in the rain // kgN ha-1 mm-1 // PARAM // 1
  real :: P_TREFh  !< // PARAMETER // temperature of reference for the soil mineralization parameters  // °C // PARAM // 1
  real :: P_difN  !< // PARAMETER // coefficient de diffusion apparente du nitrate dans le sol humide // cm2 jour-1 // PARAM // 1
  real :: P_plNmin  !< // PARAMETER // Minimal amount of precipitation to manage a fertilization // mm day-1 // PARAM // 1
  real :: P_proprac  !< // PARAMETER // Ratio root mass / mass of aerial parts at harvest  // g.g -1 // PARAM // 1
  real :: P_coefb  !< // PARAMETER // parameter defining radiation effect on  conversion efficiency // SD // PARAM // 1
  real :: P_irrlev  !< // PARAMETER // amount of irrigation applied automatically on the sowing day when the model calculates irrigation, to allow germination // mm // PARAM // 1
  real :: P_distdrain  !< // PARAMETER // distance to the drain to calculate watertable height // cm // PARAM // 1
  real :: P_khaut  !< // PARAMETER // Extinction Coefficient connecting leaf area index to height crop // * // PARAM // 1
  real :: P_dacohes  !< // PARAMETER // bulk density under which root growth is reduced due to a lack of cohesion d // g cm-3 // PARAM // 1
  real :: P_daseuilhaut  !< // PARAMETER // Threshold of bulk density of soil below that the root growth  no more possible // g cm-3 // PARAM // 1
  real :: P_daseuilbas  !< // PARAMETER // Threshold of bulk density of soil below that the root growth is not limited // g cm-3 // PARAM // 1
  real :: P_QNpltminINN  !< // PARAMETER // minimal amount of nitrogen in the plant allowing INN computing // kg ha-1 // PARAM // 1
  real :: P_finert  !< // PARAMETER // fraction of the huñus stock inactive for mineralisation  (= stable OM/ total OM) // SD // PARAM // 1
  real :: P_pHminvol  !< // PARAMETER // P_pH above which the fertilizer volatilisation is null // P_pH // PARAM // 1
  real :: P_pHmaxvol  !< // PARAMETER //  P_pH beyond which the fertilizer volatilisation is maximale // P_pH // PARAM // 1
  real :: P_Vabs2  !< // PARAMETER // nitrogen uptake rate for which  fertilizer losts of  are divided by 2 // kg/ha/jour // PARAM // 1
  real :: P_Xorgmax  !< // PARAMETER // maximal amount of oraganised nitrogen coming from the mineral fertilizer  // kg.ha-1 // PARAM // 1
  real :: P_hminm  !< // PARAMETER // moisture (proportion of field capacity) below which mineralisation rate is nil // g eau g-1 sol // PARAM // 1
  real :: P_hoptm  !< // PARAMETER // moisture (proportion of field capacity) above which mineralisation rate is maximum // g eau g-1 sol // PARAM // 1
  real :: P_hminn  !< // PARAMETER // moisture (proportion of field capacity) below which nitrification rate is nil // g eau g-1 sol // PARAM // 1
  real :: P_hoptn  !< // PARAMETER // moisture (proportion of field capacity) at which nitrification rate is maximum // g eau g-1 sol // PARAM // 1
  real :: P_pHminnit  !< // PARAMETER // effect of the P_pH on nitrification (threshold mini) // P_pH // PARAM // 1
  real :: P_pHmaxnit  !< // PARAMETER // effect of the P_pH on nitrification (threshold maxi) // P_pH // PARAM // 1
  real :: P_tnitopt  !< // PARAMETER // cardinal temperature for nitrification // °C // PARAM // 1
  real :: P_tnitmax  !< // PARAMETER // cardinal temperature for nitrification // °C // PARAM // 1
  real :: P_tnitmin  !< // PARAMETER // cardinal temperature for nitrification // °C // PARAM // 1
  real :: P_pminruis  !< // PARAMETER // Minimal amount of precipitation to start a drip  // mm day-1 // PARAM // 1
  real :: P_diftherm  !< // PARAMETER // soil thermal diffusivity // cm2 s-1 // PARAM // 1
  real :: P_Bformnappe  !< // PARAMETER // coefficient of water table shape (artificial drained soil) // SD // PARAM // 1
  real :: P_rdrain  !< // PARAMETER // drain radius // cm // PARAM // 1
  real :: P_hcccx(10)  !< // PARAMETER // field capacity moisture of each pebble type  (table) // % pondéral // PARAM // 1
  real :: P_masvolcx(10)  !< // PARAMETER // volumetric mass (bulk) of pebbles // g cm-3 // PARAM // 1
  real :: P_engamm(8)  !< // PARAMETER // proportion of ammonium in the fertilizer // SD // PARAM // 1
  real :: P_voleng(8)  !< // PARAMETER // maximal fraction of mineral fertilizer that can be volatilized  // SD // PARAM // 1
  real :: P_orgeng(8)  !< // PARAMETER // maximal quantity of mineral fertilizer that can be organized in the soil (fraction for type 8) // kg ha-1 // PARAM // 1
  real :: P_deneng(8)  !< // PARAMETER // proportion of the ñineral fertilizer that can be denitrified (useful if codenit not active) // SD // PARAM // 1
  real :: P_parsurrg  !< // PARAMETER // coefficient PAR/RG for the calculation of PAR  // * // PARAM // 1

! Residus

! DR et ML et BM 23/06/09
! ***********************
! introduction des modifications de BM
! modif Bruno2004   : le P_pH varie après l'apport de NH4 lisier
! DR et ML 15/10/09 on augmente le nombre de residus (de 12 à 14) pour prendre en compte les mulchs
!  integer :: nbresidus = 14 DR 02/02/2011 on en a 21 maintenant
!  integer :: nbresidus = 21
!  real, allocatable :: P_kbio(:)  !< // PARAMETER // constant of mortality rate of microbial biomass // day -1 // PARAM // 1
!  real, allocatable :: P_yres(:)  !< // PARAMETER // Carbon assimilation yield of the microflora // g g-1 // PARAM // 1
!  real, allocatable :: P_akres(:)  !< // PARAMETER // parameter of organic residues decomposition: kres=P_akres+P_bkres/P_CsurNres // day-1 // PARAM // 1
!  real, allocatable :: P_bkres(:)  !< // PARAMETER // parameter of organic residues decomposition: kres=P_akres+P_bkres/P_CsurNres // g g-1 // PARAM // 1
!  real, allocatable :: P_awb(:)  !< // PARAMETER // parameter  of organic residues decomposition: CsurNbio=P_awb+P_bwb/P_CsurNres // SD // PARAM // 1
!  real, allocatable :: P_bwb(:)  !< // PARAMETER // parameter of organic residues decomposition: CsurNbio=P_awb+P_bwb/P_CsurNres // g g-1 // PARAM // 1
!  real, allocatable :: P_cwb(:)  !< // PARAMETER // Minimum ratio C/N of microbial biomass in the relationship: CsurNbio=P_awb+P_bwb/P_CsurNres // g g-1 // PARAM // 1
!  real, allocatable :: P_ahres(:)  !< // PARAMETER // parameter of organic residues humification: hres=1-P_ahres*P_CsurNres/(P_bhres+P_CsurNres) // g g-1 // PARAM // 1
!  real, allocatable :: P_bhres(:)  !< // PARAMETER // parameter of organic residues humification: hres=1-P_ahres*P_CsurNres/(P_bhres+P_CsurNres) // g g-1 // PARAM // 1
!  real, allocatable :: P_CNresmin(:)  !< // PARAMETER // minimum observed value of ratio C/N of organic residues  // g g-1 // PARAM // 1
!  real, allocatable :: P_CNresmax(:)  !< // PARAMETER // maximum observed value of ratio C/N of organic residues // g g-1 // PARAM // 1
!  real, allocatable :: P_CroCo(:)  !< // PARAMETER // parameter of organic residues decomposition  //  SD// PARAM // 1
!  real, allocatable :: P_qmulchruis0(:)  !< // PARAMETER // Amount of mulch to annul the drip // t ha-1 // PARAM // 1
!  real, allocatable :: P_mouillabilmulch(:)  !< // PARAMETER // maximum wettability of crop mulch // mm t-1 ha // PARAM // 1
!  real, allocatable :: P_kcouvmlch(:)  !< // PARAMETER // Extinction Coefficient reliant la quantité de paillis végétal au taux de couverture du sol // * // PARAM // 1
!  real, allocatable :: P_albedomulchresidus(:)  !< // PARAMETER // P_albedo of crop mulch // SD // PARAM // 1
!  real, allocatable :: P_Qmulchdec(:)  !< // PARAMETER // maximal amount of decomposing mulch // t C.ha-1 // PARAM // 1


! DR 17/09/2012 on enleve les allocatable
  integer :: nbresidus = 21  
  real, dimension(nb_residus_max) :: P_kbio  !< // PARAMETER // constant of mortality rate of microbial biomass // day -1 // PARAM // 1
  real, dimension(nb_residus_max) :: P_yres  !< // PARAMETER // Carbon assimilation yield of the microflora // g g-1 // PARAM // 1
  real, dimension(nb_residus_max) :: P_akres  !< // PARAMETER // parameter of organic residues decomposition: kres=P_akres+P_bkres/P_CsurNres // day-1 // PARAM // 1
  real, dimension(nb_residus_max) :: P_bkres  !< // PARAMETER // parameter of organic residues decomposition: kres=P_akres+P_bkres/P_CsurNres // g g-1 // PARAM // 1
  real, dimension(nb_residus_max) :: P_awb  !< // PARAMETER // parameter  of organic residues decomposition: CsurNbio=P_awb+P_bwb/P_CsurNres // SD // PARAM // 1
  real, dimension(nb_residus_max) :: P_bwb  !< // PARAMETER // parameter of organic residues decomposition: CsurNbio=P_awb+P_bwb/P_CsurNres // g g-1 // PARAM // 1
  real, dimension(nb_residus_max) :: P_cwb  !< // PARAMETER // Minimum ratio C/N of microbial biomass in the relationship: CsurNbio=P_awb+P_bwb/P_CsurNres // g g-1 // PARAM // 1
  real, dimension(nb_residus_max) :: P_ahres  !< // PARAMETER // parameter of organic residues humification: hres=1-P_ahres*P_CsurNres/(P_bhres+P_CsurNres) // g g-1 // PARAM // 1
  real, dimension(nb_residus_max) :: P_bhres  !< // PARAMETER // parameter of organic residues humification: hres=1-P_ahres*P_CsurNres/(P_bhres+P_CsurNres) // g g-1 // PARAM // 1
  real, dimension(nb_residus_max) :: P_CNresmin  !< // PARAMETER // minimum observed value of ratio C/N of organic residues  // g g-1 // PARAM // 1
  real, dimension(nb_residus_max) :: P_CNresmax  !< // PARAMETER // maximum observed value of ratio C/N of organic residues // g g-1 // PARAM // 1
!26/07/2012 ajout du parametre de BM
  real, dimension(nb_residus_max) :: P_CroCo  !< // PARAMETER // parameter of organic residues decomposition  //  SD// PARAM // 1
! DR 02/02/2011 ces parametres sont deplacés dans les residus
  real, dimension(nb_residus_max) :: P_qmulchruis0  !< // PARAMETER // Amount of mulch to annul the drip // t ha-1 // PARAM // 1
!  real :: decomposmulch(0:7)  DR 02/02/2011 on supprime
  real, dimension(nb_residus_max) :: P_mouillabilmulch  !< // PARAMETER // maximum wettability of crop mulch // mm t-1 ha // PARAM // 1
  real, dimension(nb_residus_max) :: P_kcouvmlch  !< // PARAMETER // Extinction Coefficient reliant la quantité de paillis végétal au taux de couverture du sol // * // PARAM // 1
  real, dimension(nb_residus_max) :: P_albedomulchresidus  !< // PARAMETER // P_albedo of crop mulch // SD // PARAM // 1
  real, dimension(nb_residus_max) :: P_Qmulchdec  !< // PARAMETER // maximal amount of decomposing mulch // t C.ha-1 // PARAM // 1
! DR 02/02/2011 on an a plus besoin  real :: Cmulchdec(0:7)







! PB - je rajoute une variable de controle qui stocke le nb de mulch lus dans le fichier de paramètres généraux
! DR 02/02/2011 devient inutile les parametres mulch sont avec les residus
!  integer :: nbMulchsLus

! DR le 03/01/06 parametres inaki stressphot
!  real :: P_dltamsminsen  !< // PARAMETER // threshold value of deltams from which the photoperiodic effect on senescence is maximal // t ha-1j-1 // PARPLT // 1
!  real :: P_alphaphot  !< // PARAMETER // parameter of photoperiodic effect on leaf lifespan // P_Q10 // PARPLT // 1

! DR 29/10/07
  integer :: P_codesymbiose  !< // PARAMETER // option of calculation of symbiotic fixation // code 1/2 // PARAM // 0

! NB le 15/02/06 parametres DST
  real :: P_proflabour  !< // PARAMETER // soil minimal depth for ploughing when soil compaction is activated // cm // PARAM // 1
  real :: P_proftravmin  !< // PARAMETER // soil minimal depth for chisel tillage when soil compaction is activated // cm // PARAM // 1
  real :: P_trefr  !< // PARAMETER // temperature of reference for the soil mineralization parameters  // °C // PARAM // 1

! DR et ML et BM et EJ 23/06/09
! remplacent FTEM1 à FTEM4
! on a viré fpHnx
  real :: P_FTEMh  !< // PARAMETER // Parameter 2 of the temperature function on the decomposition rate of humus // °K-1 // PARAM // 1
  real :: P_FTEMr  !< // PARAMETER // Parameter 2 of the temperature function on the decomposition rate of organic residues // °K-1 // PARAM // 1
  real :: P_FTEMra  !< // PARAMETER // Parameter 1 of the temperature function on the decomposition rate of organic residues // * // PARAM // 1
  real :: P_FTEMha  !< // PARAMETER // Parameter 1 of the temperature function on the decomposition rate of humus // * // PARAM // 1
  real :: P_fhminsat  !< // PARAMETER // soil mineralisation rate at water saturation // SD // PARAM // 1
  real :: P_fnx  !< // PARAMETER // maximum nitrification rate into the soil // day-1 // PARAM // 1
! fin  DR et ML et BM et EJ 23/06/09

  real :: P_rationit  !< // PARAMETER // ratio between N2O emisson and total nitrification // kg.ha-1.j-1 // PARAM // 1
  real :: P_ratiodenit  !< // PARAMETER // ratio between N2O emisson and total denitrification // kg.ha-1.j-1 // PARAM // 1

! DR 07/03/08 on a mis en parametre
  real :: P_prophumtasssem  !< // PARAMETER // field capacity proportion above witch compaction may occur (to delay sowing) // SD // PARAM // 1
  real :: P_prophumtassrec  !< // PARAMETER // field capacity proportion above witch compaction may occur (to delay harvest) // SD // PARAM // 1

  integer :: P_codeinitprec  !< // PARAMETER // reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0
  integer :: P_flagEcriture  !< // PARAMETER // option for writing the output files (1 = mod_history.sti, 2=daily outputs,4= report outputs, 8=balance outputs,16 = profile outputs, 32 = debug  outputs; 64= screen outputs, 128 = agmip outputs) add them to have several types of outputs //  code 1/2 // PARAM // 1
  integer :: P_codesensibilite  !< // PARAMETER // code to activate the sensitivity analysis version of the model: yes (1), no (2) // code 1/2 // PARAM // 0
  integer :: P_codefrmur  !< // PARAMETER // code defining the maturity status of the fruits in the output  variable CHARGEFRUIT (1 = including ripe fruits (last box N);  2 = excluding ripe fruits (first N-1 boxes)) // code 1/2 // PARAM // 0

! ** OFFRNODU
  integer :: P_codefxn  !< // PARAMETER // option to activate the chosen way to compute fxN // code 1/2 // PARAM // 0

!: pour Guenaelle - différentes pour chaque plante ??
  integer :: P_codemsfinal  !< // PARAMETER // option defining the biomass and yield conservation after harvest (1 = yes (values maintained equal to harvest) ; 2 = no (values set at 0)) // code 1/2 // PARAM // 0

!: NB - le 07/06/2004 - calcul de psibase
  real :: P_psihumin  !< // PARAMETER // soil potential corresponding to wilting point // Mpa // PARAM // 1
  real :: P_psihucc  !< // PARAMETER // soil potential corresponding to field capacity  // Mpa // PARAM // 1

!  real :: P_masecmeta  !< // PARAMETER // biomass of the plantlet supposed to be composed of metabolic nitrogen // t ha-1 // PARPLT // 1

! DR le 04/11/05
  integer :: P_codeoutscient

  integer :: P_codeseprapport  !< // PARAMETER // choice of the kind of column separator in the rapport.sti file: separator chosen in P_separateurrapport (2), space (1) // code 1/2 // STATION // 0
!!!MODIF HISAFE 1 : suppression des chaines de caract�res
!!!  character :: P_separateurrapport  !< // PARAMETER // column separator in rapport.sti file // caractere // PARAM // 0


!: micro-climat ??
  integer :: P_codemicheur  !< // PARAMETER // option of calculation of hourly microclimatic outputs (output file humidite.sti) yes (1) no (2) // code 1/2 // PARAM // 0

! DR et ML et BM 22/06/09
! ***********************
! introduction des modifications de BM
! modif Bruno2004   : le P_pH varie après l'apport de NH4 lisier
  real :: P_alphapH  !< // PARAMETER // maximal soil pH variation per unit of inorganic N added with slurry // kg-1 ha //PARAM //1
  real :: P_dpHvolmax  !< // PARAMETER // maximal P_pH increase following the application of organic residue sutch as slurry // SD // PARAM // 1
  real :: P_pHvols  !< // PARAMETER // maximal soil P_pH above which soil P_pH is not affected by addition of organic residue (sutch as slurry)  // SD // PARAM // 1
  real :: P_fredkN  !< // PARAMETER // reduction factor of decomposition rate of residues when mineral N is limiting // SD // PARAM // 1
  real :: P_fredlN  !< // PARAMETER // reduction factor of decomposition rate of biomass when mineral N is limiting // SD // PARAM // 1
  real :: P_fNCBiomin  !< // PARAMETER // maximal reduction factor of the ratio N/C of the microbial biomass when nitrogen limits decomposition (between 0 and 1) // SD // PARAM // 1
! 22/06/09 FIN introduction des modifications de BM

!  26/07/2012 le trio on ajoute 2 parametres dans param_gen
  real :: P_tnitopt2    !< // PARAMETER // optimal temperature (2/2) for nitrification // grade C
  real :: P_y0msrac     !> // PARAMETER // minimal amount of root mass at harvest (when aerial biomass is nil)  // t.ha-1 // PARAM // 1
  real :: P_fredNsup    !< // PARAMETER // additional reduction factor of residues decomposition rate when mineral N is very limited in soil // SD // PARAM // 1
  real :: P_Primingmax  !< // PARAMETER // maximum priming ratio (relative to SOM decomposition rate) // SD //PARAM // 1



end type Parametres_Generaux_



CONTAINS




subroutine Lecture_Parametres_Generaux(pg, path, pathtempopar, mode)

  implicit none

!: ARGUMENTS

  type(Parametres_Generaux_), intent(OUT)   :: pg  
  integer, optional,          intent(INOUT) :: mode  

   ! enabling_record :le chemin pour accéder à la config
   character(len=255), intent(IN) :: path ! enabling_record
   ! enabling_record :le chemin pour accéder directement à la config
   character(len=255), intent(IN) :: pathtempopar ! enabling_record



  if ( present(mode) ) then

    select case(mode)

      case(METHOD_STICS_V6)
        call Lecture_Parametres_Generaux_V6(pg, path, pathtempopar)

    end select

  else

    call Lecture_Parametres_Generaux_V6(pg, path, pathtempopar)

  endif

return
end subroutine Lecture_Parametres_Generaux


!> ********************************************************
!! *    lecture et initialisation des parametres generaux *
!! *    fichier param.par                                 *
!! *    version 5.1 - 23/02/2004                          *
!< ********************************************************
subroutine Lecture_Parametres_Generaux_V6(pg, path , pathtempopar)

USE Messages

  implicit none

!: ARGUMENTS

  type(Parametres_Generaux_), intent(OUT) :: pg  

     ! enabling_record :le chemin pour accéder à la config
   character(len=255), intent(IN) :: path ! enabling_record
   ! enabling_record :le chemin pour accéder directement à la config
   character(len=255), intent(IN) :: pathtempopar ! enabling_record


!: VARIABLES LOCALES
  integer :: icx  !>  
  integer :: ieng  !>  
  integer :: ires ! variables de boucle  
  integer :: eof           !> variable de gestion des erreurs de lecture  

character :: P_separateurrapport  !< // PARAMETER // column separator in rapport.sti file // caractere // PARAM // 0

! DR 19/1/2013 pour Record
  ! to get the full path
  integer ib0                                               ! enabling_record
  integer ib1                                               ! enabling_record
  character(len=300) :: filepluspath                        ! enabling_record
  ib0 = len_trim(pathtempopar)                           ! enabling_record
  if (ib0 .ne. 0 ) then                                     ! enabling_record
     filepluspath =  pathtempopar                        ! enabling_record
  else
     ib1 = len_trim(path)                                   ! enabling_record
     if (ib1 .eq. 0 ) then                                     ! enabling_record
        filepluspath = "tempopar.sti"                          ! enabling_record
     else                                                      ! enabling_record
        filepluspath = path(1:ib1) // '/' // "tempopar.sti" ! enabling_record
     endif                                                     ! enabling_record
  endif




! fin record

!: lecture du fichier TEMPO.STI
!  open (36,file='tempopar.sti', status='old')
  open (36,file=filepluspath, status='old') ! enabling_record
! simulation options
! **********************
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_codeinnact
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_codeh2oact
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_codeminopt
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_iniprofil
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_codeprofmes
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_codeinitprec
!: 29/04/03 - rajout de la suppression le 09/01/02
! *- P_codemsfinal=1 -->maintient de recolte à fin
! *- P_codemsfinal=2 -->masec à 0 à la recolte
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_codemsfinal
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_codeactimulch
!: si codeulaivernal = 1 la vernalisation joue sur ulai, si = 0 joue pas
! *- on met en dur dans initial.for : codeulaivernal = 1
! --      read (36,*)
! --      read (36,*,iostat=eof,end=150) codeulaivernal
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_codefrmur
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_codemicheur
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_codeoutscient
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_codeseprapport
  read (36,*)
  read (36,fmt='(A1)', iostat=eof,end=150) P_separateurrapport
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_codesensibilite
  read (36,*)
  ! DR 29/08/2012 maintenant je lis flagecriture à la  place de codesig
  read (36,*,iostat=eof,end=150) pg%P_flagEcriture

! radiation interception
! **********************
! 09/01/2012 deplace de station vers param.par
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_parsurrg
!
! shoot growth
! **********************
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_coefb
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_proprac
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_y0msrac
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_khaut
!  read (36,*)
!  read (36,*,iostat=eof,end=150) pg%P_dltamsminsen
!  read (36,*)
!  read (36,*,iostat=eof,end=150) pg%P_alphaphot
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_dacohes
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_daseuilbas
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_daseuilhaut
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_beta
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_lvopt
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_rayon
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_difN
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_concrr
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_plNmin
  read (36,*)
!: NB - le 28/03/02
  read (36,*,iostat=eof,end=150) pg%P_irrlev
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_QNpltminINN
!  read (36,*)
!  read (36,*,iostat=eof,end=150) pg%P_masecmeta
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_codesymbiose
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_codefxn
  read (36,*)
! mineralisation and fertiliser losts
! ***********************************
  read (36,*,iostat=eof,end=150) pg%P_FTEMh
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_FTEMha
! DR tref scindé en P_trefr et P_trefh
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_TREFh
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_FTEMr
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_FTEMra
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_trefr

! DR et ML et BM et EJ 23/06/09
! on remplace FTEM1 à 4 par P_FTEMha,P_FTEMra,P_FTEMh,P_FTEMr
! on supprime FTEM
! on remplace FHUM par  P_HMINM
!      read (36,*)
!      read (36,*,err=250) FTEM
!      read (36,*)
!      read (36,*,err=250) FHUM

! modif Bruno : introduction d'une fraction inerte de MO humifiée
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_finert
! fin de modif
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_fmin1
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_fmin2
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_fmin3
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_Wh
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_pHminvol
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_pHmaxvol
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_Vabs2
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_Xorgmax
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_hminm
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_hoptm
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_hminn
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_hoptn
! DR et ML et BM et EJ 23/06/09
! on remplace fpHnx par P_fnx
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_fnx
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_pHminnit
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_pHmaxnit
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_tnitmin
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_tnitopt
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_tnitopt2
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_tnitmax
! DR ajout nadine pour emission de N2O
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_rationit
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_ratiodenit
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_alphapH
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_dpHvolmax
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_pHvols
! DR 040808 rajout du parametre de mineral
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_fhminsat

  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_fredkN
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_fredlN

! P_fNCBiomin = facteur de réduction du rapport N/C de la biomasse en conditions de limitation en N
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_fNCBiomin
! 26/07/2012 on ajoute 2 parametres de mineralisation de BM
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_fredNsup
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_Primingmax

! soil
!**************
! pertes gazeuses: dénitrification et volatilisation
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_pminruis
!  read (36,*)
!  read (36,*,iostat=eof,end=150) pg%P_aclim
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_diftherm
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_Bformnappe
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_rdrain
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_psihumin
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_psihucc
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_prophumtasssem
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_prophumtassrec
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_codhnappe
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_distdrain
  ! technics
  !***************
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_proflabour
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_proftravmin

  ! typology
  !***************
! types of pebbles
!*****************
  read (36,*)
  read (36,*,iostat=eof,end=150) pg%P_codetycailloux
  read (36,*)
  do icx = 1,10
    read (36,*,iostat=eof,end=150) pg%P_masvolcx(icx)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_hcccx(icx)
    read (36,*)
  end do
!types of mineral fertilisers
!********************************
  read (36,*,iostat=eof,end=150) pg%P_codetypeng
  read (36,*)
  do ieng = 1,8
    read (36,*,iostat=eof,end=150) pg%P_engamm(ieng)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_orgeng(ieng)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_deneng(ieng)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_voleng(ieng)
    read (36,*)
  end do
!types of residues
!**************************
!: résidus
  read (36,*,iostat=eof,end=150) pg%P_codetypres
! DR et ML et BM 23/06/09
! ***********************
! introduction des modifications de BM
! 8 devient nbresidus initialisé dans initsimul
! PB - nbresidus ajouté dans les PG. TODO : lire nbresidus dans le fichier plutot que valeur en dur dans le code.
! On alloue les tableaux de résidus
! DR 17/09/2012 j'enleve les allocatable pour record
!  allocate(pg%P_kbio(pg%nbresidus))
!  allocate(pg%P_yres(pg%nbresidus))
!  allocate(pg%P_akres(pg%nbresidus))
!  allocate(pg%P_bkres(pg%nbresidus))
!  allocate(pg%P_awb(pg%nbresidus))
!  allocate(pg%P_bwb(pg%nbresidus))
!  allocate(pg%P_cwb(pg%nbresidus))
!  allocate(pg%P_ahres(pg%nbresidus))
!  allocate(pg%P_bhres(pg%nbresidus))
!  allocate(pg%P_CNresmin(pg%nbresidus))
!  allocate(pg%P_CNresmax(pg%nbresidus))
!  allocate(pg%P_qmulchruis0(pg%nbresidus))
!  allocate(pg%P_mouillabilmulch(pg%nbresidus))
!  allocate(pg%P_kcouvmlch(pg%nbresidus))
!  allocate(pg%P_albedomulchresidus(pg%nbresidus))
!  allocate(pg%P_Qmulchdec(pg%nbresidus))
!  allocate(pg%P_CroCo(pg%nbresidus))



  do ires=1, pg%nbresidus
    read (36,*,iostat=eof,end=200,err=250)
    read (36,*,iostat=eof,end=150) pg%P_CroCo(ires)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_akres(ires)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_bkres(ires)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_awb(ires)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_bwb(ires)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_cwb(ires)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_ahres(ires)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_bhres(ires)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_kbio(ires)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_yres(ires)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_CNresmin(ires)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_cnresmax(ires)
    read (36,*)
! DR 02/02/2011 on deplace ces parametres dans les residus
    read (36,*,iostat=eof,end=150) pg%P_qmulchruis0(ires)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_mouillabilmulch(ires)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_kcouvmlch(ires)
    read (36,*)
    read (36,*,iostat=eof,end=150) pg%P_albedomulchresidus(ires)



! 16/10/09 ajout de 2 parametres BM (decomposition du mulch)
    read (36,*,iostat=eof,end=150)
    read (36,*,iostat=eof,end=200,err=250) pg%P_Qmulchdec(ires)





! DR 02/02/2011 on a deplacé les parametres mulch dans les residus
!  read (36,*,iostat=eof,end=150) pg%P_codemulch
!  do icx = 1,4
!    read (36,*,iostat=eof,end=200)
!    read (36,*,iostat=eof,end=200) pg%decomposmulch(icx)
!    read (36,*,iostat=eof,end=200)
!    read (36,*,iostat=eof,end=200) pg%P_qmulchruis0(icx)
!    read (36,*)
!    read (36,*,iostat=eof,end=200) pg%P_mouillabilmulch(icx)
!    read (36,*)
!    read (36,*,iostat=eof,end=200) pg%P_kcouvmlch(icx)
!    read (36,*)
!    read (36,*,iostat=eof,end=200) pg%albedomulch(icx)
! 16/10/09 ajout de 2 parametres BM (decomposition du mulch)
!    read (36,*,iostat=eof,end=200)
!    read (36,*,iostat=eof,end=200,err=250) pg%P_Qmulchdec(icx)
! DR 20/20/2011 plus utile
!    read (36,*,iostat=eof,end=200)
!    read (36,*,iostat=eof,end=200,err=250) pg%Cmulchdec(icx)


    !!!if (eof /= 0) EXIT ! DR 02/02/2011 pour le moment je le vire ca coince

  end do

200  close(36)

     return

! on a atteint la fin du fichier prématurément
!TODO: messages historique et/ou codeRetour ?
150 call EnvoyerMsgHistorique(105)
write (*,*) 'erreur 150'
    !stop
    call exit(9)

! on a rencontré une erreur lors de la lecture des paramètres
250 call EnvoyerMsgHistorique(105)
write (*,*) 'erreur 250'
    !stop
    call exit(9)



end subroutine Lecture_Parametres_Generaux_V6



subroutine Parametres_Generaux_Zero(pg)

type(Parametres_Generaux_), intent(OUT) :: pg  

  pg%P_codeh2oact = 0
  pg%P_codeinnact = 0
  pg%P_codhnappe = 0
  pg%P_codeminopt = 0
  pg%P_codeprofmes = 0
  pg%P_codeactimulch = 0
  pg%codeulaivernal = 0
  pg%P_codetycailloux = 0
  pg%P_codetypeng = 0
  pg%P_codetypres = 0
!  pg%P_codemulch = 0
  pg%P_iniprofil = 0
!  pg%P_aclim = 0.
  pg%P_beta = 0.
  pg%P_lvopt = 0.
  pg%P_rayon = 0.
  pg%P_fmin1 = 0.
  pg%P_fmin2 = 0.
  pg%P_fmin3 = 0.
  pg%P_Wh = 0.
  pg%P_concrr = 0.
  pg%P_FTEMh = 0.
  pg%P_TREFh = 0.
  pg%P_difN = 0.
  pg%P_plNmin = 0.
  pg%P_proprac = 0.
  pg%P_coefb = 0.
  pg%P_irrlev = 0.
  pg%P_distdrain = 0.
  pg%P_khaut = 0.
  pg%P_dacohes = 0.
  pg%P_daseuilhaut = 0.
  pg%P_daseuilbas = 0.
  pg%P_QNpltminINN = 0.
  pg%P_finert = 0.
  pg%P_pHminvol = 0.
  pg%P_pHmaxvol = 0.
  pg%P_Vabs2 = 0.
  pg%P_Xorgmax = 0.
  pg%P_hminm = 0.
  pg%P_hoptm = 0.
  pg%P_hminn = 0.
  pg%P_hoptn = 0.
  pg%P_fnx = 0.
  pg%P_pHminnit = 0.
  pg%P_pHmaxnit = 0.
  pg%P_tnitopt = 0.
  pg%P_tnitmax = 0.
  pg%P_tnitmin = 0.
  pg%P_pminruis = 0.
  pg%P_diftherm = 0.
  pg%P_Bformnappe = 0.
  pg%P_rdrain = 0.
  pg%P_hcccx(:) = 0.
  pg%P_masvolcx(:) = 0.
  pg%P_engamm(:) = 0.
  pg%P_voleng(:) = 0.
  pg%P_orgeng(:) = 0.
  pg%P_deneng(:) = 0.
!  if (allocated(pg%P_kbio)) pg%P_kbio(:) = 0.
!  if (allocated(pg%P_yres)) pg%P_yres(:) = 0.
!  if (allocated(pg%P_akres)) pg%P_akres(:) = 0.
!  if (allocated(pg%P_bkres)) pg%P_bkres(:) = 0.
!  if (allocated(pg%P_awb)) pg%P_awb(:) = 0.
!  if (allocated(pg%P_bwb)) pg%P_bwb(:) = 0.
!  if (allocated(pg%P_cwb)) pg%P_cwb(:) = 0.
!  if (allocated(pg%P_ahres)) pg%P_ahres(:) = 0.
!  if (allocated(pg%P_bhres)) pg%P_bhres(:) = 0.
!  if (allocated(pg%P_CNresmin)) pg%P_CNresmin(:) = 0.
!  if (allocated(pg%P_CNresmax)) pg%P_CNresmax(:) = 0.
! DR 02/02/2011 j'essaie d'allouer les parametres residus à 0
!  if (allocated(pg%P_qmulchruis0(ires)) pg%P_qmulchruis0(:) = 0.
!  if (allocated(pg%P_mouillabilmulch(ires))pg%P_mouillabilmulch(:) = 0.
!  if (allocated(pg%P_kcouvmlch(ires))pg%P_kcouvmlch(:) = 0.
!  if (allocated(pg%albedomulch(ires))pg%albedomulch(:) = 0.
!  if (allocated(pg%P_Qmulchdec(ires))pg%P_Qmulchdec(:) = 0.
!  pg%P_qmulchruis0(:) = 0.
!  pg%decomposmulch(:) = 0.
!  pg%P_mouillabilmulch(:) = 0.
!  pg%P_kcouvmlch(:) = 0.
!  pg%albedomulch(:) = 0.


!  pg%P_dltamsminsen = 0.
!  pg%P_alphaphot = 0.
  pg%P_codesymbiose = 0
  pg%P_proflabour = 0.
  pg%P_proftravmin = 0.
  pg%P_trefr = 0.
  pg%P_FTEMr = 0.
  pg%P_FTEMra = 0.
  pg%P_FTEMha = 0.
  pg%P_rationit = 0.
  pg%P_ratiodenit = 0.
  pg%P_alphapH = 0.
  pg%P_dpHvolmax = 0.
  pg%P_pHvols = 0.
  pg%P_prophumtasssem = 0.
  pg%P_prophumtassrec = 0.
  pg%P_fhminsat = 0.
  pg%P_codeinitprec = 0
  pg%P_flagEcriture = 0
  pg%P_codesensibilite = 0
  pg%P_codefrmur = 0
  pg%P_codefxn = 0
  pg%P_codemsfinal = 0
  pg%P_psihumin = 0.
  pg%P_psihucc = 0.
!  pg%P_masecmeta = 0.
  pg%P_codeoutscient = 0
  pg%P_codeseprapport = 0
!!!MODIF HISAFE 1 : suppression des chaines de caract�res
!!!  pg%P_separateurrapport = ''
  pg%P_codemicheur = 0
  pg%P_fredkN = 0.
  pg%P_fredlN = 0.

return
end subroutine Parametres_Generaux_Zero

end module Parametres_Generaux
 
 
