!> Module of Plant parameters
!> - Description  of the structure Plante_
!> - reading of the plant parameters in ficpltn.txt
module Plante

implicit none

! Les codes symboliques du module Plante
integer, parameter :: PLANTE_METHOD_STICS_V6 = 6         !< Code symbolique. define the use herited reading method of Stics V6
integer, parameter :: PLANTE_METHOD_XML_V7 = 7           !< Code symbolique. define the use herited reading method of XML / Javastics
integer, parameter :: PLANTE_LECTURE_OK = 1       !< Code de retour. the reading of the plant file made without error.
integer, parameter :: PLANTE_LECTURE_ERREUR_NO_METHOD = -1      !< Code retour. Erreur : the method choose is unknown.
integer, parameter :: PLANTE_LECTURE_ERREUR_EOF = -2            !< Code retour. Erreur : end of file too earlier.
integer, parameter  :: nb_variete_max = 30



!  The derived type 'Plante_'

TYPE , BIND(C) :: Plante_

!!!MODIF HISAFE 7 : déplacement de variables
!!!Varibles déplacées de SticsCommun

  logical   :: estDominante  
  integer   :: ipl  

! fichiers relatifs à la plante. ! TODO: est-ce que ça a vraiment sa place ici finalement ?
!DR 19/07/2012 j'allonge le nom du fichier de 25 à 50
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=50) :: P_fplt  !< // PARAMETER // name of the plant file // SD // P_USM/USMXML // 0
!!!  character(len=50) :: P_ftec  !< // PARAMETER // name of the technique file // SD // P_USM/USMXML // 0
!!!  character(len=50) :: P_flai  !< // PARAMETER // name of the file LAI // SD // P_USM/USMXML // 0

!  parametres plantes
!***********************
! DR 14/09/2012 j'ajoute un param statique = nb_variete et je le mets à 30 maxi, je remplace (12) par(12)
!integer     :: nb_variete_max=12
  real      :: P_adil  !< // PARAMETER // Parameter of the critical curve of nitrogen needs [Nplante]=P_adil MS^(-P_bdil) // N% MS // PARPLT // 1
  real      :: P_bdens  !< // PARAMETER // minimal density from which interplant competition starts // plants m-2 // PARPLT // 1
  real      :: P_bdil  !< // PARAMETER // parameter of the critical curve of nitrogen needs [Nplante]=P_adil MS^(-P_bdil) // SD // PARPLT // 1
  real      :: P_belong  !< // PARAMETER // parameter of the curve of coleoptile elongation // degree.days -1 // PARPLT // 1
  real      :: P_celong  !< // PARAMETER // parameter of the subsoil plantlet elongation curve // SD // PARPLT // 1
  real      :: P_cgrain  !< // PARAMETER // slope of the relationship between grain number and growth rate  // grains gMS -1 jour // PARPLT // 1
  real      :: P_cgrainv0  !< // PARAMETER // number of grains produced when growth rate is zero // grains m-2 // PARPLT // 1
  real      :: P_coefmshaut  !< // PARAMETER // ratio biomass/ useful height cut of crops  // t ha-1 m-1 // PARPLT // 1
  real      :: P_contrdamax  !< // PARAMETER // maximal root growth reduction due to soil strenghtness (high bulk density) // SD // PARPLT // 1
  real      :: P_debsenrac  !< // PARAMETER // Sum of degrees.days defining the beginning of root senescence (life-time of a root) // degree.days // PARPLT // 1
  real      :: P_deshydbase  !< // PARAMETER // phenological rate of evolution of fruit water content (>0 or <0) // g water.g MF-1.degree C-1 // PARPLT // 1
  real      :: P_dlaimax  !< // PARAMETER // Maximum rate of the setting up of LAI // m2 leaf plant-1 degree d-1 // PARPLT // 1
  real      :: P_draclong  !< // PARAMETER // Maximum rate of root length production // cm root plant-1 degree.days-1 // PARPLT // 1
  real      :: P_efcroijuv  !< // PARAMETER // Maximum radiation use efficiency during the juvenile phase(LEV-AMF) // g MJ-1 // PARPLT // 1
  real      :: P_efcroirepro  !< // PARAMETER // Maximum radiation use efficiency during the grain filling phase (DRP-MAT) // g MJ-1 // PARPLT // 1
  real      :: P_efcroiveg  !< // PARAMETER // Maximum radiation use efficiency during the vegetative stage (AMF-DRP) // g MJ-1 // PARPLT // 1
  real      :: P_elmax  !< // PARAMETER // Maximum elongation of the coleoptile in darkness condition // cm // PARPLT // 1
  real      :: P_extin  !< // PARAMETER // extinction coefficient of photosynthetic active radiation canopy // SD // PARPLT // 1
  real      :: P_fixmax  !< // PARAMETER // maximal symbiotic fixation // kg ha-1 j-1 // PARPLT // 1      // OUTPUT // Maximal symbiotic uptake // kg ha-1 j-1
  real      :: P_h2ofeuiljaune  !< // PARAMETER // water content of yellow leaves // g water g-1 MF // PARPLT // 1
  real      :: P_h2ofeuilverte  !< // PARAMETER // water content of green leaves // g water g-1 MF // PARPLT // 1
  real      :: P_h2ofrvert  !< // PARAMETER // water content of fruits before the beginning of hydrous evolution (DEBDESHYD) // g water g-1 MF // PARPLT // 1
  real      :: P_h2oreserve  !< // PARAMETER // reserve water content // g eau g-1 MF // PARPLT // 1
  real      :: P_h2otigestruc  !< // PARAMETER // structural stem part water content // g eau g-1 MF // PARPLT // 1
  real      :: P_inflomax  !< // PARAMETER // maximal number of inflorescences per plant // nb pl-1 // PARPLT // 1
  real      :: P_Kmabs1  !< // PARAMETER // Constant of nitrogen uptake by roots for the high affinity system // µmole. cm root-1 // PARPLT // 1
  real      :: P_Kmabs2  !< // PARAMETER // Constant of nitrogen uptake by roots for the low affinity system // µmole. cm root-1 // PARPLT // 1
  real      :: P_kmax  !< // PARAMETER // Maximum crop coefficient for water requirements (= ETM/ETP) // SD // PARPLT // 1
  real      :: P_kstemflow  !< // PARAMETER // Extinction Coefficient connecting leaf area index to stemflow // * // PARPLT // 1
  real      :: P_longsperac  !< // PARAMETER // specific root length // cm g-1 // PARPLT // 1
  real      :: P_lvfront  !< // PARAMETER // Root density at the root front // cm root.cm-3 soil // PARPLT // 1
  real      :: P_mouillabil  !< // PARAMETER // maximum wettability of leaves // mm LAI-1 // PARPLT // 1
  real      :: P_nbinflo  !< // PARAMETER // imposed number of inflorescences  // nb pl-1 // PARPLT // 1      // OUTPUT // Number of inflorescences // SD
  real      :: P_pentinflores  !< // PARAMETER // parameter of the calculation of the inflorescences number  // SD // PARPLT // 1
  real      :: P_phosat  !< // PARAMETER // saturating photoperiod // hours // PARPLT // 1
  real      :: P_psisto  !< // PARAMETER // absolute value of the potential of stomatal closing // bars // PARPLT // 1
  real      :: P_psiturg  !< // PARAMETER // absolute value of the potential of the beginning of decrease of the cellular extension // bars // PARPLT // 1
  real      :: P_rsmin  !< // PARAMETER // Minimal stomatal resistance of leaves // s m-1 // PARPLT // 1
  real      :: P_sensrsec  !< // PARAMETER // root sensitivity to drought (1=insensitive) // SD // PARPLT // 1
  real      :: P_spfrmax  !< // PARAMETER // maximal sources/sinks value allowing the trophic stress calculation for fruit onset // SD // PARPLT // 1
  real      :: P_spfrmin  !< // PARAMETER // minimal sources/sinks value allowing the trophic stress calculation for fruit onset // SD // PARPLT // 1
  real      :: P_splaimax  !< // PARAMETER // maximal sources/sinks value allowing the trophic stress calculation for leaf growing // SD // PARPLT // 1
  real      :: P_splaimin  !< // PARAMETER // Minimal value of ratio sources/sinks for the leaf growth  // between 0 and 1 // PARPLT // 1
  real      :: P_stemflowmax  !< // PARAMETER // Maximal fraction of rainfall which flows out along the stems  // between 0 and 1 // PARPLT // 1
  real      :: P_stpltger  !< // PARAMETER // Sum of development allowing germination // degree.days // PARPLT // 1
  real      :: P_tcmin  !< // PARAMETER // Minimum temperature of growth // degree C // PARPLT // 1
  real      :: P_tcmax  !< // PARAMETER // Maximum temperature of growth // degree C // PARPLT // 1
  real      :: P_tdebgel  !< // PARAMETER // temperature of frost beginning // degree C // PARPLT // 1
  real      :: P_tdmin  !< // PARAMETER // Minimum threshold temperature for development // degree C // PARPLT // 1
  real      :: P_tdmax  !< // PARAMETER // Maximum threshold temperature for development // degree C // PARPLT // 1
  real      :: P_tempdeshyd  !< // PARAMETER // increase in the fruit dehydration due to the increase of crop temperature (Tcult-Tair) // % water degree C-1 // PARPLT // 1
  real      :: P_tgellev10  !< // PARAMETER // temperature corresponding to 10% of frost damage on the plantlet  // degree C // PARPLT // 1
  real      :: P_tgellev90  !< // PARAMETER // temperature corresponding to 90% of frost damage on the plantlet  // degree C // PARPLT // 1
  real      :: P_tgmin  !< // PARAMETER // Minimum threshold temperature used in emergence stage // degree C // PARPLT // 1
  real      :: P_tletale  !< // PARAMETER // lethal temperature for the plant // degree C // PARPLT // 1
  real      :: P_tmaxremp  !< // PARAMETER // maximal temperature for grain filling // degree C // PARPLT // 1
  real      :: P_tminremp  !< // PARAMETER // Minimal temperature for grain filling // degree C // PARPLT // 1
  real      :: P_vitircarb  !< // PARAMETER // Rate of increase of the carbon harvest index // g grain g plant -1 day-1 // PARPLT // 1
  real      :: P_vitirazo  !< // PARAMETER // Rate of increase of the nitrogen harvest index // g grain g plant -1 day-1 // PARPLT // 1
  real      :: P_vitpropsucre  !< // PARAMETER // increase rate of sugar harvest index  // g sugar g MS-1  j-1 // PARPLT // 1
  real      :: P_vitprophuile  !< // PARAMETER // increase rate of oil harvest index  // g oil g MS-1 j-1 // PARPLT // 1
  real      :: P_Vmax1  !< // PARAMETER // Rate of slow nitrogen absorption (high affinity system) // µmole cm-1 h-1 // PARPLT // 1
  real      :: P_Vmax2  !< // PARAMETER // Rate of rapid nitrogen absorption (high affinity system) // µmole cm-1 h-1 // PARPLT // 1
  real      :: P_zlabour  !< // PARAMETER // Depth of ploughing  // cm // PARPLT // 1
  real      :: P_zprlim  !< // PARAMETER // Maximum depth of the root profile for the reference profile // cm  // PARPLT // 1
  real      :: P_zpente  !< // PARAMETER // Depth where the root density is ½ of the surface root density for the reference profile // cm  // PARPLT // 1

  real      :: P_adilmax  !< // PARAMETER // Parameter of the maximum curve of nitrogen needs [Nplante]=P_adilmax MS^(-P_bdilmax) // N% MS // PARPLT // 1
  real      :: P_bdilmax  !< // PARAMETER // Parameter of the maximum curve of nitrogen needs [Nplante]=P_adilmax MS^(-P_bdilmax) // SD // PARPLT // 1
  real      :: P_masecNmax  !< // PARAMETER // Aerial biomass  on and after there is nitrogen dilution (critical and maximal curves) // t ha-1 // PARPLT // 1
  real      :: P_INNmin  !< // PARAMETER // Minimum value of INN authorised for the crop // SD // PARPLT // 1
  real      :: P_inngrain1  !< // PARAMETER // INN minimal for net absorption of nitrogen during grain filling  // SD // PARPLT // 1
  real      :: P_inngrain2  !< // PARAMETER // INN minimal for null net absorption of nitrogen during grain filling  // SD // PARPLT // 1
  real      :: P_stlevdno  !< // PARAMETER // phasic duration between emergence and the beginning of nodulation  // degrés.jours // PARPLT // 1
  real      :: P_stdnofno  !< // PARAMETER // phasic duration between the beginning and the end of nodulation // degree.days // PARPLT // 1
  real      :: P_stfnofvino  !< // PARAMETER // phasic duration between the end of the nodulation and the end of the nodule life   // degrés.jours // PARPLT // 1
  real      :: P_vitno  !< // PARAMETER // "rate of nodule onset expressed as a proportion of  P_fixmax   per degree day   // nb degrés.jour-1 // PARPLT // 1
  real      :: P_profnod  !< // PARAMETER // nodulation depth // cm // PARPLT // 1
  real      :: P_concNnodseuil  !< // PARAMETER // maximal soil nitrogen threshold for nodule onset  // kg.ha-1.mm-1 // PARPLT // 1
  real      :: P_concNrac0  !< // PARAMETER // soil nitrogen threshold forbiding nodule activity // kg.ha-1.mm-1 // PARPLT // 1
  real      :: P_concNrac100  !< // PARAMETER // soil nitrogen threshold for full nodule activity  // kg.ha-1.mm-1 // PARPLT // 1
!  real      :: P_hunod  !< // PARAMETER // threshold soil water content for nodulation // mm cm-1 sol // PARPLT // 1
  real      :: P_tempnod1  !< // PARAMETER // cardinal temperature for nodule activity   // degree C // PARPLT // 1
  real      :: P_tempnod2  !< // PARAMETER // cardinal temperature for nodule activity   // degree C // PARPLT // 1
  real      :: P_tempnod3  !< // PARAMETER // cardinal temperature for nodule activity   // degree C // PARPLT // 1
  real      :: P_tempnod4  !< // PARAMETER // cardinal temperature for nodule activity   // degree C // PARPLT // 1
  real      :: P_coeflevamf  !< // PARAMETER // multiplier coefficient of the development phase LEVAMF to use crop temperature // SD // PARPLT // 1
  real      :: P_coefamflax  !< // PARAMETER // multiplier coefficient of the development phase AMFLAX to use crop temperature // SD // PARPLT // 1
  real      :: P_coeflaxsen  !< // PARAMETER // multiplier coefficient of the development phase LAXSEN to use crop temperature // SD // PARPLT // 1
  real      :: P_coefsenlan  !< // PARAMETER // cultiplier coefficient of the development phase SENLAN to use crop temperature // SD // PARPLT // 1
  real      :: P_coefdrpmat  !< // PARAMETER // multiplier coefficient of the development phase DRPMAT to use crop temperature // SD // PARPLT // 1
  real      :: P_coefflodrp  !< // PARAMETER // multiplier coefficient of the development phase FLODRP to use crop temperature // SD // PARPLT // 1
  real      :: P_coeflevdrp  !< // PARAMETER // multiplier coefficient of the development phase LEVDRP to use crop temperature // SD // PARPLT // 1
  real      :: P_ratiodurvieI  !< // PARAMETER // life span of early leaves expressed as a proportion of the life span of the last leaves emitted P_DURVIEF // SD // PARPLT // 1
  real      :: P_ratiosen  !< // PARAMETER // fraction of senescent biomass (by ratio at the total biomass) // between 0 and 1 // PARPLT // 1
  real      :: P_ampfroid  !< // PARAMETER // semi thermal amplitude thermique for vernalising effect // degree C // PARPLT // 1
  real      :: P_laicomp  !< // PARAMETER // LAI from which starts competition inbetween plants // m2 m-2 // PARPLT // 1
  real      :: P_phobase  !< // PARAMETER // Base photoperiod  // hours // PARPLT // 1
  real      :: P_stressdev  !< // PARAMETER // maximum phasic delay allowed  due to stresses  // SD // PARPLT // 1
  real      :: P_jvcmini  !< // PARAMETER // Minimum number of vernalising days  // day // PARPLT // 1
  real      :: P_julvernal  !< // PARAMETER // julian day (between 1 and 365) accounting for the beginning of vernalisation for perennial crops // julian day // PARPLT // 1
  real      :: P_tfroid  !< // PARAMETER // optimal temperature for vernalisation // degree C // PARPLT // 1
  real      :: P_q10  !< // PARAMETER // P_Q10 used for the dormancy break calculation  // SD // PARPLT // 1
  real      :: P_stdordebour  !< // PARAMETER // phasic duration between the dormancy break and the bud break  // degree.days // PARPLT // 1
  real      :: P_phyllotherme  !< // PARAMETER // thermal duration between the apparition of two successive leaves on the main stem // degree C day // PARPLT // 1
! ** NB et ML - le 23/02/04 - pour demarrage plantule, ajout de P_masecplantule et P_zracplantule
  real      :: P_laiplantule  !< // PARAMETER // Plantlet Leaf index at the plantation // m2 leaf  m-2 soil // PARPLT // 1
  real      :: P_masecplantule  !< // PARAMETER // initial shoot biomass of plantlet // t ha-1 // PARPLT // 1
  real      :: P_zracplantule  !< // PARAMETER // depth of the initial root front of the plantlet  // cm // PARPLT // 1
  real      :: P_hautbase  !< // PARAMETER // Base height of crop // m // PARPLT // 1
  real      :: P_hautmax  !< // PARAMETER // Maximum height of crop // m // PARPLT // 1
  real      :: P_vlaimax  !< // PARAMETER // ULAI at inflection point of the function DELTAI=f(ULAI) // SD // PARPLT // 1
  real      :: P_pentlaimax  !< // PARAMETER // parameter of the logistic curve of LAI growth  // SD // PARPLT // 1
  real      :: P_udlaimax  !< // PARAMETER // ulai from which the rate of leaf growth decreases  // SD // PARPLT // 1
  real      :: P_abscission  !< // PARAMETER // sensescent leaf proportion falling on the soil // SD // PARPLT // 1
  real      :: P_parazofmorte  !< // PARAMETER // parameter of proportionality between the C/N of died leaves and the INN // SD // PARPLT // 1
  real      :: P_innturgmin  !< // PARAMETER // parameter of the nitrogen stress function active on leaf expansion (INNLAI), that is a bilinear function passing by the point of coordinate (P_innmin, P_innturgmin) // SD // PARPLT // 1
  real      :: P_dlaimin  !< // PARAMETER // accelerating parameter for the lai growth rate // SD // PARAMV6/PLT // 1

  real      :: P_tustressmin  !< // PARAMETER //  threshold stress (min(turfac,inns)) under which there is an effect on the LAI (supplementary senescence by ratio at the natural senescence) // SD // PARPLT // 1
  real      :: P_durviesupmax  !< // PARAMETER // proportion of additional lifespan due to an overfertilization // SD // PARPLT // 1
  real      :: P_innsen  !< // PARAMETER // parameter of the nitrogen stress function active on senescence (innnsenes), bilinear function of the INN using the point (P_innmin, P_innsen) // SD // PARPLT // 1
  real      :: P_rapsenturg  !< // PARAMETER // threshold soil water content active to simulate water sensecence stress as a proportion of the turgor stress // SD // PARPLT // 1
  real      :: P_dlaimaxbrut  !< // PARAMETER // Maximum rate of the setting up of LAI // m2 leaf plant-1 degree d-1 // PARPLT // 1
  real      :: P_tauxrecouvmax  !< // PARAMETER // maximal soil cover rate // m2 plante m-2 sol // PARPLT // 1
  real      :: P_tauxrecouvkmax  !< // PARAMETER // soil cover rate corresponding to the maximal crop coefficient for water requirement  // m2 plante m-2 sol // PARPLT // 1
  real      :: P_pentrecouv  !< // PARAMETER // parameter of the logistic curve of the soil cover rate increase // * // PARPLT // 1
  real      :: P_infrecouv  !< // PARAMETER // ulai at the stage AMF (inflexion point of the soil cover rate increase) // SD // PARPLT // 1
  real      :: P_rapforme  !< // PARAMETER // Ratio thickness/width of the crop form (negative when the base of the form < top) // SD // PARPLT // 1
  real      :: P_ktrou  !< // PARAMETER // Extinction Coefficient of PAR through the crop  (radiation transfer) // * // PARPLT // 1
  real      :: P_adfol  !< // PARAMETER // parameter determining the leaf density evolution within the chosen shape // m-1 // PARPLT // 1
  real      :: P_dfolbas  !< // PARAMETER // minimal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1
  real      :: P_dfolhaut  !< // PARAMETER // maximal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1
  real      :: P_remobres  !< // PARAMETER // proportion of daily remobilisable carbon reserve // SD // PARPLT // 1
  real      :: P_temin  !< // PARAMETER // Minimum threshold temperature for development // degree C // PARPLT // 1
  real      :: P_teopt  !< // PARAMETER // Optimal temperature for the biomass growth // degree C // PARPLT // 1
  real      :: P_temax  !< // PARAMETER // Maximal threshold temperature for the biomass growth  // degree C // PARPLT // 1
  real      :: P_teoptbis  !< // PARAMETER // optimal temperature for the biomass growth (if there is a plateau between P_teopt and P_teoptbis) // degree C // PARPLT // 1
  real      :: P_slamin  !< // PARAMETER // minimal SLA of green leaves // cm2 g-1 // PARPLT // 1
  real      :: P_slamax  !< // PARAMETER // maximal SLA of green leaves // cm2 g-1 // PARPLT // 1
  real      :: P_tigefeuil  !< // PARAMETER // stem (structural part)/leaf proportion // SD // PARPLT // 1
  real      :: P_envfruit  !< // PARAMETER // proportion envelop/P_pgrainmaxi in weight  // SD // PARPLT // 1
  real      :: P_sea  !< // PARAMETER // specifique surface of fruit envelops // cm2 g-1 // PARPLT // 1
  real      :: P_nbgrmin  !< // PARAMETER // Minimum number of grain // grains m-2  // PARPLT // 1
  real      :: P_irmax  !< // PARAMETER // Maximum harvest index // SD // PARPLT // 1
  real      :: P_vitircarbT  !< // PARAMETER // Heat rate of increase of the carbon harvest index  // g grain g plant-1 degree.day-1 // PARPLT // 1
  real      :: P_afpf  !< // PARAMETER // parameter of the  function logistic defining sink strength of fruits (indeterminate growth) : relative fruit age at which growth is maximal // SD // PARPLT // 1
  real      :: P_bfpf  !< // PARAMETER // parameter of the logistic curve defining sink strength of fruits (indeterminate growth) :  rate of maximum growth proportionately to maximum weight of fruits // * // PARPLT // 1
  real      :: P_stdrpnou  !< // PARAMETER // Sum of development units between the stages DRP and NOU (end of  setting) // degree.days // PARPLT // 1
  real      :: P_sensanox  !< // PARAMETER // anoxia sensitivity (0=insensitive) // SD // PARPLT // 1
  real      :: P_allocfrmax  !< // PARAMETER // maximal daily allocation towards fruits // SD // PARPLT // 1
  real      :: P_tgeljuv10  !< // PARAMETER // temperature corresponding to 10 % of frost damage on the LAI (juvenile stage) // degree C // PARPLT // 1
  real      :: P_tgeljuv90  !< // PARAMETER // temperature corresponding to 90 % of frost damage on the LAI (juvenile stage) // degree C // PARPLT // 1
  real      :: P_tgelveg10  !< // PARAMETER // temperature corresponding to 10 % of frost damage on the LAI (adult stage) // degree C // PARPLT // 1
  real      :: P_tgelveg90  !< // PARAMETER // temperature corresponding to 90 % of frost damage on the LAI (adult stage) // degree C // PARPLT // 1
  real      :: P_tgelflo10  !< // PARAMETER // temperature corresponding to 10 % of frost damages on the flowers or the fruits // degree C // PARPLT // 1
  real      :: P_tgelflo90  !< // PARAMETER // temperature corresponding to 90 % of frost damages on the flowers or the fruits // degree C // PARPLT // 1

  integer      :: P_codelegume  !< // PARAMETER // 1 when the plant  id a legume crop, or 2 // code 1/2 // PARPLT // 0
  integer      :: P_codcalinflo  !< // PARAMETER // option of the way of calculation of the inflorescences number  // code 1/2 // PARPLT // 0
  integer      :: P_codgeljuv  !< // PARAMETER // activation of LAI frost at the juvenile stadge // code 1/2 // PARPLT // 0
  integer      :: P_codebeso  !< // PARAMETER // option computing of water needs by the k.ETP (1) approach  or resistive (2) approach // code 1/2 // PARPLT // 0
  integer      :: P_codeintercept  !< // PARAMETER // option of simulation rainfall interception by leafs: yes (1) or no (2) // code 1/2 // PARPLT // 0
  integer      :: P_codeindetermin  !< // PARAMETER // option of  simulation of the leaf growth and fruit growth : indeterminate (2) or determinate (1) // code 1/2 // PARPLT // 0
  integer      :: P_codetremp  !< // PARAMETER // option of heat effect on grain filling: yes (2), no (1) // code 1/2 // PARPLT // 0
  integer      :: P_codetemprac  !< // PARAMETER // option calculation mode of heat time for the root: with crop temperature (1)  or with soil temperature (2) // code 1/2 // PARPLT // 0
  integer      :: P_coderacine  !< // PARAMETER // Choice of estimation module of growth root in volume: standard profile (1) or by actual density (2) // code 1/2 // PARPLT // 0
  integer      :: P_codgellev  !< // PARAMETER // activation of plantlet frost // code 1/2 // PARPLT // 0
  integer      :: P_codazofruit  !< // PARAMETER // option of activation of the direct effect of the nitrogen plant status upon the fruit/grain number // code 1/2 // PARPLT // 0
  integer      :: P_codemonocot  !< // PARAMETER // option plant monocot(1) or dicot(2) // code 1/2 // PARPLT // 0
  integer      :: P_codetemp  !< // PARAMETER // option calculation mode of heat time for the plant : with air temperature (1)  or crop temperature (2) // code 1/2 // PARPLT // 0
  integer      :: P_codegdh  !< // PARAMETER // hourly (1) or daily (2) calculation of development unit // code 1/2 // PARPLT // 0
  integer      :: P_codephot  !< // PARAMETER // option of plant photoperiodism: yes (1), no (2) // code1/2 // PARPLT // 0
  integer      :: P_coderetflo  !< // PARAMETER // option slowness action of water stress before the stage DRP: yes  (1), no (2) // code 1/2 // PARPLT // 0
  integer      :: P_codebfroid  !< // PARAMETER // option of calculation of chilling requirements // code 1/2 // PARPLT // 0
  integer      :: P_codedormance  !< // PARAMETER // option of calculation of dormancy and chilling requirement // code 1/2 // PARPLT // 0
  integer      :: P_codeperenne  !< // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0
  integer      :: P_codegermin  !< // PARAMETER // option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2) // code 1/2 // PARPLT // 0
  integer      :: P_codehypo  !< // PARAMETER // option of simulation of a  phase of hypocotyl growth (1) or planting of plantlets (2) // code 1/2 // PARPLT // 0
  integer      :: P_codeir  !< // PARAMETER // option of computing the ratio grain weight/total biomass: proportional to time(1), proportional to sum temperatures (2) // code 1/2 // PARPLT // 0
  integer      :: P_codelaitr  !< // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0
  integer      :: P_codlainet  !< // PARAMETER // option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI)// code 1/2 // PARPLT // 0
  integer      :: P_codetransrad  !< // PARAMETER // simulation option of radiation 'interception: law Beer (1), radiation transfers (2) // code 1/2 // PARPLT // 0
  integer      :: codetransradb
  integer      :: P_codgelveg  !< // PARAMETER // activation of LAI frost at adult stage // code 1/2 // PARPLT // 0
  integer      :: P_codgelflo  !< // PARAMETER // activation of frost at anthesis // code 1/2 // PARPLT // 0





  integer      :: P_nbjgrain  !< // PARAMETER // Period to compute NBGRAIN // days // PARPLT // 1
  integer      :: P_nbfgellev  !< // PARAMETER // leaf number at the end of the juvenile phase (frost sensitivity)  // nb pl-1 // PARPLT // 1
  integer      :: P_nboite  !< // PARAMETER // "Number of  box  or  age class  of fruits for the fruit growth for the indeterminate crops " // SD // PARPLT // 1
  integer      :: P_idebdorm  !< // PARAMETER // day of the dormancy entrance // julian day // PARPLT // 1
  integer      :: P_ifindorm  !< // PARAMETER // dormancy break day // julian day // PARPLT // 1
  integer      :: P_nlevlim1  !< // PARAMETER // number of days after germination decreasing the emerged plants if emergence has not occur // days // PARPLT // 1
  integer      :: P_nlevlim2  !< // PARAMETER // number of days after germination after which the emerged plants are null // days // PARPLT // 1
  integer      :: P_nbfeuilplant  !< // PARAMETER // leaf number per plant when planting // nb pl-1 // PARPLT // 1
  integer      :: P_forme  !< // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!! 1=snu    7=flo
!!! 2=plt    8=drp
!!! 3=dor    9=des
!!! 4=lev    10=mat
!!! 5=amf    11=rec
!!! 6=lax    12=sen
!!!  character(len=3) ::  P_stoprac  !< // PARAMETER // stage when root growth stops (LAX or SEN) // * // PARPLT // 0
  integer ::  P_stoprac  !< // PARAMETER // stage when root growth stops (LAX or SEN) // * // PARPLT // 0

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!! 1=snu     11=poi      21=tom
!!! 2=fou     12=pdt      22=vig
!!! 3=ban     13=col      23=pom
!!! 4=esc     14=rgi      24=men
!!! 5=mai     15=sor      25=qui
!!! 6=ble     16=soj
!!! 7=fet     17=fra
!!! 8=lin     18=bet
!!! 9=sal     19=can
!!! 10=mou    20=tou
!!!  character(len=3) :: P_codeplante  !< // PARAMETER // Name code of the plant in 3 letters // * // PARPLT // 0
  integer :: P_codeplante  !< // PARAMETER // Name code of the plant in 3 letters // * // PARPLT // 0


! parametres varietaux
  integer   :: nbVariete
!  integer   :: nb_variete_max
!  character(len=15) :: P_codevar(30)     !< // PARAMETER // variety name // SD // PARPLT // 0
!  real      :: P_adens(30)     !< // PARAMETER // Interplant competition parameter // SD // PARPLT // 1
!  real      :: P_afruitpot(30)     !< // PARAMETER // maximal number of set fruits per degree.day (indeterminate growth) // nbfruits degree.day-1 // PARPLT // 1
!  real      :: P_croirac(30)     !< // PARAMETER // Growth rate of the root front  // cm degree.days-1 // PARPLT // 1
!  real      :: P_dureefruit(30)     !< // PARAMETER // total growth period of a fruit at the setting stage to the physiological maturity // degree.days // PARPLT // 1
!  real      :: P_durvieF(30)     !< // PARAMETER // maximal  lifespan of an adult leaf expressed in summation of P_Q10=2 (2**(T-Tbase)) // P_Q10 // PARPLT // 1
!  real      :: P_jvc(30)     !< // PARAMETER // Number of vernalizing days // day // PARPLT // 1
!  real      :: P_nbgrmax(30)     !< // PARAMETER // Maximum number of grain // grains m-2 // PARPLT // 1
!  real      :: P_pgrainmaxi(30)     !< // PARAMETER // Maximum weight of one grain (at 0% water content) // g // PARPLT // 1
!  real      :: P_stamflax(30)     !< // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1
!  real      :: P_stlevamf(30)     !< // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1
!  real      :: P_stlevdrp(30)     !< // PARAMETER // Sum of development units between the stages LEV and DRP // degree.days // PARPLT // 1
!  real      :: P_stflodrp(30)     !< // PARAMETER // phasic duration between FLO and DRP (only for indication) // degrés.jours // PARPLT // 1
!  real      :: P_stlaxsen(30)     !< // PARAMETER // Sum of development units between the stages LAX and SEN // degree.days // PARPLT // 1
!  real      :: P_stsenlan(30)     !< // PARAMETER // Sum of development units between the stages SEN et LAN // degree.days // PARPLT // 1
!  real      :: P_stdrpmat(30)     !< // PARAMETER // Sum of development units between the stages DRP and MAT // degree.days // PARPLT // 1
!  real      :: P_sensiphot(30)     !< // PARAMETER //  photoperiod sensitivity (1=insensitive) // SD // PARPLT // 1
!  real      :: P_stdrpdes(30)     !< // PARAMETER // phasic duration between the DRP stage and the beginning of the water fruit dynamics  // degree.days // PARPLT // 1

! DR 17/09/2012 je mets une dimension de tableau facilement modifiable
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=15),dimension(nb_variete_max) :: P_codevar    !< // PARAMETER // variety name // SD // PARPLT // 0
  integer   :: P_codevar(30)      !< // PARAMETER // variety name // SD // PARPLT // 0
  real      :: P_adens(30)        !< // PARAMETER // Interplant competition parameter // SD // PARPLT // 1
  real      :: P_afruitpot(30)    !< // PARAMETER // maximal number of set fruits per degree.day (indeterminate growth) // nbfruits degree.day-1 // PARPLT // 1
  real      :: P_croirac(30)      !< // PARAMETER // Growth rate of the root front  // cm degree.days-1 // PARPLT // 1
  real      :: P_dureefruit(30)   !< // PARAMETER // total growth period of a fruit at the setting stage to the physiological maturity // degree.days // PARPLT // 1
  real      :: P_durvieF(30)      !< // PARAMETER // maximal  lifespan of an adult leaf expressed in summation of P_Q10=2 (2**(T-Tbase)) // P_Q10 // PARPLT // 1
  real      :: P_jvc(30)          !< // PARAMETER // Number of vernalizing days // day // PARPLT // 1
  real      :: P_nbgrmax(30)      !< // PARAMETER // Maximum number of grain // grains m-2 // PARPLT // 1
  real      :: P_pgrainmaxi(30)   !< // PARAMETER // Maximum weight of one grain (at 0% water content) // g // PARPLT // 1
  real      :: P_stamflax(30)     !< // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1
  real      :: P_stlevamf(30)     !< // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1
  real      :: P_stlevdrp(30)     !< // PARAMETER // Sum of development units between the stages LEV and DRP // degree.days // PARPLT // 1
  real      :: P_stflodrp(30)     !< // PARAMETER // phasic duration between FLO and DRP (only for indication) // degrés.jours // PARPLT // 1
  real      :: P_stlaxsen(30)     !< // PARAMETER // Sum of development units between the stages LAX and SEN // degree.days // PARPLT // 1
  real      :: P_stsenlan(30)     !< // PARAMETER // Sum of development units between the stages SEN et LAN // degree.days // PARPLT // 1
  real      :: P_stdrpmat(30)     !< // PARAMETER // Sum of development units between the stages DRP and MAT // degree.days // PARPLT // 1
  real      :: P_sensiphot(30)    !< // PARAMETER //  photoperiod sensitivity (1=insensitive) // SD // PARPLT // 1
  real      :: P_stdrpdes(30)     !< // PARAMETER // phasic duration between the DRP stage and the beginning of the water fruit dynamics  // degree.days // PARPLT // 1


! DR 29/10/07 tranfert de parametres de paramv6 à plt
  integer      :: P_codazorac  !< // PARAMETER // activation of the nitrogen influence on root partitionning within the soil profile  // code 1/2 // PARPLT // 0
  integer      :: P_codtrophrac  !< // PARAMETER // trophic effect on root partitioning within the soil // code 1/2/3 // PARPLT // 0
  real      :: P_minefnra  !< // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning // SD // PARPLT // 1
  real      :: P_maxazorac  !< // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning  // kg N ha-1 mm-1 // PARPLT // 1
  real      :: P_minazorac  !< // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning // kg N ha-1 mm-1 // PARPLT // 1


  real      :: P_repracpermax  !< // PARAMETER // maximum of root biomass respective to the total biomass (permanent trophic link) // SD // PARPLT // 1
  real      :: P_repracpermin  !< // PARAMETER // minimum of root biomass respective to the total biomass (permanent trophic link) // SD // PARPLT // 1
  real      :: P_krepracperm  !< // PARAMETER // parameter of biomass root partitioning : evolution of the ratio root/total (permanent trophic link) // SD // PARPLT // 1
  real      :: P_repracseumax  !< // PARAMETER // maximum of root biomass respective to the total biomass (trophic link by thresholds) // SD // PARPLT // 1
  real      :: P_repracseumin  !< // PARAMETER // minimum of root biomass respective to the total biomass (trophic link by thresholds) // SD // PARPLT // 1
  real      :: P_krepracseu  !< // PARAMETER // parameter of biomass root partitioning : evolution of the ratio root/total (trophic link by thresholds) // SD // PARPLT // 1


! ** pour ESA - fixation symbiotique
  integer      :: P_codefixpot  !< // PARAMETER // option of calculation of the maximal symbiotic fixation // code 1/2/3 // PARPLT // 0
  real      :: P_fixmaxveg  !< // PARAMETER // parameter to calculate symbiotic fixation as a function of the plant growth   // kg N  (t MS)-1 // PARPLT // 1
  real      :: P_fixmaxgr  !< // PARAMETER // parameter to calculate symbiotic fixation as a function of the plant growth   // kg N  (t MS)-1 // PARPLT // 1

! ** Domi - le 20/09/2004 - calcul du debourrement pour perenne
  real      :: P_tdmindeb  !< // PARAMETER // minimal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
  real      :: P_tdmaxdeb  !< // PARAMETER // maximal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
  integer      :: P_codegdhdeb  !< // PARAMETER // option of calculation of the bud break date in hourly or daily growing degrees  // code 1/2 // PARPLT // 0

! ** NB le 11/02/05 nouveautés courbes de dilution N
! révisé le 11/09/05
  integer  :: P_codeplisoleN  !< // PARAMETER // code for N requirement calculations at the beginning of the cycle: dense plant population (1), isolated plants (2, new formalisation) // code 1/2 // PARPLT // 0
  real     :: P_Nmeta  !< // PARAMETER // rate of metabolic nitrogen in the plantlet // % // PARPLT // 1
  real     :: P_masecmeta  !< // PARAMETER // biomass of the plantlet supposed to be composed of metabolic nitrogen // t ha-1 // PARPLT // 1

! DR 21/04/2011 P_Nres est renommé P_Nreserve car on garde Nres pour la quantité d'azote des residus au meme titre que Cres
  real      :: P_Nreserve  !< // PARAMETER // maximal amount of nitrogen in the plant reserves (distance between the maximal dilution curve and the critical dilution curve) (as a percentage of the aboveground dry weight) // % // PARPLT // 1

! ** NB le 07/04/05 nouveautés INNI
  integer      :: P_codeINN  !< // PARAMETER // option to compute INN: cumulated (1), instantaneous (2)  // code 1/2 // PARPLT // 0
  real      :: P_INNimin  !< // PARAMETER // INNI (instantaneous INN) corresponding to P_INNmin // SD // PARPLT // 1


! NB le 11/04/05 nouveautés levées
  real      :: P_potgermi  !< // PARAMETER // humidity threshold from which seed humectation occurs, expressed in soil water potential  // Mpa // PARPLT // 1
  integer      :: P_nbjgerlim  !< // PARAMETER // Threshold number of day after grain imbibition without germination lack // days // PARPLT // 1

! NB le 22/02/07 pour germination
  real      :: P_propjgermin  !< // PARAMETER // minimal proportion of the duration P_nbjgerlim when the temperature is higher than the temperature threshold P_Tdmax  // % // PARPLT // 1


! NB le 12/04/05 nouveautés fruit
! DR 29/01/08 parametre supprimé
!    integer      :: codefruitcroi
  real      :: P_cfpf  !< // PARAMETER // parameter of the first potential growth phase of fruit, corresponding to an exponential type function describing the cell division phase. // SD // PARPLT // 1
  real      :: P_dfpf  !< // PARAMETER // parameter of the first potential growth phase of fruit, corresponding to an exponential type function describing the cell division phase. // SD // PARPLT // 1

  real      :: P_tcxstop  !< // PARAMETER // threshold temperature beyond which the foliar growth stops // degree C // PARPLT // 1

! NB le 13/05/05 battance
  real      :: P_vigueurbat  !< // PARAMETER // indicator of plant vigor allowing to emerge through the crust  // between 0 and 1 // PARPLT // 1

! DR le 03/01/06 parametres inaki stressphot
  real      :: P_phobasesen  !< // PARAMETER // photoperiod under which the photoperiodic stress is activated on the leaf lifespan // heures // PARPLT // 1
  real      :: P_dltamsmaxsen  !< // PARAMETER // threshold value of deltams from which there is no more photoperiodic effect on senescence // t ha-1j-1 // PARPLT // 1
  integer      ::  P_codestrphot  !< // PARAMETER // activation of the photoperiodic stress on lifespan : yes (1), no (2) // code 1/2 // PARPLT // 0
! 30/01/2012 ils vont la et non dans param_gen
  real :: P_dltamsminsen  !< // PARAMETER // threshold value of deltams from which the photoperiodic effect on senescence is maximal // t ha-1j-1 // PARPLT // 1
  real :: P_alphaphot  !< // PARAMETER // parameter of photoperiodic effect on leaf lifespan // P_Q10 // PARPLT// 1


! ** PB - 03/03/2005 - on recalcule le matigestruc à la fauche en fontion de la
! *- biomasse résiduelle et du paramètre plante <ratioresfauche>
! DR 29/10/07 voir si  on la garde marie disait de le supprimer
!      real      :: ratioresfauche

  real      :: P_alphaco2  !< // PARAMETER // coefficient allowing the modification of radiation use efficiency in case of  atmospheric CO2 increase // SD // PARPLT // 1



! **************************************
! paramètres spécifiques d'initialisation de l'état de la plante
! pour le début de la simulation

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!! 1=snu    7=flo
!!! 2=plt    8=drp
!!! 3=dor    9=des
!!! 4=lev    10=mat
!!! 5=amf    11=rec
!!! 6=lax    12=sen
!!!  character(len=3) :: P_stade0  !< // PARAMETER // Crop stage used at the beginning of simulation // * // INIT // 0
  integer             :: P_stade0  !< // PARAMETER // Crop stage used at the beginning of simulation // * // INIT // 0

! ** pas de distinction ombre/soleil pour les variables de départ
  real                :: P_lai0  !< // PARAMETER // Initial leaf area index // m2 m-2 // INIT // 1
  real                :: P_masec0  !< // PARAMETER // initial biomass // t ha-1 // INIT // 1
  real                :: P_magrain0  !< // PARAMETER // initial grain dry weight // g m-2 // INIT // 1
  real                :: P_QNplante0  !< // PARAMETER // initial nitrogen amount in the plant // kg ha-1 // INIT // 1
  real, dimension(5)  :: P_densinitial  !< // PARAMETER // Table of initial root density of the 5 horizons of soil for fine earth // cm cm-3 // INIT // 1
  real                :: P_resperenne0  !< // PARAMETER // initial reserve biomass // t ha-1 // INIT // 1
  real                :: P_zrac0  !< // PARAMETER // initial depth of root front  // cm // INIT // 1
! DR 14/04/2016 on ajoute les varaibles pariries
  real                :: mafeuiltombe0(0:2)   !< // OUTPUT // Dry matter of fallen leaves // t.ha-1
  real                :: masecneo0(0:2)  !< // OUTPUT // Newly-formed dry matter  // t.ha-1
  real                :: msneojaune0(0:2)   !< // OUTPUT // Newly-formed senescent dry matter  // t.ha-1
  real                :: dltamsen0(0:2)      !< // OUTPUT // // t.ha-1
  real                :: mafeuiljaune0(0:2)   !< // OUTPUT // // t.ha-1



! **************************************
! *         Variables plante           *
! **************************************


!** declarations  integer
  integer      :: ficrap  
!14/09/2011    rapport special Agmip
  integer      :: ficrap_AgMIP
  integer      :: ficsort  
  integer      :: ficsort2  
  integer      :: ficbil  

  integer      :: group  
  integer      :: parapluie  
  integer      :: codeinstal  

  integer      :: nsencour  
  integer      :: nsencourpre  
  integer      :: nsencourprerac  
  integer      :: numjtrav(10)  
! DR 01/02/2011 je rajoute le njourres
  integer      :: numjres(10)  
  integer      :: nnou  
  integer      :: nbj0remp      !< // OUTPUT // Number of shrivelling days //
  integer      :: nbjgel      !< // OUTPUT // Number of frosting days active on the plant // SD
  integer      :: nfauche(10)  

  integer      :: nlanobs  
  integer      :: nlax  
  integer      :: nrecobs  
  integer      :: nlaxobs  
  integer      :: nlev  
  integer      :: nlevobs  
  integer      :: nmatobs  
  integer      :: nplt  
  integer      :: ndrp  
  integer      :: ndebsenrac  
  integer      :: nbrecolte  
  integer      :: nrecint(20)  
  integer      :: ndebdes  
  integer      :: ntaille  
  integer      :: nger  
  integer      :: igers   !< // OUTPUT // Date of germination // jour julien
  integer      :: inous   !< // OUTPUT // Date of end of setting of harvested organs // jour julien
  integer      :: idebdess   !< // OUTPUT // Date of onset of water dynamics in harvested organs // jour julien
  integer      :: ilans   !< // OUTPUT // Date of LAN stage (leaf index nil) // jour julien
  integer      :: iplts   !< // OUTPUT // Date of sowing or planting // jour julien
  integer      :: compretarddrp  
  integer      :: iamfs   !< // OUTPUT // Date of AMF stage // jour julien
  integer      :: idebdorms   !< // OUTPUT // Date of entry into dormancy // jour julien
  integer      :: idrps   !< // OUTPUT // Date of start of filling of harvested organs // jour julien
  integer      :: ifindorms   !< // OUTPUT // Date of emergence from dormancy // jour julien
  integer      :: iflos   !< // OUTPUT // Date of flowering // jour julien
  integer      :: ilaxs   !< // OUTPUT // Date of LAX stage (leaf index maximum) // jour julien
  integer      :: namf  
  integer      :: imats   !< // OUTPUT // Date of start of physiological maturity // jour julien
  integer      :: irecs   !< // OUTPUT // Date of harvest (first if several) // jour julien
  integer      :: isens   !< // OUTPUT // Date of SEN stage // jour julien
  integer      :: nsen  
  integer      :: nsenobs  

  integer      :: ndebdorm  
  integer      :: nfindorm  
  integer      :: nbfeuille   !< // OUTPUT // Number of leaves on main stem // SD
  integer      :: nflo  
  integer      :: nfloobs  
  integer      :: nstopfeuille  
  integer      :: mortplante  
  integer      :: mortplanteN  
! DR 09/03/2016 on passe le nb d'eclaircissage possible à 10
  integer      :: neclair(10)
  integer      :: nrogne  
  integer      :: neffeuil  
  integer      :: jdepuisrec  
  integer      :: namfobs  
  integer      :: nlan  
  integer      :: nrec  
  integer      :: nrecbutoir  
  integer      :: nmat  
  integer      :: ndrpobs  
  integer      :: ndebdesobs  
  integer      :: ilevs   !< // OUTPUT // Date of emergence // jour julien
  integer      :: numcoupe   !< // OUTPUT // Cut number // SD
  integer      :: nst1coupe  
  integer      :: nst2coupe  
  integer      :: ndebsen  
! ** domi 22/06/04 pour calculer la tmoy entre 2 dates
  integer      :: nbjTmoyIpltJuin  
  integer      :: nbjTmoyIpltSept  

! ** offrnodu
  integer      :: ndno  
  integer      :: nfno  
  integer      :: nfvino  

! domi 16/09/05 pour fixpot fixreel et offrenod indexation sur ao/as
  real      :: fixpotC  
  real      :: fixreelC  
  real      :: offrenodC  
  real      :: fixmaxC  
  real      :: QfixC  
  real      :: fixpotfno  
  real      :: propfixpot  

! ** NB le 14/12/04
  integer      :: nrecalpfmax  

! ** déclaration logique
  logical      :: fauchediff  
  logical      :: sioncoupe  
  logical      :: onarretesomcourdrp  
  logical      :: etatvernal  
  logical      :: gelee  

! ** declarations real

  real      :: LRACH(5)   !< // OUTPUT // Root density in the horizon 1 // cm.cm-3
  real      :: cinterpluie   !< // OUTPUT // Amount of rain intercepted by the leaves // mm
  real      :: totpl  
  real      :: rc   !< // OUTPUT // Resistance of canopy  // s.m-1
  real      :: fco2s !< // OUTPUT // specie-dependant effect on stomate closure  // SD
  real      :: lracz(1000)
  real      :: cumlracz   !< // OUTPUT // Sum of the effective root lengths  // cm root.cm -2 soil
  real      :: dfol   !< // OUTPUT //  "Within the shape  leaf density" // m2 m-3
  real      :: rombre   !< // OUTPUT // Radiation fraction in the shade // 0-1
  real      :: rsoleil   !< // OUTPUT // Radiation fraction in the full sun // 0-1
  real      :: somcourfauche  !< // OUTPUT //actual sum of temperature between 2 cuts // 0-1
  ! DR et Fr 13/02/2015
  real      :: reste_apres_derniere_coupe
  real      :: QNplanteres  
! DR et ML 04/09/2014 on indexe sur AO/AS les varaibles de l'abscission
  real      :: QCplantetombe(0:2)
  real      :: QNplantetombe(0:2)

  real      :: QCrogne ! Bruno: ajout variables cumul des quantites C et N rognees 22/05/2012
  real      :: QNrogne
  real      :: Crac
  real      :: Nrac
  real      :: QCrac
  real      :: QNrac
  real      :: udevlaires(10)  
  real      :: cescoupe  
  real      :: cetcoupe  
  real      :: cepcoupe  
  real      :: cetmcoupe  
  real      :: cprecipcoupe  
  real      :: cep   !< // OUTPUT // Transpiration integrated over the cropping season  // mm
  real      :: cprecip   !< // OUTPUT // Water supply integrated over the cropping season // mm
  real      :: cet   !< // OUTPUT // Evapotranspiration integrated over the cropping season // mm
  real      :: ces   !< // OUTPUT // Evaporation integrated over the cropping season // mm
  real      :: cetm   !< // OUTPUT // Maximum evapotranspiration integrated over the cropping season // mm
  real      :: masectot  
! Ajout Bruno juin 2014
  real      :: rendementsec
!  
  real      :: str1coupe  
  real      :: stu1coupe  
  real      :: str2coupe  
  real      :: stu2coupe  
  real      :: inn1coupe  
  real      :: diftemp1coupe  
  real      :: inn2coupe  
  real      :: diftemp2coupe  
  real      :: QNressuite  
  real      :: QCressuite
  real      :: pfmax  
  real      :: stemflow   !< // OUTPUT // Water running along the stem // mm
  real      :: precrac(1000)
  real      :: lracsenz(1000)
  !!!MODIF HISAFE 2 : reduction dimension temporelle
  !!!real      :: drl(0:731,size_table_soil)            ! temporel pour sénescence (racinaire)
  real      :: drl(0:366,1000)            ! temporel pour sénescence (racinaire)
  real      :: somtemprac  
  !!!MODIF HISAFE 4 : suppression dimension temporelle
  !!!remplacé rl par rljour et rlveille
  !!!real      :: rl(0:731,1000)             ! On peut à priori se passer du tableau temporel, à vérifier
  real      :: poussracmoy   !< // OUTPUT // "Average index of the effect of soil constraints on the rooting profile (option  true density )" // 0-1
  real      :: Emd   !< // OUTPUT // Direct evaporation of water intercepted by leafs  // mm
  real      :: diftemp1  
  real      :: diftemp2  
  real      :: flrac(1000)
  real      :: ftemp   !< // OUTPUT // Temperature-related EPSIBMAX reduction factor // 0-1
  real      :: udevcult   !< // OUTPUT // Effective temperature for the development, computed with TCULT // degree.days
  real      :: udevair   !< // OUTPUT // Effective temperature for the development, computed with TAIR // degree.days
  real      :: ebmax  
  real      :: fpari
  real      :: efdensite_rac   !< // OUTPUT // density factor on root growth // 0-1
  real      :: efdensite     !< // OUTPUT // density factor on leaf area growth  // 0-1

!!!MODIF HISAFE 2 : reduction dimension temporelle
!!!real      :: ulai(0:731)                ! temporel pour sénescence    // OUTPUT // Daily relative development unit for LAI // 0-3
  real      :: ulai(0:366)                ! temporel pour sénescence    // OUTPUT // Daily relative development unit for LAI // 0-3
  real      :: caljvc  
  real      :: somelong  
  real      :: somger  
  real      :: cdemande   !< // OUTPUT // Sum of daily nitrogen need of the plant   // kg.ha-1


! ** rajout domi vers 4.0
  real      :: zrac   !< // OUTPUT // Depth reached by root system // cm
  real      :: znonli  
  real      :: deltaz   !< // OUTPUT // Deepening of the root front  // cm day-1
  real      :: difrac  
  real      :: resrac   !< // OUTPUT // Soil water reserve in the root zone // mm
  real      :: Scroira  


! DR 11/06/2013 ajout des varaibles de stress racinaires
 ! 11/06/2013 la variable efda devient une varaible de sortie journaliere pour Simtraces
  real     :: efda      !> // OUTPUT // efda defines the effect of soil compaction through bulk density // 0-1
  real     :: efnrac_mean    !> // OUTPUT // effect of mineral nitrogen, which contributes to the root distribution in the layers with high mineral nitrogen content. // 0-1
  real     :: humirac_mean !> // OUTPUT // soil dryness // 0-1
  real     :: humirac_z(1000) !> // OUTPUT //profile soil dryness // 0-1
  real     :: efnrac_z(1000) !> // OUTPUT // effect of minral nitrogen on roots // 0-1
  real     :: rlj !> // OUTPUT // roots length growth rate  // m.d-1
  real     :: dltmsrac_plante

! ** variables qui etaientt AO/AS et qu'on a rendu communes
  real      :: cumraint   !< // OUTPUT // Sum of intercepted radiation  // Mj.m-2
  real      :: cumrg   !< // OUTPUT // Sum of global radiation during the stage sowing-harvest   // Mj.m-2

! ** NB le 12/11/98 version 4.0
  real      :: irrigprof(0:1000)
  real      :: stlevdrp0  
  real      :: stsenlan0  
  real      :: stlaxsen0  
  real      :: remobil  
  real      :: somcourdrp  
!!!MODIF HISAFE 2 : reduction dimension temporelle
!!!real      :: dtj(0:731)                 ! temporel pour sénescence (racinaire)    // OUTPUT // Daily efficient temperature for the root growing  // degree C.j-1
  real      :: dtj(0:366)                 ! temporel pour sénescence (racinaire)    // OUTPUT // Daily efficient temperature for the root growing  // degree C.j-1
  real      :: qressuite       !< // OUTPUT // quantity of aerial residues from the previous crop // t.ha-1
  ! DR et EC 25/07/2012 on ajoute Qressuite_tot pour prendre en compte les racines dans le bilan
  real      :: qressuite_tot   !< // OUTPUT // quantity of total residues (aerials + roots) from the previous crop // t.ha-1
  real      :: CsurNressuite_tot   !<  // OUTPUT // Carbon to Nitrogen ratio of total harvest residues (aerials + roots) // t.ha-1

  real :: Nexporte      !< // OUTPUT // total of exported nitrogen // kgN.ha-1
  real :: Nrecycle      !< // OUTPUT // total of recycle nitrogen (unexported nitrogen at harvest + nitrogen from the fallen leaves) // kgN.ha-1
  real :: MSexporte     !< // OUTPUT // total of exported carbon // t.ha-1
  real :: MSrecycle     !< // OUTPUT // total of recycle carbon (unexported nitrogen at harvest + nitrogen from the fallen leaves) // t.ha-1
  real :: p1000grain      !< // OUTPUT // 1000 grain weight // g.m-2
  real :: somudevair      !< // OUTPUT // sum of air temperature from sowing // degree C
  real :: somudevcult      !< // OUTPUT // sum of crop temperature from sowing // degree C
  real :: somupvtsem      !< // OUTPUT // sum of development units from sowing // degree C




  real      :: CsurNressuite  
  real      :: cumlraczmaxi  

! ** domi passe en common pour lecsortie 08/03/99
  real      :: cumdevfr  
  real      :: nbfruit  
  real      :: tetstomate   !< // OUTPUT // Threshold soil water content limiting transpiration and photosynthesis  // % vol
  real      :: teturg   !< // OUTPUT // Threshold soil water content limiting surface area growth of leaves // % vol
  real      :: rltot   !< // OUTPUT // Total length of roots  // cm root.cm -2 soil
  real      :: rfpi   !< // OUTPUT // Slowing effect of the photoperiod on plant development  // 0-1
  real      :: lracsentot   !< // OUTPUT // Total length of senescent roots  // cm root.cm -2 soil
  real      :: largeur   !< // OUTPUT // Width of the plant shape  // m


! ** nadine modifs pour boucler bilan d'azote 8/03/99
! domi 16/09/05 cumlé  car indexe sur ao,as
! DR 12/10/06 lrac est devevu locale dans offreN
!      real      :: lrac

  real      :: utno  
  real      :: pfv  
  real      :: pfj  
  real      :: pftot  
  real      :: pfe  
  real      :: pft  
  real      :: wcf  
  real      :: wct  
  real      :: wce  
  real      :: stpltlev  
  ! DR 18/07/2012 je rajoute la germination
  real      :: stpltger
  real      :: stdrpsen  
  real      :: stmatrec  
  real      :: upvtutil  
  !!!MODIF HISAFE 4 : suppression dimension temporelle
  !!!real      :: upvt(0:731)                ! pas besoin du tableau temporel  	  // OUTPUT // Daily development unit  // degree.days

  real      :: somcour   !< // OUTPUT // Cumulated units of development between two stages // degree.days
  real      :: reajust  
  real      :: upobs(0:366)               ! tableau de forçage

  real      :: stlevamf0  
  real      :: stamflax0  
  real      :: varrapforme  


  real      :: anoxmoy   !< // OUTPUT // Index of mean anoxia on the root depth  // 0-1
  real      :: chargefruit   !< // OUTPUT // Amount of filling fruits per plant // nb fruits.plant-1
  real      :: coeflev  
  real      :: cumflrac  
  real      :: densitelev  
  real      :: durvieI  
  real      :: exobiom   !< // OUTPUT // Index of excess water active on surface growth // 0-1

  real      :: etr_etm1  
  real      :: etm_etr1  
  real      :: etr_etm2  
  real      :: etm_etr2  

  real      :: exofac   !< // OUTPUT // Variable for excess water // 0-1
  real      :: exofac1  
  real      :: exofac2  
  real      :: exolai   !< // OUTPUT // Index for excess water active on growth in biomass // 0-1
  real      :: fco2  !< // OUTPUT // specie-dependant CO2 effect on radiation use efficiency // SD
  real      :: fgelflo   !< // OUTPUT // Frost index on the number of fruits // 0-1
  real      :: fgellev  
  real      :: fstressgel   !< // OUTPUT // Frost index on the LAI // 0-1
  real      :: ftempremp  
  real      :: gel1   !< // OUTPUT // frost damage on foliage before amf // %
  real      :: gel2   !< // OUTPUT // frost damage on foliage after amf // %
  real      :: gel3   !< // OUTPUT // frost damage on foliage or fruits // %
  real      :: izrac   !< // OUTPUT // Index of excess water stress on roots  // 0-1
  real      :: idzrac  
  real      :: nbfrote  
  real      :: rmaxi   !< // OUTPUT // Maximum water reserve utilised // mm
  real      :: somcourutp  
  real      :: somcourno  
  real      :: somfeuille  
  real      :: somtemp   !< // OUTPUT // Sum of temperatures // degree C.j
  real      :: somupvt  

! DR 09/10/09 j'ajoute somupvtI
  real      :: somupvtI  

  real      :: stdrpmatini  
  real      :: stflodrp0  
  real      :: stlevflo  
  real      :: TmoyIpltJuin   !< // OUTPUT // Sum of temperatures from sowing or planting (IPLT stage) until June the 30th // degree C
  real      :: TmoyIpltSept   !< // OUTPUT // Sum of temperatures from sowing or planting (IPLT stage) until September the 30th // degree C
  real      :: zracmax  


  real      :: rfvi   !< // OUTPUT // Slowing effect of the vernalization on plant development // 0-1

  !!!MODIF HISAFE 7 : reduction dimension
  !!!real      :: racnoy(0:1000)                 ! TODO: pourquoi 1000 ? racnoy est plutot un tableau temporel à priori.
  real      :: racnoy(0:366)                 ! TODO: pourquoi 1000 ? racnoy est plutot un tableau temporel à priori.

  !!!MODIF HISAFE 2 : reduction dimension temporelle
  !!!real      :: msrac(0:731)               ! pas besoin du tableau temporel, une simple sauvegarde de n-1 pourrait suffire    // OUTPUT // Estimated dry matter of the roots // t.ha-1
  !!!real      :: tdevelop(0:731)            ! temporel pour sénescence
  !!!real      :: cu(0:731)                  ! pas besoin du tableau temporel
  real      :: msrac(0:366)               ! pas besoin du tableau temporel, une simple sauvegarde de n-1 pourrait suffire    // OUTPUT // Estimated dry matter of the roots // t.ha-1
  real      :: tdevelop(0:366)            ! temporel pour sénescence
  real      :: cu(0:366)                  ! pas besoin du tableau temporel, une simple sauvegarde de n-1 pourrait suffire

  !!!MODIF HISAFE 4 : suppression dimension temporelle
  !!!real      :: utp(0:731)                 ! pas besoin du tableau temporel

  integer   :: nst1  
  integer   :: nst2  

  real      :: inn1  
  real      :: inn2  
  real      :: str1  
  real      :: str2  
  real      :: stu1  
  real      :: stu2  

  real      :: etm_etr1moy  
  real      :: etr_etm1moy  
  real      :: etr_etm2moy  
  real      :: etm_etr2moy  

  real      :: exofac1moy   !< // OUTPUT // "Mean value of the variable for excess water during the vegetative stage (from emergence  LEV  to fruit establishment  DRP )  " // 0-1
  real      :: exofac2moy   !< // OUTPUT // "Mean value of the variable for excess water during the reproductive stage (from fruit establishment  DRP  to maturity  MAT )  " // 0-1
  real      :: inn1moy   !< // OUTPUT // Average of index of nitrogen stress over the vegetative stage // SD
  real      :: inn2moy   !< // OUTPUT // Average index of nitrogen stress over the reproductive stage // SD
  real      :: swfac1moy   !< // OUTPUT // Average of index of stomatic water stress over the vegetative stage // 0-1
  real      :: swfac2moy   !< // OUTPUT // Average of index of stomatic water stress over the reproductive stage // 0-1
  real      :: turfac1moy   !< // OUTPUT // Average of index of turgescence water stress over the vegetative stage // 0-1
  real      :: turfac2moy   !< // OUTPUT // Average of index of turgescence water stress over the reproductive stage // 0-1

! ** DR le 13/01/06 pour maintient des variables plante si P_codemsfinal=oui
  real      :: mafrais_nrec  
  real      :: pdsfruitfrais_nrec  
  real      :: mabois_nrec  
  real      :: H2Orec_nrec  
  real      :: chargefruit_nrec  
  real      :: CNgrain_nrec  
  real      :: CNplante_nrec  

  real      :: QNgrain_nrec  
  real      :: Qngrain_ntailleveille    ! on enregsitre la valeur de QNgrain de la veille du jour de taille  

! ** dr 14022006 passé en common pour etre recupere dans Ngrain
  real      :: nbgraingel  
  real      :: QNplanteCtot  
! EC 06/08/2012 ajout d'une variable quantité de N exportée pour les cultures fauchées
  real      :: QNplantefauche
! dr 170306 rajout variables cumulées
  real      :: ctmoy   !< // OUTPUT // Air temperature integrated over the cropping season // degree C
  real      :: ctcult   !< // OUTPUT // Crop temperature (TCULT) integrated over the cropping season // degree C
! DR 29/12/2014 ajout de cumul de tcult max pour faire moyenne pour Giacomo Agmip Canopy temp
  real      :: ctcultmax   !< // OUTPUT // maximum Crop temperature (TCULT) integrated over the cropping season // degree C

  real      :: cetp   !< // OUTPUT // Potential evapotranspiration (PET) integrated over the cropping season // mm
  real      :: crg   !< // OUTPUT // Global radiation integrated over the cropping season // Mj/m2
  real      :: cum_et0   !< // OUTPUT // maximum evapotranspiration (eop+eos)/mm

! DR 20/07/06 on compte le nb de jours ou l'on a repousse la fauche
  integer   :: nbrepoussefauche  
 !DR 26/06/2015 je garde les valuers de la derniere coupe quand est n'a pas pu se faire pour l'annee d'apres
  real      :: anitcoupe_anterieure
  real      :: hautcoupe_anterieure
  real      :: msresiduel_anterieure
  real      :: lairesiduel_anterieure
  real      :: ulai0
  real      :: durvie0(0:2)
integer     :: codebbch0

!!!MODIF HISAFE type incorrect
!!!  real    :: restit_anterieure
  integer :: restit_anterieure

  real    :: mscoupemini_anterieure

  real      :: tempfauche_realise
! DR 06/09/06
  real      :: tempeff   !< // OUTPUT // Efficient temperature for growth // degree C

! DR 05/07/07 on introduit une variable
!  real      :: tempfauche_ancours(20)
  real      :: tempfauche_ancours(0:20)

! DR et FR 08/06/2016 on conserve les sommes auquels sont effectivement realisees les fauches
  real      :: som_vraie_fauche(20)

! dr 27/08/07 densite devient une variable
  real      :: densite   !< // OUTPUT // Actual sowing density // plants.m-2
  ! dr 12/09/2012 j'ajoute la densite equivalenet comme varaible distincte de densite
  real      :: densiteequiv   !< // OUTPUT // Actual sowing density // plants.m-2

! DR 30/08/07 P_stdrpdes oublié
  real      :: stdrpdes0  
  real      :: str1intercoupe   !< // OUTPUT // stomatal water stress average during the cut (for forage crops in vegetative phase : emergence to maximum LAI )  // 0-1
  real      :: stu1intercoupe   !< // OUTPUT // turgescence water stress average during the cut (for forage crops in vegetative phase : emergence to maximum LAI )  // 0-1
  real      :: inn1intercoupe   !< // OUTPUT // nitrogen stress (inn) average during the cut (cut crop vegetative phase: emergence to maximum LAI)  // 0-1
  real      :: diftemp1intercoupe   !< // OUTPUT // mean difference between crop surface temperature and air temperature during the cut (cut crop vegetative phase: emergence to maximum LAI) // degree C
  real      :: str2intercoupe   !< // OUTPUT // stomatal water stress average during the cut (cut crop reproductive phase)  // 0-1
  real      :: stu2intercoupe   !< // OUTPUT // turgescence water stress average during the cut (for forage crops in reproductive phase: maximum LAI  to maturity)  // 0-1
  real      :: inn2intercoupe   !< // OUTPUT // nitrogen stress (inn) average during the cut (for forage crops in reproductive phase: maximum LAI  to maturity)  // 0-1
  real      :: diftemp2intercoupe   !< // OUTPUT // mean difference between crop surface temperature and air temperature during the cut (cut crop reproductive phase: maximum LAI  to maturity) // degree C
 ! DR 03/09/2012 on l'enleve ne sert visiblement pas
 ! real      :: totircoupe(20)   !< // OUTPUT // Total amount of water inputs for a given cut // mm
 ! real      :: totapNcoupe(20)   !< // OUTPUT // Total amount of fertiliser N added for a given cut // kgN.ha-1
  integer   :: ficsort3  
  integer   :: ficdrat

! DR 31/01/08 CLIMATOR : on garde les dates des stades lax pour la prairie
  integer   :: ilaxs_prairie(10)  

! DR et ML et SYL 15/06/09
! **************************
! introduction de la fin des modifications de Sylvain (nadine et FR)
! dans le cadre du projet PERMED
! SYL 12/09/07
    real    :: mortmasec   !< // OUTPUT // Dead tiller biomass  // t.ha-1
    real    :: densitemax  
    real    :: drlsenmortalle   !< // OUTPUT // Root biomass corresponding to dead tillers // t ha-1.d-1
! DR et ML et SYL 15/06/09 FIN introduction de la fin des modifications de Sylvain

! DR et ML 29/06/09
! **************************
! introduction de la fin des modifications de Sylvain (nadine et FR)
! dans le cadre du projet PERMED
    integer :: imontaisons   !< // OUTPUT // Date of start of stem elongation // julian day
    real    :: mortalle   !< // OUTPUT // number of dead tillers per day // tillers.d-1

  real      :: densiteger  


  real      :: mafruit   !< // OUTPUT // Dry matter of harvested organs // t.ha-1
  real      :: matuber   !< // OUTPUT // Dry matter of harvested organs // t.ha-1
  real      :: matuber_rec ! DR 06/04/2012 on garde le matuber à la recolte
  ! 05/05/2015 DR et FR ajout de la variable recoltée pour les prairies
  real      :: msrec_fou   !< // OUTPUT // Dry matter of harvested organs for forages// t.ha-1


  real      :: dNdWcrit  
  real      :: dNdWmax  

  real      :: flurac   !< // OUTPUT // Nitrogen absorption flow associated with the limiting absorption capacity of the plant // kgN ha-1 j-1
  real      :: flusol   !< // OUTPUT // Nitrogen absorption flux associated with limiting transfer soil  --> root  // kgN ha-1 j-1

! dr - 08/04/2010
! on a ajouté le codebbch pour ne pas avoir à modifier les fichiers dans la nouvelle version.
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=3) :: P_stadebbchplt  !< // PARAMETER // equivalent stage in BBCH-scale (sowing) //  // PARPLT // 0
!!!  character(len=3) :: P_stadebbchger  !< // PARAMETER // equivalent stage in BBCH-scale (germination) //  // PARPLT // 0
!!!  character(len=3) :: P_stadebbchlev  !< // PARAMETER // equivalent stage in BBCH-scale (emergence) //  // PARPLT // 0
!!!  character(len=3) :: P_stadebbchamf  !< // PARAMETER // equivalent stage in BBCH-scale (amf) //  // PARPLT // 0
!!!  character(len=3) :: P_stadebbchlax  !< // PARAMETER // equivalent stage in BBCH-scale (lax) //  // PARPLT // 0
!!!  character(len=3) :: P_stadebbchsen  !< // PARAMETER // equivalent stage in BBCH-scale (senescence) //  // PARPLT // 0
!!!  character(len=3) :: P_stadebbchflo  !< // PARAMETER // equivalent stage in BBCH-scale (flowering) //  // PARPLT // 0
!!!  character(len=3) :: P_stadebbchdrp  !< // PARAMETER // equivalent stage in BBCH-scale (drp) //  // PARPLT // 0
!!!  character(len=3) :: P_stadebbchnou  !< // PARAMETER // equivalent stage in BBCH-scale (fruit set) //  // PARPLT // 0
!!!  character(len=3) :: P_stadebbchdebdes  !< // PARAMETER // equivalent stage in BBCH-scale (debdes) //  // PARPLT // 0
!!!  character(len=3) :: P_stadebbchmat  !< // PARAMETER // equivalent stage in BBCH-scale (maturity) //  // PARPLT // 0
!!!  character(len=3) :: P_stadebbchrec  !< // PARAMETER // equivalent stage in BBCH-scale (harvest) //  // PARPLT // 0
!!!  character(len=3) :: P_stadebbchfindorm  !< // PARAMETER // equivalent stage in BBCH-scale (end of dormancy) //  // PARPLT // 0

  ! DR 19/09/2012 on les met dans la plante
  real :: profextN      !< // OUTPUT // Average depth of Nitrogen absorption // cm
  real :: profexteau    !< // OUTPUT // Average depth of water absorption // cm
  integer :: age_prairie     !< // OUTPUT // forage crop age from sowing // an
  real :: RsurRUrac     !< // OUTPUT // Fraction of available water reserve (R/RU) over the root profile // 0 à 1
  real :: RUrac     !< // OUTPUT // maximum available water reserve over the root profile // mm
  real :: somcourmont   !< // OUTPUT // Cumulatied units of development from the start of vernalisation // degree.days
  real :: psibase ! P_nbplantes       // OUTPUT // Predawn leaf water potential potentiel foliaire de base // Mpascal
  integer :: nbjpourdecisemis      !< // OUTPUT // "Number of days until sowing is launched when it's postponed by the   sowing decision  option activation" // days
  integer :: nbjpourdecirecolte      !< // OUTPUT // "Number of days until harvest is launched when it's postponed by the  harvest decision  option activation" // days

  !!!MODIF HISAFE 1 : suppression des chaines de caractères
  !!!character (len=3) :: codebbch      !< // OUTPUT // BBCH stage (see plant file) // SD
  real :: mortreserve   !< // OUTPUT // Reserve biomass corresponding to dead tillers // t.ha-1.d-1

  integer  :: codeperenne0  !< // variable de sauvergarde  // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // // 0

!DR 19/02/2016 j'ajoute les chgts unites, je ne les indice pas sur AOAS puisque c'est que des sorties
  real  ::   masec_kg_ha
  real  ::   mafruit_kg_ha
  real  ::   mafeuil_kg_ha
  real  ::   matigestruc_kg_ha

  real  :: gel1_percent   !< // OUTPUT // frost damage on foliage before amf // %
  real  :: gel2_percent   !< // OUTPUT // frost damage on foliage after amf // %
  real  :: gel3_percent   !< // OUTPUT // frost damage on foliage or fruits // %

  real  :: nbinflo_recal  !< // OUTPUT // imposed number of inflorescences  // nb pl-1
  integer :: codebbch_output      !< // OUTPUT // BBCH stage (see plant file) // 1-99

  real      :: ebmax_gr  !< // OUTPUT // Maximum radiation use efficiency during the vegetative stage (AMF-DRP) // cg MJ-1
  real      :: fpari_gr   !< // OUTPUT // radiation factor on the calculation of conversion efficiency // g MJ-1


! ********************************
! *       Variables AO/AS        *
! ********************************

!: Les variables AO/AS pourraient être indicées non pas sur un indice fixe égal à 2
!: En effet, la plante principale n'a que la partie AS, et seule la plante dominée a AO/AS
!: On pourrait donc créer un taille de tableau dynamique selon la dominance de la plante.

! domi - 25/10/05 - on indice irazo et qnplante sur le jour

  real      :: surf(2) ! surface de la plante sur chaque composante A l'Ombre, Au Soleil  	  // OUTPUT // Fraction of surface in the shade // 0-1

  real      :: surfSous(2) ! surface de la plante sur chaque composante A l'Ombre, Au Soleil  

  real      :: tursla(0:2)  
  real      :: allocfruit(0:2)   !< // OUTPUT // Allocation ratio of  assimilats to the  fruits 0 to 1  // 0-1
  real      :: mafeuiljaune(0:2)   !< // OUTPUT // Dry matter of yellow leaves // t.ha-1
  real      :: somsenreste(0:2)  
  real      :: resperenne(0:2)   !< // OUTPUT // C crop reserve, during the cropping season, or during the intercrop period (for perenial crops) // t ha-1
  real      :: deltares(0:2)  
  real      :: misenreserve(0:2)  
  real      :: innlai(0:2)   !< // OUTPUT // Index of nitrogen stress active on leaf growth // P_innmin to 1
  real      :: lairogne(0:2)  
  real      :: biorogne(0:2)  
  real      :: sla(0:2)   !< // OUTPUT // Specific surface area // cm2 g-1
  real      :: cumdltares(0:2)  
  real      :: innsenes(0:2)   !< // OUTPUT // Index of nitrogen stress active on leaf death // P_innmin to 1
  real      :: senfac(0:2)   !< // OUTPUT // Water stress index on senescence // 0-1
  real      :: deltatauxcouv(0:2)  
  real      :: mafeuiltombe(0:2)   !< // OUTPUT // Dry matter of fallen leaves // t.ha-1
  real      :: dltamstombe(0:2)  
  real      :: mafeuil(0:2)   !< // OUTPUT // Dry matter of leaves // t.ha-1
  real      :: mafeuilverte(0:2)   !< // OUTPUT // Dry matter of green leaves // t.ha-1
  real      :: matigestruc(0:2)   !< // OUTPUT // Dry matter of stems (only structural parts) // t.ha-1
  real      :: mareserve(0:2)  
  real      :: masecveg(0:2)   !< // OUTPUT // Vegetative dry matter // t.ha-1
  real      :: maenfruit(0:2)   !< // OUTPUT // Dry matter of harvested organ envelopes // t.ha-1
  real      :: pfeuiljaune(0:2)   !< // OUTPUT // Proportion of yellow leaves in total biomass // 0-1
  real      :: ptigestruc(0:2)   !< // OUTPUT // Proportion of structural stems in total biomass // 0-1
  real      :: penfruit(0:2)   !< // OUTPUT // Proportion of fruit envelopes in total biomass // 0-1
  real      :: preserve(0:2)   !< // OUTPUT // Proportion of reserve in the total biomass // 0-1
  real      :: deshyd(0:2,0:50)  
  real      :: eaufruit(0:2,0:50)  
  real      :: frplusp(0:2)  
  real      :: sucre(0:2)   !< // OUTPUT // Sugar content of fresh harvested organs // % (of fresh weight)
  real      :: huile(0:2)   !< // OUTPUT // Oil content of fresh harvested organs // % (of fresh weight)
  real      :: sucrems(0:2)  
  real      :: huilems(0:2)  
  real      :: biorognecum(0:2)  
  real      :: lairognecum(0:2)  
  real      :: pgraingel(0:2)  
  real      :: laieffcum(0:2)  
  real      :: bioeffcum(0:2)  
  real      :: rdtint(0:2,0:20)  
  real      :: nbfrint(0:2,20)  
  real      :: pdsfruittot(0:2)  
  real      :: h2orec(0:2)   !< // OUTPUT // Water content of harvested organs // 0-1
  real      :: sucreder(0:2)  
  real      :: huileder(0:2)  
  real      :: teauint(0:2,20)  
  real      :: fapar(0:2)   !< // OUTPUT // Proportion of radiation intercepted // 0-1
  real      :: mafeuilp(0:2)  
  real      :: mabois(0:2)   !< // OUTPUT // Prunning dry weight // t.ha-1
  real      :: eai(0:2)  
  real      :: spfruit(0:2)   !< // OUTPUT // Index of trophic stress applied to the number of fruits // 0 to 1
  real      :: dltaremobil(0:2)   !< // OUTPUT // Amount of perennial reserves remobilised // g.m-2.j-1
  real      :: mafrais(0:2)   !< // OUTPUT // Aboveground fresh matter // t.ha-1
  real      :: mafraisfeuille(0:2)  
  real      :: mafraisrec(0:2)  
  real      :: mafraisres(0:2)  
  real      :: mafraistige(0:2)  
  real      :: masecvegp(0:2)  

  !!!MODIF HISAFE 2 : reduction dimension temporelle
  !!!real      :: durvie(0:2,0:731)            ! temporel pour sénescence    // OUTPUT // Actual life span of the leaf surface //  degree C
  !!!real      :: abso(0:2,0:731)              ! temporel pour apports    // OUTPUT // Nitrogen absorption rate by plant  // kg N ha-1
  !!!real      :: pfeuilverte(0:2,0:731)       ! temporel pour sénescence    // OUTPUT // Proportion of green leaves in total non-senescent biomass // 0-1
  real      :: durvie(0:2,0:366)            ! temporel pour sénescence    // OUTPUT // Actual life span of the leaf surface //  degree C
  real      :: abso(0:2,0:366)              ! temporel pour apports    // OUTPUT // Nitrogen absorption rate by plant  // kg N ha-1
  real      :: pfeuilverte(0:2,0:366)       ! temporel pour sénescence    // OUTPUT // Proportion of green leaves in total non-senescent biomass // 0-1

   !!!MODIF HISAFE 4 : suppression dimension temporelle
  !!!real      :: pfeuil(0:2,0:731)            ! pas besoin du tableau temporel    // OUTPUT // Proportion of leaves in total biomass // 0-1


  real      :: CNgrain(0:2)  ! à la base, CNgrain n'est pas un tableau temporel, les calculs ne sont pas prévus pour, donc on supprime l'indice temporel    // OUTPUT // Nitrogen concentration of grains  // %
                             ! (0:2,0:731)
                             ! besoin de la valeur pour le jour nrec, on crée une variable de stockage pour cette valeur pour supprimer le tableau temporel ?
  real      :: CNplante(0:2) ! à la base, CNplante n'est pas un tableau temporel, les calculs ne sont pas prévus pour, donc on supprime l'indice temporel    // OUTPUT // Nitrogen concentration of entire plant  // %
                             ! (0:2,0:731)
                             ! besoin de la valeur pour le jour nrec, on crée une variable de stockage pour cette valeur pour supprimer le tableau temporel ?

  !!!MODIF HISAFE 2 : reduction dimension temporelle
  !!!real      :: deltai(0:2,0:731)            ! temporel pour sénescence    // OUTPUT // Daily increase of the green leaf index // m2 leafs.m-2 soil
  real      :: deltai(0:2,0:366)            ! temporel pour sénescence    // OUTPUT // Daily increase of the green leaf index // m2 leafs.m-2 soil
  real      :: demande(0:2)   !< // OUTPUT // Daily nitrogen need of the plant   // kgN.ha-1.j-1
  real      :: dltags(0:2)   !< // OUTPUT // Growth rate of the grains  // t ha-1.j-1
  real      :: dltamsen(0:2)   !< // OUTPUT // Senescence rate // t ha-1 j-1

  !!!MODIF HISAFE 2 : reduction dimension temporelle
  !!!real      :: dltams(0:2,0:731)            ! temporel pour sénescence    // OUTPUT // Growth rate of the plant  // t ha-1.j-1
  real      :: dltams(0:2,0:366)            ! temporel pour sénescence    // OUTPUT // Growth rate of the plant  // t ha-1.j-1
  real      :: dltamsres(0:2)  
  real      :: dltaisenat(0:2)  
  real      :: dltaisen(0:2)   !< // OUTPUT // Daily increase of the senescent leaf index // m2.m-2 sol.j-1
  real      :: eop(0:2)   !< // OUTPUT // Maximum transpiration flux  // mm
  real      :: ep(0:2)   !< // OUTPUT // Actual transpiration flux  // mm j-1
  real      :: epsib(0:2)   !< // OUTPUT // Radiation use efficiency  // t ha-1.Mj-1. m-2
  real      :: epz(0:2,1000)
  real      :: fpft(0:2)   !< // OUTPUT // Sink strength of fruits  // g.m-2 j-1
  !!!MODIF HISAFE 4 : suppression dimension temporelle
  !!!real      :: fpv(0:2,731)                 !   // OUTPUT // Sink strength of developing leaves // g.j-1.m-2
  real      :: hauteur(0:2)   !< // OUTPUT // Height of canopy // m

  real      :: inn0(0:2)  
  real      :: inn(0:2)   !< // OUTPUT // Nitrogen nutrition index (satisfaction of nitrogen needs ) // 0-1
  real      :: inns(0:2)   !< // OUTPUT // Index of nitrogen stress active on growth in biomass // P_innmin to 1
  real      :: interpluie(0:2)   !< // OUTPUT // Water intercepted by leaves // mm

  !!!MODIF HISAFE 2 : reduction dimension temporelle
  !!!real      :: irazo(0:2,0:731)             ! pas besoin du tableau temporel    // OUTPUT // Nitrogen harvest index  // gN grain gN plant-1
  !!!real      :: ircarb(0:2,0:731)            ! pas besoin du tableau temporel    // OUTPUT // Carbon harvest index // g  grain g plant-1
  !!!real      :: lai(0:2,0:731)   !< // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  !!!real      :: laisen(0:2,0:731)            ! pas besoin du tableau temporel(à vérifier)    // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
  !!!real      :: magrain(0:2,0:731)           ! pas besoin du tableau temporel(à vérifier mais à 99% sûr)
  !!!real      :: masec(0:2,0:731)             ! On doit pouvoir se passer du tableau temporel, simplement en sauvegardant la valeur n-1 (et encore...)    // OUTPUT // Aboveground dry matter  // t.ha-1
  real      :: irazo(0:2,0:366)             !    // OUTPUT // Nitrogen harvest index  // gN grain gN plant-1
  real      :: ircarb(0:2,0:366)            !  // OUTPUT // Carbon harvest index // g  grain g plant-1
  real      :: lai(0:2,0:366)   !< // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real      :: laisen(0:2,0:366)            ! // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
  real      :: magrain(0:2,0:366)           !
  real      :: masec(0:2,0:366)             ! On doit pouvoir se passer du tableau temporel, simplement en sauvegardant la valeur n-1 (et encore...)    // OUTPUT // Aboveground dry matter  // t.ha-1

  real      :: masecneo(0:2)   !< // OUTPUT // Newly-formed dry matter  // t.ha-1
  real      :: masecneov(0:2)  
  real      :: masecpartiel(0:2)  
  real      :: mouill(0:2)  
  real      :: msneojaune(0:2)   !< // OUTPUT // Newly-formed senescent dry matter  // t.ha-1
  real      :: msneojaunev(0:2)  
  real      :: msres(0:2)  
  real      :: msresv(0:2)  
  real      :: msresjaune(0:2)   !< // OUTPUT // Senescent residual dry matter  // t.ha-1
  real      :: msresjaunev(0:2)  
  ! dr et Fr on ajoute la matiereseche jaune totale
  real      :: msjaune(0:2)   !< // OUTPUT // Senescent dry matter  // t.ha-1
  real      :: naer(0:2)  
  real      :: nbgrains(0:2)  
  real      :: nbgrainsv(0:2)  
  real      :: nfruit(0:2,50)   !< // OUTPUT // Number of fruits in box 5 // nb fruits
  real      :: nfruitv(0:2,50)
  real      :: nfruitnou(0:2)
  real      :: pdsfruitfrais(0:2)   !< // OUTPUT // Total weight of fresh fruits // g m-2
  real      :: pgrain(0:2)  
  real      :: pdsfruit(0:2,50)   !< // OUTPUT // Weight of fruits in box 3 // g m-2
  real      :: pousfruit(0:2)   !< // OUTPUT // Number of fruits transferred from one box to the next  // nb fruits
  real      :: QNgrain(0:2)  ! à la base, QNgrain n'est pas un tableau temporel, les calculs ne sont pas prévus pour, donc on supprime l'indice temporel    // OUTPUT // Amount of nitrogen in harvested organs (grains / fruits) // kg ha-1
                             ! (0:2,0:731)
                             ! besoin de la valeur pour le jour nrec, on crée une variable de stockage pour cette valeur pour supprimer le tableau temporel ?
                             ! besoin de la valeur pour le jour ntaille, on crée une variable de stockage pour cette valeur pour supprimer le tableau temporel ?

  !!!MODIF HISAFE 2 : reduction dimension temporelle
  !!!real      :: QNplante(0:2,0:731)          ! On doit pouvoir se passer du tableau temporel, simplement en sauvegardant la valeur n-1 (et encore...)    // OUTPUT // Amount of nitrogen taken up by the plant  // kgN.ha-1
  real      :: QNplante(0:2,0:366)          ! On doit pouvoir se passer du tableau temporel, simplement en sauvegardant la valeur n-1 (et encore...)    // OUTPUT // Amount of nitrogen taken up by the plant  // kgN.ha-1

  ! Ajout Bruno
  real      :: QCplante     !< // OUTPUT // Amount of C in the aerial part of the plant  // kg.ha-1
  real      :: raint(0:2)   !< // OUTPUT // Photosynthetic active radiation intercepted by the canopy  // Mj.m-2
  real      :: remobilj(0:2)   !< // OUTPUT // Amount of biomass remobilized on a daily basis for the fruits  // g.m-2 j-1
  real      :: sourcepuits(0:2)   !< // OUTPUT // Pool/sink ratio // 0-1
  real      :: splai(0:2)   !< // OUTPUT // Pool/sink ratio applied to leaves // 0-1
  real      :: swfac(0:2)   !< // OUTPUT // Index of stomatic water stress  // 0-1
  real      :: teaugrain(0:2)  
  real      :: turfac(0:2)   !< // OUTPUT // Index of turgescence water stress  // 0-1
  real      :: vitmoy(0:2)   !< // OUTPUT // mean growth rate of the canopy (dans le livre c'est en g mais ca colle pas)  // g.m-2.d-1
! domi 16/09/05 mainetnant indexe sur ao/as
  real      :: offrenod(0:2)   !< // OUTPUT // Amount of fixed nitrogen by nodules // kg.ha-1.j-1
  real      :: fixreel(0:2)   !< // OUTPUT // Actual rate of symbiotic uptake // kg ha-1 j-1
  real      :: fixpot(0:2)   !< // OUTPUT // Potential rate of symbiotic uptake // kg ha-1 j-1
  real      :: fixmaxvar(0:2)  
  real      :: Qfix(0:2)   !< // OUTPUT // Quantity of nitrogen fixed by nodosities // kg.ha-1
! *- Nb - le 23/11/04
  real      :: deltahauteur(0:2)  


! DR 16/12/2005 je rajoute un stress sur la photoperiode pour la vigne
  real      :: strphot(0:2)  
  real      :: strphotveille(0:2)  
 !!!MODIF HISAFE 4 : suppression dimension temporelle
 !!! real      :: photnet(0:2,731)             ! pas besoin du tableau temporel    // OUTPUT // net photosynthesis // t ha-1.j-1


  real      :: masecdil(0:2)
  real      :: laimax(0:2) !< // OUTPUT // maximal LAI // SD

! je rajoute ces varaibles au lieu de faire un changement d'unite dans correspondance
! elles sont juste dimensionnés sur AOAS
  real      :: H2Orec_percent !< // OUTPUT // Water content of harvested organs in percentage// %
  real      :: sucre_percent   !< // OUTPUT // Sugar content of fresh harvested organs // % (of fresh weight)
  real      :: huile_percent   !< // OUTPUT // Oil content of fresh harvested organs // % (of fresh weight)

  real      :: et0 !< // OUTPUT // eos+eop // mm
!--------
! Quand la plante meurt, on peut la remplacer par du solnu
! puis, dans le cadre d'un enchainement d'années,
! on peut reprendre la plante originelle.
!logical   ::  is_p_originale
!type(Plante_), pointer :: p_originale
! DR 22/04/2016 la variable uniquement calculee dans bilan est à sortir pour le rapport
  real    :: QNexport ! N exporté
  integer :: day_cut  ! jour de la coupe precedente



!!! ******************************************************
!!!MODIF HISAFE 4 : suppression dimension temporelle
!!! *******************************************************
!!! Je regroupe ici tous les tableaux dont j'ai enlevé la dimension temporelle
  real      :: rlveille(1000)            ! Root length day-1
  real      :: rljour(1000)              ! Root length day
  real      :: upvt                                 ! OUTPUT // Daily development unit  // degree.days
  real      :: utp                                  !
  real      :: pfeuil(0:2)                          ! OUTPUT // Proportion of leaves in total biomass // 0-1
  real      :: fpv(0:2)                             ! OUTPUT // Sink strength of developing leaves // g.j-1.m-2
  real      :: photnet(0:2)                         ! OUTPUT // net photosynthesis // t ha-1.j-1

!!!******************************************************************************
!!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Plante.f90)
!!!*********************************************************************************
!!! Je regroupe ici toutes les varibles que j'ai enlevé de Stics_Transit_
  real :: bdilI                         ! NB le 11/02/05 nouveautés courbes de dilution N
  real :: adilI
  real :: adilmaxI
  real :: bdilmaxI
  real :: inni(0:2)       ! pour chaque plante, AO/AS
  real :: deltabso(0:2)   ! pour chaque plante, AO/AS
  real :: dltamsN(0:2)    ! pour chaque plante, AO/AS
  logical :: humectation
  integer :: nbjhumec
  real :: pluiesemis
  real :: dltaremobilN(0:2)         ! 2 plantes, AO et AS
  real :: somtemphumec
  real :: LAIapex
  integer :: nmontaison
  integer :: onestan2



end type Plante_

interface Plante_Lecture
module procedure Plante_Lecture_fp, Plante_Lecture_fichier
end interface

contains


!> Fonction générique de lecture des paramètres plante par descripteur de fichier.
!!
!!
subroutine Plante_Lecture_fp(p, file_pointer, method, codeRetour, path, pathplt)

  type(Plante_) :: p  
  integer, intent(IN) :: file_pointer  !>  
  integer, intent(IN) ::  method  
  integer, intent(OUT):: codeRetour  

    ! enabling_record :le chemin pour accéder à la config
   character(len=255), intent(IN) :: path ! enabling_record
     ! enabling_record :le chemin pour accéder directement à la config
   character(len=255), intent(IN) :: pathplt ! enabling_record


  select case(method)

    case(PLANTE_METHOD_XML_V7)
      !call Plante_Lecture_XML(p, file_pointer)

    case(PLANTE_METHOD_STICS_V6)
      call Plante_Lecture_V6(p, file_pointer, codeRetour,nb_variete_max)

    case DEFAULT !: la méthode choisie ne correspond à aucune méthode définie => ERREUR
      codeRetour = PLANTE_LECTURE_ERREUR_NO_METHOD
      return

  end select

  !codeRetour = PLANTE_LECTURE_OK

return
end subroutine Plante_Lecture_fp


!> Fonction générique de lecture des paramètres plante par nom de fichier.
!!
!!
subroutine Plante_Lecture_fichier(p, fichier, method, codeRetour, path , pathplt)

  type(Plante_), intent(INOUT)   :: p  
  character(len=255), intent(IN) :: fichier  
  integer, intent(IN), optional  :: method
  integer, intent(OUT)           :: codeRetour  

   ! enabling_record :le chemin pour accéder à la config
   character(len=255), intent(IN) :: path ! enabling_record
     ! enabling_record :le chemin pour accéder directement à la config
   character(len=255), intent(IN) :: pathplt ! enabling_record


  integer      :: fp = 36  


! 19/11/2013 pour record
  integer ib0                                       ! enabling_record
  integer ib1                                       ! enabling_record
  character(len=300) :: filepluspath                ! enabling_record
  ib0 = len_trim(pathplt)                        ! enabling_record
  if (ib0 .ne. 0 ) then                             ! enabling_record
     filepluspath =  pathplt                     ! enabling_record
  else
     ib1 = len_trim(path)                           ! enabling_record
     if (ib1 .eq. 0 ) then                             ! enabling_record
        filepluspath = fichier                          ! enabling_record
     else                                              ! enabling_record
        filepluspath = path(1:ib1) // '/' // fichier ! enabling_record
     endif                                             ! enabling_record
  endif
! fin record


  ! on ouvre le fichier
  !open(unit=fp, file=fichier, status='old',action='read',position='rewind',iostat=codeRetour)
  open(unit=fp,file = filepluspath ,status = 'old',action='read',position='rewind',iostat=coderetour)      ! enabling_record

write (*,*) 'filepluspath=',filepluspath
  if (codeRetour < 0) then ! Erreur à l'ouverture du fichier

    return
  endif

  if (present(method)) then
    call Plante_Lecture(p,fp,method,codeRetour, path, pathplt)
  else
    ! on appelle la lecture du fichier avec le méthode Stics V6
    call Plante_Lecture(p,fp,PLANTE_METHOD_STICS_V6,codeRetour,path, pathplt)
  endif

  close(fp)

  if (codeRetour /= PLANTE_LECTURE_OK) then

    return
  endif

return
end subroutine Plante_Lecture_fichier

!========================================================================================!
!========================================================================================!
!========================================================================================!

subroutine Plante_Lecture_V6(p,fp,codeRetour,nb_variete_max)

    type(Plante_), intent(INOUT) :: p  
    integer,       intent(IN)    :: fp  
    integer,       intent(OUT)   :: codeRetour  
    integer,       intent(IN)    :: nb_variete_max


! Variables locales pour la lecture des parametres

      integer  :: j  

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!enlevé de l'objet principal donc déclaré en local ici
character(len=3) :: P_stadebbchplt  !< // PARAMETER // equivalent stage in BBCH-scale (sowing) //  // PARPLT // 0
character(len=3) :: P_stadebbchger  !< // PARAMETER // equivalent stage in BBCH-scale (germination) //  // PARPLT // 0
character(len=3) :: P_stadebbchlev  !< // PARAMETER // equivalent stage in BBCH-scale (emergence) //  // PARPLT // 0
character(len=3) :: P_stadebbchamf  !< // PARAMETER // equivalent stage in BBCH-scale (amf) //  // PARPLT // 0
character(len=3) :: P_stadebbchlax  !< // PARAMETER // equivalent stage in BBCH-scale (lax) //  // PARPLT // 0
character(len=3) :: P_stadebbchsen  !< // PARAMETER // equivalent stage in BBCH-scale (senescence) //  // PARPLT // 0
character(len=3) :: P_stadebbchflo  !< // PARAMETER // equivalent stage in BBCH-scale (flowering) //  // PARPLT // 0
character(len=3) :: P_stadebbchdrp  !< // PARAMETER // equivalent stage in BBCH-scale (drp) //  // PARPLT // 0
character(len=3) :: P_stadebbchnou  !< // PARAMETER // equivalent stage in BBCH-scale (fruit set) //  // PARPLT // 0
character(len=3) :: P_stadebbchdebdes  !< // PARAMETER // equivalent stage in BBCH-scale (debdes) //  // PARPLT // 0
character(len=3) :: P_stadebbchmat  !< // PARAMETER // equivalent stage in BBCH-scale (maturity) //  // PARPLT // 0
character(len=3) :: P_stadebbchrec  !< // PARAMETER // equivalent stage in BBCH-scale (harvest) //  // PARPLT // 0
character(len=3) :: P_stadebbchfindorm  !< // PARAMETER // equivalent stage in BBCH-scale (end of dormancy) //  // PARPLT // 0



!      p%nb_variete_maxi=30
!plant name and clade
!*********************
write (*,*) 'Plante_Lecture_V6'
      read (fp,*)
      read (fp,*,err = 250) p%P_codeplante
      read (fp,*)
      read (fp,*,err = 250) p%P_codemonocot

  write (*,*) 'P_codeplante=',p%P_codeplante
!effect of atmospheric CO2 concentration
!*********************
      read (fp,*)
      read (fp,*,err = 250) p%P_alphaco2
!phasic development
!*********************
      read (fp,*)
      read (fp,*,err = 250) p%P_tdmin
      read (fp,*)
      read (fp,*,err = 250) p%P_tdmax
      read (fp,*)
      read (fp,*,err = 250) p%P_codetemp
      read (fp,*)
      read (fp,*,err = 250) p%P_codegdh
      read (fp,*)
      read (fp,*,err = 250) p%P_coeflevamf
      read (fp,*)
      read (fp,*,err = 250) p%P_coefamflax
      read (fp,*)
      read (fp,*,err = 250) p%P_coeflaxsen
      read (fp,*)
      read (fp,*,err = 250) p%P_coefsenlan
      read (fp,*)
      read (fp,*,err = 250) p%P_coeflevdrp
      read (fp,*)
      read (fp,*,err = 250) p%P_coefdrpmat
      read (fp,*)
      read (fp,*,err = 250) p%P_coefflodrp
      read (fp,*)
      read (fp,*,err = 250) p%P_codephot
      read (fp,*)
      read (fp,*,err = 250) p%P_phobase
      read (fp,*)
      read (fp,*,err = 250) p%P_phosat
      read (fp,*)
      read (fp,*,err = 250) p%P_coderetflo
      read (fp,*)
      read (fp,*,err = 250) p%P_stressdev
      read (fp,*)
      read (fp,*,err = 250) p%P_codebfroid
      read (fp,*)
      read (fp,*,err = 250) p%P_jvcmini
      read (fp,*)
      read (fp,*,err = 250) p%P_julvernal
      read (fp,*)
      read (fp,*,err = 250) p%P_tfroid
      read (fp,*)
      read (fp,*,err = 250) p%P_ampfroid
! DR 23/10/07
      read (fp,*)
      read (fp,*,err = 250) p%P_stdordebour
      read (fp,*)
      read (fp,*,err = 250) p%P_tdmindeb
      read (fp,*)
      read (fp,*,err = 250) p%P_tdmaxdeb
!
      read (fp,*)
      read (fp,*,err = 250) p%P_codedormance
      read (fp,*)
      read (fp,*,err = 250) p%P_ifindorm
      read (fp,*)
      read (fp,*,err = 250) p%P_q10
      read (fp,*)
      read (fp,*,err = 250) p%P_idebdorm
      read (fp,*)
      read (fp,*,err = 250) p%P_codegdhdeb
!emergence and starting
!*********************
      read (fp,*)
      read (fp,*,err = 250) p%P_codeperenne
      read (fp,*)
      read (fp,*,err = 250) p%P_codegermin
      read (fp,*)
      read (fp,*,err = 250) p%P_tgmin
      read (fp,*)
      read (fp,*,err = 250) p%P_stpltger
! DR 23/10/07
      read (fp,*)
      read (fp,*,err = 250) p%P_potgermi
      read (fp,*)
      read (fp,*,err = 250) p%P_nbjgerlim
      read (fp,*)
      read (fp,*,err = 250) p%P_propjgermin
!
      read (fp,*)
      read (fp,*,err = 250) p%P_codehypo
      read (fp,*)
      read (fp,*,err = 250) p%P_belong
      read (fp,*)
      read (fp,*,err = 250) p%P_celong
      read (fp,*)
      read (fp,*,err = 250) p%P_elmax
      read (fp,*)
      read (fp,*,err = 250) p%P_nlevlim1
      read (fp,*)
      read (fp,*,err = 250) p%P_nlevlim2
! DR 23/10/07
      read (fp,*)
      read (fp,*,err = 250) p%P_vigueurbat
!
      read (fp,*)
      read (fp,*,err = 250) p%P_laiplantule
      read (fp,*)
      read (fp,*,err = 250) p%P_nbfeuilplant
! DR 23/10/07
      read (fp,*)
      read (fp,*,err = 250) p%P_masecplantule
      read (fp,*)
      read (fp,*,err = 250) p%P_zracplantule
!
!leaves
!*********************
      read (fp,*)
      read (fp,*,err = 250) p%P_phyllotherme
      read (fp,*)
      read (fp,*,err = 250) p%P_bdens
      read (fp,*)
      read (fp,*,err = 250) p%P_laicomp
      read (fp,*)
      read (fp,*,err = 250) p%P_hautbase
      read (fp,*)
      read (fp,*,err = 250) p%P_hautmax
! DR 22/02/08 deplace ici car concerne le feuillage
      read (fp,*)
      read (fp,*,err = 250) p%P_tcxstop
!
      read (fp,*)
      read (fp,*,err = 250) p%P_codelaitr
      read (fp,*)
      read (fp,*,err = 250) p%P_vlaimax
      read (fp,*)
      read (fp,*,err = 250) p%P_pentlaimax
      read (fp,*)
      read (fp,*,err = 250) p%P_udlaimax
      read (fp,*)
      read (fp,*,err = 250) p%P_ratiodurvieI
      read (fp,*)
      read (fp,*,err = 250) p%P_tcmin
      read (fp,*)
      read (fp,*,err = 250) p%P_tcmax
      read (fp,*)
      read (fp,*,err = 250) p%P_ratiosen
      read (fp,*)
      read (fp,*,err = 250) p%P_abscission
      read (fp,*)
      read (fp,*,err = 250) p%P_parazofmorte
      read (fp,*)
      read (fp,*,err = 250) p%P_innturgmin
! DR 11/04/2011 le P_dlaimin passe de paramv6 dans les fichiers plantes (0.1 prairie, 0.0 pour les autres)
!      read (36,*) para
!      read (36,*,err = 250) t%P_codedlaimin
      read (fp,*)
      read (fp,*,err = 250) p%P_dlaimin
!
      read (fp,*)
      read (fp,*,err = 250) p%P_codlainet
      read (fp,*)
      read (fp,*,err = 250) p%P_dlaimax
      read (fp,*)
      read (fp,*,err = 250) p%P_tustressmin
      read (fp,*)
      read (fp,*,err = 250) p%P_dlaimaxbrut
      read (fp,*)
      read (fp,*,err = 250) p%P_durviesupmax
      read (fp,*)
      read (fp,*,err = 250) p%P_innsen
      read (fp,*)
      read (fp,*,err = 250) p%P_rapsenturg
! DR 23/10/07
      read (fp,*)
      read (fp,*,err = 250) p%P_codestrphot
      read (fp,*)
      read (fp,*,err = 250) p%P_phobasesen
      read (fp,*)
      read (fp,*,err = 250) p%P_dltamsmaxsen
! 30/01/2012 issu de param_gen
  read (fp,*)
  read (fp,*,err = 250) p%P_dltamsminsen
  read (fp,*)
  read (fp,*,err = 250) p%P_alphaphot
!
      read (fp,*)
      read (fp,*,err = 250) p%P_tauxrecouvmax
      read (fp,*)
      read (fp,*,err = 250) p%P_tauxrecouvkmax
      read (fp,*)
      read (fp,*,err = 250) p%P_pentrecouv
      read (fp,*)
      read (fp,*,err = 250) p%P_infrecouv
!radiation interception
!*********************
      read (fp,*)
      read (fp,*,err = 250) p%P_codetransrad
      read (fp,*)
      read (fp,*,err = 250) p%P_extin
      read (fp,*)
      read (fp,*,err = 250) p%P_ktrou
      read (fp,*)
      read (fp,*,err = 250) p%P_forme
      read (fp,*)
      read (fp,*,err = 250) p%P_rapforme
      read (fp,*)
      read (fp,*,err = 250) p%P_adfol
      read (fp,*)
      read (fp,*,err = 250) p%P_dfolbas
      read (fp,*)
      read (fp,*,err = 250) p%P_dfolhaut
!shoot biomass growth
!*********************
      read (fp,*)
      read (fp,*,err = 250) p%P_temin
      read (fp,*)
      read (fp,*,err = 250) p%P_temax
      read (fp,*)
      read (fp,*,err = 250) p%P_teopt
      read (fp,*)
      read (fp,*,err = 250) p%P_teoptbis
      read (fp,*)
      read (fp,*,err = 250) p%P_efcroijuv
      read (fp,*)
      read (fp,*,err = 250) p%P_efcroiveg
      read (fp,*)
      read (fp,*,err = 250) p%P_efcroirepro
      read (fp,*)
      read (fp,*,err = 250) p%P_remobres
      read (fp,*)
      read (fp,*,err = 250) p%P_coefmshaut
!partitioning of biomass in organs
!*********************
      read (fp,*)
      read (fp,*,err = 250) p%P_slamax
      read (fp,*)
      read (fp,*,err = 250) p%P_slamin
      read (fp,*)
      read (fp,*,err = 250) p%P_tigefeuil
      read (fp,*)
      read (fp,*,err = 250) p%P_envfruit
!  NB - le 28/03/02
      read (fp,*)
      read (fp,*,err = 250) p%P_sea
! DR 22/02/08 viré!
!      read (fp,*)
!      read (fp,*,err = 250) p%ratioresfauche
!
!yield formation
!*********************
      read (fp,*)
      read (fp,*,err = 250) p%P_codeindetermin
      read (fp,*)
      read (fp,*,err = 250) p%P_nbjgrain
      read (fp,*)
      read (fp,*,err = 250) p%P_cgrain
      read (fp,*)
      read (fp,*,err = 250) p%P_cgrainv0
      read (fp,*)
      read (fp,*,err = 250) p%P_nbgrmin
      read (fp,*)
      read (fp,*,err = 250) p%P_codeir
      read (fp,*)
      read (fp,*,err = 250) p%P_vitircarb
      read (fp,*)
      read (fp,*,err = 250) p%P_irmax
      read (fp,*)
      read (fp,*,err = 250) p%P_vitircarbT
      read (fp,*)
      read (fp,*,err = 250) p%P_nboite
      read (fp,*)
      read (fp,*,err = 250) p%P_allocfrmax
      read (fp,*)
      read (fp,*,err = 250) p%P_afpf
      read (fp,*)
      read (fp,*,err = 250) p%P_bfpf
! DR 23/10/07
      read (fp,*)
      read (fp,*,err = 250) p%P_cfpf
      read (fp,*)
      read (fp,*,err = 250) p%P_dfpf
!
      read (fp,*)
      read (fp,*,err = 250) p%P_stdrpnou
      read (fp,*)
      read (fp,*,err = 250) p%P_spfrmin
      read (fp,*)
      read (fp,*,err = 250) p%P_spfrmax
      read (fp,*)
      read (fp,*,err = 250) p%P_splaimin
      read (fp,*)
      read (fp,*,err = 250) p%P_splaimax
      read (fp,*)
      read (fp,*,err = 250) p%P_codcalinflo
      read (fp,*)
      read (fp,*,err = 250) p%P_nbinflo
      read (fp,*)
      read (fp,*,err = 250) p%P_inflomax
      read (fp,*)
      read (fp,*,err = 250) p%P_pentinflores
      read (fp,*)
      read (fp,*,err = 250) p%P_codetremp
      read (fp,*)
      read (fp,*,err = 250) p%P_tminremp
      read (fp,*)
      read (fp,*,err = 250) p%P_tmaxremp
      read (fp,*)
      read (fp,*,err = 250) p%P_vitpropsucre
      read (fp,*)
      read (fp,*,err = 250) p%P_vitprophuile
      read (fp,*)
      read (fp,*,err = 250) p%P_vitirazo
! roots
!************
      read (fp,*)
      read (fp,*,err = 250) p%P_sensanox
      read (fp,*)
      read (fp,*,err = 250) p%P_stoprac
      read (fp,*)
      read (fp,*,err = 250) p%P_sensrsec
      read (fp,*)
      read (fp,*,err = 250) p%P_contrdamax
      read (fp,*)
      read (fp,*,err = 250) p%P_codetemprac
      read (fp,*)
      read (fp,*,err = 250) p%P_coderacine
      read (fp,*)
      read (fp,*,err = 250) p%P_zlabour
      read (fp,*)
      read (fp,*,err = 250) p%P_zpente
      read (fp,*)
      read (fp,*,err = 250) p%P_zprlim
      read (fp,*)
      read (fp,*,err = 250) p%P_draclong
      read (fp,*)
      read (fp,*,err = 250) p%P_debsenrac
      read (fp,*)
      read (fp,*,err = 250) p%P_lvfront
      read (fp,*)
      read (fp,*,err = 250) p%P_longsperac
! DR 23/10/07
      read (fp,*)
      read (fp,*,err = 250) p%P_codazorac
      read (fp,*)
      read (fp,*,err = 250) p%P_minefnra
      read (fp,*)
      read (fp,*,err = 250) p%P_minazorac
      read (fp,*)
      read (fp,*,err = 250) p%P_maxazorac
! DR 23/10/07
      read (fp,*)
      read (fp,*,err = 250) p%P_codtrophrac
      read (fp,*)
      read (fp,*,err = 250) p%P_repracpermax
      read (fp,*)
      read (fp,*,err = 250) p%P_repracpermin
      read (fp,*)
      read (fp,*,err = 250) p%P_krepracperm
      read (fp,*)
      read (fp,*,err = 250) p%P_repracseumax
      read (fp,*)
      read (fp,*,err = 250) p%P_repracseumin
      read (fp,*)
      read (fp,*,err = 250) p%P_krepracseu
!
! frost
!**********************

      read (fp,*)
      read (fp,*,err = 250) p%P_tletale
      read (fp,*)
      read (fp,*,err = 250) p%P_tdebgel
      read (fp,*)
      read (fp,*,err = 250) p%P_codgellev
      read (fp,*)
      read (fp,*,err = 250) p%P_nbfgellev
      read (fp,*)
      read (fp,*,err = 250) p%P_tgellev10
      read (fp,*)
      read (fp,*,err = 250) p%P_tgellev90
      read (fp,*)
      read (fp,*,err = 250) p%P_codgeljuv
      read (fp,*)
      read (fp,*,err = 250) p%P_tgeljuv10
      read (fp,*)
      read (fp,*,err = 250) p%P_tgeljuv90
      read (fp,*)
      read (fp,*,err = 250) p%P_codgelveg
      read (fp,*)
      read (fp,*,err = 250) p%P_tgelveg10
      read (fp,*)
      read (fp,*,err = 250) p%P_tgelveg90
      read (fp,*)
      read (fp,*,err = 250) p%P_codgelflo
      read (fp,*)
      read (fp,*,err = 250) p%P_tgelflo10
      read (fp,*)
      read (fp,*,err = 250) p%P_tgelflo90
!water
!***************************
      read (fp,*)
      read (fp,*,err = 250) p%P_psisto
      read (fp,*)
      read (fp,*,err = 250) p%P_psiturg
      read (fp,*)
      read (fp,*,err = 250) p%P_h2ofeuilverte
      read (fp,*)
      read (fp,*,err = 250) p%P_h2ofeuiljaune
      read (fp,*)
      read (fp,*,err = 250) p%P_h2otigestruc
      read (fp,*)
      read (fp,*,err = 250) p%P_h2oreserve
      read (fp,*)
      read (fp,*,err = 250) p%P_h2ofrvert
! DR 06/02/08 P_stdrpdes devient varietal
!      read (fp,*)
!      read (fp,*,err = 250) p%P_stdrpdes
      read (fp,*)
      read (fp,*,err = 250) p%P_deshydbase

      write (*,*) 'P_deshydbase=',p%P_deshydbase
      read (fp,*)
      read (fp,*,err = 250) p%P_tempdeshyd
      read (fp,*)
      read (fp,*,err = 250) p%P_codebeso
      read (fp,*)
      read (fp,*,err = 250) p%P_kmax
      read (fp,*)
      read (fp,*,err = 250) p%P_rsmin
      read (fp,*)
      read (fp,*,err = 250) p%P_codeintercept
      read (fp,*)
      read (fp,*,err = 250) p%P_mouillabil
      read (fp,*)
      read (fp,*,err = 250) p%P_stemflowmax
      read (fp,*)
      read (fp,*,err = 250) p%P_kstemflow
! nitrogen
!****************************
      read (fp,*)
      read (fp,*,err = 250) p%P_Vmax1
      read (fp,*)
      read (fp,*,err = 250) p%P_Kmabs1
      read (fp,*)
      read (fp,*,err = 250) p%P_Vmax2
      read (fp,*)
      read (fp,*,err = 250) p%P_Kmabs2
      read (fp,*)
      read (fp,*,err = 250) p%P_adil
      read (fp,*)
      read (fp,*,err = 250) p%P_bdil
! DR 23/10/07
      read (fp,*)
      read (fp,*,err = 250) p%P_masecNmax
      read (fp,*)
      read (fp,*,err = 250) p%P_INNmin
      read (fp,*)
      read (fp,*,err = 250) p%P_INNimin
      read (fp,*)
      read (fp,*,err = 250) p%P_inngrain1
      read (fp,*)
      read (fp,*,err = 250) p%P_inngrain2
      read (fp,*)
      read (fp,*,err = 250) p%P_codeplisoleN
!
      read (fp,*)
      read (fp,*,err = 250) p%P_adilmax
      read (fp,*)
      read (fp,*,err = 250) p%P_bdilmax
      read (fp,*)
      read (fp,*,err = 250) p%P_Nmeta
      read (fp,*)
      read (fp,*,err = 250) p%P_masecmeta
      read (fp,*)
      read (fp,*,err = 250) p%P_Nreserve
      read (fp,*)
      read (fp,*,err = 250) p%P_codeINN
      read (fp,*)
      read (fp,*,err = 250) p%P_codelegume
!      read (fp,*)
!      read (fp,*,err = 250) p%P_codesymbiose
      read (fp,*)
      read (fp,*,err = 250) p%P_stlevdno
      read (fp,*)
      read (fp,*,err = 250) p%P_stdnofno
      read (fp,*)
      read (fp,*,err = 250) p%P_stfnofvino
      read (fp,*)
      read (fp,*,err = 250) p%P_vitno
      read (fp,*)
      read (fp,*,err = 250) p%P_profnod
      read (fp,*)
      read (fp,*,err = 250) p%P_concNnodseuil
      read (fp,*)
      read (fp,*,err = 250) p%P_concNrac0
      read (fp,*)
      read (fp,*,err = 250) p%P_concNrac100
!      read (fp,*)
!      read (fp,*,err = 250) p%P_hunod
      read (fp,*)
      read (fp,*,err = 250) p%P_tempnod1
      read (fp,*)
      read (fp,*,err = 250) p%P_tempnod2
      read (fp,*)
      read (fp,*,err = 250) p%P_tempnod3
      read (fp,*)
      read (fp,*,err = 250) p%P_tempnod4
! DR 23/10/07
      read (fp,*)
      read (fp,*,err = 250) p%P_codefixpot
      read (fp,*)
      read (fp,*,err = 250) p%P_fixmax
      read (fp,*)
      read (fp,*,err = 250) p%P_fixmaxveg
      read (fp,*)
      read (fp,*,err = 250) p%P_fixmaxgr
      read (fp,*)
      read (fp,*,err = 250) p%P_codazofruit
!correspondance code BBCH
!*************************************
! DR et ML 27/10/09 introduction des stades bbch
      read (fp,*)
      read (fp,*,err = 250) P_stadebbchplt
      read (fp,*)
      read (fp,*,err = 250) P_stadebbchger
      read (fp,*)
      read (fp,*,err = 250) P_stadebbchlev
      read (fp,*)
      read (fp,*,err = 250) P_stadebbchamf
      read (fp,*)
      read (fp,*,err = 250) P_stadebbchlax
      read (fp,*)
      read (fp,*,err = 250) P_stadebbchsen
      read (fp,*)
      read (fp,*,err = 250) P_stadebbchflo
      read (fp,*)
      read (fp,*,err = 250) P_stadebbchdrp
      read (fp,*)
      read (fp,*,err = 250) P_stadebbchnou
      read (fp,*)
      read (fp,*,err = 250) P_stadebbchdebdes
      read (fp,*)
      read (fp,*,err = 250) P_stadebbchmat
      read (fp,*)
      read (fp,*,err = 250) P_stadebbchrec
      read (fp,*)
      read (fp,*,err = 250) P_stadebbchfindorm


  write (*,*) 'P_stadebbchfindorm=',P_stadebbchfindorm


! TODO : créer un paramètre nbVariete ?
!      read (fp,*)
!      read (fp,*,err = 250) p%nbVariete

      do j = 1,nb_variete_max

    write (*,*) 'j=',j


        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_codevar(j)
        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_stlevamf(j)
        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_stamflax(j)
        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_stlevdrp(j)
        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_stflodrp(j)
        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_stdrpdes(j)
        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_pgrainmaxi(j)
        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_adens(j)
        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_croirac(j)
        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_durvieF(j)
        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_jvc(j)
        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_sensiphot(j)
        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_stlaxsen(j)
        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_stsenlan(j)
        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_nbgrmax(j)
        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_stdrpmat(j)
        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_afruitpot(j)
        read (fp,*,end = 80)
        read (fp,*,err = 250,end = 80) p%P_dureefruit(j)
      end do

      p%nbVariete = j

    ! si on arrive ici, il n'y a pas eu d'erreur, on quitte la routine.
      return

80    p%nbVariete = j-1

  write (*,*) 'p%nbVariete=',p%nbVariete

      return

!fin du fichier atteinte avant la lecture complète des paramètres

250   codeRetour = PLANTE_LECTURE_ERREUR_EOF

  write (*,*) 'erreur'

      return

    ! fichier introuvable
    !!!  call EnvoyerMsgHistorique(3273,p%P_fplt, 'a15')
      !stop
    !!!  call exit(9)




return
end subroutine

subroutine Plante_Tests(p,P_variete)

USE Messages

    type(Plante_), intent(INOUT) :: p  
    integer,       intent(INOUT) :: P_variete  !> // PARAMETER // variety number in the technical file // SD // PARTEC // 1 


      if (p%nbVariete < P_variete) then
        call EnvoyerMsgHistorique(439)
        call EnvoyerMsgHistorique('P_variete',P_variete)
        call EnvoyerMsgHistorique('p%nbVariete',p%nbVariete)
        call EnvoyerMsgHistorique(440)
        call exit(9)

        !P_variete = p%nbVariete
      endif

! * = == = == = == = == = = = > tests de compatibilité des parametres < = == = == = == = == = == = * c
      if (p%P_slamax <= 0 .or. p%P_slamin <= 0.) then
        call EnvoyerMsgHistorique(112)
        !stop
        call exit(9)
      endif
      if (p%P_codebeso /= 1) then
        if (p%P_rsmin == 0.0 .or. p%P_rsmin >= 999.) then
          call EnvoyerMsgHistorique(113,p%P_rsmin)
        !stop
        call exit(9)
        endif
        if (p%P_hautmax == 0.0 .or. p%P_hautmax >= 999.) then
          call EnvoyerMsgHistorique(114, p%P_hautmax)
        !stop
        call exit(9)
        endif
        if (p%P_hautbase >= 999.) then
          call EnvoyerMsgHistorique(115,p%P_hautbase)
        !stop
        call exit(9)
        endif
        if (p%P_hautmax <= p%P_hautbase) then
          call EnvoyerMsgHistorique(116)
        !stop
        call exit(9)
        endif
      endif

return
end subroutine Plante_Tests


! FIN DU MODULE PLANTE
end module Plante
 
 
