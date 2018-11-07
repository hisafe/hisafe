!> Module of development
!! Description :
!! calculation of Phenological stages and leaf development
!<
module Developpement



USE Stics
USE Plante
USE Itineraire_Technique
USE Sol
USE Climat
USE Station
USE Parametres_Generaux
USE Divers
USE Besoins_en_froid

implicit none


interface
! DR 23/07/2012 latitude est pas itilisé car on passe la photoperiode qui est suffisante
!subroutine develop2(P_latitude,phoi,tmax,tmin,tmin_demain,trr,P_codeh2oact,P_codeinitprec,P_codeinnact,codeulaivernal,P_psihucc, &
subroutine develop2(phoi,tmax,tmin,tmin_demain,trr,P_codeh2oact,P_codeinitprec,P_codeinnact,codeulaivernal,P_psihucc, &
                    P_psihumin, P_codcueille,P_codefauche,P_densitesem,P_profsem,P_variete,P_ampfroid,P_belong,P_celong,         &
                    P_codebfroid,P_codedormance,P_codegdh, P_codegdhdeb,P_codegermin,P_codehypo,P_codeperenne,P_codephot,        &
                    P_codeplante,P_coderetflo,P_codetemp,coeflev,cu_min,cu_veille, densite,densiteger,densitelev,P_elmax,        &
                    innlai,P_julvernal,P_jvc,P_jvcmini,P_nbjgerlim,ndrpobs,P_nlevlim1,P_nlevlim2,nlevobs,nplt,onarretesomcourdrp,&
                    P_phobase,P_phosat,P_potgermi,P_propjgermin,P_q10,P_sensiphot,P_sensrsec,somelong,somger,P_stdordebour,      &
                    P_stpltger,P_stressdev,P_tcxstop,P_tdmax,P_tdmaxdeb,P_tdmin,P_tdmindeb,P_tfroid,P_tgmin,turfac,P_vigueurbat, &
                    P_codefente,P_mulchbat,P_pluiebat,P_culturean,nbCouches,dacouche,hucc,humin,hur,jjul,n,nbjanrec,nbjsemis,    &
                    numcult,tairveille,tcult,tsol,xmlch1,P_codepluiepoquet,P_codetempfauche,humectation,nbjhumec,pluiesemis,     &
! DR 23/07/2012 P_nboite non utilisé
!                    somtemphumec,P_codeindetermin,P_codelaitr,P_codlainet,P_dureefruit,namfobs,P_nbcueille,P_nboite,nfloobs,     &
                    somtemphumec,P_codeindetermin,P_codelaitr,P_codlainet,P_dureefruit,namfobs,P_nbcueille,nfloobs,     &
                    nlanobs, nlaxobs,nmatobs,nrecobs,nsenobs,P_stdrpnou,upobs,P_codemontaison,sioncoupe,                         &
                    caljvc,cu,demande,etatvernal,hauteur,mafrais,mafraisfeuille,mafraisrec,mafraisres,mafraistige,masec,namf,    &
                    ndebdorm,ndrp,nfindorm,nflo,nger,nlan,nlev,nrec,nrecbutoir,pdsfruitfrais,rfpi,rfvi,somcour,somcourdrp,       &
                    somcourfauche,somcourutp,somtemp,stpltlev,tdevelop,udevair,udevcult,upvt,utp,zrac,maxwth,group,ndebdes,      &
                    nfruit,nlax,nmat,nnou,nsen,P_stamflax,P_stdrpdes,P_stdrpmat,stdrpsen,P_stflodrp,P_stlaxsen,P_stlevamf,       &
                    P_stlevdrp,stlevflo,stmatrec,P_stsenlan,upvtutil, P_codrecolte,h2orec,P_sucrerec,P_CNgrainrec,P_huilerec,    &
! dr 23/07/2012 huileder et sucreder inutilisés
!                    sucre,huile,teaugrain,sucreder,huileder,P_h2ofrvert,P_codeaumin, P_h2ograinmin,P_h2ograinmax,P_deshydbase,   &
                    sucre,huile,teaugrain,P_h2ofrvert,P_codeaumin, P_h2ograinmin,P_h2ograinmax,P_deshydbase,   &
                    CNgrain,P_cadencerec, jdepuisrec,pdsfruit,nbrecolte,nrecint,rdtint,teauint,nbfrint,onestan2,somcourmont,     &
                    nmontaison,stpltger,  P_idebdorm, P_iwater)


USE Divers, only: calcul_UDev, calcul_TemperaturesHoraires, calcul_GDH, cRFPI
USE Besoins_en_froid
USE Messages

implicit none

!  real,    intent(IN)                           :: P_latitude  !< // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0
  real,    intent(IN)                           :: phoi      !< // OUTPUT // Photoperiod // hours
  real,    intent(IN)                           :: tmax      !< // OUTPUT // Maximum active temperature of air // degree C
  real,    intent(IN)                           :: tmin      !< // OUTPUT // Minimum active temperature of air // degree C
  real,    intent(IN)                           :: tmin_demain  
  real,    intent(IN)                           :: trr      !< // OUTPUT // Rainfall  // mm.day-1
  integer, intent(IN)                           :: P_codeh2oact  !< // PARAMETER // code to activate  water stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
  integer, intent(IN)                           :: P_codeinitprec  !< // PARAMETER // reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0
  integer, intent(IN)                           :: P_codeinnact  !< // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
  integer, intent(IN)                           :: codeulaivernal  
  real,    intent(IN)                           :: P_psihucc  !< // PARAMETER // soil potential corresponding to field capacity  // Mpa // PARAM // 1
  real,    intent(IN)                           :: P_psihumin  !< // PARAMETER // soil potential corresponding to wilting point // Mpa // PARAM // 1
  integer, intent(IN)                           :: P_codcueille  !< // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0
  integer, intent(IN)                           :: P_codefauche  !< // PARAMETER // option of cut modes for forage crops: yes (1), no (2) // code 1/2 // PARTEC // 0
  real,    intent(IN)                           :: P_densitesem  !< // PARAMETER // Sowing density  // plants.m-2 // PARTEC // 1
  real,    intent(IN)                           :: P_profsem  !< // PARAMETER // Sowing depth // cm // PARTEC // 1
  integer, intent(IN)                           :: P_variete  !< // PARAMETER // variety number in the technical file // SD // PARTEC // 1
  real,    intent(IN)                           :: P_ampfroid  !< // PARAMETER // semi thermal amplitude thermique for vernalising effect // degree C // PARPLT // 1
  real,    intent(IN)                           :: P_belong  !< // PARAMETER // parameter of the curve of coleoptile elongation // degree.days -1 // PARPLT // 1
  real,    intent(IN)                           :: P_celong  !< // PARAMETER // parameter of the subsoil plantlet elongation curve // SD // PARPLT // 1
  integer, intent(IN)                           :: P_codebfroid  !< // PARAMETER // option of calculation of chilling requirements // code 1/2 // PARPLT // 0
  integer, intent(IN)                           :: P_codedormance  !< // PARAMETER // option of calculation of dormancy and chilling requirement // code 1/2 // PARPLT // 0
  integer, intent(IN)                           :: P_codegdh  !< // PARAMETER // hourly (1) or daily (2) calculation of development unit // code 1/2 // PARPLT // 0
  integer, intent(IN)                           :: P_codegdhdeb  !< // PARAMETER // option of calculation of the bud break date in hourly or daily growing degrees  // code 1/2 // PARPLT // 0
  integer, intent(IN)                           :: P_codegermin  !< // PARAMETER // option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2) // code 1/2 // PARPLT // 0
  integer, intent(IN)                           :: P_codehypo  !< // PARAMETER // option of simulation of a  phase of hypocotyl growth (1) or planting of plantlets (2) // code 1/2 // PARPLT // 0
  integer, intent(IN)                           :: P_codeperenne  !< // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0
  integer, intent(IN)                           :: P_codephot  !< // PARAMETER // option of plant photoperiodism: yes (1), no (2) // code1/2 // PARPLT // 0
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
!!!  character(len=3), intent(IN)                  :: P_codeplante  !< // PARAMETER // Name code of the plant in 3 letters // * // PARPLT // 0
  integer, intent(IN)                           :: P_codeplante  !< // PARAMETER // Name code of the plant in 3 letters // * // PARPLT // 0
  integer, intent(IN)                           :: P_coderetflo  !< // PARAMETER // option slowness action of water stress before the stage DRP: yes  (1), no (2) // code 1/2 // PARPLT // 0
  integer, intent(IN)                           :: P_codetemp  !< // PARAMETER // option calculation mode of heat time for the plant : with air temperature (1)  or crop temperature (2) // code 1/2 // PARPLT // 0
  real,    intent(IN)                           :: coeflev  
  real,    intent(IN)                           :: cu_min  
  real,    intent(IN)                           :: cu_veille  
  real,    intent(IN)                           :: densite      !< // OUTPUT // Actual sowing density // plants.m-2
  real,    intent(IN)                           :: densiteger  
  real,    intent(IN)                           :: densitelev  
  real,    intent(IN)                           :: P_elmax  !< // PARAMETER // Maximum elongation of the coleoptile in darkness condition // cm // PARPLT // 1
  real,    intent(IN)                           :: innlai      !< // OUTPUT // Index of nitrogen stress active on leaf growth // P_innmin to 1
  real,    intent(IN)                           :: P_julvernal  !< // PARAMETER // julian day (between 1 and 365) accounting for the beginning of vernalisation for perennial crops // julian day // PARPLT // 1
  real,    intent(IN)                           :: P_jvc  !< // PARAMETER // Number of vernalizing days // day // PARPLT // 1
  real,    intent(IN)                           :: P_jvcmini  !< // PARAMETER // Minimum number of vernalising days  // day // PARPLT // 1
  integer, intent(IN)                           :: P_nbjgerlim  !< // PARAMETER // Threshold number of day after grain imbibition without germination lack // days // PARPLT // 1
  integer, intent(IN)                           :: ndrpobs  
  integer, intent(IN)                           :: P_nlevlim1  !< // PARAMETER // number of days after germination decreasing the emerged plants if emergence has not occur // days // PARPLT // 1
  integer, intent(IN)                           :: P_nlevlim2  !< // PARAMETER // number of days after germination after which the emerged plants are null // days // PARPLT // 1
  integer, intent(IN)                           :: nlevobs  
  integer, intent(IN)                           :: nplt  
  logical, intent(IN)                           :: onarretesomcourdrp  
  real,    intent(IN)                           :: P_phobase  !< // PARAMETER // Base photoperiod  // hours // PARPLT // 1
  real,    intent(IN)                           :: P_phosat  !< // PARAMETER // saturating photoperiod // hours // PARPLT // 1
  real,    intent(IN)                           :: P_potgermi  !< // PARAMETER // humidity threshold from which seed humectation occurs, expressed in soil water potential  // Mpa // PARPLT // 1
  real,    intent(IN)                           :: P_propjgermin  !< // PARAMETER // minimal proportion of the duration P_nbjgerlim when the temperature is higher than the temperature threshold P_Tdmax  // % // PARPLT // 1
  real,    intent(IN)                           :: P_q10  !< // PARAMETER // P_Q10 used for the dormancy break calculation  // SD // PARPLT // 1
  real,    intent(IN)                           :: P_sensiphot  !< // PARAMETER //  photoperiod sensitivity (1=insensitive) // SD // PARPLT // 1
  real,    intent(IN)                           :: P_sensrsec  !< // PARAMETER // root sensitivity to drought (1=insensitive) // SD // PARPLT // 1
  real,    intent(IN)                           :: somelong  
  real,    intent(IN)                           :: somger  
  real,    intent(IN)                           :: P_stdordebour  !< // PARAMETER // phasic duration between the dormancy break and the bud break  // degree.days // PARPLT // 1
  real,    intent(IN)                           :: P_stpltger  !< // PARAMETER // Sum of development allowing germination // degree.days // PARPLT // 1
  real,    intent(IN)                           :: P_stressdev  !< // PARAMETER // maximum phasic delay allowed  due to stresses  // SD // PARPLT // 1
  real,    intent(IN)                           :: P_tcxstop  !< // PARAMETER // threshold temperature beyond which the foliar growth stops // degree C // PARPLT // 1
  real,    intent(IN)                           :: P_tdmax  !< // PARAMETER // Maximum threshold temperature for development // degree C // PARPLT // 1
  real,    intent(IN)                           :: P_tdmaxdeb  !< // PARAMETER // maximal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
  real,    intent(IN)                           :: P_tdmin  !< // PARAMETER // Minimum threshold temperature for development // degree C // PARPLT // 1
  real,    intent(IN)                           :: P_tdmindeb  !< // PARAMETER // minimal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
  real,    intent(IN)                           :: P_tfroid  !< // PARAMETER // optimal temperature for vernalisation // degree C // PARPLT // 1
  real,    intent(IN)                           :: P_tgmin  !< // PARAMETER // Minimum threshold temperature used in emergence stage // degree C // PARPLT // 1
  real,    intent(IN)                           :: turfac      !< // OUTPUT // Index of turgescence water stress  // 0-1
  real,    intent(IN)                           :: P_vigueurbat  !< // PARAMETER // indicator of plant vigor allowing to emerge through the crust  // between 0 and 1 // PARPLT // 1
  integer, intent(IN)                           :: P_codefente  !< // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0
  real,    intent(IN)                           :: P_mulchbat  !< // PARAMETER // mulch depth from which a crust occurs // cm // PARSOL // 1
  real,    intent(IN)                           :: P_pluiebat  !< // PARAMETER // minimal rain quantity for the crust occurrence // mm day-1 // PARSOL // 1
  integer, intent(IN)                           :: P_culturean  !< // PARAMETER // crop status 1 = over 1 calendar year ,other than 1  = on two calendar years (winter crop in northern hemisphere) // code 0/1 // P_USM/USMXML // 0
  integer, intent(IN)                           :: nbCouches  
  real,    intent(IN), dimension(0:nbCouches)   :: dacouche  
  real,    intent(IN), dimension(nbCouches)     :: hucc  
  real,    intent(IN), dimension(nbCouches)     :: humin  
  real,    intent(IN), dimension(nbCouches)     :: hur  
  integer, intent(IN)                           :: jjul  
  integer, intent(IN)                           :: n  
  integer, intent(IN)                           :: nbjanrec  
  integer, intent(IN)                           :: nbjsemis  
  integer, intent(IN)                           :: numcult  
  real,    intent(IN)                           :: tairveille      !< // OUTPUT // Mean air temperature the previous day // degree C
  real,    intent(IN)                           :: tcult      !< // OUTPUT // Crop surface temperature (daily average) // degree C
  real,    intent(IN), dimension(0:nbCouches)   :: tsol  
  real,    intent(IN)                           :: xmlch1      !< // OUTPUT // Thickness of mulch created by evaporation from the soil // cm
  integer, intent(IN)                           :: P_codepluiepoquet  !< // PARAMETER // option to replace rainfall by irrigation at poquet depth in the case of poquet sowing // code 1/2 // PARAMV6 // 0
  integer, intent(IN)                           :: P_codetempfauche  !< // PARAMETER // option of the reference temperature to compute cutting sum of temperatures : upvt (1), udevair (2) // code 1/2 // PARAMV6 // 0
  logical, intent(IN)                           :: humectation  
  integer, intent(IN)                           :: nbjhumec  
  real,    intent(IN)                           :: pluiesemis  
  real,    intent(IN)                           :: somtemphumec  
  integer, intent(IN)                           :: P_codeindetermin  !< // PARAMETER // option of  simulation of the leaf growth and fruit growth : indeterminate (2) or determinate (1) // code 1/2 // PARPLT // 0
  integer, intent(IN)                           :: P_codelaitr  !< // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0
  integer, intent(IN)                           :: P_codlainet  !< // PARAMETER // option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
  real,    intent(IN)                           :: P_dureefruit  !< // PARAMETER // total growth period of a fruit at the setting stage to the physiological maturity // degree.days // PARPLT // 1
  integer, intent(IN)                           :: namfobs  
  integer, intent(IN)                           :: P_nbcueille  !< // PARAMETER // number of fruit harvestings // code 1/2 // PARTEC // 0
!  integer, intent(IN)                           :: P_nboite  !< // PARAMETER // "Number of  box  or  age class  of fruits for the fruit growth for the indeterminate crops " // SD // PARPLT // 1
  integer, intent(IN)                           :: nfloobs  
  integer, intent(IN)                           :: nlanobs  
  integer, intent(IN)                           :: nlaxobs  
  integer, intent(IN)                           :: nmatobs  
  integer, intent(IN)                           :: nrecobs  
  integer, intent(IN)                           :: nsenobs  
  real,    intent(IN)                           :: P_stdrpnou  !< // PARAMETER // Sum of development units between the stages DRP and NOU (end of  setting) // degree.days // PARPLT // 1
  real,    intent(IN)                           :: upobs  
! PB - 03/08/2010 - Pour les modifs ML/SYL/DR de juin09
  integer, intent(IN)                           :: P_codemontaison  !< // PARAMETER // code to stop the reserve limitation from the stem elongation // code 1/2 // PARAMV6 // 0
  logical, intent(IN)                           :: sioncoupe  

  ! DR 06/03/2015 ajout pour les enchainement annuel de la vigne
  integer,  intent(IN)    :: P_idebdorm, P_iwater


  real,    intent(INOUT)                        :: caljvc  
  real,    intent(INOUT)                        :: cu  
  real,    intent(INOUT), dimension(0:2)        :: demande      !< // OUTPUT // Daily nitrogen need of the plant   // kgN.ha-1.j-1
  logical, intent(INOUT)                        :: etatvernal  
  real,    intent(INOUT), dimension(0:2)        :: hauteur      !< // OUTPUT // Height of canopy // m
  real,    intent(INOUT), dimension(0:2)        :: mafrais      !< // OUTPUT // Aboveground fresh matter // t.ha-1
  real,    intent(INOUT), dimension(0:2)        :: mafraisfeuille  
  real,    intent(INOUT), dimension(0:2)        :: mafraisrec  
  real,    intent(INOUT), dimension(0:2)        :: mafraisres  
  real,    intent(INOUT), dimension(0:2)        :: mafraistige  
  real,    intent(INOUT), dimension(0:2)        :: masec      !< // OUTPUT // Aboveground dry matter  // t.ha-1
  integer, intent(INOUT)                        :: namf  
  integer, intent(INOUT)                        :: ndebdorm  
  integer, intent(INOUT)                        :: ndrp  
  integer, intent(INOUT)                        :: nfindorm  
  integer, intent(INOUT)                        :: nflo  
  integer, intent(INOUT)                        :: nger  
  integer, intent(INOUT)                        :: nlan  
  integer, intent(INOUT)                        :: nlev  
  integer, intent(INOUT)                        :: nrec  
  integer, intent(INOUT)                        :: nrecbutoir  
  real,    intent(INOUT), dimension(0:2)        :: pdsfruitfrais      !< // OUTPUT // Total weight of fresh fruits // g m-2
  real,    intent(OUT)                          :: rfpi      !< // OUTPUT // Slowing effect of the photoperiod on plant development  // 0-1
  real,    intent(OUT)                          :: rfvi      !< // OUTPUT // Slowing effect of the vernalization on plant development // 0-1
  real,    intent(INOUT)                        :: somcour      !< // OUTPUT // Cumulated units of development between two stages // degree.days
  real,    intent(INOUT)                        :: somcourdrp  
  real,    intent(INOUT)                        :: somcourfauche  
  real,    intent(INOUT)                        :: somcourutp  
  real,    intent(INOUT)                        :: somtemp      !< // OUTPUT // Sum of temperatures // degree C.j
  real,    intent(OUT)                          :: stpltlev  
  ! DR 18/07/2012 je rajoute stpltger
  real,    intent(OUT)                          :: stpltger

  real,    intent(OUT)                          :: tdevelop  
  real,    intent(OUT)                          :: udevair      !< // OUTPUT // Effective temperature for the development, computed with TAIR // degree.days
  real,    intent(OUT)                          :: udevcult      !< // OUTPUT // Effective temperature for the development, computed with TCULT // degree.days
  real,    intent(INOUT)                        :: upvt      !< // OUTPUT // Daily development unit  // degree.days
  real,    intent(INOUT)                        :: utp  
  real,    intent(INOUT)                        :: zrac      !< // OUTPUT // Depth reached by root system // cm
  integer, intent(INOUT)                        :: maxwth  
  integer, intent(OUT)                          :: group  
  integer, intent(INOUT)                        :: ndebdes  
  real,    intent(INOUT)                        :: nfruit      !< // OUTPUT // Number of fruits in box 5 // nb fruits
  integer, intent(INOUT)                        :: nlax  
  integer, intent(INOUT)                        :: nmat  
  integer, intent(INOUT)                        :: nnou  
  integer, intent(INOUT)                        :: nsen  
  real,    intent(INOUT)                        :: P_stamflax  !< // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1
  real,    intent(INOUT)                        :: P_stdrpdes  !< // PARAMETER // phasic duration between the DRP stage and the beginning of the water fruit dynamics  // degree.days // PARPLT // 1
  real,    intent(INOUT)                        :: P_stdrpmat  !< // PARAMETER // Sum of development units between the stages DRP and MAT // degree.days // PARPLT // 1
  real,    intent(OUT)                          :: stdrpsen  
  real,    intent(OUT)                          :: P_stflodrp  !< // PARAMETER // phasic duration between FLO and DRP (only for indication) // degrés.jours // PARPLT // 1
  real,    intent(INOUT)                        :: P_stlaxsen  !< // PARAMETER // Sum of development units between the stages LAX and SEN // degree.days // PARPLT // 1
  real,    intent(INOUT)                        :: P_stlevamf  !< // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1
  real,    intent(INOUT)                        :: P_stlevdrp  !< // PARAMETER // Sum of development units between the stages LEV and DRP // degree.days // PARPLT // 1
  real,    intent(INOUT)                        :: stlevflo  
  real,    intent(OUT)                          :: stmatrec  
  real,    intent(INOUT)                        :: P_stsenlan  !< // PARAMETER // Sum of development units between the stages SEN et LAN // degree.days // PARPLT // 1
  real,    intent(OUT)                          :: upvtutil  

  ! pour recolte
  integer, intent(IN)                           :: P_codrecolte  !< // PARAMETER // harvest mode : all the plant (1) or just the fruits (2) // code 1/2 // PARTEC // 0
  real,    intent(IN)                           :: h2orec      !< // OUTPUT // Water content of harvested organs // %
  real,    intent(IN)                           :: P_sucrerec  !< // PARAMETER // minimal sugar rate at harvest // g sucre g-1 MF // PARTEC // 1
  real,    intent(IN)                           :: P_CNgrainrec  !< // PARAMETER // minimal grain nitrogen content for harvest  // 0-1 // PARTEC // 1
  real,    intent(IN)                           :: P_huilerec  !< // PARAMETER // minimal oil content allowed for harvest // g huile g-1 MF // PARTEC // 1
  real,    intent(IN)                           :: sucre      !< // OUTPUT // Sugar content of fresh harvested organs // % (of fresh weight)
  real,    intent(IN)                           :: huile      !< // OUTPUT // Oil content of fresh harvested organs // % (of fresh weight)
  real,    intent(IN)                           :: teaugrain  
!  real,    intent(IN)                           :: sucreder
!  real,    intent(IN)                           :: huileder
  real,    intent(IN)                           :: P_h2ofrvert  !< // PARAMETER // water content of fruits before the beginning of hydrous evolution (DEBDESHYD) // g water g-1 MF // PARPLT // 1
  integer, intent(IN)                           :: P_codeaumin  !< // PARAMETER // harvest as a function of grain/fruit water content // code 1/2 // PARTEC // 0
  real,    intent(IN)                           :: P_h2ograinmin  !< // PARAMETER // minimal water content allowed at harvest // g eau g-1 MF // PARTEC // 1
  real,    intent(IN)                           :: P_h2ograinmax  !< // PARAMETER // maximal water content allowed at harvest // g water g-1 MF // PARTEC // 1
  real,    intent(IN)                           :: P_deshydbase  !< // PARAMETER // phenological rate of evolution of fruit water content (>0 or <0) // g water.g MF-1.degree C-1 // PARPLT // 1
  real,    intent(IN)                           :: CNgrain      !< // OUTPUT // Nitrogen concentration of grains  // %
  integer, intent(IN)                           :: P_cadencerec  !< // PARAMETER // number of days between two harvests // day // PARTEC // 1

  integer, intent(INOUT)                        :: jdepuisrec  
  real,    intent(INOUT)                        :: pdsfruit      !< // OUTPUT // Weight of fruits in box 3 // g m-2
  integer, intent(INOUT)                        :: nbrecolte  
  integer, intent(OUT)                          :: nrecint  
!  real,    intent(OUT),   dimension(0:2,0:20)   :: rdtint
  real,    intent(OUT)                          :: rdtint  
  real,    intent(OUT)                          :: teauint  
  real,    intent(OUT)                          :: nbfrint  

! PB - Pour les modifs SYL/DR/ML de juin09
  integer, intent(INOUT)                        :: onestan2  
  real,    intent(INOUT)                        :: somcourmont      !< // OUTPUT // Cumulatied units of development from the start of vernalisation // degree.days
  integer, intent(INOUT)                        :: nmontaison  

end subroutine develop2

subroutine Stics_Develop_bfroid2(jjul,n,P_tfroid,P_ampfroid,P_julvernal,P_jvc,P_jvcmini,P_codeperenne,nger,namf,  & ! IN
                                 numcult,nbjsemis,tdev,P_codemontaison,P_culturean,P_codeinitprec,nbjanrec,       & ! IN
                                 nrecbutoir,rfvi,maxwth,etatvernal,caljvc,onestan2)                                 ! OUT & INOUT
implicit none
!: ARGUMENTS
! IN
  integer, intent(IN)    :: jjul  
  integer, intent(IN)    :: n  
  real,    intent(IN)    :: P_tfroid   !< // PARAMETER optimal temperature for vernalisation // degree C // PARPLT // 1
  real,    intent(IN)    :: P_ampfroid   !< // PARAMETER semi thermal amplitude thermique for vernalising effect // degree C // PARPLT // 1
  real,    intent(IN)    :: P_julvernal   !< // PARAMETER julian day (between 1 and 365) accounting for the beginning of vernalisation for perennial crops // julian day // PARPLT // 1
  real,    intent(IN)    :: P_jvc   !< // PARAMETER Number of vernalizing days // day // PARPLT // 1
  real,    intent(IN)    :: P_jvcmini   !< // PARAMETER Minimum number of vernalising days  // day // PARPLT // 1
  integer, intent(IN)    :: P_codeperenne   !< // PARAMETER option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0
  integer, intent(IN)    :: nger  
  integer, intent(IN)    :: namf  
  integer, intent(IN)    :: numcult  
  integer, intent(IN)    :: nbjsemis  
  real,    intent(IN)    :: tdev  
  integer, intent(IN)    :: P_codemontaison   !< // PARAMETER code to stop the reserve limitation from the stem elongation // code 1/2 // PARAMV6 // 0
  integer, intent(IN)    :: P_culturean   !< // PARAMETER crop status 1 = over 1 calendar year ,other than 1  = on two calendar years (winter crop in northern hemisphere) // code 0/1 // P_USM/USMXML // 0
  integer, intent(IN)    :: P_codeinitprec   !< // PARAMETER reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0
  integer, intent(IN)    :: nbjanrec  

! OUT
  integer, intent(OUT)   :: nrecbutoir  
  real,    intent(OUT)   :: rfvi      !< // OUTPUT // Slowing effect of the vernalization on plant development // 0-1

! INOUT
  integer, intent(INOUT) :: maxwth  
  logical, intent(INOUT) :: etatvernal  
  real,    intent(INOUT) :: caljvc  
  integer, intent(INOUT) :: onestan2  
end subroutine Stics_Develop_bfroid2


subroutine Stics_Develop_bfroid3(P_codedormance,cu_min,cu_veille,n,P_jvc,P_q10,tmin,tmax,thor, &  !IN
                                 etatvernal,cu,rfvi,ndebdorm,nfindorm,nlev,P_idebdorm, P_iwater, nbjsemis,numcult)

USE Besoins_en_froid

implicit none

  integer, intent(IN)    :: P_codedormance  !< // PARAMETER // option of calculation of dormancy and chilling requirement // code 1/2 // PARPLT // 0
  real,    intent(IN)    :: cu_min  
  real,    intent(IN)    :: cu_veille  
  integer, intent(IN)    :: n  
  real,    intent(IN)    :: P_jvc  !< // PARAMETER // Number of vernalizing days // day // PARPLT // 1
  real,    intent(IN)    :: P_q10  !< // PARAMETER // P_Q10 used for the dormancy break calculation  // SD // PARPLT // 1
  real,    intent(IN)    :: tmin      !< // OUTPUT // Minimum active temperature of air // degree C
  real,    intent(IN)    :: tmax      !< // OUTPUT // Maximum active temperature of air // degree C
  real,    intent(IN)    :: thor(24)  
  ! DR 06/03/2015 ajout pour les enchainement annuel de la vigne
  integer,  intent(IN)    :: P_idebdorm, P_iwater, nbjsemis,numcult

  logical, intent(INOUT) :: etatvernal  
  real,    intent(INOUT) :: cu  
  real,    intent(OUT)   :: rfvi      !< // OUTPUT // Slowing effect of the vernalization on plant development // 0-1
  integer, intent(INOUT) :: ndebdorm  
  integer, intent(INOUT) :: nfindorm  
  integer, intent(INOUT) :: nlev  
!  integer,  intent(IN)    :: P_idebdorm, nbjsemis,P_iwater

end subroutine Stics_Develop_bfroid3

! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> Calculation of the decision to harvest
!> - Stics book paragraphe 6.1.4.d., page 98
!>
!! The decision to harvest can be taken as a function of crop maturity status but it can also rely on other considerations such as soil water status, sanitary or even economic considerations.
!!
!! The crop maturity-dependent harvest date depends on one of the following criteria:
!> - physiological maturity (end of growth-development period)
!> - maximum water content in fruit which exhibit dehydration dynamics (h2ograinmax) or minimum water content in fruit that exhibit hydration dynamics (h2ograinmin)
!!   from the onset of water dynamics (IDEBDES stage)
!> - minimum sugar content in fruit (sucrerec)
!> - minimum nitrogen content in fruit (cngrainrec)
!> - minimum oil content in fruit (huilrec)
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine recolte(n,ndrp,P_codrecolte,nmat,P_variete,P_nbcueille,h2orec,P_sucrerec,P_CNgrainrec,        & ! IN
!   P_huilerec,sucre,huile,teaugrain,sucreder,huileder,P_h2ofrvert,P_codeaumin,    & ! 23/07/2012 hileder et sucreder non utilisés
                    P_huilerec,sucre,huile,teaugrain,P_h2ofrvert,P_codeaumin,      & ! 23/07/2012 hileder et sucreder non utilisés
                    P_h2ograinmin,P_h2ograinmax,P_deshydbase,somcourdrp,P_stdrpmat,CNgrain,P_cadencerec,  &
                    nrec,jdepuisrec,stmatrec,group,pdsfruit,nrecint,rdtint,teauint,nbfrint,     & ! INOUT
                    nfruit,nbrecolte)

  integer, intent(IN)    :: n  
  integer, intent(IN)    :: ndrp  
  integer, intent(IN)    :: P_codrecolte  !< // PARAMETER // harvest mode : all the plant (1) or just the fruits (2) // code 1/2 // PARTEC // 0
  integer, intent(IN)    :: nmat  
  integer, intent(IN)    :: P_variete  !< // PARAMETER // variety number in the technical file // SD // PARTEC // 1
  integer, intent(IN)    :: P_nbcueille  !< // PARAMETER // number of fruit harvestings // code 1/2 // PARTEC // 0
  real,    intent(IN)    :: h2orec      !< // OUTPUT // Water content of harvested organs // %
  real,    intent(IN)    :: P_sucrerec  !< // PARAMETER // minimal sugar rate at harvest // g sucre g-1 MF // PARTEC // 1
  real,    intent(IN)    :: P_CNgrainrec  !< // PARAMETER // minimal grain nitrogen content for harvest  // 0-1 // PARTEC // 1
  real,    intent(IN)    :: P_huilerec  !< // PARAMETER // minimal oil content allowed for harvest // g huile g-1 MF // PARTEC // 1
  real,    intent(IN)    :: sucre      !< // OUTPUT // Sugar content of fresh harvested organs // % (of fresh weight)
  real,    intent(IN)    :: huile      !< // OUTPUT // Oil content of fresh harvested organs // % (of fresh weight)
  real,    intent(IN)    :: teaugrain  
!  real,    intent(IN)    :: sucreder
!  real,    intent(IN)    :: huileder
  real,    intent(IN)    :: P_h2ofrvert  !< // PARAMETER // water content of fruits before the beginning of hydrous evolution (DEBDESHYD) // g water g-1 MF // PARPLT // 1
  integer, intent(IN)    :: P_codeaumin  !< // PARAMETER // harvest as a function of grain/fruit water content // code 1/2 // PARTEC // 0
  real,    intent(IN)    :: P_h2ograinmin  !< // PARAMETER // minimal water content allowed at harvest // g eau g-1 MF // PARTEC // 1
  real,    intent(IN)    :: P_deshydbase  !< // PARAMETER // phenological rate of evolution of fruit water content (>0 or <0) // g water.g MF-1.degree C-1 // PARPLT // 1
  real,    intent(IN)    :: somcourdrp  
  real,    intent(IN)    :: P_stdrpmat  !< // PARAMETER // Sum of development units between the stages DRP and MAT // degree.days // PARPLT // 1
  real,    intent(IN)    :: P_h2ograinmax  !< // PARAMETER // maximal water content allowed at harvest // g water g-1 MF // PARTEC // 1
  real,    intent(IN)    :: CNgrain      !< // OUTPUT // Nitrogen concentration of grains  // %
  integer, intent(IN)    :: P_cadencerec  !< // PARAMETER // number of days between two harvests // day // PARTEC // 1

  integer, intent(INOUT) :: nrec  
  integer, intent(INOUT) :: jdepuisrec  
  real,    intent(OUT)   :: stmatrec  
  integer, intent(OUT)   :: group  
  real,    intent(INOUT) :: pdsfruit      !< // OUTPUT // Weight of fruits in box 3 // g m-2
  integer, intent(OUT)   :: nrecint  
!  real,    intent(OUT),  dimension(0:2,0:20)        :: rdtint
  real,    intent(OUT)   :: rdtint  

  real,    intent(OUT)   :: teauint  
  real,    intent(OUT)   :: nbfrint  
  real,    intent(INOUT) :: nfruit      !< // OUTPUT // Number of fruits in box 5 // nb fruits
  integer, intent(INOUT) :: nbrecolte  

end subroutine recolte

! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - This module estimates the soil crusting.
!> - Stics book paragraphe 2.2.1.d, page 24-27
!>
!! In the particular case of loamy soils, a crust may occur after sowing, creating a physical obstacle to emergence (Duval and Boiffin, 1990). In addition to
!! the textural characteristics of the surface soil layer, the development of such a crust depends on soil fragmentation following seedbed preparation and on
!! the weather at the time. Indeed, post-sowing rainfall may destroy soil fragments and then drought renders this layer almost impenetrable for the plantlets,
!! since the resistance to emergence depends on the weather through its evaporative demand and on the force exerted by the plantlet.
!! The formalisation of these processes in STICS relies partly on the work of Durr et al. (2001). Surface crusting is assumed to occur only after sowing once
!! a certain amount of rainfall (soil-dependent parameter pluiebat) has occurred. The crust is assumed to be dry when the natural mulch depth
!! (xmlch1: variable calculated from the soil evaporation formulations, see the module solnu.f90) is greater than the threshold parameter mulchbat,
!! in which case xmlch1 is considered as the thickness of the crusted layer.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
real function battance(n,nplt,nrec,P_codeperenne,pluiesemis,trr,P_pluiebat,P_mulchbat,xmlch1,elong,P_profsem)

!: ARGUMENTS
! IN
  integer, intent(IN)   :: n  
  integer, intent(IN)   :: nplt  
  integer, intent(IN)   :: P_codeperenne  !> // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0
  integer, intent(IN)   :: nrec  
  real,    intent(IN)   :: trr      !< // OUTPUT // Rainfall  // mm.day-1
  real,    intent(IN)   :: P_pluiebat  !> // PARAMETER // minimal rain quantity for the crust occurrence // mm day-1 // PARSOL // 1
  real,    intent(IN)   :: elong  
  real,    intent(IN)   :: P_profsem  !> // PARAMETER // Sowing depth // cm // PARTEC // 1
  real,    intent(IN)   :: P_mulchbat  !> // PARAMETER // mulch depth from which a crust occurs // cm // PARSOL // 1
  real,    intent(IN)   :: xmlch1      !< // OUTPUT // Thickness of mulch created by evaporation from the soil // cm
! OUT
  real,    intent(OUT)  :: pluiesemis  

end function battance

!! ****************************************************************
!> Calcul de la date de debourrement. Auteur : I. Garcia de Cortazar
!> - Programmation:
!> - programme specifique vigne Inaki pour le calcul de la date
!! de debourrement avec des sommes de températures horaires
!< ****************************************************************
subroutine debour(P_codegdhdeb,P_codetemp,tmax,tmin,tmin_demain,P_tdmindeb,P_tdmaxdeb,rfvi,rfpi, &  ! IN
                  upvt,udevair,udevcult)                                                            ! OUT

USE Divers, only: calcul_TemperaturesHoraires,calcul_GDH

implicit none

!: ARGUMENTS
! IN
  integer, intent(IN)               :: P_codegdhdeb       !< // PARAMETER // option of calculation of the bud break date in hourly or daily growing degrees  // code 1/2 // PARPLT // 0
  integer, intent(IN)               :: P_codetemp         !< // PARAMETER // option calculation mode of heat time for the plant : with air temperature (1)  or crop temperature (2) // code 1/2 // PARPLT // 0
  real,    intent(IN)               :: tmax             !<  	  // OUTPUT // Maximum active temperature of air // degree C
  real,    intent(IN)               :: tmin             !<  	  // OUTPUT // Minimum active temperature of air // degree C
  real,    intent(IN)               :: tmin_demain      !< Minimun active temperature of the day before
  real,    intent(IN)               :: P_tdmindeb         !< // PARAMETER // minimal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
  real,    intent(IN)               :: P_tdmaxdeb         !< // PARAMETER // maximal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
  real,    intent(IN)               :: rfvi             !< // OUTPUT // Slowing effect of the vernalization on plant development // 0-1
  real,    intent(IN)               :: rfpi             !< // OUTPUT // Slowing effect of the photoperiod on plant development  // 0-1
! OUT
  real,    intent(OUT)              :: upvt             !< // OUTPUT // Daily development unit  // degree.days
  real,    intent(OUT)              :: udevair          !< // OUTPUT // Effective temperature for the development, computed with TAIR // degree.days
  real,    intent(OUT)              :: udevcult         !< // OUTPUT // Effective temperature for the development, computed with TCULT // degree.days

end subroutine debour

!! ****************************************************************
!> Calculation of the emergence. Authors : C. Durr and G. Richard.
!> - Programmation: N. Brisson.
!> - last modification 20/02/07 : l'humectation de la graine se fait en fonction d'un potentiel : humecgraine en MPa.
!> - Stics book paragraphe 2.2.2, page 21-27
!!
!> In STICS, the emergence phase is broken down into three subphases: seed imbibition, followed by germination and lastly, shoot elongation.
!! The soil physical conditions influence not only the duration of emergence but also the number of emerged plants, in particular in dry conditions or when
!! there is a surface crust.
!> - Moistening :
!!   Seed moistening can be regarded as a passive process starting at a species-dependent water potential prevailing in the seed bed (potgermi in MPa).
!!   The relationship from Clapp and Hornberger (1978), parameterized by the characteristic soil water contents of field capacity and wilting point, was used
!!   to convert potgermi into water content (in function "humpotsol", described in the module Divers_develop.f90). Once the seed is moistened, it has a limited
!!   number of days of autotrophy  (nbjgrauto) due to its reserves. This number has a species-dependent component (nbjgerlim) but also a thermal one, since
!!   it is thought that at low temperature (i.e the average soil temperature in the seed bed, from the beginning of moistening), respiration processes and
!!   the consumption of reserves are slower (the minimum at high temperature is propjgermin x nbjgermin). When the temperature is lower than the
!!   germination base temperature, tgmin, then the day number is maximal (nbjgerlim).
!> - Germination :
!!   Germination is achieved when the growing degree-days from planting in the seed bed (somger) reaches a given threshold (stpltger), with a condition as to
!!   the dryness of the soil. Soil moisture in the seedbed influences germination through the "humirac" function (described in the module Divers.f90).
!!   If the seedbed dries out, it may delay germination significantly. This does not impair grain viability as long as the grain has not already imbibed water.
!!   If however the soil water content has been high enough to allow grain moistening, grain viability is reduced. To account for this effect, we relied on
!!   Bradford’s (1990, 2002) work showing that too long a time for germination after moistening reduces the germination rate if the number of days of
!!   moistening (nbjhumec) is higher than a plant- and temperature-dependent threshold duration (nbjgrauto). It is assumed that germination occurs
!!   (IGER being the germination day) but at a reduced plant density (ratio between density of germinated plants, densiteger, to sowing density, densitesem)
!!   proportional to the thermal time deficit.
!> - Subsoil plantlet growth :
!!   Germination initiates the growth of the root and then of the shoot. The growth rate of the shoot is assumed to be a logistic function of soil degree-days
!!   that may slow down with unsuitable soil moisture (humirac). Emergence occurs when elongation (elong) is greater than sowing depth (profsem).
!> - Influence of soil crusting on emergence :
!!   The density reduction law is specific to the crusting phenomenon but analogous to the other constraint law (water content and temperature-dependent)
!!   with a minimum threshold corresponding to the vigueurbat parameter : if vigueurbat is greater than 0, which means that when the soil is crusted a
!!   proportion of plants succeed in emerging, the crusting coeflev function is less effective than the water content and temperature-dependent coeflev function.
!!   The combination of both relationships is made dynamically by calculating the daily derivatives of both laws: if the current day is a "battance=0" day
!!   (battance is calculated in the module Stics_Battance.f90) the density reduction is done according to the crusting coeflev law.
!>
!! For woody plants which have perennial dormancy, ILEV stage corresponds to the budbreak stage (for this calculation hourly temperatures are reconstituted
!! in the module Stics_Debour.f90).
!------------------------------
subroutine levee(P_codeperenne,P_codebfroid,nlevobs,nger,P_codegdhdeb,P_codetemp,P_codegermin,P_codefente,P_codepluiepoquet,   & ! IN
                 P_codehypo, P_nbjgerlim,tmin,tmin_demain,tmax,n,nplt,nrec,P_nlevlim1,P_nlevlim2,P_tdmindeb,P_tdmaxdeb,rfvi,   &
                 rfpi,P_profsem, P_stdordebour,P_tgmin,P_stpltger,P_sensrsec,P_psihucc, P_psihumin,P_potgermi,P_tdmax,         &
                 P_propjgermin, P_densitesem,pluiesemis, P_pluiebat,P_mulchbat,xmlch1,P_vigueurbat,P_celong,P_belong,P_elmax,  &
!                 nbCouches,tsol,hur,humin,hucc,trr,dacouche,jjul,                                                             & !
                 nbCouches,tsol,hur,humin,hucc,trr,dacouche,                                         & ! --! DR 20/07/2012 jjulne sert pas
                 udevair,udevcult,upvt,densiteger,densite,coeflev,densitelev,zrac,                                        & ! OUT
                 somelong,somger,nlev,humectation,nbjhumec,somtemphumec,somcour)                                            ! INOUT


USE Divers, only: F_humirac

implicit none


! IN
  integer, intent(IN) :: P_codeperenne  !< // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0
  integer, intent(IN) :: P_codebfroid   !< // PARAMETER option of calculation of chilling requirements // code 1/2 // PARPLT // 0
  integer, intent(IN) :: nlevobs  
  integer, intent(IN) :: P_codegdhdeb   !< // PARAMETER option of calculation of the bud break date in hourly or daily growing degrees  // code 1/2 // PARPLT // 0
  integer, intent(IN) :: P_codetemp   !< // PARAMETER option calculation mode of heat time for the plant : with air temperature (1)  or crop temperature (2) // code 1/2 // PARPLT // 0
  integer, intent(IN) :: P_codegermin   !< // PARAMETER option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2) // code 1/2 // PARPLT // 0
  integer, intent(IN) :: P_codefente   !< // PARAMETER option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0
  integer, intent(IN) :: P_codepluiepoquet   !< // PARAMETER option to replace rainfall by irrigation at poquet depth in the case of poquet sowing // code 1/2 // PARAMV6 // 0
  integer, intent(IN) :: P_codehypo   !< // PARAMETER option of simulation of a  phase of hypocotyl growth (1) or planting of plantlets (2) // code 1/2 // PARPLT // 0
  integer, intent(IN) :: P_nbjgerlim   !< // PARAMETER Threshold number of day after grain imbibition without germination lack // days // PARPLT // 1
  real,    intent(IN) :: tmin     !< // OUTPUT // Minimum active temperature of air // degree C
  real,    intent(IN) :: tmin_demain  
  real,    intent(IN) :: tmax      !< // OUTPUT // Maximum active temperature of air // degree C
  integer, intent(IN) :: n  
  integer, intent(IN) :: nplt  
  integer, intent(IN) :: nrec  
  integer, intent(IN) :: P_nlevlim1   !< // PARAMETER number of days after germination decreasing the emerged plants if emergence has not occur // days // PARPLT // 1
  integer, intent(IN) :: P_nlevlim2   !< // PARAMETER number of days after germination after which the emerged plants are null // days // PARPLT // 1
  real,    intent(IN) :: P_tdmindeb   !< // PARAMETER minimal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
  real,    intent(IN) :: P_tdmaxdeb   !< // PARAMETER maximal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
  real,    intent(IN) :: rfvi      !< // OUTPUT // Slowing effect of the vernalization on plant development // 0-1
  real,    intent(IN) :: rfpi      !< // OUTPUT // Slowing effect of the photoperiod on plant development  // 0-1
  real,    intent(IN) :: P_profsem   !< // PARAMETER Sowing depth // cm // PARTEC // 1
  real,    intent(IN) :: P_stdordebour   !< // PARAMETER phasic duration between the dormancy break and the bud break  // degree.days // PARPLT // 1
  real,    intent(IN) :: P_tgmin   !< // PARAMETER Minimum threshold temperature used in emergence stage // degree C // PARPLT // 1
  real,    intent(IN) :: P_stpltger   !< // PARAMETER Sum of development allowing germination // degree.days // PARPLT // 1
  real,    intent(IN) :: P_sensrsec   !< // PARAMETER root sensitivity to drought (1=insensitive) // SD // PARPLT // 1
  real,    intent(IN) :: P_psihucc   !< // PARAMETER soil potential corresponding to field capacity  // Mpa // PARAM // 1
  real,    intent(IN) :: P_psihumin   !< // PARAMETER soil potential corresponding to wilting point // Mpa // PARAM // 1
  real,    intent(IN) :: P_potgermi   !< // PARAMETER humidity threshold from which seed humectation occurs, expressed in soil water potential  // Mpa // PARPLT // 1
  real,    intent(IN) :: P_tdmax   !< // PARAMETER Maximum threshold temperature for development // degree C // PARPLT // 1
  real,    intent(IN) :: P_propjgermin   !< // PARAMETER minimal proportion of the duration P_nbjgerlim when the temperature is higher than the temperature threshold P_Tdmax  // % // PARPLT // 1
  real,    intent(IN) :: P_densitesem   !< // PARAMETER Sowing density  // plants.m-2 // PARTEC // 1
  real,    intent(IN) :: pluiesemis  
  real,    intent(IN) :: P_pluiebat   !< // PARAMETER minimal rain quantity for the crust occurrence // mm day-1 // PARSOL // 1
  real,    intent(IN) :: P_mulchbat   !< // PARAMETER mulch depth from which a crust occurs // cm // PARSOL // 1
  real,    intent(IN) :: xmlch1      !< // OUTPUT // Thickness of mulch created by evaporation from the soil // cm
  real,    intent(IN) :: P_vigueurbat   !< // PARAMETER indicator of plant vigor allowing to emerge through the crust  // between 0 and 1 // PARPLT // 1
  real,    intent(IN) :: P_celong   !< // PARAMETER parameter of the subsoil plantlet elongation curve // SD // PARPLT // 1
  real,    intent(IN) :: P_belong   !< // PARAMETER parameter of the curve of coleoptile elongation // degree.days -1 // PARPLT // 1
  real,    intent(IN) :: P_elmax   !< // PARAMETER Maximum elongation of the coleoptile in darkness condition // cm // PARPLT // 1
  integer, intent(IN) :: nbCouches  
  real,    intent(IN) :: tsol(0:nbCouches)  
  real,    intent(IN) :: hur(nbCouches)  
  real,    intent(IN) :: humin(nbCouches)  
  real,    intent(IN) :: hucc(nbCouches)  
  real,    intent(IN) :: trr      !< // OUTPUT // Rainfall  // mm.day-1
  real,    intent(IN) :: dacouche(0:nbCouches)  
!  integer, intent(IN) :: jjul

! INOUT (& OUT ?)
  real,    intent(INOUT) :: udevair      !< // OUTPUT // Effective temperature for the development, computed with TAIR // degree.days
  real,    intent(INOUT) :: udevcult      !< // OUTPUT // Effective temperature for the development, computed with TCULT // degree.days
  real,    intent(INOUT) :: upvt      !< // OUTPUT // Daily development unit  // degree.days
  real,    intent(INOUT) :: densiteger  
  real,    intent(INOUT) :: densite      !< // OUTPUT // Actual sowing density // plants.m-2
  real,    intent(INOUT) :: coeflev  
  real,    intent(INOUT) :: densitelev  
  real,    intent(INOUT) :: zrac      !< // OUTPUT // Depth reached by root system // cm
  real,    intent(INOUT) :: somelong  
  real,    intent(INOUT) :: somger  
  integer, intent(INOUT) :: nlev  
  integer, intent(INOUT) :: nger  
  logical, intent(INOUT) :: humectation  
  integer, intent(INOUT) :: nbjhumec  
  real,    intent(INOUT) :: somtemphumec  
  real,    intent(INOUT) :: somcour      !< // OUTPUT // Cumulated units of development between two stages // degree.days

end subroutine levee

! ****************************************************************
!> Calculation of the number of leaves (nbfeuille) is mainly indicative.
!> - Stics book paragraphe 3.1.5, page 48
!!
!> Its only active role is to define the duration of the plantlet phase when calculating
!! frost risks. Indeed the plantlet stage is calculated as a leaf-number stage (2 or 3). nbfeuille is calculated up to the ILAX stage from the phyllotherm (phyllotherme)
!! (the thermal period separating the emission of two successive leaves) expressed in crop degree.days as  for the phasic development.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine CalculNombreDeFeuilles(P_phyllotherme,nlax,udev,somfeuille,nbfeuille)

  ! ARGUMENTS (IN)
  real,    intent(IN)    :: P_phyllotherme   !< // PARAMETER thermal duration between the apparition of two successive leaves on the main stem // degree C day // PARPLT // 1
  integer, intent(IN)    :: nlax
  real,    intent(IN)    :: udev

  ! ARGUMENTS (INOUT)
  real,    intent(INOUT) :: somfeuille
  integer, intent(INOUT) :: nbfeuille      !< // OUTPUT // Number of leaves on main stem // SD

end subroutine CalculNombreDeFeuilles


! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module calculates daily daylength and photoperiod.
!> - Stics book paragraphe 2.3.2, page 32
!>
!! The current photoperiod (photop) is calculated on the basis of calendar days and latitude using classic astronomical functions (Sellers, 1965).
!! The photoperiod is calculated by assuming that light is perceptible until the sun is at 6° below the horizon, which corresponds to a duration 50 to 70 minutes
!! longer than the strictly defined daylength.
!!
!! Daylength is calculated with a correction for atmospheric refraction equivalent to 50 minutes of a degree.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine photpd(zlat,jday,daylen,photop)

!! ARGUMENTS
  real,    intent(IN)  :: zlat
  integer, intent(IN)  :: jday
  real,    intent(OUT) :: daylen
  real,    intent(OUT) :: photop
end subroutine photpd




! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module deals with foliage regulation by topping.
!> - Stics book paragraphe 6.1.3.a, page 96-97
!>
!! If the plant exhibits indeterminate growth, a trellis system is required, which can be simulated by imposing a maximal height and width: hautmaxtec and largtec.
!! Topping only concerns crops having a row structure and consists in restricting growth in terms of height (hautrogne) and width (largrogne) of the structure.
!! In order to ensure the efficiency of this technique, a minimum topped shoot biomass threshold must be observed (biorognem). The topped biomass and the
!! corresponding LAI are subtracted from the biomass and LAI of the plant. The calculation of this topped LAI (lairognecum) and biomass relies on the foliage
!! density dfol, the specific surface area of biomass, sbv using the variable sla.
!!
!! Topped biomass is recycled in the soil nitrogen balance. Two topping calculations may be employed. With automatic calculation, topping occurs as soon as the
!! plant height exceeds hautrogne+margerogne. The other possible calculation is done at an imposed date, nrogne (first read as "julrogne").
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine rognage(P_codcalrogne,hauteur,largeur,P_hautrogne,P_margerogne,P_largrogne,dfol,P_hautbase,P_interrang,sla,  & !IN
!        P_tigefeuil,P_biorognem,n,nrogne,CNplante,    & !IN DR 20/07/2012 on a plus besoin du Cnplante il est calculé apres
                   P_tigefeuil,P_biorognem,n,nrogne,    &
                   lairogne,biorogne,lai,masec,varrapforme,P_forme,biorognecum,lairognecum)                      !INOUT & OUT

USE Messages

implicit none

  integer, intent(IN)    :: P_codcalrogne  !< // PARAMETER // option of the way of calculation of tipping // code 1/2 // PARTEC // 0
  integer, intent(IN)    :: n
  integer, intent(IN)    :: nrogne

  real,    intent(IN)    :: hauteur      !< // OUTPUT // Height of canopy // m
  real,    intent(IN)    :: largeur      !< // OUTPUT // Width of the plant shape  // m
  real,    intent(IN)    :: P_hautrogne  !> // PARAMETER // cutting height // m // PARTEC // 1
  real,    intent(IN)    :: P_margerogne  !< // PARAMETER // allowed quantity of biomass inbetween two shapenings when asking automatic shapening  // t ha-1 // PARTEC // 1
  real,    intent(IN)    :: P_largrogne  !< // PARAMETER // width of shapening // m // PARTEC // 1
  real,    intent(IN)    :: dfol      !< // OUTPUT //  "Within the shape  leaf density" // m2 m-3
  real,    intent(IN)    :: P_hautbase  !< // PARAMETER // Base height of crop // m // PARPLT // 1
  real,    intent(IN)    :: P_interrang  !< // PARAMETER // Width of the P_interrang // m // PARTEC // 1
  real,    intent(IN)    :: sla      !< // OUTPUT // Specific surface area // cm2 g-1
  real,    intent(IN)    :: P_tigefeuil  !< // PARAMETER // stem (structural part)/leaf proportion // SD // PARPLT // 1
  real,    intent(IN)    :: P_biorognem  !< // PARAMETER // minimal biomass to be removed when tipping (automatic calculation) // t ha-1 // PARTEC // 1
! DR 20/07/2012 on a plus besoin du Cnplante il est calculé apres
!  real,    intent(IN)    :: CNplante      !< // OUTPUT // Nitrogen concentration of entire plant  // %

  real,    intent(OUT)   :: lairogne
  real,    intent(OUT)   :: biorogne
  real,    intent(OUT)   :: lai      !< // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(OUT)   :: masec      !< // OUTPUT // Aboveground dry matter  // t.ha-1
  real,    intent(OUT)   :: varrapforme
  integer, intent(OUT)   :: P_forme  !< // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0
  real,    intent(OUT)   :: biorognecum
  real,    intent(OUT)   :: lairognecum
end subroutine rognage

! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> calculation of the ground cover
!> - Stics book paragraphe3.1.4, page 47-48
!>
!!This module allows the use of ground cover instead of the leaf area index.
!! Given the complexity and the numerous parameters required for LAI calculation, de Tourdonnet (1999) proposed a simple alternative by the direct calculation
!! of the ground cover, which can be used as a state variable in calculations for radiation interception and water requirements.  This can be particularly useful
!! for plants with a complex foliage structure such as lettuce, or for a first modelling approach. It is programmed in STICS as an alternative option to all
!! the previous calculations. This formalisation is particularly interesting when leaves have complex spatial arrangement or when the individual plant foliage
!! is abundant.
!!
!! To calculate the ground cover (tauxcouv), a temporal scale similar to that of LAI is used, and called ULAI; this varies from 0 to 2, depending on the
!! phenological time.  At the IAMF stage, ULAI is equal to infrecouv. tauxcouv is calculated by a logistic curve, using trecouvmax as the asymptote, which
!! represents the proportion of the soil covered by an isolated plant, infrecouv as the abscissa of the inflexion point and pentrecouv as the slope at the
!! inflexion point. The competitive effect linked to population growth is simulated in the same way as for the leaf area index and uses the same parameters,
!! adens, bdens and laicomp (expressed as ground cover). In this case, laiplantule is the ground cover of plants at planting if the crop is  transplanted
!! rather than sown. Water and nitrogen shortage and waterlogging stresses are applied to the rate of growth of ground cover, and the method of combining
!! stresses is the same as for the leaf area index.
!------------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine TauxRecouvrement_(n,P_codeinnact,P_codeh2oact,turfac,innlai,nlev,P_codeperenne,nplt,ndrp,        & ! IN
                             P_codehypo,P_codegermin,P_laiplantule,namf,nrec,P_codcueille,nlax,nsen,        &
                             upvt,upvtutil,P_infrecouv,densite,somcour,P_stamflax,P_stlevamf,               &
                             P_codeindetermin,P_laicomp,P_adens,P_bdens,P_tauxrecouvmax,P_pentrecouv,       &
                             exolai,P_stsenlan,ulai_veille,tauxcouv_nsen,P_phyllotherme,udevcult,           &
                             tustress,tauxcouv,tauxcouv_veille,somfeuille,nbfeuille,reajust,                & ! INOUT
                             fdens,ulai,efdensite,nlan,tauxcouv_nlan)

    implicit none

    integer, intent(IN) :: n
    integer, intent(IN) :: P_codeinnact  !< // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
    integer, intent(IN) :: P_codeh2oact  !< // PARAMETER // code to activate  water stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
    real,    intent(IN) :: turfac      !< // OUTPUT // Index of turgescence water stress  // 0-1
    real,    intent(IN) :: innlai      !< // OUTPUT // Index of nitrogen stress active on leaf growth // P_innmin to 1
    integer, intent(IN) :: nlev
    integer, intent(IN) :: P_codeperenne  !< // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0
    integer, intent(IN) :: nplt
    integer, intent(IN) :: ndrp
    integer, intent(IN) :: P_codehypo  !< // PARAMETER // option of simulation of a  phase of hypocotyl growth (1) or planting of plantlets (2) // code 1/2 // PARPLT // 0
    integer, intent(IN) :: P_codegermin  !< // PARAMETER // option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2) // code 1/2 // PARPLT // 0
    real,    intent(IN) :: P_laiplantule  !< // PARAMETER // Plantlet Leaf index at the plantation // m2 leaf  m-2 soil // PARPLT // 1
    integer, intent(IN) :: namf
    integer, intent(IN) :: nrec
    integer, intent(IN) :: P_codcueille  !< // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0
    integer, intent(IN) :: nlax
    integer, intent(IN) :: nsen
    real,    intent(IN) :: upvt      !< // OUTPUT // Daily development unit  // degree.days
    real,    intent(IN) :: upvtutil
    real,    intent(IN) :: P_infrecouv  !< // PARAMETER // ulai at the stage AMF (inflection point of the soil cover rate increase) // SD // PARPLT // 1
    real,    intent(IN) :: densite      !< // OUTPUT // Actual sowing density // plants.m-2
    real,    intent(IN) :: somcour      !< // OUTPUT // Cumulated units of development between two stages // degree.days
    real,    intent(IN) :: P_stamflax  !< // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1
    real,    intent(IN) :: P_stlevamf  !< // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1
    integer, intent(IN) :: P_codeindetermin  !> // PARAMETER // option of  simulation of the leaf growth and fruit growth : indeterminate (2) or determinate (1) // code 1/2 // PARPLT // 0
    real,    intent(IN) :: P_laicomp  !< // PARAMETER // LAI from which starts competition inbetween plants // m2 m-2 // PARPLT // 1
    real,    intent(IN) :: P_adens  !< // PARAMETER // Interplant competition parameter // SD // PARPLT // 1
    real,    intent(IN) :: P_bdens  !< // PARAMETER // minimal density from which interplant competition starts // plants m-2 // PARPLT // 1
    real,    intent(IN) :: P_tauxrecouvmax  !< // PARAMETER // maximal soil cover rate // m2 plante m-2 sol // PARPLT // 1
    real,    intent(IN) :: P_pentrecouv  !< // PARAMETER // parameter of the logistic curve of the soil cover rate increase // * // PARPLT // 1
    real,    intent(IN) :: exolai      !< // OUTPUT // Index for excess water active on growth in biomass // 0-1
    real,    intent(IN) :: P_stsenlan  !< // PARAMETER // Sum of development units between the stages SEN et LAN // degree.days // PARPLT // 1
    real,    intent(IN) :: ulai_veille
    real,    intent(IN) :: tauxcouv_nsen
    real,    intent(IN) :: P_phyllotherme  !< // PARAMETER // thermal duration between the apparition of two successive leaves on the main stem // degree C day // PARPLT // 1
    real,    intent(IN) :: udevcult      !< // OUTPUT // Effective temperature for the development, computed with TCULT // degree.days

! INOUT
    real,    intent(INOUT) :: tustress      !< // OUTPUT // Stress index active on leaf growth (= minimum(turfac,innlai))  // 0-1
    real,    intent(INOUT) :: tauxcouv      !< // OUTPUT // Cover rate // SD
    real,    intent(INOUT) :: tauxcouv_veille
    real,    intent(INOUT) :: somfeuille
    integer, intent(INOUT) :: nbfeuille      !< // OUTPUT // Number of leaves on main stem // SD
    real,    intent(INOUT) :: reajust
    real,    intent(INOUT) :: fdens
    real,    intent(INOUT) :: ulai      !< // OUTPUT // Daily relative development unit for LAI // 0-3
    real,    intent(INOUT) :: efdensite
    integer, intent(INOUT) :: nlan
    real,    intent(INOUT) :: tauxcouv_nlan
end subroutine TauxRecouvrement_

! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> calculation of the LAI
!> - Stics book paragraphe 3.1.1, page 40-44
!>
!! In STICS, leaf area growth is driven by phasic development, temperature and stresses. An empirical plant density-dependent function represents inter-plant
!! competition. For indeterminate plants, trophic competition is taken into account through a trophic stress index, while for determinate plants a maximal
!! expansion rate threshold is calculated to avoid unrealistic leaf expansion.
!! The calculation of leaf growth rate (deltai in m2m-2 d-1) is broken down: a first calculation of the LAI growth rate (in m2 plant-1 degree-day-1) describes
!! a logistic curve, related to the ILEV, IAMF and ILAX phenological stages. This value is then multiplied by the effective crop temperature, the plant density
!! combined with a density factor, supposed to stand for inter-plant competition, that is characteristic for the variety, and the water and nitrogen stress indices.
!!
!! The phasic development function is comparable to that of the model PUTU (Singels and Jagger, 1991), i.e. a logistic function with dlaimaxbrut as asymptote
!! and pentlaimax as the slope at the inflection point. It is driven by a normalized leaf development unit (ULAI) equal to 1 at ILEV and 3 at ILAX.
!! At the end of the juvenile stage (IAMF), it is equal to vlaimax when the inflection of the dynamics (point of maximal rate) also occurs.
!! Between the stages ILEV, IAMF and ILAX, the model performs linear interpolation based on development units (upvt) which include all the environmental effects
!! on phasic development. As the ILAX stage approaches, it is possible to introduce a gradual decline in growth rate using the udlaimax parameter
!!- the ULAI value beyond which there is a decline in the leaf growth rate. If udlaimax=3 it has no effect and the leaf stops growing at once at ILAX.
!!
!! The thermal function relies on crop temperature and cardinal temperatures (tcmin and tcmax) which differ from the ones used for the phasic development.
!! The extreme threshold tcxstop is the same as for development.
!!
!! The density function is active solely after a given LAI threshold occurs (laicomp parameter) if the plant density (densite in plant m-2 possibly decreased
!! by early frost) is greater than the bdens threshold, below which plant leaf area is assumed independent of density.  Beyond this density value, leaf area
!! per plant decreases exponentially.  The adens parameter represents the ability of a plant to withstand increasing densities.  It depends on the species
!! and may depend on the variety.  For branching or tillering plants, adens represents the plant’s branching or tillering ability (e. g. wheat or pea).
!! For single-stem plants, adens represents competition between plant leaves within a given stand (e.g. maize or sunflower).
!!
!! Water and nitrogen affect leaf growth as limiting factors, i.e. stress indices whose values vary between 0 and 1. Water (turfac) and nitrogen deficits (innlai)
!! are assumed to interact, justifying the use of the more severe of the two stresses. Meanwhile at the whole plant level the water-logging stress index is assumed
!! to act independently
!!
!!
!!
!> -	Features of determinate crops
!!   Failure to account for trophic aspects in the calculation of leaf growth may cause problems when the radiation intercepted by the crop is insufficient to
!!   ensure leaf expansion (e.g. for crops under a tree canopy or crops growing in winter).  Consequently, from the IAMF stage, we have introduced a trophic effect
!!  to calculate the definitive LAI growth rate in the form of a maximum threshold for leaf expansion (deltaimaxi in m2m-2d-1) using the notion of the maximum
!!   leaf expansion allowed per unit of biomass accumulated in the plant (sbvmax in cm2 g-1) and the daily biomass accumulation (dltams in t.ha-1day-1 possibly
!!   complemented by remobilized reserve remobilj). sbvmax is calculated using the slamax and tigefeuil parameters.
!!
!> -	Features of indeterminate crops
!!   It has been possible to test the robustness of the above formalisation on a variety of crops, including crops where there is an overlap between the
!!   vegetative phase and the reproductive phase (soybean and flax for example).  However, when trophic competition between leaves and fruits is a driving force
!!   for the production and management of the crop (for example tomato, sugarbeet), this formalisation is unsuitable.  We therefore calculate the deltai variable
!!   so as to take trophic monitoring into consideration in the case of crops described as ‘indeterminate’, by introducing a trophic stress index (splai).
!!   As a consequence, the LAI can decrease markedly during the theoretical growth phase if the crop is experiencing severe stresses during the harvested
!!   organs filling phase.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine calai_(P_codeinnact,P_codeh2oact,P_codehypo,P_codegermin,P_codcueille,P_codlainet,codeulaivernal,      & !IN
                  P_codeindetermin,P_codeinitprec,P_codeperenne,P_codetemp,turfac,innlai,                         &
                  P_laiplantule,P_phyllotherme,udevair,udevcult,P_dlaimaxbrut,P_udlaimax,n,numcult,nbjmax,        &
                  nplt,nlev,namf,nlax,ndrp,nrec,upvt,upvtutil,P_vlaimax,P_laicomp,somcour,somcourutp,             &
                  P_stlevamf,P_stamflax,densite,P_adens,P_bdens,P_tcxstop,tcult,P_tcmin,P_tcmax,P_pentlaimax,     &
                  P_dlaimin,exolai,sourcepuits,P_splaimin,P_splaimax,P_slamin,P_slamax,P_tigefeuil,P_tustressmin, &
                  fstressgel,dltamsen,sla,remobilj,dltams,                                                        & !IN
                  tustress,efdensite,nstopfeuille,deltai,splai,fpv,P_stlaxsen,tempeff,                            & !OUT
                  lai,somfeuille,nbfeuille,nsen,nlan,P_dlaimax,reajust,ulai,vmax,dltaisenat,laisen,               & !INOUT
                  dltaisen,P_stsenlan,densiteequiv)

implicit none

integer, intent(IN)    :: nbjmax !> la taille des tableaux journaliers

integer, intent(IN)    :: P_codeinnact  !< // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
integer, intent(IN)    :: P_codeh2oact  !< // PARAMETER // code to activate  water stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
integer, intent(IN)    :: P_codehypo  !< // PARAMETER // option of simulation of a  phase of hypocotyl growth (1) or planting of plantlets (2) // code 1/2 // PARPLT // 0
integer, intent(IN)    :: P_codegermin  !< // PARAMETER // option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2) // code 1/2 // PARPLT // 0
integer, intent(IN)    :: P_codcueille  !< // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0
integer, intent(IN)    :: P_codlainet  !< // PARAMETER // option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
integer, intent(IN)    :: codeulaivernal
integer, intent(IN)    :: P_codeindetermin  !< // PARAMETER // option of  simulation of the leaf growth and fruit growth : indeterminate (2) or determinate (1) // code 1/2 // PARPLT // 0
!integer, intent(IN)    :: P_codedlaimin
integer, intent(IN)    :: P_codeinitprec  !< // PARAMETER // reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0
integer, intent(IN)    :: P_codeperenne  !< // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0
integer, intent(IN)    :: P_codetemp  !< // PARAMETER // option calculation mode of heat time for the plant : with air temperature (1)  or crop temperature (2) // code 1/2 // PARPLT // 0
real,    intent(IN)    :: turfac      !< // OUTPUT // Index of turgescence water stress  // 0-1
real,    intent(IN)    :: innlai      !< // OUTPUT // Index of nitrogen stress active on leaf growth // P_innmin to 1
real,    intent(IN)    :: P_laiplantule  !< // PARAMETER // Plantlet Leaf index at the plantation // m2 leaf  m-2 soil // PARPLT // 1
real,    intent(IN)    :: P_phyllotherme  !< // PARAMETER // thermal duration between the apparition of two successive leaves on the main stem // degree C day // PARPLT // 1
real,    intent(IN)    :: udevair      !< // OUTPUT // Effective temperature for the development, computed with TAIR // degree.days
real,    intent(IN)    :: udevcult      !< // OUTPUT // Effective temperature for the development, computed with TCULT // degree.days
real,    intent(IN)    :: P_dlaimaxbrut  !< // PARAMETER // Maximum rate of the setting up of LAI // m2 leaf plant-1 degree d-1 // PARPLT // 1
real,    intent(IN)    :: P_udlaimax  !< // PARAMETER // ulai from which the rate of leaf growth decreases  // SD // PARPLT // 1
integer, intent(IN)    :: n
integer, intent(IN)    :: numcult
integer, intent(IN)    :: nplt
integer, intent(IN)    :: nlev
integer, intent(IN)    :: namf
integer, intent(IN)    :: nlax
integer, intent(IN)    :: ndrp
integer, intent(IN)    :: nrec
real,    intent(IN)    :: upvt      !< // OUTPUT // Daily development unit  // degree.days
real,    intent(IN)    :: upvtutil
real,    intent(IN)    :: P_vlaimax  !< // PARAMETER // ULAI  at inflection point of the function DELTAI=f(ULAI) // SD // PARPLT // 1
real,    intent(IN)    :: P_laicomp  !< // PARAMETER // LAI from which starts competition inbetween plants // m2 m-2 // PARPLT // 1
real,    intent(IN)    :: somcour      !< // OUTPUT // Cumulated units of development between two stages // degree.days
real,    intent(IN)    :: somcourutp
real,    intent(IN)    :: P_stlevamf  !< // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1
real,    intent(IN)    :: P_stamflax  !< // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1
real,    intent(IN)    :: densite      !< // OUTPUT // Actual sowing density // plants.m-2
real,    intent(IN)    :: P_adens  !< // PARAMETER // Interplant competition parameter // SD // PARPLT // 1
real,    intent(IN)    :: P_bdens  !< // PARAMETER // minimal density from which interplant competition starts // plants m-2 // PARPLT // 1
real,    intent(IN)    :: P_tcxstop  !< // PARAMETER // threshold temperature beyond which the foliar growth stops // degree C // PARPLT // 1
real,    intent(IN)    :: tcult      !< // OUTPUT // Crop surface temperature (daily average) // degree C
real,    intent(IN)    :: P_tcmin  !< // PARAMETER // Minimum temperature of growth // degree C // PARPLT // 1
real,    intent(IN)    :: P_tcmax  !< // PARAMETER // Maximum temperature of growth // degree C // PARPLT // 1
real,    intent(IN)    :: P_pentlaimax  !< // PARAMETER // parameter of the logistic curve of LAI growth  // SD // PARPLT // 1
real,    intent(IN)    :: P_dlaimin  !< // PARAMETER // accelerating parameter for the lai growth rate // SD // PARAMV6/PLT // 1
real,    intent(IN)    :: exolai      !< // OUTPUT // Index for excess water active on growth in biomass // 0-1
real,    intent(IN)    :: sourcepuits      !< // OUTPUT // Pool/sink ratio // 0-1
real,    intent(IN)    :: P_splaimin  !< // PARAMETER // Minimal value of ratio sources/sinks for the leaf growth  // between 0 and 1 // PARPLT // 1
real,    intent(IN)    :: P_splaimax  !< // PARAMETER // maximal sources/sinks value allowing the trophic stress calculation for leaf growing // SD // PARPLT // 1
real,    intent(IN)    :: P_slamin  !< // PARAMETER // minimal SLA of green leaves // cm2 g-1 // PARPLT // 1
real,    intent(IN)    :: P_slamax  !< // PARAMETER // maximal SLA of green leaves // cm2 g-1 // PARPLT // 1
real,    intent(IN)    :: P_tigefeuil  !< // PARAMETER // stem (structural part)/leaf proportion // SD // PARPLT // 1
real,    intent(IN)    :: P_tustressmin  !< // PARAMETER //  threshold stress (min(turfac,inns)) under which there is an effect on the LAI (supplementary senescence by ratio at the natural senescence) // SD // PARPLT // 1
real,    intent(IN)    :: fstressgel      !< // OUTPUT // Frost index on the LAI // 0-1
real,    intent(IN)    :: dltamsen      !< // OUTPUT // Senescence rate // t ha-1 j-1
real,    intent(IN)    :: sla      !< // OUTPUT // Specific surface area // cm2 g-1
real,    intent(IN)    :: remobilj      !< // OUTPUT // Amount of biomass remobilized on a daily basis for the fruits  // g.m-2 j-1
real,    intent(IN)    :: dltams                ! (n-1)       // OUTPUT // Growth rate of the plant  // t ha-1.j-1
real,    intent(IN)    :: densiteequiv  !densite equivalente calculée chaque jour

real,    intent(OUT)   :: tustress      !< // OUTPUT // Stress index active on leaf growth (= minimum(turfac,innlai))  // 0-1
real,    intent(OUT)   :: efdensite
integer, intent(OUT)   :: nstopfeuille
real,    intent(OUT)   :: deltai                ! (n)    --(0:nbjmax)       // OUTPUT // Daily increase of the green leaf index // m2 leafs.m-2 soil
real,    intent(OUT)   :: splai      !< // OUTPUT // Pool/sink ratio applied to leaves // 0-1
real,    intent(OUT)   :: fpv      !< // OUTPUT // Sink strength of developing leaves // g.j-1.m-2
real,    intent(OUT)   :: P_stlaxsen  !< // PARAMETER // Sum of development units between the stages LAX and SEN // degree.days // PARPLT // 1
real,    intent(OUT)   :: tempeff      !< // OUTPUT // Efficient temperature for growth // degree C

real,    intent(INOUT) :: lai(0:nbjmax)            ! (n), (n-1), (nsen), (nlan)  	  // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
real,    intent(INOUT) :: somfeuille
integer, intent(INOUT) :: nbfeuille      !< // OUTPUT // Number of leaves on main stem // SD
integer, intent(INOUT) :: nsen
integer, intent(INOUT) :: nlan
real,    intent(INOUT) :: P_dlaimax  !< // PARAMETER // Maximum rate of the setting up of LAI // m2 leaf plant-1 degree d-1 // PARPLT // 1
real,    intent(INOUT) :: reajust
real,    intent(INOUT) :: ulai(0:nbjmax)        ! (n), (n-1)  	  // OUTPUT // Daily relative development unit for LAI // 0-3
real,    intent(INOUT) :: vmax
real,    intent(INOUT) :: dltaisenat
real,    intent(INOUT) :: laisen(0:nbjmax)        ! (n), (n-1)  	  // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
real,    intent(INOUT) :: dltaisen      !< // OUTPUT // Daily increase of the senescent leaf index // m2.m-2 sol.j-1
real,    intent(INOUT) :: P_stsenlan  !< // PARAMETER // Sum of development units between the stages SEN et LAN // degree.days // PARPLT // 1
end subroutine calai_


!! ****************************************************************
!> This module deals with foliage regulation by leaf removal.
!> - Stics book paragraphe6.1.3.a, page 97
!>
!! Leaf removal (laieffcum) is expressed directly by reducing the leaf area index, also according to two possible methods.
!! With the automatic calculation, a constant proportion (effeuil) of the new foliage generated each day is removed as soon as the LAI reaches a threshold
!! value (laidebeff). The other possible calculation is done on only one occasion, on day juleffeuil and the quantity laieffeuil is removed.
!!
!! The corresponding biomass is calculated from the specific leaf area (sla) and deducted from the biomass of the plant. Another option concerns the location
!! of leaf removal: the top or bottom of the canopy, which affects the radiation and water balances of crops in rows.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------*
subroutine effeuill(P_codcaleffeuil,P_laidebeff,deltai,P_effeuil,sla,n,neffeuil,P_laieffeuil,   & ! IN
                    P_codetransrad,P_hautmax,P_khaut,dfol,P_largrogne,P_codhauteff,largeur,     & ! IN
                    lai,masec,P_hautbase,varrapforme,bioeffcum,laieffcum)                         ! INOUT

USE Messages

implicit none

!: ARGUMENTS

  integer, intent(IN) :: P_codcaleffeuil  !< // PARAMETER // option of the way of caluclation of leaf removal // code 1/2 // PARTEC // 0
  real,    intent(IN) :: P_laidebeff  !< // PARAMETER // LAI of the beginning of leaf removal // m2 m-2 // PARTEC // 1
  real,    intent(IN) :: deltai      !< // OUTPUT // Daily increase of the green leaf index // m2 leafs.m-2 soil
  real,    intent(IN) :: P_effeuil  !< // PARAMETER // proportion of daily leaf removed at thinning // 0-1  // PARTEC // 1
  real,    intent(IN) :: sla      !< // OUTPUT // Specific surface area // cm2 g-1
  integer, intent(IN) :: n  !< day number in the run
  integer, intent(IN) :: neffeuil !< neffeuil
  real,    intent(IN) :: P_laieffeuil  !< // PARAMETER // LAI of the end of leaf removal // m2 m-2 // PARTEC // 1
  integer, intent(IN) :: P_codetransrad  !< // PARAMETER // simulation option of radiation interception : law Beer (1), radiation transfers (2) // code 1/2 // PARPLT // 0
  real,    intent(IN) :: P_hautmax !< // PARAMETER // Maximum height of crop // m // PARPLT // 1
  real,    intent(IN) :: P_khaut  !< // PARAMETER // Extinction Coefficient connecting leaf area index to height crop // * // PARAM // 1
  real,    intent(IN) :: dfol      !< // OUTPUT //  (Within the shape)  leaf density // m2 m-3
  real,    intent(IN) :: P_largrogne  !< // PARAMETER // width of shapening // m // PARTEC // 1
  integer, intent(IN) :: P_codhauteff  !< // PARAMETER // leaf removal heitgh // code 1/2 // PARTEC // 0
  real,    intent(IN) :: largeur      !< // OUTPUT // Width of the plant shape  // m

  real,    intent(INOUT) :: lai      !< // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(INOUT) :: masec      !< // OUTPUT // Aboveground dry matter  // t.ha-1
  real,    intent(INOUT) :: P_hautbase  !< // PARAMETER // Base height of crop // m // PARPLT // 1
  real,    intent(INOUT) :: varrapforme !< varrapforme
  real,    intent(INOUT) :: bioeffcum !< bioeffcum
  real,    intent(INOUT) :: laieffcum !< laieffcum

end subroutine effeuill



end interface


contains

!> Module of plant development
!>
!! Description : (routine qui prend des structures en arguments)
! DR 23/07/2012 sta est inutilisé
!subroutine develop(sc,p,itk,soil,c,t,g,sta)
subroutine develop(sc,p,itk,soil,c,t,g)

!-----------------------------------------------------------------------
!    group = Numero du groupe (de precocite)
!    si =-1 on n'a pas assez de sommes de temperature pour
!             aller au bout du cycle
!------------------
! DR et ML 18/03/08 :
! on avait supprimé debour et introduit les calculs de debourrement dans
! develop apres reflexion ca pose des pbs car le basculement du calcul horaire au calcul
! journalier dependait de la levée qui etait calculé apres le test de basculement
! d'ou des somtemps horaires le jour de la levée qui entrainait une scenescence
! immediate : INAKI on t'aime
!-----------------------------------------------------------------------

implicit none

type(Stics_Communs_) :: sc  

type(Parametres_Generaux_) :: g  

type(Plante_) :: p  

type(ITK_) :: itk  

type(Sol_) :: soil  

type(Climat_) :: c  

type(Stics_Transit_) :: t  

!type(Station_) :: sta

integer :: ens  
!real :: rdtint_recolte_avant
!integer :: nbrecolte_moins1

!pour faciliter l'écriture
ens = sc%ens


!p(i)%nbrecolte,p(i)%rdtint(ens,1:max(1,p(i)%nbrecolte-1))

!     if(p%nbrecolte>0)then
!              	nbrecolte_moins1=p%nbrecolte-1
!              	rdtint_recolte_avant=p%rdtint(0,nbrecolte_moins1)
!        write(*,*)'dans developpement ',rdtint_recolte_avant        ! On a un pb si numcoupe = 0, on demande l'indice -1 du tableau P_msresiduel, qui n'existe pas.,
!	 else
!			  	rdtint_recolte_avant=0.
!      write(*,*)'dans croissanceavant biomaer',p(1)%nbrecolte,rdtint_recolte_avant          ! On a un pb si numcoupe = 0, on demande l'indice -1 du tableau P_msresiduel, qui n'existe pas.,
!	 endif


! on appelle la fonction develop2 avec sa liste d'arguments individuels
! DR 23/07/2012 latitude est pas itilisé car on passe la photoperiode qui est suffisante
!call develop2(sta%P_latitude,c%phoi,c%tmax(sc%n),c%tmin(sc%n),c%tmin(sc%n+1),c%trr(sc%n),g%P_codeh2oact,g%P_codeinitprec,&



call develop2(c%phoi,c%tmax(sc%n),c%tmin(sc%n),c%tmin(sc%n+1),c%trr(sc%n),g%P_codeh2oact,g%P_codeinitprec,&
              g%P_codeinnact,&
              g%codeulaivernal,g%P_psihucc,g%P_psihumin,itk%P_codcueille,itk%P_codefauche,itk%P_densitesem,itk%P_profsem,&
              itk%P_variete,       &
              p%P_ampfroid,p%P_belong,p%P_celong,p%P_codebfroid,p%P_codedormance,p%P_codegdh,p%P_codegdhdeb,p%P_codegermin,&
              p%P_codehypo,          &
              p%P_codeperenne,p%P_codephot,p%P_codeplante,p%P_coderetflo,p%P_codetemp,p%coeflev,minval(p%cu),p%cu(sc%n-1),&
              p%densite,      &
              p%densiteger,p%densitelev,p%P_elmax,p%innlai(0),p%P_julvernal,p%P_jvc(itk%P_variete),p%P_jvcmini,p%P_nbjgerlim,&
              p%ndrpobs,     &
              p%P_nlevlim1,p%P_nlevlim2,p%nlevobs,p%nplt,p%onarretesomcourdrp,p%P_phobase,p%P_phosat,p%P_potgermi,p%P_propjgermin,&
              p%P_q10,    &
              p%P_sensiphot(itk%P_variete),p%P_sensrsec,p%somelong,p%somger,p%P_stdordebour,p%P_stpltger,p%P_stressdev,p%P_tcxstop,&
              p%P_tdmax,   &
              p%P_tdmaxdeb,p%P_tdmin,p%P_tdmindeb,p%P_tfroid,p%P_tgmin,p%turfac(0),p%P_vigueurbat,soil%P_codefente,soil%P_mulchbat,&
              soil%P_pluiebat,sc%P_culturean,sc%nbCouchesSol,sc%dacouche,sc%hucc,sc%humin,sc%hur,sc%jjul,sc%n,sc%nbjanrec,         &
              sc%nbjsemis,sc%numcult,sc%tairveille,sc%tcult,sc%tsol,sc%xmlch1,t%P_codepluiepoquet,t%P_codetempfauche,p%humectation,&
              p%nbjhumec,p%pluiesemis,p%somtemphumec,p%P_codeindetermin,p%P_codelaitr,p%P_codlainet,&
              p%P_dureefruit(itk%P_variete), &
              ! DR 23/07/2012 P_nboite non utilisé
!              p%namfobs,itk%P_nbcueille,p%P_nboite,p%nfloobs,p%nlanobs,p%nlaxobs,p%nmatobs,p%nrecobs,p%nsenobs,p%P_stdrpnou,       &
              p%namfobs,itk%P_nbcueille,p%nfloobs,p%nlanobs,p%nlaxobs,p%nmatobs,p%nrecobs,p%nsenobs,p%P_stdrpnou,       &
              p%upobs(sc%n),t%P_codemontaison(p%ipl),p%sioncoupe,p%caljvc,p%cu(sc%n),p%demande,p%etatvernal,p%hauteur,p%mafrais,  &
              p%mafraisfeuille,p%mafraisrec,p%mafraisres,p%mafraistige,p%masec(:,sc%n),p%namf,p%ndebdorm,p%ndrp,p%nfindorm,     &
              p%nflo,p%nger,p%nlan,p%nlev,p%nrec,p%nrecbutoir,p%pdsfruitfrais,p%rfpi,p%rfvi,p%somcour,p%somcourdrp,             &
              p%somcourfauche,p%somcourutp,p%somtemp,p%stpltlev,p%tdevelop(sc%n),p%udevair,p%udevcult,p%upvt,p%utp, &
              p%zrac,sc%maxwth,p%group,p%ndebdes,p%nfruit(0,p%P_nboite),p%nlax,p%nmat,p%nnou,p%nsen,p%P_stamflax(itk%P_variete),&
              p%P_stdrpdes(itk%P_variete),p%P_stdrpmat(itk%P_variete),p%stdrpsen,p%P_stflodrp(itk%P_variete),&
              p%P_stlaxsen(itk%P_variete),       &
              p%P_stlevamf(itk%P_variete),p%P_stlevdrp(itk%P_variete),p%stlevflo,p%stmatrec,p%P_stsenlan(itk%P_variete),&
              p%upvtutil,         &
              itk%P_codrecolte,p%h2orec(0),itk%P_sucrerec,itk%P_CNgrainrec,itk%P_huilerec,p%sucre(0),p%huile(0),p%teaugrain(0),   &
              ! 23/07/2012 hilueder et sucreder inutilisés
!              p%sucreder(0),p%huileder(0),p%P_h2ofrvert,itk%P_codeaumin,itk%P_h2ograinmin,itk%P_h2ograinmax,p%P_deshydbase,       &
              p%P_h2ofrvert,itk%P_codeaumin,itk%P_h2ograinmin,itk%P_h2ograinmax,p%P_deshydbase,       &
              p%CNgrain(0),itk%P_cadencerec,                                                                                      &
              p%jdepuisrec,p%pdsfruit(0,p%P_nboite),p%nbrecolte,p%nrecint(p%nbrecolte),p%rdtint(0,p%nbrecolte),                   &
!              p%jdepuisrec,p%pdsfruit(0,p%P_nboite),p%nbrecolte,p%nrecint(p%nbrecolte),p%rdtint               ,                   &
              p%teauint(0,p%nbrecolte),p%nbfrint(0,p%nbrecolte),p%onestan2,p%somcourmont,p%nmontaison,p%stpltger,          &
                ! DR 06/03/2015 ajout pour les enchainement annuel de la vigne
              p%P_idebdorm, sc%P_iwater)
!             ,p%rdtint(0,p%nbrecolte-1))
end subroutine develop

end module Developpement
