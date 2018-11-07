!> Routine develop2
!>
!! Description :
!< (routine qui prend des arguments simples)

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
                    nmontaison,stpltger,P_idebdorm, P_iwater )

USE Divers, only: calcul_UDev, calcul_TemperaturesHoraires, calcul_GDH, cRFPI
USE Besoins_en_froid
USE Messages

implicit none

!  real,    intent(IN)                           :: P_latitude  !> // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0
  real,    intent(IN)                           :: phoi   !> // OUTPUT // Photoperiod // hours
  real,    intent(IN)                           :: tmax   !> // OUTPUT // Maximum active temperature of air // degree C
  real,    intent(IN)                           :: tmin   !> // OUTPUT // Minimum active temperature of air // degree C
  real,    intent(IN)                           :: tmin_demain  
  real,    intent(IN)                           :: trr   !> // OUTPUT // Rainfall  // mm.day-1
  integer, intent(IN)                           :: P_codeh2oact  !> // PARAMETER // code to activate  water stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0 
  integer, intent(IN)                           :: P_codeinitprec  !> // PARAMETER // reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0 
  integer, intent(IN)                           :: P_codeinnact  !> // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0 
  integer, intent(IN)                           :: codeulaivernal  
  real,    intent(IN)                           :: P_psihucc  !> // PARAMETER // soil potential corresponding to field capacity  // Mpa // PARAM // 1 
  real,    intent(IN)                           :: P_psihumin  !> // PARAMETER // soil potential corresponding to wilting point // Mpa // PARAM // 1 
  integer, intent(IN)                           :: P_codcueille  !> // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0 
  integer, intent(IN)                           :: P_codefauche  !> // PARAMETER // option of cut modes for forage crops: yes (1), no (2) // code 1/2 // PARTEC // 0 
  real,    intent(IN)                           :: P_densitesem  !> // PARAMETER // Sowing density  // plants.m-2 // PARTEC // 1 
  real,    intent(IN)                           :: P_profsem  !> // PARAMETER // Sowing depth // cm // PARTEC // 1 
  integer, intent(IN)                           :: P_variete  !> // PARAMETER // variety number in the technical file // SD // PARTEC // 1 
  real,    intent(IN)                           :: P_ampfroid  !> // PARAMETER // semi thermal amplitude thermique for vernalising effect // degree C // PARPLT // 1
  real,    intent(IN)                           :: P_belong  !> // PARAMETER // parameter of the curve of coleoptile elongation // degree.days -1 // PARPLT // 1 
  real,    intent(IN)                           :: P_celong  !> // PARAMETER // parameter of the subsoil plantlet elongation curve // SD // PARPLT // 1 
  integer, intent(IN)                           :: P_codebfroid  !> // PARAMETER // option of calculation of chilling requirements // code 1/2 // PARPLT // 0 
  integer, intent(IN)                           :: P_codedormance  !> // PARAMETER // option of calculation of dormancy and chilling requirement // code 1/2 // PARPLT // 0 
  integer, intent(IN)                           :: P_codegdh  !> // PARAMETER // hourly (1) or daily (2) calculation of development unit // code 1/2 // PARPLT // 0 
  integer, intent(IN)                           :: P_codegdhdeb  !> // PARAMETER // option of calculation of the bud break date in hourly or daily growing degrees  // code 1/2 // PARPLT // 0 
  integer, intent(IN)                           :: P_codegermin  !> // PARAMETER // option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2) // code 1/2 // PARPLT // 0 
  integer, intent(IN)                           :: P_codehypo  !> // PARAMETER // option of simulation of a  phase of hypocotyl growth (1) or planting of plantlets (2) // code 1/2 // PARPLT // 0 
  integer, intent(IN)                           :: P_codeperenne  !> // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0 
  integer, intent(IN)                           :: P_codephot  !> // PARAMETER // option of plant photoperiodism: yes (1), no (2) // code1/2 // PARPLT // 0 
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=3), intent(IN)                  :: P_codeplante  !> // PARAMETER // Name code of the plant in 3 letters // * // PARPLT // 0
  integer, intent(IN)                           :: P_codeplante  !> // PARAMETER // Name code of the plant in 3 letters // * // PARPLT // 0
  integer, intent(IN)                           :: P_coderetflo  !> // PARAMETER // option slowness action of water stress before the stage DRP: yes  (1), no (2) // code 1/2 // PARPLT // 0 
  integer, intent(IN)                           :: P_codetemp  !> // PARAMETER // option calculation mode of heat time for the plant : with air temperature (1)  or crop temperature (2) // code 1/2 // PARPLT // 0 
  real,    intent(IN)                           :: coeflev  
  real,    intent(IN)                           :: cu_min  
  real,    intent(IN)                           :: cu_veille  
  real,    intent(IN)                           :: densite   !> // OUTPUT // Actual sowing density // plants.m-2
  real,    intent(IN)                           :: densiteger  
  real,    intent(IN)                           :: densitelev  
  real,    intent(IN)                           :: P_elmax  !> // PARAMETER // Maximum elongation of the coleoptile in darkness condition // cm // PARPLT // 1 
  real,    intent(IN)                           :: innlai   !> // OUTPUT // Index of nitrogen stress active on leaf growth // P_innmin à 1
  real,    intent(IN)                           :: P_julvernal  !> // PARAMETER // julian day (between 1 and 365) accounting for the beginning of vernalisation for perennial crops // julian day // PARPLT // 1 
  real,    intent(IN)                           :: P_jvc  !> // PARAMETER // Number of vernalizing days // day // PARPLT // 1 
  real,    intent(IN)                           :: P_jvcmini  !> // PARAMETER // Minimum number of vernalising days  // day // PARPLT // 1 
  integer, intent(IN)                           :: P_nbjgerlim  !> // PARAMETER // Threshold number of day after grain imbibition without germination lack // days // PARPLT // 1 
  integer, intent(IN)                           :: ndrpobs  
  integer, intent(IN)                           :: P_nlevlim1  !> // PARAMETER // number of days after germination decreasing the emerged plants if emergence has not occur // days // PARPLT // 1 
  integer, intent(IN)                           :: P_nlevlim2  !> // PARAMETER // number of days after germination after which the emerged plants are null // days // PARPLT // 1 
  integer, intent(IN)                           :: nlevobs  
  integer, intent(IN)                           :: nplt  
  logical, intent(IN)                           :: onarretesomcourdrp  
  real,    intent(IN)                           :: P_phobase  !> // PARAMETER // Base photoperiod  // hours // PARPLT // 1 
  real,    intent(IN)                           :: P_phosat  !> // PARAMETER // saturating photoperiod // hours // PARPLT // 1 
  real,    intent(IN)                           :: P_potgermi  !> // PARAMETER // humidity threshold from which seed humectation occurs, expressed in soil water potential  // Mpa // PARPLT // 1 
  real,    intent(IN)                           :: P_propjgermin  !> // PARAMETER // minimal proportion of the duration P_nbjgerlim when the temperature is higher than the temperature threshold P_Tdmax  // % // PARPLT // 1 
  real,    intent(IN)                           :: P_q10  !> // PARAMETER // P_Q10 used for the dormancy break calculation  // SD // PARPLT // 1 
  real,    intent(IN)                           :: P_sensiphot  !> // PARAMETER //  photoperiod sensitivity (1=insensitive) // SD // PARPLT // 1 
  real,    intent(IN)                           :: P_sensrsec  !> // PARAMETER // root sensitivity to drought (1=insensitive) // SD // PARPLT // 1 
  real,    intent(IN)                           :: somelong  
  real,    intent(IN)                           :: somger  
  real,    intent(IN)                           :: P_stdordebour  !> // PARAMETER // phasic duration between the dormancy break and the bud break  // degree.days // PARPLT // 1 
  real,    intent(IN)                           :: P_stpltger  !> // PARAMETER // Sum of development allowing germination // degree.days // PARPLT // 1 
  real,    intent(IN)                           :: P_stressdev  !> // PARAMETER // maximum phasic delay allowed  due to stresses  // SD // PARPLT // 1 
  real,    intent(IN)                           :: P_tcxstop  !> // PARAMETER // threshold temperature beyond which the foliar growth stops // degree C // PARPLT // 1
  real,    intent(IN)                           :: P_tdmax  !> // PARAMETER // Maximum threshold temperature for development // degree C // PARPLT // 1
  real,    intent(IN)                           :: P_tdmaxdeb  !> // PARAMETER // maximal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
  real,    intent(IN)                           :: P_tdmin  !> // PARAMETER // Minimum threshold temperature for development // degree C // PARPLT // 1
  real,    intent(IN)                           :: P_tdmindeb  !> // PARAMETER // minimal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
  real,    intent(IN)                           :: P_tfroid  !> // PARAMETER // optimal temperature for vernalisation // degree C // PARPLT // 1
  real,    intent(IN)                           :: P_tgmin  !> // PARAMETER // Minimum threshold temperature used in emergence stage // degree C // PARPLT // 1
  real,    intent(IN)                           :: turfac   !> // OUTPUT // Index of turgescence water stress  // 0-1
  real,    intent(IN)                           :: P_vigueurbat  !> // PARAMETER // indicator of plant vigor allowing to emerge through the crust  // between 0 and 1 // PARPLT // 1 
  integer, intent(IN)                           :: P_codefente  !> // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0 
  real,    intent(IN)                           :: P_mulchbat  !> // PARAMETER // mulch depth from which a crust occurs // cm // PARSOL // 1 
  real,    intent(IN)                           :: P_pluiebat  !> // PARAMETER // minimal rain quantity for the crust occurrence // mm day-1 // PARSOL // 1 
  integer, intent(IN)                           :: P_culturean  !> // PARAMETER // crop status 1 = over 1 calendar year ,other than 1  = on two calendar years (winter crop in northern hemisphere) // code 0/1 // P_USM/USMXML // 0 
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
  real,    intent(IN)                           :: tairveille   !> // OUTPUT // Mean air temperature the previous day // degree C
  real,    intent(IN)                           :: tcult   !> // OUTPUT // Crop surface temperature (daily average) // degree C
  real,    intent(IN), dimension(0:nbCouches)   :: tsol  
  real,    intent(IN)                           :: xmlch1   !> // OUTPUT // Thickness of mulch created by evaporation from the soil // cm
  integer, intent(IN)                           :: P_codepluiepoquet  !> // PARAMETER // option to replace rainfall by irrigation at poquet depth in the case of poquet sowing // code 1/2 // PARAMV6 // 0 
  integer, intent(IN)                           :: P_codetempfauche  !> // PARAMETER // option of the reference temperature to compute cutting sum of temperatures : upvt (1), udevair (2) // code 1/2 // PARAMV6 // 0 
  logical, intent(IN)                           :: humectation  
  integer, intent(IN)                           :: nbjhumec  
  real,    intent(IN)                           :: pluiesemis  
  real,    intent(IN)                           :: somtemphumec  
  integer, intent(IN)                           :: P_codeindetermin  !> // PARAMETER // option of  simulation of the leaf growth and fruit growth : indeterminate (2) or determinate (1) // code 1/2 // PARPLT // 0 
  integer, intent(IN)                           :: P_codelaitr  !> // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0 
  integer, intent(IN)                           :: P_codlainet  !> // PARAMETER // option of calculation of the LAI (net or gross) // code 1/2 // PARPLT // 0 
  real,    intent(IN)                           :: P_dureefruit  !> // PARAMETER // total growth period of a fruit at the setting stage to the physiological maturity // degree.days // PARPLT // 1 
  integer, intent(IN)                           :: namfobs  
  integer, intent(IN)                           :: P_nbcueille  !> // PARAMETER // number of fruit harvestings // code 1/2 // PARTEC // 0 
!  integer, intent(IN)                           :: P_nboite  !> // PARAMETER // "Number of  box  or  age class  of fruits for the fruit growth for the indeterminate crops " // SD // PARPLT // 1
  integer, intent(IN)                           :: nfloobs  
  integer, intent(IN)                           :: nlanobs  
  integer, intent(IN)                           :: nlaxobs  
  integer, intent(IN)                           :: nmatobs  
  integer, intent(IN)                           :: nrecobs  
  integer, intent(IN)                           :: nsenobs  
  real,    intent(IN)                           :: P_stdrpnou  !> // PARAMETER // Sum of development units between the stages DRP and NOU (end of  setting) // degree.days // PARPLT // 1 
  real,    intent(IN)                           :: upobs  
! PB - 03/08/2010 - Pour les modifs ML/SYL/DR de juin09
  integer, intent(IN)                           :: P_codemontaison  !> // PARAMETER // code to stop the reserve limitation from the stem elongation // code 1/2 // PARAMV6 // 0 
  logical, intent(IN)                           :: sioncoupe  

! DR 06/03/2015 ajout pour les enchainement annuel de la vigne
  integer,  intent(IN)    :: P_idebdorm, P_iwater

  real,    intent(INOUT)                        :: caljvc  
  real,    intent(INOUT)                        :: cu  
  real,    intent(INOUT), dimension(0:2)        :: demande   !> // OUTPUT // Daily nitrogen need of the plant   // kgN.ha-1.j-1
  logical, intent(INOUT)                        :: etatvernal  
  real,    intent(INOUT), dimension(0:2)        :: hauteur   !> // OUTPUT // Height of canopy // m
  real,    intent(INOUT), dimension(0:2)        :: mafrais   !> // OUTPUT // Aboveground fresh matter // t.ha-1
  real,    intent(INOUT), dimension(0:2)        :: mafraisfeuille  
  real,    intent(INOUT), dimension(0:2)        :: mafraisrec  
  real,    intent(INOUT), dimension(0:2)        :: mafraisres  
  real,    intent(INOUT), dimension(0:2)        :: mafraistige  
  real,    intent(INOUT), dimension(0:2)        :: masec   !> // OUTPUT // Aboveground dry matter  // t.ha-1
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
  real,    intent(INOUT), dimension(0:2)        :: pdsfruitfrais   !> // OUTPUT // Total weight of fresh fruits // g m-2
  real,    intent(OUT)                          :: rfpi   !> // OUTPUT // Slowing effect of the photoperiod on plant development  // 0-1
  real,    intent(OUT)                          :: rfvi   !> // OUTPUT // Slowing effect of the vernalization on plant development // 0-1
  real,    intent(INOUT)                        :: somcour   !> // OUTPUT // Cumulated units of development between two stages // degree.days
  real,    intent(INOUT)                        :: somcourdrp  !> // OUTPUT // Cumulated units of development between two reproductive stages // degree.days
  real,    intent(INOUT)                        :: somcourfauche  
  real,    intent(INOUT)                        :: somcourutp  
  real,    intent(INOUT)                        :: somtemp   !> // OUTPUT // Sum of temperatures // degree C.j
  real,    intent(OUT)                          :: stpltlev
  ! DR 18/07/2012 je rajoute la germination
  real,    intent(OUT)                          :: stpltger
  real,    intent(OUT)                          :: tdevelop  
  real,    intent(OUT)                          :: udevair   !> // OUTPUT // Effective temperature for the development, computed with TAIR // degree.days
  real,    intent(OUT)                          :: udevcult   !> // OUTPUT // Effective temperature for the development, computed with TCULT // degree.days
  real,    intent(INOUT)                        :: upvt   !> // OUTPUT // Daily development unit  // degree.days
  real,    intent(INOUT)                        :: utp  
  real,    intent(INOUT)                        :: zrac   !> // OUTPUT // Depth reached by root system // cm
  integer, intent(INOUT)                        :: maxwth  
  integer, intent(OUT)                          :: group  
  integer, intent(INOUT)                        :: ndebdes  
  real,    intent(INOUT)                        :: nfruit   !> // OUTPUT // Number of fruits in box 5 // nb fruits
  integer, intent(INOUT)                        :: nlax  
  integer, intent(INOUT)                        :: nmat  
  integer, intent(INOUT)                        :: nnou  
  integer, intent(INOUT)                        :: nsen  
  real,    intent(INOUT)                        :: P_stamflax  !> // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1 
  real,    intent(INOUT)                        :: P_stdrpdes  !> // PARAMETER // phasic duration between the DRP stage and the beginning of the water fruit dynamics  // degree.days // PARPLT // 1 
  real,    intent(INOUT)                        :: P_stdrpmat  !> // PARAMETER // Sum of development units between the stages DRP and MAT // degree.days // PARPLT // 1 
  real,    intent(OUT)                          :: stdrpsen  
  real,    intent(OUT)                          :: P_stflodrp  !> // PARAMETER // phasic duration between FLO and DRP (only for indication) // degrés.jours // PARPLT // 1 
  real,    intent(INOUT)                        :: P_stlaxsen  !> // PARAMETER // Sum of development units between the stages LAX and SEN // degree.days // PARPLT // 1 
  real,    intent(INOUT)                        :: P_stlevamf  !> // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1 
  real,    intent(INOUT)                        :: P_stlevdrp  !> // PARAMETER // Sum of development units between the stages LEV and DRP // degree.days // PARPLT // 1 
  real,    intent(INOUT)                        :: stlevflo  
  real,    intent(OUT)                          :: stmatrec  
  real,    intent(INOUT)                        :: P_stsenlan  !> // PARAMETER // Sum of development units between the stages SEN et LAN // degree.days // PARPLT // 1 
  real,    intent(OUT)                          :: upvtutil  

  ! pour recolte
  integer, intent(IN)                           :: P_codrecolte  !> // PARAMETER // harvest mode : all the plant (1) or just the fruits (2) // code 1/2 // PARTEC // 0 
  real,    intent(IN)                           :: h2orec   !> // OUTPUT // Water content of harvested organs // %
  real,    intent(IN)                           :: P_sucrerec  !> // PARAMETER // minimal sugar rate at harvest // g sucre g-1 MF // PARTEC // 1 
  real,    intent(IN)                           :: P_CNgrainrec  !> // PARAMETER // minimal grain nitrogen content for harvest  // 0-1 // PARTEC // 1 
  real,    intent(IN)                           :: P_huilerec  !> // PARAMETER // minimal oil content allowed for harvest // g huile g-1 MF // PARTEC // 1 
  real,    intent(IN)                           :: sucre   !> // OUTPUT // Sugar content of fresh harvested organs // % (of fresh weight)
  real,    intent(IN)                           :: huile   !> // OUTPUT // Oil content of fresh harvested organs // % (of fresh weight)
  real,    intent(IN)                           :: teaugrain  
!  real,    intent(IN)                           :: sucreder
!  real,    intent(IN)                           :: huileder
  real,    intent(IN)                           :: P_h2ofrvert  !> // PARAMETER // water content of fruits before the beginning of hydrous evolution (DEBDESHYD) // g water g-1 MF // PARPLT // 1 
  integer, intent(IN)                           :: P_codeaumin  !> // PARAMETER // harvest as a function of grain/fruit water content // code 1/2 // PARTEC // 0 
  real,    intent(IN)                           :: P_h2ograinmin  !> // PARAMETER // minimal water content allowed at harvest // g eau g-1 MF // PARTEC // 1 
  real,    intent(IN)                           :: P_h2ograinmax  !> // PARAMETER // maximal water content allowed at harvest // g water g-1 MF // PARTEC // 1 
  real,    intent(IN)                           :: P_deshydbase  !> // PARAMETER // phenological rate of evolution of fruit water content (>0 or <0) // g water.g MF-1.degree C-1 // PARPLT // 1
  real,    intent(IN)                           :: CNgrain   !> // OUTPUT // Nitrogen concentration of grains  // %
  integer, intent(IN)                           :: P_cadencerec  !> // PARAMETER // number of days between two harvests // day // PARTEC // 1 

  integer, intent(INOUT)                        :: jdepuisrec  
  real,    intent(INOUT)                        :: pdsfruit   !> // OUTPUT // Weight of fruits in box 3 // g m-2
  integer, intent(INOUT)                        :: nbrecolte  
  integer, intent(OUT)                          :: nrecint  
!! dr 22/12/2010 on a ajouté la dimension
!  real,    intent(OUT),  dimension(0:2,0:20)    :: rdtint
  real,    intent(OUT)                          :: rdtint  
  real,    intent(OUT)                          :: teauint  
  real,    intent(OUT)                          :: nbfrint  

! PB - Pour les modifs SYL/DR/ML de juin09
  integer, intent(INOUT)                        :: onestan2  
  real,    intent(INOUT)                        :: somcourmont   !> // OUTPUT // Cumulated units of development from the start of vernalisation // degree.days
  integer, intent(INOUT)                        :: nmontaison  





!: Variables locales
  real :: tdev  
  real,dimension(24) :: thor  

  !n = sc%n ! juste pour que ça soit plus facile à écrire



if (n.gt.15.and.n.lt.25)then
!   write(245,*)P_latitude,phoi,tmax,tmin,tmin_demain,trr,P_codeh2oact,P_codeinitprec,P_codeinnact,codeulaivernal,P_psihucc, &
!                    P_psihumin, P_codcueille,P_codefauche,P_densitesem,P_profsem,P_variete,P_ampfroid,P_belong,P_celong,         &
!                    P_codebfroid,P_codedormance,P_codegdh, P_codegdhdeb,P_codegermin,P_codehypo,P_codeperenne,P_codephot,        &
!                   P_codeplante,P_coderetflo,P_codetemp,coeflev,cu_min,cu_veille, densite,densiteger,densitelev,P_elmax,        &
!                    innlai,P_julvernal,P_jvc,P_jvcmini,P_nbjgerlim,ndrpobs,P_nlevlim1,P_nlevlim2,nlevobs,nplt,onarretesomcourdrp,&
!                    P_phobase,P_phosat,P_potgermi,P_propjgermin,P_q10,P_sensiphot,P_sensrsec,somelong,somger,P_stdordebour,      &
!                    P_stpltger,P_stressdev,P_tcxstop,P_tdmax,P_tdmaxdeb,P_tdmin,P_tdmindeb,P_tfroid,P_tgmin,turfac,P_vigueurbat, &
!                    P_codefente,P_mulchbat,P_pluiebat,P_culturean,nbCouches,dacouche,hucc,humin,hur,jjul,n,nbjanrec,nbjsemis,    &
!                    numcult,tairveille,tcult,tsol,xmlch1,P_codepluiepoquet,P_codetempfauche,humectation,nbjhumec,pluiesemis,     &
!                    somtemphumec,P_codeindetermin,P_codelaitr,P_codlainet,P_dureefruit,namfobs,P_nbcueille,P_nboite,nfloobs,     &
!                    nlanobs, nlaxobs,nmatobs,nrecobs,nsenobs,P_stdrpnou,upobs,P_codemontaison,sioncoupe,                         &
!                    caljvc,cu,demande,etatvernal,hauteur,mafrais,mafraisfeuille,mafraisrec,mafraisres,mafraistige,masec,namf,    &
!                    ndebdorm,ndrp,nfindorm,nflo,nger,nlan,nlev,nrec,nrecbutoir,pdsfruitfrais,rfpi,rfvi,somcour,somcourdrp,       &
!                    somcourfauche,somcourutp,somtemp,stpltlev,tdevelop,udevair,udevcult,upvt,utp,zrac,maxwth,group,ndebdes,      &
!                    nfruit,nlax,nmat,nnou,nsen,P_stamflax,P_stdrpdes,P_stdrpmat,stdrpsen,P_stflodrp,P_stlaxsen,P_stlevamf,       &
!                    P_stlevdrp,stlevflo,stmatrec,P_stsenlan,upvtutil, P_codrecolte,h2orec,P_sucrerec,P_CNgrainrec,P_huilerec,    &
!                    sucre,huile,teaugrain,sucreder,huileder,P_h2ofrvert,P_codeaumin, P_h2ograinmin,P_h2ograinmax,P_deshydbase,   &
!                    CNgrain,P_cadencerec, jdepuisrec,pdsfruit,nbrecolte,nrecint,rdtint,teauint,nbfrint,onestan2,somcourmont,     &
!                    nmontaison
endif


!: Pour les annuelles => pas de développement avant le semis
!- DR - 03/05/06
!- dans le cas de repousse du semis par decisionsemis on ne commence pas les cumul d'unite
  if (P_codeperenne == 1 .and. ( n <= nplt .or. nplt == -999 )) return


!: On remet à zéro les masses sèches, masses fraiches, masses des fruits, la hauteur de la plante, les variables de fixation (et le zrac ?)
!: Marie et Domi - 10/10/03 - bug signalé par Emmanuelle Sauboua
!- N'était jamais testé dans 'recolte'
  if (P_codeperenne == 1 .and. P_codcueille == 1) then
    if (n == nrec+1) then
      masec(:)          = 0.
      zrac              = 0.
      mafrais(:)        = 0.
      pdsfruitfrais(:)  = 0.
      hauteur(:)        = 0. ! DR - 22/10/03
      demande(:)        = 0. ! PB - 03/05/2004 - remise à zéro des variables de fixation
!: DR - 13/01/06 - remise à zero sinon mafrais ne revient pas nul
      mafraisfeuille(:) = 0.
      mafraistige(:)    = 0.
      mafraisres(:)     = 0.
      mafraisrec(:)     = 0.
    endif
!: Pour les cultures annuelles (P_codeperenne=1) moissonnées (P_codcueille=1) : pas de développement après la récolte.
    if (n > nrec .and. nlan > 0 .and. nrec > 0) return
  endif

!: PB - 11/03/2005 - On remet pdsfruitfrais à zéro après la récolte.
!: PB - 07/08/2009 - Attention, qd n=1 et nrec=0, cette condition peut être vraie alors qu'on est pas après la récolte.
!                    TODO: Ajouter un test sur nrec est nul ?
  if (P_codeperenne == 2 .and. P_codcueille == 2 .and. n == (nrec+1) .and. P_codeinitprec == 2) then
    pdsfruitfrais(:) = 0.
  endif

! TODO : GERER LE CAS DES CULTURES ANNUELLES  A RECOLTES MULTIPLES



! EFFET TEMPERATURE (calcul udevair/udevcult)
! -------------------------------------------

  !: Unités journalières
  !- NB - 07/07/06: Application du seuil thermique négatif P_TCXSTOP au développement
  if (P_codegdh == 1) then
    udevair = calcul_UDev(tairveille,P_tdmax,P_tdmin,P_tcxstop)
    udevcult = calcul_UDev(tcult,P_tdmax,P_tdmin,P_tcxstop)
  endif

  !: Unités horaires
  if (P_codegdh == 2) then

    !: Pour l'instant que températures air autorisées
    if (P_codetemp == 2) then
      call EnvoyerMsgHistorique(49)
      !stop
      print *,'EnvoyerMsgHistorique(49)'
      call exit(9)
    endif

    !: 1) Reconstitution des températures horaires.
    !-    Pour l'instant on n'autorise uniquement le cas par températures air.
    thor = calcul_TemperaturesHoraires(tmin,tmin_demain,tmax)

    !: 2) Calcul des gdh
    udevair = calcul_GDH(thor,P_tdmin,P_tdmax)
    udevcult = udevair

!-- DR et gnack (ca faisait un moment ...) 14/03/08
!-- dans le cas des plantes qui ont une phase de dormance
!-- si cette phase n'est pas realisée (fin de dormance) on ne cumule rien
!--    if (nfindorm == 0) then
!-- maintenant qu'on a tout remis d'equerre avec debour ca ne sert à rien
!--    if (nfindorm0 == 0) then
!--    udevcult=0.0
!--    udevair=0.0
!--    endif

  endif

  ! NB - le 01/09/06:
  ! effet retard du stress hydrique en phase végétative appliqué directement
  ! sur les udevair (ou udevcult) de façon à ce qu'il agisse sur le développement
  ! P_phénologique et également sur la durée de vie.
  ! modif pour Sophie pour permettre action de P_stressdev pendant tout le cycle et
  ! en n'utilisant qu'un seul des deux stress
  ! DR et ML et La Soso - 15/08/07 - y'avait un bug introduit par sophie et on sait
  ! pas qui. On passait la tout le temps meme quand P_coderetflo=2
  !       if (P_coderetflo == 1.and.P_codeinnact == 1 .or. P_codeh2oact == 1)
  if (P_coderetflo == 1 .and. (P_codeinnact == 1 .or. P_codeh2oact == 1)) then
 !!!MODIF HISAFE 1 : suppression des chaines de caractères
 !!!   if (P_codeplante == 'qui' .or. ndrp == 0) then
    if (P_codeplante == 25 .or. ndrp == 0) then
      if (P_codetemp == 2) then
        udevcult = udevcult * (P_stressdev * min(turfac,innlai) + 1 - P_stressdev)
      else
        udevair = udevair * (P_stressdev * min(turfac,innlai) + 1 - P_stressdev)
      endif
    endif
  endif


! BESOINS EN FROID (calcul de rfvi)
! ---------------------------------
  if (P_codebfroid == 1) rfvi = 1.0


  !: Calcul de l'effet vernalisation a partir de la germination
  !- ou en cours de culture après la date P_julvernal
  !- calculs données intermédiaires
  if (P_codebfroid == 2) then
    !: Choix de la température (tdev) pour les besoins en froid
    if (P_codetemp == 2) then
      tdev = tcult
    else
      tdev = tairveille
    endif
    call Stics_Develop_bfroid2(jjul,n,P_tfroid,P_ampfroid,P_julvernal,P_jvc,P_jvcmini,P_codeperenne,  &
                               nger,namf,numcult,nbjsemis,tdev,P_codemontaison,P_culturean,   &
                               P_codeinitprec,nbjanrec,                                     &
                               nrecbutoir,rfvi,maxwth,etatvernal,caljvc,onestan2)

  endif

  if (P_codebfroid == 3) then
    call Stics_Develop_bfroid3(P_codedormance,cu_min,cu_veille,n,P_jvc,P_q10,tmin,tmax,thor, &
                               etatvernal,cu,rfvi,ndebdorm,nfindorm,nlev,P_idebdorm, P_iwater, nbjsemis,numcult)



  endif


! PHOTOPERIODE (calcul de rfpi)
! -----------------------------
  if (P_codephot == 1) then
    !numdate = MOD(jjul,nbjsemis+1)
    !numdate = jjul
    !if (jjul > nbjsemis) numdate = jjul - nbjsemis

    !call photpd(P_latitude,numdate,daylen,phoi)

    if (ndrp /= 0 .or. n > ndrpobs) then
      rfpi = 1.0
    else if (P_codebfroid /= 3 .and. (nlev == 0 .or. n < nlev)) then ! pour les ligneux, photopériode active à partir de la fin de dormance
      rfpi = 1.0
    else if (P_codebfroid == 3 .and. nfindorm == 0) then
      rfpi = 1.0
    else
      ! TODO: on remplace l'appel à photpd par ses résultats. photpd n'est appelé qu'une fois par pas de temps
      rfpi = cRFPI(P_sensiphot,P_phosat,P_phobase,phoi)
    endif
  else
    rfpi = 1.0
  endif


! UNITE DE DEVELOPPEMENT (calcul de upvt)
! ---------------------------------------------------
  if (P_codetemp == 2) then
    ! DR 17/08/06 si on est sur une perenne et qu'une dormance a deja ete faite
    ! on ne fait plus jouer la vernalisation sur le calcul des stades
    ! implicitement n'est utilisé que si on est en enchainement
    !! DR et ML 26/09/2013 il manquait le test sur codebfroid si bien que le ble passait par la ligne et ne faisait de vernalisation apres levée
    !! DR et ML et IGCA 26/09/2013 on enleve le test sur la dormance qui ne sert à rien la (on cumule du chaud)
!    if (P_codebfroid == 3 .and. P_codedormance == 3 .and. nlev > 0) then
   if (P_codebfroid == 3 .and. nlev > 0) then
      upvt = udevcult * rfpi
    else
      upvt = udevcult * rfpi * rfvi
    endif

    ! option codelaisansvernal la vernalisation ne joue pas sur la ulai
    if (codeulaivernal == 0) utp = udevcult * rfpi
  else
    ! DR 17/08/06 si on est sur une perenne et qu'une dormance a deja ete faite
    ! on ne fait plus jouer la vernalisation sur le calcul des stades
    ! implicitement n'est utilisé que si on est en enchainement
    ! NB le 21/08/07 bug
    !! DR et ML et IGCA 26/09/2013 on enleve le test sur la dormance qui ne sert à rien la (on cumule du chaud)
    if (P_codebfroid == 3 .and. nlev > 0) then
      upvt = udevair * rfpi
    else
      upvt = udevair * rfpi * rfvi
    endif

    if (codeulaivernal == 0) utp = udevair * rfpi
  endif


  !: somcour = cumul d'unité entre deux stades végétatifs
  somcour = somcour + upvt

  !: somcourdrp = cumul d'unité entre deux stades reproducteurs
  !- à partir de la levee
  if (nlev > 0) somcourdrp = somcourdrp + upvt

  ! dr 13/01/06: dans le cas de la prairie on ne cumule plus d'upvt si on coupe apres amf
  ! dr 17/11/05: si on coupe apres amf et avant drp on ne pourra plus faire d'epi
  !              donc on arrete le developpement des stades reproducteurs
  if (P_codefauche == 1 .and. onarretesomcourdrp) then
    if (namf /= 0 .and. (ndrp == 0 .or. nflo == 0)) then
      somcourdrp = somcourdrp - upvt
    endif
  endif

  if (codeulaivernal == 0) then
    somcourutp = somcourutp + utp
  endif


  !: calcul d'une somme de températures même
  !- pour les plantes vernalo-photo-sensibles
  !- pour les calculs de sénescence et de nombre  de feuilles
  !-
  !- NB le 08/05 on remplace la somme des températures pour
  !- la sénescence par un P_Q10 pour que le vieillissement soit effectif meme
  !- en conditions froides
  if (P_codetemp == 2) then
    tdevelop = 2.0 ** (udevcult / 10.)
  else
    tdevelop = 2.0 ** (udevair / 10.)
  endif

  somtemp = somtemp + tdevelop
  if (P_codetempfauche == 1) then
    somcourfauche = somcourfauche + upvt
  else
    somcourfauche = somcourfauche + udevair
  endif
! write(245,*)'dev',n,tdevelop,rfvi,rfpi

! CALCUL DES STADES
! -----------------

! DR et ML et SYL 16/06/09
! calcul de la date de montaison et du jour d'entree en
! vernalisation de la prairie perenne
  if (P_codemontaison == 1)then
! ####
! entrée en vernalisation des fourrages (pérenne)
! NB le 07/03/08
    if (P_codebfroid == 2 .and. P_codeperenne == 2) then
      somcourmont = somcourmont + upvt
    ! PB - 03/08/2010 - je remplace jul par jjul qui correspond à n+P_iwater-1
      if (jjul+((onestan2-1)*nbjsemis) == P_julvernal) then
        somcourmont = 0.0
      endif
! ** stade début montaison : après vernalisation
! unique cycle reproducteur de l'année
      if (somcourmont >= P_stlevamf .and. jjul+((onestan2-1)*nbjsemis) > P_julvernal)then
        nmontaison = n
        namf = n
! DR et ML et SYL 16/06/09 - on supprime nvernal qui ne sert à rien
!--         nvernal=0
        somcourmont=0.0
        onestan2 = 1
      endif

! si la coupe intervient après le stade montaison alors on remet
! le stade à 0
      if (sioncoupe) nmontaison=0
    endif
! ####
  endif
! DR et ML et SYL 16/06/09 FIN

  !: la levée
  if (nlev == 0) &
    call levee(P_codeperenne,P_codebfroid,nlevobs,nger,P_codegdhdeb,P_codetemp,P_codegermin,P_codefente,    &
               P_codepluiepoquet,P_codehypo,P_nbjgerlim,tmin,tmin_demain,tmax,n,nplt,nrec,P_nlevlim1,   &
               P_nlevlim2,P_tdmindeb,P_tdmaxdeb,rfvi,rfpi,P_profsem,P_stdordebour,P_tgmin,                  &
               P_stpltger,P_sensrsec,P_psihucc,P_psihumin,P_potgermi,P_tdmax,P_propjgermin,P_densitesem,        &
               pluiesemis,P_pluiebat,P_mulchbat,xmlch1,P_vigueurbat,P_celong,P_belong,P_elmax,nbCouches,    &
!               tsol,hur,humin,hucc,trr,dacouche,jjul,                                           &  ! DR 20/07/2012 jjul ne srt pas
               tsol,hur,humin,hucc,trr,dacouche,                                         &  !
               udevair,udevcult,upvt,densiteger,densite,coeflev,densitelev,zrac,somelong,       &
               somger,nlev,humectation,nbjhumec,somtemphumec,somcour)

! DR 18/07/2012 je rajoute la germination
    if (n  == nger) then   ! si on est le jour de la levée
      stpltger = somcour ! on affecte le cumul de température entre le semis et la levée
! DR 18/07/2012 pour la germination on affiche juste somcour sans le reinitialiser
!      somcour = 0.0        ! on remet à zéro le cumul de température courant.
!      if (P_codeperenne == 1) somcourdrp = 0.0 ! NB le 23/03 pour les pérenne début du décompte drp à la levée de dormance
!      if (P_codefauche == 2) somcourfauche = 0.0
!      if (codeulaivernal == 0) somcourutp = 0.0
    endif


  if (nlevobs == 999) then ! pas d'observation pour la levée
    if (n  == nlev) then   ! si on est le jour de la levée
      stpltlev = somcour ! on affecte le cumul de température entre le semis et la levée
      somcour = 0.0        ! on remet à zéro le cumul de température courant.
       if (P_codeperenne == 1) somcourdrp = 0.0 ! NB le 23/03 pour les pérenne début du décompte drp à la levée de dormance
      if (P_codefauche == 2) somcourfauche = 0.0
      if (codeulaivernal == 0) somcourutp = 0.0
    endif
  else                       ! levée observée
    if (n  == nlevobs) then  ! si on est le jour de la levée observée
      nlev = nlevobs         ! on force nlev
      if (nger <= 0) nger = nlev  ! si la germination n'a pas encore été affectée, on la force au jour de la levée
      ! réajustement du parcours de dl
      stpltlev = somcour       ! on affecte le cumul de température entre le semis et la levée
      somcour = 0.0            ! on remet à zéro le cumul de température courant.'
      if (P_codeperenne == 1) somcourdrp = 0.0 ! NB le 23/03 pour les pérenne début du décompte drp à la levée de dormance
      if (P_codefauche == 2) somcourfauche = 0.0
      if (codeulaivernal == 0) somcourutp = 0.0
    endif
  endif




! STADES VEGETATIFS

  !: stade amf
  if (namfobs == 999) then
    if (somcour >= P_stlevamf .and. namf == 0 .and. nlev > 0) then
     ! write(*,*)'namf avant dans develop', namf,somcour
      namf = n
     ! write(*,*)'namf dans develop', namf,somcour
      P_stlevamf = somcour
      somcour = 0.0
      if (codeulaivernal == 0) somcourutp = 0.0
    endif
  else
    if (n == namfobs) then
      namf = namfobs
      ! réajustement du parcours de dl
      P_stlevamf = somcour
      somcour=0.0
      if (codeulaivernal == 0) somcourutp = 0.0
      if (namf < nlev .or. nlev == 0) then
        call EnvoyerMsgHistorique(46)
           print *,'EnvoyerMsgHistorique(46)'
        !stop
        call exit(9)
      endif
    endif
  endif

  !: stade laimax
  if (nlaxobs == 999) then
    if (somcour >= P_stamflax .and. nlax == 0 .and. namf > 0) then
      nlax = n
      P_stamflax = somcour
      somcour = 0.0
      if (codeulaivernal == 0) somcourutp = 0.0
    endif
  else
    if (n == nlaxobs) then
      nlax = nlaxobs
      ! réajustement du parcours de dl
      P_stamflax = somcour
      somcour = 0.0
      if (codeulaivernal == 0) somcourutp = 0.0
      if (nlax < namf .or. namf == 0) then
        call EnvoyerMsgHistorique(41)
        !stop
        call exit(9)
      endif
    endif
  endif


  !: stade sen
  !- uniquement si P_codlainet=1
  if (P_codlainet == 1) then
    if (nsenobs == 999) then
      if (somcour >= P_stlaxsen .and. nsen == 0 .and. nlax > 0) then
        nsen = n
        P_stlaxsen = somcour
        somcour = 0.0
        if (codeulaivernal == 0) somcourutp = 0.0
      endif
    else
      if (n == nsenobs) then
        ! réajustement du parcours de dl
        stdrpsen = somcour
        nsen = nsenobs

        if (nsen < nlax .or. nlax == 0) then
          call EnvoyerMsgHistorique(42)
           print *,'EnvoyerMsgHistorique(42)'
        !stop
          call exit(9)
        endif
        somcour = 0.0
        if (codeulaivernal == 0) somcourutp = 0.0
      endif
    endif
  endif

!: version 4.0 suppression du stade fir


  !: stade lan
  !- NB - le 22/04 - si colainet=2 plus de stade lan
  if (P_codelaitr == 1 .and. P_codlainet == 1 .or. P_codelaitr == 2) then
    if (nlanobs == 999) then
      if (somcour >= P_stsenlan .and. nlan == 0 .and. nsen > 0) then
        nlan = n
        P_stsenlan = somcour
        somcour = 0.0
        if (codeulaivernal == 0)somcourutp = 0.0
      endif
    else
      if (n == nlanobs) then
        ! réajustement du parcours de dl
        P_stsenlan = somcour
        nlan = nlanobs
        somcour = 0.0
        if (codeulaivernal == 0) somcourutp = 0.0
        if (nlan < nsen .or. nsen == 0) then
          call EnvoyerMsgHistorique(43)
             print *,'EnvoyerMsgHistorique(43)'
        !stop
        call exit(9)
        endif
      endif
    endif
  endif


!: STADES REPRODUCTEURS

  !: stade flo
  if (nfloobs == 999) then
    if (somcourdrp >= stlevflo .and. nflo == 0) then
      nflo = n
      stlevflo = somcourdrp

!--      somcourdrp = 0.0 ! domi 04/04/01  pb canne on supprime la remise à zero
    endif
  else
    if (n == nfloobs) then
      nflo = nfloobs
      ! réajustement du parcours de dl
      stlevflo = somcourdrp
      ! DR et ML 21/01/08 on teste le pb des sommes de temp foireuses
!--      somcourdrp = 0.0
    endif
  endif


  !: stade drp
  if (ndrpobs == 999) then
    if (somcourdrp >= P_stlevdrp .and. ndrp == 0) then
      ndrp = n

! NB le 29/3
!      P_stlevdrp=somcourdrp
!  domi 04/04/01 on fait un essai
! DR et ML 21/01/08:
! SUITE AUX PBS DE calcul des sommes de temp dans le bilan
! lorsque on force flo ou drp => 2 modifs on enleve la remise à zero de somcourdrp si
! flo est observe (je me demande bien pourquoi on faisait ca)
! et P_stflodrp =somcourdrp-stlevflo
!--      P_stflodrp=somcourdrp
      P_stflodrp = somcourdrp - stlevflo
      somcourdrp = 0.0
!--      if (ndrp  = = nflo .or. nflo == 0) then
!--        call EnvoyerMsgHistorique(47)
!--        stop
!--      endif
    endif
  else
    if (n == ndrpobs) then
      ndrp = ndrpobs
      ! réajustement du parcours de dl
      ! NB le 29/3
      !--      P_stlevdrp = somcourdrp
      ! domi 04/04/01  essai y'a un pb dans les sommes flo
      !--      P_stflodrp = somcourdrp
      !--      P_stflodrp = somcourdrp
      P_stflodrp = somcourdrp-stlevflo
      somcourdrp = 0.0
      if (ndrp < nflo .or. nflo == 0) then
        call EnvoyerMsgHistorique(47)
           print *,'EnvoyerMsgHistorique(47)'
        !stop
        call exit(9)
      endif
    endif
  endif

  !: stade fin de nouaison pour la mise en place des fruits
  if (P_codeindetermin == 2) then
    if (somcourdrp >= P_stdrpnou .and. nnou == 0 .and. ndrp > 0) nnou = n
  endif

  !: stade mat
  if (nmatobs == 999) then
    if (P_codeindetermin == 1) then
      if (somcourdrp >= P_stdrpmat .and. nmat == 0 .and. ndrp > 0) then
        nmat = n
        P_stdrpmat = somcourdrp
      endif
    else
      ! pour les indéterminées la maturité finale correspond à l'ensemble des P_nboite-1 vides
      ! Nb le 01/05 si P_nbcueille = 1
      ! si P_nbcueille = 2 : la maturité correspond au début de remplissage de la dernière boite
      if (P_nbcueille == 1) then
!--            if (nbfruit == 0.0 .and. nmat == 0 .and.
        ! 12/07/06  DR et IGC nous avons changé la condition de calcul de la date
        ! de maturité. Avant il calculait celle-ci en fonction du nombre de grains.
        ! Maintenant, on calcule en focntion de la durée de fruits.
        if (somcourdrp > P_dureefruit .and. nmat == 0 .and. n > ndrp .and. ndrp > 0) then
          nmat = n
          P_stdrpmat = somcourdrp
        endif
      else
        if (nfruit > 0.0 .and. nmat == 0 .and. n > ndrp .and. ndrp > 0) then
          nmat = n
          P_stdrpmat = somcourdrp
        endif
      endif
    endif
  else
    if (n == nmatobs) then
      nmat = nmatobs
      ! réajustement du parcours de dl
      P_stdrpmat = somcourdrp
      if (nmat < ndrp .or. ndrp == 0) then
        call EnvoyerMsgHistorique(44,ndrp)
           print *,'EnvoyerMsgHistorique(44)'
        !stop
        call exit(9)
      endif
    endif
  endif

  !: stade rec
  if (nrecobs == 999) then
    ! détemination de la date de récolte par la teneur en eau des grains à partir de la maturité
!--     if (teaugrain == h2ograin .and. nrec == 0) then
!--       nrec = n
!--       stmatrec = somcourdrp-P_stdrpmat
!--       group = gpreco
!--       write(*,*) 'rec',n,somcour,stmatrec,upvt
!--     endif

    !: Récolte
    !- PB - 18/01/2005 - pas de récolte qd culture fauchée.
    if (P_codefauche /= 1) &
       call recolte(n,ndrp,P_codrecolte,nmat,P_variete,P_nbcueille,h2orec,P_sucrerec,P_CNgrainrec,        &
!  P_huilerec,sucre,huile,teaugrain,sucreder,huileder,P_h2ofrvert,P_codeaumin,    & ! 23/07/2012 hileder et sucreder non utilisés
                    P_huilerec,sucre,huile,teaugrain,P_h2ofrvert,P_codeaumin,     & ! 23/07/2012 hileder et sucreder non utilisés
                    P_h2ograinmin,P_h2ograinmax,P_deshydbase,somcourdrp,P_stdrpmat,CNgrain,P_cadencerec,  &
                    nrec,jdepuisrec,stmatrec,group,pdsfruit,nrecint,rdtint,teauint,nbfrint,     &
                    nfruit,nbrecolte)!,P_coderecolteassoc,P_codeperenne,nrec_assoc)

!  write(*,*)'** dans develop apres recolte ',rdtint,nbrecolte

  else
    if (n == nrecobs) then
      nrec = nrecobs

      ! réajustement du parcours de dl
      stmatrec = somcourdrp - P_stdrpmat
      group = P_variete
      if (nrec < nmat .or. nmat == 0) then
        call EnvoyerMsgHistorique(45)
           print *,'EnvoyerMsgHistorique(45)'
        ! DR et IGCA le 11/04/06: on enleve le stop
        ! pour la vigne on peut recolter meme si on est pas à mat
        ! je mets un message dans bilan
!--       stop
      endif
    endif
  endif


!: Affectation des bonnes valeurs d'unités de développement
!- utilisées pour le calcul du LAI
  upvtutil = upvt

  if (nlevobs /= 999 .and. n <= nlevobs) then
    upvtutil = upobs
  endif

  if (namfobs /= 999 .and. n <= namfobs .and. n > nlev) then
    upvtutil = upobs
  endif

  if (nlaxobs /= 999 .and. n <= nlaxobs .and. n > namf) then
    upvtutil = upobs
  endif

  if (nsenobs /= 999 .and. n <= nsenobs .and. n > nlax) then
    upvtutil = upobs
  endif

  if (nlanobs /= 999 .and. n <= nlanobs .and. n > nsen) then
    upvtutil = upobs
  endif

  if (nlevobs == 999 .and. nlev == 0) then
    upvtutil = upvt
  endif

  if (namfobs == 999 .and. nlev > 0 .and. namf == 0) then
    upvtutil = upvt
  endif

  if (nlaxobs == 999 .and. namf > 0 .and. nlax == 0) then
    upvtutil = upvt
  endif

  if (nsenobs == 999 .and. nlax > 0 .and. nsen == 0) then
    upvtutil = upvt
  endif

  if (nlanobs == 999 .and. nsen > 0 .and. nlan == 0) then
    upvtutil = upvt
  endif


! 26/09/06 pour inaki on sort le test ici nrecbutoir ne sert qu'a declencher la recolte
! inaki teste et on voit
!      if (n == nrecbutoir) then
!        if (nrec == 0) then
!          group = -1
!          nrec = nrecbutoir
!        endif
!  endif

! DR et FR 15/04/2016 pour les prairies en series climatiques enchainees on ne veut pas reinitiliser les stades a recbutoir
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!if(n >= nrecbutoir .and. P_codeplante.eq.'fou'.and. P_codeinitprec == 2)then
if(n >= nrecbutoir .and. P_codeplante == 2 .and. P_codeinitprec == 2)then
  ! write(*,*)'on est à la fin mais on met pas nerecbutoir'
else
! ** Determination du groupe de precocite et effet de la date butoir
! DR 06/01/06 ajout du test sur P_culturean = 1 pour les perennes sur une portion de leur cycle
  if (n >= nrecbutoir .and. (P_codeperenne /= 2 .or. P_codeinitprec /= 2 .or. P_culturean == 1)) then
! NB et IG le 23/09/06 suppression du test pour les cultures sur plusieurs années
!      if (n == nrecbutoir) then
    if (nrec == 0) then
      group = -1
      nrec = nrecbutoir
    endif
    if (nlev == 0) nlev = nrecbutoir
    if (nlev > 0 .and. namf == 0) then
      namf = nrecbutoir
      P_stlevamf = somcour
      somcour = 0.0
    endif
    if (namf > 0 .and. nlax == 0) then
      nlax = nrecbutoir
      P_stamflax = somcour
      somcour = 0.0
    endif
    if (nlax > 0 .and. nsen == 0) then
      nsen = nrecbutoir
      P_stlaxsen = somcour
      somcour = 0.0
    endif
    if (nsen > 0 .and. nlan == 0) then
      nlan = nrecbutoir
      P_stsenlan = somcour
      somcour = 0.0
    endif
! ** NB le 26/03   (floraison)
    if (nflo == 0) then
      nflo = nrecbutoir
      stlevflo = somcourdrp
      somcourdrp = 0.0
    endif
    if (ndrp == 0) then
      ndrp = nrecbutoir
      P_stlevdrp = somcourdrp
      somcourdrp = 0.0
    endif
! NB le 25/08/04 traitement debdes en cas de recolte butoir
    if (ndebdes == 0) then
! dr 01/12/2014 si le stade ndebdes n'est pas realisé on le force plutot à recolte
!      ndebdes = nrecbutoir
      ndebdes = nrec
      P_stdrpdes = somcourdrp
    endif
!
    if (ndrp > 0 .and. nmat == 0) then
      nmat = nrecbutoir
! NB le 25/08/04 réactivation ligne suivante
      P_stdrpmat = somcourdrp
    endif
  endif
endif

! ** group = -1 signifie pas assez de somme de temperatures

!write(*,*) '=======nmat',nmat

return
end subroutine develop2

!======================================================================================!
!======================================================================================!
!======================================================================================!

!> Routine de calcul des besoins en froid pour le P_codebfroid = 2
!>
!! Description :
!<
subroutine Stics_Develop_bfroid2(jjul,n,P_tfroid,P_ampfroid,P_julvernal,P_jvc,P_jvcmini,P_codeperenne,nger,namf,   & ! IN
                                 numcult,nbjsemis,tdev,P_codemontaison,P_culturean,P_codeinitprec,nbjanrec,       &  ! IN
                                 nrecbutoir,rfvi,maxwth,etatvernal,caljvc,onestan2)                                  ! OUT & INOUT

USE Besoins_en_froid
USE Messages

  implicit none

!: ARGUMENTS
! IN
  integer, intent(IN)    :: jjul  
  integer, intent(IN)    :: n  
  real,    intent(IN)    :: P_tfroid  !> // PARAMETER // optimal temperature for vernalisation // degree C // PARPLT // 1
  real,    intent(IN)    :: P_ampfroid  !> // PARAMETER // semi thermal amplitude thermique for vernalising effect // degree C // PARPLT // 1
  real,    intent(IN)    :: P_julvernal  !> // PARAMETER // julian day (between 1 and 365) accounting for the beginning of vernalisation for perennial crops // julian day // PARPLT // 1 
  real,    intent(IN)    :: P_jvc  !> // PARAMETER // Number of vernalizing days // day // PARPLT // 1
  real,    intent(IN)    :: P_jvcmini  !> // PARAMETER // Minimum number of vernalising days  // day // PARPLT // 1 
  integer, intent(IN)    :: P_codeperenne  !> // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0 
  integer, intent(IN)    :: nger  
  integer, intent(IN)    :: namf  
  integer, intent(IN)    :: numcult  
  integer, intent(IN)    :: nbjsemis  
  real,    intent(IN)    :: tdev  
  integer, intent(IN)    :: P_codemontaison  !> // PARAMETER // code to stop the reserve limitation from the stem elongation // code 1/2 // PARAMV6 // 0 
  integer, intent(IN)    :: P_culturean  !> // PARAMETER // crop status 1 = over 1 calendar year ,other than 1  = on two calendar years (winter crop in northern hemisphere) // code 0/1 // P_USM/USMXML // 0 
  integer, intent(IN)    :: P_codeinitprec  !> // PARAMETER // reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0 
  integer, intent(IN)    :: nbjanrec  

! OUT
  integer, intent(OUT)   :: nrecbutoir  
  real,    intent(OUT)   :: rfvi   !> // OUTPUT // Slowing effect of the vernalization on plant development // 0-1

! INOUT
  integer, intent(INOUT) :: maxwth  
  logical, intent(INOUT) :: etatvernal  
  real,    intent(INOUT) :: caljvc  
  integer, intent(INOUT) :: onestan2  


!: Variables locales
  integer :: jul  

! DR et ML et SYL 16/06/09 - ajout de cette variable pour le test sur onestan2
  integer :: numcult_ver  


! DR et ML et SYL 16/06/09
! prise en cpte de la montaison pour les prairies
    if (P_codemontaison == 1)then
! ####
! SYL modif le 11/03/08
! NB le 07/03/08
      jul = jjul
      if (jul == nbjsemis) then
        onestan2 = 2
      endif
! DR et ML et SYL 16/06/09 Fin
    else
      jul = MOD(jjul,nbjsemis) ! jul = jjul modulo nbjsemis
    endif

!--    if (P_stade0.ne.'snu'.and.P_stade0.ne.'plt'.and.P_stade0.ne.'lev') then

    !: entrée en vernalisation des herbacées pérennes au jour P_julvernal
    if ( etatvernal .and. jul == P_julvernal .and. P_jvc > P_jvcmini .and. P_codeperenne == 2) then
      etatvernal = .FALSE.
    endif

!--    endif

    if (etatvernal) then
        rfvi = 1.0
    else


      !: On active P_julvernal pour les annuelles, qui joue lorqu'il se situe après la germination.
      !- NB et ML - 25/02/04 : à tester sur RGI CIPAN
      !- ML - 21/04/04 : on interdit de démarrer au stade 'dor' et d'avoir jul < P_julvernal pour les cultures perennes
      if (nger /= 0.) then

      ! DR et ML et SYL 16/06/09
      ! prise en cpte de la montaison des prairies
      ! ####
        if (P_codemontaison == 1)then
          numcult_ver = onestan2
        else
          numcult_ver = numcult
        endif

        if (jul+((numcult_ver-1)*nbjsemis) >= P_julvernal) then


          !: ML - 28/05/04 : on interdit de démarrer au stade lev et
          !- d'avoir jul > P_julvernal le jour de la levée (début de vernalisation) pour les cultures perennes
          if (P_codeperenne == 1) then
          !DR 20/07/2012 pas besoin de jjul
!            call Vernalisation(tdev,P_jvc,P_jvcmini,P_codeperenne,P_culturean,P_codeinitprec,nbjanrec,P_tfroid,P_ampfroid,jjul,n, &
            call Vernalisation(tdev,P_jvc,P_jvcmini,P_codeperenne,P_culturean,P_codeinitprec,nbjanrec,P_tfroid,P_ampfroid,n, &
                               rfvi,nrecbutoir,maxwth,caljvc,etatvernal)

          else
            if (n == namf .and. jul > P_julvernal .and. jul <= (P_julvernal+P_jvcmini)) then
              call EnvoyerMsgHistorique(32)
                 print *,'EnvoyerMsgHistorique(32)'
             !stop
             call exit(9)
            endif
            if (n > namf) then
            !DR 20/07/2012 pas besoin de jjul
              call Vernalisation(tdev,P_jvc,P_jvcmini,P_codeperenne,P_culturean,P_codeinitprec,nbjanrec,P_tfroid,P_ampfroid,    &
!                                 jjul,n, rfvi,nrecbutoir,maxwth,caljvc,etatvernal)
                                 n, rfvi,nrecbutoir,maxwth,caljvc,etatvernal)
            endif
          endif
        else

          if (P_codeperenne == 1) then
            rfvi = 1.0
          else
            call EnvoyerMsgHistorique(50)
               print *,'EnvoyerMsgHistorique(50)'
            call exit(9)
          endif
        endif


      else
        rfvi = 1.0
      endif

    endif


return
end subroutine Stics_Develop_bfroid2

!======================================================================================!
!======================================================================================!
!======================================================================================!


!> Routine de calcul des besoins en froid pour le P_codebfroid = 3
!>
!! Description :
!<
subroutine Stics_Develop_bfroid3(P_codedormance,cu_min,cu_veille,n,P_jvc,P_q10,tmin,tmax,thor, &
                                 etatvernal,cu,rfvi,ndebdorm,nfindorm,nlev,P_idebdorm, P_iwater, nbjsemis,numcult)

USE Besoins_en_froid

implicit none

  integer, intent(IN)    :: P_codedormance  !> // PARAMETER // option of calculation of dormancy and chilling requirement // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: cu_min  
  real,    intent(IN)    :: cu_veille  
  integer, intent(IN)    :: n  
  real,    intent(IN)    :: P_jvc  !> // PARAMETER // Number of vernalizing days // day // PARPLT // 1 
  real,    intent(IN)    :: P_q10  !> // PARAMETER // P_Q10 used for the dormancy break calculation  // SD // PARPLT // 1 
  real,    intent(IN)    :: tmin   !> // OUTPUT // Minimum active temperature of air // degree C
  real,    intent(IN)    :: tmax   !> // OUTPUT // Maximum active temperature of air // degree C
  real,    intent(IN)    :: thor(24)  
  ! DR 06/03/2015 ajout pour les enchainement annuel de la vigne
  integer,  intent(IN)    :: P_idebdorm, P_iwater, nbjsemis,numcult

  logical, intent(INOUT) :: etatvernal  
  real,    intent(INOUT) :: cu  
  real,    intent(OUT)   :: rfvi   !> // OUTPUT // Slowing effect of the vernalization on plant development // 0-1
  integer, intent(INOUT) :: ndebdorm  
  integer, intent(INOUT) :: nfindorm  
  integer, intent(INOUT) :: nlev  



    if (.not.etatvernal) then
      !: Calcul des cu (chill units ?)
      select case(P_codedormance)
        case(1,2)
          call Dormancy_Richardson(thor,n,cu_min,cu_veille,ndebdorm,cu)
        case(3)
          call Dormancy_Bidabe(n,ndebdorm,P_q10,tmin,tmax,cu_veille,cu)
      end select

           !write(*,*)n,ndebdorm,nfindorm,etatvernal
      if (P_codedormance >= 2) then

        !: Cas des calculs de Richardson ou Bidabe
        if (cu > P_jvc) then
          rfvi = 1.0
          !: 17/03/08 : maintenant on garde nfindorm0 si nfin s'est passe annee d'avant
          !--if (nfindorm == 0) nfindorm=n

          ! dr 10/02/20015 je recalcule ndebdorm pour le prochain debdorm !!!! voir avec GNACK le retour !!!!!
          ! DR 09/03/2015 ce calcul est ok mais il ne faut le faire que dans le cas d'enchaienment d'annees .
          if (numcult.gt.1) ndebdorm = P_idebdorm + nbjsemis - P_iwater + 1
!          ndebdorm = 213 + 365 - 346 + 1

          nfindorm = n
          etatvernal = .TRUE.

          !: DR - 20/11/06 : On est un peu perplexe , on etait sur d'avoir testé tous les cas.
          !- Quand on arrive en fin de dormance, si on ne met pas nlev=0 on ne calcule plus les sommes
          !- d'action chaude de richardson. La date de levée a été stockée dans ilevs
          nlev = 0
        else
          rfvi = 0.0
        end if

      else

        !: Cas de forçage de la levée de dormance
        if (n < nfindorm) then
         rfvi = 0.0
        else
          rfvi = 1.0
          etatvernal = .TRUE.
        end if

      end if

    else

      !: ML - le 18/10/05 : Cas de la dormance calculee avec Bidabe: on demarre la dormance à ndebdorm
      if (P_codedormance == 3 .and. n == ndebdorm) then
        etatvernal = .FALSE.
        rfvi = 0.0
      else
        rfvi = 1.0
      endif

    endif


return
end subroutine Stics_Develop_bfroid3
