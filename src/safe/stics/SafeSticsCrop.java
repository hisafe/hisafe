package safe.stics;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import com.sun.jna.Structure;
/**
 * SafeSticsCrop - Mirror object of STICS FORTRAN tetsen
 * 
 * 
 * @author Isabelle Lecomte - December 2016
 */

public class SafeSticsCrop extends Structure implements Serializable {


	public boolean estDominante;
	public int ipl;

	//***********************
	//  parametres plantes
	//***********************

	public float P_adil;   // PARAMETER // Parameter of the critical curve of nitrogen needs [Nplante]=P_adil MS^(-P_bdil) // N% MS // PARPLT // 1
	public float P_bdens;   // PARAMETER // minimal density from which interplant competition starts // plants m-2 // PARPLT // 1
	public float P_bdil;   // PARAMETER // parameter of the critical curve of nitrogen needs [Nplante]=P_adil MS^(-P_bdil) // SD // PARPLT // 1
	public float P_belong;   // PARAMETER // parameter of the curve of coleoptile elongation // degree.days -1 // PARPLT // 1
	public float P_celong;   // PARAMETER // parameter of the subsoil plantlet elongation curve // SD // PARPLT // 1
	public float P_cgrain;   // PARAMETER // slope of the relationship between grain number and growth rate  // grains gMS -1 jour // PARPLT // 1
	public float P_cgrainv0;   // PARAMETER // number of grains produced when growth rate is zero // grains m-2 // PARPLT // 1
	public float P_coefmshaut;   // PARAMETER // ratio biomass/ useful height cut of crops  // t ha-1 m-1 // PARPLT // 1
	public float P_contrdamax;   // PARAMETER // maximal root growth reduction due to soil strenghtness (high bulk density) // SD // PARPLT // 1
	public float P_debsenrac;   // PARAMETER // Sum of degrees.days defining the beginning of root senescence (life-time of a root) // degree.days // PARPLT // 1
	public float P_deshydbase;   // PARAMETER // phenological rate of evolution of fruit water content (>0 or <0) // g water.g MF-1.degree C-1 // PARPLT // 1
	public float P_dlaimax;   // PARAMETER // Maximum rate of the setting up of LAI // m2 leaf plant-1 degree d-1 // PARPLT // 1
	public float P_draclong;   // PARAMETER // Maximum rate of root length production // cm root plant-1 degree.days-1 // PARPLT // 1
	public float P_efcroijuv ;  // PARAMETER // Maximum radiation use efficiency during the juvenile phase(LEV-AMF) // g MJ-1 // PARPLT // 1
	public float P_efcroirepro;   // PARAMETER // Maximum radiation use efficiency during the grain filling phase (DRP-MAT) // g MJ-1 // PARPLT // 1
	public float P_efcroiveg;   // PARAMETER // Maximum radiation use efficiency during the vegetative stage (AMF-DRP) // g MJ-1 // PARPLT // 1
	public float P_elmax;   // PARAMETER // Maximum elongation of the coleoptile in darkness condition // cm // PARPLT // 1
	public float P_extin;   // PARAMETER // extinction coefficient of photosynthetic active radiation canopy // SD // PARPLT // 1
	public float P_fixmax;   // PARAMETER // maximal symbiotic fixation // kg ha-1 j-1 // PARPLT // 1      // OUTPUT // Maximal symbiotic uptake // kg ha-1 j-1
	public float P_h2ofeuiljaune;   // PARAMETER // water content of yellow leaves // g water g-1 MF // PARPLT // 1
	public float P_h2ofeuilverte;   // PARAMETER // water content of green leaves // g water g-1 MF // PARPLT // 1
	public float P_h2ofrvert;   // PARAMETER // water content of fruits before the beginning of hydrous evolution (DEBDESHYD) // g water g-1 MF // PARPLT // 1
	public float P_h2oreserve;   // PARAMETER // reserve water content // g eau g-1 MF // PARPLT // 1
	public float P_h2otigestruc;   // PARAMETER // structural stem part water content // g eau g-1 MF // PARPLT // 1
	public float P_inflomax;   // PARAMETER // maximal number of inflorescences per plant // nb pl-1 // PARPLT // 1
	public float P_Kmabs1;   // PARAMETER // Constant of nitrogen uptake by roots for the high affinity system // µmole. cm root-1 // PARPLT // 1
	public float P_Kmabs2;   // PARAMETER // Constant of nitrogen uptake by roots for the low affinity system // µmole. cm root-1 // PARPLT // 1
	public float P_kmax;   // PARAMETER // Maximum crop coefficient for water requirements (= ETM/ETP) // SD // PARPLT // 1
	public float P_kstemflow;   // PARAMETER // Extinction Coefficient connecting leaf area index to stemflow // * // PARPLT // 1
	public float P_longsperac;   // PARAMETER // specific root length // cm g-1 // PARPLT // 1
	public float P_lvfront;   // PARAMETER // Root density at the root front // cm root.cm-3 soil // PARPLT // 1
	public float P_mouillabil;   // PARAMETER // maximum wettability of leaves // mm LAI-1 // PARPLT // 1
	public float P_nbinflo;   // PARAMETER // imposed number of inflorescences  // nb pl-1 // PARPLT // 1      // OUTPUT // Number of inflorescences // SD
	public float P_pentinflores;   // PARAMETER // parameter of the calculation of the inflorescences number  // SD // PARPLT // 1
	public float P_phosat;   // PARAMETER // saturating photoperiod // hours // PARPLT // 1
	public float P_psisto;   // PARAMETER // absolute value of the potential of stomatal closing // bars // PARPLT // 1
	public float P_psiturg;   // PARAMETER // absolute value of the potential of the beginning of decrease of the cellular extension // bars // PARPLT // 1
	public float P_rsmin;   // PARAMETER // Minimal stomatal resistance of leaves // s m-1 // PARPLT // 1
	public float P_sensrsec;   // PARAMETER // root sensitivity to drought (1=insensitive) // SD // PARPLT // 1
	public float P_spfrmax;   // PARAMETER // maximal sources/sinks value allowing the trophic stress calculation for fruit onset // SD // PARPLT // 1
	public float P_spfrmin;   // PARAMETER // minimal sources/sinks value allowing the trophic stress calculation for fruit onset // SD // PARPLT // 1
	public float P_splaimax;   // PARAMETER // maximal sources/sinks value allowing the trophic stress calculation for leaf growing // SD // PARPLT // 1
	public float P_splaimin;   // PARAMETER // Minimal value of ratio sources/sinks for the leaf growth  // between 0 and 1 // PARPLT // 1
	public float P_stemflowmax;   // PARAMETER // Maximal fraction of rainfall which flows out along the stems  // between 0 and 1 // PARPLT // 1
	public float P_stpltger;   // PARAMETER // Sum of development allowing germination // degree.days // PARPLT // 1
	public float P_tcmin;   // PARAMETER // Minimum temperature of growth // degree C // PARPLT // 1
	public float P_tcmax;   // PARAMETER // Maximum temperature of growth // degree C // PARPLT // 1
	public float P_tdebgel;   // PARAMETER // temperature of frost beginning // degree C // PARPLT // 1
	public float P_tdmin;   // PARAMETER // Minimum threshold temperature for development // degree C // PARPLT // 1
	public float P_tdmax;   // PARAMETER // Maximum threshold temperature for development // degree C // PARPLT // 1
	public float P_tempdeshyd;   // PARAMETER // increase in the fruit dehydration due to the increase of crop temperature (Tcult-Tair) // % water degree C-1 // PARPLT // 1
	public float P_tgellev10;   // PARAMETER // temperature corresponding to 10% of frost damage on the plantlet  // degree C // PARPLT // 1
	public float P_tgellev90;   // PARAMETER // temperature corresponding to 90% of frost damage on the plantlet  // degree C // PARPLT // 1
	public float P_tgmin;   // PARAMETER // Minimum threshold temperature used in emergence stage // degree C // PARPLT // 1
	public float P_tletale;   // PARAMETER // lethal temperature for the plant // degree C // PARPLT // 1
	public float P_tmaxremp;   // PARAMETER // maximal temperature for grain filling // degree C // PARPLT // 1
	public float P_tminremp;   // PARAMETER // Minimal temperature for grain filling // degree C // PARPLT // 1
	public float P_vitircarb;   // PARAMETER // Rate of increase of the carbon harvest index // g grain g plant -1 day-1 // PARPLT // 1
	public float P_vitirazo;   // PARAMETER // Rate of increase of the nitrogen harvest index // g grain g plant -1 day-1 // PARPLT // 1
	public float P_vitpropsucre;   // PARAMETER // increase rate of sugar harvest index  // g sugar g MS-1  j-1 // PARPLT // 1
	public float P_vitprophuile;   // PARAMETER // increase rate of oil harvest index  // g oil g MS-1 j-1 // PARPLT // 1
	public float P_Vmax1;   // PARAMETER // Rate of slow nitrogen absorption (high affinity system) // µmole cm-1 h-1 // PARPLT // 1
	public float P_Vmax2;   // PARAMETER // Rate of rapid nitrogen absorption (high affinity system) // µmole cm-1 h-1 // PARPLT // 1
	public float P_zlabour;   // PARAMETER // Depth of ploughing  // cm // PARPLT // 1
	public float P_zprlim;   // PARAMETER // Maximum depth of the root profile for the reference profile // cm  // PARPLT // 1
	public float P_zpente;   // PARAMETER // Depth where the root density is ½ of the surface root density for the reference profile // cm  // PARPLT // 1
	public float P_adilmax;   // PARAMETER // Parameter of the maximum curve of nitrogen needs [Nplante]=P_adilmax MS^(-P_bdilmax) // N% MS // PARPLT // 1
	public float P_bdilmax;   // PARAMETER // Parameter of the maximum curve of nitrogen needs [Nplante]=P_adilmax MS^(-P_bdilmax) // SD // PARPLT // 1
	public float P_masecNmax;   // PARAMETER // Aerial biomass  on and after there is nitrogen dilution (critical and maximal curves) // t ha-1 // PARPLT // 1
	public float P_INNmin;   // PARAMETER // Minimum value of INN authorised for the crop // SD // PARPLT // 1
	public float P_inngrain1;   // PARAMETER // INN minimal for net absorption of nitrogen during grain filling  // SD // PARPLT // 1
	public float P_inngrain2;   // PARAMETER // INN minimal for null net absorption of nitrogen during grain filling  // SD // PARPLT // 1
	public float P_stlevdno;   // PARAMETER // phasic duration between emergence and the beginning of nodulation  // degrés.jours // PARPLT // 1
	public float P_stdnofno;   // PARAMETER // phasic duration between the beginning and the end of nodulation // degree.days // PARPLT // 1
	public float P_stfnofvino;   // PARAMETER // phasic duration between the end of the nodulation and the end of the nodule life   // degrés.jours // PARPLT // 1
	public float P_vitno;   // PARAMETER // "rate of nodule onset expressed as a proportion of  P_fixmax   per degree day   // nb degrés.jour-1 // PARPLT // 1
	public float P_profnod;   // PARAMETER // nodulation depth // cm // PARPLT // 1
	public float P_concNnodseuil;   // PARAMETER // maximal soil nitrogen threshold for nodule onset  // kg.ha-1.mm-1 // PARPLT // 1
	public float P_concNrac0;   // PARAMETER // soil nitrogen threshold forbiding nodule activity // kg.ha-1.mm-1 // PARPLT // 1
	public float P_concNrac100;   // PARAMETER // soil nitrogen threshold for full nodule activity  // kg.ha-1.mm-1 // PARPLT // 1
	public float P_tempnod1;   // PARAMETER // cardinal temperature for nodule activity   // degree C // PARPLT // 1
	public float P_tempnod2;   // PARAMETER // cardinal temperature for nodule activity   // degree C // PARPLT // 1
	public float P_tempnod3;   // PARAMETER // cardinal temperature for nodule activity   // degree C // PARPLT // 1
	public float P_tempnod4;   // PARAMETER // cardinal temperature for nodule activity   // degree C // PARPLT // 1
	public float P_coeflevamf;   // PARAMETER // multiplier coefficient of the development phase LEVAMF to use crop temperature // SD // PARPLT // 1
	public float P_coefamflax;   // PARAMETER // multiplier coefficient of the development phase AMFLAX to use crop temperature // SD // PARPLT // 1
	public float P_coeflaxsen;   // PARAMETER // multiplier coefficient of the development phase LAXSEN to use crop temperature // SD // PARPLT // 1
	public float P_coefsenlan;   // PARAMETER // cultiplier coefficient of the development phase SENLAN to use crop temperature // SD // PARPLT // 1
	public float P_coefdrpmat;   // PARAMETER // multiplier coefficient of the development phase DRPMAT to use crop temperature // SD // PARPLT // 1
	public float P_coefflodrp;   // PARAMETER // multiplier coefficient of the development phase FLODRP to use crop temperature // SD // PARPLT // 1
	public float P_coeflevdrp;   // PARAMETER // multiplier coefficient of the development phase LEVDRP to use crop temperature // SD // PARPLT // 1
	public float P_ratiodurvieI;   // PARAMETER // life span of early leaves expressed as a proportion of the life span of the last leaves emitted P_DURVIEF // SD // PARPLT // 1
	public float P_ratiosen;   // PARAMETER // fraction of senescent biomass (by ratio at the total biomass) // between 0 and 1 // PARPLT // 1
	public float P_ampfroid;   // PARAMETER // semi thermal amplitude thermique for vernalising effect // degree C // PARPLT // 1
	public float P_laicomp;   // PARAMETER // LAI from which starts competition inbetween plants // m2 m-2 // PARPLT // 1
	public float P_phobase;   // PARAMETER // Base photoperiod  // hours // PARPLT // 1
	public float P_stressdev;   // PARAMETER // maximum phasic delay allowed  due to stresses  // SD // PARPLT // 1
	public float P_jvcmini;   // PARAMETER // Minimum number of vernalising days  // day // PARPLT // 1
	public float P_julvernal;   // PARAMETER // julian day (between 1 and 365) accounting for the beginning of vernalisation for perennial crops // julian day // PARPLT // 1
	public float P_tfroid;   // PARAMETER // optimal temperature for vernalisation // degree C // PARPLT // 1
	public float P_q10;   // PARAMETER // P_Q10 used for the dormancy break calculation  // SD // PARPLT // 1
	public float P_stdordebour;   // PARAMETER // phasic duration between the dormancy break and the bud break  // degree.days // PARPLT // 1
	public float P_phyllotherme;   // PARAMETER // thermal duration between the apparition of two successive leaves on the main stem // degree C day // PARPLT // 1
	public float P_laiplantule;   // PARAMETER // Plantlet Leaf index at the plantation // m2 leaf  m-2 soil // PARPLT // 1
	public float P_masecplantule;   // PARAMETER // initial shoot biomass of plantlet // t ha-1 // PARPLT // 1
	public float P_zracplantule;   // PARAMETER // depth of the initial root front of the plantlet  // cm // PARPLT // 1
	public float P_hautbase;   // PARAMETER // Base height of crop // m // PARPLT // 1
	public float P_hautmax;   // PARAMETER // Maximum height of crop // m // PARPLT // 1
	public float P_vlaimax;   // PARAMETER // ULAI at inflection point of the function DELTAI=f(ULAI) // SD // PARPLT // 1
	public float P_pentlaimax;   // PARAMETER // parameter of the logistic curve of LAI growth  // SD // PARPLT // 1
	public float P_udlaimax;   // PARAMETER // ulai from which the rate of leaf growth decreases  // SD // PARPLT // 1
	public float P_abscission;  // PARAMETER // sensescent leaf proportion falling on the soil // SD // PARPLT // 1
	public float P_parazofmorte;   // PARAMETER // parameter of proportionality between the C/N of died leaves and the INN // SD // PARPLT // 1
	public float P_innturgmin;   // PARAMETER // parameter of the nitrogen stress function active on leaf expansion (INNLAI), that is a bilinear function passing by the point of coordinate (P_innmin, P_innturgmin) // SD // PARPLT // 1
	public float P_dlaimin;   // PARAMETER // accelerating parameter for the lai growth rate // SD // PARAMV6/PLT // 1
	public float P_tustressmin;   // PARAMETER //  threshold stress (min(turfac,inns)) under which there is an effect on the LAI (supplementary senescence by ratio at the natural senescence) // SD // PARPLT // 1
	public float P_durviesupmax;   // PARAMETER // proportion of additional lifespan due to an overfertilization // SD // PARPLT // 1
	public float P_innsen;   // PARAMETER // parameter of the nitrogen stress function active on senescence (innnsenes), bilinear function of the INN using the point (P_innmin, P_innsen) // SD // PARPLT // 1
	public float P_rapsenturg;   // PARAMETER // threshold soil water content active to simulate water sensecence stress as a proportion of the turgor stress // SD // PARPLT // 1
	public float P_dlaimaxbrut;   // PARAMETER // Maximum rate of the setting up of LAI // m2 leaf plant-1 degree d-1 // PARPLT // 1
	public float P_tauxrecouvmax;   // PARAMETER // maximal soil cover rate // m2 plante m-2 sol // PARPLT // 1
	public float P_tauxrecouvkmax;   // PARAMETER // soil cover rate corresponding to the maximal crop coefficient for water requirement  // m2 plante m-2 sol // PARPLT // 1
	public float P_pentrecouv;   // PARAMETER // parameter of the logistic curve of the soil cover rate increase // * // PARPLT // 1
	public float P_infrecouv;   // PARAMETER // ulai at the stage AMF (inflexion point of the soil cover rate increase) // SD // PARPLT // 1
	public float P_rapforme;   // PARAMETER // Ratio thickness/width of the crop form (negative when the base of the form < top) // SD // PARPLT // 1
	public float P_ktrou;   // PARAMETER // Extinction Coefficient of PAR through the crop  (radiation transfer) // * // PARPLT // 1
	public float P_adfol;   // PARAMETER // parameter determining the leaf density evolution within the chosen shape // m-1 // PARPLT // 1
	public float P_dfolbas;   // PARAMETER // minimal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1
	public float P_dfolhaut;   // PARAMETER // maximal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1
	public float P_remobres;   // PARAMETER // proportion of daily remobilisable carbon reserve // SD // PARPLT // 1
	public float P_temin;   // PARAMETER // Minimum threshold temperature for development // degree C // PARPLT // 1
	public float P_teopt;   // PARAMETER // Optimal temperature for the biomass growth // degree C // PARPLT // 1
	public float P_temax;   // PARAMETER // Maximal threshold temperature for the biomass growth  // degree C // PARPLT // 1
	public float P_teoptbis;   // PARAMETER // optimal temperature for the biomass growth (if there is a plateau between P_teopt and P_teoptbis) // degree C // PARPLT // 1
	public float P_slamin;   // PARAMETER // minimal SLA of green leaves // cm2 g-1 // PARPLT // 1
	public float P_slamax;   // PARAMETER // maximal SLA of green leaves // cm2 g-1 // PARPLT // 1
	public float P_tigefeuil;   // PARAMETER // stem (structural part)/leaf proportion // SD // PARPLT // 1
	public float P_envfruit;   // PARAMETER // proportion envelop/P_pgrainmaxi in weight  // SD // PARPLT // 1
	public float P_sea;   // PARAMETER // specifique surface of fruit envelops // cm2 g-1 // PARPLT // 1
	public float P_nbgrmin;   // PARAMETER // Minimum number of grain // grains m-2  // PARPLT // 1
	public float P_irmax;   // PARAMETER // Maximum harvest index // SD // PARPLT // 1
	public float P_vitircarbT;   // PARAMETER // Heat rate of increase of the carbon harvest index  // g grain g plant-1 degree.day-1 // PARPLT // 1
	public float P_afpf;   // PARAMETER // parameter of the  function logistic defining sink strength of fruits (indeterminate growth) : relative fruit age at which growth is maximal // SD // PARPLT // 1
	public float P_bfpf;   // PARAMETER // parameter of the logistic curve defining sink strength of fruits (indeterminate growth) :  rate of maximum growth proportionately to maximum weight of fruits // * // PARPLT // 1
	public float P_stdrpnou;   // PARAMETER // Sum of development units between the stages DRP and NOU (end of  setting) // degree.days // PARPLT // 1
	public float P_sensanox;   // PARAMETER // anoxia sensitivity (0=insensitive) // SD // PARPLT // 1
	public float P_allocfrmax;   // PARAMETER // maximal daily allocation towards fruits // SD // PARPLT // 1
	public float P_tgeljuv10;   // PARAMETER // temperature corresponding to 10 % of frost damage on the LAI (juvenile stage) // degree C // PARPLT // 1
	public float P_tgeljuv90;   // PARAMETER // temperature corresponding to 90 % of frost damage on the LAI (juvenile stage) // degree C // PARPLT // 1
	public float P_tgelveg10;   // PARAMETER // temperature corresponding to 10 % of frost damage on the LAI (adult stage) // degree C // PARPLT // 1
	public float P_tgelveg90;   // PARAMETER // temperature corresponding to 90 % of frost damage on the LAI (adult stage) // degree C // PARPLT // 1
	public float P_tgelflo10;   // PARAMETER // temperature corresponding to 10 % of frost damages on the flowers or the fruits // degree C // PARPLT // 1
	public float P_tgelflo90;   // PARAMETER // temperature corresponding to 90 % of frost damages on the flowers or the fruits // degree C // PARPLT // 1
	public int P_codelegume;   // PARAMETER // 1 when the plant  id a legume crop, or 2 // code 1/2 // PARPLT // 0
	public int P_codcalinflo;   // PARAMETER // option of the way of calculation of the inflorescences number  // code 1/2 // PARPLT // 0
	public int P_codgeljuv;   // PARAMETER // activation of LAI frost at the juvenile stadge // code 1/2 // PARPLT // 0
	public int P_codebeso;   // PARAMETER // option computing of water needs by the k.ETP (1) approach  or resistive (2) approach // code 1/2 // PARPLT // 0
	public int P_codeintercept;   // PARAMETER // option of simulation rainfall interception by leafs: yes (1) or no (2) // code 1/2 // PARPLT // 0
	public int P_codeindetermin;   // PARAMETER // option of  simulation of the leaf growth and fruit growth : indeterminate (2) or determinate (1) // code 1/2 // PARPLT // 0
	public int P_codetremp;   // PARAMETER // option of heat effect on grain filling: yes (2), no (1) // code 1/2 // PARPLT // 0
	public int P_codetemprac;   // PARAMETER // option calculation mode of heat time for the root: with crop temperature (1)  or with soil temperature (2) // code 1/2 // PARPLT // 0
	public int P_coderacine;   // PARAMETER // Choice of estimation module of growth root in volume: standard profile (1) or by actual density (2) // code 1/2 // PARPLT // 0
	public int P_codgellev;   // PARAMETER // activation of plantlet frost // code 1/2 // PARPLT // 0
	public int P_codazofruit;   // PARAMETER // option of activation of the direct effect of the nitrogen plant status upon the fruit/grain number // code 1/2 // PARPLT // 0
	public int P_codemonocot;   // PARAMETER // option plant monocot(1) or dicot(2) // code 1/2 // PARPLT // 0
	public int P_codetemp;   // PARAMETER // option calculation mode of heat time for the plant : with air temperature (1)  or crop temperature (2) // code 1/2 // PARPLT // 0
	public int P_codegdh;  // PARAMETER // hourly (1) or daily (2) calculation of development unit // code 1/2 // PARPLT // 0
	public int P_codephot;   // PARAMETER // option of plant photoperiodism: yes (1), no (2) // code1/2 // PARPLT // 0
	public int P_coderetflo;   // PARAMETER // option slowness action of water stress before the stage DRP: yes  (1), no (2) // code 1/2 // PARPLT // 0
	public int P_codebfroid;   // PARAMETER // option of calculation of chilling requirements // code 1/2 // PARPLT // 0
	public int P_codedormance;   // PARAMETER // option of calculation of dormancy and chilling requirement // code 1/2 // PARPLT // 0
	public int P_codeperenne;   // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0
	public int P_codegermin;   // PARAMETER // option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2) // code 1/2 // PARPLT // 0
	public int P_codehypo;   // PARAMETER // option of simulation of a  phase of hypocotyl growth (1) or planting of plantlets (2) // code 1/2 // PARPLT // 0
	public int P_codeir;   // PARAMETER // option of computing the ratio grain weight/total biomass: proportional to time(1), proportional to sum temperatures (2) // code 1/2 // PARPLT // 0
	public int P_codelaitr;   // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0
	public int P_codlainet;   // PARAMETER // option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI)// code 1/2 // PARPLT // 0
	public int P_codetransrad;   // PARAMETER // simulation option of radiation 'interception: law Beer (1), radiation transfers (2) // code 1/2 // PARPLT // 0
	public int codetransrad;
	public int P_codgelveg;   // PARAMETER // activation of LAI frost at adult stage // code 1/2 // PARPLT // 0
	public int P_codgelflo;   // PARAMETER // activation of frost at anthesis // code 1/2 // PARPLT // 0
	public int P_nbjgrain;   // PARAMETER // Period to compute NBGRAIN // days // PARPLT // 1
	public int P_nbfgellev;   // PARAMETER // leaf number at the end of the juvenile phase (frost sensitivity)  // nb pl-1 // PARPLT // 1
	public int P_nboite;   // PARAMETER // "Number of  box  or  age class  of fruits for the fruit growth for the indeterminate crops " // SD // PARPLT // 1
	public int P_idebdorm;   // PARAMETER // day of the dormancy entrance // julian day // PARPLT // 1
	public int P_ifindorm;   // PARAMETER // dormancy break day // julian day // PARPLT // 1
	public int P_nlevlim1;   // PARAMETER // number of days after germination decreasing the emerged plants if emergence has not occur // days // PARPLT // 1
	public int P_nlevlim2;   // PARAMETER // number of days after germination after which the emerged plants are null // days // PARPLT // 1
	public int P_nbfeuilplant;   // PARAMETER // leaf number per plant when planting // nb pl-1 // PARPLT // 1
	public int P_forme ;  // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0
	public int  P_stoprac;   // PARAMETER // stage when root growth stops (LAX or SEN) // * // PARPLT // 0
	public int  P_codeplante;   // PARAMETER // Name code of the plant in 3 letters // * // PARPLT // 0

	// parametres varietaux (nb_variete_max=30) 
	public int nbVariete;
	public int[] P_codevar;    // PARAMETER // variety name // SD // PARPLT // 0
	public float[] P_adens;         // PARAMETER // Interplant competition parameter // SD // PARPLT // 1
	public float[] P_afruitpot;     // PARAMETER // maximal number of set fruits per degree.day (indeterminate growth) // nbfruits degree.day-1 // PARPLT // 1
	public float[] P_croirac;       // PARAMETER // Growth rate of the root front  // cm degree.days-1 // PARPLT // 1
	public float[] P_dureefruit;    // PARAMETER // total growth period of a fruit at the setting stage to the physiological maturity // degree.days // PARPLT // 1
	public float[] P_durvieF;       // PARAMETER // maximal  lifespan of an adult leaf expressed in summation of P_Q10=2 (2**(T-Tbase)) // P_Q10 // PARPLT // 1
	public float[] P_jvc;           // PARAMETER // Number of vernalizing days // day // PARPLT // 1
	public float[] P_nbgrmax;       // PARAMETER // Maximum number of grain // grains m-2 // PARPLT // 1
	public float[] P_pgrainmaxi;    // PARAMETER // Maximum weight of one grain (at 0% water content) // g // PARPLT // 1
	public float[] P_stamflax;      // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1
	public float[] P_stlevamf;      // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1
	public float[] P_stlevdrp;      // PARAMETER // Sum of development units between the stages LEV and DRP // degree.days // PARPLT // 1
	public float[] P_stflodrp;      // PARAMETER // phasic duration between FLO and DRP (only for indication) // degrés.jours // PARPLT // 1
	public float[] P_stlaxsen;      // PARAMETER // Sum of development units between the stages LAX and SEN // degree.days // PARPLT // 1
	public float[] P_stsenlan;      // PARAMETER // Sum of development units between the stages SEN et LAN // degree.days // PARPLT // 1
	public float[] P_stdrpmat;      // PARAMETER // Sum of development units between the stages DRP and MAT // degree.days // PARPLT // 1
	public float[] P_sensiphot;     // PARAMETER //  photoperiod sensitivity (1=insensitive) // SD // PARPLT // 1
	public float[] P_stdrpdes;      // PARAMETER // phasic duration between the DRP stage and the beginning of the water fruit dynamics  // degree.days // PARPLT // 1
	public int P_codazorac;   // PARAMETER // activation of the nitrogen influence on root partitionning within the soil profile  // code 1/2 // PARPLT // 0
	public int P_codtrophrac;   // PARAMETER // trophic effect on root partitioning within the soil // code 1/2/3 // PARPLT // 0
	public float P_minefnra;   // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning // SD // PARPLT // 1
	public float P_maxazorac;   // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning  // kg N ha-1 mm-1 // PARPLT // 1
	public float P_minazorac;   // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning // kg N ha-1 mm-1 // PARPLT // 1
	public float P_repracpermax;   // PARAMETER // maximum of root biomass respective to the total biomass (permanent trophic link) // SD // PARPLT // 1
	public float P_repracpermin;   // PARAMETER // minimum of root biomass respective to the total biomass (permanent trophic link) // SD // PARPLT // 1
	public float P_krepracperm;   // PARAMETER // parameter of biomass root partitioning : evolution of the ratio root/total (permanent trophic link) // SD // PARPLT // 1
	public float P_repracseumax;   // PARAMETER // maximum of root biomass respective to the total biomass (trophic link by thresholds) // SD // PARPLT // 1
	public float P_repracseumin;   // PARAMETER // minimum of root biomass respective to the total biomass (trophic link by thresholds) // SD // PARPLT // 1
	public float P_krepracseu;   // PARAMETER // parameter of biomass root partitioning : evolution of the ratio root/total (trophic link by thresholds) // SD // PARPLT // 1
	public int P_codefixpot;   // PARAMETER // option of calculation of the maximal symbiotic fixation // code 1/2/3 // PARPLT // 0
	public float P_fixmaxveg;   // PARAMETER // parameter to calculate symbiotic fixation as a function of the plant growth   // kg N  (t MS)-1 // PARPLT // 1
	public float P_fixmaxgr;   // PARAMETER // parameter to calculate symbiotic fixation as a function of the plant growth   // kg N  (t MS)-1 // PARPLT // 1
	public float P_tdmindeb;   // PARAMETER // minimal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
	public float P_tdmaxdeb;   // PARAMETER // maximal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
	public int P_codegdhdeb;   // PARAMETER // option of calculation of the bud break date in hourly or daily growing degrees  // code 1/2 // PARPLT // 0
	public int P_codeplisoleN;   // PARAMETER // code for N requirement calculations at the beginning of the cycle: dense plant population (1), isolated plants (2, new formalisation) // code 1/2 // PARPLT // 0
	public float P_Nmeta;   // PARAMETER // rate of metabolic nitrogen in the plantlet // % // PARPLT // 1
	public float P_masecmeta;   // PARAMETER // biomass of the plantlet supposed to be composed of metabolic nitrogen // t ha-1 // PARPLT // 1
	public float P_Nreserve;   // PARAMETER // maximal amount of nitrogen in the plant reserves (distance between the maximal dilution curve and the critical dilution curve) (as a percentage of the aboveground dry weight) // % // PARPLT // 1
	public int P_codeINN;   // PARAMETER // option to compute INN: cumulated (1), instantaneous (2)  // code 1/2 // PARPLT // 0
	public float P_INNimin;   // PARAMETER // INNI (instantaneous INN) corresponding to P_INNmin // SD // PARPLT // 1
	public float P_potgermi;   // PARAMETER // humidity threshold from which seed humectation occurs, expressed in soil water potential  // Mpa // PARPLT // 1
	public int P_nbjgerlim;   // PARAMETER // Threshold number of day after grain imbibition without germination lack // days // PARPLT // 1
	public float P_propjgermin;   // PARAMETER // minimal proportion of the duration P_nbjgerlim when the temperature is higher than the temperature threshold P_Tdmax  // % // PARPLT // 1
	public float P_cfpf;   // PARAMETER // parameter of the first potential growth phase of fruit, corresponding to an exponential type function describing the cell division phase. // SD // PARPLT // 1
	public float P_dfpf;   // PARAMETER // parameter of the first potential growth phase of fruit, corresponding to an exponential type function describing the cell division phase. // SD // PARPLT // 1
	public float P_tcxstop;   // PARAMETER // threshold temperature beyond which the foliar growth stops // degree C // PARPLT // 1
	public float P_vigueurbat;   // PARAMETER // indicator of plant vigor allowing to emerge through the crust  // between 0 and 1 // PARPLT // 1
	public float P_phobasesen;   // PARAMETER // photoperiod under which the photoperiodic stress is activated on the leaf lifespan // heures // PARPLT // 1
	public float P_dltamsmaxsen;   // PARAMETER // threshold value of deltams from which there is no more photoperiodic effect on senescence // t ha-1j-1 // PARPLT // 1
	public int  P_codestrphot;   // PARAMETER // activation of the photoperiodic stress on lifespan : yes (1), no (2) // code 1/2 // PARPLT // 0
	public float P_dltamsminsen;   // PARAMETER // threshold value of deltams from which the photoperiodic effect on senescence is maximal // t ha-1j-1 // PARPLT // 1
	public float P_alphaphot;   // PARAMETER // parameter of photoperiodic effect on leaf lifespan // P_Q10 // PARPLT// 1
	public float P_alphaco2;   // PARAMETER // coefficient allowing the modification of radiation use efficiency in case of  atmospheric CO2 increase // SD // PARPLT // 1

	// **************************************
	// paramètres spécifiques d'initialisation de l'état de la plante
	// pour le début de la simulation
	// **************************************
	//1=snu=baresoil
	//2=plt=sowing
	//3=dor=dormancy
	//4=lev=emergence
	//5=amf=maximum acceleration of leaf growth,end of juvenile phase
	//6=lax=maximum leaf area index, end of leaf growth
	//7=flo=floraison
	//8=drp=onset of filling of harvested organs
	//9=des=onset of water dynamics in harvested organs
	//10=mat=Maturite
	//11=sen=Senescence
	//12=lan=Lai nul
	//13=rec=Harvest		
	public int P_stade0;  // PARAMETER // Crop stage used at the beginning of simulation // * // INIT // 0
	public float P_lai0;   // PARAMETER // Initial leaf area index // m2 m-2 // INIT // 1
	public float P_masec0;  // PARAMETER // initial biomass // t ha-1 // INIT // 1
	public float P_magrain0;  // PARAMETER // initial grain dry weight // g m-2 // INIT // 1
	public float P_QNplante0;  // PARAMETER // initial nitrogen amount in the plant // kg ha-1 // INIT // 1
	public float P_densinitial[];   // PARAMETER // Table of initial root density of the 5 horizons of soil for fine earth // cm cm-3 // INIT // 1
	public float P_resperenne0;   // PARAMETER // initial reserve biomass // t ha-1 // INIT // 1
	public float P_zrac0;   // PARAMETER // initial depth of root front  // cm // INIT // 1
	public float mafeuiltombe0[];    // OUTPUT // Dry matter of fallen leaves // t.ha-1
	public float masecneo0[];   // OUTPUT // Newly-formed dry matter  // t.ha-1
	public float msneojaune0[];    // OUTPUT // Newly-formed senescent dry matter  // t.ha-1
	public float dltamsen0[];       // OUTPUT // // t.ha-1
	public float mafeuiljaune0[];    // OUTPUT // // t.ha-1



	// **************************************
	// *         Variables plante           *
	// **************************************

	public int  ficrap;
	public int  ficrap_AgMIP;
	public int  ficsort;
	public int  ficsort2;
	public int  ficbil;
	public int  group;
	public int  parapluie;
	public int  codeinstal;
	public int  nsencour;
	public int  nsencourpre;
	public int  nsencourprerac;
	public int  numjtrav[];
	public int  numjres[];
	public int  nnou;
	public int  nbj0remp; // OUTPUT // Number of shrivelling days //
	public int  nbjgel; // OUTPUT // Number of frosting days active on the plant // SD
	public int  nfauche[];
	public int  nlanobs;
	public int  nlax;
	public int  nrecobs;
	public int  nlaxobs;
	public int  nlev;
	public int  nlevobs;
	public int  nmatobs;
	public int  nplt;
	public int  ndrp;
	public int  ndebsenrac;
	public int  nbrecolte;
	public int  nrecint[];
	public int  ndebdes;
	public int  ntaille;
	public int  nger;
	public int  igers; // OUTPUT // Date of germination // jour julien
	public int  inous; // OUTPUT // Date of end of setting of harvested organs // jour julien
	public int  idebdess; // OUTPUT // Date of onset of water dynamics in harvested organs // jour julien
	public int  ilans; // OUTPUT // Date of LAN stage (leaf index nil) // jour julien
	public int  iplts; // OUTPUT // Date of sowing or planting // jour julien
	public int  compretarddrp;
	public int  iamfs; // OUTPUT // Date of AMF stage // jour julien
	public int  idebdorms; // OUTPUT // Date of entry into dormancy // jour julien
	public int  idrps; // OUTPUT // Date of start of filling of harvested organs // jour julien
	public int  ifindorms; // OUTPUT // Date of emergence from dormancy // jour julien
	public int  iflos; // OUTPUT // Date of flowering // jour julien
	public int  ilaxs; // OUTPUT // Date of LAX stage (leaf index maximum) // jour julien
	public int  namf;
	public int  imats; // OUTPUT // Date of start of physiological maturity // jour julien
	public int  irecs; // OUTPUT // Date of harvest (first if several) // jour julien
	public int  isens; // OUTPUT // Date of SEN stage // jour julien
	public int  nsen;
	public int  nsenobs;
	public int  ndebdorm;
	public int  nfindorm;
	public int  nbfeuille; // OUTPUT // Number of leaves on main stem // SD
	public int  nflo;
	public int  nfloobs;
	public int  nstopfeuille;
	public int  mortplante;
	public int  mortplanteN;
	public int  neclair[];
	public int  nrogne;
	public int  neffeuil;
	public int  jdepuisrec;
	public int  namfobs;
	public int  nlan;
	public int  nrec;
	public int  nrecbutoir;
	public int  nmat;
	public int  ndrpobs;
	public int  ndebdesobs;
	public int  ilevs; // OUTPUT // Date of emergence // jour julien
	public int  numcoupe; // OUTPUT // Cut number // SD
	public int  nst1coupe;
	public int  nst2coupe;
	public int  ndebsen;
	public int  nbjTmoyIpltJuin;
	public int  nbjTmoyIpltSept;
	public int  ndno;
	public int  nfno;
	public int  nfvino;
	public float fixpotC;
	public float fixreelC;
	public float offrenodC;
	public float fixmaxC;
	public float QfixC;
	public float fixpotfno;
	public float propfixpot;
	public int  nrecalpfmax;
	public boolean fauchediff;
	public boolean sioncoupe;
	public boolean onarretesomcourdrp;
	public boolean etatvernal;
	public boolean gelee;
	public float LRACH[]; // OUTPUT // Root density in the horizon 1 // cm.cm-3
	public float cinterpluie; // OUTPUT // Amount of rain intercepted by the leaves // mm
	public float totpl;
	public float rc; // OUTPUT // Resistance of canopy  // s.m-1
	public float fco2s;  // OUTPUT // specie-dependant effect on stomate closure  // SD
	public float lracz[];
	public float cumlracz; // OUTPUT // Sum of the effective root lengths  // cm root.cm -2 soil
	public float dfol; // OUTPUT //  "Within the shape  leaf density" // m2 m-3
	public float rombre; // OUTPUT // Radiation fraction in the shade // 0-1
	public float rsoleil; // OUTPUT // Radiation fraction in the full sun // 0-1
	public float somcourfauche;   // OUTPUT //actual sum of temperature between 2 cuts // 0-1
	public float reste_apres_derniere_coupe;
	public float QNplanteres;
	public float QCplantetombe[];
	public float QNplantetombe[];
	public float QCrogne; // Bruno: ajout variables cumul des quantites C et N rognees 22/05/2012
	public float QNrogne;
	public float Crac;
	public float Nrac;
	public float QCrac;
	public float QNrac;
	public float udevlaires[];
	public float cescoupe;
	public float cetcoupe;
	public float cepcoupe;
	public float cetmcoupe;
	public float cprecipcoupe;
	public float cep; // OUTPUT // Transpiration integrated over the cropping season  // mm
	public float cprecip; // OUTPUT // Water supply integrated over the cropping season // mm
	public float cet; // OUTPUT // Evapotranspiration integrated over the cropping season // mm
	public float ces; // OUTPUT // Evaporation integrated over the cropping season // mm
	public float cetm; // OUTPUT // Maximum evapotranspiration integrated over the cropping season // mm
	public float masectot;
	public float rendementsec;
	public float str1coupe;
	public float stu1coupe;
	public float str2coupe;
	public float stu2coupe;
	public float inn1coupe;
	public float diftemp1coupe;
	public float inn2coupe;
	public float diftemp2coupe;
	public float QNressuite;
	public float QCressuite;
	public float pfmax;
	public float stemflow; // OUTPUT // Water running along the stem // mm
	public float precrac[];
	public float lracsenz[];
	public float drl[];            // temporel pour sénescence (racinaire)
	public float somtemprac;
	///MODIF HISAFE remplacé par rljour et rlveille
	///public float rl[];             //On peut à priori se passer du tableau temporel, à vérifier
	public float poussracmoy; // OUTPUT // "Average index of the effect of soil constraints on the rooting profile (option  true density )" // 0-1
	public float Emd; // OUTPUT // Direct evaporation of water intercepted by leafs  // mm
	public float diftemp1;
	public float diftemp2;
	public float flrac[];
	public float ftemp; // OUTPUT // Temperature-related EPSIBMAX reduction factor // 0-1
	public float udevcult; // OUTPUT // Effective temperature for the development, computed with TCULT // degree.days
	public float udevair; // OUTPUT // Effective temperature for the development, computed with TAIR // degree.days
	public float ebmax;
	public float fpari;
	public float efdensite_rac; // OUTPUT // density factor on root growth // 0-1
	public float efdensite; // OUTPUT // density factor on leaf area growth  // 0-1
	public float ulai[];                  // OUTPUT // Daily relative development unit for LAI // 0-3
	public float caljvc;
	public float somelong;
	public float somger;
	public float cdemande; // OUTPUT // Sum of daily nitrogen need of the plant   // kg.ha-1
	public float zrac; // OUTPUT // Depth reached by root system // cm
	public float znonli;
	public float deltaz; // OUTPUT // Deepening of the root front  // cm day-1
	public float difrac;
	public float resrac; // OUTPUT // Soil water reserve in the root zone // mm
	public float Scroira;
	public float  efda;       // OUTPUT // efda defines the effect of soil compaction through bulk density // 0-1
	public float  efnrac_mean;    // OUTPUT // effect of mineral nitrogen, which contributes to the root distribution in the layers with high mineral nitrogen content. // 0-1
	public float  humirac_mean;  // OUTPUT // soil dryness // 0-1
	public float  humirac_z[];  // OUTPUT //profile soil dryness // 0-1
	public float  efnrac_z[];  // OUTPUT // effect of minral nitrogen on roots // 0-1
	public float  rlj;  // OUTPUT // roots length growth rate  // m.d-1
	public float  dltmsrac_plante;
	public float cumraint; // OUTPUT // Sum of intercepted radiation  // Mj.m-2
	public float cumrg; // OUTPUT // Sum of global radiation during the stage sowing-harvest   // Mj.m-2
	public float irrigprof[];
	public float stlevdrp0;
	public float stsenlan0;
	public float stlaxsen0;
	public float remobil;
	public float somcourdrp;
	public float dtj[];                   // OUTPUT // Daily efficient temperature for the root growing  // degree C.j-1
	public float qressuite; // OUTPUT // quantity of aerial residues from the previous crop // t.ha-1
	public float qressuite_tot; // OUTPUT // quantity of total residues (aerials + roots) from the previous crop // t.ha-1
	public float CsurNressuite_tot;  // OUTPUT // Carbon to Nitrogen ratio of total harvest residues (aerials + roots) // t.ha-1
	public float  Nexporte; // OUTPUT // total of exported nitrogen // kgN.ha-1
	public float  Nrecycle; // OUTPUT // total of recycle nitrogen (unexported nitrogen at harvest + nitrogen from the fallen leaves) // kgN.ha-1
	public float  MSexporte; // OUTPUT // total of exported carbon // t.ha-1
	public float  MSrecycle; // OUTPUT // total of recycle carbon (unexported nitrogen at harvest + nitrogen from the fallen leaves) // t.ha-1
	public float  p1000grain; // OUTPUT // 1000 grain weight // g.m-2
	public float  somudevair; // OUTPUT // sum of air temperature from sowing // degree C
	public float  somudevcult; // OUTPUT // sum of crop temperature from sowing // degree C
	public float  somupvtsem; // OUTPUT // sum of development units from sowing // degree C
	public float CsurNressuite;
	public float cumlraczmaxi;
	public float cumdevfr;
	public float nbfruit;
	public float tetstomate; // OUTPUT // Threshold soil water content limiting transpiration and photosynthesis  // % vol
	public float teturg; // OUTPUT // Threshold soil water content limiting surface area growth of leaves // % vol
	public float rltot; // OUTPUT // Total length of roots  // cm root.cm -2 soil
	public float rfpi; // OUTPUT // Slowing effect of the photoperiod on plant development  // 0-1
	public float lracsentot; // OUTPUT // Total length of senescent roots  // cm root.cm -2 soil
	public float largeur; // OUTPUT // Width of the plant shape  // m
	public float utno;
	public float pfv;
	public float pfj;
	public float pftot;
	public float pfe;
	public float pft;
	public float wcf;
	public float wct;
	public float wce;
	public float stpltlev;
	public float stpltger;
	public float stdrpsen;
	public float stmatrec;
	public float upvtutil;
	public float somcour; // OUTPUT // Cumulated units of development between two stages // degree.days
	public float reajust;
	public float upobs[];              // tableau de forçage
	public float stlevamf0;
	public float stamflax0;
	public float varrapforme;
	public float anoxmoy; // OUTPUT // Index of mean anoxia on the root depth  // 0-1
	public float chargefruit; // OUTPUT // Amount of filling fruits per plant // nb fruits.plant-1
	public float coeflev;
	public float cumflrac;
	public float densitelev;
	public float durvieI;
	public float exobiom; // OUTPUT // Index of excess water active on surface growth // 0-1
	public float etr_etm1;
	public float etm_etr1;
	public float etr_etm2;
	public float etm_etr2;
	public float exofac; // OUTPUT // Variable for excess water // 0-1
	public float exofac1;
	public float exofac2;
	public float exolai; // OUTPUT // Index for excess water active on growth in biomass // 0-1
	public float fco2;   // OUTPUT // specie-dependant CO2 effect on radiation use efficiency // SD
	public float fgelflo; // OUTPUT // Frost index on the number of fruits // 0-1
	public float fgellev;
	public float fstressgel; // OUTPUT // Frost index on the LAI // 0-1
	public float ftempremp;
	public float gel1; // OUTPUT // frost damage on foliage before amf // %
	public float gel2; // OUTPUT // frost damage on foliage after amf // %
	public float gel3; // OUTPUT // frost damage on foliage or fruits // %
	public float izrac; // OUTPUT // Index of excess water stress on roots  // 0-1
	public float idzrac;
	public float nbfrote;
	public float rmaxi; // OUTPUT // Maximum water reserve utilised // mm
	public float somcourutp;
	public float somcourno;
	public float somfeuille;
	public float somtemp; // OUTPUT // Sum of temperatures // degree C.j
	public float somupvt;
	public float somupvtI;
	public float stdrpmatini;
	public float stflodrp0;
	public float stlevflo;
	public float TmoyIpltJuin; // OUTPUT // Sum of temperatures from sowing or planting (IPLT stage) until June the 30th // degree C
	public float TmoyIpltSept; // OUTPUT // Sum of temperatures from sowing or planting (IPLT stage) until September the 30th // degree C
	public float zracmax;
	public float rfvi; // OUTPUT // Slowing effect of the vernalization on plant development // 0-1
	public float racnoy[];       
	public float msrac[];            // pas besoin du tableau temporel, une simple sauvegarde de n-1 pourrait suffire    // OUTPUT // Estimated dry matter of the roots // t.ha-1
	public float tdevelop[];           // temporel pour sénescence
	public float cu[];                 // pas besoin du tableau temporel
	public int  nst1;
	public int  nst2;
	public float inn1;
	public float inn2;
	public float str1;
	public float str2;
	public float stu1;
	public float stu2;
	public float etm_etr1moy;
	public float etr_etm1moy;
	public float etr_etm2moy;
	public float etm_etr2moy;
	public float exofac1moy; // OUTPUT // "Mean value of the variable for excess water during the vegetative stage (from emergence  LEV  to fruit establishment  DRP )  " // 0-1
	public float exofac2moy; // OUTPUT // "Mean value of the variable for excess water during the reproductive stage (from fruit establishment  DRP  to maturity  MAT )  " // 0-1
	public float inn1moy; // OUTPUT // Average of index of nitrogen stress over the vegetative stage // SD
	public float inn2moy; // OUTPUT // Average index of nitrogen stress over the reproductive stage // SD
	public float swfac1moy; // OUTPUT // Average of index of stomatic water stress over the vegetative stage // 0-1
	public float swfac2moy; // OUTPUT // Average of index of stomatic water stress over the reproductive stage // 0-1
	public float turfac1moy; // OUTPUT // Average of index of turgescence water stress over the vegetative stage // 0-1
	public float turfac2moy; // OUTPUT // Average of index of turgescence water stress over the reproductive stage // 0-1
	public float mafrais_nrec;
	public float pdsfruitfrais_nrec;
	public float mabois_nrec;
	public float H2Orec_nrec;
	public float chargefruit_nrec;
	public float CNgrain_nrec;
	public float CNplante_nrec;
	public float QNgrain_nrec;
	public float Qngrain_ntailleveille;    // on enregsitre la valeur de QNgrain de la veille du jour de taille
	public float nbgraingel;
	public float QNplanteCtot;
	public float QNplantefauche;
	public float ctmoy; // OUTPUT // Air temperature integrated over the cropping season // degree C
	public float ctcult; // OUTPUT // Crop temperature (TCULT) integrated over the cropping season // degree C
	public float ctcultmax; // OUTPUT // maximum Crop temperature (TCULT) integrated over the cropping season // degree C
	public float cetp; // OUTPUT // Potential evapotranspiration (PET) integrated over the cropping season // mm
	public float crg; // OUTPUT // Global radiation integrated over the cropping season // Mj/m2
	public float cum_et0; // OUTPUT // maximum evapotranspiration (eop+eos)/mm
	public int  nbrepoussefauche;
	public float anitcoupe_anterieure;
	public float hautcoupe_anterieure;
	public float msresiduel_anterieure;
	public float lairesiduel_anterieure;
	public float ulai0;
	public float durvie0[];
	public int  codebbch0;
	public int  restit_anterieure;
	public float  mscoupemini_anterieure;
	public float tempfauche_realise;
	public float tempeff; // OUTPUT // Efficient temperature for growth // degree C
	public float tempfauche_ancours[];
	public float som_vraie_fauche[];
	public float densite; // OUTPUT // Actual sowing density // plants.m-2
	public float densiteequiv; // OUTPUT // Actual sowing density // plants.m-2
	public float stdrpdes0;
	public float str1intercoupe; // OUTPUT // stomatal water stress average during the cut (for forage crops in vegetative phase : emergence to maximum LAI )  // 0-1
	public float stu1intercoupe; // OUTPUT // turgescence water stress average during the cut (for forage crops in vegetative phase : emergence to maximum LAI )  // 0-1
	public float inn1intercoupe; // OUTPUT // nitrogen stress (inn) average during the cut (cut crop vegetative phase: emergence to maximum LAI)  // 0-1
	public float diftemp1intercoupe; // OUTPUT // mean difference between crop surface temperature and air temperature during the cut (cut crop vegetative phase: emergence to maximum LAI) // degree C
	public float str2intercoupe; // OUTPUT // stomatal water stress average during the cut (cut crop reproductive phase)  // 0-1
	public float stu2intercoupe; // OUTPUT // turgescence water stress average during the cut (for forage crops in reproductive phase: maximum LAI  to maturity)  // 0-1
	public float inn2intercoupe; // OUTPUT // nitrogen stress (inn) average during the cut (for forage crops in reproductive phase: maximum LAI  to maturity)  // 0-1
	public float diftemp2intercoupe; // OUTPUT // mean difference between crop surface temperature and air temperature during the cut (cut crop reproductive phase: maximum LAI  to maturity) // degree C
	public int  ficsort3;
	public int  ficdrat;
	public int  ilaxs_prairie[];
	public float  mortmasec; // OUTPUT // Dead tiller biomass  // t.ha-1
	public float  densitemax;
	public float  drlsenmortalle; // OUTPUT // Root biomass corresponding to dead tillers // t ha-1.d-1
	public int imontaisons; // OUTPUT // Date of start of stem elongation // julian day
	public float  mortalle; // OUTPUT // number of dead tillers per day // tillers.d-1
	public float densiteger;
	public float mafruit; // OUTPUT // Dry matter of harvested organs // t.ha-1
	public float matuber; // OUTPUT // Dry matter of harvested organs // t.ha-1
	public float matuber_rec; // DR 06/04/2012 on garde le matuber à la recolte
	public float msrec_fou; // OUTPUT // Dry matter of harvested organs for forages// t.ha-1
	public float dNdWcrit;
	public float dNdWmax;
	public float flurac; // OUTPUT // Nitrogen absorption flow associated with the limiting absorption capacity of the plant // kgN ha-1 j-1
	public float flusol; // OUTPUT // Nitrogen absorption flux associated with limiting transfer soil  --> root  // kgN ha-1 j-1
	public float profextN;       // OUTPUT // Average depth of Nitrogen absorption // cm
	public float profexteau;     // OUTPUT // Average depth of water absorption // cm
	public int age_prairie;      // OUTPUT // forage crop age from sowing // an
	public float RsurRUrac;      // OUTPUT // Fraction of available water reserve (R/RU) over the root profile // 0 à 1
	public float RUrac;      // OUTPUT // maximum available water reserve over the root profile // mm
	public float somcourmont;    // OUTPUT // Cumulatied units of development from the start of vernalisation // degree.days
	public float psibase;      // OUTPUT // Predawn leaf water potential potentiel foliaire de base // Mpascal
	public int nbjpourdecisemis;       // OUTPUT // "Number of days until sowing is launched when it's postponed by the   sowing decision  option activation" // days
	public int nbjpourdecirecolte;       // OUTPUT // "Number of days until harvest is launched when it's postponed by the  harvest decision  option activation" // days
	public float mortreserve;    // OUTPUT // Reserve biomass corresponding to dead tillers // t.ha-1.d-1
	public int codeperenne0;   // variable de sauvergarde  // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // // 0
	public float   masec_kg_ha;
	public float   mafruit_kg_ha;
	public float   mafeuil_kg_ha;
	public float   matigestruc_kg_ha;
	public float gel1_percent;    // OUTPUT // frost damage on foliage before amf // %
	public float gel2_percent;    // OUTPUT // frost damage on foliage after amf // %
	public float gel3_percent;    // OUTPUT // frost damage on foliage or fruits // %
	public float nbinflo_recal;   // OUTPUT // imposed number of inflorescences  // nb pl-1
	public int codebbch_output;       // OUTPUT // BBCH stage (see plant file) // 1-99
	public float ebmax_gr;   // OUTPUT // Maximum radiation use efficiency during the vegetative stage (AMF-DRP) // cg MJ-1
	public float fpari_gr;    // OUTPUT // radiation factor on the calculation of conversion efficiency // g MJ-1

	// ********************************
	// *       Variables AO/AS        *
	// ********************************

	// Les variables AO/AS pourraient être indicées non pas sur un indice fixe égal à 2
	// En effet, la plante principale n'a que la partie AS, et seule la plante dominée a AO/AS
	// On pourrait donc créer un taille de tableau dynamique selon la dominance de la plante.

	public float surf[]; // surface de la plante sur chaque composante A l'Ombre, Au Soleil      // OUTPUT // Fraction of surface in the shade // 0-1
	public float surfSous[]; // surface de la plante sur chaque composante A l'Ombre, Au Soleil
	public float tursla[];
	public float allocfruit[];    // OUTPUT // Allocation ratio of  assimilats to the  fruits 0 to 1  // 0-1
	public float mafeuiljaune[];    // OUTPUT // Dry matter of yellow leaves // t.ha-1
	public float somsenreste[];
	public float resperenne[];    // OUTPUT // C crop reserve, during the cropping season, or during the intercrop period (for perenial crops) // t ha-1
	public float deltares[];
	public float misenreserve[];
	public float innlai[];    // OUTPUT // Index of nitrogen stress active on leaf growth // P_innmin to 1
	public float lairogne[];
	public float biorogne[];
	public float sla[];    // OUTPUT // Specific surface area // cm2 g-1
	public float cumdltares[];
	public float innsenes[];    // OUTPUT // Index of nitrogen stress active on leaf death // P_innmin to 1
	public float senfac[];    // OUTPUT // Water stress index on senescence // 0-1
	public float deltatauxcouv[];
	public float mafeuiltombe[];    // OUTPUT // Dry matter of fallen leaves // t.ha-1
	public float dltamstombe[];
	public float mafeuil[];    // OUTPUT // Dry matter of leaves // t.ha-1
	public float mafeuilverte[];    // OUTPUT // Dry matter of green leaves // t.ha-1
	public float matigestruc[];    // OUTPUT // Dry matter of stems (only structural parts) // t.ha-1
	public float mareserve[];
	public float masecveg[];    // OUTPUT // Vegetative dry matter // t.ha-1
	public float maenfruit[];    // OUTPUT // Dry matter of harvested organ envelopes // t.ha-1
	public float pfeuiljaune[];    // OUTPUT // Proportion of yellow leaves in total biomass // 0-1
	public float ptigestruc[];    // OUTPUT // Proportion of structural stems in total biomass // 0-1
	public float penfruit[];    // OUTPUT // Proportion of fruit envelopes in total biomass // 0-1
	public float preserve[];    // OUTPUT // Proportion of reserve in the total biomass // 0-1
	public float deshyd[];
	public float eaufruit[];
	public float frplusp[];
	public float sucre[];    // OUTPUT // Sugar content of fresh harvested organs // % (of fresh weight)
	public float huile[];    // OUTPUT // Oil content of fresh harvested organs // % (of fresh weight)
	public float sucrems[];
	public float huilems[];
	public float biorognecum[];
	public float lairognecum[];
	public float pgraingel[];
	public float laieffcum[];
	public float bioeffcum[];
	public float rdtint[];
	public float nbfrint[];
	public float pdsfruittot[];
	public float h2orec[];    // OUTPUT // Water content of harvested organs // 0-1
	public float sucreder[];
	public float huileder[];
	public float teauint[];
	public float fapar[];    // OUTPUT // Proportion of radiation intercepted // 0-1
	public float mafeuilp[];
	public float mabois[];    // OUTPUT // Prunning dry weight // t.ha-1
	public float eai[];
	public float spfruit[];    // OUTPUT // Index of trophic stress applied to the number of fruits // 0 to 1
	public float dltaremobil[];    // OUTPUT // Amount of perennial reserves remobilised // g.m-2.j-1
	public float mafrais[];    // OUTPUT // Aboveground fresh matter // t.ha-1
	public float mafraisfeuille[];
	public float mafraisrec[];
	public float mafraisres[];
	public float mafraistige[];
	public float masecvegp[];
	public float durvie[];            // OUTPUT // Actual life span of the leaf surface //  degree C
	public float abso[];              // OUTPUT // Nitrogen absorption rate by plant  // kg N ha-1
	public float pfeuilverte[];       // OUTPUT // Proportion of green leaves in total non-senescent biomass // 0-1
	public float CNgrain[];  // OUTPUT // Nitrogen concentration of grains  // %
	public float CNplante[];    // OUTPUT // Nitrogen concentration of entire plant  // %
	public float deltai[];        // OUTPUT // Daily increase of the green leaf index // m2 leafs.m-2 soil
	public float demande[];    // OUTPUT // Daily nitrogen need of the plant   // kgN.ha-1.j-1
	public float dltags[];    // OUTPUT // Growth rate of the grains  // t ha-1.j-1
	public float dltamsen[];   // OUTPUT // Senescence rate // t ha-1 j-1
	public float dltams[];            // OUTPUT // Growth rate of the plant  // t ha-1.j-1
	public float dltamsres[];
	public float dltaisenat[];
	public float dltaisen[];    // OUTPUT // Daily increase of the senescent leaf index // m2.m-2 sol.j-1
	public float eop[];    // OUTPUT // Maximum transpiration flux  // mm
	public float ep[];    // OUTPUT // Actual transpiration flux  // mm j-1
	public float epsib[];    // OUTPUT // Radiation use efficiency  // t ha-1.Mj-1. m-2
	public float epz[]; 
	public float fpft[];    // OUTPUT // Sink strength of fruits  // g.m-2 j-1
	public float hauteur[];    // OUTPUT // Height of canopy // m
	public float inn0[];
	public float inn[];    // OUTPUT // Nitrogen nutrition index (satisfaction of nitrogen needs ) // 0-1
	public float inns[];    // OUTPUT // Index of nitrogen stress active on growth in biomass // P_innmin to 1
	public float interpluie[];    // OUTPUT // Water intercepted by leaves // mm
	public float irazo[];             // OUTPUT // Nitrogen harvest index  // gN grain gN plant-1
	public float ircarb[];           // OUTPUT // Carbon harvest index // g  grain g plant-1
	public float lai[];   // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
	public float laisen[];             // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
	public float magrain[];          
	public float masec[];          // OUTPUT // Aboveground dry matter  // t.ha-1
	public float masecneo[];    // OUTPUT // Newly-formed dry matter  // t.ha-1
	public float masecneov[];
	public float masecpartiel[];
	public float mouill[];
	public float msneojaune[];    // OUTPUT // Newly-formed senescent dry matter  // t.ha-1
	public float msneojaunev[];
	public float msres[];
	public float msresv[];
	public float msresjaune[];    // OUTPUT // Senescent residual dry matter  // t.ha-1
	public float msresjaunev[];
	public float msjaune[];    // OUTPUT // Senescent dry matter  // t.ha-1
	public float naer[];
	public float nbgrains[];
	public float nbgrainsv[];
	public float nfruit[];    // OUTPUT // Number of fruits in box 5 // nb fruits
	public float nfruitv[];
	public float nfruitnou[];
	public float pdsfruitfrais[];    // OUTPUT // Total weight of fresh fruits // g m-2
	public float pgrain[];
	public float pdsfruit[];   // OUTPUT // Weight of fruits in box 3 // g m-2
	public float pousfruit[];    // OUTPUT // Number of fruits transferred from one box to the next  // nb fruits
	public float QNgrain[];     // OUTPUT // Amount of nitrogen in harvested organs (grains / fruits) // kg ha-1
	public float QNplante[];          // OUTPUT // Amount of nitrogen taken up by the plant  // kgN.ha-1
	public float QCplante;      // OUTPUT // Amount of C in the aerial part of the plant  // kg.ha-1
	public float raint[];    // OUTPUT // Photosynthetic active radiation intercepted by the canopy  // Mj.m-2
	public float remobilj[];    // OUTPUT // Amount of biomass remobilized on a daily basis for the fruits  // g.m-2 j-1
	public float sourcepuits[];    // OUTPUT // Pool/sink ratio // 0-1
	public float splai[];    // OUTPUT // Pool/sink ratio applied to leaves // 0-1
	public float swfac[];    // OUTPUT // Index of stomatic water stress  // 0-1
	public float teaugrain[];
	public float turfac[];    // OUTPUT // Index of turgescence water stress  // 0-1
	public float vitmoy[];    // OUTPUT // mean growth rate of the canopy (dans le livre c'est en g mais ca colle pas)  // g.m-2.d-1
	public float offrenod[];    // OUTPUT // Amount of fixed nitrogen by nodules // kg.ha-1.j-1
	public float fixreel[];    // OUTPUT // Actual rate of symbiotic uptake // kg ha-1 j-1
	public float fixpot[];    // OUTPUT // Potential rate of symbiotic uptake // kg ha-1 j-1
	public float fixmaxvar[];
	public float Qfix[];    // OUTPUT // Quantity of nitrogen fixed by nodosities // kg.ha-1
	public float deltahauteur[];
	public float strphot[];
	public float strphotveille[];
	public float masecdil[];
	public float laimax[];  // OUTPUT // maximal LAI // SD
	public float H2Orec_percent;  // OUTPUT // Water content of harvested organs in percentage// %
	public float sucre_percent;    // OUTPUT // Sugar content of fresh harvested organs // % (of fresh weight)
	public float huile_percent;    // OUTPUT // Oil content of fresh harvested organs // % (of fresh weight)
	public float et0;  // OUTPUT // eos+eop // mm
	public float QNexport; // N exporté
	public int day_cut;  //jour de la coupe precedente

	// ********************************
	// *       MODIF HISAFE           *
	// ********************************
	// Je regroupe ici tous les tableaux dont j'ai enlevé la dimension temporelle
	public float rlveille[];         // root length day-1 
	public float rljour[];           // root length day 
	
	public float upvt;                // OUTPUT // Daily development unit  // degree.days

	public float upt;  
	public float pfeuil[];           // OUTPUT // Proportion of leaves in total biomass // 0-1
	public float fpv[];              // OUTPUT // Sink strength of developing leaves // g.j-1.m-2

	public float photnet[];          // OUTPUT // net photosynthesis // t ha-1.j-1  

	// ********************************
	// *       MODIF HISAFE           *
	// ********************************
	// Je regroupe ici toutes les varibles que j'ai enlevé de Stics_Transit_
	  public float bdilI;  
	  public float adilI;  
	  public float adilmaxI;  
	  public float bdilmaxI; 
	  public float inni[];       	// pour chaque plante, AO/AS  
	  public float deltabso[];  	// pour chaque plante, AO/AS  
	  public float dltamsN[];    	// pour chaque plante, AO/AS 
	  public boolean humectation;  
	  public int nbjhumec;
	  public float pluiesemis; 
	  public float dltaremobilN[];      	// 2 plantes, AO et AS  
	  public float somtemphumec; 
	  public float LAIapex; 
	  public int nmontaison;
	  public int onestan2; 

	// ********************************
	// *       MODIF HISAFE           *
	// ********************************
	// Je regroupe ici toutes les varibles que j'ai enlevé de Stics_Commun_

	   
	public SafeSticsCrop () {
		
		estDominante = true;
		
		P_codevar = new int[30];  
		P_adens= new float[30];      
		P_afruitpot= new float[30];     
		P_croirac= new float[30];      
		P_dureefruit= new float[30];    
		P_durvieF= new float[30];       
		P_jvc= new float[30];          
		P_nbgrmax= new float[30];      
		P_pgrainmaxi= new float[30];  
		P_stamflax= new float[30];      
		P_stlevamf= new float[30];     
		P_stlevdrp= new float[30];      
		P_stflodrp= new float[30];      
		P_stlaxsen= new float[30];      
		P_stsenlan= new float[30];     
		P_stdrpmat= new float[30];      
		P_sensiphot= new float[30];     
		P_stdrpdes= new float[30];   
		
		P_densinitial= new float[5]; 
		mafeuiltombe0 = new float[3];
		masecneo0 = new float[3];
		msneojaune0 = new float[3];
		dltamsen0 = new float[3];
		mafeuiljaune0 = new float[3];		
		numjtrav = new int[10];
		numjres= new int[10];
		nfauche= new int[10];
		nrecint= new int[20];
		neclair= new int[10];
		LRACH= new float[5];
		lracz= new float[1000];
		QCplantetombe= new float[3];
		QNplantetombe= new float[3];
		udevlaires= new float[10];
		precrac= new float[1000];
		lracsenz= new float[1000];
		drl= new float[367000];	
		flrac= new float[1000];
		ulai= new float[367];
		humirac_z= new float[1000];  
		efnrac_z= new float[1000]; 
		irrigprof= new float[1001];
		dtj= new float[367];
		upobs= new float[367];
		racnoy= new float[367];              
		msrac= new float[367];            
		tdevelop= new float[367];          
		cu= new float[367];                 
		durvie0= new float[3];
		tempfauche_ancours= new float[21];
		som_vraie_fauche= new float[20];
		ilaxs_prairie= new int[10];
		surf = new float[2]; 
		surfSous= new float[2]; 
		tursla= new float[3];
		allocfruit= new float[3];  
		mafeuiljaune= new float[3];   
		somsenreste= new float[3];
		resperenne= new float[3];   
		deltares= new float[3];
		misenreserve= new float[3];
		innlai= new float[3];    
		lairogne= new float[3];
		biorogne= new float[3];
		sla= new float[3];    
		cumdltares= new float[3];
		innsenes= new float[3];    
		senfac= new float[3];  
		deltatauxcouv= new float[3];
		mafeuiltombe= new float[3];   
		dltamstombe= new float[3];
		mafeuil= new float[3];    
		mafeuilverte= new float[3];   
		matigestruc= new float[3];   
		mareserve= new float[3];
		masecveg= new float[3];   
		maenfruit= new float[3];    
		pfeuiljaune= new float[3];  
		ptigestruc= new float[3];   
		penfruit= new float[3];    
		preserve= new float[3];  
		deshyd= new float[153];
		eaufruit= new float[153]; 
		frplusp= new float[3];
		sucre= new float[3];   
		huile= new float[3];   
		sucrems= new float[3];
		huilems= new float[3];
		biorognecum= new float[3];
		lairognecum= new float[3];
		pgraingel= new float[3];
		laieffcum= new float[3];
		bioeffcum= new float[3];
		rdtint= new float[63];
		nbfrint= new float[60];
		pdsfruittot= new float[3];
		h2orec= new float[3];    
		sucreder= new float[3];
		huileder= new float[3];
		teauint= new float[60];
		fapar= new float[3];    
		mafeuilp= new float[3];
		mabois= new float[3];   
		eai= new float[3];
		spfruit= new float[3];    
		dltaremobil= new float[3];    
		mafrais= new float[3];    
		mafraisfeuille= new float[3];
		mafraisrec= new float[3];
		mafraisres= new float[3];
		mafraistige= new float[3];
		masecvegp= new float[3];
		durvie= new float[1101];
		abso= new float[1101];
		pfeuilverte= new float[1101];
		CNgrain= new float[3];  
		CNplante= new float[3];  
		deltai= new float[1101];
		demande= new float[3];   
		dltags= new float[3];    
		dltamsen = new float[3];  
		dltams = new float[1101];
		dltamsres= new float[3];
		dltaisenat= new float[3];
		dltaisen= new float[3];    
		eop= new float[3];    
		ep= new float[3];    
		epsib= new float[3]; 
		epz= new float[3000]; 
		fpft= new float[3];    
		hauteur= new float[3];    
		inn0= new float[3];
		inn= new float[3];    
		inns= new float[3];    
		interpluie= new float[3];   
		irazo= new float[1101]; 
		ircarb = new float[1101]; 
		lai= new float[1101]; 
		laisen= new float[1101]; 
		magrain= new float[1101]; 
		masec= new float[1101]; 
		masecneo= new float[3];    
		masecneov= new float[3];
		masecpartiel= new float[3];
		mouill= new float[3];
		msneojaune= new float[3];    
		msneojaunev= new float[3];
		msres= new float[3];
		msresv= new float[3];
		msresjaune= new float[3];    
		msresjaunev= new float[3];
		msjaune= new float[3];   
		naer= new float[3];
		nbgrains= new float[3];
		nbgrainsv= new float[3];
		nfruit= new float[150];
		nfruitv= new float[150]; 
		nfruitnou= new float[3];
		pdsfruitfrais= new float[3];    
		pgrain= new float[3];
		pdsfruit= new float[150];
		pousfruit= new float[3];   
		QNgrain= new float[3];     
		QNplante= new float[1101]; 
		raint= new float[3];    
		remobilj= new float[3];   
		sourcepuits= new float[3];    
		splai= new float[3];    
		swfac= new float[3]; 
		teaugrain= new float[3];
		turfac= new float[3];    
		vitmoy= new float[3];    
		offrenod= new float[3];   
		fixreel= new float[3];    
		fixpot= new float[3];    
		fixmaxvar= new float[3];
		Qfix= new float[3];    
		deltahauteur= new float[3];
		strphot= new float[3];
		strphotveille= new float[3];
		masecdil= new float[3];
		laimax= new float[3];  
 

		rljour= new float[1000];
		rlveille= new float[1000];
		pfeuil= new float[3]; 
		fpv= new float[3]; 
		
		photnet = new float[3]; 
		

		inni = new float[3];       
		deltabso = new float[3];  
		dltamsN = new float[3]; 
		dltaremobilN = new float[3];
		  

	}
	/**
	* Constructor for cloning 
	*/
	
	public SafeSticsCrop (SafeSticsCrop origin) {
		
		
		estDominante = origin.estDominante;
		this.ipl=origin.ipl;

		//***********************
		//  parametres plantes
		//***********************

		this.P_adil=origin.P_adil;   // PARAMETER // Parameter of the critical curve of nitrogen needs [Nplante]=P_adil MS^(-P_bdil) // N% MS // PARPLT // 1
		this.P_bdens=origin.P_bdens;   // PARAMETER // minimal density from which interplant competition starts // plants m-2 // PARPLT // 1
		this.P_bdil=origin.P_bdil;   // PARAMETER // parameter of the critical curve of nitrogen needs [Nplante]=P_adil MS^(-P_bdil) // SD // PARPLT // 1
		this.P_belong=origin.P_belong;   // PARAMETER // parameter of the curve of coleoptile elongation // degree.days -1 // PARPLT // 1
		this.P_celong=origin.P_celong;   // PARAMETER // parameter of the subsoil plantlet elongation curve // SD // PARPLT // 1
		this.P_cgrain=origin.P_cgrain;   // PARAMETER // slope of the relationship between grain number and growth rate  // grains gMS -1 jour // PARPLT // 1
		this.P_cgrainv0=origin.P_cgrainv0;   // PARAMETER // number of grains produced when growth rate is zero // grains m-2 // PARPLT // 1
		this.P_coefmshaut=origin.P_coefmshaut;   // PARAMETER // ratio biomass/ useful height cut of crops  // t ha-1 m-1 // PARPLT // 1
		this.P_contrdamax=origin.P_contrdamax;   // PARAMETER // maximal root growth reduction due to soil strenghtness (high bulk density) // SD // PARPLT // 1
		this.P_debsenrac=origin.P_debsenrac;   // PARAMETER // Sum of degrees.days defining the beginning of root senescence (life-time of a root) // degree.days // PARPLT // 1
		this.P_deshydbase=origin.P_deshydbase;   // PARAMETER // phenological rate of evolution of fruit water content (>0 or <0) // g water.g MF-1.degree C-1 // PARPLT // 1
		this.P_dlaimax=origin.P_dlaimax;   // PARAMETER // Maximum rate of the setting up of LAI // m2 leaf plant-1 degree d-1 // PARPLT // 1
		this.P_draclong=origin.P_draclong;   // PARAMETER // Maximum rate of root length production // cm root plant-1 degree.days-1 // PARPLT // 1
		this.P_efcroijuv =origin.P_efcroijuv;  // PARAMETER // Maximum radiation use efficiency during the juvenile phase(LEV-AMF) // g MJ-1 // PARPLT // 1
		this.P_efcroirepro=origin.P_efcroirepro;   // PARAMETER // Maximum radiation use efficiency during the grain filling phase (DRP-MAT) // g MJ-1 // PARPLT // 1
		this.P_efcroiveg=origin.P_efcroiveg;   // PARAMETER // Maximum radiation use efficiency during the vegetative stage (AMF-DRP) // g MJ-1 // PARPLT // 1
		this.P_elmax=origin.P_elmax;   // PARAMETER // Maximum elongation of the coleoptile in darkness condition // cm // PARPLT // 1
		this.P_extin=origin.P_extin;   // PARAMETER // extinction coefficient of photosynthetic active radiation canopy // SD // PARPLT // 1
		this.P_fixmax=origin.P_fixmax;   // PARAMETER // maximal symbiotic fixation // kg ha-1 j-1 // PARPLT // 1      // OUTPUT // Maximal symbiotic uptake // kg ha-1 j-1
		this.P_h2ofeuiljaune=origin.P_h2ofeuiljaune;   // PARAMETER // water content of yellow leaves // g water g-1 MF // PARPLT // 1
		this.P_h2ofeuilverte=origin.P_h2ofeuilverte;   // PARAMETER // water content of green leaves // g water g-1 MF // PARPLT // 1
		this.P_h2ofrvert=origin.P_h2ofrvert;   // PARAMETER // water content of fruits before the beginning of hydrous evolution (DEBDESHYD) // g water g-1 MF // PARPLT // 1
		this.P_h2oreserve=origin.P_h2oreserve;   // PARAMETER // reserve water content // g eau g-1 MF // PARPLT // 1
		this.P_h2otigestruc=origin.P_h2otigestruc;   // PARAMETER // structural stem part water content // g eau g-1 MF // PARPLT // 1
		this.P_inflomax=origin.P_inflomax;   // PARAMETER // maximal number of inflorescences per plant // nb pl-1 // PARPLT // 1
		this.P_Kmabs1=origin.P_Kmabs1;   // PARAMETER // Constant of nitrogen uptake by roots for the high affinity system // µmole. cm root-1 // PARPLT // 1
		this.P_Kmabs2=origin.P_Kmabs2;   // PARAMETER // Constant of nitrogen uptake by roots for the low affinity system // µmole. cm root-1 // PARPLT // 1
		this.P_kmax=origin.P_kmax;   // PARAMETER // Maximum crop coefficient for water requirements (= ETM/ETP) // SD // PARPLT // 1
		this.P_kstemflow=origin.P_kstemflow;   // PARAMETER // Extinction Coefficient connecting leaf area index to stemflow // * // PARPLT // 1
		this.P_longsperac=origin.P_longsperac;   // PARAMETER // specific root length // cm g-1 // PARPLT // 1
		this.P_lvfront=origin.P_lvfront;   // PARAMETER // Root density at the root front // cm root.cm-3 soil // PARPLT // 1
		this.P_mouillabil=origin.P_mouillabil;   // PARAMETER // maximum wettability of leaves // mm LAI-1 // PARPLT // 1
		this.P_nbinflo=origin.P_nbinflo;   // PARAMETER // imposed number of inflorescences  // nb pl-1 // PARPLT // 1      // OUTPUT // Number of inflorescences // SD
		this.P_pentinflores=origin.P_pentinflores;   // PARAMETER // parameter of the calculation of the inflorescences number  // SD // PARPLT // 1
		this.P_phosat=origin.P_phosat;   // PARAMETER // saturating photoperiod // hours // PARPLT // 1
		this.P_psisto=origin.P_psisto;   // PARAMETER // absolute value of the potential of stomatal closing // bars // PARPLT // 1
		this.P_psiturg=origin.P_psiturg;   // PARAMETER // absolute value of the potential of the beginning of decrease of the cellular extension // bars // PARPLT // 1
		this.P_rsmin=origin.P_rsmin;   // PARAMETER // Minimal stomatal resistance of leaves // s m-1 // PARPLT // 1
		this.P_sensrsec=origin.P_sensrsec;   // PARAMETER // root sensitivity to drought (1=insensitive) // SD // PARPLT // 1
		this.P_spfrmax=origin.P_spfrmax;   // PARAMETER // maximal sources/sinks value allowing the trophic stress calculation for fruit onset // SD // PARPLT // 1
		this.P_spfrmin=origin.P_spfrmin;   // PARAMETER // minimal sources/sinks value allowing the trophic stress calculation for fruit onset // SD // PARPLT // 1
		this.P_splaimax=origin.P_splaimax;   // PARAMETER // maximal sources/sinks value allowing the trophic stress calculation for leaf growing // SD // PARPLT // 1
		this.P_splaimin=origin.P_splaimin;   // PARAMETER // Minimal value of ratio sources/sinks for the leaf growth  // between 0 and 1 // PARPLT // 1
		this.P_stemflowmax=origin.P_stemflowmax;   // PARAMETER // Maximal fraction of rainfall which flows out along the stems  // between 0 and 1 // PARPLT // 1
		this.P_stpltger=origin.P_stpltger;   // PARAMETER // Sum of development allowing germination // degree.days // PARPLT // 1
		this.P_tcmin=origin.P_tcmin;   // PARAMETER // Minimum temperature of growth // degree C // PARPLT // 1
		this.P_tcmax=origin.P_tcmax;   // PARAMETER // Maximum temperature of growth // degree C // PARPLT // 1
		this.P_tdebgel=origin.P_tdebgel;   // PARAMETER // temperature of frost beginning // degree C // PARPLT // 1
		this.P_tdmin=origin.P_tdmin;   // PARAMETER // Minimum threshold temperature for development // degree C // PARPLT // 1
		this.P_tdmax=origin.P_tdmax;   // PARAMETER // Maximum threshold temperature for development // degree C // PARPLT // 1
		this.P_tempdeshyd=origin.P_tempdeshyd;   // PARAMETER // increase in the fruit dehydration due to the increase of crop temperature (Tcult-Tair) // % water degree C-1 // PARPLT // 1
		this.P_tgellev10=origin.P_tgellev10;   // PARAMETER // temperature corresponding to 10% of frost damage on the plantlet  // degree C // PARPLT // 1
		this.P_tgellev90=origin.P_tgellev90;   // PARAMETER // temperature corresponding to 90% of frost damage on the plantlet  // degree C // PARPLT // 1
		this.P_tgmin=origin.P_tgmin;   // PARAMETER // Minimum threshold temperature used in emergence stage // degree C // PARPLT // 1
		this.P_tletale=origin.P_tletale;   // PARAMETER // lethal temperature for the plant // degree C // PARPLT // 1
		this.P_tmaxremp=origin.P_tmaxremp;   // PARAMETER // maximal temperature for grain filling // degree C // PARPLT // 1
		this.P_tminremp=origin.P_tminremp;   // PARAMETER // Minimal temperature for grain filling // degree C // PARPLT // 1
		this.P_vitircarb=origin.P_vitircarb;   // PARAMETER // Rate of increase of the carbon harvest index // g grain g plant -1 day-1 // PARPLT // 1
		this.P_vitirazo=origin.P_vitirazo;   // PARAMETER // Rate of increase of the nitrogen harvest index // g grain g plant -1 day-1 // PARPLT // 1
		this.P_vitpropsucre=origin.P_vitpropsucre;   // PARAMETER // increase rate of sugar harvest index  // g sugar g MS-1  j-1 // PARPLT // 1
		this.P_vitprophuile=origin.P_vitprophuile;   // PARAMETER // increase rate of oil harvest index  // g oil g MS-1 j-1 // PARPLT // 1
		this.P_Vmax1=origin.P_Vmax1;   // PARAMETER // Rate of slow nitrogen absorption (high affinity system) // µmole cm-1 h-1 // PARPLT // 1
		this.P_Vmax2=origin.P_Vmax2;   // PARAMETER // Rate of rapid nitrogen absorption (high affinity system) // µmole cm-1 h-1 // PARPLT // 1
		this.P_zlabour=origin.P_zlabour;   // PARAMETER // Depth of ploughing  // cm // PARPLT // 1
		this.P_zprlim=origin.P_zprlim;   // PARAMETER // Maximum depth of the root profile for the reference profile // cm  // PARPLT // 1
		this.P_zpente=origin.P_zpente;   // PARAMETER // Depth where the root density is ½ of the surface root density for the reference profile // cm  // PARPLT // 1
		this.P_adilmax=origin.P_adilmax;   // PARAMETER // Parameter of the maximum curve of nitrogen needs [Nplante]=P_adilmax MS^(-P_bdilmax) // N% MS // PARPLT // 1
		this.P_bdilmax=origin.P_bdilmax;   // PARAMETER // Parameter of the maximum curve of nitrogen needs [Nplante]=P_adilmax MS^(-P_bdilmax) // SD // PARPLT // 1
		this.P_masecNmax=origin.P_masecNmax;   // PARAMETER // Aerial biomass  on and after there is nitrogen dilution (critical and maximal curves) // t ha-1 // PARPLT // 1
		this.P_INNmin=origin.P_INNmin;   // PARAMETER // Minimum value of INN authorised for the crop // SD // PARPLT // 1
		this.P_inngrain1=origin.P_inngrain1;   // PARAMETER // INN minimal for net absorption of nitrogen during grain filling  // SD // PARPLT // 1
		this.P_inngrain2=origin.P_inngrain2;   // PARAMETER // INN minimal for null net absorption of nitrogen during grain filling  // SD // PARPLT // 1
		this.P_stlevdno=origin.P_stlevdno;   // PARAMETER // phasic duration between emergence and the beginning of nodulation  // degrés.jours // PARPLT // 1
		this.P_stdnofno=origin.P_stdnofno;   // PARAMETER // phasic duration between the beginning and the end of nodulation // degree.days // PARPLT // 1
		this.P_stfnofvino=origin.P_stfnofvino;   // PARAMETER // phasic duration between the end of the nodulation and the end of the nodule life   // degrés.jours // PARPLT // 1
		this.P_vitno=origin.P_vitno;   // PARAMETER // "rate of nodule onset expressed as a proportion of  P_fixmax   per degree day   // nb degrés.jour-1 // PARPLT // 1
		this.P_profnod=origin.P_profnod;   // PARAMETER // nodulation depth // cm // PARPLT // 1
		this.P_concNnodseuil=origin.P_concNnodseuil;   // PARAMETER // maximal soil nitrogen threshold for nodule onset  // kg.ha-1.mm-1 // PARPLT // 1
		this.P_concNrac0=origin.P_concNrac0;   // PARAMETER // soil nitrogen threshold forbiding nodule activity // kg.ha-1.mm-1 // PARPLT // 1
		this.P_concNrac100=origin.P_concNrac100;   // PARAMETER // soil nitrogen threshold for full nodule activity  // kg.ha-1.mm-1 // PARPLT // 1
		this.P_tempnod1=origin.P_tempnod1;   // PARAMETER // cardinal temperature for nodule activity   // degree C // PARPLT // 1
		this.P_tempnod2=origin.P_tempnod2;   // PARAMETER // cardinal temperature for nodule activity   // degree C // PARPLT // 1
		this.P_tempnod3=origin.P_tempnod3;   // PARAMETER // cardinal temperature for nodule activity   // degree C // PARPLT // 1
		this.P_tempnod4=origin.P_tempnod4;   // PARAMETER // cardinal temperature for nodule activity   // degree C // PARPLT // 1
		this.P_coeflevamf=origin.P_coeflevamf;   // PARAMETER // multiplier coefficient of the development phase LEVAMF to use crop temperature // SD // PARPLT // 1
		this.P_coefamflax=origin.P_coefamflax;   // PARAMETER // multiplier coefficient of the development phase AMFLAX to use crop temperature // SD // PARPLT // 1
		this.P_coeflaxsen=origin.P_coeflaxsen;   // PARAMETER // multiplier coefficient of the development phase LAXSEN to use crop temperature // SD // PARPLT // 1
		this.P_coefsenlan=origin.P_coefsenlan;   // PARAMETER // cultiplier coefficient of the development phase SENLAN to use crop temperature // SD // PARPLT // 1
		this.P_coefdrpmat=origin.P_coefdrpmat;   // PARAMETER // multiplier coefficient of the development phase DRPMAT to use crop temperature // SD // PARPLT // 1
		this.P_coefflodrp=origin.P_coefflodrp;   // PARAMETER // multiplier coefficient of the development phase FLODRP to use crop temperature // SD // PARPLT // 1
		this.P_coeflevdrp=origin.P_coeflevdrp;   // PARAMETER // multiplier coefficient of the development phase LEVDRP to use crop temperature // SD // PARPLT // 1
		this.P_ratiodurvieI=origin.P_ratiodurvieI;   // PARAMETER // life span of early leaves expressed as a proportion of the life span of the last leaves emitted P_DURVIEF // SD // PARPLT // 1
		this.P_ratiosen=origin.P_ratiosen;   // PARAMETER // fraction of senescent biomass (by ratio at the total biomass) // between 0 and 1 // PARPLT // 1
		this.P_ampfroid=origin.P_ampfroid;   // PARAMETER // semi thermal amplitude thermique for vernalising effect // degree C // PARPLT // 1
		this.P_laicomp=origin.P_laicomp;   // PARAMETER // LAI from which starts competition inbetween plants // m2 m-2 // PARPLT // 1
		this.P_phobase=origin.P_phobase;   // PARAMETER // Base photoperiod  // hours // PARPLT // 1
		this.P_stressdev=origin.P_stressdev;   // PARAMETER // maximum phasic delay allowed  due to stresses  // SD // PARPLT // 1
		this.P_jvcmini=origin.P_jvcmini;   // PARAMETER // Minimum number of vernalising days  // day // PARPLT // 1
		this.P_julvernal=origin.P_julvernal;   // PARAMETER // julian day (between 1 and 365) accounting for the beginning of vernalisation for perennial crops // julian day // PARPLT // 1
		this.P_tfroid=origin.P_tfroid;   // PARAMETER // optimal temperature for vernalisation // degree C // PARPLT // 1
		this.P_q10=origin.P_q10;   // PARAMETER // P_Q10 used for the dormancy break calculation  // SD // PARPLT // 1
		this.P_stdordebour=origin.P_stdordebour;   // PARAMETER // phasic duration between the dormancy break and the bud break  // degree.days // PARPLT // 1
		this.P_phyllotherme=origin.P_phyllotherme;   // PARAMETER // thermal duration between the apparition of two successive leaves on the main stem // degree C day // PARPLT // 1
		this.P_laiplantule=origin.P_laiplantule;   // PARAMETER // Plantlet Leaf index at the plantation // m2 leaf  m-2 soil // PARPLT // 1
		this.P_masecplantule=origin.P_masecplantule;   // PARAMETER // initial shoot biomass of plantlet // t ha-1 // PARPLT // 1
		this.P_zracplantule=origin.P_zracplantule;   // PARAMETER // depth of the initial root front of the plantlet  // cm // PARPLT // 1
		this.P_hautbase=origin.P_hautbase;   // PARAMETER // Base height of crop // m // PARPLT // 1
		this.P_hautmax=origin.P_hautmax;   // PARAMETER // Maximum height of crop // m // PARPLT // 1
		this.P_vlaimax=origin.P_vlaimax;   // PARAMETER // ULAI at inflection point of the function DELTAI=f(ULAI) // SD // PARPLT // 1
		this.P_pentlaimax=origin.P_pentlaimax;   // PARAMETER // parameter of the logistic curve of LAI growth  // SD // PARPLT // 1
		this.P_udlaimax=origin.P_udlaimax;   // PARAMETER // ulai from which the rate of leaf growth decreases  // SD // PARPLT // 1
		this.P_abscission=origin.P_abscission;  // PARAMETER // sensescent leaf proportion falling on the soil // SD // PARPLT // 1
		this.P_parazofmorte=origin.P_parazofmorte;   // PARAMETER // parameter of proportionality between the C/N of died leaves and the INN // SD // PARPLT // 1
		this.P_innturgmin=origin.P_innturgmin;   // PARAMETER // parameter of the nitrogen stress function active on leaf expansion (INNLAI), that is a bilinear function passing by the point of coordinate (P_innmin, P_innturgmin) // SD // PARPLT // 1
		this.P_dlaimin=origin.P_dlaimin;   // PARAMETER // accelerating parameter for the lai growth rate // SD // PARAMV6/PLT // 1
		this.P_tustressmin=origin.P_tustressmin;   // PARAMETER //  threshold stress (min(turfac,inns)) under which there is an effect on the LAI (supplementary senescence by ratio at the natural senescence) // SD // PARPLT // 1
		this.P_durviesupmax=origin.P_durviesupmax;   // PARAMETER // proportion of additional lifespan due to an overfertilization // SD // PARPLT // 1
		this.P_innsen=origin.P_innsen;   // PARAMETER // parameter of the nitrogen stress function active on senescence (innnsenes), bilinear function of the INN using the point (P_innmin, P_innsen) // SD // PARPLT // 1
		this.P_rapsenturg=origin.P_rapsenturg;   // PARAMETER // threshold soil water content active to simulate water sensecence stress as a proportion of the turgor stress // SD // PARPLT // 1
		this.P_dlaimaxbrut=origin.P_dlaimaxbrut;   // PARAMETER // Maximum rate of the setting up of LAI // m2 leaf plant-1 degree d-1 // PARPLT // 1
		this.P_tauxrecouvmax=origin.P_tauxrecouvmax;   // PARAMETER // maximal soil cover rate // m2 plante m-2 sol // PARPLT // 1
		this.P_tauxrecouvkmax=origin.P_tauxrecouvkmax;   // PARAMETER // soil cover rate corresponding to the maximal crop coefficient for water requirement  // m2 plante m-2 sol // PARPLT // 1
		this.P_pentrecouv=origin.P_pentrecouv;   // PARAMETER // parameter of the logistic curve of the soil cover rate increase // * // PARPLT // 1
		this.P_infrecouv=origin.P_infrecouv;   // PARAMETER // ulai at the stage AMF (inflexion point of the soil cover rate increase) // SD // PARPLT // 1
		this.P_rapforme=origin.P_rapforme;   // PARAMETER // Ratio thickness/width of the crop form (negative when the base of the form < top) // SD // PARPLT // 1
		this.P_ktrou=origin.P_ktrou;   // PARAMETER // Extinction Coefficient of PAR through the crop  (radiation transfer) // * // PARPLT // 1
		this.P_adfol=origin.P_adfol;   // PARAMETER // parameter determining the leaf density evolution within the chosen shape // m-1 // PARPLT // 1
		this.P_dfolbas=origin.P_dfolbas;   // PARAMETER // minimal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1
		this.P_dfolhaut=origin.P_dfolhaut;   // PARAMETER // maximal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1
		this.P_remobres=origin.P_remobres;   // PARAMETER // proportion of daily remobilisable carbon reserve // SD // PARPLT // 1
		this.P_temin=origin.P_temin;   // PARAMETER // Minimum threshold temperature for development // degree C // PARPLT // 1
		this.P_teopt=origin.P_teopt;   // PARAMETER // Optimal temperature for the biomass growth // degree C // PARPLT // 1
		this.P_temax=origin.P_temax;   // PARAMETER // Maximal threshold temperature for the biomass growth  // degree C // PARPLT // 1
		this.P_teoptbis=origin.P_teoptbis;   // PARAMETER // optimal temperature for the biomass growth (if there is a plateau between P_teopt and P_teoptbis) // degree C // PARPLT // 1
		this.P_slamin=origin.P_slamin;   // PARAMETER // minimal SLA of green leaves // cm2 g-1 // PARPLT // 1
		this.P_slamax=origin.P_slamax;   // PARAMETER // maximal SLA of green leaves // cm2 g-1 // PARPLT // 1
		this.P_tigefeuil=origin.P_tigefeuil;   // PARAMETER // stem (structural part)/leaf proportion // SD // PARPLT // 1
		this.P_envfruit=origin.P_envfruit;   // PARAMETER // proportion envelop/P_pgrainmaxi in weight  // SD // PARPLT // 1
		this.P_sea=origin.P_sea;   // PARAMETER // specifique surface of fruit envelops // cm2 g-1 // PARPLT // 1
		this.P_nbgrmin=origin.P_nbgrmin;   // PARAMETER // Minimum number of grain // grains m-2  // PARPLT // 1
		this.P_irmax=origin.P_irmax;   // PARAMETER // Maximum harvest index // SD // PARPLT // 1
		this.P_vitircarbT=origin.P_vitircarbT;   // PARAMETER // Heat rate of increase of the carbon harvest index  // g grain g plant-1 degree.day-1 // PARPLT // 1
		this.P_afpf=origin.P_afpf;   // PARAMETER // parameter of the  function logistic defining sink strength of fruits (indeterminate growth) : relative fruit age at which growth is maximal // SD // PARPLT // 1
		this.P_bfpf=origin.P_bfpf;   // PARAMETER // parameter of the logistic curve defining sink strength of fruits (indeterminate growth) :  rate of maximum growth proportionately to maximum weight of fruits // * // PARPLT // 1
		this.P_stdrpnou=origin.P_stdrpnou;   // PARAMETER // Sum of development units between the stages DRP and NOU (end of  setting) // degree.days // PARPLT // 1
		this.P_sensanox=origin.P_sensanox;   // PARAMETER // anoxia sensitivity (0=insensitive) // SD // PARPLT // 1
		this.P_allocfrmax=origin.P_allocfrmax;   // PARAMETER // maximal daily allocation towards fruits // SD // PARPLT // 1
		this.P_tgeljuv10=origin.P_tgeljuv10;   // PARAMETER // temperature corresponding to 10 % of frost damage on the LAI (juvenile stage) // degree C // PARPLT // 1
		this.P_tgeljuv90=origin.P_tgeljuv90;   // PARAMETER // temperature corresponding to 90 % of frost damage on the LAI (juvenile stage) // degree C // PARPLT // 1
		this.P_tgelveg10=origin.P_tgelveg10;   // PARAMETER // temperature corresponding to 10 % of frost damage on the LAI (adult stage) // degree C // PARPLT // 1
		this.P_tgelveg90=origin.P_tgelveg90;   // PARAMETER // temperature corresponding to 90 % of frost damage on the LAI (adult stage) // degree C // PARPLT // 1
		this.P_tgelflo10=origin.P_tgelflo10;   // PARAMETER // temperature corresponding to 10 % of frost damages on the flowers or the fruits // degree C // PARPLT // 1
		this.P_tgelflo90=origin.P_tgelflo90;   // PARAMETER // temperature corresponding to 90 % of frost damages on the flowers or the fruits // degree C // PARPLT // 1
		this.P_codelegume=origin.P_codelegume;   // PARAMETER // 1 when the plant  id a legume crop, or 2 // code 1/2 // PARPLT // 0
		this.P_codcalinflo=origin.P_codcalinflo;   // PARAMETER // option of the way of calculation of the inflorescences number  // code 1/2 // PARPLT // 0
		this.P_codgeljuv=origin.P_codgeljuv;   // PARAMETER // activation of LAI frost at the juvenile stadge // code 1/2 // PARPLT // 0
		this.P_codebeso=origin.P_codebeso;   // PARAMETER // option computing of water needs by the k.ETP (1) approach  or resistive (2) approach // code 1/2 // PARPLT // 0
		this.P_codeintercept=origin.P_codeintercept;   // PARAMETER // option of simulation rainfall interception by leafs: yes (1) or no (2) // code 1/2 // PARPLT // 0
		this.P_codeindetermin=origin.P_codeindetermin;   // PARAMETER // option of  simulation of the leaf growth and fruit growth : indeterminate (2) or determinate (1) // code 1/2 // PARPLT // 0
		this.P_codetremp=origin.P_codetremp;   // PARAMETER // option of heat effect on grain filling: yes (2), no (1) // code 1/2 // PARPLT // 0
		this.P_codetemprac=origin.P_codetemprac;   // PARAMETER // option calculation mode of heat time for the root: with crop temperature (1)  or with soil temperature (2) // code 1/2 // PARPLT // 0
		this.P_coderacine=origin.P_coderacine;   // PARAMETER // Choice of estimation module of growth root in volume: standard profile (1) or by actual density (2) // code 1/2 // PARPLT // 0
		this.P_codgellev=origin.P_codgellev;   // PARAMETER // activation of plantlet frost // code 1/2 // PARPLT // 0
		this.P_codazofruit=origin.P_codazofruit;   // PARAMETER // option of activation of the direct effect of the nitrogen plant status upon the fruit/grain number // code 1/2 // PARPLT // 0
		this.P_codemonocot=origin.P_codemonocot;   // PARAMETER // option plant monocot(1) or dicot(2) // code 1/2 // PARPLT // 0
		this.P_codetemp=origin.P_codetemp;   // PARAMETER // option calculation mode of heat time for the plant : with air temperature (1)  or crop temperature (2) // code 1/2 // PARPLT // 0
		this.P_codegdh=origin.P_codegdh;  // PARAMETER // hourly (1) or daily (2) calculation of development unit // code 1/2 // PARPLT // 0
		this.P_codephot=origin.P_codephot;   // PARAMETER // option of plant photoperiodism: yes (1), no (2) // code1/2 // PARPLT // 0
		this.P_coderetflo=origin.P_coderetflo;   // PARAMETER // option slowness action of water stress before the stage DRP: yes  (1), no (2) // code 1/2 // PARPLT // 0
		this.P_codebfroid=origin.P_codebfroid;   // PARAMETER // option of calculation of chilling requirements // code 1/2 // PARPLT // 0
		this.P_codedormance=origin.P_codedormance;   // PARAMETER // option of calculation of dormancy and chilling requirement // code 1/2 // PARPLT // 0
		this.P_codeperenne=origin.P_codeperenne;   // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0
		this.P_codegermin=origin.P_codegermin;   // PARAMETER // option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2) // code 1/2 // PARPLT // 0
		this.P_codehypo=origin.P_codehypo;   // PARAMETER // option of simulation of a  phase of hypocotyl growth (1) or planting of plantlets (2) // code 1/2 // PARPLT // 0
		this.P_codeir=origin.P_codeir;   // PARAMETER // option of computing the ratio grain weight/total biomass: proportional to time(1), proportional to sum temperatures (2) // code 1/2 // PARPLT // 0
		this.P_codelaitr=origin.P_codelaitr;   // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0
		this.P_codlainet=origin.P_codlainet;   // PARAMETER // option of calculation of the LAI (1 : direct LAInet=origin.; 2 : LAInet = gross LAI - senescent LAI)// code 1/2 // PARPLT // 0
		this.P_codetransrad=origin.P_codetransrad;   // PARAMETER // simulation option of radiation 'interception: law Beer (1), radiation transfers (2) // code 1/2 // PARPLT // 0

		this.P_codgelveg=origin.P_codgelveg;   // PARAMETER // activation of LAI frost at adult stage // code 1/2 // PARPLT // 0
		this.P_codgelflo=origin.P_codgelflo;   // PARAMETER // activation of frost at anthesis // code 1/2 // PARPLT // 0
		this.P_nbjgrain=origin.P_nbjgrain;   // PARAMETER // Period to compute NBGRAIN // days // PARPLT // 1
		this.P_nbfgellev=origin.P_nbfgellev;   // PARAMETER // leaf number at the end of the juvenile phase (frost sensitivity)  // nb pl-1 // PARPLT // 1
		this.P_nboite=origin.P_nboite;   // PARAMETER // "Number of  box  or  age class  of fruits for the fruit growth for the indeterminate crops " // SD // PARPLT // 1
		this.P_idebdorm=origin.P_idebdorm;   // PARAMETER // day of the dormancy entrance // julian day // PARPLT // 1
		this.P_ifindorm=origin.P_ifindorm;   // PARAMETER // dormancy break day // julian day // PARPLT // 1
		this.P_nlevlim1=origin.P_nlevlim1;   // PARAMETER // number of days after germination decreasing the emerged plants if emergence has not occur // days // PARPLT // 1
		this.P_nlevlim2=origin.P_nlevlim2;   // PARAMETER // number of days after germination after which the emerged plants are null // days // PARPLT // 1
		this.P_nbfeuilplant=origin.P_nbfeuilplant;   // PARAMETER // leaf number per plant when planting // nb pl-1 // PARPLT // 1
		this.P_forme =origin.P_forme;  // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0
		this.P_stoprac=origin.P_stoprac;   // PARAMETER // stage when root growth stops (LAX or SEN) // * // PARPLT // 0
		this.P_codeplante=origin.P_codeplante;   // PARAMETER // Name code of the plant in 3 letters // * // PARPLT // 0

		// parametres varietaux (nb_variete_max=30) 
		this.nbVariete=origin.nbVariete;
	    this.P_codazorac=origin.P_codazorac;   // PARAMETER // activation of the nitrogen influence on root partitionning within the soil profile  // code 1/2 // PARPLT // 0
		this.P_codtrophrac=origin.P_codtrophrac;   // PARAMETER // trophic effect on root partitioning within the soil // code 1/2/3 // PARPLT // 0
		this.P_minefnra=origin.P_minefnra;   // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning // SD // PARPLT // 1
		this.P_maxazorac=origin.P_maxazorac;   // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning  // kg N ha-1 mm-1 // PARPLT // 1
		this.P_minazorac=origin.P_minazorac;   // PARAMETER // parameter of the effect of soil nitrogen on root soil partitioning // kg N ha-1 mm-1 // PARPLT // 1
		this.P_repracpermax=origin.P_repracpermax;   // PARAMETER // maximum of root biomass respective to the total biomass (permanent trophic link) // SD // PARPLT // 1
		this.P_repracpermin=origin.P_repracpermin;   // PARAMETER // minimum of root biomass respective to the total biomass (permanent trophic link) // SD // PARPLT // 1
		this.P_krepracperm=origin.P_krepracperm;   // PARAMETER // parameter of biomass root partitioning : evolution of the ratio root/total (permanent trophic link) // SD // PARPLT // 1
		this.P_repracseumax=origin.P_repracseumax;   // PARAMETER // maximum of root biomass respective to the total biomass (trophic link by thresholds) // SD // PARPLT // 1
		this.P_repracseumin=origin.P_repracseumin;   // PARAMETER // minimum of root biomass respective to the total biomass (trophic link by thresholds) // SD // PARPLT // 1
		this.P_krepracseu=origin.P_krepracseu;   // PARAMETER // parameter of biomass root partitioning : evolution of the ratio root/total (trophic link by thresholds) // SD // PARPLT // 1
		this.P_codefixpot=origin.P_codefixpot;   // PARAMETER // option of calculation of the maximal symbiotic fixation // code 1/2/3 // PARPLT // 0
		this.P_fixmaxveg=origin.P_fixmaxveg;   // PARAMETER // parameter to calculate symbiotic fixation as a function of the plant growth   // kg N  (t MS)-1 // PARPLT // 1
		this.P_fixmaxgr=origin.P_fixmaxgr;   // PARAMETER // parameter to calculate symbiotic fixation as a function of the plant growth   // kg N  (t MS)-1 // PARPLT // 1
		this.P_tdmindeb=origin.P_tdmindeb;   // PARAMETER // minimal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
		this.P_tdmaxdeb=origin.P_tdmaxdeb;   // PARAMETER // maximal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
		this.P_codegdhdeb=origin.P_codegdhdeb;   // PARAMETER // option of calculation of the bud break date in hourly or daily growing degrees  // code 1/2 // PARPLT // 0
		this.P_codeplisoleN=origin.P_codeplisoleN;   // PARAMETER // code for N requirement calculations at the beginning of the cycle: dense plant population (1), isolated plants (2, new formalisation) // code 1/2 // PARPLT // 0
		this.P_Nmeta=origin.P_Nmeta;   // PARAMETER // rate of metabolic nitrogen in the plantlet // % // PARPLT // 1
		this.P_masecmeta=origin.P_masecmeta;   // PARAMETER // biomass of the plantlet supposed to be composed of metabolic nitrogen // t ha-1 // PARPLT // 1
		this.P_Nreserve=origin.P_Nreserve;   // PARAMETER // maximal amount of nitrogen in the plant reserves (distance between the maximal dilution curve and the critical dilution curve) (as a percentage of the aboveground dry weight) // % // PARPLT // 1
		this.P_codeINN=origin.P_codeINN;   // PARAMETER // option to compute INN: cumulated (1), instantaneous (2)  // code 1/2 // PARPLT // 0
		this.P_INNimin=origin.P_INNimin;   // PARAMETER // INNI (instantaneous INN) corresponding to P_INNmin // SD // PARPLT // 1
		this.P_potgermi=origin.P_potgermi;   // PARAMETER // humidity threshold from which seed humectation occurs, expressed in soil water potential  // Mpa // PARPLT // 1
		this.P_nbjgerlim=origin.P_nbjgerlim;   // PARAMETER // Threshold number of day after grain imbibition without germination lack // days // PARPLT // 1
		this.P_propjgermin=origin.P_propjgermin;   // PARAMETER // minimal proportion of the duration P_nbjgerlim when the temperature is higher than the temperature threshold P_Tdmax  // % // PARPLT // 1
		this.P_cfpf=origin.P_cfpf;   // PARAMETER // parameter of the first potential growth phase of fruit, corresponding to an exponential type function describing the cell division phase. // SD // PARPLT // 1
		this.P_dfpf=origin.P_dfpf;   // PARAMETER // parameter of the first potential growth phase of fruit, corresponding to an exponential type function describing the cell division phase. // SD // PARPLT // 1
		this.P_tcxstop=origin.P_tcxstop;   // PARAMETER // threshold temperature beyond which the foliar growth stops // degree C // PARPLT // 1
		this.P_vigueurbat=origin.P_vigueurbat;   // PARAMETER // indicator of plant vigor allowing to emerge through the crust  // between 0 and 1 // PARPLT // 1
		this.P_phobasesen=origin.P_phobasesen;   // PARAMETER // photoperiod under which the photoperiodic stress is activated on the leaf lifespan // heures // PARPLT // 1
		this.P_dltamsmaxsen=origin.P_dltamsmaxsen;   // PARAMETER // threshold value of deltams from which there is no more photoperiodic effect on senescence // t ha-1j-1 // PARPLT // 1
		this.P_codestrphot=origin.P_codestrphot;   // PARAMETER // activation of the photoperiodic stress on lifespan : yes (1), no (2) // code 1/2 // PARPLT // 0
		this.P_dltamsminsen=origin.P_dltamsminsen;   // PARAMETER // threshold value of deltams from which the photoperiodic effect on senescence is maximal // t ha-1j-1 // PARPLT // 1
		this.P_alphaphot=origin.P_alphaphot;   // PARAMETER // parameter of photoperiodic effect on leaf lifespan // P_Q10 // PARPLT// 1
		this.P_alphaco2=origin.P_alphaco2;   // PARAMETER // coefficient allowing the modification of radiation use efficiency in case of  atmospheric CO2 increase // SD // PARPLT // 1

		this.bdilI=origin.bdilI;
		this.adilI=origin.adilI;
		this.adilmaxI=origin.adilmaxI;
		this.bdilmaxI=origin.bdilmaxI;
		this.humectation=origin.humectation;
		this.nbjhumec=origin.nbjhumec;
		this.pluiesemis=origin.pluiesemis;
		this.somtemphumec=origin.somtemphumec;
		this.LAIapex=origin.LAIapex;
		this.nmontaison=origin.nmontaison;
		this.onestan2=origin.onestan2;
		
		
		// **************************************
		// paramètres spécifiques d'initialisation de l'état de la plante
		// pour le début de la simulation
		// **************************************

		this.P_stade0=origin.P_stade0;  // PARAMETER // Crop stage used at the beginning of simulation // * // INIT // 0
		this.P_lai0=origin.P_lai0;   // PARAMETER // Initial leaf area index // m2 m-2 // INIT // 1
		this.P_masec0=origin.P_masec0;  // PARAMETER // initial biomass // t ha-1 // INIT // 1
		this.P_magrain0=origin.P_magrain0;  // PARAMETER // initial grain dry weight // g m-2 // INIT // 1
		this.P_QNplante0=origin.P_QNplante0;  // PARAMETER // initial nitrogen amount in the plant // kg ha-1 // INIT // 1
		this.P_resperenne0=origin.P_resperenne0;   // PARAMETER // initial reserve biomass // t ha-1 // INIT // 1
		this.P_zrac0=origin.P_zrac0;   // PARAMETER // initial depth of root front  // cm // INIT // 1

		P_codevar = new int[30];  
		P_adens= new float[30];      
		P_afruitpot= new float[30];     
		P_croirac= new float[30];      
		P_dureefruit= new float[30];    
		P_durvieF= new float[30];       
		P_jvc= new float[30];          
		P_nbgrmax= new float[30];      
		P_pgrainmaxi= new float[30];  
		P_stamflax= new float[30];      
		P_stlevamf= new float[30];     
		P_stlevdrp= new float[30];      
		P_stflodrp= new float[30];      
		P_stlaxsen= new float[30];      
		P_stsenlan= new float[30];     
		P_stdrpmat= new float[30];      
		P_sensiphot= new float[30];     
		P_stdrpdes= new float[30];   		
		P_densinitial= new float[5]; 
		mafeuiltombe0 = new float[3];
		masecneo0 = new float[3];
		msneojaune0 = new float[3];
		dltamsen0 = new float[3];
		mafeuiljaune0 = new float[3];		
		numjtrav = new int[10];
		numjres= new int[10];
		nfauche= new int[10];
		nrecint= new int[20];
		neclair= new int[10];
		LRACH= new float[5];
		lracz= new float[1000];
		QCplantetombe= new float[3];
		QNplantetombe= new float[3];
		udevlaires= new float[10];
		precrac= new float[1000];
		lracsenz= new float[1000];
		drl= new float[367000];
		flrac= new float[1000];
		ulai= new float[367];
		humirac_z= new float[1000];  
		efnrac_z= new float[1000]; 
		irrigprof= new float[1001];
		dtj= new float[367];
		upobs= new float[367];
		racnoy= new float[367];              
		msrac= new float[367];            
		tdevelop= new float[367];          
		cu= new float[367];   
		//
		durvie0= new float[3];
		tempfauche_ancours= new float[21];
		som_vraie_fauche= new float[20];
		ilaxs_prairie= new int[10];
		surf = new float[2]; 
		surfSous= new float[2]; 
		tursla= new float[3];
		allocfruit= new float[3];  
		mafeuiljaune= new float[3];   
		somsenreste= new float[3];
		resperenne= new float[3];   
		deltares= new float[3];
		misenreserve= new float[3];
		innlai= new float[3];    
		lairogne= new float[3];
		biorogne= new float[3];
		sla= new float[3];    
		cumdltares= new float[3];
		innsenes= new float[3];    
		senfac= new float[3];  
		deltatauxcouv= new float[3];
		mafeuiltombe= new float[3];   
		dltamstombe= new float[3];
		mafeuil= new float[3];    
		mafeuilverte= new float[3];   
		matigestruc= new float[3];   
		mareserve= new float[3];
		masecveg= new float[3];   
		maenfruit= new float[3];    
		pfeuiljaune= new float[3];  
		ptigestruc= new float[3];   
		penfruit= new float[3];    
		preserve= new float[3];  
		deshyd= new float[153];
		eaufruit= new float[153]; 
		frplusp= new float[3];
		sucre= new float[3];   
		huile= new float[3];   
		sucrems= new float[3];
		huilems= new float[3];
		biorognecum= new float[3];
		lairognecum= new float[3];
		pgraingel= new float[3];
		laieffcum= new float[3];
		bioeffcum= new float[3];
		rdtint= new float[63];
		nbfrint= new float[60];
		pdsfruittot= new float[3];
		h2orec= new float[3];    
		sucreder= new float[3];
		huileder= new float[3];
		teauint= new float[60];
		fapar= new float[3];    
		mafeuilp= new float[3];
		mabois= new float[3];   
		eai= new float[3];
		spfruit= new float[3];    
		dltaremobil= new float[3];    
		mafrais= new float[3];    
		mafraisfeuille= new float[3];
		mafraisrec= new float[3];
		mafraisres= new float[3];
		mafraistige= new float[3];
		masecvegp= new float[3];
		durvie= new float[1101];
		abso= new float[1101];
		pfeuilverte= new float[1101];
		CNgrain= new float[3];  
		CNplante= new float[3];  
		deltai= new float[1101];
		demande= new float[3];   
		dltags= new float[3];    
		dltamsen = new float[3];  
		dltams = new float[1101];
		dltamsres= new float[3];
		dltaisenat= new float[3];
		dltaisen= new float[3];    
		eop= new float[3];    
		ep= new float[3];    
		epsib= new float[3];
		//
		epz= new float[3000]; 
		fpft= new float[3];    
		hauteur= new float[3];    
		inn0= new float[3];
		inn= new float[3];    
		inns= new float[3];    
		interpluie= new float[3];   
		irazo= new float[1101]; 
		ircarb= new float[1101];
		lai= new float[1101]; 
		laisen= new float[1101]; 
		magrain= new float[1101]; 
		masec= new float[1101]; 
		masecneo= new float[3];    
		masecneov= new float[3];
		masecpartiel= new float[3];
		mouill= new float[3];
		msneojaune= new float[3];    
		msneojaunev= new float[3];
		msres= new float[3];
		msresv= new float[3];
		msresjaune= new float[3];    
		msresjaunev= new float[3];
		msjaune= new float[3];   
		naer= new float[3];
		nbgrains= new float[3];
		nbgrainsv= new float[3];
		nfruit= new float[150];
		nfruitv= new float[150]; 
		nfruitnou= new float[3];
		pdsfruitfrais= new float[3];    
		pgrain= new float[3];
		pdsfruit= new float[150];
		pousfruit= new float[3];   
		QNgrain= new float[3];     
		QNplante= new float[1101]; 
		raint= new float[3];    
		remobilj= new float[3];   
		sourcepuits= new float[3];    
		splai= new float[3];    
		swfac= new float[3]; 
		teaugrain= new float[3];
		turfac= new float[3];    
		vitmoy= new float[3];    
		offrenod= new float[3];   
		fixreel= new float[3];    
		fixpot= new float[3];    
		fixmaxvar= new float[3];
		Qfix= new float[3];    
		deltahauteur= new float[3];
		strphot= new float[3];
		strphotveille= new float[3];
		masecdil= new float[3];
		laimax= new float[3];  
 
		rljour= new float[1000];
		rlveille= new float[1000];
		pfeuil= new float[3]; 
		fpv= new float[3]; 

		photnet = new float[3]; 		
		inni = new float[3];       
		deltabso = new float[3];  
		dltamsN = new float[3];
		dltaremobilN = new float[3];
	
		System.arraycopy(origin.P_codevar 	, 0, this.P_codevar 	, 0, 	30);  
		System.arraycopy(origin.P_adens	, 0, this.P_adens	, 0, 	30);      
		System.arraycopy(origin.P_afruitpot	, 0, this.P_afruitpot	, 0, 	30);     
		System.arraycopy(origin.P_croirac	, 0, this.P_croirac	, 0, 	30);      
		System.arraycopy(origin.P_dureefruit	, 0, this.P_dureefruit	, 0, 	30);    
		System.arraycopy(origin.P_durvieF	, 0, this.P_durvieF	, 0, 	30);       
		System.arraycopy(origin.P_jvc	, 0, this.P_jvc	, 0, 	30);          
		System.arraycopy(origin.P_nbgrmax	, 0, this.P_nbgrmax	, 0, 	30);      
		System.arraycopy(origin.P_pgrainmaxi	, 0, this.P_pgrainmaxi	, 0, 	30);  
		System.arraycopy(origin.P_stamflax	, 0, this.P_stamflax	, 0, 	30);      
		System.arraycopy(origin.P_stlevamf	, 0, this.P_stlevamf	, 0, 	30);     
		System.arraycopy(origin.P_stlevdrp	, 0, this.P_stlevdrp	, 0, 	30);      
		System.arraycopy(origin.P_stflodrp	, 0, this.P_stflodrp	, 0, 	30);      
		System.arraycopy(origin.P_stlaxsen	, 0, this.P_stlaxsen	, 0, 	30);      
		System.arraycopy(origin.P_stsenlan	, 0, this.P_stsenlan	, 0, 	30);     
		System.arraycopy(origin.P_stdrpmat	, 0, this.P_stdrpmat	, 0, 	30);      
		System.arraycopy(origin.P_sensiphot	, 0, this.P_sensiphot	, 0, 	30);     
		System.arraycopy(origin.P_stdrpdes	, 0, this.P_stdrpdes	, 0, 	30);   
		System.arraycopy(origin.P_densinitial	, 0, this.P_densinitial	, 0, 	5); 
		System.arraycopy(origin.mafeuiltombe0 	, 0, this.mafeuiltombe0 	, 0, 	3);
		System.arraycopy(origin.masecneo0 	, 0, this.masecneo0 	, 0, 	3);
		System.arraycopy(origin.msneojaune0 	, 0, this.msneojaune0 	, 0, 	3);
		System.arraycopy(origin.dltamsen0 	, 0, this.dltamsen0 	, 0, 	3);
		System.arraycopy(origin.mafeuiljaune0 	, 0, this.mafeuiljaune0 	, 0, 	3);
		System.arraycopy(origin.numjtrav 	, 0, this.numjtrav 	, 0, 	10);
		System.arraycopy(origin.numjres	, 0, this.numjres	, 0, 	10);
		System.arraycopy(origin.nfauche	, 0, this.nfauche	, 0, 	10);
		System.arraycopy(origin.nrecint	, 0, this.nrecint	, 0, 	20);
		System.arraycopy(origin.neclair	, 0, this.neclair	, 0, 	10);
		System.arraycopy(origin.LRACH	, 0, this.LRACH	, 0, 	5);
		System.arraycopy(origin.lracz	, 0, this.lracz	, 0, 	1000);
		System.arraycopy(origin.QCplantetombe	, 0, this.QCplantetombe	, 0, 	3);
		System.arraycopy(origin.QNplantetombe	, 0, this.QNplantetombe	, 0, 	3);
		System.arraycopy(origin.udevlaires	, 0, this.udevlaires	, 0, 	10);
		System.arraycopy(origin.precrac	, 0, this.precrac	, 0, 	1000);
		System.arraycopy(origin.lracsenz	, 0, this.lracsenz	, 0, 	1000);
		System.arraycopy(origin.drl	, 0, this.drl	, 0, 	367000);
		System.arraycopy(origin.flrac	, 0, this.flrac	, 0, 	1000);
		System.arraycopy(origin.ulai	, 0, this.ulai	, 0, 	367);
		System.arraycopy(origin.humirac_z	, 0, this.humirac_z	, 0, 	1000);  
		System.arraycopy(origin.efnrac_z	, 0, this.efnrac_z	, 0, 	1000); 
		System.arraycopy(origin.irrigprof	, 0, this.irrigprof	, 0, 	1001);
		System.arraycopy(origin.dtj	, 0, this.dtj	, 0, 	367);
		System.arraycopy(origin.upobs	, 0, this.upobs	, 0, 	367);
		System.arraycopy(origin.racnoy	, 0, this.racnoy	, 0, 	367);              
		System.arraycopy(origin.msrac	, 0, this.msrac	, 0, 	367);            
		System.arraycopy(origin.tdevelop	, 0, this.tdevelop	, 0, 	367);          
		System.arraycopy(origin.cu	, 0, this.cu	, 0, 	367);                 
		System.arraycopy(origin.durvie0	, 0, this.durvie0	, 0, 	3);
		System.arraycopy(origin.tempfauche_ancours	, 0, this.tempfauche_ancours	, 0, 	21);
		System.arraycopy(origin.som_vraie_fauche	, 0, this.som_vraie_fauche	, 0, 	20);
		System.arraycopy(origin.ilaxs_prairie	, 0, this.ilaxs_prairie	, 0, 	10);
		System.arraycopy(origin.surf 	, 0, this.surf 	, 0, 	2); 
		System.arraycopy(origin.surfSous	, 0, this.surfSous	, 0, 	2); 
		System.arraycopy(origin.tursla	, 0, this.tursla	, 0, 	3);
		System.arraycopy(origin.allocfruit	, 0, this.allocfruit	, 0, 	3);  
		System.arraycopy(origin.mafeuiljaune	, 0, this.mafeuiljaune	, 0, 	3);   
		System.arraycopy(origin.somsenreste	, 0, this.somsenreste	, 0, 	3);
		System.arraycopy(origin.resperenne	, 0, this.resperenne	, 0, 	3);   
		System.arraycopy(origin.deltares	, 0, this.deltares	, 0, 	3);
		System.arraycopy(origin.misenreserve	, 0, this.misenreserve	, 0, 	3);
		System.arraycopy(origin.innlai	, 0, this.innlai	, 0, 	3);    
		System.arraycopy(origin.lairogne	, 0, this.lairogne	, 0, 	3);
		System.arraycopy(origin.biorogne	, 0, this.biorogne	, 0, 	3);
		System.arraycopy(origin.sla	, 0, this.sla	, 0, 	3);    
		System.arraycopy(origin.cumdltares	, 0, this.cumdltares	, 0, 	3);
		System.arraycopy(origin.innsenes	, 0, this.innsenes	, 0, 	3);    
		System.arraycopy(origin.senfac	, 0, this.senfac	, 0, 	3);  
		System.arraycopy(origin.deltatauxcouv	, 0, this.deltatauxcouv	, 0, 	3);
		System.arraycopy(origin.mafeuiltombe	, 0, this.mafeuiltombe	, 0, 	3);   
		System.arraycopy(origin.dltamstombe	, 0, this.dltamstombe	, 0, 	3);
		System.arraycopy(origin.mafeuil	, 0, this.mafeuil	, 0, 	3);    
		System.arraycopy(origin.mafeuilverte	, 0, this.mafeuilverte	, 0, 	3);   
		System.arraycopy(origin.matigestruc	, 0, this.matigestruc	, 0, 	3);   
		System.arraycopy(origin.mareserve	, 0, this.mareserve	, 0, 	3);
		System.arraycopy(origin.masecveg	, 0, this.masecveg	, 0, 	3);   
		System.arraycopy(origin.maenfruit	, 0, this.maenfruit	, 0, 	3);    
		System.arraycopy(origin.pfeuiljaune	, 0, this.pfeuiljaune	, 0, 	3);  
		System.arraycopy(origin.ptigestruc	, 0, this.ptigestruc	, 0, 	3);   
		System.arraycopy(origin.penfruit	, 0, this.penfruit	, 0, 	3);    
		System.arraycopy(origin.preserve	, 0, this.preserve	, 0, 	3);  
		System.arraycopy(origin.deshyd	, 0, this.deshyd	, 0, 	153);
		System.arraycopy(origin.eaufruit	, 0, this.eaufruit	, 0, 	153); 
		System.arraycopy(origin.frplusp	, 0, this.frplusp	, 0, 	3);
		System.arraycopy(origin.sucre	, 0, this.sucre	, 0, 	3);   
		System.arraycopy(origin.huile	, 0, this.huile	, 0, 	3);   
		System.arraycopy(origin.sucrems	, 0, this.sucrems	, 0, 	3);
		System.arraycopy(origin.huilems	, 0, this.huilems	, 0, 	3);
		System.arraycopy(origin.biorognecum	, 0, this.biorognecum	, 0, 	3);
		System.arraycopy(origin.lairognecum	, 0, this.lairognecum	, 0, 	3);
		System.arraycopy(origin.pgraingel	, 0, this.pgraingel	, 0, 	3);
		System.arraycopy(origin.laieffcum	, 0, this.laieffcum	, 0, 	3);
		System.arraycopy(origin.bioeffcum	, 0, this.bioeffcum	, 0, 	3);
		System.arraycopy(origin.rdtint	, 0, this.rdtint	, 0, 	63);
		System.arraycopy(origin.nbfrint	, 0, this.nbfrint	, 0, 	60);
		System.arraycopy(origin.pdsfruittot	, 0, this.pdsfruittot	, 0, 	3);
		System.arraycopy(origin.h2orec	, 0, this.h2orec	, 0, 	3);    
		System.arraycopy(origin.sucreder	, 0, this.sucreder	, 0, 	3);
		System.arraycopy(origin.huileder	, 0, this.huileder	, 0, 	3);
		System.arraycopy(origin.teauint	, 0, this.teauint	, 0, 	60);
		System.arraycopy(origin.fapar	, 0, this.fapar	, 0, 	3);    
		System.arraycopy(origin.mafeuilp	, 0, this.mafeuilp	, 0, 	3);
		System.arraycopy(origin.mabois	, 0, this.mabois	, 0, 	3);   
		System.arraycopy(origin.eai	, 0, this.eai	, 0, 	3);
		System.arraycopy(origin.spfruit	, 0, this.spfruit	, 0, 	3);    
		System.arraycopy(origin.dltaremobil	, 0, this.dltaremobil	, 0, 	3);    
		System.arraycopy(origin.mafrais	, 0, this.mafrais	, 0, 	3);    
		System.arraycopy(origin.mafraisfeuille	, 0, this.mafraisfeuille	, 0, 	3);
		System.arraycopy(origin.mafraisrec	, 0, this.mafraisrec	, 0, 	3);
		System.arraycopy(origin.mafraisres	, 0, this.mafraisres	, 0, 	3);
		System.arraycopy(origin.mafraistige	, 0, this.mafraistige	, 0, 	3);
		System.arraycopy(origin.masecvegp	, 0, this.masecvegp	, 0, 	3);
		System.arraycopy(origin.durvie	, 0, this.durvie	, 0, 	1101);
		System.arraycopy(origin.abso	, 0, this.abso	, 0, 	1101);
		System.arraycopy(origin.pfeuilverte	, 0, this.pfeuilverte	, 0, 	1101);
		System.arraycopy(origin.CNgrain	, 0, this.CNgrain	, 0, 	3);  
		System.arraycopy(origin.CNplante	, 0, this.CNplante	, 0, 	3);  
		System.arraycopy(origin.deltai	, 0, this.deltai	, 0, 	1101);
		System.arraycopy(origin.demande	, 0, this.demande	, 0, 	3);   
		System.arraycopy(origin.dltags	, 0, this.dltags	, 0, 	3);    
		System.arraycopy(origin.dltamsen 	, 0, this.dltamsen 	, 0, 	3);  
		System.arraycopy(origin.dltams 	, 0, this.dltams 	, 0, 	1101);
		System.arraycopy(origin.dltamsres	, 0, this.dltamsres	, 0, 	3);
		System.arraycopy(origin.dltaisenat	, 0, this.dltaisenat	, 0, 	3);
		System.arraycopy(origin.dltaisen	, 0, this.dltaisen	, 0, 	3);    
		System.arraycopy(origin.eop	, 0, this.eop	, 0, 	3);    
		System.arraycopy(origin.ep	, 0, this.ep	, 0, 	3);    
		System.arraycopy(origin.epsib	, 0, this.epsib	, 0, 	3); 
		System.arraycopy(origin.epz	, 0, this.epz	, 0, 	3000); 
		System.arraycopy(origin.fpft	, 0, this.fpft	, 0, 	3);    
		System.arraycopy(origin.hauteur	, 0, this.hauteur	, 0, 	3);    
		System.arraycopy(origin.inn0	, 0, this.inn0	, 0, 	3);
		System.arraycopy(origin.inn	, 0, this.inn	, 0, 	3);    
		System.arraycopy(origin.inns	, 0, this.inns	, 0, 	3);    
		System.arraycopy(origin.interpluie	, 0, this.interpluie	, 0, 	3);   
		System.arraycopy(origin.irazo	, 0, this.irazo	, 0, 	1101); 
		System.arraycopy(origin.ircarb	, 0, this.ircarb	, 0, 	1101); 
		System.arraycopy(origin.lai	, 0, this.lai	, 0, 	1101); 
		System.arraycopy(origin.laisen	, 0, this.laisen	, 0, 	1101); 
		System.arraycopy(origin.magrain	, 0, this.magrain	, 0, 	1101); 
		System.arraycopy(origin.masec	, 0, this.masec	, 0, 	1101); 
		System.arraycopy(origin.masecneo	, 0, this.masecneo	, 0, 	3);    
		System.arraycopy(origin.masecneov	, 0, this.masecneov	, 0, 	3);
		System.arraycopy(origin.masecpartiel	, 0, this.masecpartiel	, 0, 	3);
		System.arraycopy(origin.mouill	, 0, this.mouill	, 0, 	3);
		System.arraycopy(origin.msneojaune	, 0, this.msneojaune	, 0, 	3);    
		System.arraycopy(origin.msneojaunev	, 0, this.msneojaunev	, 0, 	3);
		System.arraycopy(origin.msres	, 0, this.msres	, 0, 	3);
		System.arraycopy(origin.msresv	, 0, this.msresv	, 0, 	3);
		System.arraycopy(origin.msresjaune	, 0, this.msresjaune	, 0, 	3);    
		System.arraycopy(origin.msresjaunev	, 0, this.msresjaunev	, 0, 	3);
		System.arraycopy(origin.msjaune	, 0, this.msjaune	, 0, 	3);   
		System.arraycopy(origin.naer	, 0, this.naer	, 0, 	3);
		System.arraycopy(origin.nbgrains	, 0, this.nbgrains	, 0, 	3);
		System.arraycopy(origin.nbgrainsv	, 0, this.nbgrainsv	, 0, 	3);
		System.arraycopy(origin.nfruit	, 0, this.nfruit	, 0, 	150);
		System.arraycopy(origin.nfruitv	, 0, this.nfruitv	, 0, 	150); 
		System.arraycopy(origin.nfruitnou	, 0, this.nfruitnou	, 0, 	3);
		System.arraycopy(origin.pdsfruitfrais	, 0, this.pdsfruitfrais	, 0, 	3);    
		System.arraycopy(origin.pgrain	, 0, this.pgrain	, 0, 	3);
		System.arraycopy(origin.pdsfruit	, 0, this.pdsfruit	, 0, 	150);
		System.arraycopy(origin.pousfruit	, 0, this.pousfruit	, 0, 	3);   
		System.arraycopy(origin.QNgrain	, 0, this.QNgrain	, 0, 	3);     
		System.arraycopy(origin.QNplante	, 0, this.QNplante	, 0, 	1101); 
		System.arraycopy(origin.raint	, 0, this.raint	, 0, 	3);    
		System.arraycopy(origin.remobilj	, 0, this.remobilj	, 0, 	3);   
		System.arraycopy(origin.sourcepuits	, 0, this.sourcepuits	, 0, 	3);    
		System.arraycopy(origin.splai	, 0, this.splai	, 0, 	3);    
		System.arraycopy(origin.swfac	, 0, this.swfac	, 0, 	3); 
		System.arraycopy(origin.teaugrain	, 0, this.teaugrain	, 0, 	3);
		System.arraycopy(origin.turfac	, 0, this.turfac	, 0, 	3);    
		System.arraycopy(origin.vitmoy	, 0, this.vitmoy	, 0, 	3);    
		System.arraycopy(origin.offrenod	, 0, this.offrenod	, 0, 	3);   
		System.arraycopy(origin.fixreel	, 0, this.fixreel	, 0, 	3);    
		System.arraycopy(origin.fixpot	, 0, this.fixpot	, 0, 	3);    
		System.arraycopy(origin.fixmaxvar	, 0, this.fixmaxvar	, 0, 	3);
		System.arraycopy(origin.Qfix	, 0, this.Qfix	, 0, 	3);    
		System.arraycopy(origin.deltahauteur	, 0, this.deltahauteur	, 0, 	3);
		System.arraycopy(origin.strphot	, 0, this.strphot	, 0, 	3);
		System.arraycopy(origin.strphotveille	, 0, this.strphotveille	, 0, 	3);
		System.arraycopy(origin.masecdil	, 0, this.masecdil	, 0, 	3);
		System.arraycopy(origin.laimax	, 0, this.laimax	, 0, 	3);  

		System.arraycopy(origin.rljour	, 0, this.rljour	, 0, 	1000);
		System.arraycopy(origin.rlveille	, 0, this.rlveille	, 0, 	1000);
		System.arraycopy(origin.pfeuil	, 0, this.pfeuil	, 0, 	3); 
		System.arraycopy(origin.fpv	, 0, this.fpv	, 0, 	3); 
		System.arraycopy(origin.photnet 	, 0, this.photnet 	, 0, 	3); 
		System.arraycopy(origin.inni 	, 0, this.inni 	, 0, 	3); 
		System.arraycopy(origin.deltabso 	, 0, this.deltabso 	, 0, 	3);
		System.arraycopy(origin.dltamsN 	, 0, this.dltamsN 	, 0, 	3);
		System.arraycopy(origin.dltaremobilN 	, 0, this.dltaremobilN 	, 0, 	3);

	}
	

	@Override
	protected List getFieldOrder() {
		return Arrays.asList(new String[] { "estDominante", "ipl", "P_adil", "P_bdens", "P_bdil", "P_belong",
				"P_celong", "P_cgrain", "P_cgrainv0", "P_coefmshaut", "P_contrdamax", "P_debsenrac", "P_deshydbase",
				"P_dlaimax", "P_draclong", "P_efcroijuv", "P_efcroirepro", "P_efcroiveg", "P_elmax", "P_extin",
				"P_fixmax", "P_h2ofeuiljaune", "P_h2ofeuilverte", "P_h2ofrvert", "P_h2oreserve", "P_h2otigestruc",
				"P_inflomax", "P_Kmabs1", "P_Kmabs2", "P_kmax", "P_kstemflow", "P_longsperac", "P_lvfront",
				"P_mouillabil", "P_nbinflo", "P_pentinflores", "P_phosat", "P_psisto", "P_psiturg", "P_rsmin",
				"P_sensrsec", "P_spfrmax", "P_spfrmin", "P_splaimax", "P_splaimin", "P_stemflowmax", "P_stpltger",
				"P_tcmin", "P_tcmax", "P_tdebgel", "P_tdmin", "P_tdmax", "P_tempdeshyd", "P_tgellev10", "P_tgellev90",
				"P_tgmin", "P_tletale", "P_tmaxremp", "P_tminremp", "P_vitircarb", "P_vitirazo", "P_vitpropsucre",
				"P_vitprophuile", "P_Vmax1", "P_Vmax2", "P_zlabour", "P_zprlim", "P_zpente", "P_adilmax", "P_bdilmax",
				"P_masecNmax", "P_INNmin", "P_inngrain1", "P_inngrain2", "P_stlevdno", "P_stdnofno", "P_stfnofvino",
				"P_vitno", "P_profnod", "P_concNnodseuil", "P_concNrac0", "P_concNrac100", "P_tempnod1", "P_tempnod2",
				"P_tempnod3", "P_tempnod4", "P_coeflevamf", "P_coefamflax", "P_coeflaxsen", "P_coefsenlan",
				"P_coefdrpmat", "P_coefflodrp", "P_coeflevdrp", "P_ratiodurvieI", "P_ratiosen", "P_ampfroid",
				"P_laicomp", "P_phobase", "P_stressdev", "P_jvcmini", "P_julvernal", "P_tfroid", "P_q10",
				"P_stdordebour", "P_phyllotherme", "P_laiplantule", "P_masecplantule", "P_zracplantule", "P_hautbase",
				"P_hautmax", "P_vlaimax", "P_pentlaimax", "P_udlaimax", "P_abscission", "P_parazofmorte",
				"P_innturgmin", "P_dlaimin", "P_tustressmin", "P_durviesupmax", "P_innsen", "P_rapsenturg",
				"P_dlaimaxbrut", "P_tauxrecouvmax", "P_tauxrecouvkmax", "P_pentrecouv", "P_infrecouv", "P_rapforme",
				"P_ktrou", "P_adfol", "P_dfolbas", "P_dfolhaut", "P_remobres", "P_temin", "P_teopt", "P_temax",
				"P_teoptbis", "P_slamin", "P_slamax", "P_tigefeuil", "P_envfruit", "P_sea", "P_nbgrmin", "P_irmax",
				"P_vitircarbT", "P_afpf", "P_bfpf", "P_stdrpnou", "P_sensanox", "P_allocfrmax", "P_tgeljuv10",
				"P_tgeljuv90", "P_tgelveg10", "P_tgelveg90", "P_tgelflo10", "P_tgelflo90", "P_codelegume",
				"P_codcalinflo", "P_codgeljuv", "P_codebeso", "P_codeintercept", "P_codeindetermin", "P_codetremp",
				"P_codetemprac", "P_coderacine", "P_codgellev", "P_codazofruit", "P_codemonocot", "P_codetemp",
				"P_codegdh", "P_codephot", "P_coderetflo", "P_codebfroid", "P_codedormance", "P_codeperenne",
				"P_codegermin", "P_codehypo", "P_codeir", "P_codelaitr", "P_codlainet", "P_codetransrad", "codetransrad", "P_codgelveg",
				"P_codgelflo", "P_nbjgrain", "P_nbfgellev", "P_nboite", "P_idebdorm", "P_ifindorm", "P_nlevlim1",
				"P_nlevlim2", "P_nbfeuilplant", "P_forme", "P_stoprac", "P_codeplante", "nbVariete", "P_codevar",
				"P_adens", "P_afruitpot", "P_croirac", "P_dureefruit", "P_durvieF", "P_jvc", "P_nbgrmax",
				"P_pgrainmaxi", "P_stamflax", "P_stlevamf", "P_stlevdrp", "P_stflodrp", "P_stlaxsen", "P_stsenlan",
				"P_stdrpmat", "P_sensiphot", "P_stdrpdes", "P_codazorac", "P_codtrophrac", "P_minefnra", "P_maxazorac",
				"P_minazorac", "P_repracpermax", "P_repracpermin", "P_krepracperm", "P_repracseumax", "P_repracseumin",
				"P_krepracseu", "P_codefixpot", "P_fixmaxveg", "P_fixmaxgr", "P_tdmindeb", "P_tdmaxdeb", "P_codegdhdeb",
				"P_codeplisoleN", "P_Nmeta", "P_masecmeta", "P_Nreserve", "P_codeINN", "P_INNimin", "P_potgermi",
				"P_nbjgerlim", "P_propjgermin", "P_cfpf", "P_dfpf", "P_tcxstop", "P_vigueurbat", "P_phobasesen",
				"P_dltamsmaxsen", "P_codestrphot", "P_dltamsminsen", "P_alphaphot", "P_alphaco2", "P_stade0", "P_lai0",
				"P_masec0", "P_magrain0", "P_QNplante0", "P_densinitial", "P_resperenne0", "P_zrac0", "mafeuiltombe0",
				"masecneo0", "msneojaune0", "dltamsen0", "mafeuiljaune0", "ficrap", "ficrap_AgMIP", "ficsort",
				"ficsort2", "ficbil", "group", "parapluie", "codeinstal", "nsencour", "nsencourpre", "nsencourprerac",
				"numjtrav", "numjres", "nnou", "nbj0remp", "nbjgel", "nfauche", "nlanobs", "nlax", "nrecobs", "nlaxobs",
				"nlev", "nlevobs", "nmatobs", "nplt", "ndrp", "ndebsenrac", "nbrecolte", "nrecint", "ndebdes",
				"ntaille", "nger", "igers", "inous", "idebdess", "ilans", "iplts", "compretarddrp", "iamfs",
				"idebdorms", "idrps", "ifindorms", "iflos", "ilaxs", "namf", "imats", "irecs", "isens", "nsen",
				"nsenobs", "ndebdorm", "nfindorm", "nbfeuille", "nflo", "nfloobs", "nstopfeuille", "mortplante",
				"mortplanteN", "neclair", "nrogne", "neffeuil", "jdepuisrec", "namfobs", "nlan", "nrec", "nrecbutoir",
				"nmat", "ndrpobs", "ndebdesobs", "ilevs", "numcoupe", "nst1coupe", "nst2coupe", "ndebsen",
				"nbjTmoyIpltJuin", "nbjTmoyIpltSept", "ndno", "nfno", "nfvino", "fixpotC", "fixreelC", "offrenodC",
				"fixmaxC", "QfixC", "fixpotfno", "propfixpot", "nrecalpfmax", "fauchediff", "sioncoupe",
				"onarretesomcourdrp", "etatvernal", "gelee", "LRACH", "cinterpluie", "totpl", "rc", "fco2s", "lracz",
				"cumlracz", "dfol", "rombre", "rsoleil", "somcourfauche", "reste_apres_derniere_coupe", "QNplanteres",
				"QCplantetombe", "QNplantetombe", "QCrogne", "QNrogne", "Crac", "Nrac", "QCrac", "QNrac", "udevlaires",
				"cescoupe", "cetcoupe", "cepcoupe", "cetmcoupe", "cprecipcoupe", "cep", "cprecip", "cet", "ces", "cetm",
				"masectot", "rendementsec", "str1coupe", "stu1coupe", "str2coupe", "stu2coupe", "inn1coupe",
				"diftemp1coupe", "inn2coupe", "diftemp2coupe", "QNressuite", "QCressuite", "pfmax", "stemflow",
				"precrac", "lracsenz", "drl", "somtemprac", "poussracmoy", "Emd", "diftemp1", "diftemp2", "flrac",
				"ftemp", "udevcult", "udevair", "ebmax", "fpari", "efdensite_rac", "efdensite",
				"ulai", "caljvc", "somelong", "somger", "cdemande", "zrac", "znonli", "deltaz", "difrac", "resrac",
				"Scroira", "efda", "efnrac_mean", "humirac_mean", "humirac_z", "efnrac_z", "rlj", "dltmsrac_plante",
				"cumraint", "cumrg", "irrigprof", "stlevdrp0", "stsenlan0", "stlaxsen0", "remobil", "somcourdrp", "dtj",
				"qressuite", "qressuite_tot", "CsurNressuite_tot", "Nexporte", "Nrecycle", "MSexporte", "MSrecycle",
				"p1000grain", "somudevair", "somudevcult", "somupvtsem", "CsurNressuite", "cumlraczmaxi", "cumdevfr",
				"nbfruit", "tetstomate", "teturg", "rltot", "rfpi", "lracsentot", "largeur", "utno", "pfv", "pfj",
				"pftot", "pfe", "pft", "wcf", "wct", "wce", "stpltlev", "stpltger", "stdrpsen", "stmatrec", "upvtutil",
				"somcour", "reajust", "upobs", "stlevamf0", "stamflax0", "varrapforme", "anoxmoy", "chargefruit", "coeflev",
				"cumflrac", "densitelev", "durvieI", "exobiom", "etr_etm1", "etm_etr1", "etr_etm2", "etm_etr2",
				"exofac", "exofac1", "exofac2", "exolai", "fco2", "fgelflo", "fgellev", "fstressgel", "ftempremp",
				"gel1", "gel2", "gel3", "izrac", "idzrac", "nbfrote", "rmaxi", "somcourutp", "somcourno", "somfeuille",
				"somtemp", "somupvt", "somupvtI", "stdrpmatini", "stflodrp0", "stlevflo", "TmoyIpltJuin",
				"TmoyIpltSept", "zracmax", "rfvi", "racnoy", "msrac", "tdevelop", "cu", "nst1", "nst2", "inn1", "inn2",
				"str1", "str2", "stu1", "stu2", "etm_etr1moy", "etr_etm1moy", "etr_etm2moy", "etm_etr2moy",
				"exofac1moy", "exofac2moy", "inn1moy", "inn2moy", "swfac1moy", "swfac2moy", "turfac1moy", "turfac2moy",
				"mafrais_nrec", "pdsfruitfrais_nrec", "mabois_nrec", "H2Orec_nrec", "chargefruit_nrec", "CNgrain_nrec",
				"CNplante_nrec", "QNgrain_nrec", "Qngrain_ntailleveille", "nbgraingel", "QNplanteCtot",
				"QNplantefauche", "ctmoy", "ctcult", "ctcultmax", "cetp", "crg", "cum_et0", "nbrepoussefauche",
				"anitcoupe_anterieure", "hautcoupe_anterieure", "msresiduel_anterieure", "lairesiduel_anterieure",
				"ulai0", "durvie0", "codebbch0", "restit_anterieure", "mscoupemini_anterieure", "tempfauche_realise",
				"tempeff", "tempfauche_ancours", "som_vraie_fauche", "densite", "densiteequiv", "stdrpdes0",
				"str1intercoupe", "stu1intercoupe", "inn1intercoupe", "diftemp1intercoupe", "str2intercoupe",
				"stu2intercoupe", "inn2intercoupe", "diftemp2intercoupe", "ficsort3", "ficdrat", "ilaxs_prairie",
				"mortmasec", "densitemax", "drlsenmortalle", "imontaisons", "mortalle", "densiteger", "mafruit",
				"matuber", "matuber_rec", "msrec_fou", "dNdWcrit", "dNdWmax", "flurac", "flusol", "profextN",
				"profexteau", "age_prairie", "RsurRUrac", "RUrac", "somcourmont", "psibase", "nbjpourdecisemis",
				"nbjpourdecirecolte", "mortreserve", "codeperenne0", "masec_kg_ha", "mafruit_kg_ha", "mafeuil_kg_ha",
				"matigestruc_kg_ha", "gel1_percent", "gel2_percent", "gel3_percent", "nbinflo_recal", "codebbch_output",
				"ebmax_gr", "fpari_gr", "surf", "surfSous", "tursla", "allocfruit", "mafeuiljaune", "somsenreste",
				"resperenne", "deltares", "misenreserve", "innlai", "lairogne", "biorogne", "sla", "cumdltares",
				"innsenes", "senfac", "deltatauxcouv", "mafeuiltombe", "dltamstombe", "mafeuil", "mafeuilverte",
				"matigestruc", "mareserve", "masecveg", "maenfruit", "pfeuiljaune", "ptigestruc", "penfruit",
				"preserve", "deshyd", "eaufruit", "frplusp", "sucre", "huile", "sucrems", "huilems", "biorognecum",
				"lairognecum", "pgraingel", "laieffcum", "bioeffcum", "rdtint", "nbfrint", "pdsfruittot", "h2orec",
				"sucreder", "huileder", "teauint", "fapar", "mafeuilp", "mabois", "eai", "spfruit", "dltaremobil",
				"mafrais", "mafraisfeuille", "mafraisrec", "mafraisres", "mafraistige", "masecvegp", "durvie", "abso",
				"pfeuilverte", "CNgrain", "CNplante", "deltai", "demande", "dltags", "dltamsen", "dltams", "dltamsres",
				"dltaisenat", "dltaisen", "eop", "ep", "epsib", "epz", "fpft", "hauteur", "inn0", "inn", "inns",
				"interpluie", "irazo", "ircarb","lai", "laisen", "magrain", "masec", "masecneo", "masecneov", "masecpartiel",
				"mouill", "msneojaune", "msneojaunev", "msres", "msresv", "msresjaune", "msresjaunev", "msjaune",
				"naer", "nbgrains", "nbgrainsv", "nfruit", "nfruitv", "nfruitnou", "pdsfruitfrais", "pgrain",
				"pdsfruit", "pousfruit", "QNgrain", "QNplante", "QCplante", "raint", "remobilj", "sourcepuits", "splai",
				"swfac", "teaugrain", "turfac", "vitmoy", "offrenod", "fixreel", "fixpot", "fixmaxvar", "Qfix",
				"deltahauteur", "strphot", "strphotveille", "masecdil", "laimax", "H2Orec_percent", "sucre_percent",
				"huile_percent", "et0", "QNexport", "day_cut",  "rlveille", "rljour", "upvt", "upt", "pfeuil",
				"fpv", "photnet",
				"bdilI",  "adilI","adilmaxI",  "bdilmaxI", "inni","deltabso", "dltamsN", "humectation", "nbjhumec", "pluiesemis",
				"dltaremobilN", "somtemphumec", "LAIapex", "nmontaison", "onestan2"
		});
	}


}





