package safe.stics;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import com.sun.jna.Structure;


/**
 * SafeSticsCommun - Mirror object of STICS FORTRAN Stics_Communs_ 
 * 
 * 
 * @author Isabelle Lecomte - December 2016
 */

public class SafeSticsCommun extends Structure implements Serializable {


	
	 public boolean flag_record;
	   public int ipl;  
	   public int P_nbplantes;     // PARAMETER // number of simulated plants // SD // P_USM/USMXML // 0
	   public int jour;            //  variable pour le stockage du jour courant de la simulation (01 à 31)
	   public int nummois;         //  variable pour le stockage du numéro de mois courant (01 à 12)

	   public int P_culturean;  	// PARAMETER // crop status 1 = over 1 calendar year ,other than 1  = on two calendar years (winter crop in northern hemisphere) // code 0/1 // P_USM/USMXML // 0
	   public int P_iwater;  	// PARAMETER // julian day of the beginning of the simulation // jour julien // P_USM // 1
	   public int P_ifwater;  	// PARAMETER // julian day of the end of simulation // julian day // P_USM // 1
	   public int ifwater_courant;
	   public int ifwater0;  
	   public int dernier_n;  
	   public int n; 
	   public int nbjmax;        // taille des tableaux temporels (366 pour l'instant)  
	   public int jjul;  
	   public int jul; 
	   public int nbans;  
	   public int nbjanrec;  
	   public int nstoc;  
	   public int numcult;  
	   public int ens; // *** le code d'ensoleillement AO ou AS ***  
	   public int fichist;  
	   public int ficsta;  
	   public int ficdbg; // ** PB - indice du fichier de debugging - 16/01/2004  
	   public int ficdbg2; // ** DR - indice du fichier de debugging number 2
	   
	   public int codoptim;  
	   public int P_codesuite;  	// PARAMETER // code for successive P_USM ( 1=yes, 0=no) // SD // P_USM/USMXML // 0
	   public int nbjsemis;  
	   public int maxwth;  
	   public int nstoprac;  
	   public int numdate;  
	   public int bouchon;      	// OUTPUT // Index showing if the shrinkage slots are opened (0) or closed (1)  // 0-1
	   public int nouvdensrac;  
	   public int nbCouchesSol;  
	   public int nappmulch;  
	   public int ires;  
	   public int itrav;  
	   public int ansemis;  
	   public int anrecol;
	   public int annee[];  
	   public int NH;
	   public int codeprofil;  
	   public int nbjrecol;  
	   public int nbjrecol0;  
	   public int NHE;  
	   public int napini[];   // DR 10/06/2013 indexe sur les plantes
	   public int napNini[];     // DR 10/06/2013 indexe sur les plantes
	   public int nbjresini[];  // DR 17/06/2016
	   public int faucheannule;  
 //////MODIF HISAFE déplacement dans Sol.f90
 //////  public float Ninitf(5)
 //////  public float P_Hinitf(5)     // PARAMETER // Table of initial gravimetric water content of each soil layer (/fine earth) // % w // INIT // 1
	   public float delta;  
	   public float devjourfr;  
	   public float esz[];
	   public float fdens;  
	   public float tustress;      	// OUTPUT // Stress index active on leaf growth (= minimum(turfac,innlai))  // 0-1
	   public float rdif;      		// OUTPUT // Ratio between diffuse radiation and global radiation  // 0-1
	   public float originehaut;  
	   public float tairveille;      // OUTPUT // Mean air temperature the previous day // degree C
	   public float coefbmsres;  
	   public float coefaifres;  
	   public float coefbifres;  
	   public float coefcifres;  
	   public float a;  
	   public float effN; // TODO: variable locale. Pas d'interêt dans la version actuelle de la stocker  
	   public float hi;  
	   public float ha;  
	   public float hpf;  
	   public float rglo;  
	   public float hurlim;  
	   public float rnetS;       	// OUTPUT // Net radiation in the soil // Mj.m-2
	   public float rnet;       	// OUTPUT // Net radiation  // MJ m-2
	   public float albedolai;       // OUTPUT // P_Albedo of the crop cobining soil with vegetation // SD
	   public float resmes;       	// OUTPUT // Amount of soil water in the measurement depth // mm
	   public float dacouche[]; 	// 0 à dacouche sinon pb le jour de la recolte dans densirac (si codeculture = feuille)
	   public float ruisselt;       // OUTPUT // Total quantity of water in run-off (surface + overflow) // mm
	   public float infilj[]; 
	   public float exces[];       	// OUTPUT // Amount of water  present in the macroporosity of the horizon 1  // mm
	   public float anox[];
	   public float pluieruissel;  
	   public float sat[];
	   public float cpreciptout;	// OUTPUT // Water supply integrated over the simulation period  // mm
	   public float ruissel;       	// OUTPUT // Daily run-off // mm
	   public float QeauI;  
	   public float QeauFS;  
	   public float Qeau0;  
	   public float doi;  
	   public float Edirect;       	// OUTPUT // Water amount evaporated by the soil + intercepted by leafs + intercepted by the mulch  // mm
	   public float humidite;       // OUTPUT // Moisture in the canopy // 0-1
	   public float mouillmulch;  
	   public float Emulch;       	// OUTPUT // Direct evaporation of water intercepted by the mulch // mm
	   public float intermulch;     // OUTPUT // Water intercepted by the mulch (vegetal) // mm
	   public float cintermulch;    // OUTPUT // Amount of rain intercepted by the mulch // mm
	   public float ruisselsurf;    // OUTPUT // Surface run-off // mm
	   public float ras;       		// OUTPUT // Aerodynamic resistance between the soil and the canopy   // s.m-1
	   public float Nvolat;  
	   public float eptcult;  
	   public float TcultMin;  
	   public float TcultMax;       // OUTPUT // Crop surface temperature (daily maximum) // degree C
	   public float dessecplt;  
	   public float eo;       		// OUTPUT // Intermediary variable for the computation of evapotranspiration   // mm
	   public float eos;       		// OUTPUT // Maximum evaporation flux  // mm
	   public float Ratm;       	// OUTPUT // Atmospheric radiation  // Mj.m-2
	   public float hres[];
	   public float Wb[];
	   public float kres[];
	   public float NCbio;       	// OUTPUT // N/C ratio of biomass decomposing organic residues // gN.g-1C
	   public float saturation;     // OUTPUT // Amount of water remaining in the soil macroporosity // mm
	   public float qmulch;       	// OUTPUT // Quantity of plant mulch // t.ha-1

 //////MODIF HISAFE déplacement dans Sol.f90
 //////  public float Ninit(5)
 //////  public float Hinit(5)
 //////  public float HR(5)     
	   
	   public float azomes;       	// OUTPUT // Amount of  mineral nitrogen in the depth of measurement // kgN.ha-1
	   public float ammomes;       	// OUTPUT // Amount of ammonium in the depth of measurement // kgN.ha-1
	   public float FsNH3;       	// OUTPUT // Volatilisation of NH3  // µg.m-2.j-1
	   public float RsurRU;       	// OUTPUT // Fraction of available water reserve (R/RU) over the entire profile // 0 à 1
	   public float DRAT;       	// OUTPUT // Water flux drained at the base of the soil profile integrated over the simulation periodout of the soil   // mm
	   public float QNdrp;  
	   public float esol;       	// OUTPUT // Actual soil evaporation flux  // mm day-1
	   public float et;       		// OUTPUT // Daily evapotranspiration (= es + et) // mm day-1
	   public float tnhc;       	// OUTPUT // "Cumulated  normalized   time  for the mineralisation of humus" // days
	   public float tnrc;       	// OUTPUT // "Cumulated  normalized  time for the mineralisation of organic residues" // days
	   public float pluieN;  
	   public float irrigN;  
	   public float precip;       	// OUTPUT // Daily amount of water (precipitation + irrigation)   // mm day-1
	   public float precipN;  
	   public float cumoffrN;  
	   public float cumoffrN0;  
	   public float cumoffrN100;  
	   public float azorac0;  
	   public float azorac100; 
	   public float demandebrute;  
	   public float absodrp;  
	   public float cpluie;       // OUTPUT // Cumulative rainfall over the simulation period // mm
	   public float Chumt;        // OUTPUT // Total amount of C humus (active + inert fractions) in the soil // kg.ha-1
	   public float Chumt0;       // OUTPUT // Initial amount of C humus (active + inert fractions) in the soil // kg.ha-1
	   public float Nhuma;        // OUTPUT // Amount of active nitrogen in the soil humus pool // kg.ha-1
	   public float Chuma;
	   public float Nhuma0;
	   public float Chuma0;
	   public float Nhumi;
	   public float Chumi;
	   public float Nhumt;       // OUTPUT // Total quantity of N humus (active + inert fractions) in the soil // kg.ha-1
	   public float Nhumt0;
	   public float QCprimed;
	   public float QNprimed;
	   public float Cr;       // OUTPUT // Amount of C in the soil organic residues // kg.ha-1
	   public float Nr;       // OUTPUT // Amount of N remaining in the decaying organic residues in the soil  // kg.ha-1
	   public float Cb;       // OUTPUT // amount of C in the microbial biomass decomposing organic residues mixed with soil // kg.ha-1
	   public float Nb;       // OUTPUT // Amount of N remaining in the biomass decaying organic residues // kg.ha-1
	   public float Nb0;
	   public float Nr0;
	   public float Cb0;
	   public float Cr0;
	   public float Cbmulch0;
	   public float Nbmulch0;
	   public float etm;       // OUTPUT // Maximum evapotranspiration ( = eop + es)  // mm
	   public float precipamm;
	   
	 //////MODIF HISAFE déplacement dans Sol.f90
	 //////  public float P_NH4initf(5)     // PARAMETER // Amounts of initial mineral N in the 5 soil layers (fine earth) // kg.ha-1 // INIT // 1
	 //////  public float NH4init(5)

	   public float eaunoneffic;  
	   public float toteaunoneffic;  
	   public float raamax;  
	   public float raamin;  
	   public float laiTot;                //  LAI total pour un jour de l'ensemble des plantes de la parcelle
	   public float stemflowTot;  
	   public float EmdTot;  
	   public float epTot;  
	   public float hauteurMAX;  
	   public float Chum[];
	   public float Nhum[];
	   public float Cres[];
	   public float Nres[];
	   public float Cbio[];
	   public float Nbio[];
	   public float xmlch1;       	// OUTPUT // Thickness of mulch created by evaporation from the soil // cm
	   public float xmlch2;  
	   public float supres;  
	   public float stoc;  
	   public float cestout;       // OUTPUT // Evaporation integrated over the simulation period // mm
	   public float pfz[];
	   public float etz[];
	   public float parapluieetz;  
	   public float totapN;       	// OUTPUT // Total amount of N inputs from fertiliser and residues // kg.ha-1
	   public float Qminh;       	// OUTPUT // Cumulative mineral N arising from humus // kg.ha-1
	   public float Qminr;       	// OUTPUT // Cumulative mineral N arising from organic residues // kg.ha-1
	   public float QLES;       	// OUTPUT // Cumulative NO3-N leached at the bottom of the soil profile // kg.ha-1
	   
//////MODIF HISAFE déplacement dans Sol.f90
//////public float TS(5)       // OUTPUT // Mean soil temperature (mean of the 5 layers) // degree C
	   
	   public float totapNres;    	// OUTPUT // Total amount of N in organic residues inputs  // kg.ha-1
	   public float Qnitrif;       	// OUTPUT // "cumulative nitrification of nitrogen (if option  nitrification  is activated)" // kg.ha-1
	   public float tcult;       	// OUTPUT // Crop surface temperature (daily average) // degree C
	   public float tcultveille;  
	   public float tsol[];
	   public float tsolveille[];
	   public float HUR[];
	   public float hurmini[];
	   public float HUCC[];
	   public float HUMIN[];
	   
//////MODIF HISAFE déplacement dans Sol.f90
////// public float AZamm(5)       // OUTPUT // Amounts of NH4-N in the 5 soil horizons // kg.ha-1
	   
	   public float effamm;  
	   public float tauxcouv[];       // OUTPUT // Cover rate // SD
	   public float azsup;  
	   public float smes02;  
	   public float sumes0;  
	   public float sumes1;  
	   public float sumes2;  
	   public float sesj0;  
	   public float ses2j0;  
	   public float sum2;  
	   public float esreste;  
	   public float esreste2;  
	   public float drain;       	// OUTPUT // Water flux drained at the base of the soil profile // mm j-1
	   public float lessiv;       	// OUTPUT // daily N-NO3 leached at the base of the soil profile // kgN.ha-1
	   public float fxa;       		// OUTPUT // Anoxic effect on symbiotic uptake // 0-1
	   public float fxn;       		// OUTPUT // Nitrogen effect on symbiotic uptake // 0-1
	   public float fxt;       		// OUTPUT // Temperature effect on symbiotic uptake // 0-1
	   public float fxw;       		// OUTPUT // Water effect on symbiotic uptake // 0-1
	   public float absoTot[];  
	   public float cu0[];   
	   public float somelong0[];   
	   public int nfindorm0[];   
	   public float vmax;  
	   public float cumdltaremobilN;  
	   public float cum_immob;       // OUTPUT // cumulated mineral nitrogen arising from organic residues(immobilization) // kg.ha-1
	   public float QCapp;      	// OUTPUT // cumulative amount of organic C added to soil // kg.ha-1
	   public float QNapp;     		// OUTPUT // cumulative amount of organic N added to soil // kg.ha-1
	   public float QCresorg;  		// OUTPUT // cumulative amount of exogenous C added to soil // kg.ha-1
	   public float QNresorg;  		// OUTPUT // cumulative amount of exogenous N added to soil // kg.ha-1
	   public boolean posibsw;  
	   public boolean posibpe;  
	   
///MODIF HISAFE on remplace les tableaux de boolean qui ne passent pas avec JNA
/// logical :: repoussesemis(2)
/// logical :: repousserecolte(2)	   
	   public int repoussesemis[];  
	   public int repousserecolte[];   
	   
	   public boolean recolte1;  
	   public boolean P_datefin;   // PARAMETER // date of the end of the simulation // days // USMXML // 0


//MODIF HISAFE On enlève les chaines de caractère
	 //  character(len=50) :: codeversion    //  le numéro de version
	 //  character(len=12) :: P_codesimul      //  le type de simulation (culture ou feuille)      // PARAMETER // simulation code (culture ou feuille=lai forcé) // SD // P_USM/USMXML // 0

	   public int P_codesimul;  //  le type de simulation (1=culture ou 2=feuille)
	   public int AOAS; // Cumul des parties Au Soleil & A l'Ombre  
	   public int AS; // Au Soleil  
	   public int AO; // A l'Ombre  
	   public int nouvre2;
	   public int nouvre3;
	   public int naptot;  
	   public int napNtot;  
	   public float anit[];       			// OUTPUT // Daily nitrogen provided  // kgN.ha-1 j-1
	   public float anit_engrais[];        	// OUTPUT // Daily nitrogen provided by fertiliser // kgN.ha-1 j-1
	   public float anit_uree[];       		// OUTPUT // Daily nitrogen provided  by pasture (uree) // kgN.ha-1 j-1
	   public int type_ferti[];   			// OUTPUT // type of fertilizer // SD
	   public float airg[];        			// OUTPUT // Daily irrigation // mm
	   public float totir;       			// OUTPUT // Total amount of water inputs  // mm
	   public float co2res;       			// OUTPUT // CO2 mass flow from the residues // kgC.ha-1.d-1
	   public float co2hum;       			// OUTPUT // CO2 mass flow from the soil humus // kgC.ha-1.d-1
	   public float CO2sol;       			// OUTPUT // CO2 mass flow from the soil // mgCO2.m-2.d-1
	   public float QCO2sol;
	   public float QCO2res;
	   public float QCO2hum;
	   public float QCO2mul;
	   public float tmoy_an[];  
	   public float Tm_histo;  
	   public float deltat_an[]; 
	   public float tm_glisse[]; 
	   public float deltaT_adaptCC[];  
	   public float var_trefh[];  
	   public float var_trefr[];  
	   public float var_tnitmin[];  
	   public float var_tnitmax[];  
	   public float var_tnitopt[];  
	   public float var_tnitopt2[];
	   public float var_TREFdenit1[];  
	   public float var_TREFdenit2[];  
	   public float var_TREFfhum[];  
	   public float var_FTEM[];  
	   public float var_FTEMr[]; 
	   
///MODIF HISAFE ne sert à rien
///  character(len=50) :: fplt_ori(2)
///  character(len=3) :: codeplante_ori(2)
	   
///MODIF HISAFE on enleve les tableaux de boolean qui ne passent pas avec JNA	 
///	   logical :: plante_ori(2)	   
	   public boolean plante_ori; 
	   public int iplt_ori[];  
	   public float Qem_N2O;       // OUTPUT // cumulated N2O emission // kg.ha-1
	   public float em_N2O;       // OUTPUT // daily N2O emission // kgN.ha-1.j-1
	   public float Qem_N2Onit;       // OUTPUT // cumulated N2O-N emission due to nitrification// kg.ha-1
	   public float em_N2Onit;       // OUTPUT // daily N2O-N emission due to nitrification // kg.ha-1.d-1
	   public float Qem_N2Oden;       // OUTPUT // cumulated N2O-N emission due to denitrification// kg.ha-1
	   public float em_N2Oden;       // OUTPUT // daily N2O-N emission due to denitrification // kgN.ha-1.d-1

///MODIF HISAFE ne sert à rien
/// character(len=60) :: sys  
/// character(len=14) :: nomplante  

	   public int nbcoupe_reel[];  
	   public boolean onafaitunecoupedelanneedavant;
	   public int nbcoupe_an1[];   
	   public int julfauche_an1[];    
	   public float lairesiduel_an1[];    
	   public float hautcoupe_an1[];   
	   public float msresiduel_an1[];   
	   public float anitcoupe_an1[];    
	   public float tempfauche_an1[];    
	   public float tempfauche_ancours_an1[];    
	   public int nbcoupe_an2[];   
	   public int julfauche_an2[];    
	   public float lairesiduel_an2[];    
	   public float hautcoupe_an2[];    
	   public float msresiduel_an2[];    
	   public float anitcoupe_an2[];    
	   public float tempfauche_an2[];    
	   public float tempfauche_ancours_an2[];    
	   public int restit_an1[];  
	   public float mscoupemini_an1[];  
	   public int restit_an2[];  
	   public float mscoupemini_an2[];  
	   public float irrigjN;      // OUTPUT // mineral nitrogen from irrigation // kgN
	   public float precipjN;       // OUTPUT // mineral nitrogen from rainfall // kgN
	   public float apport_mini_semis;  

///MODIF HISAFE ne sert à rien
///   character(len=15) :: nom_variete  

	   public int iplt[];  
	   public int iwater0;
	   public int ansemis0;
	   public int iwaterapres;  
	   public int ifwaterapres;  
	   public int nbjsemis0;  
	   public int iwater_cultsuiv;  
	   public int ifwater_cultsuiv;  
	   public float beta_sol[];  
	   public float offrN[]; 
	   public float absz[]; 
	   public float nodn;       // OUTPUT // Nitrogen stress effect on nodosities establishment // 0 ou 1
	   public float trosemax[];   


///MODIF HISAFE ne sert à rien pour nous 
///   character(len=20) :: valprof  //
///   character(len=20) :: valrap(100)
///   character(len=20) :: valpar(100)    //  stockage des noms des variables de sortie
//  character(len=10) :: nomvarprof(10)

	   public int numdateprof[], numdebprof[],dateprof[],nbvarsortie;
	   public int nbvarrap;  
	   public boolean ecritrap;  

///MODIF HISAFE ne sert à rien pour nous
///	   public float tabprof[], valsortie[], valsortierap[];
///	   public float  valsortie_flo[], valsortie_mat[] , valsortie_iplt[];
	   public float QH2Of; 

///MODIF HISAFE il faudra remplacer les chaines de caratères par des entiers   
/// character(len=9) :: staderap(20)
	   
	   public int codeaucun;  //
	   public int codeenteterap;  //
	   public int codeenteterap_agmip; // dr 11/03/2013 ajout pour pouvoir avoir entete ou pas dans rappeort_agmip
	   public int  codetyperap;  

	   public boolean raplev;  
	   public boolean rapamf;  
	   public boolean raplax;  
	   public boolean rapflo;  
	   public boolean rapdrp;  
	   public boolean raprec;  
	   public boolean rapsen;  
	   public boolean rapfin;  
	   public boolean rapplt;  
	   public boolean rapger;  
	   public boolean rapdebdes;  
	   public boolean rapdebdebour;  
	   public boolean rapmat;  
	   public boolean rapdebdorm;  
	   public boolean rapfindorm;  
	   public boolean rapdeb;
	   public boolean start_rap;
	   public float RU;       // OUTPUT // maximum available water reserve over the entire profile // mm
	   public float concNO3sol[];       // OUTPUT // Nitrate concentration in the horizon 4 water // mg NO3 l-1
	   public float FTEMhb;  
	   public float FTEMrb;  
	   public float Cresdec[];    // OUTPUT // C in residue (i) decomposing mixed with soil // kg.ha-1
	   public float Nresdec[];    // OUTPUT // N in residue (i) decomposing mixed with soil // kg.ha-1
	   public float Cnondec[];    // OUTPUT // undecomposable C in residue i present in the mulch // kg.ha-1
	   public float Nnondec[];   // OUTPUT // undecomposable N in residue i present in the mulch // kg.ha-1
	   public float Cmuldec[];    // OUTPUT // C in residue (i) decomposing in mulch at soil surface // kg.ha-1
	   public float Nmuldec[];    // OUTPUT // N in residue (i) decomposing in mulch at soil surface // kg.ha-1
	   public float Cmulch;     // OUTPUT // Total C in mulch at soil surface // kg.ha-1
	   public float Nmulch;     // OUTPUT // Total N in mulch at soil surface // kg.ha-1
	   public float Cmulchnd;       // OUTPUT // total undecomposable C stock in mulch // kg.ha-1
	   public float Nmulchnd;       // OUTPUT // total undecomposable N stock in mulch // kg.ha-1
	   public float Cmulchdec;       // OUTPUT // total undecomposable C stock in mulch // kg.ha-1
	   public float Nmulchdec;       // OUTPUT // total undecomposable N stock in mulch // kg.ha-1
	   public float Cbmulch;       // OUTPUT // amount of C in the microbial biomass decomposing organic residues at soil surface (mulch) // kg.ha-1
	   public float Nbmulch;       // OUTPUT // amount of N in microbial biomass decomposing mulch // kg.ha-1
	   public float Cmulch0;      // OUTPUT // total undecomposable C stock in mulch at time 0 // kg.ha-1
	   public float Nmulch0;      // OUTPUT // total undecomposable N stock in mulch at time 0 // kg.ha-1
	   public float couvermulch;       // OUTPUT // Cover ratio of mulch  // 0-1
	   public float Ctousresidusprofil;       // OUTPUT // total of carbon from the residues (all residues on P_profhum) // kg.ha-1
	   public float Ntousresidusprofil;       // OUTPUT // total of Nitrogen from residues (all residues on P_profhum) // kgN.ha-1
	   public float Cresiduprofil[];        // OUTPUT // total of Carbon from residues (ires) on P_profhum // kg.ha-1
	   public float Nresiduprofil[];        // OUTPUT // total of nitrogen from residues (ires) on P_profhum // kgN.ha-1
	   public float qmulcht;  
	   public int irmulch;  
	   public float drain_from_plt;        // OUTPUT // cumulative amount of water drained at the base of the soil profile over the crop period (planting-harvest)  // mm
	   public float leaching_from_plt;     // OUTPUT //cumulative N-no3 leached at the base of the soil profile over the crop period (planting-harvest) // kg.ha-1
	   public float runoff_from_plt;       // OUTPUT // cumulative Total quantity of water in run-off (surface + overflow) over the crop period (planting-harvest) // mm
	   public float Nmineral_from_plt;  // OUTPUT // mineral N from the mineralisation of humus and organic residues cumulated over the crop period (planting-harvest) // kg.ha-1
	   public float Nvolat_from_plt;  // OUTPUT // cumulative amount of N volatilised from fertiliser + organic inputs over the crop period (planting-harvest)// kg.ha-1
	   public float QNdenit_from_plt;  // OUTPUT // Cumulative denitrification of nitrogen from fertiliser or soil (if option  denitrification  is activated) over the crop period (planting-harvest)// kg.ha-1
	   public float SoilAvW;    //OUTPUT// variable eau dispo pour la plante  sur la profondeur profmes // mm
	   public float SoilWatM;
	   public float SoilNM;
	   public int P_profmesW;      // PARAMETER // depth of observed Water content  // cm //depths_paramv6.txt // 1
	   public int P_profmesN;      // PARAMETER // depth of observed Nitrogen content  // cm //depths_paramv6.txt // 1

	   public int ECRITURE_HISTORIQUE;  
	   public int ECRITURE_SORTIESJOUR;  
	   public int ECRITURE_RAPPORTS;  
	   public int ECRITURE_BILAN;  
	   public int ECRITURE_PROFIL;  
	   public int ECRITURE_DEBUG;
	   public int ECRITURE_ECRAN;
	   public int ECRITURE_AGMIP;
	   public boolean DEBUG;  
	   public int DEVELOP;  
	   public int CALAI;  
	   public int BIOMAER;  
	   public int SENESCEN;  
	   public int FRUIT;  
	   public int GRAIN;  
	   public int EAUQUAL;  
	   public int CROISSANCEFRONTRACINAIRE;  
	   public int PROFILRACINAIRE;  
	   public int DENSITEVRAIERACINAIRE;  
	   public int CALPSIBASE;  
	   public int REPARTIR;  
	   public int IRRIG;  
	   public int CALAPNENUPVT;  
	   public int CALCULAUTOMATIQUEIRRIGATION;  
	   public int APPORTSNPARPLUIEETIRRIGATION;  
	   public int APPORTSNPARENGRAISMINERAUX;  
	   public int APPORTSORGANIQUESETTRAVAILDUSOL;  
	   public int RESIDUS;  
	   public int ETATSURF;  
	   public int MINERAL;  
	   public int KETP;  
	   public int SHUTWALL;  
	   public int OFFRNODU;  
	   public int BNPL;  
	   public int LIXIV;  
	   public int TRANSPI;  
	   public int OFFREN;  
	   public int ABSON;  
	   public int MAJNSOL;  
	   public int STRESSEAU;  
	   public int STRESSN;  
	   public int NGRAIN;  
	   public int EXCESDEAU;  
	   public int CALTCULT_SJ;  
	   public int CALTCULT_SHUTWALL;  
	   public int CALRNET_SHUTWALL;  
	   public int TEMPSOL;  
	   public int HUMCOUV_SJ;  
	   public int HUMHEURE;  
	   public int SOLNU;  
	   public int DETASSEMENT;  
	   public int TASSESEMISRECOLTE;  
	   public float surfAO;
	   public float surfAS;
	   public int  compt_calcul_taylor;
	   public int n_datedeb_irrigauto;
	   public int n_datefin_irrigauto;
	   public int code_ecrit_nom_usm;
	   public float HR_vol_1_30;    // OUTPUT //water content of the horizon 1-30 cm  (table) //mm
	   public float HR_vol_31_60;   // OUTPUT //water content of the horizon 31-60 cm  (table) //mm
	   public float HR_vol_61_90;   // OUTPUT //water content of the horizon 61-90 cm  (table) //mm
	   public float HR_vol_91_120;    // OUTPUT //water content of the horizon 91-120 cm  (table) //mm
	   public float HR_vol_121_150;    // OUTPUT //water content of the horizon 121-150 cm  (table) //mm
	   public float HR_vol_151_180;    // OUTPUT //water content of the horizon 151-180 cm  (table) //mm
	   public int  day_after_sowing;  // OUTPUT //days after sowing //days

///MODIF HISAFE : ne sert à rien
///   character*20  :: info_level , model_name


	   public float N_mineralisation;
	   public float soilN;
	   public float tcult_tairveille;
	   public float humidite_percent;   // OUTPUT // Moisture in the canopy // %
	   public boolean flag_onacoupe;
	   public float CsurNres_pature;
	   public float qres_pature;

	   
	 ///MODIF HISAFE : in est obligé de passer cette variable climat ici   
      public int nitetcult[];    // OUTPUT // Number of iterations to calculate TCULT // SD
  
      ///MODIF HISAFE on rajoute les tableaux d'extraction eau et azote de l'arbre
      public float treeWaterUptake[];
      public float treeNitrogenUptake[];

      
      
	public SafeSticsCommun () {
		flag_record=false;
		P_nbplantes = 1;
		P_codesimul = 1;
		nbjmax      = 366;
		nbans 		= 1;
		AOAS 		= 0; 		// Cumul des parties Au Soleil & A l'Ombre  
		AS 			= 1; 		// Au Soleil  
		AO 			= 2; 		// A l'Ombre  
		ECRITURE_HISTORIQUE = 1;  
		ECRITURE_SORTIESJOUR = 2;  
		ECRITURE_RAPPORTS = 4;  
		ECRITURE_BILAN = 8;  
		ECRITURE_PROFIL = 16;  
		ECRITURE_DEBUG = 32;
		ECRITURE_ECRAN = 64;
		ECRITURE_AGMIP = 128;
		DEBUG = true;  
		DEVELOP = 0;  
		CALAI = 0;  
		BIOMAER = 0;  
		SENESCEN = 0;  
		FRUIT = 0;  
		GRAIN = 0;  
		EAUQUAL = 0;  
		CROISSANCEFRONTRACINAIRE = 0;  
		PROFILRACINAIRE = 0;  
		DENSITEVRAIERACINAIRE = 0;  
		CALPSIBASE = 0;  
		REPARTIR = 0;  
		IRRIG = 0;  
		CALAPNENUPVT = 0;  
		CALCULAUTOMATIQUEIRRIGATION = 0;  
		APPORTSNPARPLUIEETIRRIGATION = 0;  
		APPORTSNPARENGRAISMINERAUX = 0;  
		APPORTSORGANIQUESETTRAVAILDUSOL = 0;  
		RESIDUS = 0;  
		ETATSURF = 0;  
		MINERAL = 0;  
		KETP = 0;  
		SHUTWALL = 0;  
		OFFRNODU = 0;  
		BNPL = 0;  
		LIXIV = 0;  
		TRANSPI = 0;  
		OFFREN = 0;  
		ABSON = 0;  
		MAJNSOL = 0;  
		STRESSEAU = 0;  
		STRESSN = 0;  
		NGRAIN = 0;  
		EXCESDEAU = 0;  
		CALTCULT_SJ = 0;  
		CALTCULT_SHUTWALL = 0;  
		CALRNET_SHUTWALL = 0;  
		TEMPSOL = 0;  
		HUMCOUV_SJ = 0;  
		HUMHEURE = 0;  
		SOLNU = 0;  
		DETASSEMENT = 0;  
		TASSESEMISRECOLTE = 0;  

		//RAZ
		
		ipl = 0;
		jour = 0;
		nummois = 0;
		P_iwater = 0;
		P_ifwater = 0;
		P_culturean = 1;
		  ifwater_courant = 0;
		  ifwater0 = 0;
		  dernier_n = 0;
		  n = 0;
		  jjul = 0;
		  jul = 0;
		  
		  nbjanrec = 0;
		  nstoc = 0;
		  numcult = 0;
		  ens = 0;
		  fichist = 0;
		  ficsta = 0;
		  ficdbg = 0;
		  codoptim = 0;
		  nbjsemis = 0;
		  maxwth = 0;
		  nstoprac = 0;
		  numdate = 0;
		  bouchon = 0;
		  nouvdensrac = 0;
		  nbCouchesSol = 0;
		  nappmulch = 0;
		  ires = 0;
		  itrav = 0;
		  ansemis = 0;
		  anrecol = 0;
		  NH = 0;
		  codeprofil = 0;
		  nbjrecol = 0;
		  nbjrecol0 = 0;
		  NHE = 0;
		  faucheannule = 0;
		  delta = 0.0f;
		  devjourfr = 0.0f;
		  fdens = 0.0f;
		  tustress = 0.0f;
		  rdif = 0.0f;
		  originehaut = 0.0f;
		  tairveille = 0.0f;
		  coefbmsres = 0.0f;
		  coefaifres = 0.0f;
		  coefbifres = 0.0f;
		  coefcifres = 0.0f;
		  a = 0.0f;
		  effN = 0.0f;
		  hi = 0.0f;
		  ha = 0.0f;
		  hpf = 0.0f;
		  rglo = 0.0f;
		  hurlim = 0.0f;
		  rnetS = 0.0f;
		  rnet = 0.0f;
		  albedolai = 0.0f;
		  resmes = 0.0f;
		  ruisselt = 0.0f;
		  pluieruissel = 0.0f;
		  cpreciptout = 0.0f;
		  ruissel = 0.0f;
		  QeauI = 0.0f;
		  QeauFS = 0.0f;
		  Qeau0 = 0.0f;
		  doi = 0.0f;
		  Edirect = 0.0f;
		  humidite = 0.0f;
		  mouillmulch = 0.0f;
		  Emulch = 0.0f;
		  intermulch = 0.0f;
		  cintermulch = 0.0f;
		  ruisselsurf = 0.0f;
		  ras = 0.0f;
		  Nvolat = 0.0f;
		  eptcult = 0.0f;
		  TcultMin = 0.0f;
		  TcultMax = 0.0f;
		  dessecplt = 0.0f;
		  eo = 0.0f;
		  eos = 0.0f;
		  Ratm = 0.0f;
		  NCbio = 0.0f;
		  saturation = 0.0f;
		  qmulch = 0.0f;
		  couvermulch = 0.0f;
		  azomes = 0.0f;
		  ammomes = 0.0f;
		  FsNH3 = 0.0f;
		  RsurRU = 0.0f;
		  DRAT = 0.0f;
		  QNdrp = 0.0f;
		  esol = 0.0f;
		  et = 0.0f;
		  tnhc = 0.0f;
		  tnrc = 0.0f;
		  pluieN = 0.0f;
		  irrigN = 0.0f;
		  precip = 0.0f;
		  precipN = 0.0f;
		  cumoffrN = 0.0f;
		  cumoffrN0 = 0.0f;
		  cumoffrN100 = 0.0f;
		  azorac0 = 0.0f;
		  azorac100 = 0.0f;
		  demandebrute = 0.0f;
		  absodrp = 0.0f;
		  cpluie = 0.0f;
		  Chumt = 0.0f;
		  Chumt0 = 0.0f;
		  Nhuma = 0.0f;
		  Nhuma0 = 0.0f;
		  Nhumi = 0.0f;
		  Nhumt = 0.0f;
		  Nhumt0 = 0.0f;
		  Cr = 0.0f;
		  Nr = 0.0f;
		  Cb = 0.0f;
		  Nb = 0.0f;
		  etm = 0.0f;
		  precipamm = 0.0f;
		  eaunoneffic = 0.0f;
		  toteaunoneffic = 0.0f;
		  raamax = 0.0f;
		  raamin = 0.0f;
		  laiTot = 0.0f;
		  stemflowTot = 0.0f;
		  EmdTot = 0.0f;
		  epTot = 0.0f;
		  hauteurMAX = 0.0f;
		  Cmulchnd    = 0.0f;   
		  Nmulchnd    = 0.0f;
		  Cmulch0   = 0.0f;
		  Nmulch0   = 0.0f;
		  xmlch1 = 0.0f;
		  xmlch2 = 0.0f;
		  supres = 0.0f;
		  stoc = 0.0f;
		  cestout = 0.0f;
		  parapluieetz = 0.0f;
		  totapN = 0.0f;
		  Qminh = 0.0f;
		  Qminr = 0.0f;
		  cum_immob = 0.0f;
		  QLES = 0.0f;
		  totapNres = 0.0f;
		  Qnitrif = 0.0f;
		  tcult = 0.0f;
		  tcultveille = 0.0f;
		  effamm = 0.0f;
		  azsup = 0.0f;
		  smes02 = 0.0f;
		  sumes0 = 0.0f;
		  sumes1 = 0.0f;
		  sumes2 = 0.0f;
		  sesj0 = 0.0f;
		  ses2j0 = 0.0f;
		  sum2 = 0.0f;
		  esreste = 0.0f;
		  esreste2 = 0.0f;
		  drain = 0.0f;
		  lessiv = 0.0f;
		  fxa = 0.0f;
		  fxn = 0.0f;
		  fxt = 0.0f;
		  fxw = 0.0f;
		  vmax = 0.0f;
		  cumdltaremobilN = 0.0f;
		  posibsw = false;
		  posibpe = false;

		  recolte1 = false;
		  P_datefin = false;
		  nouvre2 = 0;
		  nouvre3 = 0;
		  naptot = 0;
		  napNtot = 0;
		  totir = 0.0f;
		  co2res = 0.0f;
		  co2hum = 0.0f;
		  CO2sol = 0.0f; 
		  QCO2sol = 0.0f; 
		  QCO2hum = 0.0f;
		  QCO2res = 0.0f;
		  QCO2mul = 0.0f;
		  QCprimed = 0.0f;
		  QNprimed = 0.0f;
		  Tm_histo = 0.0f;
		  plante_ori = false;
		  Qem_N2O = 0.0f;
		  em_N2O = 0.0f;
		  Qem_N2Onit = 0.0f;
		  em_N2Onit = 0.0f;
		  Qem_N2Oden = 0.0f;
		  em_N2Oden = 0.0f;
		  irrigjN = 0.0f;
		  precipjN = 0.0f;
		  apport_mini_semis = 0.0f;
		  iwater0 = 0;
		  ansemis0=0;
		  iwaterapres = 0;
		  ifwaterapres = 0;
		  nbjsemis0 = 0;
		  iwater_cultsuiv = 0;
		  ifwater_cultsuiv = 0;
		  nodn = 0.0f;
		  nbvarsortie = 0;
		  nbvarrap = 0;
		  ecritrap = false;
		  QH2Of = 0.0f;
		  codeaucun = 0;
		  codeenteterap = 0;
		  codeenteterap_agmip = 0;
		  codetyperap = 0;
		  raplev = false;
		  rapamf = false;
		  raplax = false;
		  rapflo = false;
		  rapdrp = false;
		  raprec = false;
		  rapsen = false;
		  rapfin = false;
		  rapplt = false;
		  rapger = false;
		  rapdebdes = false;
		  rapdebdebour = false;
		  rapmat = false;
		  rapdebdorm = false;
		  rapfindorm = false;
		  rapdeb = false;
		  start_rap=false;
		  FTEMhb = 0.0f;
		  FTEMrb = 0.0f;
		  Cmulch   = 0.0f;
		  Nmulch   = 0.0f;
		  drain_from_plt = 0.0f;
		  runoff_from_plt = 0.0f;
		  leaching_from_plt = 0.0f;
		  Nmineral_from_plt =0.0f;
		  Nvolat_from_plt = 0.0f;
		  QNdenit_from_plt = 0.0f;
		  SoilAvW = 0;
		  SoilWatM = 0;
		  SoilNM = 0;
		  HR_vol_1_30 = 0.0f;
		  HR_vol_31_60 = 0.0f;
		  HR_vol_61_90 = 0.0f;
		  HR_vol_91_120 = 0.0f;
		  HR_vol_121_150 = 0.0f;
		  HR_vol_151_180 = 0.0f;
		  day_after_sowing = 0;
		  Ctousresidusprofil = 0.0f;
		  Ntousresidusprofil = 0.0f;
		  flag_onacoupe=false;
		  CsurNres_pature =0.0f;
		  qres_pature=0.0f;
				
				//Creation tables
		annee = new int[732];  	   
		napini = new int[2];  
		napNini = new int[2];  
		nbjresini = new int[2];  
		esz = new float[1000]; 
		dacouche = new float[1001];		   
		infilj = new float[6];
		exces = new float[6];
		anox = new float[1000];
		sat = new float[1000];		   
		hres = new float[21];
		Wb = new float[21];
		kres = new float[21];	
		Chum = new float[1000];
		Nhum = new float[1000];
		Cres = new float[21000];
		Nres = new float[21000];
		Cbio = new float[21000];
		Nbio = new float[21000];
		pfz = new float[1000];
		etz = new float[1000];
		tsol = new float[1001];
		tsolveille = new float[1000];
		HUR = new float[1000];
		hurmini = new float[1000];
		HUCC = new float[1000];
		HUMIN = new float[1000];
		tauxcouv = new float[367];
		absoTot = new float[5];
		cu0 = new float[2];
		somelong0 = new float[2];
		nfindorm0 = new int[2];
		
		repoussesemis = new int[2];  
		repousserecolte = new int[2];
		
		anit = new float[366];
		anit_engrais = new float[366];
		anit_uree = new float[366];
		type_ferti = new int[366];
		airg = new float[366];
		tmoy_an = new float[600];
		deltat_an = new float[300];
		tm_glisse = new float[200];
		deltaT_adaptCC = new float[200];
		var_trefh = new float[200];
		var_trefr = new float[200];
		var_tnitmin = new float[200];
		var_tnitmax = new float[200];
		var_tnitopt = new float[200];
		var_tnitopt2 = new float[200];
		var_TREFdenit1 = new float[200];
		var_TREFdenit2 = new float[200];
		var_TREFfhum = new float[200];
		var_FTEM = new float[200];
		var_FTEMr = new float[200];
		iplt_ori = new int[2];
		nbcoupe_reel = new int[2];
		nbcoupe_an1 = new int[2];
		julfauche_an1 = new int[40];
		lairesiduel_an1 = new float[40];
		hautcoupe_an1 = new float[40];
		msresiduel_an1 = new float[40];
		anitcoupe_an1 = new float[40];
		anitcoupe_an1 = new float[40];
		tempfauche_an1 = new float[40];
		tempfauche_ancours_an1 = new float[40];
		nbcoupe_an2 = new int[2];  
		julfauche_an2 = new int[40]; 
		lairesiduel_an2 = new float[40]; 
		hautcoupe_an2 = new float[40];  
		msresiduel_an2 = new float[40]; 
		anitcoupe_an2 = new float[40];  
		tempfauche_an2 = new float[40]; 
		tempfauche_ancours_an2 = new float[40];  
		restit_an1 = new int[40];
		mscoupemini_an1 = new float[40];
		restit_an2 = new int[40];
		mscoupemini_an2 = new float[40];		
		iplt = new int[2];
		beta_sol = new float[2];
		offrN = new float[1000];
		absz = new float[1000];
		trosemax = new float[367];
		numdateprof = new int[2];
		numdebprof = new int[2];
		dateprof = new int[1200];

		concNO3sol = new float[5];
		Cresdec = new float[11];
		Nresdec = new float[11];
		Cnondec = new float[10];
		Nnondec = new float[10];
		Cmuldec = new float[10];
		Nmuldec = new float[10];
		Cresiduprofil = new float[11];
		Nresiduprofil = new float[11];
		nitetcult = new int[367];

		
	   treeWaterUptake = new float[1000];
	   treeNitrogenUptake = new float[1000];

	      
	      
	}
	
	
	/**
	* Constructor for cloning 
	*/
	public SafeSticsCommun (SafeSticsCommun origin) {
		

		flag_record=false;
		P_nbplantes = 1;
		nbjmax      = 366;
		AOAS = 0; 		// Cumul des parties Au Soleil & A l'Ombre  
		AS = 1; 		// Au Soleil  
		AO = 2; 		// A l'Ombre  
		ECRITURE_HISTORIQUE = 1;  
		ECRITURE_SORTIESJOUR = 2;  
		ECRITURE_RAPPORTS = 4;  
		ECRITURE_BILAN = 8;  
		ECRITURE_PROFIL = 16;  
		ECRITURE_DEBUG = 32;
		ECRITURE_ECRAN = 64;
		ECRITURE_AGMIP = 128;
		DEBUG = false;  
		DEVELOP = 0;  
		CALAI = 0;  
		BIOMAER = 0;  
		SENESCEN = 0;  
		FRUIT = 0;  
		GRAIN = 0;  
		EAUQUAL = 0;  
		CROISSANCEFRONTRACINAIRE = 0;  
		PROFILRACINAIRE = 0;  
		DENSITEVRAIERACINAIRE = 0;  
		CALPSIBASE = 0;  
		REPARTIR = 0;  
		IRRIG = 0;  
		CALAPNENUPVT = 0;  
		CALCULAUTOMATIQUEIRRIGATION = 0;  
		APPORTSNPARPLUIEETIRRIGATION = 0;  
		APPORTSNPARENGRAISMINERAUX = 0;  
		APPORTSORGANIQUESETTRAVAILDUSOL = 0;  
		RESIDUS = 0;  
		ETATSURF = 0;  
		MINERAL = 0;  
		KETP = 0;  
		SHUTWALL = 0;  
		OFFRNODU = 0;  
		BNPL = 0;  
		LIXIV = 0;  
		TRANSPI = 0;  
		OFFREN = 0;  
		ABSON = 0;  
		MAJNSOL = 0;  
		STRESSEAU = 0;  
		STRESSN = 0;  
		NGRAIN = 0;  
		EXCESDEAU = 0;  
		CALTCULT_SJ = 0;  
		CALTCULT_SHUTWALL = 0;  
		CALRNET_SHUTWALL = 0;  
		TEMPSOL = 0;  
		HUMCOUV_SJ = 0;  
		HUMHEURE = 0;  
		SOLNU = 0;  
		DETASSEMENT = 0;  
		TASSESEMISRECOLTE = 0;  
		
		
		ipl=origin.ipl;
		P_iwater=origin.P_iwater;
		P_ifwater=origin.P_ifwater;
		P_culturean = origin.P_culturean;
		ifwater_courant=origin.ifwater_courant;
		ifwater0=origin.ifwater0;
		dernier_n=origin.dernier_n;
		n=origin.n;

		jjul=origin.jjul;
		jul=origin.jul;
		nbans=origin.nbans;
		nbjanrec=origin.nbjanrec;
		nstoc=origin.nstoc;
		numcult=origin.numcult;
		ens=origin.ens;
		fichist=origin.fichist;
		ficsta=origin.ficsta;
		ficdbg=origin.ficdbg;
		ficdbg2=origin.ficdbg2;

		codoptim=origin.codoptim;
		P_codesuite=origin.P_codesuite;
		nbjsemis=origin.nbjsemis;
		maxwth=origin.maxwth;
		nstoprac=origin.nstoprac;
		numdate=origin.numdate;
		bouchon=origin.bouchon;
		nouvdensrac=origin.nouvdensrac;
		nbCouchesSol=origin.nbCouchesSol;
		nappmulch=origin.nappmulch;
		ires=origin.ires;
		itrav=origin.itrav;
		ansemis=origin.ansemis;
		anrecol=origin.anrecol;
		NH=origin.NH;
		codeprofil=origin.codeprofil;
		nbjrecol=origin.nbjrecol;
		nbjrecol0=origin.nbjrecol0;
		NHE=origin.NHE;
		faucheannule=origin.faucheannule;
		delta=origin.delta;
		devjourfr=origin.devjourfr;
		fdens=origin.fdens;
		tustress=origin.tustress;
		rdif=origin.rdif;
		originehaut=origin.originehaut;
		tairveille=origin.tairveille;
		coefbmsres=origin.coefbmsres;
		coefaifres=origin.coefaifres;
		coefbifres=origin.coefbifres;
		coefcifres=origin.coefcifres;
		a=origin.a;
		effN=origin.effN;
		hi=origin.hi;
		ha=origin.ha;
		hpf=origin.hpf;
		rglo=origin.rglo;
		hurlim=origin.hurlim;
		rnetS=origin.rnetS;
		rnet=origin.rnet;
		albedolai=origin.albedolai;
		resmes=origin.resmes;
		ruisselt=origin.ruisselt;
		pluieruissel=origin.pluieruissel;
		cpreciptout=origin.cpreciptout;
		ruissel=origin.ruissel;
		QeauI=origin.QeauI;
		QeauFS=origin.QeauFS;
		Qeau0=origin.Qeau0;
		doi=origin.doi;
		Edirect=origin.Edirect;
		humidite=origin.humidite;
		mouillmulch=origin.mouillmulch;
		Emulch=origin.Emulch;
		intermulch=origin.intermulch;
		cintermulch=origin.cintermulch;
		ruisselsurf=origin.ruisselsurf;
		ras=origin.ras;
		Nvolat=origin.Nvolat;
		eptcult=origin.eptcult;
		TcultMin=origin.TcultMin;
		TcultMax=origin.TcultMax;
		dessecplt=origin.dessecplt;
		eo=origin.eo;
		eos=origin.eos;
		Ratm=origin.Ratm;
		NCbio=origin.NCbio;
		saturation=origin.saturation;
		qmulch=origin.qmulch;
		azomes=origin.azomes;
		ammomes=origin.ammomes;
		FsNH3=origin.FsNH3;
		RsurRU=origin.RsurRU;
		DRAT=origin.DRAT;
		QNdrp=origin.QNdrp;
		esol=origin.esol;
		et=origin.et;
		tnhc=origin.tnhc;
		tnrc=origin.tnrc;
		pluieN=origin.pluieN;
		irrigN=origin.irrigN;
		precip=origin.precip;
		precipN=origin.precipN;
		cumoffrN=origin.cumoffrN;
		cumoffrN0=origin.cumoffrN0;
		cumoffrN100=origin.cumoffrN100;
		azorac0=origin.azorac0;
		azorac100=origin.azorac100;
		demandebrute=origin.demandebrute;
		absodrp=origin.absodrp;
		cpluie=origin.cpluie;
		Chumt=origin.Chumt;
		Chumt0=origin.Chumt0;
		Nhuma=origin.Nhuma;
		Chuma=origin.Chuma;
		Nhuma0=origin.Nhuma0;
		Chuma0=origin.Chuma0;
		Nhumi=origin.Nhumi;
		Chumi=origin.Chumi;
		Nhumt=origin.Nhumt;
		Nhumt0=origin.Nhumt0;
		QCprimed=origin.QCprimed;
		QNprimed=origin.QNprimed;
		Cr=origin.Cr;
		Nr=origin.Nr;
		Cb=origin.Cb;
		Nb=origin.Nb;
		Nb0=origin.Nb0;
		Nr0=origin.Nr0;
		Cb0=origin.Cb0;
		Cr0=origin.Cr0;
		Cbmulch0=origin.Cbmulch0;
		Nbmulch0=origin.Nbmulch0;
		etm=origin.etm;
		precipamm=origin.precipamm;
		eaunoneffic=origin.eaunoneffic;
		toteaunoneffic=origin.toteaunoneffic;
		raamax=origin.raamax;
		raamin=origin.raamin;
		laiTot=origin.laiTot;
		stemflowTot=origin.stemflowTot;
		EmdTot=origin.EmdTot;
		epTot=origin.epTot;
		hauteurMAX=origin.hauteurMAX;
		xmlch1=origin.xmlch1;
		xmlch2=origin.xmlch2;
		supres=origin.supres;
		stoc=origin.stoc;
		cestout=origin.cestout;
		parapluieetz=origin.parapluieetz;
		totapN=origin.totapN;
		Qminh=origin.Qminh;
		Qminr=origin.Qminr;
		QLES=origin.QLES;			
		totapNres=origin.totapNres;
		Qnitrif=origin.Qnitrif;
		tcult=origin.tcult;
		tcultveille=origin.tcultveille;			
		effamm=origin.effamm;
		azsup=origin.azsup;
		smes02=origin.smes02;
		sumes0=origin.sumes0;
		sumes1=origin.sumes1;
		sumes2=origin.sumes2;
		sesj0=origin.sesj0;
		ses2j0=origin.ses2j0;
		sum2=origin.sum2;
		esreste=origin.esreste;
		esreste2=origin.esreste2;
		drain=origin.drain;
		lessiv=origin.lessiv;
		fxa=origin.fxa;
		fxn=origin.fxn;
		fxt=origin.fxt;
		fxw=origin.fxw;
		vmax=origin.vmax;
		cumdltaremobilN=origin.cumdltaremobilN;
		cum_immob=origin.cum_immob;
		QCapp=origin.QCapp;
		QNapp=origin.QNapp;
		QCresorg=origin.QCresorg;
		QNresorg=origin.QNresorg;
		posibsw=origin.posibsw;
		posibpe=origin.posibpe;

		recolte1=origin.recolte1;
		P_datefin=origin.P_datefin;
		P_codesimul=origin.P_codesimul;
		AOAS=origin.AOAS;
		AS=origin.AS;
		AO=origin.AO;
		nouvre2=origin.nouvre2;
		nouvre3=origin.nouvre3;
		naptot=origin.naptot;
		napNtot=origin.napNtot;
		totir=origin.totir;
		co2res=origin.co2res;
		co2hum=origin.co2hum;
		CO2sol=origin.CO2sol;
		QCO2sol=origin.QCO2sol;
		QCO2res=origin.QCO2res;
		QCO2hum=origin.QCO2hum;
		QCO2mul=origin.QCO2mul;
		Qem_N2O=origin.Qem_N2O;
		em_N2O=origin.em_N2O;
		Qem_N2Onit=origin.Qem_N2Onit;
		em_N2Onit=origin.em_N2Onit;
		Qem_N2Oden=origin.Qem_N2Oden;
		em_N2Oden=origin.em_N2Oden;		
		onafaitunecoupedelanneedavant=origin.onafaitunecoupedelanneedavant;
		irrigjN=origin.irrigjN;
		precipjN=origin.precipjN;
		apport_mini_semis=origin.apport_mini_semis;
		iwater0=origin.iwater0;
		ansemis0=origin.ansemis0;
		iwaterapres=origin.iwaterapres;
		ifwaterapres=origin.ifwaterapres;
		nbjsemis0=origin.nbjsemis0;
		iwater_cultsuiv=origin.iwater_cultsuiv;
		ifwater_cultsuiv=origin.ifwater_cultsuiv;
		nbvarrap=origin.nbvarrap;
		ecritrap=origin.ecritrap;
		QH2Of=origin.QH2Of;			
		codeaucun=origin.codeaucun;
		codeenteterap=origin.codeenteterap;
		codeenteterap_agmip=origin.codeenteterap_agmip;
		codetyperap=origin.codetyperap;
		raplev=origin.raplev;
		rapamf=origin.rapamf;
		raplax=origin.raplax;
		rapflo=origin.rapflo;
		rapdrp=origin.rapdrp;
		raprec=origin.raprec;
		rapsen=origin.rapsen;
		rapfin=origin.rapfin;
		rapplt=origin.rapplt;
		rapger=origin.rapger;
		rapdebdes=origin.rapdebdes;
		rapdebdebour=origin.rapdebdebour;
		rapmat=origin.rapmat;
		rapdebdorm=origin.rapdebdorm;
		rapfindorm=origin.rapfindorm;
		rapdeb=origin.rapdeb;
		start_rap=origin.start_rap;
		RU=origin.RU;
		FTEMhb=origin.FTEMhb;
		FTEMrb=origin.FTEMrb;
		Cmulch=origin.Cmulch;
		Nmulch=origin.Nmulch;
		Cmulchnd=origin.Cmulchnd;
		Nmulchnd=origin.Nmulchnd;
		Cmulchdec=origin.Cmulchdec;
		Nmulchdec=origin.Nmulchdec;
		Cbmulch=origin.Cbmulch;
		Nbmulch=origin.Nbmulch;
		Cmulch0=origin.Cmulch0;
		Nmulch0=origin.Nmulch0;
		couvermulch=origin.couvermulch;
		Ctousresidusprofil=origin.Ctousresidusprofil;
		Ntousresidusprofil=origin.Ntousresidusprofil;
		qmulcht=origin.qmulcht;
		irmulch=origin.irmulch;
		drain_from_plt=origin.drain_from_plt;
		leaching_from_plt=origin.leaching_from_plt;
		runoff_from_plt=origin.runoff_from_plt;
		Nmineral_from_plt=origin.Nmineral_from_plt;
		Nvolat_from_plt=origin.Nvolat_from_plt;
		QNdenit_from_plt=origin.QNdenit_from_plt;
		SoilAvW=origin.SoilAvW;
		SoilWatM=origin.SoilWatM;
		SoilNM=origin.SoilNM;
		P_profmesW=origin.P_profmesW;
		P_profmesN=origin.P_profmesN;

		
		surfAO=origin.surfAO;
		surfAS=origin.surfAS;
		compt_calcul_taylor=origin.compt_calcul_taylor;
		n_datedeb_irrigauto=origin.n_datedeb_irrigauto;
		n_datefin_irrigauto=origin.n_datefin_irrigauto;
		code_ecrit_nom_usm=origin.code_ecrit_nom_usm;
		HR_vol_1_30=origin.HR_vol_1_30;
		HR_vol_31_60=origin.HR_vol_31_60;
		HR_vol_61_90=origin.HR_vol_61_90;
		HR_vol_91_120=origin.HR_vol_91_120;
		HR_vol_121_150=origin.HR_vol_121_150;
		HR_vol_151_180=origin.HR_vol_151_180;
		day_after_sowing=origin.day_after_sowing;
			
			
		N_mineralisation=origin.N_mineralisation;
		soilN=origin.soilN;
		tcult_tairveille=origin.tcult_tairveille;
		humidite_percent=origin.humidite_percent;
		flag_onacoupe=origin.flag_onacoupe;
		CsurNres_pature=origin.CsurNres_pature;
		qres_pature=origin.qres_pature;

		
		annee = new int[732];  	   
		napini = new int[2];  
		napNini = new int[2];  
		nbjresini = new int[2];  
		esz = new float[1000]; 
		dacouche = new float[1001];		   
		infilj = new float[6];
		exces = new float[6];
		anox = new float[1000];
		sat = new float[1000];		   
		hres = new float[21];
		Wb = new float[21];
		kres = new float[21];	
		Chum = new float[1000];
		Nhum = new float[1000];
		Cres = new float[21000];
		Nres = new float[21000];
		Cbio = new float[21000];
		Nbio = new float[21000];
		pfz = new float[1000];
		etz = new float[1000];
		tsol = new float[1001];
		tsolveille = new float[1000];
		HUR = new float[1000];
		hurmini = new float[1000];
		HUCC = new float[1000];
		HUMIN = new float[1000];
		tauxcouv = new float[367];
		absoTot = new float[5];
		cu0 = new float[2];
		somelong0 = new float[2];
		nfindorm0 = new int[2];
		repoussesemis = new int[2];  
		repousserecolte = new int[2];
		anit = new float[366];
		anit_engrais = new float[366];
		anit_uree = new float[366];
		type_ferti = new int[366];
		airg = new float[366];
		tmoy_an = new float[600];
		deltat_an = new float[300];
		tm_glisse = new float[200];
		deltaT_adaptCC = new float[200];
		var_trefh = new float[200];
		var_trefr = new float[200];
		var_tnitmin = new float[200];
		var_tnitmax = new float[200];
		var_tnitopt = new float[200];
		var_tnitopt2 = new float[200];
		var_TREFdenit1 = new float[200];
		var_TREFdenit2 = new float[200];
		var_TREFfhum = new float[200];
		var_FTEM = new float[200];
		var_FTEMr = new float[200];
		iplt_ori = new int[2];
		nbcoupe_reel = new int[2];
		nbcoupe_an1 = new int[2];
		julfauche_an1 = new int[40];
		lairesiduel_an1 = new float[40];
		hautcoupe_an1 = new float[40];
		msresiduel_an1 = new float[40];
		anitcoupe_an1 = new float[40];
		anitcoupe_an1 = new float[40];
		tempfauche_an1 = new float[40];
		tempfauche_ancours_an1 = new float[40];
		nbcoupe_an2 = new int[2];  
		julfauche_an2 = new int[40]; 
		lairesiduel_an2 = new float[40]; 
		hautcoupe_an2 = new float[40];  
		msresiduel_an2 = new float[40]; 
		anitcoupe_an2 = new float[40];  
		tempfauche_an2 = new float[40]; 
		tempfauche_ancours_an2 = new float[40];  
		restit_an1 = new int[40];
		mscoupemini_an1 = new float[40];
		restit_an2 = new int[40];
		mscoupemini_an2 = new float[40];		
		iplt = new int[2];
		beta_sol = new float[2];
		offrN = new float[1000];
		absz = new float[1000];
		trosemax = new float[367];
		numdateprof = new int[2];
		numdebprof = new int[2];
		dateprof = new int[1200];

		concNO3sol = new float[5];
		Cresdec = new float[11];
		Nresdec = new float[11];
		Cnondec = new float[10];
		Nnondec = new float[10];
		Cmuldec = new float[10];
		Nmuldec = new float[10];
		Cresiduprofil = new float[11];
		Nresiduprofil = new float[11];
		nitetcult = new int[367];
		
	   treeWaterUptake = new float[1000];
	   treeNitrogenUptake = new float[1000];
	
		    
		System.arraycopy(origin.annee 	, 0, this.annee 	, 0, 	732);
		System.arraycopy(origin.napini 	, 0, this.napini 	, 0, 	2);  
		System.arraycopy(origin.napNini 	, 0, this.napNini 	, 0, 	2);  
		System.arraycopy(origin.nbjresini 	, 0, this.nbjresini 	, 0, 	2);  
		System.arraycopy(origin.esz 	, 0, this.esz 	, 0, 	1000); 
		System.arraycopy(origin.dacouche 	, 0, this.dacouche 	, 0, 	1001);
		System.arraycopy(origin.infilj 	, 0, this.infilj 	, 0, 	6);
		System.arraycopy(origin.exces 	, 0, this.exces 	, 0, 	6);
		System.arraycopy(origin.anox 	, 0, this.anox 	, 0, 	1000);
		System.arraycopy(origin.sat 	, 0, this.sat 	, 0, 	1000);
		System.arraycopy(origin.hres 	, 0, this.hres 	, 0, 	21);
		System.arraycopy(origin.Wb 	, 0, this.Wb 	, 0, 	21);
		System.arraycopy(origin.kres 	, 0, this.kres 	, 0, 	21);
		System.arraycopy(origin.Chum 	, 0, this.Chum 	, 0, 	1000);
		System.arraycopy(origin.Nhum 	, 0, this.Nhum 	, 0, 	1000);
		System.arraycopy(origin.Cres 	, 0, this.Cres 	, 0, 	21000);
		System.arraycopy(origin.Nres 	, 0, this.Nres 	, 0, 	21000);
		System.arraycopy(origin.Cbio 	, 0, this.Cbio 	, 0, 	21000);
		System.arraycopy(origin.Nbio 	, 0, this.Nbio 	, 0, 	21000);
		System.arraycopy(origin.pfz 	, 0, this.pfz 	, 0, 	1000);
		System.arraycopy(origin.etz 	, 0, this.etz 	, 0, 	1000);
		System.arraycopy(origin.tsol 	, 0, this.tsol 	, 0, 	1001);
		System.arraycopy(origin.tsolveille 	, 0, this.tsolveille 	, 0, 	1000);
		System.arraycopy(origin.HUR 	, 0, this.HUR 	, 0, 	1000);
		System.arraycopy(origin.hurmini 	, 0, this.hurmini 	, 0, 	1000);
		System.arraycopy(origin.HUCC 	, 0, this.HUCC 	, 0, 	1000);
		System.arraycopy(origin.HUMIN 	, 0, this.HUMIN 	, 0, 	1000);
		System.arraycopy(origin.tauxcouv 	, 0, this.tauxcouv 	, 0, 	367);
		System.arraycopy(origin.absoTot 	, 0, this.absoTot 	, 0, 	5);
		System.arraycopy(origin.cu0 	, 0, this.cu0 	, 0, 	2);
		System.arraycopy(origin.somelong0 	, 0, this.somelong0 	, 0, 	2);
		System.arraycopy(origin.nfindorm0 	, 0, this.nfindorm0 	, 0, 	2);
		System.arraycopy(origin.repoussesemis 	, 0, this.repoussesemis 	, 0, 	2);
		System.arraycopy(origin.repousserecolte 	, 0, this.repousserecolte 	, 0, 	2);	
		System.arraycopy(origin.anit 	, 0, this.anit 	, 0, 	366);
		System.arraycopy(origin.anit_engrais 	, 0, this.anit_engrais 	, 0, 	366);
		System.arraycopy(origin.anit_uree 	, 0, this.anit_uree 	, 0, 	366);
		System.arraycopy(origin.type_ferti 	, 0, this.type_ferti 	, 0, 	366);
		System.arraycopy(origin.airg 	, 0, this.airg 	, 0, 	366);
		System.arraycopy(origin.tmoy_an 	, 0, this.tmoy_an 	, 0, 	600);
		System.arraycopy(origin.deltat_an 	, 0, this.deltat_an 	, 0, 	300);
		System.arraycopy(origin.tm_glisse 	, 0, this.tm_glisse 	, 0, 	200);
		System.arraycopy(origin.deltaT_adaptCC 	, 0, this.deltaT_adaptCC 	, 0, 	200);
		System.arraycopy(origin.var_trefh 	, 0, this.var_trefh 	, 0, 	200);
		System.arraycopy(origin.var_trefr 	, 0, this.var_trefr 	, 0, 	200);
		System.arraycopy(origin.var_tnitmin 	, 0, this.var_tnitmin 	, 0, 	200);
		System.arraycopy(origin.var_tnitmax 	, 0, this.var_tnitmax 	, 0, 	200);
		System.arraycopy(origin.var_tnitopt 	, 0, this.var_tnitopt 	, 0, 	200);
		System.arraycopy(origin.var_tnitopt2 	, 0, this.var_tnitopt2 	, 0, 	200);
		System.arraycopy(origin.var_TREFdenit1 	, 0, this.var_TREFdenit1 	, 0, 	200);
		System.arraycopy(origin.var_TREFdenit2 	, 0, this.var_TREFdenit2 	, 0, 	200);
		System.arraycopy(origin.var_TREFfhum 	, 0, this.var_TREFfhum 	, 0, 	200);
		System.arraycopy(origin.var_FTEM 	, 0, this.var_FTEM 	, 0, 	200);
		System.arraycopy(origin.var_FTEMr 	, 0, this.var_FTEMr 	, 0, 	200);
		System.arraycopy(origin.iplt_ori 	, 0, this.iplt_ori 	, 0, 	2);
		System.arraycopy(origin.nbcoupe_reel 	, 0, this.nbcoupe_reel 	, 0, 	2);
		System.arraycopy(origin.nbcoupe_an1 	, 0, this.nbcoupe_an1 	, 0, 	2);
		System.arraycopy(origin.julfauche_an1 	, 0, this.julfauche_an1 	, 0, 	40);
		System.arraycopy(origin.lairesiduel_an1 	, 0, this.lairesiduel_an1 	, 0, 	40);
		System.arraycopy(origin.hautcoupe_an1 	, 0, this.hautcoupe_an1 	, 0, 	40);
		System.arraycopy(origin.msresiduel_an1 	, 0, this.msresiduel_an1 	, 0, 	40);
		System.arraycopy(origin.anitcoupe_an1 	, 0, this.anitcoupe_an1 	, 0, 	40);
		System.arraycopy(origin.anitcoupe_an1 	, 0, this.anitcoupe_an1 	, 0, 	40);
		System.arraycopy(origin.tempfauche_an1 	, 0, this.tempfauche_an1 	, 0, 	40);
		System.arraycopy(origin.tempfauche_ancours_an1 	, 0, this.tempfauche_ancours_an1 	, 0, 	40);
		System.arraycopy(origin.nbcoupe_an2 	, 0, this.nbcoupe_an2 	, 0, 	2);  
		System.arraycopy(origin.julfauche_an2 	, 0, this.julfauche_an2 	, 0, 	40); 
		System.arraycopy(origin.lairesiduel_an2 	, 0, this.lairesiduel_an2 	, 0, 	40); 
		System.arraycopy(origin.hautcoupe_an2 	, 0, this.hautcoupe_an2 	, 0, 	40);  
		System.arraycopy(origin.msresiduel_an2 	, 0, this.msresiduel_an2 	, 0, 	40); 
		System.arraycopy(origin.anitcoupe_an2 	, 0, this.anitcoupe_an2 	, 0, 	40);  
		System.arraycopy(origin.tempfauche_an2 	, 0, this.tempfauche_an2 	, 0, 	40); 
		System.arraycopy(origin.tempfauche_ancours_an2 	, 0, this.tempfauche_ancours_an2 	, 0, 	40);  
		System.arraycopy(origin.restit_an1 	, 0, this.restit_an1 	, 0, 	40);
		System.arraycopy(origin.mscoupemini_an1 	, 0, this.mscoupemini_an1 	, 0, 	40);
		System.arraycopy(origin.restit_an2 	, 0, this.restit_an2 	, 0, 	40);
		System.arraycopy(origin.mscoupemini_an2 	, 0, this.mscoupemini_an2 	, 0, 	40);
		System.arraycopy(origin.iplt 	, 0, this.iplt 	, 0, 	2);
		System.arraycopy(origin.beta_sol 	, 0, this.beta_sol 	, 0, 	2);
		System.arraycopy(origin.offrN 	, 0, this.offrN 	, 0, 	1000);
		System.arraycopy(origin.absz 	, 0, this.absz 	, 0, 	1000);
		System.arraycopy(origin.trosemax 	, 0, this.trosemax 	, 0, 	367);
		System.arraycopy(origin.numdateprof 	, 0, this.numdateprof 	, 0, 	2);
		System.arraycopy(origin.numdebprof 	, 0, this.numdebprof 	, 0, 	2);
		System.arraycopy(origin.dateprof 	, 0, this.dateprof 	, 0, 	1200);

		System.arraycopy(origin.concNO3sol 	, 0, this.concNO3sol 	, 0, 	5);
		System.arraycopy(origin.Cresdec 	, 0, this.Cresdec 	, 0, 	11);
		System.arraycopy(origin.Nresdec 	, 0, this.Nresdec 	, 0, 	11);
		System.arraycopy(origin.Cnondec 	, 0, this.Cnondec 	, 0, 	10);
		System.arraycopy(origin.Nnondec 	, 0, this.Nnondec 	, 0, 	10);
		System.arraycopy(origin.Cmuldec 	, 0, this.Cmuldec 	, 0, 	10);
		System.arraycopy(origin.Nmuldec 	, 0, this.Nmuldec 	, 0, 	10);
		System.arraycopy(origin.Cresiduprofil 	, 0, this.Cresiduprofil 	, 0, 	11);
		System.arraycopy(origin.Nresiduprofil 	, 0, this.Nresiduprofil 	, 0, 	11);
		System.arraycopy(origin.nitetcult 	, 0, this.nitetcult 	, 0, 	367);
	
	}
	
	/**
	* Common crop variables intialisation for other simulation  
	* recopie de Lecture_DonneesCyclePrecedent dans STICS
	*/
	public void reinitialise (SafeSticsReinit origin) {
		
		
		this.tcultveille = origin.tcultveille;
		this.tcult = origin.tcultveille;
		this.tairveille = origin.tairveille;
		
		this.Chumt = origin.Chumt;
		this.Chuma = origin.Chuma;
		this.Chumi = origin.Chumi;
		this.Nhumt = origin.Nhumt;
		this.Nhuma = origin.Nhuma;
		this.Nhumi = origin.Nhumi;
		this.Cr = origin.Cr;
		this.Nr = origin.Nr;
		this.Cb = origin.Cb;
		this.Nb = origin.Nb;
		
		this.Cmulchnd = origin.Cmulchnd;
		this.Nmulchnd = origin.Nmulchnd;
		this.Cmulchdec = origin.Cmulchdec;
		this.Nmulchdec = origin.Nmulchdec;
		this.Cbmulch = origin.Cbmulch;
		this.Nbmulch = origin.Nbmulch;
	
		System.arraycopy(origin.kres 	, 0, this.kres 	, 0, 	21);
		System.arraycopy(origin.hres 	, 0, this.hres 	, 0, 	21);
		System.arraycopy(origin.Wb 	, 0, this.Wb 	, 0, 	21);
		
		System.arraycopy(origin.Cnondec 	, 0, this.Cnondec 	, 0, 	10);
		System.arraycopy(origin.Nnondec 	, 0, this.Nnondec 	, 0, 	10);

		System.arraycopy(origin.Chum 	, 0, this.Chum 	, 0, 	1000);
		System.arraycopy(origin.Nhum 	, 0, this.Nhum 	, 0, 	1000);
		System.arraycopy(origin.Cres 	, 0, this.Cres 	, 0, 	21000);
		System.arraycopy(origin.Nres 	, 0, this.Nres 	, 0, 	21000);
		System.arraycopy(origin.Cbio 	, 0, this.Cbio 	, 0, 	21000);
		System.arraycopy(origin.Nbio 	, 0, this.Nbio 	, 0, 	21000);
		
		System.arraycopy(origin.HUR 	, 0, this.HUR 	, 0, 	1000);
		System.arraycopy(origin.sat 	, 0, this.sat 	, 0, 	1000);
		System.arraycopy(origin.tsolveille 	, 0, this.tsolveille 	, 0, 	1000);
 
	    this.Nb0 = origin.Nb;
	    this.Cb0 = origin.Cb;
	    this.Nr0 = origin.Nr;
	    this.Cr0 = origin.Cr;
	    this.Cmulch0 = origin.Cmulchdec + origin.Cmulchnd;
	    this.Nmulch0 = origin.Nmulchdec + origin.Nmulchnd;
	    this.Cbmulch0 = origin.Cbmulch;
	    this.Nbmulch0 = origin.Nbmulch;   
	     
	}

	
	  @Override
	protected List getFieldOrder() {
		return Arrays.asList(new String[] { "flag_record", "ipl","P_nbplantes","jour","nummois",
				"P_culturean", "P_iwater", "P_ifwater",  "ifwater_courant", "ifwater0", "dernier_n", "n",
				"nbjmax", "jjul", "jul", "nbans", "nbjanrec", "nstoc", "numcult", "ens", "fichist", "ficsta", "ficdbg",
				"ficdbg2", "codoptim", "P_codesuite", "nbjsemis", "maxwth", "nstoprac", "numdate",
				"bouchon", "nouvdensrac", "nbCouchesSol", "nappmulch", "ires", "itrav", "ansemis", "anrecol", "annee",
				"NH", "codeprofil", "nbjrecol", "nbjrecol0", "NHE", "napini", "napNini", "nbjresini", "faucheannule",
				"delta", "devjourfr", "esz", "fdens", "tustress", "rdif", "originehaut", "tairveille", "coefbmsres",
				"coefaifres", "coefbifres", "coefcifres", "a", "effN", "hi", "ha", "hpf", "rglo", "hurlim", "rnetS",
				"rnet", "albedolai", "resmes", "dacouche", "ruisselt", "infilj", "exces", "anox", "pluieruissel", "sat",
				"cpreciptout", "ruissel", "QeauI", "QeauFS", "Qeau0", "doi", "Edirect", "humidite", "mouillmulch",
				"Emulch", "intermulch", "cintermulch", "ruisselsurf", "ras", "Nvolat", "eptcult", "TcultMin",
				"TcultMax", "dessecplt", "eo", "eos", "Ratm", "hres", "Wb", "kres", "NCbio", "saturation", "qmulch",
				"azomes", "ammomes", "FsNH3", "RsurRU", "DRAT", "QNdrp", "esol", "et", "tnhc", "tnrc", "pluieN",
				"irrigN", "precip", "precipN", "cumoffrN", "cumoffrN0", "cumoffrN100", "azorac0", "azorac100",
				"demandebrute", "absodrp", "cpluie", "Chumt", "Chumt0", "Nhuma", "Chuma", "Nhuma0", "Chuma0", "Nhumi",
				"Chumi", "Nhumt", "Nhumt0", "QCprimed", "QNprimed", "Cr", "Nr", "Cb", "Nb", "Nb0", "Nr0", "Cb0", "Cr0",
				"Cbmulch0", "Nbmulch0", "etm", "precipamm", "eaunoneffic", "toteaunoneffic", "raamax", "raamin",
				"laiTot", "stemflowTot", "EmdTot", "epTot", "hauteurMAX", "Chum", "Nhum", "Cres", "Nres", "Cbio",
				"Nbio", "xmlch1", "xmlch2", "supres", "stoc", "cestout", "pfz", "etz", "parapluieetz", "totapN",
				"Qminh", "Qminr", "QLES", "totapNres", "Qnitrif", "tcult", "tcultveille", "tsol", "tsolveille", "HUR",
				"hurmini", "HUCC", "HUMIN", "effamm", "tauxcouv", "azsup", "smes02", "sumes0", "sumes1", "sumes2",
				"sesj0", "ses2j0", "sum2", "esreste", "esreste2", "drain", "lessiv", "fxa", "fxn", "fxt", "fxw",
				"absoTot", "cu0", "somelong0", "nfindorm0", "vmax", "cumdltaremobilN", "cum_immob", "QCapp", "QNapp",
				"QCresorg", "QNresorg", "posibsw", "posibpe", "repoussesemis", "repousserecolte", "recolte1",
				"P_datefin", "P_codesimul", "AOAS", "AS", "AO", "nouvre2", "nouvre3", "naptot", "napNtot", "anit",
				"anit_engrais", "anit_uree", "type_ferti", "airg", "totir", "co2res", "co2hum", "CO2sol", "QCO2sol",
				"QCO2res", "QCO2hum", "QCO2mul", "tmoy_an", "Tm_histo", "deltat_an", "tm_glisse", "deltaT_adaptCC",
				"var_trefh", "var_trefr", "var_tnitmin", "var_tnitmax", "var_tnitopt", "var_tnitopt2", "var_TREFdenit1",
				"var_TREFdenit2", "var_TREFfhum", "var_FTEM", "var_FTEMr", "plante_ori", "iplt_ori", "Qem_N2O",
				"em_N2O", "Qem_N2Onit", "em_N2Onit", "Qem_N2Oden", "em_N2Oden", "nbcoupe_reel",
				"onafaitunecoupedelanneedavant", "nbcoupe_an1", "julfauche_an1", "lairesiduel_an1", "hautcoupe_an1",
				"msresiduel_an1", "anitcoupe_an1", "tempfauche_an1", "tempfauche_ancours_an1", "nbcoupe_an2",
				"julfauche_an2", "lairesiduel_an2", "hautcoupe_an2", "msresiduel_an2", "anitcoupe_an2",
				"tempfauche_an2", "tempfauche_ancours_an2", "restit_an1", "mscoupemini_an1", "restit_an2",
				"mscoupemini_an2", "irrigjN", "precipjN", "apport_mini_semis", "iplt", "iwater0", "ansemis0",
				"iwaterapres", "ifwaterapres", "nbjsemis0", "iwater_cultsuiv", "ifwater_cultsuiv", "beta_sol", "offrN",
				"absz", "nodn", "trosemax", "numdateprof", "numdebprof", "dateprof", "nbvarsortie", "nbvarrap",
				"ecritrap", "QH2Of",
				"codeaucun", "codeenteterap", "codeenteterap_agmip", "codetyperap", 
				"raplev", "rapamf", "raplax", "rapflo", "rapdrp", "raprec", "rapsen",
				"rapfin", "rapplt", "rapger", "rapdebdes", "rapdebdebour", "rapmat", "rapdebdorm", "rapfindorm",
				"rapdeb", "start_rap", "RU", "concNO3sol", "FTEMhb", "FTEMrb", "Cresdec", "Nresdec", "Cnondec",
				"Nnondec", "Cmuldec", "Nmuldec", "Cmulch", "Nmulch", "Cmulchnd", "Nmulchnd", "Cmulchdec", "Nmulchdec",
				"Cbmulch", "Nbmulch", "Cmulch0", "Nmulch0", "couvermulch", "Ctousresidusprofil", "Ntousresidusprofil",
				"Cresiduprofil", "Nresiduprofil", "qmulcht", "irmulch", "drain_from_plt", "leaching_from_plt",
				"runoff_from_plt", "Nmineral_from_plt", "Nvolat_from_plt", "QNdenit_from_plt", "SoilAvW", "SoilWatM",
				"SoilNM", "P_profmesW", "P_profmesN", "ECRITURE_HISTORIQUE", "ECRITURE_SORTIESJOUR",
				"ECRITURE_RAPPORTS", "ECRITURE_BILAN", "ECRITURE_PROFIL", "ECRITURE_DEBUG", "ECRITURE_ECRAN",
				"ECRITURE_AGMIP", "DEBUG", "DEVELOP", "CALAI", "BIOMAER", "SENESCEN", "FRUIT", "GRAIN", "EAUQUAL",
				"CROISSANCEFRONTRACINAIRE", "PROFILRACINAIRE", "DENSITEVRAIERACINAIRE", "CALPSIBASE", "REPARTIR",
				"IRRIG", "CALAPNENUPVT", "CALCULAUTOMATIQUEIRRIGATION", "APPORTSNPARPLUIEETIRRIGATION",
				"APPORTSNPARENGRAISMINERAUX", "APPORTSORGANIQUESETTRAVAILDUSOL", "RESIDUS", "ETATSURF", "MINERAL",
				"KETP", "SHUTWALL", "OFFRNODU", "BNPL", "LIXIV", "TRANSPI", "OFFREN", "ABSON", "MAJNSOL", "STRESSEAU",
				"STRESSN", "NGRAIN", "EXCESDEAU", "CALTCULT_SJ", "CALTCULT_SHUTWALL", "CALRNET_SHUTWALL", "TEMPSOL",
				"HUMCOUV_SJ", "HUMHEURE", "SOLNU", "DETASSEMENT", "TASSESEMISRECOLTE", "surfAO", "surfAS",
				"compt_calcul_taylor", "n_datedeb_irrigauto", "n_datefin_irrigauto", "code_ecrit_nom_usm",
				"HR_vol_1_30", "HR_vol_31_60", "HR_vol_61_90", "HR_vol_91_120", "HR_vol_121_150", "HR_vol_151_180",
				"day_after_sowing", "N_mineralisation", "soilN", "tcult_tairveille", "humidite_percent",
				"flag_onacoupe", "CsurNres_pature", "qres_pature", "nitetcult",
			    "treeWaterUptake","treeNitrogenUptake"
		});
	}
	    
	 
}

	
	
  

