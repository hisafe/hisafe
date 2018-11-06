package safe.stics;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import com.sun.jna.Structure;
import safe.model.*;

/**
 * SafeSticsItk - Mirror object of STICS FORTRAN ITK_ 
 * 
 * 
 * @author Isabelle Lecomte - December 2016
 */
public class SafeSticsItk extends Structure implements Serializable {

	public int ipl; // indice associé
	public int P_ressuite; // PARAMETER // Name of residue type (for the next crop) // straw, roots, crops, nothing // PARTEC
	public int P_stadecoupedf; // PARAMETER // Stage of automatic cut // PARTEC // 0
	public boolean lecfauche;
	public int P_codefauche; // PARAMETER // option of cut modes for forage crops: yes (1), no (2) // code 1/2 // PARTEC
	public int P_codeaumin; // PARAMETER // harvest as a function of grain/fruit// water content // code 1/2 // PARTEC // 0
	public int P_codetradtec; // PARAMETER // description of crop structure with// use of radiation transfers: yes (2), non(1)// // code1/2 // PARTEC // 0
	public int P_codlocirrig; // PARAMETER // code of irrigation localisation:// 1= above the foliage, 2= below the foliageabove the soil, 3 = in the soil // code 1/2/3PARTEC // 0
	public int P_codlocferti; // PARAMETER // code of fertilisation localisation: 1: at the soil surface, 2 = in the soil // code 1/2 // PARTEC // 0
	public int P_codemodfauche; // PARAMETER // option defining the cut mode:(1) automatic calculation depending on phenologic and trophic state crop, (2) calendar pre-established in days, (3) calendar pre-established in degrees.days // code 1/2/3 // PARTEC // 0
	public int P_codeffeuil; // PARAMETER // option technique of thinning: yes (1), no (2) // code 1/2 // PARTEC // 0
	public int P_codecalirrig; // PARAMETER // automatic calculation of irrigation requirements: yes (2), no (1) // code 1/2 // PARTEC // 1
	public int P_codestade; // PARAMETER // option of forcing for one or several development stages: yes (2), no (1) // code 1/2 PARTEC // 0
	public int P_codcalrogne; // PARAMETER // option of the way of calculation of tipping // code 1/2 // PARTEC // 0
	public int P_codepaillage; // PARAMETER // option: option: 1 = no cover, 2 = plastic cover partly covering the soil // code 1/2 // PARTEC // 0
	public int P_codeclaircie; // PARAMETER // option to activate techniques of fruit removal // code 1/2 // PARTEC // 0
	public int P_codcaleffeuil; // PARAMETER // option of the way of caluclation of leaf removal // code 1/2 // PARTEC // 0
	public int P_codrognage; // PARAMETER // option of foliage control // code 1/2 // PARTEC // 0
	public int P_codhauteff; // PARAMETER // leaf removal heitgh // code 1/2 // PARTEC // 0
	public int P_codetaille; // PARAMETER // option of pruning // code 1/2 // PARTEC // 0
	public int P_codrecolte; // PARAMETER // harvest mode : all the plant (1) or just the fruits (2) // code 1/2 // PARTEC // 0
	public int P_codcueille; // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0
	public int P_iplt0; // PARAMETER // Date of sowing // julian day // PARTEC
	public int P_ilev; // PARAMETER // Day of the stage LEV (emergence) when the stage is observed (else 999) // julian day // PARTEC
	public int P_iamf; // PARAMETER // Day of the stage AMF (maximal of leaf growth , end of juvenile phase ) when the stage isobserved (else 999) // * // PARTEC // 1
	public int P_ilax; // PARAMETER // Day of the stage LAX(maximal leaf areaindex) when the stage is observed (else 999) // julian day // PARTEC // 1
	public int P_idrp; // PARAMETER // Day of the stage DRP (beginning of grain filling) when the stage is observed (else 999) // julian day // PARTEC // 1
	public int P_isen; // PARAMETER // Day of the stage SEN (beginning of net senescence) when the stage is observed (else 999) // julian day // PARTEC // 1
	public int P_imat; // PARAMETER // Day of the stage MAT (physiological maturity) when the stage is observed (else 999) // julian day // PARTEC // 1
	public int P_irec; // PARAMETER // Day of the stage REC (harvest) when the stage is observed (else 999) // julian day // PARTEC
	public int P_irecbutoir; // PARAMETER // Date of harvest butoir (if the crop has not finished his cycle at this date, the harvest is imposed) // julian day // PARTEC
	public int P_variete; // PARAMETER // variety number in the technical file SD // PARTEC // 1
	public int nap;
	public int napN;
	public int napN_uree;
	public int napN_engrais;
	public int P_nbjres; // PARAMETER // number of residue addition // days // PARTEC // 1
	public int P_nbjtrav; // PARAMETER // number of cultivations oprerations // days // PARTEC // 1
	public int nbcoupe;
	public int P_julfauche[]; // PARAMETER // Day in year of each cut (table) // julian day // PARTEC // 1
	public int P_nbcueille; // PARAMETER // number of fruit harvestings // code 1/2 // PARTEC // 0
	public int P_ilan; // PARAMETER // Day of the stage LAN () if the stage is observed (else 999) // julian day // PARTEC // 1
	public int P_iflo; // PARAMETER // flowering day // julian day // PARTEC //
	public int P_locirrig; // PARAMETER // Depth of water apply (when irrigation is applied in depth of soil) // cm // PARTEC // 1
	public int P_engrais[]; // PARAMETER // fertilizer type : 1 =Nitrate.of ammonium ,2=Solution,3=urea,4=Anhydrous ammoniac,5= Sulfate of ammonium,6=phosphate of ammonium,7=Nitrateof calcium,8= fixed efficiency * // PARTEC // 1
	public int P_locferti; // PARAMETER // Depth of nitrogen apply (when fertiliser is applied in depth of soil) // cm // PARTEC // 1
	public int P_cadencerec; // PARAMETER // number of days between two harvests day // PARTEC // 1
	public int P_julrogne; // PARAMETER // day of plant shapening // julian day PARTEC // 1
	public int P_juleclair[]; // PARAMETER // julian day of fruits removal // julian day // PARTEC // 1
	public int P_juleffeuil; // PARAMETER // julian day of leaf removal // julian day // PARTEC // 1
	public int P_jultaille; // PARAMETER // pruning day // julian day // PARTEC
	public int P_jultrav[]; // PARAMETER // Day in year of the soil cultivation operations and/or apply of organic residues // julian day // PARTEC // 1
	public int P_julres[]; // PARAMETER // Day in year of the residue apply // julian day // PARTEC // 1
	public int P_coderes[]; // PARAMETER // residue type: 1=crop residues, 2=residues of CI, 3=manure, 4=compost OM, 5=mud SE, 6=vinasse, 7=corn, 8=other // code 1 to 10 // PARTEC // 0
	public float P_profsem; // PARAMETER // Sowing depth // cm // PARTEC // 1
	public float P_ratiol; // PARAMETER // Water stress index below which we start an irrigation in automatic mode (0 in manual mode) // between 0 and 1 // PARTEC // 1
	public float P_dosimx; // PARAMETER // maximum water amount of irrigation authorised at each time step (mode automatic irrigation) // mm day-1 // PARTEC // 1
	public float P_doseirrigmin; // DR le 12/09/07 pour benjamin // PARAMETER // minimal amount of irrigation // mm // PARTEC // 1
	public float P_profmes; // PARAMETER // measure depth of soil water reserve cm // PARTEC // 1
	public float P_densitesem; // PARAMETER // Sowing density // plants.m-2 // PARTEC // 1
	public float P_concirr; // PARAMETER // Nitrogen concentration of water // kgN mm-1 // PARTEC // 1
	public float P_effirr; // PARAMETER // irrigation efficiency // SD // PARTEC
	public float P_lairesiduel[]; // PARAMETER // Residual leaf index after each cut (table) // m2 leaf m-2 soil // PARTEC
	public float P_hautcoupedefaut; // PARAMETER // Cut height for forage crops (calendar calculated) // m // PARTEC // 1
	public float P_hautcoupe[]; // PARAMETER // Cut height for forage crops (calendar fixed) // m // PARTEC // 1
	public float P_msresiduel[]; // PARAMETER // Residual dry matter after a cut // t ha-1 // PARTEC // 1
	public float P_anitcoupe[]; // PARAMETER // amount of mineral fertilizer applications at each cut (forage crop) // kg N ha-1 // PARTEC // 1
	public float P_tempfauche[]; // PARAMETER // Sum of development units between 2 cuts // degree.days // PARTEC
	public int P_restit[]; // PARAMETER // option of restitution in case of pasture yes (1), no (2) // 1-2 // PARTEC // 1
	public float P_mscoupemini[]; // PARAMETER // Minimum threshold value of dry matter to make a cut // t ha-1 // PARTEC
	public float P_largrogne; // PARAMETER // width of shapening // m // PARTEC
	public float P_margerogne; // PARAMETER // allowed quantity of biomass inbetween two shapenings when asking automatic shapening // t ha-1 // PARTEC // 1
	public float P_hautrogne; // PARAMETER // cutting height // m // PARTEC // 1
	public float P_effeuil; // PARAMETER // proportion of daily leaf removed at thinning // 0-1 // PARTEC // 1
	public float P_interrang; // PARAMETER // Width of the P_interrang // m // PARTEC // 1
	public float P_orientrang; // PARAMETER // Direction of ranks // rd (0=NS) PARTEC // 1
	public float P_Crespc[]; // PARAMETER // carbon proportion in organic residue // // PARTEC // 1
	public float P_proftrav[]; // PARAMETER // Depth of residue incorporation (max. 40 cm) // cm // PARTEC // 1
	public float P_profres[]; // PARAMETER // minimal depth of organic residue incorporation // cm // PARTEC // 1
	public float P_CsurNres[]; // PARAMETER // C/N ratio of residues // g g-1 // PARTEC // 1
	public float P_Nminres[]; // PARAMETER // N mineral content of organic residues // % fresh matter // PARTEC // 1
	public float P_eaures[]; // PARAMETER // Water amount of organic residues // % fresh matter // PARTEC // 1
	public float P_qres[]; // PARAMETER // amount of crop residue or organic amendments applied to the soil (fresh weight) // t MF ha-1 // PARTEC // 1
	public int P_julapI[]; // PARAMETER // dates in julian days of irrigation // julian day // PARTEC // 1
	public int P_julapN[]; // PARAMETER // dates in julian days of fertilizer application // julian day // PARTEC // 1
	public float P_doseI[]; // PARAMETER // irrigation amount // mm.jour-1 // PARTEC // 1
	public float P_doseN[]; // PARAMETER // fertilizer amount // kgN.jour-1 // PARTEC // 1
	public int P_upvttapI[]; // PARAMETER // thermal time from emergence (UPVT units) driving irrigation // degree C // PARTEC // 1
	public int P_upvttapN[]; // PARAMETER // thermal time from emergence (UPVT units) driving fertilization // degree C // PARTEC // 1
	public float P_fracN[]; // PARAMETER // percentage of P_Qtot_N applied // % PARTEC // 1
	public float P_h2ograinmin; // PARAMETER // minimal water content allowed at harvest // g eau g-1 MF // PARTEC // 1
	public float P_h2ograinmax; // PARAMETER // maximal water content allowed at harvest // g water g-1 MF // PARTEC // 1
	public float P_CNgrainrec; // PARAMETER // minimal grain nitrogen content for harvest // 0-1 // PARTEC // 1
	public float P_huilerec; // PARAMETER // minimal oil content allowed for harvest // g huile g-1 MF // PARTEC // 1
	public float P_sucrerec; // PARAMETER // minimal sugar rate at harvest // g sucre g-1 MF // PARTEC // 1
	public float P_nbinfloecl[]; // PARAMETER // "number of inflorescences or fruits removed at fruit removal // nb pl-1 // PARTEC // 1
	public float P_laidebeff; // PARAMETER // LAI of the beginning of leaf removal // m2 m-2 // PARTEC // 1
	public float P_laieffeuil; // PARAMETER // LAI of the end of leaf removal // m2 m-2 // PARTEC // 1
	public float P_biorognem; // PARAMETER // minimal biomass to be removed when tipping (automatic calculation) // t ha-1 // PARTEC // 1
	public int P_nb_eclair; // PARAMETER // removal number // 1-10 // PARTEC //
	public int P_codeDSTtass; // PARAMETER // activation of compaction at sowing and harvest : yes (1), no (2) // code1/2 // PARTEC // 0
	public int P_codedecisemis; // PARAMETER // activation of moisture effect on the decision to harvest // code 1/2 // PARTEC
	public int P_codedecirecolte; // PARAMETER // activation of moisture and frost effects on the decision to harvest code 1/2 // PARTEC // 0
	public int P_nbjmaxapressemis; // PARAMETER // maximal delay (number of days) of sowing date in case of soil compaction activation // jours // PARPLT
	public int P_nbjseuiltempref; // PARAMETER // number of days for which we check if there is no frost sowing conditions when decision to sow is activated // jours // PARTEC // 1
	public int P_nbjmaxapresrecolte; // PARAMETER // maximal delay (number of days) of harvest date in case of soil compaction activation // jours // PARTEC // 1
	public int P_codeDSTnbcouche; // PARAMETER // activation of the number of compacted soil layers: one layer (1), two layers (2) // code 1/2 // PARTEC // 0
	public float P_profhumsemoir; // PARAMETER // soil depth for which moisture is considered as a reference to allow or not sowing in the case of soil compaction activation // cm // PARTEC // 1
	public float P_profhumrecolteuse; // PARAMETER // soil depth for which moisture is considered as a reference to allow or not harvesting in the case of soil compaction activation // cm // PARTEC // 1
	public float P_dasemis; // PARAMETER // bulk density modification as a result of sowing // g.cm-3 // PARTEC // 1
	public float P_darecolte; // PARAMETER // bulk density modification as a result of harvest // g.cm-3 // PARSOL // 1
	
	public int P_codefracappN; // PARAMETER // option of cut modes for forage crops: yes (1), no (2) // code 1/2 // PARTEC
	public int P_Qtot_N; // PARAMETER // amount of mineral fertilizer applications // kg N ha-1 // PARTEC // 1
	public int P_codeDST; // PARAMETER // activation of modifications of physical soil deep conditions through cropping management : yes (1), no (2) // code 1/2 // PARTEC // 0
	public float P_dachisel; // PARAMETER // bulk density modification as a result of soil management (Chisel) // g.cm-3 PARTEC // 1
	public float P_dalabour; // PARAMETER // bulk density modification as a result of soil management (Plough) // g.cm-3 PARTEC // 1
	public float P_rugochisel; // PARAMETER // bare soil rugosity lenght (P_z0solnu) for a chisel when soil compaction is activated // m // PARTEC // 1
	public float P_rugolabour; // PARAMETER // bare soil rugosity lenght (P_z0solnu) for a plough when soil compaction is activated // m // PARPLT // 1
	public int P_codabri; // PARAMETER // option of calculation of the climate under a shelter // code 1/2 // PARTEC // 0
	public int P_julouvre2; // PARAMETER // day of opening of the shelter // julian day // PARTEC // 1
	public int P_julouvre3; // PARAMETER // day of opening of the shelter // julian day // PARTEC // 1
	public float P_transplastic; // PARAMETER // translission coefficient of the shelter plastic // SD // PARTEC // 1
	public float P_surfouvre1; // PARAMETER // surface proportion of the shelter opened the first day of opening // SD // PARTEC // 1
	public float P_surfouvre2; // PARAMETER // surface proportion of the shelter opened the second day of opening // SD // PARTEC // 1
	public float P_surfouvre3; // PARAMETER // surface proportion of the shelter opened the third day of opening // SD // PARTEC // 1
	public float P_couvermulchplastique; // PARAMETER // proportion of soil covered by the cover // SD // PARTEC // 1
	public float P_albedomulchplastique; // PARAMETER // P_albedo of plastic cover // SD // PARTEC // 1
	public float P_hautmaxtec; // PARAMETER // maximal height of the plant allowed by the management // m // PARTEC // 1
	public float P_largtec; // PARAMETER // technical width // m // PARTEC // 1
	public int P_codepalissage; // PARAMETER // option: no (1), yes2) // code 1/2 // PARTEC // 0
	public int P_coderecolteassoc; // PARAMETER // option to harvest intercrop species simultaneously, at the physiological maturity date of the earliest one: yes(1), no (2) // code 1/2/ PARTEC // 0
	public int P_codedateappH2O; // PARAMETER // irrigation application dates given as sum of temperatures / yes (1), no (2) // code 1/2 // PARTEC // 0
	public int numeroappI;
	public int numeroappN;
	public int P_codedateappN; // PARAMETER // mineral fertilizer application dates given as sum of temperatures / yes (1), no (2) // code 1/2 // PARTEC // 0
	public float mscoupemini_courant; // variable // Minimum threshold value of dry matter to make a cut // t ha-1 //
	public boolean flag_eclairmult; // variable // flag for activation or not of the option several thinning // 1-2 //
	public boolean flag_pature; // variable // flag for activation or not of pasture and restitution// 1-2 //
	public boolean flag_plusieurs_engrais; // variable // flag for activation or not of several fertilisations type// 1-2 //

	/**
	* Constructor 
	*/
	public SafeSticsItk () {

		P_julfauche = new int[20];
		P_engrais = new int[10];
		P_juleclair = new int[10];
		P_jultrav = new int[11];
		P_julres = new int[11];
		P_coderes = new int[11];
		P_lairesiduel = new float[21];
		P_hautcoupe = new float[21];
		P_msresiduel = new float[22];
		P_anitcoupe = new float[21];
		P_tempfauche = new float[21];
		P_restit = new int[21];
		P_mscoupemini = new float[21];
		P_Crespc = new float[11];
		P_proftrav = new float[11];
		P_profres = new float[11];
		P_CsurNres = new float[11];
		P_Nminres = new float[11];
		P_eaures = new float[11];
		P_qres = new float[11];
		P_nbinfloecl = new float[10];
		P_julapN = new int[100];
		P_julapI = new int[100];
		P_doseI = new float[100];
		P_doseN = new float[100];
		P_upvttapN = new int[100];
		P_upvttapI = new int[100];		
		P_fracN = new float[100];

	}

	/**
	* Constructor for cloning 
	*/
	public SafeSticsItk (SafeSticsItk origin) {

		ipl=origin.ipl;
		P_ressuite=origin.P_ressuite;
		P_stadecoupedf=origin.P_stadecoupedf;
		lecfauche=origin.lecfauche;
		P_codefauche=origin.P_codefauche;
		P_codeaumin=origin.P_codeaumin;
		P_codetradtec=origin.P_codetradtec;
		P_codlocirrig=origin.P_codlocirrig;
		P_codlocferti=origin.P_codlocferti;
		P_codemodfauche=origin.P_codemodfauche;
		P_codeffeuil=origin.P_codeffeuil;
		P_codecalirrig=origin.P_codecalirrig;
		P_codestade=origin.P_codestade;
		P_codcalrogne=origin.P_codcalrogne;
		P_codepaillage=origin.P_codepaillage;
		P_codeclaircie=origin.P_codeclaircie;
		P_codcaleffeuil=origin.P_codcaleffeuil;
		P_codrognage=origin.P_codrognage;
		P_codhauteff=origin.P_codhauteff;
		P_codetaille=origin.P_codetaille;
		P_codrecolte=origin.P_codrecolte;
		P_codcueille=origin.P_codcueille;
		P_iplt0=origin.P_iplt0;
		P_ilev=origin.P_ilev;
		P_iamf=origin.P_iamf;
		P_ilax=origin.P_ilax;
		P_idrp=origin.P_idrp;
		P_isen=origin.P_isen;
		P_imat=origin.P_imat;
		P_irec=origin.P_irec;
		P_irecbutoir=origin.P_irecbutoir;
		P_variete=origin.P_variete;
		nap=origin.nap;
		napN=origin.napN;
		napN_uree=origin.napN_uree;
		napN_engrais=origin.napN_engrais;
		P_nbjres=origin.P_nbjres;
		P_nbjtrav=origin.P_nbjtrav;
		nbcoupe=origin.nbcoupe;
		P_nbcueille=origin.P_nbcueille;
		P_ilan=origin.P_ilan;
		P_iflo=origin.P_iflo;
		P_locirrig=origin.P_locirrig;
		P_locferti=origin.P_locferti;
		P_cadencerec=origin.P_cadencerec;
		P_julrogne=origin.P_julrogne;
		P_juleffeuil=origin.P_juleffeuil;
		P_jultaille=origin.P_jultaille;
		P_profsem=origin.P_profsem;
		P_ratiol=origin.P_ratiol;
		P_dosimx=origin.P_dosimx;
		P_doseirrigmin=origin.P_doseirrigmin;
		P_profmes=origin.P_profmes;
		P_densitesem=origin.P_densitesem;
		P_concirr=origin.P_concirr;
		P_effirr=origin.P_effirr;
		P_hautcoupedefaut=origin.P_hautcoupedefaut;
		P_largrogne=origin.P_largrogne;
		P_margerogne=origin.P_margerogne;
		P_hautrogne=origin.P_hautrogne;
		P_effeuil=origin.P_effeuil;
		P_interrang=origin.P_interrang;
		P_orientrang=origin.P_orientrang;
		P_h2ograinmin=origin.P_h2ograinmin;
		P_h2ograinmax=origin.P_h2ograinmax;
		P_CNgrainrec=origin.P_CNgrainrec;
		P_huilerec=origin.P_huilerec;
		P_sucrerec=origin.P_sucrerec;
		P_laidebeff=origin.P_laidebeff;
		P_laieffeuil=origin.P_laieffeuil;
		P_biorognem=origin.P_biorognem;
		P_nb_eclair=origin.P_nb_eclair;
		P_codeDSTtass=origin.P_codeDSTtass;
		P_codedecisemis=origin.P_codedecisemis;
		P_codedecirecolte=origin.P_codedecirecolte;
		P_nbjmaxapressemis=origin.P_nbjmaxapressemis;
		P_nbjseuiltempref=origin.P_nbjseuiltempref;
		P_nbjmaxapresrecolte=origin.P_nbjmaxapresrecolte;
		P_codeDSTnbcouche=origin.P_codeDSTnbcouche;
		P_profhumsemoir=origin.P_profhumsemoir;
		P_profhumrecolteuse=origin.P_profhumrecolteuse;
		P_dasemis=origin.P_dasemis;
		P_darecolte=origin.P_darecolte;
		P_codefracappN=origin.P_codefracappN;
		P_Qtot_N=origin.P_Qtot_N;
		P_codeDST=origin.P_codeDST;
		P_dachisel=origin.P_dachisel;
		P_dalabour=origin.P_dalabour;
		P_rugochisel=origin.P_rugochisel;
		P_rugolabour=origin.P_rugolabour;
		P_codabri=origin.P_codabri;
		P_julouvre2=origin.P_julouvre2;
		P_julouvre3=origin.P_julouvre3;
		P_transplastic=origin.P_transplastic;
		P_surfouvre1=origin.P_surfouvre1;
		P_surfouvre2=origin.P_surfouvre2;
		P_surfouvre3=origin.P_surfouvre3;
		P_couvermulchplastique=origin.P_couvermulchplastique;
		P_albedomulchplastique=origin.P_albedomulchplastique;
		P_hautmaxtec=origin.P_hautmaxtec;
		P_largtec=origin.P_largtec;
		P_codepalissage=origin.P_codepalissage;
		P_coderecolteassoc=origin.P_coderecolteassoc;
		P_codedateappH2O=origin.P_codedateappH2O;
		numeroappI=origin.numeroappI;
		numeroappN=origin.numeroappN;
		P_codedateappN=origin.P_codedateappN;
		mscoupemini_courant=origin.mscoupemini_courant;
		flag_eclairmult=origin.flag_eclairmult;
		flag_pature=origin.flag_pature;
		flag_plusieurs_engrais=origin.flag_plusieurs_engrais;
		
		P_julfauche = new int[20];
		P_engrais = new int[10];
		P_juleclair = new int[10];
		P_jultrav = new int[11];
		P_julres = new int[11];
		P_coderes = new int[11];
		P_lairesiduel = new float[21];
		P_hautcoupe = new float[21];
		P_msresiduel = new float[22];
		P_anitcoupe = new float[21];
		P_tempfauche = new float[21];
		P_restit = new int[21];
		P_mscoupemini = new float[21];
		P_Crespc = new float[11];
		P_proftrav = new float[11];
		P_profres = new float[11];
		P_CsurNres = new float[11];
		P_Nminres = new float[11];
		P_eaures = new float[11];
		P_qres = new float[11];
		P_nbinfloecl = new float[10];

		P_julapN = new int[100];
		P_julapI = new int[100];
		P_doseI = new float[100];
		P_doseN = new float[100];
		P_upvttapN = new int[100];
		P_upvttapI = new int[100];		
		P_fracN = new float[100];		
		
		System.arraycopy(origin.P_julfauche, 0, this.P_julfauche, 0, 20);
		System.arraycopy(origin.P_engrais, 0, this.P_engrais, 0, 10);
		System.arraycopy(origin.P_juleclair, 0, this.P_juleclair, 0, 10);
		System.arraycopy(origin.P_jultrav, 0, this.P_jultrav, 0, 11);
		System.arraycopy(origin.P_julres, 0, this.P_julres, 0, 11);
		System.arraycopy(origin.P_coderes, 0, this.P_coderes, 0, 11);

		
		System.arraycopy(origin.P_lairesiduel, 0, this.P_lairesiduel, 0, 21);
		System.arraycopy(origin.P_hautcoupe, 0, this.P_hautcoupe, 0, 21);
		System.arraycopy(origin.P_msresiduel, 0, this.P_msresiduel, 0, 22);
		System.arraycopy(origin.P_tempfauche, 0, this.P_tempfauche, 0, 21);
		System.arraycopy(origin.P_restit, 0, this.P_restit, 0, 21);
		System.arraycopy(origin.P_mscoupemini, 0, this.P_mscoupemini, 0, 21);
		System.arraycopy(origin.P_Crespc, 0, this.P_Crespc, 0, 11);
		System.arraycopy(origin.P_proftrav, 0, this.P_proftrav, 0, 11);

		System.arraycopy(origin.P_proftrav, 0, this.P_proftrav, 0, 11);
		System.arraycopy(origin.P_profres, 0, this.P_profres, 0, 11);
		System.arraycopy(origin.P_CsurNres, 0, this.P_CsurNres, 0, 11);
		System.arraycopy(origin.P_Nminres, 0, this.P_Nminres, 0, 11);
		System.arraycopy(origin.P_eaures, 0, this.P_eaures, 0, 11);
		System.arraycopy(origin.P_qres, 0, this.P_qres, 0, 11);
		System.arraycopy(origin.P_nbinfloecl, 0, this.P_nbinfloecl, 0, 10);
	
		System.arraycopy(origin.P_julapN, 0, this.P_julapN, 0, 100);
		System.arraycopy(origin.P_julapI, 0, this.P_julapI, 0, 100);
		System.arraycopy(origin.P_doseI, 0, this.P_doseI, 0, 100);
		System.arraycopy(origin.P_doseN, 0, this.P_doseN, 0, 100);
		System.arraycopy(origin.P_upvttapN, 0, this.P_upvttapN, 0, 100);
		System.arraycopy(origin.P_upvttapI, 0, this.P_upvttapI, 0, 100);
		System.arraycopy(origin.P_fracN, 0, this.P_fracN, 0, 100);
		

	}
	@Override
	protected List getFieldOrder() {
		return Arrays.asList(new String[] { "ipl", "P_ressuite", "P_stadecoupedf", "lecfauche", "P_codefauche",
				"P_codeaumin", "P_codetradtec", "P_codlocirrig", "P_codlocferti", "P_codemodfauche", "P_codeffeuil",
				"P_codecalirrig", "P_codestade", "P_codcalrogne", "P_codepaillage", "P_codeclaircie", "P_codcaleffeuil",
				"P_codrognage", "P_codhauteff", "P_codetaille", "P_codrecolte", "P_codcueille", "P_iplt0", "P_ilev",
				"P_iamf", "P_ilax", "P_idrp", "P_isen", "P_imat", "P_irec", "P_irecbutoir", "P_variete", "nap", "napN",
				"napN_uree", "napN_engrais", "P_nbjres", "P_nbjtrav", "nbcoupe", "P_julfauche", "P_nbcueille", "P_ilan",
				"P_iflo", "P_locirrig", "P_engrais", "P_locferti", "P_cadencerec", "P_julrogne", "P_juleclair",
				"P_juleffeuil", "P_jultaille", "P_jultrav", "P_julres", "P_coderes", 
				"P_profsem", "P_ratiol", "P_dosimx", "P_doseirrigmin", "P_profmes",
				"P_densitesem", "P_concirr", "P_effirr", "P_lairesiduel", "P_hautcoupedefaut", "P_hautcoupe",
				"P_msresiduel", "P_anitcoupe", "P_tempfauche", "P_restit", "P_mscoupemini", "P_largrogne",
				"P_margerogne", "P_hautrogne", "P_effeuil", "P_interrang", "P_orientrang", "P_Crespc", "P_proftrav",
				"P_profres", "P_CsurNres", "P_Nminres", "P_eaures", "P_qres", 
				"P_julapI","P_julapN","P_doseI", "P_doseN", 
				"P_upvttapI","P_upvttapN", "P_fracN",
				"P_h2ograinmin",
				"P_h2ograinmax", "P_CNgrainrec", "P_huilerec", "P_sucrerec", "P_nbinfloecl", "P_laidebeff",
				"P_laieffeuil", "P_biorognem", "P_nb_eclair", "P_codeDSTtass", "P_codedecisemis", "P_codedecirecolte",
				"P_nbjmaxapressemis", "P_nbjseuiltempref", "P_nbjmaxapresrecolte", "P_codeDSTnbcouche",
				"P_profhumsemoir", "P_profhumrecolteuse", "P_dasemis", "P_darecolte",  "P_codefracappN",
				"P_Qtot_N", "P_codeDST", "P_dachisel", "P_dalabour", "P_rugochisel", "P_rugolabour", "P_codabri",
				"P_julouvre2", "P_julouvre3", "P_transplastic", "P_surfouvre1", "P_surfouvre2", "P_surfouvre3",
				"P_couvermulchplastique", "P_albedomulchplastique", "P_hautmaxtec", "P_largtec", "P_codepalissage",
				"P_coderecolteassoc", "P_codedateappH2O", "numeroappI", "numeroappN", "P_codedateappN",
				"mscoupemini_courant", "flag_eclairmult", "flag_pature", "flag_plusieurs_engrais" });
	}

}
