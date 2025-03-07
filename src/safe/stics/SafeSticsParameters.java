/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree�Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE 
 * 
 * Authors  
 * C.DUPRAZ       	- INRAE Montpellier France
 * M.GOSME       	- INRAE Montpellier France
 * G.TALBOT       	- INRAE Montpellier France
 * B.COURBAUD      	- INRAE Montpellier France
 * H.SINOQUET		- INRAE Montpellier France
 * N.DONES			- INRAE Montpellier France
 * N.BARBAULT 		- INRAE Montpellier France 
 * I.LECOMTE       	- INRAE Montpellier France
 * M.Van NOORDWIJK  - ICRAF Bogor Indonisia 
 * R.MULIA       	- ICRAF Bogor Indonisia
 * D.HARJA			- ICRAF Bogor Indonisia
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software under the terms of the CC-BY License as published by the Creative Commons Corporation
 *
 * You are free to:
 *		Share � copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt � remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution � 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions � You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
 *               
 * Notices:
 * 		You do not have to comply with the license for elements of the material in the public domain or where your use is permitted 
 *      by an applicable exception or limitation .
 *		No warranties are given. The license may not give you all of the permissions necessary for your intended use. 
 *		For example, other rights such as publicity, privacy, or moral rights may limit how you use the material.  
 *
 * For more details see <https://creativecommons.org/licenses/by/4.0/>.
 *
 */

package safe.stics;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import com.sun.jna.Structure;

/**
 * SafeSticsParameters - JNA mirror object for STICS general parameters
 *                       This object have to be the exact mirror of the FORTRAN Parametres_Generaux.f90 
 * 
 * @author Isabelle Lecomte - December 2016
 */
public class SafeSticsParameters extends Structure implements Serializable {


  private static final long serialVersionUID = 1L;
  
  public int P_codeh2oact;   // PARAMETER // code to activate  water stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
  public int P_codeinnact;   // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
  public int P_codhnappe;   // PARAMETER // mode of calculation of watertable level // code 1/2 // PARAM // 0
  public int P_codeminopt;   // PARAMETER // option to maintain a constant water content in bare soil rainfall and PET ni): yes (1), no (2)  // code 1/2 // PARAM // 0
  public int P_codeprofmes;   // PARAMETER // option of depth of calculation for water and nitrogen stocks (1=P_profmes or 2=soil depth) // code 1/2 // PARAM // 0
  public int P_codeactimulch;   // PARAMETER // activation of the accounting for natural mulching in the partitioning of soil evaporation within the soil profile: yes (1), no (2) // code 1/2 // PARAM // 0
  public int codeulaivernal;  
  public int P_codetycailloux;   // PARAMETER // stones type code // code 1 to 10 // PARAM // 0
  public int P_codetypeng;   // PARAMETER // fertiliser type code // code 1 to 8 // PARAM // 0
  public int P_codetypres;   // PARAMETER // organic residue tyope code // code 1 to 8 // PARAM // 0
  public int P_iniprofil;   // PARAMETER // Option of smoothing out (function spline) the initial nitrogen and water profile: yes (1), no (0) // code 0/1 // PARAM // 0
  public float P_beta;   // PARAMETER // parameter of increase of maximal transpiration when occurs a water stress // SD // PARAM // 1
  public float P_lvopt;   // PARAMETER // Optimum root density // cm root.cm-3 soil // PARAM // 1
  public float P_rayon;   // PARAMETER // Average radius of roots // cm  // PARAM // 1
  public float P_fmin1;   // PARAMETER // Parameter of the constant of mineralization potential rate: K2=P_fmin1/(P_fmin2+P_argi)/(P_fmin3+P_calc) // 10000.day-1 // PARAM // 1
  public float P_fmin2;   // PARAMETER // Parameter of the constant of mineralization potential rate: K2=P_fmin1/(P_fmin2+P_argi)/(P_fmin3+P_calc) // % // PARAM // 1
  public float P_fmin3;   // PARAMETER // Parameter of the constant of mineralization potential rate: K2=P_fmin1/(P_fmin2+P_argi)/(P_fmin3+P_calc) // % // PARAM // 1
  public float P_Wh;   // PARAMETER // ratio N/C of humus // g g–1 // PARAM // 1
  public float P_concrr;   // PARAMETER // inorganic N concentration (NH4+NO3-N) in the rain // kgN ha-1 mm-1 // PARAM // 1
  public float P_TREFh;   // PARAMETER // temperature of reference for the soil mineralization parameters  // °C // PARAM // 1
  public float P_difN ;  // PARAMETER // coefficient de diffusion apparente du nitrate dans le sol humide // cm2 jour-1 // PARAM // 1
  public float P_plNmin;   // PARAMETER // Minimal amount of precipitation to manage a fertilization // mm day-1 // PARAM // 1
  public float P_proprac;   // PARAMETER // Ratio root mass / mass of aerial parts at harvest  // g.g -1 // PARAM // 1
  public float P_coefb;   // PARAMETER // parameter defining radiation effect on  conversion efficiency // SD // PARAM // 1
  public float P_irrlev;   // PARAMETER // amount of irrigation applied automatically on the sowing day when the model calculates irrigation, to allow germination // mm // PARAM // 1
  public float P_distdrain;   // PARAMETER // distance to the drain to calculate watertable height // cm // PARAM // 1
  public float P_khaut;   // PARAMETER // Extinction Coefficient connecting leaf area index to height crop // * // PARAM // 1
  public float P_dacohes;   // PARAMETER // bulk density under which root growth is reduced due to a lack of cohesion d // g cm-3 // PARAM // 1
  public float P_daseuilhaut;   // PARAMETER // Threshold of bulk density of soil below that the root growth  no more possible // g cm-3 // PARAM // 1
  public float P_daseuilbas;   // PARAMETER // Threshold of bulk density of soil below that the root growth is not limited // g cm-3 // PARAM // 1
  public float P_QNpltminINN;   // PARAMETER // minimal amount of nitrogen in the plant allowing INN computing // kg ha-1 // PARAM // 1
  public float P_finert;   // PARAMETER // fraction of the huñus stock inactive for mineralisation  (= stable OM/ total OM) // SD // PARAM // 1
  public float P_pHminvol;   // PARAMETER // P_pH above which the fertilizer volatilisation is null // P_pH // PARAM // 1
  public float P_pHmaxvol;   // PARAMETER //  P_pH beyond which the fertilizer volatilisation is maximale // P_pH // PARAM // 1
  public float P_Vabs2;   // PARAMETER // nitrogen uptake rate for which  fertilizer losts of  are divided by 2 // kg/ha/jour // PARAM // 1
  public float P_Xorgmax;   // PARAMETER // maximal amount of oraganised nitrogen coming from the mineral fertilizer  // kg.ha-1 // PARAM // 1
  public float P_hminm;   // PARAMETER // moisture (proportion of field capacity) below which mineralisation rate is nil // g eau g-1 sol // PARAM // 1
  public float P_hoptm;   // PARAMETER // moisture (proportion of field capacity) above which mineralisation rate is maximum // g eau g-1 sol // PARAM // 1
  public float P_hminn;   // PARAMETER // moisture (proportion of field capacity) below which nitrification rate is nil // g eau g-1 sol // PARAM // 1
  public float P_hoptn;   // PARAMETER // moisture (proportion of field capacity) at which nitrification rate is maximum // g eau g-1 sol // PARAM // 1
  public float P_pHminnit;   // PARAMETER // effect of the P_pH on nitrification (threshold mini) // P_pH // PARAM // 1
  public float P_pHmaxnit;   // PARAMETER // effect of the P_pH on nitrification (threshold maxi) // P_pH // PARAM // 1
  public float P_tnitopt;   // PARAMETER // cardinal temperature for nitrification // °C // PARAM // 1
  public float P_tnitmax;   // PARAMETER // cardinal temperature for nitrification // °C // PARAM // 1
  public float P_tnitmin;   // PARAMETER // cardinal temperature for nitrification // °C // PARAM // 1
  public float P_pminruis;   // PARAMETER // Minimal amount of precipitation to start a drip  // mm day-1 // PARAM // 1
  public float P_diftherm;   // PARAMETER // soil thermal diffusivity // cm2 s-1 // PARAM // 1
  public float P_Bformnappe;   // PARAMETER // coefficient of water table shape (artificial drained soil) // SD // PARAM // 1
  public float P_rdrain;   // PARAMETER // drain radius // cm // PARAM // 1
  public float P_hcccx[];   // PARAMETER // field capacity moisture of each pebble type  (table) // % pond�ral // PARAM // 1
  public float P_masvolcx[];   // PARAMETER // volumetric mass (bulk) of pebbles // g cm-3 // PARAM // 1
  public float P_engamm[];   // PARAMETER // proportion of ammonium in the fertilizer // SD // PARAM // 1
  public float P_voleng[];   // PARAMETER // maximal fraction of mineral fertilizer that can be volatilized  // SD // PARAM // 1
  public float P_orgeng[];   // PARAMETER // maximal quantity of mineral fertilizer that can be organized in the soil (fraction for type 8) // kg ha-1 // PARAM // 1
  public float P_deneng[];   // PARAMETER // proportion of the ñineral fertilizer that can be denitrified (useful if codenit not active) // SD // PARAM // 1
  public float P_parsurrg;   // PARAMETER // coefficient PAR/RG for the calculation of PAR  // * // PARAM // 1
  public int nbresidus;  
  public float  P_kbio[];   // PARAMETER // constant of mortality rate of microbial biomass // day -1 // PARAM // 1
  public float  P_yres[];   // PARAMETER // Carbon assimilation yield of the microflora // g g-1 // PARAM // 1
  public float  P_akres[];   // PARAMETER // parameter of organic residues decomposition: kres=P_akres+P_bkres/P_CsurNres // day-1 // PARAM // 1
  public float  P_bkres[];   // PARAMETER // parameter of organic residues decomposition: kres=P_akres+P_bkres/P_CsurNres // g g-1 // PARAM // 1
  public float  P_awb[];   // PARAMETER // parameter  of organic residues decomposition: CsurNbio=P_awb+P_bwb/P_CsurNres // SD // PARAM // 1
  public float  P_bwb[];   // PARAMETER // parameter of organic residues decomposition: CsurNbio=P_awb+P_bwb/P_CsurNres // g g-1 // PARAM // 1
  public float  P_cwb[];   // PARAMETER // Minimum ratio C/N of microbial biomass in the relationship: CsurNbio=P_awb+P_bwb/P_CsurNres // g g-1 // PARAM // 1
  public float  P_ahres[];   // PARAMETER // parameter of organic residues humification: hres=1-P_ahres*P_CsurNres/(P_bhres+P_CsurNres) // g g-1 // PARAM // 1
  public float  P_bhres[];   // PARAMETER // parameter of organic residues humification: hres=1-P_ahres*P_CsurNres/(P_bhres+P_CsurNres) // g g-1 // PARAM // 1
  public float  P_CNresmin[];   // PARAMETER // minimum observed value of ratio C/N of organic residues  // g g-1 // PARAM // 1
  public float  P_CNresmax[];   // PARAMETER // maximum observed value of ratio C/N of organic residues // g g-1 // PARAM // 1
  public float  P_CroCo[];   // PARAMETER // parameter of organic residues decomposition  //  SD// PARAM // 1
  public float  P_qmulchruis0[];   // PARAMETER // Amount of mulch to annul the drip // t ha-1 // PARAM // 1
  public float  P_mouillabilmulch[];   // PARAMETER // maximum wettability of crop mulch // mm t-1 ha // PARAM // 1
  public float  P_kcouvmlch[];   // PARAMETER // Extinction Coefficient reliant la quantité de paillis végétal au taux de couverture du sol // * // PARAM // 1
  public float  P_albedomulchresidus[];   // PARAMETER // P_albedo of crop mulch // SD // PARAM // 1
  public float  P_Qmulchdec[];   // PARAMETER // maximal amount of decomposing mulch // t C.ha-1 // PARAM // 1
  public int P_codesymbiose;   // PARAMETER // option of calculation of symbiotic fixation // code 1/2 // PARAM // 0
  public float P_proflabour;   // PARAMETER // soil minimal depth for ploughing when soil compaction is activated // cm // PARAM // 1
  public float P_proftravmin;   // PARAMETER // soil minimal depth for chisel tillage when soil compaction is activated // cm // PARAM // 1
  public float P_trefr;   // PARAMETER // temperature of reference for the soil mineralization parameters  // °C // PARAM // 1
  public float P_FTEMh;   // PARAMETER // Parameter 2 of the temperature function on the decomposition rate of humus // °K-1 // PARAM // 1
  public float P_FTEMr;   // PARAMETER // Parameter 2 of the temperature function on the decomposition rate of organic residues // °K-1 // PARAM // 1
  public float P_FTEMra;   // PARAMETER // Parameter 1 of the temperature function on the decomposition rate of organic residues // * // PARAM // 1
  public float P_FTEMha;   // PARAMETER // Parameter 1 of the temperature function on the decomposition rate of humus // * // PARAM // 1
  public float P_fhminsat;   // PARAMETER // soil mineralisation rate at water saturation // SD // PARAM // 1
  public float P_fnx;   // PARAMETER // maximum nitrification rate into the soil // day-1 // PARAM // 1
  public float P_rationit;   // PARAMETER // ratio between N2O emisson and total nitrification // kg.ha-1.j-1 // PARAM // 1
  public float P_ratiodenit;   // PARAMETER // ratio between N2O emisson and total denitrification // kg.ha-1.j-1 // PARAM // 1
  public float P_prophumtasssem;   // PARAMETER // field capacity proportion above witch compaction may occur (to delay sowing) // SD // PARAM // 1
  public float P_prophumtassrec;   // PARAMETER // field capacity proportion above witch compaction may occur (to delay harvest) // SD // PARAM // 1
  public int P_codeinitprec;   // PARAMETER // reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0
  public int P_flagEcriture;   // PARAMETER // option for writing the output files (1 = mod_history.sti, 2=daily outputs,4= report outputs, 8=balance outputs,16 = profile outputs, 32 = debug  outputs; 64= screen outputs, 128 = agmip outputs) add them to have several types of outputs //  code 1/2 // PARAM // 1
  public int P_codesensibilite;   // PARAMETER // code to activate the sensitivity analysis version of the model: yes (1), no (2) // code 1/2 // PARAM // 0
  public int P_codefrmur;   // PARAMETER // code defining the maturity status of the fruits in the output  variable CHARGEFRUIT (1 = including ripe fruits (last box N);  2 = excluding ripe fruits (first N-1 boxes)) // code 1/2 // PARAM // 0
  public int P_codefxn;   // PARAMETER // option to activate the chosen way to compute fxN // code 1/2 // PARAM // 0
  public int P_codemsfinal;   // PARAMETER // option defining the biomass and yield conservation after harvest (1 = yes (values maintained equal to harvest) ; 2 = no (values set at 0)) // code 1/2 // PARAM // 0
  public float P_psihumin;   // PARAMETER // soil potential corresponding to wilting point // Mpa // PARAM // 1
  public float P_psihucc;   // PARAMETER // soil potential corresponding to field capacity  // Mpa // PARAM // 1
  public int P_codeoutscient;
  public int P_codeseprapport;   // PARAMETER // choice of the kind of column separator in the rapport.sti file: separator chosen in P_separateurrapport (2), space (1) // code 1/2 // STATION // 0
  public int P_codemicheur;   // PARAMETER // option of calculation of hourly microclimatic outputs (output file humidite.sti) yes (1) no (2) // code 1/2 // PARAM // 0
  public float P_alphapH;   // PARAMETER // maximal soil pH variation per unit of inorganic N added with slurry // kg-1 ha //PARAM //1
  public float P_dpHvolmax;   // PARAMETER // maximal P_pH increase following the application of organic residue sutch as slurry // SD // PARAM // 1
  public float P_pHvols;   // PARAMETER // maximal soil P_pH above which soil P_pH is not affected by addition of organic residue (sutch as slurry)  // SD // PARAM // 1
  public float P_fredkN;   // PARAMETER // reduction factor of decomposition rate of residues when mineral N is limiting // SD // PARAM // 1
  public float P_fredlN;   // PARAMETER // reduction factor of decomposition rate of biomass when mineral N is limiting // SD // PARAM // 1
  public float P_fNCBiomin;   // PARAMETER // maximal reduction factor of the ratio N/C of the microbial biomass when nitrogen limits decomposition (between 0 and 1) // SD // PARAM // 1
  public float P_tnitopt2;     // PARAMETER // optimal temperature (2/2) for nitrification // grade C
  public float P_y0msrac;     //> // PARAMETER // minimal amount of root mass at harvest (when aerial biomass is nil)  // t.ha-1 // PARAM // 1
  public float P_fredNsup;     // PARAMETER // additional reduction factor of residues decomposition rate when mineral N is very limited in soil // SD // PARAM // 1
  public float P_Primingmax;   // PARAMETER // maximum priming ratio (relative to SOM decomposition rate) // SD //PARAM // 1

  public SafeSticsParameters () {
		  codeulaivernal = 0; 
		  nbresidus = 21;
		  P_hcccx= new float[10];   
		  P_masvolcx= new float[10];  
		  P_engamm= new float[8];   
		  P_voleng= new float[8];    
		  P_orgeng= new float[8];   
		  P_deneng= new float[8]; 
		  P_kbio = new float[nbresidus];   
		  P_yres = new float[nbresidus];    
		  P_akres = new float[nbresidus];  
		  P_bkres = new float[nbresidus];   
		  P_awb = new float[nbresidus];  
		  P_bwb = new float[nbresidus];  
		  P_cwb = new float[nbresidus];   
		  P_ahres = new float[nbresidus]; 
		  P_bhres = new float[nbresidus];  
		  P_CNresmin = new float[nbresidus];   
		  P_CNresmax = new float[nbresidus];   
		  P_CroCo = new float[nbresidus];     
		  P_qmulchruis0 = new float[nbresidus];    
		  P_mouillabilmulch = new float[nbresidus];   
		  P_kcouvmlch = new float[nbresidus];    
		  P_albedomulchresidus = new float[nbresidus];  
		  P_Qmulchdec = new float[nbresidus];  
  
	}

	@Override
	protected List<String> getFieldOrder() {
		return Arrays.asList(new String[] { "P_codeh2oact", "P_codeinnact", "P_codhnappe", "P_codeminopt",
				"P_codeprofmes", "P_codeactimulch", "codeulaivernal", "P_codetycailloux", "P_codetypeng",
				"P_codetypres", "P_iniprofil", "P_beta", "P_lvopt", "P_rayon", "P_fmin1", "P_fmin2", "P_fmin3", "P_Wh",
				"P_concrr", "P_TREFh", "P_difN", "P_plNmin", "P_proprac", "P_coefb", "P_irrlev", "P_distdrain",
				"P_khaut", "P_dacohes", "P_daseuilhaut", "P_daseuilbas", "P_QNpltminINN", "P_finert", "P_pHminvol",
				"P_pHmaxvol", "P_Vabs2", "P_Xorgmax", "P_hminm", "P_hoptm", "P_hminn", "P_hoptn", "P_pHminnit",
				"P_pHmaxnit", "P_tnitopt", "P_tnitmax", "P_tnitmin", "P_pminruis", "P_diftherm", "P_Bformnappe",
				"P_rdrain", "P_hcccx", "P_masvolcx", "P_engamm", "P_voleng", "P_orgeng", "P_deneng", "P_parsurrg",
				"nbresidus", "P_kbio", "P_yres", "P_akres", "P_bkres", "P_awb", "P_bwb", "P_cwb", "P_ahres", "P_bhres",
				"P_CNresmin", "P_CNresmax", "P_CroCo", "P_qmulchruis0", "P_mouillabilmulch", "P_kcouvmlch",
				"P_albedomulchresidus", "P_Qmulchdec", "P_codesymbiose", "P_proflabour", "P_proftravmin", "P_trefr",
				"P_FTEMh", "P_FTEMr", "P_FTEMra", "P_FTEMha", "P_fhminsat", "P_fnx", "P_rationit", "P_ratiodenit",
				"P_prophumtasssem", "P_prophumtassrec", "P_codeinitprec", "P_flagEcriture", "P_codesensibilite",
				"P_codefrmur", "P_codefxn", "P_codemsfinal", "P_psihumin", "P_psihucc", "P_codeoutscient",
				"P_codeseprapport", "P_codemicheur", "P_alphapH", "P_dpHvolmax", "P_pHvols", "P_fredkN", "P_fredlN",
				"P_fNCBiomin", "P_tnitopt2", "P_y0msrac", "P_fredNsup", "P_Primingmax"
		});
	}
	    
	 
}

	
	
  

