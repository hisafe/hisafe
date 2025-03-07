/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
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
 *		Share — copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt — remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution — 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
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

import java.util.Iterator;

import jeeb.lib.util.Import;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;
import safe.model.*;

/**
 * Format to read STICS general parameters file 
 * 
 * @author Isabelle Lecomte - December 2016
 */
public class SafeSticsParametersFormat extends RecordSet {

	private static final long serialVersionUID = 1L;

	// Parameters fertilizer record is described here  
	@Import
	static public class ParameterFertilizerRecord extends Record {
		private static final long serialVersionUID = 1L;
		public String 	name;    		// to define parameter name
		public float 	engamm;
		public float 	orgeng;
		public float 	deneng;
		public float 	voleng;

		public ParameterFertilizerRecord () {super ();}
		public ParameterFertilizerRecord (String line) throws Exception {super (line);}

	}
								
	// Parameters residues record is described here
	@Import
	static public class ParameterResidusRecord extends Record {
		private static final long serialVersionUID = 1L;
		public String 	name;    		// to define parameter name
		public float 	CroCo;
		public float 	akres;
		public float 	bkres;
		public float 	awb;
		public float 	bwb;
		public float 	cwb;
		public float 	ahres;
		public float 	bhres;
		public float 	kbio;
		public float 	yres;		
		public float 	CNresmin;
		public float 	CNresmax;
		public float 	qmulchruis0;
		public float 	mouillabilmulch;
		public float 	kcouvmlch;
		public float 	albedomulchresidus;
		public float 	Qmulchdec;
		
		public ParameterResidusRecord () {super ();}
		public ParameterResidusRecord (String line) throws Exception {super (line);}

	}
	
	public SafeSticsParametersFormat (String fileName) throws Exception {
		prepareImport (fileName);
	}

	/**
	* Saving data in a recordSet
	*/
	public SafeSticsParametersFormat (SafeSticsParameters sc)
						throws Exception {createRecordSet (sc);}

	public void createRecordSet (SafeSticsParameters sc) throws Exception {
	
	}

	/**
	* Read data from a recordSet
	*/
	public void load (SafeGeneralParameters settings,  SafeSticsTransit transit, SafeSticsParameters param) throws Exception {


		int indexFertilizer = 0;
		int indexResidus = 0;
		
		for (Iterator<Record> i = this.iterator (); i.hasNext ();) {

			Record record = i.next ();


			if (record instanceof SafeSticsParametersFormat.ParameterFertilizerRecord) {

				SafeSticsParametersFormat.ParameterFertilizerRecord r =
						(SafeSticsParametersFormat.ParameterFertilizerRecord) record;	// cast to precise type
			
				if(indexFertilizer < 8){
					param.P_engamm[indexFertilizer] 	= r.engamm;
					param.P_orgeng[indexFertilizer] 	= r.orgeng;
					param.P_deneng[indexFertilizer] 	= r.deneng;
					param.P_voleng[indexFertilizer] 	= r.voleng;
					indexFertilizer++; 
				}
			}
			else if (record instanceof SafeSticsParametersFormat.ParameterResidusRecord) {

				SafeSticsParametersFormat.ParameterResidusRecord r =
						(SafeSticsParametersFormat.ParameterResidusRecord) record;	// cast to precise type

				if(indexResidus < 21){
					param.P_CroCo[indexResidus] 	= r.CroCo;
					param.P_akres[indexResidus] 	= r.akres;
					param.P_bkres[indexResidus] 	= r.bkres;
					param.P_awb[indexResidus] 		= r.awb;
					param.P_bwb[indexResidus] 		= r.bwb;
					param.P_cwb[indexResidus] 		= r.cwb;
					param.P_ahres[indexResidus] 	= r.ahres;
					param.P_bhres[indexResidus] 	= r.bhres;
					param.P_kbio[indexResidus] 		= r.kbio;
					param.P_yres[indexResidus] 		= r.yres;
					param.P_CNresmin[indexResidus] 	= r.CNresmin;
					param.P_CNresmax[indexResidus] 	= r.CNresmax;	
					param.P_qmulchruis0[indexResidus] 	= r.qmulchruis0;
					param.P_mouillabilmulch[indexResidus] 	= r.mouillabilmulch;
					param.P_kcouvmlch[indexResidus] 	= r.kcouvmlch;
					param.P_albedomulchresidus[indexResidus] 	= r.albedomulchresidus;
					param.P_Qmulchdec[indexResidus] 	= r.Qmulchdec;
					

					indexResidus++; 
				}
			}			
			else if (record instanceof SafeSticsParametersFormat.KeyRecord) {
				 SafeSticsParametersFormat.KeyRecord r =
						(SafeSticsParametersFormat.KeyRecord) record;	// cast to precise type
				 
	 
				if (r.key.equals ("codeh2oact")) {
					param.P_codeh2oact= r.getIntValue ();
				}					
				else if (r.key.equals ("codeinnact")) {
					param.P_codeinnact = r.getIntValue ();
				}
				else if (r.key.equals ("codhnappe")) {
					param.P_codhnappe = r.getIntValue();
				}
				else if (r.key.equals ("codeminopt")) {
					param.P_codeminopt = r.getIntValue ();
				}				
				else if (r.key.equals ("codeprofmes")) {
					param.P_codeprofmes= r.getIntValue ();
				}
				else if (r.key.equals ("codeactimulch")) {
					param.P_codeactimulch = r.getIntValue ();
				}
				else if (r.key.equals ("codetycailloux")) {
					param.P_codetycailloux = r.getIntValue ();
				}	
				else if (r.key.equals ("codetypeng")) {
					param.P_codetypeng = r.getIntValue ();
				}
				else if (r.key.equals ("codetypres")) {
					param.P_codetypres = r.getIntValue ();
				}
				else if (r.key.equals ("iniprofil")) {
					param.P_iniprofil = r.getIntValue ();
				}
				else if (r.key.equals ("beta")) {
					param.P_beta = r.getFloatValue ();
				}
				else if (r.key.equals ("lvopt")) {
					param.P_lvopt = r.getFloatValue ();
				}		
				else if (r.key.equals ("rayon")) {
					param.P_rayon = r.getFloatValue ();
				}	
				else if (r.key.equals ("FMIN1")) {
					param.P_fmin1 = r.getFloatValue ();
				}						
				else if (r.key.equals ("FMIN2")) {
					param.P_fmin2 = r.getFloatValue ();
				}
				else if (r.key.equals ("FMIN3")) {
					param.P_fmin3 = r.getFloatValue ();
				}
				else if (r.key.equals ("Wh")) {
					param.P_Wh = r.getFloatValue ();
				}
				else if (r.key.equals ("concrr")) {
					param.P_concrr = r.getFloatValue ();
				}
				else if (r.key.equals ("TREFh")) {
					param.P_TREFh = r.getFloatValue ();
				}
				else if (r.key.equals ("difN")) {
					param.P_difN = r.getFloatValue ();
				}
				else if (r.key.equals ("plNmin")) {
					param.P_plNmin = r.getFloatValue ();
				}
				else if (r.key.equals ("proprac")) {
					param.P_proprac = r.getFloatValue ();
				}
				else if (r.key.equals ("coefb")) {
					param.P_coefb = r.getFloatValue ();
					param.P_coefb = param.P_coefb / 100.0f;
				}
				else if (r.key.equals ("irrlev")) {
					param.P_irrlev = r.getFloatValue ();
				}
				else if (r.key.equals ("distdrain")) {
					param.P_distdrain = r.getFloatValue ();
				}
				else if (r.key.equals ("khaut")) {
					param.P_khaut = r.getFloatValue ();
				}
				else if (r.key.equals ("dacohes")) {
					param.P_dacohes = r.getFloatValue ();
				}
				else if (r.key.equals ("daseuilhaut")) {
					param.P_daseuilhaut = r.getFloatValue ();
				}
				else if (r.key.equals ("daseuilbas")) {
					param.P_daseuilbas = r.getFloatValue ();
				}					
				else if (r.key.equals ("QNpltminINN")) {
					param.P_QNpltminINN= r.getFloatValue ();
				}
				else if (r.key.equals ("FINERT")) {
					param.P_finert = r.getFloatValue ();
				}		
				else if (r.key.equals ("pHminvol")) {
					param.P_pHminvol = r.getFloatValue ();
				}						
				else if (r.key.equals ("pHmaxvol")) {
					param.P_pHmaxvol = r.getFloatValue ();
				}
				else if (r.key.equals ("Vabs2")) {
					param.P_Vabs2 = r.getFloatValue();
				}
				else if (r.key.equals ("Xorgmax")) {
					param.P_Xorgmax = r.getFloatValue ();
				}	
				else if (r.key.equals ("hminm")) {
					param.P_hminm = r.getFloatValue();
				}		
				else if (r.key.equals ("hoptm")) {
					param.P_hoptm = r.getFloatValue();
				}
				else if (r.key.equals ("hminn")) {
					param.P_hminn = r.getFloatValue();
				}
				else if (r.key.equals ("hoptn")) {
					param.P_hoptn = r.getFloatValue();
				}
				else if (r.key.equals ("pHminnit")) {
					param.P_pHminnit = r.getFloatValue();
				}					
				else if (r.key.equals ("pHmaxnit")) {
					param.P_pHmaxnit = r.getFloatValue ();
				}
				else if (r.key.equals ("tnitopt")) {
					param.P_tnitopt = r.getFloatValue ();
				}
				else if (r.key.equals ("tnitmax")) {
					param.P_tnitmax= r.getFloatValue ();
				}
				else if (r.key.equals ("tnitmin")) {
					param.P_tnitmin = r.getFloatValue ();
				}		
				else if (r.key.equals ("pminruis")) {
					param.P_pminruis= r.getFloatValue ();
				}
				else if (r.key.equals ("diftherm")) {
					param.P_diftherm = r.getFloatValue ();
				}					
				else if (r.key.equals ("bformnappe")) {
					param.P_Bformnappe = r.getFloatValue ();
				}
				else if (r.key.equals ("rdrain")) {
					param.P_rdrain = r.getFloatValue ();
				}
				else if (r.key.equals ("codesymbiose")) {
					param.P_codesymbiose = r.getIntValue ();
				}
				else if (r.key.equals ("proflabour")) {
					param.P_proflabour = r.getFloatValue ();
				}
				else if (r.key.equals ("proftravmin")) {
					param.P_proftravmin = r.getFloatValue ();
				}
				else if (r.key.equals ("TREFr")) {
					param.P_trefr = r.getFloatValue ();
				}
				else if (r.key.equals ("FTEMh")) {
					param.P_FTEMh = r.getFloatValue ();
				}
				else if (r.key.equals ("FTEMr")) {
					param.P_FTEMr = r.getFloatValue ();
				}				
				else if (r.key.equals ("FTEMra")) {
					param.P_FTEMra = r.getFloatValue ();
				}
				else if (r.key.equals ("FTEMha")) {
					param.P_FTEMha = r.getFloatValue ();
				}
				else if (r.key.equals ("fhminsat")) {
					param.P_fhminsat = r.getFloatValue ();
				}
				else if (r.key.equals ("Fnx")) {
					param.P_fnx = r.getFloatValue ();
				}
				else if (r.key.equals ("rationit")) {
					param.P_rationit = r.getFloatValue ();
				}
				else if (r.key.equals ("ratiodenit")) {
					param.P_ratiodenit= r.getFloatValue ();
				}
				else if (r.key.equals ("prophumtasssem")) {
					param.P_prophumtasssem = r.getFloatValue ();
				}
				else if (r.key.equals ("prophumtassrec")) {
					param.P_prophumtassrec = r.getFloatValue ();
				}					
				else if (r.key.equals ("codeinitprec")) {
					param.P_codeinitprec = r.getIntValue ();
				}					
				else if (r.key.equals ("flagEcriture")) {
					param.P_flagEcriture = r.getIntValue ();
				}					
				else if (r.key.equals ("codesensibilite")) {
					param.P_codesensibilite = r.getIntValue ();
				}					
				else if (r.key.equals ("codefrmur")) {
					param.P_codefrmur = r.getIntValue ();
				}					
				else if (r.key.equals ("codefxn")) {
					param.P_codefxn = r.getIntValue ();
				}																				
				else if (r.key.equals ("codemsfinal")) {
					param.P_codemsfinal = r.getIntValue ();
				}					
				else if (r.key.equals ("psihumin")) {
					param.P_psihumin = r.getFloatValue ();
				}	
				else if (r.key.equals ("psihucc")) {
					param.P_psihucc = r.getFloatValue ();
				}		
				else if (r.key.equals ("codeoutscient")) {
					param.P_codeoutscient = r.getIntValue ();
				}
				else if (r.key.equals ("codeseprapport")) {
					param.P_codeseprapport = r.getIntValue ();
				}
				else if (r.key.equals ("codemicheur")) {
					param.P_codemicheur = r.getIntValue ();
				}
				else if (r.key.equals ("alphaph")) {
					param.P_alphapH = r.getFloatValue ();
				}
				else if (r.key.equals ("dphvolmax")) {
					param.P_dpHvolmax = r.getFloatValue ();
				}
				else if (r.key.equals ("phvols")) {
					param.P_pHvols = r.getFloatValue ();
				}
				else if (r.key.equals ("fredkN")) {
					param.P_fredkN = r.getFloatValue ();
				}
				else if (r.key.equals ("fredlN")) {
					param.P_fredlN = r.getFloatValue ();
				}
				else if (r.key.equals ("fNCbiomin")) {
					param.P_fNCBiomin = r.getFloatValue ();
				}
				else if (r.key.equals ("tnitopt2")) {
					param.P_tnitopt2 = r.getFloatValue ();
				}
				else if (r.key.equals ("y0msrac")) {
					param.P_y0msrac = r.getFloatValue ();
				}
				else if (r.key.equals ("fredNsup")) {
					param.P_fredNsup = r.getFloatValue ();
				}
				else if (r.key.equals ("Primingmax")) {
					param.P_Primingmax = r.getFloatValue ();
				}	
				
				//TRANSIT
				else if (r.key.equals ("codetempfauche")) {
					transit.P_codetempfauche = r.getIntValue ();
				}
				else if (r.key.equals ("codepluiepoquet")) {
					transit.P_codepluiepoquet = r.getIntValue ();
				}
				else if (r.key.equals ("nbjoursrrversirrig")) {
					transit.P_nbjoursrrversirrig = r.getIntValue ();
				}
				else if (r.key.equals ("code_adapt_MO_CC")) {
					transit.P_code_adapt_MO_CC = r.getIntValue ();
				}
				else if (r.key.equals ("code_adaptCC_miner")) {
					transit.P_code_adaptCC_miner = r.getIntValue ();
				}
				else if (r.key.equals ("code_adaptCC_nit")) {
					transit.P_code_adaptCC_nit= r.getIntValue ();
				}
				else if (r.key.equals ("code_adaptCC_denit")) {
					transit.P_code_adaptCC_denit = r.getIntValue ();
				}					
				else if (r.key.equals ("periode_adapt_CC")) {
					transit.P_periode_adapt_CC = r.getIntValue ();
				}	
				else if (r.key.equals ("an_debut_serie_histo")) {
					transit.P_an_debut_serie_histo = r.getIntValue ();
				}	
				else if (r.key.equals ("an_fin_serie_histo")) {
					transit.P_an_fin_serie_histo = r.getIntValue ();
				}	
				else if (r.key.equals ("param_tmoy_histo")) {
					transit.P_param_tmoy_histo = r.getFloatValue ();
				}	
				else if (r.key.equals ("TREFdenit1")) {
					transit.P_TREFdenit1 = r.getFloatValue ();
				}	
				else if (r.key.equals ("TREFdenit2")) {
					transit.P_TREFdenit2 = r.getFloatValue ();
				}	
				else if (r.key.equals ("nbj_pr_apres_semis")) {
					transit.P_nbj_pr_apres_semis = r.getIntValue ();
				}	
				else if (r.key.equals ("eau_mini_decisemis")) {
					transit.P_eau_mini_decisemis = r.getIntValue ();
				}	
				else if (r.key.equals ("humirac_decisemis")) {
					transit.P_humirac_decisemis = r.getFloatValue ();
				}	
				else if (r.key.equals ("swfacmin")) {
					transit.P_swfacmin = r.getFloatValue ();
					settings.stressMin = transit.P_swfacmin;
				}		
				else if (r.key.equals ("codetranspitalle")) {
					transit.P_codetranspitalle = r.getIntValue ();
				}	
	
				else if (r.key.equals ("codeNmindec")) {
					transit.P_codeNmindec = r.getIntValue ();
				}	
				else if (r.key.equals ("rapNmindec")) {
					transit.P_rapNmindec = r.getFloatValue ();
				}	
				else if (r.key.equals ("fNmindecmin")) {
					transit.P_fNmindecmin = r.getFloatValue ();
				}	
				else if (r.key.equals ("codetrosee")) {
					transit.P_codetrosee = r.getIntValue ();
				}	
				else if (r.key.equals ("codeSWDRH")) {
					transit.P_codeSWDRH = r.getIntValue ();
				}	
				else if (r.key.equals ("codedate_irrigauto")) {
					transit.P_codedate_irrigauto = r.getIntValue ();
				}	
				else if (r.key.equals ("datedeb_irrigauto")) {
					transit.P_datedeb_irrigauto = r.getIntValue ();
				}	
				else if (r.key.equals ("datefin_irrigauto")) {
					transit.P_datefin_irrigauto = r.getIntValue ();
				}	
				else if (r.key.equals ("codemortalracine")) {
					transit.P_codemortalracine = r.getIntValue ();
				}	
				else if (r.key.equals ("rules_sowing_AgMIP")) {
					transit.P_rules_sowing_AgMIP = r.getIntValue ();
				}	
				else if (r.key.equals ("Flag_Agmip_rap")) {
					transit.P_Flag_Agmip_rap = r.getIntValue ();
				}	
				else if (r.key.equals ("type_project")) {
					transit.P_type_project = r.getIntValue ();
				}	
				else if (r.key.equals ("option_thinning")) {
					transit.P_option_thinning = r.getIntValue ();
				}	
				else if (r.key.equals ("option_pature")) {
					transit.P_option_pature = r.getIntValue ();
				}	
				else if (r.key.equals ("option_engrais_multiple")) {
					transit.P_option_engrais_multiple = r.getIntValue ();
				}
				else if (r.key.equals ("coderes_pature")) {
					transit.P_coderes_pature = r.getIntValue ();
				}	
				else if (r.key.equals ("pertes_restit_ext")) {
					transit.P_pertes_restit_ext = r.getFloatValue ();
				}	
				else if (r.key.equals ("Crespc_pature")) {
					transit.P_Crespc_pature = r.getFloatValue ();
				}	
				else if (r.key.equals ("Nminres_pature")) {
					transit.P_Nminres_pature = r.getFloatValue ();
				}
				else if (r.key.equals ("eaures_pature")) {
					transit.P_eaures_pature = r.getFloatValue ();
				}	
				else if (r.key.equals ("coef_calcul_qres")) {
					transit.P_coef_calcul_qres = r.getFloatValue ();
				}	
				else if (r.key.equals ("engrais_pature")) {
					transit.P_engrais_pature = r.getIntValue ();
				}	
				else if (r.key.equals ("coef_calcul_doseN")) {
					transit.P_coef_calcul_doseN = r.getFloatValue ();
				}	
				//tableaux
				else if (r.key.equals ("coefracoupe1")) {
					transit.P_coefracoupe[0] = r.getFloatValue ();
				}	
				else if (r.key.equals ("coefracoupe2")) {
					transit.P_coefracoupe[1] = r.getFloatValue ();
				}	
				else if (r.key.equals ("SurfApex1")) {
					transit.P_SurfApex[0] = r.getFloatValue ();
				}
				else if (r.key.equals ("SurfApex2")) {
					transit.P_SurfApex[1] = r.getFloatValue ();
				}	
				else if (r.key.equals ("SeuilMorTalle1")) {
					transit.P_SeuilMorTalle[0] = r.getFloatValue ();
				}	
				else if (r.key.equals ("SeuilMorTalle2")) {
					transit.P_SeuilMorTalle[1] = r.getFloatValue ();
				}	
				else if (r.key.equals ("SigmaDisTalle1")) {
					transit.P_SigmaDisTalle[0] = r.getFloatValue ();
				}	
				else if (r.key.equals ("SigmaDisTalle2")) {
					transit.P_SigmaDisTalle[1] = r.getFloatValue ();
				}	
				else if (r.key.equals ("VitReconsPeupl1")) {
					transit.P_VitReconsPeupl[0] = r.getFloatValue ();
				}
				else if (r.key.equals ("VitReconsPeupl2")) {
					transit.P_VitReconsPeupl[1]= r.getFloatValue ();
				}	
				else if (r.key.equals ("SeuilReconsPeupl1")) {
					transit.P_SeuilReconsPeupl[0] = r.getFloatValue ();
				}	
				else if (r.key.equals ("SeuilReconsPeupl2")) {
					transit.P_SeuilReconsPeupl[1] = r.getFloatValue ();
				}	
				else if (r.key.equals ("MaxTalle1")) {
					transit.P_MaxTalle[0] = r.getFloatValue ();
				}	
				else if (r.key.equals ("MaxTalle2")) {
					transit.P_MaxTalle[1]  = r.getFloatValue ();
				}	
				else if (r.key.equals ("SeuilLAIapex1")) {
					transit.P_SeuilLAIapex[0] = r.getFloatValue ();
				}	
				else if (r.key.equals ("SeuilLAIapex2")) {
					transit.P_SeuilLAIapex[1] = r.getFloatValue ();
				}
				else if (r.key.equals ("codedyntalle1")) {
					transit.P_codedyntalle[0] = r.getIntValue ();
				}	
				else if (r.key.equals ("codedyntalle2")) {
					transit.P_codedyntalle[1] = r.getIntValue ();
				}	
				else if (r.key.equals ("tigefeuilcoupe1")) {
					transit.P_tigefeuilcoupe[0] = r.getFloatValue ();
				}	
				else if (r.key.equals ("tigefeuilcoupe2")) {
					transit.P_tigefeuilcoupe[1] = r.getFloatValue ();
				}		
				else if (r.key.equals ("resplmax1")) {
					transit.P_resplmax[0] = r.getFloatValue ();
				}	
				else if (r.key.equals ("resplmax2")) {
					transit.P_resplmax[1] = r.getFloatValue ();
				}
				else if (r.key.equals ("codemontaison1")) {
					transit.P_codemontaison[0] = r.getIntValue ();
				}	
				else if (r.key.equals ("codemontaison2")) {
					transit.P_codemontaison[1] = r.getIntValue ();
				}	
			}
			else {
				System.out.println("Unrecognized record : "+record);	
				throw new Exception ("Unrecognized record");	
			}
		}
		
		//On recopie dans STICS les valeurs de paramï¿½tres que l'on a dï¿½jï¿½ dans Hisafe
		param.P_parsurrg =  (float) settings.parGlobalCoefficient;

		for (int i=0;i<10;i++) {
			param.P_masvolcx[i] = (float) settings.STONE_VOLUMIC_DENSITY[i];
			param.P_hcccx[i]    =  (float) settings.STONE_WATER_CONTENT[i] * 100;
		}

	}
}

	
  

