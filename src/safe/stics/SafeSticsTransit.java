package safe.stics;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import com.sun.jna.Structure;
import safe.model.*;

/**
 * SafeSticsTransit - Mirror object of STICS FORTRAN Stics_Transit_ 
 * 
 * 
 * @author Isabelle Lecomte - December 2016
 */
public class SafeSticsTransit extends Structure implements Serializable {

  
	  public int P_codetempfauche;  		// PARAMETER // option of the reference temperature to compute cutting sum of temperatures : upvt (1), udevair (2) // code 1/2 // PARAMV6 // 0
	  public float P_coefracoupe[];     	// 2 plantes 	  // PARAMETER // coefficient to define the proportion of dying roots after cut (grass) // SD // PARAMV6/PLT // 1 
	  public int P_codepluiepoquet;  		// PARAMETER // option to replace rainfall by irrigation at poquet depth in the case of poquet sowing // code 1/2 // PARAMV6 // 0
	  public int P_nbjoursrrversirrig;  	// PARAMETER // number of days during which rainfall is replaced by irrigation in the soil after a sowing poquet // jours // PARAMV6 // 1	
	  public float P_SurfApex[];     		// PARAMETER // equivalent surface of a transpiring apex // m² // PARAMV6/PLT // 1
	  public float P_SeuilMorTalle[];     	// PARAMETER // relative transpiring threshold to calculate tiller mortality // mm // PARAMV6/PLT // 1
	  public float P_SigmaDisTalle[];     	// PARAMETER // Coefficient used for the gamma law calculating tiller mortality //  // PARAMV6/PLT // 1
	  public float P_VitReconsPeupl[];     	// PARAMETER // thermal time for the regeneration of the tiller population // nb tillers/degree C/m² // PARAMV6 // 1
	  public float P_SeuilReconsPeupl[];    // PARAMETER // tiller density threshold below which the entire population won't be regenerated // nb tillers/m² // PARAMV6/PLT // 1
	  public float P_MaxTalle[];     		// PARAMETER // maximal density of tillers/m² // Nb tillers/ // PARAMV6/PLT // 1
	  public int P_code_adapt_MO_CC;  		// PARAMETER // activation code for organic matter adaptation to climate change (1 = yes, 2 = no) // code1/2 // PARAMV6 // 0
	  public int P_code_adaptCC_miner;  	// PARAMETER // activation code for the impact of climate change on mineralisation, parameter modification P_trefh and P_trefr (1 = yes, 2 = no) // code1/2 // PARAMV6 // 0
	  public int P_code_adaptCC_nit;  		// PARAMETER // activation code for the impact of climate change on nitrification, parameter modification P_tnitmin, P_tnitmax, P_tnitopt and P_tnitopt2 (1 = yes, 2 = no) // code1/2 // PARAMV6 // 0
	  public int P_code_adaptCC_denit;  	// PARAMETER // activation code for the impact of climate change on denitrification, parameter modification P_trefdenit1 and P_trefdenit2 (1 = yes, 2 = no) // code1/2 // PARAMV6 // 0
	  public int P_periode_adapt_CC;  		// PARAMETER // year number to calculate moving temperature average // year // PARAMV6 // 1
	  public int P_an_debut_serie_histo;  	// PARAMETER // beginning year for the calculation of moving average temperature on period_adapt_CC // year // PARAMV6 // 0
	  public int P_an_fin_serie_histo;  	// PARAMETER // ending year for the calculation of moving average temperature on period_adapt_CC // year // PARAMV6 // 0
	  public float P_param_tmoy_histo;  	// PARAMETER // mean temperature over the period of adaptation to climate change // degree C // PARAMV6 // 1
	  public float P_TREFdenit1;  			// PARAMETER // temperature of reference for the soil denitrification parameters (11degree C for temperate soils and 20degree C for tropical soils) // degree C // PARAMV6 // 1
	  public float P_TREFdenit2;  			// PARAMETER // temperature of reference for the soil denitrification parameters (20degree C for temperate soils and 29degree C for tropical soils) // degree C // PARAMV6 // 1
	  public int P_nbj_pr_apres_semis;  	// PARAMETER // days number to calculate rainfall need to start sowing (is codesemis is activated) // day // PARAMV6 // 1
	  public int P_eau_mini_decisemis;  	// PARAMETER // minimum amount of rainfall to start sowing (when codesemis is activated) // mm // PARAMV6 // 1
	  public float P_humirac_decisemis;  	// PARAMETER // effect of soil moisture for sowing decision ( from 0 to 1 : 0 = no sensitivity to drought; 1 = very sensitive) // SD // PARAMV6 // 1
	  public float P_swfacmin;  			// PARAMETER // minimul value for drought stress index (turfac, swfac, senfac) // SD // PARAMV6 // 1
	  public float P_SeuilLAIapex[];   		// PARAMETER // Maximal value of LAI+LAIapex when LAIapex isn't nil // m²/m² // PARAMV6/PLT // 1
	  public int P_codetranspitalle;  		// PARAMETER // Choice of the ratio used to calculate tiller mortality: et/etm (1) ou epc2 / eopC (2) // code 1/2 // PARAMV6 // 0
	  public int P_codedyntalle[];   		// PARAMETER // Activation of the module simulating tiller dynamic: yes (1), no (2) // code 1/2 // PARAMV6/PLT // 0
	  public float P_tigefeuilcoupe[];   	// PARAMETER // stem (structural part)/leaf proportion the cutting day // SD // PARAMV6/PLT // 1
	  public float P_resplmax[];   			// PARAMETER // maximal reserve biomass // t ha-1 // PARAMV6 // 1
	  public int P_codemontaison[];      	// PARAMETER // code to stop the reserve limitation from the stem elongation // code 1/2 // PARAMV6 // 0 
	  public int P_codecalferti;  			// PARAMETER // automatic calculation of fertilisation requirements: yes (2), no (1) // code 1/2 // PARAMV6 // 0
	  public int P_codetesthumN;  			// PARAMETER // automatic fertilisation calculations // code 1/2 // PARAMV6 // 0
	  public float P_dosimxN;  				// PARAMETER // maximum amount of fertilisation authorised at each time step (mode automatic fertilisation) // kg N ha-1 // PARAMV6 // 1
	  public float P_ratiolN;  				// PARAMETER // Nitrogen stress index below which we start an fertilisation in automatic mode (0 in manual mode) // between 0 and 1  // PARAMV6 // 1
	  public int P_codeNmindec; 			// PARAMETER // option to activate the available N :yes (1), no(2) // code 1/2 //PARAMv6 // 1
	  public float P_rapNmindec;  			// PARAMETER //slope of the linear relationship between the fraction of mineral N available for residue decomposition and the amount of C in decomposing residues (0.001)// g.g-1 // PARAMV6 //1
	  public float P_fNmindecmin; 			// PARAMETER //minimal fraction of mineral N available for residues decomposition (if codeNmindec is activated) // SD // PARAMV6 //1
	  public int P_codetrosee;   			// PARAMETER // option to choose the way to calculate the hourly dew temperature : linear interpolation (1), sinusoidal interpolation (Debele Bekele et al.,2007)(2) // code 1/2 //PARAMv6 // 1
	  public int P_codeSWDRH;    			// PARAMETER // option to activate the calculation of surface wetness duration : yes (1), no (2) // code 1/2 //PARAMv6 // 1
	  public int P_codedate_irrigauto; 		// PARAMETER // option to activate the beginning and the ending date in case of automatic irrigation  : yes (1), no (2) // code 1/2 //PARAMv6 // 1
	  public int P_datedeb_irrigauto;  		// PARAMETER // date of beginning automatic irrigations  : yes (1), no (2) // code 1/2 //PARAMv6 // 1
	  public int P_datefin_irrigauto; 		// PARAMETER // date of ending automatic irrigations  : yes (1), no (2) // code 1/2 //PARAMv6 // 1
	  public int P_codemortalracine; 		// PARAMETER // masec servant a calculer les racines mortes a la coupe  : masec (1), masectot (2) // code 1/2 //PARAMv6 // 1
	  public int P_rules_sowing_AgMIP; 		// PARAMETER // activation of the semis rules AgMIP wheat3 yes(1) no(2) // code 1/2 //PARAMv6 // 1
	  public int P_Flag_Agmip_rap;  		// PARAMETER // report specific outputs AgMIP nothing(1) AgMIP(2) Macsur(3) // code 1/2/3 //PARAMv6 // 1
	  public int P_type_project;  			// PARAMETER // activation des regles de semis AgMIP AgMIP Wheat(1) "AgMIP Wheat Giacomo (HSC)(2) wheat Canopy temp(3) face_maize(4) new wheat3(5) // code 1/2/3/4/5 //PARAMv6 // 1
	  public int P_option_thinning; 		// PARAMETER // enabling of several thinning yes(1),(no) 2 //code 1/2 //PARAMv6 // 1
	  public int P_option_pature; 			// PARAMETER // enabling of pasture of grassland yes(1),(no) 2 //code 1/2 //PARAMv6 // 1
	  public int P_option_engrais_multiple;  // PARAMETER // enabling of using several kind of fertilizer yes(1),(no) 2 //code 1/2 //PARAMv6 // 1
	  public int P_coderes_pature; 			// PARAMETER // residue type: 1=crop residues,  2=residues of CI,  3=manure,  4=compost OM,  5=mud SE,  6=vinasse,  7=corn,  8=other // code 1 to 10 // PARAMv6 // 0
	  public float P_pertes_restit_ext;   	// PARAMETER // dejections animales non restituees sur les parcelles //  // PARAMv6 // 1
	  public float P_Crespc_pature;  		// PARAMETER // carbon proportion in organic residue //  // PARAMv6 // 1
	  public float P_Nminres_pature;    	// PARAMETER // N mineral content of organic residues  // % fresh matter // PARAMv6 // 1
	  public float P_eaures_pature;    		// PARAMETER // Water amount of organic residues  // % fresh matter // PARAMv6 // 1
	  public float P_coef_calcul_qres;   	// PARAMETER // ?  // ? // PARAMv6 // 1
	  public int P_engrais_pature;  		// PARAMETER // fertilizer type  : 1 =Nitrate.of ammonium ,2=Solution,3=urea,4=Anhydrous ammoniac,5= Sulfate of ammonium,6=phosphate of ammonium,7=Nitrateof calcium,8= fixed efficiency    // * // PARAMv6 // 1
	  public float P_coef_calcul_doseN;   // PARAMETER // ?  // ? // PARAMv6 // 1

  
	public SafeSticsTransit () {
		
		  P_coefracoupe = new float[2];
		  P_SurfApex = new float[2];      
		  P_SeuilMorTalle = new float[2];     
		  P_SigmaDisTalle = new float[2];   
		  P_VitReconsPeupl = new float[2];     
		  P_SeuilReconsPeupl = new float[2];    
		  P_MaxTalle = new float[2];    
		  P_SeuilLAIapex= new float[2];  
		  P_codedyntalle= new int[2];   
		  P_tigefeuilcoupe= new float[2];  
		  P_resplmax= new float[2];  
		  P_codemontaison= new int[2];
	}

	@Override
	protected List getFieldOrder() {
		return Arrays.asList(new String[] {"P_codetempfauche", "P_coefracoupe", "P_codepluiepoquet",
				"P_nbjoursrrversirrig",  "P_SurfApex", "P_SeuilMorTalle", "P_SigmaDisTalle",
				"P_VitReconsPeupl", "P_SeuilReconsPeupl", "P_MaxTalle", 
				"P_code_adapt_MO_CC", "P_code_adaptCC_miner", "P_code_adaptCC_nit",
				"P_code_adaptCC_denit", "P_periode_adapt_CC", "P_an_debut_serie_histo", "P_an_fin_serie_histo",
				"P_param_tmoy_histo", "P_TREFdenit1", "P_TREFdenit2", "P_nbj_pr_apres_semis", "P_eau_mini_decisemis",
				"P_humirac_decisemis", "P_swfacmin", "P_SeuilLAIapex", "P_codetranspitalle",
				"P_codedyntalle",  "P_tigefeuilcoupe", "P_resplmax", "P_codemontaison", 
				"P_codecalferti", "P_codetesthumN", "P_dosimxN", "P_ratiolN", "P_codeNmindec", "P_rapNmindec",
				"P_fNmindecmin", "P_codetrosee", "P_codeSWDRH", "P_codedate_irrigauto", "P_datedeb_irrigauto",
				"P_datefin_irrigauto", "P_codemortalracine", "P_rules_sowing_AgMIP", "P_Flag_Agmip_rap",
				"P_type_project", "P_option_thinning", "P_option_pature", "P_option_engrais_multiple",
				"P_coderes_pature", "P_pertes_restit_ext", "P_Crespc_pature", "P_Nminres_pature", "P_eaures_pature",
				"P_coef_calcul_qres", "P_engrais_pature", "P_coef_calcul_doseN"

		});
	}
	    
	 
}

	
	
  

