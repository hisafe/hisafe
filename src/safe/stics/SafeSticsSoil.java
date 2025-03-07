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

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import com.sun.jna.Structure;
import safe.model.*;

/**
 * SafeSticsSoil - JNA mirror object for STICS soil
 *                  This object have to be the exact mirror of the FORTRAN Sol.f90 
 * 
 * @author Isabelle Lecomte - December 2016
 */

public class SafeSticsSoil extends Structure implements Serializable {

  private static final long serialVersionUID = 1L;
  
  public int nbCouchesSol_max;
  public int itrav1;  
  public int itrav2;  
  public int nstoc0;  
  public int profcalc;  
  public float Ndenit;       	// OUTPUT // Daily denitrification of nitrogen from fertiliser or soil (if option  denitrification  is activated)" // kg.ha-1.j-1
  public float Norgeng;       	// OUTPUT // Daily organisation of nitrogen from fertiliser // kgN.ha-1.j-1
  public float Nvolorg;  
  public float Nvoleng;       	// OUTPUT // Daily volatilisation of nitrogen from fertiliser // kgN.ha-1.j-1
  public float QNdenit;       	// OUTPUT // Cumulated denitrification of nitrogen from fertiliser or soil (if option  denitrification  is activated)" // kgN.ha-1
  public float QNorgeng;       	// OUTPUT // Cumulated organisation of nitrogen from fertiliser // kgN.ha-1
  public float QNvolorg;       	// OUTPUT // Cumulated volatilisation of nitrogen from organic inputs // kgN.ha-1
  public float Nvolatorg;  
  public float Qminrcult;  
  public float cumvminr;       	// OUTPUT // daily mineral nitrogen arising from humus // kgN.ha-1.j-1
  public float qdraincum;       // OUTPUT // Cumulated quantity of water evacuated towards drains // mm
  public float qdrain;       	// OUTPUT // Flow rate towards drains // mm j-1
  public float remontee;       	// OUTPUT // Capillary uptake in the base of the soil profile // mm j-1
  public float hmax;      		// OUTPUT // Maximum height of water table between drains // cm
  public float hnappe;       	// OUTPUT // Height of water table with active effects on the plant // cm
  public float hph;       		// OUTPUT // Maximum depth of perched water table // cm
  public float hpb;       		// OUTPUT // Minimum depth of perched water table // cm
  public float qlesd;       	// OUTPUT // Cumulated N-NO3 leached into drains // kgN.ha-1
  public float Ldrains;  
  public float nitrifj;       	// OUTPUT // Daily nitrification of nitrogen (if option  nitrification  is activated) // kg.ha-1
  public float profnappe;       // OUTPUT // Depth of water table // cm
  public float condenit;       	// OUTPUT // Denitrifying condition rate regard to the potential // 0-1
  public float concno3les;      // OUTPUT // Nitrate concentration in drainage water // mg NO3 l-1
  public float azlesd;       	// OUTPUT // Nitric nitrogen flow in drains // kgN.ha-1 j-1
  public float AZnit[];       	// OUTPUT // Amount of nitric nitrogen in the horizon 3 (table)  // kgN.ha-1
  public float QNvoleng;       	// OUTPUT // Cumulated volatilisation of nitrogen from fertiliser // kgN.ha-1
  public float sumes00;  
  public float sumes10;  
  public float sumes20;  
  public float supres0;  
  public float ses2j00;  
  public float sesj00;  
  public float smes020;  
  public float stoc0;  
  public float minoffrN;  
  public float NO3init[];  
  public float vminr;  
  public float aevap; 
  public float amm[];
  public float nit[];  		
  public float P_profdenit;   	// PARAMETER // soil depth on which denitrification is active (with the appropriate option) // cm // PARSOL // 1
  public int P_codedenit;   	// PARAMETER // option to allow the calculation of denitrification :yes (1), no(2) // code 1/2 // PARSOL // 0
  public float P_zesx;   		// PARAMETER // maximal depth of soil affected by soil evaporation // cm // PARSOL // 1
  public float P_cfes;   		// PARAMETER // parameter defining the soil contribution to evaporation as a function of depth  // SD // PARSOL // 1
  public float P_vpotdenit;   	// PARAMETER // potential rate of denitrification (per 1 cm soil layer) // kg ha-1 j-1 cm-1 // PARSOL // 1
  public float P_z0solnu;   	// PARAMETER // roughness length of bare soil // m // PARSOL // 1
  public int P_numsol;       	// PARAMETER // Soil number in the file PARAM.SOL // * // PARSOL // 1
  public int P_typecailloux[];  // PARAMETER // Pebbles type defined by a volumetric mass value (masvolx) and a field capacity moisture value (HCCCX) only used  if codecailloux=1 . (typecailloux= 1:calcaire B1,  2:calcaire B2,  3:calcaire L,  4:caillasse L,  5:gravier m,  6:silex, 7:granite, 8:calcaire J, 9-10:others) // SD // PARSOL // 1
  public int P_epd[];      		// PARAMETER //  mixing cells thickness (=2 x dispersivity) // cm // PARSOL // 1
  public int P_codecailloux;   	// PARAMETER // option of accounting of pebbles in the soil balances (1 = yes; 2 = no)  // code1/2 // PARSOL // 0
  public int P_codemacropor;   	// PARAMETER // "Simulation option of water flux in the macroporosity of soils to estimate water excess and drip by  overflowing : yes(1), no (0)" // code 1/2 // PARSOL // 0
  public int P_codefente;   	// PARAMETER // option allowing an additional water compartment for the swelling soils (1 = yes; 2 = no) // code1/2 // PARSOL // 0
  public int P_codrainage;   	// PARAMETER // artificial drainage (1 = yes; 2 = no)  // code1/2 // PARSOL // 0
  public int P_codenitrif;   	// PARAMETER // option to activate nitrification calculation (1 = yes; 2 = no) // code 1/2 // PARSOL // 0
  public int P_coderemontcap;   // PARAMETER // option to activate capillary rise (1 = yes; 2 = no)  // code1/2 // PARSOL // 0
  public int izcel[];  
  public int izc[];  
  public int ncel; 
  public int icel[];
  public float P_NO3initf[];      	// PARAMETER // initial nitric nitrogen in the soil per horizon  // kg N ha-1 // INIT // 1
  public float P_profhum;   		// PARAMETER // Humification depth  (max.60 cm) // cm // PARSOL // 1
  public float P_pH;   				// PARAMETER // P_pH of mixing soil + organic amendments  // SD // PARSOL // 1
  public float P_q0;   				// PARAMETER // Parameter of the end of the maximum evaporation stage  // mm // PARSOL // 1
  public float P_ruisolnu;   		// PARAMETER // fraction of drip rainfall (by ratio at the total rainfall) on a bare soil  // between 0 and 1 // PARSOL // 1
  public float P_obstarac;   		// PARAMETER // Soil depth which will block the root growth  // cm // PARSOL // 1
  public float P_profimper;   		// PARAMETER // Upper depth of the impermeable layer (from the soil surface). May be greater than the soil depth // cm // PARSOL // 1
  public float P_ecartdrain;   		// PARAMETER // inbetween drains distance // cm // PARSOL // 1
  public float P_Ksol;   			// PARAMETER // hydraulic conductivity in the soil above and below the drains // SD // PARSOL // 1
  public float P_profdrain;   		// PARAMETER // drain depth // cm // PARSOL // 1
  public float P_DAF[];      		// PARAMETER // Table of bulk density of the fine earth fraction in each soil layer  // g cm-3 // PARSOL // 1
  public float P_hminf[];      		// PARAMETER // gravimetric water content at wilting point of each soil layer (/fine earth) (table) // % w // PARSOL // 1
  public float P_hccf[];      		// PARAMETER // gravimetric water content at field capacity of each soil layer (/fine earth) (table) // % w // PARSOL // 1
  public float da[];       			// OUTPUT // Bulk density in the horizon 1 // g cm-3
  public float P_epc[];      		// PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm
  public float hcc[];  
  public float hmin[];  
  public float P_cailloux[];      	// PARAMETER // stone volumetric content per horizon // m3 m-3 // PARSOL // 1
  public float P_infil[];      		// PARAMETER // infiltrability parameter at the base of each horizon (if codemacropor = 1) // mm day-1 // PARSOL // 1      // OUTPUT // Infiltrability parameter at the base of the horizon 1 // mm day-1
  public float P_calc;   			// PARAMETER // percentage of calcium carbonate in the surface layer // % // PARSOL // 1
  public float P_argi;   			// PARAMETER // percentage of clay in the surface layer  // % // PARSOL // 1
  public float P_Norg;   			// PARAMETER // Organic Nitrogen  content of the tilled layer  (supposed constant on the depth  P_profhum) // % pondéral // PARSOL // 1
  public float profsol;  
  public float P_albedo;   			// PARAMETER // P_albedo of the bare dry soil // SD // PARSOL // 1
  public float P_humcapil;   		// PARAMETER // threshold of soil gravimetric water content under which capillary rise occurs // % w // PARSOL // 1
  public float P_capiljour;   		// PARAMETER // capillary rise upward water flux // mm j-1 // PARSOL // 1
  public float P_concseuil;   		// PARAMETER // Minimum Concentration of soil to NO3 // kgN ha-1 mm-1 // PARSOL // 1
  public float da_ini[];  
  public float q0_ini;  
  public float zesx_ini;  
  public float cumvminh;       		// OUTPUT // daily mineral nitrogen arising from organic residues // kgN.ha-1.j-1
  public float profhum_tass[];  
  public float P_pluiebat;   		// PARAMETER // minimal rain quantity for the crust occurrence // mm day-1 // PARSOL // 1
  public float P_mulchbat;   		// PARAMETER // mulch depth from which a crust occurs // cm // PARSOL // 1
  public float pHvol;       		// OUTPUT // soil surface P_pH varying after organic residue application (such as slurry) // SD
  public float dpH;  
  public float P_CsurNsol;    		// PARAMETER // Initial C to N ratio of soil humus // SD // PARSOL // 1 // Bruno-declaration du parametre CN du sol
  public float P_penterui;    		// PARAMETER // runoff coefficient taking account for plant mulch // SD // PARSOL // 1 // 27/07/2012 ajout de ce parametre pour le bresil
  public float N_volatilisation;   // OUTPUT // Cumulated volatilisation of nitrogen from organic inputs and fertiliser // kgN.ha-1
  public float epc_recal[];
  public float infil_recal[];
  public float P_Hinitf[];    		// PARAMETER // Table of initial gravimetric water content of each soil layer (/fine earth) // % w // INIT // 1
  public float P_NH4initf[];    	// PARAMETER // Amounts of initial mineral N in the 5 soil layers (fine earth) // kg.ha-1 // INIT // 1
  public float NH4init[];
  public float Hinit[];
  public float HR[];      			// OUTPUT // Water content of the horizon 5 (table)    // % pond.
  public float AZamm[];   			// OUTPUT // Amounts of NH4-N in the 5 soil horizons // kg.ha-1
  public float TS[];      			// OUTPUT // Mean soil temperature (mean of the 5 layers) // degree C

	/**
	* Constructor 
	*/
  public SafeSticsSoil (SafeSoil s) {

      nbCouchesSol_max = 1000; 
      
	  P_epc = new float[5];       
	  P_hminf = new float[5];    
	  P_hccf = new float[5];      
	  P_DAF = new float[5];    
	  P_typecailloux = new int[5];      
	  P_cailloux = new float[5];    
	  P_infil = new float[6];    
	  P_epd = new int[5];      
	  P_NO3initf = new float[5];    
	  AZnit = new float[5];        
	  NO3init = new float[5]; 
	  amm = new float[1000];
	  nit = new float[1001];  
	  izcel = new int[5];  
	  izc = new int[5];
	  icel = new int[1001];		  
	  da = new float[5];       	  
	  hcc = new float[5];  
	  hmin = new float[5];  
	  da_ini = new float[2]; 
	  profhum_tass = new float[2];  
	  epc_recal = new float[5];
	  infil_recal = new float[5];
	  P_Hinitf = new float[5];
	  P_NH4initf = new float[5];
	  NH4init = new float[5];  
	  Hinit = new float[5];
	  HR = new float[5];
	  AZamm = new float[5];
	  TS = new float[5];
	  
	  P_codecailloux = 2;   
	  P_codemacropor = 2;  
	  P_codefente = 2;   
	  P_codrainage = 2;   
	  P_codenitrif = 2;   
	  P_coderemontcap = 2;
	  P_codedenit = 2;

	  P_profhum = (float)  s.getHumificationDepth () * 100;	//cm
	  P_Norg = (float)  s.getOrganicNitrogen ();
	  P_albedo = (float)  s.getAlbedo ();
	  P_q0 = (float)  s.getEvaporationValue ();
	  P_ruisolnu = (float)  s.getRainRunOffFraction ();
	  P_obstarac = (float)  s.getCropRootObstruction () * 100;	//cm
	  P_concseuil = (float)  s.getMinNh4Concentration ();		//kg.ha-1 mm-1
	   
	  P_pH = (float)  s.getPh ();

	  //Capillary uptake options
	  if (s.getCapillary()) P_coderemontcap = 1;	
	  P_capiljour = (float)  s.getCapillaryUptake ();
	  P_humcapil = (float)  s.getCapillaryUptakeMinWater () * 10;
	  
	  //drainage options 
	  if (s.getArtificialDrainage()) P_codrainage = 1;   
	  P_profimper = (float)  s.getImpermeableLayerDepth () * 100; 	//cm
	  P_ecartdrain = (float)  s.getDrainagePipesSpacing () * 100;
	  P_profdrain = (float) s.getDrainagePipesDepth () * 100;
	  P_Ksol = (float)  s.getWaterConductivity ();
		
	  //other options
	  if (s.getSwellingClaySoil()) P_codefente = 1;  	
	  if (s.getMacroporosity()) P_codemacropor = 1; 
	  if (s.getNitrification()) P_codenitrif = 1;  	 
	  if (s.getDenitrification()) P_codedenit = 1; 

	  P_argi = (float) s.getLayer(0).getClay ();
	  P_calc = (float) s.getLayer(0).getLimestone ();
		
	  //new parameters
	  P_pluiebat = (float) s.getSoilCrustRainMin();
	  P_mulchbat  = (float) s.getSoilCrustDepth();      
	  P_zesx = (float) s.getEvaporationMaxDepth();		    
	  P_cfes   = (float) s.getEvaporationDepthContribution();
	  P_z0solnu =  (float) s.getRoughnessLength(); 
	  P_CsurNsol  =  (float) s.getSoilHumusCN();
	  P_penterui = (float) s.getRunOffCoefPlantMulch();
	  P_profdenit = (float) s.getDenitrificationDepth(); 
	  P_vpotdenit = (float) s.getDenitrificationRate(); 

	  P_infil[0] = 0;
	  
	  //soil layer settings and initial values
		for (int i=0; i<5; i++) {
			
			if (s.getLayer(i) != null) {

				P_epc[i] = (float) (s.getLayer(i).getThickness () * 100);	//convert m to cm

				//convert FC and WP from m3 m-3 in g g-1
				P_hccf[i] = (float) (s.getLayer(i).getFieldCapacityFineSoil () * 100	/ s.getLayer(i).getBulkDensityFineSoil ()); 
				P_hminf[i] = (float) (s.getLayer(i).getWiltingPointFineSoil () * 100	/ s.getLayer(i).getBulkDensityFineSoil ()); 
			

				P_DAF[i] = (float) s.getLayer(i).getBulkDensityFineSoil ();		
				P_infil[i+1] = (float) s.getLayer(i).getInfiltrability ();
				P_cailloux[i] = (float) s.getLayer(i).getStone ();
				P_typecailloux[i] = (int) s.getLayer(i).getStoneType ();

				if (P_cailloux[i]>0) P_codecailloux = 1; 

				//STICS new parameter
				P_epd[i] = 10;

			}
		}	
		
	}
	/**
	* Soil first intialisation 
	*/
  public void initialise (SafeSoil s, SafePlotSettings settings) {
	  
	  //soil layer settings and initial values
		for (int i=0; i<5; i++) {
			
			if (s.getLayer(i) != null) {
				//convert initial water content of fine soil from % to % ponderal
				if (s.getLayer(i).getBulkDensityFineSoil () > 0 ) {			
					P_Hinitf[i] 	= (float) (settings.layerWaterContent[i] * 100 / s.getLayer(i).getBulkDensityFineSoil ());	
				}

				P_NO3initf[i]  = (float) settings.layerNo3Content[i];	//kg h-1
				P_NH4initf[i] = (float) settings.layerNh4Content[i];	//kg h-1		
			}
		}
  	}
  
	/**
	* Soil initial values SET with current soil values (for STICS REPORT)   
	*/
  	public void reinitialise () {
	  
	  //soil layer settings and initial values
		for (int i=0; i<5; i++) {				
			Hinit[i] = HR[i];
			NO3init[i] = AZnit[i];
			NH4init[i]= AZamm[i];		
		}
	}


	/**
	* Constructor for cloning 
	*/
	public SafeSticsSoil (SafeSticsSoil origin) {
		
		nbCouchesSol_max=origin.nbCouchesSol_max;
		itrav1=origin.itrav1;
		itrav2=origin.itrav2;
		nstoc0=origin.nstoc0;
		profcalc=origin.profcalc;
		Ndenit=origin.Ndenit;
		Norgeng=origin.Norgeng;
		Nvolorg=origin.Nvolorg;
		Nvoleng=origin.Nvoleng;
		QNdenit=origin.QNdenit;
		QNorgeng=origin.QNorgeng;
		QNvolorg=origin.QNvolorg;
		Nvolatorg=origin.Nvolatorg;
		Qminrcult=origin.Qminrcult;
		cumvminr=origin.cumvminr;
		qdraincum=origin.qdraincum;
		qdrain=origin.qdrain;
		remontee=origin.remontee;
		hmax=origin.hmax;
		hnappe=origin.hnappe;
		hph=origin.hph;
		hpb=origin.hpb;
		qlesd=origin.qlesd;
		Ldrains=origin.Ldrains;
		nitrifj=origin.nitrifj;
		profnappe=origin.profnappe;
		condenit=origin.condenit;
		concno3les=origin.concno3les;
		azlesd=origin.azlesd;
		QNvoleng=origin.QNvoleng;
		sumes00=origin.sumes00;
		sumes10=origin.sumes10;
		sumes20=origin.sumes20;
		supres0=origin.supres0;
		ses2j00=origin.ses2j00;
		sesj00=origin.sesj00;
		smes020=origin.smes020;
		stoc0=origin.stoc0;
		minoffrN=origin.minoffrN;
		vminr=origin.vminr;
		aevap=origin.aevap;
		P_profdenit=origin.P_profdenit;
		P_codedenit=origin.P_codedenit;
		P_zesx=origin.P_zesx;
		P_cfes=origin.P_cfes;
		P_vpotdenit=origin.P_vpotdenit;
		P_z0solnu=origin.P_z0solnu;
		P_numsol=origin.P_numsol;
		P_codecailloux=origin.P_codecailloux;
		P_codemacropor=origin.P_codemacropor;
		P_codefente=origin.P_codefente;
		P_codrainage=origin.P_codrainage;
		P_codenitrif=origin.P_codenitrif;
		P_coderemontcap=origin.P_coderemontcap;
		ncel=origin.ncel;
		P_profhum=origin.P_profhum;
		P_pH=origin.P_pH;
		P_q0=origin.P_q0;
		P_ruisolnu=origin.P_ruisolnu;
		P_obstarac=origin.P_obstarac;
		P_profimper=origin.P_profimper;
		P_ecartdrain=origin.P_ecartdrain;
		P_Ksol=origin.P_Ksol;
		P_profdrain=origin.P_profdrain;
		P_calc=origin.P_calc;
		P_argi=origin.P_argi;
		P_Norg=origin.P_Norg;
		profsol=origin.profsol;
		P_albedo=origin.P_albedo;
		P_humcapil=origin.P_humcapil;
		P_capiljour=origin.P_capiljour;
		P_concseuil=origin.P_concseuil;
		q0_ini=origin.q0_ini;
		zesx_ini=origin.zesx_ini;
		cumvminh=origin.cumvminh;
		P_pluiebat=origin.P_pluiebat;
		P_mulchbat=origin.P_mulchbat;
		pHvol=origin.pHvol;
		dpH=origin.dpH;
		P_CsurNsol=origin.P_CsurNsol;
		P_penterui=origin.P_penterui;
		N_volatilisation=origin.N_volatilisation;
		
		
		P_epc = new float[5];       
		P_hminf = new float[5];    
		P_hccf = new float[5];      
		P_DAF = new float[5];    
		P_typecailloux = new int[5];      
		P_cailloux = new float[5];    
		P_infil = new float[6];    
		P_epd = new int[5];      
		P_NO3initf = new float[5];    
		AZnit = new float[5];        
		NO3init = new float[5]; 
		amm = new float[1000];
		nit = new float[1001];  
		izcel = new int[5];  
		izc = new int[5];
		icel = new int[1001];		  
		da = new float[5];       	  
		hcc = new float[5];  
		hmin = new float[5];  
		da_ini = new float[2]; 
		profhum_tass = new float[2];  
		epc_recal = new float[5];
		infil_recal = new float[5];
		P_Hinitf = new float[5];
		P_NH4initf = new float[5];
		NH4init = new float[5];	  
		Hinit = new float[5];
		HR = new float[5];
		AZamm = new float[5];
		TS = new float[5];
		  
		  
		System.arraycopy(origin.P_epc, 0, this.P_epc, 0, 5);
		System.arraycopy(origin.P_hminf, 0, this.P_hminf, 0, 5);
		System.arraycopy(origin.P_hccf, 0, this.P_hccf, 0, 5);
		System.arraycopy(origin.P_DAF, 0, this.P_DAF, 0, 5);
		System.arraycopy(origin.P_typecailloux, 0, this.P_typecailloux, 0, 5);
		System.arraycopy(origin.P_cailloux, 0, this.P_cailloux, 0, 5);
		System.arraycopy(origin.P_infil, 0, this.P_infil, 0, 6);
		System.arraycopy(origin.P_epd, 0, this.P_epd, 0, 5);
		System.arraycopy(origin.P_NO3initf, 0, this.P_NO3initf, 0, 5);
		System.arraycopy(origin.AZnit, 0, this.AZnit, 0, 5);
		System.arraycopy(origin.NO3init, 0, this.NO3init, 0, 5);
		System.arraycopy(origin.amm, 0, this.amm, 0, 1000);
		System.arraycopy(origin.nit, 0, this.nit, 0, 1001);
		System.arraycopy(origin.izcel, 0, this.izcel, 0, 5);
		System.arraycopy(origin.izc, 0, this.izc, 0, 5);
		System.arraycopy(origin.icel, 0, this.icel, 0, 1001);
		System.arraycopy(origin.da, 0, this.da, 0, 5);
		System.arraycopy(origin.hcc, 0, this.hcc, 0, 5);
		System.arraycopy(origin.hmin, 0, this.hmin, 0, 5);
		System.arraycopy(origin.da_ini, 0, this.da_ini, 0, 2);
		System.arraycopy(origin.profhum_tass, 0, this.profhum_tass, 0, 2);
		System.arraycopy(origin.epc_recal, 0, this.epc_recal, 0, 5);
		System.arraycopy(origin.infil_recal, 0, this.infil_recal, 0, 5);
		System.arraycopy(origin.P_Hinitf, 0, this.P_Hinitf, 0, 5);
		System.arraycopy(origin.P_NH4initf, 0, this.P_NH4initf, 0, 5);
		System.arraycopy(origin.NH4init, 0, this.NH4init, 0, 5);
		System.arraycopy(origin.Hinit, 0, this.Hinit, 0, 5);	
		System.arraycopy(origin.HR, 0, this.HR, 0, 5);
		System.arraycopy(origin.AZamm, 0, this.AZamm, 0, 5);
		System.arraycopy(origin.TS, 0, this.TS, 0, 5);

	}
	
	
	
	@Override
	protected List<String> getFieldOrder() {
		return Arrays.asList(new String[] { "nbCouchesSol_max", "itrav1", "itrav2", "nstoc0", "profcalc", "Ndenit",
				"Norgeng", "Nvolorg", "Nvoleng", "QNdenit", "QNorgeng", "QNvolorg", "Nvolatorg", "Qminrcult",
				"cumvminr", "qdraincum", "qdrain", "remontee", "hmax", "hnappe", "hph", "hpb", "qlesd", "Ldrains",
				"nitrifj", "profnappe", "condenit", "concno3les", "azlesd", "AZnit", "QNvoleng", "sumes00", "sumes10",
				"sumes20", "supres0", "ses2j00", "sesj00", "smes020", "stoc0", "minoffrN", "NO3init", "vminr", "aevap",
				"amm", "nit", "P_profdenit", "P_codedenit", "P_zesx", "P_cfes", "P_vpotdenit", "P_z0solnu", "P_numsol",
				"P_typecailloux", "P_epd", "P_codecailloux", "P_codemacropor", "P_codefente", "P_codrainage",
				"P_codenitrif", "P_coderemontcap", "izcel", "izc", "ncel", "icel", "P_NO3initf", "P_profhum", "P_pH",
				"P_q0", "P_ruisolnu", "P_obstarac", "P_profimper", "P_ecartdrain", "P_Ksol", "P_profdrain", "P_DAF",
				"P_hminf", "P_hccf", "da", "P_epc", "hcc", "hmin", "P_cailloux", "P_infil", "P_calc", "P_argi",
				"P_Norg", "profsol", "P_albedo", "P_humcapil", "P_capiljour", "P_concseuil", "da_ini", "q0_ini",
				"zesx_ini", "cumvminh", "profhum_tass", "P_pluiebat", "P_mulchbat", "pHvol", "dpH", "P_CsurNsol",
				"P_penterui", "N_volatilisation", "epc_recal", "infil_recal", "P_Hinitf", "P_NH4initf", "NH4init",
				"Hinit", "HR", "AZamm", "TS" });
    }
	    
	 
}

	
	
  

