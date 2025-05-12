/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE - CC-BY License
 * 
 * LIST OF AUTHORS
 * --------------- 
 * Christian Dupraz 1, Kevin J.Wolz 1 , Isabelle Lecomte 1, Grégoire Talbot 1, Nicolas Barbault 1, 
 * Grégoire Vincent 2 , Rachmat Mulia 3, François Bussière 4, Harry Ozier-Lafontaine 4,
 * Sitraka Andrianarisoa 1, Nick Jackson 5, Gerry Lawson 5, Nicolas Dones 6, Hervé Sinoquet 6,
 * Betha Lusiana 3, Degi Harja 3, Suzy Domenicano 7 , Francesco Reyes 1 , Marie Gosme 1 ,
 * Meine Van Noordwijk 3, Benoit Courbaud 8
 *
 * 1 INRA (UMR-ABSYS), University of Montpellier, 34090 Montpellier, France
 * 2 IRD (UMR-AMAP), University of Montpellier, 34090 Montpellier, France
 * 3 ICRAF, Bogor 16001, Indonesia
 * 4 INRA (UR ASTRO 1231) Centre Antilles-Guyane, Petit-Bourg, 97170 Guadeloupe, France
 * 5 CEH, NERC,Wallingford OX10 8BB, UK
 * 6 INRA (UMR-PIAF), Université Clermont Auvergne, 63000 Clermont-Ferrand, France
 * 7 Centre d’étude de la forêt, Université du Quebec, Montreal H2X 3Y5, Canada
 * 8 CEMAGREF, Mountain Ecosystems and Landcapes Research Unit, Saint-Martin-d’Hères, France
 *
 *----------------------------------------------------------------------------------------------
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

/**
 * SafeSticsStation - JNA mirror object for STICS weather station
 *                    This object have to be the exact mirror of the FORTRAN Station.f90 
 * 
 * @author Isabelle Lecomte - December 2016
 */

public class SafeSticsStation extends Structure implements Serializable {

	  private static final long serialVersionUID = 1L;
	
	  public int P_codecaltemp;   // PARAMETER // option of use of crop temperature for phasic development calculation : yes (2), no (1)  // code 1/2 // STATION // 0
	  public int P_codernet;   // PARAMETER // option of calculation of net radiation // code 1/2/3 // STATION // 0
	  public int P_codeclichange;   // PARAMETER // option for climatel change : yes (2), no (1)  // code 1/2 // STATION // 0
	  public float P_zr;   // PARAMETER // Reference height of meteorological data measurement // m // STATION // 0
	  public float P_ra;   // PARAMETER // Aerodynamic resistance (used in volatilization  module when we use ETP approach) // s m-1 // STATION // 1 	!zarbi a voir avce marie  // OUTPUT // Aerodynamic resistance between the cover and the reference level P_zr // s.m-1
	  public float P_NH3ref;   // PARAMETER // NH3 concentration in the atmosphere // ug.m-3 // STATION // 1
	  public float P_aangst;   // PARAMETER // coefficient of the Angstrom's relationship for extraterrestrial radiation // SD // STATION // 1
	  public float P_bangst;   // PARAMETER // coefficient of the Angstrom's relationship for extraterrestrial radiation // SD // STATION // 1
	  public float P_coefdevil;   // PARAMETER // multiplier coefficient of the exterior radiation to compute PET inside of a greenhouse // SD // STATION // 1
	  public float P_albveg;   // PARAMETER // P_albedo of the vegetation // SD // STATION // 1
	  public float P_altistation;   // PARAMETER // altitude of the input metorological station  // m // STATION // 0
	  public float P_altisimul;   // PARAMETER // altitude of simulation // m // STATION // 0
	  public float P_gradtn;   // PARAMETER // thermal gradient in altitude for minimal temperatures  // degree C m-1 // STATION // 1
	  public float P_gradtx;   // PARAMETER // thermal gradient in altitude for maximal temperatures  // degree C m-1 // STATION // 1
	  public float P_altinversion;   // PARAMETER // altitude of inversion of the thermal gradiant // m // STATION // 1
	  public float P_gradtninv;   // PARAMETER // thermal gradient in altitude for minimal temperatures under the inversion level // degree C m-1 // STATION // 1
	  public float P_cielclair;   // PARAMETER // threshold for the proportion of sunny hours allowing the inversion of thermal gradiant with altitude // SD // STATION // 1
	  public float P_ombragetx;   // PARAMETER // shadow effect to calculate the thermal modification in the northern parts of montains  // degree C // STATION // 1
	  public float P_latitude;   // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0
	  public float P_aks;   // PARAMETER // parameter of calculation of the energetic lost between the inside and the outside of a greenhouse  // Wm-2K-1 // STATION // 1
	  public float P_bks;   // PARAMETER // parameter of calculation of the energetic lost between the inside and the outside of a greenhouse  // Wm-2K-1 // STATION // 1
	  public float P_cvent;   // PARAMETER // parameter of the climate calculation under the shelter // SD // STATION // 1
	  public float P_phiv0;   // PARAMETER // parameter allowing the calculation of the under shelter climate // * // STATION // 1
	  public float P_coefrnet;   // PARAMETER // coefficient of calculation of the net radiation under greenhouse // * // STATION // 1
	  public float P_patm;   // PARAMETER // atmospheric pressure // mbars // STATION // 0
	  public float P_corecTrosee;   // PARAMETER // temperature to substract to Tmin to estimate dew point temperature (in case of missing air humidity data) // degree C // STATION // 1
	  public int P_codeetp;	  // PARAMETER // code of calculation mode of ETP [pe/pc/sw/pt] // code 1/2/3/4 // STATION // 0
	  public float P_alphapt;   // PARAMETER // Parameter of Priestley-Taylor  // SD // STATION // 1
	  public int P_codaltitude;   // PARAMETER // option of calculation of the climate in altitude // code 1/2 // STATION // 0
	  public int P_codadret;   // PARAMETER // option of calculation of climate in montain accounting for the orientation (1 : south, 2 : north) // code 1/2 // STATION // 0
	  public float P_aclim;   // PARAMETER // climatic component of A // mm // STATION // 1
	  public float ra_recal;   // OUTPUT // Aerodynamic resistance (used in volatilization  module when we use ETP approach) // s m-1

  
	public SafeSticsStation (double elevation, double latitude) {
	
		P_altisimul = (float) elevation; 
		P_latitude = (float) latitude; 
	}

	@Override
	protected List<String> getFieldOrder() {
		return Arrays.asList(new String[] { "P_codecaltemp", "P_codernet", "P_codeclichange", "P_zr", "P_ra",
				"P_NH3ref", "P_aangst", "P_bangst", "P_coefdevil", "P_albveg", "P_altistation", "P_altisimul",
				"P_gradtn", "P_gradtx", "P_altinversion", "P_gradtninv", "P_cielclair", "P_ombragetx", "P_latitude",
				"P_aks", "P_bks", "P_cvent", "P_phiv0", "P_coefrnet", "P_patm", "P_corecTrosee", "P_codeetp",
				"P_alphapt", "P_codaltitude", "P_codadret", "P_aclim", "ra_recal" });
	}
	    
	 
}

	
	
  

