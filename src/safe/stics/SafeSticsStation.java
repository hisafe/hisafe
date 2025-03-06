/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2024 INRAE 
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
 * Hi-SAFE is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Hi-SAFE is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * If not, see <http://www.gnu.org/licenses/>.
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

  
	public SafeSticsStation () {
	
	
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

	
	
  

