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
 * SafeSticsClimat - JNA mirror object for STICS climate 
 *                   This object have to be the exact mirror of the FORTRAN Climat.f90 
 * 
 * @author Isabelle Lecomte - December 2016
 */

public class SafeSticsClimat extends Structure implements Serializable {
	  
	private static final long serialVersionUID = 1L;

	public int nometp;

	  public float tetp[]; 	  // Efficient potential evapotranspiration (entered or calculated) // mm day-1
	  public float trr[];     // Rainfall  // mm.day-1
	  public float tmoy[];    // Mean active temperature of air // degree C
	  public float tmin[];    // Minimum active temperature of air // degree C
	  public float tmax[];    // Maximum active temperature of air // degree C
	  public float trg[]; 	  // Active radiation (entered or calculated) // MJ.m-2
	  public float tpm[];  	  //  Vapour pressure in air // mbars
	  public float tvent[];   //  Mean speed of B2vent // m.s-1
	  public float co2[];      // CO2 concentration // ppm
	  public float trrext[];	  // Exterior Rainfall  // mm.day-1
	  public float tmoyext[];  	  // Mean temperature of external air // degree C
	  public float tminext[];  	  // Minimum temperature of external air // degree C
	  public float tmaxext[];  	  // Maximum temperature of external air // degree C
	  public float trgext[];  	  // Exterior radiation // MJ.m-2
	  public float tpmext[];
	  public float tutilrnet;  
	  public float difftcult;  
	  public float daylen;  
	  public float humimoy;  
	  public float humair;      // Air moisture // 0-1
	  public float phoi;        // Photoperiod // hours
	  public float etpp[];      // Potential evapotranspiration as given by Penman formula // mm day-1
	  public float dureehumec;  // wetness duration    // hour
	  public float dureeRH1;
	  public float dureeRH2;
	  public float dureeRH;     	// duration of night relative humidity higher than a given threshold   // hour
	  public float Ctculttout;    	// Crop temperature (TCULT) integrated over the simulation period //  degree C
	  public float Ctairtout;  
	  public float somdifftculttair;    // Cumulated temperature differences (TCULT-TAIR) over the simulation period //  degree C
	  public float Ctetptout;    	// Potential evapotranspiration (PET) integrated over the simulation period // mm
	  public float Cetmtout;    	// Maximum evapotranspiration integrated over the simulation period // mm
	  public float Crgtout;    		// Global radiation integrated over the simulation period // Mj/m2
	  public float tncultmat;    	// Average of minimum crop temperatures (TCULTMIN) between LAX and REC // degree C
	  public float amptcultmat;     //Mean range of tcult between lax and rec // degree C
	  public int dureelaxrec;  
	  public int nbjechaudage;     	// umber of shrivelling days between LAX and REC // jours
	  public int julfin;  
	  public int julzero;  
	  public int jourzero;  
	  public int jourfin;  
	  public int moiszero;  
	  public int moisfin;  
	  public int anneezero;  
	  public int anneefin;  
	  public float humair_percent;    // Air moisture // %

	public SafeSticsClimat () {
		
		tetp = new float[366];     
		trr = new float[366];    
		tmoy = new float[367];     
		tmin = new float[366];    
		tmax = new float[366];   
		trg = new float[366];    
		tpm = new float[366];     
		tvent = new float[366]; 
		co2 = new float[366];  
		trrext = new float[366];   
		tmoyext  = new float[367];  	  
		tminext = new float[366];  
		tmaxext = new float[366];  
		trgext = new float[366];  
		tpmext = new float[366];   
		etpp = new float[367];   
	
	}

	public SafeSticsClimat (SafeSticsClimat origin) {
		
	  this.tutilrnet = origin.tutilrnet;  
	  this.difftcult = origin.difftcult;  
	  this.daylen = origin.daylen;  
	  this.humimoy = origin.humimoy;  
	  this.humair = origin.humair;      
	  this.phoi = origin.phoi;      
	  this.dureehumec = origin.dureehumec;
	  this.dureeRH1 = origin.dureeRH1;
	  this.dureeRH2 = origin.dureeRH2;
	  this.dureeRH = origin.dureeRH;     	
	  this.Ctculttout = origin.Ctculttout;    
	  this.Ctairtout = origin.Ctairtout;  
	  this.somdifftculttair = origin.somdifftculttair;   
	  this.Ctetptout = origin.Ctetptout;    
	  this.Cetmtout = origin.Cetmtout;    
	  this.Crgtout = origin.Crgtout;    		
	  this.tncultmat = origin.tncultmat;    	
	  this.amptcultmat = origin.amptcultmat;   
	  this.dureelaxrec = origin.dureelaxrec;  
	  this.nbjechaudage = origin.nbjechaudage;     	
	  this.julfin = origin.julfin;  
	  this.julzero = origin.julzero;  
	  this.jourzero = origin.jourzero;  
	  this.jourfin = origin.jourfin;  
	  this.moiszero = origin.moiszero;  
	  this.moisfin = origin.moisfin;  
	  this.anneezero = origin.anneezero;  
	  this.anneefin = origin.anneefin;  
	  this.humair_percent = origin.humair_percent;    
		  
		  
		tetp = new float[366];     
		trr = new float[366];    
		tmoy = new float[367];     
		tmin = new float[366];    
		tmax = new float[366];   
		trg = new float[366];    
		tpm = new float[366];     
		tvent = new float[366]; 
		co2 = new float[366];  
		trrext = new float[366];   
		tmoyext  = new float[367];  	  
		tminext = new float[366];  
		tmaxext = new float[366];  
		trgext = new float[366];  
		tpmext = new float[366];   
		etpp = new float[367]; 
		
		System.arraycopy(origin.tetp 	, 0, this.tetp 	, 0, 	366);
		System.arraycopy(origin.trr 	, 0, this.trr 	, 0, 	366);
		System.arraycopy(origin.tmoy 	, 0, this.tmoy 	, 0, 	367);
		System.arraycopy(origin.tmin 	, 0, this.tmin 	, 0, 	366);
		System.arraycopy(origin.tmax 	, 0, this.tmax 	, 0, 	366);
		System.arraycopy(origin.trg 	, 0, this.trg 	, 0, 	366);
		System.arraycopy(origin.tpm 	, 0, this.tpm 	, 0, 	366);
		System.arraycopy(origin.tvent 	, 0, this.tvent 	, 0, 	366);
		System.arraycopy(origin.co2 	, 0, this.co2 	, 0, 	366);
		System.arraycopy(origin.trrext 	, 0, this.trrext 	, 0, 	366);
		System.arraycopy(origin.tmoyext 	, 0, this.tmoyext 	, 0, 	367);
		System.arraycopy(origin.tminext 	, 0, this.tminext 	, 0, 	366);
		System.arraycopy(origin.tmaxext 	, 0, this.tmaxext 	, 0, 	366);
		System.arraycopy(origin.trgext 	, 0, this.trgext 	, 0, 	366);
		System.arraycopy(origin.tpmext 	, 0, this.tpmext 	, 0, 	366);
		System.arraycopy(origin.etpp 	, 0, this.etpp 	, 0, 	367);

	}
	
	@Override
	protected List<String> getFieldOrder() {
		return Arrays.asList(new String[] { "nometp", "tetp", "trr", "tmoy", "tmin", "tmax", "trg", "tpm", "tvent", "co2", "trrext", "tmoyext",
				"tminext", "tmaxext", "trgext", "tpmext", "tutilrnet", "difftcult", "daylen", "humimoy",
				"humair", "phoi", "etpp", "dureehumec", "dureeRH1", "dureeRH2", "dureeRH", "Ctculttout", "Ctairtout",
				"somdifftculttair", "Ctetptout", "Cetmtout", "Crgtout", "tncultmat", "amptcultmat", "dureelaxrec",
				"nbjechaudage", "julfin", "julzero", "jourzero", "jourfin", "moiszero", "moisfin", "anneezero",
				"anneefin", "humair_percent" });
    }
	    
 
}

	
	
  

