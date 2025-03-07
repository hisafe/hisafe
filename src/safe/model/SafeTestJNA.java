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

package safe.model;

import safe.stics.*;

import java.io.Serializable;
import com.sun.jna.Library;
import com.sun.jna.Native;

/**
 * SafeTestJNA is the class for calling native functions in STICS FORTRAN via JNA 
 * 
 * Available native functions are : 
 * =================================
 * verifParam : general parameters initialization verification
 * verifPlante : plants and itk parameters initialization verification
 * initClimat : climatic data initialization
 * initBoucleAnnuelle : annual loop initialization
 * apport : add carbon litters from tree
 * boucleJour1 : daily loop part 1
 * boucleJour2 : daily loop part 2
 * finBoucleAnnuelle : annual loop end process
 * 
 * @author Isabelle Lecomte - INRA Montpellier France  - November 2016
 */


public class SafeTestJNA implements  Serializable {


	private static final long serialVersionUID = 1L;


	public interface TestJNA extends Library {

		// nb-26.01.2018
		// Loads the gfortran library.
		// Needed in order the functions of SticsV8 library that depend on gfortran library can be called.
		// Indeed, without this instruction, the gfortran library should have been placed in classical paths (/lib or 
		// /usr/lib for Linux) or the LD_LIBRARY_PATH environment variable (for Linux) or the PATH variable (for 
		// Windows) should have contained the path to the directory containing the gfortran library (capsis4/ext for example).
		// Windows) should have contained the path to the directory containing the gfortran library (capsis4/ext for example). 
//		TestJNA INSTANCE_GFORTRAN = (TestJNA) Native.loadLibrary("gfortran", TestJNA.class);

//		if ( System.getProperty("os.name").equals("Linux")  ) {
//			TestJNA INSTANCE_GFORTRAN = (TestJNA) Native.loadLibrary("gfortran", TestJNA.class);	 
//		}
		

		// Loads the SticsV8.so library (libSticsV8.so under Linux, SticsV8.dll under Windows, libSticsV8.dylib under MacOS).
		TestJNA INSTANCE_STICS = (TestJNA) Native.loadLibrary("SticsV8", TestJNA.class);

		 void verifParam (SafeSticsParameters pg, SafeSticsTransit t, SafeSticsSoil soil, SafeSticsCommun sc, int lg, String outputDir);
		 void verifPlante (SafeSticsParameters pg, SafeSticsTransit t, SafeSticsCommun sc,  SafeSticsItk itk, SafeSticsCrop p, int zoneId, int lg, String outputDir);
		 void initClimat (SafeSticsParameters pg, SafeSticsTransit t, SafeSticsStation sta, SafeSticsClimat c);
		 void initBoucleAnnuelle (SafeSticsParameters pg, SafeSticsTransit t, SafeSticsStation sta, SafeSticsClimat c, SafeSticsCommun sc, SafeSticsSoil soil,  SafeSticsCrop p,SafeSticsItk itk, int cellId, int lg, String outputDir);
		 void apport (SafeSticsParameters pg,  SafeSticsSoil soil, SafeSticsCommun sc, SafeSticsCrop p, SafeSticsItk itk, float profmax, float carbonLitter, float cnLitter, float cfeupc, float waterLitter, int typeLitter);
		 void boucleJour1 (SafeSticsParameters pg, SafeSticsTransit t, SafeSticsStation sta, SafeSticsClimat c, SafeSticsCommun sc, SafeSticsSoil soil,  SafeSticsCrop p,SafeSticsItk itk, float cellVisibleSky, int flagFirst);
		 void boucleJour2 (SafeSticsParameters pg, SafeSticsTransit t, SafeSticsStation sta, SafeSticsClimat c, SafeSticsCommun sc, SafeSticsSoil soil,  SafeSticsCrop p,SafeSticsItk itk, int hisafeInfluence, float cellVisibleSky);
		 void finBoucleAnnuelle (SafeSticsParameters pg, SafeSticsTransit t, SafeSticsStation sta, SafeSticsClimat c, SafeSticsCommun sc, SafeSticsSoil soil,  SafeSticsCrop p,SafeSticsItk itk, int cellId);
	}

	/**
	 * verifParam : general parameters initialization verification
	 */
	 
    public static void verifParam(SafeSticsParameters pg, SafeSticsTransit t, SafeSticsSoil soil, SafeSticsCommun sc, String exportDir ) { 
    	
    	TestJNA.INSTANCE_STICS.verifParam (pg, t, soil, sc, exportDir.length(), exportDir);

		return;
   } 
    
    /**
     * verifPlante : plants and itk parameters initialization verification
     */ 
    public static void verifPlante(SafeSticsParameters pg, SafeSticsTransit t, SafeSticsCommun sc, 
						    		SafeSticsItk itk, SafeSticsCrop p, 
						    		int zoneId,
						    		String exportDir) { 
    	

    	//main = true ne sert plus il faudra l'enlever
   	 	TestJNA.INSTANCE_STICS.verifPlante (pg, t, sc, itk, p, zoneId, exportDir.length(), exportDir);

		return; 
   }  
    
    /**
     * initClimat : climatic data initialization
     */
    public static void initClimat(SafeSticsParameters pg, SafeSticsTransit t,
					    		SafeSticsStation sta, SafeSticsClimat c)  { 
    	

    	TestJNA.INSTANCE_STICS.initClimat (pg, t, sta, c); 

		return;
   }   
    
    /**
     * initBoucleAnnuelle : annual loop initialization
     */   
 
    public static void initBoucleAnnuelle(SafeSticsParameters pg, SafeSticsTransit t,
    								     SafeSticsStation sta, SafeSticsClimat c, 
    								     SafeSticsCommun sc, SafeSticsSoil soil,
    								     SafeSticsCrop p, SafeSticsItk itk,
    								     int dayStart, int dayEnd, 
    								     int cellId,
    								     String exportDir,
    								     boolean sticsReport
    								     ) { 
    	
	//	System.out.println("initBoucleAnnuelle start ="+dayStart+" end ="+dayEnd);
		
    	sc.P_iwater = dayStart;	
    	sc.P_ifwater = dayEnd;	
    	
    	sc.ifwater_courant =  dayEnd; 
    	sc.P_culturean = 1;
    	//culture sur 2 ans
    	if ((dayStart>1) && (dayStart+dayEnd>366)) sc.P_culturean = 0;
    	
    	//pas de sorties BILAN STICS par defaut 
    	pg.P_flagEcriture = 0;
    	if (sticsReport) pg.P_flagEcriture = 31;

    	TestJNA.INSTANCE_STICS.initBoucleAnnuelle (pg, t, sta, c, sc, soil, p, itk, cellId, exportDir.length(), exportDir);
	  
		return;     
   } 
    /**
     * apport : add carbon litters from tree
     */
    public static void apport(SafeSticsParameters pg,  
    						  SafeSticsSoil soil,
    						  SafeSticsCommun sc, 
    						  SafeSticsCrop p, 
    						  SafeSticsItk itk,
    						  float profMax,
							  float	carbonLitter, 							   
							  float	cnLitter, 
							  float cfeupc, 
							  float  waterLitter,
							  int    typeLitter) { 
    	
        TestJNA.INSTANCE_STICS.apport (pg, soil, sc, p, itk, profMax, carbonLitter, cnLitter, cfeupc, waterLitter, typeLitter);
    
    }
    /**
     * boucleJour1 : daily loop part 1
     */
    public static void boucleJour1(SafeSticsParameters pg, SafeSticsTransit t,
							     SafeSticsStation sta, SafeSticsClimat c, 
							     SafeSticsCommun sc, SafeSticsSoil soil,
							     SafeSticsCrop p, SafeSticsItk itk,
							     int sticsDay, 
							     int julianDay,
							     double	cellRad, 							   
							     double	cellRain, 
							     double cellEtp,
							     double cellVisibleSky,
							     int flagFirst) { 
 
		//n=1 to 365
    	//julianDay = 1 to 364 or 290 to 654
    	//day = 1 to 365 
		

		
       sc.n = sticsDay;
       sc.jjul = julianDay;
       sc.jul = julianDay;
       if (sc.jul > 365) sc.jul = sc.jul - 365;
       sc.numdate = sc.jul;
		
       	//Tree influence on cell data
   		c.trg[sticsDay-1]    = (float) cellRad; 				  /* RG on crop after tree interception */		 
		c.trr[sticsDay-1]    = (float) cellRain;                  /* rain on crop after tree interception (mm) */
		c.tetp[sticsDay-1]   = (float) cellEtp;                   /* ETP on crop (mm) */

		//STICS HISAFE COMPARAISON
		//il faut arrondir à une decimale

		//c.trg[sticsDay-1] = (float)Math.round(cellRad * 10) / 10 ;
		//c.trr[sticsDay-1] = (float)Math.round(cellRain * 10) / 10 ;
		//c.tetp[sticsDay-1] = (float)Math.round(cellEtp * 10) / 10 ;

		float visibleSky = (float) cellVisibleSky;				//% of visible sky (1=100%)
		
		
		//RAZ Emulch (bug in STICS) 
		sc.Emulch=0;
		sc.drain=0;
		p.offrenod[1]=0;


       TestJNA.INSTANCE_STICS.boucleJour1 (pg, t, sta, c, sc, soil, p, itk, visibleSky, flagFirst);

		return;
    } 
    
    /**
     * boucleJour2 : daily loop part 2
     */ 
    public static void boucleJour2(SafeSticsParameters pg, SafeSticsTransit t,
		     SafeSticsStation sta, SafeSticsClimat c, 
		     SafeSticsCommun sc, SafeSticsSoil soil,
		     SafeSticsCrop p, SafeSticsItk itk,
		     int sticsDay, int julianDay,
		     int hisafeInfluence,
		     double cellVisibleSky
		    ) { 


	       	//Tree influence on cell data		
			float visibleSky = (float) cellVisibleSky;		//% of visible sky (1=100%)
			

	    	TestJNA.INSTANCE_STICS.boucleJour2 (pg, t, sta, c, sc, soil, p, itk, hisafeInfluence, visibleSky);
	   	  
  		}		


    /**
     * finBoucleAnnuelle : annual loop end process
     */   
    
    public static void finBoucleAnnuelle(SafeSticsParameters pg, SafeSticsTransit t,
								     SafeSticsStation sta, SafeSticsClimat c, 
								     SafeSticsCommun sc, SafeSticsSoil soil,
								     SafeSticsCrop p, SafeSticsItk itk,
								     int cellId) { 

		TestJNA.INSTANCE_STICS.finBoucleAnnuelle (pg, t, sta, c, sc, soil, p, itk, cellId);

		return;
    }   
  
}
