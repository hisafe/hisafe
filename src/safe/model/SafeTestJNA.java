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
 * verifPlant : plants and itk parameters initialization verification
 * initClimat : climatic data initialization
 * annualLoopStart : annual loop initialization
 * addLitterInSoil : add carbon litters from tree (branches, stem, leaves, roots) 
 * dailyLoopPart1 : daily loop part 1
 * dailyLoopPart2 : daily loop part 2
 * annualLoopEnd : annual loop end process
 * 
 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
 */

public class SafeTestJNA implements  Serializable {

	private static final long serialVersionUID = 1L;

	public interface TestJNA extends Library {

		// Loads the SticsV8.so library (libSticsV8.so under Linux, SticsV8.dll under Windows, libSticsV8.dylib under MacOS).
		TestJNA INSTANCE_STICS = (TestJNA) Native.loadLibrary("SticsV8", TestJNA.class);

		 void verifParam (SafeSticsParameters param, SafeSticsTransit transit, SafeSticsSoil soil, SafeSticsCommun commun, int lg, String outputDir);
		 void verifPlant (SafeSticsParameters param, SafeSticsTransit transit, SafeSticsCommun commun,  SafeSticsItk itk, SafeSticsCrop plant, int zoneId, int lg, String outputDir);
		 void initClimat (SafeSticsParameters param, SafeSticsTransit transit, SafeSticsStation sta, SafeSticsClimat climat);
		 void annualLoopStart (SafeSticsParameters param, SafeSticsTransit transit, SafeSticsStation sta, SafeSticsClimat climat, SafeSticsCommun commun, SafeSticsSoil soil,  SafeSticsCrop plant,SafeSticsItk itk, int cellId, int lg, String outputDir);
		 void addLitterInSoil (SafeSticsParameters param,  SafeSticsSoil soil, SafeSticsCommun commun, SafeSticsCrop plant, SafeSticsItk itk, float profmax, float carbonLitter, float cnLitter, float cfeupc, float waterLitter, int typeLitter);
		 void dailyLoopPart1 (SafeSticsParameters param, SafeSticsTransit transit, SafeSticsStation sta, SafeSticsClimat climat, SafeSticsCommun commun, SafeSticsSoil soil,  SafeSticsCrop plant,SafeSticsItk itk, float cellVisibleSky, int flagFirst);
		 void dailyLoopPart2 (SafeSticsParameters param, SafeSticsTransit transit, SafeSticsStation sta, SafeSticsClimat climat, SafeSticsCommun commun, SafeSticsSoil soil,  SafeSticsCrop plant,SafeSticsItk itk, int hisafeWaterExtraction, float cellVisibleSky);
		 void annualLoopEnd (SafeSticsParameters param, SafeSticsTransit transit, SafeSticsStation sta, SafeSticsClimat climat, SafeSticsCommun commun, SafeSticsSoil soil,  SafeSticsCrop plant,SafeSticsItk itk, int cellId);
	}

	/**
	 * STICS general parameters initialization verification
	 * @param param Reference on SafeSticsParameters object
	 * @param transit Reference on SafeSticsTransit object
	 * @param soil Reference on SafeSticsSoil object
	 * @param commun Reference on SafeSticsCommun object
	 * @param exportDir Name of the export folder
	 */
	 
    public static void verifParam  (SafeSticsParameters param, 
						    		SafeSticsTransit transit, 
						    		SafeSticsSoil soil, 
						    		SafeSticsCommun commun, 
						    		String exportDir ) { 
    	
    	TestJNA.INSTANCE_STICS.verifParam (param, transit, soil, commun, exportDir.length(), exportDir);

		return;
   } 
    
    /**
     * STICS plants and itk parameters initialization verification
 	 * @param param Reference on SafeSticsParameters object
	 * @param transit Reference on SafeSticsTransit object
	 * @param commun Reference on SafeSticsCommun object
	 * @param itk Reference on SafeSticsItk object
	 * @param plant Reference on SafeSticsCrop object
	 * @param zoneId ID of the zone
	 * @param exportDir Name of the export folder
     */ 
    public static void verifPlant  (SafeSticsParameters param, 
						    		SafeSticsTransit transit, 
						    		SafeSticsCommun commun, 
						    		SafeSticsItk itk, 
						    		SafeSticsCrop plant, 
						    		int zoneId,
						    		String exportDir) { 
    	
   	 	TestJNA.INSTANCE_STICS.verifPlant (param, transit, commun, itk, plant, zoneId, exportDir.length(), exportDir);

		return; 
   }  
    
    /**
     * STICS climatic data initialization
  	 * @param param Reference on SafeSticsParameters object
	 * @param transit Reference on SafeSticsTransit object
	 * @param sta Reference on SafeSticsStation object
	 * @param climat Reference on SafeSticsClimat object
     */
    public static void initClimat  (SafeSticsParameters param, 
    								SafeSticsTransit transit,
    								SafeSticsStation sta, 
    								SafeSticsClimat climat)  { 
    	
    	TestJNA.INSTANCE_STICS.initClimat (param, transit, sta, climat); 

		return;
   }   
    
    /**
     * STICS annual loop initialization
   	 * @param param Reference on SafeSticsParameters object
	 * @param transit Reference on SafeSticsTransit object
	 * @param sta Reference on SafeSticsStation object
	 * @param climat Reference on SafeSticsClimat object
	 * @param commun Reference on SafeSticsCommun object
	 * @param soil Reference on SafeSticsSoil object
	 * @param plant Reference on SafeSticsCrop object
	 * @param itk Reference on SafeSticsItk object
	 * @param dayStart Julian day start of simulation
	 * @param dayEnd Julian day end of simulation
	 * @param cellId ID of the cell
	 * @param exportDir Name of the export folder
	 * @param sticsReport true if report generation
     */   
 
    public static void annualLoopStart  (SafeSticsParameters param, 
    									 SafeSticsTransit transit,
    								     SafeSticsStation sta, 
    								     SafeSticsClimat climat, 
    								     SafeSticsCommun commun, 
    								     SafeSticsSoil soil,
    								     SafeSticsCrop plant, 
    								     SafeSticsItk itk,
    								     int dayStart, 
    								     int dayEnd, 
    								     int cellId,
    								     String exportDir,
    								     boolean sticsReport
    								     ) { 
    	
    	commun.P_iwater = dayStart;	
    	commun.P_ifwater = dayEnd;	
    	
    	commun.ifwater_courant =  dayEnd; 
    	commun.P_culturean = 1;
    	//culture sur 2 ans
    	if ((dayStart>1) && (dayStart+dayEnd>366)) commun.P_culturean = 0;
    	
    	//pas de sorties BILAN STICS par defaut 
    	param.P_flagEcriture = 0;
    	if (sticsReport) param.P_flagEcriture = 31;

    	TestJNA.INSTANCE_STICS.annualLoopStart (param, transit, sta, climat, commun, soil, plant, itk, cellId, exportDir.length(), exportDir);
	  
		return;     
   } 
    /**
     * Add carbon litters from tree (branches - stem - leaves - roots) in soil
     * @param param Reference on SafeSticsParameters object
  	 * @param soil Reference on SafeSticsSoil object
	 * @param commun Reference on SafeSticsCommun object
	 * @param plant Reference on SafeSticsCrop object
	 * @param itk Reference on SafeSticsItk object
	 * @param profMax Max depth of residues incorporation
	 * @param carbonLitter Fresh matter (FM) added from residue (t.ha-1)
	 * @param cnLitter C/N ratio of residue
	 * @param cfeupc C content of residue  (%FM)
	 * @param waterLitter Water content of residue   (%FM)
	 * @param typeLitter Type of litter
     */
    public static void addLitterInSoil   (SafeSticsParameters param,  
			    						  SafeSticsSoil soil,
			    						  SafeSticsCommun commun, 
			    						  SafeSticsCrop plant, 
			    						  SafeSticsItk itk,
			    						  float profMax,
										  float	carbonLitter, 							   
										  float	cnLitter, 
										  float cfeupc, 
										  float  waterLitter,
										  int    typeLitter) { 
    	
        TestJNA.INSTANCE_STICS.addLitterInSoil (param, soil, commun, plant, itk, profMax, carbonLitter, cnLitter, cfeupc, waterLitter, typeLitter);
    
    }
    /**
     * STICS daily loop part 1
     * @param param Reference on SafeSticsParameters object
     * @param transit Reference on SafeSticsTransit object
     * @param sta Reference on SafeSticsStation object
     * @param climat Reference on SafeSticsClimat object
	 * @param commun Reference on SafeSticsCommun object
	 * @param soil Reference on SafeSticsSoil object
	 * @param plant Reference on SafeSticsCrop object
	 * @param itk Reference on SafeSticsItk object
	 * @param simulationJulianDay Julian of the simulation
	 * @param sticsJulianDay Julian day of the simulation for STICS (can be different from simulationJulianDay)
	 * @param cellRad Radiation on this cell (MJ m-2)
	 * @param cellRain Rain on this cell (mm)
	 * @param cellEtp ETP on this cell (mm)
	 * @param cellVisibleSky Visible sky on this cell (ratio)
	 * @param flagFirst True if this cell is the first one of the same ZONE
     */
    public static void dailyLoopPart1   (SafeSticsParameters param, 
    									 SafeSticsTransit transit,
									     SafeSticsStation sta, 
									     SafeSticsClimat climat, 
									     SafeSticsCommun commun, 
									     SafeSticsSoil soil,
									     SafeSticsCrop plant, 
									     SafeSticsItk itk,
									     int simulationJulianDay,
									     int sticsJulianDay, 						    
									     double	cellRad, 							   
									     double	cellRain, 
									     double cellEtp,
									     double cellVisibleSky,
									     int flagFirst) { 
 
    	commun.n = sticsJulianDay;
    	commun.jjul = simulationJulianDay;
    	commun.jul = simulationJulianDay;
        if (commun.jul > 365) commun.jul = commun.jul - 365;
        commun.numdate = commun.jul;
		
       	//Tree influence on cell data
        climat.trg[sticsJulianDay-1]    = (float) cellRad; 				  /* RG on crop after tree interception */		 
        climat.trr[sticsJulianDay-1]    = (float) cellRain;                  /* rain on crop after tree interception (mm) */
        climat.tetp[sticsJulianDay-1]   = (float) cellEtp;                   /* ETP on crop (mm) */

		//STICS HISAFE COMPARAISON
		//il faut arrondir à une decimale

		//climat.trg[sticsDay-1] = (float)Math.round(cellRad * 10) / 10 ;
		//climat.trr[sticsDay-1] = (float)Math.round(cellRain * 10) / 10 ;
		//climat.tetp[sticsDay-1] = (float)Math.round(cellEtp * 10) / 10 ;

		float visibleSky = (float) cellVisibleSky;				//% of visible sky (1=100%)
		
		
		//RAZ Emulch (bug in STICS) 
		commun.Emulch=0;
		commun.drain=0;
		plant.offrenod[1]=0;

       TestJNA.INSTANCE_STICS.dailyLoopPart1 (param, transit, sta, climat, commun, soil, plant, itk, visibleSky, flagFirst);

		return;
    } 
    
    /**
     * STICS daily loop part 2
     * @param param Reference on SafeSticsParameters object
     * @param transit Reference on SafeSticsTransit object
     * @param sta Reference on SafeSticsStation object
     * @param climat Reference on SafeSticsClimat object
	 * @param commun Reference on SafeSticsCommun object
	 * @param soil Reference on SafeSticsSoil object
	 * @param plant Reference on SafeSticsCrop object
	 * @param itk Reference on SafeSticsItk object
	 * @param hisafeWaterExtraction Water extraction is calculated by Hisafe 0=no 1=yes
	 * @param cellVisibleSky Visible sky on this cell (ratio)

     */ 
    public static void dailyLoopPart2 ( SafeSticsParameters param, 
    								 	SafeSticsTransit transit,
    								 	SafeSticsStation sta, 
    								 	SafeSticsClimat climat, 
    								 	SafeSticsCommun commun, 
    								 	SafeSticsSoil soil,
    								 	SafeSticsCrop plant, 
    								 	SafeSticsItk itk,
    								 	int hisafeWaterExtraction,
    								 	double cellVisibleSky) { 
	
			float visibleSky = (float) cellVisibleSky;		//% of visible sky (1=100%)

	    	TestJNA.INSTANCE_STICS.dailyLoopPart2 (param, transit, sta, climat, commun, soil, plant, itk, hisafeWaterExtraction, visibleSky);
	   	  
  		}		


    /**
     * STICS annual loop end
     * @param param Reference on SafeSticsParameters object
     * @param transit Reference on SafeSticsTransit object
     * @param sta Reference on SafeSticsStation object
     * @param climat Reference on SafeSticsClimat object
	 * @param commun Reference on SafeSticsCommun object
	 * @param soil Reference on SafeSticsSoil object
	 * @param plant Reference on SafeSticsCrop object
	 * @param itk Reference on SafeSticsItk object
	 * @param cellId ID of the call
     */   
    
    public static void annualLoopEnd    (SafeSticsParameters param, 
    									 SafeSticsTransit transit,
									     SafeSticsStation sta, 
									     SafeSticsClimat climat, 
									     SafeSticsCommun commun, 
									     SafeSticsSoil soil,
									     SafeSticsCrop plant, 
									     SafeSticsItk itk,
									     int cellId) { 

		TestJNA.INSTANCE_STICS.annualLoopEnd (param, transit, sta, climat, commun, soil, plant, itk, cellId);

		return;
    }   
  
}
