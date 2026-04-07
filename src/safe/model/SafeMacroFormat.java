/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree�Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE - CC-BY License
 * 
 * LIST OF AUTHORS
 * --------------- 
 * Christian Dupraz 1, Kevin J.Wolz 1 , Isabelle Lecomte 1, Gr�goire Talbot 1, Nicolas Barbault 1, 
 * Gr�goire Vincent 2 , Rachmat Mulia 3, Fran�ois Bussi�re 4, Harry Ozier-Lafontaine 4,
 * Sitraka Andrianarisoa 1, Nick Jackson 5, Gerry Lawson 5, Nicolas Dones 6, Herv� Sinoquet 6,
 * Betha Lusiana 3, Degi Harja 3, Suzy Domenicano 7 , Francesco Reyes 1 , Marie Gosme 1 ,
 * Meine Van Noordwijk 3, Benoit Courbaud 8
 *
 * 1 INRA (UMR-ABSYS), University of Montpellier, 34090 Montpellier, France
 * 2 IRD (UMR-AMAP), University of Montpellier, 34090 Montpellier, France
 * 3 ICRAF, Bogor 16001, Indonesia
 * 4 INRA (UR ASTRO 1231) Centre Antilles-Guyane, Petit-Bourg, 97170 Guadeloupe, France
 * 5 CEH, NERC,Wallingford OX10 8BB, UK
 * 6 INRA (UMR-PIAF), Universit� Clermont Auvergne, 63000 Clermont-Ferrand, France
 * 7 Centre d��tude de la for�t, Universit� du Quebec, Montreal H2X 3Y5, Canada
 * 8 CEMAGREF, Mountain Ecosystems and Landcapes Research Unit, Saint-Martin-d�H�res, France
 *
 *----------------------------------------------------------------------------------------------
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

package safe.model;

import java.util.Iterator;

import jeeb.lib.util.Import;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;

/**
 * MACRO CLIMAT parameters format for reading in a file 
 * First format possible with real year , a second without
 *
 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
 */
public class SafeMacroFormat extends RecordSet {

	@Import
	/**
	 * First type of climate record
	 */
	static public class ClimatRecord extends Record {
		public ClimatRecord () {super ();}
		public ClimatRecord (String line) throws Exception {super (line);}
		
		/** Number of the day in the year (1 to 365) */
		public int julianDay;			
		/** Year YYYY */
		public int year;				
		/** Month MM (1 to 12) */
		public int month;				
		/** Day DD (1 to 31)  */
		public int day;					
		/** Temperature max (degree) */
		public float tmax;				
		/** Temperature min (degree) */
		public float tmin;				
		/** Relative humidity max (%)*/
		public float rhmax;				
		/** Relative humidity min (%)*/
		public float rhmin;				
		/** Global radiation (MJ m-2) */
		public float globalRadiation;	
		/** Precipitation (mm)*/
		public float rain;				
		/** Wind speed (m s-1) */
		public float windSpeed;		
		/** water table depth (m) */
		public float waterTableDepth;	
		/** CO2 concentration (ppm) */
		public float co2Concentration;	
		/** Real year YYYY (in case it is a copy of a missing climate measurement)  */
		public int realYear;		
	}

	@Import
	/**
	 * Second type of climate record
	 */
	static public class ClimatRecord2 extends Record {
		public ClimatRecord2 () {super ();}
		public ClimatRecord2 (String line) throws Exception {super (line);}

		/** Number of the day in the year (1 to 365) */
		public int julianDay;			
		/** Year YYYY */
		public int year;				
		/** Month MM (1 to 12) */
		public int month;				
		/** Day DD (1 to 31)  */
		public int day;					
		/** Temperature max (degree) */
		public float tmax;				
		/** Temperature min (degree) */
		public float tmin;				
		/** Relative humidity max (%)*/
		public float rhmax;				
		/** Relative humidity min (%)*/
		public float rhmin;				
		/** Global radiation (MJ m-2) */
		public float globalRadiation;	
		/** Precipitation (mm)*/
		public float rain;				
		/** Wind speed (m s-1) */
		public float windSpeed;		
		/** water table depth (m) */
		public float waterTableDepth;	
		/** CO2 concentration (ppm) */
		public float co2Concentration;	
	}
	
	/**
	 * Constructor
	 * @param climatFileName Name of the climate entry file
	 */
	public SafeMacroFormat (String climatFileName) throws Exception {prepareImport (climatFileName);}

	/**
	 * Load climate data to SafeMacroClimat
	 * @param generalParameters Reference to SafeGeneralParameters object
	 * @param climat Reference to SafeMacroClimat object
	 * @param latitude Latitude of the plot
	 */
	public void load(SafeGeneralParameters generalParameters,
					 SafeMacroClimat climat,
					 double latitude) throws Exception {

		for (Iterator<Record> i = this.iterator(); i.hasNext();) {
			Record record =  i.next();

			if (record instanceof SafeMacroFormat.ClimatRecord) {
			
				SafeMacroFormat.ClimatRecord cr =
							(SafeMacroFormat.ClimatRecord) record;	// cast to precise type

				if (cr.tmax < cr.tmin) {
					System.out.println ("Climat error : tmax  < tmin  "+cr.year+"/"+cr.month+"/"+cr.day);	
					throw new Exception ("Weather error");	// automatic toString () (or null)
				}
				if (cr.rhmax < cr.rhmin) {
					System.out.println ("Climat error : rhmax  < rhmin  "+cr.year+"/"+cr.month+"/"+cr.day);	
					throw new Exception ("Weather error");	// automatic toString () (or null)
				}	
				if (cr.globalRadiation <= 0) {
					System.out.println ("Climat error : globalRadiation <= 0  "+cr.year+"/"+cr.month+"/"+cr.day);	
					throw new Exception ("Weather error");	// automatic toString () (or null)
				}		
				if (cr.windSpeed < 0) {
					System.out.println("Climat error : windSpeed < 0  "+cr.year+"/"+cr.month+"/"+cr.day);	
					throw new Exception ("Weather error");	// automatic toString () (or null)
				}
				if (cr.rain < 0) {
					System.out.println ("Climat error : rain < 0  "+cr.year+"/"+cr.month+"/"+cr.day);	
					throw new Exception ("Weather error");	// automatic toString () (or null)
				}	
				if (cr.co2Concentration < 0) {
					System.out.println ("Climat error : co2Concentration < 0  "+cr.year+"/"+cr.month+"/"+cr.day);	
					throw new Exception ("Weather error");	// automatic toString () (or null)
				}			
				if (cr.realYear==0) cr.realYear = cr.year;
				
				climat.createDailyClimat(generalParameters,  (float)latitude,
										  cr.julianDay, cr.year , cr.realYear, cr.month, cr.day,
										  cr.tmin, cr.tmax, cr.rhmin, cr.rhmax, cr.globalRadiation,
										  cr.rain, cr.windSpeed, cr.waterTableDepth, cr.co2Concentration);


			}
			else if (record instanceof SafeMacroFormat.ClimatRecord2) {
				

				SafeMacroFormat.ClimatRecord2 cr =
							(SafeMacroFormat.ClimatRecord2) record;	// cast to precise type

				if (cr.tmax < cr.tmin) {
					System.out.println ("Climat error : tmax  < tmin  "+cr.year+"/"+cr.month+"/"+cr.day);	
					throw new Exception ("Weather error");	// automatic toString () (or null)
				}
				if (cr.rhmax < cr.rhmin) {
					System.out.println ("Climat error : rhmax  < rhmin  "+cr.year+"/"+cr.month+"/"+cr.day);
					throw new Exception ("Weather error");	// automatic toString () (or null)
				}	
				if (cr.globalRadiation <= 0) {
					System.out.println ("Climat error : globalRadiation <= 0  "+cr.year+"/"+cr.month+"/"+cr.day);
					throw new Exception ("Weather error");	// automatic toString () (or null)
				}		
				if (cr.windSpeed < 0) {
					System.out.println ("Climat error : windSpeed < 0  "+cr.year+"/"+cr.month+"/"+cr.day);	
					throw new Exception ("Weather error");	// automatic toString () (or null)
				}
				if (cr.rain < 0) {
					System.out.println ("Climat error : rain < 0  "+cr.year+"/"+cr.month+"/"+cr.day);	
					throw new Exception ("Weather error");	// automatic toString () (or null)
				}	
				if (cr.co2Concentration < 0) {
					System.out.println ("Climat error : co2Concentration < 0  "+cr.year+"/"+cr.month+"/"+cr.day);	
					throw new Exception ("Weather error");	// automatic toString () (or null)
				}
						
				climat.createDailyClimat(generalParameters,  (float) latitude,
						  cr.julianDay, cr.year , cr.year, cr.month, cr.day,
						  cr.tmin, cr.tmax, cr.rhmin, cr.rhmax, cr.globalRadiation,
						  cr.rain, cr.windSpeed, cr.waterTableDepth, cr.co2Concentration);

			} else {
				System.out.println ("Unrecognized record : "+record);	// automatic toString () (or null)
				throw new Exception ("Weather error");	// automatic toString () (or null)

			}	
		}
	}
}
