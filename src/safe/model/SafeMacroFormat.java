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

package safe.model;

import java.util.Iterator;
import jeeb.lib.util.Import;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;
import safe.stics.*;

/**
 * MACRO CLIMAT parameters format for reading in a file 
 *
 * @author Isabelle Lecomte - INRAE Montpellier - January 2003
 */
public class SafeMacroFormat extends RecordSet {


	private static final long serialVersionUID = 1L;

	// Safe crop species record is described here
	@Import
	static public class ClimatRecord extends Record {
		public ClimatRecord () {super ();}
		public ClimatRecord (String line) throws Exception {super (line);}
		//public String getSeparator () {return ";";}	// to change default "\t" separator

		public int julianDay;			//number of day in the year
		public int year;				//year YYYY
		public int month;				//month MM
		public int day;					//day DD
		public float tmax;				//temperature max in degree
		public float tmin;				//temperature min in degree
		public float rhmax;				//relative humidity in %
		public float rhmin;				//relative humidity min in %
		public float globalRadiation;	//global radiation in KW m-2
		public float rain;				//rain in mm
		public float windSpeed;			//m s-1
		public float waterTableDepth;	//m
		public float co2Concentration;	//ppm
		public int realYear;			//real year YYYY (in case this is not real climate data but a copy) 
	}

	@Import
	static public class ClimatRecord2 extends Record {
		public ClimatRecord2 () {super ();}
		public ClimatRecord2 (String line) throws Exception {super (line);}
		//public String getSeparator () {return ";";}	// to change default "\t" separator

		public int julianDay;			//number of day in the year
		public int year;				//year YYYY
		public int month;				//month MM
		public int day;					//day DD
		public float tmax;				//temperature max in degree
		public float tmin;				//temperature min in degree
		public float rhmax;				//relative humidity in %
		public float rhmin;				//relative humidity min in %
		public float globalRadiation;	//global radiation in KW m-2
		public float rain;				//rain in mm
		public float windSpeed;			//m s-1
		public float waterTableDepth;	//m
		public float co2Concentration;	//ppm

	}
	

	public SafeMacroFormat (String climatFileName) throws Exception {prepareImport (climatFileName);}

	/**
	 * Load RecordSet -> SafeMacroClimat
	 */
	public void load(SafeGeneralParameters settings,
			SafeMacroClimat ms,
			SafeSticsStation sticsStation, 
			double latitude, double elevation) throws Exception {

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
				ms.createDailyClimat(settings,  latitude,
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
						
				ms.createDailyClimat(settings,  latitude,
						  cr.julianDay, cr.year , cr.year, cr.month, cr.day,
						  cr.tmin, cr.tmax, cr.rhmin, cr.rhmax, cr.globalRadiation,
						  cr.rain, cr.windSpeed, cr.waterTableDepth, cr.co2Concentration);


			} else {
				System.out.println ("Unrecognized record : "+record);	// automatic toString () (or null)
				throw new Exception ("Weather error");	// automatic toString () (or null)

			}
			
		}

		//Copy  parameters in STICS 
		if (sticsStation != null) {
			sticsStation.P_alphapt = (float) settings.priestleyTaylorCoeff;
			sticsStation.P_aangst = (float) settings.aangst;
			sticsStation.P_bangst = (float) settings.bangst;
			sticsStation.P_altisimul = (float) elevation; 
			sticsStation.P_latitude = (float) latitude; 
		}

	}

}
