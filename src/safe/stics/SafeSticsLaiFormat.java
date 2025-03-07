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
 * Format to read STICS lai file 
 * 
 * @author Isabelle Lecomte - December 2016
 */


public class SafeSticsLaiFormat extends RecordSet {

	private static final long serialVersionUID = 1L;

		// Crop lai record is described here
		@Import
		static public class CropLai extends Record {
			private static final long serialVersionUID = 1L;
			public int year;
			public int month;
			public int day;
			public int julianDay;	
			public float lai;
	
			public CropLai () {super ();}
			public CropLai (String line) throws Exception {super (line);}

		}

		
		public SafeSticsLaiFormat (String fileName) throws Exception {
			prepareImport (fileName);
		}

		/**
		* Saving data in a recordSet
		*/
		public SafeSticsLaiFormat (SafeCrop sc)
							throws Exception {createRecordSet (sc);}

		public void createRecordSet (SafeCrop sc) throws Exception {}

		/**
		* Read data from a recordSet
		*/
		public void load (SafeCrop crop) throws Exception {
			
			for (Iterator<Record> i = this.iterator (); i.hasNext ();) {

				Record record = i.next ();

				if (record instanceof SafeSticsLaiFormat.CropLai) {

					SafeSticsLaiFormat.CropLai r =
							(SafeSticsLaiFormat.CropLai) record;	// cast to precise type		
				
					SafeSticsLai s = new SafeSticsLai (r.year, r.month, r.day, r.julianDay, r.lai);

					crop.addLaiMap (s);

				} 
					
				else {
					throw new Exception ("Unrecognized record : "+record);	// automatic toString () (or null)
				}
			}
		}
	}
