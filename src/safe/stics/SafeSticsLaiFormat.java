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
