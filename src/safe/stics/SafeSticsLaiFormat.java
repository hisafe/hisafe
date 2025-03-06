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
