package safe.stics;

import java.util.Iterator;

import jeeb.lib.util.Import;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;
import safe.model.*;



public class SafeSticsLaiFormat extends RecordSet {

	// Crop lai record is described here
		@Import
		static public class CropLai extends Record {
			public int day;
			public int month;
			public int year;
			public int julianDay;	
			public float lai;  
	
			public CropLai () {super ();}
			public CropLai (String line) throws Exception {super (line);}

		}

		
		//CONSTRUCTEUR
		public SafeSticsLaiFormat (String fileName) throws Exception {
			createRecordSet (fileName);
		}

		/**
		* Saving data in a recordSet
		*/
		public SafeSticsLaiFormat (SafeSticsCrop sc)
							throws Exception {createRecordSet (sc);}

		public void createRecordSet (SafeSticsCrop sc) throws Exception {}

		/**
		* Read data from a recordSet
		*/
		public void load (SafeSticsCrop crop) throws Exception {


			int index = 1;
			
			for (Iterator i = this.iterator (); i.hasNext ();) {

				Record record = (Record) i.next ();

				if (record instanceof SafeSticsLaiFormat.CropLai) {

					SafeSticsLaiFormat.CropLai r =
							(SafeSticsLaiFormat.CropLai) record;	// cast to precise type		

					int indice          = ((index-1)*3)+1;
					if (indice < 1099) {
						crop.lai[indice]	= (float) r.lai;
						crop.lai[indice+1]	= crop.lai[indice];
						crop.lai[indice+2]	= crop.lai[indice];
						index++; 
					
					}

				} //KEY  Records
				
					
				else {
					throw new Exception ("Unrecognized record : "+record);	// automatic toString () (or null)
				}
			}
		}
	}
