package safe.model;

import java.util.Iterator;

import jeeb.lib.util.Import;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;
import safe.stics.*;

/**
 * Records description for  MacroClimat
 *
 * @author Isabelle Lecomte - January 2003
 */
public class SafeMacroFormat extends RecordSet {


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
	}

	public SafeMacroFormat (String climatFileName) throws Exception {createRecordSet (climatFileName);}

	/**
	 * Load RecordSet -> SafeMacroClimat
	 */
	public void load(SafeInitialParameters settings,
			SafeMacroClimat ms,
			SafeSticsStation sticsStation, 
			double latitude, double elevation) throws Exception {

		for (Iterator i = this.iterator(); i.hasNext();) {
			Record record = (Record) i.next();

			if (record instanceof SafeMacroFormat.KeyRecord) {

				SafeMacroFormat.KeyRecord r = (SafeMacroFormat.KeyRecord) record;
				if (sticsStation != null) {
					
				}
					  
			}
			else if (record instanceof SafeMacroFormat.ClimatRecord) {
				

				SafeMacroFormat.ClimatRecord cr =
							(SafeMacroFormat.ClimatRecord) record;	// cast to precise type

				ms.createDailyClimat(settings,  latitude,
										  cr.julianDay, cr.year , cr.month, cr.day,
										  cr.tmin, cr.tmax, cr.rhmin, cr.rhmax, cr.globalRadiation,
										  cr.rain, cr.windSpeed, cr.waterTableDepth, cr.co2Concentration);

			} else {
				throw new Exception ("Unrecognized record : "+record);	// automatic toString () (or null)

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
