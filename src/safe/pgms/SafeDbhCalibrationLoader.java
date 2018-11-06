package safe.pgms;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;

/**
 * A loader for an HiSAFE calibration
 * Testing DBH 
 * 
 * @author : I.LECOMTE 17-04-2018
 */
public class SafeDbhCalibrationLoader extends RecordSet {


	//CALIBRATION (DBH parameters)
	public List<Integer> calibrationDbhYear; 		//year to check
	public List<Integer> calibrationDbhMonth; 		//month to check
	public List<Integer> calibrationDbhDay; 		//day to check
	public List<Integer> calibrationDbhIdTree; 		//tree ID
	public List<Double>  calibrationDbhMin; 		//tree dbh min
	public List<Double>  calibrationDbhMax; 		//tree dbh max


	
	/**
	 * Constructor.
	 */
	public SafeDbhCalibrationLoader (String fileName) throws Exception {
		createRecordSet (fileName);
	}

	public void load() throws Exception {

		calibrationDbhYear = new ArrayList<Integer>();
		calibrationDbhMonth = new ArrayList<Integer>();
		calibrationDbhDay = new ArrayList<Integer>();
		calibrationDbhIdTree = new ArrayList<Integer>();
		calibrationDbhMin = new ArrayList<Double>();
		calibrationDbhMax = new ArrayList<Double>();
		

		for (Iterator i = this.iterator(); i.hasNext();) {
			Record record = (Record) i.next();

			if (record instanceof SafeDbhCalibrationLoader.KeyRecord) {

				SafeDbhCalibrationLoader.KeyRecord r = (SafeDbhCalibrationLoader.KeyRecord) record;

				String param = r.key;
				
				 if (param.equals("calibration")) {
					String[] st = (r.value).split(",");
	
					calibrationDbhYear.add(new Integer(Integer.parseInt(st[0])));
					calibrationDbhMonth.add(new Integer(Integer.parseInt(st[1])));
					calibrationDbhDay.add(new Integer(Integer.parseInt(st[2])));
					calibrationDbhIdTree.add(new Integer(Integer.parseInt(st[3])));
					calibrationDbhMin.add(new Double(Double.parseDouble(st[4])));
					calibrationDbhMax.add(new Double(Double.parseDouble(st[5])));		
				}
			}
		}

		//missing calibration parameters
		if (calibrationDbhYear.size() == 0) {
			throw new Exception("Missing calibration parameters  ! ");

		}
	}




}
