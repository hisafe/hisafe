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
 * Testing ROOTS
 * 
 * @author : I.LECOMTE 17-04-2018
 */
public class SafeRootCalibrationLoader extends RecordSet {


	//CALIBRATION (root parameters)
	public List<Integer> calibrationRootYear; 			//year to check
	public List<Integer> calibrationRootMonth; 			//month to check
	public List<Integer> calibrationRootDay; 			//day to check
	public List<Integer> calibrationRootIdTree; 		//tree ID
	public List<Double>  calibrationRootDistance4m; 	//tree root distance to check at 4m (0=no,1=yes)
	public List<Double>  calibrationRootDistance6m; 	//tree root distance to check at 6m (0=no,1=yes)
	public List<Double>  calibrationRootTotal; 			//tree root on all scene (0=no,1=yes)

	
	/**
	 * Constructor.
	 */
	public SafeRootCalibrationLoader (String fileName) throws Exception {
		createRecordSet (fileName);
	}

	public void load() throws Exception {

		calibrationRootYear = new ArrayList<Integer>();
		calibrationRootMonth = new ArrayList<Integer>();
		calibrationRootDay = new ArrayList<Integer>();
		calibrationRootIdTree = new ArrayList<Integer>();
		calibrationRootDistance4m = new ArrayList<Double>();
		calibrationRootDistance6m = new ArrayList<Double>();
		calibrationRootTotal = new ArrayList<Double>();
		

		for (Iterator i = this.iterator(); i.hasNext();) {
			Record record = (Record) i.next();

			if (record instanceof SafeRootCalibrationLoader.KeyRecord) {

				SafeRootCalibrationLoader.KeyRecord r = (SafeRootCalibrationLoader.KeyRecord) record;

				String param = r.key;
				
				 if (param.equals("calibration")) {
					String[] st = (r.value).split(",");
	
					calibrationRootYear.add(new Integer(Integer.parseInt(st[0])));
					calibrationRootMonth.add(new Integer(Integer.parseInt(st[1])));
					calibrationRootDay.add(new Integer(Integer.parseInt(st[2])));
					calibrationRootIdTree.add(new Integer(Integer.parseInt(st[3])));
					calibrationRootDistance4m.add(new Double(Double.parseDouble(st[4])));
					calibrationRootDistance6m.add(new Double(Double.parseDouble(st[5])));
					calibrationRootTotal.add(new Double(Double.parseDouble(st[6])));		
				}
			}
		}

		//missing calibration parameters
		if (calibrationRootYear.size() == 0) {
			throw new Exception("Missing calibration parameters  ! ");

		}
	}




}
