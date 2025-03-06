package capsis.lib.samsaralight;

import java.util.List;

import jeeb.lib.util.fileloader.FileLoader;

/**
 * A file loader for the samsaraLight options file. Replaces the former
 * SLSettingsLoader, now a FileLoader instance, all possible keywords are now
 * mandatory, clearer.
 * 
 * <pre>
 * // Usage
 * SLSettingsLoader2024 loader = new SLSettingsLoader2024();
 * loader.load(samsaraLightFileName);
 * loader.interpret(slSettings);
 * </pre>
 * 
 * @author F. de Coligny - October 2024
 */
public class SLSettingsLoader2024 extends FileLoader {

	// soc: Standard Overcast Sky, if false: Uniform Overcast Sky

	// GMT: Time lag between the local time and the local solar time meridian
	// (GL, 06/09/2012)
	// In Occidental Europe : + 1 during the winter; + 2 during the summer.

	private String fileName; // private, not in the file

	// 12 parameters needed in the file
	public boolean parallel_mode;
	public boolean tag_mode;
	public boolean turbid_medium;
	public boolean trunk_interception;
	public double direct_angle_step; // deg
	public double height_angle_min; // deg
	public double diffuse_angle_step; // deg
	public boolean soc;
	public int GMT;
	public int leaf_on_doy; // [1,366]
	public int leaf_off_doy; // [1,366]
	public boolean sensor_light_only;

	// We expect a list of monthlyRecords OR a list of hourlyRecords
	public List<SLMonthlyRecord> monthlyRecords;

	public List<SLHourlyRecord> hourlyRecords;

	// --- Extra local variables
	private boolean loadHasBeenCalled;

	/**
	 * Constructor
	 */
	public SLSettingsLoader2024() throws Exception {
		super();
	}

	@Override
	public void load(String fileName) throws Exception {
		super.load(fileName);

		if (loadHasBeenCalled)
			throw new Exception("SLSettingsLoader2024.load () error, load() has been called a second time");

		loadHasBeenCalled = true;

		this.fileName = fileName;
	}

	@Override
	protected void checks() throws Exception {

		// check () is called by FileLoader.load () after reading and intepreting the
		// file

		boolean monthlyRecordsLoaded = (monthlyRecords != null) && (!monthlyRecords.isEmpty());
		boolean hourlyRecordsLoaded = (hourlyRecords != null) && (!hourlyRecords.isEmpty());

		if (!monthlyRecordsLoaded && !hourlyRecordsLoaded)
			throw new Exception(
					"SLSettingsLoader2024.load () error, missing records in the SamsaraLight file, expecting monthly or hourly records."
							+ " SamsaraLight file: " + fileName);

		if (monthlyRecordsLoaded && hourlyRecordsLoaded)
			throw new Exception(
					"SLSettingsLoader2024.load () error, extra records in the SamsaraLight file, expecting monthly OR hourly records."
							+ " SamsaraLight file: " + fileName);

		if (monthlyRecordsLoaded && monthlyRecords.size() != 12)
			throw new Exception(
					"SLSettingsLoader2024.load () error, wrong number of monthly records in the SamsaraLight file, expecting 12 records."
							+ " SamsaraLight file: " + fileName);

		// No checks on number of hourly records, depends on leap / not leap year

	}

	/**
	 * Interprets the file, loads information in the given settings object.
	 */
	public void interpret(SLSettings sets) throws Exception {

		if (!loadHasBeenCalled)
			throw new Exception("SLSettingsLoader2024.interpret () error, load() must be called before interpret ()");

		sets.setFileName(fileName);

		sets.setParallelMode(parallel_mode);
		sets.setTagMode(tag_mode);
		sets.setTurbidMedium(turbid_medium);
		sets.setTrunkInterception(trunk_interception);
		sets.setDirectAngleStep(direct_angle_step);
		sets.setHeightAngleMin(height_angle_min);
		sets.setDiffuseAngleStep(diffuse_angle_step);
		sets.setSoc(soc);
		sets.setGMT(GMT);
		sets.setLeafOnDoy(leaf_on_doy);
		sets.setLeafOffDoy(leaf_off_doy);
		sets.setSensorLightOnly(sensor_light_only);

		boolean monthlyRecordsLoaded = (monthlyRecords != null) && (!monthlyRecords.isEmpty());
		boolean hourlyRecordsLoaded = (hourlyRecords != null) && (!hourlyRecords.isEmpty());

		if (monthlyRecordsLoaded) {
			for (SLMonthlyRecord r : monthlyRecords)
				sets.addMontlyRecord(r);
		} else if (hourlyRecordsLoaded) {
			for (SLHourlyRecord r : hourlyRecords)
				sets.addHourlyRecord(r);

		} else
			throw new Exception(
					"SLSettingsLoader2024.interpret () error, missing records in the SamsaraLight file, expecting monthly or hourly records."
							+ " SamsaraLight file: " + fileName);

	}

}
