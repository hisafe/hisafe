package capsis.lib.cstability.context;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

/**
 * A loader for the C-Stability environment file.
 * 
 * @author J. Sainte-Marie, F. de Coligny - March 2024
 */
@SuppressWarnings("serial")
public class EnvironmentFileLoader implements Serializable {

	private String fileName;
	private String setupFileTimeUnit;
	private int setupFileInitialDate;
	private int setupFileFinalDate;

	private Map<Integer, EnvironmentRecord> environmentMap;
	private int dateMin;
	private int dateMax;

	/**
	 * Constructor
	 */
	public EnvironmentFileLoader(String fileName, Timeline timeline) {
		this.fileName = fileName;

		this.setupFileTimeUnit = timeline.getUnit();
		this.setupFileInitialDate = timeline.getInitialDate();
		this.setupFileFinalDate = timeline.getFinalDate();
	}

	/**
	 * load(): load the data in the given environment file
	 */
	public void load() throws Exception {

		try {
			environmentMap = new HashMap<>();
			dateMin = -1;

			BufferedReader in = new BufferedReader(new FileReader(fileName));
			String line;
			int lastDate = -1;
			int i = 0;
			while ((line = in.readLine()) != null) {
				line.trim();
				i++;

				if (line.startsWith("#") || line.length() == 0)
					continue;

				try {
					EnvironmentRecord er = new EnvironmentRecord(line, setupFileTimeUnit);

					int date = er.getDate();
					if (lastDate != -1)
						if (date != lastDate + 1)
							throw new Exception("Wrong date: " + date + ", expected: " + (lastDate + 1));
					lastDate = date;

					environmentMap.put(date, er);

					if (dateMin == -1)
						dateMin = date;
					dateMax = date;

				} catch (Exception e1) {
					throw new Exception("Could not interpret line " + i + ": " + line, e1);
				}
			}
			in.close();

			boolean timeLineCompatible = dateMin <= setupFileInitialDate && setupFileFinalDate <= dateMax;
			if (!timeLineCompatible)
				throw new Exception("The given timeline [" + setupFileInitialDate + "," + setupFileFinalDate
						+ "] is not compatible with the dates in the environment file [" + dateMin + "," + dateMax
						+ "]");

		} catch (Exception e) {
			throw new Exception("EnvironmentFileLoader.load(), could not read environment file: " + fileName, e);
		}

	}

	public EnvironmentRecord getEnvironmentRecord(int date) throws Exception {
		if (date < dateMin || date > dateMax)
			throw new Exception("EnvironmentFileLoader.getEnvironmentRecord(), date: " + date + " out of bounds: ["
					+ dateMin + "," + dateMax + "]");

		EnvironmentRecord er = environmentMap.get(date);

		// Later, introduce interpolation here
		if (er == null)
			throw new Exception(
					"EnvironmentFileLoader.getEnvironmentRecord(), could not find EnvironmentRecord for date: " + date);

		return er;
	}

}
