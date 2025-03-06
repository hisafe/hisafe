package capsis.lib.cstability.context;

import java.io.Serializable;
import java.util.StringTokenizer;

/**
 * An environment record of the model C-STABILITY
 *
 * @author J. Sainte-Marie, F. de Coligny - May 2021
 */
@SuppressWarnings("serial")
public class EnvironmentRecord implements Serializable {

	// fc+jsm-5.3.2024 Review

	// Must be the same than in setupFile, e.g. day
	private String timeUnit;

	// A line number (int) in the env file, may become double in case of
	// interpolation
	private int date;

	// Optional supplementary information about experimental design, not read, for
	// user only, if not used, can be equal to date
	private String actualDate;

	private double temperature; // Kelvin

	private double pH;

	private double relativeHumidity; // percentage

//	private double soilClayContent; // percentage
//	
//	private double soilSandContent; // percentage
//	
//	private double soilSiltContent; // percentage

	/**
	 * Constructor, evaluates the given encoded String, extracts the expected
	 * variables, throws an exception in case of trouble.
	 */
	public EnvironmentRecord(String encodedRecord, String setupFileTimeUnit) throws Exception {

		String colName = null;

		try {
			String TAB = "\t";

			StringTokenizer st = new StringTokenizer(encodedRecord, TAB);

			int expectedColNumber = 6;
			int colNumber = st.countTokens();
			if (colNumber != expectedColNumber)
				throw new Exception("Wrong column number, expected: " + expectedColNumber + ", found: " + colNumber);

			colName = "timeUnit";
			timeUnit = st.nextToken();
			if (!timeUnit.contentEquals(setupFileTimeUnit))
				throw new Exception(
						"Wrong timeUnit: " + timeUnit + ", expected setup file timeUnit: " + setupFileTimeUnit);

			colName = "date";
			date = Integer.parseInt(st.nextToken());
			if (date < 0)
				throw new Exception("Wrong date: " + date + ", must be positive");

			colName = "actualDate";
			actualDate = st.nextToken();

			colName = "temperature";
			temperature = parseDoubleValueOrNaN(st.nextToken());
			if (temperature < 0)
				throw new Exception("Wrong temperature (Kelvin): " + temperature + ", must be positive");

			colName = "pH";
			pH = parseDoubleValueOrNaN(st.nextToken());
			if (pH < 0 || pH > 14)
				throw new Exception("Wrong pH [0,14]): " + pH);

			colName = "relativeHumidity";
			relativeHumidity = parseDoubleValueOrNaN(st.nextToken());
			if (relativeHumidity < 0 || relativeHumidity > 100)
				throw new Exception("Wrong relativeHumidity [0,100]): " + relativeHumidity);

		} catch (Exception e) {
			throw new Exception("EnvironmentRecord, trouble with " + colName, e);
		}

	}

	/**
	 * Only for variables accepting "NA": if the token contains "NA", return
	 * Double.NaN, else evaluate and return the number in token. In case of trouble,
	 * send an exception.
	 */
	private double parseDoubleValueOrNaN(String token) throws Exception {
		if (token.contentEquals("NA"))
			return Double.NaN;
		else
			return Double.parseDouble(token);
	}

	public String getTimeUnit() {
		return timeUnit;
	}

	public int getDate() {
		return date;
	}

	public String getActualDate() {
		return actualDate;
	}

	public double getTemperature() throws Exception {
		if (Double.isNaN(temperature))
			throw new Exception ("EnvironmentRecord.getTemperature(), error, date: "+date+", temperature: NaN");
		return temperature;
	}

	public double getpH() throws Exception {
		if (Double.isNaN(pH))
			throw new Exception ("EnvironmentRecord.getpH(), error, date: "+date+", pH: NaN");
		return pH;
	}

	public double getRelativeHumidity() throws Exception {
		if (Double.isNaN(relativeHumidity))
			throw new Exception ("EnvironmentRecord.getRelativeHumidity(), error, date: "+date+", relativeHumidity: NaN");
		return relativeHumidity;
	}


}
