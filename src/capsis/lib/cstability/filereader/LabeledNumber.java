package capsis.lib.cstability.filereader;

import java.util.StringTokenizer;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.parameter.Parameters;

/**
 * A number decoded from a labeled number, e.g. userPolymerizationStep = 0.1
 * 
 * @author J. Sainte-Marie, F. de Coligny - March 2021
 */
@SuppressWarnings("serial")
public class LabeledNumber implements Decodable {

	private String label;
	private double number;

	/**
	 * Default constructor
	 */
	public LabeledNumber() {
	}

	/**
	 * Constructor
	 */
	public LabeledNumber(String label, double number) {
		this.label = label;
		this.number = number;
	}

	/**
	 * getLabel()
	 */
	public String getLabel() {
		return label;
	}

	/**
	 * getDouble()
	 */
	public double getDouble() {
		return number;
	}

	/**
	 * getInt()
	 */
	public int getInt() {
		return (int) number;
	}

	/**
	 * decode(): decoding method from an encoded string
	 */
	@Override
	public LabeledNumber decode(String encodedString, Parameters p, Context c) throws Exception {

		// e.g. [0.12,0.86]
		try {
			String s = encodedString.trim();

			if (!s.contains("="))
				throw new Exception("Not a LabeledNumber (no = found)");

			StringTokenizer st = new StringTokenizer(s, "=");
			String label = st.nextToken().trim();
			double number = Double.parseDouble(st.nextToken().trim());

			return new LabeledNumber(label, number);

		} catch (Exception e) {
			throw new Exception("LabeledNumber(), could not parse this encodedString: " + encodedString, e);
		}
	}

}
