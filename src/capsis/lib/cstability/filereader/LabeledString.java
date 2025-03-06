package capsis.lib.cstability.filereader;

import java.util.StringTokenizer;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.parameter.Parameters;

/**
 * A String decoded from a labeled string, e.g. integrationMethod =
 * INTEGRATION_TRAPEZE
 * 
 * @author J. Sainte-Marie, F. de Coligny - March 2021
 */
@SuppressWarnings("serial")
public class LabeledString implements Decodable {

	private String label;
	private String string;

	/**
	 * Default constructor
	 */
	public LabeledString() {
	}

	/**
	 * Constructor
	 */
	public LabeledString(String label, String string) {
		this.label = label;
		this.string = string;
	}

	/**
	 * getLabel()
	 */
	public String getLabel() {
		return label;
	}

	/**
	 * getString()
	 */
	public String getString() {
		return string;
	}

	/**
	 * decode(): decoding method from an encoded string
	 */
	@Override
	public LabeledString decode(String encodedString, Parameters p, Context c) throws Exception {

		// e.g. [0.12,0.86]
		try {
			String s = encodedString.trim();

			if (!s.contains("="))
				throw new Exception("Not a LabeledString (no = found)");

			StringTokenizer st = new StringTokenizer(s, "=");
			String label = st.nextToken().trim();
			String string = st.nextToken().trim();

			return new LabeledString(label, string);

		} catch (Exception e) {
			throw new Exception("LabeledString(), could not parse this encodedString: " + encodedString, e);
		}
	}
}