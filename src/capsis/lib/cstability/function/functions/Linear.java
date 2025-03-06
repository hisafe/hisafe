package capsis.lib.cstability.function.functions;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.function.Function;
import capsis.lib.cstability.function.util.Availability;
import capsis.lib.cstability.function.util.OneVariable;
import capsis.lib.cstability.function.util.Variables;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.state.State;

/**
 * A uniform linear function.
 * 
 * @author J. Sainte-Marie, F. de Coligny - March 2021
 */
@SuppressWarnings("serial")
public class Linear extends Function {

	// Note: Function implements Decodable and Encodable

	private double slope;

	public static final String NAME = "LINEAR";
	public static final Encoder LINEAR = new Encoder(NAME);
	public static final Set<Encoder> ENCODERS = new HashSet<>();

	static {
		ENCODERS.add(LINEAR);
	}

	public static final List<Availability> availabilities = new ArrayList<>();

	static {
		availabilities.add(Availability.MICROBE_MORTALITY);
		availabilities.add(Availability.MICROBE_ENZYME_PRODUCTION);
	}

	/**
	 * Default constructor
	 */
	public Linear() {
	}

	/**
	 * Constructor
	 */
	public Linear(double slope) {
		this.slope = slope;
	}

	/**
	 * decode(): decoding method from an encoded string
	 */
	@Override
	public Linear decode(String encodedString, Parameters p, Context c) throws Exception {

		// e.g. linear(0.4)

		try {
			String s = encodedString.trim();

			if (!s.startsWith("linear("))
				throw new Exception("Not a linear");

			s = encodedString.replace("linear(", "");
			s = s.replace(")", "");
			StringTokenizer st = new StringTokenizer(s, ";");

			slope = Double.parseDouble(st.nextToken().trim());

			return new Linear(slope);

		} catch (Exception e) {
			throw new Exception("Linear.decode (), could not parse this encodedString: " + encodedString, e);
		}

	}

	/**
	 * execute(): the function is uniform on the domain (x1) and then linear (x2)
	 */
	@Override
	public double execute(Parameters p, Context c, State s, Variables v) throws Exception {
		v.checkIf(OneVariable.class);

		double x1 = ((OneVariable) v).x1;

		return slope * x1;

	}

	/**
	 * expectedVariables()
	 */
	@Override
	public Class expectedVariables() {
		return OneVariable.class;
	}

	@Override
	public Map<Encoder, String> encode() {
		Map<Encoder, String> out = new HashMap<>();
		out.put(LINEAR, "linear(" + slope + ")");
		return out;
	}

	@Override
	public String encode(Encoder encoder) {
		return encode().get(encoder);
	}

	@Override
	public Set<Encoder> getEncoders() {
		return ENCODERS;
	}

	@Override
	public String getName() {
		return NAME;
	}

	@Override
	public List<Availability> getAvailabilities() {
		return availabilities;
	}

	public double getSlope() {
		return slope;
	}

	@Override
	public String getEncodedFunction() {
		return encode(LINEAR);
	}

}
