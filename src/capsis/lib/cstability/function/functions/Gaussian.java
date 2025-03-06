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
 * A Gaussian
 * 
 * @author J. Sainte-Marie, F. de Coligny - March 2021
 */
@SuppressWarnings("serial")
public class Gaussian extends Function {

	// Note: Function implements Decodable and Encodable

	private double mean;
	private double sd;

	public static final String NAME = "GAUSSIAN";
	public static final Encoder GAUSSIAN = new Encoder(NAME);
	public static final Set<Encoder> ENCODERS = new HashSet<>();

	static {
		ENCODERS.add(GAUSSIAN);
	}

	public static final List<Availability> availabilities = new ArrayList<>();

	static {
//		availabilities.add();
	}

	/**
	 * Default constructor
	 */
	public Gaussian() {
	}

	/**
	 * Constructor
	 */
	public Gaussian(double mean, double sd) {
		this.mean = mean;
		this.sd = sd;
	}

	/**
	 * decode(): decoding method from an encoded string
	 */
	@Override
	public Gaussian decode(String encodedString, Parameters p, Context c) throws Exception {

		// e.g. gaussian(1;0.4)

		try {
			String s = encodedString.trim();

			if (!s.startsWith("gaussian("))
				throw new Exception("Not a Gaussian");

			s = encodedString.replace("gaussian(", "");
			s = s.replace(")", "");
			StringTokenizer st = new StringTokenizer(s, ";");

			mean = Double.parseDouble(st.nextToken().trim());

			sd = Double.parseDouble(st.nextToken().trim());

			return new Gaussian(mean, sd);

		} catch (Exception e) {
			throw new Exception("Gaussian.decode (), could not parse this encodedString: " + encodedString, e);
		}

	}

	/**
	 * execute()
	 */
	@Override
	public double execute(Parameters p, Context c, State s, Variables v) throws Exception {
		v.checkIf(OneVariable.class);
		double x = ((OneVariable) v).x1;

		return BasicFunctions.gaussian(mean, sd, x);
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
		out.put(GAUSSIAN, "gaussian(" + mean + ";" + sd + ")");
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

	public double getMean() {
		return mean;
	}

	public double getSd() {
		return sd;
	}

	@Override
	public String getEncodedFunction() {
		return encode(GAUSSIAN);
	}

}
