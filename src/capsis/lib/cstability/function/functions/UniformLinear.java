package capsis.lib.cstability.function.functions;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.filereader.Decodable;
import capsis.lib.cstability.function.Function;
import capsis.lib.cstability.function.util.Availability;
import capsis.lib.cstability.function.util.TwoVariables;
import capsis.lib.cstability.function.util.Variables;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.state.State;
import capsis.lib.cstability.util.Interval;

/**
 * A uniform linear function.
 * 
 * @author J. Sainte-Marie, F. de Coligny - March 2021
 */
@SuppressWarnings("serial")
public class UniformLinear extends Function {

	// Note: Function implements Decodable and Encodable

	private Interval<Double> domain;
	private double slope;

	public static final String NAME = "UNIFORM_LINEAR";
	public static final Encoder UNIFORM_LINEAR = new Encoder(NAME);
	public static final Set<Encoder> ENCODERS = new HashSet<>();

	static {
		ENCODERS.add(UNIFORM_LINEAR);
	}

	public static final List<Availability> availabilities = new ArrayList<>();

	static {
		availabilities.add(Availability.ENZYME_DEPOLYMERIZATION_RATE);
		availabilities.add(Availability.MICROBE_UPTAKE_RATE);
	}

	/**
	 * Default constructor
	 */
	public UniformLinear() {
	}

	/**
	 * Constructor
	 */
	public UniformLinear(Interval<Double> domain, double slope) {
		this.domain = domain;
		this.slope = slope;
	}

	/**
	 * decode(): decoding method from an encoded string
	 */
	@Override
	public UniformLinear decode(String encodedString, Parameters p, Context c) throws Exception {

		// e.g. uniformLinear(1;[0,0.4])

		try {
			String s = encodedString.trim();

			if (!s.startsWith("uniformLinear("))
				throw new Exception("Not a uniformLinear");

			s = encodedString.replace("uniformLinear(", "");
			s = s.replace(")", "");
			StringTokenizer st = new StringTokenizer(s, ";");

			domain = (Interval<Double>) Decodable.pleaseDecode(Interval.class, st.nextToken().trim(), p, c);

			slope = Double.parseDouble(st.nextToken().trim());

			return new UniformLinear(domain, slope);

		} catch (Exception e) {
			throw new Exception("UniformLinear.decode (), could not parse this encodedString: " + encodedString, e);
		}

	}

	/**
	 * execute(): the function is uniform on the domain (x1) and then linear (x2)
	 */
	@Override
	public double execute(Parameters p, Context c, State s, Variables v) throws Exception {
		v.checkIf(TwoVariables.class);

		double x1 = ((TwoVariables) v).x1;
		double x2 = ((TwoVariables) v).x2;

		return BasicFunctions.indicator(domain, x1) * slope * x2;

	}

	/**
	 * expectedVariables()
	 */
	@Override
	public Class expectedVariables() {
		return TwoVariables.class;
	}

	@Override
	public Map<Encoder, String> encode() {
		Map<Encoder, String> out = new HashMap<>();
		out.put(UNIFORM_LINEAR, "uniformLinear(" + domain.encode(Interval.INTERVAL) + ";" + slope + ")");
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

	public Interval<Double> getDomain() {
		return domain;
	}

	public double getSlope() {
		return slope;
	}

	@Override
	public String getEncodedFunction() {
		return encode(UNIFORM_LINEAR);
	}

}
