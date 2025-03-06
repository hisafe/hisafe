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
import capsis.lib.cstability.function.util.Variables;
import capsis.lib.cstability.function.util.ZeroVariable;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.state.State;

/**
 * A Constant
 * 
 * @author J. Sainte-Marie, F. de Coligny - March 2021
 */
@SuppressWarnings("serial")
public class Constant extends Function {

	// Note: Function implements Decodable and Encodable

	private double constant;

	public static final String NAME = "CONSTANT";
	public static final Encoder CONSTANT = new Encoder(NAME);
	public static final Set<Encoder> ENCODERS = new HashSet<>();

	static {
		ENCODERS.add(CONSTANT);
	}

	public static final List<Availability> availabilities = new ArrayList<>();

	static {
		availabilities.add(Availability.MICROBE_CARBON_USE_EFFICIENCY);
	}

	/**
	 * Default constructor
	 */
	public Constant() {
	}

	/**
	 * Constructor
	 */
	public Constant(double constant) {
		this.constant = constant;
	}

	/**
	 * decode(): decoding method from an encoded string
	 */
	@Override
	public Constant decode(String encodedString, Parameters p, Context c) throws Exception {

		// e.g. constant(0.3)

		try {
			String s = encodedString.trim();

			if (!s.startsWith("constant("))
				throw new Exception("Not a constant");

			s = encodedString.replace("constant(", "");
			s = s.replace(")", "");
			StringTokenizer st = new StringTokenizer(s, ";");

			constant = Double.parseDouble(st.nextToken().trim());

			return new Constant(constant);

		} catch (Exception e) {
			throw new Exception("Constant.decode (), could not parse this encodedString: " + encodedString, e);
		}
	}

	/**
	 * execute()
	 */
	@Override
	public double execute(Parameters p, Context c, State s, Variables v) throws Exception {
		v.checkIf(ZeroVariable.class);
		return constant;
	}

	/**
	 * expectedVariables()
	 */
	@Override
	public Class expectedVariables() {
		return ZeroVariable.class;
	}

	@Override
	public Map<Encoder, String> encode() {
		Map<Encoder, String> out = new HashMap<>();
		out.put(CONSTANT, "constant(" + constant + ")");
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

	public double getConstant() {
		return constant;
	}

	@Override
	public String getEncodedFunction() {
		return encode(CONSTANT);
	}

}
