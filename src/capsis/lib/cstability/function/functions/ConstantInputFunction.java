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
import capsis.lib.cstability.function.util.TwoVariables;
import capsis.lib.cstability.function.util.Variables;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.state.State;

/**
 * A Constant input function of C-STABILITY library
 * 
 * @author J. Sainte-Marie, F. de Coligny - May 2021
 */
@SuppressWarnings("serial")
public class ConstantInputFunction extends Function {

	private double constant;
	private Function signature;

	public static final String NAME = "CONSTANT_INPUT_FUNCTION";
	public static final Encoder CONSTANT_INPUT_FUNCTION = new Encoder(NAME);
	public static final Set<Encoder> ENCODERS = new HashSet<>();

	static {
		ENCODERS.add(CONSTANT_INPUT_FUNCTION);
	}

	public static List<Availability> availabilities = new ArrayList<>();

	static {
		availabilities.add(Availability.POOL_INPUT);
	}

	/**
	 * Default constructor required to use Decodable, Function implements Decodable
	 */
	public ConstantInputFunction() {
	}

	/**
	 * Constructor
	 */
	public ConstantInputFunction(double constant, Function signature) {
		this.constant = constant;
		this.signature = signature;
	}

	/**
	 * decode(): decoding method from an encoded string
	 */
	@Override
	public ConstantInputFunction decode(String encodedString, Parameters p, Context c) throws Exception {

		// e.g. constantInput(0.3:signatureFunction)

		try {
			String s = encodedString.trim();

			if (!s.startsWith("constantInput("))
				throw new Exception("Not a constantInput");

			s = encodedString.replace("constantInput(", "");
			s = s.replace(")", "");
			StringTokenizer st = new StringTokenizer(s, ":");

			constant = Double.parseDouble(st.nextToken().trim());

			signature = Function.getFunction(st.nextToken().trim(), p, c);

			return new ConstantInputFunction(constant, signature);

		} catch (Exception e) {
			throw new Exception("ConstantInput.decode (), could not parse this encodedString: " + encodedString, e);
		}
	}

	/**
	 * execute()
	 */
	@Override
	public double execute(Parameters p, Context c, State s, Variables v) throws Exception {
		v.checkIf(TwoVariables.class);

		double date = ((TwoVariables) v).x1;
		double x = ((TwoVariables) v).x2;

		return constant * signature.execute(p, c, s, new OneVariable(x));
	}

	@Override
	public Class expectedVariables() {
		return TwoVariables.class;
	}

	@Override
	public Map<Encoder, String> encode() {
		Map<Encoder, String> out = new HashMap<>();
		Encoder signatureEncoder = (new ArrayList<>(signature.getEncoders())).get(0);
		out.put(CONSTANT_INPUT_FUNCTION, "constantInput(" + constant + ":" + signature.encode(signatureEncoder) + ")");
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

	@Override
	public String getEncodedFunction() {
		return encode(CONSTANT_INPUT_FUNCTION);
	}

}
