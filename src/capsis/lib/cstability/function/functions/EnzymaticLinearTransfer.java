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
 * A linear enzymatic transfer
 * 
 * @author J. Sainte-Marie, F. de Coligny - May 2021
 */
@SuppressWarnings("serial")
public class EnzymaticLinearTransfer extends Function {

	// Note: Function implements Decodable and Encodable

	private String enzymeName;
	private double transferRate;

	public static final String NAME = "ENZYMATIC_LINEAR_TRANSFER";
	public static final Encoder ENZYMATIC_LINEAR_TRANSFER = new Encoder(NAME);
	public static final Set<Encoder> ENCODERS = new HashSet<>();

	static {
		ENCODERS.add(ENZYMATIC_LINEAR_TRANSFER);
	}

	public static final List<Availability> availabilities = new ArrayList<>();

	static {
		availabilities.add(Availability.POOL_TRANSFER);
	}

	/**
	 * Default constructor
	 */
	public EnzymaticLinearTransfer() {
	}

	/**
	 * Constructor
	 */
	public EnzymaticLinearTransfer(String enzymeName, double transferRate) {
		this.enzymeName = enzymeName;
		this.transferRate = transferRate;
	}

	/**
	 * decode(): decoding method from an encoded string
	 */
	@Override
	public EnzymaticLinearTransfer decode(String encodedString, Parameters p, Context c) throws Exception {

		// e.g. enzymaticLinearTransfer(cellulase;1)

		try {
			String s = encodedString.trim();

			if (!s.startsWith("enzymaticLinearTransfer("))
				throw new Exception("Not an EnzymaticLinearTransfer");

			s = encodedString.replace("enzymaticLinearTransfer(", "");
			s = s.replace(")", "");
			StringTokenizer st = new StringTokenizer(s, ";");

			enzymeName = st.nextToken().trim();
			if (!p.getEnzymeTraitsMap().keySet().contains(enzymeName))
				throw new Exception("EnzymaticLinearTransfer.decode (), unknown enzymeName " + enzymeName);

			transferRate = Double.parseDouble(st.nextToken().trim());

			return new EnzymaticLinearTransfer(enzymeName, transferRate);

		} catch (Exception e) {
			throw new Exception(
					"EnzymaticLinearTransfer.decode (), could not parse this encodedString: " + encodedString, e);
		}
	}

	/**
	 * execute()
	 */
	@Override
	public double execute(Parameters p, Context c, State s, Variables v) throws Exception {
		v.checkIf(OneVariable.class);
		// x is a value of the transfered distribution
		double x = ((OneVariable) v).x1;

		return transferRate * s.getEnzyme(enzymeName).getActivity() * x;
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
		out.put(ENZYMATIC_LINEAR_TRANSFER, "enzymaticLinearTransfer(" + enzymeName + ";" + transferRate + ")");
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

	public String getEnzymeName() {
		return enzymeName;
	}

	public double getTransferRate() {
		return transferRate;
	}

	@Override
	public String getEncodedFunction() {
		return encode(ENZYMATIC_LINEAR_TRANSFER);
	}

}
