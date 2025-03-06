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
import capsis.lib.cstability.function.interfaces.PrimitiveAvailable;
import capsis.lib.cstability.function.util.Availability;
import capsis.lib.cstability.function.util.TwoVariables;
import capsis.lib.cstability.function.util.Variables;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.state.State;
import capsis.lib.cstability.util.Interval;

/**
 * A Kernel function
 * 
 * @author J. Sainte-Marie, F. de Coligny - March 2021
 */
@SuppressWarnings("serial")
public class KernelAlpha extends Function implements PrimitiveAvailable {

	// Note: Function implements Decodable and Encodable

	private Interval<Double> domain;
	private double alpha;

	public static final String NAME = "KERNEL_ALPHA";
	public static final Encoder KERNEL_ALPHA = new Encoder(NAME);
	public static final Set<Encoder> ENCODERS = new HashSet<>();

	static {
		ENCODERS.add(KERNEL_ALPHA);
	}

	public static final List<Availability> availabilities = new ArrayList<>();

	static {
		availabilities.add(Availability.ENZYME_DEPOLYMERIZATION_KERNEL);
	}

	/**
	 * Default constructor
	 */
	public KernelAlpha() {
	}

	/**
	 * Constructor
	 */
	public KernelAlpha(Interval<Double> domain, double alpha) {
		this.domain = domain;
		this.alpha = alpha;
	}

	/**
	 * decode(): decoding method from an encoded string
	 */
	@Override
	public KernelAlpha decode(String encodedString, Parameters p, Context c) throws Exception {

		// e.g. kernelAlpha([0,1];2.1)

		try {
			String s = encodedString.trim();

			if (!s.startsWith("kernelAlpha("))
				throw new Exception("Not a kernelAlpha");

			s = encodedString.replace("kernelAlpha(", "");
			s = s.replace(")", "");
			StringTokenizer st = new StringTokenizer(s, ";");

			Interval<Double> domain = (Interval<Double>) Decodable.pleaseDecode(Interval.class, st.nextToken().trim(),
					p, c);

			alpha = Double.parseDouble(st.nextToken().trim());

			return new KernelAlpha(domain, alpha);

		} catch (Exception e) {
			throw new Exception("KernelAlpha.decode (), could not parse this encodedString: " + encodedString, e);
		}

	}

	/**
	 * execute()
	 */
	@Override
	public double execute(Parameters p, Context c, State s, Variables v) throws Exception {
		v.checkIf(TwoVariables.class);
		double pi = ((TwoVariables) v).x1;
		double pj = ((TwoVariables) v).x2;
		double pmin = domain.getMin();
		double pmax = domain.getMax();
		double val = 0;
		if (pmin < pi && pi <= pj && pj <= pmax)
			val = (alpha + 1) * Math.pow(pi - pmin, alpha) / Math.pow(pj - pmin, alpha + 1);
		else if (pmin == pi && pi == pj)
			val = 1 / p.getUserPolymerizationStep();

		return val;
	}

	/**
	 * executePrimitive(): in C-Stability, a primitive is always relative to second
	 * variable of the kernel
	 */
	@Override
	public double executePrimitive(Parameters p, Context c, State s, double[] discretization, Variables v)
			throws Exception {
		v.checkIf(TwoVariables.class);

		int i = (int) ((TwoVariables) v).x1;
		int j = (int) ((TwoVariables) v).x2;

		double pmin = domain.getMin();

		double pi = discretization[i];
		double pj = discretization[j];
		double pjplus1 = discretization[j + 1];

		double Lij = 0;
		if (j >= i)
			if (j == 0)
				Lij = (alpha + 1) / alpha;
			else
				Lij = (alpha + 1) / alpha * (Math.pow((pi - pmin) / (pj - pmin), alpha)
						- Math.pow((pi - pmin) / (pjplus1 - pmin), alpha));

		return Lij;
	}

	/**
	 * expectedVariables()
	 */
	@Override
	public Class expectedVariables() {
		return TwoVariables.class;
	}

	/**
	 * primitiveExpectedVariables()
	 */
	@Override
	public Class primitiveExpectedVariables() {
		return TwoVariables.class;
	}

	@Override
	public Map<Encoder, String> encode() {
		Map<Encoder, String> out = new HashMap<>();
		out.put(KERNEL_ALPHA, "kernelAlpha(" + domain.encode(Interval.INTERVAL) + ";" + alpha + ")");
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

	public double getAlpha() {
		return alpha;
	}

	@Override
	public String getEncodedFunction() {
		return encode(KERNEL_ALPHA);
	}

}
