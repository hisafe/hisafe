package capsis.lib.cstability.function.functions;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.distribution.DiscretePositiveDistribution;
import capsis.lib.cstability.filereader.Decodable;
import capsis.lib.cstability.function.Function;
import capsis.lib.cstability.function.util.Availability;
import capsis.lib.cstability.function.util.OneVariable;
import capsis.lib.cstability.function.util.Variables;
import capsis.lib.cstability.parameter.BiochemicalClass;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.state.State;
import capsis.lib.cstability.util.Interval;

/**
 * A GaussianTruncatedNormalized
 * 
 * @author J. Sainte-Marie, F. de Coligny - April 2021
 */
@SuppressWarnings("serial")
public class GaussianTruncatedProportionalized extends Function {

	// Note: Function implements Decodable and Encodable

	private String biochemicalClassName;
	private double proportion;
	private double mean;
	private double sd;
	private Interval<Double> domain; // name to be checked

	public static final String NAME = "GAUSSIAN_TRUNCATED_PROPORTIONALIZED";
	public static final Encoder GAUSSIAN_TRUNCATED_PROPORTIONALIZED = new Encoder(NAME);
	public static final Set<Encoder> ENCODERS = new HashSet<>();

	static {
		ENCODERS.add(GAUSSIAN_TRUNCATED_PROPORTIONALIZED);
	}

	public static final List<Availability> availabilities = new ArrayList<>();

	static {
		availabilities.add(Availability.POOL_INITIALIZATION);
		availabilities.add(Availability.POOL_INPUT);
	}

	// Calculated in init ()
	private double proportionalizationCoefficient = -1;

	/**
	 * Default constructor
	 */
	public GaussianTruncatedProportionalized() {
	}

	/**
	 * Constructor
	 */
	public GaussianTruncatedProportionalized(String biochemicalClassName, double proportion, double mean, double sd,
			Interval<Double> domain) {
		this.biochemicalClassName = biochemicalClassName;
		this.proportion = proportion;
		this.mean = mean;
		this.sd = sd;
		this.domain = domain;

	}

	/**
	 * decode(): decoding method from an encoded string
	 */
	@Override
	public GaussianTruncatedProportionalized decode(String encodedString, Parameters p, Context c) throws Exception {

		// e.g. gaussianTruncatedProportionalized(lipid;95;1;0.4;[0,2])

		try {
			String s = encodedString.trim();

			if (!s.startsWith("gaussianTruncatedProportionalized("))
				throw new Exception("Not a gaussianTruncatedProportionalized");

			s = encodedString.replace("gaussianTruncatedProportionalized(", "");
			s = s.replace(")", "");
			StringTokenizer st = new StringTokenizer(s, ";");

			biochemicalClassName = st.nextToken().trim();

			proportion = Double.parseDouble(st.nextToken().trim());

			mean = Double.parseDouble(st.nextToken().trim());

			sd = Double.parseDouble(st.nextToken().trim());

			domain = (Interval<Double>) Decodable.pleaseDecode(Interval.class, st.nextToken().trim(), p, c);

			return new GaussianTruncatedProportionalized(biochemicalClassName, proportion, mean, sd, domain);

		} catch (Exception e) {
			throw new Exception(
					"GaussianTruncatedProportionalized.decode (), could not parse this encodedString: " + encodedString,
					e);
		}

	}

	/**
	 * init(): called at first execute () call, calculates proportionalization
	 * coefficient once for all.
	 */
	private void init(Parameters p) throws Exception {

		BiochemicalClass bc = p.getBiochemicalClassMap().get(biochemicalClassName);
		double[] valuesX = bc.getPolymerization().getDiscretization();

		double[] valuesY = new double[valuesX.length];

		for (int i = 0; i < valuesX.length; i++) {
			double x = valuesX[i];
			valuesY[i] = BasicFunctions.indicator(domain, x) * BasicFunctions.gaussian(mean, sd, x);
		}

		DiscretePositiveDistribution dpd = new DiscretePositiveDistribution(valuesX, valuesY, p.getIntegrationMethod());
		dpd.proportionalize(proportion);
		proportionalizationCoefficient = dpd.getIntegral();
	}

	/**
	 * execute()
	 */
	@Override
	public double execute(Parameters p, Context c, State s, Variables v) throws Exception {

		v.checkIf(OneVariable.class);
		double x = ((OneVariable) v).x1;

		if (proportionalizationCoefficient < 0)
			init(p);
		return BasicFunctions.indicator(domain, x) * BasicFunctions.gaussian(mean, sd, x)
				* proportionalizationCoefficient;

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
		out.put(GAUSSIAN_TRUNCATED_PROPORTIONALIZED, "gaussianTruncatedProportionalized(" + biochemicalClassName + ";"
				+ proportion + ";" + mean + ";" + sd + ";" + domain.encode(Interval.INTERVAL) + ")");
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

	public String getBiochemicalClassName() {
		return biochemicalClassName;
	}

	public double getProportion() {
		return proportion;
	}

	public double getMean() {
		return mean;
	}

	public double getSd() {
		return sd;
	}

	public Interval<Double> getDomain() {
		return domain;
	}

	@Override
	public String getEncodedFunction() {
		return encode(GAUSSIAN_TRUNCATED_PROPORTIONALIZED);
	}

}
