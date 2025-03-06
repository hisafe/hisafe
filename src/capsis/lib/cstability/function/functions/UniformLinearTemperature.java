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
 * A uniform linear function depending on temperature.
 * 
 * @author J. Sainte-Marie, F. de Coligny - March 2024
 */
@SuppressWarnings("serial")
public class UniformLinearTemperature extends Function {

	// Note: Function implements Decodable and Encodable

	private Interval<Double> domain;
	private double slope;
	private double qRef; // fc+jsm-5.3.2024
	private double tRef; // fc+jsm-5.3.2024

	public static final String NAME = "UNIFORM_LINEAR_TEMPERATURE";
	public static final Encoder UNIFORM_LINEAR_TEMPERATURE = new Encoder(NAME);
	public static final Set<Encoder> ENCODERS = new HashSet<>();

	static {
		ENCODERS.add(UNIFORM_LINEAR_TEMPERATURE);
	}

	public static final List<Availability> availabilities = new ArrayList<>();

	static {
		availabilities.add(Availability.ENZYME_DEPOLYMERIZATION_RATE);
		availabilities.add(Availability.MICROBE_UPTAKE_RATE);
	}

	/**
	 * Default constructor
	 */
	public UniformLinearTemperature() {
	}

	/**
	 * Constructor
	 */
	public UniformLinearTemperature(Interval<Double> domain, double slope, double qRef, double tRef) {
		this.domain = domain;
		this.slope = slope;
		this.qRef = qRef;
		this.tRef = tRef;
	}

	/**
	 * decode(): decoding method from an encoded string
	 */
	@Override
	public UniformLinearTemperature decode(String encodedString, Parameters p, Context c) throws Exception {

		// e.g. UniformLinearTemperature([0,2];1;1;1)

		try {
			String s = encodedString.trim();

			if (!s.startsWith("uniformLinearTemperature("))
				throw new Exception("Not a UniformLinearTemperature");

			s = encodedString.replace("uniformLinearTemperature(", "");
			s = s.replace(")", "");
			StringTokenizer st = new StringTokenizer(s, ";");

			domain = (Interval<Double>) Decodable.pleaseDecode(Interval.class, st.nextToken().trim(), p, c);

			slope = Double.parseDouble(st.nextToken().trim());

			qRef = Double.parseDouble(st.nextToken().trim()); // fc+jsm-5.3.2024

			tRef = Double.parseDouble(st.nextToken().trim()); // fc+jsm-5.3.2024

			return new UniformLinearTemperature(domain, slope, qRef, tRef);

		} catch (Exception e) {
			throw new Exception(
					"UniformLinearTemperature.decode (), could not parse this encodedString: " + encodedString, e);
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

		// TODO: reformulate this...
		
		int date = (int) s.getDate();
		double t = c.getEnvironmentFileLoader().getEnvironmentRecord(date).getTemperature ();
		
		double a = Math.abs(t - tRef) / qRef;
		
		System.out.println("UniformLinearTemperature a: "+a);
		
		double temperatureFactor = 1; // Math.exp(-a);

		return BasicFunctions.indicator(domain, x1) * slope * x2 * temperatureFactor;

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
		out.put(UNIFORM_LINEAR_TEMPERATURE, "uniformLinearTemperature(" + domain.encode(Interval.INTERVAL) + ";" + slope
				+ ";" + qRef + ";" + tRef + ")");
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

	public double getqRef() {
		return qRef;
	}

	public double gettRef() {
		return tRef;
	}

	@Override
	public String getEncodedFunction() {
		return encode(UNIFORM_LINEAR_TEMPERATURE);
	}

}