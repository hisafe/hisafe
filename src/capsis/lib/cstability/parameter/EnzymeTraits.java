package capsis.lib.cstability.parameter;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.filereader.CodedFlags;
import capsis.lib.cstability.filereader.Decodable;
import capsis.lib.cstability.function.Function;
import capsis.lib.cstability.function.util.EnzymeKernelMatrix;
import capsis.lib.cstability.state.State;
import capsis.lib.cstability.util.Encodable;
import capsis.lib.cstability.util.Interval;

/**
 * EnzymeTraits of the model C-STABILITY.
 * 
 * @author J. Sainte-Marie, F. de Coligny - March 2021
 */
@SuppressWarnings("serial")
public class EnzymeTraits implements Encodable, Decodable, Serializable {

	private String name;
	private BiochemicalClass biochemicalClass;
	private Interval<Double> depolymerizationDomain;
	private Function depolymerizationRateFunction;
	private EnzymeKernelMatrix kernelMatrix;

	public static final Encoder ENZYME_TRAITS = new Encoder(CodedFlags.ENZYME_TRAITS);

	public static final Set<Encoder> ENCODERS = new HashSet<>();
	static {
		ENCODERS.add(ENZYME_TRAITS);
	}

	/**
	 * Default constructor
	 */
	public EnzymeTraits() {

	}

	/**
	 * Constructor
	 */
	public EnzymeTraits(String name, BiochemicalClass biochemicalClass, Interval<Double> depolymerizationDomain,
			Function depolymerizationRateFunction, Function kernelFunction, String kernelIntegrationMethod,
			Parameters p, Context c) throws Exception {
		this.name = name;
		this.biochemicalClass = biochemicalClass;
		if ((double) biochemicalClass.getPolymerization().getMin() > (double) depolymerizationDomain.getMin()
				|| (double) biochemicalClass.getPolymerization().getMax() < (double) depolymerizationDomain.getMax())
			throw new Exception(
					"EnzymeTraits.constructor(), depolymerizationDomain exceed biochemical class polymerization range.");
		this.depolymerizationDomain = depolymerizationDomain;
		this.depolymerizationRateFunction = depolymerizationRateFunction;
		this.kernelMatrix = new EnzymeKernelMatrix(biochemicalClass.getPolymerization(), kernelFunction,
				kernelIntegrationMethod, p, c, new State());
	}

	/**
	 * decode(): decoding method from an encoded string
	 */
	@Override
	public EnzymeTraits decode(String encodedString, Parameters p, Context c) throws Exception {

		// e.g. ENZYME_TRAITS \tab cellulolysis \tab cellulose \tab [0,2] \tab
		// uniformLinear([0,2];1.8) \tab kernelAlpha([0,2];5) \tab
		// INTEGRAL_KERNEL_INTEGRATION
		try {
			String s = encodedString.trim();
			StringTokenizer st = new StringTokenizer(s, "\t");

			String flag = st.nextToken().trim();

			if (flag.equals(CodedFlags.ENZYME_TRAITS)) {

				String name = st.nextToken().trim();

				String biochemicalClassName = st.nextToken().trim();
				BiochemicalClass biochemicalClass = p.getBiochemicalClassMap().get(biochemicalClassName);
				if (biochemicalClass == null)
					throw new Exception("Unknown biochemicalClass name: " + biochemicalClassName);

				Interval<Double> depolymerizationDomain = (Interval<Double>) Decodable.pleaseDecode(Interval.class,
						st.nextToken().trim(), p, c);
				Function depolymerizationRateFunction = Function.getFunction(st.nextToken().trim(), p, c);
				Function kernelFunction = Function.getFunction(st.nextToken().trim(), p, c);
				String kernelIntegrationMethod = st.nextToken().trim();

				return new EnzymeTraits(name, biochemicalClass, depolymerizationDomain, depolymerizationRateFunction,
						kernelFunction, kernelIntegrationMethod, p, c);
			} else {
				throw new Exception("Wrong flag, expect " + CodedFlags.ENZYME_TRAITS);
			}

		} catch (Exception e) {
			throw new Exception("EnzymeTraits.decode (), could not parse this encodedString: " + encodedString, e);
		}

	}

	/**
	 * getName()
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * getBiochemicalClass()
	 */
	public BiochemicalClass getBiochemicalClass() {
		return biochemicalClass;
	}

	/**
	 * getDepolymerizationDomain()
	 */
	public Interval<Double> getDepolymerizationDomain() {
		return depolymerizationDomain;
	}

	/**
	 * getDepolymerizationRateFunction()
	 */
	public Function getDepolymerizationRateFunction() {
		return depolymerizationRateFunction;
	}

	/**
	 * getKernelMatrix()
	 */
	public EnzymeKernelMatrix getKernelMatrix() {
		return kernelMatrix;
	}

	/**
	 * toString()
	 */
	@Override
	public String toString() {
		return "EnzymeTraits, name: " + name + ", biochemicalClass: " + biochemicalClass;
	}

	@Override
	public Map<Encoder, String> encode() {

		Map<Encoder, String> encodedMap = new HashMap<>();
		encodedMap.put(ENZYME_TRAITS,
				CodedFlags.ENZYME_TRAITS + TAB + getName() + TAB + getBiochemicalClass().getName() + TAB
						+ getDepolymerizationDomain().encode(Polymerization.INTERVAL) + TAB
						+ depolymerizationRateFunction.getEncodedFunction() + TAB
						+ kernelMatrix.getKernelFunction().getEncodedFunction() + TAB
						+ kernelMatrix.getKernelIntegrationMethod());
		return encodedMap;
	}

	@Override
	public String encode(Encoder encoder) {
		return encode().get(encoder);
	}

	@Override
	public Set<Encoder> getEncoders() {
		return ENCODERS;
	}

}
