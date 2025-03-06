package capsis.lib.cstability.parameter;

import java.io.Serializable;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.distribution.DiscretePositiveDistribution;
import capsis.lib.cstability.filereader.CodedFlags;
import capsis.lib.cstability.function.Function;
import capsis.lib.cstability.util.Encodable;
import capsis.lib.cstability.util.Format;

/**
 * MicrobeSpecies of the model C-STABILITY.
 * 
 * @author J. Sainte-Marie, F. de Coligny - March 2021
 */
@SuppressWarnings("serial")
public class MicrobeSpecies implements Encodable, Serializable {

	public static final Encoder MICROBE_SIGNATURE = new Encoder(CodedFlags.MICROBE_SIGNATURE);
	public static final Encoder MICROBE_ENZYME_PRODUCTION = new Encoder(CodedFlags.MICROBE_ENZYME_PRODUCTION);
	public static final Encoder MICROBE_ASSIMILATION = new Encoder(CodedFlags.MICROBE_ASSIMILATION);
	public static final Encoder MICROBE_MORTALITY = new Encoder(CodedFlags.MICROBE_MORTALITY);
	public static final Set<Encoder> ENCODERS = new HashSet<>();

	static {
		ENCODERS.add(MICROBE_SIGNATURE);
		ENCODERS.add(MICROBE_ENZYME_PRODUCTION);
		ENCODERS.add(MICROBE_ASSIMILATION);
		ENCODERS.add(MICROBE_MORTALITY);
	}

	/**
	 * Instance variables
	 */
	private String name;
	private Map<String, SignatureElement> signatureMap; // Signature: key: biochemicalClassName
	private Map<String, Function> enzymeProductionMap; // Enzyme production: key: biochemicalClassName
	private Map<String, AssimilationElement> assimilationMap; // Assimilation: key: biochemicalClassName
	private Function mortalityFunction; // Mortality

	/**
	 * SignatureElement: Inner Class
	 */
	private class SignatureElement implements Serializable {
		public DiscretePositiveDistribution polymerization;
		public Function polymerizationFunction;
		public double proportion;
	}

	/**
	 * AssimilationElement: Inner Class
	 */
	private class AssimilationElement implements Serializable {
		public Function uptakeFluxFunction;
		public Function carbonUseEfficiencyFunction;

		/**
		 * Constructor
		 */
		public AssimilationElement(Function uptakeFluxFunction, Function carbonUseEfficiencyFunction) {
			this.uptakeFluxFunction = uptakeFluxFunction;
			this.carbonUseEfficiencyFunction = carbonUseEfficiencyFunction;
		}
	}

	/**
	 * Constructor
	 */
	public MicrobeSpecies() {
		signatureMap = new HashMap<>();
		enzymeProductionMap = new HashMap<String, Function>();
		assimilationMap = new HashMap<>();
	}

	public MicrobeSpecies(String name) {
		this();
		this.name = name;
	}

	/**
	 * checkSignature(): check microbe species signature (sum = 1)
	 */
	public void checkSignature() throws Exception {
		double sum = 0;
		for (String signatureName : signatureMap.keySet()) {
			DiscretePositiveDistribution signature = signatureMap.get(signatureName).polymerization;
			sum += signature.getIntegral();
		}
		if (Math.abs(sum - 1.) > 10.e-9)
			throw new Exception("MicrobeSpecies, " + name + " Signature integral equals " + sum + " instead of 1.");

	}

	/**
	 * addSignatureElement()
	 */
	public void addSignatureElement(String biochemicalClassName,
			DiscretePositiveDistribution polymerizationDistribution) {

		SignatureElement signatureElement = new SignatureElement();
		signatureElement.polymerization = polymerizationDistribution;
		signatureMap.put(biochemicalClassName, signatureElement);

	}

	/**
	 * addSignatureElement()
	 */
	public void addSignatureElement(BiochemicalClass biochemicalClass, Function polymerizationFunction,
			double proportion, Parameters p, Context c) throws Exception {

		DiscretePositiveDistribution polymerizationDistribution = DiscretePositiveDistribution
				.getZeroDiscreteDistribution(biochemicalClass.getPolymerization().getDiscretization(),
						p.getIntegrationMethod());

		polymerizationDistribution.setValuesY(p, c, null, polymerizationFunction);
		polymerizationDistribution.proportionalize(proportion);

		SignatureElement signatureElement = new SignatureElement();
		signatureElement.polymerization = polymerizationDistribution;
		signatureElement.proportion = proportion;
		signatureElement.polymerizationFunction = polymerizationFunction;

		signatureMap.put(biochemicalClass.getName(), signatureElement);
	}

	/**
	 * addEnzyme()
	 */
	public void addEnzyme(String enzymeName, Function productionFunction) throws Exception {
		if (enzymeProductionMap.containsKey(enzymeName))
			throw new Exception("MicrobeSpecies, " + name + " Enzyme, " + enzymeName + " is defined twice.");
		enzymeProductionMap.put(enzymeName, productionFunction);
	}

	/**
	 * addAssimilationElement()
	 */
	public void addAssimilationElement(String biochemicalClassName, Function uptakeFluxFunction,
			Function carbonUseEfficiencyFunction) throws Exception {
		if (assimilationMap.containsKey(biochemicalClassName))
			throw new Exception(
					"MicrobeSpecies, " + name + " Assimilation for " + biochemicalClassName + " is defined twice.");
		AssimilationElement ae = new AssimilationElement(uptakeFluxFunction, carbonUseEfficiencyFunction);
		assimilationMap.put(biochemicalClassName, ae);
	}

	/**
	 * setMortalityFunction()
	 */
	public void setMortalityFunction(Function mortalityFunction) {
		this.mortalityFunction = mortalityFunction;
	}

	/**
	 * setName()
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * getName()
	 */
	public String getName() {
		return name;
	}

	/**
	 * getEnzymeNames()
	 */
	public Set<String> getEnzymeNames() {
		return enzymeProductionMap.keySet();
	}

	/**
	 * getEnzymeProductionMap()
	 */
	public Map<String, Function> getEnzymeProductionMap() {
		return enzymeProductionMap;
	}

	/**
	 * getMortalityFunction()
	 */
	public Function getMortalityFunction() {
		return mortalityFunction;
	}

	/**
	 * getSignatureDistribution()
	 */
	public DiscretePositiveDistribution getSignatureDistribution(String biochemicalClassName) {
		return signatureMap.get(biochemicalClassName).polymerization;
	}

	/**
	 * getSignatureBCNames()
	 */
	public Set<String> getSignatureBCNames() {
		return signatureMap.keySet();
	}

	/**
	 * getUptakeFluxFunction()
	 */
	public Function getUptakeFluxFunction(String biochemicalClassName) {
		return assimilationMap.get(biochemicalClassName).uptakeFluxFunction;
	}

	/**
	 * getCarbonUseEfficiencyFunction()
	 */
	public Function getCarbonUseEfficiencyFunction(String biochemicalClassName) {
		return assimilationMap.get(biochemicalClassName).carbonUseEfficiencyFunction;
	}

	/**
	 * getAssimilationBCNames()
	 */
	public Set<String> getAssimilationBCNames() {
		return assimilationMap.keySet();
	}

	/**
	 * toString()
	 */
	@Override
	public String toString() {
		String CR = "\n";
		String CR2 = "\n  ";

		return "MicrobeSpecies, " + name + CR2 + "signatureMap: " + Format.printKeys(signatureMap) + CR2
				+ "enzymeProductionMap: " + Format.printKeys(enzymeProductionMap) + CR2 + "assimilationMap: "
				+ Format.printKeys(assimilationMap) + CR;
	}

	@Override
	public Map<Encoder, String> encode() {

		Map<Encoder, String> encodedMap = new HashMap<>();

		// MICROBE_SIGNATURE
		String encodedSignature = "";
		for (String bcName : signatureMap.keySet())
			encodedSignature += CodedFlags.MICROBE_SIGNATURE + Encodable.TAB + getName() + Encodable.TAB
					+ signatureMap.get(bcName).proportion + Encodable.TAB
					+ signatureMap.get(bcName).polymerizationFunction.getEncodedFunction();
		encodedMap.put(MICROBE_SIGNATURE, encodedSignature);

		// MICROBE_ENZYME_PRODUCTION
		String encodedEnzymeProduction = "";
		for (String enzymeName : enzymeProductionMap.keySet())
			encodedEnzymeProduction += CodedFlags.MICROBE_ENZYME_PRODUCTION + Encodable.TAB + getName() + Encodable.TAB
					+ enzymeName + Encodable.TAB + enzymeProductionMap.get(enzymeName).getEncodedFunction();
		encodedMap.put(MICROBE_ENZYME_PRODUCTION, encodedEnzymeProduction);

		// MICROBE_ASSIMILATION
		String encodedAssimilation = "";
		for (String bcName : assimilationMap.keySet())
			encodedAssimilation += CodedFlags.MICROBE_ASSIMILATION + Encodable.TAB + getName() + Encodable.TAB + bcName
					+ Encodable.TAB + assimilationMap.get(bcName).uptakeFluxFunction.getEncodedFunction()
					+ Encodable.TAB + assimilationMap.get(bcName).carbonUseEfficiencyFunction.getEncodedFunction();
		encodedMap.put(MICROBE_ASSIMILATION, encodedAssimilation);

		// MICROBE_MORTALITY
		String encodedMortality = CodedFlags.MICROBE_MORTALITY + TAB + getName() + TAB
				+ mortalityFunction.getEncodedFunction();
		encodedMap.put(MICROBE_MORTALITY, encodedMortality);

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
