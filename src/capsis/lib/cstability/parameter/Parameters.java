package capsis.lib.cstability.parameter;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import capsis.lib.cstability.distribution.DiscretePositiveDistribution;
import capsis.lib.cstability.util.Format;

/**
 * Parameters of the model C-STABILITY
 *
 * @author J. Sainte-Marie, F. de Coligny - February 2021
 */
@SuppressWarnings("serial")
public class Parameters implements Serializable {

	// DiscreteDistribution.INTEGRATION_RECTANGLE_LEFT, INTEGRATION_RECTANGLE_RIGHT,
	// INTEGRATION_TRAPEZE
	private String integrationMethod;
	private double userPolymerizationStep = -1;

	private Map<String, BiochemicalClass> biochemicalClassMap;
	// key is biochemicalClassName
	private Map<String, List<SubstrateAccessibility>> substrateAccessibilityMap;
	private Map<String, MicrobeSpecies> microbeSpeciesMap;
	private Map<String, EnzymeTraits> enzymeTraitsMap;
	// key is poolTransferTraits key (bcName + origin + arrival)
	private Map<String, PoolTransferTraits> poolTransferTraitsMap;

	/**
	 * Constructor
	 */
	public Parameters() {
		biochemicalClassMap = new HashMap<>();
		substrateAccessibilityMap = new HashMap<>();
		microbeSpeciesMap = new HashMap<>();
		enzymeTraitsMap = new HashMap<>();
		poolTransferTraitsMap = new HashMap<>();
	}

	/**
	 * setIntegrationMethod()
	 */
	public void setIntegrationMethod(String im) throws Exception {

		boolean correct = im.equals(DiscretePositiveDistribution.INTEGRATION_RECTANGLE_LEFT)
				|| im.equals(DiscretePositiveDistribution.INTEGRATION_RECTANGLE_RIGHT)
				|| im.equals(DiscretePositiveDistribution.INTEGRATION_TRAPEZE);

		if (!correct)
			throw new Exception("Parameters, wrong integrationMethod, expected: "
					+ DiscretePositiveDistribution.INTEGRATION_METHODS);

		integrationMethod = im;
	}

	/**
	 * setUserPolymerizationStep()
	 */
	public void setUserPolymerizationStep(double userPolymerizationStep) {
		this.userPolymerizationStep = userPolymerizationStep;
	}

	/**
	 * addBiochemicalClass()
	 */
	public void addBiochemicalClass(BiochemicalClass bc) {
		biochemicalClassMap.put(bc.getName(), bc);
	}

	/**
	 * addSubstrateAccessibility()
	 */
	public void addSubstrateAccessibility(String biochemicalClassName, SubstrateAccessibility sa) throws Exception {
		if (!biochemicalClassMap.keySet().contains(biochemicalClassName))
			throw new Exception(
					"Unknown biochemicalClass name: " + biochemicalClassName + " for SubstrateAccessibility");
		if (!substrateAccessibilityMap.keySet().contains(biochemicalClassName)) {
			List<SubstrateAccessibility> sal = new ArrayList<>();
			sal.add(sa);
			substrateAccessibilityMap.put(biochemicalClassName, sal);
		} else {
			substrateAccessibilityMap.get(biochemicalClassName).add(sa);
		}
	}

	/**
	 * addMicrobeSpecies()
	 */
	public void addMicrobeSpecies(MicrobeSpecies ms) throws Exception {
		for (String enzymeName : ms.getEnzymeNames()) {
			if (!enzymeTraitsMap.keySet().contains(enzymeName))
				throw new Exception("Unknown enzyme name: " + enzymeName + " for MicrobeSpecies: " + ms.getName());
		}
		microbeSpeciesMap.put(ms.getName(), ms);
	}

	/**
	 * addEnzymeTraits()
	 */
	public void addEnzymeTraits(EnzymeTraits et) {
		enzymeTraitsMap.put(et.getName(), et);
	}

	/**
	 * addPoolTransferTraits()
	 */
	public void addPoolTransferTraits(PoolTransferTraits ptt) {
		poolTransferTraitsMap.put(ptt.getKey(), ptt);
	}

	/**
	 * getIntegrationMethod()
	 */
	public String getIntegrationMethod() {
		return integrationMethod;
	}

	/**
	 * getUserPolymerizationStep()
	 */
	public double getUserPolymerizationStep() {
		return userPolymerizationStep;
	}

	/**
	 * getBiochemicalClassMap()
	 */
	public Map<String, BiochemicalClass> getBiochemicalClassMap() {
		return biochemicalClassMap;
	}

	/**
	 * getSubstrateAccessibilityMap()
	 */
	public Map<String, List<SubstrateAccessibility>> getSubstrateAccessibilityMap() {
		return substrateAccessibilityMap;
	}

	/**
	 * getSubstrateAccessibilities()
	 */
	public List<SubstrateAccessibility> getSubstrateAccessibilities(String bcName) {
		return substrateAccessibilityMap.get(bcName);
	}

	/**
	 * getMicrobeSpeciesMap()
	 */
	public Map<String, MicrobeSpecies> getMicrobeSpeciesMap() {
		return microbeSpeciesMap;
	}

	/**
	 * getEnzymeTraitsMap()
	 */
	public Map<String, EnzymeTraits> getEnzymeTraitsMap() {
		return enzymeTraitsMap;
	}

	/**
	 * getPoolTransferTraitsMap()
	 */
	public Map<String, PoolTransferTraits> getPoolTransferTraitsMap() {
		return poolTransferTraitsMap;
	}

	/**
	 * toString()
	 */
	@Override
	public String toString() {
		String CR = "\n";
		StringBuffer b = new StringBuffer("--- Parameters");

		b.append(CR);
		b.append("integrationMethod: " + integrationMethod);

		b.append(CR);
		b.append("userPolymerizationStep: " + userPolymerizationStep);

		b.append(CR);
		b.append("biochemicalClassMap: " + Format.toString(biochemicalClassMap));

		b.append(CR);
		b.append("microbeSpeciesMap: " + Format.toString(microbeSpeciesMap));

		b.append(CR);
		b.append("enzymeTraitsMap: " + Format.toString(enzymeTraitsMap));

		b.append(CR);
		b.append("poolTransferMap: " + Format.toString(poolTransferTraitsMap));

		b.append(CR);
		b.append("--- end-of-Parameters");

		return "" + b;
	}
}
