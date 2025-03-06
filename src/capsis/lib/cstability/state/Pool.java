package capsis.lib.cstability.state;

import java.io.Serializable;

import capsis.lib.cstability.distribution.DiscreteDistribution;
import capsis.lib.cstability.distribution.SubstrateDistribution;
import capsis.lib.cstability.parameter.BiochemicalClass;
import capsis.lib.cstability.parameter.SubstrateAccessibility;

/**
 * A pool of substrate of C-STABILITY
 * 
 * @author J. Sainte-Marie, F. de Coligny - February 2021
 */
@SuppressWarnings("serial")
public class Pool extends SubstrateDistribution implements Serializable {

	private double carbonMass; // in g
	private PoolKey poolKey;

	/**
	 * Constructor
	 */
	public Pool(double[] valuesX, double[] valuesY, String integrationMethod, BiochemicalClass biochemicalClass,
			SubstrateAccessibility accessibility) throws Exception {
		super(valuesX, valuesY, integrationMethod, biochemicalClass, accessibility);
		carbonMass = integral;
		poolKey = new PoolKey(biochemicalClass, accessibility);
	}

	/**
	 * copy()
	 */
	public Pool copy() throws Exception {
		return new Pool(valuesX.clone(), valuesY.clone(), integrationMethod, biochemicalClass, accessibility);
	}

	/**
	 * getEmptySubstratePool()
	 */
	public static Pool getEmptySubstratePool(BiochemicalClass bc, SubstrateAccessibility sa, String integrationMethod)
			throws Exception {
		DiscreteDistribution zeroDD = DiscreteDistribution
				.getZeroDiscreteDistribution(bc.getPolymerization().getDiscretization(), integrationMethod);
		return new Pool(zeroDD.getValuesX(), zeroDD.getValuesY(), integrationMethod, bc, sa);
	}

	/**
	 * isAccessible()
	 */
	public boolean isAccessible() { // Shortcut
		return accessibility.isAccessible();
	}

	/**
	 * getCarbonMass()
	 */
	public double getCarbonMass() {
		carbonMass = integral;
		return carbonMass;
	}

	/**
	 * getPoolKey()
	 */
	public PoolKey getPoolKey() {
		return poolKey;
	}

	/**
	 * getKey()
	 */
	public String getKey() {// Shortcut
		return poolKey.getKey();
	}

	/**
	 * toString()
	 */
	@Override
	public String toString() {
		return "Pool, biochemical class: " + biochemicalClass.getName() + ", accessibility: "
				+ accessibility.getStatus();
	}
}
