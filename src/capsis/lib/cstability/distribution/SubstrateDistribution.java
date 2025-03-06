package capsis.lib.cstability.distribution;

import capsis.lib.cstability.parameter.BiochemicalClass;
import capsis.lib.cstability.parameter.SubstrateAccessibility;

/**
 * A substrate distribution of C-STABILITY
 * 
 * @author J. Sainte-Marie, F. de Coligny - May 2021
 */
@SuppressWarnings("serial")
public class SubstrateDistribution extends DiscretePositiveDistribution implements Cloneable {

	protected BiochemicalClass biochemicalClass;
	protected SubstrateAccessibility accessibility;

	/**
	 * Constructor
	 */
	public SubstrateDistribution(double[] valuesX, double[] valuesY, String integrationMethod,
			BiochemicalClass biochemicalClass, SubstrateAccessibility accessibility) throws Exception {
		super(valuesX, valuesY, integrationMethod);
		this.biochemicalClass = biochemicalClass;
		this.accessibility = accessibility;
	}

	/**
	 * clone()
	 */
	@Override
	public SubstrateDistribution clone() {
		try {
			return new SubstrateDistribution(valuesX.clone(), valuesY.clone(), integrationMethod, biochemicalClass,
					accessibility);
		} catch (Exception e) {
			return null;
		}
	}

	/**
	 * getBiochemicalClass()
	 */
	public BiochemicalClass getBiochemicalClass() {
		return biochemicalClass;
	}

	/**
	 * getAccessibility()
	 */
	public SubstrateAccessibility getAccessibility() {
		return accessibility;
	}
}