package capsis.lib.cstability.context.poolinput;

import capsis.lib.cstability.distribution.SubstrateDistribution;
import capsis.lib.cstability.parameter.BiochemicalClass;
import capsis.lib.cstability.parameter.SubstrateAccessibility;

/**
 * A PoolInput of the model C-STABILITY. An input is a flux.
 *
 * @author J. Sainte-Marie, F. de Coligny - May 2021
 */
@SuppressWarnings("serial")
public class PoolInput extends SubstrateDistribution {

	private double carbonFlux; // in g.tunit-1
	
	/**
	 * Constructor
	 */
	public PoolInput(double[] valuesX, double[] valuesY, String integrationMethod, BiochemicalClass biochemicalClass,
			SubstrateAccessibility accessibility) throws Exception {
		super(valuesX, valuesY, integrationMethod, biochemicalClass, accessibility);
		carbonFlux = integral;
	}

	/**
	 * getCarbonFlux()
	 */
	public double getCarbonFlux() {
		return carbonFlux;
	}
	
}
