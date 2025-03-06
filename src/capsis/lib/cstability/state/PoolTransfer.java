package capsis.lib.cstability.state;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.distribution.DiscreteDistribution;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.parameter.PoolTransferTraits;

/**
 * State of the model C-STABILITY.
 * 
 * @author J. Sainte-Marie, F. de Coligny - May 2021
 */
@SuppressWarnings("serial")
public class PoolTransfer extends DiscreteDistribution {

	private PoolTransferTraits traits;

	/**
	 * Constructor
	 */
	public PoolTransfer(String pttName, Parameters p) throws Exception {
		super(p.getPoolTransferTraitsMap().get(pttName).getBiochemicalClass().getPolymerization().getDiscretization(),
				new double[p.getPoolTransferTraitsMap().get(pttName).getBiochemicalClass().getPolymerization()
						.getDiscretization().length],
				p.getIntegrationMethod());
		traits = p.getPoolTransferTraitsMap().get(pttName);
	}

	/**
	 * Constructor
	 */
	public PoolTransfer(PoolTransfer pt) throws Exception {
		super(pt.valuesX.clone(), pt.valuesY.clone(), pt.getIntegrationMethod());
		traits = pt.traits;
	}

	/**
	 * evaluate()
	 */
	public void evaluate(Parameters p, Context c, State s) throws Exception {
		Pool originPool = s.getSubstrate().getPool(traits.getBiochemicalClass().getName(), traits.getOriginName());
		this.valuesY = DiscreteDistribution.apply(originPool, p, c, s, traits.getTransferFunction()).getValuesY();
	}

	/**
	 * getTraits()
	 */
	public PoolTransferTraits getTraits() {
		return traits;
	}

}
