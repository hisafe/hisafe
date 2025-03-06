package capsis.lib.cstability.context.poolinput;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.function.Function;
import capsis.lib.cstability.function.util.TwoVariables;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.state.Pool;
import capsis.lib.cstability.state.State;

/**
 * A manager of pool temporal inputs of C-STABILITY with a function
 * 
 * @author J. Sainte-Marie, F. de Coligny - May 2021
 */
public class FunctionalPoolInputManager extends PoolInputManager {

	private Function function;

	/**
	 * Constructor
	 */
	public FunctionalPoolInputManager(String bcName, String accessibilityKey, Function function) throws Exception {
		super(bcName, accessibilityKey);
		if (!function.expectedVariables().equals(TwoVariables.class))
			throw new Exception(
					"FunctionalPoolInputManager.constructor() expects a TwoVariable function, get " + function);
		this.function = function;
	}

	/**
	 * getInput()
	 */
	@Override
	public PoolInput getInput(Parameters p, Context c, State s, double date) throws Exception {
		Pool pool = s.getSubstrate().getPool(bcName, accessibilityKey);
		double[] valuesX = pool.getValuesX();
		double[] valuesY = new double[valuesX.length];
		for (int i = 0; i < valuesX.length; ++i) {
			valuesY[i] = function.execute(p, c, s, new TwoVariables(date, valuesX[i]));
		}
		return new PoolInput(valuesX, valuesY, pool.getIntegrationMethod(), pool.getBiochemicalClass(),
				pool.getAccessibility());
	}

}
