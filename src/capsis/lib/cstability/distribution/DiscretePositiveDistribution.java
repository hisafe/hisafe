package capsis.lib.cstability.distribution;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.function.Function;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.state.State;
import capsis.lib.cstability.util.Log;

/**
 * A discrete positive distribution.
 * 
 * @author J. Sainte-Marie, F. de Coligny - April 2021
 */
@SuppressWarnings("serial")
public class DiscretePositiveDistribution extends DiscreteDistribution implements Cloneable {

	/**
	 * Constructor
	 */
	public DiscretePositiveDistribution(double[] valuesX, double[] valuesY, String integrationMethod) throws Exception {
		super(valuesX, valuesY, integrationMethod);
		checkIfNonNegative();
	}

	/**
	 * clone()
	 */
	@Override
	public DiscretePositiveDistribution clone() {
		try {
			return new DiscretePositiveDistribution(valuesX.clone(), valuesY.clone(), integrationMethod);
		} catch (Exception e) {
			return null;
		}
	}

	/**
	 * checkIfNonNegative()
	 */
	public void checkIfNonNegative() throws Exception {
		try {
			for (int i = 0; i < this.getLength(); ++i) {
				if (valuesY[i] < 0d)
					throw new Exception("DiscretePositiveDistribution.checkIfNonNegative(), Negative value in valuesY");
			}
		} catch (Exception e) {
			Log.println("DiscretePositiveDistribution.checkIfNonNegative()", "illegal value", e);
			throw e;
		}
	}

	/**
	 * getZeroDiscreteDistribution()
	 */
	public static DiscretePositiveDistribution getZeroDiscreteDistribution(double[] valuesX, String integrationMethod)
			throws Exception {
		double[] valuesY = new double[valuesX.length];
		return new DiscretePositiveDistribution(valuesX, valuesY, integrationMethod);
	}

	/**
	 * getZeroDiscreteDistribution()
	 */
	public static DiscretePositiveDistribution getZeroDiscreteDistribution(DiscreteDistribution dd) throws Exception {
		return getZeroDiscreteDistribution(dd.getValuesX(), dd.integrationMethod);
	}

	/**
	 * getComplementary()
	 */
	public DiscretePositiveDistribution getComplementary() throws Exception {
		DiscreteDistribution dd = super.getComplementary();
		return new DiscretePositiveDistribution(dd.getValuesX(), dd.getValuesY(), dd.getIntegrationMethod());
	}

	/**
	 * add()
	 */
	public void add(DiscreteDistribution dd) throws Exception {
		super.add(dd);
		checkIfNonNegative();
	}

	/**
	 * setValuesY()
	 */
	public void setValuesY(Parameters p, Context c, State s, Function f) throws Exception {
		super.setValuesY(p, c, s, f);
		checkIfNonNegative();
	}

	/**
	 * setValuesY()
	 */
	public void setValuesY(double[] valuesY, Parameters p, Context c, State s, Function f) throws Exception {
		super.setValuesY(valuesY, p, c, s, f);
		checkIfNonNegative();
	}

	/**
	 * toString()
	 */
	@Override
	public String toString() {
		String s = "DiscretePositiveDistribution, \n";
		for (int i = 0; i < length; i++) {
			s += "x: " + valuesX[i] + ", y: " + valuesY[i] + "\n";
		}
		return s;

	}
}
