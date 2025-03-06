package capsis.lib.cstability.distribution;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.function.Function;
import capsis.lib.cstability.function.util.OneVariable;
import capsis.lib.cstability.function.util.TwoVariables;
import capsis.lib.cstability.function.util.ZeroVariable;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.state.State;
import capsis.lib.cstability.util.Interval;
import capsis.lib.cstability.util.Matrix;
import capsis.lib.cstability.util.XYseries;

/**
 * A discrete distribution.
 * 
 * @author J. Sainte-Marie, F. de Coligny - February 2021
 */
@SuppressWarnings("serial")
public class DiscreteDistribution extends XYseries implements Cloneable, Serializable {

	public static final String INTEGRATION_TRAPEZE = "INTEGRATION_TRAPEZE";
	public static final String INTEGRATION_RECTANGLE_LEFT = "INTEGRATION_RECTANGLE_LEFT";
	public static final String INTEGRATION_RECTANGLE_RIGHT = "INTEGRATION_RECTANGLE_RIGHT";

	public static final List<String> INTEGRATION_METHODS = new ArrayList<>(
			Arrays.asList(INTEGRATION_TRAPEZE, INTEGRATION_RECTANGLE_LEFT, INTEGRATION_RECTANGLE_RIGHT));

	protected String integrationMethod;
	protected double integral;

	/**
	 * Constructor
	 */
	public DiscreteDistribution(double[] valuesX, double[] valuesY, String integrationMethod) throws Exception {

		super(valuesX, valuesY);

		if (valuesX.length != valuesY.length)
			throw new Exception("DiscreteDistribution.constructor, Dimension mismatch: X length, " + valuesX.length
					+ " != Y length: " + valuesY.length);

		if (getLength() < 2)
			throw new Exception("DiscreteDistribution.constructor, Dimension is inferior to 2");

		if (!integrationMethod.equals(INTEGRATION_RECTANGLE_LEFT)
				&& !integrationMethod.equals(INTEGRATION_RECTANGLE_RIGHT)
				&& !integrationMethod.equals(INTEGRATION_TRAPEZE))
			throw new Exception("DiscreteDistribution.constructor, Wrong integrationMethod: " + integrationMethod);

		for (int i = 0; i < getLength() - 1; ++i)
			if (valuesX[i] >= valuesX[i + 1])
				throw new Exception("DiscreteDistribution.constructor, Wrong valuesX sort");

		this.integrationMethod = integrationMethod;
		integrate();
	}

	/**
	 * clone()
	 */
	@Override
	public DiscreteDistribution clone() throws CloneNotSupportedException {
		try {
			return new DiscreteDistribution(valuesX.clone(), valuesY.clone(), integrationMethod);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * Methods
	 */

	/**
	 * getZeroDiscreteDistribution(): returns a DiscreteDistribution with values
	 * equal to zero for Y.
	 */
	public static DiscreteDistribution getZeroDiscreteDistribution(double[] valuesX, String integrationMethod)
			throws Exception {
		double[] valuesY = new double[valuesX.length];
		return new DiscreteDistribution(valuesX, valuesY, integrationMethod);
	}

	/**
	 * getZeroDiscreteDistribution()
	 */
	public static DiscreteDistribution getZeroDiscreteDistribution(DiscreteDistribution dd) throws Exception {
		return getZeroDiscreteDistribution(dd.valuesX, dd.integrationMethod);
	}

	/**
	 * getComplementary()
	 */
	public DiscreteDistribution getComplementary() throws Exception {
		double[] newValuesY = new double[valuesY.length];
		for (int i = 0; i < valuesY.length; i++) {
			if (valuesY[i] < 0 || valuesY[i] > 1)
				throw new Exception("DiscreteDictribution.getComplementary(), a value is not in [0,1]: " + valuesY[i]);
			newValuesY[i] = 1 - valuesY[i];
		}
		return new DiscreteDistribution(valuesX, newValuesY, integrationMethod);
	}

	/**
	 * apply()
	 */
	public static DiscreteDistribution apply(DiscreteDistribution dd, Parameters p, Context c, State s, Function f)
			throws Exception {
		DiscreteDistribution newDd = DiscreteDistribution.getZeroDiscreteDistribution(dd);
		newDd.setValuesY(dd.getValuesY(), p, c, s, f);
		return newDd;
	}

	/**
	 * Integration
	 */

	/**
	 * integrate()
	 */
	private void integrate() {
		if (integrationMethod.equals(INTEGRATION_RECTANGLE_LEFT))
			integral = integrateRectangleLeft();
		else if (integrationMethod.equals(INTEGRATION_RECTANGLE_RIGHT))
			integral = integrateRectangleRight();
		else if (integrationMethod.equals(INTEGRATION_TRAPEZE))
			integral = integrateTrapeze();
	}

	/**
	 * integrateRectangleLeft()
	 */
	private double integrateRectangleLeft() {
		double integral = 0d;
		for (int i = 0; i < length - 1; i++)
			integral += (valuesX[i + 1] - valuesX[i]) * valuesY[i];
		return integral;
	}

	/**
	 * integrateRectangleRight()
	 */
	private double integrateRectangleRight() {
		double integral = 0d;
		for (int i = 0; i < length - 1; i++)
			integral += (valuesX[i + 1] - valuesX[i]) * valuesY[i + 1];
		return integral;
	}

	/**
	 * integrateTrapeze()
	 */
	private double integrateTrapeze() {
		double integral = 0d;
		for (int i = 0; i < length - 1; i++)
			integral += (valuesX[i + 1] - valuesX[i]) * (valuesY[i] + valuesY[i + 1]) / 2d;
		return integral;
	}

	/**
	 * getIntegral()
	 */
	public double getIntegral(Interval interval) throws Exception {

		double integralValue = 0;

		double xMin = valuesX[0];
		double xMax = valuesX[valuesX.length - 1];
		double iMin = (double) interval.getMin();
		double iMax = (double) interval.getMax();
		if (iMin < xMin || iMax > xMax)
			throw new Exception(
					"DiscreteDistribution.integrate(), integration interval mismatches with distribution extremums");

		int indexMin = getPreviousIndex(iMin);
		int indexMax = getNextIndex(iMax);

		if (indexMin + 1 == indexMax) {
			// interval is included between two successive points of the distribution
			// the integral is based on interpolation
			// i.e. x[indexMin] < imin < imax < x[indexMin]
			double[] X = new double[] { iMin, iMax };
			double[] Y = new double[] { interpolate(iMin), interpolate(iMax) };
			integralValue += (new DiscreteDistribution(X, Y, this.integrationMethod)).getIntegral();

		} else {
			// left interpolation required;
			if (!(valuesX[indexMin] == iMin)) {
				double[] X = new double[] { iMin, valuesX[indexMin] };
				double[] Y = new double[] { interpolate(iMin), valuesY[indexMin] };
				integralValue += (new DiscreteDistribution(X, Y, this.integrationMethod)).getIntegral();
				indexMin += 1;
			}
			// right interpolation required
			if (!(valuesX[indexMax] == iMax)) {
				double[] X = new double[] { valuesX[indexMax], iMax };
				double[] Y = new double[] { valuesY[indexMax], interpolate(iMax) };
				integralValue += (new DiscreteDistribution(X, Y, this.integrationMethod)).getIntegral();
				indexMax -= 1;
			}

			if (indexMin != indexMax)
				integralValue += (new DiscreteDistribution(Arrays.copyOfRange(valuesX, indexMin, indexMax),
						Arrays.copyOfRange(valuesY, indexMin, indexMax), this.integrationMethod)).getIntegral();
		}
		return integralValue;
	}

	/**
	 * interpolate(): linear interpolation
	 */
	public double interpolate(double x) throws Exception {
		double xMin = valuesX[0];
		double xMax = valuesX[valuesX.length - 1];
		if (x < xMin || x > xMax)
			throw new Exception("DiscreteDistribution.interpolate(), x value mismatches with distribution extremums");
		int i = getPreviousIndex(x);
		return valuesY[i] + (valuesY[i + 1] - valuesY[i]) * (x - valuesX[i]) / (valuesX[i + 1] - valuesX[i]);
	}

	/**
	 * getPreviousIndex()
	 */
	private int getPreviousIndex(double x) {
		int index = 0;
		for (int i = 0; i < valuesX.length - 1; ++i)
			if (valuesX[i] <= x & x < valuesX[i + 1]) {
				index = i;
				break;
			}
		return index;
	}

	/**
	 * getNextIndex()
	 */
	private int getNextIndex(double x) {
		int index = 0;
		for (int i = 0; i < valuesX.length - 1; ++i)
			if (valuesX[i] < x & x <= valuesX[i + 1]) {
				index = i + 1;
				break;
			}
		return index;
	}

	/**
	 * Non-static operations
	 */

	/**
	 * normalize()
	 */
	public void normalize() {
		double integral = getIntegral();
		if (!(integral == 0d))
			for (int i = 0; i < length; i++)
				this.valuesY[i] /= integral;
		integrate();
	}

	/**
	 * proportionalize()
	 */
	public void proportionalize(double proportion) {
		double integral = getIntegral();
		if (!(integral == 0d))
			for (int i = 0; i < length; i++)
				this.valuesY[i] *= (proportion / integral);

		integrate();
	}

	/**
	 * add()
	 */
	public void add(DiscreteDistribution dd) throws Exception {
		for (int i = 0; i < this.length; i++) {
			if (this.valuesX[i] != dd.valuesX[i])
				throw new Exception("DiscreteDistribution.add(), mismatch of discretization");
			this.valuesY[i] += dd.valuesY[i];
		}
		integrate();
	}

	/**
	 * substract()
	 */
	public void substract(DiscreteDistribution dd) throws Exception {
		for (int i = 0; i < this.length; i++) {
			if (this.valuesX[i] != dd.valuesX[i])
				throw new Exception("DiscreteDistribution.sub(), mismatch of discretization");
			this.valuesY[i] -= dd.valuesY[i];
		}
		integrate();
	}

	/**
	 * Static operations
	 */

	/**
	 * add()
	 */
	public static DiscreteDistribution add(DiscreteDistribution d1, DiscreteDistribution d2) throws Exception {
		for (int i = 0; i < d1.length; i++)
			if (d1.valuesX[i] != d2.valuesX[i])
				throw new Exception("DiscreteDistribution.sub(), mismatch of discretization");

		if (d1.integrationMethod != d2.integrationMethod)
			throw new Exception("DiscreteDistribution.sub(), mismatch of integrationMethod");

		Matrix addition = Matrix.addition(Matrix.arrayToLine(d1.valuesY), Matrix.arrayToLine(d2.valuesY));

		return new DiscreteDistribution(d1.valuesX, Matrix.lineToArray(addition), d1.integrationMethod);
	}

	/**
	 * sub()
	 */
	public static DiscreteDistribution sub(DiscreteDistribution d1, DiscreteDistribution d2) throws Exception {
		for (int i = 0; i < d1.length; i++)
			if (d1.valuesX[i] != d2.valuesX[i])
				throw new Exception("DiscreteDistribution.sub(), mismatch of discretization");

		if (d1.integrationMethod != d2.integrationMethod)
			throw new Exception("DiscreteDistribution.sub(), mismatch of integrationMethod");

		Matrix substraction = Matrix.substraction(Matrix.arrayToLine(d1.valuesY), Matrix.arrayToLine(d2.valuesY));

		return new DiscreteDistribution(d1.valuesX, Matrix.lineToArray(substraction), d1.integrationMethod);
	}

	/**
	 * mult()
	 */
	public static DiscreteDistribution mult(double scalar, DiscreteDistribution d) throws Exception {
		Matrix product = Matrix.product(scalar, Matrix.arrayToLine(d.valuesY));
		return new DiscreteDistribution(d.valuesX, Matrix.lineToArray(product), d.integrationMethod);
	}

	/**
	 * mult()
	 */
	public static DiscreteDistribution mult(DiscreteDistribution d1, DiscreteDistribution d2) throws Exception {
		for (int i = 0; i < d1.length; i++)
			if (d1.valuesX[i] != d2.valuesX[i])
				throw new Exception("DiscreteDistribution.mult(), mismatch of discretization");

		if (d1.integrationMethod != d2.integrationMethod)
			throw new Exception("DiscreteDistribution.mult(), mismatch of integrationMethod");

		Matrix product = Matrix.productByElement(Matrix.arrayToLine(d1.valuesY), Matrix.arrayToLine(d2.valuesY));

		return new DiscreteDistribution(d1.valuesX, Matrix.lineToArray(product), d1.integrationMethod);
	}

	/**
	 * mult()
	 */
	public static DiscreteDistribution mult(Matrix m, DiscreteDistribution d) throws Exception {
		if (m == null || d == null)
			throw new Exception("DiscreteDistribution.mult(), m: " + m + " d: " + d);

		if (m.getNColumns() != d.length)
			throw new Exception("DiscreteDistribution.mult(), mismatch dimensions");

		Matrix newY = Matrix.product(m, Matrix.arrayToColumn(d.valuesY));

		return new DiscreteDistribution(d.valuesX, Matrix.columnToArray(newY), d.integrationMethod);
	}

	/**
	 * Setters
	 */

	/**
	 * setValuesY()
	 */
	public void setValuesY(Parameters p, Context c, State s, Function f) throws Exception {
		for (int i = 0; i < this.length; i++)
			this.valuesY[i] = f.execute(p, c, s, new OneVariable(this.valuesX[i]));
		integrate();
	}

	/**
	 * setValuesY()
	 */
	public void setValuesY(double[] valuesY, Parameters p, Context c, State s, Function f) throws Exception {
		if (valuesY.length != this.getLength())
			throw new Exception("DiscreteDistribution.setValuesY(), wrong length " + valuesY.length
					+ " for valuesY, expected " + this.length);

		for (int i = 0; i < this.length; i++)
			if (f.expectedVariables().equals(ZeroVariable.class))
				this.valuesY[i] = f.execute(p, c, s, new ZeroVariable());
			else if (f.expectedVariables().equals(OneVariable.class))
				this.valuesY[i] = f.execute(p, c, s, new OneVariable(valuesY[i]));
			else if (f.expectedVariables().equals(TwoVariables.class))
				this.valuesY[i] = f.execute(p, c, s, new TwoVariables(valuesX[i], valuesY[i]));
			else
				throw new Exception(
						"DiscreteDistribution.setValuesY(), we expect a function depending on zero, one or two parameters, got: "
								+ f);

		integrate();
	}

	/**
	 * Getters
	 */

	/**
	 * getFormated()
	 */
	public String getFormated(String linePrefix, String separator) {

		String prefix = linePrefix + separator;
		if (linePrefix.length() == 0)
			prefix = "";

		StringBuffer sb = new StringBuffer();

		for (int i = 0; i < length; ++i) {
			sb.append(prefix + valuesX[i] + separator + valuesY[i]);
			if (i < length - 1)
				sb.append("\n");
		}
		return sb.toString();
	}

	/**
	 * getHeader()
	 */
	public String getHeader(String linePrefix, String xLabel, String yLabel, String separator) {
		return linePrefix + separator + xLabel + separator + yLabel;
	}

	/**
	 * getIntegral()
	 */
	public double getIntegral() {
		return integral;
	}

	/**
	 * getIntegrationMethod()
	 */
	public String getIntegrationMethod() {
		return integrationMethod;
	}

	/**
	 * toString()
	 */
	@Override
	public String toString() {
		String s = "DiscreteDistribution, \n";
		for (int i = 0; i < length; i++)
			s += "x: " + valuesX[i] + ", y: " + valuesY[i] + "\n";
		return s;
	}
}
