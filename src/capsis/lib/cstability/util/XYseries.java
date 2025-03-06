package capsis.lib.cstability.util;

import java.io.Serializable;

/**
 * An XYseries.
 * 
 * @author J. Sainte-Marie, F. de Coligny - February 2021
 */
@SuppressWarnings("serial")
public class XYseries implements Serializable, Cloneable {

	protected double[] valuesX;
	protected double[] valuesY;
	protected int length;

	/**
	 * Constructor
	 */
	public XYseries(double[] valuesX, double[] valuesY) throws Exception {

		if (valuesX.length != valuesY.length)
			throw new Exception("DiscreteDistribution.constructor, Dimension mismatch: X length, " + valuesX.length
					+ " != Y length: " + valuesY.length);

		this.length = valuesX.length;

		if (this.length < 2)
			throw new Exception("DiscreteDistribution.constructor, Dimension is inferior to 2");

		for (int i = 0; i < length - 1; ++i) {
			if (valuesX[i] >= valuesX[i + 1])
				throw new Exception("DiscreteDistribution.constructor, Wrong valuesX sort");
		}

		this.valuesX = valuesX;
		this.valuesY = valuesY;
	}

	/**
	 * clone()
	 */
	@Override
	public XYseries clone() throws CloneNotSupportedException {
		XYseries s = (XYseries) super.clone();
		s.valuesX = valuesX.clone();
		s.valuesY = valuesY.clone();
		return s;
	}

	/**
	 * getLength()
	 */
	public int getLength() {
		return length;
	}

	/**
	 * getValuesX()
	 */
	public double[] getValuesX() {
		return valuesX;
	}

	/**
	 * getValuesY()
	 */
	public double[] getValuesY() {
		return valuesY;
	}

	/**
	 * toString()
	 */
	@Override
	public String toString() {
		String s = "XYseries, \n";
		for (int i = 0; i < length; i++)
			s += "x: " + valuesX[i] + ", y: " + valuesY[i] + "\n";
		return s;
	}
}
