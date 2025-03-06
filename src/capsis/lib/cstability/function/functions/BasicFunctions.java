package capsis.lib.cstability.function.functions;

import capsis.lib.cstability.util.Interval;

/**
 * Functions library to build C-STABILITY Functions.
 * 
 * @author J. Sainte-Marie, F. de Coligny - February 2021
 */
public class BasicFunctions {

	/**
	 * indicator()
	 */
	public static double indicator(Interval interval, double x) {
		return interval.contains(x) ? 1 : 0;
	}

	/**
	 * indicator()
	 */
	public static double[] indicator(Interval interval, double[] valuesX) throws Exception {
		double[] valuesY = new double[valuesX.length];
		for (int i = 0; i < valuesY.length; ++i) {
			valuesY[i] = indicator(interval, valuesX[i]);
		}
		return valuesY;
	}

	/**
	 * gaussian()
	 */
	public static double gaussian(double mean, double sd, double x) throws Exception {
		if (sd <= 0)
			throw new Exception("Functions.gaussian(), wrong sd: " + sd + ", should be positive");

		return 1d / sd / Math.sqrt(2 * Math.PI) * Math.exp(-Math.pow((x - mean) / sd, 2) / 2d);
	}

	/**
	 * gaussian()
	 */
	public static double[] gaussian(double mean, double sd, double[] valuesX) throws Exception {
		double[] valuesY = new double[valuesX.length];
		for (int i = 0; i < valuesY.length; ++i) {
			valuesY[i] = gaussian(mean, sd, valuesX[i]);
		}
		return valuesY;
	}

	/**
	 * gaussianNormal()
	 */
	public static double gaussianNormal(double x) throws Exception {
		return gaussian(0., 1., x);
	}

	/**
	 * gaussianNormal()
	 */
	public static double[] gaussianNormal(double[] valuesX) throws Exception {
		double[] valuesY = new double[valuesX.length];
		for (int i = 0; i < valuesY.length; ++i) {
			valuesY[i] = gaussianNormal(valuesX[i]);
		}
		return valuesY;
	}
}
