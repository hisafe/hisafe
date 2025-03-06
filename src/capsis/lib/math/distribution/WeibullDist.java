package capsis.lib.math.distribution;

/**
 * Weibull distribution
 * @author Dany Camirand
 */
public class WeibullDist  {

	public static double relativeCumulativeProbability (double x, double p1, double p2, double p3) {
		return  p1 + ((1 - p1) * (1 - Math.exp(- Math.pow((x/p2), p3))));
	}
}
