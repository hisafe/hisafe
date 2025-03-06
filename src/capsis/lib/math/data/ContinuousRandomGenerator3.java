package capsis.lib.math.data;

import java.io.Serializable;
import java.util.Random;

import org.apache.commons.math3.distribution.RealDistribution;
import org.apache.commons.math3.random.AbstractRandomGenerator;

/**
 * A version of ContiuousGenerator based on org.apache.commons.math3 See the
 * original ContinuousRandomGenerator.
 * 
 * @author F. de Coligny - November 2021
 */
public class ContinuousRandomGenerator3 extends AbstractRandomGenerator implements Serializable {

	// Compatible with org.apache.commons.math3.distribution.RealDistribution
	
	private RealDistribution distribution;
	private Random randomSource;

	/**
	 * Constructor 1.
	 * 
	 * @param distribution : the underlying distribution.
	 * @param random : the underlying random generator.
	 */
	public ContinuousRandomGenerator3(RealDistribution distribution, Random random) {
		super();
		this.distribution = distribution;
		this.randomSource = random;

	}

	/**
	 * Constructor 2.
	 * 
	 * @param distribution : the underlying distribution.
	 */
	public ContinuousRandomGenerator3(RealDistribution distribution) {
		super();
		this.distribution = distribution;
		this.randomSource = new Random();

	}

	/**
	 * Constructor 3, with randomSeed.
	 */
	public ContinuousRandomGenerator3(RealDistribution distribution, int randomSeed) {
		this(distribution);
		this.setSeed (randomSeed);

	}

	/**
	 * @return a double taken from the distribution
	 */
	public double nextDouble() {

		try {
			return distribution.inverseCumulativeProbability(randomSource.nextDouble());
		} catch (Exception e) {
			String text = "Error in ContinuousRandomGenerator3.nextDouble (), with distribution: "
					+ distribution.toString();
			throw new RuntimeException(text, e);
		}
	}

	/**
	 * @param n : number of values to generate
	 * @return a array of n double
	 */
	public double[] getValues(int n) {

		double[] values = new double[n];

		for (int i = 0; i < n; i++) {
			double v = this.nextDouble();
			values[i] = v;
		}
		return values;
	}

	/**
	 * Added this method to comply with the AbstractRandomGenerator superclass in
	 * apache commons math 3, for use with StableRandomGenerator / skewness.
	 * fc+ed-23.3.2015
	 */
	@Override
	public void setSeed(long seed) {
		clear();
		this.randomSource = new Random(seed);

	}

}