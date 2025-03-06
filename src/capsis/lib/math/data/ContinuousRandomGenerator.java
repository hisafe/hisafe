/**
 * 
 */
package capsis.lib.math.data;

import java.io.Serializable;
import java.util.Random;

import org.apache.commons.math.distribution.ContinuousDistribution;
import org.apache.commons.math3.random.AbstractRandomGenerator;

/**
 * Random generator build on an AbstractContinuousDistribution (see
 * org.apache.commons.maths.distribution) example : x = new RandomGenerator(new
 * ChiSquaredDistributionImpl(...)); x.nextDouble()
 * 
 * @author S. Dufour Kowalski - 2009-2010
 */
public class ContinuousRandomGenerator extends AbstractRandomGenerator implements Serializable {

	// fc-17.11.2021 Adapted, still based on
	// org.apache.commons.math.distribution.ContinuousDistribution

	private ContinuousDistribution distribution;
	private Random randomSource;

	/**
	 * Constructor.
	 * 
	 * @param distribution : the underlying distribution.
	 */
	public ContinuousRandomGenerator(ContinuousDistribution distribution) {
		super();
		this.distribution = distribution;
		this.randomSource = new Random();

	}

	/**
	 * Constructor 2, with randomSeed.
	 */
	public ContinuousRandomGenerator(ContinuousDistribution distribution, int randomSeed) {
		this(distribution);
		this.setSeed(randomSeed);

	}

	/**
	 * @return a double taken from the distribution
	 */
	public double nextDouble() {

		try {
			return distribution.inverseCumulativeProbability(randomSource.nextDouble());
		} catch (Exception e) {
			String text = "Error in ContinuousRandomGenerator.nextDouble (), with distribution: "
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