/**
 * 
 */
package capsis.lib.math.data;

import java.io.Serializable;
import java.util.Random;

import org.apache.commons.math.MathException;
import org.apache.commons.math.distribution.IntegerDistribution;
import org.apache.commons.math.distribution.PoissonDistributionImpl;

/**
 * Random generator build on an IntegerDistribution (see
 * org.apache.commons.maths.distribution) example : x = new RandomGenerator(new
 * BinomialDistributionImpl(...)); x.nextInt()
 * 
 * @author S. Dufour Kowalski - 2009-2010
 */
public class IntegerRandomGenerator implements Serializable {

	private IntegerDistribution distribution;
	final Random randomSource;

	/**
	 * Constructor 1
	 */
	public IntegerRandomGenerator(IntegerDistribution distribution) {
		super();
		this.distribution = distribution;
		this.randomSource = new Random();
	}

	/**
	 * Constructor 2, with a random seed.
	 */
	public IntegerRandomGenerator(IntegerDistribution distribution,
			long randomSeed) {
		super();
		this.distribution = distribution;
		this.randomSource = new Random(randomSeed);
	}

	// fc-17.11.2021 Unused
//	/**
//	 * Constructor 3, with a random seed, without distribution. The distribution
//	 * must be set before use.
//	 */
//	public IntegerRandomGenerator(long randomSeed) {
//		super();
//		this.distribution = null;
//		this.randomSource = new Random(randomSeed);
//	}

	// fc-17.11.2021 Unused
//	/**
//	 * Change the distribution (see constructor 3).
//	 */
//	public void setDistribution(IntegerDistribution distribution) {
//		this.distribution = distribution;
//	}

	/**
	 * @return an int taken from the distribution
	 */
	public int nextInt() {

		try {
			return distribution.inverseCumulativeProbability(randomSource
					.nextDouble());
			
		} catch (MathException e) {
			throw new RuntimeException(e);
		}
	}

	public static void main(String a[]) throws Exception {

//		IntegerRandomGenerator integerRandom = new IntegerRandomGenerator(1234);

		PoissonDistributionImpl distribution = new PoissonDistributionImpl(0.5);
//		integerRandom.setDistribution(distribution);
		int R = distribution.sample();
		System.out.println("IntegerRandomGenerator, R: " + R);

		distribution = new PoissonDistributionImpl(0.4);
//		integerRandom.setDistribution(distribution);
		R = distribution.sample();
		System.out.println("IntegerRandomGenerator, R: " + R);

		distribution = new PoissonDistributionImpl(0.2);
//		integerRandom.setDistribution(distribution);
		R = distribution.sample();
		System.out.println("IntegerRandomGenerator, R: " + R);

		distribution = new PoissonDistributionImpl(0.1);
//		integerRandom.setDistribution(distribution);
		R = distribution.sample();
		System.out.println("IntegerRandomGenerator, R: " + R);

		distribution = new PoissonDistributionImpl(0.05);
//		integerRandom.setDistribution(distribution);
		R = distribution.sample();
		System.out.println("IntegerRandomGenerator, R: " + R);


	}

	/**
	 * @param n
	 *            : number of values to generate
	 * @return a array of n double
	 */
	public int[] getValues(int n) {

		int[] values = new int[n];

		for (int i = 0; i < n; i++) {
			int v = this.nextInt();
			values[i] = v;
		}
		return values;
	}
}