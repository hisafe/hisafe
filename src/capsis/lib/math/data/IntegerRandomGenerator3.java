package capsis.lib.math.data;

import java.io.Serializable;
import java.util.Random;

import org.apache.commons.math.distribution.PoissonDistributionImpl;
import org.apache.commons.math3.distribution.IntegerDistribution;
import org.apache.commons.math3.exception.OutOfRangeException;

/**
 * A version of IntegerRandomGenerator based on org.apache.commons.math3 See the
 * original IntegerRandomGenerator.
 * 
 * @author F. de Coligny - November 2021
 */
public class IntegerRandomGenerator3 implements Serializable {

	// Compatible with org.apache.commons.math3.distribution.IntegerDistribution

	private IntegerDistribution distribution;
	final Random randomSource;

	/**
	 * Constructor
	 */
	public IntegerRandomGenerator3(IntegerDistribution distribution, Random random) {
		super();
		this.distribution = distribution;
		this.randomSource = random;
	}

	/**
	 * Constructor 1
	 */
	public IntegerRandomGenerator3(IntegerDistribution distribution) {
		super();
		this.distribution = distribution;
		this.randomSource = new Random();
	}

	/**
	 * Constructor 2, with a random seed.
	 */
	public IntegerRandomGenerator3(IntegerDistribution distribution, long randomSeed) {
		super();
		this.distribution = distribution;
		this.randomSource = new Random(randomSeed);
	}

	/**
	 * @return an int taken from the distribution
	 */
	public int nextInt() {

		try {
			return distribution.inverseCumulativeProbability(randomSource.nextDouble());

		} catch (OutOfRangeException e) {
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
	 * @param n : number of values to generate
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