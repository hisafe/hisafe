package capsis.lib.cstability.test;

import capsis.lib.cstability.distribution.DiscretePositiveDistribution;
import capsis.lib.cstability.util.Interval;

/**
 * Test of cstability library.
 *
 * to launch tests: java -cp ./class capsis.lib.cstability.test.CstabilityTest
 *
 * @author J. Sainte-Marie, F. de Coligny - February 2021
 */

public class CstabilityTest {
	
	private static final boolean SUCCESS = true;

	public static void main (String[] args) throws Exception {
		new CstabilityTest();
	}

	/**
	 * Constructor
	 */
	public CstabilityTest () throws Exception {
		testUtil();
		testState();
	}
	
	/**
	 * testUtil()
	 */
	private void testUtil () throws Exception {
		String packageName = "util";
		String className = "";
		String methodName = "";
		String testDescription = "";

		/*
		 * Interval
		 */
		className = "Interval";
		
		methodName = "constructor";
		testDescription = "exception expected";
		double min = 0;
		double max = -1;
		try {
			Interval interval = new Interval(min, max);
			print(packageName, className, methodName, !SUCCESS, testDescription);
		} catch (Exception e) {
			print(packageName, className, methodName, SUCCESS, testDescription);
		}

		methodName = "length";
		testDescription = "1 expected";
		Interval interval = new Interval(0d, 1d);
		if (interval.length() == 1d) {
			print(packageName, className, methodName, SUCCESS, testDescription);
		} else {
			print(packageName, className, methodName, !SUCCESS, testDescription);
		}
			
		/*
		 * DiscretePositiveDistribution
		 */
		className = "DiscretePositiveDistribution";
		
		methodName = "constructor";
		testDescription = "";
		String integrationMethod = DiscretePositiveDistribution.INTEGRATION_TRAPEZE;
		//
		//
		//		
	}

	private void testState() throws Exception {
		String packageName = "state";
	}

	/**
	 * Methods
	 */

	private void print(String packageName, String className, String methodName, Boolean success, String message) {
		if (success) {
			System.out.println(packageName + "." + className + "." + methodName + ", SUCCESS: " + message);
		} else {
			System.out.println(packageName + "." + className + "." + methodName + ", FAILURE: " + message);
		}
	}

	
}
