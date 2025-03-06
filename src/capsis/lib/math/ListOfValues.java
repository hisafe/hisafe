package capsis.lib.math;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.math3.stat.StatUtils;
import org.apache.commons.math3.stat.descriptive.moment.Skewness;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.DefaultNumberFormat;


/**
 * A facade for stat methods, mainly based on Apache commons maths library.
 * 
 * @author F. de Coligny, B. Courbaud - March 2021
 */
public class ListOfValues {

	public static final String SUM = "SUM";
	public static final String ARITHMETIC_MEAN = "ARITHMETIC_MEAN";
	public static final String GEOMETRIC_MEAN = "GEOMETRIC_MEAN";
	public static final String POPULATION_VARIANCE = "POPULATION_VARIANCE";
	public static final String POPULATION_STANDARD_DEVIATION = "POPULATION_STANDARD_DEVIATION";
	public static final String SAMPLE_VARIANCE = "SAMPLE_VARIANCE";
	public static final String SAMPLE_STANDARD_DEVIATION = "SAMPLE_STANDARD_DEVIATION";
	
	public static final String SKEWNESS = "SKEWNESS";
	public static final String MIN = "MIN";
	public static final String MAX = "MAX";

	public static List<String> knownStatistics;
	static {
		knownStatistics = new ArrayList<>();
		knownStatistics.add(SUM);
		knownStatistics.add(ARITHMETIC_MEAN);
		knownStatistics.add(GEOMETRIC_MEAN);
		knownStatistics.add(POPULATION_VARIANCE);
		knownStatistics.add(POPULATION_STANDARD_DEVIATION);
		knownStatistics.add(SKEWNESS);
		knownStatistics.add(MIN);
		knownStatistics.add(MAX);

	}

	private List<Double> list;
	private double[] values;

	private String storedMessage;

	/**
	 * Constructor 1, direct.
	 */
	public ListOfValues(double[] values) {
		list = null;
		this.values = values;
	}

	/**
	 * Constructor 2, incremental, see add ().
	 */
	public ListOfValues() {
		list = new ArrayList<>();
		values = null;
	}

	/**
	 * Constructor 3, with a collection of objects, see getValue(Object). In case of
	 * exception in getValue (), aborts, clear the list, store a message, will be
	 * detected at turnListIntoValues at calling time. override mode must be true
	 * and getValue(Object o) must have been overriden to provide the value when
	 * called on each object in the given collection.
	 * 
	 * <pre>
	 * // E.g. from WriterOutput
	 * boolean overrideMode = true; // fc+fw+xm-25.11.2021 added overrideMode
	 * ListOfValues lov = new ListOfValues(treesSupDbhMin, overrideMode) {
	 * 	&#64;Override
	 * 	public double getValue(Object o) {
	 * 		Samsa2Tree tree = (Samsa2Tree) o;
	 * 		return tree.getDbh();
	 * 	}
	 * };
	 * </pre>
	 */
	public ListOfValues(Collection objectsWithAValue, boolean overrideMode) throws Exception {
		this();

		if (!overrideMode)
			throw new Exception(
					"ListOfValue (objectsWithAValue, overrideMode) expects overrideMode to be true and the getValue (o) overriden to provide a value for each object sent");

		storedMessage = ""; // no problem yet...

		Object evaluatedObject = null;
		try {
			for (Object o : objectsWithAValue) {
				// getValue () may throw an exception
				evaluatedObject = o; // for the catch below
				add(getValue(o));
			}

		} catch (Exception e) {
			storedMessage = "ListOfValue, getValue () error: " + e + " for object: " + evaluatedObject;
			// See turnListIntoValues (), an error will be returned
			list.clear();
		}
	}

	/**
	 * Constructor 4
	 */
	public ListOfValues(List<Double> list) {
		// fc+fw+xm-25.11.2021 new constructor, the values are directly given in a list
		this.list = new ArrayList<Double>(list);
	}

	/**
	 * To be used with ListOfValues(List objectsWithAValue). This method is to be
	 * overriden to cast the object and return the expected value.
	 */
	public double getValue(Object o) throws Exception {
		// Used only with the constructor ListOfValues(List objectsWithAValue)
		// fc+fw+xm-25.11.2021 this getValue () method MUST be overriden if constructor3
		// is used
		throw new RuntimeException(
				"ListOfValues error, getValue (o) must be overriden to provide a value for each object sent");
//		return 0;
	}

	/**
	 * To be used with ListOfValues().
	 */
	public void add(double value) throws Exception {

		// Once values has been set, it cannot be changed
		if (values != null)
			throw new Exception("This list of values can not be changed");

		list.add(value);
	}

	/**
	 * Ensures values is available, does nothing if already called earlier.
	 */
	private void turnListIntoValues() throws Exception {

		// Already done, return
		if (values != null)
			return;

		// storedMessage below may be blank or something set at construction time
		if (list == null || list.isEmpty())
			throw new Exception("No values found" + " " + storedMessage);

		int n = list.size();
		values = new double[n];
		int k = 0;
		for (double v : list) {
			values[k++] = v;
		}

	}

	/**
	 * A way to get the result directly in a String, and to propose a value to be
	 * returned in this string in case of error, possibly convenient when writing
	 * files.
	 */
	public String getFormattedStatistics(String statistics, NumberFormat nf, String returnedValueIfError) {
		return getFormattedStatistics(statistics, 1, nf, returnedValueIfError);
	}

	public String getFormattedStatistics(String statistics, double coefficient, NumberFormat nf,
			String returnedValueIfError) {
		try {

			// Coefficient is only to pass sum per hectare
			boolean coefficientNotAcceptable = !statistics.equals(SUM) && coefficient != 1;

			if (coefficientNotAcceptable)
				throw new Exception();

			if (statistics.equals(SUM))
				return nf.format(getSum() * coefficient);
			else if (statistics.equals(ARITHMETIC_MEAN))
				return nf.format(getArithmeticMean());
			else if (statistics.equals(GEOMETRIC_MEAN))
				return nf.format(getGeometricMean());
			else if (statistics.equals(POPULATION_VARIANCE))
				return nf.format(getPopulationVariance());
			else if (statistics.equals(POPULATION_STANDARD_DEVIATION))
				return nf.format(getPopulationStandardDeviation());
			
			// fc+vf-14.4.2022
			else if (statistics.equals(SAMPLE_VARIANCE))
				return nf.format(getSampleVariance());
			else if (statistics.equals(SAMPLE_STANDARD_DEVIATION))
				return nf.format(getSampleStandardDeviation());

			else if (statistics.equals(SKEWNESS))
				return nf.format(getSkewness());
			else if (statistics.equals(MIN))
				return nf.format(getMin());
			else if (statistics.equals(MAX))
				return nf.format(getMax());
			else
				return "ListOfValues, unknown statistics: " + statistics + ", expected one of "
						+ AmapTools.toString(knownStatistics);

		} catch (Exception e) {
			return returnedValueIfError;
		}
	}

	public double getSum() throws Exception {
		turnListIntoValues();

		return StatUtils.sum(values);
	}

	public double getArithmeticMean() throws Exception {
		turnListIntoValues();

		return StatUtils.mean(values);
	}

	public double getGeometricMean() throws Exception {
		turnListIntoValues();

		return StatUtils.geometricMean(values);
	}

	public double getPopulationVariance() throws Exception {
		turnListIntoValues();

		return StatUtils.populationVariance(values);
	}

	public double getPopulationStandardDeviation() throws Exception {
		return Math.sqrt(getPopulationVariance());
	}

	// fc+vf-14.4.2022 Added sample variance
	public double getSampleVariance() throws Exception {
		turnListIntoValues();

		return StatUtils.variance(values);
	}

	// fc+vf-14.4.2022 Added sample sd
	public double getSampleStandardDeviation() throws Exception {
		return Math.sqrt(getSampleVariance());
	}

	public double getSkewness() throws Exception {
		turnListIntoValues();

		Skewness sk = new Skewness();
		int n = values.length;
		double res = sk.evaluate(values, 0, n);

		if (Double.isNaN(res))
			throw new Exception("Could not evaluate Skewness");

		return res;
	}

	// vf-10.5.2022 Added sample size
	public double getSampleSize() throws Exception {

		int n = list.size();

		return n;
	}

	public double getMin() throws Exception {
		turnListIntoValues();

		return StatUtils.min(values);
	}

	public double getMax() throws Exception {
		turnListIntoValues();

		return StatUtils.max(values);
	}

	public String toString() {
		NumberFormat nf = DefaultNumberFormat.getInstance(2);

		StringBuffer b = new StringBuffer("ListOfValues");

		try {
			turnListIntoValues();
		} catch (Exception e) {
			b.append("Exception: " + e);
			return "" + b;
		}

		for (double v : values) {
			b.append(" " + nf.format(v));

		}
		return "" + b;
	}

}
