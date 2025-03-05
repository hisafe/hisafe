package capsis.extension.datarenderer.drgraph;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jfree.data.category.DefaultCategoryDataset;

import jeeb.lib.util.ListMap;
import jeeb.lib.util.Log;

/**
 * A tool to help CategoryConverter manage correctly the labels data series
 * coming from several DFCurves with different column numbers.
 * 
 * @author F. de Coligny - January 2019
 */
public class CategoryConverterHelper {

	// fc-15.4.2019 we need to keep insertion order
	private List<String> seriesNames;

	// Key is seriesName (row), value is a set of colNames (columns)
	private ListMap<String, String> keyMap;

	// Key is seriesName+"_"+colName, value is a number
	private Map<String, Number> valueMap;

	private Set<String> allColNames;

	private boolean checked;

	/**
	 * Constructor
	 */
	public CategoryConverterHelper() {
		seriesNames = new ArrayList<>();
		keyMap = new ListMap<>();

		valueMap = new HashMap<>();

		allColNames = new HashSet<>();

	}

	public void addValue(Number value, String seriesName, String colName) {

		// fc-15.4.2019
		if (!seriesNames.contains(seriesName))
			seriesNames.add(seriesName);

		keyMap.addObject(seriesName, colName);
		valueMap.put(key(seriesName, colName), value);
		allColNames.add(colName);
		checked = false; // check is needed
	}

	private void fillHoles() {
		if (checked)
			return;

		// Fill holes by adding zeroes for missing colNames
		for (String seriesName : seriesNames) { // fc-15.4.2019
//		for (String seriesName : keyMap.getKeys()) {

			for (String colName : allColNames) {
				Number n = valueMap.get(key(seriesName, colName));

				if (n == null)
					addValue(0, seriesName, colName);

			}
		}

		checked = true;
	}

	/**
	 * Feed the given categoryDataset with all the values for all the series and
	 * labels, take care of addition order.
	 */
	public void feedCategoryDataset(DefaultCategoryDataset categoryDataset) {

		// Ensure we have values in all series for the union of all labels
		fillHoles();

		// fc-15.4.2019 respect addValues () calls order
		// There was a bug in Samsara2, before and after intervention bars were inverted

		// fc-1.10.2020
		// Another bug was found for EsPaCe in CategoryConverterHelper.RangeComparator,
		// fixed

		for (String seriesName : seriesNames) { // fc-15.4.2019
//		for (String seriesName : keyMap.getKeys()) {

			// Get the labels for the series
			List<String> colNames = new ArrayList<>(keyMap.getObjects(seriesName));

			// Ensure we iterate on labels with same order for all series:
			// ascending order on labels range first value

			// fc+bc-18.1.2023
			// Try to detect if the columns are ranges and must be sorted
			if (colNames != null && !colNames.isEmpty()) {
				String colName1 = colNames.get(0);
				
				try {
					double firstValue = RangeComparator.getFirstValue(colName1);
					if (firstValue == colName1.hashCode())
						throw new Exception (); // no sorting required
					
					Collections.sort(colNames, new RangeComparator());
					
				} catch (Exception e) {
					// do not sort
				}
			}

			// Feed the categoryDataset for this series
			for (String colName : colNames) {

				Number value = valueMap.get(key(seriesName, colName));
				categoryDataset.addValue(value, seriesName, colName);
			}
		}

		// fc-25.1.2019
		// Tracing the categoryDataset contents for debugging ----------------
//		Log.println("CategoryConverterHelper", "");
//		Log.println("CategoryConverterHelper", "Tracing...");
//
//		NumberFormat nf = DefaultNumberFormat.getInstance(2);
//
//		for (int row = 0; row < categoryDataset.getRowCount(); row++) {
//
//			String rowName = (String) categoryDataset.getRowKey(row);
//			StringBuffer b = new StringBuffer("" + rowName);
//
//			for (int col = 0; col < categoryDataset.getColumnCount(); col++) {
//
//				String colName = (String) categoryDataset.getColumnKey(col);
//				Number value = categoryDataset.getValue(row, col);
//
//				String v = value == null ? "" + value : nf.format(value);
//
//				b.append(" ");
//				b.append(colName);
//				b.append(": ");
//				b.append(v);
//
//			}
//
//			Log.println("CategoryConverterHelper", "" + b);
//
//		}

	}

	private String key(String seriesName, String colName) {
		return "" + seriesName + "_" + colName;
	}

	/**
	 * A comparator to compare labels with this form: value1-value2
	 */
	private static class RangeComparator implements Comparator {

		@Override
		public int compare(Object o1, Object o2) {
			String s1 = (String) o1;
			String s2 = (String) o2;

			try {
				double res = getFirstValue(s1) - getFirstValue(s2);
				if (res > 0)
					return 1;
				else if (res < 0)
					return -1;
				else
					return 0;

			} catch (Exception e) {
				Log.println(Log.WARNING, "CategoryConverterHelper.RangeComparator.compare ()",
						"Exception while comparing: " + s1 + " and " + s2 + ", used poorer String.compareTo ()", e);
				// If trouble, default String (poorer) comparison
				return s1.compareTo(s2);

			}
		}

		/**
		 * Labels are supposed to have this form: value1-value2 (i.e. a range of
		 * values). Extract and return value1 for comparison between several instances
		 * of labels
		 */
		static public double getFirstValue(String v) throws Exception {
			v = v.replace(',', '.');

			// fc-1.10.2020 There was an error on [5,10[
			v = v.replace('[', ' '); // ' 5-10['
			v = v.replace(']', ' '); // ' 5-10 '
			v = v.trim(); // '5-10'

			if (v.contains("-")) {
				String u = v.substring(0, v.indexOf("-"));
				double d = Double.parseDouble(u);

				return d;
			}

			try {
				// Try a single number
				double d = Double.parseDouble(v);
				return d;

			} catch (Exception e) {
				// fc-14.9.2021 In case the label is different than expected, return its
				// hashCode (it is a number, can be used in sorts, better than nothing).
				return v.hashCode();
//				throw new Exception("CategoryConverterHelper getFirstValue(" + v + "): Exception: " + e);
			}
		}

	}

}
