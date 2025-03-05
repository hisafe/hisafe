package capsis.extension.datarenderer.drgraph;

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.jfree.data.category.CategoryDataset;
import org.jfree.data.category.DefaultCategoryDataset;

import capsis.extension.dataextractor.Categories;
import capsis.extension.dataextractor.format.DFColoredCurves;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.extension.dataextractor.format.DFListOfCategories;
import jeeb.lib.util.AdditionMap;

/**
 * A data converter for the Bar Graph renderer.
 * 
 * @author F. de Coligny - October 2015
 */
public class CategoryConverter {

	private String title;
	private String xAxisName;
	private String yAxisName;
	private DefaultCategoryDataset categoryDataset;
	private List<Color> seriesColors;

	// fc-5.10.2021 Better management when appending (1) to the series names
	private Set<String> uniqueSeriesNames;

	/**
	 * Constructor
	 */
	public CategoryConverter() {
	}

	/**
	 * Converts a list of DFCurves into a single category graph
	 */
	public static CategoryConverter convertDFCurves(List<DFCurves> dataList) throws Exception {
		CategoryConverter c = new CategoryConverter();

		c.categoryDataset = new DefaultCategoryDataset();
		c.seriesColors = new ArrayList<Color>();

		// Extractors have all of the same class: consider the first
		DFCurves representative = (DFCurves) dataList.iterator().next();

		c.title = representative.getName();
		c.xAxisName = representative.getAxesNames().get(0);
		c.yAxisName = representative.getAxesNames().get(1);

		c.uniqueSeriesNames = new HashSet<String> ();

		// fc-24-11.2015 adding twice a dataExtractor on same steps results
		// in twice the same name, e.g. mod.65a -> change to mod.65a(2)...
		// fc-5.10.2021 Replaced by uniqueSeriesNames
//		Map<String, Integer> uniqueNameMap = new HashMap<>();

		// fc-25.1.2019
		CategoryConverterHelper helper = new CategoryConverterHelper();

		// For each DFCurves (ex for data extractor)
		for (DFCurves ex : dataList) {

			List<List<? extends Number>> curves = ex.getCurves();
			int nbCurves = curves.size() - 1;

			List<String> names = GraphConverter.searchDFCurvesNames(ex, nbCurves);

			// labels
			List<List<String>> labels = ex.getLabels();
			List<String> xLabels = null;
			if (labels != null)
				xLabels = labels.get(0);

			Color color = ex.getColor();
			Vector<Color> colors = null;
			if (ex instanceof DFColoredCurves) {
				colors = new Vector<Color>(((DFColoredCurves) ex).getColors());
			}

			// fc-24.11.2015 ensuring exCaption are unique
			String exCaption = ex.getCaption();

			// fc-5.10.2021 Replaced by uniqueSeriesNames
//			Integer number = uniqueNameMap.get(exCaption);
//			if (number == null) {
//				uniqueNameMap.put(exCaption, 1); // first time
//			} else {
//				int newNumber = number + 1;
//				uniqueNameMap.put(exCaption, newNumber);
//				exCaption += "(" + newNumber + ")";
//			}

			// System.out.println("CategoryConverter.convertDFCurves ()...");
			
			// fc-5.10.2021 Replaced by uniqueSeriesNames
//			Map<String, String> uniqueLabels = new HashMap<>();

			// Unused
//			AdditionMap exCaptionMap = new AdditionMap();

			// For each curves
			for (int j = 0; j < nbCurves; j++) {
				int k = j + 1;

				List<? extends Number> curve = curves.get(k);
				int n = curve.size();

				// System.out.println("CategoryConverter curve #"+k+" size:
				// "+n);

				// fc-5.10.2021 Replaced by uniqueSeriesNames
//				uniqueLabels.clear();

				// fc-25.1.2019 fill holes by adding zeroes (bug by bc, bars for
				// small x values located at the end of the graph instead of the
				// start). Set: no duplicates
				// Note: row key is seriesName, columnKey is labelName
				// fc-5.10.2021 Replaced by uniqueSeriesNames
//				Set<String> seriesNames = new HashSet<>();
//				Set<String> labelNames = new HashSet<>();

				// fc-12.2.2021 in case names was empty, seriesNames were equal and a bug
				// occurred at plot rendering time, autoRange was fooled and some data were not
				// visible (by J. Labonne)
//				String kLabel = " (" + k + ")"; // fc-12.2.2021 kLabel never empty
				
				String name = j < names.size() ? names.get(j) : ""; // see below unicity...
//				String name = j < names.size() ? names.get(j) : kLabel;

				// fc+bc-9.1.2019 REMOVED, was messing with several
				// extractors on same step with different groups, seriesName
				// was the same, bars were missing
				// fc-5.3.2018 shorter series names if too long
				// String seriesName = AmapTools.cutIfTooLong(exCaption,
				// 20) + " " + name;
				String seriesName = exCaption + " " + name;
				
//				seriesNames.add(seriesName);
				
				// fc-5.10.2021 Check series names unicity
				// Check candidateSeriesName vs the series names already used 
				int u = 1;
				String candidateName = seriesName;
				while (c.uniqueSeriesNames.contains (candidateName)) {
					candidateName = seriesName + "["+u+"]";
					u++;
				}
				seriesName = candidateName; // Now unique
				c.uniqueSeriesNames.add(seriesName);

				
				
				
				// For each value
				for (int i = 0; i < n; i++) {

					Number y = curve.get(i);

//					// fc-12.2.2021 in case names was empty, seriesNames were equal and a bug
//					// occurred at plot rendering time, autoRange was fooled and some data were not
//					// visible (by J. Labonne)
////					String kLabel = " (" + k + ")"; // fc-12.2.2021 kLabel never empty
//					
//					String name = j < names.size() ? names.get(j) : ""; // see below unicity...
////					String name = j < names.size() ? names.get(j) : kLabel;
//
//					// fc+bc-9.1.2019 REMOVED, was messing with several
//					// extractors on same step with different groups, seriesName
//					// was the same, bars were missing
//					// fc-5.3.2018 shorter series names if too long
//					// String seriesName = AmapTools.cutIfTooLong(exCaption,
//					// 20) + " " + name;
//					String seriesName = exCaption + " " + name;
//					
////					seriesNames.add(seriesName);
//					
//					// fc-5.10.2021 Check series names unicity
//					// Check candidateSeriesName vs the series names already used 
//					int u = 1;
//					String candidateName = seriesName;
//					while (c.uniqueSeriesNames.contains (candidateName)) {
//						candidateName = seriesName + "["+u+"]";
//						u++;
//					}
//					seriesName = candidateName; // Now unique
//					c.uniqueSeriesNames.add(seriesName);

					// e.g. pla.45a Ddom

					// fc-7.1.2016 fixed a bug in pp3's DEBiomassDistrib (C.
					// Meredieu, T. Labbé)
					String label = (labels != null && i < xLabels.size()) ? xLabels.get(i) : "" + curves.get(0).get(i);

					// fc-21.9.2016
					// in case the series comes from a scenario 2012, 2017,
					// *2022, 2022
					// curves.get(0).get(i) may return 2012, 2017, 2022, 2022
					// we need unique labels, turn them into
					// 2012, 2017, 2022, 2022' (the * is lost at this time)
					
//					label = getUniqueLabel(uniqueLabels, label);
					
//					labelNames.add(label);

					// System.out.println("CategoryConverter: adding value y:
					// "+y+" seriesName: "+seriesName+" label: "+label);

					// DefaultCategoryDataset.addValue (number, rowKey,
					// columnKey)
					helper.addValue(y, seriesName, label);
					// c.categoryDataset.addValue(y, new
					// ComparableKey(seriesName), new ComparableKey(label));

				}

				if (colors == null)
					c.seriesColors.add(color); // single color for all curves
				else
					c.seriesColors.add(colors.get(j)); // DFColoredCurves
			}

		}

		// fc-25.1.2019 fills missing labels holes and takes care of feeding
		// ascending order on label range first value
		helper.feedCategoryDataset(c.categoryDataset);

		return c;
	}

	// fc-5.10.2021 Replaced by uniqueSeriesNames
//	static private String getUniqueLabel(Map<String, String> uniqueLabels, String candidateLabel) {
//
//		// get the memo label which was returned last time for this
//		// candidateLabel
//		String memoLabel = uniqueLabels.get(candidateLabel);
//
//		if (memoLabel == null) {
//			// candidateLabel not found in map: first time it is proposed
//			// memo it for next time in case the same label is proposed
//			// and return it: it is unique so far
//			uniqueLabels.put(candidateLabel, candidateLabel);
//
//			// System.out.println("CategoryConverter getUniqueLabel() proposed:
//			// "+candidateLabel+" returned: "+ candidateLabel);
//			return candidateLabel;
//
//		} else {
//			// getUniqueLabel was found in the map: it was already proposed
//			// choose new label by appending "'" to the memo label
//			// memo the new label and return it
//			String newLabel = memoLabel + "\'"; // 'prime'
//			uniqueLabels.put(candidateLabel, newLabel);
//
//			// System.out.println("CategoryConverter getUniqueLabel() proposed:
//			// "+candidateLabel+" returned: "+ newLabel);
//			return newLabel;
//		}
//
//	}

	public String toString() {
		String newLine = "\n";
		String tab = "\t";
		StringBuffer b = new StringBuffer("CategoryConverter");

		b.append(newLine);

		b.append(newLine + "   DefaultCategoryDataset:t");

		int nRow = categoryDataset.getRowCount();
		int nCol = categoryDataset.getColumnCount();

		for (int i = 0; i < nRow; i++) {
			b.append(newLine + "      ");
			for (int j = 0; j < nCol; j++) {
				b.append(categoryDataset.getValue(i, j) + tab);
			}
		}

		b.append(newLine);

		b.append(newLine + "   RowKeys: ");
		for (int i = 0; i < nRow; i++) {
			b.append(categoryDataset.getRowKey(i) + tab);
		}

		b.append(newLine + "   RowIndexs: ");
		for (int i = 0; i < nRow; i++) {
			b.append(categoryDataset.getRowIndex(categoryDataset.getRowKey(i)) + tab);
		}

		b.append(newLine);

		b.append(newLine + "   ColumnKeys: ");
		for (int j = 0; j < nCol; j++) {
			b.append(categoryDataset.getColumnKey(j) + tab);
		}

		b.append(newLine + "   ColumnIndexs: ");
		for (int j = 0; j < nCol; j++) {
			b.append(categoryDataset.getColumnIndex(categoryDataset.getColumnKey(j)) + tab);
		}

		return b.toString();
	}

	/**
	 * Converts a list of DFListOCategories into a single category graph
	 */
	public static CategoryConverter convertDFListOfCategories(List<DFListOfCategories> dataList) throws Exception {
		CategoryConverter c = new CategoryConverter();

		c.categoryDataset = new DefaultCategoryDataset();
		c.seriesColors = new ArrayList<Color>();

		// Extractors have all of the same class: consider the first
		DFListOfCategories representative = (DFListOfCategories) dataList.iterator().next();

		c.title = representative.getName();
		c.xAxisName = representative.getAxesNames().get(0);
		c.yAxisName = representative.getAxesNames().get(1);

		// fc-3.5.2016 adding twice a dataExtractor on same steps results
		// in twice the same name, e.g. mod.65a -> change to mod.65a(2)...
		Map<String, Integer> uniqueNameMap = new HashMap<>();

		// For each DFListOfXYSeries
		for (DFListOfCategories catList : dataList) {

			int nbHisto = catList.getListOfCategories().size();

			// // labels
			// List<List<String>> labels = xys.getLabels();
			// List<String> xLabels = null;
			// if (labels != null)
			// xLabels = labels.get(0);

			// fc-3.5.2016 ensuring catCaption are unique
			String catCaption = catList.getCaption();

			Integer number = uniqueNameMap.get(catCaption);
			if (number == null) {
				uniqueNameMap.put(catCaption, 1); // first time
			} else {
				int newNumber = number + 1;
				uniqueNameMap.put(catCaption, newNumber);
				catCaption += "(" + newNumber + ")";
			}

			// For each histo
			int j = 0;
			for (Categories cat : catList.getListOfCategories()) {
				int k = j + 1;

				for (Categories.Entry e : cat.getEntries()) {

					String label = e.label;
					double value = e.value;

					// String kLabel = "";
					// if (xy.getName () != null && xy.getName ().length () > 0)
					// kLabel = "" + k; // only if several
					// String name = j < names.size() ? names.get(j) : kLabel;
					// String seriesName = xys.getCaption() + " " + name; //
					// e.g.
					// // pla.45a
					// // Ddom

					// fc+bc-9.1.2019 REMOVED, was messing with several
					// extractors on same step with different groups, seriesName
					// was the same, bars were missing
					// fc-5.3.2018 shorter series names if too long
					// String seriesName = AmapTools.cutIfTooLong(catCaption,
					// 20) + " " + cat.getName();
					String seriesName = catCaption + " " + cat.getName();

					c.categoryDataset.addValue(value, seriesName, label);

				}
				c.seriesColors.add(cat.getColor());

				j++;

			}

		}

		return c;

	}

	public String getTitle() {
		return title;
	}

	public String getXAxisName() {
		return xAxisName;
	}

	public String getYAxisName() {
		return yAxisName;
	}

	public CategoryDataset getCategoryDataset() {
		return categoryDataset;
	}

	public List<Color> getSeriesColors() {
		return seriesColors;
	}

}
