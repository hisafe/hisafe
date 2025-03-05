package capsis.extension.datarenderer.drgraph;

import java.awt.Color;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import org.jfree.data.xy.XYDataItem;
import org.jfree.data.xy.XYSeriesCollection;

import capsis.extension.dataextractor.format.DFColoredCurves;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.extension.dataextractor.format.DFListOfXYSeries;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.DefaultNumberFormat;
import jeeb.lib.util.Log;
import jeeb.lib.util.Vertex2d;
import jeeb.lib.util.XYSeries;

/**
 * A data converter for the Graph renderer.
 * 
 * @author F. de Coligny - October 2015
 */
public class GraphConverter {

	private String title;
	private String xAxisName;
	private String yAxisName;
	private XYSeriesCollection xySeriesCollection;
	private List<Color> seriesColors;
	// Index of the first series of each source
	private List<Integer> sourceHeads;

	// fc-5.10.2021 Better management when appending (1) to the series names
	private Set<String> uniqueSeriesNames;

	/**
	 * Constructor
	 */
	public GraphConverter() {
	}

	/**
	 * Optional process, to be called after the GraphConverter has been updated, in
	 * some cases only. In some situations, several entries have the same date in
	 * the xySeries in the xySeriesCollection (e.g. if there was an intervention at
	 * date 2012 -> 2012 and *2012 have same date: 2012) and this can be a problem
	 * for certain dataRenderers, e.g. DRStackedAreaGraph. In this case, calling
	 * this method will shift a little the duplicate x value to get a better
	 * rendering, e.g. 2012 and 2012.1.
	 */
	public void shiftEntriesHavingSameX() throws Exception {

		// fc+bc-17.1.2024

		XYSeriesCollection fixed_xySeriesCollection = new XYSeriesCollection();

		for (Object o : xySeriesCollection.getSeries()) {

			org.jfree.data.xy.XYSeries series = (org.jfree.data.xy.XYSeries) o;

			// Clone the series to get a copy with same name, color...
			org.jfree.data.xy.XYSeries fixed_series = (org.jfree.data.xy.XYSeries) series.clone();

			// Clear
			fixed_series.clear();

			// Refeed
			double lastX = -Double.MAX_VALUE; // smallest possible value
			XYDataItem last_item = null;
			XYDataItem last_fixed_item = null;

			for (int i = 0; i < series.getItemCount(); i++) {
				XYDataItem item = series.getDataItem(i);

				if (item.getX().doubleValue() != lastX) { // x is different

					// Write the fixed_item of previous iteration if any
					if (last_item != null) {

						// Check if the added step is too large, if so abort
						if (last_fixed_item.getXValue() >= item.getXValue())
							return; // xySeriesCollection is unchanged

						fixed_series.add(last_fixed_item); // write the fixed
						last_item = null;
						last_fixed_item = null;
					}

					// In this entry, x is different than the previous one in the series: keep this
					// entry
					fixed_series.add((XYDataItem) item.clone());

				} else { // same x

					// Write the unchanged item of previous iteration if any
					if (last_item != null) {

						// We finally skip the last_item here, we will write only the last fixed_item if
						// several successive have the same x
//						fixed_series.add(last_item); // write the unchanged

						last_item = null;
						last_fixed_item = null;
					}

					// Fix the item by shifting a little its x value
					double fixed_x = item.getXValue() + 0.2;

					XYDataItem fixed_item = new XYDataItem(fixed_x, item.getY());

					// Store the unchanged and fixed items, we will decide at next iteration if we
					// add item or fixed_item
					// These two variables are both set or both null
					last_item = item;
					last_fixed_item = fixed_item;

					// Do not add yet
//					fixed_series.add(fixed_item); // Shift x

				}

				// Update lastX for next item evaluation
				lastX = item.getX().doubleValue();

			} // Next entry in the series

			// Add the last entry in the series
			if (last_fixed_item != null)
				fixed_series.add(last_fixed_item);

			fixed_xySeriesCollection.addSeries(fixed_series);

		} // Next series

		xySeriesCollection = fixed_xySeriesCollection;

	}

	/**
	 * Converts a list of DFCurves into a single graph
	 */
	public static GraphConverter convertDFCurves(List<DFCurves> dataList) throws Exception {
		GraphConverter c = new GraphConverter();

		c.xySeriesCollection = new XYSeriesCollection();
		c.seriesColors = new ArrayList<>();
		c.sourceHeads = new ArrayList<>();

		// Extractors have all of the same class: consider the first
		DFCurves representative = (DFCurves) dataList.iterator().next();

		c.title = representative.getName();
		c.xAxisName = representative.getAxesNames().get(0);
		c.yAxisName = representative.getAxesNames().get(1);

		c.uniqueSeriesNames = new HashSet<String>();

		// For each DFCurves
		int sourceIndex = 0;
		for (DFCurves dfc : dataList) {

			c.sourceHeads.add(sourceIndex);

			List<List<? extends Number>> curves = dfc.getCurves();
			int nbCurves = curves.size() - 1;

			List<String> names = searchDFCurvesNames(dfc, nbCurves);

			Color color = dfc.getColor();
			Vector<Color> colors = null;
			if (dfc instanceof DFColoredCurves) {
				colors = new Vector<Color>(((DFColoredCurves) dfc).getColors());
			}

			// For each curves
			for (int j = 0; j < nbCurves; j++) {
				int k = j + 1;

				// fc-6.10.2021 When only one curve, we do not want (1) at the end...
				// Reviewed again seriesNames unicity, finer management

				// fc-12.2.2021 in case names was empty, seriesNames were equal and a bug
				// occurred at plot rendering time, autoRange was fooled and some data were not
				// visible (by J. Labonne)
//				String kLabel = " (" + k + ")"; // fc-12.2.2021 kLabel never empty

				String name = j < names.size() ? names.get(j) : ""; // see below unicity...
//				String name = j < names.size() ? names.get(j) : kLabel;

				// fc-5.3.2018 shorter series names if too long
				// String seriesName = dfc.getCaption() + " " + name;
				String seriesName = AmapTools.cutIfTooLong(dfc.getCaption(), 20) + " " + name;
				// e.g. pla.45a Ddom

				// fc-5.10.2021 Check series names unicity
				// Check candidateSeriesName vs the series names already used
				int u = 1;
				String candidateName = seriesName;
				while (c.uniqueSeriesNames.contains(candidateName)) {
					candidateName = seriesName + "[" + u + "]";
					u++;
				}
				seriesName = candidateName; // Now unique
				c.uniqueSeriesNames.add(seriesName);

				// Warning: autoSort must be false to prevent changing points
				// order (!)
				boolean autoSort = false;
				boolean allowDuplicateXValues = true;
				org.jfree.data.xy.XYSeries s = new org.jfree.data.xy.XYSeries(seriesName, autoSort,
						allowDuplicateXValues);
				s.setDescription(name); // short name, e.g. Dg

				// For each value
				int n = curves.get(k).size();
				for (int i = 0; i < n; i++) {

					Number y = curves.get(k).get(i);
					Number x = curves.get(0).get(i);

					// In DFCurves, some y can be NaN -> ignore the point
					if (Double.isNaN(x.doubleValue()) || Double.isNaN(y.doubleValue()))
						continue;

					s.add(x, y);

				}

				c.xySeriesCollection.addSeries(s);

				if (colors == null)
					c.seriesColors.add(color); // single color for all curves
				else
					c.seriesColors.add(colors.get(j)); // DFColoredCurves

			}
			sourceIndex += nbCurves;
		}

		return c;
	}

	/**
	 * Search for a given DFCurves the curves names if any (e.g. Ddom, Dg...).
	 */
	static public List<String> searchDFCurvesNames(DFCurves dfc, int nbCurves) {

		List<String> names = new ArrayList<>();

		List<List<String>> labels = dfc.getLabels();
		if (labels != null && labels.size() != 0) {

			if (labels.size() > 1) {
				try {
					// There should be an yLabels list for each curve
					// With ONE label only inside (for curves annotation)
					for (int i = 1; i <= nbCurves; i++) {
						List<String> yLabels = labels.get(i);
						if (yLabels == null)
							throw new Exception(
									"Converter: DFCurves yLabels should not be null here (looking for curves names)");
						if (yLabels.size() == 0)
							throw new Exception(
									"Converter: DFCurves yLabels should not be empty here (looking for curves names)");
						if (yLabels.size() > 1)
							throw new Exception("Converter: DFCurves yLabels size should be 1 here (found "
									+ yLabels.size() + ", looking for curves names)");
						names.add(yLabels.get(0));
					}
				} catch (Exception e) {
					Log.println(Log.WARNING, "Converter.evaluateDFCurvesNames ()",
							"DFCurves inconsistency: incorrect curves names, ignored names: "
									+ dfc.getClass().getName(),
							e);
				}

			}

		}
		return names;
	}

	/**
	 * Converts a list of DFListOfXYSeries into a single graph
	 */
	public static GraphConverter convertDFListOfXYSeries(List<DFListOfXYSeries> dataList) throws Exception {
		GraphConverter c = new GraphConverter();

		c.xySeriesCollection = new XYSeriesCollection();
		c.seriesColors = new ArrayList<Color>();
		c.sourceHeads = new ArrayList<>();

		int sourceIndex = 0;
		for (DFListOfXYSeries xys : dataList) {

			c.sourceHeads.add(sourceIndex);

			if (c.title != null && !c.title.equals(xys.getName()))
				throw new Exception("Converter: can not merge several DFListOfXYSeries with different titles, found: "
						+ c.title + " and: " + xys.getName());
			c.title = xys.getName();

			List<String> l = xys.getAxesNames();
			if (l.size() < 2)
				throw new Exception("Converter: could not convert a DFListOfXYSeries without 2 axes Names: "
						+ AmapTools.toString(l));
			if (c.xAxisName != null && !c.xAxisName.equals(l.get(0)))
				throw new Exception(
						"Converter: can not merge several DFListOfXYSeries with different xAxisName, found: "
								+ c.xAxisName + " and: " + l.get(0));
			if (c.yAxisName != null && !c.yAxisName.equals(l.get(1)))
				throw new Exception(
						"Converter: can not merge several DFListOfXYSeries with different yAxisName, found: "
								+ c.yAxisName + " and: " + l.get(1));
			c.xAxisName = l.get(0);
			c.yAxisName = l.get(1);

			// For each XYSeries
			for (XYSeries xySeries : xys.getListOfXYSeries()) {

				// long name, e.g. mod.35a Dg
				// fc-5.3.2018 shorter series names if too long
//				String seriesName = xys.getCaption() + " " + xySeries.getName();
				String seriesName = AmapTools.cutIfTooLong(xys.getCaption(), 20) + " " + xySeries.getName();

				// Warning: autoSort must be false to prevent changing points
				// order (!)
				boolean autoSort = false;
				boolean allowDuplicateXValues = true;
				org.jfree.data.xy.XYSeries s = new org.jfree.data.xy.XYSeries(seriesName, autoSort,
						allowDuplicateXValues);
				s.setDescription(xySeries.getName()); // short name, e.g. Dg

				for (Vertex2d v : xySeries.getPoints()) {

					Number x = v.x;
					Number y = v.y;
					s.add(x, y);

				}
				c.xySeriesCollection.addSeries(s);
				c.seriesColors.add(xySeries.getColor());
			}
			int n = xys.getListOfXYSeries().size();
			sourceIndex += n;

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

	public XYSeriesCollection getXYSeriesCollection() {
		return xySeriesCollection;
	}

	public List<Color> getSeriesColors() {
		return seriesColors;
	}

	public int getSeriesNumber() {
		// fc-10.2.2021 Seems better
		return xySeriesCollection.getSeriesCount();
//		return getSourceHeads() == null ? 0 : getSourceHeads().size();
	}

	public List<Integer> getSourceHeads() {
		return sourceHeads;
	}

	public int getSourceNumber() {
		return getSourceHeads() == null ? 0 : getSourceHeads().size();
	}

	public String toString() {

		NumberFormat nf = DefaultNumberFormat.getInstance(2);
		String newLine = "\n";
		String tab = "\t";
		StringBuffer b = new StringBuffer("GraphConverter" + newLine);

		b.append("   title:     " + title + newLine);
		b.append("   xAxisName: " + xAxisName + newLine);
		b.append("   yAxisName: " + yAxisName + newLine);
		b.append("   #sources:  " + getSourceNumber() + newLine);
		b.append("   #series:   " + getSeriesNumber() + " (from all sources)" + newLine);

		b.append(newLine);

		b.append("   XYSeriesCollection" + newLine);

		for (int i = 0; i < xySeriesCollection.getSeriesCount(); i++) {
			org.jfree.data.xy.XYSeries series = xySeriesCollection.getSeries(i);

			b.append("   " + series.getKey() + ": "); // series name
			for (Object o : series.getItems()) {
				XYDataItem xy = (XYDataItem) o;
				b.append("(" + nf.format(xy.getXValue()) + ", " + nf.format(xy.getYValue()) + ") ");

			}
			b.append(newLine);

		}

		b.append(newLine);
		int cpt = 1;
		for (Color color : seriesColors) {
			b.append("   Color for series #" + (cpt++) + ": " + color.toString() + newLine);
		}

		// fc-11.2.2021
		cpt = 1;
		for (int sh : sourceHeads) {
			b.append("   SourceHead (index of the first series of each source) for series #" + (cpt++) + ": " + sh
					+ newLine);
		}

		return b.toString();
	}

}
