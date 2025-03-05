package capsis.extension.datarenderer.drcurves;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.jfree.data.Values2D;
import org.jfree.data.category.CategoryDataset;
import org.jfree.data.xy.XYDataset;

import capsis.extension.dataextractor.Categories;
import capsis.extension.dataextractor.format.DFBoxPlots;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.extension.dataextractor.format.DFListOfCategories;
import capsis.extension.dataextractor.format.DFListOfXYSeries;
import capsis.extensiontype.DataExtractor;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.DoubleKeyMap;
import jeeb.lib.util.Log;
import jeeb.lib.util.Vertex2d;
import jeeb.lib.util.XYSeries;

/**
 * A tool class to turn a list of data extractors into a single table (extracted
 * from DRTable fc-28.9.2015, adapted to DFListOfXYSeries fc-5.10.2015, adapted
 * to DFListOfCategories fc-12.10.2015, adapted to DFBoxPlots fc-7.7.2021).
 * 
 * @author F. de Coligny - September 2015
 */
public class DRTableBuilder {

	private int nLinMin = 50;
	private int nColMin = 50;

	// Outputs
	private int nLin;
	private int nCol;
	private String colNames[];
	private String mat[][];
	private List<Color> colors = new ArrayList<Color>();

	/**
	 * Constructor for a list of DataExtractors. Accepts DFCurves and
	 * DFListOfXYSeries instances. Builds the table components (column titles, main
	 * content, column colors...).
	 */
	public DRTableBuilder(List extractors) throws Exception {
		Object representative = extractors.iterator().next();

		// 0. Check data
		if (representative instanceof DFCurves) {
			initDFCurvesTable(new ArrayList<DFCurves>(extractors));

		} else if (representative instanceof DFListOfXYSeries) {
			initDFListOfXYSeriesTable(new ArrayList<DFListOfXYSeries>(extractors));

		} else if (representative instanceof DFListOfCategories) {
			initDFListOfCategoriesTable(new ArrayList<DFListOfCategories>(extractors));

		} else if (representative instanceof DFBoxPlots) { // fc-7.7.2021
			initDFDFBoxPlotsTable(new ArrayList<DFBoxPlots>(extractors));

		} else {
			throw new Exception(
					"DRTableBuilder expected DFCurves or DFListOfXYSeries instance, could not deal with this type: "
							+ representative);
		}

	}

	// fc+mj-30.10.2015
	public DRTableBuilder(CategoryDataset cd) throws Exception {

		Values2D ds = (Values2D) cd;

		nCol = ds.getColumnCount();
		nCol += 1; // col1 for labels
		nLin = ds.getRowCount();

		colNames = new String[nCol];

		for (int c = 0; c < cd.getColumnCount(); c++) {
			colNames[c + 1] = "" + cd.getColumnKey(c);
		}

		mat = new String[nLin][nCol];

		for (int i = 0; i < nLin; i++) {

			// col 0: label
			mat[i][0] = "" + cd.getRowKey(i);

			for (int j = 0; j < ds.getColumnCount(); j++) {
				mat[i][j + 1] = "" + ds.getValue(i, j);
			}
		}

	}

	// fc-2.11.2015
	public DRTableBuilder(XYDataset d) throws Exception {

		int seriesCount = d.getSeriesCount();

		nLin = 0;
		nCol = seriesCount * 2; // x and y

		// First pass, search the global size
		for (int s = 0; s < seriesCount; s++) {
			int itemCount = d.getItemCount(s);
			if (itemCount > nLin)
				nLin = itemCount;
		}

		// Size the String array
		int headerLines = 2;
		nLin += headerLines;
		colNames = new String[nCol];
		mat = new String[nLin][nCol];

		// Second pass, fill in the String array
		int c = 0;
		for (int s = 0; s < d.getSeriesCount(); s++) {

			int itemCount = d.getItemCount(s);

			colNames[c] = "" + d.getSeriesKey(s);
			colNames[c + 1] = "" + d.getSeriesKey(s);

			mat[0][c] = "X";
			mat[0][c + 1] = "Y";

			for (int i = 0; i < itemCount; i++) {
				double x = d.getXValue(s, i);
				double y = d.getYValue(s, i);

				int l = i + headerLines;

				mat[l][c] = "" + x;
				mat[l][c + 1] = "" + y;

			}
			c += 2;

		}

	}

	/**
	 * Method for a DFBoxPlots. Builds the table components (column titles, main
	 * content, column colors...).
	 */
	public void initDFDFBoxPlotsTable(List<DFBoxPlots> extractors) throws Exception {

		// fc-7.7.2021 Under progress...

//		System.out.println("DRTableBuilder...");

		// We create columns for each series of each type (category, e.g. year) of each
		// extractor (generally one single here)

		List<List> columns = new ArrayList<>();

		// First column is a header
		List col0 = new ArrayList();
		columns.add(col0);
		// One color per column
		colors.add(Color.GRAY);

		col0.add("");
		col0.add("Type");
		col0.add("Series");
		col0.add(""); // blank line
		col0.add("Values");

		nLin = 0;
		nCol = 0;

		for (DFBoxPlots ex : extractors) {

//			System.out.println();
//			System.out.println("DRTableBuilder Extractor: " + ex);

			DoubleKeyMap<String, String, List<Double>> dataset = ex.getDatasets();

			for (String key1 : dataset.keySet1()) {

//				System.out.println();
//				System.out.println("DRTableBuilder key1: " + key1);

				List col = null;

				for (String key2 : dataset.keySet2(key1)) {

					col = new ArrayList();
					columns.add(col);
					
					// One color per column
					colors.add(ex.getColor ());

					col.add(ex.getCaption ()); // may contain variable name...
					col.add(key1);
					col.add(key2);
					col.add(""); // blank line

					List<Double> values = dataset.get(key1, key2);
					col.addAll(values);

//					System.out.println("DRTableBuilder key2: " + key2 + " " + AmapTools.toString(values));

				}

				nLin = Math.max(nLin, col.size());

			}

			nCol = columns.size();

			// Create the expected arrays
			colNames = new String[nCol];
			mat = new String[nLin][nCol];

			for (int col = 0; col < nCol; col++) {

				List column = columns.get(col);
				
				// Keep colNames empty
//				colNames[col] = "XXX";

				for (int lin = 0; lin < nLin; lin++) {
					try {
						mat[lin][col] = column.get(lin).toString();
					} catch (Exception e) {
						// nLin is the size of the bigger column, we will have exception for shorter
						// columns
						// -> No more lines in the column
						// Jump to next column
						break;
					}
				}

			}

//			System.out.println("DRTableBuilder values:");
//			for (int row = 0; row < nRows; row++) {
//				for (int col = 0; col < nCol; col++) {
//					System.out.print(""+dataset.getValue(row, col)+" ");
//				}
//				System.out.println();
//			}

		}
//		System.out.println();
	}

	/**
	 * Method for a list of DFListOfCategories. Builds the table components (column
	 * titles, main content, column colors...).
	 */
	public void initDFListOfCategoriesTable(List<DFListOfCategories> extractors) throws Exception {

		// 1. Compute table total size to define layout arrays sizes
		nLin = 0;
		nCol = 0;

		for (Iterator i = extractors.iterator(); i.hasNext();) {
			DFListOfCategories extr = (DFListOfCategories) i.next();

			int n = extr.getListOfCategories().size();
			nCol += n * 2; // {(label,value)}

			for (Categories cat : extr.getListOfCategories()) {
				nLin = Math.max(nLin, cat.size());
			}

		}

		nLin += 3; // 3 lines for upper column titles

		// Enlarge table for better rendering
		if (nLin < nLinMin)
			nLin = nLinMin;
		if (nCol < nColMin)
			nCol = nColMin;

		// 2. Size layout arrays
		colNames = new String[nCol];
		mat = new String[nLin][nCol];
		int c = 0;

		for (Iterator i = extractors.iterator(); i.hasNext();) {
			DFListOfCategories extr = (DFListOfCategories) i.next();

			String extractorCaption = AmapTools.cutIfTooLong(extr.getCaption(), 50);

			List<String> axesNames = extr.getAxesNames();
			String xName = (String) axesNames.get(0);
			String yName = (String) axesNames.get(1);

			for (Categories cat : extr.getListOfCategories()) {

				// header: 3 lines
				mat[0][c] = extractorCaption;
				mat[0][c + 1] = extractorCaption;
				mat[1][c] = cat.getName();
				mat[1][c + 1] = cat.getName();
				mat[2][c] = xName;
				mat[2][c + 1] = yName;

				int l = 3;
				for (Categories.Entry e : cat.getEntries()) {

					mat[l][c] = "" + e.label;
					mat[l][c + 1] = "" + e.value;
					l++;

				}

				// space to replace null values
				for (int _i = l; _i < nLin; _i++) {
					mat[_i][c] = "";
					mat[_i][c + 1] = "";
				}

				colors.add(cat.getColor());
				colors.add(cat.getColor());

				c += 2;
			}

		}

		// Enlarge table for better rendering
		if (c < mat[0].length) {
			for (int _j = c; _j < mat[0].length; _j++) {
				for (int _i = 0; _i < nLin; _i++) {
					mat[_i][_j] = ""; // space to replace null values
				}
				colors.add(new Color(245, 245, 245)); // Color for extra
														// column headers:
														// very light gray
			}

		}

	}

	/**
	 * Method for a list of DFListOfXYSeries. Builds the table components (column
	 * titles, main content, column colors...).
	 */
	public void initDFListOfXYSeriesTable(List<DFListOfXYSeries> extractors) throws Exception {

		// 1. Compute table total size to define layout arrays sizes
		nLin = 0;
		nCol = 0;

		for (Iterator i = extractors.iterator(); i.hasNext();) {
			DFListOfXYSeries extr = (DFListOfXYSeries) i.next();

			int n = extr.getListOfXYSeries().size();
			nCol += n * 2; // {(x,y)}

			for (XYSeries xySeries : extr.getListOfXYSeries()) {
				nLin = Math.max(nLin, xySeries.size());
			}

		}

		nLin += 3; // 3 lines for upper column titles

		// Enlarge table for better rendering
		if (nLin < nLinMin)
			nLin = nLinMin;
		if (nCol < nColMin)
			nCol = nColMin;

		// 2. Size layout arrays
		colNames = new String[nCol];
		mat = new String[nLin][nCol];
		int c = 0;

		for (Iterator i = extractors.iterator(); i.hasNext();) {
			DFListOfXYSeries extr = (DFListOfXYSeries) i.next();

			String extractorCaption = AmapTools.cutIfTooLong(extr.getCaption(), 50);

			List<String> axesNames = extr.getAxesNames();
			String xName = (String) axesNames.get(0);
			String yName = (String) axesNames.get(1);

			for (XYSeries xySeries : extr.getListOfXYSeries()) {

				// header: 3 lines
				mat[0][c] = extractorCaption;
				mat[0][c + 1] = extractorCaption;
				mat[1][c] = xySeries.getName();
				mat[1][c + 1] = xySeries.getName();
				mat[2][c] = xName;
				mat[2][c + 1] = yName;

				int l = 3;
				for (Vertex2d v : xySeries.getPoints()) {

					mat[l][c] = "" + v.x;
					mat[l][c + 1] = "" + v.y;
					l++;

				}

				// space to replace null values
				for (int _i = l; _i < nLin; _i++) {
					mat[_i][c] = "";
					mat[_i][c + 1] = "";
				}

				colors.add(xySeries.getColor());
				colors.add(xySeries.getColor());

				c += 2;
			}

		}

		// Enlarge table for better rendering
		if (c < mat[0].length) {
			for (int _j = c; _j < mat[0].length; _j++) {
				for (int _i = 0; _i < nLin; _i++) {
					mat[_i][_j] = ""; // space to replace null values
				}
				colors.add(new Color(245, 245, 245)); // Color for extra
														// column headers:
														// very light gray
			}

		}

	}

	/**
	 * Method for a list of DFCurves. Builds the table components (column titles,
	 * main content, column colors...).
	 */
	public void initDFCurvesTable(List<DFCurves> extractors) throws Exception {

		// 1. Compute table total size to define layout arrays sizes
		nLin = 0;
		nCol = 0;

		for (Iterator i = extractors.iterator(); i.hasNext();) {
			DFCurves extr = (DFCurves) i.next();

			List<List<? extends Number>> curves = extr.getCurves();
			nCol += curves.size(); // + n 1D coordinates vectors (x, y1, y2
									// ...yn-1)

			for (List<? extends Number> curve : curves) {
				if (curve.size() > nLin)
					nLin = curve.size();
			}
		}

		nLin += 3; // 3 lines for upper column titles

		// fc-24.11.2014 Enlarge table for better rendering
		if (nLin < nLinMin)
			nLin = nLinMin;
		if (nCol < nColMin)
			nCol = nColMin;

		// 2. Size layout arrays
		colNames = new String[nCol];
		mat = new String[nLin][nCol];

		// 3. Put data in table (vertically)
		//
		// x1 y1 y'1 a1 b1
		// x2 y2 y'2 a2 b2
		// x3 y3 y'3 . .
		// . . . . .
		// . . . am bm
		// . . .
		// xn yn y'n
		//
		// NOTE: a "curve" represents here a vector of numbers of type
		// Integer or Double which are xs or ys of a real curve.
		// ex: For a given data extractor:
		// x curve = [1, 2, 3, 4] Integer
		// y1 curve = [5, 6, 7, 8] Integer
		// y2 curve = [1.1, 2.2, 3.3, 4.4] Double
		// represent 2 real curves with four points each :
		// 1: (1, 5) (2, 6) (3, 7) (4, 8)
		// 2: (1, 1.1) (2, 2.2) (3, 3.3) (4, 4.4)
		//
		int iLin = 0;
		int iCol = 0;
		int c = 0;

		Color extractorColor; // fc - 5.10.2002

		// Iterate on extractors
		for (Iterator i = extractors.iterator(); i.hasNext();) {
			DFCurves extr = (DFCurves) i.next();

			extractorColor = ((DataExtractor) extr).getColor(); // fc -
																// 5.10.2002

			List<List<? extends Number>> curves = extr.getCurves();

			// Retrieve axes names for current extractor
			List<String> axesNames = extr.getAxesNames();
			String xName = "";
			String yName = "";
			String zName = "";
			if (axesNames.size() < 2) { // should be 2 or 3
				Log.println(Log.ERROR, "DRTable.update (int, int)",
						"Wrong name of axes names: " + axesNames.size() + ", should be 2 or 3");
				return;
			} else {
				c = 0; // column number inside one extractor
				xName = (String) axesNames.get(0);
				yName = (String) axesNames.get(1);

				if (axesNames.size() > 2) { // three axes
					zName = (String) axesNames.get(2); // if more, ignore
				}
			}

			// If extr.getLabels () contains labels for x axes, "swap"
			// vectors
			boolean xLabelsToBeReplaced = false;
			boolean yCurveNames = false;
			boolean pointNames = false;
			List xLabels = null;
			Iterator yNamesIterator = null;
			try {
				xLabels = (List) extr.getLabels().get(0); // first line =
															// labels in x
				if (xLabels != null && xLabels instanceof List && !xLabels.isEmpty()) {
					if (xLabels.size() != ((List) curves.get(0)).size()) {
						Log.println(Log.ERROR, "DRTable.update (int, int)",
								"Wrong length for first vector in extr.getLabels (). Is " + xLabels.size()
										+ ", should be " + ((List) curves.get(0)).size() + ", extr=" + extr);
						return;
					} else {
						xLabelsToBeReplaced = true;
					}
				}
				if (extr.getLabels().size() > 1) {
					List v = (List) extr.getLabels().get(1);
					yNamesIterator = extr.getLabels().iterator();
					Object nada = yNamesIterator.next(); // jump xLabels
					if (v.size() == 1) {
						yCurveNames = true; // each real curve has a name
											// (ex: tree #3, min,
											// max...)
						// ~ yNamesIterator = extr.getLabels ().iterator ();
						// ~ Object nada = yNamesIterator.next (); // jump
						// xLabels
					} else {
						pointNames = true; // each point has a name (ex:
											// 1994, 1995, 1996...)
						mat = new String[(nLin - 3) * 2 + 3][nCol]; // fc -
																	// 23.12.2004
																	// -
																	// more
																	// lines
					}
				}
			} catch (Exception e) {
			} // extr.getLabels () may be null if unused

			// Compute nY number of curves in y (there may be some more nZ
			// in z) Rq: nX = 1
			// always
			int nY = 0;
			if (extr.getNY() != 0) {
				nY = extr.getNY(); // if nY is specified, take it
			} else {
				nY = axesNames.size() - 1; // - 1 for x, all in y, nothing
											// in z
			}
			int nZ = axesNames.size() - 1 - nY; // total - nX - nY

			// For one extractor, iterate on curves
			for (Iterator j = curves.iterator(); j.hasNext();) {

				colors.add(extractorColor); // fc - 5.10.2002

				c++; // iCol in [0, N-1], c in [1, n] for each extr
				List curve = null;

				if (c == 1 && xLabelsToBeReplaced) { // first "curve" &
														// labels to replace
					curve = (List) j.next(); // to eliminate it
					curve = xLabels;
				} else {
					curve = (List) j.next();
				}

				// Line 1: extractor label
				String extractorCaption = AmapTools.cutIfTooLong(extr.getCaption(), 50);
				mat[0][iCol] = extractorCaption;

				// Line 2: axis name
				if (c == 1) {
					mat[1][iCol] = xName;
				} else if (c >= 2 && c <= nY + 1) {
					mat[1][iCol] = yName;
				} else {
					mat[1][iCol] = zName;
				}

				// Line 3: y curve name (optional)
				if (yCurveNames && c > 1) {
					List v = (List) yNamesIterator.next();
					String name = (String) v.iterator().next();
					mat[2][iCol] = name;
				} else {
					mat[2][iCol] = ""; // space to replace null values
				}

				// Prepare pointNames management
				Iterator yLabels = null; // fc - 23.12.2004
				if (pointNames && c > 1) {
					List v = (List) yNamesIterator.next();
					yLabels = v.iterator();
				}

				// for one curve, iterate on numbers
				iLin = 3; // we begin at line 4
				for (Iterator k = curve.iterator(); k.hasNext();) {
					Object item = k.next();

					if (pointNames) {
						if (c <= 1) {
							mat[iLin][iCol] = ""; // pointNames : nothing in
													// column 0 on this
													// line
						} else {
							mat[iLin][iCol] = (String) yLabels.next();
						}
						iLin++;
					}

					if (item instanceof Integer) {
						Integer integer = (Integer) item;
						mat[iLin][iCol] = integer.toString();
					} else if (item instanceof Double) {
						Double d = (Double) item;
						mat[iLin][iCol] = d.isNaN() ? "" : d.toString(); // fc
																			// -
																			// 23.12.2004
					} else if (item instanceof String) {
						mat[iLin][iCol] = (String) item;
					} else {
						Log.println(Log.ERROR, "DRTable.createUI ()",
								"Wrong type for item in getCurves () : " + item.getClass().getName());
					}
					iLin++;
				}
				for (int _i = iLin; _i < nLin; _i++)
					mat[_i][iCol] = ""; // space to replace null values

				iCol++; // next column
			}

		}

		// fc-24.11.2014 Enlarge table for better rendering
		if (iCol < mat[0].length) {
			for (int _j = iCol; _j < mat[0].length; _j++) {
				for (int _i = 0; _i < nLin; _i++) {
					mat[_i][_j] = ""; // space to replace null values
				}
				colors.add(new Color(245, 245, 245)); // Color for extra
														// column headers:
														// very light gray
			}

		}

	}

	/**
	 * Returns the whole table in a single string, with tabs between columns and
	 * newline at each line end.
	 */
	public String getTableInAString() {
		StringBuffer b = new StringBuffer();

		char separator = '\t';
		char newLine = '\n';

		// column headers
		for (int c = 0; c < colNames.length; c++) {
			String s = colNames[c] != null ? colNames[c] : "";
			b.append(s);
			b.append(separator);
		}
		b.append(newLine);

		// matrix content
		for (int l = 0; l < mat.length; l++) {

			for (int c = 0; c < colNames.length; c++) {
				String s = mat[l][c] != null ? mat[l][c] : "";
				b.append(s);
				b.append(separator);

			}
			b.append(newLine);

		}

		return b.toString();
	}

	public int getnLin() {
		return nLin;
	}

	public int getnCol() {
		return nCol;
	}

	public String[] getColNames() {
		return colNames;
	}

	public String[][] getMat() {
		return mat;
	}

	public List<Color> getColors() {
		return colors;
	}

}
