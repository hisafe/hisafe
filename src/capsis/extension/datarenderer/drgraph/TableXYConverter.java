package capsis.extension.datarenderer.drgraph;

import java.awt.Color;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jeeb.lib.util.DefaultNumberFormat;

import org.jfree.data.xy.DefaultTableXYDataset;
import org.jfree.data.xy.XYDataItem;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 * A data converter for the StackedXYAreaRenderer2 renderer.
 * 
 * @author F. de Coligny - September 2016
 */
public class TableXYConverter {

	public static final double EPSILON = 1E-12;

	private NumberFormat nf = DefaultNumberFormat.getInstance(2);
	
	private String title;
	private String xAxisName;
	private String yAxisName;
	private DefaultTableXYDataset tableXYDataset;
	private List<Color> seriesColors;
	// Index of the first series of each source
	private List<Integer> sourceHeads;
	
	/**
	 * Constructor
	 */
	public TableXYConverter() {
	}

	/**
	 * Converts a GraphConverter (containing an XYSeriesCollection) into a
	 * TableXYConverter (which tableXYDataset contains XYSeries **with unique X
	 * values**). Used for the StackedXYAreaRenderer2, needs unique X values.
	 */
	public static TableXYConverter convert(GraphConverter in) throws Exception {
		TableXYConverter c = new TableXYConverter();

		c.title = in.getTitle();
		c.xAxisName = in.getXAxisName();
		c.yAxisName = in.getYAxisName();

		c.tableXYDataset = new DefaultTableXYDataset();
		c.sourceHeads = in.getSourceHeads();
		
		XYSeriesCollection xySeriesCollection = in.getXYSeriesCollection();

		Map<Double, Double> uniqueXMap = new HashMap<>();

		// For each XYSeries
		for (int i = 0; i < xySeriesCollection.getSeriesCount(); i++) {
			XYSeries series = xySeriesCollection.getSeries(i);
			
			uniqueXMap.clear ();
			
			Comparable key = series.getKey(); // same key
			boolean autoSort = true; // needed
			boolean allowDuplicateXValues = false; // needed

			XYSeries newSeries = new XYSeries(key, autoSort, allowDuplicateXValues);

			for (Object o2 : series.getItems()) {
				XYDataItem xy = (XYDataItem) o2;
				double candidateX = xy.getXValue();
				double y = xy.getYValue();

				double x = getUniqueX(uniqueXMap, candidateX);

				newSeries.add(x, y); // x will be unique
			}

			c.tableXYDataset.addSeries(newSeries);

		}

		// Colors
		// Drawing a stacked area graph with all same colors is not possible
		// If the same colors are found, change something
		c.seriesColors = new ArrayList <> ();
		for (Color color : in.getSeriesColors()) {
			
			// remove the second and next occurrences of the same color
			if (!c.seriesColors.contains (color)) {
				c.seriesColors.add(color);
			}
				
		}

		
		return c;
	}

	static private double getUniqueX(Map<Double, Double> uniqueXMap, double candidateX) {

		// get the memo value which was returned last time for this candidateX
		Double memoX = uniqueXMap.get(candidateX);

		if (memoX == null) {
			// candidateX not found in map: first time it is proposed
			// memo it for next time in case the same value is proposed
			// and return it: it is unique so far
			uniqueXMap.put(candidateX, candidateX);
			
//			System.out.println("TableXYConverter getUniqueX() proposed: "+candidateX+" returned: "+ candidateX);
			return candidateX;

		} else {
			// candidateX was found in the map: it was already proposed
			// choose new value by adding EPSILON to the memo value
			// memo the new value and return it
			double newValue = memoX + EPSILON;
			uniqueXMap.put(candidateX, newValue);
			
//			System.out.println("TableXYConverter getUniqueX() proposed: "+candidateX+" returned: "+ newValue);
			return newValue;
		}

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
	public DefaultTableXYDataset getTableXYDataset() {
		return tableXYDataset;
	}
	public List<Color> getSeriesColors() {
		return seriesColors;
	}

	public List<Integer> getSourceHeads() {
		return sourceHeads;
	}

	public int getSourceNumber () {
		return getSourceHeads() == null ? 0 : getSourceHeads().size ();
	}

	public String toString() {
		String newLine = "\n";
		String tab = "\t";
		StringBuffer b = new StringBuffer("TableXYConverter"+newLine);

		b.append("   title: "+title+newLine);
		b.append("   xAxisName: "+xAxisName+newLine);
		b.append("   yAxisName: "+yAxisName+newLine);
		
		b.append (newLine);
		
		b.append("   DefaultTableXYDataset"+newLine);

		for (int i = 0; i < tableXYDataset.getSeriesCount(); i++) {
		
			XYSeries xys = tableXYDataset.getSeries(i);

			b.append("   "+xys.getKey()+": "); // series name
			for (Object o : xys.getItems()) {
				XYDataItem xy = (XYDataItem) o;
				b.append("("+nf.format(xy.getXValue())+", "+nf.format(xy.getYValue())+") ");
				
			}
			b.append (newLine);

			
		}
		
		b.append(newLine);
		int cpt = 1;
		for (Color color : seriesColors) {
			b.append("   Color for series #"+(cpt++)+": "+color.toString()+newLine);
		}
			

		return b.toString();
	}


}
