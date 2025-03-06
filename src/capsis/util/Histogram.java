package capsis.util;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JPanel;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.StandardChartTheme;
import org.jfree.chart.axis.CategoryAxis;
import org.jfree.chart.axis.CategoryLabelPositions;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.Plot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.renderer.category.BarRenderer;
import org.jfree.data.category.CategoryDataset;
import org.jfree.data.category.DefaultCategoryDataset;

import jeeb.lib.util.DefaultNumberFormat;
import jeeb.lib.util.Vertex2d;
import jeeb.lib.util.XYSeries;

/**
 * A simple histogram based on XYSeries series and a jfreechart component.
 * 
 * @author F. de Coligny - December 2021
 */
public class Histogram extends JPanel {

	private String chartTitle;
	private String xAxisLabel;
	private String yAxisName = "N"; // default value, no fr/en translation needed
	private List<XYSeries> seriesList;
	private boolean xAreAllIntegers;

	public static final NumberFormat intFormatter = DefaultNumberFormat.getInstance(0);
	static {
		intFormatter.setParseIntegerOnly(true);
	}

	/**
	 * Constructor, single series.
	 */
	public Histogram(String chartTitle, String xAxisLabel, XYSeries series) {
		super();

		List<XYSeries> list = new ArrayList<>();
		list.add(series);
		init(chartTitle, xAxisLabel, list);
	}

	/**
	 * Constructor 2, list of series.
	 */
	public Histogram(String chartTitle, String xAxisLabel, List<XYSeries> seriesList) {
		super();
		init(chartTitle, xAxisLabel, seriesList);
	}

	/**
	 * Constructor 3, specific yAxisName and list of series.
	 */
	public Histogram(String chartTitle, String xAxisLabel, String yAxisLabel, List<XYSeries> seriesList) {
		// fc+fa-29.3.2022 yAxisLabel
		super();
		this.yAxisName = yAxisLabel;
		init(chartTitle, xAxisLabel, seriesList);
	}

	private boolean checkIfXAreAllIntegers() {

		for (XYSeries series : seriesList) {
			for (Vertex2d v : series.getPoints()) {
				if (!isInt(v.x))
					return false;
			}

		}
		return true;
	}

	private boolean isInt(double v) {
		return (int) v == v;
	}

	/**
	 * Constructor
	 */
	public void init(String chartTitle, String xAxisLabel, List<XYSeries> seriesList) {

		this.chartTitle = chartTitle;
		this.xAxisLabel = xAxisLabel;
		this.seriesList = seriesList;

		xAreAllIntegers = checkIfXAreAllIntegers();

		CategoryDataset dataset = createDataSet();

		final JFreeChart chart = createChart(dataset);

		setLayout(new BorderLayout());

		// Theme customization
		Plot plot = chart.getPlot();
		chart.setBackgroundPaint(Color.WHITE);
		plot.setBackgroundPaint(Color.WHITE);

		ChartPanel chartPanel = new ChartPanel(chart);

		chartPanel.setMinimumSize(new Dimension(100, 100));
		chartPanel.setPreferredSize(new Dimension(200, 200));

		add(chartPanel, BorderLayout.CENTER);

	}

	private CategoryDataset createDataSet() {

		DefaultCategoryDataset dataset = new DefaultCategoryDataset();

		for (XYSeries series : seriesList) {
			for (Vertex2d v : series.getPoints()) {

				Comparable x = null;
				if (xAreAllIntegers)
					x = new Integer((int) v.x);
				else
					x = new Double(v.x);

				dataset.addValue(v.y, series.getName(), x);
			}
		}

		return dataset;
	}

	/**
	 * Creates a sample chart.
	 */
	private JFreeChart createChart(final CategoryDataset dataset) {

		// Use a simple theme fc+mj-30.10.2015
		StandardChartTheme theme = (StandardChartTheme) StandardChartTheme.createLegacyTheme();
		ChartFactory.setChartTheme(theme);

		// create the chart...
		final JFreeChart chart = ChartFactory.createBarChart("", // title: see
																	// below
				xAxisLabel, // domain axis label
				yAxisName, // range axis label
				dataset, // data
				PlotOrientation.VERTICAL, // orientation
				true, // include legend
				true, // tooltips?
				false // URLs?
		);

		String title = chartTitle;

		// Change title size (set it a little smaller), see HistogramPanel
		chart.setTitle(
				new org.jfree.chart.title.TextTitle(title, new java.awt.Font("SansSerif", java.awt.Font.ITALIC, 14)));

		// NOW DO SOME OPTIONAL CUSTOMISATION OF THE CHART...

		// set the background color for the chart...
		chart.setBackgroundPaint(null);

		// get a reference to the plot for further customisation...
		final CategoryPlot plot = chart.getCategoryPlot();
		// plot.setBackgroundPaint(Color.lightGray);
		// plot.setDomainGridlinePaint(Color.white);
		// plot.setRangeGridlinePaint(Color.white);

		// set the domain axis to display integers only...
		final NumberAxis rangeAxis = (NumberAxis) plot.getRangeAxis();
		CategoryAxis domainAxis = (CategoryAxis) plot.getDomainAxis();

		// fc+fa-29.3.2022 Tryng to tune the number on the X axis...
//		NumberFormat nf = DefaultNumberFormat.getInstance(3);
//		domainAxis.set
		
//		System.out.println("Histogram chartTitle: " + chartTitle + ", xAreAllIntegers: " + xAreAllIntegers);
//
//		System.out.println("Histogram domainAxis categories... ");
//		for (Object o : plot.getCategoriesForAxis(domainAxis)) {
//			Comparable c = (Comparable) o;
//			System.out.println("Histogram " + c.toString() + ", class: " + c.getClass());
//		}

		if (xAreAllIntegers) { // fc-7.12.2021
//			rangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());
//			rangeAxis.setNumberFormatOverride(intFormatter);
		}

		// disable bar outlines...
		final BarRenderer renderer = (BarRenderer) plot.getRenderer();
		renderer.setDrawBarOutline(true);

		// Removes the bar shadows fc+mj-30.10.2015
		renderer.setShadowVisible(false);

		// set up gradient paints for series...
		// final GradientPaint gp0 = new GradientPaint(0.0f, 0.0f, Color.blue,
		// 0.0f, 0.0f, Color.RED);
		// final GradientPaint gp1 = new GradientPaint(0.0f, 0.0f, Color.green,
		// 0.0f, 0.0f, Color.BLUE);
//		renderer.setSeriesPaint(0, Color.RED);
		int i = 0;
		for (XYSeries series : seriesList) {
			renderer.setSeriesPaint(i++, series.getColor());
		}

//		final CategoryAxis domainAxis = plot.getDomainAxis();
		domainAxis.setCategoryLabelPositions(CategoryLabelPositions.createUpRotationLabelPositions(Math.PI / 6.0));
		// OPTIONAL CUSTOMISATION COMPLETED.

		return chart;

	}

}
