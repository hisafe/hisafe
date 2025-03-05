package capsis.extension.datarenderer.drgraph;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.MouseListener;
import java.util.Collection;

import javax.swing.JComponent;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.StandardChartTheme;
import org.jfree.chart.axis.CategoryAxis;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.labels.BoxAndWhiskerToolTipGenerator;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.renderer.category.BoxAndWhiskerRenderer;

import capsis.extension.PanelDataRenderer;
import capsis.extension.dataextractor.format.DFBoxPlots;
import capsis.extension.datarenderer.jfreechart.MessagePanel;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;
import capsis.util.configurable.ConfigurationPanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * A data renderer for data extractors implementing DFBoxPlots: draws box plots.
 * 
 * @author F. de Coligny - July 2021
 */
public class DRBoxPlots extends PanelDataRenderer {

	static {
		Translator.addBundle("capsis.extension.datarenderer.drgraph.Labels");
	}

	protected boolean meanVisible; // to show the mean in the box plots

	protected GraphChartPanel chartPanel; // must be disposed
	protected DRLegendExplorer legendExplorer; // fc-9.4.2018

	/**
	 * Constructor
	 */
	public DRBoxPlots() {
		super();
	}

	/**
	 * Inits the graph
	 */
	@Override
	public void init(DataBlock db) {
		try {
			super.init(db);

			meanVisible = Settings.getProperty("drboxplots.meanVisible", false);

			legendExplorer = new DRLegendExplorer();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DRBoxPlots.init ()", "Error in init (), wrote in Log and passed", e);
		}

	}

	/**
	 * Tells if the renderer can show an extractor's production. True if the
	 * extractor is an instance of the renderer's compatible data formats Note:
	 * DataExtractor must implement a data format in order to be recognized by
	 * DataRenderers.
	 */
	static public boolean matchWith(Object target) {

		return target instanceof DataExtractor && (target instanceof DFBoxPlots);

	}

	@Override
	public String getName() {
		return Translator.swap("DRBoxPlots.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("DRBoxPlots.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	/**
	 * Update strategy for DRBoxPlots. This method is used to refresh the renderer
	 * after configuration.
	 */
	public void update() {
		super.update();
		removeAll();
		add(createView(), BorderLayout.CENTER);
		revalidate();

	}

	@Override
	public void dispose() {
		super.dispose();
		legendExplorer.dispose();

	}

	/**
	 * Renderer is closing, see if a panel legend must be closed.
	 */
	@Override
	public void close() {
		super.close();
		legendExplorer.dispose();

	}

	/**
	 * One single chart with maybe several extractors inside with maybe several
	 * boxPlots in each category (e.g. years) each.
	 */
	protected JComponent createView() {
		try {

			// Consider all extractors
			Collection extractors = dataBlock.getDataExtractors();

			// Special extractors are more dedicated to DRGraph and DRCurves
//			Collection specialExtractors = dataBlock.getSpecialExtractors();

//			List allExtractors = new ArrayList<>(extractors);
//			if (specialExtractors != null) {
//				allExtractors.addAll(specialExtractors);
//			}

			// Extractors have all of the same class: consider the first
			Object representative = extractors.iterator().next();

			// fc-7.7.2021 DRBoxPlots draws a single extractor at a time, the others if any
			// are ignored
			// fc-7.7.2021 DRBoxPlots draws a single extractor at a time, the others if any
			// are ignored
			// fc-7.7.2021 DRBoxPlots draws a single extractor at a time, the others if any
			// are ignored

			DFBoxPlots ex = (DFBoxPlots) representative;

			return createXYChart(ex);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DRBoxPlots.createView ()", "Exception", e);

			// fc-23.12.2020
			return new MessagePanel(getName() + " " + Translator.swap("DRBoxPlots.canNotShowTheseDataSeeLog"),
					(MouseListener) this);

		}

	}

	/**
	 * Creates a BoxAndWhisker chart.
	 */
	protected JComponent createXYChart(DFBoxPlots ex) {

		// Use a simple theme
		StandardChartTheme theme = (StandardChartTheme) StandardChartTheme.createLegacyTheme();
		ChartFactory.setChartTheme(theme);

		// Prepare the chart

		// Axes
		final CategoryAxis xAxis = new CategoryAxis(ex.getAxesNames().get(0)); // xLabel
		final NumberAxis yAxis = new NumberAxis(ex.getAxesNames().get(1)); // yLabel
		yAxis.setAutoRangeIncludesZero(false);

		// Renderer
		final BoxAndWhiskerRenderer renderer = new BoxAndWhiskerRenderer();
		renderer.setFillBox(false);
		renderer.setToolTipGenerator(new BoxAndWhiskerToolTipGenerator());

		// Configurable
		renderer.setMeanVisible(meanVisible);

		// Plot
		final CategoryPlot plot = new CategoryPlot(ex.getBoxAndWiskerCategoryDataset(), xAxis, yAxis, renderer);

		// Chart
		// Chart title -> leave blank, already written in frame title bar
		String title = "";
		final JFreeChart chart = new JFreeChart(title, new Font("SansSerif", Font.BOLD, 14), plot, true);

		// Theme customization
		chart.setBackgroundPaint(Color.WHITE);
		plot.setBackgroundPaint(Color.WHITE);

		if (chartPanel != null)
			chartPanel.dispose();

		chartPanel = new GraphChartPanel(chart, dataBlock);

//		chartPanel.setPreferredSize(new java.awt.Dimension(450, 270));

//		setContentPane(chartPanel);

		// fc-9.4.2018
		legendExplorer.update(getTitle(), chartPanel);
		return legendExplorer.getChartWithLegend();

		// -----------------------------------------------
		// -----------------------------------------------
		// -----------------------------------------------
		// -----------------------------------------------

		// fc-12.2.2021 The autoRange problem appeared in jfreechart-1.0.13 without
		// error. Switching to jfreechart-1.0.19 (2014) sent an error telling all the
		// series names were the same. Added code in GraphConverter and
		// CategoryConverter which deal with DFCurves, added (1), (2)... to the series
		// names if needed, fixed the problem.
		// DataExtractors sending non null and different labels for their data series
		// are not impacted
//
//		JFreeChart chart = ChartFactory.createXYLineChart(title, c.getXAxisName(), // domain
//																					// axis
//																					// label
//				c.getYAxisName(), // range axis label
//				c.getXYSeriesCollection(), // data
//				PlotOrientation.VERTICAL, // orientation
//				true, // include legend
//				true, // tooltips
//				false // URLs?
//		);
//
//		XYPlot plot = (XYPlot) chart.getPlot();
//
//		// Add line names at the right side of the lines
//		try {
//			XYSeriesCollection sc = c.getXYSeriesCollection();
//
//			for (Object o : sc.getSeries()) {
//				org.jfree.data.xy.XYSeries xys = (org.jfree.data.xy.XYSeries) o;
//
//				String text = xys.getDescription();
//				if (text != null && text.length() > 0) {
//					int n = xys.getItemCount();
//					double xLast = xys.getX(n - 1).doubleValue();
//					double yLast = xys.getY(n - 1).doubleValue();
//					XYTextAnnotation ta = new XYTextAnnotation(text, xLast, yLast);
//					ta.setTextAnchor(TextAnchor.BOTTOM_CENTER); // or
//																// BASELINE_LEFT
//					plot.addAnnotation(ta);
//				}
//			}
//		} catch (Exception e) {
//			// Annotations should not prevent displaying
//			Log.println(Log.WARNING, "DRGraph.createXYChart ()",
//					"Trouble while adding annotations in the graph, ignored", e);
//		}
//
//		// Tune the renderer
//		boolean lines = true;
//		boolean shapes = true;
//		XYLineAndShapeRenderer renderer = new XYLineAndShapeRenderer(lines, shapes) {
//
//			/**
//			 * Shape scaling
//			 */
//			public Shape getItemShape(int row, int column) {
//				Shape sup = super.getItemShape(row, column);
//				return scaleShape(sup);
//			}
//
//		};
//
//		plot.setRenderer(renderer);
//
////		// fc-10.2.2021
////		plot.configureRangeAxes();
////		plot.configureDomainAxes();
////		plot.setRangeGridlinesVisible(true);
////		plot.zoom(100);
//
//		// Set colors
//		int i = 0;
//		for (Color color : c.getSeriesColors()) {
//			renderer.setSeriesPaint(i++, color);
//		}
//
//		// Theme customization
//		chart.setBackgroundPaint(Color.WHITE);
//		plot.setBackgroundPaint(Color.WHITE);
//
//		// fc-9.3.2016
//		if (chartPanel != null)
//			chartPanel.dispose();
//
//		chartPanel = new GraphChartPanel(chart, dataBlock);
//
//		// fc-12.2.2021 Interesting (toString () was completed)
////		System.out.println("DRGraph > createXYChart() GraphConverter.toString (): " + c.toString());
//
//		// fc-9.4.2018
//		legendExplorer.update(getTitle(), chartPanel);
//		return legendExplorer.getChartWithLegend();
//
//		// return chartPanel;

	}

	/**
	 * In Configurable
	 */
	@Override
	public ConfigurationPanel getConfigPanel(Object parameter) {
		return new DRBoxPlotsConfigurationPanel(this);
	}

	/**
	 * In Configurable
	 */
	@Override
	public void applyConfig(ConfigurationPanel panel) {
		super.applyConfig(panel); // DataRenderer configuration

		DRBoxPlotsConfigurationPanel p = (DRBoxPlotsConfigurationPanel) panel;

		meanVisible = p.isMeanVisible();

		// Memo for next time
		Settings.setProperty("drboxplots.meanVisible", meanVisible);

	}

	/**
	 * In Configurable
	 */
	@Override
	public void postConfig() {
		ExtensionManager.recordSettings(this);
	}

}
