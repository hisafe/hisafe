package capsis.extension.datarenderer.drgraph;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.MouseListener;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import javax.swing.JComponent;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.StandardChartTheme;
import org.jfree.chart.axis.CategoryAxis;
import org.jfree.chart.axis.CategoryLabelPositions;
import org.jfree.chart.labels.StandardCategoryItemLabelGenerator;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.renderer.category.BarRenderer;

import capsis.app.CapsisExtensionManager;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.commongui.projectmanager.RelatedColorProvider;
import capsis.extension.PanelDataRenderer;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.extension.dataextractor.format.DFListOfCategories;
import capsis.extension.datarenderer.jfreechart.MessagePanel;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;
import capsis.util.configurable.ConfigurationPanel;
import jeeb.lib.util.DefaultNumberFormat;
import jeeb.lib.util.Log;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * A data renderer for data extractors implementing DFListOfXYSeries and
 * DFCurves : draws histograms. Version 3, replaces DRHistogram (original 2003).
 * 
 * @author F. de Coligny - October 2015
 */
public class DRBarGraph extends PanelDataRenderer {

	static {
		Translator.addBundle("capsis.extension.datarenderer.drgraph.Labels");
	}

	// nb-06.08.2018
	// static final public String NAME = Translator.swap("DRBarGraph");
	// static final public String VERSION = "3.0";
	// static final public String AUTHOR = "F. de Coligny";
	// static final public String DESCRIPTION =
	// Translator.swap("DRBarGraph.description");

	protected boolean visibleValues;
	protected boolean cumulative;
	protected DRLegendExplorer legendExplorer; // fc-9.4.2018

	/**
	 * Constructor
	 */
	public DRBarGraph() {
		super();
	}

	/**
	 * Inits the graph
	 */
	@Override
	public void init(DataBlock db) {
		try {
			super.init(db);

			visibleValues = Settings.getProperty("drbargraph.visibleValues", false);
			cumulative = Settings.getProperty("drbargraph.cumulative", false);

			legendExplorer = new DRLegendExplorer();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DRBarGraph.init ()", "Error in init (), wrote in Log and passed", e);
		}

	}

	/**
	 * Tells if the renderer can show an extractor's production. True if the
	 * extractor is an instance of the renderer's compatible data formats Note:
	 * DataExtractor must implement a data format in order to be recognized by
	 * DataRenderers.
	 */
	static public boolean matchWith(Object target) {

		return target instanceof DataExtractor && (target instanceof DFListOfCategories || target instanceof DFCurves);

	}

	@Override
	public String getName() {
		return Translator.swap("DRBarGraph.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("DRBarGraph.description");
	}

	@Override
	public String getVersion() {
		return "3.0";
	}

	/**
	 * Update strategy for DRGraph and its subclasses. This method is used to
	 * refresh the renderer after configuration.
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
	 * One single chart with maybe several histograms inside.
	 */
	protected JComponent createView() {
		try {

			// Consider all extractors
			Collection extractors = dataBlock.getDataExtractors();
			Collection specialExtractors = dataBlock.getSpecialExtractors();

			List allExtractors = new ArrayList<>(extractors);
			if (specialExtractors != null) {
				allExtractors.addAll(specialExtractors);
			}

			// Extractors have all of the same class: consider the first
			Object representative = allExtractors.iterator().next();

			CategoryConverter c = null;

			if (representative instanceof DFCurves) {
				// Case of DFCurve instances
				List<DFCurves> dataList = new ArrayList<DFCurves>();
				for (Object e : allExtractors) {
					dataList.add((DFCurves) e);
				}

				// WARNING: DFCurves may contain extra XYSeries... (since Teresa
				// Fonseca SDI graph with two additional lines for min and max
				// sdi corridor)
				// -> Remove extra XYSeries, replace by a DFListOfXYSeries

				c = CategoryConverter.convertDFCurves(dataList);

				// fc-1.10.2020
				// The bug was in CategoryConverterHelper.RangeComparator, fixed
//				System.out.println("DRBarGraph.createView()...");
//				System.out.println("CategoryConverter.toString (): " + c);

//				DataExtractor ex = (DataExtractor) allExtractors.get(0);
//				if (ex instanceof DETimeDendroHabitats)
//					System.out.println("DRBarGraph... representative DETimeDendroHabitats.getYAxisVariableNames (): "
//							+ toString (((DETimeDendroHabitats) ex).getYAxisVariableNames()));

				// TMP: fc-7.1.2016 pb on pp3 DEBiomassDistrib histogram (found
				// it...)
				// System.out.println(c.toString ());

			} else if (representative instanceof DFListOfCategories) {
				// Case of DFListOfCategories instances
				List<DFListOfCategories> dataList = new ArrayList<DFListOfCategories>();
				for (Object e : allExtractors) {
					dataList.add((DFListOfCategories) e);
				}
				c = CategoryConverter.convertDFListOfCategories(dataList);
			}

			return createCategoryChart(c);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DRBarGraph.createView ()", "Exception", e);

			// fc-23.12.2020
			return new MessagePanel(getName() + " "
					+ Translator.swap("DRBarGraph.canNotShowTheseDataSeeLog"), (MouseListener) this);
//			return new MessagePanel(CapsisExtensionManager.getInstance().getName(this) + " "
//					+ Translator.swap("DRBarGraph.canNotShowTheseDataSeeLog"), (MouseListener) this);
			
		}

	}

	// Should be MOVED to AmapTools // fc-21.9.2016
	/**
	 * Create a String image of a double[] array.
	 */
	public static String toString(String[] x) {

		if (x == null)
			return "null";

//		NumberFormat nf = NumberFormat.getInstance();
//		nf.setMaximumFractionDigits(3);
//		nf.setGroupingUsed(false);

		StringBuffer s = new StringBuffer();
		s.append("[");
		int length = x.length;
		int lengthMinusOne = length - 1;
		for (int i = 0; i < length; i++) {
			s.append(x[i]);
			if (i < lengthMinusOne) {
				s.append(" ");
			}
		}
		s.append("]");
		return s.toString();
	}

	/**
	 * Creates a category chart.
	 */
	protected JComponent createCategoryChart(CategoryConverter c) {

		// Use a simple theme
		StandardChartTheme theme = (StandardChartTheme) StandardChartTheme.createLegacyTheme();
		ChartFactory.setChartTheme(theme);

		// Chart title -> leave blank, already written in frame title bar
		String title = "";
		// String title = c.getTitle();

		JFreeChart chart = null;

		// Introduced the cumulative histogram // fc+bc-19.9.2016
		// Activated per user on renderer config panel
		// Compatible only if all the data series come from the same step
		// to prevent inconsistent views
		if (cumulative && dataBlock.allExtractorsAreOnTheSameStep()) {

			chart = ChartFactory.createStackedBarChart(title, // chart
																// title
																// ->
					// leave blank,
					// written in
					// frame title
					// bar
					c.getXAxisName(), // domain axis label
					c.getYAxisName(), // range axis label
					c.getCategoryDataset(), // data
					PlotOrientation.VERTICAL, // orientation
					true, // include legend
					true, // tooltips
					false // URLs?
			);

		} else {

			chart = ChartFactory.createBarChart(title, // chart title
														// ->
					// leave blank,
					// written in
					// frame title
					// bar
					c.getXAxisName(), // domain axis label
					c.getYAxisName(), // range axis label
					c.getCategoryDataset(), // data
					PlotOrientation.VERTICAL, // orientation
					true, // include legend
					true, // tooltips
					false // URLs?
			);

		}

//		// Set legend above the graph
//		LegendTitle legend = chart.getLegend();
//		legend.setPosition(RectangleEdge.TOP);
//		legend.setFrame(BlockBorder.NONE);
//		legend.setHorizontalAlignment(HorizontalAlignment.RIGHT); // bugs when
//																	// too
//																	// long...

		CategoryPlot plot = (CategoryPlot) chart.getPlot();

		BarRenderer renderer = (BarRenderer) plot.getRenderer();

		// Removes the bar shadows
		renderer.setShadowVisible(false);
		renderer.setMaximumBarWidth(.15); // set maximum width to 35% of chart

		// Rotate domain labels vertically
		CategoryAxis domainAxis = plot.getDomainAxis();
		domainAxis.setCategoryLabelPositions(CategoryLabelPositions.UP_45);

		// Set colors / item labels
		NumberFormat nf = DefaultNumberFormat.getInstance();
		nf.setMaximumFractionDigits(1);
		nf.setGroupingUsed(false);
		StandardCategoryItemLabelGenerator cilg = new StandardCategoryItemLabelGenerator(
				StandardCategoryItemLabelGenerator.DEFAULT_LABEL_FORMAT_STRING, nf);

		int i = 0;
		Set<Color> usedColors = new HashSet<>();

		// fc-5.11.2020
		RelatedColorProvider cp = ProjectManager.getInstance().getColorProvider();

		// fc-5.11.2020 Reproductible draws
		Random random = new Random(1);
//		Random random = new Random();

//		// fc-5.11.2020
//		Map<Color, ColorBag> bagMap = new HashMap<>();
//		for (Color color : c.getSeriesColors()) {
//			ColorBag bag = cp.getColorBag(new UserColor (color));
//			if (bag != null) {
//				bagMap.put(color, bag);
//				bag.setCurrentIndex(new UserColor (color));
//			}
//		}

		for (Color color : c.getSeriesColors()) {

			if (usedColors.contains(color)) {
				
				// Try to find a related (close) color
//				System.out.print("DRBarGraph... ");
				color = cp.getRelatedColor(color, i);
				
				// Moved as a security in RelatedColorProvider
				// Get a related color, e.g. change blue and alpha
//				color = new Color(color.getRed(), color.getGreen(), random.nextInt(255), random.nextInt(255));
				
			}	

			renderer.setSeriesPaint(i, color);

			if (visibleValues) {
				renderer.setSeriesItemLabelGenerator(i, cilg);
				renderer.setSeriesItemLabelsVisible(i, true);
			}

			usedColors.add(color);

			i++;
		}

		// Theme customization
		chart.setBackgroundPaint(Color.WHITE);
		plot.setBackgroundPaint(Color.WHITE);

		ChartPanel chartPanel = new GraphChartPanel(chart, dataBlock);

		// fc-9.4.2018
		legendExplorer.update(getTitle(), chartPanel);
		return legendExplorer.getChartWithLegend();

//		return chartPanel;

	}

	/**
	 * In Configurable
	 */
	@Override
	public ConfigurationPanel getConfigPanel(Object parameter) {
		return new DRBarGraphConfigurationPanel(this);
	}

	/**
	 * In Configurable
	 */
	@Override
	public void applyConfig(ConfigurationPanel panel) {
		super.applyConfig(panel); // DataRenderer configuration

		DRBarGraphConfigurationPanel p = (DRBarGraphConfigurationPanel) panel;

		visibleValues = p.isVisibleValues();
		cumulative = p.isCumulative();

		// Memo for next time
		Settings.setProperty("drbargraph.visibleValues", visibleValues);
		Settings.setProperty("drbargraph.cumulative", cumulative);

	}

	/**
	 * In Configurable
	 */
	@Override
	public void postConfig() {
		ExtensionManager.recordSettings(this);
	}

}
