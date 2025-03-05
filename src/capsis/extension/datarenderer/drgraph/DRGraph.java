package capsis.extension.datarenderer.drgraph;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.MouseListener;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JComponent;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.StandardChartTheme;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.ui.TextAnchor;

import capsis.extension.PanelDataRenderer;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.extension.dataextractor.format.DFListOfXYSeries;
import capsis.extension.datarenderer.jfreechart.MessagePanel;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;
import capsis.util.configurable.ConfigurationPanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * A data renderer for data extractors implementing DFListOfXYSeries and
 * DFCurves : draws curves. Version 3, replaces DRCurves (original 2003,
 * reviewed 2011).
 * 
 * @author F. de Coligny - October 2015
 */
public class DRGraph extends PanelDataRenderer {

	static {
		Translator.addBundle("capsis.extension.datarenderer.drgraph.Labels");
	}

	// nb-06.08.2018
	// static final public String NAME = Translator.swap("DRGraph");
	// static final public String VERSION = "3.0";
	// static final public String AUTHOR = "F. de Coligny";
	// static final public String DESCRIPTION =
	// Translator.swap("DRGraph.description");

	protected boolean enlargedMode; // larger points on the graph

	protected GraphChartPanel chartPanel; // must be disposed
	protected DRLegendExplorer legendExplorer; // fc-9.4.2018

	/**
	 * Constructor
	 */
	public DRGraph() {
		super();
	}

	/**
	 * Inits the graph
	 */
	@Override
	public void init(DataBlock db) {
		try {
			super.init(db);

			enlargedMode = Settings.getProperty("drgraph.enlargedMode", false);

			legendExplorer = new DRLegendExplorer();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DRGraph.init ()", "Error in init (), wrote in Log and passed", e);
		}

	}

	/**
	 * Tells if the renderer can show an extractor's production. True if the
	 * extractor is an instance of the renderer's compatible data formats Note:
	 * DataExtractor must implement a data format in order to be recognized by
	 * DataRenderers.
	 */
	static public boolean matchWith(Object target) {

		return target instanceof DataExtractor && (target instanceof DFListOfXYSeries || target instanceof DFCurves);

	}

	@Override
	public String getName() {
		return Translator.swap("DRGraph.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("DRGraph.description");
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

//		JInternalFrame ifr = Pilot.getInstance().getPositioner().getInternalFrame(this);
//		boolean visible = ifr != null && ifr.isVisible();
//		System.out.println("DRGraph update () ifr: " + (ifr != null) + " visible: " + visible);

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
	 * curves each.
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

			GraphConverter c = null;

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

				c = GraphConverter.convertDFCurves(dataList);

			} else if (representative instanceof DFListOfXYSeries) {
				// Case of DFListOfXYSeries instances
				List<DFListOfXYSeries> dataList = new ArrayList<DFListOfXYSeries>();
				for (Object e : allExtractors) {
					dataList.add((DFListOfXYSeries) e);
				}
				c = GraphConverter.convertDFListOfXYSeries(dataList);
			}

			return createXYChart(c);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DRGraph.createView ()", "Exception", e);

			// fc-23.12.2020
			return new MessagePanel(getName() + " " + Translator.swap("DRGraph.canNotShowTheseDataSeeLog"),
					(MouseListener) this);
//			return new MessagePanel(CapsisExtensionManager.getInstance().getName(this) + " "
//					+ Translator.swap("DRGraph.canNotShowTheseDataSeeLog"), (MouseListener) this);

		}

	}

	/**
	 * Returns a copy of the given shape, scaled with the given factor.
	 */
	protected Shape scaleShape(Shape shape) {

		// Enlarged shape is the normal JFreeChart shape (relatively big)
		if (enlargedMode)
			return shape;

		// Not enlarged: we scale the normal shape to make it smaller
		// warning: too small, we could not distinguish squares, circles,
		// triangles...
		double scale = 0.7;
		AffineTransform t = new AffineTransform();
		t.scale(scale, scale);

		Shape s = t.createTransformedShape(shape);

		return s;
	}

	public ImageIcon makeImageIcon(Shape shape, Color color) {

		// // move the shape in the region of the image
		// gr.translate(-r.x, -r.y);
		// gr.draw(s);
		// gr.dispose();
		// return image;
		//

		// Image image = new BufferedImage(width, height,
		// BufferedImage.TYPE_INT_RGB);

		Rectangle r = shape.getBounds();
		Image image = new BufferedImage(r.width, r.height, BufferedImage.TYPE_INT_RGB);
		// Image image = new BufferedImage(r.width, r.height,
		// BufferedImage.TYPE_BYTE_BINARY);

		Graphics2D g2 = (Graphics2D) image.getGraphics();

		g2.setColor(color);
		g2.draw(shape);

		ImageIcon icon = new ImageIcon();
		icon.setImage(image);

		return icon;
	}

	/**
	 * Creates a XYSeriesCollection chart: Xs are numbers.
	 */
	protected JComponent createXYChart(GraphConverter c) {

		// Use a simple theme
		StandardChartTheme theme = (StandardChartTheme) StandardChartTheme.createLegacyTheme();
		ChartFactory.setChartTheme(theme);

		// Chart title -> leave blank, already written in frame title bar
		String title = "";
		// String title = c.getTitle();

		// fc-12.2.2021 The autoRange problem appeared in jfreechart-1.0.13 without
		// error. Switching to jfreechart-1.0.19 (2014) sent an error telling all the
		// series names were the same. Added code in GraphConverter and
		// CategoryConverter which deal with DFCurves, added (1), (2)... to the series
		// names if needed, fixed the problem.
		// DataExtractors sending non null and different labels for their data series
		// are not impacted

//		// fc-10.2.2021 Checking an auto range problem
//		System.out.println("DRGraph > createXYChart() > domain lower bound: "
//				+ c.getXYSeriesCollection().getDomainLowerBound(true));
//		System.out.println("DRGraph > createXYChart() > domain upper bound: "
//				+ c.getXYSeriesCollection().getDomainUpperBound(true));
//		System.out.println("DRGraph > createXYChart() > range lower bound:  "
//				+ c.getXYSeriesCollection().getRangeLowerBound(true));
//		System.out.println("DRGraph > createXYChart() > range upper bound:  "
//				+ c.getXYSeriesCollection().getRangeUpperBound(true));

		JFreeChart chart = ChartFactory.createXYLineChart(title, c.getXAxisName(), // domain
																					// axis
																					// label
				c.getYAxisName(), // range axis label
				c.getXYSeriesCollection(), // data
				PlotOrientation.VERTICAL, // orientation
				true, // include legend
				true, // tooltips
				false // URLs?
		);

		XYPlot plot = (XYPlot) chart.getPlot();
		
		// fc-10.2.2021 Checking an auto range problem
//		System.out.println("DRGraph > createXYChart () > plot instanceof Zoomable: " + (plot instanceof Zoomable));
//		Zoomable z = (Zoomable) plot;
//		System.out.println("DRGraph > createXYChart () > z.isRangeZoomable(): " + z.isRangeZoomable());
//
//		System.out.println("DRGraph > createXYChart () > plot: " + plot.getClass());
//		System.out.println("DRGraph > createXYChart () > plot.getDomainAxis(): " + plot.getDomainAxis().getClass());
//		System.out.println("DRGraph > createXYChart () > plot.getRangeAxis():  " + plot.getRangeAxis().getClass());
//		System.out.println("DRGraph > createXYChart () > plot.getDomainAxis().isAutoRange(): "
//				+ plot.getDomainAxis().isAutoRange());
//		System.out.println("DRGraph > createXYChart () > plot.getRangeAxis().isAutoRange():   "
//				+ plot.getRangeAxis().isAutoRange());
//
//		System.out.println("DRGraph > createXYChart () > plot contains " + plot.getDatasetCount() + " datasets");
//
//		System.out.println();
//		System.out.println("DRGraph > createXYChart () > RangeAxis...");
//
//		NumberAxis axis = (NumberAxis) plot.getRangeAxis();
//		ValueAxisPlot vap = (ValueAxisPlot) plot;
//		Range r = vap.getDataRange(axis);
//		System.out.println("DRGraph > createXYChart () > plot.getDataRange(axis): " + r);
//
//		int domainIndex = plot.getDomainAxisIndex(axis);
//		int rangeIndex = plot.getRangeAxisIndex(axis);
//		System.out.println("DRGraph > createXYChart () > plot.getDomainAxisIndex(axis): " + domainIndex);
//		System.out.println("DRGraph > createXYChart () > plot.getRangeAxisIndex(axis):  " + rangeIndex);
//
//		int axisIndex = 0; // i.e. rangeIndex, checked
//		for (int i = 0; i < plot.getDatasetCount(); i++) {
//
//			ValueAxis ax = plot.getRangeAxisForDataset(i);
//
//			System.out.println("DRGraph > createXYChart () > dataSet " + i + " matches axis: " + ax
//					+ ", with axis index: " + plot.getRangeAxisIndex(ax));
//
//			XYDataset ds = plot.getDataset(i);
//			int sc = ds.getSeriesCount();
//			System.out.println("DRGraph > createXYChart () > dataSet " + i + " contains " + sc + " series");
//			System.out.println("DRGraph > createXYChart () > dataSet " + i + " is instanceof RangeInfo: "
//					+ (ds instanceof RangeInfo));
//
//			// Mediterranea: Introgression graphs: 1 single dataset with 5 series inside, ok
//			// Samsara2: G/species/time: A dataset with 10 series, ok too
//
//			// iterate through the datasets that map to the axis and get the union
//			// of the ranges.
//
//			Range result = null;
//
//			if (ds != null) {
//				XYItemRenderer ren = plot.getRendererForDataset(ds);
//
//				System.out.println("DRGraph > createXYChart () > renderer for the dataset: " + ren);
//
//				if (ren != null) {
//					result = Range.combine(result, ren.findRangeBounds(ds));
//				} else {
//					result = Range.combine(result, DatasetUtilities.findRangeBounds(ds));
//				}
//
//				RangeInfo info = (RangeInfo) ds;
//
//				boolean includeInterval = true;
//				Range range = info.getRangeBounds(includeInterval);
//
//				System.out.println("DRGraph > createXYChart () > range info  with includeInterval: " + includeInterval
//						+ " gives range: " + range);
//
//				includeInterval = false;
//				range = info.getRangeBounds(includeInterval);
//
//				System.out.println("DRGraph > createXYChart () > range info  with includeInterval: " + includeInterval
//						+ " gives range: " + range);
//
//				// --------------- WRONG RESULT here ----------------
//				System.out.println("DRGraph > createXYChart () > renderer range: "
//						+ Range.combine(result, ren.findRangeBounds(ds))); // WRONG, based on series 1 only
//
//				System.out.println("DRGraph > createXYChart () > iterateXYRangeBounds(ds) range: "
//						+ DatasetUtilities.iterateXYRangeBounds(ds));
//
//				System.out.println(
//						"DRGraph > createXYChart () > DatasetUtilities range: " + DatasetUtilities.findRangeBounds(ds)); // CORRECT
//
//			}
//
//			System.out.println("DRGraph > createXYChart () > final range: " + result);
//
//		}
//
//		System.out.println();

		// Add line names at the right side of the lines
		try {
			XYSeriesCollection sc = c.getXYSeriesCollection();

			for (Object o : sc.getSeries()) {
				org.jfree.data.xy.XYSeries xys = (org.jfree.data.xy.XYSeries) o;

				String text = xys.getDescription();
				if (text != null && text.length() > 0) {
					int n = xys.getItemCount();
					double xLast = xys.getX(n - 1).doubleValue();
					double yLast = xys.getY(n - 1).doubleValue();
					XYTextAnnotation ta = new XYTextAnnotation(text, xLast, yLast);
					ta.setTextAnchor(TextAnchor.BOTTOM_CENTER); // or
																// BASELINE_LEFT
					plot.addAnnotation(ta);
				}
			}
		} catch (Exception e) {
			// Annotations should not prevent displaying
			Log.println(Log.WARNING, "DRGraph.createXYChart ()",
					"Trouble while adding annotations in the graph, ignored", e);
		}

		// Tune the renderer
		boolean lines = true;
		boolean shapes = true;
		XYLineAndShapeRenderer renderer = new XYLineAndShapeRenderer(lines, shapes) {

			/**
			 * Shape scaling
			 */
			public Shape getItemShape(int row, int column) {
				Shape sup = super.getItemShape(row, column);
				return scaleShape(sup);
			}

		};

		plot.setRenderer(renderer);

//		// fc-10.2.2021
//		plot.configureRangeAxes();
//		plot.configureDomainAxes();
//		plot.setRangeGridlinesVisible(true);
//		plot.zoom(100);

		// Set colors
		int i = 0;
		for (Color color : c.getSeriesColors()) {
			renderer.setSeriesPaint(i++, color);
		}

		// Theme customization
		chart.setBackgroundPaint(Color.WHITE);
		plot.setBackgroundPaint(Color.WHITE);

		// fc-9.3.2016
		if (chartPanel != null)
			chartPanel.dispose();

		chartPanel = new GraphChartPanel(chart, dataBlock);

		// fc-12.2.2021 Interesting (toString () was completed)
//		System.out.println("DRGraph > createXYChart() GraphConverter.toString (): " + c.toString());

		// fc-9.4.2018
		legendExplorer.update(getTitle(), chartPanel);
		return legendExplorer.getChartWithLegend();

		// return chartPanel;

	}

	/**
	 * In Configurable
	 */
	@Override
	public ConfigurationPanel getConfigPanel(Object parameter) {
		return new DRGraphConfigurationPanel(this);
	}

	/**
	 * In Configurable
	 */
	@Override
	public void applyConfig(ConfigurationPanel panel) {
		super.applyConfig(panel); // DataRenderer configuration

		DRGraphConfigurationPanel p = (DRGraphConfigurationPanel) panel;

		enlargedMode = p.isEnlargedMode();

		// Memo for next time
		Settings.setProperty("drgraph.enlargedMode", enlargedMode);

	}

	/**
	 * In Configurable
	 */
	@Override
	public void postConfig() {
		ExtensionManager.recordSettings(this);
	}

}
