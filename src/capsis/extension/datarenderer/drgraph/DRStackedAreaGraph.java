package capsis.extension.datarenderer.drgraph;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.MouseListener;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.ColoredIcon;
import jeeb.lib.util.Log;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.LegendItem;
import org.jfree.chart.LegendItemCollection;
import org.jfree.chart.StandardChartTheme;
import org.jfree.chart.annotations.XYPointerAnnotation;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.block.BlockBorder;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.StackedXYAreaRenderer2;
import org.jfree.chart.title.LegendTitle;
import org.jfree.data.xy.DefaultTableXYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.ui.HorizontalAlignment;
import org.jfree.ui.RectangleEdge;
import org.jfree.ui.TextAnchor;

import capsis.app.CapsisExtensionManager;
import capsis.extension.PanelDataRenderer;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.extension.dataextractor.format.DFListOfXYSeries;
import capsis.extension.datarenderer.jfreechart.MessagePanel;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;
import capsis.util.configurable.ConfigurationPanel;

/**
 * A data renderer for data extractors implementing DFListOfXYSeries and
 * DFCurves: draws stacked area charts.
 * 
 * @author F. de Coligny - September 2016
 */
public class DRStackedAreaGraph extends PanelDataRenderer {

	static {
		Translator.addBundle("capsis.extension.datarenderer.drgraph.Labels");
	}

	// nb-06.08.2018
	// static final public String NAME = Translator.swap("DRStackedAreaGraph");
	// static final public String VERSION = "1.0";
	// static final public String AUTHOR = "F. de Coligny";
	// static final public String DESCRIPTION =
	// Translator.swap("DRStackedAreaGraph.description");

	protected boolean enlargedMode; // larger points on the graph

	protected GraphChartPanel chartPanel; // must be disposed
	protected DRLegendExplorer legendExplorer; // fc-9.4.2018

	/**
	 * Constructor
	 */
	public DRStackedAreaGraph() {
		super();
	}

	/**
	 * Inits the graph
	 */
	@Override
	public void init(DataBlock db) {
		try {
			super.init(db);

			enlargedMode = Settings.getProperty("DRStackedAreaGraph.enlargedMode", false);

			legendExplorer = new DRLegendExplorer();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DRStackedAreaGraph.init ()", "Error in init (), wrote in Log and passed", e);
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

	// static final public String NAME = Translator.swap("DRStackedAreaGraph");
	// static final public String VERSION = "1.0";
	// static final public String AUTHOR = "F. de Coligny";
	// static final public String DESCRIPTION =
	// Translator.swap("DRStackedAreaGraph.description");

	@Override
	public String getName() {
		return Translator.swap("DRStackedAreaGraph.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("DRStackedAreaGraph.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	/**
	 * Update strategy for DRStackedAreaGraph and its subclasses. This method is
	 * used to refresh the renderer after configuration.
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

			// fc+bc-17.1.2024 Fixing the successive series entries with same x, only for
			// stacked graphs
			c.shiftEntriesHavingSameX();

			// fc+bc-21.9.2016: TableXYConverter has series with unique x
			// values,
			// needed for StackedXYAreaRenderer2
			TableXYConverter tableXYConverter = TableXYConverter.convert(c);

			// System.out.println("DRStackedAreaGraph.createView ()...");
			// System.out.println("GraphConverter ---");
			// System.out.println(c.toString ());
			// System.out.println("TableXYConverter ---");
			// System.out.println(tableXYConverter.toString ());

			return createXYChart(tableXYConverter);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DRStackedAreaGraph.createView ()", "Exception", e);

			// fc-23.12.2020
			return new MessagePanel(getName() + " " + Translator.swap("DRStackedAreaGraph.canNotShowTheseDataSeeLog"),
					(MouseListener) this);
//			return new MessagePanel(CapsisExtensionManager.getInstance().getName(this) + " "
//					+ Translator.swap("DRStackedAreaGraph.canNotShowTheseDataSeeLog"), (MouseListener) this);

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
	protected JComponent createXYChart(TableXYConverter c) {

		// Use a simple theme
		StandardChartTheme theme = (StandardChartTheme) StandardChartTheme.createLegacyTheme();
		ChartFactory.setChartTheme(theme);

		// Chart title -> leave blank, already written in frame title bar
		String title = "";

		// Testing for xy stacked area charts // fc+bc-20.9.2016
		JFreeChart chart = ChartFactory.createXYAreaChart(title, c.getXAxisName(), // domain
				// axis
				// label
				c.getYAxisName() + " (" + Translator.swap("DRStackedAreaGraph.cumulativeAreas") + ")", // range
																										// axis
																										// label
				c.getTableXYDataset(), // data
				PlotOrientation.VERTICAL, // orientation
				true, // include legend
				true, // tooltips
				false // URLs?
		);

		XYPlot plot = (XYPlot) chart.getPlot();

		// fc-16.10.2015 Log axes should be checked with Robert Schneider: not
		// activated
		// // Log axis test
		// LogAxis xAxis = new LogAxis(c.getXAxisName());
		// plot.setDomainAxis(xAxis);
		// LogAxis yAxis = new LogAxis(c.getYAxisName());
		// plot.setRangeAxis(yAxis);
		// // make sure the current theme is applied to the axes just added
		// // ChartUtilities.applyCurrentTheme(chart);
		// // Log axis test

		// Set legend above the graph

		// Organise big legend
		LegendItemCollection lic = plot.getLegendItems();
		int ITEM_MAX = 10;

		// fc+bc-24.1.2018 Make a map to help for annotation below
		Map<Integer, String> seriesNamesMap = new HashMap<>();
//		System.out.println("DRStackedAreaGraph c.getSourceNumber(1): "+c.getSourceNumber());
		for (int i = 0; i < c.getTableXYDataset().getSeriesCount(); i++) {

			XYSeries xys = c.getTableXYDataset().getSeries(i);

			String seriesName = "" + xys.getKey();
			seriesNamesMap.put(i, seriesName);
		}

//		// If legend size is small, complete legend
//		if (lic.getItemCount() <= ITEM_MAX) {
//			LegendTitle legend = chart.getLegend();
//			legend.setPosition(RectangleEdge.TOP);
//			legend.setFrame(BlockBorder.NONE);
//			legend.setHorizontalAlignment(HorizontalAlignment.RIGHT);
//
//		} else {
//				
//			// fc-24.1.2018 This code fails to make a legend  getSourceNumber () seems to be always equal to 1...
//			
//			// if legend is too big, extract one title per source
//			LegendTitle legend = chart.getLegend();
//			legend.setVisible(false);
//			
//			// Create a shorter legend
//			JPanel newLegend = new JPanel(new FlowLayout(FlowLayout.RIGHT));
//			newLegend.setBackground(Color.WHITE);
//			add(newLegend, BorderLayout.NORTH);
//
//			int k = 1;
//
//			System.out.println("DRStackedAreaGraph c.getSourceNumber(2): "+c.getSourceNumber());
//			for (int i = 0; i < c.getSourceNumber(); i++) {
//				// Get the first legend of each source
//				// Each should have a different source color
//				int sourceHead = c.getSourceHeads().get(i);
//				LegendItem li = lic.get(sourceHead);
//
//				JLabel l = new JLabel("" + li.getSeriesKey());
//				Color co = c.getSeriesColors().get(sourceHead);
//				l.setIcon(new ColoredIcon(co, 8, 8));
//				newLegend.add(l);
//
//				if (k++ >= ITEM_MAX)
//					break;
//			}
//		
//		}

		// Add line names at the right side of the lines
		try {
			DefaultTableXYDataset sc = c.getTableXYDataset();

			// related to annotations when two y are found for the same
			// x
			double y0sum = 0;
			double y1sum = 0;

			for (int i = 0; i < sc.getSeriesCount(); i++) {

				// Consider this xy series
				XYSeries xys = sc.getSeries(i);

				// Description of the series
				String text = xys.getDescription();

				// Manage text annotations at the right of the line
				if (text != null && text.length() > 0) {
					int n = xys.getItemCount();
					double xLast = xys.getX(n - 1).doubleValue();
					double yLast = xys.getY(n - 1).doubleValue();
					XYTextAnnotation ta = new XYTextAnnotation(text, xLast, yLast);
					ta.setTextAnchor(TextAnchor.BOTTOM_CENTER); // or
																// BASELINE_LEFT
					plot.addAnnotation(ta);

				}

				// fc+bc-24.1.2018
				// Manage warning annotations when two y are found for the same
				// x for min x and max x: these initial or final changes can not
				// be rendered by the cumulated area renderer. Try to make them
				// visible to enable user to switch renderers to see the detail
				int n = xys.getItemCount();

				double EPSILON = 1E-5;

				// check at minX
				double x0a = xys.getX(0).doubleValue();
				double x0b = xys.getX(1).doubleValue();

				double y0a = xys.getY(0).doubleValue();
				double y0b = xys.getY(1).doubleValue();

//				System.out.println("DRStackedAreaGraph x0a: "+x0a);
//				System.out.println("DRStackedAreaGraph x0b: "+x0b);

				// We need to cumulate ys to have the good cumulated y for the annotations
				double y0forCumul = y0a;

				// Same minX and different y values ?
				if (Math.abs(x0a - x0b) < EPSILON && Math.abs(y0a - y0b) > EPSILON) {
					String warning = "hum";

					// Look for the series name
					String name = seriesNamesMap.get(i);
					if (name != null)
						warning = name;

					double yAnnotation = y0sum + y0forCumul;
					XYPointerAnnotation pa = new XYPointerAnnotation(warning, x0a, yAnnotation, 0.785);
					plot.addAnnotation(pa);

					System.out.println("DRStackedAreaGraph detected an annotation to be added at minX and y: "
							+ yAnnotation + " for series: " + warning);
				}

				y0sum += y0forCumul;

				// check at maxX...
				double x1a = xys.getX(n - 2).doubleValue();
				double x1b = xys.getX(n - 1).doubleValue();

				double y1a = xys.getY(n - 2).doubleValue();
				double y1b = xys.getY(n - 1).doubleValue();

//				System.out.println("DRStackedAreaGraph x1a: "+x1a);
//				System.out.println("DRStackedAreaGraph x1b: "+x1b);

				// We need to cumulate ys to have the good cumulated y for the annotations
				double y1forCumul = y1b;

				// Same maxX and different y values ?
				if (Math.abs(x1a - x1b) < EPSILON && Math.abs(y1a - y1b) > EPSILON) {
					String warning = "hum";

					// Look for the series name
					String name = seriesNamesMap.get(i);
					if (name != null)
						warning = name;

					double yAnnotation = y1sum + y1forCumul;
					XYPointerAnnotation pa = new XYPointerAnnotation(warning, x1b, yAnnotation, 0.785);
					plot.addAnnotation(pa);

					System.out.println("DRStackedAreaGraph detected an annotation to be added at maxX and y: "
							+ yAnnotation + " for series: " + warning);
				}

				y1sum += y1forCumul;
				// fc+bc-24.1.2018

			}
		} catch (Exception e) {
			// Annotations should not prevent displaying
			Log.println(Log.WARNING, "DRStackedAreaGraph.createXYChart ()",
					"Trouble while adding annotations in the graph, ignored", e);
		}

		// Tune the renderer
		boolean lines = true;
		boolean shapes = true;

		// XYLineAndShapeRenderer renderer = new XYLineAndShapeRenderer(lines,
		// shapes) {

		// XYAreaRenderer2 renderer = new XYAreaRenderer2() {

		// XYAreaRenderer renderer = new XYAreaRenderer() {

		StackedXYAreaRenderer2 renderer = new StackedXYAreaRenderer2() {

			/**
			 * Shape scaling
			 */
			public Shape getItemShape(int row, int column) {
				Shape sup = super.getItemShape(row, column);
				return scaleShape(sup);
			}

		};

		plot.setRenderer(renderer);

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
		return new DRStackedAreaGraphConfigurationPanel(this);
	}

	/**
	 * In Configurable
	 */
	@Override
	public void applyConfig(ConfigurationPanel panel) {
		super.applyConfig(panel); // DataRenderer configuration

		DRStackedAreaGraphConfigurationPanel p = (DRStackedAreaGraphConfigurationPanel) panel;

		enlargedMode = p.isEnlargedMode();

		// Memo for next time
		Settings.setProperty("DRStackedAreaGraph.enlargedMode", enlargedMode);

	}

	/**
	 * In Configurable
	 */
	@Override
	public void postConfig() {
		ExtensionManager.recordSettings(this);
	}

}
