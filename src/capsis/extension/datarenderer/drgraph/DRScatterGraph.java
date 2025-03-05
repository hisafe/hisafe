package capsis.extension.datarenderer.drgraph;

import java.awt.Color;
import java.awt.Shape;

import javax.swing.JComponent;

import jeeb.lib.util.Translator;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.StandardChartTheme;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;

import capsis.extension.dataextractor.format.DFCurves;
import capsis.extension.dataextractor.format.DFListOfXYSeries;
import capsis.extensiontype.DataExtractor;

/**
 * A data renderer for data extractors implementing DFListOfXYSeries and
 * DFCurves : draws scatter graphs. Version 3, replaces DRScatterPlot (original
 * 2003).
 * 
 * @author F. de Coligny - October 2015
 */
public class DRScatterGraph extends DRGraph {

	static { // fc-14.12.2016 Was missing
		Translator.addBundle("capsis.extension.datarenderer.drgraph.Labels");
	}

	// nb-06.08.2018
	//static final public String NAME = Translator.swap("DRScatterGraph");
	//static final public String VERSION = "3.0";
	//static final public String AUTHOR = "F. de Coligny";
	//static final public String DESCRIPTION = Translator.swap("DRScatterGraph.description");

	/**
	 * Tells if the renderer can show an extractor's production. True if the
	 * extractor is an instance of the renderer's compatible data formats Note:
	 * DataExtractor must implement a data format in order to be recognized by
	 * DataRenderers.
	 */
	static public boolean matchWith(Object target) { // fc-14.12.2016 Was missing

		return target instanceof DataExtractor && (target instanceof DFListOfXYSeries || target instanceof DFCurves);

	}

	@Override
	public String getName() {
		return Translator.swap("DRScatterGraph.name");
	}
	
	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}
	
	@Override
	public String getDescription() {
		return Translator.swap("DRScatterGraph.description");
	}
	
	@Override
	public String getVersion() {
		return "3.0";
	}
	
	/**
	 * Creates a XYSeriesCollection chart.
	 */
	protected JComponent createXYChart(GraphConverter c) {

		// Use a simple theme
		StandardChartTheme theme = (StandardChartTheme) StandardChartTheme.createLegacyTheme();
		ChartFactory.setChartTheme(theme);

		// Chart title -> leave blank, already written in frame title bar
		String title = "";
		// String title = c.getTitle();

//		JFreeChart chart = ChartFactory.createScatterPlot(
		JFreeChart chart = ChartFactory.createXYLineChart( // fc-7.1.2016 missing series with ScatterPlot...
				title, 
				c.getXAxisName(),// domain axis label
				c.getYAxisName(), // range axis label
				c.getXYSeriesCollection(), // data
				PlotOrientation.VERTICAL, // orientation
				true, // include legend
				true, // tooltips
				false // URLs?
				);

//		// Set legend above the graph
//		LegendTitle legend = chart.getLegend();
//		legend.setPosition(RectangleEdge.TOP);
//		legend.setFrame(BlockBorder.NONE);
//		legend.setHorizontalAlignment(HorizontalAlignment.RIGHT); // bugs when
//																	// too
//																	// long...

		XYPlot plot = (XYPlot) chart.getPlot();
		
		// Tune the renderer
		boolean lines = false;
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

		// Set colors
		int i = 0;
		for (Color color : c.getSeriesColors()) {
			renderer.setSeriesPaint(i++, color);
		}

		// Theme customization
		chart.setBackgroundPaint(Color.WHITE);
		plot.setBackgroundPaint(Color.WHITE);

		ChartPanel chartPanel = new GraphChartPanel(chart, dataBlock);

		// fc-9.4.2018
		legendExplorer.update (getTitle (), chartPanel);
		return legendExplorer.getChartWithLegend ();
		
//		return chartPanel;
	}

}
