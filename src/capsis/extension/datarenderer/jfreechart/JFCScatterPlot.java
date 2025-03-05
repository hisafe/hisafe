/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2011  F. de Coligny, S. Dufour-Kowalski
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE. See the GNU Lesser General Public 
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package capsis.extension.datarenderer.jfreechart;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JComponent;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.StandardChartTheme;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRenderer;

import capsis.app.CapsisExtensionManager;
import capsis.extension.PanelDataRenderer;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;

/**
 * Renders multiple line charts with JFreeChart.
 * 
 * @author S. Dufour-Kowalski - 2010 reviewed by fc 4.2.2011
 */
public class JFCScatterPlot extends PanelDataRenderer {

	static {
		Translator.addBundle("capsis.extension.datarenderer.jfreechart.JFC");
	}

	// nb-06.08.2018
	// static final public String NAME = Translator.swap ("JFCScatterPlot");
	// static final public String VERSION = "1.1";
	// static final public String AUTHOR = "S. Dufour-Kowalski";
	// static final public String DESCRIPTION = Translator.swap
	// ("JFCScatterPlot.description");

//	private Collection<DataExtractor> extractors;

	/**
	 * Inits this renderer on the given DataBlock.
	 */
	@Override
	public void init(DataBlock db) {
		super.init(db);
	}

	/**
	 * Extensions compatibility method. Returns true of this extension can deal with
	 * the given object.
	 */
	static public boolean matchWith(Object target) {
		if (target instanceof DataExtractor && target instanceof DFCurves) {
			return true;
		}
		return false;
	}

	@Override
	public String getName() {
		return Translator.swap("JFCScatterPlot.name");
	}

	@Override
	public String getAuthor() {
		return "S. Dufour-Kowalski";
	}

	@Override
	public String getDescription() {
		return Translator.swap("JFCScatterPlot.description");
	}

	@Override
	public String getVersion() {
		return "1.1";
	}

	/**
	 * Update strategy for JFCScatterPlot and its subclasses. This method is used to
	 * refresh the renderer after configuration.
	 */
	public void update() {
		super.update();
		removeAll();
		add(createView(), BorderLayout.CENTER);
		revalidate();
	}

	/**
	 * Creates the complex line chart component to be displayed.
	 */
	protected JComponent createView() {
		try {
			// One single chart with maybe several curves inside
			List<DFCurves> dfcurves = new ArrayList<DFCurves>();
			for (DataExtractor e : dataBlock.getDataExtractors()) {
				// We can cast because the test was done in matchWith ()
				dfcurves.add((DFCurves) e);
			}

			XYDatasetConverter dsc = new XYDatasetConverter(dfcurves);

			System.out.println("JFCScatterPlot datasetConverter:\n" + dsc);

			if (dsc.isXsWithLabels() || !dsc.isXYSeriesCollection()) {

				// fc-23.12.2020
				return new MessagePanel(getName() + " " + Translator.swap("JFCScatterPlot.canNotShowTheseData"),
						(MouseListener) this);
//				return new MessagePanel (
//						CapsisExtensionManager.getInstance().getName(this)
//						+" "+Translator.swap ("JFCScatterPlot.canNotShowTheseData"), (MouseListener) this);

			}

			return createXYChart(dsc);

		} catch (Exception e) {
			Log.println(Log.ERROR, "JFCScatterPlot.createView ()", "Exception", e);

			// fc-23.12.2020
			return new MessagePanel(getName() + " " + Translator.swap("JFCScatterPlot.canNotShowTheseDataSeeLog"),
					(MouseListener) this);
//			Log.println(Log.ERROR, "JFCScatterPlot.createView ()", "Exception", e);
//			return new MessagePanel(CapsisExtensionManager.getInstance().getName(this) + " "
//					+ Translator.swap("JFCScatterPlot.canNotShowTheseDataSeeLog"), (MouseListener) this);

		}
	}

	/**
	 * Creates a XYSeriesCollection chart: Xs are numbers.
	 */
	protected JComponent createXYChart(XYDatasetConverter dsc) {

		// Use a simple theme
		StandardChartTheme theme = (StandardChartTheme) StandardChartTheme.createLegacyTheme();
		ChartFactory.setChartTheme(theme);
//		BarRenderer.setDefaultShadowsVisible(false);  // removes the bars shadows

		JFreeChart chart = ChartFactory.createScatterPlot(dsc.getTitle(), // chart title -> leave blank, written in
																			// frame title bar
				dsc.getXAxisName(), // domain axis label
				dsc.getYAxisName(), // range axis label
				dsc.getXyDataset(), // data
				PlotOrientation.VERTICAL, // orientation
				true, // include legend
				true, // tooltips
				false // URLs?
		);

		XYPlot plot = (XYPlot) chart.getPlot();
		XYItemRenderer renderer = plot.getRenderer();

		// Set colors
		int i = 0;
		for (Color c : dsc.getYColors()) {
			renderer.setSeriesPaint(i++, c);
		}

//		// Set color
//		int i = 0;
//		for(DataExtractor e : extractors) {
//			DFCurves extr = (DFCurves) de;				
//			List<List<? extends Number>> curves = extr.getCurves ();
//			int nbCurves = curves.size ();	
//			
//			for(int j=1; j<nbCurves; j++) {
//				renderer.setSeriesPaint(i++, e.getColor());
//			}
//		}

		// Theme customization
		chart.setBackgroundPaint(Color.WHITE);
		plot.setBackgroundPaint(Color.WHITE);

		ChartPanel chartPanel = new JFCPanel(chart, dataBlock);
		return chartPanel;

	}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

	@Override
	public String getTitle() {
		return dataBlock.getName();
	}

//---------------------- nothing below this line

//	/** Creates a CategoryDataset chart: Xs are categories.
//	 */
//	protected JComponent createCategoryChart (XYDatasetConverter dsc) {
//
//		// Use a simple theme
//		StandardChartTheme theme = (StandardChartTheme) StandardChartTheme.createLegacyTheme ();
//		ChartFactory.setChartTheme (theme);
////		BarRenderer.setDefaultShadowsVisible(false);  // removes the bars shadows
//		
//		JFreeChart chart = ChartFactory.createLineChart(
//				"",					// chart title -> leave blank, written in frame title bar
//				dsc.getXAxisName(),	// domain axis label
//				dsc.getYAxisName(),	// range axis label
//				dsc.getCategoryDataset (),	// data
//				PlotOrientation.VERTICAL,	// orientation
//				true,				// include legend
//				true,				// tooltips
//				false				// URLs?
//		);
//		
//		CategoryPlot plot = (CategoryPlot) chart.getPlot();
//		CategoryItemRenderer renderer = plot.getRenderer();
//		
//		// Set colors
//		int i = 0;
//		for (Color c : dsc.getYColors ()) {
//			renderer.setSeriesPaint (i++, c);
//		}
//		
//		// Theme customization
//		chart.setBackgroundPaint (Color.WHITE);
//		plot.setBackgroundPaint (Color.WHITE);
//
//		ChartPanel chartPanel = new JFCPanel (chart, dataBlock);
//		return chartPanel;
//
//	}

//	/**	Creates the complex line chart component to be displayed.
//	 */
//	protected JComponent createView () {
//		try {
//			// One single chart with maybe several curves inside
//			JComponent chart;
//			
//			extractors = dataBlock.getDataExtractors ();
//			DataExtractor de = extractors.iterator ().next ();
//			DFCurves dc = (DFCurves) de;
//			if (dc.getLabels () == null || dc.getLabels().isEmpty ()) {
//				chart = createXYChart (extractors);
//			} else {
//				chart = createCategoryChart (extractors);	
//			}
//			
//			return chart;
//
//		} catch (Exception e) {
//			Log.println (Log.ERROR, "JFCScatterPlot.createView ()", "Exception", e);
//			return new JPanel ();
//		}
//	}

//	/** Creates a CategoryDataset chart: Xs are categories.
//	 */
//	protected JComponent createCategoryChart (Collection<DataExtractor> extractors) {
//
//		CategoryDataset dataset = JFCBarChart.createCategoryDataSet (extractors);
//		
//		DataExtractor de = extractors.iterator().next();
//		List<String> axesNames = ((DFCurves)de).getAxesNames ();
//		
//		//String title = de.getName();
//		String title = "";
//		String yTitle = (String) axesNames.get (1);
//		String xTitle = (String) axesNames.get (0);
//		
//		// Use a simple theme
//		StandardChartTheme theme = (StandardChartTheme) StandardChartTheme.createLegacyTheme ();
//		ChartFactory.setChartTheme (theme);
////		BarRenderer.setDefaultShadowsVisible(false);  // removes the bars shadows
//		
//		JFreeChart chart = ChartFactory.createLineChart(
//				title,         // chart title
//				xTitle,               // domain axis label
//				yTitle,                  // range axis label
//				dataset,                  // data
//				PlotOrientation.VERTICAL, // orientation
//				true,                     // include legend
//				true,                     // tooltips
//				false                     // URLs?
//		);
//		
//		CategoryPlot plot = (CategoryPlot) chart.getPlot();
//		CategoryItemRenderer renderer = plot.getRenderer();
//		
//		// Set color
//		int i = 0;
//		for(DataExtractor e : extractors) {
//			renderer.setSeriesPaint(i++, e.getColor());
//		}
//		
//		// Theme customization
//		chart.setBackgroundPaint (Color.WHITE);
//		plot.setBackgroundPaint (Color.WHITE);
//
//		ChartPanel chartPanel = new JFCPanel(chart, dataBlock);
//		return chartPanel;
//
//	}

//	/** Creates a XYSeriesCollection chart: Xs are numbers.
//	 */
//	protected JComponent createXYChart (Collection<DataExtractor> extractors) {
//
//		XYSeriesCollection dataset = new XYSeriesCollection();
//		
//		
//		// for each extractor 
//		for(DataExtractor de : extractors) {
//			// number of curves
//			int nbCurves = 0;
//			int nbPoints = 0;
//	
//			DFCurves extr = (DFCurves) de;				
//			List<List<? extends Number>> curves = extr.getCurves ();
//			nbCurves = curves.size ();		
//			nbPoints = curves.get(0).size();
//		
//			
//				
//			//curves
//			for(int j=1; j<nbCurves; j++) {
//				XYSeries s = new XYSeries(de.getCaption() + "_" + j);
//				for(int i=0; i<nbPoints; i++) {
//					Number n = curves.get(j).get(i);
//					Number x = curves.get(0).get(i);
//					s.add(x, n);
//				}
//				dataset.addSeries(s);
//			}
//			
//		}
//		
//		DataExtractor de = extractors.iterator().next();
//		List<String> axesNames = ((DFCurves)de).getAxesNames ();
//		
//		//String title = de.getName();
//		String title = "";
//		String yTitle = (String) axesNames.get (1);
//		String xTitle = (String) axesNames.get (0);
//		
//		// Use a simple theme
//		StandardChartTheme theme = (StandardChartTheme) StandardChartTheme.createLegacyTheme ();
//		ChartFactory.setChartTheme (theme);
////		BarRenderer.setDefaultShadowsVisible(false);  // removes the bars shadows
//		
//		JFreeChart chart = ChartFactory.createXYLineChart(
//				title,						// chart title
//				xTitle,						// domain axis label
//				yTitle,						// range axis label
//				dataset,					// data
//				PlotOrientation.VERTICAL,	// orientation
//				true,						// include legend
//				true,						// tooltips
//				false						// URLs?
//		);
//		
//		XYPlot plot = (XYPlot) chart.getPlot();
//		XYItemRenderer renderer = plot.getRenderer();
//		
//		// Set color
//		int i = 0;
//		for(DataExtractor e : extractors) {
//			DFCurves extr = (DFCurves) de;				
//			List<List<? extends Number>> curves = extr.getCurves ();
//			int nbCurves = curves.size ();	
//			
//			for(int j=1; j<nbCurves; j++) {
//				renderer.setSeriesPaint(i++, e.getColor());
//			}
//		}
//
//		// Theme customization
//		chart.setBackgroundPaint (Color.WHITE);
//		plot.setBackgroundPaint (Color.WHITE);
//
//		ChartPanel chartPanel = new JFCPanel (chart, dataBlock);
//		return chartPanel;
//
//	}

}
