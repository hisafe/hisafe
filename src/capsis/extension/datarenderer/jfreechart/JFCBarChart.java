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
import java.awt.event.MouseEvent;
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
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.renderer.category.BarRenderer;

import capsis.app.CapsisExtensionManager;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.extension.datarenderer.AbstractPanelDataRenderer;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;

/**
 * Renders multiple bar charts with JFreeChart.
 * 
 * @author S. Dufour-Kowalski - 2010 reviewed by fc 4.2.2011
 */
public class JFCBarChart extends AbstractPanelDataRenderer implements MouseListener {

	static {
		Translator.addBundle("capsis.extension.datarenderer.jfreechart.JFC");
	}

	// nb-06.08.2018
	// static final public String NAME = Translator.swap ("JFCBarChart");
	// static final public String VERSION = "1.1";
	// static final public String AUTHOR = "S. Dufour-Kowalski";
	// static final public String DESCRIPTION = Translator.swap
	// ("JFCBarChart.description");

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
		return Translator.swap("JFCBarChart.name");
	}

	@Override
	public String getAuthor() {
		return "S. Dufour-Kowalski";
	}

	@Override
	public String getDescription() {
		return Translator.swap("JFCBarChart.description");
	}

	@Override
	public String getVersion() {
		return "1.1";
	}

	/**
	 * Update strategy for JFCBarChart and its subclasses. This method is used to
	 * refresh the renderer after configuration.
	 */
	public void update() {
		super.update();
		removeAll();

//		// fc-30.10.2014 extractors may be unavailable on the step
//		Collection extractors = dataBlock.getDataExtractors ();
//		Collection specialExtractors = dataBlock.getSpecialExtractors ();
//
//		System.out.println("JFCBarChart update...");
//		
//		// fc-30.10.2014 if one of the extractors is not avaialble, send a message
//		List specialAndNormalExtractors = new ArrayList(extractors);
//		if (specialExtractors != null) { // fc - 14.12.2007
//			specialAndNormalExtractors.addAll(specialExtractors);
//		}
//		for (Object e : specialAndNormalExtractors) {
//			if (e instanceof DataFormat) {
//				DataFormat df = (DataFormat) e;
//				if (!df.isAvailable()) {
//					addMessage(Translator.swap ("Shared.notAvailableOnThisStep"));
//					return;
//				}
//			}
//		}
//		// fc-30.10.2014 extractors may be unavailable on the step
//

		add(createView(), BorderLayout.CENTER);
		revalidate();
	}
//
//	// Prints a warning message ("see configuration")
//	//
//	private void addMessage (String message) {
//		LinePanel l1 = new LinePanel ();
//		l1.add (new JLabel (message));
//		l1.addGlue ();
//		l1.setBackground (Color.WHITE);
//		add (l1, BorderLayout.NORTH);
//		revalidate ();
//		repaint ();
//		
//		System.out.println("JFCBarChart wrote message: "+message);
//	}

	/**
	 * Creates the complex chart component to be displayed.
	 */
	protected JComponent createView() {
		try {
			// One single chart with maybe several dfcurves inside
			// (and each dfcurve may contain several curves))
			List<DFCurves> dfcurves = new ArrayList<DFCurves>();
			for (DataExtractor e : dataBlock.getDataExtractors()) {
				// We can cast because the test was done in matchWith ()
				dfcurves.add((DFCurves) e);
			}

			CategoryDatasetConverter dsc = new CategoryDatasetConverter(dfcurves);

			System.out.println("JFCBarChart datasetConverter:\n" + dsc);

			if (dsc.isCategoryDataset()) {
				return createCategoryChart(dsc);
			} else {

				// fc-23.12.2020
				return new MessagePanel(getName() + " " + Translator.swap("JFCBarChart.canNotShowTheseData"),
						(MouseListener) this);
//				return new MessagePanel (
//						CapsisExtensionManager.getInstance().getName(this)
//						+" "+Translator.swap ("JFCBarChart.canNotShowTheseData"), (MouseListener) this);

			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "JFCBarChart.createView ()", "Exception", e);

			// fc-23.12.2020
			return new MessagePanel(getName() + " " + Translator.swap("JFCBarChart.canNotShowTheseDataSeeLog"),
					(MouseListener) this);
//			return new MessagePanel(CapsisExtensionManager.getInstance().getName(this) + " "
//					+ Translator.swap("JFCBarChart.canNotShowTheseDataSeeLog"), (MouseListener) this);

		}
	}

	/**
	 * Creates a XYSeriesCollection chart: Xs are numbers.
	 */
	protected JComponent createCategoryChart(CategoryDatasetConverter dsc) {

		// Use a simple theme
		StandardChartTheme theme = (StandardChartTheme) StandardChartTheme.createLegacyTheme();
		ChartFactory.setChartTheme(theme);
//		BarRenderer.setDefaultShadowsVisible(false);  // removes the bars shadows

		JFreeChart chart = ChartFactory.createBarChart(dsc.getTitle(), // chart title -> leave blank, written in frame
																		// title bar
				dsc.getXAxisName(), // domain axis label
				dsc.getYAxisName(), // range axis label
				dsc.getCategoryDataset(), // data
				PlotOrientation.VERTICAL, // orientation
				true, // include legend
				true, // tooltips
				false // URLs?
		);

		CategoryPlot plot = (CategoryPlot) chart.getPlot();

		BarRenderer renderer = (BarRenderer) plot.getRenderer();

		// Removes the bar shadows
		renderer.setShadowVisible(false);

		// Set colors
		int i = 0;
		for (Color c : dsc.getYColors()) {
			renderer.setSeriesPaint(i++, c);
		}

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

	@Override
	public void mouseClicked(MouseEvent e) {
		// TODO Auto-generated method stub

	}

	@Override
	public void mousePressed(MouseEvent e) {
		// TODO Auto-generated method stub

	}

	@Override
	public void mouseReleased(MouseEvent e) {
		// TODO Auto-generated method stub

	}

	@Override
	public void mouseEntered(MouseEvent e) {
		// TODO Auto-generated method stub

	}

	@Override
	public void mouseExited(MouseEvent e) {
		// TODO Auto-generated method stub

	}

//---------------------- nothing below this line

//	/**	Creates the complex chart component to be displayed.
//	 */
//	protected JComponent createView () {
//		try {
//			
//			extractors = dataBlock.getDataExtractors ();
//			// One single chart with maybe several histograms inside
//			JComponent chart = createCategoryChart (extractors);
//			return chart;	
//
//		} catch (Exception e) {
//			Log.println (Log.ERROR, "JFCBarChart.createView ()", "Exception", e);
//			return new JPanel ();
//		}
//	}

//	/** Creates a CategoryDataset chart: Xs are categories.
//	 */
//	protected JComponent createCategoryChart (Collection<DataExtractor> extractors) {
//
//		CategoryDataset dataset = createCategoryDataSet(extractors);
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
//		JFreeChart chart = ChartFactory. /* createBarChart3D */ createBarChart (
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
//		BarRenderer renderer = (BarRenderer) plot.getRenderer();
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
//		ChartPanel chartPanel = new JFCPanel (chart, dataBlock);
//		return chartPanel;
//
//	}

//	/**	Turns the Capsis extractors into a JFreeChart CategoryDataSet.
//	 */
//	static protected CategoryDataset createCategoryDataSet (Collection<DataExtractor> extractors) {
//		
//		DefaultCategoryDataset dataset = new DefaultCategoryDataset();
//		
//		// for each extractor 
//		for (DataExtractor de : extractors) {
//			// number of curves
//			int nbCurves = 0;
//			int nbPoints = 0;
//	
//			DFCurves extr = (DFCurves) de;				
//			List<List<? extends Number>> curves = extr.getCurves ();
//			nbCurves = curves.size ();		
//			nbPoints = curves.get(0).size();
//		
//			// labels
//			List<List<String>> labels = extr.getLabels ();
//			
//			List<String> xLabels = null;
//			
//			if(labels != null) {
//				xLabels = labels.get(0);
//			} 
//			
//			//curves
//			for(int i=0; i<nbPoints; i++) {
//			
//				for(int j=1; j<nbCurves; j++) {
//					Number n = curves.get(j).get(i);
//					String x;
//					if(xLabels != null && xLabels.size() > i) {
//						x = xLabels.get(i);
//					} else {
//						x = curves.get(0).get(i).toString();
//					}
//					dataset.addValue(n, de.getCaption() + "_" + j, x);
//				}
//			}
//			
//		}
//
//		return dataset;
//	}

}
