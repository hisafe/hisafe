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

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Random;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.statistics.HistogramDataset;
import org.jfree.data.statistics.HistogramType;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

import capsis.extension.dataextractor.format.DFCurves;

/**
 * A tool to convert Capsis datasets formats into JFreeChart XYSeriesCollection
 * datasets.
 * 
 * @author F. de Coligny - february 2011
 */
public class XYDatasetConverter extends DatasetConverter {

	// The resulting dataset
	private XYSeriesCollection xyDataset;

	/**
	 * Constructor. All instances in the given list must have the same type
	 * (same class).
	 */
	public XYDatasetConverter(List<? extends DFCurves> listOfDfcurves) {
		super(listOfDfcurves);

		// Create the dataset
		createXYSeriesCollection();

	}

	/**
	 * Create a xy series dataset
	 */
	private void createXYSeriesCollection() {
		System.out.println("XYDatasetConverter createXYSeriesCollection...");

		xyDataset = new XYSeriesCollection();

		// For each DFCurves
		for (DFCurves dfc : listOfDfcurves) {

			List<List<? extends Number>> curves = dfc.getCurves();

			// Curves
			for (int j = 0; j < nbCurves; j++) {
				int k = j + 1;

				String caption = annotatedCurves ? dfc.getCaption() + " " + yAnnotations.get(j) : dfc.getCaption()
						+ " " + k;

				XYSeries s = new XYSeries(caption.toString());

				int n = curves.get(k).size();
				for (int i = 0; i < n; i++) {
					Number y = curves.get(k).get(i);
					Number x = curves.get(0).get(i);
					s.add(x, y);
				}
				xyDataset.addSeries(s);
			}

		}
	}

	// Accessors
	public boolean isXYSeriesCollection() {
		return xyDataset != null;
	}

	public XYSeriesCollection getXyDataset() {
		return xyDataset;
	}

	public String toString() {
		return "DataSetConverter" + " title " + getTitle() + " xAxisName " + getXAxisName() + " yAxisName "
				+ getYAxisName() + " isXYSeriesCollection " + isXYSeriesCollection();
	}

	// Testing HistogramDataset.
	// java -cp .:../ext/*
	// capsis.extension.datarenderer.jfreechart.XYDatasetConverter
	//
	public static void main(String[] args) {
		double[] value = new double[100];
		Random generator = new Random();
		for (int i = 1; i < 100; i++) {
			value[i] = generator.nextDouble();
			int number = 10;
			HistogramDataset dataset = new HistogramDataset();
			dataset.setType(HistogramType.RELATIVE_FREQUENCY);
			dataset.addSeries("Histogram", value, number);
			String plotTitle = "Histogram";
			String xaxis = "number";
			String yaxis = "value";
			PlotOrientation orientation = PlotOrientation.VERTICAL;
			boolean show = false;
			boolean toolTips = false;
			boolean urls = false;
			JFreeChart chart = ChartFactory.createHistogram(plotTitle, xaxis, yaxis, dataset, orientation, show,
					toolTips, urls);
			int width = 500;
			int height = 300;
			try {
				ChartUtilities.saveChartAsPNG(new File("/home/coligny/tmp/histogram.PNG"), chart, width, height);
			} catch (IOException e) {
			}
		}
	}

}
