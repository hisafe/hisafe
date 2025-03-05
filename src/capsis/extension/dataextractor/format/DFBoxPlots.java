package capsis.extension.dataextractor.format;

import java.awt.Color;
import java.util.List;

import org.jfree.data.statistics.DefaultBoxAndWhiskerCategoryDataset;

import capsis.extension.DataFormat;
import jeeb.lib.util.DoubleKeyMap;

/**
 * A data format containing data for box plots. Data extractors can implement
 * this type.
 * 
 * @see http://www.java2s.com/Code/Java/Chart/JFreeChartBoxAndWhiskerDemo.htm
 * 
 * @author F. de Coligny - July 2021
 */
public interface DFBoxPlots extends DataFormat {

	/**
	 * Returns the box and wisker data set.
	 */
	public DefaultBoxAndWhiskerCategoryDataset getBoxAndWiskerCategoryDataset();

	/**
	 * Returns the dataset containing all the data added in the
	 * DefaultBoxAndWhiskerCategoryDataset. Key 1 is datasetTypeKey (e.g. year for
	 * an evolution), key 2 is datasetSeriesKey, and the list is the list of values
	 * in the distribution matching these two keys. Helps showing the data in
	 * tables.
	 */
	public DoubleKeyMap<String, String, List<Double>> getDatasets();

	/**
	 * Returns x and y axes names in this order.
	 */
	public List<String> getAxesNames();

	/**
	 * All extractors must be able to return their name. The caller can try to
	 * translate it with Translator.swap (name) if necessary (ex: gui renderer
	 * translates, file writer does not).
	 */
	public String getName(); // in DataFormat

	/**
	 * All extractors must be able to return their caption. This tells what data are
	 * represented there (from which Step, which tree...).
	 */
	public String getCaption(); // in DataFormat

	/**
	 * All extractors must be able to return their color.
	 */
	public Color getColor(); // in DataFormat

	/**
	 * All extractors must be able to return their default data renderer class name.
	 */
	public String getDefaultDataRendererClassName(); // in DataFormat

	/**
	 * Returns true is the extractor can work on the current Step (e.g. false if
	 * works on cut trees and no cut trees on this step).
	 */
	public boolean isAvailable(); // in DataFormat

}