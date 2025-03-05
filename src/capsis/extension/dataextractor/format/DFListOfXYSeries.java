package capsis.extension.dataextractor.format;

import java.awt.Color;
import java.util.List;

import capsis.extension.DataFormat;
import jeeb.lib.util.XYSeries;


/**
 * A data format containing a list of XYSeries. Data extractors can implement
 * this type.
 * 
 * @author F. de Coligny - October 2015
 */
public interface DFListOfXYSeries extends DataFormat {

	/**
	 * Returns the list of XYSeries.
	 */
	public List<XYSeries> getListOfXYSeries();

	/**
	 * Returns x and y axes names in this order for a graph where all the
	 * xySeries can be drawn together.
	 */
	public List<String> getAxesNames();

	/**
	 * All extractors must be able to return their name. The caller can try to
	 * translate it with Translator.swap (name) if necessary (ex: gui renderer
	 * translates, file writer does not).
	 */
	public String getName(); // in DataFormat

	/**
	 * All extractors must be able to return their caption. This tells what data
	 * are represented there (from which Step, which tree...).
	 */
	public String getCaption(); // in DataFormat

	/**
	 * All extractors must be able to return their color.
	 */
	public Color getColor(); // in DataFormat

	/**
	 * All extractors must be able to return their default data renderer class
	 * name.
	 */
	public String getDefaultDataRendererClassName(); // in DataFormat

	/**
	 * Returns true is the extractor can work on the current Step (e.g. false if
	 * works on cut trees and no cut trees on this step).
	 */
	public boolean isAvailable(); // in DataFormat

}
