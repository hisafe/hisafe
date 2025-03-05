package capsis.extension;

import capsis.extension.dataextractor.configuration.DESettings;

/**
 * This interface enables the setting of default parameters that are different from 
 * the default values inherent to CAPSIS. A GModel derived class must implement it. 
 * @author Mathieu Fortin - August 2011
 */
public interface OverridableDataExtractorParameter {
	/**
	 * This method makes it possible to set the default parameters. For instance, the quebecmrnf
	 * modules set the per ha option to true by default.
	 * @param settings a DESettings object from a data extractor
	 */
	public void setDefaultProperties(DESettings settings);
	
}
