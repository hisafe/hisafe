package capsis.extension.dataextractor;

import java.util.Collection;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.superclass.DETimeY;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.util.methodprovider.CarbonProvider;

/**
 * This data extractor computes the aboveground carbon (branches + boles) in time.
 * Requires the CarbonProvider interface.
 * 
 * @author P. Vallet - Winter 2003
 * 	modified M. Fortin - August 2010 (extended from DETimeY)
 */
public class DETimeCarbon extends DETimeY {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeCarbon");
	}
	
	// nb-07.08.2018
	//public static final String AUTHOR = "P. Vallet";
	//public static final String VERSION = "2.0";

	/**
	 * Extension dynamic compatibility mechanism. 
	 * This static matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof GModel)) {return false;}
			GModel m = (GModel) referent;
			MethodProvider mp = m.getMethodProvider ();
			if (!(mp instanceof CarbonProvider)) {return false;}

		} catch (Exception e) {
			Log.println (Log.ERROR, "DETimeCarbon.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("DETimeCarbon.name");
	}
	
	@Override
	public String getAuthor() {
		return "P. Vallet";
	}

	@Override
	public String getDescription() {
		return Translator.swap("DETimeCarbon.description");
	}

	@Override
	public String getVersion() {
		return "2.0";
	}
	
	/**
	 * This method is called by superclass DataExtractor.
	 */
	@Override
	public void setConfigProperties () {
		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
		addConfigProperty (PaleoDataExtractor.TREE_GROUP);
	}

	
	@SuppressWarnings("unchecked")
	@Override
	protected Number getValue(GModel m, GScene stand, int date) {
		double areaFactor = 1d;
		if (isSet("perHectare")) {
			areaFactor = 10000d / getSceneArea(stand); // fc-2.6.2020
//			areaFactor = 10000d / stand.getArea();
		}
		Collection trees = doFilter(stand);
		CarbonProvider carbonProvider = (CarbonProvider) m.getMethodProvider();
		return carbonProvider.getCarbonContent(stand, trees) * areaFactor;
	}

}
