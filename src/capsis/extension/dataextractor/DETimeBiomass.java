package capsis.extension.dataextractor;

import java.util.Collection;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.extension.dataextractor.superclass.AbstractDataExtractor;
import capsis.extension.dataextractor.superclass.DETimeY;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.util.methodprovider.BiomassProvider;

/**
 * Biomass versus Time, calls BiomassProvider.
 * 
 * @author P. Vallet
 */
public class DETimeBiomass extends DETimeY {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeBiomass");
	}

	// nb-07.08.2018
	//public static final String AUTHOR = "P. Vallet";
	//public static final String VERSION = "3.0";

	static public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) {
				return false;
			}
			GModel m = (GModel) referent;
			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof BiomassProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeBiomass.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}
	
	@Override
	public String getName() {
		return Translator.swap("DETimeBiomass.name");
	}
	
	@Override
	public String getAuthor() {
		return "P. Vallet";
	}

	@Override
	public String getDescription() {
		return Translator.swap("DETimeBiomass.description");
	}

	@Override
	public String getVersion() {
		return "3.0";
	}
	
	/**
	 * This method is called by superclass DataExtractor.
	 */
	@Override
	public void setConfigProperties() {
		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
		addConfigProperty(AbstractDataExtractor.TREE_GROUP);
	}

	@Override
	protected Number getValue(GModel m, GScene stand, int date) {
		MethodProvider methodProvider = m.getMethodProvider();

		Collection trees = doFilter(stand);
		double biomass = ((BiomassProvider) methodProvider).getBiomass(stand, trees);
		return biomass;
	}

}
