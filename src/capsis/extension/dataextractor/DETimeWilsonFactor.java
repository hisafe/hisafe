package capsis.extension.dataextractor;

import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.WilsonFactorInterface;

/**
 * Evolution of the Wilson factor over time.
 * 
 * @author F. de Coligny, T. Fonseca - May 2011
 */
public class DETimeWilsonFactor extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeWilsonFactor");
	}

	protected Vector curves;

	/**
	 * Constructor.
	 */
	public DETimeWilsonFactor() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeWilsonFactor(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeWilsonFactor.c ()", "Exception during construction", e);
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) 
				return false;
			
			MethodProvider mp = ((GModel) referent).getMethodProvider();
			return mp instanceof WilsonFactorInterface;

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeWilsonFactor.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeWilsonFactor.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "F. de Coligny, T. Fonseca";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeWilsonFactor.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.0";
	}

	// nb-09.08.2018
	//public static final String VERSION = "1.0";

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		// ~ addConfigProperty (DataExtractor.HECTARE);
		// ~ addConfigProperty (DataExtractor.TREE_GROUP); // group
		// multiconfiguration
		// ~ addConfigProperty (DataExtractor.I_TREE_GROUP); // group individual
		// configuration
	}

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step. This output computes the basal area of the stand versus
	 * date from the root Step to this one.
	 * 
	 * Return false if trouble while extracting.
	 */
	public boolean doExtraction() {
		if (upToDate) {
			return true;
		}
		if (step == null) {
			return false;
		}

		try {

			GModel model = (GModel) step.getProject().getModel();
			WilsonFactorInterface wfInterface = (WilsonFactorInterface) model.getMethodProvider();

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates

			// Data extraction : points with (Double, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				GScene stand = s.getScene();

				double age = stand.getDate();
				double sdi = wfInterface.getWf(stand);

				c1.add(new Double(age));
				c2.add(new Double(sdi));
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeWilsonFactor.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<? extends Number>> getCurves() {
		return curves;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<String>> getLabels() {
		return null; // optional : unused
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DETimeWilsonFactor.xLabel"));
		v.add(Translator.swap("DETimeWilsonFactor.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return 1; // Only one curve
	}

}
