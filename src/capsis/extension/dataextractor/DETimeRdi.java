/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2003  Francois de Coligny
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

package capsis.extension.dataextractor;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.defaulttype.TreeCollection;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.DgProvider;
import capsis.util.methodprovider.NProvider;
import capsis.util.methodprovider.RDIProvider;
import capsis.util.methodprovider.RdiProviderEnhanced.RdiTool;

/**
 * Rdi (relative density index) over time.
 * 
 * @author P. Vallet - January 2003
 */
public class DETimeRdi extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeRdi");
	}

	private Vector labels;
	protected Vector curves;

	private GModel model;

	/**
	 * Constructor.
	 */
	public DETimeRdi() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeRdi(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeRdi.c ()", "Exception during construction", e);
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

			GModel m = (GModel) referent;
			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof RDIProvider))
				return false;

			if (!(mp instanceof NProvider))
				return false;

			if (!(mp instanceof DgProvider))
				return false;

			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof TreeCollection))
				return false;

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeRdi.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return Translator.swap("DETimeRdi.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "P. Vallet";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeRdi.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.1";
	}

	// nb-08.08.2018
	//public static final String VERSION = "1.1";

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
	}

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step. This output computes the Rdi of the stand versus year
	 * from the root Step to this one.
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

		// Retrieve method provider
		model = step.getProject().getModel();
		methodProvider = model.getMethodProvider();

		try {
			int treeNumber = getTreeIds().size();

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector x = new Vector(); // x coordinates (years)
			Vector y1 = new Vector(); // y coordinates (Rdi)

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				GScene stand = s.getScene();
				Collection trees = ((TreeCollection) stand).getTrees();

				int year = stand.getDate();

				x.add(new Integer(year));
				
				// fc-2.6.2020
				double nha = ((NProvider) methodProvider).getN(stand, trees) / (getSceneArea(stand) / 10000d);
//				double nha = ((NProvider) methodProvider).getN(stand, trees) / (stand.getArea() / 10000d);
				
				double dg = ((DgProvider) methodProvider).getDg(stand, trees);

				double rdi = 0;
				// todo: choose a species instead of null - fc - 15.2.2006
				rdi = RdiTool.getRdiDependingOnMethodProviderInstance(stand, model, nha, dg);
				// if (methodProvider instanceof RdiProviderEnhanced && stand
				// instanceof Speciable) {
				// rdi = ((RdiProviderEnhanced)
				// methodProvider).getRDI((Speciable) stand, nha, dg);
				// } else {
				// rdi = ((RDIProvider) methodProvider).getRDI (model, nha, dg,
				// null);
				// }

				y1.add(new Double(rdi));
			}

			curves.clear();
			curves.add(x);
			curves.add(y1);

			labels.clear();
			labels.add(new Vector()); // no x labels
			Vector y1Labels = new Vector();
			y1Labels.add("rdi");
			labels.add(y1Labels); // y1 : label "Rdi"

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeRdi.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	// fc - 21.4.2004 - DataExtractor.getCaption () is better
	// ~ public String getCaption () {
	// ~ String caption = getStep ().getCaption ();
	// ~ if (treeIds != null && !treeIds.isEmpty ()) {
	// ~ caption += " - "+Translator.swap ("DETimeRdi")
	// ~ +" "+Tools.toString (treeIds);
	// ~ }
	// ~ return caption;
	// ~ }

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
		return labels;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DETimeRdi.xLabel"));
		v.add(Translator.swap("DETimeRdi.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

}
