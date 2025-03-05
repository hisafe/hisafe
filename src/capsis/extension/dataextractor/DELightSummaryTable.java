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

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import org.apache.commons.math.stat.descriptive.DescriptiveStatistics;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.defaulttype.plotofcells.RectangularPlot;
import capsis.defaulttype.plotofcells.SquareCell;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFTables;
import capsis.kernel.GModel;
import capsis.kernel.MethodProvider;
import capsis.kernel.extensiontype.GenericExtensionStarter;


/**
 * A summary light table compatible with instance of samsaralight
 * 
 * @author G. Ligot - February 2012
 */
public class DELightSummaryTable extends PaleoDataExtractor implements DFTables {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DELightSummaryTable");
	}

	// nb-07.08.2018
	//public static final String AUTHOR = "G. Ligot";
	//public static final String VERSION = "1.0";

	protected Collection<String[][]> tables;
	protected Collection<String> titles;

	protected MethodProvider methodProvider;
	private GenericExtensionStarter starter;

	protected NumberFormat f;

	/**
	 * Constructor.
	 */
	public DELightSummaryTable() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DELightSummaryTable(GenericExtensionStarter s) {

		this.starter = s;

		try {
			tables = new ArrayList();
			titles = new ArrayList();

			// Used to format decimal part with 2 digits only
			f = NumberFormat.getInstance(Locale.ENGLISH);
			f.setGroupingUsed(false);
			f.setMaximumFractionDigits(1);
			f.setMinimumFractionDigits(1);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DELightSummaryTable.c ()", "Exception during construction : ", e);
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) {
				return false;
			} // should be changed
			GModel m = (GModel) referent;


		} catch (Exception e) {
			Log.println(Log.ERROR, "DELightSummaryTable.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("DELightSummaryTable.name");
	}
	
	@Override
	public String getAuthor() {
		return "G. Ligot";
	}
	
	@Override
	public String getDescription() {
		return Translator.swap("DELightSummaryTable.description");
	}
	
	@Override
	public String getVersion() {
		return "1.0";
	}

	// nb-15.01.2019
	@Override
	public String getDefaultDataRendererClassName() {
		return "capsis.extension.datarenderer.DRTables";
	}

	/**
	 * This method is called by superclass DataExtractor.
	 */
	@Override
	public void setConfigProperties() {

		// observation
		String[] tab = { "T_cell", "T_sensor" };
		addRadioProperty(tab);

		// statistics
		addBooleanProperty("S_mean", true);
		addBooleanProperty("S_min", true);
		addBooleanProperty("S_max", true);
		addBooleanProperty("S_stddev", true);

		// Variable
		addBooleanProperty("V_PACLTOT", true);
		addBooleanProperty("V_PACLDIF", true);
		addBooleanProperty("V_PACLDIR", true);

		addBooleanProperty("V_IRRADIANCETOT", true);
		addBooleanProperty("V_IRRADIANCEDIF", true);
		addBooleanProperty("V_IRRADIANCEDIR", true);

		addBooleanProperty("V_IRRADIANCETOTHORIZ", true);
		addBooleanProperty("V_IRRADIANCEDIFHORIZ", true);
		addBooleanProperty("V_IRRADIANCEDIRHORIZ", true);

		addBooleanProperty("V_IRRADIANCEABOVECANOPY", true);

	}

	/**
	 * From DataExtractor Interface. Effectively process the extraction. Should
	 * only be called if needed (time consuming). One solution is to trigger
	 * real extraction by data renderer paintComponent (). So, the work will
	 * only be done when needed. Return false if trouble.
	 */
	public boolean doExtraction() {


		return true;

	}

	/**
	 * From DFTables interface.
	 */
	public Collection getTables() {
		return tables;
	}

	public Collection getTitles() {
		return titles;
	}

}
