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
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Vector;

import jeeb.lib.util.Check;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFTables;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.group.GrouperManager;

/**
 * Econ data report.
 * 
 * @author C. Orazio - December 2005
 */
public class DEEconTable extends PaleoDataExtractor implements DFTables {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DEEconTable");
	}

	public static final int MAX_FRACTION_DIGITS = 2;

	private static class EconLine implements Comparable {

		public String startingDate;
		public String endingDate;
		public String label;
		public String income;
		public String expense;
		public String balance;
		public String actualizedBalance;

		public EconLine() {
		}

		public void validate() {
			if (startingDate == null)
				startingDate = "";
			if (endingDate == null)
				endingDate = "";
			if (label == null)
				label = "";
			if (income == null)
				income = "";
			if (expense == null)
				expense = "";
			if (balance == null)
				balance = "";
			if (actualizedBalance == null)
				actualizedBalance = "";
		}

		/**
		 * A method to compare this EconLine with another. If the Econ lines are
		 * in a TreeSet, they will be sorted.
		 */
		@Override
		public int compareTo(Object o) {
			if (!(o instanceof EconLine))
				return -1;
			EconLine other = (EconLine) o;
			try {
				int a = Check.intValue(startingDate);
				int b = Check.intValue(other.startingDate);

				return a - b;
			} catch (Exception e) {
				return 1; // if text in col 0, we arrive here: at the top
			}
		}

		public String toString() {
			return startingDate + ", " + endingDate + ", " + label + ", " + income + ", " + expense + ", " + balance
					+ ", " + actualizedBalance;
		}

	}

	protected Collection tables;
	protected Collection titles;
	protected MethodProvider methodProvider;

	protected NumberFormat formater;
	protected NumberFormat formater3;

	/**
	 * Constructor.
	 */
	public DEEconTable() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DEEconTable(GenericExtensionStarter s) {
		super(s);

		try {
			tables = new ArrayList();
			titles = new ArrayList();
			// Used to format decimal part with 2 digits only

			// add cm + tl 10032005 : US format to export to excel
			formater = NumberFormat.getInstance(Locale.US); // to impose
			// decimal dot
			// instead of
			// "," for
			// french number
			// format
			// formater = NumberFormat.getInstance ();
			formater.setMaximumFractionDigits(MAX_FRACTION_DIGITS);
			formater.setMinimumFractionDigits(MAX_FRACTION_DIGITS);
			formater.setGroupingUsed(false);
			formater3 = NumberFormat.getInstance(Locale.US); // to impose
			// decimal dot
			// instead of
			// "," for
			// french number
			// format
			// formater3 = NumberFormat.getInstance ();
			formater3.setMaximumFractionDigits(3);
			formater3.setMinimumFractionDigits(3);
			formater3.setGroupingUsed(false);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEEconTable.c ()", "Exception during construction : ", e);
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {


		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DEEconTable.name");
	}

	/**
	 * Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "C. Orazio";
	}

	/**
	 * Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DEEconTable.description");
	}

	/**
	 * Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.1.1";
	}

	// nb-08.08.2018
	//public static final String VERSION = "1.1.1";

	// nb-14.01.2019
	@Override
	public String getDefaultDataRendererClassName() {
		return "capsis.extension.datarenderer.DRTables";
	}

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property

		// CHANGED: these options are not any more options (simplifies a lot)
		// addBooleanProperty ("ActualizedBalance", true);
		// addBooleanProperty ("Balance", true);
		// addBooleanProperty ("Expense", true);
		// addBooleanProperty ("Income", true);
		// addBooleanProperty ("Label", true);
		// addBooleanProperty ("EndingDate", true);
	}

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step. This output display in a teble format data hidden in
	 * economic variables date from the root Step to this one.
	 * 
	 * Return false if trouble while extracting.
	 */
	public boolean doExtraction() {
		
		return true;
	}

	/**
	 * This prefix is built depending on current settings. ex: "+ 25 years /ha"
	 */
	public String getNamePrefix() {
		String prefix = "";
		try {
			if (isCommonGrouper() && isGrouperMode()
					&& GrouperManager.getInstance().getGrouperNames().contains(getGrouperName())) {
				prefix += getGrouperName() + " - ";
			}
			if (isSet("perHectare")) {
				prefix += "/ha - ";
			}
		} catch (Exception e) {
		} // if trouble, prefix is empty
		return prefix;
	}

	/**
	 * DFTables interface.
	 */
	public Collection getTables() {
		return tables;
	}

	/**
	 * DFTables interface.
	 */
	public Collection getTitles() {
		return titles;
	}

}
