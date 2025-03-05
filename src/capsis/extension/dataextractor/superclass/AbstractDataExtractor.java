/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2021 INRA
 * 
 * Authors: F. de Coligny, N. Beudez, S. Dufour-Kowalski,
 * 
 * This file is part of Capsis Capsis is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the Free Software
 * Foundation, either version 2.1 of the License, or (at your option) any later version.
 * 
 * Capsis is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU lesser General Public License along with Capsis. If
 * not, see <http://www.gnu.org/licenses/>.
 * 
 */

package capsis.extension.dataextractor.superclass;

import java.awt.Color;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import capsis.app.CapsisExtensionManager;
import capsis.commongui.projectmanager.ButtonColorer;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.commongui.projectmanager.RelatedColorProvider;
import capsis.commongui.projectmanager.StepButton;
import capsis.commongui.util.Tools;
import capsis.defaulttype.TreeList;
import capsis.extension.DataFormat;
import capsis.extension.OverridableDataExtractorParameter;
import capsis.extension.dataextractor.configuration.DESettings;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;
import capsis.gui.GrouperChooserListener;
import capsis.gui.StatusChooser;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.SharedConfigurable;
import capsis.util.group.Group;
import capsis.util.group.GroupableType;
import capsis.util.group.Grouper;
import capsis.util.group.GrouperManager;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * Superclass for all Capsis data extractors. Data Extractors are tools that
 * extract data series from a reference step or a list of steps from the
 * simulation root step to the reference step, to be viewed in graphs or tables.
 * 
 * @author F. de Coligny - November 2000
 */
abstract public class AbstractDataExtractor extends ConfigurableExtractor implements /* DataExtractor, */ DataFormat,
		SharedConfigurable, Configurable, GrouperChooserListener, Comparable {

	// fc-25.7.2021 Removed DataExtractor upper, see ConfigurableExtractor
	// fc-25.7.2021 MOVED all configuration stuff to ConfigurableExtractor

	// fc+nb-14.01.2019 No needed anymore. getDefaultDataRendererClassName()
	// returns the data renderer class name's default value and
	// the calculation of defaultDataRendererClassName is moved in
	// capsis.extensiontype.DataBlock.getDefaultDataRendererClassName().
	// Extractors have a default DataRenderer (e.g. a table renderer)
	// protected String defaultDataRendererClassName;

	// The extractor is synchronized on this step (can change)
	protected Step step;

	// fc-29.4.2003 Optional (for PhD & SChalon)
	protected Color forcedColor;

	// fc-5.5.2003 If several extractors of same type on same step (e.g. !=
	// groups), they have a rank
	protected int rank;

	// When false, indicates that current configuration has changed and that
	// extraction will have to be redone before next data renderer
	// paintComponent ();
	// NOTE: To be checked at the beginning of doExtraction (), if true, return
	protected boolean upToDate;

	// E.g. NProvider, VProvider... -> will search pp3.NProvider,
	// mountain.VProvider... and document them in DEPropertyPage
	protected Collection<String> documentationKeys; // fc-29.3.2005

	// This DataBlock contains the extractor and maybe others with same type but
	// maybe synchronized on other steps or with other configurations
	private DataBlock dataBlock;

	/**
	 * Constructor
	 */
	public AbstractDataExtractor() {
		// fc-3.8.2018 added this default constructor
	}

	/**
	 * Returns true if this extractor does not need to be rerun. If configuration or
	 * step changes, upToDate is set to false and doExtraction () must be rerun.
	 */
	public boolean isUpToDate() {
		return upToDate;
	}

	// fc-6.9.2021 Added this to help propagate configuration properties
	public void setUpToDate(boolean upToDate) {
		this.upToDate = upToDate;
	}

	/**
	 * Inits the extractor on the given simulation step.
	 */
	@Override
	public void init(GModel m, Step s) throws Exception {

		// fc-15.9.2021 If already inited on this step, return
		// This could be an important fix: extractors were maybe sometimes inited
		// several times
		if (this.step != null && s.equals(this.step))
			return;

//		System.out.println("AbstractDataExtractor.init() called for " + getClassName() + " INIT ()...");

		try {
			this.step = s;

			upToDate = false;
			rank = 0;

			// A set is fast on contains ()
			documentationKeys = new HashSet<String>(); // fc-29.3.2005

			// fc-21.7.2021 calling a method in ConfigurableExtractor to init
			// comboPropertyTranslationToValue and treeIds
			stepChanged(s);

			// fc-27.8.2021 This does not propagate shared settings in other opened
			// extractors when changed in an extractor and when moving from a step to
			// another
			retrieveSettings(); // fc-14.2.2002 Was lower

		} catch (Exception e) {

			// fc-26.3.2012
			// Added this clause (M. Fortin, CancelException, init now throws Exception)
			Log.println(Log.ERROR, "DataExtractor.init ()", "Exception", e);
			throw e;
		}

	}

	/**
	 * Added this to release unused colors when closing diagrams
	 */
	public void dispose() { // fc-2.1.2017 to release step button colors

		StepButton sb = ProjectManager.getInstance().getStepButton(step);
		ButtonColorer.getInstance().seeIfColorShouldBeRemoved(sb);

	}

	/**
	 * Defines an order on the extractors for later sorting
	 */
	public int compareTo(Object o) {

		try {
			Step step1 = step;
			DataExtractor e2 = (DataExtractor) o;
			Step step2 = e2.getStep();

			// 1. compare project names
			String projectName1 = step1.getProject().getName();
			String projectName2 = step2.getProject().getName();
			if (projectName1.compareTo(projectName2) < 0) {
				return -1;
			} else if (projectName1.compareTo(projectName2) > 0) {
				return 1;
			} else {

				// 2. compare step dates
				int date1 = step1.getScene().getDate();
				int date2 = step2.getScene().getDate();
				if (date1 < date2) {
					return -1;
				} else if (date1 > date2) {
					return 1;
				} else {

					// 3. compare the scenario letter ('a', 'b'...), depends on
					// step width
					int w1 = step1.getWidth();
					int w2 = step2.getWidth();
					if (w1 < w2) {
						return -1;
					} else if (w1 > w2) {
						return 1;
					} else {

						// 4. compare '*' (means 'is an intervention result')
						boolean i1 = step1.getScene().isInterventionResult();
						boolean i2 = step2.getScene().isInterventionResult();
						if (!i1 && i2) {
							return -1;
						} else if (i1 && !i2) {
							return 1;
						} else {

							// 5. compare step depth
							int d1 = step1.getDepth();
							int d2 = step2.getDepth();
							if (d1 < d2) {
								return -1;
							} else if (d1 > d2) {
								return 1;
							} else {

								// 6. compare captions
								int r = this.getCaption().compareTo(e2.getCaption());
								if (r < 0 || r > 0) {
									return r;
								} else {

									// 7. compare extractors hashcodes
									// (fc-16.11.2011)
									int c1 = this.hashCode();
									int c2 = e2.hashCode();
									return c1 - c2;

								}
							}
						}

					}

				}
			}

		} catch (Exception e) {
			return -1; // error
		}
	}

	/**
	 * Return true if the extractor can deal with only one individual at a time (ex:
	 * one single tree). Can be detected by config tools to enable single selection
	 * only.
	 */
	public boolean isSingleIndividual() {
		return false;
	} // fc-28.9.2006

	/**
	 * Rank management. If several extractors of same type (e.g. DETimeN) are opened
	 * on same step, they have a different rank. Rank can be used to change data
	 * extractor color (darker, brighter). Default case: rank is set to 0.
	 * fc-5.5.2003
	 */
	public void setRank(int rank) {
		this.rank = rank;
	}

	public int getRank() {
		return rank;
	}

	/**
	 * Create a settings object for this extractor and call the setConfigProperty ()
	 * method to build the properties with their default values.
	 */
	protected void resetSettings() {

		// fc-1.8.2018 if doExtraction () fails, reset the settings and retry

		settings = new DESettings();

		setConfigProperties();

//		// fc-2.8.2018
//		// setProperties memorisation is managed specifically: see if for each
//		// property and its set of possible values there was a user selection
//		// saved, if so restore it, else do nothing more (the property may have
//		// been inited in setConfigProperty with selectedValues =
//		// possibleValues). See in sharedConfigure below for the storing of the
//		// user chosen values.
		restoreUserSelectionInSetPropertiesIfNeeded(); // fc-25.7.2021

		// fc-11.10.2016 note: this was added by M. Fortin in August 2011, makes
		// it possible for some models to force e.g. perHa option in graphs
		if (step.getProject().getModel() instanceof OverridableDataExtractorParameter)
			((OverridableDataExtractorParameter) step.getProject().getModel()).setDefaultProperties(settings);

	}

	/**
	 * Asks the extension manager for last version of settings for this extension
	 * type. Redefinable by subclasses to get settings subtypes.
	 */
	protected void retrieveSettings() {

		System.out.println(""+getClass().getSimpleName ()+" "+getStep()+" retrieveSettings ()...");
		
		// fc-1.8.2018 resetSettings () can be called alone
		resetSettings();

		// // fc-11.10.2016 note: this was added by M. Fortin in August 2011,
		// makes
		// // it possible for some models to force e.g. perHa option in graphs
		// if (step.getProject().getModel() instanceof
		// OverridableDataExtractorParameter)
		// ((OverridableDataExtractorParameter)
		// step.getProject().getModel()).setDefaultProperties(settings);

		// fc-11.10.2016 note: this was refactored by S. Dufour, settings are
		// stored in xml files

		ExtensionManager.applySettings(this);

	}

	// -----------------------------------------------------------
	// Extension related methods

	/**
	 * This prefix is built depending on current settings. ex: "+ 25 years /ha"
	 */
	public String getNamePrefix() {
		String prefix = "";
		try {
			GrouperManager gm = GrouperManager.getInstance();
			if (isCommonGrouper() && isGrouperMode() && gm.getGrouperNames().contains(gm.removeNot(getGrouperName()))) {
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
	 * Changes current step.
	 */
	public void setStep(Step stp) {
		step = stp;
		upToDate = false;
	}

	public Step getStep() {
		return step;
	}

	public DataBlock getDataBlock() { // fc-26.9.2012
		return dataBlock;
	}

	/**
	 * Update on toStep if required, depending on fromStep.
	 */
	@Override
	public boolean update(Step fromStep, Step toStep) throws Exception {

		// fc-20.1.2014 Added throws Exception, trying to prevent an extractor
		// from blocking Capsis

		if (fromStep != null && fromStep.equals(step)) {
			setStep(toStep);

			// fc-1.8.2018 try to cope with settings property values restoration
			// trouble: if doExtraction fails, resetSettings and try again
			// doExtraction();
			doExtractionFailSafe();

			return true;
		}

		return false;
	}

	/**
	 * During doExtraction, trouble can arise due to restored values of the
	 * configuration properties. These restored values are generally very useful but
	 * may be nonsense on another project or with another inventory file (locus
	 * names changed completely...). Calls doExtraction and in case of trouble,
	 * resets the config properties to their default values and recalls it. Returns
	 * true if doExtraction returned true, false otherwise.
	 */
	public boolean doExtractionFailSafe() throws Exception {

		// fc-1.8.2018

		boolean ok = false;
		Exception doExtractionReturnedFalse = new Exception("doExtraction () returned false");

		try {
			ok = doExtraction();

			if (!ok)
				throw doExtractionReturnedFalse;

		} catch (Exception e) {

			// doExtraction returned false or sent an exception: trouble

			// fc-16.9.2021 Added the message in Log for information
			Log.println(Log.WARNING, "AbstractDataExtractor.doExtractionFailSafe ()",
					"Extractor: " + getClassName()
							+ ", exception, see below for information, reran it with config properties default values",
					e);

			System.out.println("AbstractDataExtractor.doExtractionFailSafe (): doExtraction () was recalled for "
					+ getClassName() + " with config properties default values, more details in Log");

			resetSettings();

			try {
				ok = doExtraction();
			} catch (Exception e2) {
				ok = false;
			}
		}

		return ok;

	}

	// -------------------------------------------------------------------------
	// Filtering trees with a grouper (e.g. tree dbh > 30) or a status (e.g. cut
	// trees)

	/**
	 * Apply current status and grouper on the given scene, return a sub collection
	 * of individuals. The Collection contains individuals which type is known by
	 * the Group class. This method may be called from doExtraction, e.g. if the
	 * extractor configProperties include a grouper.
	 */
	public Collection doFilter(GScene scene) { // fc-17.9.2004

		if (settings.c_grouperType == null) {
			Log.println(Log.ERROR, "DataExtractor.doFilter (stand)",
					"settings.c_grouperType == null, used TREE instead");
			settings.c_grouperType = TreeList.GROUP_ALIVE_TREE;

		}

		return doFilter(scene, settings.c_grouperType);
	}

	/**
	 * This method should be preferred to doFilter (GScene)
	 */
	public Collection doFilter(GScene scene, GroupableType grouperType) { // fc-17.9.2004

		// GrouperType e.g. TreeList.GROUP_ALIVE_TREE, Group.CELL...
		Collection input = Group.whichCollection(scene, grouperType);

		// Consider TreeList's statusSelection
		if (grouperType.equals(TreeList.GROUP_ALIVE_TREE) && statusSelection != null) {

			TreeList gtcstand = (TreeList) scene;
			input = gtcstand.getTrees(statusSelection);
		}

		if (!isGrouperMode()) {
			return input;
		} // no grouper selected

		GrouperManager gm = GrouperManager.getInstance();

		// If group not found, a DummyGrouper is returned
		Grouper g = gm.getGrouper(getGrouperName());

		// fc-16.11.2011 - use a copy of the grouper (several data extractors
		// are updated in several threads, avoid concurrence problems)
		Grouper copy = g.getCopy();

		// Note: a DummyGrouper returns the Collection unchanged
		Collection output = copy.apply(input, getGrouperName().toLowerCase().startsWith("not "));

		return output;

	}

	/**
	 * Applies the current grouper on the given list. This is a more flexible
	 * version of doFilter(), can be applied on various tree lists.
	 * 
	 * @param input A list of trees to be restricted by the current grouper
	 */
	// fc+bc-3.5.2016
	public Collection doFilter(List input) {

		if (!isGrouperMode()) {
			return input;
		} // no grouper selected

		GrouperManager gm = GrouperManager.getInstance();
		Grouper g = gm.getGrouper(getGrouperName()); // if group not found,
														// return a DummyGrouper

		// fc-16.11.2011 - use a copy of the grouper (several data extractors
		// are updated in several threads, avoid concurrence problems)
		Grouper copy = g.getCopy();

		// The DummyGrouper returns the Collection unchanged
		Collection output = copy.apply(input, getGrouperName().toLowerCase().startsWith("not "));
		return output;

	}

	/**
	 * Returns the scene area (m2), can be used for per hectare computations. If a
	 * grouper is there, it can change the answer.
	 */
	public double getSceneArea(GScene scene) {

		// fc-2.6.2020 see Samsa2CoreTreeGrouper, returns the area of the core
		// polygon

		GrouperManager gm = GrouperManager.getInstance();
		Grouper g = gm.getGrouper(getGrouperName()); // if group not found,
														// return a DummyGrouper

		// Note: the DummyGrouper returns the initial area of the scene

//		System.out.println("[AbstractDataExtractor] getSceneArea(): "+g.getArea(scene));

		return g.getArea(scene);

	}

	/**
	 * Returns the data renderer class name's default value of the data extractor.
	 * From DataFormat interface.
	 */
	@Override
	public String getDefaultDataRendererClassName() {

		// fc+nb-14.01.2019 Moved in
		// capsis.extensiontype.DataBlock.getDefaultDataRendererClassName()
		// // fc-29.7.2016 new renderers are preferred only if user likes (in
		// Edit
		// // > Options), PhD request
		// if (Settings.getProperty("open.graphs.with.new.renderers", true)) {
		//
		// // fc-15.10.2015 New graphs in Capsis4.2.4
		// if
		// (defaultDataRendererClassName.equals("capsis.extension.datarenderer.drcurves.DRCurves"))
		// defaultDataRendererClassName =
		// "capsis.extension.datarenderer.drgraph.DRGraph";
		// if
		// (defaultDataRendererClassName.equals("capsis.extension.datarenderer.drcurves.DRHistogram"))
		// defaultDataRendererClassName =
		// "capsis.extension.datarenderer.drgraph.DRBarGraph";
		// if
		// (defaultDataRendererClassName.equals("capsis.extension.datarenderer.drcurves.DRScatterPlot"))
		// defaultDataRendererClassName =
		// "capsis.extension.datarenderer.drgraph.DRScatterGraph";
		//
		// }
		//
		// return defaultDataRendererClassName;

		// Default value.
		return "capsis.extension.datarenderer.drcurves.DRCurves";
	}

	/**
	 * From DataFormat interface. Returns a caption for this extractor.
	 */
	public String getCaption() {

		// fc-5.10.2021 If several extractors on the same step, we would like different
		// captions, e.g. sam.2012a and sam.2012a[1]
		String caption = dataBlock.getExtractorBlockUniqueCaption(this);
//		String caption = getStep().getCaption();

		// fc-5.10.2021 Reviewed the following lines

		// Case of treeIds
//		if (getTreeIds() != null && !getTreeIds().isEmpty())
//			caption += " " + Translator.swap("Shared.id") + ":";
//		caption += " [" + Tools.cutIfTooLong(Tools.toString(getTreeIds()), 20) + "]";
//		caption += " - " + Translator.swap("Shared.id") + " " + Tools.toString(getTreeIds());

		// Individual group
		if (isGrouperMode() && !isCommonGrouper())
			caption += " [" + getGrouperName() + "]";

		// If Status is used, not all types of trees are concerned
		if (statusSelection != null)
			caption += StatusChooser.getName(statusSelection); // e.g.
																// (cut+dead)

		return caption;
	}

	/**
	 * From DataFormat interface. Returns the color of the step button of the step.
	 * If step button has no color, returns another color.
	 */
	public Color getColor() {

		// Check if forced color
		if (forcedColor != null)
			return forcedColor; // fc-29.4.2003

		Color col = null;
		StepButton b = ProjectManager.getInstance().getStepButton(step);

		col = b.getColor();
		if (col == null)
			col = StepButton.DEFAULT_COLOR;

		// Several extractors on the button
		if (rank > 0) {

			// fc-9.11.2020
			RelatedColorProvider cp = ProjectManager.getInstance().getColorProvider();

			col = cp.getRelatedColor(col, rank);

		}

		return col;
	}

	/**
	 * Force extractor color. Can be used when several extractors are used on the
	 * same step with different groupers. In normal case, color is not forced and is
	 * equal to the step button's color.
	 */
	public void setColor(Color forcedColor) {
		this.forcedColor = forcedColor;
	} // fc-29.4.2003

	/**
	 * Return a String representation of this object.
	 */
	public String toString() {
		String step = "";
		try {
			step = getStep().toString();
		} catch (Exception a) {
		}
		return getName() + ", " + step;
	}

	/**
	 * This method is called if user selects or changes groupers. It can be
	 * redefined in subclasses to do things in this case. Ex: updateSetProperty
	 * ("somePropertyKey", someSetOfPossibleValues);
	 */
	public void grouperChanged(String gestGrouperName) {
	}

	/**
	 * SharedConfigurable interface.
	 */
	public void postConfig() {

		ExtensionManager.recordSettings(this);

		upToDate = false;

	}

	public Collection<String> getDocumentationKeys() {
		return documentationKeys;
	}

	/**
	 * Returns true is the extractor can work on the current Step (e.g. false if
	 * works on cut trees and no cut trees on this step).
	 */
	public boolean isAvailable() {
		return true;
	}

	/**
	 * Returns the type of the Extension.
	 */
	// fc+mj-8.3.2017 Added this method in AbstractDataExtractor
	public String getType() {
		return CapsisExtensionManager.DATA_EXTRACTOR;
	}

	/**
	 * Returns the class name of the Extension: getClass ().getName ().
	 */
	// fc+mj-8.3.2017 Added this method in AbstractDataExtractor
	public String getClassName() {
		return this.getClass().getName();
	}

	// fc-5.9.2019 Used when adding a step in a dataBlock
	public void setDataBlock(DataBlock db) {
		this.dataBlock = db;

		if (db == null)
			System.out.println("[AbstractDataExtractor.setDataBlock()] *** assigning a null dataBlock !");

	}

}
