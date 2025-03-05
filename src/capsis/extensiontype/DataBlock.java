/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2012 INRA
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski,
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
package capsis.extensiontype;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;

import capsis.app.CapsisExtensionManager;
import capsis.commongui.projectmanager.ButtonColorer;
import capsis.commongui.projectmanager.ButtonColorerListener;
import capsis.commongui.projectmanager.StepButton;
import capsis.extension.AbstractDiagram;
import capsis.extension.DECalibration;
import capsis.extension.dataextractor.configuration.DataBlockManager;
import capsis.extension.dataextractor.superclass.ConfigurableExtractor;
import capsis.gui.Pilot;
import capsis.kernel.GModel;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.CancellationException;
import jeeb.lib.util.Log;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.SwingWorker3;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * A data block contains one or more data extractors. These extractors are all
 * instances of the same class (e.g.: Basal area / time). A data renderer is
 * associated to the data block. It's originally the default renderer of first
 * data extractor. Then, it can be swapped by user action on the renderer. The
 * renderer must be able to show the extracted data.
 * 
 * This data block listens to the ButtonColorer to manage color move and removal
 * in the ProjectManager by synchronizing / closing the related extractors.
 * 
 * @author F. de Coligny - December 1999 / March 2003 / April 2010.
 */
public class DataBlock implements ButtonColorerListener {

	// Extractor type is a data extractor className, e.g.
	// "capsis.extension.dataextractor.DETimeDbh"
	protected String extractorType;
	protected Set<DataExtractor> extractors;
	protected DataRenderer renderer;
	protected ExtensionManager extMan;
	// protected Step step;

	protected boolean captionRequired;
	private Collection<DataExtractor> specialExtractors; // never null, possibly
															// empty, e.g.
															// DECalibration
															// instances

	// fc-4.9.2019
	// The data block contains one or several extractors of same type.
	// The extractors may have a COMMON configuration, e.g. group (aka multi
	// configuration or shared configuration). [Group feature is only available
	// to some modules managing elements, see the Group framework.] If so, all
	// extractors of this block will show only the elements in the group (e.g.
	// distribution of spruce trees on several steps).
	// The extractors may also have an INDIVIDUAL configuration, e.g. group or
	// treeIds. If so, only the configured extractor will restrict to the
	// elements of the group / with the given ids (e.g. spruce and fir
	// distribution with two extractors on same step).
	// To manage consistency between COMMON and INDIVIDUAL elements
	// restrictions, we introduce the two booleans below. They must be set
	// correctly when user changes options in the configuration panels.
	// Before changing the element restriction configuration at a given level,
	// check before it has not been changed at the other, else send a message to
	// user. See DEMultiConfPanel and DEConfigurationPanel for resp. common
	// configuration / individual configuration of the extractors.
	// Note: there may be no restriction at common nor individual levels.
	// The whole data extractors system is only interactive.
	private boolean elementRestrictionAtCommonLevel;
	private boolean elementRestrictionAtIndividualLevel;

	// fc-6.9.2021 The Selector is a DataBlockManager: opens the DataBlocks and is
	// told when they are disposed. This dbManager can be null.
	private DataBlockManager dbManager;

	// fc-5.10.2021 Better management of unique captions for extractors and
	// dataSeries
	// in the DataRenderers
	// Key is each data extractor in the block, value is its unique step caption in
	// this block (a suffix is appended to the step caption if needed). This map is
	// invalidated
	// when the unique step captions are to be reevaluated, e.g. when adding an
	// extractor, when changing steps...
	private Map<DataExtractor, String> extractorBlockUniqueStepCaptions; // may be null

	/**
	 * Default constructor.
	 */
	public DataBlock() {
	}

	/**
	 * Preferred constructor.
	 */
	public DataBlock(String type, Step step) {
		this(type, step, null, null);
	}

	/**
	 * Builds a dataBlock with the given extractor (type, i.e. graph) opened on the
	 * given step, renderered by the suggested or default renderer, evrything is
	 * done in a thread to keep the gui reactive.
	 * 
	 * The suggestedRendererClassName field has been added for diagramLists, it is
	 * optional and may be passed null.
	 */
	public DataBlock(String type, Step step, DataBlockManager dbManager, String suggestedRendererClassName) {

		// fc-6.9.2021 Added dbManager, may be null (generally the Selector)

		this(); // fc-4.9.2019

		this.dbManager = dbManager; // fc-6.9.2021

		extractorType = type; // Must be a complete className (with package
								// name)

		extractors = new TreeSet<DataExtractor>(); // sorted on step date
		specialExtractors = new ArrayList<DataExtractor>();
		renderer = null;
		// this.step = step;
		captionRequired = true; // Change if needed with setCaptionRequired
								// (false)

		extMan = CapsisExtensionManager.getInstance();

		// Final variables for the worker thread below
		final String finalExtractorType = extractorType;
		final Step finalStep = step;
		final ExtensionManager finalExtMan = extMan;
		final Set<DataExtractor> finalExtractors = extractors;
		final String finalSuggestedRendererClassName = suggestedRendererClassName;

		// Data block construction: in a thread
		SwingWorker3 worker = new SwingWorker3() {

			// Runs in new Thread
			public Object construct() {
				try {
					// 1. Load an extractor of given type for given step
					DataExtractor extractor = (DataExtractor) finalExtMan.loadInitData(finalExtractorType,
							new GenericExtensionStarter("model", finalStep.getProject().getModel(), "step", finalStep));

					extractor.init(finalStep.getProject().getModel(), finalStep);

					finalExtractors.add(extractor);

					extractor.doExtraction(); // initialize extractor

					return extractor; // correct return of construct

				} catch (Exception e) {
					Log.println(Log.ERROR, "DataBlock (type, step)",
							"Constructor: unable to load data extractor of type " + finalExtractorType, e);
					return e;
				}
			}

			// Runs in dispatch event thread when construct is over
			public void finished() {

				// fc-25.7.2021 Isolating extractor configuration
				ConfigurableExtractor extractor = null;
//				DataExtractor extractor = null;
				String rendererClassName = null;

				try {
					Object result = get(); // result is either the correct
											// return of construct or an
											// exception
					if (result instanceof Exception)
						throw (Exception) result;

					// 2. Load the default renderer for dataBlock
					extractor = (ConfigurableExtractor) result;
//					extractor = (DataExtractor) result;

					rendererClassName = getDefaultDataRendererClassName(extractor);

					// fc-10.12.2015 for diagramLists, must reopen with same
					// renderer
					if (finalSuggestedRendererClassName != null)
						rendererClassName = finalSuggestedRendererClassName;

					renderer = (DataRenderer) finalExtMan.instantiate(rendererClassName);

					renderer.init(DataBlock.this);

					renderer.update(); // update renderer

					// Restore the calibration extractor if the option was set
					// at last extractor opening time
					if (extractor.hasConfigProperty("activateCalibration")) {

						String modelName = extractor.getComboProperty("activateCalibration");

						// At opening time, check if the calibration is
						// activated and if the selected model is not ours,
						// desactivate it (the user may reactivate it if needed)
						// fc-26.9.2012
						String ourModelName = finalStep.getProject().getModel().getIdCard().getModelPackageName();

						if (modelName.equals(Translator.swap("Shared.noneFeminine"))) {
							// Calibration is not activated, do nothing

						} else if (modelName.equals(ourModelName)) {
							// Add the calibration extractor
							addCalibrationExtractor(modelName);

						} else {

							// fc-13.9.2021 REMOVED this option, selectOptionInComboProperty() was moved in
							// the new ComboPropertiesManager class, managed in DEMultiConfPanel.

//							// Let the user select the calibration extractor if
//							// he likes (the memorised option is not our model,
//							// deselect it)
//							String toBeSelected = Translator.swap("Shared.noneFeminine");
//							((AbstractDataExtractor) extractor).selectOptionInComboProperty("activateCalibration",
//									toBeSelected);

							// fc-13.9.2021 REMOVED this option

						}

					}

					// Register to the ButtonColorer
					ButtonColorer.getInstance().addListener((ButtonColorerListener) DataBlock.this);

					String extensionName = ExtensionManager.getName(extractorType);
					StatusDispatcher.print("" + extensionName);

				} catch (Exception e) {
					if (e instanceof CancellationException) {
						StatusDispatcher.print(Translator.swap("Shared.operationCancelled")); // nothing
																								// more
					} else {
						Log.println(Log.ERROR, "DataBlock (type, step)",
								"finished (): cannot instanciate default DataRenderer <" + rendererClassName + ">", e);
						StatusDispatcher.print(Translator.swap("Shared.errorSeeLog"));
					}
				}
			}
		};

		String extensionName = ExtensionManager.getName(extractorType);
		StatusDispatcher.print(Translator.swap("Shared.opening") + " " + extensionName + "...");

		worker.start(); // launch the thread

	}

	/**
	 * Returns the default data renderer class name of the given data extractor.
	 */
	protected String getDefaultDataRendererClassName(DataExtractor extractor) {

		String dr = extractor.getDefaultDataRendererClassName();

		// fc+nb-14.01.2019 The below block was previously stored in :
		// capsis.extension.dataextractor.superclass.AbstractDataExtractor.getDefaultDataRendererClassName()
		// fc-29.7.2016 new renderers are preferred only if user likes (in Edit
		// > Options), PhD request
		if (Settings.getProperty("open.graphs.with.new.renderers", true)) {

			// fc-15.10.2015 New graphs in Capsis4.2.4
			if (dr.equals("capsis.extension.datarenderer.drcurves.DRCurves"))
				dr = "capsis.extension.datarenderer.drgraph.DRGraph";
			if (dr.equals("capsis.extension.datarenderer.drcurves.DRHistogram"))
				dr = "capsis.extension.datarenderer.drgraph.DRBarGraph";
			if (dr.equals("capsis.extension.datarenderer.drcurves.DRScatterPlot"))
				dr = "capsis.extension.datarenderer.drgraph.DRScatterGraph";

		}

		return dr;
	}

	/**
	 * Add a calibration extractor.
	 */
	public void addCalibrationExtractor(String modelName) {

		if (specialExtractors.contains(modelName))
			return; // Already there

		String extractorName = AmapTools.getClassSimpleName(extractorType);
		DECalibration c = new DECalibration(this, modelName, extractorName);
		c.doExtraction();

		// Calibration extractors are managed aside
		specialExtractors.clear(); // only one calibration extractor at the same
									// time
		specialExtractors.add(c);

//		// fc-5.10.2021 Invalidate unique step captions map
//		extractorBlockUniqueStepCaptions = null;

		renderer.update();
	}

	/**
	 * Remove a calibration extractor.
	 */
	public void removeCalibrationExtractor() {

		// fc-5.10.2021 Update unique step captions map
		for (DataExtractor ex : specialExtractors) {
			extractorBlockUniqueStepCaptions.remove(ex);
		}

		specialExtractors.clear();

	}

	/**
	 * Accessor for special extractors, ex: DECalibration.
	 */
	public Collection<DataExtractor> getSpecialExtractors() {
		return specialExtractors;
	}

	/**
	 * Returns true if all extractors in this block are synchronized on same step.
	 */
	public boolean allExtractorsAreOnTheSameStep() { // fc+bc-19.9.2016
		Step step = null;
		for (DataExtractor e : extractors) {
			if (step == null) {
				step = e.getStep(); // 1st one
			} else if (!e.getStep().equals(step)) {
				return false; // found 2 different steps
			}
		}
		return true;

	}

	/**
	 * Add an extractor to the data block.
	 */
	public void addExtractor(Step step) {
		if (step == null) {
			return;
		}

		// For security
		if (!extMan.isCompatible(extractorType, step.getProject().getModel())) {
			return;
		}

		// Calculate rank : how many extractors of this type already
		// synchronized on same step ?
		int rank = 0;
		for (DataExtractor e : extractors) {
			if (e.getStep().equals(step)) {
				rank++;
			}
		}

//		// fc-5.10.2021 Invalidate unique step captions map
//		extractorBlockUniqueStepCaptions = null;

		final String finalExtractorType = extractorType;
		final Step finalStep = step;
		final ExtensionManager finalExtMan = CapsisExtensionManager.getInstance();
		final int finalRank = rank;
		final Set<DataExtractor> finalExtractors = extractors;
		final DataBlock dataBlock = this; // fc-5.9.2019

		// fc - 20.9.2005 - try to thread data block construction
		SwingWorker3 worker = new SwingWorker3() {

			// Runs in new Thread
			public Object construct() {
				try {

					DataExtractor extractor = (DataExtractor) finalExtMan.loadInitData(finalExtractorType,
							new GenericExtensionStarter("model", finalStep.getProject().getModel(), "step", finalStep));

					// fc-11.10.2023 Check is there is a veto (e.g. incompatible configuration)
					if (!extractor.acceptsToBeAddedToThisDataBlock(dataBlock, finalStep))
						throw new Exception(
								"Unable to add the data series to the graph (acceptsToBeAddedToThisDataBlock() return false)");

					extractor.init(finalStep.getProject().getModel(), finalStep);

					// fc-5.9.2019 when adding a Step in an existing dataBlock,
					// the dataBlock reference in the new extractor was missing
					// (null).
					// Became a problem when managing the common / individual
					// configuration consistency (treeIds, groups...)
					extractor.setDataBlock(dataBlock);

					if (finalRank != 0)
						extractor.setRank(finalRank); // fc - 5.5.2003
					finalExtractors.add(extractor);

					// fc-1.8.2018 recalls doExtraction with props default
					// values if trouble
					// extractor.doExtraction(); // initialize new extractor
					extractor.doExtractionFailSafe(); // initialize new
														// extractor

					return null; // correct return of construct
				} catch (Exception e) {

					Log.println(Log.ERROR, "DataBlock.addExtractor ()",
							"construct (): exception during loading / doExtraction for <" + finalExtractorType + ">.",
							e);
					return e;
				}
			}

			// Runs in dispatch event thread when construct is over
			public void finished() {
				try {
					Object result = get(); // result is either the correct
											// return of construct or an
											// exception
					if (result instanceof Exception) {
						throw (Exception) result;
					}

					renderer.update(); // update renderer

				} catch (Exception e) {
					if (e instanceof CancellationException) {
						StatusDispatcher.print(Translator.swap("Shared.operationCancelled")); // nothing
																								// more
					} else {

						Log.println(Log.ERROR, "DataBlock.addExtractor ()",
								"finished (): exception while updating renderer", e);
						StatusDispatcher.print(Translator.swap("Shared.errorSeeLog"));
					}
				}
			}
		};

		renderer.setUpdating();
		worker.start(); // launch the thread

	}

	/**
	 * Remove an extractor from the data block, then update renderer. If last
	 * extractor was removed, dispose the renderer (which also disposes this
	 * dataBlock).
	 */
	public void removeExtractor(DataExtractor target) {
		int initialSize = extractors.size();

		for (Iterator<DataExtractor> i = extractors.iterator(); i.hasNext();) {
			DataExtractor extractor = i.next();
			if (extractor.equals(target)) {
				i.remove();
			}
		}

		// fc-5.10.2021 Update unique step captions map
		extractorBlockUniqueStepCaptions.remove(target);

		// Update or close renderer
		if (extractors.size() == 0) {
			renderer.close();
		} else if (extractors.size() != initialSize) {
			renderer.update();
		}
	}

	/**
	 * Set the renderer invisible, then dispose this data block.
	 */
	public void dispose() {

		// fc-6.9.2021 If there is a dbManager, tell him we are disposing
		if (dbManager != null)
			dbManager.dataBlockDisposed(this);

		// Deregister from the ButtonColorer
		ButtonColorer.getInstance().removeListener(this);

		// fc-2.1.2017 releasing sb colors
		for (DataExtractor e : extractors)
			e.dispose();

	}

	/**
	 * Return the collection of the data extractors connected to this block.
	 */
	public Collection<DataExtractor> getDataExtractors() {

		// fc-3.6.2020
		// Occasional bug in DEConfigurationPanel.c (), extractor dataBlock is null
		// Could not find the cause, not systematic
		// Forcing the extractors' ref to this dataBlock to try to fix this
		for (DataExtractor ex : extractors) {
			if (ex.getDataBlock() == null) // fc-11.10.2023 Added this test (needed for a configuration check)
				ex.setDataBlock(this);
		}

		return extractors;
	}

	/**
	 * Force every extractor update (after configuration changed for example).
	 */
	public void updateExtractors() {

//		// fc-5.10.2021 Invalidate unique step captions map
//		extractorBlockUniqueStepCaptions = null;

		final Collection<DataExtractor> finalExtractors = new ArrayList<DataExtractor>(extractors);

		// fc - 19.9.2005 - try to thread extractors update
		// Launch a thread extractors update
		SwingWorker3 worker = new SwingWorker3() {

			// Runs in new Thread
			public Object construct() {
				try {
					for (DataExtractor extractor : finalExtractors) {

						// fc-1.8.2018 manage property values restoring errors
						// extractor.doExtraction();
						extractor.doExtractionFailSafe();

//						// fc-5.10.2021 Unique step captions to be reevaluated (group name...)
//						extractorBlockUniqueStepCaptions.remove(extractor);

//						System.out.println("[DataBlock] updated extractor "+extractor);

					}
					return null; // correct return of construct
				} catch (final Throwable e) {
					Log.println(Log.ERROR, "DataBlock.updateExtractors ()", "Exception/Error in construct ()", e);
					return e;
				}
			}

			// Runs in dispatch event thread when construct is over
			public void finished() {
				try {
					Object result = get(); // result is either the correct
											// return of construct or an
											// exception
					if (result instanceof Exception) {
						throw (Exception) result;
					}

					renderer.update();

//					System.out.println("[DataBlock] updated renderer "+renderer);

				} catch (Exception e) {
					Log.println(Log.ERROR, "DataBlock ().updateExtractors ()", "Exception/Error in finished ()", e);
					StatusDispatcher.print(Translator.swap("Shared.errorSeeLog"));
				}
			}
		};
		// no message in StatusDispatcher here (occurs very often)
		renderer.setUpdating();
		worker.start(); // launch the thread
	}

	/**
	 * Tell the extractors to update from source to target. Only the extractors
	 * synchronized on source will change.
	 */
	private void updateExtractors(Step source, Step target) {

		final Step finalSource = source;
		final Step finalTarget = target;
		final Collection<DataExtractor> finalExtractors = new ArrayList<DataExtractor>(extractors);

		// fc - 19.9.2005 - try to thread extractors update
		// Launch a thread extractors update
		SwingWorker3 worker = new SwingWorker3() {

			// Runs in new Thread
			public Object construct() {
				try {

					boolean someChange = false;
					for (DataExtractor extractor : finalExtractors) {
						boolean moved = extractor.update(finalSource, finalTarget); // if
																					// extractor
																					// is
																					// concerned,
																					// update
																					// returns
																					// true
						if (moved) {

							// fc-5.10.2021 Unique step captions to be reevaluated
							extractorBlockUniqueStepCaptions.remove(extractor);

							someChange = true;
						}
					}

					return new Boolean(someChange); // correct return of
													// construct
				} catch (final Throwable e) {
					Log.println(Log.ERROR, "DataBlock.updateExtractors (source, target)",
							"Exception/Error in construct ()", e);
					return e;
				}
			}

			// Runs in dispatch event thread when construct is over
			public void finished() {
				try {
					Object result = get(); // result is either the correct
											// return of construct or an
											// exception
					if (result instanceof Exception) {
						throw (Exception) result;
					}

					Boolean b = (Boolean) result;
					boolean someChange = b.booleanValue();

					if (someChange) {

						// Redo the extractors sorting
						extractors = new TreeSet<DataExtractor>(new Vector(extractors));

						renderer.update();

					} else {
						renderer.setUpdated(); // not updating... any more
					}
				} catch (Exception e) {
					Log.println(Log.ERROR, "DataBlock ().updateExtractors (source, target)",
							"Exception/Error in finished ()", e);
					StatusDispatcher.print(Translator.swap("Shared.errorSeeLog"));
				}
			}
		};
		// no message in StatusDispatcher here (occurs very often)
		renderer.setUpdating();
		worker.start(); // launch the thread
	}

//	private void updateExtractors(Step source, Step target) {
//
//		// fc-30.8.2021 Searching the config propagation bug... 
//		// -> trying extractors updating without threads
//		
//		try {
//		
//			final Step finalSource = source;
//			final Step finalTarget = target;
//			final Collection<DataExtractor> finalExtractors = new ArrayList<DataExtractor>(extractors);
//	
//	//		// fc - 19.9.2005 - try to thread extractors update
//	//		// Launch a thread extractors update
//	//		SwingWorker3 worker = new SwingWorker3() {
//	//
//	//			// Runs in new Thread
//	//			public Object construct() {
//	//				try {
//						
//						boolean someChange = false;
//						for (DataExtractor extractor : finalExtractors) {
//							boolean moved = extractor.update(finalSource, finalTarget); // if
//																						// extractor
//																						// is
//																						// concerned,
//																						// update
//																						// returns
//																						// true
//							if (moved) {
//								someChange = true;
//							}
//						}
//						
//	//					return new Boolean(someChange); // correct return of
//	//													// construct
//	//				} catch (final Throwable e) {
//	//					Log.println(Log.ERROR, "DataBlock.updateExtractors (source, target)",
//	//							"Exception/Error in construct ()", e);
//	//					return e;
//	//				}
//	//			}
//	//
//	//			// Runs in dispatch event thread when construct is over
//	//			public void finished() {
//	//				try {
//	//					Object result = get(); // result is either the correct
//	//											// return of construct or an
//	//											// exception
//	//					if (result instanceof Exception) {
//	//						throw (Exception) result;
//	//					}
//	//
//	//					Boolean b = (Boolean) result;
//	//					boolean someChange = b.booleanValue();
//	
//						if (someChange) {
//	
//							// Redo the extractors sorting
//							extractors = new TreeSet<DataExtractor>(new Vector(extractors));
//	
//							renderer.update();
//	
//						} else {
//							renderer.setUpdated(); // not updating... any more
//						}
//	//				} catch (Exception e) {
//	//					Log.println(Log.ERROR, "DataBlock ().updateExtractors (source, target)",
//	//							"Exception/Error in finished ()", e);
//	//					StatusDispatcher.print(Translator.swap("Shared.errorSeeLog"));
//	//				}
//	//			}
//	//		};
//	//		// no message in StatusDispatcher here (occurs very often)
//	//		renderer.setUpdating();
//	//		worker.start(); // launch the thread
//		} catch (Exception e) {
//			System.out.println("DataBlock, exception");
//			e.printStackTrace(System.out);
//		}
//	}

	/**
	 * Can be used to swap compatible data renderers.
	 */
	public void setRenderer(String rendererFullClassName) {
		DataRenderer memory = renderer;

		// New renderer instanciation

		try {
			renderer = (DataRenderer) extMan.instantiate(rendererFullClassName);

			// Avoid to recreate a new window
			Pilot.getInstance().getPositioner().replaceDiagram((AbstractDiagram) memory, (AbstractDiagram) renderer);
			renderer.init(this);
			renderer.update();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DataBlock.setRenderer ()",
					"Unable to load DataRenderer <" + rendererFullClassName + ">", e);
			renderer = memory;
			return;
		}
		// memory.destroyRendererOnly (); // this destroys the renderer without
		// disposing the dataBlock
	}

	/**
	 * Returns the current data renderer.
	 */
	public DataRenderer getRenderer() {
		return renderer;
	}

	/**
	 * Return the connected extractors' type (i.e. complete extractor className).
	 */
	public String getExtractorType() {
		return extractorType;
	}

	/**
	 * Returns a caption unique in the block for the given extractor (a suffix may
	 * be appended to its original caption).
	 */
	public String getExtractorBlockUniqueCaption(DataExtractor extractor) {

		// fc-5.10.2021

		if (extractorBlockUniqueStepCaptions == null)
			extractorBlockUniqueStepCaptions = new HashMap<>(); // first call

		// See if the map containing the unique step captions should be updated for this
		// extractor (it is invalidated when adding / removing extractors, changing
		// steps...)
		if (!extractorBlockUniqueStepCaptions.containsKey(extractor)) {

			Set<String> existingCaptions = new HashSet<>(extractorBlockUniqueStepCaptions.values());

			// Maybe not unique if several extractors on this step
			String extractorCaption = extractor.getStep().getCaption();

			int u = 1;
			String candidateCaption = extractorCaption;
			while (existingCaptions.contains(candidateCaption)) {
				candidateCaption = extractorCaption + "#" + u;
				u++;
			}
			extractorCaption = candidateCaption; // Now unique
			existingCaptions.add(extractorCaption);

			extractorBlockUniqueStepCaptions.put(extractor, extractorCaption);

		}

		return extractorBlockUniqueStepCaptions.get(extractor);

	}

	/**
	 * Return the class names of DataRenderers that can render the block.
	 */
	public Collection<String> getCompatibleDataRendererClassNames() {
		DataExtractor extractor = (DataExtractor) extractors.iterator().next();
		if (extractor != null) {
			return extMan.getExtensionClassNames(CapsisExtensionManager.DATA_RENDERER, extractor);
		} else {
			return null;
		}
	}

	/**
	 * Name of the block is the name of one of its extractors.
	 */
	public String getName() {
		Iterator<DataExtractor> i = extractors.iterator();
		if (i.hasNext()) {
			return i.next().getName(); // may change depending on configuration
										// (ex: "GroupName - /ha - name")
		} else {
			return "";
		}
	}

	/**
	 * Renderers may not draw the caption if not required. Normal case : required.
	 */
	public boolean isCaptionRequired() {
		return captionRequired;
	}

	public void setCaptionRequired(boolean v) {
		captionRequired = v;
	}

	/**
	 * Returns a String representation of this data block.
	 */
	public String toString() {
		int ne = extractors == null ? 0 : extractors.size();
		return "DataBlock [" + getName() + "] with " + ne + " extractors";
	}

	@Override
	public void colorMoved(StepButton previousButton, StepButton newButton) {
		try {
			updateExtractors(previousButton.getStep(), newButton.getStep());

		} catch (Exception e) { // fc-20.1.2014
			renderer.security();
		}

	}

	@Override
	public void colorRemoved(StepButton stepButton) {
		int initialSize = extractors.size();
		for (Iterator<DataExtractor> i = extractors.iterator(); i.hasNext();) {
			DataExtractor extractor = i.next();
			if (extractor.getStep().equals(stepButton.getStep())) {

				i.remove();

				// fc-5.10.2021 Update unique step captions map
				extractorBlockUniqueStepCaptions.remove(extractor);

			}
		}

		// Update or close renderer
		if (extractors.size() == 0) {
			renderer.close();
		} else if (extractors.size() != initialSize) {
			renderer.update();
		}

	}

	/**
	 * ButtonColorerListener interface
	 */
	@Override
	public boolean isListening(StepButton sb) { // fc-2.1.2017

		if (sb == null)
			return false; // nonsense

		for (DataExtractor e : extractors) {
			if (e.getStep().equals(sb.getStep()))
				return true; // someone listens
		}

		// noone listens
		return false;
	}

	/**
	 * Check if calibration data is available If so, add a comboProperty to propose
	 * to select calibration data for some model Return a collection with the model
	 * names of the data block which propose calibration data
	 */
	public Collection<String> detectCalibrationData() { // fc-12.10.2004

		Set<String> modelNames = new HashSet<String>(); // no duplicates

		for (Iterator<DataExtractor> i = getDataExtractors().iterator(); i.hasNext();) {
			DataExtractor e = i.next();

			// Skip DECalibration if any
			if (e instanceof DECalibration)
				continue;

			GModel m = e.getStep().getProject().getModel();
			modelNames.add(m.getIdCard().getModelPackageName());
		}

		String extractorName = AmapTools.getClassSimpleName(extractorType);

		for (Iterator<String> i = modelNames.iterator(); i.hasNext();) {
			String modelName = i.next();

			Object[] modelAndExtractorNames = new String[2];
			modelAndExtractorNames[0] = modelName;
			modelAndExtractorNames[1] = extractorName;
			DECalibration c = new DECalibration(this, modelName, extractorName);

			if (!c.matchWith(modelAndExtractorNames))
				i.remove(); // no calibration data found for this model name

		}

		if (modelNames.isEmpty())
			return modelNames; // no calibration data found

		return modelNames;
	}

	// UNUSED: all extractors in this data block may be synchronised on
	// different steps
	/**
	 * Step accessor.
	 */
	// public Step getStep () {
	// return step;
	// }

	// fc-4.9.2019 consistent elements restriction at common / individual levels
	public void setElementRestrictionAtCommonLevel(boolean elementRestrictionAtCommonLevel, String requesterIdentity) {

		// see the explanation near the elementRestrictionAtCommonLevel instance
		// variable

		this.elementRestrictionAtCommonLevel = elementRestrictionAtCommonLevel;
	}

	// fc-4.9.2019 consistent elements restriction at common / individual levels
	public void setElementRestrictionAtIndividualLevel(boolean elementRestrictionAtIndividualLevel,
			String requesterIdentity) {

		// see the explanation near the elementRestrictionAtCommonLevel instance
		// variable

		this.elementRestrictionAtIndividualLevel = elementRestrictionAtIndividualLevel;
	}

	// fc-4.9.2019 consistent elements restriction at common / individual levels
	public boolean isElementRestrictionAtCommonLevel() {
		return elementRestrictionAtCommonLevel;
	}

	// fc-4.9.2019 consistent elements restriction at common / individual levels
	public boolean isElementRestrictionAtIndividualLevel() {
		return elementRestrictionAtIndividualLevel;
	}

}
