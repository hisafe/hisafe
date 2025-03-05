/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package capsis.extensiontype;

import java.awt.Color;
import java.util.Collection;
import java.util.Iterator;
import java.util.TreeSet;

import jeeb.lib.util.CancellationException;
import jeeb.lib.util.Log;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.app.CapsisExtensionManager;
import capsis.extension.PaleoDataExtractor;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;

/**
 * A data block contains one or more data extractors. These extractors are all
 * of same type (i.e. same format interface, ex: Basal area / time).
 * 
 * This Mute DataBlock works with ObjectViewers : it may hold only one extractor
 * and Does not listen to the scenario manager (see DataBlock).
 * 
 * A data renderer is associated to the data block. It's originaly the default
 * renderer of first data extractor. Then, it can be swapped by user action on
 * the renderer. The renderer must be able to show the extracted data (ex:
 * Histograms).
 * 
 * @author F. de Coligny - April 2003.
 */
public class MuteDataBlock extends DataBlock {

	/**
	 * Default constructor.
	 */
	public MuteDataBlock(String type, Step step, String grouperName,
			Color forcedColor) { // forcedColor may be null
		super();

		extractorType = type; // Must be a complete className (with package
								// name)
		extractors = new TreeSet();
		renderer = null;

		extMan = CapsisExtensionManager.getInstance();

		// 1. Load an extractor of given type for given step
		DataExtractor extractor = null;
		try {
			extractor = (DataExtractor) extMan.loadInitData(extractorType,
					new GenericExtensionStarter("model", step.getProject()
							.getModel(), "step", step));
			extractor.init(step.getProject().getModel(), step);

			extractor.setGrouperName(grouperName); // added this on 14.4.2003

			extractor.setColor(forcedColor); // fc - 29.4.2003 - normal case,
												// null.

			extractors.add(extractor);

			// fc-1.8.2018 manage configuration properties restoring errors
//			extractor.doExtraction(); // initialize extractor
			extractor.doExtractionFailSafe(); // initialize extractor
			
		} catch (Exception e) {
			if (e instanceof CancellationException) {
				StatusDispatcher.print(Translator
						.swap("Shared.Shared.operationCancelled")); // nothing
																	// more
			} else {
				Log.println(Log.ERROR, "MuteDataBlock.c ()",
						"Unable to load data extractor of type <"
								+ extractorType + ">.", e);
			}
			return;
		}

		// 2. Load the default renderer for DataBlock

		String rendererClassName = null;
		
		try {
			rendererClassName = getDefaultDataRendererClassName(extractor);
			//fc+nb-14.01.2019 renderer = (DataRenderer) extMan.instantiate(extractor
				//	.getDefaultDataRendererClassName());
			renderer = (DataRenderer) extMan.instantiate(rendererClassName);
			renderer.init(this);
			renderer.update(); // update renderer
		} catch (Exception e) {
			Log.println(
					Log.ERROR,
					"MuteDataBlock.c ()",
					"Cannot instanciate default DataRenderer <"
							//fc+nb-14.01.2019  + extractor.getDefaultDataRendererClassName() + ">",
					+ rendererClassName + ">",
					e);
			return;
		}

	}

	/**
	 * Add an extractor to the data block.
	 */
	public void addExtractor(Step step) {
		addExtractor(step, "", null);
	}

	public void addExtractor(Step step, String grouperName, Color forcedColor) { // forcedColor
																					// may
																					// be
																					// null
		if (step == null) {
			return;
		}

		// For security
		if (!extMan.isCompatible(extractorType, step.getProject().getModel())) {
			return;
		}

		DataExtractor extractor = null;
		try {

			extractor = (DataExtractor) extMan.loadInitData(extractorType,
					new GenericExtensionStarter("model", step.getProject()
							.getModel(), "step", step));
			extractor.init(step.getProject().getModel(), step);

			extractor.setGrouperName(grouperName); // added this on 14.4.2003
			extractors.add(extractor);

			extractor.setColor(forcedColor); // fc - 29.4.2003 - normal case,
												// null.

			// fc-1.8.2018 manage property values restoring errors
//			extractor.doExtraction(); // initialize new extractor
			extractor.doExtractionFailSafe();
			
			renderer.update(); // update renderer
		} catch (Exception e) {
			if (e instanceof CancellationException) {
				StatusDispatcher.print(Translator
						.swap("Shared.Shared.operationCancelled")); // nothing
																	// more
			} else {
				Log.println(Log.ERROR, "MuteDataBlock.addExtractor ()",
						"Unable to load extension of type <" + extractorType
								+ ">.", e);
			}
		}
	}

	/**
	 * Remove an extractor from the data block, then update renderer. If last
	 * extractor was removed, dispose the renderer (which also disposes this
	 * MuteDataBlock).
	 */
	public void removeExtractor(DataExtractor target) {
		int initialSize = extractors.size();

		for (Iterator i = extractors.iterator(); i.hasNext();) {
			PaleoDataExtractor extractor = (PaleoDataExtractor) i.next();
			if (extractor.equals(target)) {
				i.remove();
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
	 * set the renderer unvisible, then dispose this data block.
	 */
	public void dispose() {
		stopListening();
	}

	/**
	 * We do not listen to a scenario manager : do nothing.
	 */
	public void stopListening() {
	}

	/**
	 * Return the collection of the data extractors connected to this block.
	 */
	public Collection getDataExtractors(boolean b1, boolean b2) {
		return extractors;
	}

	public Collection getDataExtractors() {
		return extractors;
	}

	/**
	 * Force every extractor update (after configuration changed for example).
	 */
	public void updateExtractors() {
		try {
			for (Iterator i = extractors.iterator(); i.hasNext();) {
				DataExtractor extractor = (PaleoDataExtractor) i.next();
				
				// fc-1.8.2018 manage configuration properties restoring errors
//				extractor.doExtraction();
				extractor.doExtractionFailSafe();
		}
			renderer.update();
			
		} catch (Exception e ) { // fc-20.1.2014
			renderer.security ();
		}
	}

	/**
	 * Return current data renderer.
	 */
	public DataRenderer getRenderer() {
		return renderer;
	}

	/**
	 * Return connected extractors' type (i.e. complete extractor className).
	 */
	public String getExtractorType() {
		return extractorType;
	}

	/**
	 * Return class names of DataRenderers that can render the block.
	 */
	public Collection getCompatibleDataRendererClassNames() {

		DataExtractor extractor = (DataExtractor) extractors.iterator().next();
		if (extractor != null) {
			return extMan.getExtensionClassNames(
					CapsisExtensionManager.DATA_RENDERER, extractor);
		} else {
			return null;
		}
	}

	/**
	 * Name of the block is the name of one of its extractors.
	 */
	public String getName() {
		Iterator i = extractors.iterator();
		if (i.hasNext()) {
			DataExtractor ex = (PaleoDataExtractor) i.next();
			return ExtensionManager.getName(ex.getClass().getName()); // may
																		// change
																		// depending
																		// on
																		// configuration
																		// (ex:
																		// "GrouperName - /ha - name")
		} else {
			return "";
		}
	}

	/**
	 * Return a String representation of this data block.
	 */
	public String toString() {
		String s = "DataBlock [" + getName() + "]";
		return s;
	}

}
