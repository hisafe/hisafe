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

package capsis.extension;

import jeeb.lib.defaulttype.PaleoExtension;
import jeeb.lib.util.Log;

import java.util.HashSet;

import capsis.app.CapsisExtensionManager;
import capsis.extension.dataextractor.superclass.AbstractDataExtractor;
import capsis.kernel.extensiontype.GenericExtensionStarter;

/**
 * Superclass for all Old data extractors.
 *
 * @author F. de Coligny - November 2000
 */
abstract public class PaleoDataExtractor extends AbstractDataExtractor implements PaleoExtension {

	// fc-15.9.2021 Reviewed init () management
	
	/**
	 * Default constructor
	 */
	public PaleoDataExtractor() {
	}

	/**
	 * Legacy constructor of the PareoDataExtractors, takes a
	 * GenericExtensionStarter param.
	 */
	public PaleoDataExtractor(GenericExtensionStarter s) {

		// fc-15.9.2021 Restored the lines and adapted init() to return directly if
		// called several times, seems ok

		// fc-15.9.2021 Commenting the lines below results in problems with some
		// extractors like DETimeDbhCumulativePerSpecies running several instances of
		// DEDbhClassN, with Samsara2, when opening the multiConfPanel, exception in the
		// terminal, settings seems to be null...

		// fc-27.8.2021 Removed, at this time, settings is still null, too early. init
		// () will be called later, see DataBlock
		try {
			init(s.getModel(), s.getStep());

		} catch (Exception e) {
			Log.println(Log.ERROR, "PaleoDataExtractor.c ()", "Exception", e);
		}

	}

	/**
	 * Returns the type of the PaleoExtension.
	 */
	@Override
	public String getType() {
		return CapsisExtensionManager.DATA_EXTRACTOR;
	}

	/**
	 * Returns the class name of the PaleoExtension: getClass ().getName ().
	 */
	@Override
	public String getClassName() {
		return this.getClass().getName();
	}

}
