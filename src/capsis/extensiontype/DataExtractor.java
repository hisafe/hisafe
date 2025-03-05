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

import jeeb.lib.defaulttype.Extension;
import capsis.kernel.GModel;
import capsis.kernel.Step;

/**
 * The super interface for all data extractors.
 * 
 * @author F. de Coligny - December 1999
 */
public interface DataExtractor extends Extension {

	public void init(GModel m, Step s) throws Exception;

	/**
	 * Effectively process the extraction. Should only be called if needed (time
	 * consuming). One solution is to trigger real extraction by data renderer
	 * paintComponent (). So, the work will only be done when needed. Return false
	 * if trouble.
	 */
	public boolean doExtraction() throws Exception;
	// added throws Exception, fc-20.1.2014 trying to prevent an extractor from
	// blocking Capsis

	/**
	 * Calls doExtraction and in case of trouble, reset the configutation properties
	 * and recalls it (storing and restoring configuration property values may cause
	 * problems in new project with different input data, e.g. locus names may
	 * change and restoring such names may cause trouble)
	 */
	// fc-1.8.2018 See AbstractDataExtractor for an implementation
	public boolean doExtractionFailSafe() throws Exception;

	public void setColor(Color forcedColor);

	public String getDefaultDataRendererClassName();

	public void setRank(int r);

	// fc-25.7.2021
	// --- This looks like configuration properties and might be moved with the
	// others...
	public void setGrouperName(String grouperName);

	// fc-25.7.2021 MOVED to ExtractorConfiguration
//	public boolean hasConfigProperty(String property);

	public String getComboProperty(String property);
	// ... This looks like configuration properties and might be moved with the
	// others ---

	public Step getStep();

	// added throws Exception, fc-20.1.2014 trying to prevent an extractor from
	// blocking Capsis
	boolean update(Step fromStep, Step toStep) throws Exception;

	public String getName();

	public Color getColor();

	public String getCaption();

	public Collection<String> getDocumentationKeys();

	public void dispose(); // fc-2.1.2017 to release step button colors

	// fc-5.9.2019 Used when adding a step in a dataBlock
	public void setDataBlock(DataBlock db);

	// fc-11.10.2023 Added, useful for specific configuration conflicts detection
	// between several extractors in the same block (J. Sainte-Marie)
	public DataBlock getDataBlock();

	/**
	 * An extractor may refuse to be added to a block already containing other
	 * extractors with incompatible configuration possibilities (See Cstab, has a
	 * comboProperty with possible values depending on the setup loaded file). This
	 * must be called before init (). incomingInitStep is the step on which the
	 * extractor will be inited just after if it accepts.
	 */
	public boolean acceptsToBeAddedToThisDataBlock(DataBlock db, Step incomingInitStep);

}
