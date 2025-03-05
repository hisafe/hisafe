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

import capsis.commongui.projectmanager.StepButton;
import capsis.extension.standviewer.StandViewerManager;
import capsis.kernel.GModel;
import capsis.kernel.Step;
import jeeb.lib.defaulttype.Extension;

/**
 * StandViewer - Superclass of Capsis Viewers.
 * 
 * @author F. de Coligny - December 1999
 */
public interface StandViewer extends Extension {

	/**
	 * Initializes the standViewer on the given step.
	 */
	public void init(GModel model, Step s, StepButton but) throws Exception;

	/**
	 * Stand viewers have an resettle method, called after constructor.
	 */
	// fc-14.12.2020 Renamed activate () in resettle () here. All Extensions
	// unused activate () methods are being removed, kept this one (used for
	// standViewers) but renamed it
	// fc-9.10.2020 Removed activate from Extension, found used activate () methods
	// in StandViewers, let them for a while, might be reviewed again, added this
	// method
	public void resettle();

	// fc-28.9.2021 Optional, a standViewerManager wants to be told when the
	// standViewer closes
	public void setStandViewerManager(StandViewerManager svMan);

	/**
	 * Updates the stand viewer
	 */
	// fc-28.9.2021 Added, was present in AbstractStandViewer, now accessible to all
	// StandViewers
	public void update();

}
