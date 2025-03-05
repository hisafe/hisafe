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
package capsis.kernel.extensiontype;

import capsis.kernel.GModel;
import capsis.kernel.Step;
import jeeb.lib.defaulttype.Extension;

/**
 * An interface for ModelTool extensions which can be opened on a simulation
 * Step.
 * 
 * @author S. Dufour-Kowalski - January 2010
 */
public interface StepExtension extends Extension {

	/**
	 * A method to initialise the tool on the given simulation Step
	 * 
	 * @param model: the GModel object of the Project
	 * @param step: the simulation Step on which we init the extension
	 */
	public void init(GModel model, Step step);

}
