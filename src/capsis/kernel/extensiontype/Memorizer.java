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

import java.io.Serializable;

import capsis.kernel.Project;
import capsis.kernel.Step;
import jeeb.lib.defaulttype.Extension;

/**
 * Memorizers superclass. A memorizer will decide to keep the given Step in the
 * Project or not. A DefaultMemorizer may keep everything, while other
 * memorizers may keep only some steps to spare memory.
 * 
 * @author F. de Coligny - October 2002, September 2010
 */
public interface Memorizer extends Extension, Serializable {

	/**
	 * Memorizes the newStep in the project.
	 */
	public void memorize(Project s, Step fatherStep, Step newStep);

	/**
	 * Returns the memorizer caption.
	 */
	public String getCaption();

	/**
	 * To be called before an evolution stage.
	 */
	public void reinit();

}
