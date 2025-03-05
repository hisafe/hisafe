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

import java.util.Collection;

import jeeb.lib.defaulttype.Extension;

/**
 * Filter Interface. Filters must be cloned when added to a Filtrer grouper:
 * several groupers may be used from different threads and should not rely on a
 * single Filter instance.
 */
public interface Filter extends Extension, Cloneable {

	/**
	 * Cloneable interface
	 */
	public Object clone ();
	
	/**
	 * Some filters may need the complete collection to be set correctly
	 */
	public void preset (Collection individuals) throws Exception;

	/**
	 * Return true if the filter keeps the given individual. This means that the
	 * object corresponds to the rules of the filter.
	 */
	public boolean retain (Object individual) throws Exception;

}
