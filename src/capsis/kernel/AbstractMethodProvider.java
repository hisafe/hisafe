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
package capsis.kernel;

/**
 * AbstractMethodProvider: an abstract superclass for the method providers. A
 * method provider proposes conventional methods (listed in shared interfaces)
 * to be make some Capsis modules compatible with external tools (e.g.
 * DataExtractors / charts).
 * 
 * @author F. de Coligny - April 2001, September 2010
 */
abstract public class AbstractMethodProvider implements MethodProvider {

	// nb-27.08.2018
	// private static final long serialVersionUID = 1L;

	/**
	 * Default constructor.
	 */
	public AbstractMethodProvider() {
	}

}