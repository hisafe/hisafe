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

package capsis.defaulttype;

import jeeb.lib.defaulttype.Quantity;

/**
 * Interface for numberable objects.
 *
 * @author F. de Coligny - march 2001
 */
public interface Numberable extends Quantity {

	// fc-30.9.2020 Added the Quantity superinterface for Classifier enhancement

	/**
	 * Returns the number of occurrences of this object.
	 */
	public double getNumber();

	/**
	 * Sets the number of occurrences of this object.
	 */
	public void setNumber(double n);

}
