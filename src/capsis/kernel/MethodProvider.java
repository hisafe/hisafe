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

import java.io.Serializable;

/**	MethodProvider: interface of method providers (mp) for modules.
 * 	The mps are objects that contain a given method (or several), 
 * 	described in an interface. All objects implementing the interface
 * 	provide the method. E.g. GProvider is an interface with a method:  
 * 	<pre>public double getG (GScene stand, Collection trees);</pre> This means the 
 * 	objects implementing the interface are able to calculate the basal 
 * 	area (G) on a given scene, restricted to the given list of trees if
 * 	any.
 * 	The Capsis data extractors use this feature to check if they are 
 * 	compatible with a given module: they may test if the mp of the object 
 * 	(getMethodProvider () in the main model class) is instance of say 
 * 	GProvider. If yes, they may declare they are compatible with the module 
 * 	and they will be able to call the getG (...) method when needed to  
 * 	calculate the evolution of G or the G distribution.
 * 
 *	@author F. de Coligny - april 2001, september 2010
 */
public interface MethodProvider extends Serializable {

}
