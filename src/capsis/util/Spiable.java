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
package capsis.util;


/**	Objects implementing Spiable may be listened by a spy, see Spy.
*	@author F. de Coligny - december 2004
*/
public interface Spiable {
	
	/**	Set a spy on this objet
	*/
	public void setSpy (Spy s);
	
	/**	Get the spy of this object
	*/
	public Spy getSpy ();
	
	/**	Call this method when the spy should be informed of something.
	*	This method must call spy.action (this, something). Be careful, 
	*	spy may be null if no spy was (yet) set on this object.
	*/
	public void action (Object something);	// something may be null
	
}