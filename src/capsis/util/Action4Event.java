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

import java.awt.event.ActionEvent;

/**	A general purpose Action Event.
*	Carries up to 4 parameters to its listeners.
*	
*	@author F. de Coligny - october 2006
*/
public class Action4Event extends ActionEvent {
	
	// The event can contain up to 4 piece of information
	private Object first;
	private Object second;
	private Object third;
	private Object fourth;
	
	public Action4Event (Object source, String command) {
		super (source, -1, command);
	}
	
	public void set1 (Object v) {first = v;}
	public void set2 (Object v) {second = v;}
	public void set3 (Object v) {third = v;}
	public void set4 (Object v) {fourth = v;}
	
	public Object get1 () {return first;}
	public Object get2 () {return second;}
	public Object get3 () {return third;}
	public Object get4 () {return fourth;}
	
}
