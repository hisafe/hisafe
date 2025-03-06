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


/**
 * DialogItem - One dialog among several boxes. They can be
 * selected from one to another with "next" and "previous" buttons.
 *
 * @author F.de Coligny - october 2001
 */
public interface DialogItem {

	public boolean isPending ();	// i.e. saidNext () or saidPrevious ()
	public boolean saidNext ();		// "next" button was hit
	public boolean saidPrevious ();	// "previous" button was hit
	public void dispose ();
	public void setVisible (boolean b);
	
}



