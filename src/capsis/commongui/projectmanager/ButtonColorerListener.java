/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2017 INRA 
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
package capsis.commongui.projectmanager;

/**
 * ButtonColorerListener must be implemented by objects listening to the
 * ButtonColorer
 * 
 * @author F. de Coligny - April 2010
 */
public interface ButtonColorerListener {

	/**
	 * Called when the user moved a color from a StepButton to another in the
	 * ProjectManager
	 */
	public void colorMoved(StepButton previousButton, StepButton newButton);

	/**
	 * Called when the user removed the color of a StepButton in the
	 * ProjectManager
	 */
	public void colorRemoved(StepButton stepButton);

	/**
	 * Returns true if the listener listens to the given stepButton (if no one
	 * listens to the StepButton, its color might be removed).
	 */
	public boolean isListening(StepButton sb); // fc-2.1.2017

}
