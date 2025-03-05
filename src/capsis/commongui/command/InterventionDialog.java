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
package capsis.commongui.command;

import capsis.kernel.extensiontype.Intervener;

/**
 * An interface for the dialogs managed by the Intervention command.
 * 
 * @author F. de Coligny - October 2010
 */
public interface InterventionDialog {

	/**
	 * The subclasses need a constructor with a JFrame and a Step.
	 */
	// public InterventionDialog (JFrame f, Step s);

	/**
	 * Returns true if all the user entries in this dialog were checked and are
	 * correct, i.e. if the process can continue with no errors expected.
	 */
	public boolean isValidDialog();

	/**
	 * Disposes the dislog when no more needed.
	 */
	public void dispose();

	/**
	 * Returns the intervener chosen and configured by the user, ready to use,
	 * e.g. in an intervention task.
	 */
	public Intervener getIntervener();

}
