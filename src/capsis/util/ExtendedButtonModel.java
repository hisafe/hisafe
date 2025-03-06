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

import javax.swing.DefaultButtonModel;

/**
 * This button model creates action events containing the expected
 * values for modifiers (CTRL, ALT, SHIF, META).
 * DefaultButtonModel does not.
 * 
 * @author ???
 */
public class ExtendedButtonModel extends DefaultButtonModel {
	private KeyModifiersProducer modifierProducer;

	public ExtendedButtonModel (KeyModifiersProducer p) {
		super ();
		modifierProducer = p;
	}

	/**
	* Sets the button to pressed or unpressed.
	* 
	* @param b true to set the button to "pressed"
	* @see #isPressed
	*/
	public void setPressed(boolean b) {
		if ((isPressed() == b) || !isEnabled()) {
		    return;
		}

		if (b) {
		    stateMask |= PRESSED;
		} else {
		    stateMask &= ~PRESSED;
		}

		if (!isPressed() && isArmed()) {
		    fireActionPerformed(
			new ActionEvent(this, ActionEvent.ACTION_PERFORMED,
					getActionCommand(), modifierProducer.getModifiers ())
			);
		}

		fireStateChanged();
	}   




}