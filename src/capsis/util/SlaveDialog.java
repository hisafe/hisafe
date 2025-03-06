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
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JRootPane;
import javax.swing.KeyStroke;


/**
 * A SlaveDialog calls update of its master when enter is hit on it.
 * 
 * @author F. de Coligny - november 2000
 */
public class SlaveDialog extends JDialog {
	private Updatable master;
	
	public SlaveDialog (java.awt.Dialog d, String title, boolean modal, Updatable master) {
		super (d, title, modal);
		this.master = master;
	}

	public SlaveDialog (java.awt.Frame f, String title, boolean modal, Updatable master) {
		super (f, title, modal);
		this.master = master;
	}

	// Enter key calls updateListener to update the master
	protected JRootPane createRootPane() {
		ActionListener updateListener = new ActionListener() {
			public void actionPerformed(ActionEvent actionEvent) {
				master.update (null, null);	// fc - 23.11.2007 - changed Updatable
			}
		};

		JRootPane rootPane = new JRootPane();
		KeyStroke stroke = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0);
		rootPane.registerKeyboardAction(updateListener, stroke,
				JComponent.WHEN_IN_FOCUSED_WINDOW);
		return rootPane;
	}
}

