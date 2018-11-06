/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
package safe.extension.standviewer.safe;

import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.ButtonGroup;
import javax.swing.JRadioButton;

import jeeb.lib.util.Translator;
/**
 * GroundOptionPanel - A component to choose BELOW-GROUND state variable to display
 *
 * @author N.Marchais - September 2003
 */
public class GroundOptionPanel extends GenericOptionPanel {
// review - fc - 11.3.2004


	/**	Constructor.
	*/
	public GroundOptionPanel (SVSafeSettings sets, SVSafe svSafe) {
		super (sets, svSafe, Translator.swap ("SVSafe.OptionPanel.groundOption"));

		radButton = new JRadioButton[4];

		radButton[0] = new JRadioButton (Translator.swap ("SVSafe.OptionPanel.groundOption1"));
		radButton[1] = new JRadioButton (Translator.swap ("SVSafe.OptionPanel.groundOption2"));
		radButton[2] = new JRadioButton (Translator.swap ("SVSafe.OptionPanel.groundOption3"));
		radButton[3] = new JRadioButton (Translator.swap ("SVSafe.OptionPanel.groundOption4"));

		rdGroup = new ButtonGroup ();
		rdGroup.add (radButton[0]);
		rdGroup.add (radButton[1]);
		rdGroup.add (radButton[2]);
		rdGroup.add (radButton[3]);

		if (settings.groundChoice == 1) {
			rdGroup.setSelected (radButton[0].getModel (), true);
		} else if (settings.groundChoice == 2) {
			rdGroup.setSelected (radButton[1].getModel (), true);
		} else if (settings.groundChoice == 3) {
			rdGroup.setSelected (radButton[2].getModel (), true);
		} else if (settings.groundChoice == 4) {
			rdGroup.setSelected (radButton[3].getModel (), true);
		}

		GroundItemListener listener = new GroundItemListener ();
		radButton[0].addItemListener (listener);
		radButton[1].addItemListener (listener);
		radButton[2].addItemListener (listener);
		radButton[3].addItemListener (listener);

		panel.add (radButton[0]);
		panel.add (radButton[1]);
		panel.add (radButton[2]);
		panel.add (radButton[3]);
	}


	//	Listener class
	//
	private class GroundItemListener implements ItemListener {
		public void itemStateChanged (ItemEvent e) {
			if (e.getSource () == radButton[0]) {			// WATER
				settings.groundChoice = 1;
				svSafe.updateLateralPanel ();
			} else if (e.getSource () == radButton[1]) {	// NITROGEN
				settings.groundChoice = 2;
				svSafe.updateLateralPanel ();
			} else if (e.getSource () == radButton[2]) {	// TREE ROOTS
				settings.groundChoice = 3;
				svSafe.updateLateralPanel ();
			} else if (e.getSource () == radButton[3]) {	// CROP ROOTS
				settings.groundChoice = 4;
				svSafe.updateLateralPanel ();
			}
		}
	}

}
