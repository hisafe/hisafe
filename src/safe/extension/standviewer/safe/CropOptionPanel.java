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
 * CropOptionPanel - A component to choose CROP state variable to display
 *
 * @author N.Marchais - September 2003
 */
public class CropOptionPanel extends GenericOptionPanel {
// review - fc - 11.3.2004


	/**	Constructor.
	*/
	public CropOptionPanel (SVSafeSettings sets, SVSafe svSafe) {
		super (sets, svSafe, Translator.swap ("SVSafe.OptionPanel.cropOption"));

		radButton = new JRadioButton[4];

		radButton[0] = new JRadioButton (Translator.swap ("SVSafe.OptionPanel.cropOption1"));
		radButton[1] = new JRadioButton (Translator.swap ("SVSafe.OptionPanel.cropOption2"));
		radButton[2] = new JRadioButton (Translator.swap ("SVSafe.OptionPanel.cropOption3"));
		radButton[3] = new JRadioButton (Translator.swap ("SVSafe.OptionPanel.cropOption4"));

		rdGroup = new ButtonGroup ();
		rdGroup.add (radButton[0]);
		rdGroup.add (radButton[1]);
		rdGroup.add (radButton[2]);
		rdGroup.add (radButton[3]);

		if (settings.cropChoice== 1) {
			rdGroup.setSelected (radButton[0].getModel (), true);
		} else if (settings.cropChoice == 2) {
			rdGroup.setSelected (radButton[1].getModel (), true);
		} else if (settings.cropChoice == 3) {
			rdGroup.setSelected (radButton[2].getModel (), true);
		} else if (settings.cropChoice == 4) {
			rdGroup.setSelected (radButton[3].getModel (), true);
		}

		CropItemListener listener = new CropItemListener ();
		radButton[0].addItemListener (listener);
		radButton[1].addItemListener (listener);
		radButton[2].addItemListener (listener);
		radButton[3].addItemListener (listener);

		panel.add (radButton[0]);
		panel.add (radButton[1]);
		panel.add (radButton[2]);
		panel.add (radButton[3]);
	}


	// Listener class
	//
	private class CropItemListener implements ItemListener {
		public void itemStateChanged (ItemEvent e)	{
			if (e.getSource () == radButton[0]) {			// LAI
				settings.cropChoice = 1;
				svSafe.updateLateralPanel ();
			} else if (e.getSource () == radButton[1]) {	// BIOMASS
				settings.cropChoice = 2;
				svSafe.updateLateralPanel ();
			} else if (e.getSource () == radButton[2]) {	// TEMPERATURE
				settings.cropChoice = 3;
				svSafe.updateLateralPanel ();
			} else if (e.getSource () == radButton[3]) {	// ROOT DEPTH
				settings.cropChoice = 4;
				svSafe.updateLateralPanel ();
			}
		}
	}

}
