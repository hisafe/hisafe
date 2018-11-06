/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2001 Francois de Coligny
 * 
 * This program is free software; you can redistribute it and/or modify it under the terms of the
 * GNU General Public License as published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with this program; if
 * not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */
package safe.extension.standviewer.safe;

import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.ButtonGroup;
import javax.swing.JRadioButton;

import jeeb.lib.util.Translator;
import safe.model.SafeStand;
import capsis.kernel.Step;

/**
 * LightOptionPanel - A component to choose LIGHT state variable to display
 * 
 * @author N.Marchais - September 2003
 */
public class LightOptionPanel extends GenericOptionPanel {

	// Light model options
	public int nbTimeStep;
	public boolean directOption;

	/**
	 * Constructor.
	 */
	public LightOptionPanel (SVSafeSettings sets, Step step, SVSafe svSafe) {

		super (sets, svSafe, Translator.swap ("SVSafe.OptionPanel.lightOption"));

		rdGroup = new ButtonGroup ();
		radButton = new JRadioButton[nbTimeStep + 3];

		radButton[0] = new JRadioButton (Translator.swap ("SVSafe.OptionPanel.lightOption1"));
		radButton[1] = new JRadioButton (Translator.swap ("SVSafe.OptionPanel.lightOption2"));
		radButton[2] = new JRadioButton (Translator.swap ("SVSafe.OptionPanel.lightOption3"));
		rdGroup.add (radButton[0]);
		rdGroup.add (radButton[1]);
		rdGroup.add (radButton[2]);

		// direct option is inra daily tilme step repartition
		if (directOption) {
			for (int i = 0; i < nbTimeStep; i++) {
				radButton[3 + i] = new JRadioButton (Translator.swap ("SVSafe.OptionPanel.lightOption4") + " "
						+ (i + 1));
				rdGroup.add (radButton[3 + i]);
			}
		} else
			nbTimeStep = 0;

		for (int i = 0; i < nbTimeStep + 3; i++) {
			if (settings.lightChoice == (i + 1)) {
				rdGroup.setSelected (radButton[i].getModel (), true);
			}
		}

		LightItemListener listener = new LightItemListener ();
		for (int i = 0; i < nbTimeStep + 3; i++) {
			radButton[i].addItemListener (listener);
			panel.add (radButton[i]);

		}
	}

	public int getNbTimeStep () {
		return nbTimeStep;
	}


	// Listener class
	//
	private class LightItemListener implements ItemListener {

		public void itemStateChanged (ItemEvent e) {
			for (int i = 0; i < nbTimeStep + 3; i++) {
				if (e.getSource () == radButton[i]) {
					settings.lightChoice = i + 1;
					svSafe.updateLateralPanel ();
				}
			}
		}
	}

}
