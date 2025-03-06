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

import java.awt.GridLayout;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.border.Border;

import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**
 * DisplayChoicePanel - A component to choose display option (crop-ground-light)
 *
 * @author N.Marchais - September 2003
 */
public class DisplayChoicePanel extends JPanel {
// review - fc - 11.3.2004

	private ButtonGroup rdGroup1;
	private JRadioButton rdLight, rdCrop, rdGround;
	private SVSafe svSafe;
	private SVSafeSettings settings;


	/**	Constructor.
	*/
	public DisplayChoicePanel (SVSafeSettings sets, SVSafe svSafe) {

		settings = sets;
		this.svSafe = svSafe;

		rdCrop= new JRadioButton (Translator.swap ("SVSafe.OptionPanel.cellOption1"));
		rdLight = new JRadioButton (Translator.swap ("SVSafe.OptionPanel.cellOption2"));
		rdGround = new JRadioButton (Translator.swap ("SVSafe.OptionPanel.cellOption3"));

		rdGroup1 = new ButtonGroup ();

		rdGroup1.add (rdCrop);
		rdGroup1.add (rdLight);
		rdGroup1.add (rdGround);

		if (settings.cellOption == 1) {//Crop roots
			rdGroup1.setSelected (rdCrop.getModel (), true);
		} else if (settings.cellOption == 2) {//light
			rdGroup1.setSelected (rdLight.getModel (), true);
		} else if (settings.cellOption == 3) {//ground content
			rdGroup1.setSelected (rdGround.getModel (), true);
		}

		DisplayChoiceListener listener = new DisplayChoiceListener ();
		rdCrop.addItemListener (listener);
		rdLight.addItemListener (listener);
		rdGround.addItemListener (listener);

		LinePanel cellPanel = new LinePanel ();		// fc - 8.3.2004
		Border etched = BorderFactory.createEtchedBorder ();
		Border cellBorder = BorderFactory.createTitledBorder (etched, Translator.swap ("SVSafe.OptionPanel.choice"));
		cellPanel.setBorder (cellBorder);
		cellPanel.add (rdCrop);
		cellPanel.add (rdLight);
		cellPanel.add (rdGround);
		cellPanel.addGlue ();	// fc - 8.3.2004

		setLayout (new GridLayout (1, 1));	// fc - 8.3.2004
		add (cellPanel);
	}


	//	Listener class
	//
	private class DisplayChoiceListener implements ItemListener {
		public void itemStateChanged (ItemEvent e) {
			if (e.getSource () == rdCrop) {
				if (settings.cellOption != 1) {
					settings.cellOption = 1;
				}
			} else if (e.getSource () == rdLight) {
				if (settings.cellOption !=2 ) {
					settings.cellOption = 2;
				}
			} else if (e.getSource () == rdGround) {
				if (settings.cellOption != 3){
					settings.cellOption = 3;
				}
			}
			svSafe.updateLateralPanel ();
		}
	}
}
