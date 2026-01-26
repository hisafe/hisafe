/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE - CC-BY License
 * 
 * LIST OF AUTHORS
 * --------------- 
 * Christian Dupraz 1, Kevin J.Wolz 1 , Isabelle Lecomte 1, Grégoire Talbot 1, Nicolas Barbault 1, 
 * Grégoire Vincent 2 , Rachmat Mulia 3, François Bussière 4, Harry Ozier-Lafontaine 4,
 * Sitraka Andrianarisoa 1, Nick Jackson 5, Gerry Lawson 5, Nicolas Dones 6, Hervé Sinoquet 6,
 * Betha Lusiana 3, Degi Harja 3, Suzy Domenicano 7 , Francesco Reyes 1 , Marie Gosme 1 ,
 * Meine Van Noordwijk 3, Benoit Courbaud 8
 *
 * 1 INRA (UMR-ABSYS), University of Montpellier, 34090 Montpellier, France
 * 2 IRD (UMR-AMAP), University of Montpellier, 34090 Montpellier, France
 * 3 ICRAF, Bogor 16001, Indonesia
 * 4 INRA (UR ASTRO 1231) Centre Antilles-Guyane, Petit-Bourg, 97170 Guadeloupe, France
 * 5 CEH, NERC,Wallingford OX10 8BB, UK
 * 6 INRA (UMR-PIAF), Université Clermont Auvergne, 63000 Clermont-Ferrand, France
 * 7 Centre d’étude de la forêt, Université du Quebec, Montreal H2X 3Y5, Canada
 * 8 CEMAGREF, Mountain Ecosystems and Landcapes Research Unit, Saint-Martin-d’Hères, France
 *
 *----------------------------------------------------------------------------------------------
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software under the terms of the CC-BY License as published by the Creative Commons Corporation
 *
 * You are free to:
 *		Share — copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt — remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution — 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
 *               
 * Notices:
 * 		You do not have to comply with the license for elements of the material in the public domain or where your use is permitted 
 *      by an applicable exception or limitation .
 *		No warranties are given. The license may not give you all of the permissions necessary for your intended use. 
 *		For example, other rights such as publicity, privacy, or moral rights may limit how you use the material.  
 *
 * For more details see <https://creativecommons.org/licenses/by/4.0/>.
 *
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
	private static final long serialVersionUID = 1L;


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
