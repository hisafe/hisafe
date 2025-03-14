/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree�Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE 
 * 
 * Authors  
 * C.DUPRAZ       	- INRAE Montpellier France
 * M.GOSME       	- INRAE Montpellier France
 * G.TALBOT       	- INRAE Montpellier France
 * B.COURBAUD      	- INRAE Montpellier France
 * H.SINOQUET		- INRAE Montpellier France
 * N.DONES			- INRAE Montpellier France
 * N.BARBAULT 		- INRAE Montpellier France 
 * I.LECOMTE       	- INRAE Montpellier France
 * M.Van NOORDWIJK  - ICRAF Bogor Indonisia 
 * R.MULIA       	- ICRAF Bogor Indonisia
 * D.HARJA			- ICRAF Bogor Indonisia
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software under the terms of the CC-BY License as published by the Creative Commons Corporation
 *
 * You are free to:
 *		Share � copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt � remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution � 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions � You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
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
import capsis.kernel.Step;

/**
 * LightOptionPanel - A component to choose LIGHT state variable to display
 * 
 * @author N.Marchais - September 2003
 */
public class LightOptionPanel extends GenericOptionPanel {
	private static final long serialVersionUID = 1L;
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
