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
	private static final long serialVersionUID = 1L;

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
