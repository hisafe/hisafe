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
 * 1 INRA (UMR-SYSTEM), University of Montpellier, 34090 Montpellier, France
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

package safe.extension.intervener.safepruning;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;

/**
 * This dialog box is used to set SafePruning parameters in interactive context
 * @author C Alauzet - July 2004
 */
public class SafePruningDialog extends AmapDialog implements ActionListener {
	private static final long serialVersionUID = 1L;
	private JTextField newHeight; 			//new crown base Height
	protected JButton ok;
	protected JButton cancel;
	protected JButton help;


	public SafePruningDialog () {
		super ();

		setTitle (Translator.swap ("SafePruningDialog.title"));

		
		setModal (true);

		createUI ();

		// location is set by AmapDialog
		pack ();	// uses component's preferredSize
		show ();
	}



	/**
	 * Accessor for pruning height
	 */
	public float getPruningHeight() {
		if (newHeight.getText ().trim ().length () == 0) {return 0;}
		return (float) Check.doubleValue (newHeight.getText ().trim ());
	}



	//
	// Action on ok button.
	//
	private void okAction () {

		if (!Check.isDouble (newHeight.getText ())) {
			MessageDialog.print (this, Translator.swap ("SafePruningDialog.error"));
			return;
		}

		double height = Check.doubleValue (newHeight.getText ());

		if (height <= 0) {
			MessageDialog.print (this, Translator.swap ("SafePruningDialog.error"));
			return;
		}

		setValidDialog (true);
	}

	//
	// Action on cancel button.
	//
	private void cancelAction () {setValidDialog (false);}

	/**
	 * Someone hit a button.
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (ok)) {
			okAction ();
		} else if (evt.getSource ().equals (cancel)) {
			cancelAction ();
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}

	//
	// Create the dialog box user interface.
	//
	private void createUI () {

		Box fond = Box.createVerticalBox();

		JPanel panel = new JPanel(new FlowLayout());
		JLabel l1 = new JLabel(Translator.swap ("SafePruningDialog.height"));
		panel.add(l1);
		newHeight = new JTextField(10);
		panel.add(newHeight);
		fond.add(panel);
		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (fond, BorderLayout.NORTH);

		// 2. control panel (ok cancel help);
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		ok = new JButton (Translator.swap ("Shared.ok"));
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		help = new JButton (Translator.swap ("Shared.help"));
		pControl.add (ok);
		pControl.add (cancel);
		pControl.add (help);
		ok.addActionListener (this);
		cancel.addActionListener (this);
		help.addActionListener (this);
		getContentPane ().add (pControl, BorderLayout.SOUTH);

		// sets ok as default (see AmapDialog)
		ok.setDefaultCapable (true);
		getRootPane ().setDefaultButton (ok);

	}

}

