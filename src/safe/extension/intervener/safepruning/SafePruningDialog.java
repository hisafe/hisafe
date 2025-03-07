/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
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

