/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2003  Francois de Coligny
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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

