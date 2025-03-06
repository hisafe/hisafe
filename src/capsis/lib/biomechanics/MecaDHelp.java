/*
 * Biomechanics library for Capsis4.
 *
 * Copyright (C) 2001-2003  Philippe Ancelin.
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

package capsis.lib.biomechanics;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Translator;

/**
 * MecaDHelp - Interface for help dialog.
 *
 * @author Ph. Ancelin - october 2001
 */
public class MecaDHelp extends AmapDialog implements ActionListener {
//checked for c4.1.1_08 - fc - 3.2.2003
// NOTE: could be replaced by Helper.helpFor (this) for each help button - fc - 3.2.2003

	private JButton close;

	/*static {
		Translator.addBundle("capsis.lib.biomechanics.MecaDHelp");
	}*/


	/**
	 * Constructor.
	 */
	public MecaDHelp (AmapDialog parent, String title, String dialog, int width, int heigth) {
		super (parent);
		setTitle (title);

		createUI (dialog);
		// location is set by AmapDialog
		//~ setBounds(200, 200, width, heigth);	// fc - 31.1.2003
		
		setModal (false);
		pack ();
		show ();
	}

	/**
	 * Manage gui events.
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (close)) {
			dispose ();
		}
	}

	/**
	 * User interface definition.
	 */
	private void createUI (String dialog) {
		JPanel mainPanel = new JPanel ();
		mainPanel.setLayout (new BorderLayout ());
		// 1. Display to output informations
		JTextArea display = new JTextArea (dialog);
		JScrollPane scroll = new JScrollPane (display);
		mainPanel.add (scroll);

		// 2. Control panel
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.CENTER));
		close = new JButton (Translator.swap ("Shared.close"));
		close.addActionListener (this);
		pControl.add (close);

		// set close as default (see AmapDialog)
		close.setDefaultCapable (true);
		getRootPane ().setDefaultButton (close);

		// layout parts
		getContentPane ().add (mainPanel, BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
	}

}


