/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package capsis.gui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import capsis.util.Controlable;

/**
 * DUser is a dialog box which can show a JPanel with a control panel Ok, Cancel, Help.
 * If Ok is hit, the dialog is valid (AmapDialog.isValidDialog () returns true).
 * (AmapDialog.isValidDialog () returns true).
 *
 * @author F. de Coligny - May 2000
 */
public class DialogWithOkCancel extends AmapDialog implements ActionListener {

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;

	protected JPanel panel;
	protected JButton ok;
	protected JButton cancel;
	protected JButton help;
	
	protected Component embedded;


	/**
	 * Constructor 1.
	 */
	public DialogWithOkCancel (JPanel pan) {
		this (pan, null);
	}

	/**
	 * Constructor 2.
	 */
	public DialogWithOkCancel (JPanel pan, String dialogTitle) {
		this (pan, dialogTitle, true);
	}

	/**
	 * Constructor 3.
	 */
	public DialogWithOkCancel (JPanel pan, String dialogTitle, boolean modal) {
	
		this(pan, dialogTitle, modal, BorderLayout.NORTH);
	}
	
	
	/**
	 * Constructor 3.
	 */
	public DialogWithOkCancel (JPanel pan, String dialogTitle, boolean modal, String layoutPosition) {
		super ();
		panel = pan;
		
		// fc-15.10.2010, trying to fix a bug in SVSamsara: preference dialog opens 
		// too small to show all options...
		activateSizeMemorization (this.getClass ().getName ());
		
		// fc-30.10.2014 also activated this option, convenient
		activateLocationMemorization(this.getClass ().getName ());
		
		createUI (layoutPosition);

		if (dialogTitle != null && !dialogTitle.equals ("")) {
			setTitle (dialogTitle);
		} else {
			setTitle (Translator.swap (panel.getClass ().getSimpleName ()));
		}
		
		setModal (modal);

		beforeShow ();

		// location is set by AmapDialog
		pack ();	// uses component's preferredSize
		setVisible (true);
	}

	/**
	 * May be redefined in subclasses.
	 */
	public void okAction () {

		// Panel may want to check some input before ok.
		// It is then responsible for displaying messages for the user to correct.
		if (panel instanceof Controlable) {
			if (!((Controlable) panel).isControlSuccessful ()) {return;}
		}

		setValidDialog (true);
	}

	/**
	 * Hook for subclasses.
	 */
	protected void beforeShow () {}

	/**
	 * May be redefined in subclasses.
	 */
	public void cancelAction () {setValidDialog (false);}

	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (ok)) {
			okAction ();
		} else if (evt.getSource ().equals (cancel)) {
			cancelAction ();
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}

	/**
	 * Initializes the GUI.
	 */
	private void createUI (String layout) {
		ImageIcon icon;
		
		// 1. taken in panel
		getContentPane ().setLayout (new BorderLayout ());

		// fc - 25.4.2007 - changed CENTER into NORTH
		// used for layout in SVLollypopConfigPanel
		//~ getContentPane ().add (panel, BorderLayout.CENTER);
		getContentPane ().add (panel, layout);
		// fc - 25.4.2007 - changed CENTER into NORTH

		// 2. control panel (ok cancel help);
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		ok = new JButton (Translator.swap ("Shared.ok"));
		icon = IconLoader.getIcon ("ok_16.png");
		ok.setIcon(icon);
		
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		icon = IconLoader.getIcon ("cancel_16.png");
		cancel.setIcon(icon);
		
		help = new JButton (Translator.swap ("Shared.help"));
		icon = IconLoader.getIcon ("help_16.png");
		help.setIcon(icon);
		
		
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

	/**	Embedder interface. 
	 */
	@Override
	public void setTitle (String newTitle) {
		super.setTitle (newTitle);
	}

	
	
	/**	Embedder interface. 
	 */
	@Override
	public void dispose () {
		super.dispose ();
	}

}

