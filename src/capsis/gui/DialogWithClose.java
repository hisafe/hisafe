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
import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;

/**
 * DUser2 is a dialog box which can show a JPanel.
 * Control panel contains "Close" & "Help" buttons.
 * 
 * @author F. de Coligny - March 2001
 */
public class DialogWithClose extends AmapDialog implements ActionListener {

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;

	protected JComponent panel;	// 3.12.2002 - fc - changed JPanel by JComponent
	protected JButton close;
	protected JButton help;
	private boolean withControlPanel;
	
	protected Container embedded;

	/**
	 * Constructor 1.
	 */
	public DialogWithClose (Component caller, JComponent pan) {
		this (caller, pan, null, true, true);
	}
	
	/**
	 * Constructor 2.
	 */
	public DialogWithClose (Component caller, JComponent pan, String dialogTitle) {
		this (caller, pan, dialogTitle, true, true);
	}
	
	/**
	 * Constructor 3.
	 */
	public DialogWithClose (Component caller, JComponent pan, String dialogTitle, boolean modal) {
		this (caller, pan, dialogTitle, modal, true);
	}
	
	/**
	 * Constructor 4.
	 */
	public DialogWithClose (Component caller, JComponent pan, String dialogTitle, boolean modal, boolean withControlPanel) {
		super (AmapTools.getWindow ((Component) caller));
		init (pan, dialogTitle, modal, withControlPanel, false, false);
	}
	
	/**	
	 * Constructor 5.
	 */
	public DialogWithClose (Component caller, JComponent pan, String dialogTitle, boolean modal, boolean withControlPanel, boolean memoSize, boolean memoLocation) {
		super (AmapTools.getWindow ((Component) caller));
		init (pan, dialogTitle, modal, withControlPanel, memoSize, memoLocation);
	}
	
	/**	
	 * Constructor 6, if the caller is not a Component (should be avoided, leads to focus troubles).
	 */
	public DialogWithClose (Object caller, JComponent pan, String dialogTitle, boolean modal, boolean withControlPanel) {
		super ();  // no parent window available -> maybe focus trouble
		init (pan, dialogTitle, modal, withControlPanel, false, false);
	}
		
	/**	
	 * Initializes the dialog.	
	 */
	private void init(JComponent pan, String dialogTitle, boolean modal, boolean withControlPanel, boolean memoSize, boolean memoLocation) {

		this.withControlPanel = withControlPanel;
		panel = pan;
		
		createUI ();
		
		if (dialogTitle != null && !dialogTitle.equals ("")) {
			setTitle (dialogTitle);
		} else {
			setTitle (Translator.swap (panel.getClass ().getSimpleName ()));
		}
		
		setModal (modal);
		
		if (memoSize) {activateSizeMemorization(getClass ().getName ());}
		pack ();
		
		// fc-11.1.2021 Customise the location key with the dialogTitle
		if (memoLocation) {activateLocationMemorization(getClass ().getName () + "." + dialogTitle);}
//		if (memoLocation) {activateLocationMemorization(getClass ().getName ());}
		show ();
	}

	/** 
	 * May be redefined in subclasses.
	 */
	public void closeAction () { setValidDialog (true); }

	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (close)) {
			closeAction ();
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}

	/** 
	 * Initializes the GUI. 
	 */
	private void createUI () {
		
		ImageIcon icon;
		
		// 1. Taken in panel
		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (panel, BorderLayout.CENTER);
		
		// 2. Control panel (close help);
		if (withControlPanel) {
			JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
			
			close = new JButton (Translator.swap ("Shared.close"));
			icon = IconLoader.getIcon ("cancel_16.png");
			close.setIcon(icon);
			
			help = new JButton (Translator.swap ("Shared.help"));
			icon = IconLoader.getIcon ("help_16.png");
			help.setIcon(icon);
			
			
			pControl.add (close);
			pControl.add (help);
			close.addActionListener (this);
			help.addActionListener (this);
			getContentPane ().add (pControl, BorderLayout.SOUTH);
			
			// Set ok as default (see AmapDialog)
			close.setDefaultCapable (true);
			getRootPane ().setDefaultButton (close);
		}
		
	}

}

