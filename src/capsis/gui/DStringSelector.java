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
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;

/**
 * Dialog box to select a String among several.
 * 
 * @author F. de Coligny - august 2002
 */
public class DStringSelector extends AmapDialog implements ActionListener, ListSelectionListener {
	private final static int LIST_HEIGHT = 5;

	private String dlgTitle;
	private String dlgText;
	private Map<String, String> possibilities;
	private String selectedString;

	private JList list;

	private JButton ok;
	private JButton cancel;
	private JButton helpButton;


	/**
	 * Constructor.
	 */
	public DStringSelector (String title, String explanationText, Collection<String> choices) {
		super ();
		dlgTitle = title;
		dlgText = explanationText;
		
		selectedString = "";
		
		possibilities = new HashMap<String, String> ();
		for (String choice : choices ) {
			 
			String humanString = Translator.swap (choice);
			if (humanString.indexOf (".") != -1) {
				String classLittleName = AmapTools.getClassSimpleName (choice);
				humanString = Translator.swap (classLittleName);
			}
			possibilities.put (humanString, choice);
		}
		
		createUI ();
		
		pack ();
		show ();
		
	}

	public void okAction () {
		
		selectedString = (String) possibilities.get ((String) list.getSelectedValue ());
		
		// Back to the caller
		setValidDialog (true);
		
	}

	public void actionPerformed (ActionEvent evt) {
		
		// Ok button
		if (evt.getSource ().equals (ok)) {
			okAction ();
			
		// Cancel button
		} else if (evt.getSource ().equals (cancel)) {
			setValidDialog (false);
			
		// Help button
		} else if (evt.getSource ().equals (helpButton)) {
			Helper.helpFor (this);
		}
		
	}

	/** 
	 * Processe actions on an item in the models list. 
	 */
	public void valueChanged (ListSelectionEvent evt) {
		// nothing
	}

	/**
	 * Initialize the dialog's user interface.
	 */
	private void createUI () {
		
		// 1. Explanation text
		JLabel label = new JLabel (dlgText);
		
		// 2. Selection list
		list = new JList (possibilities.keySet ().toArray ());
		list.setVisibleRowCount (LIST_HEIGHT);
		list.setSelectedIndex (0);
		
		// 3. Control panel
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		ok = new JButton (Translator.swap ("Shared.ok"));
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		helpButton = new JButton (Translator.swap ("Shared.help"));
		pControl.add (ok);
		pControl.add (cancel);
		pControl.add (helpButton);
		ok.addActionListener (this);
		cancel.addActionListener (this);
		helpButton.addActionListener (this);
		
		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (label, BorderLayout.NORTH);
		getContentPane ().add (new JScrollPane (list), BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
		
		setTitle (dlgTitle);
		
		setModal (true);
		
	}

	public String getSelectedString () {return selectedString;}

}
