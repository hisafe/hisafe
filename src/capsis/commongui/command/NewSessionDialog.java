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

package capsis.commongui.command;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import jeeb.lib.util.PathManager;

/**	NewSessionDialog is a dialog box for new Session creation.
 * @author F. de Coligny - may 1999, may 2002, april 2010
 */
public class NewSessionDialog extends AmapDialog implements ActionListener {

	private String sessionName;
	private String defaultSessionDirectory;

	private JTextField zonSessionName;
	private JCheckBox firstProject;
	private JButton ok;
	private JButton cancel;
	private JButton help;
	
	private JFrame frame;

	
	/**	Default constructor.
	 */
	public NewSessionDialog (JFrame frame) {
		super (frame);
		this.frame = frame;
	
		defaultSessionDirectory = Settings.getProperty ("capsis.session.path", 
				PathManager.getInstallDir());
		
		createUI ();
		
		// location is set by AmapDialog
		pack ();	// validate (); does not work for this JDialog...
		setVisible (true);
		
	}

	private void okAction () {
		// classic checks...
		if (Check.isEmpty (zonSessionName.getText ())) {
			MessageDialog.print (frame, Translator.swap ("NewSessionDialog.sessionNameMustNotBeEmpty"));
		} else {
			sessionName = zonSessionName.getText ();
			
			// back to the caller
			setValidDialog (true);
		}
	}

	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (ok)) {
			okAction ();
		} else if (evt.getSource ().equals (cancel)) {
			setValidDialog (false);
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}

	/**	Initialize the dialog's GUI.
	 */ 
	private void createUI () {
		
		LinePanel l1 = new LinePanel ();
		l1.add (new JWidthLabel (Translator.swap ("NewSessionDialog.sessionName")+" :", 160));
		zonSessionName = new JTextField (15);
		l1.add (zonSessionName);
		l1.addStrut0 ();
		
		LinePanel l2 = new LinePanel ();
		firstProject = new JCheckBox (Translator.swap ("NewSessionDialog.createFirstProject"), true);
		l2.add (firstProject);
		l2.addGlue ();
		
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
		
		ColumnPanel part1 = new ColumnPanel ();
		part1.add (l1);
		part1.add (l2);
		part1.addGlue ();
		
		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (part1, BorderLayout.NORTH);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
		
		// sets ok as default (see AmapDialog)
		ok.setDefaultCapable (true);
		getRootPane ().setDefaultButton (ok);
		
		setTitle (Translator.swap ("NewSessionDialog.newSession"));
		
		setModal (true);
	}

	public String getSessionName () {
		return sessionName.trim ();
	}
	
	public void setSessionName (String nam) {
		sessionName = nam;
	}
	
	public JCheckBox getFirstProject () {
		return firstProject;
	}
	
}



