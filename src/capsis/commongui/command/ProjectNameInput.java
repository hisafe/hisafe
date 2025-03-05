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
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**
 * Configuration Dialog for project name.
 *
 * @author F. de Coligny - october 2002
 */

public class ProjectNameInput extends AmapDialog implements ActionListener {
	
	private JTextField projectName;
	private JButton ok;
	private JButton cancel;
	
	
	/**
	 * Constructor.
	 */
	public ProjectNameInput (String currentProjectName) {
		super ();

		getContentPane ().setLayout (new BorderLayout ());
		LinePanel l1 = new LinePanel ();
		l1.add (new JLabel (Translator.swap ("ProjectNameInput.projectName")+" :"));
		projectName = new JTextField (5);
		projectName.setText (currentProjectName);
		l1.add (projectName);

		JPanel controlPanel = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		ok = new JButton (Translator.swap ("Shared.ok"));
		ok.addActionListener (this);
		setDefaultButton (ok);
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		cancel.addActionListener (this);
		controlPanel.add (ok);
		controlPanel.add (cancel);

		getContentPane ().add (l1, BorderLayout.NORTH);
		getContentPane ().add (controlPanel, BorderLayout.SOUTH);
		
		setTitle (Translator.swap ("ProjectNameInput"));
		
		setModal (true);
		
		pack ();
		show ();
	}
	
	public void okAction () {
		if (projectName.getText ().trim ().length () == 0) {
			JOptionPane.showMessageDialog (this, Translator.swap ("ProjectNameInput.invalidProjectName"), 
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}	
		setValidDialog (true);
	}
	
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (ok)) {
			okAction ();
		} else {
			setValidDialog (false);
		}
	}
	
	public String getProjectName () {return projectName.getText ();}
	
}

