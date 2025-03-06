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
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import capsis.commongui.command.NewProject;
import capsis.commongui.command.OpenProject;
import capsis.commongui.command.OpenSession;
import capsis.commongui.command.ShutdownHook;
import capsis.extension.generictool.assistant.AutomationEditor;

/**	WhatDoYouWantToDo can be shown at beginning time to get the user choice.
*	@author F. de Coligny - may 2002
*/
public class WhatDoYouWantToDo extends AmapDialog implements ActionListener {
	
	private JRadioButton restoreSession;	// appears only if ShutdownHook.applicationEndedAbnormally ()
	private JRadioButton openSession;
	private JRadioButton openProject;
	private JRadioButton newProject;
	private JRadioButton runAutomation;
	
	private JFrame frame;
	
	private JCheckBox showWelcomeToCapsisDialog;

	private JButton ok;
	private JButton close;

	
	/**	Constructor
	*/
	public WhatDoYouWantToDo (JFrame frame) {
		super (frame);
		
		this.frame = frame;
		createUI ();
		setPreferredSize (new Dimension (400, 280));
		
		activateSizeMemorization(getClass ().getName ());
		activateLocationMemorization(getClass ().getName ());
		
		pack ();
		setVisible (true);
	}

	private void okAction () {
		
		setVisible (false);
		try {
			// On startup
			Settings.setProperty ("capsis.what.do.you.want.to.do", ""+showWelcomeToCapsisDialog.isSelected ());
			
			if (restoreSession != null && restoreSession.isSelected ()) {
				Settings.setProperty ("capsis.what.do.you.want.to.do.default.options", "restore.session");
				ShutdownHook.getInstance ().restoreRescueSession ();
				
			} else if (openSession.isSelected ()) {
				Settings.setProperty ("capsis.what.do.you.want.to.do.default.options", "open.session");
				OpenSession o = new OpenSession (MainFrame.getInstance ());
				o.execute ();
				if (o.getStatus () != 0) {throw new Exception ();}
				
			} else if (openProject.isSelected ()) {
				Settings.setProperty ("capsis.what.do.you.want.to.do.default.options", "open.project");
				OpenProject o = new OpenProject (MainFrame.getInstance ());
				o.execute ();
				if (o.getStatus () != 0) {throw new Exception ();}
				
			} else if (newProject.isSelected ()) {
				Settings.setProperty ("capsis.what.do.you.want.to.do.default.options", "new.project");
				NewProject n = new NewProject (MainFrame.getInstance ());
				n.execute ();
				if (n.getStatus () != 0) {throw new Exception ();}
				
			} else if ( runAutomation.isSelected ()) {
				Settings.setProperty ("capsis.what.do.you.want.to.do.default.options", "run.automation");
				new AutomationEditor (frame);
				
				
			}
		} catch (Exception e) {
			setVisible (true);
			return;
		}
		
		// Clean the rescue session for next time
		ShutdownHook.getInstance ().disableRescueSession ();
		dispose ();
		
	}

	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (ok)) {
			okAction ();
		} else if (evt.getSource ().equals (close)) {
			// On startup
			Settings.setProperty ("capsis.what.do.you.want.to.do", ""+showWelcomeToCapsisDialog.isSelected ());
			ShutdownHook.getInstance ().disableRescueSession ();
			
			setValidDialog (false);
		}
	}

	/**	Initialize the dialog's GUI
	*/
	private void createUI () {
		
		ColumnPanel part1 = new ColumnPanel (5, 0);
		part1.setMargin (5);
	
		LinePanel l0 = new LinePanel ();
		l0.add (new JLabel (Translator.swap ("WhatDoYouWantToDo.whatDoYouWantToDo")));
		l0.setUnderlined1 ();
		l0.addGlue ();
		part1.add (l0);

		ButtonGroup group1 = new ButtonGroup ();
		
		LinePanel l4 = new LinePanel ();
		newProject = new JRadioButton (Translator.swap ("WhatDoYouWantToDo.newProject"));
		group1.add (newProject);
		l4.add (newProject);
		l4.addGlue ();
		part1.add (l4);

		LinePanel l3 = new LinePanel ();
		openProject = new JRadioButton (Translator.swap ("WhatDoYouWantToDo.openProject"));
		group1.add (openProject);
		l3.add (openProject);
		l3.addGlue ();
		part1.add (l3);
		
		LinePanel l5 = new LinePanel ();
		runAutomation = new JRadioButton (Translator.swap ("WhatDoYouWantToDo.runAutomation"));
		group1.add (runAutomation);
		l5.add (runAutomation);
		l5.addGlue ();
		part1.add (l5);
		
		if (ShutdownHook.getInstance ().applicationEndedAbnormally ()) {
			LinePanel l1 = new LinePanel ();
			restoreSession = new JRadioButton (Translator.swap ("WhatDoYouWantToDo.restoreSession"));
			group1.add (restoreSession);
			l1.add (restoreSession);
			l1.addGlue ();
			part1.add (l1);
		}
		
		LinePanel l2 = new LinePanel ();
		openSession = new JRadioButton (Translator.swap ("WhatDoYouWantToDo.openSession"));
		group1.add (openSession);
		l2.add (openSession);
		l2.addGlue ();
		part1.add (l2);
		
		if (ShutdownHook.getInstance ().applicationEndedAbnormally ()) {
			restoreSession.setSelected (true);
			
		} else {
			String option = Settings.getProperty ("capsis.what.do.you.want.to.do.default.options", "");
			
			if ("open.session".equals (option)) {
				openSession.setSelected (true);
			} else if ("open.project".equals(option)) {
				openProject.setSelected (true);
			} else if ("new.project".equals (option)) {
				newProject.setSelected (true);
			} else if ("run.automation".equals (option)) {
					runAutomation.setSelected (true);
			} else {	// default if nothing selected
				newProject.setSelected (true);
			}
			
		}

		LinePanel l10 = new LinePanel ();
		showWelcomeToCapsisDialog = new JCheckBox (Translator.swap ("WhatDoYouWantToDo.showWelcomeToCapsisDialog"));
		boolean yep = Settings.getProperty ("capsis.what.do.you.want.to.do", true);
		showWelcomeToCapsisDialog.setSelected (yep);
		l10.add (showWelcomeToCapsisDialog);
		l10.addGlue ();
		
		// Control panel
		LinePanel controlPanel = new LinePanel ();
		controlPanel.addGlue ();
		ok = new JButton (Translator.swap ("Shared.ok"));
		ImageIcon icon = IconLoader.getIcon ("ok_16.png");
		ok.setIcon(icon);
		
		close = new JButton (Translator.swap ("Shared.close"));
		icon = IconLoader.getIcon ("cancel_16.png");
		close.setIcon(icon);
		
		ok.addActionListener (this);
		close.addActionListener (this);
		controlPanel.add (ok);
		controlPanel.add (close);
		controlPanel.addStrut0 ();
		
		setDefaultButton (ok);	// from AmapDialog
		
		JPanel aux = new JPanel (new BorderLayout ());
		aux.add (part1, BorderLayout.NORTH);
		aux.add (l10, BorderLayout.SOUTH);
		
		getContentPane ().add (aux, BorderLayout.CENTER);
		getContentPane ().add (controlPanel, BorderLayout.SOUTH);
		
		setTitle (Translator.swap ("WhatDoYouWantToDo.welcomeToCapsis"));
		
		setModal (true);
	}

}



