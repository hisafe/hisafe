/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package safe.extension.ioformat.safeExport;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.WindowConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Question;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import capsis.gui.MainFrame;
import capsis.kernel.GScene;
import jeeb.lib.util.PathManager;
import capsis.util.WrapperPanel;


/**
 * SafeExportProfileCatalogEditor is a Dialog box for safe profiles management.
 *
 * @author R. Tuquet Laburre - august 2003
 */
public class SafeExportProfileCatalogEditor extends AmapDialog implements ActionListener, ListSelectionListener {

	private Vector subjects;
	private Map profiles;
	private String memoProfile;
	private String currentSelection;
	private SafeExportProfileCatalog catalog;

	private JList profileList;
	private JScrollPane profileScroll;

	private JButton exec;
	private JButton newProfile;
	private JButton modifyProfile;
	private JButton removeProfile;

	private JButton cancel;
	private JButton help;

	private GScene stand;

//INTERACTIVE MODE
	public SafeExportProfileCatalogEditor (Vector subjects, GScene stand) {
		super ();
		catalog = new SafeExportProfileCatalog ();
		this.subjects = subjects;
		this.stand = stand;
		currentSelection = null;

		profiles = catalog.getProfiles ();	// create some Maps...

		createUI ();
		update ("");
		if (profiles.size()>0) {
			profileList.setSelectedIndex(0);
			update((String) profileList.getSelectedValue());
		}

		setDefaultCloseOperation (WindowConstants.DO_NOTHING_ON_CLOSE);
		addWindowListener (new WindowAdapter () {
			public void windowClosing (WindowEvent evt) {
				cancelAction ();
			}
		});

		pack ();
		show ();
	}

	private void update(String item) {
		Vector items = new Vector (profiles.keySet ());
		Collections.sort(items);
		profileList = new JList (items);
		profileList.addListSelectionListener (this);
		profileList.setSelectionMode (ListSelectionModel.SINGLE_SELECTION);
		profileList.setFixedCellWidth (200);
		profileList.setVisibleRowCount (10);
		profileScroll.getViewport ().setView (profileList);

		// Set
		profileList.clearSelection();
		for (int i=0;i<items.size();i++){
			if (items.get(i).equals(item)) {
				profileList.setSelectedIndex(i);
				currentSelection=(String) items.get(i);
			}
		}


	}



	public String getCurrentSelection() {
		return currentSelection;
	}
	////////////////////////////////////////////


	// List selection
	//
	private void selectionAction (String selection) {
		currentSelection = selection;
	}




	/**
	 * List management.
	 */
	public void valueChanged (ListSelectionEvent evt) {
	// System.out.println ("valueChanged ()");
		if (evt.getSource ().equals (profileList)) {
			JList src = (JList) evt.getSource ();
			if (src.getModel ().getSize () == 0) {return;}	// maybe list is empty -> no selection possible

			String selection = (String) src.getSelectedValue ();

			// For one item selection, two evenements are generated : click and de-click
			// this feature hears only one
			if (!selection.equals (memoProfile)) {
				memoProfile = selection;
				selectionAction (selection);
			}
		}
	}

	/**
	 *
	 */
	protected void escapePressed () {
		cancelAction ();
	}

	private void execAction () {
		if (currentSelection == null) {
			MessageDialog.print (this, Translator.swap ("SafeExport.ErrorNoProfileIsSelected"));
			return;
		}
		if (!Question.ask (MainFrame.getInstance (),
				Translator.swap ("Shared.confirm"), Translator.swap ("SafeExport.confirmRun"))) {
			return;
		}
		setValidDialog(true);
	}

	private void cancelAction () {
		String val = "";
		if (profileList.getModel ().getSize () != 0) {
			val = ""+profileList.getSelectedValue ();
		}
		setValidDialog (false);
	}

	private void newProfileAction () {
		SafeExportProfile newProfile = new SafeExportProfile ();
		SafeExportProfileEditor dlg = new SafeExportProfileEditor (newProfile, subjects,"",stand);
		if (dlg.isValidDialog ()) {
			catalog.saveProfile (newProfile, dlg.getProfileName ());
			profiles = catalog.getProfiles ();
			update (dlg.getProfileName());
		}
		dlg.dispose ();
	}

	private void modifyProfileAction () {
		if (currentSelection == null) {return;}

		SafeExportProfile someProfile = (SafeExportProfile) profiles.get (currentSelection);
			
		SafeExportProfileEditor dlg = new SafeExportProfileEditor (someProfile, subjects,currentSelection,stand);
		if (dlg.isValidDialog ()) {
			catalog.deleteProfile(dlg.getProfileName());
			catalog.saveProfile (someProfile, dlg.getProfileName ());
			profiles = catalog.getProfiles ();
			update (dlg.getProfileName ());
		}
		dlg.dispose ();
	}

	private void removeProfileAction () {
		if (currentSelection == null) {return;}

		if (Question.ask (MainFrame.getInstance (),
				Translator.swap ("Shared.confirm"), Translator.swap ("SafeExport.confirmRemove"))) {

			catalog.deleteProfile (currentSelection);
			profiles = null; 
			profiles = catalog.getProfiles();
			update ("");
		}
	}

	/**
	 * Returns the profile with the given name.
	 */
	public SafeExportProfile getProfileFromName (String name) {
		return (SafeExportProfile) profiles.get (name);
	}

	
	/**
	 * Buttons management..
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (exec)) {
			execAction ();
		} else if (evt.getSource ().equals (newProfile)) {
			newProfileAction ();
		} else if (evt.getSource ().equals (modifyProfile)) {
			modifyProfileAction ();
		} else if (evt.getSource ().equals (removeProfile)) {
			removeProfileAction ();
		} else if (evt.getSource ().equals (cancel)) {
			cancelAction ();
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}

	//
	// Initializes the dialog's graphical user interface.
	//
	private void createUI () {

		//0. Left column
		JPanel left = new JPanel (new BorderLayout ());

		// 2. Profile list
		LinePanel l2 = new LinePanel ();
		profileList = new JList ();
		profileList.addListSelectionListener (this);
		profileList.setSelectionMode (ListSelectionModel.SINGLE_SELECTION);
		profileList.setFixedCellWidth (200);
		profileList.setVisibleRowCount (10);
		profileScroll = new JScrollPane (profileList);
		l2.add (profileScroll);
		l2.addStrut0 ();
		left.add (new WrapperPanel (l2, 2, 2), BorderLayout.CENTER);
		// 3. Right column
		JPanel right = new JPanel (new BorderLayout ());
		JPanel up = new JPanel (new GridLayout (4, 1, 4, 4));
		JPanel down = new JPanel (new GridLayout (2, 1, 4, 4));
		// 3.1 Select
		exec = new JButton (Translator.swap ("SafeExport.run")+"...");
		exec.setToolTipText (Translator.swap ("SafeExport.run"));
		exec.addActionListener (this);

		// 3.2 New profile
		newProfile = new JButton (Translator.swap ("SafeExport.new")+"...");
		newProfile.setToolTipText (Translator.swap ("SafeExport.new"));
		newProfile.addActionListener (this);

		// 3.3 Modify profile
		modifyProfile = new JButton (Translator.swap ("SafeExport.modify")+"...");
		modifyProfile.setToolTipText (Translator.swap ("SafeExport.modify"));
		modifyProfile.addActionListener (this);

		// 3.4 Remove profile
		removeProfile = new JButton (Translator.swap ("SafeExport.remove"));
		removeProfile.setToolTipText (Translator.swap ("SafeExport.remove"));
		removeProfile.addActionListener (this);

		// 3.5 Cancel
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		cancel.setToolTipText (Translator.swap ("Shared.cancel"));
		cancel.addActionListener (this);

		// 3.6 Help
		help = new JButton (Translator.swap ("Shared.help"));
		help.setToolTipText (Translator.swap ("Shared.help"));
		help.addActionListener (this);

		up.add (exec);
		up.add (newProfile);
		up.add (modifyProfile);
		up.add (removeProfile);

		down.add (cancel);
		down.add (help);

		right.add (new WrapperPanel (up, 2, 2), BorderLayout.NORTH);
		right.add (new WrapperPanel (down, 2, 2), BorderLayout.SOUTH);

		// Sets ok as default (see AmapDialog)
		setDefaultButton (exec);	// should ask for confirmation

		// General layout
		JPanel general = new JPanel (new BorderLayout ());
		general.add (left, BorderLayout.CENTER);
		general.add (right, BorderLayout.EAST);
		setContentPane (new WrapperPanel (general, 4, 4));

		setTitle (Translator.swap ("SafeExport.profileCatalog"));
		setModal (true);
		setResizable (true);
	}


}

