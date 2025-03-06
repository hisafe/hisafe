/*
 * Samsaralight library for Capsis4.
 * 
 * Copyright (C) 2008 / 2012 Benoit Courbaud.
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library;
 * if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 */

package capsis.lib.samsaralight;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JTextField;

import capsis.commongui.util.Helper;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;

/**
 * SLSettingsDialog is a dialog box for initial light parameters input. Initially based on
 * mountain.gui.SLSettingsDialog.
 * 
 * On Ok, data is checked for validity. If trouble, a message is sent and close is aborted. When
 * everything is ok, data are set in the SLSettings object, then dialog is set unvisible. Caller can
 * check for dialog status: isValidDialog () is true if Ok was hit and all tests were ok, false in
 * any other case (cancellation...). When the dialog is no longer useful, the caller must dispose
 * it.
 * 
 * @author B. Courbaud, N. Donès, M. Jonard, G. Ligot, F. de Coligny - October 2008 / June 2012
 */
public class SLSettingsDialog extends AmapDialog implements ActionListener {

	static {
		Translator.addBundle ("capsis.lib.samsaralight.SLSettingsDialog");
	}

	private SLSettings slSettings;

	private JTextField fileName;
	private JButton browse;

	private JButton ok;
	private JButton cancel;
	private JButton help;

	/**
	 * Constructor.
	 */
	public SLSettingsDialog (Window owner, SLSettings slSettings) {
		super (owner);

		this.slSettings = slSettings;

		createUI ();

		setSize (new Dimension (500, 200));

		show ();
	}

	/**
	 * Action on browse
	 */
	private void browseAction () {

		JFileChooser chooser = new JFileChooser (Settings.getProperty ("capsis.inventory.path", PathManager
				.getDir ("data")));
		// chooser.setFileSelectionMode ();
		int returnVal = chooser.showOpenDialog (this);
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			Settings.setProperty ("capsis.inventory.path", chooser.getSelectedFile ().toString ());
			fileName.setText (chooser.getSelectedFile ().toString ());
		}

	}

	/**
	 * Action on ok: tests and set valid if everything is ok.
	 */
	private void okAction () {

		// 1. Checks
		if (!Check.isFile (fileName.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("SLSettingsDialog.wrongFileName") + " : "
					+ fileName.getText ().trim ());
			return;
		}
		slSettings.setFileName(fileName.getText ().trim ());

		Settings.setProperty ("slsettingsdialog.fileName", slSettings.getFileName ());

		setValidDialog (true);
	}

	/**
	 * Manage the gui events.
	 */
	public void actionPerformed (ActionEvent evt) {

		if (evt.getSource ().equals (browse)) {
			browseAction ();

		} else if (evt.getSource ().equals (ok)) {
			okAction ();

		} else if (evt.getSource ().equals (cancel)) {
			setValidDialog (false);

		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}

	/**
	 * Create the user interface.
	 */
	private void createUI () {

		ColumnPanel part1 = new ColumnPanel ();

		LinePanel l1 = new LinePanel ();
		l1.add (new JWidthLabel (Translator.swap ("SLSettingsDialog.fileName") + " : ", 150));
		fileName = new JTextField (5);
		fileName.setText (Settings.getProperty ("slsettingsdialog.fileName", ""));

		l1.add (fileName);
		browse = new JButton (Translator.swap ("Shared.browse"));
		browse.addActionListener (this);
		l1.add (browse);
		l1.addStrut0 ();

		part1.add (l1);
		part1.addStrut0 ();

		// Control panel
		LinePanel controlPanel = new LinePanel ();
		controlPanel.addGlue ();
		ok = new JButton (Translator.swap ("Shared.ok"));
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		help = new JButton (Translator.swap ("Shared.help"));
		controlPanel.add (ok);
		controlPanel.add (cancel);
		controlPanel.add (help);
		controlPanel.addStrut0 ();
		ok.addActionListener (this);
		cancel.addActionListener (this);
		help.addActionListener (this);

		setDefaultButton (ok); // from AmapDialog

		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (part1, BorderLayout.NORTH);
		getContentPane ().add (controlPanel, BorderLayout.SOUTH);

		setTitle (Translator.swap ("SLSettingsDialog"));

		setModal (true);
	}

}
