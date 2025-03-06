/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2024 INRAE 
 * 
 * Authors  
 * C.DUPRAZ       	- INRAE Montpellier France
 * M.GOSME       	- INRAE Montpellier France
 * G.TALBOT       	- INRAE Montpellier France
 * B.COURBAUD      	- INRAE Montpellier France
 * H.SINOQUET		- INRAE Montpellier France
 * N.DONES			- INRAE Montpellier France
 * N.BARBAULT 		- INRAE Montpellier France 
 * I.LECOMTE       	- INRAE Montpellier France
 * M.Van NOORDWIJK  - ICRAF Bogor Indonisia 
 * R.MULIA       	- ICRAF Bogor Indonisia
 * D.HARJA			- ICRAF Bogor Indonisia
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Hi-SAFE is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * If not, see <http://www.gnu.org/licenses/>.
 *
 */
package safe.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.FileAccessory;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Question;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import safe.model.SafeGeneralParameters;
import safe.model.SafeModel;
import safe.model.SafePlotSettings;
import capsis.commongui.InitialDialog;
import capsis.commongui.util.Helper;
import capsis.kernel.GModel;

/**
 * SafeInitialDialog - Dialog box to create the initial Safe stand.
 * WARNING : GUI interface IS NOT USED IN HI-sAFe
 * 
 * @author Isabelle LECOMTE - August 2002
 * 
 */
public class SafeInitialDialog extends InitialDialog implements ActionListener {

	private static final long serialVersionUID = 1L;
	
	private SafeModel model;
	private SafeGeneralParameters safeSettings;
	private SafePlotSettings plotSettings;
	
	//choose the pld file name
	private JRadioButton loadPldFile;
	private JTextField pldFileName; 

	//control buttons
	private ButtonGroup group1;
	private JButton ok;
	private JButton cancel;
	private JButton help;
	private JButton browse;

	/**
	 * Constructor.
	 */
	public SafeInitialDialog (GModel m) {

		super ();

		model = (SafeModel) m;
		safeSettings = (SafeGeneralParameters) model.getSettings ();

		// Paths initialisation
		Settings.setProperty ("safe.simulation.path", PathManager.getInstallDir () + "/data/safe/");
		Settings.setProperty ("safe.output.path", PathManager.getInstallDir () + "/data/safe/output/");

		// Activate AmapDialog features
		activateSizeMemorization (getClass ().getName ());
		activateLocationMemorization (getClass ().getName ());

		// Launch the user interface
		createUI ();
		setSize (new Dimension (750, 300));
		show ();
	}

	/**
	 * A radio button was pressed, check if something to lose and tell the user if needed.
	 */
	private void loadPldFileAction () {

		if (plotSettings != null) {
			boolean confirm = Question.ask (this, Translator.swap ("SafeInitialDialog.warning"), Translator
					.swap ("SafeInitialDialog.theCurrentSceneWillBeLostPleaseConfirm"));
			if (confirm) {
				plotSettings = null;
			} 
		}

	}

	/**
	 * Choose an existing plot description file
	 */
	private void browseAction () {

		if (plotSettings != null) {
			boolean confirm = Question.ask (this, Translator.swap ("SafeInitialDialog.warning"), Translator
					.swap ("SafeInitialDialog.theCurrentSceneWillBeLostPleaseConfirm"));
			if (confirm) {
				plotSettings = null;
			} else {
				return;
			}
		}

		JFileChooser chooser = new JFileChooser (Settings.getProperty ("safe.pld.path", PathManager.getDir ("data")));
		new FileAccessory (chooser);

		int returnVal = chooser.showOpenDialog (this);

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			String fileName = chooser.getSelectedFile ().toString ();
			Settings.setProperty ("safe.pld.path", fileName);
			pldFileName.setText (fileName);
		}

	}


	/**
	 * For launching the model
	 */
	private void okAction () {

		if (plotSettings == null && loadPldFile.isSelected ()) {

			if (!model.checkIfLoadable (pldFileName.getText ().trim ())) {
				MessageDialog.print (this, Translator.swap ("SafeInitialDialog.wrongPldFileName"));
				return;
			}

			safeSettings.pldFileName = pldFileName.getText ().trim ();
			Settings.setProperty ("safe.pld.path", safeSettings.pldFileName);

		} else if (plotSettings != null) {

			safeSettings.plotSettings = plotSettings;
		
		} else {

			MessageDialog.print (this, Translator.swap ("SafeInitialDialog.checkSceneCreation"));
			return;

		}

		try {
			safeSettings.buildInitScene (model);
			setInitialParameters (safeSettings);

		} catch (Exception e) {
			Log.println (Log.ERROR, "SafeInitialDialog.okAction ()", "Error during buildInitScene", e);
			MessageDialog.print (this, Translator.swap ("SafeInitialDialog.anUnexpectedErrorOccurredDuringInitialisationPleaseCheckTheLog"), e);
			return;
		}

		setValidDialog (true);
	}


	public void actionPerformed (ActionEvent evt) {

		if (evt.getSource ().equals (loadPldFile)) {
			loadPldFileAction ();
		} else if (evt.getSource ().equals (browse)) {
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
	 * Inits the GUI.
	 */
	private void createUI () {

		ColumnPanel part1 = new ColumnPanel ();

		// Scene creation
		ColumnPanel sceneCreation = new ColumnPanel (Translator.swap ("SafeInitialDialog.sceneCreation"));

		// Load pld file
		LinePanel l1 = new LinePanel ();
		loadPldFile = new JRadioButton (Translator.swap ("SafeInitialDialog.loadPldFile") + " :");
		loadPldFile.addActionListener (this);
		l1.add (loadPldFile);
		pldFileName = new JTextField ();
		pldFileName.setText ("");
		pldFileName.addActionListener (this);
		browse = new JButton (Translator.swap ("Shared.browse"));
		browse.addActionListener (this);

		l1.add (pldFileName);
		l1.add (browse);

		l1.addStrut0 ();
		sceneCreation.add (l1);
	
		sceneCreation.addStrut0 ();
		part1.add (sceneCreation);

		group1 = new ButtonGroup ();
		group1.add (loadPldFile);
		loadPldFile.setSelected (true);


		// Control panel
		JPanel controlPanel = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		ok = new JButton (Translator.swap ("Shared.ok"));
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		help = new JButton (Translator.swap ("Shared.help"));
		controlPanel.add (ok);
		controlPanel.add (cancel);
		controlPanel.add (help);
		ok.addActionListener (this);
		cancel.addActionListener (this);
		help.addActionListener (this);

		setDefaultButton (ok); // from AmapDialog

		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (part1, BorderLayout.NORTH);
		getContentPane ().add (controlPanel, BorderLayout.SOUTH);

		setTitle (Translator.swap ("SafeInitialDialog.safeInitialisation"));

		setModal (true);
	}


}
