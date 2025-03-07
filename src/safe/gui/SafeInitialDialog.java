/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE 
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
 * Hi-SAFE is free software under the terms of the CC-BY License as published by the Creative Commons Corporation
 *
 * You are free to:
 *		Share — copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt — remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution — 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
 *               
 * Notices:
 * 		You do not have to comply with the license for elements of the material in the public domain or where your use is permitted 
 *      by an applicable exception or limitation .
 *		No warranties are given. The license may not give you all of the permissions necessary for your intended use. 
 *		For example, other rights such as publicity, privacy, or moral rights may limit how you use the material.  
 *
 * For more details see <https://creativecommons.org/licenses/by/4.0/>.
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
