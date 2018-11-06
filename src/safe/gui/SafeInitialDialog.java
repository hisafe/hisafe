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
import safe.model.SafeInitialParameters;
import safe.model.SafeInitialValues;
import safe.model.SafeModel;
import safe.model.SafePlotSettings;
import safe.model.SafeStand;
import capsis.commongui.InitialDialog;
import capsis.commongui.util.Helper;
import capsis.kernel.GModel;

/**
 * SafeInitialDialog - Dialog box to create the initial Safe stand.
 * 
 * 1) PlotOfCells can be initialize with inventory files or created with a specific interface
 * 
 * 2) Settings for each modules can be modified
 * 
 * 3) Initial values can be entered for soil - trees - crops - tree roots
 * 
 * When the user click OK, the stand is generated with all related objects.
 * 
 * @author Isabelle LECOMTE - August 2002
 * 
 */
public class SafeInitialDialog extends InitialDialog implements ActionListener {

	private SafeModel model;
	private SafeInitialParameters safeSettings;

	private SafePlotSettings plotSettings;
	private SafeInitialValues initialValues;

	private SafeStand initStand;

	private ButtonGroup group1;

	private JRadioButton loadPldFile;
	private JTextField pldFileName; // name of the plot description file
	private JButton browse;
	private JButton preload; // preload for edition (optional)


	// Control panel
	private JButton ok;
	private JButton cancel;
	private JButton help;

	/**
	 * Constructor.
	 */
	public SafeInitialDialog (GModel m) {

		super ();

		model = (SafeModel) m;
		safeSettings = (SafeInitialParameters) model.getSettings ();

		// plotSettings and initialValues must stay null at construction time fc-30.4.2014

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
	 * Preload (optional): can be used to edit the data before Ok. Note: if a .pld fileName is
	 * given, Ok can be hit directly without preloading.
	 */
	private void preloadAction () {

		if (plotSettings != null) {
			boolean confirm = Question.ask (this, Translator.swap ("SafeInitialDialog.warning"), Translator
					.swap ("SafeInitialDialog.theCurrentSceneWillBeLostPleaseConfirm"));
			if (confirm) {
				plotSettings = null;
			} else {
				return;
			}
		}

		try {
			// This loads the file in a fakeSettings
			SafeModel fakeModel = new SafeModel ();
			SafeInitialParameters fakeSettings = new SafeInitialParameters ();

			model.loadInitStand (pldFileName.getText ().trim (), fakeModel, fakeSettings);

			// Keep refs on the loaded information
			plotSettings = fakeSettings.plotSettings;
			initialValues = fakeSettings.initialValues;

			pldFileName.setEnabled (false);
			browse.setEnabled (false);

		} catch (Exception e) {

			MessageDialog.print (this, Translator.swap ("SafeInitialDialog.wrongPldFileName"));
			return;
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
			safeSettings.initialValues = initialValues;

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
			initStand = null;
			setValidDialog (false);
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}

		//synchro ();

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
		pldFileName.setText (Settings.getProperty ("safe.pld.path", ""));
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



		//synchro (); // enables / disables what must be done

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
