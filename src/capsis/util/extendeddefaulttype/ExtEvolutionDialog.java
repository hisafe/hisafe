/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2015 LERFoB AgroParisTech/INRA 
 * 
 * Authors: M. Fortin, 
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
package capsis.util.extendeddefaulttype;

import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.border.Border;

import capsis.commongui.EvolutionDialog;
import capsis.commongui.util.Helper;
import capsis.gui.MainFrame;
import capsis.kernel.EvolutionParameters;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.util.extendeddefaulttype.methodprovider.DDomEstimateProvider;
import capsis.util.extendeddefaulttype.methodprovider.HDomEstimateProvider;
import jeeb.lib.util.Log;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Settings;
import repicea.gui.CommonGuiUtility;
import repicea.gui.CommonGuiUtility.FileChooserOutput;
import repicea.gui.Refreshable;
import repicea.gui.SynchronizedListening;
import repicea.gui.UIControlManager;
import repicea.gui.UIControlManager.CommonControlID;
import repicea.gui.components.NumberFormatFieldFactory;
import repicea.gui.components.NumberFormatFieldFactory.JFormattedNumericField;
import repicea.gui.components.NumberFormatFieldFactory.Range;
import repicea.gui.genericwindows.CancellableWarningMessage;
import repicea.io.REpiceaFileFilter;
import repicea.io.REpiceaFileFilter.FileType;
import repicea.io.REpiceaFileFilterList;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;


public abstract class ExtEvolutionDialog<M extends ExtModel> extends EvolutionDialog implements SynchronizedListening, ActionListener, Refreshable {
	
	public static enum BreakVariable implements TextableEnum {
		Date("Age or date (yrs)", "Age ou date (ann\u00E9es)"),
		D0("Dominant diameter (cm)", "Diam\u00E8tre dominant (cm)"),
		H0("Dominant height (m)", "Hauteur dominante (m)"),
		;

		BreakVariable(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}
		
		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
		
		@Override
		public String toString() {return REpiceaTranslator.getString(this);}
	}

	protected static enum MessageID implements TextableEnum {
		BreakVariableMessage("Grow until", "Faire \u00E9voluer jusqu'\u00E0"),
		ProjectionNoLongerThan("Growth projections cannot exceed ", "La projection de croissance ne peut exc\u00E9der "),
		ProjectionYear(" years!"," ans!"),
		WarningMessageLength("Growth forecasts over more than 60 years are not recommended. Beyond this limit the reliability of the predictions decreases.", 
				"Il est recommand\u00E9 de limiter la dur\u00E9e des simulations \u00E0 60 ans. Au-del\u00E0 de cette dur\u00E9e, la pr\u00E9cision des pr\u00E9visions d\u00E9cro\u00EEt fortement."),
		LoadAScenario("Use a predifined scenario", "Utiliser un sc\u00E9nario pr\u00E9d\u00E9fini"),
		ScenarioFilename("Scenario File:", "Fichier de sc\u00E9nario :"),
;
		
		MessageID(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}
		
		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
		
		@Override
		public String toString() {
			return REpiceaTranslator.getString(this);
		}
		
	}

	protected JButton ok;
	protected JButton cancel;
	protected JButton help;

	protected JButton browse;

	protected String defaultScenarioPath;
	protected JTextField fldFileName;	// name 
	protected String filename;
	
	protected JRadioButton loadPredefinedScenario;
	
	protected final M model;

	protected Step step;									// step from which to grow may be null
	
	protected JFormattedNumericField numberOfYears;
	
	protected final JComboBox<BreakVariable> breakVariableSelector;


	/**
	 * Main constructor.
	 */
	private ExtEvolutionDialog(M model, boolean guiMode) {
		super();
		this.model = model;
		try {
			setIconImages(MainFrame.getInstance().getIconImages());
		} catch (Error e) {}
		ok = UIControlManager.createCommonButton(CommonControlID.Ok);
		cancel = UIControlManager.createCommonButton(CommonControlID.Cancel);
		help = UIControlManager.createCommonButton(CommonControlID.Help);

		numberOfYears = NumberFormatFieldFactory.createNumberFormatField(NumberFormatFieldFactory.Type.Double, Range.StrictlyPositive, false);
		numberOfYears.setHorizontalAlignment(SwingConstants.RIGHT);
		
		fldFileName = new JTextField(25);
		browse = UIControlManager.createCommonButton(CommonControlID.Browse);
		
		loadPredefinedScenario = new JRadioButton(MessageID.LoadAScenario.toString());
		loadPredefinedScenario.setSelected(!guiMode); // by default is set to true when not in guiMode
		loadPredefinedScenario.setEnabled(guiMode);	// by default is set to false when not in guiMode
		List<BreakVariable> eligibleBreakVariables = new ArrayList<BreakVariable>();
		MethodProvider mp = this.model.getMethodProvider();
		eligibleBreakVariables.add(BreakVariable.Date);
		if (mp instanceof DDomEstimateProvider) {
			eligibleBreakVariables.add(BreakVariable.D0);
		}
		if (mp instanceof HDomEstimateProvider) {
			eligibleBreakVariables.add(BreakVariable.H0);
		}
		breakVariableSelector = new JComboBox<BreakVariable>();
		breakVariableSelector.setModel(new DefaultComboBoxModel<BreakVariable>(eligibleBreakVariables.toArray(new BreakVariable[]{})));
	}

	/**
	 * Constructor for script mode.
	 * @param model a ExtModel derived instance
	 */
	protected ExtEvolutionDialog(M model) {
		this(model, false);	// not in gui mode
	}

	/**
	 * Constructor in gui mode.
	 * @param s a Step instance
	 */
	protected ExtEvolutionDialog(Step s) {
		this((M) s.getProject().getModel(), true); // in gui mode
		this.step = s;
	}

	/**
	 * This method returns either the current step date or the initial date of the simulation if
	 * the step is null.
	 * @return an integer
	 */
	protected int getDate() {
		if (step != null) {
			return step.getScene().getDate();
		} else {
			ExtInitialParameters initParms = model.getSettings();
			return initParms.getInitialDateYr();
		}
	}

	/**
	 * Initialization common to the two previous constructors
	 */
	protected void loadDefaultValues() {
		if (model != null) {
			defaultScenarioPath = Settings.getProperty(getPropertyName("scenariopath"), model.getSettings().getInventoryPath());
			/*
			 * The following while loop ensures that the last File.separator
			 * inherited from the Settings.getProperty ("capsis.inventory.path",  Options.getDir ("data")) is deleted
			 */
			while (defaultScenarioPath.endsWith(File.separator)) {
				int lastFileSeparatorPosition = defaultScenarioPath.lastIndexOf(File.separator);
				defaultScenarioPath = defaultScenarioPath.substring(0, lastFileSeparatorPosition);
			}

			if ((defaultScenarioPath == null) || defaultScenarioPath.equals ("")) {
				defaultScenarioPath = PathManager.getInstallDir();
			}
			int index = Settings.getProperty(getPropertyName("breakindex"), 0);
			if (index > breakVariableSelector.getModel().getSize() - 1) {
				breakVariableSelector.setSelectedIndex(0);
			} else {
				breakVariableSelector.setSelectedIndex(index);
			}
			Double value = Settings.getProperty(getPropertyName("maxbreak"), 100d);
			numberOfYears.setText(value.toString());
		}
	}

	protected String getPropertyName(String property) {
		String modelName = model.getIdCard().getModelName().toLowerCase().trim();
		return "evolutiondialog." + modelName + "." + property;
	}
	
	protected boolean performFieldChecks() {
		if (fldFileName.getText().length() == 0) {
			JOptionPane.showMessageDialog(this, 
					ExtInitialDialog.MessageID.FileNameMustNotBeEmpty.toString(), 
					UIControlManager.InformationMessageTitle.Error.toString(),
					JOptionPane.ERROR_MESSAGE);
			return false;
		}

		if (!new File (fldFileName.getText()).isFile ()) {
			JOptionPane.showMessageDialog(this, 
					ExtInitialDialog.MessageID.IsNotAFile.toString(), 
					UIControlManager.InformationMessageTitle.Error.toString(),
					JOptionPane.ERROR_MESSAGE);
			return false;
		}

		FileType currentFileType = REpiceaFileFilter.getFileType(fldFileName.getText());
		if (currentFileType != FileType.DBF && currentFileType != FileType.CSV) {
			JOptionPane.showMessageDialog(this, 
					ExtInitialDialog.MessageID.FileIsNotDBForCSV.toString(), 
					UIControlManager.InformationMessageTitle.Error.toString(),
					JOptionPane.ERROR_MESSAGE);
			return false;
		}

		String filename = fldFileName.getText();
		Settings.setProperty(getPropertyName("scenariopath"), filename);
		try {
			setEvolutionParameters(createEvolutionParametersFromFilename(filename));
		} catch (IOException e) {
			JOptionPane.showMessageDialog(this, 
					UIControlManager.InformationMessage.ErrorWhileLoadingData.toString(), 
					UIControlManager.InformationMessageTitle.Error.toString(),
					JOptionPane.ERROR_MESSAGE);
			return false;
		}
		return true;
	}

	/**
	 * This method is used by derived class to create an EvolutionParameters instance from
	 * a filename.
	 * @param filename
	 * @return an EvolutionParameters instance
	 */
	protected abstract EvolutionParameters createEvolutionParametersFromFilename(String filename) throws IOException;

	
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource().equals(loadPredefinedScenario)) {
			refreshInterface();
		} else if (evt.getSource ().equals (browse)) {
			try {
				REpiceaFileFilterList fileFilters = new REpiceaFileFilterList(REpiceaFileFilter.DBF, REpiceaFileFilter.CSV);
				FileChooserOutput fileChooserOutput = CommonGuiUtility.browseAction(this, 
						JFileChooser.FILES_ONLY, 
						fldFileName.getText(),
						fileFilters,
						JFileChooser.OPEN_DIALOG);
				fldFileName.setText(fileChooserOutput.getFilename());
			} catch (Exception e) {
				Log.println(Log.ERROR, "ArtEvolutionDialog.actionPerformed",
						"Error while opening the file chooser. Consult the log", e);
				e.printStackTrace();
			}
		} else if (evt.getSource ().equals (ok)) {
			okAction();
		} else if (evt.getSource ().equals (cancel)) {
			setValidDialog(false);
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor(this);
		} 
	}

	
	protected abstract void okAction();
	
	/**
	 * This method initializes the dialog.
	 */
	public abstract void init();

	protected abstract boolean isProjectionLengthWarningEnabled();

	protected abstract void setProjectionLengthWarningEnabled(boolean b);

	@Override
	public void refreshInterface() {
		boolean bool = loadPredefinedScenario.isSelected();
		numberOfYears.setEnabled(!bool);
		fldFileName.setEnabled(bool);
		browse.setEnabled(bool);
		breakVariableSelector.setEnabled(!bool);
	}

	/**
	 * This method checks whether the number of steps is ok
	 */
	protected boolean checkNumberOfSteps(ExtEvolutionParametersList<? extends ExtEvolutionParameters> oVec) {
		int timeStep = model.getSettings().getSimulationSettings().getTimeStep();
		int maxNumberOfSteps = model.getSettings().getSimulationSettings().getMaxNumberOfSteps();
		
		int initialDate = 0;
		int currentDate = getDate();
		
		ExtInitialParameters initParms = (ExtInitialParameters) model.getSettings();
		initialDate = initParms.getInitialDateYr();
		
		if (step == null) {			
			currentDate = initialDate; 
		}
		
		int n = 0;		// n: number of steps
		for (ExtEvolutionParameters evolParam : oVec) {
			n += evolParam.getNbSteps();
		}
		
		// The following condition ensures that the projection does not exceed the maximum number of steps
		if (n * timeStep + currentDate - initialDate > maxNumberOfSteps * timeStep)  {
			String message = ExtEvolutionDialog.MessageID.ProjectionNoLongerThan.toString() + maxNumberOfSteps * timeStep + ExtEvolutionDialog.MessageID.ProjectionYear.toString();
			JOptionPane.showMessageDialog(this, 
					message, 
					UIControlManager.InformationMessageTitle.Warning.toString(), 
					JOptionPane.WARNING_MESSAGE);
			return false;
		} 
		
		// the following condition enables a warning dialog if the projection exceeds 60 years
		if ((n * timeStep + currentDate - initialDate > 60) && isProjectionLengthWarningEnabled()) {
			CancellableWarningMessage warningDlg = new CancellableWarningMessage(this, MessageID.WarningMessageLength.toString());
			if (warningDlg.isCancelled())
				return false;
			if (warningDlg.isWarningDisabled()) {
				setProjectionLengthWarningEnabled(false);			// warning is disabled
			}
		}
		return true;
	}

	@Override
	public void listenTo() {
		ok.addActionListener(this);
		cancel.addActionListener(this);
		help.addActionListener(this);
		loadPredefinedScenario.addActionListener(this);
		browse.addActionListener(this);
	}

	@Override
	public void doNotListenToAnymore() {
		ok.removeActionListener (this);
		cancel.removeActionListener (this);
		help.removeActionListener (this);
		loadPredefinedScenario.removeActionListener(this);
		browse.removeActionListener(this);
	}

	@Override
	public void setValidDialog(boolean isValid) {
		doNotListenToAnymore();
		super.setValidDialog(isValid);
	}
	
	protected JPanel getControlPanel() {
		JPanel pControl = new JPanel(new FlowLayout (FlowLayout.RIGHT));
		pControl.add(ok);
		pControl.add(cancel);
		pControl.add(help);
		return pControl;
	}
	
	
	protected JPanel getMainPanel() {
		Border etchedBorder = BorderFactory.createEtchedBorder();
		
		JPanel breakPanel = new JPanel();
		breakPanel.add(breakVariableSelector);
		breakPanel.add(Box.createHorizontalStrut(10));
		breakPanel.add(new JLabel(">"));
		breakPanel.add(Box.createHorizontalStrut(10));
		breakPanel.add(numberOfYears);
		numberOfYears.setColumns(10);
		breakPanel.add(Box.createHorizontalStrut(5));
		
		Border titledBorder = BorderFactory.createTitledBorder(etchedBorder, MessageID.BreakVariableMessage.toString());
		breakPanel.setBorder(titledBorder);
		
		
		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel,BoxLayout.Y_AXIS));
		mainPanel.add(breakPanel);
		return mainPanel;
	}

}
