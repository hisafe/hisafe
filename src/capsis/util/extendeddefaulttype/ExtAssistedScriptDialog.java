/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2013 LERFoB AgroParisTech/INRA 
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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.lang.reflect.Constructor;
import java.text.NumberFormat;
import java.util.Iterator;
import java.util.Vector;
import java.util.concurrent.CancellationException;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JProgressBar;
import javax.swing.SwingWorker;

import capsis.commongui.util.HelpPageImmutable;
import capsis.commongui.util.Helper;
import capsis.util.extendeddefaulttype.ExtModel.InfoType;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Settings;
import repicea.gui.UIControlManager;
import repicea.gui.UIControlManager.CommonControlID;
import repicea.io.tools.REpiceaExportToolDialog;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;


public class ExtAssistedScriptDialog extends AmapDialog implements ActionListener,
														PropertyChangeListener,
														HelpPageImmutable {
	static {
		UIControlManager.setTitle (ExtAssistedScriptDialog.class, "Processing the script", "Ex\u00E9cution d'un script");
	}
	
	
	protected static enum MessageID implements TextableEnum {
		
		Start("Start", "D\u00E9marrer"),
		Next("Next >", "Suivant >"),
		Previous("< Previous", "< Pr\u00E9c\u00E9dent"),
		MultiThreadOption("Enable multithreading", "Activer le mode traitement multifil"),
		MultiThreadOptionToolTip("Enable many tasks to be carried out simultaneously", "Permet d'ex\u00E9cuter plusieurs t\u00E2ches de mani\u00E8re simultan\u00E9e."),
		SetOutputFilename("Export File", "Fichier d'extrants"),
		OutputInventory("Select an export file", "Choisissez un fichier d'extrants"),
		TaskList("Task list", "Liste des t\u00E2ches"),
		StandCount("Stand count", "D\u00E9compte des placettes"),
		WarningScript("Executing the script (", "Ex\u00E9cution du script ("),
		ScriptCancel("The script has been canceled!", "Le script a \u00E9t\u00E9 annul\u00E9 !"),
		ScriptSuccessfullyTerminated("The script has been successfully executed in ", "Le script a \u00E9t\u00E9 ex\u00E9cut\u00E9 avec succ\u00E8s en "),
		ScriptAborted("The script has been aborted for the following reason: ", "Le script a \u00E9t\u00E9 interrompu pour la raison suivante :"),
		Strata(" strata", " strates"),
		Stratum(" stratum", " strate"),
		ReadingInventoryFile("Reading inventory file", "Lecture du fichier d'inventaire"),
		ReadingEvolutionParameters("Reading evolution parameters", "Lecture des param\u00E8tres d'\u00E9volution"),
		ReadingExportParameters("Reading export parameters", "Lecture des param\u00E8tres d'exportation"),
		ExceptionDuringStandLoad("An error occurred while loading the inventory file.", "Une erreur est survenue durant le chargement des donn\u00E9es d'inventaire.");
		
		MessageID(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}
		
		@Override
		public void setText (String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
		
		@Override
		public String toString() {
			return REpiceaTranslator.getString(this);
		}
	}


	private ExtAssistedScriptParameters scriptParameters;
	private ExtModel model;

	private int m_iNbStrata = 0;

	private double initialTime;
	private double elapsedTime = 0;
	private NumberFormat formatter;

	// UI buttons and booleans
	private JButton help;
	private JButton next;
	private JButton previous;
	private JButton cancel;
	private JButton start;
	private JCheckBox multiThreadOption;

	private Vector<JLabel> taskVector = new Vector<JLabel>();
	private int m_iCurrentTask = 0;


	JLabel labelNumberOfStrata = new JLabel();


	// progress bar features
	private JProgressBar progressBarStratum;

	// thread that handles the script
	private ExtAssistedScriptJobManager script;


	public ExtAssistedScriptDialog(ExtInitialDialog caller, ExtModel model) {
		super(caller);

		this.model = model;
		scriptParameters = new ExtAssistedScriptParameters(model);

		formatter = NumberFormat.getNumberInstance();
		formatter.setMinimumIntegerDigits(2);
		formatter.setMaximumIntegerDigits(2);
		formatter.setMinimumFractionDigits(0);
		formatter.setMaximumFractionDigits(2);


		start = new JButton(MessageID.Start.toString());
		ImageIcon icon = IconLoader.getIcon ("ok_16.png");
		start.setIcon(icon);
		start.addActionListener(this);

		next = new JButton(MessageID.Next.toString());
		next.addActionListener(this);
		previous = new JButton (MessageID.Previous.toString());
		previous.addActionListener(this);

		cancel = UIControlManager.createCommonButton (CommonControlID.Cancel);
		cancel.addActionListener(this);

		help = UIControlManager.createCommonButton (CommonControlID.Help);
		help.addActionListener(this);

		progressBarStratum = new JProgressBar();
		progressBarStratum.setBorder(BorderFactory.createEtchedBorder());
		progressBarStratum.setStringPainted(true);
		progressBarStratum.setFont(ExtSimulationSettings.LABEL_FONT);
		progressBarStratum.setString(formatTime(elapsedTime));

		multiThreadOption = new JCheckBox(MessageID.MultiThreadOption.toString());
		multiThreadOption.setToolTipText(MessageID.MultiThreadOptionToolTip.toString());
		multiThreadOption.setSelected(Settings.getProperty("QuebecMRNFScriptDialog.multiThreadOption", true));		

		setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);		// dialog cannot be closed with the "x" icon

		createUI();
		pack();
		show();
	}

	/**
	 * This method is part of the ActionListener interface
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource().equals(start)) {
			try {
				progressBarStratum.setMinimum(0);
				progressBarStratum.setMaximum(100);

				Settings.setProperty("QuebecMRNFScriptDialog.multiThreadOption", multiThreadOption.isSelected());
				multiThreadOption.setEnabled(false);
				scriptParameters.setEnableMultiThreading(multiThreadOption.isSelected());

				script = new ExtAssistedScriptJobManager(scriptParameters, true);	// true export to file enabled
				script.addPropertyChangeListener(this);
				script.execute();
			} catch (Exception e) {
				Log.println(Log.ERROR, "QuebecMRNFScriptDialog.actionPerformed", "Error while running the script", e);
				MessageDialog.print(this, e.toString());
				setCurrentTask(--m_iCurrentTask);
			}
			initialTime = System.currentTimeMillis();
			start.setEnabled(false);
			previous.setEnabled(false);
		} else if (evt.getSource().equals(cancel)) {
			cancelAction();
		} else if (evt.getSource().equals(next)) {
			switch(m_iCurrentTask) {
			case 0:					// read the dbf file
				try {
					try {
						((ExtRecordReader) model.getSettings().getRecordReader()).setStrataSelectionEnabled(false);	// the user cannot select a stratum
						model.getSettings().initImport();			
					} catch (CancellationException cancelled) {
						return;
					}
					setCurrentTask(++m_iCurrentTask);
				} catch (Exception e) {
					if (!(e.getMessage().compareTo("DBFImportCancel") == 0)) {				// if the error message is not cancel then the error window pops up
						Log.println (Log.ERROR, "QuebecMRNFScriptDialog.actionPerformed()", "Error while loading stand" );
						MessageDialog.print(this, MessageID.ExceptionDuringStandLoad.toString () + " Cause(s) : " + e.getMessage() +".");
					}
				}
				return;
			case 1:					// read the scenario
				try {
					//Class<?> c = ClassLoader.getSystemClassLoader().loadClass(model.getInfoMap().get(InfoType.EVOLUTION_DIALOG));
					//Class<?> c = ClassLoader.getSystemClassLoader().loadClass(model.getSettings().getSimulationSettings().getInfoMap().get(InfoType.EVOLUTION_DIALOG));
					Class<?> c = getClass().getClassLoader().loadClass(model.getSettings().getSimulationSettings().getInfoMap().get(InfoType.EVOLUTION_DIALOG));
					Constructor<?> constructor = c.getConstructor(model.getClass());
//					ExtEvolutionDialog evolDlg = (ExtEvolutionDialog) c.newInstance();
					ExtEvolutionDialog evolDlg = (ExtEvolutionDialog) constructor.newInstance(model);
					evolDlg.init();
					if (!evolDlg.isValidDialog()) {
						evolDlg.dispose();
					} else {
						scriptParameters.setEvolutionParameters(evolDlg.getEvolutionParameters());
						setCurrentTask(++m_iCurrentTask);
					}
				} catch (Exception e) {
					Log.println(Log.ERROR, "QuebecMRNFScriptDialog.actionPerformed", "Error while instantiating the Evolution dialog", e);
				}
				return;
			case 2:					// set the level of output info
				try {
					//Class<?> c = ClassLoader.getSystemClassLoader().loadClass(model.getInfoMap().get(InfoType.EXPORT));
//					Class<?> c = ClassLoader.getSystemClassLoader().loadClass(model.getSettings().getSimulationSettings().getInfoMap().get(InfoType.EXPORT));
					Class<?> c = getClass().getClassLoader().loadClass(model.getSettings().getSimulationSettings().getInfoMap().get(InfoType.EXPORT));
					ExtExportTool exportTool = (ExtExportTool) c.newInstance();
//					exportTool.setMultipleSelection(false);
					exportTool.setMultipleSelection(true);   // MF20200903 Changed to true to allow multiple selections
					exportTool.init(model, null);
					((REpiceaExportToolDialog) exportTool.getUI(this)).setTriggerRecordSetAndSaveManually(true);
					exportTool.showUI(this);
					if (!exportTool.isCanceled()) {
						scriptParameters.setExportTool(exportTool);
						setCurrentTask(++m_iCurrentTask);
					}
				} catch (Exception e) {
					Log.println(Log.ERROR, "QuebecMRNFScriptDialog.actionPerformed", "Error while instantiating the Export dialog", e);
				}
				return;
			} 
		} else if (evt.getSource().equals(previous)) {
			setCurrentTask(--m_iCurrentTask);
		} else if (evt.getSource().equals(help)) {
			Helper.helpFor(this);	// fc - 19.10.2001
		}
	}


	/**
	 * Sets the value of the progress bar
	 * @param i
	 */
	private void setValue(int i) {
		progressBarStratum.setValue(i);
		elapsedTime = (System.currentTimeMillis() - initialTime)*0.001; 
		progressBarStratum.setString(formatTime(elapsedTime));
	}

	/**
	 * Format the timer
	 */
	private String formatTime(double sec) {
		int hours = (int) (sec/3600.0);
		sec = sec - hours*3600.0;
		int minutes = (int) (sec/60.0);
		int seconds = (int) (sec - minutes*60.0);
		String time = formatter.format((double) hours)+
				":"+
				formatter.format((double) minutes)+
				":"+
				formatter.format((double) seconds);
		return time;
	}

	/**
	 * Creates the UI
	 */
	private void createUI() {
		setTitle(UIControlManager.getTitle(getClass()));

		ColumnPanel c1 = new ColumnPanel();
		initializeTaskVector();

		JLabel labelTitle = new JLabel(MessageID.TaskList.toString());
		labelTitle.setFont(ExtSimulationSettings.LABEL_FONT);
		c1.add(labelTitle);

		if (!taskVector.isEmpty()) {
			for (Iterator<JLabel> label = taskVector.iterator(); label.hasNext();) {
				c1.add(label.next());
			}
		}

		c1.addStrut1();
		c1.add(multiThreadOption);
		c1.addStrut1();

		JLabel progressTitle = new JLabel(REpiceaTranslator.getString(UIControlManager.InformationMessageTitle.Progress));
		progressTitle.setFont(ExtSimulationSettings.LABEL_FONT);
		c1.add(progressTitle);
		c1.add(new JLabel(MessageID.StandCount.toString ()));
		LinePanel l1 = new LinePanel();
		l1.addStrut0();
		l1.add(progressBarStratum);
		c1.add(l1);


		getContentPane().setLayout(new BorderLayout());
		LinePanel controlPanel = new LinePanel();
		controlPanel.add(previous);
		controlPanel.add(next);
		controlPanel.add(start);
		controlPanel.add(cancel);
		controlPanel.add(help);

		getContentPane().add(c1, BorderLayout.CENTER);
		getContentPane().add(controlPanel, BorderLayout.SOUTH);
		setMinimumSize(new Dimension(500,310));
		setModal(true);
	}


	/**
	 * Initializes the vector that contains the different task
	 */
	private void initializeTaskVector() {
		taskVector.clear();
		// task 0  -  Read the inventory file
		taskVector.add(new JLabel(MessageID.ReadingInventoryFile.toString()));
		// task 1  -  Read the scenario
		taskVector.add(new JLabel(MessageID.ReadingEvolutionParameters.toString()));
		// task 2  -  Read the exporting details
		taskVector.add(new JLabel(MessageID.ReadingExportParameters.toString()));
		refreshNbStrata();
		taskVector.add(labelNumberOfStrata);
		setCurrentTask(0);	// set to first task;
	}

	private void refreshNbStrata() {
		if (m_iNbStrata > 1) {
			labelNumberOfStrata.setText(MessageID.WarningScript.toString() + m_iNbStrata + MessageID.Strata.toString() + ")");
		} else {
			labelNumberOfStrata.setText(MessageID.WarningScript.toString () + m_iNbStrata + MessageID.Stratum.toString() + ")");
		}
	}

	private void setCurrentTask(int taskID) {
		if (!taskVector.isEmpty()) {
			for (int i = 0; i < taskVector.size(); i++) {
				if (i == taskID) {
					taskVector.get(i).setFont(ExtSimulationSettings.LABEL_FONT);
					taskVector.get(i).setForeground(Color.RED);
				} else {
					taskVector.get(i).setFont(ExtSimulationSettings.TEXT_FONT);
					taskVector.get(i).setForeground(Color.BLACK);
				}
			}
			if (taskID == taskVector.size()-1) {
				start.setEnabled(true);
				start.requestFocusInWindow();
				setDefaultButton(start);
				next.setEnabled(false);
			} else {
				start.setEnabled(false);
				next.setEnabled(true);
				next.requestFocusInWindow();
				setDefaultButton(next);
			}
			if (taskID == 0) {
				previous.setEnabled(false);
				m_iNbStrata = 0;
				refreshNbStrata();
			} else {
				m_iNbStrata = 1;							// default value
				if (model.getSettings().getRecordReader().getGroupList().size() > 1) {	 				// the inventory file has more than one stratum
					m_iNbStrata = model.getSettings().getRecordReader().getGroupList().size();
				}
				refreshNbStrata();
				previous.setEnabled(true);
			}
		}
	}

	public ExtAssistedScriptParameters getScriptParameters() {return scriptParameters;}

	/*
	 * Override of the method in the AmapDialog class.
	 */
	@Override
	protected void escapePressed() {
		cancelAction();
	}

	private void cancelAction() {
		if (script != null && script.getState() == SwingWorker.StateValue.STARTED) {
			script.setCancelRequested(true);
//			script.cancel(true);
		} else  {
			setValidDialog(false);
		}
	}


	private void disposeDialog() {
		if (script.hasBeenNormallyTerminated()) {
			if (script.isCancelRequested()) {
//			if (script.isCancelled()) {
				JOptionPane.showMessageDialog(getParent(), 
						MessageID.ScriptCancel.toString(), 
						UIControlManager.InformationMessageTitle.Warning.toString(), 
						JOptionPane.WARNING_MESSAGE);
			} else {
				double elapsedTime = (System.currentTimeMillis() - initialTime) * 0.001;
				JOptionPane.showMessageDialog(getParent(), 
						MessageID.ScriptSuccessfullyTerminated.toString() + " " + formatTime(elapsedTime) + " .",
						UIControlManager.InformationMessageTitle.Information.toString(), 
						JOptionPane.INFORMATION_MESSAGE);
			}
		} else {		// was aborted
			JOptionPane.showMessageDialog(getParent(),
					MessageID.ScriptAborted.toString() + script.getEndReport(), 
					UIControlManager.InformationMessageTitle.Error.toString(),
					JOptionPane.ERROR_MESSAGE);
		}
		try {
			//new LogBrowser(this);			// TODO FP open the log
		} catch (Exception e) {
			System.out.println("Unable to open the log browser");
		}
		setValidDialog(false);
	}

	@Override
	public String getHelpPageAddress() {
		return "quebecmrnf.gui.QuebecMRNFScriptDialog";
	}

	@Override
	public void propertyChange(PropertyChangeEvent evt) {
		if ("progress".equals(evt.getPropertyName())) {
			setValue((Integer) evt.getNewValue());
		} else if ("state".equals(evt.getPropertyName())
				&& SwingWorker.StateValue.DONE == evt.getNewValue()) {
			disposeDialog();
		}

	}

}


