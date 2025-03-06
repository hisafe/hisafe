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
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Collection;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

import capsis.commongui.InitialDialog;
import capsis.commongui.command.NewProjectDialog;
import capsis.commongui.util.Helper;
import capsis.kernel.Engine;
import capsis.kernel.GModel;
import capsis.kernel.IdCard;
import capsis.kernel.InitialParameters;
import capsis.kernel.ModelManager;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MemoPanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.gui.NorthPanel;

/**
 * DNewProject is a Dialog box for new Project creation.
 * 
 * @author F. de Coligny - May 1999, May 2002, May 2010
 */
public class DNewProject extends AmapDialog implements NewProjectDialog, ActionListener, ListSelectionListener {
	// complete review fc - 18.1.2010

	// nb-27.08.2018
	// inal long serialVersionUID = 1L;

	private Engine engine;
	private Collection<String> modelNames; // list of the model names
	private GModel model;

	private InitialParameters initialParameters;

	private String lastModelName; // to avoid two evts on one single click

	private JTextField projectName;
	private JList modelList;
	private JTextPane details; // details of the selected model

	private JButton initialize;
	private JButton cancel;
	private JButton help;

	private Icon infoIcon = IconLoader.getIcon("information_16.png");
	private Icon licenceIcon = IconLoader.getIcon("license_16.png");
	private JButton info;
	private JButton licence;

	// fc-30.4.2020 If not null, this is the name of the single model in the
	// ModelManager list and the gui looks a bit different
	// If several models in the ModelManager list, singleModelName is null
	private String singleModelName;

	/**
	 * Default constructor
	 */
	public DNewProject(JFrame frame) {
		super(frame);

		try {
			engine = Engine.getInstance();

			ModelManager mm = engine.getModelManager();
			modelNames = mm.getModelNames();
			modelNames = new TreeSet<String>(modelNames); // sorted list

			// fc-30.4.2020 Check if there is only one model
			if (modelNames.size() == 1)
				// From now on, isSingleModelCase() is true
				singleModelName = modelNames.iterator().next();

			createUI();

			if (isSingleModelCase())
				modelSelectedAction();

			activateSizeMemorization(getClass().getName());
			pack();
			setVisible(true);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DNewProject.c ()", "Error at construction time", e);
		}
	}

	private boolean isSingleModelCase() {
		return singleModelName != null;
	}

	private String getModelName() {

		// fc-30.4.2020
		if (isSingleModelCase()) {
			return singleModelName;

		} else {

			String modelName = (String) modelList.getSelectedValue();
			return modelName;

		}

	}

	/**
	 * Ensure project name is set
	 */
	private void checkProjectName() {

		if (Check.isEmpty(projectName.getText())) {
			// fc-3.2.2011 shorter project name is better everywhere -> default
			// name is now short
			String name = model.getPackageName().length() <= 3 ? model.getPackageName()
					: model.getPackageName().substring(0, 3);
			projectName.setText(name);
			// projectName.setText(model.getPackageName() + "_Unnamed");
		}

	}

	/**
	 * Retrieve the package name for the selected model in the list
	 */
	private String getSelectedModelPackageName() {

		String modelName = getModelName();
		// String modelName = (String) modelList.getSelectedValue();

		ModelManager mm = engine.getModelManager();
		try {
			return mm.getPackageName(modelName);
		} catch (Exception e) {
			return null;
		}

	}

	/**
	 * Try to load the model which is selected in the list
	 */
	private GModel getSelectedModel() {

		GModel m = null;
		String pkgName = getSelectedModelPackageName();

		StatusDispatcher.print(Translator.swap("DNewProject.loadingModel"));
		try {
			// ClassName is full class name ex: moutain.model.MountModel
			m = engine.loadModel(pkgName);
			StatusDispatcher.print(Translator.swap("Shared.done"));

		} catch (Exception e) {
			Log.println(Log.ERROR, "DNewProject.getSelectedModel ()", "Unable to load requested model. ", e);
			MessageDialog.print(this, Translator.swap("DNewProject.errorWhileLoadingModel"), e);
			return null;
		}
		StatusDispatcher.print(Translator.swap("DNewProject.modelLoaded"));
		return m;

	}

	/**
	 * Dynamic instanciation of the selected model chosen in the list, and call for
	 * its getInitialParameters () method
	 */
	private void initializeAction() {

		initialParameters = null;

		model = getSelectedModel();

		// Call the "initialize" dialog box of the now loaded module
		try {
			StatusDispatcher.print(Translator.swap("Shared.retrievingInitialParameters"));

			// Added this line to try to manage better the focus (with T.
			// Fonseca)
			InitialDialog.setOwnerWindow(this);

			initialParameters = model.getRelay().getInitialParameters();
			// if (initialParameters == null) {return;} // User cancelation
			if (initialParameters == null) {
				StatusDispatcher.print(Translator.swap("DNewProject.userCancellation"));
				return;
			} // User cancellation

			checkProjectName();
			setValidDialog(true);

			StatusDispatcher.print(Translator.swap("DNewProject.initializationInProgress"));

		} catch (Exception e) {
			Log.println(Log.ERROR, "DNewProject.initializeAction ()",
					"Unable to retrieve initial parameters from loaded model. ", e);
			MessageDialog.print(this, Translator.swap("DNewProject.errorWhileRetrievingInitialParameters"), e);
			return;

		}

	}

	/**
	 * If a model is selected in the choice list, enable initialize button
	 */
	private void modelSelectedAction() {

		initialize.setEnabled(true);

		Settings.setProperty("capsis.last.selected.module", getModelName());
		// Settings.setProperty("capsis.last.selected.module", (String)
		// modelList.getSelectedValue());

		String mpn = getSelectedModelPackageName();
		info.setEnabled(Helper.hasHelpFor(mpn));
		licence.setEnabled(true); // we have a default licence -> always found

		updateLabel();
	}

	/**
	 * Update model description label
	 */
	private void updateLabel() {

		String pkgName = getSelectedModelPackageName();

		IdCard card;
		String name = "", author = "", institute = "", description = "";
		try {
			card = ModelManager.getInstance().getIdCard(pkgName);
			name = card.getModelName();
			author = card.getModelAuthor();
			institute = card.getModelInstitute();
			description = card.getModelDescription();
		} catch (Exception e) {
			Log.println(Log.WARNING, "DNewProject.updateLabel ()", "Could not get the idCard for package " + pkgName,
					e);
			return;
		}

		// Write in text pane
		// Initialize some styles...
		details.setText("");
		StyledDocument doc = details.getStyledDocument();
		Style def = StyleContext.getDefaultStyleContext().getStyle(StyleContext.DEFAULT_STYLE);

		Style regular = doc.addStyle("regular", def);
		StyleConstants.setFontFamily(def, "SansSerif");

		Style s = doc.addStyle("italic", regular);
		StyleConstants.setItalic(s, true);

		s = doc.addStyle("bold", regular);
		StyleConstants.setBold(s, true);

		try {
			doc.insertString(doc.getLength(), name, doc.getStyle("bold"));
			doc.insertString(doc.getLength(), " : " + author, doc.getStyle("regular"));
			doc.insertString(doc.getLength(), ", " + institute + "\n", doc.getStyle("italic"));
			doc.insertString(doc.getLength(), description, doc.getStyle("regular"));
		} catch (BadLocationException e) {
			Log.println(Log.WARNING, "DNewProject.updateLabel ()", "Could not insert text into text pane", e);
			return;
		}

		// Ensure the top of the text pane is visible
		details.setCaretPosition(0);

	}

	/**
	 * Processes actions when selection occurs in the model list
	 */
	public void valueChanged(ListSelectionEvent evt) {

		// fc-7.10.2020
		String modelName = (String) modelList.getSelectedValue();
		
//		JList src = (JList) evt.getSource();
//		String modelName = (String) src.getSelectedValue();

		// fc-7.10.2020
		synchro();

		// fc-7.10.2020 Nothing selected
		if (modelName == null)
			return;

		// For one selection, two events are generated : click and de-click
		// this feature hears only one
		if (!modelName.equals(lastModelName)) {
			lastModelName = modelName;
			modelSelectedAction();
		}

	}

	/**
	 * Enable / disable buttons according to the user selection.
	 */
	private void synchro() {

		// fc-30.10.2020 fixed a bug (detected by J. Labonne), added isSingleModelCase()
		// fc-7.10.2020
		boolean aModelIsSelected = isSingleModelCase() ? true : (modelList.getSelectedIndex() != -1);

		info.setEnabled(aModelIsSelected);
		licence.setEnabled(aModelIsSelected);
		initialize.setEnabled(aModelIsSelected);

	}

	/**
	 * Click on the info button
	 */
	private void infoAction() {

		String selectedModelName = getModelName();
		// String selectedModelName = (String) modelList.getSelectedValue();

		if (selectedModelName == null)
			return;

		String mpn = getSelectedModelPackageName();
		Helper.helpFor(mpn);

	}

	/**
	 * Someone clicked on the licence button
	 */
	private void licenceAction() {

		String selectedModelName = getModelName();
		// String selectedModelName = (String) modelList.getSelectedValue();

		if (selectedModelName == null)
			return;

		String mpn = getSelectedModelPackageName();
		Helper.licenseFor(mpn);

	}

	/**
	 * Process actions on the dialog box's buttons.
	 */
	public void actionPerformed(ActionEvent evt) {

		if (evt.getSource().equals(initialize)) {
			initializeAction();

		} else if (evt.getSource().equals(cancel)) {
			setValidDialog(false);

		} else if (evt.getSource().equals(info)) {
			infoAction();

		} else if (evt.getSource().equals(licence)) {
			licenceAction();

		} else if (evt.getSource().equals(help)) {
			Helper.helpFor(this);
		}

	}

	/**
	 * Initialize the dialog's graphical user interface
	 */
	private void createUI() {

		// Model name
		LinePanel l1 = new LinePanel();
		l1.add(new JWidthLabel(Translator.swap("DNewProject.projectName") + " :", 120));
		projectName = new JTextField(15);
		l1.add(LinePanel.addWithStrut0(projectName));
		l1.addStrut0();

		// Model panel
		LinePanel l2 = new LinePanel();

		// 1. Label
		ColumnPanel left = new ColumnPanel();
		JWidthLabel l = new JWidthLabel(Translator.swap("DNewProject.modelName") + " :", 120);
		left.add(l);
		left.addGlue();

		l2.add(left);

		// 2. Model list or single name, see below

		// 3. Current model description
		details = new JTextPane();
		details.setEditable(false);

		// 4. Current model info button
		info = new JButton(Translator.swap("DNewProject.documentation"), infoIcon);
		info.setToolTipText(Translator.swap("Shared.info"));
		info.addActionListener(this);
		info.setEnabled(false);

		// 5. Current model license button
		licence = new JButton(Translator.swap("DNewProject.license"), licenceIcon);
		licence.setToolTipText(Translator.swap("Shared.licence"));
		licence.addActionListener(this);
		licence.setEnabled(false);

		// Documentation and License for the current model
		LinePanel buttons = new LinePanel();
		buttons.addGlue();

		if (!isSingleModelCase()) {
			buttons.add(new JLabel(Translator.swap("DNewProject.selectedModel") + " : "));
		}
		buttons.add(info);
		buttons.add(licence);
		buttons.addGlue();

		// fc-30.4.2020 single model case
		if (isSingleModelCase()) {

			// There is no choice
			ColumnPanel right = new ColumnPanel();
			JLabel label2 = new JLabel(singleModelName);
			right.add(LinePanel.addWithGlue(label2));

			right.add(LinePanel.addWithStrut0(new JScrollPane(details)));
			right.add(LinePanel.addWithStrut0(new MemoPanel(Translator.swap("DNewProject.singleModelNote"))));

			// In sungleModelCase, the buttons are higher in the UI
			right.add(buttons);

			right.addGlue();

			l2.add(new NorthPanel(right));

		} else {

			// A list to choose the model
			modelList = new JList(new Vector<String>(modelNames));
			modelList.addListSelectionListener(this);
			modelList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

			JScrollPane scroll1 = new JScrollPane(modelList);

			// Manage double click on the list
			modelList.addMouseListener(new MouseAdapter() {
				/** Double click on model list opens initialize dialog */
				public void mouseClicked(MouseEvent e) {
					if (e.getClickCount() == 2) {
						initialize.doClick(); // emulate button click
					}
				}
			});

			JScrollPane scroll2 = new JScrollPane(details);

			// Splitpane

			scroll1.setPreferredSize(new Dimension(400, 600)); // fc-30.4.2020
			scroll2.setPreferredSize(new Dimension(400, 400)); // fc-30.4.2020

			JSplitPane split = new JSplitPane(JSplitPane.VERTICAL_SPLIT, scroll1, scroll2);

			split.setAutoscrolls(true);

			// fc-30.4.2020 This has no effect if the component is not realized
//			split.setDividerLocation(0.7);

			split.setResizeWeight(0.7); // was 0.9
			split.setContinuousLayout(true);

			JPanel right = new JPanel(new BorderLayout());
			right.add(LinePanel.addWithStrut0(split), BorderLayout.CENTER);

			l2.add(right);

		}

		// 5. Control panel
		JPanel controlPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		initialize = new JButton(Translator.swap("DNewProject.initialize"));
		initialize.setMnemonic(KeyEvent.VK_A);
		ImageIcon icon = IconLoader.getIcon("ok_16.png");
		initialize.setIcon(icon);
		initialize.setEnabled(false);
		initialize.addActionListener(this);
		controlPanel.add(initialize);

		cancel = new JButton(Translator.swap("Shared.cancel"));
		icon = IconLoader.getIcon("cancel_16.png");
		cancel.setIcon(icon);
		cancel.addActionListener(this);
		controlPanel.add(cancel);

		help = new JButton(Translator.swap("Shared.help"));
		icon = IconLoader.getIcon("help_16.png");
		help.setIcon(icon);
		help.addActionListener(this);
		controlPanel.add(help);

		// Set initialise as default (see AmapDialog)
		initialize.setDefaultCapable(true);
		getRootPane().setDefaultButton(initialize);

		// General layout
		JPanel part1 = new JPanel(new BorderLayout());

		ColumnPanel col1 = new ColumnPanel();
		col1.add(l1);
		col1.addStrut0();
		part1.add(col1, BorderLayout.NORTH);

		ColumnPanel col2 = new ColumnPanel();
		col2.add(l2);
		col2.addStrut0();
		part1.add(col2, BorderLayout.CENTER);

		if (!isSingleModelCase()) {
			// If several models, the button line is at the bottom
			ColumnPanel col3 = new ColumnPanel();
			col3.add(buttons);
			col3.addStrut0();
			part1.add(col3, BorderLayout.SOUTH);
		}

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(part1, BorderLayout.CENTER);
		getContentPane().add(controlPanel, BorderLayout.SOUTH);

		if (!isSingleModelCase()) {
			String memo = Settings.getProperty("capsis.last.selected.module", (String) null);
			if (memo != null) {
				modelList.setSelectedValue(memo, true); // shouldScroll = true
			} else {
				modelList.setSelectedIndex(0);
			}
		}

		setTitle(Translator.swap("DNewProject.newProject"));
		setMinimumSize(new Dimension(400, 350));
		setPreferredSize(new Dimension(600, 500));

		synchro(); // fc-7.10.2020
		
		setModal(true);
	}

	/**
	 * Returns the project name
	 */
	@Override
	public String getProjectName() {
		return projectName.getText().trim();
	}

	/**
	 * Returns the GModel instance
	 */
	@Override
	public GModel getModel() {
		return model;
	}

	/**
	 * Returns the InitialParameters instance
	 */
	@Override
	public InitialParameters getInitialParameters() {
		return initialParameters;
	}

	/**
	 * Mathieu Fortin: fixed a memory leak - fc-31.8.2015
	 */
	@Override
	public void finalize() { // to avoid memory leak
		initialParameters = null;
		model = null;
	}

}
