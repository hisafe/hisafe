/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2010  Francois de Coligny, Samuel Dufour
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE. See the GNU Lesser General Public 
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package capsis.commongui.command;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Collection;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import capsis.commongui.InitialDialog;
import capsis.commongui.util.Helper;
import capsis.kernel.Engine;
import capsis.kernel.GModel;
import capsis.kernel.IdCard;
import capsis.kernel.InitialParameters;
import capsis.kernel.ModelManager;

/**
 * DefaultNewProjectDialog - A simple dialog for new projects creation.
 * 
 * @author F. de Coligny - September 2008, may 2010
 */
public class DefaultNewProjectDialog extends AmapDialog implements NewProjectDialog, ActionListener,
		ListSelectionListener {
	static {
		Translator.addBundle("capsis.commongui.command.CommandManager");
	}

	private JTextField projectName;
	private GModel model;
	private InitialParameters initialParameters;

	private ModelManager modelManager;
	private Collection<String> modelNames; // list of the model names
	private JList modelList;

	private String modelName;

	private JTextArea description;
	private JButton ok;
	private JButton cancel;
	private JButton help;

	/**
	 * Constructor, creates the user interface.
	 */
	public DefaultNewProjectDialog(JFrame frame) {
		super(frame);

		modelManager = ModelManager.getInstance();

		modelNames = modelManager.getModelNames();
		modelNames = new TreeSet<String>(modelNames); // sorted list

		createUI();

		// this is an AmapDialog: when closing it with the title bar cross, will
		// call escapePressed
		setTitle(Translator.swap("DefaultNewProjectDialog.title"));

		activateSizeMemorization(getClass().getName());
		activateLocationMemorization(getClass().getName());
		pack();
		setModal(true);

		setVisible(true);
	}

	private void okAction() {

		try {
			Object o = modelList.getSelectedValue();
			modelName = (String) o;
			ModelManager modMan = ModelManager.getInstance();
			String packageName = modMan.getPackageName(modelName);
			model = Engine.getInstance().loadModel(packageName);

			// Check that a model was selected (modelName and model set)
			if (modelName == null || model == null) {
				MessageDialog.print(this, Translator.swap("DefaultNewProjectDialog.pleaseSelectAModelInTheList"));
				return;
			}

			initialParameters = null;

			// Call the "initialize" dialog box of the now loaded module
			StatusDispatcher.print(Translator.swap("DefaultNewProjectDialog.retrievingInitialParameters"));

			// Added this line to try to manage better the focus (with T.
			// Fonseca)
			InitialDialog.setOwnerWindow(this);

			initialParameters = model.getRelay().getInitialParameters();

			if (initialParameters == null) {
				StatusDispatcher.print(Translator.swap("DefaultNewProjectDialog.userCancellation"));
				return;
			} // User cancellation

			// Check project name, use a default name if not set by user
			if (Check.isEmpty(projectName.getText().trim())) {
				// fc-3.2.2011 shorter project name is better everywhere ->
				// default name is now short

				String name = modelName.replace('<', ' ').replace('>', ' ').trim(); // '<Editor>'
																					// ->
																					// 'Editor'

				name = name.length() <= 3 ? name : name.substring(0, 3);
				projectName.setText(name);
				// projectName.setText (modelName + "_Unnamed");
			}

			setValidDialog(true);

			StatusDispatcher.print(Translator.swap("DefaultNewProjectDialog.done"));

		} catch (Exception e) {
			Log.println(Log.WARNING, "DefaultNewProjectDialog.initializeAction ()",
					"Unable to retrieve initial parameters from loaded model. ", e);
			StatusDispatcher.print(Translator.swap("DefaultNewProjectDialog.errorWhileRetrievingInitialParameters"));
			// MessageDialog.print (this,
			// Translator.swap
			// ("DefaultNewProjectDialog.errorWhileRetrievingInitialParameters"),
			// e);
			return;

		}

	}

	/**
	 * Selection in the list
	 */
	public void valueChanged(ListSelectionEvent evt) {
		try {
			Object o = modelList.getSelectedValue();
			modelName = (String) o;
			Settings.setProperty("new.project.dialog.selected.model.name", modelName);

			ModelManager modMan = ModelManager.getInstance();
			String packageName = modMan.getPackageName(modelName);

			IdCard card = modMan.getIdCard(packageName);
			String modelName = card.getModelName();
			String modelAuthor = card.getModelAuthor();
			// String modelInstitute = card.getModelInstitute();
			String modelDescription = card.getModelDescription();

			StringBuffer b = new StringBuffer();
			b.append(modelName);
			b.append('\n');
			b.append(Translator.swap("DefaultNewProjectDialog.authors"));
			b.append(" : ");
			b.append(modelAuthor);
			b.append('\n');
			b.append(modelDescription);

			description.setText(b.toString());
			description.setCaretPosition(0);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DefaultNewProjectDialog.valueChanged ()", "Error during model selection", e);
		}

	}

	/**
	 * Some button was hit...
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(ok)) {
			okAction();
		} else if (evt.getSource().equals(cancel)) {
			setValidDialog(false);
		} else if (evt.getSource().equals(help)) {
			Helper.helpFor(this);
		}
	}

	/**
	 * Called on ctrl-Z. Can trigger an undo () method.
	 */
	protected void ctrlZPressed() {
		System.out.println("DefaultNewProjectDialog.ctrlZPressed ()");
	}

	/**
	 * Called on ctrl-Y. Can trigger a redo () method.
	 */
	protected void ctrlYPressed() {
		System.out.println("DefaultNewProjectDialog.ctrlYPressed ()");
	}

	/**
	 * Called on Escape.
	 */
	@Override
	protected void escapePressed() {
		System.out.println("escape was pressed");
		setValidDialog(false);
	}

	/**
	 * Initializes the GUI.
	 */
	private void createUI() {

		ColumnPanel c1 = new ColumnPanel();

		// Project name
		LinePanel l0 = new LinePanel();
		l0.add(new JLabel(Translator.swap("DefaultNewProjectDialog.projectName") + " : "));
		projectName = new JTextField(5);
		l0.add(projectName);
		l0.addStrut0();
		c1.add(l0);

		// Project type

		LinePanel l1 = new LinePanel();
		l1.add(new JLabel(Translator.swap("DefaultNewProjectDialog.projectType") + " : "));
		l1.addGlue();
		c1.add(l1);
		c1.addStrut0();

		// Model list (CENTER)
		LinePanel l2 = new LinePanel();
		modelList = new JList(new Vector(modelNames));
		modelList.addListSelectionListener(this);

		// Manage double click on the list
		modelList.addMouseListener(new MouseAdapter() {
			/** Double click on model list opens initialize dialog */
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					ok.doClick(); // emulate button click
				}
			}
		});

		l2.add(new JScrollPane(modelList));
		l2.addStrut0();

		// Under the list
		ColumnPanel c2 = new ColumnPanel();

		LinePanel l3 = new LinePanel();
		l3.add(new JLabel(Translator.swap("DefaultNewProjectDialog.description") + " : "));
		l3.addGlue();
		c2.add(l3);
		c2.addStrut0();

		// Model description
		LinePanel l4 = new LinePanel();
		description = new JTextArea(5, 4);
		description.setLineWrap(true);
		description.setWrapStyleWord(true);
		description.setEditable(false);
		JScrollPane z = new JScrollPane(description);
		z.setPreferredSize(new Dimension(400, 150));
		l4.add(z);
		l4.addStrut0();
		c2.add(l4);
		c2.addStrut0();

		LinePanel controlPanel = new LinePanel();
		controlPanel.addGlue();
		ok = new JButton(Translator.swap("Shared.ok"));
		ok.addActionListener(this);
		controlPanel.add(ok);
		cancel = new JButton(Translator.swap("Shared.cancel"));
		cancel.addActionListener(this);
		controlPanel.add(cancel);
		help = new JButton(Translator.swap("Shared.help"));
		help.addActionListener(this);
		controlPanel.add(help);

		JPanel aux = new JPanel(new BorderLayout());
		aux.add(c1, BorderLayout.NORTH);
		aux.add(l2, BorderLayout.CENTER);
		aux.add(c2, BorderLayout.SOUTH);

		getContentPane().setLayout(new BorderLayout());

		// fc-24.11.2014 Better look on dialog resizing
		getContentPane().add(aux, BorderLayout.CENTER);
		// getContentPane().add(c1, BorderLayout.NORTH);
		// getContentPane().add(c2, BorderLayout.CENTER);

		getContentPane().add(controlPanel, BorderLayout.SOUTH);

		// getContentPane().setMinimumSize(new Dimension (150, 100));

		setDefaultButton(ok);

		// Try to select in the list
		try {
			// default value "" will be returned if not found
			String lastSelection = Settings.getProperty("new.project.dialog.selected.model.name", "");
			if (!lastSelection.equals("")) {
				modelList.setSelectedValue(lastSelection, true); // shouldScroll
																	// = true
			} else {
				modelList.setSelectedIndex(0);
			}
		} catch (Exception e) {
		} // if trouble, ignore
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
		// TODO Auto-generated method stub
		return model;
	}

	/**
	 * Returns the InitialParameters instance
	 */
	@Override
	public InitialParameters getInitialParameters() {
		// TODO Auto-generated method stub
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
