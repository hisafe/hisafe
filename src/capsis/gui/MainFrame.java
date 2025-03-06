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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GraphicsEnvironment;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowStateListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JToolBar;
import javax.swing.ToolTipManager;
import javax.swing.UIManager;

import capsis.app.CapsisExtensionManager;
import capsis.commongui.ProjectLoader;
import capsis.commongui.command.ChangeStepColor;
import capsis.commongui.command.CloseProject;
import capsis.commongui.command.CloseSession;
import capsis.commongui.command.CommandManager;
import capsis.commongui.command.ConfigureProject;
import capsis.commongui.command.DeleteStep;
import capsis.commongui.command.Evolution;
import capsis.commongui.command.Intervention;
import capsis.commongui.command.NewProject;
import capsis.commongui.command.NewSession;
import capsis.commongui.command.OpenProject;
import capsis.commongui.command.OpenSession;
import capsis.commongui.command.Quit;
import capsis.commongui.command.SaveAsProject;
import capsis.commongui.command.SaveAsSession;
import capsis.commongui.command.SaveProject;
import capsis.commongui.command.SaveSession;
import capsis.commongui.command.ShutdownHook;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.commongui.projectmanager.StepPopup;
import capsis.gui.command.About;
import capsis.gui.command.AutoLayoutDesktop;
import capsis.gui.command.BuildGrouper;
import capsis.gui.command.CapsisHelp;
import capsis.gui.command.CapsisOnTheWeb;
import capsis.gui.command.CleanView;
import capsis.gui.command.DockedOrg;
import capsis.gui.command.EditAutomation;
import capsis.gui.command.ExportStep;
import capsis.gui.command.Faq;
import capsis.gui.command.HelpContents;
import capsis.gui.command.Licence;
import capsis.gui.command.Options;
import capsis.gui.command.Print;
import capsis.gui.command.PrintPreview;
import capsis.gui.command.ReportOrg;
import capsis.gui.command.RestoreView;
import capsis.gui.command.SaveView;
import capsis.gui.command.StepProperties;
import capsis.gui.command.Summary;
import capsis.gui.command.Tool;
import capsis.gui.command.ToolBox;
import capsis.gui.command.Tutorial;
import capsis.gui.command.ViewToolbar;
import capsis.gui.command.WindowMosaic;
import capsis.kernel.Engine;
import capsis.util.ActionHolder;
import capsis.util.CommandAction;
import capsis.util.DummyAction;
import capsis.util.JSmartMenu;
import capsis.util.KeyModifiersProducer;
import capsis.util.SelectableMenuItem;
import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import jeeb.lib.util.screen.ScreenManager;
import jeeb.lib.util.task.StatusBar;

/**
 * MainFrame is Capsis' main window. It defines actions which can be performed
 * using the main menu. Each action is connected to a command. Triggering the
 * action executes the command. The command can be used programatically: new
 * Command ([optionalParams]).execute (). Some actions and command are
 * Selectable. That means they are triggered by a Checkbox. See SelectableAction
 * and SelectableCommand.
 * 
 * @author F. de Coligny - October 2000
 */
public class MainFrame extends JFrame implements ActionHolder, KeyModifiersProducer {
	private static final int FRAMES_DEFAULT_WIDTH = 1000;
	private static final int FRAMES_DEFAULT_HEIGHT = 600;
	public final static int BUTTON_SIZE = 23;

	static private MainFrame instance; // Singleton pattern

	private ProjectManager projectManager;
	private CommandManager commandManager;

	private JToolBar toolBar;
	private StatusBar statusBar;

	// This will be replaced by the CommandManager
	private HashMap<String, Action> commandName_action; // map commandName ->
														// action

	private KeyEvent lastPressedKeyEvent;

	private JMenuBar menuBar;
	// private String baseTitle;

	// fc-9.8.2018 REMOVED editMenu, useless, moved Options to viewMenu
	// private JSmartMenu editMenu;

	private JSmartMenu viewMenu;
	private JSmartMenu projectMenu;
	private JSmartMenu stepMenu;

	private JSmartMenu toolMenu;
	// private JSmartMenu windowMenu;
	private JSmartMenu helpMenu;

	public List<Action> visiActions = new ArrayList<Action>();
	private JSmartMenu automationMenu;

	private Selector selector;

	/**
	 * To get an instance of MainFrame.
	 */
	static public MainFrame getInstance() { // Singleton pattern
		if (instance == null) {
			instance = new MainFrame();
		}
		return instance;
	}

	/**
	 * For convenience
	 */
	static public boolean isInstanceLoaded() {
		return instance != null;
	}

	/**
	 * Private constructor to comply with the singleton pattern.
	 */
	private MainFrame() {

		setPropertyDefaults();

		// UI customizations (not too many ;-)
		setLookAndFeel();

		// Setup icon loader
		IconLoader.addPath("capsis/images");

		int internalFrameBorderSize = Settings.getProperty("internal.frame.border.size", 2);

		// Tooltip dismiss delay - nb-02.12.2016
		int toolTipDefaultDismissDelayInMilliseconds = ToolTipManager.sharedInstance().getDismissDelay();
		int toolTipDismissDelayInSeconds = Settings.getProperty("capsis.tooltip.dismiss.delay",
				toolTipDefaultDismissDelayInMilliseconds / 1000);
		ToolTipManager.sharedInstance().setDismissDelay(toolTipDismissDelayInSeconds * 1000);

		UIManager.put("SplitPane.dividerSize", new Integer(7));
		UIManager.put("InternalFrame.border", BorderFactory.createLineBorder(Color.GRAY, internalFrameBorderSize));
		UIManager.put("InternalFrame.activeTitleBackGround", Color.YELLOW);
		UIManager.put("InternalFrame.inactiveTitleBackGround", Color.WHITE);
		UIManager.put("InternalFrame.useTaskBar", false);

		// fc-4.5.2018
		LinePanel.configureDefaults(6, 4);
		ColumnPanel.configureDefaults(6, 4);

		// Capsis Icon
		try {
			ImageIcon logo = IconLoader.getIcon("capsis-logo.png");
			setIconImage(logo.getImage());

		} catch (Exception e) {
			Log.println(Log.WARNING, "MainFrame.c ()", "Unable to load capsis icon", e);
		}

		lastPressedKeyEvent = null;
		commandName_action = new HashMap<String, Action>();

		// Set the frame title
		setTitle("Capsis " + Engine.getVersionAndRevision()); // fc-13.2.2019
		// setTitle("Capsis " + Engine.getVersion());

		// Register a shutdown hook to be called if the application is
		// interrupted
		// in an unexpected way (Ctrl+C in shell...)
		new ShutdownHook(this, "capsis");

		// Build the projectManager
		boolean withTaskManagerView = true; // fc-29.9.2021
		projectManager = new ProjectManager(this, withTaskManagerView);

		// Build the CommandManager, create all the basic commands (Project...)
		commandManager = new CommandManager(this, "capsis");

		// Add the Capsis specific commmands to the CommandManager
		createCapsisCommands();

		// Create actions, place them in the main menu
		this.setJMenuBar(createMainMenu());

		// StatusBar listens to StatusDispatcher & ProgressDispatcher
		// It will be added in content pane by positioner
		
		// fc-29.9.2021
		withTaskManagerView = false;
		boolean withTaskManagerButton = true;
		statusBar = new StatusBar( withTaskManagerView,  withTaskManagerButton);
//		statusBar = new StatusBar();
		
		StatusDispatcher.addListener(statusBar);

		// Tool bar for main window
		// It will be added in content pane by positioner
		createMainToolBar();
		
		// fc-23.3.2020 Introducing ScreenManager
//		// Positioner takes care of accurate size according to current options
//		// It may also manage the location also depending on options
//		setPreferredSize(new java.awt.Dimension(1400, 800)); // fc-4.4.2019
		
		// fc-23.3.2020 Introducing ScreenManager
//		// Positioner.layoutComponent (this);
//		initPositionAndSize(this); // getting rid of the positioners
		
		// fc-23.3.2020 Introducing ScreenManager
		// Size for first opening, then the size at close time is restored
		// at opening time
		// Setting no size here lets the ScreenManager use its default
		this.setSize(1200, 800);
		
		// fc-23.3.2020 Introducing ScreenManager
		ScreenManager.getInstance().locate(this);		
		
		// Close the main window exits the application
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent evt) {
				new Quit(MainFrame.this).execute();
			}
		});

		// Writing in the StatusDispatcher will go to the status bar
		StatusDispatcher.print(Translator.swap("Shared.ready"));

		// We want autoUIs to use the pretty layout
		System.setProperty("auto.ui.pretty.layout", "true");

		// Sets a default parent frame for all AmapDialogs with a null parent
		// fc-23.5.2014
		AmapDialog.setParentFrame(this);

		setVisible(true);
	}

	/**
	 * Set default values for some unset properties regarding the GUI.
	 */
	private void setPropertyDefaults() {
		Settings.setProperty("capsis.viewers.visible", "true");

	}

	/**
	 * Set some chekboxes of the menu and perform their action if true.
	 */
	public void init() {

		// Build the Selector
		selector = new Selector(projectManager);

		// bugs... s.setSelected (Check.isTrue ("capsis.main.toolbar.visible"));
		SelectableAction v = getSelectableAction("ViewToolbar");
		v.setSelected(Settings.getProperty("capsis.main.toolbar.visible", true));

		// ProjectLoader can load a project (or several) at boot time
		ProjectLoader loader = ProjectLoader.getInstance();

		// Project loader / prepare the loading of the requested projects if any
		capsis.kernel.Options options = capsis.kernel.Options.getInstance();

		if (!options.getProjects().equals("")) {
			loader.loadMacroFile(options.getProjects());
		}
		if (!options.getProject().equals("")) {
			loader.addProjectFileName(options.getProject());
		}

		if (loader.isEmpty()) {

			// WhatDoYouWantToDo dialog manages rescueSession
			if (Settings.getProperty("capsis.what.do.you.want.to.do", true)) {
				new WhatDoYouWantToDo(this);

			} else { // else manage it directly

				// Rescue mode: check if the previous Capsis session crashed and
				// if so,
				// propose to reload the previous rescue session
				ShutdownHook.getInstance().proposeToRestoreRescueSessionIfAny();

			}

		} else {

			loader.execute();

		}

	}

	/**
	 * Redefine JFrame.processKeyEvent (KeyEvent e) Used to remember if CTRL,
	 * SHIFT, ALT or META is down. The information can be retrieved by other
	 * components using KeyModifierProducer.getModifiers ().
	 */
	protected void processKeyEvent(KeyEvent e) {
		super.processKeyEvent(e);

		if (e.getID() == KeyEvent.KEY_PRESSED) {
			lastPressedKeyEvent = e;
		} else {
			lastPressedKeyEvent = null;
		}
	}

	/**
	 * From KeyModifiersProducer interface.
	 */
	public int getModifiers() {
		if (lastPressedKeyEvent == null) {
			return 0;
		} else {
			return lastPressedKeyEvent.getModifiers();
		}
	}

	/**
	 * Creates checkBox items in menus for SelectableCommand more easily.
	 */
	public SelectableMenuItem makeCheckBoxMenuItem(JMenu menu, String command, SelectableAction action) {
		SelectableMenuItem item = new SelectableMenuItem(Translator.swap(command), action);
		item.setActionCommand(command);
		menu.add(item);
		return item;
	}

	/**
	 * Accessor for the status bar.
	 */
	public StatusBar getStatusBar() {
		return statusBar;
	}

	/**
	 * Accessor for tool bar.
	 */
	public JToolBar getToolBar() {
		return toolBar;
	}

	/**
	 * Add the Capsis specific commands to the CommandManager
	 */
	private void createCapsisCommands() {

		commandManager.addCommand(new Intervention(this), CommandManager.Level.PROJECT);
		commandManager.addCommand(new Summary(this), CommandManager.Level.PROJECT);
		commandManager.addCommand(new BuildGrouper(this), CommandManager.Level.PROJECT);
		commandManager.addCommand(new ExportStep(this), CommandManager.Level.PROJECT);
		commandManager.addCommand(new ToolBox(this), CommandManager.Level.PROJECT);
		commandManager.addCommand(new StepProperties(this), CommandManager.Level.PROJECT);
		commandManager.addCommand(new About(this), CommandManager.Level.BASIC);

		// fc-30.3.2021
		commandManager.addCommand(new AutoLayoutDesktop(this), CommandManager.Level.BASIC);
		
		// No session opened yet -> disable session & project commands
		commandManager.setCommandsEnabled(CommandManager.Level.PROJECT, false);

	}

	/**
	 * Main tool bar creation.
	 */
	private void createMainToolBar() {
		toolBar = new JToolBar();
		// toolBar.setFloatable (false);

		toolBar.add(commandManager.getCommand(NewProject.class));
		toolBar.add(commandManager.getCommand(OpenProject.class));
		toolBar.addSeparator();
		toolBar.add(commandManager.getCommand(ConfigureProject.class));
		toolBar.add(commandManager.getCommand(SaveProject.class));
		toolBar.add(commandManager.getCommand(SaveAsProject.class));
		toolBar.add(commandManager.getCommand(CloseProject.class));
		toolBar.addSeparator();
		toolBar.add(commandManager.getCommand(BuildGrouper.class));
		toolBar.add(commandManager.getCommand(AutoLayoutDesktop.class)); // fc-30.3.2021
		toolBar.addSeparator();
		toolBar.add(
				commandManager.getCommand(About.class) /*
														 * getAction ("About")
														 */);

		updateMainToolBarAppearance();
	}

	/**
	 * The main toolbar Appearance can be changed in Capsis preferences
	 */
	public void updateMainToolBarAppearance() {

		// Not nice with the 24x24 icons, removed fc-1.10.2012
		// for (int i = 0; i < toolBar.getComponentCount (); i++) {
		// JComponent c = (JComponent) toolBar.getComponentAtIndex (i);
		// Tools.setSizeExactly (c, BUTTON_SIZE, BUTTON_SIZE);
		// if (c instanceof AbstractButton) {
		// Settings.getProperty ("main.menu.buttons.borders", false));
		// }
		// }

	}

	/**
	 * Create the step menu to be added in the main menu. Also creates the step
	 * popup menu that looks similar.
	 */
	private JMenu getStepMenu() {

		stepMenu = new JSmartMenu(Translator.swap("MainFrame.step"), Translator.swap("MainFrame.step.shortcut"));

		stepMenu.add(commandManager.getCommand(Evolution.class));
		stepMenu.add(commandManager.getCommand(Intervention.class));
		stepMenu.add(commandManager.getCommand(Summary.class));
		stepMenu.add(commandManager.getCommand(DeleteStep.class));

		stepMenu.addSeparator();

		stepMenu.add(commandManager.getCommand(BuildGrouper.class));
		stepMenu.add(commandManager.getCommand(ExportStep.class));
		stepMenu.add(commandManager.getCommand(ToolBox.class));

		stepMenu.addSeparator();

		// Step visibility sub menu
		JMenu visibilityMenu = new JMenu(Translator.swap("ShowStep.visibility"));
		stepMenu.add(visibilityMenu);

		visibilityMenu.add(commandManager.getCommand("ShowStep1"));
		visibilityMenu.add(commandManager.getCommand("ShowStep5"));
		visibilityMenu.add(commandManager.getCommand("ShowStep10"));
		visibilityMenu.add(commandManager.getCommand("ShowStep20"));

		stepMenu.addSeparator();

		stepMenu.add(commandManager.getCommand(StepProperties.class));

		// We want the step popup menu to look like the main step menu
		List<ActionCommand> actionCommands = new ArrayList<ActionCommand>();
		actionCommands.add(commandManager.getCommand(Evolution.class));
		actionCommands.add(commandManager.getCommand(Intervention.class));
		actionCommands.add(commandManager.getCommand(Summary.class));
		actionCommands.add(commandManager.getCommand(DeleteStep.class));
		actionCommands.add(null);
		actionCommands.add(commandManager.getCommand(BuildGrouper.class));
		actionCommands.add(commandManager.getCommand(ExportStep.class));
		actionCommands.add(commandManager.getCommand(ToolBox.class));
		actionCommands.add(null);
		actionCommands.add(commandManager.getCommand(ConfigureProject.class));
		actionCommands.add(null);

		// fc-3.1.2017
		actionCommands.add(commandManager.getCommand(ChangeStepColor.class));

		// TODO: Add a JMenu visibilityMenu containing the four next actions
		actionCommands.add(commandManager.getCommand("ShowStep1"));
		actionCommands.add(commandManager.getCommand("ShowStep5"));
		actionCommands.add(commandManager.getCommand("ShowStep10"));
		actionCommands.add(commandManager.getCommand("ShowStep20"));
		actionCommands.add(null);
		actionCommands.add(commandManager.getCommand(StepProperties.class));

		StepPopup.setActionCommands(actionCommands);

		return stepMenu;
	}

	/**
	 * Build the project menu.
	 */
	private JMenu getProjectMenu() {

		Action a = null; // temp handler
		ImageIcon icon = null;

		// Projects management
		projectMenu = new JSmartMenu(Translator.swap("MainFrame.scenario"),
				Translator.swap("MainFrame.scenario.shortcut"));

		projectMenu.add(commandManager.getCommand(NewProject.class));
		projectMenu.add(commandManager.getCommand(OpenProject.class));
		projectMenu.add(commandManager.getCommand(ConfigureProject.class));
		projectMenu.add(commandManager.getCommand(SaveProject.class));
		projectMenu.add(commandManager.getCommand(SaveAsProject.class));
		projectMenu.add(commandManager.getCommand(CloseProject.class));

		// Step sub-menu
		// projectMenu.addSeparator ();
		// projectMenu.add (getStepMenu());

		projectMenu.addSeparator();

		// Session
		JSmartMenu sessionMenu = new JSmartMenu(Translator.swap("MainFrame.session"),
				Translator.swap("MainFrame.file.shortcut"));
		projectMenu.add(sessionMenu);

		sessionMenu.add(commandManager.getCommand(NewSession.class));
		sessionMenu.add(commandManager.getCommand(OpenSession.class));
		sessionMenu.add(commandManager.getCommand(SaveSession.class));
		sessionMenu.add(commandManager.getCommand(SaveAsSession.class));
		sessionMenu.add(commandManager.getCommand(CloseSession.class));

		projectMenu.addSeparator();

		// Automation
		// automationMenu = new JSmartMenu (Translator.swap("Automation"), "",
		// IconLoader.getIcon ("empty_16.png"));
		automationMenu = new JSmartMenu(Translator.swap("Automation"), "");

		icon = IconLoader.getIcon("empty_16.png");
		a = new CommandAction(Translator.swap("Automation.export"), icon, new EditAutomation(true, this), this);
		a.putValue(Action.SHORT_DESCRIPTION, Translator.swap("Automation.export"));
		automationMenu.add(a, Translator.swap("Automation.export"));

		icon = IconLoader.getIcon("empty_16.png");
		a = new CommandAction(Translator.swap("Automation.import"), icon, new EditAutomation(false, this), this);
		a.putValue(Action.SHORT_DESCRIPTION, Translator.swap("Automation.import")); // button
																					// tooltiptext
		automationMenu.add(a, Translator.swap("Automation.import"));

		projectMenu.add(automationMenu);
		projectMenu.addSeparator();
		// ////////////

		icon = IconLoader.getIcon("printpreview_16.png");
		a = new CommandAction(Translator.swap("MainFrame.filePrintPreview"), icon, new PrintPreview(), this);
		a.putValue(Action.SHORT_DESCRIPTION, Translator.swap("MainFrame.filePrintPreview")); // button
																								// tooltiptext
		projectMenu.add(a, Translator.swap("MainFrame.filePrintPreview.shortcut"));
		a.setEnabled(false);

		icon = IconLoader.getIcon("print_16.png");
		a = new CommandAction(Translator.swap("MainFrame.filePrint"), icon, new Print(), this);
		a.putValue(Action.SHORT_DESCRIPTION, Translator.swap("MainFrame.filePrint")); // button
																						// tooltiptext
		projectMenu.add(a, KeyEvent.VK_P, Translator.swap("MainFrame.filePrint.shortcut"));
		a.setEnabled(false);

		projectMenu.addSeparator();

		projectMenu.add(commandManager.getCommand(Quit.class));

		return projectMenu;

	}

	/**
	 * Build the main menu with actions. These actions are ActionCommands
	 * registered in the CommandManager (2010-2011).
	 * 
	 * Note : SelectableActions are retrieved with : MainFrame.getInstance
	 * ().getSelectableAction ("SelectableCommandName")
	 */
	private JMenuBar createMainMenu() {

		Action a = null;
		ImageIcon icon = null;

		// Tell NewProject to use this specific dialog for Capsis
		NewProject.setDialog(DNewProject.class);

		// Tell Intervention to use this specific dialog for Capsis
		Intervention.setDialog(DIntervention.class);

		// Main menu bar creation
		menuBar = new JMenuBar();

		// Project
		menuBar.add(getProjectMenu());

		// Step
		menuBar.add(getStepMenu());

		// Edit
		// editMenu = new JSmartMenu(Translator.swap("MainFrame.edit"),
		// Translator.swap("MainFrame.edit.shortcut"));
		//
		// icon = IconLoader.getIcon("edit-undo_16.png");
		// Undo u = new Undo(); // fc - 1.3.2005
		// a = new CommandAction(Translator.swap("MainFrame.editUndo"), icon, u,
		// this);
		// // ~ u.setAction (a); // fc - 1.3.2005 - if undo disabled -> action
		// // disabled
		// a.putValue(Action.SHORT_DESCRIPTION,
		// Translator.swap("MainFrame.editUndo")); // button
		// // tooltiptext
		// editMenu.add(a, KeyEvent.VK_Z,
		// Translator.swap("MainFrame.editUndo.shortcut"));
		// a.setEnabled(false);
		//
		// editMenu.addSeparator();
		//
		// icon = IconLoader.getIcon("edit-cut_16.png");
		// a = new CommandAction(Translator.swap("MainFrame.editCut"), icon, new
		// Cut(), this);
		// a.putValue(Action.SHORT_DESCRIPTION,
		// Translator.swap("MainFrame.editCut")); // button
		// // tooltiptext
		// editMenu.add(a, KeyEvent.VK_X,
		// Translator.swap("MainFrame.editCut.shortcut"));
		// a.setEnabled(false);
		//
		// icon = IconLoader.getIcon("edit-copy_16.png");
		// a = new CommandAction(Translator.swap("MainFrame.editCopy"), icon,
		// new Copy(), this);
		// a.putValue(Action.SHORT_DESCRIPTION,
		// Translator.swap("MainFrame.editCopy")); // button
		// // tooltiptext
		// editMenu.add(a, KeyEvent.VK_C,
		// Translator.swap("MainFrame.editCopy.shortcut"));
		// a.setEnabled(false);
		//
		// icon = IconLoader.getIcon("edit-paste_16.png");
		// a = new CommandAction(Translator.swap("MainFrame.editPaste"), icon,
		// new Paste(), this);
		// a.putValue(Action.SHORT_DESCRIPTION,
		// Translator.swap("MainFrame.editPaste")); // button
		// // tooltiptext
		// editMenu.add(a, KeyEvent.VK_V,
		// Translator.swap("MainFrame.editPaste.shortcut"));
		// a.setEnabled(false);
		//
		// editMenu.addSeparator();
		//
		// icon = IconLoader.getIcon("edit-find_16.png");
		// a = new CommandAction(Translator.swap("MainFrame.editFind"), icon,
		// new Find(), this);
		// a.putValue(Action.SHORT_DESCRIPTION,
		// Translator.swap("MainFrame.editFind")); // button
		// // tooltiptext
		// editMenu.add(a, KeyEvent.VK_F,
		// Translator.swap("MainFrame.editFind.shortcut"));
		// a.setEnabled(false);
		//
		// icon = IconLoader.getIcon("edit-find-replace_16.png");
		// a = new CommandAction(Translator.swap("MainFrame.editReplace"), icon,
		// new Replace(), this);
		// a.putValue(Action.SHORT_DESCRIPTION,
		// Translator.swap("MainFrame.editReplace")); // button
		// // tooltiptext
		// editMenu.add(a, KeyEvent.VK_H,
		// Translator.swap("MainFrame.editReplace.shortcut"));
		// a.setEnabled(false);
		//
		// editMenu.addSeparator();

		// fc-9.8.2018 moved to view menu
		// // fc-26.8.2008 - added VK_COMMA
		// icon = IconLoader.getIcon("option_16.png");
		// a = new CommandAction(Translator.swap("MainFrame.editOptions"), icon,
		// new Options(), this);
		// a.putValue(Action.SHORT_DESCRIPTION,
		// Translator.swap("MainFrame.editOptions")); // button
		// // tooltiptext
		// editMenu.add(a, KeyEvent.VK_COMMA,
		// Translator.swap("MainFrame.editOptions.shortcut"));

		// menuBar.add(editMenu);

		// View ------------------------------------
		viewMenu = new JSmartMenu(Translator.swap("MainFrame.view"), Translator.swap("MainFrame.view.shortcut"));
		SelectableAction s = null; // temp handler

		s = new SelectableAction(Translator.swap("MainFrame.viewToolbar"), null, new ViewToolbar(), this);
		makeCheckBoxMenuItem(viewMenu, "MainFrame.viewToolbar", s); // menu,
																	// key,
																	// action

		viewMenu.addSeparator();

		// ////////////////////////////////
		icon = IconLoader.getIcon("docked-org_16.png");
		a = new CommandAction(Translator.swap("MainFrame.viewDockedOrg"), icon, new DockedOrg(), this);
		a.putValue(Action.SHORT_DESCRIPTION, Translator.swap("MainFrame.viewDockedOrg")); // button
																							// tooltiptext
		viewMenu.add(a, Translator.swap("MainFrame.viewDockedOrg.shortcut"));

		// ////////////////////////////////
		icon = IconLoader.getIcon("report-org_16.png");
		a = new CommandAction(Translator.swap("MainFrame.viewReportOrg"), icon, new ReportOrg(), this);
		a.putValue(Action.SHORT_DESCRIPTION, Translator.swap("MainFrame.viewReportOrg")); // button
																							// tooltiptext
		viewMenu.add(a, Translator.swap("MainFrame.viewReportOrg.shortcut"));

		viewMenu.addSeparator();

		s = new SelectableAction(Translator.swap("MainFrame.windowMosaic"), null, new WindowMosaic(), this);
		makeCheckBoxMenuItem(viewMenu, "MainFrame.windowMosaic", s); // menu,
																		// key,
																		// action

		s.setSelected(Settings.getProperty("auto.mode.mosaic", true));

		viewMenu.addSeparator();

		a = new CommandAction(Translator.swap("MainFrame.saveView"), null, new SaveView(), this);
		a.putValue(Action.SHORT_DESCRIPTION, Translator.swap("MainFrame.saveView")); // button
																						// tooltiptext
		viewMenu.add(a, Translator.swap("MainFrame.saveView.shortcut"));

		a = new CommandAction(Translator.swap("MainFrame.restoreView"), null, new RestoreView(), this);
		a.putValue(Action.SHORT_DESCRIPTION, Translator.swap("MainFrame.restoreView")); // button
																						// tooltiptext
		viewMenu.add(a, Translator.swap("MainFrame.retoreView.shortcut"));

		a = new CommandAction(Translator.swap("MainFrame.cleanView"), null, new CleanView(), this);
		a.putValue(Action.SHORT_DESCRIPTION, Translator.swap("MainFrame.cleanView")); // button
																						// tooltiptext
		viewMenu.add(a, Translator.swap("MainFrame.cleanView.shortcut"));

		// fc-9.8.2018 moved Options to view, removed edit menu (useless)
		// fc-26.8.2008 - added VK_COMMA
		icon = IconLoader.getIcon("option_16.png");
		a = new CommandAction(Translator.swap("MainFrame.editOptions"), icon, new Options(), this);
		a.putValue(Action.SHORT_DESCRIPTION, Translator.swap("MainFrame.editOptions")); // button
																						// tooltiptext
		viewMenu.add(a, KeyEvent.VK_COMMA, Translator.swap("MainFrame.editOptions.shortcut"));

		menuBar.add(viewMenu);

		// Tools (dynamic load of extensions) ------------------------
		Collection<String> toolsClassNames = CapsisExtensionManager.getInstance()
				.getExtensionClassNames(CapsisExtensionManager.GENERIC_TOOL);

		// Create Tool menu only if needed
		if (toolsClassNames != null && !toolsClassNames.isEmpty()) {
			toolMenu = new JSmartMenu(Translator.swap("MainFrame.tools"), Translator.swap("MainFrame.tools.shortcut"));
			menuBar.add(toolMenu);
		}

		// Add tools entries in menu
		SortedMap<String, Action> map = new TreeMap<String, Action>(); // fc -
																		// 19.9.2005

		if (toolsClassNames != null) {
			for (String className : toolsClassNames) {

				// loads the class if not loaded yet (executes static
				// initialiser for translation)
				try {

					String name = ExtensionManager.getName(className);
					a = new CommandAction(name, null, new Tool(className, this), this);

					map.put(name, a);

				} catch (Throwable t) {
					Log.println(Log.ERROR, "MainFrame.createMainMenu", "Unable to load extension <" + className + ">",
							t);
				} // failure is not important here
			}
		}

		// fc - 19.9.2005 - Tools menu entries are now sorted
		for (String name : map.keySet()) {
			toolMenu.add(map.get(name));
		}

		// Help ----------------------------------
		helpMenu = new JSmartMenu(Translator.swap("MainFrame.help"), Translator.swap("MainFrame.help.shortcut"));

		icon = IconLoader.getIcon("help2_16.png");
		a = new CommandAction(Translator.swap("MainFrame.capsisHelp"), icon, new CapsisHelp(), this);
		a.putValue(Action.SHORT_DESCRIPTION, Translator.swap("MainFrame.capsisHelp")); // button
																						// tooltiptext
		helpMenu.add(a, KeyEvent.VK_F1, Translator.swap("MainFrame.capsisHelp.shortcut"), 0);
		// stepMenu.add (a, KeyEvent.VK_E,
		// Translator.swap("MainFrame.evolution.shortcut"),
		// InputEvent.CTRL_MASK);
		a.setEnabled(true);

		icon = IconLoader.getIcon("empty_16.png");
		a = new CommandAction(Translator.swap("MainFrame.helpContents"), icon, new HelpContents(), this);
		a.putValue(Action.SHORT_DESCRIPTION, Translator.swap("MainFrame.helpContents")); // button
																							// tooltiptext
		// ~ helpMenu.add (a, KeyEvent.VK_F1,
		// Translator.swap("MainFrame.helpContents.shortcut"), 0);
		helpMenu.add(a, Translator.swap("MainFrame.helpContents.shortcut"));
		// stepMenu.add (a, KeyEvent.VK_E,
		// Translator.swap("MainFrame.evolution.shortcut"),
		// InputEvent.CTRL_MASK);
		a.setEnabled(true);

		helpMenu.addSeparator();

		icon = IconLoader.getIcon("empty_16.png");
		a = new CommandAction(Translator.swap("MainFrame.helpTutorial"), icon, new Tutorial(), this);
		a.putValue(Action.SHORT_DESCRIPTION, Translator.swap("MainFrame.helpTutorial")); // button
																							// tooltiptext
		helpMenu.add(a, Translator.swap("MainFrame.helpTutorial.shortcut"));
		// ~ a.setEnabled (false);

		icon = IconLoader.getIcon("empty_16.png");
		a = new CommandAction(Translator.swap("MainFrame.helpFaq"), icon, new Faq(), this);
		a.putValue(Action.SHORT_DESCRIPTION, Translator.swap("MainFrame.helpFaq")); // button
																					// tooltiptext
		helpMenu.add(a, Translator.swap("MainFrame.helpFaq.shortcut"));
		// ~ a.setEnabled (false);

		icon = IconLoader.getIcon("home_16.png");
		a = new CommandAction(Translator.swap("MainFrame.helpCapsisOnTheWeb"), icon, new CapsisOnTheWeb(), this);
		a.putValue(Action.SHORT_DESCRIPTION, Translator.swap("MainFrame.helpCapsisOnTheWeb")); // button
																								// tooltiptext
		helpMenu.add(a, Translator.swap("MainFrame.helpCapsisOnTheWeb.shortcut"));

		icon = IconLoader.getIcon("license_16.png");
		a = new CommandAction(Translator.swap("MainFrame.helpLicence"), icon, new Licence(), this);
		a.putValue(Action.SHORT_DESCRIPTION, Translator.swap("MainFrame.helpLicence")); // button
																						// tooltiptext
		helpMenu.add(a, Translator.swap("MainFrame.helpLicence.shortcut"));
		// a.setEnabled (false);

		// Debug Action ******************************
		/*
		 * helpMenu.addSeparator ();
		 * 
		 * icon = new IconLoader ().getIcon ("stop_16.png"); a = new
		 * CommandAction (Translator.swap ("MainFrame.debug"), icon, new Debug
		 * (), this); helpMenu.add (a,
		 * Translator.swap("MainFrame.debug.shortcut")); a.setEnabled (true);
		 */
		// **********************************

		helpMenu.addSeparator();

		// icon = IconLoader.getIcon ("help_16.png");
		// a = new CommandAction (Translator.swap ("MainFrame.helpAbout"), icon,
		// new About (), this);
		// a.putValue (Action.SHORT_DESCRIPTION, Translator.swap
		// ("MainFrame.helpAbout")); // button tooltiptext
		// helpMenu.add (a, Translator.swap("MainFrame.helpAbout.shortcut"));
		helpMenu.add(commandManager.getCommand(About.class));

		menuBar.add(helpMenu);

		return menuBar;
	}

	/**
	 * From ActionHolder interface. Used in CommandActions constructor to
	 * register themselves.
	 */
	public void registerAction(String key, Action a) {
		commandName_action.put(key, a);
	}

	/**
	 * From ActionHolder interface. Never returns null.
	 */
	public Action getAction(String key) {

		Action a = commandName_action.get(key);
		if (a != null) {
			return a;
		} else {
			return new DummyAction(); // this action does nothing
		}

	}

	public SelectableAction getSelectableAction(String key) {
		return (SelectableAction) getAction(key);
	}

	public ProjectManager getProjectManager() {
		return projectManager;
	}

	private void setLookAndFeel() {

		UIManager.installLookAndFeel("Kunststoff", "com.incors.plaf.kunststoff.KunststoffLookAndFeel");
		UIManager.installLookAndFeel("TonicLookAndFeel", "com.digitprop.tonic.TonicLookAndFeel");

		// fc-4.12.2017
		UIManager.installLookAndFeel("LiquidLookAndFeel", "com.birosoft.liquid.LiquidLookAndFeel");

		// Try last look and feel
		try {
			UIManager.setLookAndFeel(Settings.getProperty("capsis.last.lookandfeel", ""));

		} catch (Exception e) {
			// In case of trouble, install the system look and feel
			try {
				// This one is always available
				UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());

			} catch (Exception e2) { // ...should always be available
				System.out.println("Look and feel error " + e2);
			}
		}

	}

	public Selector getSelector() {
		return selector;
	}

	/**
	 * Was in Positioner.
	 */
	private void initPositionAndSize(JFrame frame) {

		final String className = frame.getClass().getName();
		LocationSizeManager.registerListener(frame, className);

		// Add a listener to detect frame maximization
		frame.addWindowStateListener(new WindowStateListener() {
			@Override
			public void windowStateChanged(WindowEvent e) {
				boolean maximized = (e.getNewState() == JFrame.MAXIMIZED_BOTH);
				Settings.setProperty(className + "is.maximized", maximized);
			}
		});

		// Restore maximized state if needed
		if (Settings.getProperty(className + "is.maximized", false)) {
			// This is not exactly a maximization but I did not find a better
			// way
			GraphicsEnvironment env = GraphicsEnvironment.getLocalGraphicsEnvironment();
			frame.setBounds(env.getMaximumWindowBounds());
			frame.setExtendedState(frame.MAXIMIZED_BOTH);
			return; // if max, do not go further
		}

		// Get screen size
		Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
		double ratio = ((double) screenSize.width) / screenSize.height;
		boolean singleScreen = ratio <= 2 ? true : false;
		Log.println(Log.INFO, "Positioner.layoutComponent (JFrame)",
				"Screen ratio (width/height)=" + ratio + " singleScreen=" + singleScreen);

		// Frame size restoration strategy
		try {
			// Restore size
			Dimension size = LocationSizeManager.restoreSize(className);
			if (size == null) {
				throw new Exception("size was not stored");
			}
			if (size.width <= 0 || size.height <= 0) {
				throw new Exception("size was not set");
			}
			if (size.width > screenSize.width || size.height > screenSize.height) {
				throw new Exception("size is bigger than screen size");
			}
			frame.setSize(size);

		} catch (Exception e) {

			try {
				// Otherwise use preferred size
				Dimension size = frame.getPreferredSize();
				if (size.width <= 0 || size.height <= 0) {
					throw new Exception("preferred size was not set");
				}
				if (size.width > screenSize.width || size.height > screenSize.height) {
					throw new Exception("size is bigger than screen size");
				}
				frame.setSize(size);

			} catch (Exception e2) {
				// Otherwise set a default size
				frame.setSize(FRAMES_DEFAULT_WIDTH, FRAMES_DEFAULT_HEIGHT);
			}
		}

		// Frame location restoration strategy
		// NOTE: windows.located.by.capsis can be set to true/false in
		// etc/capsis.properties

		boolean wlbc = !Settings.getProperty("veto.dialog.location.memorization", false); // default
																							// value
																							// =
																							// false

		if (!wlbc) {
			// Location is set by the System Desktop Manager
			frame.setLocationByPlatform(true);

		} else {

			try {
				// Restore location
				if (!LocationSizeManager.hasStoredLocation (className))
					throw new Exception (); // fc-4.4.2019
				
				Point p = LocationSizeManager.restoreLocation(className);
				
				if (p == null)
					throw new Exception (); // fc-4.4.2019
				
				if (!AmapDialog.isThisPointVisibleOnScreens(p)) { // fc-7.9.2015
					throw new Exception("This location is not visible on screens (maybe a screen was removed): " + p);
				}
				if (p.x <= 0 || p.y <= 0) {
					throw new Exception("location was not set");
				}

				// fc-7.2.2018 location bug under Linux, see also AmapDialog

				// Trouble: the insets value changes when restarting Capsis
				// under Linux,
				// it is reported that maybe Compiz and Java do not cooperate
				// well for
				// this particular point... -> We have ins.top = 28 or 0

				// Ref of the bug:
				// https://bugs.java.com/view_bug.do?bug_id=6632124
				// JDK-6632124 : fix for 6429775 was incomplete

				// Checked under Windows, no problem, everything's ok, this is
				// under
				// Linux only...
				if (AmapTools.isLinuxPlatform()) {

					// Top value of insets should be approx. 28 under Linux,
					// height of title bar
					Insets ins = this.getInsets(); // ok
					int titleBarHeight = ins.top; // approx. 28 pixels, or 0...

					int correction = 28; // default
					if (titleBarHeight > 0)
						correction = titleBarHeight;

					p = new Point(p.x, p.y - correction);

				}

				// fc-7.2.2018 location bug under Linux, see also AmapDialog

				frame.setLocation(p);

			} catch (Exception e) {
				// Trouble: center the frame on screen
				// be careful, there may be several screens and they may have
				// various sizes
				if (singleScreen) {
					// Center the frame on single screen
					frame.setLocation((screenSize.width - frame.getWidth()) / 2,
							(screenSize.height - frame.getHeight()) / 2);
				} else {

					frame.setLocationByPlatform(true); // location is set by the
														// system desktop
														// manager

				}

			}

		}

	}

}
