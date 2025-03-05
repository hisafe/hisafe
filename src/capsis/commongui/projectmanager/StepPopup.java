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
package capsis.commongui.projectmanager;

import java.awt.Component;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.SetMap;
import capsis.commongui.command.ChangeStepColor;
import capsis.commongui.command.CommandManager;
import capsis.commongui.command.DeleteStep;
import capsis.commongui.command.Evolution;
import capsis.commongui.command.Intervention;
import capsis.gui.MainFrame;
import capsis.kernel.GModel;
import capsis.kernel.Step;

/**
 * StepPopup is popup menu which can be opened on a StepButton in a
 * ProjectManager.
 * 
 * @author F. de Coligny - December 2009
 */
public class StepPopup extends JPopupMenu {

	private StepButton stepButton;

	private static Collection<ActionCommand> actionCommands;
	private static boolean replaceGenericCommands;
	private static SetMap<Class<? extends GModel>, ActionCommand> specificActionCommands;

	/**
	 * Constructor
	 */
	public StepPopup(StepButton stepButton) {
		super();

		this.stepButton = stepButton;

		CommandManager commandManager = CommandManager.getInstance();

		// Default generic popup, part 1.
		if (!replaceGenericCommands) {

			add(commandManager.getCommand(Evolution.class));
			add(commandManager.getCommand(Intervention.class)); // added on
																// 14.10.2010
			add(commandManager.getCommand(DeleteStep.class));

		}

		// actionCommands added by add/setActionCommands (...)
		if (StepPopup.actionCommands != null) {

			if (getComponentCount() > 0) {
				addSeparator();
			}

			for (ActionCommand a : StepPopup.actionCommands) {
				if (a == null) {
					this.addSeparator();
				} else {
					this.add(a);
				}
			}

		}

		// Build optional part of the popup specific to the linked model
		if (StepPopup.specificActionCommands != null) {

			// Get the class of the linked model
			Class<? extends GModel> modelClass = stepButton.getStep().getProject().getModel().getClass();

			// If there are specific commands for this model AND commands were
			// already
			// added, then add a separator
			if (!StepPopup.specificActionCommands.getObjects(modelClass).isEmpty() && getComponentCount() > 0) {
				addSeparator();
			}

			for (ActionCommand a : StepPopup.specificActionCommands.getObjects(modelClass)) {
				if (a == null) {
					this.addSeparator();
				} else {
					this.add(a);
				}
			}

		}

//		// fc-3.1.2017 Added a command to change the stepButton's color
//		if (stepButton.isColored())
//			add(new ChangeStepColor(MainFrame.getInstance()));

		// Default generic popup, part 2.
		if (!replaceGenericCommands) {

			addSeparator();

			add(commandManager.getCommand("ShowStep1"));
			add(commandManager.getCommand("ShowStep5"));
			add(commandManager.getCommand("ShowStep10"));
			add(commandManager.getCommand("ShowStep20"));

		}

	}

	/**
	 * Set a list of actions that all the instances of this popup will contain.
	 * They will replace the generic commands.
	 */
	public static void setActionCommands(List<ActionCommand> actionCommands) {
		replaceGenericCommands = true;
		addActionCommands(actionCommands);
	}

	/**
	 * Set a list of actions that the instances of this popup will contain when
	 * opened on projects with the given model linked
	 */
	public static void setActionCommands(Class<? extends GModel> modelClass, List<ActionCommand> actionCommands) {
		replaceGenericCommands = true;
		addActionCommands(modelClass, actionCommands);
	}

	/**
	 * Add a generic list of actions that all the instances of this popup will
	 * contain. They will be added to the generic commands.
	 */
	public static void addActionCommands(List<ActionCommand> actionCommands) {
		if (StepPopup.actionCommands == null) {
			StepPopup.actionCommands = new ArrayList<ActionCommand>();
		}
		StepPopup.actionCommands.addAll(actionCommands);
	}

	/**
	 * Optional, add a list of actions that the instances of this popup will
	 * contain when opened on projects with the given model linked
	 */
	public static void addActionCommands(Class<? extends GModel> modelClass, List<ActionCommand> actionCommands) {
		if (StepPopup.specificActionCommands == null) {
			StepPopup.specificActionCommands = new SetMap<Class<? extends GModel>, ActionCommand>();
		}
		StepPopup.specificActionCommands.addObjects(modelClass, actionCommands);
	}

	// Test
	public static final void main(String[] a) {
		JMenu m = new JMenu("Menu");
		m.add(new JMenuItem("1"));
		m.add(new JMenuItem("2"));
		m.add(new JMenuItem("3"));

		JMenuItem i0 = m.getItem(0);
		Component c0 = m.getMenuComponent(0);

		System.out.println("i0 = " + i0);
		System.out.println("c0 = " + c0);

	}

}
