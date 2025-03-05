/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski,
 * 
 * This file is part of Capsis Capsis is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the Free Software
 * Foundation, either version 2.1 of the License, or (at your option) any later version.
 * 
 * Capsis is distributed in the hope that it will be useful, MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU lesser General Public License along with Capsis. If
 * not, see <http://www.gnu.org/licenses/>.
 * 
 */

package capsis.commongui.projectmanager;

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jeeb.lib.util.ListMap;
import capsis.kernel.Project;

/**
 * ButtonColorer can add a color on a step. It can be listened by listeners to
 * be told when the colors move from a step to another or when they are removed.
 * It provides a way to synchronize tools on steps.
 * 
 * @author F. de Coligny - April 2010
 */

public class ButtonColorer {

	static private ButtonColorer instance;

	// Remembers for each project the colored stepButtons
	// Key is a project, value is a list of stepButtons
	private ListMap<Project, StepButton> history;

	// Listeners must be told when colors change, are moved...
	private List<ButtonColorerListener> listeners;

	/**
	 * Singleton pattern
	 */
	static public ButtonColorer getInstance() {
		if (instance == null)
			instance = new ButtonColorer();

		return instance;
	}

	/**
	 * Singleton pattern: private constructor
	 */
	private ButtonColorer() {
		history = new ListMap<Project, StepButton>();

	}

	/**
	 * Assigns a color to a StepButton Called when a ButtonColorer listening tool is
	 * created
	 */
	public void newColor(StepButton b) {

		// If button is already colored, do nothing
		boolean stepButtonIsNotColored = b.getColor() == null || b.getColor().equals(StepButton.DEFAULT_COLOR);
		if (!stepButtonIsNotColored)
			return;

		b.colorize();

		history.addObject(b.getStep().getProject(), b);

	}

	/**
	 * Sets the given stepButton as the most recent in its project's history. E.g.
	 * when a colored stepButton is clicked again, before a move.
	 */
	public void rearm(StepButton b) {
		// Only interesting for colored buttons
		if (!b.isColored())
			return;

		// Update history
		Project p = b.getStep().getProject();
		history.removeObject(p, b);
		history.addObject(p, b);

	}

	/**
	 * Notify a color move to the listeners. The given parameter is the target
	 * button, the one which was clicked.
	 */
	public void moveColor(StepButton b) {

		// Get the previous button in the history for the same project than b
		Project p = b.getStep().getProject();
		StepButton prevButton = null;

//		System.out.println("ButtonColorer > moveColor ()...");
//		System.out.println("ButtonColorer > moveColor () b: " + b);

		try {
			List<StepButton> prevButtons = history.getObjects(p);

			// The last one in the history: the most recent
			prevButton = prevButtons.get(prevButtons.size() - 1);

//			System.out.println("ButtonColorer > moveColor () prevButton: " + prevButton);

			// Special case: if b is colored
			// moveColor is called on a 2-click
			// the first of the 2 clicks may have rearmed b
			// if prevButton == b, cancel the rearm and consider the previous
			// one again
			// fc-20.10.2021 Added check on history size (PDG bug, viewers do not sync)
			if (prevButton == b && history.get(p).size() > 1) {
//			if (prevButton == b) {

				// Remove b from history
				history.removeObject(p, b);
				// Last in history
				prevButtons = history.getObjects(p);
				prevButton = prevButtons.get(prevButtons.size() - 1);
			}

		} catch (Exception e) {

			// fc-20.10.2021 Debugging PDG viewers updating
//			System.out.println("ButtonColorer > moveColor () exception");
//			e.printStackTrace(System.out);

			return; // nothing in history...
		}

		if (prevButton == null) {

			// fc-20.10.2021 Debugging PDG viewers updating
//			System.out.println("ButtonColorer > moveColor () prevButton == null");

			return;
		}

		if (!prevButton.isColored()) {

			// fc-20.10.2021 Debugging PDG viewers updating
//			System.out.println("ButtonColorer > moveColor () !prevButton.isColored()");

			return;
		}

		// b.colorize ();
		// prevButton.uncolorize ();

		// Use the same color (stays unavailable in the colorProvider)
		Color uc = prevButton.getColor();
		prevButton.uncolorize();
		b.colorize(uc);

		// Update history
		history.removeObject(p, prevButton);
		history.addObject(p, b);

		// Tell the listeners
		if (listeners == null) {

			// fc-20.10.2021 Debugging PDG viewers updating
//			System.out.println("ButtonColorer > moveColor () listeners == null");

			return;
		}

		for (ButtonColorerListener l : listeners) {
			l.colorMoved(prevButton, b);
		}

	}

	/**
	 * Removes the color of a StepButton, notifies the listeners May be called when
	 * a ButtonColorer listening tool is disposed
	 */
	public void removeColor(StepButton b) {

		// Set the user color available again
		Color uc = b.getColor();

		// fc-9.11.2020 Unused
//		if (uc != null)
//			uc.setAvailable(true); // can be reused for another StepButton

		b.uncolorize();

		// Update history
		try {
			history.removeObject(b.getStep().getProject(), b);
		} catch (Exception e) {
		}

		// Tell the listeners
		if (listeners == null)
			return;

		for (ButtonColorerListener l : new ArrayList<ButtonColorerListener>(listeners)) {
			l.colorRemoved(b);
		}

	}

	/**
	 * Changes the color of the given step button
	 */
	public void changeColor(StepButton b, Color newColor) { // fc-3.1.2017

		// Set the user color available again
		Color uc = b.getColor();
//		UserColor uc = b.getColor();

		// fc-9.11.2020 Unused
//		if (uc != null)
//			uc.setAvailable(true); // can be reused for another StepButton

		b.uncolorize();

		// fc-9.11.2020 Removed UserColor
		b.colorize(newColor);
//		b.colorize(new UserColor (newColor));

		// Tell the listeners
		if (listeners == null)
			return;

		for (ButtonColorerListener l : listeners) {
			l.colorMoved(b, b);
		}
	}

	/**
	 * Check if noone listens to the given stepButton and if so, remove its color.
	 */
	public void seeIfColorShouldBeRemoved(StepButton sb) { // fc-2.1.2017
		if (listeners == null)
			return;

		boolean atLeastOneListener = false;

		for (ButtonColorerListener l : listeners) {
			if (l.isListening(sb)) {
				atLeastOneListener = true;
				break;
			}
		}

		if (!atLeastOneListener)
			removeColor(sb);

	}

	/**
	 * Disposes all the StepButtons having their Step reference equals to null,
	 * notifies the listeners. May be called after a project is closed.
	 */
	public void clean() {
		// Deferred history removal to cope with ConcurrentModificationException
		Map<StepButton, Project> trashCan = new HashMap<StepButton, Project>();

		for (Project p : history.getKeys()) {

			List<StepButton> list = history.getObjects(p);

			for (StepButton b : list) {

				if (b.shouldBeDisposed()) {
					// Dispose unused StepButton
					b.dispose();
					trashCan.put(b, p);
				}
			}
		}

		// b.dispose () calls removeColor (b) (see upper)
		// The history removal may have failed in removeColor (b)
		// due to project closing context (project null reference)
		// -> do it again
		for (StepButton b : trashCan.keySet()) {
			Project p = trashCan.get(b);
			history.removeObject(p, b);
		}
	}

	/**
	 * Add a step colorer listener
	 */
	public void addListener(ButtonColorerListener l) {
		if (listeners == null)
			listeners = new ArrayList<ButtonColorerListener>();

		listeners.add(l);
	}

	/**
	 * Remove a step colorer listener
	 */
	public void removeListener(ButtonColorerListener l) {
		if (listeners == null)
			return;

		listeners.remove(l);
	}

}
