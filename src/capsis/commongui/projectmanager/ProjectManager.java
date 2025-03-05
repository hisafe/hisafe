/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2010 Francois de Coligny, Samuel Dufour
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library;
 * if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 */
package capsis.commongui.projectmanager;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.border.Border;

import capsis.kernel.Engine;
import capsis.kernel.Project;
import capsis.kernel.Session;
import capsis.kernel.Step;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.ListenedTo;
import jeeb.lib.util.Listener;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.gui.NorthPanel;
import jeeb.lib.util.task.TaskManager;
import jeeb.lib.util.task.TaskManagerView;

/**
 * A project manager for applications based on the capsis kernel. If the
 * "project.manager.compact.mode" Settings property is true, the display will be
 * more compact (see StepButton).
 * 
 * @author F. de Coligny - August 2009
 */
public class ProjectManager extends JPanel implements Listener {

	static {
		Translator.addBundle("capsis.commongui.projectmanager.ProjectManager");
	}
	static private ProjectManager instance;

	private JFrame frame;
	private JScrollPane scroll;
	private ColumnPanel content;

	private Color selectionColor; // steps selection for visibility tuning
	private Color currentColor; // current project / step selection

	private Border currentProjectBorder;
	private Border currentStepBorder;
	private Border selectedStepBorder;

	// private StepButton current;
	// private StepButton previous;

	// Main Map to get the stepButton for a given Step
	private Map<Step, StepButton> step_stepButton;

	// Management for single step button selection (matching the current step)
	private JButtonGroup singleSelectionGroup;

	// A range of selected StepButtons (during Expand...)
	private Collection<StepButton> selectedButtons;

	private RelatedColorProvider colorProvider; // fc-25.9.2012

	// Optional, may be null
	private TaskManagerView tmView; // fc-29.9.2021

	/**
	 * Constructor. Must be called once before getInstance (), takes parameters.
	 */
	public ProjectManager(JFrame frame, boolean withTaskManagerView) {

		// fc-29.9.2021 Added withTaskManager (to save space in the statusBar)

		super(new BorderLayout());

		if (instance != null) {
			Log.println(Log.ERROR, "ProjectManager.c ()", "This constructor cannot be called twice, design error");
			System.out.println("This constructor cannot be called twice, design error, aborting");
			System.exit(-1);
		}

		instance = this;
		this.frame = frame;
		this.step_stepButton = new HashMap<Step, StepButton>();

		singleSelectionGroup = new JButtonGroup();

		// Create user interface
		content = new ColumnPanel(0, 0);

		// fc-10.10.2023 tuning (JSM, Cstab, wrong layout with thousands of steps)
		LinePanel aux = new LinePanel ();
		aux.add(content);
		aux.addGlue();

//		JPanel aux = new JPanel(new BorderLayout()); // for better layout
//		aux.add(content, BorderLayout.NORTH);

		// fc-10.10.2023 tuning (JSM, Cstab, wrong layout with thousands of steps)

		scroll = new JScrollPane(new NorthPanel (aux));
//		scroll = new JScrollPane(aux);

		scroll.setBorder(null);

		// Better wheel scrolling management (nov 2011)
		aux.addMouseWheelListener(new MouseWheelListener() {

			public void mouseWheelMoved(MouseWheelEvent event) {
				final JScrollBar scrollBar = scroll.getVerticalScrollBar();
				final int rotation = event.getWheelRotation();
				if (scrollBar != null) {
					scrollBar.setValue(scrollBar.getValue() + 20 * rotation);
				}
			}
		});

		add(scroll, BorderLayout.CENTER);

		// fc-29.9.2021
		if (withTaskManagerView)
			addTaskManagerView(); // at the SOUTH

		// We will be told when current step / project changes
		Current.getInstance().addListener(this);

		colorProvider = new RelatedColorProvider();

		update();
	}

	/**
	 * Add a taskManagerView to show the progress of the most recent task directly
	 * in the ProjectManager
	 */
	private void addTaskManagerView() {
		// fc-29.9.2021

		// A TaskManagerView showing the most recent task of the TaskManager
		tmView = new TaskManagerView(TaskManager.getInstance(), true); // singleTaskOnly
																		// =
																		// true
		tmView.addListener(this);

		add(tmView, BorderLayout.SOUTH);

	}

	/**
	 * Not exactly a Singleton pattern. Constructor must be called once before
	 * calling getInstance ().
	 */
	public static ProjectManager getInstance() {
		return instance;
	}

	/**
	 * Called by ListenedTo when something happens.
	 */
	public void somethingHappened(ListenedTo l, Object param) {

		if (l.equals(Current.getInstance())) {

			// Step step = Current.getInstance ().getStep ();
			// previous = current;
			// current = getStepButton (step);

			update(); // step changed
		}

	}

	/**
	 * Use this method to update the project manager.
	 */
	public void update() {
		refreshUI();

		revalidate();
		repaint();

		setVisible(true);
	}

	private void refreshUI() {

		// Update colors (may be tuned)
		currentColor = Settings.getProperty("capsis.project.selection.color", new Color(164, 78, 238));
		selectionColor = Color.BLUE;

		currentProjectBorder = BorderFactory.createLineBorder(currentColor, 1);
		currentStepBorder = BorderFactory.createLineBorder(currentColor, 2);
		selectedStepBorder = BorderFactory.createLineBorder(selectionColor, 2);

		// Get the session, test if empty
		Session session = Engine.getInstance().getSession();
		if (session == null || session.isEmpty()) {
			noProjects();
			return;
		}

		// Draw the projects
		content.removeAll();

		List<Project> projects = session.getProjects();

		synchronized (projects) {

			int sumHeight = 0;

			// The last project opened -> in first position (nov 2011)
			List<Project> copy = new ArrayList<Project>(projects);
			if (Settings.getProperty("project.manager.reverse.order", true)) { // default value is
																				// true
				Collections.reverse(copy);
			}

			for (Project p : copy) {

				if (p == null || p.getRoot() == null)
					continue;

				// fc-13.4.2022 Do not show the technical projects (attached to a technical
				// model, e.g. Metatrom or Lilo from EcoAF)
				if (p.isTechnical())
					continue;

				ProjectPanel panel = new ProjectPanel(this, p);
				// We need to set panel location accurately
				panel.setLocation(0, sumHeight);
				sumHeight += panel.getPreferredSize().height;

				content.add(panel);

			}
		}
		content.addGlue();

		// Set selection border / current border for all the StepButtons
		Border buttonDefaultBorder = new JButton().getBorder(); // depends on the Look and Feel
		StepButton currentStepButton = getStepButton(Current.getInstance().getStep());
		for (StepButton b : step_stepButton.values()) {
			if (b == currentStepButton) {
				b.setBorder(currentStepBorder);

			} else if (selectedButtons != null && selectedButtons.contains(b)) {
				b.setBorder(selectedStepBorder);

			} else {
				b.setBorder(buttonDefaultBorder);

			}

		}

		clean();
		// System.out.println(report ());

	}

	/**
	 * Updates the main step_stepButton map to remove the entries for non visible
	 * steps.
	 */
	private void clean() {

		// Update the button group
		singleSelectionGroup = new JButtonGroup();

		for (Iterator<Step> i = step_stepButton.keySet().iterator(); i.hasNext();) {
			Step s = i.next();
			StepButton sb = step_stepButton.get(s);

			// fc-20.4.2011 - try to better free memory when a project is closed
			Step step = sb.getStep();
			Session session = Engine.getInstance().getSession();
			if (!s.isVisible() || !session.getProjects().contains(step.getProject())) {

				// Dispose StepButton
				sb.dispose();

				// Remove entry
				i.remove();
			} else {
				singleSelectionGroup.add(sb);
			}
		}

		// // Trace
		// System.out.println ("PM end-of-clean");
		// for (StepButton sb : step_stepButton.values ()) {
		// System.out.println ("PM end-of-clean: living sb "+sb.getCaption
		// ()+" for project "+sb.getStep ().getProject ());
		// }

	}

	/**
	 * Print a message if the session is empty: 'no projects'.
	 */
	private void noProjects() {
		content.removeAll();
		content.addGlue();

		LinePanel l0 = new LinePanel();
		l0.addGlue();
		l0.add(new JLabel(Translator.swap("ProjectManager.noProjects")));
		l0.addGlue();
		content.add(l0);

		content.addGlue();

		clean();
	}

	/**
	 * Returns the StepButton list for the given Project.
	 */
	public List<StepButton> getStepButtons(Project p) {
		List<StepButton> buttons = new ArrayList<StepButton>();
		for (StepButton b : step_stepButton.values()) {
			try {
				if (b.getStep().getProject().equals(p)) {
					buttons.add(b);
				}
			} catch (Exception e) {
			} // ignore
		}
		return buttons;

	}

	/**
	 * Returns the StepButton for the given Step. Returns null if not found.
	 */
	public StepButton getStepButton(Step step) {
		return step_stepButton.get(step); // null if not found
	}

	/**
	 * Returns the StepButton for the given Step. If not found, creates the
	 * StepButton. Never returns null except if the given step is null.
	 */
	protected StepButton getCreateStepButton(Step step) {
		if (step == null) {
			return null;
		}

		StepButton sb = step_stepButton.get(step);

		if (sb == null) {
			sb = new StepButton(this, step);

			// Only one StepButton selected in the session
			singleSelectionGroup.add(sb);

			// Memo the new StepButton in our main Map
			step_stepButton.put(step, sb);

		}
		return sb;
	}

	/**
	 * Put all the StepButtons between first and second in a selection range. The
	 * buttons in the range are shown differently ("selected"). Returns false if
	 * first is not an ancestor of second: wrong range.
	 */
	public boolean setSelectionRange(StepButton first, StepButton second) {
		if (selectedButtons == null) {
			selectedButtons = new HashSet<StepButton>();
		}

		selectedButtons.clear();

		// Only clear the selection range ?
		if (first == null || second == null) {
			return true;
		}

		// Ensure first is the nearest to the root
		if (first.getX() > second.getX()) {
			StepButton aux = first;
			first = second;
			second = aux;
		}

		// Feed the selection range
		StepButton sb = second;
		while (sb != null && sb != first) {
			selectedButtons.add(sb);

			// There was a MISTAKE: all the non visible steps -> null StepButtons
			// !!!!!!!!!!!!!!!!
			sb = getStepButton((Step) sb.getStep().getVisibleFather());
			// sb = getStepButton ((Step) sb.getStep ().getFather ());

		}
		selectedButtons.add(first);

		if (sb != first) { // range is not correct: ignore
			selectedButtons.clear();
		}

		return sb == first; // if first was found on the path: it is an ancestor of second
	}

	/**
	 * Change visibility between previous and current step buttons
	 */
	public void updateVisibilityRange(StepButton b0, StepButton b1, boolean visible) {
		if (b0 == null || b1 == null) {
			return;
		}

		// Choose first and second
		Step first;
		Step second;
		if (b0.getX() < b1.getX()) {
			first = b0.getStep();
			second = b1.getStep();
		} else {
			first = b1.getStep();
			second = b0.getStep();
		}

		if (first.getWidth() > second.getWidth()) {
			MessageDialog.print(this, Translator.swap("ProjectManager.visibilityErrorDifferentBranches"));
			return;
		}

		second = (Step) second.getFather();
		StepButton fb = getStepButton(first);

		while (second != null && second != first) {

			StepButton sb = getStepButton(second);

			if (sb != null && (first.getWidth() == second.getWidth() && fb.getX() > sb.getX())) { // test
																									// button
																									// order
				break;
			}

			second.setVisible(visible);
			second = (Step) second.getFather();
		}

		// Remove selection range
		setSelectionRange(null, null);

		update();
	}

	/**
	 * Returns the project manager's frame
	 */
	public JFrame getFrame() {
		return frame;
	}

	// /** Previously selected step button
	// */
	// public StepButton getPrevious () {return previous;}

	// /** Currently selected step button
	// */
	// public StepButton getCurrent () {return current;}

	/**
	 * Selection border for project
	 */
	public Border getCurrentProjectBorder() {
		return currentProjectBorder;
	}

	/**
	 * Selection range may contain contiguous selected step buttons
	 */
	public Collection<StepButton> getSelectedButtons() {
		return selectedButtons;
	}

	public String getName() {
		return Translator.swap("ProjectManager.name");
	}

	public Color getSelectionColor() {
		return selectionColor;
	}

	public Color getCurrentColor() {
		return currentColor;
	}

	public String report() {
		Set<String> list = new TreeSet<String>();
		for (Map.Entry<Step, StepButton> entry : step_stepButton.entrySet()) {
			list.add(entry.getKey() + " " + entry.getValue());
		}
		StringBuffer b = new StringBuffer("ProjectManager report");
		b.append('\n');
		for (String s : list) {
			b.append(s);
			b.append('\n');
		}
		b.append('\n');
		return b.toString();
	}

	/**
	 * The colorProvider may be changed
	 */
	public void setColorProvider(RelatedColorProvider cp) {
		colorProvider = cp;
	}

	public RelatedColorProvider getColorProvider() {
		return colorProvider;
	}

}
