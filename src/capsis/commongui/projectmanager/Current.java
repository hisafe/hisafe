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

import java.util.ArrayList;
import java.util.List;

import jeeb.lib.util.ListenedTo;
import jeeb.lib.util.Listener;
import capsis.kernel.Engine;
import capsis.kernel.Project;
import capsis.kernel.Step;

/**
 * This class memorizes current project and step. Use Current.getInstance
 * ().setStep (currentStep) and Current.getInstance ().setProject
 * (currentProject).
 * 
 * @author F. de Coligny - august 2009
 */
public class Current implements ListenedTo {
	public static final String STEP_CHANGED = "step.changed";
	public static final String FORCE_UPDATE = "force.update";

	private static Current instance; // Singleton pattern
	private List<Listener> listeners; // ListenedTo interface

	private Step step;
	private Project project;

	private Step previousStep;
	private Project previousProject;

	// fc - 18.5.2010 - removed doubleSelection, managed in ButtonColorer
	// private boolean doubleSelection;

	// fc-9.3.2016 During tellSomethingHappened(), some listeners may ask to be
	// forgotten
	private List<Listener> listenersToBeForgotten;

	/**
	 * Singleton pattern
	 */
	public static Current getInstance() {
		if (instance == null) {
			instance = new Current();
		}
		return instance;
	}

	/**
	 * Singleton pattern
	 */
	private Current() {
	}

	/**
	 * Sets the current step.
	 */
	public void setStep(Step step) {
		Project _project = (step == null) ? null : step.getProject();
		set(step, _project);
	}

	/**
	 * Sets the current project.
	 */
	public void setProject(Project project) {
		Step _step = (project == null) ? null : (Step) project.getRoot();
		set(_step, project);
	}

	/**
	 * Set the given project and step as current.
	 */
	private void set(Step step, Project project) {

		// Remember the previous state ONLY if the previous project was not
		// closed
		// This could lead in keeping references on projects, a kind of memory
		// leak - fc-19.4.2011
		// fc-19.4.2011 // still in session, not closed
		if (Engine.getInstance().getSession().getProjects().contains(this.project)) { // fc-19.4.2011
			this.previousStep = this.step;
			this.previousProject = this.project;
		} else { // fc-19.4.2011
			this.previousStep = null; // fc-19.4.2011
			this.previousProject = null; // fc-19.4.2011
		} // fc-19.4.2011
			// Store the new state
		this.step = step;
		this.project = project;

		// Notify listeners
		tellSomethingHappened(Current.STEP_CHANGED);

	}

	/**
	 * Tell the listeners to update without any cache nor memories.
	 */
	public void forceUpdate() {
		tellSomethingHappened(Current.FORCE_UPDATE);
	}

	/**
	 * Returns the current step.
	 */
	public Step getStep() {
		return step;
	}

	/**
	 * Returns the current project.
	 */
	public Project getProject() {
		return project;
	}

	/**
	 * Returns the previous step.
	 */
	public Step getPreviousStep() {
		return previousStep;
	}

	/**
	 * Returns the previous project.
	 */
	public Project getPreviousProject() {
		return previousProject;
	}

	/**
	 * Returns true if the currentStep was selected twice. i.e. double click in
	 * the ProjectManager.
	 */
	// public boolean isDoubleSelection () {
	// return doubleSelection;
	// }

	/**
	 * Returns true if the given step is the current step.
	 */
	public boolean isStep(Step step) {
		if (step == null) {
			return false;
		}
		return step.equals(step);
	}

	/**
	 * Returns true if the given project is the current project.
	 */
	public boolean isProject(Project project) {
		if (project == null) {
			return false;
		}
		return project.equals(project);
	}

	/**
	 * Add a listener to this object. ListenedTo interface
	 */
	public void addListener(Listener l) {
		if (listeners == null) {
			listeners = new ArrayList<Listener>();
		}
		listeners.add(l);
	}

	/**
	 * Remove a listener to this object. ListenedTo interface
	 */
	public void removeListener(Listener l) {
		if (listeners == null) {
			return;
		}
		listeners.remove(l);
	}

	/**
	 * Notify all the listeners by calling their somethingHappened (listenedTo,
	 * param) method. ListenedTo interface
	 */
	public void tellSomethingHappened(Object param) {
		if (listeners == null) {
			return;
		}
		
//		System.out.println("Current calling somethingHappened () for listeners: #"+listeners.size());
		
		for (Listener l : listeners) {
			l.somethingHappened(this, param);
		}

		// fc-9.3.2016 listeners may ask for deregistration in their
		// somethingHappened ()
		if (listenersToBeForgotten != null && !listenersToBeForgotten.isEmpty())
			for (Listener l : listenersToBeForgotten)
				removeListener(l);
	}

	/**
	 * Can be called by any listener, for example during their somethingHappened
	 * () method, to ask for deregistration.
	 * 
	 * @param l
	 */
	public void pleaseForgetMe(Listener l) {
		if (listenersToBeForgotten == null)
			listenersToBeForgotten = new ArrayList<>();

		listenersToBeForgotten.add(l);
	}

	public String toString() {
		return "\nCurrent  project " + project + " step " + step + "\nPrevious project " + previousProject + " step "
				+ previousStep
		// +"\ndoubleSelection "+doubleSelection
		;
	}

}
