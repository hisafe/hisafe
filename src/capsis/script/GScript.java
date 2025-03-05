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

package capsis.script;

import java.util.ArrayList;
import java.util.Collection;

import capsis.kernel.Engine;
import capsis.kernel.EvolutionParameters;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.InitialParameters;
import capsis.kernel.Project;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.Intervener;
import capsis.kernel.extensiontype.Memorizer;
import capsis.kernel.extensiontype.OFormat;
import capsis.util.group.Group;
import capsis.util.group.GroupableIntervener;
import capsis.util.group.Grouper;
import jeeb.lib.util.Log;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * GScript is a superclass for all Capsis scripts. New release, trying to get
 * simplier.
 * 
 * See the script doc on the Capsis web site documentation pages.
 * 
 * @author F. de Coligny - september 2002
 */
public abstract class GScript {

	protected GModel model;
	protected Project project;
	protected Step currentStep;

	// public GScript (String[] args) {} // subclasses should have a constructor
	// of this form

	public abstract void run() throws Exception; // subclasses should have a run
													// () method

	/**
	 * Loads a model from its package name (e.g. "mountain" or ).
	 */
	public GModel loadModel(String modelPackageName) {

		// - Important note -
		// If you have a "Design error..." problem here while in a script
		// with a main(String[]) method, consider extending C4Script (deals
		// with capsis.kernel.Engine initialisation in a static initializer)
		// If you are not in Capsis, extend the matching class (SimeoScript...)

		model = null;
		try {
			model = Engine.getInstance().loadModel(modelPackageName, "script");
		} catch (Exception e) {
			e.printStackTrace(System.out);
		}
		return model;
	}

	/**
	 * Creates a project with the given model and InitialParameters. The root step
	 * is created and associated to the initial scene found in InitialParameters.
	 * e.g. : Project p1 = createProject ("a", "maddmodel", ip);
	 */
	public Project createProject(String projectName, GModel model, InitialParameters ip) throws Exception {
		resetCurrentStep();

		// Create Project
		project = Engine.getInstance().processNewProject(projectName, model);
		project.initialize(ip);

		return project;
	}

	/**
	 * Set a memorizer to the given project
	 */
	public void setMemorizer(Project project, Memorizer memorizer) throws Exception {
		boolean verbose = false;
		try {
			project.setMemorizer(memorizer);
			if (verbose) {
				StatusDispatcher.print(
						"Memorizer " + memorizer.getClass().getName() + " was correctly set for project " + project);
			}
		} catch (Exception e) {
			StatusDispatcher.print("Error while setting memorizer " + e);
			e.printStackTrace(System.out);
			throw new Exception("Error while setting memorizer");
		}
	}

	/**
	 * Triggers evolution from the current step.
	 */
	public Step evolve(EvolutionParameters ep) {
		if (currentStep == null) {
			currentStep = (Step) project.getRoot();
		}
		return evolve(currentStep, ep);
	}

	/**
	 * Triggers evolution from the given step according to the given evolution
	 * parameters. Return the last created step.
	 */
	public Step evolve(Step referentStep, EvolutionParameters ep) {

		try {
			currentStep = referentStep.getProject().evolve(referentStep, ep);
			return currentStep;

		} catch (Exception e) {
			Log.println(Log.ERROR, "GScript.evolve ()", "Exception ", e);
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * Close the current project
	 */
	public void closeProject() {
		closeProject(project);
	}

	/**
	 * Close the given project
	 */
	public void closeProject(Project p) {
		Engine.getInstance().processCloseProject(p);
		project = null;
		// MF20210922 There is still a reference to the project in the model member of
		// the script
	}

	/**
	 * Run an intervener on the given step.
	 * 
	 * @param intervener     : the intervener to be run (must have been parametrized
	 *                       with a constructor and must be ready to apply after
	 *                       init ())
	 * @param step           : the step carrying the scene (not changed by
	 *                       intervention) on which the intervention must be run
	 *                       (the scene is copied before intervention)
	 * @param concernedTrees : - NOW UNUSED, see below - a list of trees in the
	 *                       scene on which the intervention should be restricted or
	 *                       null for considering all trees
	 * @param                grouper: the grouper which produced the concernedTrees
	 *                       collection, can be null if no grouper
	 * @return a new step carrying the new scene resulting of the intervention
	 */
	public Step runIntervener(Intervener intervener, Step step, Collection concernedTrees, Grouper grouper) {

		// fc+vf-4.4.2022 Collection concernedTrees now unused, see below

		// fc-8.6.2020 Added grouper (possibly null). If not null, passed to the
		// intervener to allow getSceneArea (scene)

		// We run the intervener on a copy of the scene under the given step
		GScene interventionBase = (GScene) step.getScene().getInterventionBase();

		// fc+vf-4.4.2022
		// The concernedTrees given to this method are not part of the interventionBase
		// (which is created in this method). So the restriction to the group could not
		// occur in script mode until now. This restriction was unused so far.
		// We rerun the grouper in interventionBase to fix this.
		Collection concernedTreesInCopy = null;
		if (grouper != null) {
			concernedTreesInCopy = new ArrayList();
			concernedTreesInCopy = Group.whichCollection(interventionBase, grouper.getType());

			// Apply the grouper on the interventionBase
			concernedTreesInCopy = grouper.apply(concernedTreesInCopy,
					grouper.getName().toLowerCase().startsWith("not "));
		}

		// fc-8.6.2020 before init ()
		if (grouper != null && intervener instanceof GroupableIntervener)
			((GroupableIntervener) intervener).setGrouper(grouper);

		// Init the intervener
		intervener.init(model, step, interventionBase, concernedTreesInCopy);
//		intervener.init(model, step, interventionBase, concernedTrees); // fc+vf-4.4.2022

		// Check compatibility
		String className = intervener.getClass().getName();
		if (!ExtensionManager.matchWith(className, model)) {
			StatusDispatcher.print("GScript.runIntervener (): Could not use intervener " + className
					+ ": intervener is not compatible. ");
			System.exit(0);
		}

		// Check intervener parameters
		if (!intervener.isReadyToApply()) {
			StatusDispatcher.print("GScript.runIntervener (): Could not apply intervener " + className + " on step "
					+ step + ": intervener was not ready to apply. Check var/capsis.log. ");
			System.exit(0);
		}

		// Run the intervener
		try {
			StatusDispatcher.print("Processing Intervention...");

			if (project == null)
				project = step.getProject(); // Added this on 25.4.2012 -bug,
												// script, PhD

			Step newStep = project.intervention(step, intervener, true);

			currentStep = newStep;

			return newStep;

		} catch (Exception e) {
			StatusDispatcher.print("GScript.runIntervener (): Could not apply intervener " + intervener + " on step "
					+ step + " due to " + e);
			e.printStackTrace(System.out);
			System.exit(0);
			return null;
		}

	}

	/**
	 * Shortcut to run an intervener on all the trees of the scene
	 */
	public Step runIntervener(Intervener intervener, Step step) {

		// fc-8.6.2020 no grouper here: null
		return runIntervener(intervener, step, (Collection) null, null);
//		return runIntervener(intervener, step, (Collection) null);

	}

	/**
	 * Run an output format on the given step, write to the given fileName.
	 */
	public void runOFormat(OFormat export, Step s, String fileName) {

		try {
			export.initExport(model, s);
			project.export(s, export, fileName);

		} catch (Exception e) {
			Log.println(Log.ERROR, "GScript.runOFormat ()", "Exception ", e);
			StatusDispatcher.print("GScript.runOFormat (): Could not run oformat " + export + " due to exception " + e
					+ ". Check log. ");
			System.exit(0);
		}

	}

	/**
	 * This method makes sure the current step is set to null
	 */
	protected void resetCurrentStep() {
		if (currentStep != null) {
			currentStep = null;
		}
	}

	/**
	 * Returns the step created by the last evolution or intervention (last step of
	 * the project)
	 */
	public Step getLastStep() {
		// fc+ld-9.2.2023
		if (currentStep == null)
			return getRoot();
		else
			return currentStep;

	}

	/**
	 * Returns the model
	 */
	public GModel getModel() {
		return model;
	}

	/**
	 * Returns the project
	 */
	public Project getProject() {
		return project;
	}

	/**
	 * Returns the project's root step
	 */
	public Step getRoot() {
		try {
			return (Step) project.getRoot();
		} catch (Exception e) {
			return null;
		}
	}

	/**
	 * Return Capsis data directory
	 */
	public String getDataDir() {
		return PathManager.getDir("data");
	}

	/**
	 * Return Capsis root directory
	 */
	public String getRootDir() {
		return PathManager.getInstallDir();
	}

}
