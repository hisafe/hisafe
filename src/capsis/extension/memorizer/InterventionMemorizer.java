/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2020 INRA 
 * 
 * Authors: F. de Coligny, N. Beudez, S. Dufour-Kowalski. 
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
 * along with Capsis. If not, see <http://www.gnu.org/licenses/>.
 */
package capsis.extension.memorizer;

import capsis.defaulttype.SimpleScene;
import capsis.kernel.GModel;
import capsis.kernel.Project;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.Memorizer;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * A Project Memorizer memorizing intervention result steps and their preceding
 * step.
 * 
 * @author F. Andr� - February 2023
 */
public class InterventionMemorizer implements Memorizer {

	static {
		Translator.addBundle("capsis.extension.memorizer.InterventionMemorizer");
	}

	/**
	 * Default constructor.
	 */
	public InterventionMemorizer() {
	}

	/**
	 * Extension dynamic compatibility mechanism. This method checks if the
	 * extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			return referent instanceof GModel;

		} catch (Exception e) {
			Log.println(Log.ERROR, "InterventionMemorizer.matchWith ()",
					"Exception in matchWith () method, returned false", e);
			return false;
		}
	}

	@Override
	public String getName() {
		return Translator.swap("InterventionMemorizer.name");
	}

	@Override
	public String getAuthor() {
		return "F. Andre";
	}

	@Override
	public String getDescription() {
		return Translator.swap("InterventionMemorizer.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	/**
	 * Memorizer interface.
	 */
	@Override
	public void reinit() {
		// nothing here
	}

	/**
	 * Memorizer interface. Memorization strategy: memorizes only steps that are
	 * intervention results, and their farther steps.
	 */
	@Override
	public void memorize(Project project, Step fatherStep, Step newStep) {

		// fc-23.2.2023 When newStep is root, fatherStep is null (happens when the
		// InterventionMemorizer was used last time and is restored at project creation
		// time)
		SimpleScene fatherScene = null;
		if (fatherStep != null)
			fatherScene = (SimpleScene) fatherStep.getScene();

		SimpleScene newScene = (SimpleScene) newStep.getScene();

		if (fatherStep == null || fatherStep.isRoot() || fatherScene.isInterventionResult()) {
			project.tieStep(fatherStep, newStep);

		} else if (newScene.isInterventionResult()) {
//			project.tieStep(fatherStep, prevStep); // include also the step preceding the intervention
			project.tieStep(fatherStep, newStep);

		} else {
			project.replaceStep(fatherStep, newStep);
		}
	}

	/**
	 * Memorizer interface.
	 */
	@Override
	public String getCaption() {
		return getName();
	}

	@Override
	public String toString() {
		// fc-23.2.2023 This is used when a memorizer is set to remember the
		// last.memorizernext time a project is created with the same model. See
		// project.setMemorizer ()
		return "InterventionMemorizer";
//		return getCaption(); // Nope
	}

}
