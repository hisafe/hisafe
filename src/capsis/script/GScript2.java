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

import capsis.commongui.projectmanager.Current;
import capsis.extension.memorizer.DefaultMemorizer;
import capsis.kernel.GModel;
import capsis.kernel.InitialParameters;
import capsis.kernel.extensiontype.Memorizer;
import jeeb.lib.util.Log;

/**
 * GScript2 is an abstract superclass for scripts in Java. This class must be
 * subclassed, see capsis.app.C4Script. See the script doc on the Capsis web
 * site documentation pages.
 * 
 * @author F. de Coligny - September 2010
 */
public abstract class GScript2 extends GScript {

	/**
	 * Default constructor, needed for specific scripts subclassing GScript2 and
	 * implementing a main method to do simple tasks (M. Fortin). E.g. load a
	 * project and run some exporter.
	 */
	public GScript2() {
	}

	/**
	 * Constructor 1
	 */
	public GScript2(String modelName) {
		model = loadModel(modelName);
	}

	/**
	 * Constructor 2
	 */
	public GScript2(GModel m) {
		model = m;
	}

	/**
	 * Constructor 3
	 */
	public GScript2(String modelName, InitialParameters ip) throws Exception {
		model = loadModel(modelName);
		init(ip);
	}

	/**
	 * Init
	 */
	public void init(InitialParameters ip) throws Exception {
		init(ip, new DefaultMemorizer());
	}

	/**
	 * Init
	 */
	public void init(InitialParameters ip, Memorizer memorizer) throws Exception {

		// fc-20.2.2024 Added this methof for backward compatibility (usually,
		// technicalProject is false)

		boolean technicalProject = false;
		init(ip, memorizer, technicalProject);
	}

	/**
	 * Init
	 */
	public void init(InitialParameters ip, Memorizer memorizer, boolean technicalProject) throws Exception {

		// fc-20.2.2024 Added technicalProject to set it as early as possible and before
		// the projectManager update

		resetCurrentStep();

		ip.buildInitScene(model);

		String projectName = this.getClass().getName();
		project = createProject(projectName, model, ip);

		// fc-20.2.2024 Technical projects do not appear in the projectManager
		// (SamsaraLight calculation...)
		if (technicalProject)
			project.setTechnical(true);

		// fc+al-18.4.2023 Needed for getInterventionBase ()and GenotypableImpl
		Current.getInstance().setStep(project.getRoot());

		try {
			setMemorizer(project, memorizer);

		} catch (Exception e) {
			Log.println(Log.ERROR, "GScript2.init ()", "Memorizer error", e);
			throw new Exception("Memorizer error", e);
		}

	}

	/**
	 * Unused in GScript2. Legacy scripts used to extend directly GScript and were
	 * built with a constructor and a run () method. More recent scripts extend a
	 * subclass of GScript2 and do not use the run () method any more (see
	 * capsis.app.C4Script).
	 */
	public void run() throws Exception {
	}

}
