/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski
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

package capsis.app;

import capsis.commongui.projectmanager.ProjectManager;
import capsis.gui.Pilot;
import capsis.kernel.Engine;
import capsis.kernel.GModel;
import capsis.kernel.InitialParameters;
import capsis.kernel.Project;
import capsis.script.GScript2;

/**
 * C4Script is a default Capsis script. It is mainly a GScript2. The particular
 * point is the static initializer ensuring the good initialization of Capsis by
 * calling the Starter at least once. The constructors chain to the superclass
 * constructors. Can be instanciated: C4Script s = new C4Script
 * ("modelPackageName"). Can also be subclassed.
 * 
 * See the script doc on the Capsis web site documentation pages.
 * 
 * @author S. Dufour-Kowalski - april 2009
 */
public class C4Script extends GScript2 {

	static {
		// Loads the Capsis app Engine, Settings, ModelManager and
		// ExtensionManager
		// Ensures inits are ok even if a script was launched outside the
		// Starter, directly
		// with a main(String[] args) method.
		new capsis.app.Starter(new String[] { "capsis", "-p", "script" });
	}

	private boolean addedToGui = false;

	/**
	 * Default constructor, needed for some scripts subclassing C4Script and
	 * implementing a main method to do simple tasks (M. Fortin). e.g. load a
	 * project and run some exporter
	 */
	public C4Script() {
		super();
	}

	/**
	 * Constructor 1
	 */
	public C4Script(String modelName) {
		super(modelName);
	}

	/**
	 * Constructor 2
	 */
	public C4Script(GModel m) {
		super(m);
	}

	/**
	 * Constructor 3
	 */
	public C4Script(String modelName, InitialParameters param) throws Exception {
		super(modelName, param);
	}

	/**
	 * A way to hide the project in the projectManager
	 */
	public void setTechnicalProject (boolean b) {
		// fc-13.4.2022
		if (project == null) {
			System.out.println("C4Script error, project = null, setTechnicalProject() should be called after init ()");
			return;
		}
		project.setTechnical(b);
	}
	
	/**
	 * A method to reload a saved project in a C4Script. 
	 * 
	 * <pre>
	 * e.g. C4Script s = C4Script.openProject (someProjectFile);
	 * </pre>
	 */
	public static C4Script openProject(String projectFileName) throws Exception {
		// fc-12.5.2015 for Isabelle Lecomte / Hi-sAFe

		Project p = Engine.getInstance().processOpenProject(projectFileName);

		C4Script s = new C4Script();
		s.model = p.getModel();
		s.project = p;
		s.currentStep = p.getRoot();

		return s;
	}

	/**
	 * Update GUI
	 */
	public void updateGUI() throws Exception {

		if (Pilot.getMainFrame() == null) {
			return;
		}

		if (!addedToGui) {
			addedToGui = true;
		}

		ProjectManager projectManager = ProjectManager.getInstance();
		if (projectManager != null) {
			projectManager.update();
		}
	}

}
