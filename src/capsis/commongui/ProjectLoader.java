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

package capsis.commongui;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import jeeb.lib.util.Alert;
import jeeb.lib.util.Command;
import jeeb.lib.util.Import;
import jeeb.lib.util.Log;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;
import jeeb.lib.util.Translator;
import capsis.commongui.command.OpenProject;
import capsis.commongui.projectmanager.Current;
import capsis.kernel.Engine;
import capsis.kernel.Project;
import capsis.kernel.Session;
import capsis.kernel.Step;

/**
 * A loader to load projects at the app opening time
 * 
 * @author F. de Coligny - December 2007, September 2010
 */
public class ProjectLoader extends RecordSet implements Command {

	// nb-27.08.2018
	// private static final long serialVersionUID = 1L;

	static {
		// fc-4.10.2021 Reviewed the translations
		Translator.addBundle("capsis.commongui.Labels");
	}

	/**
	 * If a macro file is used, it contains ProjectRecords. Each ProjectRecord
	 * contains the file name of a Project to be loaded at startup time.
	 */
	@Import
	static public class ProjectRecord extends Record {
		public ProjectRecord() {
			super();
		}

		public ProjectRecord(String line) throws Exception {
			super(line);
		}

		// public String getSeparator () {return ";";} // to change default "\t"
		// separator
		public String fileName;
	}

	/** Singleton pattern */
	static private ProjectLoader instance;

	/** Singleton pattern */
	static public ProjectLoader getInstance() {
		if (instance == null) {
			instance = new ProjectLoader();
		}
		return instance;
	}

	/** The file names of the projects to be loaded in execute () */
	private Collection<String> projectFileNames;
	/** The macro file name in case loadMacroFile () was called */
	private String macroFileName;

	/**
	 * Constructor. Use addProjectFileName () OR loadMacroFile () after to add the
	 * file names of the projects to be loaded. Then call execute () to load the
	 * projects.
	 */
	private ProjectLoader() {
		super();
		projectFileNames = new ArrayList<String>();
	}

	/**
	 * Loads a macro file containing project file names. Then, execute () must be
	 * called to load the projects.
	 */
	public void loadMacroFile(String macroFileName) {
		this.macroFileName = macroFileName;
		try {
			prepareImport(macroFileName);
			interpret();
		} catch (Exception e) {
			// fc-4.10.2021 If Translator possibly not available at this time, do not
			// translate
			String message = "Errors during projects loading" 
					+ "\nCould not load projects file: " + macroFileName;
//			String message = "Errors during projects loading" // Translator not instanciated yet :> english
//					+ "\n" + Translator.swap("Could not load projects file") + ": " + macroFileName;
			Alert.print(message, e);
		}
	}

	/**
	 * Adds a project file name to be loaded.
	 */
	public void addProjectFileName(String projectFileName) {
		projectFileNames.add(projectFileName);
	}

	/**
	 * To check from outside if we have projects to load.
	 */
	public boolean isEmpty() {
		return projectFileNames.isEmpty();
	}

	/**
	 * Loads the file names in the projectFileNames list.
	 */
	public int execute() {
		// This report will be shown at the end if trouble only
		StringBuffer troubleReport = new StringBuffer();

		// Create an untitled Session
		// fc-4.10.2021 Reviewed translations
		Session session = Engine.getInstance().processNewSession(Translator.swap("ProjectLoader.untitledSession"));
//		Session session = Engine.getInstance ().processNewSession (Translator.swap ("untitled"));

		// Open the projects
		for (String fileName : projectFileNames) {
			try {

				OpenProject.loadProject(this, fileName);

			} catch (Exception e) {
				Log.println(Log.ERROR, "ProjectLoader.execute ()", "Could not load the project file: " + fileName, e);
				troubleReport.append(fileName);
				troubleReport.append("\n");
			}
		}

		// Set Current Step
		try {
			Project project1 = session.getProjects().get(0);
//			ProjectManager.getInstance ().processCreate ((Step) project1.getRoot ());
			Current.getInstance().setStep((Step) project1.getRoot());
		} catch (Exception e) {
		}

		// Show report if trouble
		if (troubleReport.length() != 0) {
			String message = Translator.swap("ProjectLoader.ErrorsDuringProjectsLoading") + "\n";
			troubleReport.insert(0, (Translator.swap("ProjectLoader.couldNotLoad") + ": \n"));
			Alert.print(message, new Exception(troubleReport.toString()));
		}

		return 0;
	}

	/**
	 * If loadMacroFile () was used, interprets the macro file to extract the
	 * project file names and store them in the projectFileNames field.
	 */
	private void interpret() throws Exception {
		for (Iterator i = this.iterator(); i.hasNext();) {
			Object record = i.next();
			if (record instanceof ProjectRecord) {
				ProjectRecord r = (ProjectRecord) record;
				projectFileNames.add(r.fileName);
			} else {
				throw new Exception("Wrong format in " + macroFileName + " near record " + record);
			}
		}
	}

}
