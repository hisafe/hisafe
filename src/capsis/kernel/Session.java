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

package capsis.kernel;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import jeeb.lib.util.PathManager;
import jeeb.lib.util.Settings;

/**
 * Simulation session (one per simulation) contains some simulation scenarios.
 * 
 * @author F. de Coligny - may 1999, september 2010
 */
public class Session implements Serializable {

	// nb-27.08.2018
	// private static final long serialVersionUID = 1L;

	/** Session name */
	private String name;
	/** List of projects in the session */
	private List<Project> projects;
	/** Can be closed without saving */
	private boolean saved;
	/** Was already saved under this name, set true on open and save Session */
	private boolean wasSaved;

	/**
	 * Constructor.
	 */
	public Session(String nam) {
		name = nam;
		projects = Collections.synchronizedList(new ArrayList<Project>());
		saved = false;
		wasSaved = false;
	}
	

	/**
	 * Adds a Project in the Session.
	 */
	synchronized public void addProject(Project p) {

		// fc-28.9.2021 See if the project should be renamed for clarity in the session
		p.ensureNameIsUniqueInTheSession ();

		projects.add(p);
	}

	/**
	 * Removes a Project from the Session.
	 */
	synchronized public void removeProject(Project p) {
		projects.remove(p);
	}

	/**
	 * Changes the order of the two projects: moves source before target.
	 */
	public void moveSourceBeforeTarget(Project source, Project target) {
		if (source == null || target == null || source.equals(target)) {
			return;
		}

		Vector<Project> newList = new Vector<Project>();
		for (Iterator<Project> i = projects.iterator(); i.hasNext();) {
			Project s = (Project) i.next();
			if (s.equals(target)) {
				newList.add(source);
			} else if (s.equals(source)) {
				continue;
			}
			newList.add(s);
		}
		projects = newList;
	}

	/**
	 * Changes the order of the two Projects: moves source after target.
	 */
	public void moveSourceAfterTarget(Project source, Project target) {
		if (source == null || target == null || source.equals(target)) {
			return;
		}

		Vector<Project> newList = new Vector<Project>();
		for (Iterator<Project> i = projects.iterator(); i.hasNext();) {
			Project s = (Project) i.next();
			if (!s.equals(source)) {
				newList.add(s);
			}
			if (s.equals(target)) {
				newList.add(source);
			}
		}
		projects = newList;
	}

	/**
	 * Returns true if the Session contains no Projects.
	 */
	public boolean isEmpty() {
		return (projects == null || projects.isEmpty());
	}

	/**
	 * Session name.
	 */
	public String getName() {
		return name;
	}

	public void setName(String n) {
		name = n;
	}

	/**
	 * Complete file name is capsis.session.path + file separator + name (e.g.
	 * /home/coligny/capsis4/sessions/session1).
	 */
	public String getFileName() {
		
		// fc-4.10.2021 Check that capsis.session.path exists
		// Else use capsis4/tmp/
		// (bug: '/Sans titre')
		
		return Settings.getProperty("capsis.session.path", PathManager.getDir("tmp")) + File.separator + name;
		
//		return Settings.getProperty("capsis.session.path", "") + File.separator + name;
	}

	/**
	 * MP 220916 (granted by fc): Add synchronized key for Simreg threading Prevent
	 * java.util.ConcurrentModificationException bug found when executing
	 * gymnos.myscripts.SIMREG.java main function SIMREG.java creates multiple
	 * threads with each own instance of C4Script in a Executors.newFixedThreadPool
	 * java.util.ConcurrentModificationException bug occurs when threads will init
	 * C4Script at the same time. This bug occurs very rarely because it needs that
	 * two or more threads initialize their C4Script at the same time. Example cmd:
	 * capsis.bat -p script gymnos.myscripts.SIMREG
	 * E:\workspace\capsis4\data\gymnos\SIMREG_test_small.csv
	 */
	synchronized public List<Project> getProjects() {
		return projects;
	}
	/**
	 * Returns the list of Projects.
	 */
	// public List<Project> getProjects () {return projects;}

	/**
	 * Returns the project count.
	 */
	public int getProjectCount() {
		if (projects != null) {
			return projects.size();
		} else {
			return 0;
		}
	}

	public boolean isSaved() {
		return saved;
	}

	public void setSaved(boolean b) {
		saved = b;
	}

	public boolean wasSaved() {
		return wasSaved;
	}

	public void setWasSaved(boolean b) {
		wasSaved = b;
	}

	/**
	 * String representation of the Session.
	 */
	public String toString() {
		return "Session_" + name;
	}

//	public String bigString () {
//		StringBuffer sb = new StringBuffer (toString ());
//		sb.append (" saved=");
//		sb.append (saved);
//		sb.append (" wasSaved=");
//		sb.append (wasSaved);
//		sb.append (" Scenarios {");
//		if (projects != null) {
//			for (Iterator<Project> ite = projects.iterator (); ite.hasNext ();) {
//				Project s = (Project) ite.next ();
//				sb.append (s.toString ());
//				if (ite.hasNext ()) {sb.append (", ");}
//			}
//		}
//		sb.append ("}");
//		return sb.toString ();
//	}

}