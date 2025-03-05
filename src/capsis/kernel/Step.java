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

import jeeb.lib.util.Node;

/**	A Step carries a scene (GScene) at a given time in the simulation.
 * 	All Steps are part of a Project.
 *	Steps are nodes in a n-tree (General tree data structure). 
 *	A given step may have several sons if different scenarios have been studied 
 *	from its given state.
 *
 *	@author F. de Coligny - june 1999, september 2010
 *	@see Node and NTree for more details.
 */
public class Step extends Node {

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;

	/**	The Step is part of this Project */
	private Project project;
	/**	The scene this Step carries */
	private GScene scene;
	/**	A unique id in the Project */
	private int id;
	/**	A free String telling why this Step was built e.g. "evolution till 20 years" */
	public String reason = "";
	/**	Internal management, related to Memorizers, means 'kept in memory' */
	private boolean tied;


	
	/**	Constructor.
	 */
	public Step (Project project, GScene scene, int id) {
		super ();
		
		this.project = project;
		this.scene = scene;
		scene.setStep (this);
		this.id = id;
	}

	
	/**	For memory management by the Memorizers.
	 */
	public void setTied (boolean t) {tied = t;}
	public boolean isTied () {return tied;}
	
	/**	Disposes the step.
	 */
	public void dispose () {
		super.dispose ();
		project = null;
		scene = null;
	}

	
// Main properties 
	
	public GScene getScene () {return scene;}
	public void setScene (GScene stand) {this.scene = stand;}
	
	public Project getProject () {return project;}
	public void setProject (Project project) {this.project = project;}

	public int getId () {return id;}

	public String getReason () {return reason;}
	public void setReason (String reason) {this.reason = reason;}

	
// Name, Caption and other String representations
	
	/**	Step name is of type "*17a" (* only if scene isInterventionResult (), 
	 * 	17 = scene date, a = scenario name).
	 */
	public String getName () {
		
		char c = (char)(getWidth ()%26 + 'a');
				
		StringBuffer b = new StringBuffer (scene != null ? scene.getCaption () : "scene=null");
		b.append (c);
		
		return b.toString ();
	}
	
	/**	Step caption is of type "pnn.*17a" (pnn = project name, then getName ()).
	 */
	public String getCaption () {
		
		StringBuffer b = new StringBuffer (project != null ? project.getName () : "project=null");
		
		b.append (".");
		b.append (getName ());
		
		return b.toString ();
		
	}
	
	/**	toString () method.
	 */
	public String toString () {
		
		return "Step_"+getCaption ();
		
//		String c = "";
//		try {
//			c = getCaption ();
//		} catch (Exception e) {
//			c = "null";
//		}
//		return "Step_"+c;
	}

	
	
//	public String bigString () {
//		StringBuffer sb = new StringBuffer (toString ());
//		sb.append (" visible=");
//		sb.append (visible);
//		sb.append (" project=");
//		if (project != null) {
//			sb.append (project.toString ());
//		} else {
//			sb.append (" *** NO SCENARIO");
//		}
//		sb.append (" stand=");
//		if (scene != null) {
//			sb.append (scene.toString ());
//		} else {
//			sb.append (" *** NO STAND");
//		}
//		sb.append (" reason=");
//		sb.append (reason);
//		
//		return sb.toString ();
//	}



}
