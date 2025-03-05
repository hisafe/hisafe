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

import java.awt.BorderLayout;
import java.text.NumberFormat;
import java.util.Locale;

import javax.swing.JLabel;
import javax.swing.JPanel;

import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;
import capsis.kernel.GScene;
import capsis.kernel.Project;
import capsis.kernel.Step;

/**	A panel do draw a Header + Project within an application based on the capsis kernel.
*	@author F. de Coligny - august 2009
*/
public class ProjectPanel extends JPanel {
	
	protected ProjectManager projMan;
	private Project project;
	// A ProjectPanel is composed of a header and a projectDrawing
	private LinePanel header;
	private ProjectDrawing projectDrawing;

	/**	Constructor.
	*/
	public ProjectPanel (ProjectManager projMan, Project project) {
		super (new BorderLayout ());
		this.projMan = projMan;
		this.project = project;
		
		createUI ();
	}
	
	public Project getProject () {
		return project;
	}
	
	/**	Create the user interface: a header and a drawing with 
	*	StepButtons for the scenario.
	*/
	private void createUI () {
		// If the scenario is currently selected, set a selection border
		if (Current.getInstance ().getProject () != null 
				&& Current.getInstance ().getProject ().equals (project)) {
			setBorder (projMan.getCurrentProjectBorder());
		} else {
			setBorder (null);
		}
		
		ColumnPanel c = new ColumnPanel (0, 0);
		
		// Project title
		StringBuffer b = new StringBuffer ();
		
		// Model name
		b.append (Translator.swap ("ProjectPanel.project"));
		b.append (" ");
		b.append (project.getModel ().getIdCard ().getModelName ());
		
		// Project name
		b.append (" [");
		b.append (project.getName ());
		b.append ("]");
		
		// Plot area
		b.append (" - ");
		b.append (getSurface ());
		
		// Current memorizer
		b.append (" - ");
		b.append (project.getMemorizer ().getCaption ());
		
		// Source data under the root step
		GScene rootScene = ((Step) project.getRoot ()).getScene ();
		// fc-15.12.2020 Added test on length (Salem)
		if (rootScene.getSourceName () != null && rootScene.getSourceName ().length() > 0) {
//		if (rootScene.getSourceName () != null) {
			b.append (" - ");
			b.append (rootScene.getSourceName ());
		}
		
		// Header
		header = new LinePanel ();
		header.add (new JLabel (b.toString ()));
		header.addGlue ();
		c.add (header);
	
		// The project will be drawn under the header
		projectDrawing = new ProjectDrawing (projMan, project);
	
		// We need to set projectDrawing location accurately
		int height = header.getPreferredSize ().height;	
		projectDrawing.setLocation (0, height);
	
		LinePanel l1 = new LinePanel (0, 0);
		l1.add (projectDrawing);
		l1.addGlue ();
		c.add (l1);
		
		c.addStrut0 ();
			
		this.add (c, BorderLayout.NORTH);
		
		int w = Math.max (header.getWidth (), projectDrawing.getWidth ());
		int h = header.getHeight () + projectDrawing.getHeight ();
		setSize (w, h);
		
	}

	/**	Returns the plot surface in a human readable string.
	 */
	private String getSurface () {
		String surface = null;
		
		// get root scene
		Step root = (Step) project.getRoot ();
		if(root == null) { return ""; }
		GScene rootScene = root.getScene ();
		if(rootScene == null) { return ""; }
		
		double area_m2 = rootScene.getArea ();
		
		if (area_m2 <= 0) {
			surface = Translator.swap ("ProjectPanel.unknownSurface");
			
		} else {
			
			String unit = "m2";
			if (area_m2 >= 10000) {
				area_m2 = area_m2 / 10000;
				unit = "ha";
			}
			
			NumberFormat nf = NumberFormat.getInstance (Locale.ENGLISH);
			nf.setMaximumFractionDigits(2);
			nf.setMinimumFractionDigits(0);
			nf.setGroupingUsed(false);
			surface = nf.format(area_m2) + " " + unit;
			
		}
		
		return surface;
	}

	public String toString () {
		StringBuffer b = new StringBuffer ();
		b.append ("ProjectPanel_(");
		b.append (project.toString ());
		b.append (")");
		return b.toString ();
	}

	
}

