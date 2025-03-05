/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2003  Francois de Coligny
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

package capsis.extension.grouperdisplay;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.util.Collection;
import java.util.HashSet;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Translator;
import capsis.extensiontype.GrouperDisplay;
import capsis.kernel.GScene;
import capsis.util.group.Group;
import capsis.util.group.Grouper;

/**
 * InspectorDisplay is a display for groupers on spatially explicit stands.
 * 
 * @author F. de Coligny - October 2004
 */
public class InspectorDisplay extends JPanel implements GrouperDisplay {	

	static {
		Translator.addBundle("capsis.extension.grouperdisplay.InspectorDisplay");
	}

	// nb-07.08.2018
	//public static final String VERSION = "1.0";
	//public static final String AUTHOR =  "F. de Coligny";

	/**	Phantom constructor. 
	*	Only to ask for extension properties (authorName, version...).
	*/
	public InspectorDisplay () {}

	/**	Referent is a couple of objects : Object[] typeAndStand = (Objet[])  referent;
	*/
	static public boolean matchWith (Object referent) {
		return true;	// inspector can deal with everything
	}

	@Override
	public String getName() {
		return Translator.swap("InspectorDisplay.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("InspectorDisplay.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	
	/**	Update the display on given stand, according to the given grouper
	*/
	public void update (GScene stand, Grouper grouper){	// not ?
		
		
		Collection selectedIndividuals = new HashSet ();
		if (grouper != null) {
			Collection individuals = Group.whichCollection (stand, grouper.getType ());
			selectedIndividuals = grouper.apply (individuals);
		}
		
		removeAll ();
		JComponent panel = null;
		if (selectedIndividuals == null || selectedIndividuals.isEmpty ()) {
			panel = new JPanel ();
			panel.add (new JLabel (Translator.swap ("Shared.empty")), BorderLayout.NORTH);
		} else {
			panel = AmapTools.createInspectorPanel (selectedIndividuals);
		}
		setLayout (new GridLayout (1, 1));	// needed to mnage correctly layout in FilterManager - fc - 6.10.2004
		add (panel);
		revalidate ();
		repaint ();
	}
	

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}
	
}



