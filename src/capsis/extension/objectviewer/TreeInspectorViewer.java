/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
package capsis.extension.objectviewer;

import java.awt.GridLayout;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Tree;

/**
 * An inspector for trees.
 * 
 * @author F. de Coligny - June 2003
 */
public class TreeInspectorViewer extends InspectorViewer {

	static {
		Translator.addBundle ("capsis.extension.objectviewer.TreeInspectorViewer");
	}
	
	// nb-07.08.2018
	//static public final String NAME = Translator.swap ("TreeInspectorViewer");
	//static public final String DESCRIPTION = Translator.swap ("TreeInspectorViewer.description");
	//static public final String AUTHOR = "F. de Coligny";
	//static public final String VERSION = "1.3";

	/**
	 * Default constructor.
	 */
	public TreeInspectorViewer () {
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith (Object referent) {
		// This tool is compatible with tree collections only
		try {
			// fc - 6.12.2007 - referent is now always a Collection
			Collection c = (Collection) referent;
			if (c.isEmpty ()) { return false; }

			// fc - 22.9.2005
			// Find representative objects (ie with different classes)
			// if there is at least one GTree in the collection, ok
			Collection reps = Tools.getRepresentatives (c);
			for (Iterator i = reps.iterator (); i.hasNext ();) {
				if (i.next () instanceof Tree) { return true; }
			}
			return false;
		} catch (Exception e) {
			Log.println (Log.ERROR, "TreeInspectorViewer.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}

	@Override
	public String getName() {
		return Translator.swap("TreeInspectorViewer.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}
	
	@Override
	public String getDescription() {
		return Translator.swap("TreeInspectorViewer.description");
	}
	
	@Override
	public String getVersion() {
		return "1.3";
	}

	/**
	 * User interface definition.
	 */
	protected void createUI (Collection subject) {
		setLayout (new GridLayout (1, 1));
		// ~ updateUI (subject); // fc - 30.11.2007 // removed by fc - 11.9.2008
		// - new OVSelector framework
	}

	protected Collection updateUI (Collection subject) {
		// Take care to remove from the collection the indivs that are not trees
		if (subject == null) {
			subject = new ArrayList ();
		} // fc - 7.12.2007 - ovs should accept null subject
		Collection copy = new ArrayList (subject);
		for (Iterator i = copy.iterator (); i.hasNext ();) {
			Object indiv = i.next ();
			if (!(indiv instanceof Tree)) {
				i.remove ();
			} // trees only
		}

		return super.updateUI (copy);

	}

}
