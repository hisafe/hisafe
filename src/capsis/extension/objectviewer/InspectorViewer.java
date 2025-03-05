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

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Disposable;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.extension.AbstractObjectViewer;

/**
 * An inspector for objects.
 * 
 * @author F. de Coligny - August 2002
 */
public class InspectorViewer extends AbstractObjectViewer implements Disposable {

	static {
		Translator.addBundle ("capsis.extension.objectviewer.InspectorViewer");
	}
	
	// nb-07.08.2018
	//static public final String NAME = Translator.swap ("InspectorViewer");
	//static public final String DESCRIPTION = Translator.swap ("InspectorViewer.description");
	//static public final String AUTHOR = "S. Chalon, F. de Coligny";
	//static public final String VERSION = "1.2";

	/**
	 * Default constructor.
	 */
	public InspectorViewer () {
	}

	public void init (Collection s) throws Exception {

		try {
			createUI ();
		
			// fc-5.7.2018 added thisIsAReselection
			boolean thisIsAReselection = false;
			show (s, thisIsAReselection);

		} catch (Exception exc) {
			Log.println (Log.ERROR, "InspectorViewer.c ()", exc.toString (), exc);
			throw exc; // fc - 4.11.2003 - object viewers may throw exception
		}

	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith (Object referent) {

		// GCells and GTrees (and Collections of them) are viewed by specific
		// subclasses
		// Every other object can be viewed by InspectorViewer
		//
		try {
			// fc - 6.12.2007 - referent is now always a Collection
			Collection c = (Collection) referent;
			if (c.isEmpty ()) { return false; }

			return true;

			// ~ Collection reps = Tools.getRepresentatives (c); // one instance
			// of each class
			// ~ for (Iterator i = reps.iterator (); i.hasNext ();) {
			// ~ Object e = i.next ();
			// ~ if (e instanceof GTree) {return false;} // ->
			// TreeInspectorViewer
			// ~ if (e instanceof GCell) {return false;} // ->
			// CellInspectorViewer
			// ~ }
		} catch (Exception e) {
			Log.println (Log.ERROR, "InspectorViewer.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		// ~ return true;
	}

	@Override
	public String getName() {
		return Translator.swap ("InspectorViewer.name");
	}

	@Override
	public String getAuthor() {
		return "S. Chalon, F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap ("InspectorViewer.description");
	}

	@Override
	public String getVersion() {
		return "1.2";
	}

	/**
	 * Disposable.
	 */
	public void dispose () {

	}

	// fc - 9.9.2008 - new OVSelector framework
	public Collection show (Collection candidateSelection, boolean thisIsAReselection) {
		
		realSelection = updateUI (candidateSelection);
		//System.out.println ("" + NAME + ".select candidateSelection " + candidateSelection.size () + " realSelection "
		//		+ realSelection.size ());
		System.out.println ("" + getName() + ".select candidateSelection " + candidateSelection.size () + " realSelection "
				+ realSelection.size ());
		return realSelection;
	}

	/**
	 * User interface definition.
	 */
	// ~ protected void createUI (Collection subject) {
	protected void createUI () { // fc - 11.9.2008

		// Do not set sizes explicitly inside object viewers
		// ~ setPreferredSize (new Dimension (250, 250));

		setLayout (new GridLayout (1, 1));
		// ~ updateUI (subject); // fc - 30.11.2007
	}

	/**
	 * Shows subject, returns what it effectively showed, here: everything.
	 */
	protected Collection updateUI (Collection subject) {
		removeAll ();
		if (subject == null) {
			subject = new ArrayList ();
		} // fc - 7.12.2007 - ovs should accept null subject
		add (AmapTools.createInspectorPanel (subject));
		revalidate ();
		repaint ();

		return subject; // showed everything
	}

}
