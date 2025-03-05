/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2001-2003  Francois de Coligny
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

package capsis.extension.objectviewer;

import java.awt.Color;
import java.awt.GridLayout;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.JPanel;

import jeeb.lib.util.Disposable;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Tree;
import capsis.extension.AbstractObjectViewer;
import capsis.util.SideViewDrawer;

/**
 * A side viewer for trees.
 * 
 * @author F. de Coligny - August 2002
 */
public class TreeSideViewer extends AbstractObjectViewer implements Disposable {

	static {
		Translator.addBundle ("capsis.extension.objectviewer.TreeSideViewer");
	}
	
	// nb-07.08.2018
	//static public final String NAME = Translator.swap ("TreeSideViewer");
	//static public final String DESCRIPTION = Translator.swap ("TreeSideViewer.description");
	//static public final String AUTHOR = "F. de Coligny";
	//static public final String VERSION = "1.3";

	private Collection trees;

	// ~ protected SelectionSource source; // fc - 11.12.2007

	/**
	 * Default constructor.
	 */
	public TreeSideViewer () {
	}

	public void init (Collection s) throws Exception {

		try {
			// fc - 11.12.2007 - Selection listeners
			// ~ source = s.getSelectionSource ();
			// ~ source.addSelectionListener (this);
			// fc - 11.12.2007 - Selection listeners

			// fc - 11.9.2008 - removed this line
			// ~ extractTrees (s.getCollection ());

			createUI ();

			// fc - 11.9.2008 - removed this line
			// ~ reset (); // fc - 6.2.2008

			// fc-5.7.2018 added thisIsAReselection
			boolean thisIsAReselection = false;
			show (new ArrayList (s), thisIsAReselection);

		} catch (Exception exc) {
			Log.println (Log.ERROR, "TreeSideViewer.c ()", exc.toString (), exc);
			throw exc; // fc - 4.11.2003 - object viewers may throw exception
		}

	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith (Object referent) {
		try {
			// fc - 6.2.2008 - referent is now always a Collection
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
			Log.println (Log.ERROR, "TreeSideViewer.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}

	@Override
	public String getName() {
		return Translator.swap("TreeSideViewer.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("TreeSideViewer.description");
	}

	@Override
	public String getVersion() {
		return "1.3";
	}

	/**
	 * Disposable
	 */
	public void dispose () {
		// ~ System.out.println ("TreeSideViewer.dispose ()...");
		// ~ try {
		// ~ source.removeSelectionListener (this);
		// ~ } catch (Exception e) {} // does not matter very much
	}

	/**
	 * SelectionListener
	 */
	// ~ public void sourceSelectionChanged (SelectionEvent e) {
	// ~ SelectionSource source = e.getSource ();
	// ~ Collection newSelection = source.getSelection ();
	// ~ boolean selectionActuallyChanged = e.hasSelectionActuallyChanged (); //
	// fc - 13.12.2007
	// ~ System.out.println
	// ("TreeSideViewer, sourceSelectionChanged, selectionActuallyChanged="+selectionActuallyChanged);

	// ~ Collection listenerEffectiveSelection = extractTrees (newSelection);

	// ~ //if (panel2D == null || selectionActuallyChanged) {calculatePanel2D
	// (listenerEffectiveSelection);} // fc - 13.12.2007

	// ~ // Tell the source what we've selected effectively - fc - 6.12.2007
	// ~ e.setListenerEffectiveSelection (listenerEffectiveSelection);

	// ~ reset ();
	// ~ }

	// fc - 6.2.2008
	private Collection extractTrees (Collection objects) {
		trees = new ArrayList ();
		for (Object o : objects) {
			if (o instanceof Tree) {
				trees.add (o);
			}
		}
		return trees;
	}

	// fc - 6.2.2008
	private void reset () {
		removeAll ();
		Color treeColor = Color.DARK_GRAY;
		Color labelColor = Color.BLACK;
		boolean showLabels = true;
		JPanel part1 = new SideViewDrawer (trees, treeColor, labelColor, showLabels);
		add (part1);

		revalidate ();
		repaint ();
	}

	// fc - 9.9.2008 - new OVSelector framework
	public Collection show (Collection candidateSelection, boolean thisIsAReselection) {
		realSelection = extractTrees (candidateSelection);
		reset ();
		System.out.println ("" + getName () + ".select candidateSelection " + candidateSelection.size ()
				+ " realSelection " + realSelection.size ());
		return realSelection;
	}

	/**
	 * User interface definition.
	 */
	private void createUI () {

		// ~ // 1. tree drawing
		// ~ Color treeColor = Color.DARK_GRAY;
		// ~ Color labelColor = Color.BLACK;
		// ~ boolean showLabels = true;
		// ~ JPanel part1 = new SideViewDrawer (trees, treeColor, labelColor,
		// showLabels);

		// Layout parts
		setLayout (new GridLayout (1, 1));
		// ~ add (part1);

		// Do not set sizes explicitly inside object viewers
		// ~ setPreferredSize (new Dimension (300, 400));

		// Now call reset
	}

}
