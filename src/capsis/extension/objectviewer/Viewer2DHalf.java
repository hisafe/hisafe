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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Disposable;
import jeeb.lib.util.Log;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex2d;
import jeeb.lib.util.Vertex3d;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Tree;
import capsis.defaulttype.plotofcells.Cell;
import capsis.defaulttype.plotofcells.RectangularPlot;
import capsis.defaulttype.plotofcells.SquareCell;
import capsis.defaulttype.plotofcells.SquareCellHolder;
import capsis.extension.AbstractObjectViewer;
import capsis.extension.PaleoLollypop;
import capsis.extension.lollypop.GenericLollypop;
import capsis.extension.lollypop.GenericLollypopStarter;
import capsis.kernel.Plot;
import capsis.util.ConfigPanel;
import capsis.util.Drawer;
import capsis.util.Location;
import capsis.util.LocationComparator;
import capsis.util.Panel2D;
import capsis.util.SpatializedObject;

/**
 * A 2.5D viewer for trees (almost 3D). Only points at the base of the trees are
 * placed in 3D. From this point, a lollypop is drawn vertically. The Lollypop
 * should be symetrical arround its vertical axis. [fc - march 2003] added a
 * method to calculate z coordinates for each cell vertex considering the tree
 * locations.
 * 
 * @author F. de Coligny - June 2005
 */
public class Viewer2DHalf extends AbstractObjectViewer
// ~ implements Drawer, ChangeListener, ActionListener, SelectionListener,
// Disposable {
		implements Drawer, ChangeListener, ActionListener, Disposable {

	static {
		Translator.addBundle ("capsis.extension.objectviewer.Viewer2DHalf");
	}
	
	// nb-07.08.2018
	//static public final String NAME = Translator.swap ("Viewer2DHalf");
	//static public final String DESCRIPTION = Translator.swap ("Viewer2DHalf.description");
	//static public final String AUTHOR = "Ph. Borianne, F. de Coligny";
	//static public final String VERSION = "1.0";

	private Collection trees;
	private Collection selectedSubjects;
	private Collection cells; // optionnal (cells may not exist for some
								// modules)
	private Collection drawnCells; // optionnal (cells may not exist for some
									// modules)

	private JScrollPane scroll; // fc - 7.12.2007
	private Panel2D panel2D;

	private Rectangle.Double groundRectangle;
	private JSlider slider;
	private double zAngle0;
	private double zAngle;

	private Map memo; // memo what was drawn: bounding /*box -> tree*/ box ->
						// Spatialized Object -fc - 19.6.2006)

	private PaleoLollypop lollypop; // Temporary, extension will be chosen by
									// user

	private Map cellElevations; // SquareCell -> CellElevation

	// ~ private SelectionSource source; // fc - 23.11.2007

	// For a Square cell
	// When only one level of cells
	// Elevation (z coordinates for the 4 vertices)
	// z0 is bottom left vertex
	// other zi are in clockwise order
	private class CellElevation {
		public double zCenter;
		public double z0;
		public double z1;
		public double z2;
		public double z3;
	}

	/**
	 * Default constructor.
	 */
	public Viewer2DHalf () {
	}

	public void init (Collection s) throws Exception {

		try {

			// Temporary, extension will be chosen by user (LollypopChooser...)
			// this.actionPerformed (...) will be called when Drawer config
			// changes
			lollypop = new GenericLollypop (new GenericLollypopStarter (this));

			memo = new HashMap (); // fc - 27.3.2006

			// nothing selected at the beginning
			selectedSubjects = new ArrayList ();

			// scene elevation
			zAngle0 = Math.PI / 8;

			createUI ();

			// fc-5.7.2018 added thisIsAReselection
			boolean thisIsAReselection = false;
			show (new ArrayList (s), thisIsAReselection);

		} catch (Exception exc) {
			Log.println (Log.ERROR, "Viewer2DHalf.c ()", exc.toString (), exc);
			throw exc; // fc - 4.11.2003 - object viewers may throw exception
		}

	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith (Object referent) {
		try {
			// fc - 6.12.2007 - referent is now always a Collection
			Collection c = (Collection) referent;
			if (c.isEmpty ()) { return false; }

			// Possibly several subclasses in the collection
			// if there is at least one spatialized tree in the collection, ok
			// fc - 23.11.2007 - there may be also Cells in the collection
			Collection reps = Tools.getRepresentatives (c); // one instance of
															// each class
			for (Iterator i = reps.iterator (); i.hasNext ();) {
				Object e = i.next ();

				if (e instanceof Cell) { return true; } // fc - 23.11.2007
				if (e instanceof Spatialized && e instanceof Tree) { return true; }
				if (e instanceof SpatializedObject) { // fc - 19.6.2006 -
														// Spatialized Gtres are
														// ok
					SpatializedObject so = (SpatializedObject) e;
					if (so.getObject () instanceof Tree) { return true; }
				}

			}
			return false;

		} catch (Exception e) {
			Log.println (Log.ERROR, "Viewer2DHalf.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}

	@Override
	public String getName() {
		return Translator.swap("Viewer2DHalf.name");
	}

	@Override
	public String getAuthor() {
		return "Ph. Borianne, F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("Viewer2DHalf.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}
	
	/**
	 * Modify the point of view
	 */
	public void stateChanged (ChangeEvent e) {
		// ~ if (slider.getValueIsAdjusting ()) {return;}
		double v = slider.getValue ();

		Settings.setProperty ("Viewer2DHalf.slider.position", "" + (int) v);

		zAngle = v / 100 * Math.PI; // [-PI, PI]
		resetPanel2D ();
	}

	private void resetPanel2D () {
		if (panel2D != null) {
			panel2D.reset ();
			panel2D.repaint ();
		}
	}

	/**
	 * Used for the settings and filtering buttons.
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (lollypop)) {
			resetPanel2D ();
		}
	}

	/**
	 * Disposable
	 */
	public void dispose () {
		System.out.println ("Viewer2DHalf.dispose ()...");
		// ~ try {
		// ~ source.removeSelectionListener (this);
		// ~ } catch (Exception e) {} // does not matter very much
	}

	// fc - 9.9.2008 - new OVSelector framework
	public Collection show (Collection candidateSelection, boolean thisIsAReselection) {
		realSelection = extractTreesAndCells (candidateSelection);
		calculatePanel2D (new ArrayList (realSelection));
		resetPanel2D ();
		System.out.println ("" + getName () + ".select candidateSelection " + candidateSelection.size ()
				+ " realSelection " + realSelection.size ());
		return realSelection;
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
	// ("Viewer2DHalf, sourceSelectionChanged, selectionActuallyChanged="+selectionActuallyChanged);

	// ~ Collection listenerEffectiveSelection = extractTreesAndCells
	// (newSelection);

	// ~ if (panel2D == null || selectionActuallyChanged) {calculatePanel2D
	// (listenerEffectiveSelection);} // fc - 13.12.2007

	// ~ // Tell the source what we've selected effectively - fc - 6.12.2007
	// ~ e.setListenerEffectiveSelection (listenerEffectiveSelection);

	// ~ resetPanel2D ();
	// ~ }

	// Prepare tree and cell lists, return the effective selection
	//
	private Collection extractTreesAndCells (Collection selection) { // fc -
																		// 23.11.2007
		trees = new ArrayList ();
		cells = new ArrayList ();

		if (selection == null) { return new ArrayList (); }

		cellElevations = null; // fc - 13.3.2006

		for (Iterator i = selection.iterator (); i.hasNext ();) {
			Object o = i.next ();
			if (o instanceof Tree) {
				trees.add (o);
			}
			if (o instanceof Cell) {
				cells.add (o);
			}
		}

		// Cell elevation calculation in RectangularPlots (see Mountain...)
		try {
			// activate cell elevation ? - fc - 16.3.2006
			Plot plot = null;
			if (cells != null && !cells.isEmpty ()) {
				Cell firstCell = (Cell) cells.iterator ().next ();
				plot = firstCell.getPlot ();
				if (plot instanceof RectangularPlot) { // RectangularPlot
					try {
						calculateCellElevations ((SquareCellHolder) plot, cells);
					} catch (Exception e) {
					}
				}
			}
		} catch (Exception e) {
		} // no cells available

		// ~ try {
		// ~ panel2D = calculatePanel2D (trees);
		// ~ scroll.getViewport ().setView (panel2D);
		// ~ } catch (Exception e) {} // fc - 7.12.2007 - we may be constructed
		// with a null collection to be shown

		// Manage effective selection to inform source
		Collection effectiveSelection = new HashSet (trees); // ArrayList ->
																// HashSet - fc
																// - 12.12.2007
																// - faster on
																// contains
		effectiveSelection.addAll (cells);
		return effectiveSelection;
	}

	/**
	 * User interface definition.
	 */
	private void createUI () {

		// Layout parts
		setLayout (new BorderLayout ());

		// This scrollpane will contain the panel2D
		scroll = new JScrollPane (); // panel2D inside later - fc - 7.12.2007
		add (scroll, BorderLayout.CENTER); // fc - 7.12.2007

		slider = new JSlider (-100, 100, 0);
		slider.addChangeListener (this);
		try {
			int v = new Integer (Settings.getProperty ("Viewer2DHalf.slider.position", "")).intValue ();
			slider.setValue (v);
		} catch (Exception e) {
		}

		add (slider, BorderLayout.SOUTH);

		// Lollypop config panel
		ConfigPanel configPanel = lollypop.getConfigPanel ();
		JPanel aux = new JPanel (new BorderLayout ());
		aux.add (configPanel, BorderLayout.NORTH);
		add (aux, BorderLayout.EAST);

		// Do not set sizes explicitly inside object viewers
		// ~ this.setPreferredSize (new Dimension (450, 330));

	}

	/**
	 * We have to redraw the subscene
	 */
	public void draw (Graphics g, Rectangle.Double r) {
		Graphics2D g2 = (Graphics2D) g;

		// ~ System.out.println ("Viewer2DHalf.draw ... trees="
		// ~ +(trees!=null?trees.size ():0)
		// ~ +" cells="+(cells!=null?cells.size ():0));

		/*
		 * System.out.println ("v2.draw (), rectangle:"); System.out.println
		 * ("  x="+r.x); System.out.println ("  y="+r.y); System.out.println
		 * ("  w="+r.width); System.out.println ("  h="+r.height);
		 */

		// 1. calculate the coordinates for the center of the scene
		//
		double minX = Double.MAX_VALUE;
		double maxX = Double.MIN_VALUE;
		double minY = Double.MAX_VALUE;
		double maxY = Double.MIN_VALUE;
		double minZ = Double.MAX_VALUE;
		double maxZ = Double.MIN_VALUE;
		double cumZ = 0; // fc - 26.3.2006

		// inside trees :
		// Spatialized GTree
		// or SpatializedObject with getObject () is a GTree
		// Note: SpatializedObject implements Spatialized
		// OR GCell - fc - 23.11.2007
		for (Iterator i = trees.iterator (); i.hasNext ();) {
			Spatialized s = (Spatialized) i.next ();

			java.awt.geom.Point2D.Double p = new java.awt.geom.Point2D.Double (s.getX (), s.getY ());
			if (!r.contains (p)) {
				continue;
			}

			minX = Math.min (minX, s.getX ());
			maxX = Math.max (maxX, s.getX ());
			minY = Math.min (minY, s.getY ());
			maxY = Math.max (maxY, s.getY ());
			minZ = Math.min (minZ, s.getZ ());
			maxZ = Math.max (maxZ, s.getZ ());

			// ~ cumZ += s.getZ ();
		}

		// ~ for (Iterator i = cells.iterator (); i.hasNext ();) {
		// ~ GCell c = (GCell) i.next ();

		// ~ //java.awt.geom.Point2D.Double p = new java.awt.geom.Point2D.Double
		// (s.getX (), s.getY ());
		// ~ //if (!c.getShape ().intersects (r)) {continue;}
		// ~ Rectangle2D bbox = c.getShape ().getBounds2D ();

		// ~ minX = Math.min (minX, bbox.getMinX ());
		// ~ maxX = Math.max (maxX, bbox.getMaxX ());
		// ~ minY = Math.min (minY, bbox.getMinY ());
		// ~ maxY = Math.max (maxY, bbox.getMaxY ());
		// ~ //minZ = Math.min (minZ, 0);
		// ~ //maxZ = Math.max (maxZ, 0);
		// ~ //cumZ += s.getZ ();
		// ~ }

		Rectangle.Double b = panel2D.getUserBounds ();

		// fc - 17.5.2006 - trying to place x more accurately
		// ~ double Cx = (maxX+minX)/2;
		// ~ double Cx = b.x + b.width/2;
		double Cx = r.x + r.width / 2;

		// double Cy = b.y + b.height/2;

		double Cy = (maxY + minY) / 2;

		double Cz = (maxZ + minZ) / 2;
		/*
		 * double Cz = 0; if (trees.size () != 0) { Cz = cumZ / trees.size (); }
		 */

		// double Cx = 0;
		// double Cy = 0;
		// double Cz = 0;

		// drawSelectionMark (g2, panel2D, Cx, Cy);

		// 2. build the rotation matrices
		//
		double[][] Mx = makeMx (Math.PI / 8);
		double[][] Mz = makeMz (zAngle0 + zAngle);
		// ~ double[][] rotations = matrix33Product (Mz, Mx);
		double[][] rotations = matrix33Product (Mx, Mz);

		// drawRectangle (g2, panel2D, groundRectangle, Cx, Cy, Cz, rotations);

		drawReferencialFrame (g2, panel2D, Cx, Cy, Cz, rotations);

		// 3. for each cell : (translate to center then apply rotations then)
		// draw
		//
		if (cells != null) {
			g2.setColor (Color.GRAY);
			drawnCells = new ArrayList ();
			for (Iterator i = cells.iterator (); i.hasNext ();) {
				Cell c = (Cell) i.next ();
				try {
					drawCell (g, groundRectangle, c, Cx, Cy, Cz, rotations);
				} catch (Exception e) {
					System.out.println ("Viewer2DHalf.drawCell: exception " + e);
				}
			}
		}

		// 4. create locations for all tree (will then be sorted)
		//
		ArrayList locations = new ArrayList ();
		for (Iterator i = trees.iterator (); i.hasNext ();) {
			Spatialized s = (Spatialized) i.next ();

			Vertex2d v2 = rotate (s.getX (), s.getY (), s.getZ (), Cx, Cy, Cz, rotations);

			// Location contains rotated x and y and the tree to be drawn there
			// : GTree or SpatializedObject
			Location l = new Location (v2.x, v2.y, s);
			locations.add (l);
		}

		// 5. Sort from the furthest to the nearest for good display
		Collections.sort (locations, new LocationComparator (LocationComparator.Y_AXIS, false));

		// 6. for each tree : (translate to center then apply rotations then)
		// draw
		//
		int ind = 0;
		memo.clear ();

		for (Iterator i = locations.iterator (); i.hasNext ();) {
			Location l = (Location) i.next ();
			Spatialized t = (Spatialized) l.subject;
			drawTree (g, r, (Spatialized) l.subject, l.x, l.y);
		}

		// 7. draw information on the panel2D
		StringBuffer label = new StringBuffer ();
		label.append (Translator.swap ("Viewer2DHalf.numberOfTrees"));
		label.append (": ");
		label.append (locations.size ());
		if (drawnCells != null && !drawnCells.isEmpty ()) {
			label.append (" ");
			label.append (Translator.swap ("Viewer2DHalf.numberOfCells"));
			label.append (": ");
			label.append (drawnCells.size ());
		}
		// java.awt.geom.Point2D.Double o = panel2D.getUserPoint (new Point (0,
		// y));
		// ~ FontMetrics fm = g.getFontMetrics ();
		int fontHeight = 10;
		java.awt.geom.Point2D.Double up = panel2D.getUserPoint (new Point (0, fontHeight)); // Rectangle2D.Double
		float x = (float) up.x;
		float y = (float) up.y;
		g2.setColor (Color.BLACK);
		g2.drawString (label.toString (), x, y);

	}

	// Draw a cell at the given coordinates
	//
	private void drawCell (java.awt.Graphics g, Rectangle.Double r, Cell c, double Cx, double Cy, double Cz,
			double[][] rotations) {
		Graphics2D g2 = (Graphics2D) g;

		if (c.getShape ().intersects (r)) {

			CellElevation ce = null;
			Collection correctedZs = new ArrayList ();
			if (cellElevations != null) {
				ce = (CellElevation) cellElevations.get (c);
				if (ce == null) {
					Log.println (Log.ERROR, "Viewer2DHalf.drawCell ()", "cell elevation is null for cell: " + c);
				} else {
					correctedZs.add (ce.z0);
					correctedZs.add (ce.z1);
					correctedZs.add (ce.z2);
					correctedZs.add (ce.z3);
				}
			}

			Collection vertices = c.getVertices ();
			Vertex2d prevV2 = null;
			Vertex2d firstV2 = null;
			Iterator aux = correctedZs.iterator ();

			for (Iterator j = vertices.iterator (); j.hasNext ();) {
				Vertex3d v = (Vertex3d) j.next ();

				double z = v.z; // without elevation
				try {
					if (ce != null) {
						z = ((Double) aux.next ()).doubleValue ();
					} // fc - 16.3.2006
				} catch (Exception e) {
					Log.println (Log.ERROR, "Viewer2DHalf.drawCell ()",
							"exception when trying to get a corrected elevated z, for cell: " + c, e);
				}

				Vertex2d v2 = rotate (v.x, v.y, z, Cx, Cy, Cz, rotations);
				if (firstV2 == null) {
					firstV2 = v2;
				} // memo first point to close the polygon at the end
				if (prevV2 != null) {
					Shape line = new Line2D.Double (prevV2.x, prevV2.y, v2.x, v2.y);
					// ~ Rectangle2D bBox = line.getBounds2D ();
					// ~ if (r.intersects (bBox)) {
					g2.draw (line);
				}

				prevV2 = v2;
			}

			// close the polygon
			if (prevV2 != null && firstV2 != null) {
				Shape line = new Line2D.Double (prevV2.x, prevV2.y, firstV2.x, firstV2.y);
				g2.draw (line);
			}
			drawnCells.add (c);
		}

	}

	// Draw a tree at the given coordinates
	// subject is a GTree + Spatialized or a SpatializedObject with a GTree
	// inside its getObject ()
	//
	private void drawTree (java.awt.Graphics g, Rectangle.Double r, Spatialized subject, double x, double y) {

		Graphics2D g2 = (Graphics2D) g;

		Tree t = null;
		if (subject instanceof Tree) {
			t = (Tree) subject;
		} else {
			t = (Tree) ((SpatializedObject) subject).getObject ();
		}

		// 1. Draw the trunk
		//
		double treeDbh = t.getDbh () / 100;
		double treeRadius = treeDbh / 2;
		double treeHeight = t.getHeight ();

		// Lollipop lollie = LollipopFactory.getLollipop (t);

		// lollie.draw (g, t, x, y, withLabel, fast);
		SpatializedObject so = new SpatializedObject (t, x, y, 0);
		if (selectedSubjects.contains (subject)) {
			so.setSelected (true);
		}

		try {
			Rectangle2D bBox = lollypop.draw2DView (g, r, so);
			// ~ memo.put (bBox, t);
			if (!bBox.isEmpty ()) {
				memo.put (bBox, subject);
			}
		} catch (Exception e) {
			Log.println (Log.ERROR, "Viewer2DHalf", "error while drawing tree", e);
		}
	}

	/**
	 * Some selection was done directly in this ObjectViewer -> open an
	 * inspector
	 */
	public JPanel select (Rectangle.Double r, boolean ctrlIsDown) {
		if (!ctrlIsDown) {
			selectedSubjects.clear ();
		}

		for (Iterator i = memo.keySet ().iterator (); i.hasNext ();) {
			Rectangle2D bBox = (Rectangle2D) i.next ();
			if (bBox.intersects (r)) {
				Object aux = memo.get (bBox);
				// add or remove aux from selection depending on the ctrl key
				if (selectedSubjects.contains (aux)) {
					selectedSubjects.remove (aux);
				} else {
					selectedSubjects.add (aux);
				}
			}
		}

		// update view, the selected trees are in red
		resetPanel2D ();

		return (selectedSubjects == null) ? null : AmapTools.createInspectorPanel (selectedSubjects);
	}

	// Calculate the size extension of the panel2D to view the complete
	// selected scene
	//
	private void calculatePanel2D (Collection trees) {
		if (trees == null || trees.isEmpty ()) {
			panel2D = null;
			return;
		}

		double x0 = Double.MAX_VALUE;
		double x1 = Double.MIN_VALUE;
		double y0 = Double.MAX_VALUE;
		double y1 = Double.MIN_VALUE;
		double hMax = Double.MIN_VALUE;
		for (Iterator i = trees.iterator (); i.hasNext ();) {

			// We may meet cells, not spatialized trees... - fc - 22.9.2005
			Object o = i.next ();

			// ~ if (!(o instanceof GTree && o instanceof Spatialized)
			// ~ && !(o instanceof SpatializedObject)) { // fc - 19.6.2006 -
			// tree may be in a SpatializedObject

			if (!(o instanceof Spatialized && (o instanceof Tree || o instanceof SpatializedObject))) { // fc
																										// -
																										// 19.6.2006
																										// -
																										// tree
																										// may
																										// be
																										// in
																										// a
																										// SpatializedObject
				i.remove (); // element will be known as "unselected"
				continue;
			}

			Spatialized s = (Spatialized) o;
			Tree t = null;
			if (s instanceof Tree) {
				t = (Tree) o;
			} else {
				t = (Tree) ((SpatializedObject) o).getObject ();
			}

			// fc - 5.2.2004 - allow not spatialized trees
			double getX = 0;
			double getY = 0;
			double getZ = 0;
			getX = s.getX (); // tree real coordinates
			getY = s.getY ();
			getZ = s.getZ ();
			double h = t.getHeight ();

			x0 = Math.min (x0, getX);
			x1 = Math.max (x1, getX);
			y0 = Math.min (y0, getY);
			y1 = Math.max (y1, getY);
			hMax = Math.max (hMax, h);
		}

		// fc - 7.12.2007 - when we zoom, we want to keep the same panel2D with
		// its zoom factor
		// ~ if (panel2D != null
		// ~ && groundRectangle != null
		// ~ && new Rectangle.Double (x0, y0, x1 - x0, y1 - y0)
		// ~ .intersects (groundRectangle)) {
		// ~ return;
		// ~ }

		// We will draw the cells only if they intersect the ground rectangle
		groundRectangle = new Rectangle.Double (x0, y0, x1 - x0, y1 - y0);

		// We need to consider hMax to be sure the trees will be entirely
		// visible
		Rectangle.Double r2 = new Rectangle.Double (x0, y0, x1 - x0, y1 - y0 + hMax);

		panel2D = new Panel2D (this, r2, Panel2D.X_MARGIN_IN_PIXELS, Panel2D.Y_MARGIN_IN_PIXELS);
		// 0,
		// 0);
		scroll.getViewport ().setView (panel2D);
	}

	// Calculate the new coordinates with the given rotations
	// Point to be rotated : (x, y, z)
	// Center of the rotation : (Cx, Cy, Cz)
	// Rotations to be performed : rotations
	//
	private Vertex2d rotate (double x, double y, double z, double Cx, double Cy, double Cz, double[][] rotations) {
		double[] xyz = new double[3];
		xyz[0] = x - Cx;
		xyz[1] = y - Cy;
		xyz[2] = z - Cz;

		double[] xyz2 = matrix33Vector3Product (rotations, xyz);

		double x2, y2;
		x2 = xyz2[0] + Cx; // x
		y2 = xyz2[2] + Cy; // z
		return new Vertex2d (x2, y2);
	}

	// Create a rotation matrix arround the X axis
	//
	private double[][] makeMx (double angle) {
		double[][] m = new double[3][3];
		m[0][0] = 1;
		m[0][1] = 0;
		m[0][2] = 0;
		m[1][0] = 0;
		m[1][1] = Math.cos (angle);
		m[1][2] = -Math.sin (angle);
		m[2][0] = 0;
		m[2][1] = Math.sin (angle);
		m[2][2] = Math.cos (angle);
		return m;
	}

	// Create a rotation matrix arround the Z axis
	//
	private double[][] makeMz (double angle) {
		double[][] m = new double[3][3];
		m[0][0] = Math.cos (angle);
		m[0][1] = -Math.sin (angle);
		m[0][2] = 0;
		m[1][0] = Math.sin (angle);
		m[1][1] = Math.cos (angle);
		m[1][2] = 0;
		m[2][0] = 0;
		m[2][1] = 0;
		m[2][2] = 1;
		return m;
	}

	// Matrices product for 2 matrices [3][3]
	//
	private double[][] matrix33Product (double[][] m, double[][] n) {
		double[][] r = new double[3][3];

		r[0][0] = m[0][0] * n[0][0] + m[0][1] * n[1][0] + m[0][2] * n[2][0];
		r[0][1] = m[0][0] * n[0][1] + m[0][1] * n[1][1] + m[0][2] * n[2][1];
		r[0][2] = m[0][0] * n[0][2] + m[0][1] * n[1][2] + m[0][2] * n[2][2];

		r[1][0] = m[1][0] * n[0][0] + m[1][1] * n[1][0] + m[1][2] * n[2][0];
		r[1][1] = m[1][0] * n[0][1] + m[1][1] * n[1][1] + m[1][2] * n[2][1];
		r[1][2] = m[1][0] * n[0][2] + m[1][1] * n[1][2] + m[1][2] * n[2][2];

		r[2][0] = m[2][0] * n[0][0] + m[2][1] * n[1][0] + m[2][2] * n[2][0];
		r[2][1] = m[2][0] * n[0][1] + m[2][1] * n[1][1] + m[2][2] * n[2][1];
		r[2][2] = m[2][0] * n[0][2] + m[2][1] * n[1][2] + m[2][2] * n[2][2];

		return r;
	}

	// Matrices product for 2 matrices [3][3]
	//
	private double[] matrix33Vector3Product (double[][] m, double[] v) {
		double[] r = new double[3];

		r[0] = m[0][0] * v[0] + m[0][1] * v[1] + m[0][2] * v[2];
		r[1] = m[1][0] * v[0] + m[1][1] * v[1] + m[1][2] * v[2];
		r[2] = m[2][0] * v[0] + m[2][1] * v[1] + m[2][2] * v[2];

		return r;
	}

	// Cell elevations are estimated relatively to the trees inside
	//
	private void calculateCellElevations (SquareCellHolder matrix, Collection cells) {

		cellElevations = new HashMap ();
		int nLin = matrix.getNLin ();
		int nCol = matrix.getNCol ();

		Collection emptyCells = new ArrayList ();

		// 1 Cell center elevation for each cell

		// Cells with trees inside
		for (int i = 0; i < nLin; i++) {
			for (int j = 0; j < nCol; j++) {
				SquareCell c = matrix.getCell (i, j);
				if (c.isEmpty ()) {
					emptyCells.add (c);
				} else {
					CellElevation ce = new CellElevation ();
					cellElevations.put (c, ce);
					for (Iterator k = c.getTrees ().iterator (); k.hasNext ();) {
						Spatialized t = (Spatialized) k.next ();
						ce.zCenter += t.getZ ();
					}
					ce.zCenter /= c.getTreeNumber ();
				}
			}
		}

		// Empty cells
		Collection neighbours = new ArrayList ();
		for (Iterator l = emptyCells.iterator (); l.hasNext ();) {
			SquareCell c = (SquareCell) l.next ();
			neighbours.clear ();
			int i = c.getIGrid ();
			int j = c.getJGrid ();
			for (int a = i - 1; a <= i + 1; a++) {
				for (int b = j - 1; b <= j + 1; b++) {
					// if (a >=0 && a <= nLin && b >= 0 && b <= nCol) {
					if (a >= 0 && a < nLin && b >= 0 && b < nCol) {
						neighbours.add (matrix.getCell (a, b));
					}
				}
			}

			CellElevation ce = new CellElevation ();
			cellElevations.put (c, ce);
			for (Iterator m = neighbours.iterator (); m.hasNext ();) {
				SquareCell n = (SquareCell) m.next ();
				ce.zCenter += n.getZ ();
			}
			ce.zCenter /= neighbours.size ();

		}

		// 2. calculate z0, z1, z3 and z4 for each cell

		CellElevation hole = new CellElevation ();

		for (int i = 0; i < nLin; i++) {
			for (int j = 0; j < nCol; j++) {
				SquareCell c = matrix.getCell (i, j);
				CellElevation c_ce = (CellElevation) cellElevations.get (c);

				SquareCell w = matrix.getCell (i, j - 1);
				SquareCell sw = matrix.getCell (i + 1, j - 1);
				SquareCell s = matrix.getCell (i + 1, j);
				SquareCell se = matrix.getCell (i + 1, j + 1);
				SquareCell e = matrix.getCell (i, j + 1);
				SquareCell ne = matrix.getCell (i - 1, j + 1);
				SquareCell n = matrix.getCell (i - 1, j);
				SquareCell nw = matrix.getCell (i - 1, j - 1);

				CellElevation c_w = (CellElevation) cellElevations.get (w);
				CellElevation c_sw = (CellElevation) cellElevations.get (sw);
				CellElevation c_s = (CellElevation) cellElevations.get (s);
				CellElevation c_se = (CellElevation) cellElevations.get (se);
				CellElevation c_e = (CellElevation) cellElevations.get (e);
				CellElevation c_ne = (CellElevation) cellElevations.get (ne);
				CellElevation c_n = (CellElevation) cellElevations.get (n);
				CellElevation c_nw = (CellElevation) cellElevations.get (nw);

				if (i == 0) { // 1st line
					nw = null;
					n = null;
					ne = null;
					c_nw = hole;
					c_n = hole;
					c_ne = hole;
				}
				if (i == nLin - 1) { // last line
					sw = null;
					s = null;
					se = null;
					c_sw = hole;
					c_s = hole;
					c_se = hole;
				}
				if (j == 0) { // 1st column
					nw = null;
					w = null;
					sw = null;
					c_nw = hole;
					c_w = hole;
					c_sw = hole;
				}
				if (j == nCol - 1) { // last column
					ne = null;
					e = null;
					se = null;
					c_ne = hole;
					c_e = hole;
					c_se = hole;
				}

				// z0 : first vertex
				if (c_ce.z0 == 0) { // if not yet set
					int nb = countCells (c, w, sw, s);
					double z = cumulateZCenters (c, w, sw, s);
					if (nb != 0) {
						z /= nb;
					}
					// ~ if (nb < 2) {z = c_ce.zCenter;} // plot borders

					c_ce.z0 = z;
					c_w.z3 = z;
					c_sw.z2 = z;
					c_s.z1 = z;
				}

				// z1 : second vertex
				if (c_ce.z1 == 0) { // if not yet set
					int nb = countCells (c, w, nw, n);
					double z = cumulateZCenters (c, w, nw, n);
					if (nb != 0) {
						z /= nb;
					}
					// ~ if (nb < 2) {z = c_ce.zCenter;} // plot borders

					c_ce.z1 = z;
					c_w.z2 = z;
					c_nw.z3 = z;
					c_n.z0 = z;
				}

				// z2 : third vertex
				if (c_ce.z2 == 0) { // if not yet set
					int nb = countCells (c, n, ne, e);
					double z = cumulateZCenters (c, n, ne, e);
					if (nb != 0) {
						z /= nb;
					}
					// ~ if (nb < 2) {z = c_ce.zCenter;} // plot borders

					c_ce.z2 = z;
					c_n.z3 = z;
					c_ne.z0 = z;
					c_e.z1 = z;
				}

				// z3 : fourth vertex
				if (c_ce.z3 == 0) { // if not yet set
					int nb = countCells (c, e, se, s);
					double z = cumulateZCenters (c, e, se, s);
					if (nb != 0) {
						z /= nb;
					}
					// ~ if (nb < 2) {z = c_ce.zCenter;} // plot borders

					c_ce.z3 = z;
					c_e.z0 = z;
					c_se.z1 = z;
					c_s.z2 = z;
				}

			}
		}
	}

	// Return the number of non null cells (cell elevation neighbourhood)
	//
	private int countCells (SquareCell c1, SquareCell c2, SquareCell c3, SquareCell c4) {
		int n = 0;
		if (c1 != null) {
			n++;
		}
		if (c2 != null) {
			n++;
		}
		if (c3 != null) {
			n++;
		}
		if (c4 != null) {
			n++;
		}
		return n;
	}

	// Cumulates the elevations of the centers of the non null cells
	// (cell elevation neighbourhood)
	//
	private double cumulateZCenters (SquareCell c1, SquareCell c2, SquareCell c3, SquareCell c4) {
		SquareCell currentCell = null;
		try {
			int cum = 0;
			currentCell = c1;
			if (c1 != null) {
				CellElevation ce = (CellElevation) cellElevations.get (c1);
				cum += ce.zCenter;
			}
			currentCell = c2;
			if (c2 != null) {
				CellElevation ce = (CellElevation) cellElevations.get (c2);
				cum += ce.zCenter;
			}
			currentCell = c3;
			if (c3 != null) {
				CellElevation ce = (CellElevation) cellElevations.get (c3);
				cum += ce.zCenter;
			}
			currentCell = c4;
			if (c4 != null) {
				CellElevation ce = (CellElevation) cellElevations.get (c4);
				cum += ce.zCenter;
			}
			return cum;
		} catch (Exception e) {
			Log.println (Log.ERROR, "Viewer2DHalf.CumulateZCenters ()", "Error in zCenter cumulation for cell: "
					+ currentCell, e);
			return 0;
		}
	}

	// Draw the scene bounding rectangle
	//
	protected void drawRectangle (Graphics2D g2, Panel2D panel2D, Rectangle.Double r, double Cx, double Cy, double Cz,
			double[][] rotations) {

		/*
		 * System.out.println ("----- r:"); System.out.println
		 * ("x="+r.x+" getX()="+r.getX ()); System.out.println
		 * ("y="+r.y+" getY()="+r.getY ()); System.out.println
		 * ("w="+r.width+" getWidth()="+r.getWidth ()); System.out.println
		 * ("h="+r.height+" getHeight()="+r.getHeight ());
		 */

		Vertex2d v0 = rotate (r.x, r.y, 0, Cx, Cy, Cz, rotations);
		Vertex2d v1 = rotate (r.x, r.y + r.height, 0, Cx, Cy, Cz, rotations);
		Vertex2d v2 = rotate (r.x + r.width, r.y + r.height, 0, Cx, Cy, Cz, rotations);
		Vertex2d v3 = rotate (r.x + r.width, r.y, 0, Cx, Cy, Cz, rotations);

		Color memo = g2.getColor ();
		g2.setColor (Color.BLACK);

		g2.draw (new Line2D.Double (v0.x, v0.y, v1.x, v1.y));
		g2.draw (new Line2D.Double (v1.x, v1.y, v2.x, v2.y));
		g2.draw (new Line2D.Double (v2.x, v2.y, v3.x, v3.y));
		g2.draw (new Line2D.Double (v3.x, v3.y, v0.x, v0.y));

		g2.setColor (memo);

	}

	// Draw a referencial frame on target origin
	//
	protected void drawReferencialFrame (Graphics2D g2, Panel2D panel2D, double Cx, double Cy, double Cz,
			double[][] rotations) {
		double w = panel2D.getUserWidth (15); // 15 pixels

		Vertex2d v0 = rotate (Cx, Cy, Cz, Cx, Cy, Cz, rotations);
		Vertex2d vX = rotate (Cx + w, Cy, Cz, Cx, Cy, Cz, rotations);
		Vertex2d vY = rotate (Cx, Cy + w, Cz, Cx, Cy, Cz, rotations);
		Vertex2d vZ = rotate (Cx, Cy, Cz + w, Cx, Cy, Cz, rotations);

		Color memo = g2.getColor ();
		g2.setColor (Color.BLACK);

		g2.draw (new Line2D.Double (v0.x, v0.y, vX.x, vX.y));
		g2.drawString ("x", (float) vX.x, (float) vX.y);

		g2.draw (new Line2D.Double (v0.x, v0.y, vY.x, vY.y));
		g2.drawString ("y", (float) vY.x, (float) vY.y);

		g2.draw (new Line2D.Double (v0.x, v0.y, vZ.x, vZ.y));
		g2.drawString ("z", (float) vZ.x, (float) vZ.y);

		g2.setColor (memo);
	}

	// Draw a selection mark arround target point
	//
	protected void drawSelectionMark (Graphics2D g2, Panel2D panel2D, double x, double y) {
		double w = panel2D.getUserWidth (2); // 2 pixels
		double h = panel2D.getUserHeight (2); // 2 pixels

		double wDec = panel2D.getUserWidth (5);
		double hDec = panel2D.getUserHeight (5);

		double xLeft = x - wDec;
		double xRight = x + wDec;
		double yTop = y + hDec;
		double yBottom = y - hDec;

		Color memo = g2.getColor ();
		g2.setColor (Color.BLACK);

		g2.draw (new Line2D.Double (xLeft, yTop, xLeft, yTop - h));
		g2.draw (new Line2D.Double (xLeft, yTop, xLeft + w, yTop));
		g2.draw (new Line2D.Double (xLeft, yTop - h, xLeft + w, yTop));

		g2.draw (new Line2D.Double (xRight, yTop, xRight - w, yTop));
		g2.draw (new Line2D.Double (xRight, yTop, xRight, yTop - h));
		g2.draw (new Line2D.Double (xRight - w, yTop, xRight, yTop - h));

		g2.draw (new Line2D.Double (xRight, yBottom, xRight, yBottom + h));
		g2.draw (new Line2D.Double (xRight, yBottom, xRight - w, yBottom));
		g2.draw (new Line2D.Double (xRight, yBottom + h, xRight - w, yBottom));

		g2.draw (new Line2D.Double (xLeft, yBottom, xLeft + w, yBottom));
		g2.draw (new Line2D.Double (xLeft, yBottom, xLeft, yBottom + h));
		g2.draw (new Line2D.Double (xLeft + w, yBottom, xLeft, yBottom + h));

		g2.setColor (memo);
	}

}

/*
 * private Panel2D calculatePanel2D (Collection trees) { double x0 =
 * Double.MAX_VALUE; double x1 = Double.MIN_VALUE; double z0 = Double.MAX_VALUE;
 * double z1 = Double.MIN_VALUE; for (Iterator i = trees.iterator (); i.hasNext
 * ();) { GTree t = (GTree) i.next ();;
 * 
 * double treeHeight = t.getHeight (); double treeDbh = t.getDbh () / 100; //
 * cm. -> m. double treeRadius = treeDbh / 2;
 * 
 * if (t instanceof AMAPsimRequestableTree && ((AMAPsimRequestableTree)
 * t).getAMAPsimTreeData () != null) { AMAPsimTreeData simData =
 * ((AMAPsimRequestableTree) t).getAMAPsimTreeData (); treeRadius =
 * simData.treeStep.dbh / 2 / 100; // cm -> m treeHeight =
 * simData.treeStep.height; } else if (t instanceof SimpleCrownDescription) {
 * treeRadius = ((SimpleCrownDescription) t).getCrownRadius (); // m. }
 * 
 * // fc - 5.2.2004 - allow not spatialized trees double getX = 0; double getY =
 * 0; double getZ = 0; getX = ((Spatialized) t).getX (); // tree real
 * coordinates getY = ((Spatialized) t).getY (); getZ = ((Spatialized) t).getZ
 * (); //~ treeXs.put (new Integer (t.getId ()), new Double (getX)); //~
 * treeYs.put (new Integer (t.getId ()), new Double (getY)); //~ treeZs.put (new
 * Integer (t.getId ()), new Double (getZ));
 * 
 * double x = getX; //~ if (viewPoint == EAST || viewPoint == WEST) {x = getY;}
 * 
 * x0 = Math.min (x0, x - treeRadius); x1 = Math.max (x1, x + treeRadius); z0 =
 * Math.min (z0, getZ); z1 = Math.max (z1, getZ + treeHeight); }
 * Rectangle.Double r2 = new Rectangle.Double (x0, z0, x1 - x0, z1 - z0); //~
 * width = 2*r2.getX () + r2.getWidth ();
 * 
 * Panel2D panel2D = new Panel2D (this, r2, Panel2D.X_MARGIN_IN_PIXELS,
 * Panel2D.Y_MARGIN_IN_PIXELS); return panel2D; }
 */
