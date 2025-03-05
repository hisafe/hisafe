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

//import com.sun.j3d.utils.behaviors.mouse.*;
//import com.sun.j3d.utils.behaviors.vp.*;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.swing.JPanel;
import javax.swing.JScrollPane;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Tree;
import capsis.extension.AbstractObjectViewer;
import capsis.util.Drawer;
import capsis.util.Panel2D;
import capsis.util.methodprovider.GBranch;
import capsis.util.methodprovider.GBranchTree;

/**
 * A side viewer for trees with branches.
 * 
 * @author D. Pont - December 2005
 */
public class NZ1SideViewer extends AbstractObjectViewer implements Drawer {

	static {
		Translator.addBundle ("capsis.extension.objectviewer.NZ1SideViewer");
	}
	
	// nb-07.08.2018
	//public static final String NAME = Translator.swap ("NZ1SideViewer");
	//public static final String DESCRIPTION = Translator.swap ("NZ1SideViewer.description");
	//static public final String AUTHOR = "D. Pont";
	//static public final String VERSION = "1.0";

	private Collection trees;
	private Panel2D panel2D;
	private Map shape_Tree;
	private Map shape_Branch;
	private double maxH_m;

	private JScrollPane scrollPane; // fc - 12.9.2008

	/**
	 * Default constructor.
	 */
	public NZ1SideViewer () {
	}

	@Override
	public void init (Collection s) throws Exception {

		try {
			shape_Tree = new HashMap ();
			shape_Branch = new HashMap ();

			createUI ();

			// fc-5.7.2018 added thisIsAReselection
			boolean thisIsAReselection = false;
			show (s, thisIsAReselection);

		} catch (Exception exc) {
			Log.println (Log.ERROR, "NZ1SideViewer.c ()", exc.toString (), exc);
			throw exc; // fc - 4.11.2003 - object viewers may throw exception
		}

	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith (Object referent) {
		try {
			if (referent instanceof GBranchTree) { return true; }
			if (referent instanceof Collection) {
				Collection c = (Collection) referent;
				if (c.isEmpty ()) { return false; }

				// fc - 22.9.2005
				// Find representative objects (ie with different classes)
				Collection reps = Tools.getRepresentatives (c);
				for (Iterator i = reps.iterator (); i.hasNext ();) {
					if (!(i.next () instanceof GBranchTree)) { return false; }
				}
				return true;
			}

		} catch (Exception e) {
			Log.println (Log.ERROR, "NZ1SideViewer.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return false;
	}

	@Override
	public String getName() {
		return Translator.swap ("NZ1SideViewer.name");
	}

	@Override
	public String getAuthor() {
		return "D. Pont";
	}

	@Override
	public String getDescription() {
		return Translator.swap ("NZ1SideViewer.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	private Collection extractTrees (Collection candidateSelection) {
		trees = (Collection) candidateSelection;

		// fc - 22.9.2005 - keep only the trees, discard other objects
		// (cells...)
		for (Iterator i = trees.iterator (); i.hasNext ();) {
			if (!(i.next () instanceof GBranchTree)) {
				i.remove ();
			}
		}

		return trees;
	}

	// Calculate the size extension of the panel2D to view the complete
	// selected scene
	//
	private void calculatePanel2D (Collection trees) {
		// ~ if (trees == null || trees.isEmpty ()) {
		// ~ panel2D = null;
		// ~ return;
		// ~ }

		maxH_m = 0;
		for (Iterator i = trees.iterator (); i.hasNext ();) {
			GBranchTree gba = (GBranchTree) i.next ();
			Tree gt = gba.getGTree ();
			double h_m = gt.getHeight ();
			if (h_m > maxH_m) {
				maxH_m = h_m;
			}
		}

		Rectangle.Double r2 = new Rectangle.Double (0, 0, trees.size () * maxH_m, maxH_m);
		panel2D = new Panel2D (this, r2, Panel2D.X_MARGIN_IN_PIXELS, Panel2D.Y_MARGIN_IN_PIXELS);
		scrollPane.getViewport ().setView (panel2D);
	}

	private void resetPanel2D () {
		if (panel2D != null) {
			panel2D.reset ();
			panel2D.repaint ();
		}
	}

	@Override
	public Collection show (Collection candidateSelection, boolean thisIsAReselection) {
		realSelection = extractTrees (candidateSelection);
		calculatePanel2D (new ArrayList (realSelection));
		resetPanel2D ();
		// ~ System.out.println (""+getName
		// ()+".select candidateSelection "+candidateSelection.size ()
		// ~ +" realSelection "+realSelection.size ());
		return realSelection;
	}

	/**
	 * User interface definition.
	 */
	private void createUI () {
		// Layout parts
		setLayout (new GridLayout (1, 1));
		// ~ add (panel2D);
		scrollPane = new JScrollPane ();
		add (scrollPane);

		// Do not set sizes explicitly inside object viewers
		// ~ setPreferredSize (new Dimension (250, 250));
	}

	/**
	 * We have to redraw the subscene
	 */
	public void draw (Graphics g, Rectangle.Double r) {
		Graphics2D g2 = (Graphics2D) g;
		shape_Tree.clear ();
		shape_Branch.clear ();
		Color labelColor = null;

		// Draw a scale
		// 1 or 10m in length
		double scaleLength_m = 1;
		if (maxH_m > 10) {
			scaleLength_m = 10;
		}
		Rectangle.Double bounds = panel2D.getInitialUserBounds ();
		int xMarginInPixels = panel2D.getXMarginInPixels ();
		int yMarginInPixels = panel2D.getYMarginInPixels ();
		double xMarginUser = panel2D.getUserWidth (xMarginInPixels);
		double yMarginUser = panel2D.getUserHeight (yMarginInPixels);
		double onePixel = panel2D.getUserWidth (1);
		// double threePixels = panel2D.getUserWidth (2);
		double x0 = bounds.getX () + (onePixel * 5); // move scale in a few
														// pixels so we can see
														// it
		double y0 = 0;

		Shape rod = new Rectangle2D.Double (x0, y0, onePixel, scaleLength_m);
		g2.setColor (new Color (31, 31, 153));
		g2.fill (rod);

		Shape tick = new Rectangle2D.Double (x0, y0, (onePixel * 5), onePixel); // tick
																				// at
																				// 0
																				// meter
		g2.fill (tick);
		tick = new Rectangle2D.Double (x0, y0 + scaleLength_m, (onePixel * 5), onePixel); // tick
																							// at
																							// 1
																							// or
																							// 10
		g2.fill (tick);
		if (maxH_m > 10) {
			g2.drawString ("10m", (float) (x0 + (onePixel * 5)), (float) (y0 + scaleLength_m));
		} else {
			g2.drawString ("1m", (float) (x0 + (onePixel * 5)), (float) (y0 + scaleLength_m));
		}

		int k = 0;
		// Draw the trees
		//
		for (Iterator i = trees.iterator (); i.hasNext ();) {

			GBranchTree b = (GBranchTree) i.next ();
			Tree t = b.getGTree ();

			double x = maxH_m / 2 + (k++ * maxH_m);

			// Tree height, dbh and radius - fc - 10.2.2004
			//
			double treeHeight = t.getHeight ();
			double treeDbh = t.getDbh () / 100; // cm. -> m.
			double treeRadius = treeDbh / 2;
			if (treeRadius < onePixel) {
				treeRadius = onePixel;
			} // make sure radius at least 1 pixel

			// 1. Draw the trunk
			//
			GeneralPath path = new GeneralPath ();
			Shape testTrunk = new Rectangle2D.Double (x - treeRadius, 0, treeDbh, treeHeight);
			Rectangle2D bBox = testTrunk.getBounds2D ();
			boolean canDrawLabel = false;
			if (r.intersects (bBox)) {
				canDrawLabel = true;
				g2.setColor (Color.BLACK);
				// g2.fill (trunk);
				// g2.draw (trunk);
				// shape_Tree.put (bBox, t);

				path.moveTo ((float) x, 0);
				path.lineTo ((float) (x + treeRadius), 0);
				path.lineTo ((float) (x + onePixel), (float) treeHeight); // tip
																			// radius
																			// is
																			// fixed
																			// at
																			// 1
																			// pixel
				path.lineTo ((float) (x - onePixel), (float) treeHeight);
				path.lineTo ((float) (x - treeRadius), 0);
				path.closePath ();
				g2.draw (path);
				shape_Tree.put (path, t);
			}

			// crown
			// Draw branches
			Collection branches = b.getGBranches ();
			// Log.println ("NBranches" + branches.size() );
			for (Iterator j = branches.iterator (); j.hasNext ();) {
				GBranch gb = (GBranch) j.next ();
				drawBranch (g2, r, (float) x, gb);
			}

			// 3. Label drawing
			if (canDrawLabel) {
				// A label if detail threshold is reached
				if (true) {
					g2.setColor (Color.RED);
					g2.drawString (String.valueOf (t.getId ()), (float) (x - (treeRadius)),
							(float) (treeHeight + (10 * treeRadius)));
				}
			}

		}

	}

	/**
	 * Draws a branch
	 */
	private void drawBranch (Graphics2D g2, Rectangle.Double r, float xTree, GBranch b) {
		AffineTransform svg = g2.getTransform ();

		float halfPixel = (float) (panel2D.getUserWidth (1) / 2.0);

		GeneralPath path = new GeneralPath ();
		float diameter_m = (float) (b.getDiameter_mm () / 1000); // mm -> m
		float height_m = (float) b.getInsertionHeight_mm () / 1000;
		float length_m = (float) b.getLength_mm () / 1000;

		Shape testBranch;
		// A rectangle is better when drawing is very small (very far tree)
		// Rectangle locationX, locationY, width, height
		if (b.getInsertionAzimuth_deg () < 180) {
			testBranch = new Rectangle2D.Double (xTree, height_m, length_m, length_m);
		} else {
			testBranch = new Rectangle2D.Double (xTree - length_m, height_m, length_m, length_m);
		}

		// R is the visible rectangle (changes with zoom)
		//
		if (testBranch.intersects (r)) {
			double angleInRadians;

			// construct a branch as a vertical triangle with its base centered
			// at the origin
			// it is translated and rotated later
			// ensure tip and base of branch at least one pixel thick to see
			// something when small
			if (diameter_m < (halfPixel * 2)) {
				diameter_m = (halfPixel * 2);
			}
			path.moveTo (0, 0);
			path.lineTo (diameter_m / 2, 0);
			path.lineTo (halfPixel, length_m);
			path.lineTo (-halfPixel, length_m);
			path.lineTo (-diameter_m / 2, 0);
			path.closePath ();

			// branches with azimuth < 180 are drawn to right, >= 180 to left
			angleInRadians = Math.toRadians (b.getInsertionAngle_deg ());
			if (b.getInsertionAzimuth_deg () >= 180) {
				angleInRadians = -angleInRadians;
			}

			AffineTransform rot = new AffineTransform ();
			AffineTransform pos = new AffineTransform ();
			rot.rotate (angleInRadians);
			pos.translate (xTree, height_m);
			g2.transform (pos);
			g2.transform (rot);

			g2.fill (path);
			shape_Branch.put (path, b);

			g2.setTransform (svg);
		}
	}

	/**
	 * Some selection was done, open an inspector panel
	 */
	public JPanel select (Rectangle.Double r, boolean more) {

		// if no branches selected see if one or more trees selected
		Collection selTrees = new ArrayList ();
		for (Iterator i = shape_Tree.keySet ().iterator (); i.hasNext ();) {
			Rectangle2D bBox = (Rectangle2D) i.next ();
			if (bBox.intersects (r)) {
				selTrees.add (shape_Tree.get (bBox));
			}
		}
		return AmapTools.createInspectorPanel (selTrees);
	}

}
