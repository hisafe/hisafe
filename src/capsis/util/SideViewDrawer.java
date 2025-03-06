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
package capsis.util;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;

import jeeb.lib.defaulttype.SimpleCrownDescription;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex2d;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Tree;
import capsis.gui.MainFrame;
import capsis.lib.amapsim.AMAPsimBranch;
import capsis.lib.amapsim.AMAPsimLayer;
import capsis.lib.amapsim.AMAPsimRequestableTree;
import capsis.lib.amapsim.AMAPsimTreeData;


/**
 * Draw a tree from side view.
 * Can deal with SimpleCrownDescription and AMAPsimRequestableTree.
 * 
 * @author F. de Coligny - september 2001 - reviewed in february 2004
 */
// If trees are not spatialized, draw them on a line
// fc - 5.2.2004
public class SideViewDrawer extends JPanel implements Drawer, ActionListener {
	public static final int BUTTON_SIZE = 24;
	
	public static final int NORTH = 0;
	public static final int EAST = 1;
	public static final int SOUTH = 2;
	public static final int WEST = 3;
	
	private Tree[] trees;	// can be sorted
	
	private Map treeXs;		// fc - 5.2.2004
	private Map treeYs;
	private Map treeZs;
	
	private boolean treesAreSpatialized;
	
	private Color treeColor;
	
	private boolean showLabels;
	
	private Map shape_Tree;
	
	private JScrollPane scrollPane;
	private Panel2D panel2D;
	
	private Icon fromTop = IconLoader.getIcon ("from-top_24.png");
	private Icon fromRight = IconLoader.getIcon ("from-right_24.png");
	private Icon fromBottom = IconLoader.getIcon ("from-bottom_24.png");
	private Icon fromLeft = IconLoader.getIcon ("from-left_24.png");
	private Icon selectedFromTop = IconLoader.getIcon ("from-top-selected_24.png");
	private Icon selectedFromRight = IconLoader.getIcon ("from-right-selected_24.png");
	private Icon selectedFromBottom = IconLoader.getIcon ("from-bottom-selected_24.png");
	private Icon selectedFromLeft = IconLoader.getIcon ("from-left-selected_24.png");
	
	private int viewPoint;
	private double width;
	
	private JButton fromNorth;
	private JButton fromSouth;
	private JButton fromEast;
	private JButton fromWest;

	private JCheckBox cbShowLabels;
	private JButton export;		// fc - 23.7.2004


	/**	Constructor : we will draw the selected tree
	*/
	public SideViewDrawer (Collection selectedTrees, Color treeColor, Color labelColor, boolean showLabels) {
		super (new BorderLayout ());
		
		treeXs = new HashMap ();
		treeYs = new HashMap ();
		treeZs = new HashMap ();
		
		this.treeColor = treeColor;
		//~ this.showLabels = showLabels;
		this.showLabels = Settings.getProperty ("capsis.sideViewDrawer.showLabels", false);
		
		shape_Tree = new HashMap ();
		trees = (Tree[]) selectedTrees.toArray (new Tree[selectedTrees.size ()]);
		
		// fc - 10.2.2004 - if trees are not spatialized, they will be placed on a line
		//
		treesAreSpatialized = true;
		for (int i = 0; i < trees.length; i++) {
			if (!(trees[i] instanceof Spatialized)) {treesAreSpatialized = false;}
		}
		
		this.add (getControlPanel (), BorderLayout.NORTH);	// this line must be before getSouthPanel () one (icons)
		
		scrollPane = new JScrollPane (getSouthPanel2D ());
		
		this.setPreferredSize (new Dimension (250, 250));
		this.add (scrollPane, BorderLayout.CENTER);
	}


	/**	Creates a panel2D for the subscene according to the view point
	*/
	private Panel2D getPanel2D (int viewPoint) {
		double interTreeSpace = 5;			// trees inter space (ex: 5m)
		
		double x0 = Double.MAX_VALUE;
		double x1 = Double.MIN_VALUE;
		double z0 = Double.MAX_VALUE;
		double z1 = Double.MIN_VALUE;
		for (int i = 0; i < trees.length; i++) {
			Tree t = trees[i];
			
			double treeHeight = t.getHeight ();
			double treeDbh = t.getDbh () / 100;		// cm. -> m.
			double treeRadius = treeDbh / 2;
			
			if (t instanceof AMAPsimRequestableTree
					&& ((AMAPsimRequestableTree) t).getAMAPsimTreeData () != null) {
				AMAPsimTreeData simData = ((AMAPsimRequestableTree) t).getAMAPsimTreeData ();
				treeRadius = simData.treeStep.dbh / 2 / 100;	// cm -> m
				treeHeight = simData.treeStep.height;
			} else if (t instanceof SimpleCrownDescription) {
				treeRadius = ((SimpleCrownDescription) t).getCrownRadius ();	// m.
			}
			
			// fc - 5.2.2004 - allow not spatialized trees
			double getX = 0;
			double getY = 0;
			double getZ = 0;
			if (treesAreSpatialized) {
				getX = ((Spatialized) t).getX ();	// tree real coordinates
				getY = ((Spatialized) t).getY ();
				getZ = ((Spatialized) t).getZ ();
			} else {
				getX = i * interTreeSpace;
				getY = 0;	// trees in a line
				getZ = 0;	// no slope
			}
			treeXs.put (new Integer (t.getId ()), new Double (getX));
			treeYs.put (new Integer (t.getId ()), new Double (getY));
			treeZs.put (new Integer (t.getId ()), new Double (getZ));
			
			double x = getX;
			if (viewPoint == EAST || viewPoint == WEST) {x = getY;}
			
			x0 = Math.min (x0, x - treeRadius);
			x1 = Math.max (x1, x + treeRadius);
			z0 = Math.min (z0, getZ);
			z1 = Math.max (z1, getZ + treeHeight);
		}
		Rectangle.Double r2 = new Rectangle.Double (x0, z0, x1 - x0, z1 - z0);
		width = 2*r2.getX () + r2.getWidth ();

		if (panel2D == null) {
			panel2D = new Panel2D (this, 
					r2, 
					Panel2D.X_MARGIN_IN_PIXELS, 
					Panel2D.Y_MARGIN_IN_PIXELS);
		} else {
			panel2D.initUserBounds (r2);
		}
		return panel2D;
	}


	//	From North
	//
	protected Panel2D getNorthPanel2D () {
		// Sort the trees on decreasing Y
		if (treesAreSpatialized) {Arrays.sort (trees, new SpatializedNorthComparator ());}
		viewPoint = NORTH;
		fromNorth.setIcon (selectedFromTop);
		return getPanel2D (viewPoint);
	}


	//	From East
	//
	protected Panel2D getEastPanel2D () {
		// Sort the trees on decreasing Y
		if (treesAreSpatialized) {Arrays.sort (trees, new SpatializedEastComparator ());}
		viewPoint = EAST;
		fromEast.setIcon (selectedFromRight);
		return getPanel2D (viewPoint);
	}


	//	From South
	//
	protected Panel2D getSouthPanel2D () {
		// Sort the trees on increasing Y
		if (treesAreSpatialized) {Arrays.sort (trees, new SpatializedSouthComparator ());}
		viewPoint = SOUTH;
		fromSouth.setIcon (selectedFromBottom);
		return getPanel2D (viewPoint);
	}


	//	From West
	//
	protected Panel2D getWestPanel2D () {
		// Sort the trees on increasing Y
		if (treesAreSpatialized) {Arrays.sort (trees, new SpatializedWestComparator ());}
		viewPoint = WEST;
		fromWest.setIcon (selectedFromLeft);
		return getPanel2D (viewPoint);
	}


	/**	Some button was hit
	*/
	public void actionPerformed (ActionEvent evt) {
		fromNorth.setIcon (fromTop);	
		fromEast.setIcon (fromRight);
		fromSouth.setIcon (fromBottom);
		fromWest.setIcon (fromLeft);
		
		if (evt.getSource ().equals (fromNorth)) {
			scrollPane.getViewport ().setView (getNorthPanel2D ());

		} else if (evt.getSource ().equals (fromEast)) {
			scrollPane.getViewport ().setView (getEastPanel2D ());
		
		} else if (evt.getSource ().equals (fromSouth)) {
			scrollPane.getViewport ().setView (getSouthPanel2D ());
			
			
		} else if (evt.getSource ().equals (fromWest)) {
			scrollPane.getViewport ().setView (getWestPanel2D ());
		
		} else if (evt.getSource ().equals (cbShowLabels)) {
			showLabels = cbShowLabels.isSelected ();
			Settings.setProperty ("capsis.sideViewDrawer.showLabels", ""+showLabels);
			panel2D.reset ();
			panel2D.repaint ();
		
		} else if (evt.getSource ().equals (export)) {
			new ExportComponent (this, MainFrame.getInstance ());
		}
		panel2D.repaint ();
	}


	/**	We have to redraw the subscene
	*/
	public void draw (Graphics g, Rectangle.Double r) {
		Graphics2D g2 = (Graphics2D) g;
		shape_Tree.clear ();
		Color labelColor = null;
		
		// Draw a 1 unit height measure rod
		// fc - 4.2.2004
		//
		Rectangle.Double bounds = panel2D.getInitialUserBounds ();
		int xMarginInPixels = panel2D.getXMarginInPixels ();
		int yMarginInPixels = panel2D.getYMarginInPixels ();
		double xMarginUser = panel2D.getUserWidth (xMarginInPixels);
		double yMarginUser = panel2D.getUserHeight (yMarginInPixels);
		double onePixel = panel2D.getUserWidth (1);
		double threePixels = panel2D.getUserWidth (2);
		double x0 = bounds.getX ();
		double y0 = 0;

		Shape rod = new Rectangle2D.Double (x0, y0, onePixel, 1);
		g2.setColor (new Color (31, 31, 153));
		g2.fill (rod);
		
		Shape tick = new Rectangle2D.Double (x0, y0, threePixels, onePixel);	// tick at 0 meter
		g2.fill (tick);
		tick = new Rectangle2D.Double (x0, y0+1, threePixels, onePixel);	// tick at 1 meter
		g2.fill (tick);
		g2.drawString ("1", (float) (x0+threePixels), (float) (y0+1));
		
		
		// Draw the trees
		//
		for (int i = 0; i < trees.length; i++) {
			Tree t = trees[i];
			
			boolean amapsimTree = false;
			boolean crownDescriptionTree = false;
			
			// Tree position
			// If we draw from east or west, we use y instead of x
			//
			double x = ((Double) treeXs.get (new Integer (t.getId ()))).doubleValue ();
			double y = ((Double) treeYs.get (new Integer (t.getId ()))).doubleValue ();
			double z = ((Double) treeZs.get (new Integer (t.getId ()))).doubleValue ();
			if (viewPoint == NORTH) {x = width - x;}
			if (viewPoint == EAST) {x = y;}
			if (viewPoint == WEST) {x = width - y;}
			
			// Tree height, dbh and radius - fc - 10.2.2004
			//
			double treeHeight = t.getHeight ();
			double treeDbh = t.getDbh () / 100;		// cm. -> m.
			
			if (t instanceof AMAPsimRequestableTree 
					&& ((AMAPsimRequestableTree) t).getAMAPsimTreeData () != null) {
				amapsimTree = true;
				AMAPsimTreeData simData = ((AMAPsimRequestableTree) t).getAMAPsimTreeData ();
				treeHeight = simData.treeStep.height;
				treeDbh = simData.treeStep.dbh / 100;		// cm. -> m.
			}
				
			if (treeDbh < onePixel) {treeDbh = onePixel;} 	// min dbh for this viewer =1 pixel. (fc - 10.2.2004)
			double treeRadius = treeDbh / 2;
			
			// 1. Draw the trunk
			//
			Shape trunk = new Rectangle2D.Double (x - treeRadius, z, treeDbh, treeHeight);
			Rectangle2D bBox = trunk.getBounds2D ();
			boolean canDrawLabel = false;
			if (r.intersects (bBox)) {
				canDrawLabel = true;
				g2.setColor (treeColor);
				g2.fill (trunk);
				//~ Shape line = new Line2D.Double (x, z, x, z + treeHeight);
				//~ g2.draw (line);
				
				shape_Tree.put (bBox, t);
			}
			
			// 2. Draw the crown
			//
			boolean crownDrawn = false;		// not drawn yet
			
			// 2.1 AMAPsim crown
			//
			if (amapsimTree) {
				AMAPsimTreeData simData = ((AMAPsimRequestableTree) t).getAMAPsimTreeData ();
				Collection layers = simData.treeStep.layers;
				Collection branches = simData.treeStep.branches;
				
				boolean layersFound = layers != null && !layers.isEmpty ();
				boolean branchesFound = branches != null && !branches.isEmpty ();
				
				Color crownColor = Color.GRAY;
				g2.setColor (crownColor);
					
				if (layersFound) {
					// Draw layers
					double zLayer = z;
					for (Iterator k = layers.iterator (); k.hasNext ();) {
						AMAPsimLayer l = (AMAPsimLayer) k.next ();
						double h = l.layerHeight;	// m - the top of layer height (from ground)
						double hLay = h - zLayer;	// m - the layer real local height
						double d = l.layerDiameter;	// m
						if (d > 0) {
							double rad = d / 2d;	// m
							double size = d;
							if (branchesFound) {size = rad;}
							Shape layer = new Rectangle2D.Double (x - rad, zLayer, size, hLay);
							bBox = trunk.getBounds2D ();
							if (r.intersects (bBox)) {g2.draw (layer);}
						}
						zLayer = z + h;
					}
					crownDrawn = true;
				}
				
				if (branchesFound) {
					// Draw branches
					for (Iterator k = branches.iterator (); k.hasNext ();) {
						AMAPsimBranch b = (AMAPsimBranch) k.next ();
						drawBranch (g2, r, (float) x, b);
					}
				}
				
			}
			
/* Isabelle -> 
			// 2.2 SafeCrownDescription crown
			//
			if (!crownDrawn && t instanceof SafeCrownDescription) {
				crownDescriptionTree= true;
				SafeCrownDescription c = (SafeCrownDescription) t;
				
				// possible methods in SafeCrownDescription - proposals - fc - 6.10.2004
				c.getCrownRadius1 ();	// m.
				c.getCrownRadius2 ();	// m.
				c.getCrownRadius3 ();	// m.
				c.getCrownBaseHeight ();	// m.
				c.getCrownColor ();
				
				// compute crownBaseHeight, crownHeight and crownRadius, 
				// the latter according to the point of view : radius1 if NORTH or SOUTH, 
				// or radius2 if EAST or WEST
				
				Shape crown = new Ellipse2D.Double (x-crownRadius, z+crownBaseHeight, 2*crownRadius, crownHeight);
				bBox = crown.getBounds2D ();
				if (r.intersects (bBox)) {
					if (crownColor == null) {crownColor = Color.green;}
					Rectangle ir = crown.getBounds();
					g2.setPaint(new GradientPaint(ir.x,ir.y+ir.height,Color.white,
							ir.x+ir.width,ir.y, crownColor.darker().darker(),false));
					g2.fill (crown);
				}
				crownDrawn = true;
			}
Isabelle <- */
			
			// 2.3 SimpleCrownDescription crown
			//
			if (!crownDrawn && t instanceof SimpleCrownDescription) {
				crownDescriptionTree= true;
				SimpleCrownDescription c = (SimpleCrownDescription) t;
				double crownRadius = c.getCrownRadius ();	// m.
				double crownBaseHeight = c.getCrownBaseHeight (); // m.
				double crownHeight = treeHeight - crownBaseHeight;
				Color crownColor = c.getCrownColor ();
				
				Shape crown  = null;
				if (c.getCrownType () == SimpleCrownDescription.CONIC) {
					Vertex2d v1 = new Vertex2d (x-crownRadius, z+crownBaseHeight);
					Vertex2d v2 = new Vertex2d (x, z+treeHeight);
					Vertex2d v3 = new Vertex2d (x+crownRadius, z+crownBaseHeight);
					Collection vertices = new ArrayList ();
					vertices.add (v1);
					vertices.add (v2);
					vertices.add (v3);
					try {
						crown = new Polygon2D (vertices).getShape ();
					} catch (Exception e) {
						System.out.println ("SideViewDrawer.draw (): Error while trying to build a CONIC crown due to "+e);
					}
				}
				if (crown == null) {
					crown = new Ellipse2D.Double (x-crownRadius, z+crownBaseHeight, 2*crownRadius, crownHeight);
				}
				bBox = crown.getBounds2D ();
				if (r.intersects (bBox)) {
					if (crownColor == null) {crownColor = Color.green;}
					Rectangle ir = crown.getBounds();
					g2.setPaint(new GradientPaint(ir.x,ir.y+ir.height,Color.white,
							ir.x+ir.width,ir.y, crownColor.darker().darker(),false));
					g2.fill (crown);
				}
				crownDrawn = true;
				
			}
			
			// 2.4 Default crown : a gray ellipse
			//
			if (!crownDrawn) {
				double crownRadius = treeHeight / 3 / 2;	// m.
				double crownHeight = treeHeight * 3/4;
				double crownBaseHeight = treeHeight - crownHeight; // m.
				
				Shape crown  = new Ellipse2D.Double (x-crownRadius, z+crownBaseHeight, 2*crownRadius, crownHeight);
				bBox = crown.getBounds2D ();
				if (r.intersects (bBox)) {
					Rectangle ir = crown.getBounds();
					g2.setPaint(new GradientPaint(ir.x,ir.y+ir.height,Color.white,
							ir.x+ir.width,ir.y, Color.BLACK,false));
					g2.draw (crown);
				}
				crownDrawn = true;
			}
			g2.setPaint (Color.BLACK);
			
			// 3. Label drawing
			if (canDrawLabel) {
				// A label if detail threshold is reached
				if (showLabels) {
					if (amapsimTree) {
						g2.setColor (Color.RED.darker ());
					} else if (crownDescriptionTree) {
						g2.setColor (Color.GREEN.darker ());
					} else {
						g2.setColor (labelColor);
					}
					g2.drawString (String.valueOf (t.getId ()), (float) x, (float) (z + treeHeight));
				}
			}
			
		}
		
	}


	/**	Draws a branch
	*/
	private void drawBranch (Graphics2D g2, Rectangle.Double r, float xTree, AMAPsimBranch b) {
		AffineTransform svg = g2.getTransform ();
		
		double onePixel = panel2D.getUserWidth (1);
		
		GeneralPath path = new GeneralPath ();
		float diameter = b.branchDiameter/100;	// cm -> m
		float height = b.branchHeight;
		float length = b.branchLength;
		
		// A rectangle is better when drawing is very small (very far tree)
		Shape minimumBranch = new Rectangle2D.Double (0, 0, length, onePixel);
		Shape testBranch = new Rectangle2D.Double (xTree, height-length, length, 2*length);
		
		// R is the visible rectangle (changes with zoom)
		//
		if (testBranch.intersects (r)) {
			path.moveTo (0, 0);
			path.lineTo (0, -diameter/2);
			path.lineTo (length, 0);
			path.lineTo (0, diameter/2);
			path.closePath ();
			
			double angleInRadians = Math.PI/2 - Math.toRadians (b.branchAngle);
			
			AffineTransform rot = new AffineTransform ();
			AffineTransform pos = new AffineTransform ();
			rot.rotate (angleInRadians);
			pos.translate (xTree, height);
			g2.transform (pos);
			g2.transform (rot);
			
			g2.fill (minimumBranch);	// id too small, path may be not completly drawn
			g2.fill (path);
			
			g2.setTransform (svg);
		}
	}


	/**	Some selection was done, open an inspector panel
	*/
	public JPanel select (Rectangle.Double r, boolean more) {
		Collection selTrees = new ArrayList ();
		for (Iterator i = shape_Tree.keySet ().iterator (); i.hasNext ();) {
			Rectangle2D bBox = (Rectangle2D) i.next ();
			if (bBox.intersects (r)) {
				selTrees.add (shape_Tree.get (bBox));
			}
		}
			
		return AmapTools.createInspectorPanel (selTrees);
	}


	//	Control panel with view points buttons
	//
	private JComponent getControlPanel () {
		
		// Init icons
		fromNorth = new JButton (fromTop);	
		fromEast = new JButton (fromRight);
		fromSouth = new JButton (fromBottom);
		fromWest = new JButton (fromLeft);
		
		Tools.setSizeExactly (fromNorth, BUTTON_SIZE, BUTTON_SIZE);
		Tools.setSizeExactly (fromEast, BUTTON_SIZE, BUTTON_SIZE);
		Tools.setSizeExactly (fromSouth, BUTTON_SIZE, BUTTON_SIZE);
		Tools.setSizeExactly (fromWest, BUTTON_SIZE, BUTTON_SIZE);
		
		cbShowLabels = new JCheckBox (Translator.swap ("Shared.showLabels"), showLabels);
		ImageIcon icon = IconLoader.getIcon ("export_24.png");
		export = new JButton (icon);
		export.setToolTipText (Translator.swap ("ExportComponent"));
		Tools.setSizeExactly (export, BUTTON_SIZE, BUTTON_SIZE);
		
		JToolBar toolbar = new JToolBar ();
		toolbar.add (fromSouth);
		toolbar.add (fromEast);
		toolbar.add (fromNorth);
		toolbar.add (fromWest);
		toolbar.add (cbShowLabels);
		toolbar.add (export);
		toolbar.setVisible (true);
			
		fromNorth.addActionListener (this);
		fromEast.addActionListener (this);
		fromSouth.addActionListener (this);
		fromWest.addActionListener (this);
		cbShowLabels.addActionListener (this);
		export.addActionListener (this);
		
		return toolbar;
	}


	/**	Try to dispose panel2D with its own infoDialog
	*/
	public void dispose () {
		panel2D.dispose ();
	}
	
}

