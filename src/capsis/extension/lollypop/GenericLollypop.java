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

package capsis.extension.lollypop;

import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.JPanel;

import jeeb.lib.defaulttype.SimpleCrownDescription;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex2d;
import jeeb.lib.util.annotation.Ignore;
import jeeb.lib.util.annotation.Param;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Tree;
import capsis.extension.PaleoLollypop;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.ConfigPanel;
import capsis.util.ConfigPanelable;
import capsis.util.Polygon2D;
import capsis.util.SpatializedObject;



/**	GenericLollypop: draw simple lollypop trees from top or in 2D / 3D.
*
*	@author F. de Coligny - march 2006
*/
public class GenericLollypop extends PaleoLollypop
		implements ConfigPanelable {

	public final static Rectangle2D EMPTY_RECTANGLE_2D = new Rectangle2D.Double (0, 0, 0, 0);
			
	static {
		Translator.addBundle("capsis.extension.lollypop.GenericLollypop");
	}
		

	public static final String MODE_TOP = "modeTop";	// maybe not a String
	public static final String MODE_2D = "mode2D";
	public static final String MODE_3D = "mode3D";

	@Param
	private GenericLollypopStarter starter;
	@Ignore
	private ActionListener listener;
	private ConfigPanel configPanel;
	private NumberFormat formater;


	/**	Phantom constructor.
	*	Only to ask for extension properties (authorName, version...).
	*/
	public GenericLollypop () {}

	/**	Official constructor. It uses the standard Extension starter.
	*/
	public GenericLollypop (GenericExtensionStarter s) throws Exception {

		// Temporary
		if (s == null) {
				throw new Exception ("GenericLollypop needs a non null GenericLollypopStarter");}

		this.starter = (GenericLollypopStarter) s;
		this.listener = starter.listener;

		
		formater = NumberFormat.getNumberInstance();
		formater.setMaximumFractionDigits(2);
		formater.setGroupingUsed (false);
		
		ExtensionManager.applySettings(createMemoKey(), this);

	}

	/**	This is Extension dynamic compatibility mechanism.
	*
	*	This matchwith method must be redefined for each extension subclass.
	*	It must check if the extension can deal (i.e. is compatible) with the referent.
	*	Here, referent must be at least a GModel instance.
	*
	*	Compatible with GTree, GTree+Numberable or collections of them (GMaddTree, GMaidTree...)
	*/
	public boolean matchWith (Object referent) {
		try {
			if (referent instanceof Tree) {return true;}
			if (referent instanceof Collection) {
				Collection c = (Collection) referent;
				Collection reps = Tools.getRepresentatives (c);
				for (Iterator i = reps.iterator (); i .hasNext ();) {
					if (!(i.next () instanceof Tree)) {
						return false;
					}
				}
			}
			return true;

		} catch (Exception e) {
			Log.println (Log.ERROR, "GenericLollypop.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getName () {
		return Translator.swap ("GenericLollypop.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor () {
		return "F. de Coligny";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription () {
		return Translator.swap ("GenericLollypop.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion () {
		return "1.1";
	}

	// nb-09.08.2018
	//public static final String VERSION = "1.1";

	/**	Called when config changes, save it for next time
	*/
	protected void memoSettings () {
		
		ExtensionManager.recordSettings (createMemoKey (), this);
	}

	// Create a key from class name and listener class name
	// This key is used to save config for the couple (this drawer, listener)
	// Listener can for instance be SVLollypopDrawer of Vier2DHalf: they may be
	// configured differently
	private String createMemoKey () {
		String key = ""+this.getClass ().getName ()+"+"+listener.getClass ().getName ();
		return key;
	}

	
	/**	Accessor for starter
	*/
	public GenericLollypopStarter getStarter () {return starter;}

	/**	LollypopDrawer interface
	*/
	public Rectangle2D drawTopView (Graphics g, Rectangle.Double r, SpatializedObject so)
			throws Exception {
		Graphics2D g2 = (Graphics2D) g;
		Color memoColor = g2.getColor ();

		Tree t = (Tree) so.getObject ();
		double x = so.x;
		double y = so.y;
		boolean selected = so.isSelected ();

		Rectangle2D b1 = drawTopCrown (g2, r, t, so.x, so.y, selected);
		Rectangle2D b2 = drawTopTrunk (g2, r, t, so.x, so.y, selected);
		drawTopLabel (g2, r, t, so.x, so.y, selected);

		g2.setColor (memoColor);
		return b1;
	}

	/**	LollypopDrawer interface.
	*	Draw a lollipop for the given object in the given graphics at the given location.
	*	g : the graphics inside which the tree is drawn ;
	*	r : the visible rectangle (in case of zoom...) ;
	*	so : the tree to be drawn ;
	*	so.x, so.y : the location (2D) where the object sould be drawn ;
	*	Current settings in the starter apply (ex: label, colors, shape...)
	*	IMPORTANT : the drawing (2D) must be symetrical according to its vertical axis.
	*/
	public Rectangle2D draw2DView (Graphics g, Rectangle.Double r, SpatializedObject so)
			throws Exception {
		Graphics2D g2 = (Graphics2D) g;
		Color memoColor = g2.getColor ();

		Tree t = (Tree) so.getObject ();
		double x = so.x;
		double y = so.y;
		boolean selected = so.isSelected ();

		Rectangle2D b1 = draw2DTrunk (g2, r, t, so.x, so.y, selected);
		Rectangle2D b2 = draw2DCrown (g2, r, t, so.x, so.y, selected);
		drawTopLabel (g2, r, t, so.x, so.y+t.getHeight (), selected);

		g2.setColor (memoColor);
		return b2;
	}

//=======================================================================================================
	/**	3D view.
	*	Prepare draw3DView in case of settings changes (update GL display lists...). fc - 25.9.2006
	*	Optional: default empty implementation.
	*/
								//~ private int trunk;
								//~ private int sphericCrown;
								
								//~ public void init (GLAutoDrawable drawable)
										//~ throws Exception {
									//~ GL gl = drawable.getGL();
									//~ GLU glu = new GLU ();
											
									//~ // trunk display list
									//~ float someColor[] = { 0.7f, 0.5f, 0.9f, 1.0f };
									//~ GLUquadric gluQuatric = glu.gluNewQuadric ();
									//~ glu.gluQuadricOrientation (gluQuatric, GLU.GLU_OUTSIDE);
									
									//~ trunk = gl.glGenLists (1);
									//~ gl.glNewList (trunk, GL.GL_COMPILE);
									//~ gl.glMaterialfv (GL.GL_FRONT, GL.GL_AMBIENT_AND_DIFFUSE, someColor, 0);
									//~ //glu.gluCylinder (gluQuatric, 0.5, 0, 1, 8, 3);	// a normalized cone
									//~ glu.gluCylinder (gluQuatric, 0.5, 0.5, 1, 8, 3);	// a normalized cone
									//~ gl.glEndList ();
									
									//~ // sphericCrown display list
									//~ float someColor2[] = { 0.7f, 0.9f, 0.5f, 1.0f };
									//~ GLUquadric gluQuatric2 = glu.gluNewQuadric ();
									//~ glu.gluQuadricOrientation (gluQuatric2, GLU.GLU_OUTSIDE);
									
									//~ sphericCrown = gl.glGenLists (1);
									//~ gl.glNewList (sphericCrown, GL.GL_COMPILE);
									//~ gl.glMaterialfv (GL.GL_FRONT, GL.GL_AMBIENT_AND_DIFFUSE, someColor2, 0);
									//~ glu.gluSphere (gluQuatric2, 0.5, 8, 8);	// a normalized sphere
									//~ gl.glEndList ();
								//~ }

	/**	3D view.
	*	Draw the given object in the GLAutoDrawable. fc - 25.9.2006
	*/
									//~ public void draw3DView (GLAutoDrawable drawable, SpatializedObject subject)
											//~ throws Exception {
										//~ GL gl = drawable.getGL();
										
										//~ double x = subject.getX ();
										//~ double y = subject.getY ();
										//~ double z = subject.getZ ();
										
										//~ GTree tree = (GTree) subject.getObject ();
										//~ double treeDbh = tree.getDbh ()/100;
										//~ double treeHeight = tree.getHeight ();
												
										//~ // draw trunk
										//~ if (starter.trunkEnabled) {
											//~ gl.glPushMatrix ();
											//~ gl.glTranslated (x, y, z);
											//~ gl.glScaled (treeDbh, treeHeight, treeDbh);
											//~ gl.glRotated (-90d, 1d, 0d, 0d);
											//~ gl.glCallList (trunk);
											//~ gl.glPopMatrix ();
										//~ }
										
										//~ if (starter.crownEnabled) {
											//~ if (tree instanceof SimpleCrownDescription) {
												//~ SimpleCrownDescription c = (SimpleCrownDescription) tree;
												
												//~ double crownHeight = treeHeight - c.getCrownBaseHeight ();
												//~ double yCrownCenter = (treeHeight + c.getCrownBaseHeight ()) / 2;
												//~ double crownDiameter = 2*c.getCrownRadius ();
									
												//~ // draw sphericCrown
												//~ gl.glPushMatrix ();
												//~ gl.glTranslated (x, yCrownCenter, z);
												//~ gl.glScaled (crownDiameter, crownHeight, crownDiameter);
												//~ // gl.glRotated (-90d, 1d, 0d, 0d);
												//~ gl.glCallList (sphericCrown);
												//~ gl.glPopMatrix ();
											//~ }
										//~ }
										
										//~ // draw a cross on the ground at subject location
										//~ // gl.glPushMatrix();
										//~ // double shift = 0.5;
										//~ // if (starter.trunkEnabled) {
											//~ //drawLine (gl, x-shift, y, z-shift, x+shift, y, z+shift, 0, 0, 1);
										//~ // }
										//~ // drawLine (gl, x-shift, y, z+shift, x+shift, y, z-shift, 0, 0, 1);
										//~ // gl.glPopMatrix();
										
									//~ }

								//~ private void drawLine (GL gl, double x0, double y0, double z0, 
										//~ double x1, double y1, double z1, double c1, double c2, double c3) {
									//~ gl.glBegin (GL.GL_LINES); 
							//~ //		gl.glMaterialfv(GL.GL_FRONT, GL.GL_AMBIENT_AND_DIFFUSE, red, 0);
									//~ gl.glColor3d (c1, c2, c3);
									//~ //gl.glColor3d (1, 0, 1);
									//~ gl.glVertex3d (x0, y0, z0); 
									//~ gl.glVertex3d (x1, y1, z1); 
									//~ gl.glEnd (); 		
									
								//~ }
//-------------------------------------------------------------------------------------------------------------------------------------------------
	
	private Rectangle2D drawTopCrown (Graphics2D g2, Rectangle.Double r,
			Tree t, double x, double y, boolean selected) {
		if (!starter.crownEnabled) {return null;}

		// crown description available ?
		Shape crown = null;
		Color crownColor = Tools.getCrownColor (t);	// fc - 4.9.2008 - centralized method
		if (t instanceof SimpleCrownDescription) {
			SimpleCrownDescription data = (SimpleCrownDescription) t;

			int crownType = data.getCrownType ();
			double crownBaseHeight = data.getCrownBaseHeight ();
			double crownRadius = data.getCrownRadius ();
			double crownDiameter = crownRadius * 2;
			//~ Color crownColor = data.getCrownColor ();
			//~ if (crownColor == null) {crownColor = starter.crownColor;}

			crown = new Ellipse2D.Double (x-crownRadius, y-crownRadius, crownDiameter, crownDiameter);
			if (!crown.getBounds2D ().intersects (r)) {return EMPTY_RECTANGLE_2D;}	// fc - 10.12.2007

			drawCrownShape (g2, r, crown, crownColor, selected);

		} else {	// no crown description : draw a gray circle
			double crownRadius = t.getHeight () / 3 / 2;	// m.
			//double crownHeight = treeHeight * 3/4;
			double crownDiameter = 2*crownRadius; // m.

			crown  = new Ellipse2D.Double (x-crownRadius, y-crownRadius, crownDiameter, crownDiameter);
			if (!crown.getBounds2D ().intersects (r)) {return EMPTY_RECTANGLE_2D;}	// fc - 10.12.2007
			
			//~ Color crownColor = Color.GRAY;
			drawCrownShape (g2, r, crown, crownColor, selected);
		}
		return crown.getBounds2D ();
	}

	private Rectangle2D drawTopTrunk (Graphics2D g2, Rectangle.Double r,
			Tree t, double x, double y, boolean selected) {
		if (!starter.trunkEnabled) {return null;}

		// trunk
		double d = t.getDbh () / 100 * starter.trunkMagnifyFactor;	// cm -> m
		double radius = d/2;
		Shape trunk = new Ellipse2D.Double (x-radius, y-radius, d, d);
		if (!trunk.getBounds2D ().intersects (r)) {return EMPTY_RECTANGLE_2D;}	// fc - 10.12.2007

		Rectangle2D bBox = trunk.getBounds2D ();
		if (r.intersects (bBox)) {
			g2.setColor (selected ? starter.selectionColor : starter.trunkColor);
			g2.fill (trunk);
			g2.setColor (selected ? starter.selectionColor : starter.trunkColor.darker());
			g2.draw (trunk);
		}
		return bBox;
	}

	private void drawTopLabel (Graphics2D g2, Rectangle.Double r,
			Tree t, double x, double y, boolean selected) {
		// tree label: id / dbh...
		String label = starter.labelDbh ? formater.format (t.getDbh ()) : ""+t.getId ();
		if (starter.labelEnabled) {
			if (!starter.labelFrequencyEnabled
					|| t.getId () % starter.labelFrequency == 0) {
				if (!r.contains (new java.awt.geom.Point2D.Double (x, y))) {return;}	// fc - 10.12.2007
				
				g2.setColor (starter.labelColor);
				g2.drawString (label, (float) x, (float) y);
			}
		}

	}

	private Rectangle2D draw2DTrunk (Graphics2D g2, Rectangle.Double r,
			Tree t, double x, double y, boolean selected) {
		if (!starter.trunkEnabled) {return null;}

		//~ double onePixel = panel2D.getUserWidth (1);
		double treeHeight = t.getHeight ();
		double treeDbh = t.getDbh () / 100 * starter.trunkMagnifyFactor;	// cm. -> m.
		double treeRadius = treeDbh / 2;

		// trunk
		Shape trunk = new Rectangle2D.Double (x - treeRadius, y, treeDbh, treeHeight);
		if (!trunk.getBounds2D ().intersects (r)) {return EMPTY_RECTANGLE_2D;}	// fc - 10.12.2007
		
		Rectangle2D bBox = trunk.getBounds2D ();
		if (r.intersects (bBox)) {
			g2.setColor (selected ? starter.selectionColor : starter.trunkColor);
			g2.fill (trunk);
		}
		return bBox;
	}

	private Rectangle2D draw2DCrown (Graphics2D g2, Rectangle.Double r,
			Tree t, double x, double y, boolean selected) {
		if (!starter.crownEnabled) {return null;}

		double treeHeight = t.getHeight ();

		// crown description available ?
		Shape crown  = null;
		Color crownColor = Tools.getCrownColor (t);	// fc - 4.9.2008 - centralized method
		if (t instanceof SimpleCrownDescription) {
			SimpleCrownDescription data = (SimpleCrownDescription) t;

			int crownType = data.getCrownType ();
			double crownBaseHeight = data.getCrownBaseHeight ();
			double crownRadius = data.getCrownRadius ();
			double crownDiameter = crownRadius * 2;
				double crownHeight = treeHeight - crownBaseHeight;
			//~ Color crownColor = data.getCrownColor ();
			//~ if (crownColor == null) {crownColor = starter.crownColor;}

			//Shape crown = new Ellipse2D.Double (x-crownRadius, y-crownRadius, crownDiameter, crownDiameter);
				crown  = null;
				if (crownType == SimpleCrownDescription.CONIC) {
					Vertex2d v1 = new Vertex2d (x-crownRadius, y+crownBaseHeight);
					Vertex2d v2 = new Vertex2d (x, y+treeHeight);
					Vertex2d v3 = new Vertex2d (x+crownRadius, y+crownBaseHeight);
					Collection vertices = new ArrayList ();
					vertices.add (v1);
					vertices.add (v2);
					vertices.add (v3);
					try {
						crown = new Polygon2D (vertices).getShape ();
					} catch (Exception e) {
						Log.println (Log.WARNING, "DefaultLollipopDrawer.draw2DCrown ()", "error while trying to build a CONIC crown due to "+e);
					}
				}
				if (crown == null) {	// CONIC may have failed
					crown = new Ellipse2D.Double (x-crownRadius, y+crownBaseHeight, 2*crownRadius, crownHeight);
				}
			
			// fc - 10.12.2007 - if not in view rectangle, do not draw
			if (!crown.getBounds2D ().intersects (r)) {return EMPTY_RECTANGLE_2D;}
			drawCrownShape (g2, r, crown, crownColor, selected);

		} else {	// no crown description : draw a gray ellipse
			double crownRadius = treeHeight / 3 / 2;	// m.
			double crownHeight = treeHeight * 3/4;
			double crownBaseHeight = treeHeight - crownHeight; // m.

			crown  = new Ellipse2D.Double (x-crownRadius, y+crownBaseHeight, 2*crownRadius, crownHeight);
			if (!crown.getBounds2D ().intersects (r)) {return EMPTY_RECTANGLE_2D;}
			//~ Color crownColor = Color.GRAY;
			drawCrownShape (g2, r, crown, crownColor, selected);
		}
		return crown.getBounds2D ();
	}

	// tool method, used for top and 2D crown shapes
	private void drawCrownShape (Graphics2D g2, Rectangle.Double r, Shape crown,
			Color crownColor, boolean selected) {
		Rectangle2D bBox = crown.getBounds2D ();
		if (r.intersects (bBox)) {
			if (starter.crownOutline) {
				g2.setColor (selected ? starter.selectionColor : crownColor);
				g2.draw (crown);
			} else {
				if (starter.crownFilledFlat) {
					g2.setColor (selected ? starter.selectionColor : crownColor);
					g2.fill (crown);
					g2.setColor (selected ? starter.selectionColor.darker () : crownColor.darker ());
					g2.draw (crown);
				} else if (starter.crownFilledLight) {
					Rectangle ir = crown.getBounds();
					Color c = selected ? starter.selectionColor : crownColor;
					g2.setPaint (new GradientPaint (ir.x, ir.y+ir.height, Color.WHITE,
							ir.x+ir.width, ir.y, c.darker ().darker (), false));
					g2.fill (crown);
				} else if (starter.crownFilledTransparent) {
					Color c = selected ? starter.selectionColor : crownColor;
					int crownAlphaValue = starter.crownAlphaValue;
					int red = c.getRed ();
					int green = c.getGreen ();
					int blue = c.getBlue ();
					Color transparence = new Color (red, green, blue, crownAlphaValue);
					g2.setColor (transparence);
					g2.fill (crown);
				}
			}
		}
	}

	/**	Should return a JPanel if selection done, null if nothing selected
	*	True value for more means add selection to the previous selection
	*/
	public JPanel select (Rectangle.Double r, boolean more) {
		return null;
	}

	/**	ConfigPanel notifies config changes by calling this actionPerformed method
	*/
	public void actionPerformed (ActionEvent e) {
		if (e.getSource ().equals (configPanel)) {
			if (listener != null) {
				// our config changed
				// notify the tool using this Drawer
				// (it may redraw the scene by calling us again)
				Object source = this;
				int id = 0;	//unused
				String command = "config changed";
				ActionEvent e2 = new ActionEvent (source, id, command);

				listener.actionPerformed (e2);
			}
			memoSettings ();	// config changed, save it
		}
	}

	/**	Get the config panel for the subject
	*/
	public ConfigPanel getConfigPanel () {
		if (configPanel == null) {
				configPanel = new GenericLollypopPanel (this);}
		return configPanel;
	}

}




