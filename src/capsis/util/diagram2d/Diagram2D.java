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

package capsis.util.diagram2d;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;

import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex2d;
import capsis.lib.rubberband.Rubberband;
import capsis.lib.rubberband.RubberbandLine;
import capsis.lib.rubberband.RubberbandRectangle;
import capsis.util.Drawer;
import capsis.util.InfoDialog;

/**
 * A Panel with zoom and move functions for 2D drawing. 
 * Drawing is made in user coordinates (ex : meters).
 * 
 * The creator must specify a Drawer (may be itself) whose 
 * draw (g, r) method will be called as needed.
 * 
 * Drawing is made in an image which is just redisplayed on repaint () in 
 * general case.
 * repaint () force call to paintComponent () which does the job. If
 * the last painted image is not null, it is redisplayed. To force calculation, 
 * call reset () then repaint () (needed when zoom in, zoom out, move on the 
 * map, resize component, options in Drawer specifying different manner to draw).
 * 
 * <PRE>
 * Mouse controls :
 *    left : nothing (because often used to get focus on window)
 *    double-left : select
 *    right & drag : select in rectangle
 * </PRE>
 * 
 * @author F. de Coligny - june 2001, january 2002
 */
public class Diagram2D extends JPanel implements MouseListener, MouseMotionListener {

	// To speed redisplay when nothing changed
	private Image buffer;	
	
	// This component knows what to draw (implements Drawer interface)
	private Drawer drawer;	
	private Rectangle.Double userBounds;
	private GraduationContext xGradContext;
	private GraduationContext yGradContext;

	// This rectangle is smaller than the complete panelBounds rectangle. Drawer will draw in it.
	private Rectangle bounds;	
	
	private AffineTransform panelTransform;
	private AffineTransform fontTransform;
	private Rubberband rubberBand;	// makes selection rectangles
	private JDialog infoDialog;	// an information dialog may be prompted on selection
	private Vertex2d currentScale;

	private boolean drawXAxis;
	private boolean drawYAxis;

	
	/**	Constructor.
	*/
	public Diagram2D (Drawer drawer, 
					Rectangle.Double userBounds, 
					GraduationContext xGradContext, 
					GraduationContext yGradContext) {
		super ();

		// What component will draw in us ?
		this.drawer = drawer;
		
		this.userBounds = userBounds;
		this.xGradContext = xGradContext;
		this.yGradContext = yGradContext;
		
		drawXAxis = true;	// can be changed by accessor
		drawYAxis = true;
		
		// Listens to mouse events
		addMouseListener (this);
		addMouseMotionListener (this);
		
		// Resets buffer image in case of resizing
		addComponentListener (new ComponentAdapter () {
			public void componentResized (ComponentEvent e) {
				reset ();
				repaint ();
			}
		});

		setBackground (Color.WHITE);
	}

	/**	Sets new info concerning Diagram2D drawing. 
	*/
	public void set (Rectangle.Double userBounds, 
					GraduationContext xGradContext, 
					GraduationContext yGradContext) {
		this.userBounds = userBounds;
		this.xGradContext = xGradContext;
		this.yGradContext = yGradContext;
		
		reset ();		
		repaint ();
	}

	/**	Reset the image buffer : will force the recalculation in paintComponent (). 
	*	Callable from the Drawer component : if draw options have changed.
	*/
	public void reset () {buffer = null;}

	/**	Tell if X axis should be drawn.
	*/
	public void setDrawXAxis (boolean b) {drawXAxis = b;}

	/**	Tell if X axis should be drawn.
	*/
	public void setDrawYAxis (boolean b) {drawYAxis = b;}

	/**	This method is called by java when needed or explicit call to
	*	Diagram2D.repaint (). If an image was buffered, redisplay it in
	*	panel else recompute the scene in image before displaying it.
	*	Drawing is delegated to Drawer component.
	*/
	public void paintComponent (Graphics graphics) {
		
		// If no change, redraw same thing...
		if (buffer != null) {
			graphics.drawImage (buffer, 0, 0, null);	// null : image observer
			return;
		}
		
		// panelBounds is the rectangle matching the complete panel
		Rectangle panelBounds = getBounds ();
		
		// Create an empty image with good size
		buffer = createImage (panelBounds.width, panelBounds.height);	
				
		// From now on, we work in the image
		Graphics g = buffer.getGraphics ();
		
		super.paintComponent (g);
		
		Graphics2D g2 = (Graphics2D) g;
		
		// fc - 5.1.2005 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
		Font gFont = g.getFont ();	
		Font userFont = new Font (gFont.getName (), gFont.getStyle (), gFont.getSize ()-2);	// main font used to write in the panel
		g2.setFont (userFont);
		// fc - 5.1.2005 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
		
		FontMetrics fm = g2.getFontMetrics ();

		// Adjust Y and X axes graduation contexts according to FontMetrics and panel size
		// We estimate that we will have half panel width (resp. height) to draw  X (resp. Y) axis
		GraduationContext ygc = new GraduationContext ();	
		GraduationContext xgc = new GraduationContext ();
		if (yGradContext != null) {
			ygc = yGradContext.adjustYAxis (fm, panelBounds.height/2);
		}
		if (xGradContext != null) {
			xgc = xGradContext.adjustXAxis (fm, panelBounds.width/2);
		}
		// Set margins
		int rightMargin = 10;	// in pixels
		int topMargin = ("".equals (ygc.axisName)) ? 10 : 25;		// in pixels
		int leftMargin = ygc.margin;	// in pixels, computed during adjustment
		
		//System.out.println ("ygc.margin = "+ygc.margin+" (in Diagram2D, paintComponent (), after adjustment");
		
		int bottomMargin = (xgc.margin == 0) ? 10 : xgc.margin;	// in pixels, computed during adjustment
		
		// If special instructions, change margins
		if (!drawYAxis) {leftMargin = 10;}
		if (!drawXAxis) {bottomMargin = 10;}
		
		// panelBounds minus margins gives the bounds rectangle where to draw
		bounds = new Rectangle (	leftMargin, 
									topMargin, 
									panelBounds.width - leftMargin - rightMargin, 
									panelBounds.height - topMargin - bottomMargin);

		// SECURITY
		if (bounds.width <= 1 || bounds.height <= 1) {
			// Empty graphics
			graphics.drawImage (buffer, 0, 0, null);	// null : image observer
			return;
		}

		// Get scale factor to represent the chosen user zone on
		// the available device zone
		Vertex2d scaleFactor = getScale ();
		
		// Transform : Origin is panel's bottom left corner and Y grows upward
		panelTransform = new AffineTransform ();
		
		// A translation is added here to take the margins into account - NEW - fc - 22.1.2002
		panelTransform.translate ((double) leftMargin, (double) topMargin);
				
		panelTransform.translate (((double) bounds.width)/2, ((double) bounds.height)/2);
		panelTransform.scale (1, -1);
		panelTransform.scale (scaleFactor.x, scaleFactor.y);
	
		panelTransform.translate (	-userBounds.width/2 -userBounds.x, 
									-userBounds.height/2 -userBounds.y);
	
		g2.transform (panelTransform);
		
		// Panel's font transform
		fontTransform = new AffineTransform ();
		
		// fc - 5.1.2005 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
		// bug correction - bad alignment for grads on the Y axis : "-2" was considered in user space
		//~ fontTransform.translate (0, -2);	// fc - 31.3.2003 
		
		fontTransform.scale (1, -1);
		fontTransform.scale (1/scaleFactor.x, 1/scaleFactor.y);
		
		// fc - 5.1.2005 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
		// bug correction - bad horiz anchor for grads on the X axis caused by wrong FontMetrics use : Moved upper
		//~ Font gFont = g.getFont ();	
		//~ Font userFont = new Font (gFont.getName (), gFont.getStyle (), gFont.getSize ()-2);	// main font used to write in the panel
		
		g2.setFont (userFont.deriveFont (fontTransform));
		
		// Like this, pencel size is always one pixel large
		g2.setStroke (new BasicStroke (0f));
		
		// Draw the axes
		
		double x0 = userBounds.x;
		double y0 = userBounds.y;

		if (drawYAxis) {
			ygc.drawYAxis (g2, x0, fm, this);
		}
		
		if (drawXAxis) {
			xgc.drawXAxis (g2, y0, fm, this);
		}
		
		Rectangle.Double r = getUserRectangle (bounds);
		
		// NOTE: do not clip: horizontal lines at the bottom of the rectangle would not be drawn
		//g2.setClip (r);

		// Delegate drawing to the Drawer : draws in the g Grapphics restricted 
		// to the r Rectangle (r in user coordinates).
		drawer.draw (g, r);
		
		// Memo image to avoid unneeded repaints next time
		graphics.drawImage (buffer, 0, 0, null);	// null : image observer

	}
	
	/**	Return scale. This can be computed only if userBounds and bounds are correctly set.
	*	userBounds is set in constructor or in the set () method. bounds is set in
	*	paintComponent () before calling the Drawer draw () method. So getScale () is
	*	usable from the Drawer's draw method (getUserWidth ()...).
	*/
	public Vertex2d getScale () {
		double w = bounds.getWidth () / userBounds.getWidth ();
		double h = bounds.getHeight () / userBounds.getHeight ();

		currentScale = new Vertex2d (w, h);
		return currentScale;
	}

	/**	Return user rectangle from pixel rectangle.
	*/
	public Rectangle.Double getUserRectangle (Rectangle r) {
		
		java.awt.geom.Point2D.Double p2 = 
				getUserPoint (new Point ((int) r.getMinX (), (int) r.getMinY ()));
		double w2 = getUserWidth (r.width);
		double h2 = getUserHeight (r.height);
		Rectangle.Double r2 = new Rectangle.Double (p2.x, p2.y-h2, w2, h2);

		return r2;
	}

	/**	Width of an horizontal line : pixels -> user size.
	*/
	public double getUserWidth (int pixelLength) {
		Vertex2d scaleFactor = getScale ();
		return ((double) pixelLength) / scaleFactor.x;
	}

	/**	Height of a vertical line : pixels -> user size.
	*/
	public double getUserHeight (int pixelLength) {
		Vertex2d scaleFactor = getScale ();
		return ((double) pixelLength) / scaleFactor.y;
	}

	/**	Width of an horizontal line : user size -> pixels. EXPERIMENTAL - fc
	*/
	public int getPixelWidth (double userLength) {
		Vertex2d scaleFactor = getScale ();
		return (int) (userLength * scaleFactor.x);
	}

	/**	User point -> pixel point. EXPERIMENTAL - fc
	*/
	public Point getPixelPoint (java.awt.geom.Point2D userPoint) {
		try {
			java.awt.geom.Point2D p2 = panelTransform.transform (userPoint, null);
			return new Point ((int) p2.getX (), (int) p2.getY ());
		} catch (Exception e) {
			Log.println (Log.WARNING, "Diagram2D.getPixelPoint ()", 
					"Exception during panel transformation", e);
			return new Point ();		// should not occur
		}
	}

	/**	Translates pixel X and Y to user coordinates.
	*/
	public java.awt.geom.Point2D.Double getUserPoint (Point pixelPoint) {
		try {
			return (java.awt.geom.Point2D.Double) panelTransform.inverseTransform (pixelPoint, 
					new java.awt.geom.Point2D.Double ());
		} catch (Exception e) {
			Log.println (Log.WARNING, "Diagram2D.getUserPoint ()", 
					"Exception during panel transform inversion", e);
			return new java.awt.geom.Point2D.Double ();		// should not occur
		}
	}

	/**	Return current user bounds.
	*/
	public Rectangle2D.Double getUserBounds () {return userBounds;}
	
	/**	Return current panel transform.
	*/
	public AffineTransform getPanelTransform () {return panelTransform;}

	// Show a panel in a dialog box (used in response to some selection).
	//
	private void show (JPanel pan) {
		if (infoDialog == null) {
			infoDialog = new InfoDialog ();
			
			infoDialog.addWindowListener (new WindowAdapter (){
				public void windowClosing (WindowEvent evt) {
					infoDialog.setVisible (false);
					infoDialog.dispose ();
					infoDialog = null;
				}
			});
			
			infoDialog.setLocationRelativeTo (this.getParent ());
			infoDialog.setTitle (Translator.swap ("Shared.info"));
			infoDialog.setSize (new Dimension (200, 200));
			
			infoDialog.setResizable (true);	
		
			infoDialog.setContentPane (pan);
			infoDialog.pack ();
			infoDialog.setVisible (true);
		} else {
			infoDialog.setContentPane (pan);	// don't pack / resize / reposition
			pan.revalidate ();
		}
	}

	/**	Dispose the used resources (dialog box).
	*/
	public void dispose () {
		if (infoDialog != null) {infoDialog.dispose ();}
	}

	// MouseListener stuff -----------------------------------------
	public void mouseClicked (MouseEvent evt) {
		
		// Left double-click
		if (SwingUtilities.isLeftMouseButton (evt) && (evt.getClickCount () == 2)) {
			// Local selection
		//	int shift = 5;	// rectangle selection width = 2*shifft
			int shift = 0;	// special case, one pixel selection
			
			rubberBand = new RubberbandRectangle (this);
			Point center = evt.getPoint ();
			if (shift == 0) {
				rubberBand.anchor (new Point (center.x, center.y));
				rubberBand.end (new Point (center.x+1, center.y+1));
			} else {
				rubberBand.anchor (new Point (center.x-shift, center.y-shift));
				rubberBand.end (new Point (center.x+shift, center.y+shift));
			}			
			JPanel info = null;
			Rectangle rect = rubberBand.lastBounds ();
			Rectangle.Double r2 = getUserRectangle (rect);
			
			// Should we select or deselect ?
			boolean more;
			if (evt.isControlDown ()) {
				more = false;	// Deselect
			} else {
				more = true;	// Do more selection
			}
			info = drawer.select (r2, more);
			reset ();
			repaint ();

			// If selection succeeded, display info component
			if (info != null) {
				show (info);
			}		
		}
	}
	
	public void mouseEntered (MouseEvent evt) {}	// do nothing
	
	public void mouseExited (MouseEvent evt) {}		// do nothing
	
	public void mousePressed (MouseEvent evt) {
		// Prepare rubberband to Move, Zoom or Select
		if (evt.isControlDown () && SwingUtilities.isLeftMouseButton (evt)) {
			// ctrl-left : prepare to move
			rubberBand = new RubberbandLine (this);
		} else {
			// right or left : prepare to zoom in or out
			rubberBand = new RubberbandRectangle (this);
		}
		rubberBand.anchor (evt.getPoint ());
	}
	
	public void mouseReleased (MouseEvent evt) {
		repaint ();	// remove selection rectangle

		// Right button
		if (SwingUtilities.isRightMouseButton (evt)) {
			if (rubberBand instanceof RubberbandRectangle) {	// i.e. not ctrl (move)
				JPanel info = null;
				
				// Right selection rectangle : select
				// If mouse has moved, tell the drawer to select in the rectangle
				Rectangle rect = rubberBand.lastBounds ();
				if (rect.width > 3 && rect.height > 3) {	// mouse has moved
					Rectangle.Double r2 = getUserRectangle (rect);

					// Should we select or deselect ?
					boolean more;
					if (evt.isControlDown ()) {
						more = false;	// Deselect
					} else {
						more = true;	// Do more selection
					}
					info = drawer.select (r2, more);	// should select "first" item found in rectangle
					reset ();
					repaint ();
		
					// If selection succeeded, display info component
					if (info != null) {
						show (info);
					}		
						
				}
			}
			
		// Left button
		} /*else {
			rubberBand.end (evt.getPoint ());

			// Move
			if (rubberBand instanceof RubberbandLine) {
				// we are going to move
				move (rubberBand.getAnchor (), rubberBand.getLast ());

			// Zoom in
			} else if (rubberBand instanceof RubberbandRectangle) {
				Rectangle rect = rubberBand.lastBounds ();
				if (rect.width > 3 && rect.height > 3) {	// Mouse has moved : zoom in
					zoomIn (getUserPoint (new Point ((int) rect.getMinX (), (int) rect.getMaxY ())), 
							getUserPoint (new Point ((int) rect.getMaxX (), (int) rect.getMinY ())));
				}
			}
		}*/
	}
	
	// MouseMotionListener stuff -----------------------------------------------
	public void mouseDragged (MouseEvent evt) {
		// Use only left button
		try {
			if (rubberBand != null) {
				rubberBand.stretch (evt.getPoint());
			}
		} catch (Exception e) {
			System.out.println ("error in mouseDragged (): "+e);
		}
	}
	
	public void mouseMoved (MouseEvent evt) {}	// do nothing

}

// See in Panel2D
/*class InfoDialog extends AmapDialog {
}*/





