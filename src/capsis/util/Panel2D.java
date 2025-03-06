/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2020 INRAE
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, Nicolas Beudez
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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Event;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.WxHString;
import capsis.commongui.util.Tools;
import capsis.gui.DUserConfiguration;
import capsis.lib.rubberband.Rubberband;
import capsis.lib.rubberband.RubberbandLine;
import capsis.lib.rubberband.RubberbandRectangle;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * A Panel with zoom and move functions for 2D drawing. Drawing is made in user
 * coordinates (e.g. : meters).
 *
 * The creator must specify a Drawer (may be itself) which draw (g, r) method
 * will be called as needed.
 *
 * Drawing is made in an image which is just redisplayed on repaint () in
 * general case. repaint () force call to paintComponent () which does the job.
 * If the last painted image is not null, it is redisplayed. To force
 * calculation, call reset () then repaint () (needed when zoom in, zoom out,
 * move on the map, resize component, options in Drawer specifying different
 * manner to draw).
 *
 * <PRE>
 * Mouse controls :
 *    left : nothing (because often used to get focus on window)
 *    left & drag : zoom in selected rectangle
 *    ctrl-left : move in given direction according to length
 *    right : cancel last zoom, if nothing to cancel, gets back to initial view
 *    double-left : select
 *    right & drag : select in rectangle
 * </PRE>
 *
 * @author F. de Coligny - June 2001, January 2002
 */
public class Panel2D extends JPanel implements MouseListener, MouseMotionListener, Configurable {
	public static final int X_MARGIN_IN_PIXELS = 30;
	public static final int Y_MARGIN_IN_PIXELS = 30;

	// To speed redisplay when nothing changed
	private Image buffer;

	// Used to add margin to user bounds in paintComponent ()
	private boolean firstTimeOnly;
	int xMarginInPixels;
	int yMarginInPixels;

	boolean wrapPanel; // if true, distort image to wrap the page

	// A panel has EITHER a single drawer (old way) OR a collection of
	// Drawer2D (new way) depending on the constructor
	private Drawer drawer;
	private Collection<Drawer2D> drawer2Ds;

	private ActionListener caller; // fc-29.11.2006 If set, tell him when a selection occurs
	private Collection lastSelection; // fc-29.11.2006 When told, caller can call getLastSelection ()

	private Rectangle.Double initialUserBounds;

	private Rectangle.Double userBounds;
	private AffineTransform panelTransform;
	private AffineTransform fontTransform;
	private int fontSizeInPixels;

	private Rubberband rubberBand; // makes selection rectangles
	private LinkedList memoUserBounds;
	protected InfoDialog infoDialog; // an information dialog may be prompted on selection
	private Point2D.Double currentScale;
	private String dialogTitle;
	private Dimension memoPreferredSize;

	// Panel2D configuration
	private Panel2DSettings settings = new Panel2DSettings(); // default values

	private boolean zoomEnabled; // fc-18.2.2003
	private boolean selectionEnabled; // fc-18.2.2003
	private boolean moveEnabled; // fc-18.2.2003

	private Collection<Rectangle.Double> selections; // fc-15.11.2005

	private Point infoDialogLocation; // fc-15.11.2005

	private boolean infoIconEnabled;
	private ImageIcon infoIcon = IconLoader.getIcon("information_16.png"); // fc-19.4.2007
	private int iconWidth;
	private int iconHeight;
	private int iconX;
	private int iconY;
	private JComponent informationPanel; // under the "i", holds a tooltip

	static {
		Translator.addBundle("capsis.util.Panel2D");
	}

	/**
	 * New constructor : default margins, same scale in x and y, add Drawer2Ds after
	 * with addDrawer2D (). Drawer is set to null, Drawer2Ds will be used instead.
	 * 
	 * <pre>
	*	Example:
	*	Panel2D panel2D = new Panel2D (r, this);	// "this" will be told if selection occurs
	*	panel2D.addDrawer2D (d2);
	 * </pre>
	 */
	public Panel2D(Rectangle.Double initialUserBounds, ActionListener caller) { // fc-29.11.2006
		this(null, initialUserBounds, X_MARGIN_IN_PIXELS, Y_MARGIN_IN_PIXELS);
		this.caller = caller; // fc-29.11.2006
	}

	/**
	 * Constructor 1 : default margins, same scale in x and y.
	 */
	public Panel2D(Drawer drawer, Rectangle.Double initialUserBounds) {
		this(drawer, initialUserBounds, X_MARGIN_IN_PIXELS, Y_MARGIN_IN_PIXELS);
	}

	/**
	 * Constructor 2 : specified margins, same scale in x and y.
	 */
	public Panel2D(Drawer drawer, Rectangle.Double initialUserBounds, int xMarginInPixels, int yMarginInPixels) {
		this(drawer, initialUserBounds, xMarginInPixels, yMarginInPixels, false);
	}

	/**
	 * Constructor 3 : specified margins, different scale in x and y.
	 */
	public Panel2D(Drawer drawer, Rectangle.Double initialUserBounds, int xMarginInPixels, int yMarginInPixels,
			boolean wrapPanel) {
		super();

		zoomEnabled = true;
		selectionEnabled = true;
		moveEnabled = true;
		selections = new ArrayList<Rectangle.Double>(); // fc-15.11.2005
		infoIconEnabled = true; // fc-19.4.2007

		this.wrapPanel = wrapPanel;
		dialogTitle = Translator.swap("Shared.info");

		// The margins will be set when first calling paintComponent () (size () must be
		// known)
		firstTimeOnly = true;
		this.xMarginInPixels = xMarginInPixels;
		this.yMarginInPixels = yMarginInPixels;

		// What component will draw in us ?
		this.drawer = drawer;
		this.initialUserBounds = initialUserBounds; // margins will be added in first paintComponent ()

		// Let's remind positions when zooming to get back when needed
		memoUserBounds = new LinkedList();

		// Listens to mouse events
		addMouseListener(this);
		addMouseMotionListener(this);

		// Resets buffer image in case of resizing
		addComponentListener(new ComponentAdapter() {
			public void componentResized(ComponentEvent e) {
				reset();
				repaint();
			}
		});

		setBackground(Color.WHITE);

	}

	public void setDrawer(Drawer drawer) {
		this.drawer = drawer;
		drawer2Ds = null; // fc-29.11.2006 Drawer and drawer2Ds are exclusive
	}

	/**
	 * Reset the image buffer: will force the recalculation in paintComponent ().
	 * Callable from the Drawer component: if draw options have changed.
	 */
	public void reset() {
		buffer = null;
	}

	/**
	 * Dispose the used resources (dialog box).
	 */
	public void dispose() {
		if (infoDialog != null) {
			infoDialog.close();
			infoDialog.dispose();
		}
	}

	/**
	 * Return to original position and scale (cancel all zooms and moves).
	 */
	public void resetUserBounds() {
		userBounds = initialUserBounds;
		reset();
		repaint();
	}

	/**
	 * Called one time only at the beginning: adds margins to enlarge user bounds.
	 */
	public Rectangle.Double addMarginToUserBounds(Rectangle.Double userBounds, Rectangle bounds) {
		this.userBounds = userBounds;

		// We compute margins for a "normal" window size (200x200)
		Rectangle normale = new Rectangle(0, 0, 200, 200);
		Point2D.Double scaleFactor = getScale(userBounds, normale);

		double xUserMargin = ((double) xMarginInPixels) / scaleFactor.x;
		double yUserMargin = ((double) yMarginInPixels) / scaleFactor.y;

		return new Rectangle.Double(userBounds.x - xUserMargin, userBounds.y - yUserMargin,
				userBounds.width + 2 * xUserMargin, userBounds.height + 2 * yUserMargin);
	}

	/**
	 * Return scale.
	 */
	public Point2D.Double getScale(Rectangle.Double userBounds, Rectangle bounds) {
		double w = bounds.getWidth() / userBounds.getWidth();
		double h = bounds.getHeight() / userBounds.getHeight();

		if (!wrapPanel) {
			double d = Math.min(w, h);
			currentScale = new Point2D.Double(d, d);
		} else {
			currentScale = new Point2D.Double(w, h);
		}

		return currentScale;
	}

	/**
	 * Return the current scale, i.e. the result of the last call to getScale ().
	 * Drawer can use this method to know the current scale.
	 */
	public Point2D.Double getCurrentScale() {
		return currentScale;
	}

	/**
	 * Re init user bounds to be drawn in Panel2D.
	 */
	public void initUserBounds(Rectangle.Double r) {
		initialUserBounds = r;
		// The margins will be set when first calling paintComponent () (size () must be
		// known)
		firstTimeOnly = true;

		reset();
		repaint();
	}

	/**
	 * Return initial user bounds, those given to the constructor.
	 */
	// fc-4.2.2004 (AmapSim / SideViewDrawer measure rod
	public Rectangle.Double getInitialUserBounds() {
		return initialUserBounds;
	}

	/**
	 * Return xMarginInPixels
	 */
	public int getXMarginInPixels() {
		return xMarginInPixels;
	}

	/**
	 * Return yMarginInPixels
	 */
	public int getYMarginInPixels() {
		return yMarginInPixels;
	}

	/**
	 * Sets new user bounds to be drawn in Panel2D.
	 */
	public void setUserBounds(Rectangle.Double r) {
		userBounds = r;
		reset();
		repaint();
	}

	/**
	 * Zoom to match rectangle described with these two extreme points.
	 */
	public void zoomIn(java.awt.geom.Point2D.Double fromP, java.awt.geom.Point2D.Double toP) {
		memoUserBounds.addLast(userBounds);

		double xMin = Math.min(fromP.getX(), toP.getX());
		double yMin = Math.min(fromP.getY(), toP.getY());
		double xMax = Math.max(fromP.getX(), toP.getX());
		double yMax = Math.max(fromP.getY(), toP.getY());

		setUserBounds(new Rectangle.Double(xMin, yMin, xMax - xMin, yMax - yMin));
	}

	/**
	 * Cancel one memorized zoom action.
	 */
	public void zoomOut() {
		if (!memoUserBounds.isEmpty()) {
			Rectangle.Double r = (Rectangle.Double) memoUserBounds.removeLast();
			setUserBounds(r);
		} else {
			setUserBounds(initialUserBounds);
		}
	}

	/**
	 * Moves the drawing from last to anchor.
	 */
	public void move(Point anchor, Point last) {
		java.awt.geom.Point2D.Double userAnchor = getUserPoint(anchor);
		java.awt.geom.Point2D.Double userLast = getUserPoint(last);

		double dx = -(userAnchor.x - userLast.x);
		double dy = -(userAnchor.y - userLast.y);

		setUserBounds(new Rectangle.Double(userBounds.x + dx, userBounds.y + dy, userBounds.width, userBounds.height));
	}

	/**
	 * Return user rectangle from pixel rectangle.
	 */
	public Rectangle.Double getUserRectangle(Rectangle r) {

		java.awt.geom.Point2D.Double p2 = getUserPoint(new Point((int) r.getMinX(), (int) r.getMinY()));
		double w2 = getUserWidth(r.width);
		double h2 = getUserHeight(r.height);
		Rectangle.Double r2 = new Rectangle.Double(p2.x, p2.y - h2, w2, h2);

		return r2;
	}

	/**
	 * Width of an horizontal line : pixels -> user size.
	 */
	public double getUserWidth(int pixelLength) {
		Point2D.Double scaleFactor = getScale(userBounds, getBounds());
		return ((double) pixelLength) / scaleFactor.x;
	}

	/**
	 * Height of a vertical line : pixels -> user size.
	 */
	public double getUserHeight(int pixelLength) {
		Point2D.Double scaleFactor = getScale(userBounds, getBounds());
		return ((double) pixelLength) / scaleFactor.y;
	}

	/**
	 * Width of an horizontal line : user size -> pixels. EXPERIMENTAL-fc
	 */
	public int getPixelWidth(double userLength) {
		Point2D.Double scaleFactor = getScale(userBounds, getBounds());
		return (int) (userLength * scaleFactor.x);
	}

	/**
	 * User point -> pixel point. EXPERIMENTAL-fc
	 */
	public Point getPixelPoint(java.awt.geom.Point2D userPoint) {
		try {
			java.awt.geom.Point2D p2 = panelTransform.transform(userPoint, null);
			return new Point((int) p2.getX(), (int) p2.getY());
		} catch (Exception e) {
			Log.println(Log.WARNING, "Panel2D.getPixelPoint ()", "Exception during panel transformation", e);
			return new Point(); // should not occur
		}
	}

	/**
	 * Translates pixel X and Y to user coordinates.
	 */
	public java.awt.geom.Point2D.Double getUserPoint(Point pixelPoint) {
		try {
			return (java.awt.geom.Point2D.Double) panelTransform.inverseTransform(pixelPoint,
					new java.awt.geom.Point2D.Double());
		} catch (Exception e) {
			Log.println(Log.WARNING, "Panel2D.getUserPoint ()", "Exception during panel transform inversion", e);
			return new java.awt.geom.Point2D.Double(); // should not occur
		}
	}

	/**
	 * Return current user bounds.
	 */
	public Rectangle2D.Double getUserBounds() {
		return userBounds;
	}

	/**
	 * Return current panel transform.
	 */
	public AffineTransform getPanelTransform() {
		return panelTransform;
	}

	/**
	 * This method is called by java when needed or explicit call to Panel2D.repaint
	 * (). If an image was buffered, redisplay it in panel else recomputes the scene
	 * in image before displaying it. Drawing is delegated to Drawer component.
	 */
	public void paintComponent(Graphics graphics) {

		// Add margin to initial user bounds: only once !
		// this must be done here because in constructor, getBounds () is wrong
		// this inits this.initialUserBounds only once
		if (firstTimeOnly) {
			initialUserBounds = addMarginToUserBounds(initialUserBounds, getBounds());
			resetUserBounds();
			firstTimeOnly = false;
		}

		// If no change, redraw same thing...
		Rectangle bounds = getBounds();
		if (buffer != null) {
			graphics.drawImage(buffer, 0, 0, null); // null : image observer

			// fc-19.4.2007
			addInfoIcon(graphics, bounds);

			return;

			// ...else create an empty image with good size
		} else {
			buffer = createImage(bounds.width, bounds.height);
		}

		// From now on, we work in the image
		Graphics g = buffer.getGraphics();

		super.paintComponent(g);

		// ~ Rectangle bounds = getBounds ();
		Graphics2D g2 = (Graphics2D) g;

		// Apply scale factor to represent the chosen user zone on
		// the available device zone
		Point2D.Double scaleFactor = getScale(userBounds, bounds);

		// Transform : Origin is panel's bottom left corner and Y grows upward
		panelTransform = new AffineTransform();
		panelTransform.translate(((double) bounds.width) / 2, ((double) bounds.height) / 2);
		panelTransform.scale(1, -1);
		panelTransform.scale(scaleFactor.x, scaleFactor.y);
		panelTransform.translate(-userBounds.width / 2 - userBounds.x, -userBounds.height / 2 - userBounds.y);
		g2.transform(panelTransform);

		// Panel's font transform
		fontTransform = new AffineTransform();
		fontTransform.scale(1, -1);
		fontTransform.scale(1 / scaleFactor.x, 1 / scaleFactor.y);

		Font gFont = g.getFont();

		// Possible to set the font size just after the construction with
		// setFontSizeInPixels (int)
		// if not, reduce the default font size : -2
		// fc-16.6.2006-(Isgm)
		if (fontSizeInPixels == 0) {
			fontSizeInPixels = gFont.getSize() - 2;
		}

		Font userFont = new Font(gFont.getName(), gFont.getStyle(), fontSizeInPixels); // main font used to write in the
																						// panel
		g2.setFont(userFont.deriveFont(fontTransform));

		// Like this, pencil size is always one pixel large
		g2.setStroke(new BasicStroke(0f));

		Rectangle.Double r = getUserRectangle(getBounds());

		// Not sufficient : outside trees are drawn if nothing is done
		// (so something can be done in drawer according to the given rectangle
		// below :-). See SVSimple "preprocesses").
		g2.setClip(r);

		// Set antialiasing and pencil size according to current options fc-30.04.2002
		if (settings.isAntiAliased()) {
			g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		}
		g2.setStroke(new BasicStroke(settings.getPencilSize()));

		// Delegate drawing to the Drawer : draws in the g Graphics restricted
		// to the r Rectangle (r in user coordinates).
		drawer.draw(g, r);

		// fc-19.4.2007
		addInfoIcon(g, bounds);

		// Memo image to avoid unneeded repaints next time
		graphics.drawImage(buffer, 0, 0, null); // null : image observer

	}

	// fc-19.4.2007
	// add an icon at the top right corner to open Panel2D config panel
	//
	private void addInfoIcon(Graphics g, Rectangle bounds) {
		if (!infoIconEnabled) {
			return;
		} // this icon can be vetoed
		if (infoIcon == null)
			return;

		Graphics2D g2 = (Graphics2D) g;

		// fc-19.4.2007 Add "i" little image for configuration in a corner
		g2.setTransform(new AffineTransform()); // reset Identity
		iconWidth = infoIcon.getIconWidth();
		iconHeight = infoIcon.getIconHeight();
		iconX = bounds.width - iconWidth;
		iconY = 0;
		infoIcon.paintIcon(this, g2, iconX, iconY);

		if (informationPanel != null)
			this.remove(informationPanel);
		
		informationPanel = new JPanel();
		informationPanel.setOpaque(false);
		informationPanel.setBounds(iconX, iconY, iconWidth, iconHeight);
		this.add(informationPanel);
		informationPanel.setToolTipText("<html>" + "<p>" + Translator.swap("Panel2D.preferences") + "<p>"
				+ Translator.swap("Panel2D.userGuide") + "</html>");
		informationPanel.addMouseListener(this);

	}

	/**
	 * Return infoDialog
	 */
	public InfoDialog getInfoDialog() {
		return infoDialog;
	} // fc-7.12.2007

	/**
	 * Return true if infoDialog is visible
	 */
	public boolean isInfoDialogVisible() {
		// fc-7.12.2007
		return infoDialog == null ? false : infoDialog.isVisible();
	}

	// Shows a panel in a dialog box (used in response to some selection).
	// fc-6.12.2007 Now public for some special cases related to reselection,
	// see SVSimple
	public void show(JPanel pan) {

		if (pan == null) {
			if (infoDialog != null) {
				infoDialog.destroy();
				infoDialog = null;
			}
			return;
		}

		if (infoDialog == null) {
			Window w = Tools.getWindow(this);
			if (w instanceof JDialog) {
				infoDialog = new InfoDialog((JDialog) w);
			} else if (w instanceof JFrame) {
				infoDialog = new InfoDialog((JFrame) w);
			} else {
				infoDialog = new InfoDialog();
			}

			infoDialog.addComponentListener(new ComponentAdapter() { // fc-15.11.2005
				public void componentMoved(ComponentEvent evt) {
					infoDialogLocation = infoDialog.getLocation();
				}

				public void componentResized(ComponentEvent evt) {
					memoInfoDialogSize(infoDialog, infoDialog.getSize());
				}
			});

			infoDialog.addWindowListener(new WindowAdapter() {
				public void windowClosing(WindowEvent evt) {
					infoDialog.destroy();
					infoDialog = null;
					// fc-15.11.2005 Cancel selection
					Rectangle.Double emptySelection = new Rectangle.Double(-Double.MAX_VALUE, -Double.MAX_VALUE, 1, 1);
					drawer.select(emptySelection, false);
				}
			});

			// fc-27.2.2004 Use positioner is difficult because an info dialog can
			// open another infoDialog...
			//
			if (infoDialogLocation != null) {
				infoDialog.setLocation(infoDialogLocation);
			} else {
				infoDialog.setLocationRelativeTo(this.getParent());
			}

			infoDialog.setPanel(pan);
			infoDialog.setSize(getInfoDialogSize(infoDialog));
			infoDialog.setResizable(true);

			// fc-14.12.2020 Unused, deprecated
//			if (pan instanceof capsis.extension.AbstractObjectViewer) {
//				((capsis.extension.AbstractObjectViewer) pan).activate(); // Top dialog story in Sketch
//			}

			infoDialog.setVisible(true);

		} else {

			infoDialog.setPanel(pan);
			infoDialog.setVisible(true);

		}
	}

	// Try to memorize the size of info dialog according to what is inside.
	// If it contains an inspector, will be higher, if contains a 2D viewer, may be
	// larger.
	private void memoInfoDialogSize(InfoDialog dlg, Dimension dim) { // fc-25.4.2007
		WxHString wh = new WxHString(dim.width, dim.height);
		Settings.setProperty(getInfoDialogKey(dlg), wh.toString());
	}

	// Get a size for InfoDialog depending of what is inside
	// default dimension if nothing found
	private Dimension getInfoDialogSize(InfoDialog dlg) { // fc-25.4.2007
		String s = Settings.getProperty(getInfoDialogKey(dlg), (String) null);
		if (s == null) {
			return new Dimension(300, 400);
		} // default value

		WxHString wh = new WxHString(s);
		return new Dimension(wh.getW(), wh.getH());
	}

	// Make a memorisation key with the content name included.
	// fc-25.4.2007
	private String getInfoDialogKey(InfoDialog d) {
		StringBuffer key = new StringBuffer("panel2D.info.dialog.size");
		if (d == null) {
			return key.toString();
		} // nothing inside infoDialog

		Container container = d.getContentPane(); // things inside, refine key
		Component[] components = container.getComponents();
		for (int i = 0; i < components.length; i++) {
			Component c = components[i];
			String name = c.getClass().getSimpleName();
			key.append('.');
			key.append(name);
		}
		return key.toString();
	}

	public String getDialogTitle() {
		return dialogTitle;
	}

	public void setDialogTitle(String title) {
		dialogTitle = title;
	}

	private JPanel select(Rectangle.Double selectionRectangle, boolean ctrlKey) {
		if (drawer != null) {
			JPanel info = drawer.select(selectionRectangle, ctrlKey);
			return info; // old way

		} else if (drawer2Ds != null) {

			// to be seen later. Drawer2D implies drawer == null fc-29.11.2006
			lastSelection = new ArrayList();
			for (Drawer2D d : drawer2Ds) {
				lastSelection.addAll(d.select2D(selectionRectangle));
			}

			// new way selection: selection is passed to caller
			if (caller != null) {
				ActionEvent e = new ActionEvent(this, 0, "selection");
				caller.actionPerformed(e);
			}

		}
		return null;
	}

	// MouseListener stuff -----------------------------------------
	public void mouseClicked(MouseEvent evt) {

		// Left double-click
		if (SwingUtilities.isLeftMouseButton(evt) && (evt.getClickCount() == 2)) {
			if (!selectionEnabled) {
				return;
			}

			// Local selection
			// rectangle selection width = 2*shift
			int shift = (int) settings.getSelectionSquareSize() / 2; // fc-19.4.2007
			if (shift < 1) {
				shift = 1;
			} // fc-19.4.2007

			rubberBand = new RubberbandRectangle(this);
			Point center = evt.getPoint();
			rubberBand.anchor(new Point(center.x - shift, center.y - shift));
			rubberBand.end(new Point(center.x + shift, center.y + shift));

			JPanel info = null;
			Rectangle rect = rubberBand.lastBounds();
			Rectangle.Double r2 = getUserRectangle(rect);

			boolean ctrlKey = (evt.getModifiers() & Tools.getCtrlMask()) != 0;
			info = drawer.select(r2, ctrlKey);

			// Reselection management fc-15.11.2005
			if (!ctrlKey) {
				selections.clear();
			}
			selections.add(r2);

			repaint();

			// If selection succeeded, display info component
			show(info);

//		} else {
//			if (evt.getSource().equals(informationPanel)) {
//				return;
//			} // fc-19.4.2007
			
		}
	}

	public void mouseEntered(MouseEvent evt) {
	} // do nothing

	public void mouseExited(MouseEvent evt) {
	} // do nothing

	public void mousePressed(MouseEvent evt) {
		
		if (evt.getSource().equals(informationPanel)) {
			// fc-19.4.2007 "i" at the top right corner -> Panel2D preferences

			String title = Translator.swap("Panel2D.preferences");
			List<ConfigurationPanel> configPans = new ArrayList<ConfigurationPanel>();
			configPans.add(new Panel2DConfigPanel(this));

			new DUserConfiguration(AmapTools.getWindow(this), title, this, configPans);
			return;
		}

		// Prepare rubberband to Move, Zoom or Select
		
		boolean startZooming = SwingUtilities.isLeftMouseButton(evt);
		boolean startSelecting = evt.isControlDown () && SwingUtilities.isLeftMouseButton(evt) || SwingUtilities.isRightMouseButton(evt);
		boolean startMoving = evt.isShiftDown () && SwingUtilities.isLeftMouseButton(evt);
		
		// fc-2.10.2020 Move with shift + mouse movement
		if (startMoving) {

			// Prepare RubberbandLine to move
			
			// ctrl-left : prepare to move
			if (!moveEnabled) {
				return;
			}
			rubberBand = new RubberbandLine(this);
			rubberBand.anchor(evt.getPoint());
			
		} else if (startZooming || startSelecting) {
			
			// Prepare RubberbandRectangle to zoom or select
			
			// right or left : prepare to zoom in or out
			if (!selectionEnabled && !zoomEnabled) {
				return;
			}
			rubberBand = new RubberbandRectangle(this);
			rubberBand.anchor(evt.getPoint());
		}
		
	}

	public void mouseReleased(MouseEvent evt) {

		boolean endZooming = SwingUtilities.isLeftMouseButton(evt);
		boolean endSelecting = evt.isControlDown () && SwingUtilities.isLeftMouseButton(evt) || SwingUtilities.isRightMouseButton(evt);
		boolean endMoving = evt.isShiftDown () && SwingUtilities.isLeftMouseButton(evt);

		// Right button
		if (endSelecting) {
//		if (SwingUtilities.isRightMouseButton(evt)) {

			if (rubberBand instanceof RubberbandRectangle) { // i.e. not ctrl (move)

				// Right selection rectangle : select
				// If mouse has moved, tell the drawer to select in the rectangle
				Rectangle rect = rubberBand.lastBounds();
				if (rect.width > 3 && rect.height > 3) { // mouse has moved
					if (!selectionEnabled) {
						return;
					}

					Rectangle.Double r2 = getUserRectangle(rect);
					boolean ctrlKey = (evt.getModifiers() & Tools.getCtrlMask()) != 0;

					JPanel info = select(r2, ctrlKey); // info = old way, may be null if new way selection

					// Reselection management fc-15.11.2005
					if (!ctrlKey) {
						selections.clear();
					}
					selections.add(r2);

					repaint();
					if (info != null) {
						show(info);
					}

				} else {
					if (!zoomEnabled) {
						return;
					}
					zoomOut();
				}
			}

			// Left button
		} else {
			try {
				rubberBand.end(evt.getPoint());
			} catch (Exception e) {
			} // actions may be disabled

			// Move
			if (rubberBand instanceof RubberbandLine) {
				// we are going to move
				if (!moveEnabled) {
					return;
				}
				move(rubberBand.getAnchor(), rubberBand.getLast());

				// Zoom in
			} else if (rubberBand instanceof RubberbandRectangle) {
				Rectangle rect = rubberBand.lastBounds();
				if (rect.width > 3 && rect.height > 3) { // Mouse has moved : zoom in
					if (!zoomEnabled) {
						return;
					}
					zoomIn(getUserPoint(new Point((int) rect.getMinX(), (int) rect.getMaxY())),
							getUserPoint(new Point((int) rect.getMaxX(), (int) rect.getMinY())));
				}
			}
		}
	}

	// MouseMotionListener stuff -----------------------------------------------
	public void mouseDragged(MouseEvent evt) {
		// Use only left button
		try {
			if (rubberBand != null) {
				rubberBand.stretch(evt.getPoint());
			}
		} catch (Exception e) {
			System.out.println("error in mouseDragged (): " + e);
		}
	}

	public void mouseMoved(MouseEvent evt) {
	} // do nothing

	/**
	 * From Configurable interface
	 */
	public String getConfigLabel() {
		return Translator.swap("Panel2D.panel2D");
	}

	/**
	 * From Configurable interface
	 */
	public ConfigurationPanel getConfigPanel(Object param) {
		return new Panel2DConfigPanel(this);
	}

	/**
	 * From Configurable interface
	 */
	public void applyConfig(ConfigurationPanel panel) {
		// settings have been modified in ConfigPanel on ok action
		reset();
		repaint();
	}

	/**
	 * From Configurable interface
	 */
	public void postConfig() {
	}

	public Panel2DSettings getSettings() {
		return settings;
	}

	public void setSettings(Panel2DSettings s) {
		settings = s;
	}

	public void setZoomEnabled(boolean v) {
		zoomEnabled = v;
	}

	public boolean isZoomEnabled() {
		return zoomEnabled;
	}

	public void setSelectionEnabled(boolean v) {
		selectionEnabled = v;
	}

	public boolean isSelectionEnabled() {
		return selectionEnabled;
	}

	public void setMoveEnabled(boolean v) {
		moveEnabled = v;
	}

	public boolean isMoveEnabled() {
		return moveEnabled;
	}

	public void setFontSizeInPixels(int v) {
		fontSizeInPixels = v;
	}

	public int getFontSizeInPixels() {
		return fontSizeInPixels;
	}

	public void setInfoIconEnabled(boolean b) {
		infoIconEnabled = b;
	} // fc-19.4.2007

	public boolean isInfoIconEnabled() {
		return infoIconEnabled;
	} // fc-19.4.2007

} // end of Panel2D
