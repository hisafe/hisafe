/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2001-2013 Francois de Coligny
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library;
 * if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 */
package capsis.extension.standviewer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Rectangle2D;
import java.awt.geom.Rectangle2D.Double;
import java.util.ArrayList;
import java.util.Collection;

import javax.swing.JPanel;
import javax.swing.JScrollPane;

import jeeb.lib.util.Log;
import jeeb.lib.util.RGB;
import jeeb.lib.util.SetMap;
import jeeb.lib.util.Translator;
import capsis.commongui.projectmanager.StepButton;
import capsis.defaulttype.plotofcells.Cell;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.defaulttype.plotofcells.PolygonalPlot;
import capsis.extension.AbstractStandViewer;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Plot;
import capsis.kernel.Step;
import capsis.util.Drawer;
import capsis.util.Panel2D;

/**
 * SVPolygons is a cartography simple viewer for models with polygonal plots but
 * without trees (e.g. Castaneaonly).
 * 
 * @author F. de Coligny - February 2013
 */
public class SVPolygons extends AbstractStandViewer implements ActionListener, Drawer
/* , OVSelectorSource */{

	static {
		Translator.addBundle("capsis.extension.standviewer.SVPolygons");
	}

	// nb-07.08.2018
	//static public String AUTHOR = "F. de Coligny";
	//static public String VERSION = "1.0";
	//static public String NAME = Translator.swap("SVPolygons.name");
	//static public String DESCRIPTION = Translator.swap("SVPolygons.description");

	// protected OVSelector ovSelector;
	// protected boolean thisIsAReselection; // fc - 11.9.2008

	protected Panel2D panel2D;
	protected Rectangle.Double userBounds; // fc - 12.1.2005

	protected GScene scene;

	protected SetMap<Object, Shape> shapeMap; // drawn shapes, helps for
												// selection

	// protected JPanel mainPanel
	// protected JPanel optionPanel;
	protected JScrollPane scrollPane;

	// protected Rectangle.Double memoSelectionRectangle;
	// protected Collection<Object> memoSelection;
	// protected Collection<Object> effectiveSelection;
	// protected boolean selectCalledByReselect;

	@Override
	public void init(GModel model, Step s, StepButton but) throws Exception {
		super.init(model, s, but);

		// In drawTree, this setMap will be updated to help selection response
		// shapeMap = new SetMap<Tree, Shape>();
		shapeMap = new SetMap<Object, Shape>(); // fc-10.2.2012 trying to draw
												// avatars with S. de Miguel

		// memoSelection = new HashSet<Object> ();
		// effectiveSelection = new HashSet<Object> ();
		// selectCalledByReselect = false;

		scene = step.getScene(); // step is set by superclass

		// ExtensionManager.applySettings (this.getClass ().getName (), this);

		// createOVSelector (scene);

		userBounds = createUserBounds();

		panel2D = new Panel2D(this, userBounds, getPanel2DXMargin(), getPanel2DYMargin());
		// try {
		// panel2D.setSettings ((Panel2DSettings) settings.panel2DSettings.clone
		// ());
		// } catch (Exception e) {
		// // can fail in subclass using their own settings (SVBidasoa)
		// }

		createUI();
	}

	/**
	 * Inits the userBounds: the rectangle in which the drawing will be drawn.
	 */
	protected Rectangle.Double createUserBounds() {
		Plot plot = scene.getPlot();
		userBounds = (Rectangle.Double) plot.getShape().getBounds2D();

		return userBounds;

	}

	// /**
	// * Define what objects are candidate for selection by OVSelector Can be
	// redefined in
	// subclasses
	// * to select other objects
	// */
	// protected void createOVSelector (GScene stand) {
	// try {
	// // Default : trees and cells if any
	// Collection candidateObjects = new ArrayList (((TreeList) stand).getTrees
	// ());
	// if (stand.hasPlot ()) {
	// candidateObjects.addAll (stand.getPlot ().getCells ());
	// }
	// JScrollPane targetScrollPane = null;
	// GModel modelForVetoes = null;
	// ExtensionManager extMan = CapsisExtensionManager.getInstance ();
	// ovSelector = new OVSelector (extMan, this, candidateObjects,
	// targetScrollPane, false, false,
	// modelForVetoes);
	//
	// } catch (Exception e) {
	// Log.println (Log.ERROR, "SVPolygons.createOVSelector ()",
	// "Exception during OVSelector construction, wrote this error and passed",
	// e);
	// }
	// }

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) {
				return false;
			}
			GModel m = (GModel) referent;
			GScene s = ((Step) m.getProject().getRoot()).getScene();

			Plot p = s.getPlot();
			if (!(p instanceof PolygonalPlot))
				return false;

			PolygonalPlot pp = (PolygonalPlot) p;
			if (pp.getShape() == null)
				return false;

			return true;

		} catch (Exception e) {
			Log.println(Log.ERROR, "SVPolygons.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}

	@Override
	public String getName() {
		return Translator.swap("SVPolygons.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("SVPolygons.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	/**
	 * Method to draw a GCell within this viewer. Only rectangle r is visible
	 * (user coordinates) -> do not draw if outside.
	 */
	public void drawCell(Graphics2D g2, Cell cell, Rectangle.Double r) {

		Shape sh = cell.getShape();
		Rectangle2D bBox = sh.getBounds2D();
		if (r.intersects(bBox)) {

			// fc-26.1.2012 added the rgb option for colored cells
			boolean indsideWasFilled = false;
			if (cell instanceof RGB) {
				try {
					RGB rgbCell = (RGB) cell;
					int[] rgb = rgbCell.getRGB();
					Color c = new Color(rgb[0], rgb[1], rgb[2]);
					g2.setColor(c);
					g2.fill(sh);
					indsideWasFilled = true;
				} catch (Exception e) { // if error, ignore (rgb may be null,
										// the generic viewer tries what it can)
				}
			}

			if (!indsideWasFilled){
				// fc-11.4.2019 always fill the cell inside
				g2.setColor(new Color(143, 216, 246)); // A kind of blue
				g2.fill(sh);
			}

			g2.setColor(Color.BLACK); // cell outline
			g2.draw(sh);

		}
	}

	// // Cell selection : change color ("red")
	// // Should not be redefined
	// // fc - 28.5.2003
	// //
	// protected void selectCellIfNeeded (Graphics2D g2, Cell cell,
	// Rectangle.Double r) {
	// if (effectiveSelection == null || !effectiveSelection.contains (cell)) {
	// return; }
	// Shape sh = cell.getShape ();
	// Rectangle2D bBox = sh.getBounds2D ();
	//
	// // Little red square within the selected cells
	// double x = bBox.getCenterX () - bBox.getWidth () / 8;
	// double y = bBox.getCenterY () - bBox.getWidth () / 8;
	// double w = bBox.getWidth () / 4;
	// double h = bBox.getWidth () / 4;
	// sh = new Rectangle2D.Double (x, y, w, h);
	//
	// if (r.intersects (bBox)) {
	// Color memo = g2.getColor ();
	// g2.setColor (getCellSelectionColor ());
	// g2.fill (sh);
	// g2.setColor (memo);
	// }
	// }

	// // Draw a selection mark arround target point
	// //
	// protected void drawSelectionMark (Graphics2D g2, Panel2D panel2D, double
	// x, double y) {
	// double w = panel2D.getUserWidth (2); // 2 pixels
	// double h = panel2D.getUserHeight (2); // 2 pixels
	//
	// double wDec = panel2D.getUserWidth (5);
	// double hDec = panel2D.getUserHeight (5);
	//
	// double xLeft = x - wDec;
	// double xRight = x + wDec;
	// double yTop = y + hDec;
	// double yBottom = y - hDec;
	//
	// Color memo = g2.getColor ();
	// g2.setColor (settings.selectionColor);
	//
	// g2.draw (new Line2D.Double (xLeft, yTop, xLeft, yTop - h));
	// g2.draw (new Line2D.Double (xLeft, yTop, xLeft + w, yTop));
	// g2.draw (new Line2D.Double (xLeft, yTop - h, xLeft + w, yTop));
	//
	// g2.draw (new Line2D.Double (xRight, yTop, xRight - w, yTop));
	// g2.draw (new Line2D.Double (xRight, yTop, xRight, yTop - h));
	// g2.draw (new Line2D.Double (xRight - w, yTop, xRight, yTop - h));
	//
	// g2.draw (new Line2D.Double (xRight, yBottom, xRight, yBottom + h));
	// g2.draw (new Line2D.Double (xRight, yBottom, xRight - w, yBottom));
	// g2.draw (new Line2D.Double (xRight, yBottom + h, xRight - w, yBottom));
	//
	// g2.draw (new Line2D.Double (xLeft, yBottom, xLeft + w, yBottom));
	// g2.draw (new Line2D.Double (xLeft, yBottom, xLeft, yBottom + h));
	// g2.draw (new Line2D.Double (xLeft + w, yBottom, xLeft, yBottom + h));
	//
	// g2.setColor (memo);
	// }

	/**
	 * From Drawer interface. This method draws in the Panel2D each time the
	 * latter must be repainted. The given Rectangle is the sub-part (zoom) of
	 * the stand to draw in user coordinates (i.e. meters...). It can be used in
	 * preprocesses to avoid drawing invisible trees or cells.
	 * 
	 * <PRE>
	 * 1. draw the cells
	 * 2. restrict the tree set to a given grouper if needed
	 * 3. draw the trees, sometimes with label
	 * </PRE>
	 * 
	 * The drawCell () and drawTree () methods may be redefined in subclasses to
	 * draw differently.
	 */
	public void draw(Graphics g, Rectangle.Double r) {

		// System.out.println("SVPolygons.draw ()...");

		Graphics2D g2 = (Graphics2D) g;

		// Optionaly draw before
		drawBefore(g2, r, scene);

		g2.setColor(Color.LIGHT_GRAY);
		g2.draw(userBounds);

		shapeMap.clear();

		// Deal with cells
		Collection candidateTrees = new ArrayList();
		if (scene.hasPlot() && scene.getPlot() instanceof PlotOfCells) {
			PlotOfCells plotc = scene.getPlot();
			Collection<Cell> cells = plotc.getCells();

			// 4.2 Draw the cells, memorize their trees
			for (Cell c : cells) {
				drawCell(g2, c, r);
			}
		}

		// Optionally draw more
		drawMore(g2, r, scene);

		// // Ensure title is ok
		// defineTitle ();

	}

	// /**
	// * From Drawer interface. We may receive (from Panel2D) a selection
	// rectangle (in user space
	// * i.e. meters) and return a JPanel containing information about the
	// objects (trees) inside
	// the
	// * rectangle. If no objects are found in the rectangle, return null. fc -
	// 21.11.2003 - uses
	// * OVSelector
	// */
	// public JPanel select (Rectangle.Double r, boolean ctrlIsDown) {
	//
	// // moved this line here - fc - 11.9.2008
	// memoSelectionRectangle = r; // for reselect
	//
	// if (r == null) { // fc - 6.12.2007 - select () may be called early, on
	// // update ()
	// thisIsAReselection = false;
	// // If there was some selection, remove it
	// if (effectiveSelection != null && !effectiveSelection.isEmpty ()) {
	// effectiveSelection.clear ();
	// panel2D.reset (); // Force panel repainting for selection removal
	// panel2D.repaint ();
	// }
	// return null;
	// } // fc - 6.12.2007 - select () may be called early, on update ()
	// // ctrl desactivated - fc - 23.11.2007
	//
	// // 2.1 Manage Ctrl-selection : adding selection to existing selection
	// // ~ if (!ctrlIsDown) {
	// // ~ selectedTrees = new HashSet();
	// // ~ selectedCells = new HashSet();
	// // ~ }
	//
	// // ~ memoSelectionRectangle = r; // for reselect
	//
	// memoSelection = searchInRectangle (r); // fc - 23.11.2007
	//
	// // 2.5b Hook for subclasses : to add things which are in the rectangle,
	// // but not trees nor cells (landscape element, human landmark...)
	// selectMore (r, ctrlIsDown, memoSelection);
	//
	// // fc - 9.9.2008
	// effectiveSelection = ovSelector.select (memoSelection,
	// thisIsAReselection);
	// // rearm selection for next time (detail: in reselection mode, if the
	// // OVDialog is unvisible, it is not set visible)
	// thisIsAReselection = false;
	//
	// // ~ if (currentOV == null) {
	// // ~ openNewOV ();
	// // ~ }
	//
	// // Fire a selection event to tell the selection changed -> the
	// // objectViewer updates
	// // ~ boolean selectionActuallyChanged = !selectCalledByReselect;
	// // ~ SelectionEvent evt = new SelectionEvent (this,
	// // selectionActuallyChanged);
	// // ~ fireSelectionEvent (evt);
	//
	// // 2.7 Selected trees / cells will be enlighted
	// // effectiveListenerSelection is what the ovchooser actually shows
	// // (maybe it can not show everything)
	// // ~ effectiveSelection = evt.getListenerEffectiveSelection ();
	//
	// // Force panel repainting for effective selection enlighting
	// panel2D.reset ();
	// panel2D.repaint ();
	//
	// // ~ selectCalledByReselect = false; // unset this boolean, will be set
	// // if reselect () is called
	// // ~ return currentOV; // never null (see OVChooser)
	// return null; // OVSelector framework: the OVSelector takes care of
	// // showing the ov, return null to panel2D
	// }

	// // When this viewer updates (ex: changes StepButtons), see if reselection
	// // must
	// // be triggered towards OVSelector
	// public void reselect () { // fc - 23.11.2007
	//
	// // System.out.println ("*** SVPolygons.reselect ()...");
	//
	// boolean ctrlIsDown = false;
	// thisIsAReselection = true;
	// select (memoSelectionRectangle, ctrlIsDown);
	// }

	// // If ovDialog is closed by user, remove any selection marks
	// public void cancelSelection () {
	// select (null, false);
	// }

	// /**
	// * This methods returns the object selected by the given rectangle. Called
	// by select
	// (Rectangle,
	// * boolean) in SVPolygons superclass.
	// */
	// protected Collection<Object> searchInRectangle (Rectangle.Double r) {
	// // fc - 7.12.2007
	// // this search mehod can be used without redefinition in sub classes
	// // in case of redefinition, search well...
	// Collection<Object> inRectangle = new ArrayList<Object> ();
	//
	// if (r == null) return inRectangle; // null rectangle: nothing found
	//
	// // 1. If Status is set, restrict stand to active Status
	// TreeList gtcstand = (TreeList) stand;
	// Collection<? extends Tree> treesInStatus = gtcstand.getTrees ();
	// if (statusSelection != null) {
	// treesInStatus = gtcstand.getTrees (statusSelection);
	// }
	//
	// // 2. If grouper is set, restrict stand to given grouper
	// Collection<? extends Tree> treesInGroup = treesInStatus; // fc -
	// // 5.4.2004
	// if (settings.grouperMode) {
	// String name = settings.grouperName;
	// GrouperManager gm = GrouperManager.getInstance ();
	// Grouper gr = gm.getGrouper (name); // return null if not found
	// treesInGroup = gr.apply (treesInGroup, name.toLowerCase ().startsWith
	// ("not "));
	// }
	//
	// // 2.2 What trees are in user selection rectangle ?
	// for (Iterator i = treesInGroup.iterator (); i.hasNext ();) {
	// // fc - 10.4.2008
	// Tree t = (Tree) i.next ();
	// Spatialized s = (Spatialized) t;
	//
	// if (t.isMarked ()) continue; // fc - 5.1.2004 - dead tree
	//
	// if (t instanceof Numberable && ((Numberable) t).getNumber () <= 0)
	// continue; // fc -
	// // 18.11.2004
	//
	// Point.Double p = new Point.Double (s.getX (), s.getY ());
	// if (r.contains (p)) {
	// // Check if the tree location is within the selection rectangle
	// inRectangle.add (t);
	//
	// } else {
	//
	// // See the various shapes drawn for the tree by drawTree
	// // fc-1.2.2012
	// if (shapeMap != null) {
	// Set<Shape> shapes = shapeMap.get (t);
	// if (shapes != null) {
	// for (Shape sh : shapes) {
	// if (sh.intersects (r)) {
	// inRectangle.add (t);
	// break;
	// }
	// }
	// }
	// }
	//
	// }
	// }
	//
	// // 2.3 What cells are in user selection rectangle ?
	// if (stand.hasPlot ()) {
	// for (Cell c : (Collection<Cell>) stand.getPlot ().getCells ()) {
	//
	// Shape cellShape = c.getShape ();
	// if (cellShape.intersects (r)) {
	// inRectangle.add (c);
	// }
	// }
	//
	// }
	//
	// return inRectangle;
	// }

	/**
	 * Subclasses can draw things before the cells and trees by redefining this
	 * method.
	 */
	protected void drawBefore(Graphics2D g2, Rectangle.Double r, GScene s) {
	}

	/**
	 * Subclasses can draw things after the cells and trees by redefining this
	 * method.
	 */
	protected void drawMore(Graphics2D g2, Rectangle.Double r, GScene s) {
	}

	// /**
	// * Subclasses can select things other then trees and cells : add them to
	// the currentSelection
	// * (which was prepared by select () : already possibly contains trees and
	// cells). Note: do not
	// * alter currentSelection, just add things to it.
	// */
	// protected void selectMore (Rectangle.Double r, boolean ctrlIsDown,
	// Collection
	// currentSelection) {}

	public Color getSelectionColor() {
		return Color.RED;
	}

	/**
	 * Used for the settings and filtering buttons.
	 */
	public void actionPerformed(ActionEvent evt) {

		// if (evt.getSource ().equals (helpButton)) {
		// Helper.helpFor (this);
		// }

	}

	/**
	 * Update the viewer (for example, when changing step).
	 */
	public void update() {
		super.update();

		scene = step.getScene();
		// defineTitle ();
		scrollPane.setViewportView(panel2D); // panel2D might change size
		panel2D.reset(); // to disable buffered image and force redrawing
		panel2D.repaint();

		// // Reselect must be done after repaint
		// SwingUtilities.invokeLater (new Runnable () {
		//
		// public void run () {
		// reselect ();
		// }
		// });

	}

	/**
	 * Refresh the GUI with another Step.
	 */
	public void update(StepButton sb) {
		super.update(sb);
		update();
	}

	// // Viewer title definition : reference to current step and considered
	// group.
	// //
	// protected void defineTitle () {
	// try {
	// StringBuffer t = new StringBuffer ();
	//
	// // 1. Step reference
	// t.append (step.getProject ().getName ());
	// t.append (".");
	// t.append (step.getName ());
	//
	// // 1.1 Status : ask name considering stand under current step
	// // and current statusChooser selection (we may have changed
	// // steps since selection). Ex: (cut+windfall).
	// if (stand instanceof TreeList) { // fc - 16.9.2004
	//
	// String statusName = StatusChooser.getName (statusSelection);
	// if (statusName != null && !statusName.equals ("")) {
	// t.append (statusName); // ex: (alive+cut)
	// }
	// }
	//
	// // 2. Group name if exists
	// // fc - 12.9.2008 - added try / catch - bidasoa viewer has a better
	// // title
	// try {
	// if (settings.grouperMode && settings.grouperName != null) {
	// // fc - 16.9.2004 - if group has been proposed, it must be
	// // compatible
	// // ~ Grouper g = GrouperManager.getInstance ()
	// // ~ .getGrouper (settings.grouperName);
	// // ~ if (g.matchWith (stand)) { // <<<<<<<<<<<<<<<<<< nuts,
	// // needs Collection
	// t.append (" / ");
	// t.append (settings.grouperName);
	// // ~ }
	// }
	// } catch (Exception e) {} // does not matter
	//
	// // 3. Viewer name
	// t.append (" - ");
	// t.append (ExtensionManager.getName (this));
	// customTitle = t.toString ();
	//
	// } catch (Exception e) {
	// Log.println (Log.WARNING, "SVPolygons.defineTitle ()",
	// "can not build fine viewer title, returned simply getName ()", e);
	// customTitle = ExtensionManager.getName (this);
	// }
	// setTitle (customTitle);
	// }

	/**
	 * Create the GUI.
	 */
	protected void createUI() {

		getContentPane().setLayout(new BorderLayout()); // mainBox in the
														// internalFrame
		scrollPane = new JScrollPane(panel2D);

		scrollPane.getViewport().putClientProperty("EnableWindowBlit", Boolean.TRUE); // faster

		getContentPane().add(scrollPane, BorderLayout.CENTER);

	}

	public void dispose() {
		super.dispose();
		// ovSelector.dispose ();
		panel2D.dispose();
		scene = null;
		panel2D = null;
		scrollPane = null;
	}

	public int getPanel2DXMargin() {
		return Panel2D.X_MARGIN_IN_PIXELS;
	}

	public int getPanel2DYMargin() {
		return Panel2D.Y_MARGIN_IN_PIXELS;
	}

	@Override
	public JPanel select(Double r, boolean more) {
		return null;
	}

	// //
	// ---------------------------------------------------------------------------
	// // MainPanel
	// //
	// ---------------------------------------------------------------------------
	//
	// /**
	// * Main option panel. Secondary option panel may be described in
	// subclasses.
	// */
	// static public class MainPanel extends JPanel implements ActionListener,
	// Controlable {
	//
	// private JTabbedPane tabs;
	//
	// private GrouperChooser grouperChooser;
	// private StatusChooser statusChooser;
	//
	// private JCheckBox ckShowLabels;
	// private JTextField maxLabelNumber; // fc - 18.12.2003
	// private JCheckBox ckShowDiameters;
	// private JCheckBox ckShowTransparency;
	// private ColoredButton labelColorButton;
	// private ColoredButton treeColorButton;
	// private ColoredButton cellColorButton;
	// private ColoredButton selectionColorButton;
	//
	// /*
	// * private JRadioButton sideViewMode; private JRadioButton inspectorMode;
	// private
	// * ButtonGroup selectMode;
	// */
	//
	// // private ConfigurationPanel p2DConfigPanel; // REMOVED fc-3.9.2012
	//
	// private JButton exportButton;
	// private JButton exportDrawer;
	// private Container embedder; // fc - 23.7.2004
	// private SVPolygons sv;
	// private Rectangle.Double userBounds;
	//
	// public MainPanel (SVPolygons v, SVPolygonsSettings settings, GScene
	// stand, Panel2D panel2D,
	// JPanel optionPanel,
	// Rectangle.Double ub) {
	// super ();
	// sv = v;
	// userBounds = ub;
	// setLayout (new BorderLayout ());
	// // ~ Border etched = BorderFactory.createEtchedBorder ();
	//
	// ColumnPanel part1 = new ColumnPanel (0, 0);
	//
	// // 1. Common
	// ColumnPanel p1 = new ColumnPanel (Translator.swap
	// ("SVPolygons.MainPanel.common"));
	// // ~ Border b1 = BorderFactory.createTitledBorder (etched,
	// // Translator.swap ("SVPolygons.MainPanel.common"));
	// // ~ p1.setBorder (b1);
	//
	// ckShowLabels = new JCheckBox (Translator.swap
	// ("SVPolygons.MainPanel.showLabels"),
	// settings.showLabels);
	// maxLabelNumber = new JTextField (5);
	// maxLabelNumber.setText ("" + settings.maxLabelNumber);
	// ckShowDiameters = new JCheckBox (Translator.swap
	// ("SVPolygons.MainPanel.showDiameters"),
	// settings.showDiameters);
	// ckShowTransparency = new JCheckBox (Translator.swap
	// ("SVPolygons.MainPanel.showTransparency"),
	// settings.showTransparency);
	//
	// LinePanel l6 = new LinePanel ();
	// LinePanel l7 = new LinePanel ();
	// LinePanel l70 = new LinePanel ();
	// LinePanel l71 = new LinePanel ();
	// LinePanel l8 = new LinePanel ();
	// LinePanel l9 = new LinePanel ();
	// l6.add (ckShowLabels);
	// // ~ l6.add (new JLabel ("- "+Translator.swap
	// // ("SVPolygons.MainPanel.maxLabelNumber")+" : "));
	// l6.add (maxLabelNumber);
	// l6.addGlue ();
	// l7.add (ckShowDiameters);
	// l7.addGlue ();
	// l70.add (ckShowTransparency);
	// l70.addGlue ();
	//
	// // fc - 22.4.2004 - Status
	//
	// try {
	// TreeList gtcstand = (TreeList) stand;
	// statusChooser = new StatusChooser (gtcstand.getStatusKeys (),
	// v.getStatusSelection ());
	// l71.add (statusChooser);
	// } catch (Exception e) {} // sometimes not a TreeList...
	//
	// // NEW... groups
	// boolean checked = settings.grouperMode;
	// boolean not = settings.grouperNot;
	// String selectedGroupName = settings.grouperName;
	// grouperChooser = new GrouperChooser (stand, TreeList.GROUP_ALIVE_TREE,
	// selectedGroupName, not, true,
	// checked);
	// l8.add (grouperChooser);
	//
	// exportButton = new JButton (Translator.swap ("Shared.export"));
	// exportButton.addActionListener (this);
	// l9.add (exportButton);
	//
	// exportDrawer = new JButton (Translator.swap ("ExportDrawer"));
	// exportDrawer.addActionListener (this);
	// l9.add (exportDrawer);
	// l9.addGlue ();
	//
	// p1.add (l6);
	// p1.add (l7);
	// p1.add (l70);
	// p1.add (l71);
	// p1.add (l8);
	// p1.add (l9);
	// p1.addStrut0 (); // fc - 25.4.2007
	// part1.add (p1);
	//
	// // 3. Colors
	// ColumnPanel p2 = new ColumnPanel (Translator.swap
	// ("SVPolygons.MainPanel.colors"));
	// // ~ Border b2 = BorderFactory.createTitledBorder (etched,
	// // Translator.swap ("SVPolygons.MainPanel.colors"));
	// // ~ p2.setBorder (b2);
	//
	// // ~ labelColorButton = new JButton (); // empty coloured buttons
	// // ~ treeColorButton = new JButton ();
	// // ~ cellColorButton = new JButton ();
	// // ~ selectionColorButton = new JButton ();
	// // ~ labelColorButton.setBackground (getLabelColor ());
	// // ~ treeColorButton.setBackground (getTreeColor ());
	// // ~ cellColorButton.setBackground (getCellColor ());
	// // ~ selectionColorButton.setBackground (getSelectionColor ());
	//
	// labelColorButton = new ColoredButton (v.getLabelColor ());
	// treeColorButton = new ColoredButton (v.getTreeColor ());
	// cellColorButton = new ColoredButton (v.getCellColor ());
	// selectionColorButton = new ColoredButton (v.getSelectionColor ());
	//
	// labelColorButton.addActionListener (this);
	// treeColorButton.addActionListener (this);
	// cellColorButton.addActionListener (this);
	// selectionColorButton.addActionListener (this);
	//
	// LinePanel l1 = new LinePanel ();
	// LinePanel l2 = new LinePanel ();
	// LinePanel l3 = new LinePanel ();
	// LinePanel l4 = new LinePanel ();
	// l1.add (new JWidthLabel (Translator.swap
	// ("SVPolygons.MainPanel.labelColor") + " :", 170));
	// l1.add (labelColorButton);
	// l1.addGlue ();
	// l2.add (new JWidthLabel (Translator.swap
	// ("SVPolygons.MainPanel.treeColor") + " :", 170));
	// l2.add (treeColorButton);
	// l2.addGlue ();
	// l3.add (new JWidthLabel (Translator.swap
	// ("SVPolygons.MainPanel.cellColor") + " :", 170));
	// l3.add (cellColorButton);
	// l3.addGlue ();
	// l4.add (new JWidthLabel (Translator.swap
	// ("SVPolygons.MainPanel.selectionColor") + " :",
	// 170));
	// l4.add (selectionColorButton);
	// l4.addGlue ();
	//
	// p2.add (l1);
	// p2.add (l2);
	// p2.add (l3);
	// p2.add (l4);
	// p2.addStrut0 (); // fc - 25.4.2007
	// part1.add (p2);
	// // fc - 25.4.2007
	// part1.addGlue ();
	//
	// // 4. NEW - panel2D configuration (fc - 30.4.2002)
	// // p2DConfigPanel = panel2D.getConfigurationPanel(null); // REMOVED
	// fc-3.9.2012
	// // // fc - 25.4.2007
	// // ColumnPanel p3 = new ColumnPanel(panel2D.getConfigurationLabel());
	// // p3.add(p2DConfigPanel);
	// // p3.addStrut0();
	// // // fc - 25.4.2007
	//
	// // fc - 25.4.2007
	// LinePanel main = new LinePanel ();
	// main.add (new NorthPanel (part1));
	// // main.add(new NorthPanel(p3)); // REMOVED fc-3.9.2012
	// main.addStrut0 ();
	//
	// // 5. Correct layout
	// JPanel worker = new JPanel (new BorderLayout ());
	// worker.add (main, BorderLayout.NORTH);
	//
	// if (optionPanel.getComponentCount () != 0) {
	// JPanel worker2 = new JPanel (new BorderLayout ());
	// worker2.add (optionPanel, BorderLayout.NORTH);
	//
	// tabs = new JTabbedPane ();
	// tabs.setTabLayoutPolicy (JTabbedPane.SCROLL_TAB_LAYOUT); // fc -
	// // 2.4.2003
	//
	// tabs.addTab (Translator.swap ("SVPolygons.MainPanel.options"), null,
	// worker2);
	// tabs.addTab (Translator.swap ("SVPolygons.MainPanel.general"), null,
	// worker);
	// add (tabs, BorderLayout.CENTER);
	// } else {
	// // ~ add (part1, BorderLayout.NORTH);
	// add (main, BorderLayout.NORTH);
	// }
	//
	// }
	//
	// public boolean isControlSuccessful () {
	// // Panel2D config panel must be ok
	// // if (!p2DConfigPanel.checksAreOk()) { // REMOVED fc-3.9.2012
	// // return false;
	// // }
	//
	// // Max label number must be an integer
	// try {
	// new Integer (maxLabelNumber.getText ().trim ()).intValue ();
	// } catch (Exception e) {
	// MessageDialog.print (this, Translator.swap
	// ("Shared.maxLabelNumberMustBeInteger"));
	// return false;
	// }
	//
	// // fc - 22.4.2004
	// try {
	// if (!statusChooser.isChooserValid ()) {
	// MessageDialog.print (this, Translator.swap
	// ("Shared.chooseAtLeastOneStatus"));
	// return false;
	// }
	// sv.setStatusSelection (statusChooser.getSelection ());
	// } catch (Exception e) {} // sometimes not a treelist
	//
	// return true;
	// }
	//
	// // public ConfigurationPanel getPanel2DConfigPanel() { // REMOVED
	// fc-3.9.2012
	// // return p2DConfigPanel;
	// // }
	//
	// public void actionPerformed (ActionEvent evt) {
	//
	// if (evt.getSource ().equals (labelColorButton)) {
	// Color newColor = JColorChooser
	// .showDialog (this, Translator.swap ("SVPolygons.MainPanel.chooseAColor"),
	// labelColorButton
	// .getBackground ());
	// if (newColor != null) {
	// labelColorButton.colorize (newColor);
	// }
	//
	// } else if (evt.getSource ().equals (treeColorButton)) {
	// Color newColor = JColorChooser
	// .showDialog (this, Translator.swap ("SVPolygons.MainPanel.chooseAColor"),
	// treeColorButton
	// .getBackground ());
	// if (newColor != null) {
	// treeColorButton.colorize (newColor);
	// }
	//
	// } else if (evt.getSource ().equals (cellColorButton)) {
	// Color newColor = JColorChooser
	// .showDialog (this, Translator.swap ("SVPolygons.MainPanel.chooseAColor"),
	// cellColorButton
	// .getBackground ());
	// if (newColor != null) {
	// cellColorButton.colorize (newColor);
	// }
	//
	// } else if (evt.getSource ().equals (selectionColorButton)) {
	// Color newColor = JColorChooser
	// .showDialog (this, Translator.swap ("SVPolygons.MainPanel.chooseAColor"),
	// selectionColorButton
	// .getBackground ());
	// if (newColor != null) {
	// selectionColorButton.colorize (newColor);
	// }
	//
	// } else if (evt.getSource ().equals (exportButton)) {
	// if (embedder instanceof JFrame) {
	// new ExportComponent (sv, (JFrame) embedder);
	// } else {
	// new ExportComponent (sv, (JDialog) embedder);
	// }
	//
	// } else if (evt.getSource ().equals (exportDrawer)) { // fc - 12.1.2005
	// if (embedder instanceof JFrame) {
	// new ExportDrawer (sv, userBounds, (JFrame) embedder);
	// } else {
	// new ExportDrawer (sv, userBounds, (JDialog) embedder);
	// }
	// }
	// }
	//
	// public void dispose () {
	// try {
	// tabs.removeAll ();
	// } catch (Exception e) {}
	// }
	//
	// public GrouperChooser getGrouperChooser () {
	// return grouperChooser;
	// }
	//
	// public StatusChooser getStatusChooser () {
	// return statusChooser;
	// }
	//
	// public JCheckBox getCkShowLabels () {
	// return ckShowLabels;
	// }
	//
	// public JCheckBox getCkShowDiameters () {
	// return ckShowDiameters;
	// }
	//
	// public JCheckBox getCkShowTransparency () {
	// return ckShowTransparency;
	// }
	//
	// public Color getNewLabelColor () {
	// // ~ return new Color (labelColorButton.getBackground ().getRGB ());
	// return labelColorButton.getColor ();
	// }
	//
	// public Color getNewTreeColor () {
	// // ~ return new Color (treeColorButton.getBackground ().getRGB ());
	// return treeColorButton.getColor ();
	// }
	//
	// public Color getNewCellColor () {
	// // ~ return new Color (cellColorButton.getBackground ().getRGB ());
	// return cellColorButton.getColor ();
	// }
	//
	// public Color getNewSelectionColor () {
	// // ~ return new Color (selectionColorButton.getBackground ().getRGB
	// // ());
	// return selectionColorButton.getColor ();
	// }
	//
	// public int getMaxLabelNumber () { // format was checked in
	// // isControlSuccessful ()
	// return new Integer (maxLabelNumber.getText ().trim ()).intValue ();
	// }
	//
	// public String getTitle () {
	// // return Translator.swap (AmapTools.getClassSimpleName
	// // (this.getClass ().getName ()));
	// return Translator.swap (this.getClass ().getSimpleName ());
	// }
	//
	// }

}
