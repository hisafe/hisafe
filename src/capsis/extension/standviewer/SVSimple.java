/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2001-2003 Francois de Coligny
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
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;

import capsis.app.CapsisExtensionManager;
import capsis.commongui.projectmanager.StepButton;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.defaulttype.plotofcells.Cell;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.defaulttype.plotofcells.TreeListCell;
import capsis.extension.AbstractStandViewer;
import capsis.gui.DialogWithOkCancel;
import capsis.gui.GrouperChooser;
import capsis.gui.StatusChooser;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Plot;
import capsis.kernel.Step;
import capsis.util.Controlable;
import capsis.util.Drawer;
import capsis.util.ExportComponent;
import capsis.util.ExportDrawer;
import capsis.util.Panel2D;
import capsis.util.Panel2DSettings;
import capsis.util.Pilotable;
import capsis.util.TreeHeightComparator;
import capsis.util.group.Grouper;
import capsis.util.group.GrouperManager;
import jeeb.lib.defaulttype.SimpleCrownDescription;
import jeeb.lib.defaulttype.TreeWithCrownProfile;
import jeeb.lib.util.ColoredButton;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.OVSelector;
import jeeb.lib.util.OVSelectorSource;
import jeeb.lib.util.RGB;
import jeeb.lib.util.SetMap;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Translator;
import jeeb.lib.util.annotation.Param;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import jeeb.lib.util.gui.NorthPanel;

/**
 * SVSimple is a cartography simple viewer for trees with coordinates. It draws
 * the trees within the cells.
 *
 * @author F. de Coligny
 */
public class SVSimple extends AbstractStandViewer implements ActionListener, Drawer, Pilotable, OVSelectorSource {

	static {
		Translator.addBundle("capsis.extension.standviewer.SVSimple");
	}

	// nb-07.08.2018
	// static public String AUTHOR = "F. de Coligny";
	// static public String VERSION = "1.7";

	public final static int BUTTON_SIZE = 23;

	public final static int TREE_MODE = 1;
	public final static int CELL_MODE = 2;
	public final static int MANY_TREES_MODE = 3;
	public final static int MANY_CELLS_MODE = 4;

	protected JComboBox selectionCombo;
	protected Color cellSelectionColor; // = selection color with some
										// transparency

	// fc - 9.9.2008 - replacing OVChooser by the new OVSelector framework
	// ~ protected OVChooser ovChooser; // DEL
	protected OVSelector ovSelector;
	protected boolean thisIsAReselection; // fc - 11.9.2008

	protected Panel2D panel2D;
	protected Rectangle.Double userBounds; // fc - 12.1.2005

	protected int labelCounter; // for trees label drawing strategy
	protected int labelFrequency;

	@Param
	protected SVSimpleSettings settings;

	@Param
	protected String[] statusSelection; // not in settings (available keys
										// change with steps)

	protected GScene stand;

	// protected SetMap<Tree, Shape> shapeMap;
	protected SetMap<Object, Shape> shapeMap; // fc-10.2.2012 trying to draw
												// avatars with S. de Miguel

	protected JButton settingsButton;
	protected JButton helpButton;

	protected JPanel mainPanel;
	protected JPanel optionPanel;
	protected JScrollPane scrollPane;

	protected JScrollPane legend; // use setLegend (JComponent) to add a legend

	protected double visibleThreshold; // things under this user size should not
										// be drawn (too small on screen)

	// fc-23.12.2020 Reorganized title management
//	protected String customTitle;

	protected JTextField fMagnifyFactor; // fc - 23.12.2003
	protected int magnifyFactor;

	protected JSplitPane splitPane; // fc - 7.4.2006

	// ~ protected Collection<SelectionListener> selectionListeners; // fc -
	// 23.11.2007
	protected Rectangle.Double memoSelectionRectangle; // fc - 23.11.2007
	protected Collection<Object> memoSelection; // fc - 23.11.2007
	protected Collection<Object> effectiveSelection; // fc - 7.12.2007
	// ~ protected JPanel currentOV; // DEL
	// this boolean is true if select () is called by reselect (): when OV
	// changes in OVChoer,
	// when user changes steps, i.e. when the 'selection" does not change
	// this boolean is false if select () is called by the panel2D, meaning that
	// the user
	// actually changed the selection by hand (needed by OVs to reset zooms,
	// etc.)
	protected boolean selectCalledByReselect; // fc - 13.12.2007

	/**
	 * Can be redefined in superclasses to create things early in init ()
	 */
	protected void beginningOfInit() {
	} // fc-15.3.2017

	@Override
	public void init(GModel model, Step s, StepButton but) throws Exception {

		try {

			super.init(model, s, but);

			beginningOfInit();

			// In drawTree, this setMap will be updated to help selection
			// response
			// shapeMap = new SetMap<Tree, Shape>();
			shapeMap = new SetMap<Object, Shape>(); // fc-10.2.2012 trying to
													// draw
													// avatars with S. de Miguel

			// fc - 7.12.2007
			memoSelection = new HashSet<Object>(); // fc - 12.12.2007 -
													// ArrayList ->
													// HashSet, faster in
													// contains
													// ()
			effectiveSelection = new HashSet<Object>(); // fc - 12.12.2007 -
														// ArrayList -> HashSet,
														// faster in contains ()
			selectCalledByReselect = false;

			stand = step.getScene(); // step is defined from sb by superclass

			retrieveSettings();

			ExtensionManager.applySettings(this.getClass().getName(), this);

			// ~ createOVChooser (stand); // fc - 11.2.2008
			createOVSelector(stand); // fc - 9.9.2008

			// fc - 10.12.2007
			// ~ initOVChooser (); // DEP - fc - 11.2.2008

			if (isMagnifyVetoed()) {
				magnifyFactor = 1;
			} else {
				try {
					String t = Settings.getProperty("extension.svsimple.magnify.factor", (String) null);
					magnifyFactor = new Integer(t).intValue();
				} catch (Exception e) {
					magnifyFactor = 1; // default value for magnify factor
				}
			}

			// fc + PhD - 22.10.2010
			userBounds = createUserBounds();

			// double x, y, width, height;
			// if (stand.hasPlot ()) {
			// Plot plot = stand.getPlot ();
			// userBounds = (Rectangle.Double) plot.getShape ().getBounds2D ();
			//
			// } else {
			// x = stand.getOrigin ().x;
			// y = stand.getOrigin ().y;
			// width = stand.getXSize ();
			// height = stand.getYSize ();
			// userBounds = new Rectangle.Double (x, y, width, height);
			// }

			panel2D = new Panel2D(this, userBounds, getPanel2DXMargin(), getPanel2DYMargin());
			try {
				panel2D.setSettings((Panel2DSettings) settings.panel2DSettings.clone());
			} catch (Exception e) {
				// can fail in subclass using their own settings (SVBidasoa)
			}

			optionPanel = new JPanel();

			//System.out.println("SVSimple > init () 7. settings: " + settings);

			createUI();
			// ~ forwardSelectionToCurrentOV (); // fc - 11.2.2008

			//System.out.println("SVSimple > init () 8. settings: " + settings);

		} catch (Exception e) {
			Log.println(Log.ERROR, "SVSimple.init ()", "Exception", e);

		}

	}

	/**
	 * Inits the userBounds: the rectangle in which the drawing will be drawn. Can
	 * be overriden.
	 */
	protected Rectangle.Double createUserBounds() {

		if (stand.hasPlot()) {
			Plot plot = stand.getPlot();
			userBounds = (Rectangle.Double) plot.getShape().getBounds2D();

		} else {
			double x, y, width, height;

			x = stand.getOrigin().x;
			y = stand.getOrigin().y;
			width = stand.getXSize();
			height = stand.getYSize();
			userBounds = new Rectangle.Double(x, y, width, height);
		}

		return userBounds;

	}

	// fc-5.2.2018
	public Rectangle.Double getUserBounds() {
		return userBounds;
	}

	/**
	 * Define what objects are candidate for selection by OVSelector Can be
	 * redefined in subclasses to select other objects
	 */
	protected void createOVSelector(GScene stand) {
		try {
			// Default : trees and cells if any
			Collection candidateObjects = new ArrayList(((TreeList) stand).getTrees());
			if (stand.hasPlot() && stand.getPlot() instanceof PlotOfCells) {
				PlotOfCells plotc = stand.getPlot(); // fc-26.10.2017
				candidateObjects.addAll(plotc.getCells());
			}
			JScrollPane targetScrollPane = null;
			GModel modelForVetoes = null;
			ExtensionManager extMan = CapsisExtensionManager.getInstance();
			ovSelector = new OVSelector(extMan, this, candidateObjects, targetScrollPane, false, false, modelForVetoes);

			// ~ // This object viewer is SelectionListener, it has registered
			// in its constructor
			// ~ // to listen to SelectionEvents from HERE (see
			// fireSelectionEvent ())
			// ~ objectViewer = ovChooser.getOV (null); // this panel is set
			// unvisible, will appear on first selection - fc 7.12.2007
		} catch (Exception e) {
			Log.println(Log.ERROR, "SVSimple.createOVSelector ()",
					"Exception during OVSelector construction, wrote this error and passed", e);
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks if
	 * the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) {
				return false;
			}
			GModel m = (GModel) referent;
			GScene s = ((Step) m.getProject().getRoot()).getScene();

			if (!(s instanceof TreeList)) {
				return false;
			} // for statusMap - fc - 22.4.2004
			TreeList gtcstand = (TreeList) s;
			if (gtcstand.getTrees().isEmpty()) {
				return true;
			} // fc - 26.10.2001 - bc bare soil
				// problem

			// fc - 10.4.2008 - done it better
			// ~ if (!(gtcstand.getFirstTree () instanceof Spatialized)) {return
			// false;} // fc - 17.2.2003
			Collection reps = Tools.getRepresentatives(gtcstand.getTrees());
			for (Iterator i = reps.iterator(); i.hasNext();) {
				Object o = i.next();
				if (!(o instanceof Spatialized)) {
					return false;
				}
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "SVSimple.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("SVSimple.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("SVSimple.description");
	}

	@Override
	public String getVersion() {
		return "1.7";
	}

	/**
	 * Method to draw a GCell within this viewer. Only rectangle r is visible (user
	 * coordinates) -> do not draw if outside. May be redefined in subclasses.
	 */
	public void drawCell(Graphics2D g2, Cell cell, Rectangle.Double r) {

		Shape sh = cell.getShape();
		Rectangle2D bBox = sh.getBounds2D();
		if (r.intersects(bBox)) {

			// fc-26.1.2012 added the rgb option for colored cells
			if (cell instanceof RGB) {
				try {
					RGB rgbCell = (RGB) cell;
					int[] rgb = rgbCell.getRGB();
					Color c = new Color(rgb[0], rgb[1], rgb[2]);
					g2.setColor(c);
					g2.fill(sh);
				} catch (Exception e) { // if error, ignore (rgb may be null,
										// the generic viewer tries what it can)
				}
			}

			g2.setColor(getCellColor());
			g2.draw(sh);

		}
	}

	// Process a cell and its daughters, memorize the trees in the drawn cells
	// for further painting.
	// fc - 17.2.2003
	//
	protected void processCell(TreeListCell cell, Collection candidateTrees, Graphics2D g2, Rectangle.Double r) {

		// Stop condition : cell not visible
		Rectangle2D bBox = cell.getShape().getBounds2D();
		if (!r.intersects(bBox)) {
			return;
		} // sub cells are not considered

		// Draw cell (may be redefined in subclass)
		drawCell(g2, cell, r);
		selectCellIfNeeded(g2, cell, r); // fc - 28.5.2003 - may change cell
											// color

		// If cell contains trees, memorize them
		if (cell.isTreeLevel() && !cell.isEmpty()) {
			candidateTrees.addAll(cell.getTrees());
		}

		// If daughter cells, process them
		if (cell.isMother()) {
			for (Cell c : cell.getCells()) {
				processCell((TreeListCell) c, candidateTrees, g2, r); // recursive
																		// descent
																		// on
																		// cell
																		// daugthers
			}
		}
	}

	// Cell selection : change color ("red")
	// Should not be redefined
	// fc - 28.5.2003
	//
	protected void selectCellIfNeeded(Graphics2D g2, Cell cell, Rectangle.Double r) {
		if (effectiveSelection == null || !effectiveSelection.contains(cell)) {
			return;
		}
		Shape sh = cell.getShape();
		Rectangle2D bBox = sh.getBounds2D();

		// Little red square within the selected cells
		double x = bBox.getCenterX() - bBox.getWidth() / 8;
		double y = bBox.getCenterY() - bBox.getWidth() / 8;
		double w = bBox.getWidth() / 4;
		double h = bBox.getWidth() / 4;
		sh = new Rectangle2D.Double(x, y, w, h);

		if (r.intersects(bBox)) {
			Color memo = g2.getColor();
			g2.setColor(getCellSelectionColor());
			g2.fill(sh);
			g2.setColor(memo);
		}
	}

	/**
	 * Called before whole trees drawing process. May be redefined in subclasses.
	 */
	public Object[] preProcessTrees(Object[] trees, Rectangle.Double r) {
		// Sort the trees in height order
		Arrays.sort(trees, new TreeHeightComparator(!settings.showTransparency)); // asc
																					// =
																					// !settings.showTransparency
		return trees;
	}

	/**
	 * Method to draw a Spatialized GTree within this viewer. Only rectangle r is
	 * visible (user coordinates) -> do not draw if outside. May be redefined in
	 * subclasses. Tree label is managed in draw () method, deals with max labels
	 * number for performance.
	 */
	public void drawTree(Graphics2D g2, Tree tree, Rectangle.Double r) {

		// System.out.println ("SVSimple drawing tree: "+tree.getId ()+"...");
		// System.out.println (" dbh: "+tree.getDbh ());

		// 1. Marked trees are considered dead by generic tools -> don't draw
		if (tree.isMarked()) {
			return;
		}
		if (tree instanceof Numberable && ((Numberable) tree).getNumber() <= 0) {
			return;
		} // fc -
			// 18.11.2004

		Spatialized s = (Spatialized) tree;

		// 2. Tree location
		double x = s.getX();
		double y = s.getY();
		double width = tree.getDbh() / 100; // in m. (we draw in Graphics in m.)
		int mf = magnifyFactor;

		// 3. A detailled view is requested
		//
		if (settings.showDiameters) {

			// 3.1 In some cases, a tree crown can be drawn
			//
			Color crownColor = Tools.getCrownColor(tree);
			double crownRadius = 0;

			if (tree instanceof SimpleCrownDescription) {
				mf = 1; // when crown is drawn, trunk is not magnified
				SimpleCrownDescription data = (SimpleCrownDescription) tree;
				crownRadius = data.getCrownRadius();
			}
			if (tree instanceof TreeWithCrownProfile) {
				mf = 1; // when crown is drawn, trunk is not magnified
				TreeWithCrownProfile data = (TreeWithCrownProfile) tree;
				crownRadius = data.getCrownRadius();
				crownColor = data.getCrownColor();
			}

			if (settings.showTransparency) {
				int alpha = 50;

				int red = crownColor.getRed();
				int green = crownColor.getGreen();
				int blue = crownColor.getBlue();
				crownColor = new Color(red, green, blue, alpha);
			}

			double d = crownRadius * 2; // crown diameter

			// 3.1.1 Crown diameter is less than 1 pixel : draw 1 pixel
			//
			if (d <= visibleThreshold) {
				if (r.contains(new java.awt.geom.Point2D.Double(x, y))) {
					Rectangle2D.Double p = new Rectangle2D.Double(x, y, visibleThreshold, visibleThreshold); // fc
																												// -
																												// 15.12.2003
																												// (bug
																												// by
																												// PhD)
					g2.setColor(crownColor);
					g2.fill(p);
				}

				// 3.1.2 Bigger than 1 pixel : fill a circle
				//
			} else {
				Shape sh = new Ellipse2D.Double(x - crownRadius, y - crownRadius, d, d);

				shapeMap.addObject(tree, sh);

				Rectangle2D bBox = sh.getBounds2D();

				// System.out.println("SVSimple crownColor: "+crownColor);

				if (r.intersects(bBox)) {
					g2.setColor(crownColor);
					g2.fill(sh);
					g2.setColor(crownColor.darker());
					g2.draw(sh);
				}
			}

		}

		width = width * mf; // in m. (we draw in Graphics in m.)
		width = Math.max(visibleThreshold, width); // fc - 18.11.2004

		// Draw the trunk : always and after the crown : visible
		if (r.contains(new java.awt.geom.Point2D.Double(x, y))) {
			Shape p = new Ellipse2D.Double(x - width / 2, y - width / 2, width, width); // fc
																						// -
																						// 18.11.2004
			g2.setColor(getTreeColor());
			g2.fill(p);
		}

		// System.out.println (" tree: "+tree.getId ()+" was drawn.");
	}

	// Draws a label for the given tree
	// Implements a labels restriction strategy (see Draw (), very long to draw
	// if numerous)
	// fc - 22.12.2003
	//
	protected void drawLabel(Graphics2D g2, String label, float x, float y) {
		if (labelCounter % labelFrequency == 0) {
			labelCounter = 0;
			g2.setColor(getLabelColor());
			g2.drawString(label, x, y);
		}
		labelCounter++;
	}

	// Tree selection : change color ("red")
	// Should not be redefined
	// fc - 28.5.2003
	//
	protected void selectTreeIfNeeded(Graphics2D g2, Tree tree, Rectangle.Double r) {
		// 1. Marked trees are considered dead by generic tools -> ignore
		if (tree.isMarked()) {
			return;
		}
		if (tree instanceof Numberable && ((Numberable) tree).getNumber() <= 0) {
			return;
		} // fc -
			// 18.11.2004

		if (effectiveSelection == null || !effectiveSelection.contains(tree)) {
			return;
		}

		Spatialized s = (Spatialized) tree;

		// 2. Tree location
		double x = s.getX();
		double y = s.getY();
		if (r.contains(new java.awt.geom.Point2D.Double(x, y))) {
			drawSelectionMark(g2, panel2D, x, y);
		}

	}

	// Draw a selection mark arround target point
	//
	protected void drawSelectionMark(Graphics2D g2, Panel2D panel2D, double x, double y) {
		double w = panel2D.getUserWidth(2); // 2 pixels
		double h = panel2D.getUserHeight(2); // 2 pixels

		double wDec = panel2D.getUserWidth(5);
		double hDec = panel2D.getUserHeight(5);

		double xLeft = x - wDec;
		double xRight = x + wDec;
		double yTop = y + hDec;
		double yBottom = y - hDec;

		Color memo = g2.getColor();
		g2.setColor(settings.selectionColor);

		g2.draw(new Line2D.Double(xLeft, yTop, xLeft, yTop - h));
		g2.draw(new Line2D.Double(xLeft, yTop, xLeft + w, yTop));
		g2.draw(new Line2D.Double(xLeft, yTop - h, xLeft + w, yTop));

		g2.draw(new Line2D.Double(xRight, yTop, xRight - w, yTop));
		g2.draw(new Line2D.Double(xRight, yTop, xRight, yTop - h));
		g2.draw(new Line2D.Double(xRight - w, yTop, xRight, yTop - h));

		g2.draw(new Line2D.Double(xRight, yBottom, xRight, yBottom + h));
		g2.draw(new Line2D.Double(xRight, yBottom, xRight - w, yBottom));
		g2.draw(new Line2D.Double(xRight, yBottom + h, xRight - w, yBottom));

		g2.draw(new Line2D.Double(xLeft, yBottom, xLeft + w, yBottom));
		g2.draw(new Line2D.Double(xLeft, yBottom, xLeft, yBottom + h));
		g2.draw(new Line2D.Double(xLeft + w, yBottom, xLeft, yBottom + h));

		g2.setColor(memo);
	}

	/**
	 * A way to force the scale, must be called before draw (), can be used from
	 * scripts, experimental, see maddmodel.myscripts.SimpleScript.
	 */
	public void forceScale(int xPixels, int yPixels, int xSize_pixel, int ySize_pixel) { // fc+PhD-5.2.2018

		// fc-5.4.2018 exploring how to paint viewers in an image from scripts
		// for PhD

		setBounds(0, 0, xSize_pixel, ySize_pixel); // pixels

		// This code sets scaleFactor, needed in draw, was not set in script
		// mode when the viewer is not visible (experimental)
		Rectangle normale = new Rectangle(xPixels, yPixels, xSize_pixel, ySize_pixel);
		capsis.util.Point2D.Double scaleFactor = panel2D.getScale(userBounds, normale);

		// fc-5.4.2018

	}

	/**
	 * From Drawer interface. This method draws in the Panel2D each time the latter
	 * must be repainted. The given Rectangle is the sub-part (zoom) of the stand to
	 * draw in user coordinates (i.e. meters...). It can be used in preprocesses to
	 * avoid drawing invisible trees or cells. or cells.
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

		// System.out.println("SVSimple.draw ()...");

		Graphics2D g2 = (Graphics2D) g;

		// 1. Cast settings for ease
		SVSimpleSettings set = (SVSimpleSettings) settings;

		// 1.1. Optionally draw before
		drawBefore(g2, r, stand); // fc - 7.4.2005

		// Added this for cases where there is a DefaultPlot // fc-13.1.2012
		g2.setColor(Color.LIGHT_GRAY);
		g2.draw(userBounds);

		// 2. Choose a pixel detailThreshold, compute it in meters with current
		// scale
		// if dbh in m. >= detailThreshold -> detailled level is reached

		// // fc-5.4.2018 exploring how to paint viewers in an image from
		// scripts
		// // for PhD
		// // We compute margins for a "normal" window size (200x200)
		// if (panel2D.getCurrentScale() == null) {
		// Rectangle normale = new Rectangle(0, 0, 200, 200);
		// capsis.util.Point2D.Double scaleFactor = panel2D.getScale(userBounds,
		// normale);
		// }
		// // fc-5.4.2018
		//
		visibleThreshold = 1.1 / panel2D.getCurrentScale().x; // 1 pixel in in
																// meters

		// 3. Consider optional status and grouper
		//
		TreeList gtcstand = (TreeList) stand;
		Collection treesInStatus = gtcstand.getTrees();
		if (statusSelection != null) { // fc - 23.4.2004
			treesInStatus = gtcstand.getTrees(statusSelection);
			// System.out.println ("status : "+ statusSelection);
			// System.out.println ("number of tree in status :" +
			// treesInStatus.toArray().length);
		}
		HashSet treesInGroup = null;
		if (set.grouperMode) {
			String name = set.grouperName;
			GrouperManager gm = GrouperManager.getInstance();
			Grouper gr = gm.getGrouper(name); // return null if not found
			Collection aux = gr.apply(treesInStatus, name.toLowerCase().startsWith("not "));
			treesInGroup = new HashSet(aux);

			// Write directly in the panel2D if there is a group fc-3.9.2012
			Point origin_pixel = panel2D.getPixelPoint(new Point2D.Double(r.x, r.y));
			origin_pixel = new Point(origin_pixel.x, origin_pixel.y - 10);
			Point2D.Double text_user_anchor = panel2D.getUserPoint(origin_pixel);
			g2.drawString(Translator.swap("SVSimple.group") + ": " + name, (float) text_user_anchor.x,
					(float) text_user_anchor.y);

		} else {
			treesInGroup = new HashSet(treesInStatus);
		}

		// System.out.println("SVSimple (0) #treesInGroup: "+treesInGroup.size
		// ());

		// 4. Deal with cells
		Collection candidateTrees = new ArrayList();
		if (stand.hasPlot() && stand.getPlot() instanceof PlotOfCells) {
			PlotOfCells plot = stand.getPlot(); // fc-26.10.2017
			Collection<Cell> level1 = plot.getCellsAtLevel(1);

			// 4.2 Draw the cells, memorize their trees
			for (Cell c : level1) {
				processCell((TreeListCell) c, candidateTrees, g2, r);
			}
			// Caution: cells only contain alive trees !
			if (statusSelection != null && statusSelection.length == 1 && statusSelection[0].equals("alive")) {
				candidateTrees.retainAll(treesInGroup);
			} else {
				candidateTrees = treesInGroup;
			}

			// System.out.println("SVSimple (a) #candidateTrees: "+candidateTrees.size
			// ());

		} else {
			candidateTrees = treesInGroup;

			// System.out.println("SVSimple (b) #candidateTrees: "+candidateTrees.size
			// ());

		}

		// 5.1 Give the possibility to subclasses to make something at the
		// beginning (sorting...)
		Object[] trees = candidateTrees.toArray();

		// System.out.println("SVSimple (1) #trees: "+trees.length);

		trees = preProcessTrees(trees, r);

		// System.out.println("SVSimple (2) #trees: "+trees.length);

		// 5.2 Prepare label drawing strategy
		if (settings.maxLabelNumber <= 0) {
			labelCounter = 1;
			labelFrequency = Integer.MAX_VALUE;
		} else {
			labelCounter = 0;
			labelFrequency = Math.max(1, (int) trees.length / Math.max(1, settings.maxLabelNumber));
		}

		// 5.3 Draw the trees
		boolean enableMagnifyFactor = true; // fc - 4.3.2005

		shapeMap.clear();

		// System.out.println("SVSimple (3) #trees: "+trees.length);

		for (int i = 0; i < trees.length; i++) {
			// fc - 10.4.2008 - Spatialized is better
			// ~ GMaddTree t = (GMaddTree) trees[i];
			Tree t = (Tree) trees[i];
			Spatialized s = (Spatialized) t;
			if (t.isMarked()) {
				continue;
			} // fc - 5.1.2004
			if (t instanceof Numberable && ((Numberable) t).getNumber() <= 0) {
				continue;
			} // fc - 18.11.2004

			double dbh = t.getDbh();

			if (t instanceof SimpleCrownDescription) {
				enableMagnifyFactor = false;
			} // fc - 4.3.2005

			drawTree(g2, t, r);
			selectTreeIfNeeded(g2, t, r); // fc - 28.5.2003

			// 5.4 May draw tree label according to frequency - fc - 22.12.2003
			// showSomeLabels () : if return false, no labels this way
			// settings.showLabels : user asks for labels
			//
			float x = (float) s.getX();
			float y = (float) s.getY();
			if (showSomeLabels() && settings.showLabels && r.contains(new Point.Double(x, y))) {
				drawLabel(g2, String.valueOf(t.getId()), x, y);
			}
		}

		// If some trees implement SimpleCrownDescription, disable MagnifyFactor
		try { // fMagnifyFactor may not exist -> null possible -> ignore
				// exception
			fMagnifyFactor.setEnabled(enableMagnifyFactor); // fc - 4.3.2005
			fMagnifyFactor.setText("1"); // fc-12.3.2013
			magnifyFactor = 1; // fc-12.3.2013
		} catch (Exception e) {
		}

		// 6. Optionally draw more
		drawMore(g2, r, stand); // fc + bc - 6.2.2003

		// 7. Ensure title is ok
		// fc-23.12.2020
		// defineTitle();

		// 8. Relative to filtering (see FilteringPanel)
		updateToolTipText();
	}

	/**
	 * From Drawer interface. We may receive (from Panel2D) a selection rectangle
	 * (in user space i.e. meters) and return a JPanel containing information about
	 * the objects (trees) inside the rectangle. If no objects are found in the
	 * rectangle, return null. fc - 21.11.2003 - uses OVSelector
	 */
	public JPanel select(Rectangle.Double r, boolean ctrlIsDown) {

		// moved this line here - fc - 11.9.2008
		memoSelectionRectangle = r; // for reselect

		if (r == null) { // fc - 6.12.2007 - select () may be called early, on
							// update ()
			thisIsAReselection = false;
			// If there was some selection, remove it
			if (effectiveSelection != null && !effectiveSelection.isEmpty()) {
				effectiveSelection.clear();
				panel2D.reset(); // Force panel repainting for selection removal
				panel2D.repaint();
			}
			return null;
		} // fc - 6.12.2007 - select () may be called early, on update ()
			// ctrl desactivated - fc - 23.11.2007

		// 2.1 Manage Ctrl-selection : adding selection to existing selection
		// ~ if (!ctrlIsDown) {
		// ~ selectedTrees = new HashSet();
		// ~ selectedCells = new HashSet();
		// ~ }

		// ~ memoSelectionRectangle = r; // for reselect

		memoSelection = searchInRectangle(r); // fc - 23.11.2007

		// 2.5b Hook for subclasses : to add things which are in the rectangle,
		// but not trees nor cells (landscape element, human landmark...)
		selectMore(r, ctrlIsDown, memoSelection);

		// fc - 9.9.2008
		effectiveSelection = ovSelector.select(memoSelection, thisIsAReselection);
		// rearm selection for next time (detail: in reselection mode, if the
		// OVDialog is unvisible, it is not set visible)
		thisIsAReselection = false;

		// ~ if (currentOV == null) {
		// ~ openNewOV ();
		// ~ }

		// Fire a selection event to tell the selection changed -> the
		// objectViewer updates
		// ~ boolean selectionActuallyChanged = !selectCalledByReselect;
		// ~ SelectionEvent evt = new SelectionEvent (this,
		// selectionActuallyChanged);
		// ~ fireSelectionEvent (evt);

		// 2.7 Selected trees / cells will be enlighted
		// effectiveListenerSelection is what the ovchooser actually shows
		// (maybe it can not show everything)
		// ~ effectiveSelection = evt.getListenerEffectiveSelection ();

		// Force panel repainting for effective selection enlighting
		panel2D.reset();
		panel2D.repaint();

		// ~ selectCalledByReselect = false; // unset this boolean, will be set
		// if reselect () is called
		// ~ return currentOV; // never null (see OVChooser)
		return null; // OVSelector framework: the OVSelector takes care of
						// showing the ov, return null to panel2D
	}

	// When this viewer updates (ex: changes StepButtons), see if reselection
	// must
	// be triggered towards OVSelector
	public void reselect() { // fc - 23.11.2007

		// System.out.println ("*** SVSimple.reselect ()...");

		boolean ctrlIsDown = false;
		thisIsAReselection = true;
		select(memoSelectionRectangle, ctrlIsDown);
	}

	// If ovDialog is closed by user, remove any selection marks
	public void cancelSelection() {
		select(null, false);
	}

	// fc - 23.11.2007 - OVChooserListener
	// The object viewer was just changed by user (in OVChooser)
	// ~ public void selectWasHit () { // ovChooser asks for reselection //DEL
	// ~ System.out.println
	// ("SVSimple.selectWasHit ()... calling reselect ()...");

	// ~ // change currentOV
	// ~ openNewOV (); // fc - 11.2.2008

	// ~ // 3. Update the new object viewer with current selection
	// ~ reselect ();
	// ~ }

	// ~ // fc - 23.11.2007 - SelectionSource
	// ~ public void addSelectionListener (SelectionListener l) {
	// ~ if (selectionListeners == null) {selectionListeners = new
	// ArrayList<SelectionListener> ();}
	// ~ selectionListeners.add (l);
	// ~ }
	// ~ public void removeSelectionListener (SelectionListener l) {
	// ~ if (selectionListeners == null) {return;}
	// ~ selectionListeners.remove (l);
	// ~ }
	// ~ public void fireSelectionEvent (SelectionEvent e) {
	// ~ if (selectionListeners == null) {return;}
	// ~ for (SelectionListener l : selectionListeners) {
	// ~ l.sourceSelectionChanged (e);
	// ~ }
	// ~ }
	// ~ public Collection getSelection () {
	// ~ return memoSelection;
	// ~ }
	// ~ // fc - 23.11.2007 - SelectionSource

	/**
	 * This methods returns the object selected by the given rectangle. Called by
	 * select (Rectangle, boolean) in SVSimple superclass.
	 */
	protected Collection<Object> searchInRectangle(Rectangle.Double r) {
		// fc - 7.12.2007
		// this search mehod can be used without redefinition in sub classes
		// in case of redefinition, search well...
		Collection<Object> inRectangle = new ArrayList<Object>();

		if (r == null)
			return inRectangle; // null rectangle: nothing found

		// 1. If Status is set, restrict stand to active Status
		TreeList gtcstand = (TreeList) stand;
		Collection<? extends Tree> treesInStatus = gtcstand.getTrees();
		if (statusSelection != null) {
			treesInStatus = gtcstand.getTrees(statusSelection);
		}

		// 2. If grouper is set, restrict stand to given grouper
		Collection<? extends Tree> treesInGroup = treesInStatus; // fc -
																	// 5.4.2004
		if (settings.grouperMode) {
			String name = settings.grouperName;
			GrouperManager gm = GrouperManager.getInstance();
			Grouper gr = gm.getGrouper(name); // return null if not found
			treesInGroup = gr.apply(treesInGroup, name.toLowerCase().startsWith("not "));
		}

		// 2.2 What trees are in user selection rectangle ?
		for (Iterator i = treesInGroup.iterator(); i.hasNext();) {
			// fc - 10.4.2008
			Tree t = (Tree) i.next();
			Spatialized s = (Spatialized) t;

			if (t.isMarked())
				continue; // fc - 5.1.2004 - dead tree

			if (t instanceof Numberable && ((Numberable) t).getNumber() <= 0)
				continue; // fc -
							// 18.11.2004

			Point.Double p = new Point.Double(s.getX(), s.getY());
			if (r.contains(p)) {
				// Check if the tree location is within the selection rectangle
				inRectangle.add(t);

			} else {

				// See the various shapes drawn for the tree by drawTree
				// fc-1.2.2012
				if (shapeMap != null) {
					Set<Shape> shapes = shapeMap.get(t);
					if (shapes != null) {
						for (Shape sh : shapes) {
							if (sh.intersects(r)) {
								inRectangle.add(t);
								break;
							}
						}
					}
				}

			}
		}

		// 2.3 What cells are in user selection rectangle ?
		if (stand.hasPlot() && stand.getPlot() instanceof PlotOfCells) {
			PlotOfCells plotc = stand.getPlot(); // fc-26.10.2017
			for (Cell c : (Collection<Cell>) plotc.getCells()) {

				Shape cellShape = c.getShape();
				if (cellShape.intersects(r)) {
					inRectangle.add(c);
				}
			}

		}

		return inRectangle;
	}

	/**
	 * From Pilotable interface. A tool bar for SVSimple.
	 */
	public JComponent getPilot() {
		// ImageIcon icon = IconLoader.getIcon ("properties_16.png");
		ImageIcon icon = IconLoader.getIcon("option_16.png");
		settingsButton = new JButton(icon);
		Tools.setSizeExactly(settingsButton, BUTTON_SIZE, BUTTON_SIZE);
		settingsButton.setToolTipText(Translator.swap("SVSimple.settings"));
		settingsButton.addActionListener(this);

		// ~ icon = new IconLoader ().getIcon ("zoom-out_16.png");
		// ~ filteringButton = new JButton (icon);
		// ~ Tools.setSizeExactly (filteringButton, BUTTON_SIZE, BUTTON_SIZE);
		// ~ filteringButton.setToolTipText (Translator.swap
		// ("Shared.filtering"));
		// ~ filteringButton.addActionListener (this);

		icon = IconLoader.getIcon("help_16.png");
		helpButton = new JButton(icon);
		Tools.setSizeExactly(helpButton, BUTTON_SIZE, BUTTON_SIZE);
		helpButton.setToolTipText(Translator.swap("Shared.help"));
		helpButton.addActionListener(this);

		JToolBar toolbar = new JToolBar();
		toolbar.add(settingsButton);
		toolbar.addSeparator();
		// ~ toolbar.add (filteringButton);
		toolbar.add(ovSelector);
		// ~ toolbar.add (ovChooser);

		if (!isMagnifyVetoed()) {
			fMagnifyFactor = new JTextField(2);
			fMagnifyFactor.setText("" + magnifyFactor);
			fMagnifyFactor.setToolTipText(Translator.swap("Shared.magnifyFactorExplanation"));
			fMagnifyFactor.addActionListener(this);
			JLabel l20 = new JLabel(Translator.swap("Shared.magnifyFactor") + ": ");
			l20.setToolTipText(Translator.swap("Shared.magnifyFactorExplanation"));
			toolbar.addSeparator();
			toolbar.add(l20);
			toolbar.add(fMagnifyFactor);
		}

		toolbar.addSeparator();
		toolbar.add(helpButton);
		toolbar.setVisible(true);

		return toolbar;
	}

	/**
	 * Subclasses can draw things before the cells and trees by redefining this
	 * method.
	 */
	protected void drawBefore(Graphics2D g2, Rectangle.Double r, GScene s) {
	} // fc - 26.11.2003 -
		// added visible
		// rectangle

	/**
	 * Subclasses can draw things after the cells and trees by redefining this
	 * method.
	 */
	protected void drawMore(Graphics2D g2, Rectangle.Double r, GScene s) {
	} // fc - 26.11.2003 -
		// added visible
		// rectangle

	/**
	 * Subclasses can select things other then trees and cells : add them to the
	 * currentSelection (which was prepared by select () : already possibly contains
	 * trees and cells). Note: do not alter currentSelection, just add things to it.
	 */
	protected void selectMore(Rectangle.Double r, boolean ctrlIsDown, Collection currentSelection) {
	}

	/**
	 * This viewer tries to show some tree labels, this can be desactivated by
	 * redefining this method and return false.
	 */
	protected boolean showSomeLabels() {
		return true;
	}

	/**
	 * Retrieve the settings for this viewer as saved the last time they were
	 * changed.
	 */
	protected void retrieveSettings() {

		System.out.println("SVSimple > retrieveSettings () a. settings: " + settings);

		settings = new SVSimpleSettings();

		System.out.println("SVSimple > retrieveSettings () b. settings: " + settings);

		cellSelectionColor = new Color(settings.selectionColor.getRed(), settings.selectionColor.getGreen(),
				settings.selectionColor.getBlue(), 150);

		System.out.println("SVSimple > retrieveSettings () c. settings: " + settings);
	}

	/**
	 * Veto on magnify facility for subclasses. To remove the veto for some
	 * particular subclass, redefine thss and return false.
	 */
	protected boolean isMagnifyVetoed() {
		return !getClass().getName().equals("capsis.extension.standviewer.SVSimple");
	} // fc - 18.11.2004

	public Color getLabelColor() {
		return settings.labelColor;
	} // use getSettings ()

	public Color getTreeColor() {
		return settings.treeColor;
	}

	public Color getCellColor() {
		return settings.cellColor;
	}

	public Color getSelectionColor() {
		return settings.selectionColor;
	}

	/**
	 * Used for the settings and filtering buttons.
	 */
	public void actionPerformed(ActionEvent evt) {

		SVSimpleSettings s = settings;

		if (evt.getSource().equals(settingsButton)) {
			MainPanel mainPanel = new MainPanel(this, s, stand, panel2D, optionPanel, userBounds);
			DialogWithOkCancel dlg = new DialogWithOkCancel(mainPanel);
			if (dlg.isValidDialog()) {

				// 1. config for SVSimple
				s.showLabels = mainPanel.getCkShowLabels().isSelected();
				s.maxLabelNumber = mainPanel.getMaxLabelNumber();
				s.showDiameters = mainPanel.getCkShowDiameters().isSelected();
				s.showTransparency = mainPanel.getCkShowTransparency().isSelected();
				s.labelColor = mainPanel.getNewLabelColor();
				s.treeColor = mainPanel.getNewTreeColor();
				s.cellColor = mainPanel.getNewCellColor();
				s.selectionColor = mainPanel.getNewSelectionColor();
				cellSelectionColor = new Color(s.selectionColor.getRed(), s.selectionColor.getGreen(),
						s.selectionColor.getBlue(), 150);

				s.grouperMode = mainPanel.getGrouperChooser().isGrouperAvailable(); // fc
																					// -
																					// 30.4.2003
				s.grouperNot = mainPanel.getGrouperChooser().isGrouperNot(); // fc
																				// -
																				// 21.4.2004
				s.grouperName = mainPanel.getGrouperChooser().getGrouperName();

				// ~ statusChooser = mainPanel.getStatusChooser (); // fc -
				// 22.4.2004
				try {
					statusSelection = mainPanel.getStatusChooser().getSelection();
				} catch (Exception e) {
				}

				s.panel2DSettings = panel2D.getSettings();

				// 2. config for panel2D
				// panel2D.configure(mainPanel.getPanel2DConfigPanel()); //
				// REMOVED fc-3.9.2012

				// 3. config for SVSimple subclass
				optionAction(); // hook for subclasses

				panel2D.invalidate();
			}
			mainPanel.dispose();
			dlg.dispose();
			update();

		} else if (evt.getSource().equals(helpButton)) {
			Helper.helpFor(this);

		} else if (evt.getSource().equals(fMagnifyFactor)) { // never happens
																// when
																// isMagnifyVetoed
																// () is true
			try {
				String t = fMagnifyFactor.getText().trim();
				magnifyFactor = new Integer(t).intValue();
				Settings.setProperty("extension.svsimple.magnify.factor", "" + magnifyFactor);

				panel2D.reset();
				panel2D.repaint();

			} catch (Exception e) {
				MessageDialog.print(this, Translator.swap("Shared.magnifyFactorMustBeAnInteger"));
				return;
			}

			// ~ } else if (evt.getSource ().equals (filteringButton)) {

			// ~ FilteringPanel filteringPanel = new FilteringPanel
			// (filtration);

			// ~ DUser2 dlg = new DUser2 (filteringPanel);

			// ~ filtration = filteringPanel.getSettings (); // mandatory
			// (updates values)
			// ~ panel2D.reset ();
			// ~ panel2D.repaint ();
			// ~ updateToolTipText ();

			// ~ filteringPanel.dispose ();
			// ~ dlg.dispose ();
		}
	}

	/**
	 * Creates a tool tip text on panel2D to inform user if some filtering is
	 * currently active.
	 */
	protected void updateToolTipText() {
		StringBuffer toolTipText = new StringBuffer();

		// ~ if (filtration.isDetailSet) {
		// ~ toolTipText.append ("dDet=");
		// ~ toolTipText.append (filtration.detailValue);
		// ~ toolTipText.append ("p. ");
		// ~ }
		// ~ if (filtration.isMinimumSet) {
		// ~ toolTipText.append ("dMin=");
		// ~ toolTipText.append (filtration.minimumValue);
		// ~ toolTipText.append ("cm. ");
		// ~ }
		// ~ if (filtration.isMaximumSet) {
		// ~ toolTipText.append ("dMax=");
		// ~ toolTipText.append (filtration.maximumValue);
		// ~ toolTipText.append ("cm. ");
		// ~ }
		// ~ if (toolTipText.length () == 0) {
		// ~ toolTipText.append (Translator.swap ("Shared.noActiveFilters"));
		// ~ }

		panel2D.setToolTipText(toolTipText.toString());
	}

	/**
	 * Called when ok on option panel, must be redefined by subclasses which use
	 * optionPanel
	 */
	protected void optionAction() {
		ExtensionManager.recordSettings(this);
	}

	/**
	 * Update the viewer (for example, when changing step).
	 */
	public void update() {

		super.update();

		stand = step.getScene();

		// fc-23.12.2020
//		defineTitle();

		scrollPane.setViewportView(panel2D); // panel2D might change size
		panel2D.reset(); // to disable buffered image and force redrawing
		panel2D.repaint();

		// Reselect must be done after repaint
		SwingUtilities.invokeLater(new Runnable() {

			public void run() {
				reselect();
			}
		});

		// reselect ();

	}

	/**
	 * Refresh the GUI with another Step.
	 */
	public void update(StepButton sb) {
		super.update(sb);
		update();
	}

	/**
	 * An optional complement to the title defined in AbstractStandViewer (appended
	 * after it if not null)
	 */
	@Override
	public String getTitleComplement() {

		// fc-23.12.2020 Replaces defineTitle ()

		StringBuffer b = new StringBuffer();

		// fc-18.1.2021 Added try catch and explanation
		try {
			// Group name if any group
			if (settings.grouperMode && settings.grouperName != null) {
				b.append(Translator.swap("Shared.group") + " : " + settings.grouperName);
			}
		} catch (Exception e) {
			// Ignore, subclasses may have their own configuration and settings may be null
			// here, see SVMediterranea.retrieveSettings ()
		}

		// Status: ask name considering stand under current step
		// and current statusChooser selection (we may have changed
		// steps since selection). E.g. (cut+windfall).
		if (stand instanceof TreeList) {

			String statusName = StatusChooser.getName(statusSelection);
			if (statusName != null && statusName.length() > 0) {
				if (b.length() > 0)
					b.append(", ");
				b.append(statusName); // e.g. (alive+cut)
			}
		}

		return "" + b;
	}

	// fc-23.12.2020 Replaced by getTitleComplement ()
//	// Viewer title definition : reference to current step and considered group.
//	//
//	protected void defineTitle() {
//		try {
//			StringBuffer t = new StringBuffer();
//
//			// 1. Step reference
//			t.append(step.getProject().getName());
//			t.append(".");
//			t.append(step.getName());
//
//			// 1.1 Status : ask name considering stand under current step
//			// and current statusChooser selection (we may have changed
//			// steps since selection). Ex: (cut+windfall).
//			if (stand instanceof TreeList) { // fc - 16.9.2004
//
//				String statusName = StatusChooser.getName(statusSelection);
//				if (statusName != null && !statusName.equals("")) {
//					t.append(statusName); // ex: (alive+cut)
//				}
//			}
//
//			// 2. Group name if exists
//			// fc - 12.9.2008 - added try / catch - bidasoa viewer has a better
//			// title
//			try {
//				if (settings.grouperMode && settings.grouperName != null) {
//					t.append(" / ");
//					t.append(settings.grouperName);
//				}
//			} catch (Exception e) {
//			} // does not matter
//
//			// 3. Viewer name
//			t.append(" - ");
//			t.append(this.getName ());
//			customTitle = t.toString();
//
//		} catch (Exception e) {
//			Log.println(Log.WARNING, "SVSimple.defineTitle ()",
//					"can not build fine viewer title, returned simply getName ()", e);
//			customTitle = this.getName ();
//		}
//		setTitle(customTitle);
//	}

	// nb-07.08.2018
//	/**
//	 * Redefines superclass getTitle () (title from step button and extension
//	 * name) : maybe group name added.
//	 */
//	public String getName() {
//		return (customTitle == null) ? super.getName() : customTitle;
//	}

	/**
	 * Create the GUI.
	 */
	protected void createUI() {

		// fc-23.12.2020
//		defineTitle();

		getContentPane().setLayout(new BorderLayout()); // mainBox in the
														// internalFrame
		scrollPane = new JScrollPane(panel2D);

		scrollPane.getViewport().putClientProperty("EnableWindowBlit", Boolean.TRUE); // faster

		getContentPane().add(getPilot(), BorderLayout.NORTH);

		// fc - 7.4.2006
		splitPane = new JSplitPane();
		splitPane.setLeftComponent(scrollPane);
		splitPane.setRightComponent(null);
		splitPane.setResizeWeight(1); // when resizing, the extra space goes to
										// the left component
		// fc - 7.4.2006

		// ~ getContentPane ().add (scrollPane, BorderLayout.CENTER); // fc -
		// 7.4.2006
		getContentPane().add(splitPane, BorderLayout.CENTER); // fc - 7.4.2006

	}

	/**
	 * Add a legend component on the right.
	 */
	protected void setLegend(JComponent newLegend) {

		// Security
		if (newLegend instanceof JScrollPane) {
			Log.println(Log.ERROR, "SVSimple.setLegend ()", "Legend must not be a JScrollPane");
			return;
		}

		// fc - 7.4.2006
		if (newLegend == null) {
			layoutLegend(newLegend);
			return;
		}

		// fc - 7.4.2006
		legend = new JScrollPane(newLegend);
		Dimension d = newLegend.getPreferredSize();
		legend.setPreferredSize(new Dimension(d.width + 20, d.height + 20));
		layoutLegend(legend);

		return;

	}

	// fc - 8.3.2004
	protected void layoutLegend(JComponent legend) {
		// ~ getContentPane ().add (legend, BorderLayout.EAST); // fc - 7.4.2006

		splitPane.setRightComponent(legend); // fc - 7.4.2006
		splitPane.resetToPreferredSizes(); // fc - 7.4.2006
	}

	public void dispose() {
		super.dispose();
		ovSelector.dispose(); // fc - 11.9.2008
		panel2D.dispose();
		settings = null;
		stand = null;
		panel2D = null;
		settingsButton = null;
		mainPanel = null;
		optionPanel = null;
		scrollPane = null;
	}

	public Color getCellSelectionColor() {
		return cellSelectionColor;
	}

	public int getPanel2DXMargin() {
		return Panel2D.X_MARGIN_IN_PIXELS;
	}

	public int getPanel2DYMargin() {
		return Panel2D.Y_MARGIN_IN_PIXELS;
	}

	public String[] getStatusSelection() {
		return statusSelection;
	}

	public void setStatusSelection(String[] s) {
		statusSelection = s;
	}

	// fc-18.1.2021 Added to help debugging
	public SVSimpleSettings getSettings() {
		return settings;
	}

	// ---------------------------------------------------------------------------
	// MainPanel
	// ---------------------------------------------------------------------------
	// MainPanel
	// ---------------------------------------------------------------------------
	// MainPanel
	// ---------------------------------------------------------------------------
	// MainPanel
	// ---------------------------------------------------------------------------
	// MainPanel

	/**
	 * Main option panel. Secondary option panel may be described in subclasses.
	 */
	static public class MainPanel extends JPanel implements ActionListener, Controlable {

		private JTabbedPane tabs;

		private GrouperChooser grouperChooser;
		private StatusChooser statusChooser;

		private JCheckBox ckShowLabels;
		private JTextField maxLabelNumber; // fc - 18.12.2003
		private JCheckBox ckShowDiameters;
		private JCheckBox ckShowTransparency;
		private ColoredButton labelColorButton;
		private ColoredButton treeColorButton;
		private ColoredButton cellColorButton;
		private ColoredButton selectionColorButton;

		/*
		 * private JRadioButton sideViewMode; private JRadioButton inspectorMode;
		 * private ButtonGroup selectMode;
		 */

		// private ConfigurationPanel p2DConfigPanel; // REMOVED fc-3.9.2012

		private JButton exportButton;
		private JButton exportDrawer;
		private Container embedder; // fc - 23.7.2004
		private SVSimple sv;
		private Rectangle.Double userBounds;

		public MainPanel(SVSimple v, SVSimpleSettings settings, GScene stand, Panel2D panel2D, JPanel optionPanel,
				Rectangle.Double ub) {
			super();
			sv = v;
			userBounds = ub;
			setLayout(new BorderLayout());
			// ~ Border etched = BorderFactory.createEtchedBorder ();

			ColumnPanel part1 = new ColumnPanel(0, 0);

			// 1. Common
			ColumnPanel p1 = new ColumnPanel(Translator.swap("SVSimple.MainPanel.common"));
			// ~ Border b1 = BorderFactory.createTitledBorder (etched,
			// Translator.swap ("SVSimple.MainPanel.common"));
			// ~ p1.setBorder (b1);

			ckShowLabels = new JCheckBox(Translator.swap("SVSimple.MainPanel.showLabels"), settings.showLabels);
			maxLabelNumber = new JTextField(5);
			maxLabelNumber.setText("" + settings.maxLabelNumber);
			ckShowDiameters = new JCheckBox(Translator.swap("SVSimple.MainPanel.showDiameters"),
					settings.showDiameters);
			ckShowTransparency = new JCheckBox(Translator.swap("SVSimple.MainPanel.showTransparency"),
					settings.showTransparency);

			LinePanel l6 = new LinePanel();
			LinePanel l7 = new LinePanel();
			LinePanel l70 = new LinePanel();
			LinePanel l71 = new LinePanel();
			LinePanel l8 = new LinePanel();
			LinePanel l9 = new LinePanel();
			l6.add(ckShowLabels);
			// ~ l6.add (new JLabel ("- "+Translator.swap
			// ("SVSimple.MainPanel.maxLabelNumber")+" : "));
			l6.add(maxLabelNumber);
			l6.addGlue();
			l7.add(ckShowDiameters);
			l7.addGlue();
			l70.add(ckShowTransparency);
			l70.addGlue();

			// fc - 22.4.2004 - Status

			try {
				TreeList gtcstand = (TreeList) stand;
				statusChooser = new StatusChooser(gtcstand.getStatusKeys(), v.getStatusSelection());
				l71.add(statusChooser);
			} catch (Exception e) {
			} // sometimes not a TreeList...

			// NEW... groups
			boolean checked = settings.grouperMode;
			boolean not = settings.grouperNot;
			String selectedGroupName = settings.grouperName;

			// fc-3.6.2020 Restricted to GROUP_ALIVE_TREE types
			grouperChooser = new GrouperChooser(stand, TreeList.GROUP_ALIVE_TREE,
					Arrays.asList(TreeList.GROUP_ALIVE_TREE), selectedGroupName, not, true, checked);
//			grouperChooser = new GrouperChooser(stand, TreeList.GROUP_ALIVE_TREE, selectedGroupName, not, true, checked);

			l8.add(grouperChooser);

			exportButton = new JButton(Translator.swap("Shared.export"));
			exportButton.addActionListener(this);
			l9.add(exportButton);

			exportDrawer = new JButton(Translator.swap("ExportDrawer"));
			exportDrawer.addActionListener(this);
			l9.add(exportDrawer);
			l9.addGlue();

			p1.add(l6);
			p1.add(l7);
			p1.add(l70);
			p1.add(l71);
			p1.add(l8);
			p1.add(l9);
			p1.addStrut0(); // fc - 25.4.2007
			part1.add(p1);

			// 3. Colors
			ColumnPanel p2 = new ColumnPanel(Translator.swap("SVSimple.MainPanel.colors"));
			// ~ Border b2 = BorderFactory.createTitledBorder (etched,
			// Translator.swap ("SVSimple.MainPanel.colors"));
			// ~ p2.setBorder (b2);

			// ~ labelColorButton = new JButton (); // empty coloured buttons
			// ~ treeColorButton = new JButton ();
			// ~ cellColorButton = new JButton ();
			// ~ selectionColorButton = new JButton ();
			// ~ labelColorButton.setBackground (getLabelColor ());
			// ~ treeColorButton.setBackground (getTreeColor ());
			// ~ cellColorButton.setBackground (getCellColor ());
			// ~ selectionColorButton.setBackground (getSelectionColor ());

			labelColorButton = new ColoredButton(v.getLabelColor());
			treeColorButton = new ColoredButton(v.getTreeColor());
			cellColorButton = new ColoredButton(v.getCellColor());
			selectionColorButton = new ColoredButton(v.getSelectionColor());

			labelColorButton.addActionListener(this);
			treeColorButton.addActionListener(this);
			cellColorButton.addActionListener(this);
			selectionColorButton.addActionListener(this);

			LinePanel l1 = new LinePanel();
			LinePanel l2 = new LinePanel();
			LinePanel l3 = new LinePanel();
			LinePanel l4 = new LinePanel();
			l1.add(new JWidthLabel(Translator.swap("SVSimple.MainPanel.labelColor") + " :", 170));
			l1.add(labelColorButton);
			l1.addGlue();
			l2.add(new JWidthLabel(Translator.swap("SVSimple.MainPanel.treeColor") + " :", 170));
			l2.add(treeColorButton);
			l2.addGlue();
			l3.add(new JWidthLabel(Translator.swap("SVSimple.MainPanel.cellColor") + " :", 170));
			l3.add(cellColorButton);
			l3.addGlue();
			l4.add(new JWidthLabel(Translator.swap("SVSimple.MainPanel.selectionColor") + " :", 170));
			l4.add(selectionColorButton);
			l4.addGlue();

			p2.add(l1);
			p2.add(l2);
			p2.add(l3);
			p2.add(l4);
			p2.addStrut0(); // fc - 25.4.2007
			part1.add(p2);
			// fc - 25.4.2007
			part1.addGlue();

			// 4. NEW - panel2D configuration (fc - 30.4.2002)
			// p2DConfigPanel = panel2D.getConfigurationPanel(null); // REMOVED
			// fc-3.9.2012
			// // fc - 25.4.2007
			// ColumnPanel p3 = new
			// ColumnPanel(panel2D.getConfigurationLabel());
			// p3.add(p2DConfigPanel);
			// p3.addStrut0();
			// // fc - 25.4.2007

			// fc - 25.4.2007
			LinePanel main = new LinePanel();
			main.add(new NorthPanel(part1));
			// main.add(new NorthPanel(p3)); // REMOVED fc-3.9.2012
			main.addStrut0();

			// 5. Correct layout
			JPanel worker = new JPanel(new BorderLayout());
			worker.add(main, BorderLayout.NORTH);

			if (optionPanel.getComponentCount() != 0) {
				JPanel worker2 = new JPanel(new BorderLayout());
				worker2.add(optionPanel, BorderLayout.NORTH);

				tabs = new JTabbedPane();
				tabs.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT); // fc -
																		// 2.4.2003

				tabs.addTab(Translator.swap("SVSimple.MainPanel.general"), null, worker);
				tabs.addTab(Translator.swap("SVSimple.MainPanel.options"), null, worker2);
				add(tabs, BorderLayout.CENTER);
			} else {
				// ~ add (part1, BorderLayout.NORTH);
				add(main, BorderLayout.NORTH);
			}

		}

		public boolean isControlSuccessful() {
			// Panel2D config panel must be ok
			// if (!p2DConfigPanel.checksAreOk()) { // REMOVED fc-3.9.2012
			// return false;
			// }

			// Max label number must be an integer
			try {
				new Integer(maxLabelNumber.getText().trim()).intValue();
			} catch (Exception e) {
				MessageDialog.print(this, Translator.swap("Shared.maxLabelNumberMustBeInteger"));
				return false;
			}

			// fc - 22.4.2004
			try {
				if (!statusChooser.isChooserValid()) {
					MessageDialog.print(this, Translator.swap("Shared.chooseAtLeastOneStatus"));
					return false;
				}
				sv.setStatusSelection(statusChooser.getSelection());
			} catch (Exception e) {
			} // sometimes not a treelist

			return true;
		}

		// public ConfigurationPanel getPanel2DConfigPanel() { // REMOVED
		// fc-3.9.2012
		// return p2DConfigPanel;
		// }

		public void actionPerformed(ActionEvent evt) {

			if (evt.getSource().equals(labelColorButton)) {
				Color newColor = JColorChooser.showDialog(this, Translator.swap("SVSimple.MainPanel.chooseAColor"),
						labelColorButton.getBackground());
				if (newColor != null) {
					labelColorButton.colorize(newColor);
				}

			} else if (evt.getSource().equals(treeColorButton)) {
				Color newColor = JColorChooser.showDialog(this, Translator.swap("SVSimple.MainPanel.chooseAColor"),
						treeColorButton.getBackground());
				if (newColor != null) {
					treeColorButton.colorize(newColor);
				}

			} else if (evt.getSource().equals(cellColorButton)) {
				Color newColor = JColorChooser.showDialog(this, Translator.swap("SVSimple.MainPanel.chooseAColor"),
						cellColorButton.getBackground());
				if (newColor != null) {
					cellColorButton.colorize(newColor);
				}

			} else if (evt.getSource().equals(selectionColorButton)) {
				Color newColor = JColorChooser.showDialog(this, Translator.swap("SVSimple.MainPanel.chooseAColor"),
						selectionColorButton.getBackground());
				if (newColor != null) {
					selectionColorButton.colorize(newColor);
				}

			} else if (evt.getSource().equals(exportButton)) {
				if (embedder instanceof JFrame) {
					new ExportComponent(sv, (JFrame) embedder);
				} else {
					new ExportComponent(sv, (JDialog) embedder);
				}

			} else if (evt.getSource().equals(exportDrawer)) { // fc - 12.1.2005
				if (embedder instanceof JFrame) {
					new ExportDrawer(sv, userBounds, (JFrame) embedder);
				} else {
					new ExportDrawer(sv, userBounds, (JDialog) embedder);
				}
			}
		}

		public void dispose() {
			try {
				tabs.removeAll();
			} catch (Exception e) {
			}
		}

		public GrouperChooser getGrouperChooser() {
			return grouperChooser;
		}

		public StatusChooser getStatusChooser() {
			return statusChooser;
		}

		public JCheckBox getCkShowLabels() {
			return ckShowLabels;
		}

		public JCheckBox getCkShowDiameters() {
			return ckShowDiameters;
		}

		public JCheckBox getCkShowTransparency() {
			return ckShowTransparency;
		}

		public Color getNewLabelColor() {
			// ~ return new Color (labelColorButton.getBackground ().getRGB ());
			return labelColorButton.getColor();
		}

		public Color getNewTreeColor() {
			// ~ return new Color (treeColorButton.getBackground ().getRGB ());
			return treeColorButton.getColor();
		}

		public Color getNewCellColor() {
			// ~ return new Color (cellColorButton.getBackground ().getRGB ());
			return cellColorButton.getColor();
		}

		public Color getNewSelectionColor() {
			// ~ return new Color (selectionColorButton.getBackground ().getRGB
			// ());
			return selectionColorButton.getColor();
		}

		public int getMaxLabelNumber() { // format was checked in
											// isControlSuccessful ()
			return new Integer(maxLabelNumber.getText().trim()).intValue();
		}

		public String getTitle() {
			// return Translator.swap (AmapTools.getClassSimpleName
			// (this.getClass ().getName ()));
			return Translator.swap(this.getClass().getSimpleName());
		}

	}

}
