package capsis.extension.standviewer;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.geom.Arc2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JToolBar;

import jeeb.lib.util.Log;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex2d;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.commongui.projectmanager.StepButton;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.SpatializedTree;
import capsis.defaulttype.Speciable;
import capsis.defaulttype.Species;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.plotofcells.Cell;
import capsis.defaulttype.plotofcells.PolygonalCell;
import capsis.defaulttype.plotofcells.RoundMask;
import capsis.defaulttype.plotofcells.SquareCell;
import capsis.defaulttype.plotofcells.SquareCellHolder;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.util.Polygon2D;
import capsis.util.group.Grouper;
import capsis.util.group.GrouperManager;
import capsis.util.methodprovider.CellInformation;

/**
 * SVLox is a cartography simple viewer for trees with coordinates. It
 * draws the trees within the cells. It's based on SVSimple.
 * It was built for loxodonta model and is compatible with all derived models.
 *
 * @author Ph. Dreyfus - January 2003 - March 2003
 */
public class SVLox extends SVSimple {
	
	static {
		Translator.addBundle("capsis.extension.standviewer.SVLox");
	}

	// nb-07.08.2018
	//static public String AUTHOR = "Ph. Dreyfus";
	//static public String VERSION = "1.0";

			// see also SVLox_en.properties or SVLox_fr.properties for the meaning of these checkboxes
	private JCheckBox ckCellLines;						// see cell borders
	private JCheckBox ckSquareCells;					// see square cells
	private JCheckBox ckGhaTint;							// background darkness of the cell is a function of basal area / ha
	private JCheckBox ckCrownProjection;				// in view from above : crown is shown (if disabled, only stem is shown)
	private JCheckBox ckRefined;							// crown drawing is refined
	private JCheckBox ckNhaValue;						// show value of tree number / ha in the cell
	private JCheckBox ckGhaValue;						// show value of basal area / ha in the cell
	private JCheckBox ckDrawU130;						// show trees with height under 1.30 m
	private JCheckBox ckDrawLT1pix;					// very small details (small seedlings) less then 1 pixel are emphasized (2 pixels) in order to be visible
	private JCheckBox ckCTO_grid;						// CTO for "class-tree only" : in a compartment WITH a grid of square cells, only draw one tree for a "cohort-tree"
																		// representing several (sometimes many !) trees (number > 1 ... or number >> 1 !)
																		// useful to avoid drawing a very high number of trees
	private JCheckBox ckCTO_nogrid;					// CTO for "class-tree only" : in a compartment WITHOUT a grid of square cells, only draw one tree for a "cohort-tree"
																		// representing several (sometimes many !) trees (number > 1 ... or number >> 1 !)
																		// useful to avoid drawing a very high number of trees
	private JCheckBox ckCompName;								// compartment name

	private JCheckBox ckSp1;	// in Tool bar
	private JCheckBox ckSp2;

	private Color grayed;
	private static int alpha = 150;

	public static Color DARK_BROWN = new Color (95, 70, 50);

	//public static Color SPECIES_1_COLOR_ALPHA = new Color (0, 255, 0, alpha);
	//public Color SPECIES_1_COLOR_ALPHA = new Color (getTreeColor ().getRed(), getTreeColor ().getGreen(), getTreeColor ().getBlue(), alpha);
	public Color SPECIES_1_COLOR_ALPHA;
	//public static Color SPECIES_1_COLOR = new Color (0, 255, 0);
	//public Color SPECIES_1_COLOR = getTreeColor ();
	public Color SPECIES_1_COLOR;

	public static Color SPECIES_2_COLOR_ALPHA = new Color (230, 230, 230, alpha);
	public static Color SPECIES_2_COLOR = new Color (230, 230, 230);

	public static Color NHA_COLOR = Color.black;

	public static final int SPECIES_1 = 1;
	public static final int SPECIES_2 = 2;

	private JButton aboveOrSectionalView;

	/** Init function */
	@Override
	public void init(GModel model, Step s, StepButton but) throws Exception {
		super.init (model, s, but);
	

		SVLoxSettings set = (SVLoxSettings) settings;

		SPECIES_1_COLOR_ALPHA = new Color (getTreeColor ().getRed(), getTreeColor ().getGreen(), getTreeColor ().getBlue(), alpha);
		SPECIES_1_COLOR = getTreeColor ();

		ckCellLines = new JCheckBox (Translator.swap ("SVLox.cellLines"), set.cellLines);
		ckSquareCells = new JCheckBox (Translator.swap ("SVLox.squareCells"), set.squareCells);
		ckCrownProjection = new JCheckBox (Translator.swap ("SVLox.crownProjection"), set.crownProjection);
		ckNhaValue = new JCheckBox (Translator.swap ("SVLox.nhaValue"), set.nhaValue);
		ckGhaTint = new JCheckBox (Translator.swap ("SVLox.ghaTint"), set.ghaTint);
		ckGhaValue = new JCheckBox (Translator.swap ("SVLox.ghaValue"), set.ghaValue);
		ckRefined = new JCheckBox (Translator.swap ("SVLox.refined"), set.refined);
		ckDrawU130 = new JCheckBox (Translator.swap ("SVLox.drawUnder130"), set.drawUnder130);
		ckDrawLT1pix = new JCheckBox (Translator.swap ("SVLox.drawLT1pix"), set.drawLT1pix);
		ckCTO_grid = new JCheckBox (Translator.swap ("SVLox.CTO_grid"), set.CTO_grid);
		ckCTO_nogrid = new JCheckBox (Translator.swap ("SVLox.CTO_nogrid"), set.CTO_nogrid);
		ckCompName = new JCheckBox (Translator.swap ("SVLox.compName"), set.compName);

		optionPanel.setLayout (new BoxLayout (optionPanel, BoxLayout.Y_AXIS));
		optionPanel.add (ckCellLines);			// checkboxCellLines
		optionPanel.add (ckSquareCells);
		optionPanel.add (ckGhaTint);
		optionPanel.add (ckCrownProjection);	// checkboxCrownProjection
		optionPanel.add (ckRefined);
		optionPanel.add (ckNhaValue);
		optionPanel.add (ckGhaValue);
		optionPanel.add (ckDrawU130);
		optionPanel.add (ckDrawLT1pix);
		optionPanel.add (ckCTO_grid);
		optionPanel.add (ckCTO_nogrid);
		optionPanel.add (ckCompName);

		// Note: location and size are set in ITool
		revalidate ();
	}

	/**
	 * Extension dynamic compatibility mechanism.
	 * This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof GModel)) {return false;}
			GModel m = (GModel) referent;
			GScene s = ((Step) m.getProject ().getRoot ()).getScene ();
			if (!(s instanceof TreeCollection)) {return false;}
			TreeCollection tc = (TreeCollection) s;
			if (tc.getTrees().isEmpty()) {return false;} // fa-09.07.2021: to allow for scene without any tree
														// fa-09.07.2021: if no tree, return false
														// TODO: !!! to be checked by Francois !!!
			Tree t = tc.getTrees ().iterator ().next ();
			if (!(t instanceof Numberable)) {return false;}
			if (!(t instanceof SpatializedTree)) {return false;}
			//~ if (!(t instanceof SpeciesDefined)) {return false;}
			if (!(t instanceof Speciable)) {return false;}	// fc - 15.11.2004

		} catch (Exception e) {
			Log.println (Log.ERROR, "SVLox.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("SVLox.name");
	}
	
	@Override
	public String getAuthor() {
		return "Ph. Dreyfus";
	}
	
	@Override
	public String getDescription() {
		return Translator.swap("SVLox.description");
	}
	
	@Override
	public String getVersion() {
		return "1.0";
	}
	
	public int getPanel2DXMargin () {return 5;}

	public int getPanel2DYMargin () {return 5;}

	protected void retrieveSettings () {
		settings = new SVLoxSettings ();
		((SVLoxSettings) settings).viewFromAbove = true;
	}

	/**
	 * Memorize options, called when option configuration panel is validated.
	 */
	protected void optionAction () {
		SVLoxSettings set = (SVLoxSettings) settings;
		set.cellLines = ckCellLines.isSelected ();
		set.squareCells = ckSquareCells.isSelected ();
		set.crownProjection = ckCrownProjection.isSelected ();
		set.nhaValue = ckNhaValue.isSelected ();
		set.ghaTint = ckGhaTint.isSelected ();
		set.ghaValue = ckGhaValue.isSelected ();
		set.refined = ckRefined.isSelected ();
		set.drawUnder130 = ckDrawU130.isSelected ();
		set.drawLT1pix = ckDrawLT1pix.isSelected ();
		set.CTO_grid = ckCTO_grid.isSelected ();
		set.CTO_nogrid = ckCTO_nogrid.isSelected ();
		set.compName = ckCompName.isSelected ();
		ExtensionManager.recordSettings (this);
	}

	/**
	 * Action on vertical/horizontal mode button in toolbar.
	 */
	protected void aboveOrSectionalViewAction () {
		SVLoxSettings set = (SVLoxSettings) settings;
		if (set.viewFromAbove) {
			set.viewFromAbove = false;
		} else {
			set.viewFromAbove = true;
		}
		ExtensionManager.recordSettings (this);
	}

	/**
	 * Action on species checkboxes in toolbar.
	 */
	protected void changeDrawnSpecies () {
		SVLoxSettings set = (SVLoxSettings) settings;
		set.drawSp1 = ckSp1.isSelected ();
		set.drawSp2 = ckSp2.isSelected ();
		ExtensionManager.recordSettings (this);
	}

	/**
	 * Update the viewer (for example, when changing step).
	 */
	// == compared to SVSimple : added update of colors when tree color is changed using main option panel
	public void update () {
		super.update ();
		SPECIES_1_COLOR_ALPHA = new Color (getTreeColor ().getRed(), getTreeColor ().getGreen(), getTreeColor ().getBlue(), alpha);
		SPECIES_1_COLOR = getTreeColor ();
	}

//=================================================================================================>
//			Current scale
// 				Si bounds est le rectangle utile en pixels dans lequel on dessine et
// 				userBounds est le Rectangle.Double en metres (formellement : en
// 				coordon�es utilisateur) que l'on cherche � repr�senter dans le premier,
// 				l'�chelle est d�finie comme suit :
// 				double w = bounds.getWidth () / userBounds.getWidth ();
// 				double h = bounds.getHeight () / userBounds.getHeight ();
// 				currentScale = Math.min (w, h);
// 				L'�chelle 1 correspond donc � "n metres dans n pixels en x comme en y".
// 				Le Math.min r�gle "habilement" la question des fenetres utiles non carr�es.
//=================================================================================================>

//=================================================================================================>
//			Shrinkage of tree height (+hcb, crownd) and X coordinates
// in order to have :
//	- for a cell side = 10 m and, for example,  shrink_h = 0.3333 applied to height : a cell height (10 m) <=> tree height of 30 m
//	- as an option, any X coordinate shrinked by the same factor, in order to restore tree relative spacing appearance
//=================================================================================================>
	private double shrink_h = 0.3333;
	private double shrink_x = 1;
	//private double csmin = 0.4;	// min value of current scale, under which several things are not drawn
	private double csmin = 0;	// min value of current scale, under which several things are not drawn

	/**
	 * In Sectional View, selection must intersect a part of the stem
	 * (i.e. a part of the virtual line between the stump and the top).
	 * In View from Above, selection is delegated to superclass' select method.
	 */
	public Collection searchInRectangle (Rectangle.Double r) {
		// 1. Vertical (sectional) mode
		SVLoxSettings set = (SVLoxSettings) settings;
		if (!set.viewFromAbove) {
			if (super.panel2D.getCurrentScale ().x < csmin) {return new ArrayList ();}		// Trees are not drawn if current scale is too low

			// 1.1. If group is set, restrict stand to given group
			Collection trees = ((TreeCollection) stand).getTrees ();
			if (set.grouperMode) {
				String name = set.grouperName;
				GrouperManager gm = GrouperManager.getInstance ();
				Grouper grouper = gm.getGrouper (name);
				try {
					trees = grouper.apply (trees);
				} catch (Exception e) {
					Log.println (Log.ERROR, "SVLox.select ()",
							"Exception while applying grouper "+name+" on stand", e);
					set.grouperMode = false;
					set.grouperName = "";
				}
			}

			// 1.2. Look in restricted stand for selected trees
			Collection selectedTrees = new ArrayList ();
			for (Iterator i = trees.iterator (); i.hasNext ();) {
				SpatializedTree t = (SpatializedTree) i.next ();
				double dbh = t.getDbh ();

				// Selection by the whole stem (from stump to top of the tree) :
				Line2D l2;
				if (t.getCell () instanceof SquareCell) { 	// Si cellule carr�e, tout ramen� sur le bas de la cellule :
					l2 = new Line2D.Double (t.getX ()*shrink_x,
							t.getCell ().getOrigin ().y,
							t.getX ()*shrink_x,
							t.getCell ().getOrigin ().y+t.getHeight ()*shrink_h); // stem to total height (vertical line)
				} else {
					l2 = new Line2D.Double (t.getX ()*shrink_x,
							t.getY (),
							t.getX ()*shrink_x,
							t.getY ()+t.getHeight ()*shrink_h); // stem to total height (vertical line)
				}
				if (r.intersectsLine (l2)) {selectedTrees.add (t);}
			}
			return selectedTrees != null?new ArrayList ():selectedTrees;

		// 2. Horizontal mode
		} else {
			return super.searchInRectangle (r);
		}
	}

	/**
	 * Method to draw a GCell within this viewer.
	 * This method is called for each cell which bounding box intersects the visible area (zoom managed).
	 */
	public void drawCell (Graphics2D g2, Cell gcell, Rectangle.Double r) {

		// fc - 10.5.2001 - Relies on polygon origin

		Rectangle2D bb = gcell.getShape ().getBounds2D ();
		SVLoxSettings set = (SVLoxSettings) settings;

		// 1. Draw Comp (polygon)
		if (gcell instanceof PolygonalCell) {

			float cs = (float) super.panel2D.getCurrentScale ().x;
			//Log.println(" Current Scale : "+cs);

			PolygonalCell comp = (PolygonalCell) gcell;
			if (!comp.isTreeLevel () && set.squareCells) {return;}

			g2.setColor (new Color (255, 0, 0));			// for testing

			Shape sh = comp.getShape ();

			// Fill cell (with G/ha tint if required)
			if (sh.intersects (r)) {
				if (set.ghaTint) {
					try {
						// compute "gray" scales
						double gha = ((CellInformation) comp).getGha ();
	
						float val_0_1 = (float) (Math.sqrt (gha / 100d));
						if (val_0_1 < 0) {System.out.println ("val_0_1 < 0 !! :"); val_0_1 = 0;}
						if (val_0_1 > 1) {System.out.println ("val_0_1 > 1 !! :"); val_0_1 = 1;}
						grayed = new Color (100, 50, 0, (int) (250f*val_0_1));
	
						g2.setColor (grayed);
					} catch (Exception e) {}
				} else {
					g2.setColor (getCellColor ());
				}
				g2.fill (sh);
			}

			// Draw cell border if needed
			if (set.cellLines) {	// fc - 17.2.2003
				if (set.ghaTint) {
					g2.setColor(getCellColor ());
				} else {
					g2.setColor (new Color (255 - getCellColor().getRed(), 255 - getCellColor().getGreen(), 255 - getCellColor().getBlue()));
				}
				Rectangle2D bBox = sh.getBounds2D ();	// for security
				if (r.intersects (bBox)) {g2.draw (sh);}
			}

			// Center of the cell :
			float xc = 0, yc = 0;
			if (set.compName || set.ghaValue || set.nhaValue) {
				Polygon2D polygon = null;
				try {
					polygon = new Polygon2D (comp.getVertices());
				} catch (Exception e) {
					Log.println (Log.ERROR, "Error - Center of the cell", "Exception caught", e);
				}
				Vertex2d center = polygon.getGeometricalCenter();
				xc = (float) center.x;
				yc = (float) center.y;
			}

			// Write cell name
			if (set.compName && super.panel2D.getCurrentScale ().x > csmin) {
				g2.getFont ().deriveFont (Font.PLAIN, 10f);
				g2.setColor (Color.magenta);
				g2.drawString (String.valueOf (((CellInformation) comp).getName ()), xc, yc);
			}

			// Write G/ha value if required
			if (set.ghaValue && super.panel2D.getCurrentScale ().x >= csmin) {
				double gha = ((CellInformation) comp).getGha ();
				g2.setColor (Color.yellow);
				g2.getFont ().deriveFont (Font.ITALIC, 14f);
				g2.drawString (String.valueOf (Math.round (gha)), xc -10/cs, yc - 10/cs);  //  G m� / ha
			}

			// Write N/ha if required
			if (set.nhaValue && super.panel2D.getCurrentScale ().x >=csmin) {
				g2.setColor (NHA_COLOR);
				g2.getFont ().deriveFont (Font.ITALIC, 14f);
				g2.drawString (String.valueOf (((CellInformation) comp).getNha ()), xc + 10/cs, yc - 10/cs);  // Nha
			}

		} else {

			// 2. Draw Cell (square)
			if (!set.squareCells) {return;}

			SquareCell cell = (SquareCell) gcell;
			PolygonalCell comp = (PolygonalCell) cell.getMother ();
			Shape sh = cell.getShape ();	// fc - 10.5.2001

			// compute m_ cell corners coordinates
			// bottom-left origin : (a, b)
			double a = bb.getX ()*shrink_x;
			double b = bb.getY ();

			double w =  bb.getWidth ()*shrink_x;
			double h =  bb.getHeight ();

			double c =  bb.getX () + w;
			double d =  bb.getY () + h;

			// Fill cell with G/ha tint if required
			if (sh.intersects (r)) {	// for security
				if (set.ghaTint && cell instanceof CellInformation) {	// fc - 28.4.2004
					
					// compute "gray" scales
					double gha = ((CellInformation) cell).getGha ();

					float val_0_1 = (float) (Math.sqrt (gha / 100d));
					if (val_0_1 < 0)  {System.out.println ("val_0_1 < 0 !! :"); val_0_1 = 0;}
					if (val_0_1 > 1)  {System.out.println ("val_0_1 > 1 !! :"); val_0_1 = 1;}
					grayed = new Color (100, 50, 0, (int) (250f*val_0_1));

					g2.setColor (grayed);
					
				} else {
					g2.setColor (getCellColor ());
				}
				g2.fill (sh);
			}

			// Draw cell border lines if required
			if (set.cellLines) {
				if (set.ghaTint) {
					g2.setColor(getCellColor ());
				} else {
					g2.setColor (new Color (255 - getCellColor().getRed(), 255 - getCellColor().getGreen(), 255 - getCellColor().getBlue()));
				}

				if (sh.intersects (r)) {g2.draw (sh);}	// for security

				// Draw height ticks
				if (!set.viewFromAbove ) {     // not drawn when trees seen from above
					double cw = w;
					double aa =  (a+cw/25)*shrink_x; // "25" means cell width / 25
					double bd =  b+ 5*shrink_h;		// Note : as X and Y are now in meters, "5" means "5 meters" (whatever the cell width)
					if (5*shrink_h < cw) {g2.draw (new Line2D.Double (a, bd, aa, bd));}
					bd =  b+ 10*shrink_h;
					if (10*shrink_h < cw) {g2.draw (new Line2D.Double (a, bd, aa, bd));}
					bd =  b+ 15*shrink_h;
					if (15*shrink_h < cw) {g2.draw (new Line2D.Double (a, bd, aa, bd));}
					bd =  b+ 20*shrink_h;
					if (20*shrink_h < cw) {g2.draw (new Line2D.Double (a, bd, aa, bd));}
					bd =  b+ 25*shrink_h;
					if (25*shrink_h < cw) {g2.draw (new Line2D.Double (a, bd, aa, bd));}

					aa =  (a+cw/35)*shrink_x;
					bd =  b+ 1*shrink_h;
					if (1*shrink_h < cw) {g2.draw (new Line2D.Double (a, bd, aa, bd));}
					bd =  b+ 2*shrink_h;
					if (2*shrink_h < cw) {g2.draw (new Line2D.Double (a, bd, aa, bd));}
					bd =  b+ 3*shrink_h;
					if (3*shrink_h < cw) {g2.draw (new Line2D.Double (a, bd, aa, bd));}
					bd =  b+ 4*shrink_h;
					if (4*shrink_h < cw) {g2.draw (new Line2D.Double (a, bd, aa, bd));}
				}
			}

			// Write G/ha value if required
			if (set.ghaValue && super.panel2D.getCurrentScale ().x >= csmin) {
				double gha = ((CellInformation) cell).getGha ();
				g2.setColor (Color.yellow);
				g2.getFont ().deriveFont (Font.ITALIC, 14f);
				g2.drawString (String.valueOf (Math.round (gha)),
						(float) ((a+c)/2), (float) ((b+d)/2));  //  G m� / ha
			}

			// Write  N/ha if required
			if (set.nhaValue && super.panel2D.getCurrentScale ().x >= csmin) {
				g2.setColor (NHA_COLOR);
				g2.getFont ().deriveFont (Font.ITALIC, 14f);
				g2.drawString (String.valueOf (((CellInformation) cell).getNha () ),
						(float) (a+0.1*w), (float) (b + h/4));  // Nha
			}
		}	// end if PolygonalCell
	}

	/**
	 * Draw a tree in the viewer according to current options.
	 * This method is called by SVSimple (superclass) draw () method for each tree
	 * that should be drawn (in the currently zoomed area if zoom is used).
	 * However, a few trees may be out of the visible area, so visibility is checked
	 * for each tree (bounds2D intersects visible rectangle).
	 */
	public void drawTree (Graphics2D g2, Tree gtree, Rectangle.Double r) {

		if (super.panel2D.getCurrentScale ().x < csmin) {return;}		// Trees are not drawn if current scale is too low

		// Marked trees are considered dead by generic tools -> don't draw
		if (gtree.isMarked ()) {return;}

		SVLoxSettings set = (SVLoxSettings) settings;
		SpatializedTree tree = (SpatializedTree) gtree;
		
		Spatialized spa = (Spatialized) tree;	// fc - 10.4.2008
		
		
		//~ EnumProperty species = null;
		Species species = null;		// fc - 15.11.2004
		if (tree instanceof Speciable) {	// fc - 28.4.2004 // fc - 15.11.2004
			try {
				//~ species = (EnumProperty) ((SpeciesDefined) tree).getSpecies ();		// fc 27.2.2001
				//~ if (!set.drawSp1 && species.isValue (SPECIES_1)) {return;}		// Species 1 not drawn
				//~ if (!set.drawSp2 && species.isValue (SPECIES_2)) {return;}		// Species 2 not drawn
				species = ((Speciable) tree).getSpecies ();		// fc 27.2.2001
				if (!set.drawSp1 && species.getValue () == SPECIES_1) {return;}		// Species 1 not drawn
				if (!set.drawSp2 && species.getValue () == SPECIES_2) {return;}		// Species 2 not drawn
			} catch (Exception e) {}
		}
		
		if ((!set.drawUnder130) && (tree.getHeight () <= 1.30)) {return;}

		// Tree species
		//
		Color colCrown = Color.BLACK;
		Color colCrownAlpha = Color.GRAY;
		if (tree instanceof Speciable && species != null) {	// fc - 28.4.2004
			if (species.getValue () == SPECIES_1) {
				colCrown = SPECIES_1_COLOR;
			} else {
				colCrown = SPECIES_2_COLOR;			// light gray
			}
			if (species.getValue () == SPECIES_1) {
				colCrownAlpha = SPECIES_1_COLOR_ALPHA;
			} else {
				colCrownAlpha = SPECIES_2_COLOR_ALPHA;			// light gray
			}
		}
		
		// Tree coordinates
		//
		Cell cell = null;
		try {
			cell = tree.getCell ();
		} catch (Exception e) {
			Log.println (Log.ERROR, "SVLox.drawTree ()", "For tree (id="+tree.getId ()
					+"): tree.getCell () throws exception", e);
			return;
		}

		double eph = ((Numberable) tree).getNumber ();	// fc - 22.8.2006 - Numberable returns double

		double cw, cxc, cyc;
		if (cell instanceof SquareCell) {
			SquareCell squarecell = (SquareCell) cell;
			cw = squarecell.getWidth ();
			cxc = squarecell.getXCenter ();
			cyc = squarecell.getYCenter ();
			if (set.CTO_grid) {eph = 1;}
		} else {
			CellInformation polygonalcell = (CellInformation) cell;
			cw = polygonalcell.getPseudoRadius () / 4.0;
			cxc = polygonalcell.getXCenter ();
			cyc = polygonalcell.getYCenter ();
			if (set.CTO_nogrid) {eph = 1;}
		}
		double dx = spa.getX () - cxc;
		double dy = spa.getY () - cyc;
		double fu = 1;
		double xrand, yrand;
		for (int e = 0; e < eph; e++) {
			if (e == 0) {
				xrand = cxc + dx; yrand = cyc + dy;	// in this case, xrand = spa.getX () and yrand = spa.getY ()
			} else {
				xrand = cxc + Math.IEEEremainder ((e*0.3+dx)*(e*1.7+dy),cw);
				yrand = cyc + Math.IEEEremainder ((e*1.3+dx)*(e*0.7+dy),cw);
				fu = (1 - 0.3) + 2 * 0.3 * (e-1)/eph;
			}

			// Scaling tree position
			//
			// Real X :
			double m_X = xrand*shrink_x;
			// Real Y ...  :
			double m_Y;
			if (set.viewFromAbove) {
				m_Y = yrand;
			// ... or Y = cell bottom :
			} else {
				if (tree.getCell () instanceof SquareCell) {
					m_Y = tree.getCell ().getOrigin ().y;	// Si cellule carr�e, tout ramen� sur le bas de la cellule :
				} else {
					m_Y = yrand;
				}
			}

			// Tree size + scaling, shrinkage of heights
			//
			double crownd, ht, dcm, hcb;
			double m_CrownD, m_Hcb, m_Ht, m_RealCrownD, m_RealCrownD_2, m_CrownD_2;
			ht = fu * tree.getHeight ();
			hcb = fu * tree.getHeight () * 0.6667;
			crownd = (ht - hcb) * 0.6667;

			m_Ht = shrink_h * ht;
			m_Hcb =  shrink_h * hcb;
			m_CrownD = shrink_h * crownd;
			m_RealCrownD = crownd;
			m_CrownD_2 = m_CrownD/2;
			m_RealCrownD_2 = m_RealCrownD/2;

			// Tree FROM ABOVE View
			//
			double size = 0;	// fc - detail threshold - 10.2.2003

			if (set.viewFromAbove) {

				// ------- drawing the Stem
				if (!set.crownProjection) {   // when crown projection is not drawn, draw stem diameter
					double diameter = (tree.getDbh () * fu) / 100d;	// dbh in cm.
					if (tree.getDbh () == 0) {diameter = 0.01;}	// if tree < 1.30 m, position is drawn by a circle corresponding to dbh = 10 cm (if not drawUnder130, tree is not drawn)

					size = diameter;
					if (size <= visibleThreshold) {
						if(set.drawLT1pix) {diameter = 2 / panel2D.getCurrentScale ().x;} // less than 1 pixel : draw to 2 pixels
						else {return;}	// less than 1 pixel : do not draw
					}

					g2.setColor (getTreeColor ());
					Shape sh = new Ellipse2D.Double (m_X - diameter/2,
							m_Y - diameter/2, diameter, diameter);	// circle
					Rectangle2D bBox = sh.getBounds2D ();
					if (r.intersects (bBox)) {g2.draw (sh);}

				} else {

					size = m_RealCrownD;
					if (size <= visibleThreshold) {
						if(set.drawLT1pix) {m_RealCrownD = 2 / panel2D.getCurrentScale ().x;} // less than 1 pixel : draw to 2 pixels
						else {return;}	// less than 1 pixel : do not draw
					}

					g2.setColor (colCrownAlpha);
					Shape sh = new Ellipse2D.Double (m_X - m_RealCrownD_2,
							m_Y - m_RealCrownD_2, m_RealCrownD, m_RealCrownD);	// circle
					Rectangle2D bBox = sh.getBounds2D ();
					if (r.intersects (bBox)) {g2.fill (sh);}

					if (set.refined && (ht > 1.30)) {
						//g2.setColor (getTreeColor ());
						g2.setColor (colCrown);
						sh = new Ellipse2D.Double (m_X - m_RealCrownD_2,
								m_Y - m_RealCrownD_2, m_RealCrownD, m_RealCrownD);	// circle
						bBox = sh.getBounds2D ();
						if (r.intersects (bBox)) {g2.draw (sh);}
					}
				}

				// ------- Tree Label - fc - 22.12.2003
				if (e == 0 && set.showLabels && r.contains (new Point.Double (m_X, m_Y))) {
					drawLabel (g2, String.valueOf (tree.getId ()), 
							(float) m_X, (float) m_Y);
				}
				// ------- Tree Label
				//~ if (set.showLabels && size >= detailThreshold) {	// fc
					//~ g2.setColor (getLabelColor ());
					//~ g2.getFont ().deriveFont (Font.PLAIN, 9f);

					//~ if (r.contains (new Point.Double (m_X, m_Y))) {
						//~ g2.drawString (String.valueOf (tree.getId ()), (float) m_X, (float) m_Y);
					//~ }
				//~ }

			// Tree in SECTIONAL VIEW
			//
			} else {  // (viewFromAbove is  false)
				Rectangle2D bBox = null;
				Rectangle2D bBoxi = null;
				Rectangle2D bBoxa = null;

				// -------- drawing the Crown from aside
				if (set.refined && (ht > 1.30)) {		// Nice Crown Shape
					double[] xCrown = new double[5];
					double[] yCrown = new double[5];
					double crwn_025, crwn_050, crwn_0x;

					size = Math.max (m_CrownD, m_Ht-m_Hcb);	// fc
					//if (size <= visibleThreshold) {return;}	// less than 1 pixel : do not draw

					// ------- drawing the Stem
					g2.setColor (DARK_BROWN);							// dark brown

					Line2D l2 = new Line2D.Double (m_X, m_Y, m_X, m_Y+m_Hcb); // stem to hcb (vertical line)
					if (r.intersectsLine (l2)) {g2.draw (l2);}

					Shape s = new Arc2D.Double (m_X - m_CrownD_2,
							m_Y+m_Hcb,m_CrownD, m_Ht-m_Hcb, 0,360, Arc2D.CHORD);
					bBox = s.getBounds2D ();
					if (r.intersects (bBox)) {
						g2.setColor (colCrownAlpha);
						g2.fill (s);
						//g2.setColor (getTreeColor ());
						g2.setColor (colCrown);
						g2.draw (s);
					}

				} else {	// simple full shape without darker envelope (when "refined" is not selected or ht <= 1.30)
					// Ellipse
					size = Math.max (m_CrownD, m_Ht-m_Hcb);	// fc
					//if (size <= visibleThreshold) {return;}	// less than 1 pixel : do not draw

					// ------- drawing the Stem
					g2.setColor (DARK_BROWN);							// dark brown

					Line2D l2 = new Line2D.Double (m_X, m_Y, m_X, m_Y+m_Hcb); // stem to hcb (vertical line)
					if (r.intersectsLine (l2)) {g2.draw (l2);}


					g2.setColor (colCrownAlpha);
					Shape s = new Arc2D.Double (m_X - m_CrownD_2,
							m_Y+m_Hcb,m_CrownD, m_Ht-m_Hcb,0,360, Arc2D.CHORD);
					bBox = s.getBounds2D ();
					if (r.intersects (bBox)) {g2.fill (s);}
				}

				// ------- Tree Label - fc - 22.12.2003
				if (e == 0 && set.showLabels && r.contains (new Point.Double (m_X, m_Y +  m_Ht))) {
					drawLabel (g2, String.valueOf (tree.getId ()), 
							(float) m_X, (float) (m_Y +  m_Ht));
				}
				//~ if (set.showLabels && size >= detailThreshold) {	// fc
					//~ g2.setColor (getLabelColor ());
					//~ g2.getFont ().deriveFont (Font.PLAIN, 9f);
					//~ if (r.contains (new Point.Double (m_X, m_Y +  m_Ht))) {
						//~ g2.drawString (String.valueOf (tree.getId ()), (float) m_X, (float) (m_Y +  m_Ht));
					//~ }
				//~ }
			}
		}
	}


	/**	This disables the SVSimple.draw () labels drawing procedure.
	*	We draw labels in drawTree ().
	*/
	protected boolean showSomeLabels () {return false;}


	/**
	 * Redefine SVSimple.getPilot () method in order to add new button(s)
	  * in the toolbar of SVLox.
	 */
	public JComponent getPilot () {
		/*ImageIcon icon = new IconLoader ().getIcon ("properties_16.png");
		settingsButton = new JButton (icon);
		Tools.setSizeExactly (settingsButton, BUTTON_SIZE, BUTTON_SIZE);
		settingsButton.setToolTipText (Translator.swap ("SVSimple.settings"));
		settingsButton.addActionListener (this);

		icon = new IconLoader ().getIcon ("zoom-out_16.png");
		filteringButton = new JButton (icon);
		Tools.setSizeExactly (filteringButton, BUTTON_SIZE, BUTTON_SIZE);
		filteringButton.setToolTipText (Translator.swap ("Shared.filtering"));
		filteringButton.addActionListener (this);

		icon = new ImageIcon ("../images/svventou_1.gif");
		aboveOrSectionalView = new JButton (icon);
		Tools.setSizeExactly (aboveOrSectionalView, BUTTON_SIZE, BUTTON_SIZE);
		aboveOrSectionalView.setToolTipText (Translator.swap ("SVLox.aboveOrSectionalView"));
		aboveOrSectionalView.addActionListener (this);

		icon = new IconLoader ().getIcon ("help_16.png");
		helpButton = new JButton (icon);
		Tools.setSizeExactly (helpButton, BUTTON_SIZE, BUTTON_SIZE);
		helpButton.setToolTipText (Translator.swap ("Shared.help"));
		helpButton.addActionListener (this);

		SVLoxSettings set = (SVLoxSettings) settings;
		ckSp1 = new JCheckBox (Translator.swap ("SVLox.drawSp1"), set.drawSp1);
		ckSp1.addActionListener (this);
		ckSp2 = new JCheckBox (Translator.swap ("SVLox.drawSp2"), set.drawSp2);
		ckSp2.addActionListener (this);

		JToolBar toolbar = new JToolBar ();
		toolbar.add (settingsButton);
		toolbar.add (filteringButton);
		toolbar.add (aboveOrSectionalView);
		toolbar.add (ckSp1);
		toolbar.add (ckSp2);
		toolbar.add (helpButton);
		toolbar.setVisible (true);*/

		JToolBar toolbar = (JToolBar) super.getPilot ();

		ClassLoader cldr = this.getClass().getClassLoader();
		URL imageURL   = cldr.getResource("capsis/extension/standviewer/svventou_1.gif");
		ImageIcon icon = new ImageIcon (imageURL);
		
		aboveOrSectionalView = new JButton (icon);
		Tools.setSizeExactly (aboveOrSectionalView, BUTTON_SIZE, BUTTON_SIZE);
		aboveOrSectionalView.setToolTipText (Translator.swap ("SVLox.aboveOrSectionalView"));
		aboveOrSectionalView.addActionListener (this);
		toolbar.add (aboveOrSectionalView);

		SVLoxSettings set = (SVLoxSettings) settings;
		ckSp1 = new JCheckBox (Translator.swap ("SVLox.drawSp1"), set.drawSp1);
		ckSp1.addActionListener (this);
		ckSp2 = new JCheckBox (Translator.swap ("SVLox.drawSp2"), set.drawSp2);
		ckSp2.addActionListener (this);
		toolbar.add (ckSp1);
		toolbar.add (ckSp2);

		return toolbar;
	}

	/**
	 * Redefine SVSimple.actionPerformed () method in order to manage new button(s)
	  * in the toolbar of SVLox.
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (aboveOrSectionalView)) {
			aboveOrSectionalViewAction ();
			panel2D.invalidate ();
			update ();

		} else if (evt.getSource ().equals (ckSp1)) {
			changeDrawnSpecies ();
			panel2D.invalidate ();
			update ();

		} else if (evt.getSource ().equals (ckSp2)) {
			changeDrawnSpecies ();
			panel2D.invalidate ();
			update ();

		} else {
			super.actionPerformed (evt);	// manage option, help & filtering buttons
		}
	}


	// Test method. When activated, selection traces neighbourhoods in console.
	// fc - 24.2.2003
	//
	private void traceNeighbourhoods (SVLoxSettings set, Rectangle.Double r) {

		// TEST - discard in normal use - fc - 21.2.2003
		// TEST - discard in normal use - fc - 21.2.2003
		// TEST - discard in normal use - fc - 21.2.2003
		// TEST - discard in normal use - fc - 21.2.2003

		// 1.1. If group is set, restrict stand to given group
		Collection trees = ((TreeCollection) stand).getTrees ();
		if (set.grouperMode) {
			String name = set.grouperName;
			GrouperManager gm = GrouperManager.getInstance ();
			Grouper grouper = gm.getGrouper (name);
			try {
				trees = grouper.apply (trees);
			} catch (Exception e) {
				Log.println (Log.ERROR, "SVLox.select ()",
						"Exception while applying group "+name+" on stand", e);
				set.grouperMode = false;
				set.grouperName = "";
			}
		}

		// get one tree
		Iterator i = trees.iterator (); i.hasNext ();

		SpatializedTree tree = null;
		boolean found = false;
		while (i.hasNext () && !found) {
			SpatializedTree t = (SpatializedTree) i.next ();
			Point.Double p = new Point.Double (t.getX (), t.getY ());
			if (r.contains (p)) {
				tree = t;
				found = true;
			}
		}

		if (tree != null) {

			System.out.println ("-- Neighbourhood for tree "+tree.getId ());

			// consider 1st comp
			try {
				SquareCellHolder comp = (SquareCellHolder) tree.getCell ().getMother ();

				RoundMask m10 = new RoundMask (comp, 10, true);

				Collection c = m10.getTreesNear (tree);

				System.out.println ("neighbours m10=");
				for (Iterator i1 = c.iterator (); i1.hasNext ();) {
					SpatializedTree t = (SpatializedTree) i1.next ();
					System.out.print (" t."+t.getId ());
				}
				System.out.println ();

				RoundMask m25 = new RoundMask (comp, 25, true);

				c = null;
				c = m25.getTreesNear (tree);

				System.out.println ("neighbours m25=");
				for (Iterator i1 = c.iterator (); i1.hasNext ();) {
					SpatializedTree t = (SpatializedTree) i1.next ();
					System.out.print (" t."+t.getId ());
				}
				System.out.println ();

			} catch (Exception e) {
				System.out.println ("exception: "+e);
			}
		}
	}


}
