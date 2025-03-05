/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2001-2012  Francois de Coligny
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

package capsis.extension.standviewer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.Ellipse2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.JCheckBox;

import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.projectmanager.StepButton;
import capsis.defaulttype.Spatializer;
import capsis.defaulttype.TreeAvatar;
import capsis.defaulttype.TreeList;
import capsis.kernel.GModel;
import capsis.kernel.Step;

/**
 * SVAvatars is a cartography viewer for TreeLists. The TreeList may contain
 * individual trees, in this case one avatar will be drawn for each tree. It may
 * also contain dbh classes (instances of Numberable), in this case, several
 * avatars will be drawn for one class (depending on class.getNumber ()). This
 * viewer is compatible with the models which model class is instanceof
 * Spatializer. This means it can turn a TreeList into a list of spatialized
 * individual avatars.
 * 
 * @author F. de Coligny - February 2012
 */
public class SVAvatars extends SVSimple {

	static {
		Translator.addBundle("capsis.extension.standviewer.SVAvatars");
	}

	// nb-07.08.2018
	// static public final String NAME = Translator.swap("SVAvatars");
	// static public final String DESCRIPTION =
	// Translator.swap("SVAvatars.description");
	// static public String AUTHOR = "F. de coligny";
	// static public String VERSION = "1.0";

	private Spatializer spatializer;

	private JCheckBox dbhInsteadOfCrowns;

	/**
	 * Constructor
	 */
	public SVAvatars() {
	}

	/**
	 * Init function
	 */
	@Override
	public void init(GModel model, Step s, StepButton but) throws Exception {
		super.init(model, s, but);

		this.spatializer = (Spatializer) model;

		// Extra configuration
		optionPanel = new ColumnPanel();

		ColumnPanel c1 = new ColumnPanel();

		LinePanel l1 = new LinePanel();
		dbhInsteadOfCrowns = new JCheckBox(Translator.swap("SVAvatars.dbhInsteadOfCrowns"));
		dbhInsteadOfCrowns.addActionListener(this);
		l1.add(dbhInsteadOfCrowns);
		l1.addGlue();

		c1.add(l1);

		c1.addGlue();

		optionPanel.add(c1, BorderLayout.NORTH);

	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {

		try {
			GModel m = (GModel) referent;
			return m instanceof Spatializer;

		} catch (Exception e) {
			Log.println(Log.ERROR, "SVAvatars.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}

	@Override
	public String getName() {
		return Translator.swap("SVAvatars.name");
	}

	@Override
	public String getAuthor() {
		return "F. de coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("SVAvatars.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	// /**
	// * Used for the settings and filtering buttons.
	// */
	// public void actionPerformed(ActionEvent evt) {
	// super.actionPerformed(evt);
	//
	// if (evt.getSource ().equals())
	//
	// }

	/**
	 * From Drawer interface. This method draws in the Panel2D each time the
	 * latter must be repainted. The given Rectangle is the sub-part (zoom) of
	 * the stand to draw in user coordinates (i.e. meters...). It can be used in
	 * preprocesses to avoid drawing invisible trees or cells.
	 */
	public void draw(Graphics g, Rectangle.Double r) {

		Graphics2D g2 = (Graphics2D) g;

		// Added this for cases where there is a DefaultPlot
		g2.setColor(Color.LIGHT_GRAY);
		g2.draw(userBounds);

		// 2. Choose a pixel detailThreshold, compute it in meters with current
		// scale
		// if dbh in m. >= detailThreshold -> detailled level is reached
		visibleThreshold = 1.1 / panel2D.getCurrentScale().x; // 1 pixel in in
																// meters
		TreeList treeList = (TreeList) stand;
		List<TreeAvatar> avatars = spatializer.getAvatars(treeList);

		// Sort to draw the small trees first

		// fc-1.7.2020 Added a control: if avatar ids are not correctly set (not
		// unique), sorting may lose avatars
		int counterBeforeSorting = avatars.size();
		
		// ascending = true
		TreeSet<TreeAvatar> sortedSet = new TreeSet<TreeAvatar>(new AvatarHeightThenIdComparator(true)); 
		sortedSet.addAll(avatars);
		
		// fc-1.7.2020
		int counterAfterSorting = sortedSet.size();
		if (counterBeforeSorting != counterAfterSorting) {
			System.out.println(
					"SVAvatars.draw () Caution: sorting lost avatars, check their height and ids #avatars before: "
							+ counterBeforeSorting + ",  #avatars after: " + counterAfterSorting);
		}

		shapeMap.clear();

//		System.out.println("SVAvatars.draw()...");
//		System.out.println("SVAvatars userBounds: "+userBounds);
		
		for (TreeAvatar a : sortedSet) {

			double radius = a.getCrownRadius();
			Color color = a.getCrownColor();
			if (dbhInsteadOfCrowns.isSelected()) {
				radius = a.getDbh() / 200d;
				color = a.getTrunkColor();
			}

//			System.out.println("SVAvatars id: "+a.getId()+" ,dbh: "+a.getDbh()+", x,y: "+a.getX()+","+a.getY ());
			
			double d = radius * 2; // crown diameter
			Shape sh = new Ellipse2D.Double(a.getX() - radius, a.getY() - radius, d, d);

			g2.setColor(color);
			g2.fill(sh);

			g2.setColor(color.darker());
			g2.draw(sh);

			shapeMap.addObject(a, sh);

		}

	}

	/**
	 * This methods returns the object selected by the given rectangle. Called
	 * by select (Rectangle, boolean) in SVSimple superclass.
	 */
	protected Collection<Object> searchInRectangle(Rectangle.Double r) {

		Collection<Object> inRectangle = new ArrayList<Object>();

		if (r == null)
			return inRectangle; // null rectangle: nothing found

		for (Object o : shapeMap.keySet()) {
			Set<Shape> shapes = shapeMap.get(o);
			if (shapes != null) {
				for (Shape sh : shapes) {
					if (sh.intersects(r)) {
						inRectangle.add(o);
						break;
					}
				}
			}
		}

		return inRectangle;
	}

	/**
	 * A comparator to sort TreeAvatars instances on ascending or descending
	 * height.
	 */
	static private class AvatarHeightThenIdComparator implements Comparator {

		private boolean ascending;

		public AvatarHeightThenIdComparator() {
			this.ascending = true;
		}

		public AvatarHeightThenIdComparator(boolean ascending) {
			this.ascending = ascending;
		}

		public int compare(Object o1, Object o2) throws ClassCastException {
			if (!(o1 instanceof TreeAvatar)) {
				throw new ClassCastException("Object is not a TreeAvatar : " + o1);
			}
			if (!(o2 instanceof TreeAvatar)) {
				throw new ClassCastException("Object is not a TreeAvatar : " + o2);
			}

			TreeAvatar a1 = ((TreeAvatar) o1);
			TreeAvatar a2 = ((TreeAvatar) o2);

			double h1 = a1.getHeight();
			double h2 = a2.getHeight();

			if (h1 < h2) {
				return ascending ? -1 : 1; // asc : t1 < t2
			} else if (h1 > h2) {
				return ascending ? 1 : -1; // asc : t1 > t2
			} else {

				// fc-1.7.2020 2nd sort on x is not enough, replace by id !
				if (a1.getId() < a2.getId()) {
					return ascending ? -1 : 1;
				} else if (a1.getId() > a2.getId()) {
					return ascending ? 1 : -1;
				} else {
					return 0; // same height and same id: should not occur
				}

				// if (a1.getX () < a2.getX ()) {
				// return ascending ? -1 : 1;
				// } else if (a1.getX () > a2.getX ()) {
				// return ascending ? 1 : -1;
				// } else {
				// return 0;
				// }

			}
		}

	}

	// ---------------------------

	// protected void retrieveSettings() {
	//
	// settings = new SVAvatarsSettings();
	//
	// }

	/**
	 * Optional, do something before the trees are drawn.
	 */
	// public Object[] preProcessTrees(Object[] trees, Rectangle.Double r) {
	//
	// // Sort the trees in height order
	// if (((SVAvatarsSettings) settings).ascendingSort) {
	// Arrays.sort(trees, new TreeHeightComparator());
	// }
	//
	// // Prepare colors for drawTree ()
	// int crownOption = ((SVAvatarsSettings) settings).crownView;
	//
	// if (crownOption == SVAvatarsSettings.TRANSPARENT) {
	// int alpha = ((SVAvatarsSettings) settings).alphaValue;
	// colorT0 = new Color(0, 170, 0, alpha);
	// colorT1 = new Color(0, 0, 170, alpha);
	// colorT2 = new Color(170, 0, 0, alpha);
	// }
	// return trees;
	// }

	/**
	 * Method to draw a GCell within this viewer.
	 */
	// public void drawCell(Graphics2D g2, Cell gcell, Rectangle.Double r) {
	//
	// SVSamCell cell = (SVSamCell) gcell;
	//
	// ButtonModel selection = cellColorGroup.getSelection();
	// RGBManager man = cellRGBMap.get(selection);
	// cell.setRGB(man.getRGB(cell));
	//
	// // if (showLight.isSelected ()) {
	// //
	// // cell.setRGB (lightRGBManager.getRGB (cell));
	// //
	// // } else if (showRegeneration.isSelected ()) {
	// //
	// // cell.setRGB (regeRGBManager.getRGB (cell));
	// //
	// // }
	//
	// Shape sh = gcell.getShape();
	// Rectangle2D bBox = sh.getBounds2D();
	// if (r.intersects(bBox)) {
	// int[] rgb = cell.getRGB();
	// g2.setColor(new Color(rgb[0], rgb[1], rgb[2]));
	// g2.fill(sh);
	// }
	//
	// // Draw cell lines if required
	// if (((SVAvatarsSettings) settings).cellLines) {
	// g2.setColor(getCellColor());
	// if (r.intersects(bBox)) {
	// g2.draw(sh);
	// }
	// }
	// }

	// /**
	// * Method to draw a Spatialized Tree within this viewer.
	// */
	// public void drawTree(Graphics2D g2, Tree t, Rectangle.Double r) {
	// // Marked trees are considered dead by generic tools -> don't draw
	// if (t.isMarked()) {
	// return;
	// }
	//
	// Spatialized s = (Spatialized) t; // fc - 10.4.2008
	//
	// // New rendering hints for clean outputs (for a paper)
	// // g2.setRenderingHint (RenderingHints.KEY_ANTIALIASING,
	// // RenderingHints.VALUE_ANTIALIAS_ON);
	//
	// double width = 0.1; // 10 cm.
	// double x = s.getX();
	// double y = s.getY();
	//
	// // fc - 26.11.2008
	// SVSamTree tree = (SVSamTree) t;
	//
	// int speCode = tree.getSpecies().getValue();
	//
	// // Colors were prepared in preProcess ()
	// if (speCode == 0) {
	// g2.setColor(COLORSP0L1);
	// } else if (speCode == 1) {
	// g2.setColor(COLORSP1L1);
	// } else {
	// g2.setColor(COLORSP2L1);
	// }
	//
	// // Shall we draw the trunk ?
	// if (((SVSimpleSettings) settings).showDiameters) {
	// width = tree.getDbh() / 100;
	//
	// Shape sh = new Ellipse2D.Double(x - width / 2, y - width / 2,
	// width, width);
	// Rectangle2D bBox = sh.getBounds2D();
	// if (r.intersects(bBox)) {
	// g2.fill(sh);
	// }
	// }
	//
	// // How to draw the crown ?
	// width = 2 * tree.getCrownRadius();
	//
	// int crownOption = ((SVAvatarsSettings) settings).crownView;
	//
	// if (crownOption == SVAvatarsSettings.NONE) {
	// // Do nothing for the crown.
	//
	// } else if (crownOption == SVAvatarsSettings.OUTLINED) {
	//
	// Shape sh = new Ellipse2D.Double(x - width / 2, y - width / 2,
	// width, width);
	// Rectangle2D bBox = sh.getBounds2D();
	// if (r.intersects(bBox)) {
	// g2.draw(sh);
	// }
	//
	// } else if (crownOption == SVAvatarsSettings.FILLED) {
	// // Colors were prepared in preProcess ()
	// int layer = tree.getLayer();
	// if (layer == 4) {
	// if (speCode == 0) {
	// g2.setColor(COLORSP0L4);
	// } else if (speCode == 1) {
	// g2.setColor(COLORSP1L4);
	// } else {
	// g2.setColor(COLORSP2L4);
	// }
	// } else if (layer == 3) {
	// if (speCode == 0) {
	// g2.setColor(COLORSP0L3);
	// } else if (speCode == 1) {
	// g2.setColor(COLORSP1L3);
	// } else {
	// g2.setColor(COLORSP2L3);
	// }
	// } else if (layer == 2) {
	// if (speCode == 0) {
	// g2.setColor(COLORSP0L2);
	// } else if (speCode == 1) {
	// g2.setColor(COLORSP1L2);
	// } else {
	// g2.setColor(COLORSP2L2);
	// }
	// } else {
	// if (speCode == 0) {
	// g2.setColor(COLORSP0L1);
	// } else if (speCode == 1) {
	// g2.setColor(COLORSP1L1);
	// } else {
	// g2.setColor(COLORSP2L1);
	// }
	// }
	//
	// Shape sh = new Ellipse2D.Double(x - width / 2, y - width / 2,
	// width, width);
	// Rectangle2D bBox = sh.getBounds2D();
	// if (r.intersects(bBox)) {
	// g2.fill(sh);
	// }
	//
	// } else if (crownOption == SVAvatarsSettings.TRANSPARENT) {
	//
	// // Colors were prepared in preProcess ()
	// if (speCode == 0) {
	// g2.setColor(colorT0);
	// } else if (speCode == 1) {
	// g2.setColor(colorT1);
	// } else {
	// g2.setColor(colorT2);
	// }
	//
	// Shape sh = new Ellipse2D.Double(x - width / 2, y - width / 2,
	// width, width);
	// Rectangle2D bBox = sh.getBounds2D();
	// if (r.intersects(bBox)) {
	// g2.fill(sh);
	// }
	// }
	//
	// // A label
	// drawLabel(g2, String.valueOf(tree.getId()), (float) x, (float) y);
	//
	// }

}
