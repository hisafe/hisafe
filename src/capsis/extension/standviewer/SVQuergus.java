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
package capsis.extension.standviewer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.Arc2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.border.Border;

import capsis.commongui.projectmanager.StepButton;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.lib.samsaralight.SLCompass;
import capsis.lib.samsaralight.SLLightableModel;
import capsis.lib.samsaralight.SLSettings;
import capsis.util.FourCrownRadiusTree;
import capsis.util.SceneWithFourCrownRadiusTree;
import capsis.util.TreeHeightComparator;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Translator;

/**
 * Stand visualisator to see crown ascimentry and understory light. SVQuergus is
 * a generic viewer in capsis.extension.standviewer.
 * 
 * @author G. Ligot inspired from SVSimple written by F. de Coligny
 */
public class SVQuergus extends SVSimple {

	// fc-22.10.2021 Adapted to use only generic classes, see
	// SceneWithFourCrownRadiusTree and FourCrownRadiusTree

	static {
		Translator.addBundle("capsis.extension.standviewer.SVQuergus");
	}

	// fc-22.10.2021 Optional, can be null, if set, appears in the legend
	private JComponent externalLegend;

	// nb-07.08.2018
	// static public String AUTHOR = "G. Ligot";
	// static public String VERSION = "1";

	@Override
	public void init(GModel model, Step s, StepButton but) throws Exception {
		super.init(model, s, but);

		try {
			updateLegend();
		} catch (Exception e) {
			Log.println(Log.ERROR, "SVQuergus ()", "Error in constructor - legend construction", e);
			throw e; // propagate
		}

	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks if
	 * the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {

		try {

			// fc-22.10.2021 Adapted matchWith () to use only generic classes (we are
			// in capsis.extension.standviewer, we can not refer to classes in models which
			// could be omitted in an installer, e.g. Ecoaf + Lilo without Quergus)

			if (!(referent instanceof GModel))
				return false;

			GModel m = (GModel) referent;
			GScene scene = m.getProject().getRoot().getScene();
			return (scene instanceof TreeList && scene instanceof SceneWithFourCrownRadiusTree);

//			if (!((referent instanceof QGModel) || (referent instanceof LiloModel))) {
//				return false;
//			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "SVQuergus.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}

	@Override
	public String getName() {
		return Translator.swap("SVQuergus.name");
	}

	@Override
	public String getAuthor() {
		return "G. Ligot";
	}

	@Override
	public String getDescription() {
		return Translator.swap("SVQuergus.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	/**
	 * Called before whole trees drawing process. May be redefined in subclasses.
	 */
	@Override
	public Object[] preProcessTrees(Object[] trees, Rectangle.Double r) {
		// Sort the trees in height order
//		Arrays.sort(trees, new TreeHeightComparator(!settings.showTransparency)); //CANNOT PUT SMALL TREE BENEATH BIGGER ONES
		Arrays.sort(trees, new TreeHeightComparator(true));
		return trees;
	}

	// fc-22.10.2021 Optional, to add a legend when used in another component (e.g.
	// EcoafRadiativeBalance)
	public void addExternalLegend(JComponent externalLegend) {
		this.externalLegend = externalLegend;
	}

	private void updateLegend() {
		ColumnPanel legend = new ColumnPanel(2, 0);

		// colors
		Border etched = BorderFactory.createEtchedBorder();
		Border border1 = BorderFactory.createTitledBorder(etched, Translator.swap("legend"));
		legend.setBorder(border1);

		// compass and slope
		SLSettings slsets = ((SLLightableModel) model).getSLModel().getSettings();
		SLCompass compass = new SLCompass(slsets.getPlotAspect_deg(),
				slsets.getNorthToXAngle_cw_deg() - slsets.getPlotAspect_deg());
		legend.add(compass);

		// light colors
		// TODO
		
		// fc-22.10.2021 External legend (optional)
		if (externalLegend != null)
			legend.add(externalLegend);

		legend.addGlue();

		legend.setOpaque(false);

		JPanel aux = new JPanel(new BorderLayout());
		aux.add(legend, BorderLayout.NORTH);
		setLegend(aux);

	}

	/**
	 * Method to draw a Spatialized Tree within this viewer. Only rectangle r is
	 * visible (user coordinates) -> do not draw if outside. May be redefined in
	 * subclasses. Tree label is managed in draw () method, deals with max labels
	 * number for performance.
	 */
	@Override
	public void drawTree(Graphics2D g2, Tree tree, Rectangle.Double r) {

		// 1. Marked trees are considered dead by generic tools -> don't draw
		if (tree.isMarked()) {
			return;
		}
		if (tree instanceof Numberable && ((Numberable) tree).getNumber() <= 0) {
			return;
		} // fc - 18.11.2004

		Spatialized s = (Spatialized) tree;

		// 2. Tree location
		double x = s.getX();
		double y = s.getY();
		double width = tree.getDbh() / 100; // in m. (we draw in Graphics in m.)
		int mf = magnifyFactor;

		// 3. A detailed view is requested
		//
		if (settings.showDiameters) {

			// 3.1 In some cases, a tree crown can be drawn
			//
			Color crownColor = Tools.getCrownColor(tree);

			// fc-22.10.2021 Now more generic
			// See matchWith ()
			FourCrownRadiusTree t4r = (FourCrownRadiusTree) tree;
			double radius = t4r.getCrownRadius();
			double rymax = t4r.getCrownRadiusNorth();
			double rxmax = t4r.getCrownRadiusEast();
			double rymin = t4r.getCrownRadiusSouth();
			double rxmin = t4r.getCrownRadiusWest();

//			QGTree qgt;
//			LiloTree llt;
//			double crownRadius = 0;
//			
//			double radius = 0;
//			double rymax = 0;
//			double rxmax = 0;
//			double rymin = 0;
//			double rxmin = 0;
//
//			if (model instanceof QGModel) {
//				qgt = (QGTree) tree;
//
//				radius = qgt.getCrownRadius();
//				rymax = qgt.getCrownRadiusN();
//				rxmax = qgt.getCrownRadiusE();
//				rymin = qgt.getCrownRadiusS();
//				rxmin = qgt.getCrownRadiusW();
//
//			} else if (model instanceof LiloModel) {
//				llt = (LiloTree) tree;
//
//				radius = llt.getCrownRadius();
//				rymax = llt.getCrownRadiusYMax();
//				rxmax = llt.getCrownRadiusXMax();
//				rymin = llt.getCrownRadiusYMin();
//				rxmin = llt.getCrownRadiusXMin();
//			}
//
//			if (settings.showTransparency) {
//				int alpha = 50;
//				int red = crownColor.getRed();
//				int green = crownColor.getGreen();
//				int blue = crownColor.getBlue();
//				crownColor = new Color(red, green, blue, alpha);
//			}

			double d = radius * 2; // crown diameter

			// 3.1.1 Crown diameter is less than 1 pixel : draw 1 pixel
			//
			if (d <= visibleThreshold) {
				if (r.contains(new java.awt.geom.Point2D.Double(x, y))) {
					Rectangle2D.Double p = new Rectangle2D.Double(x, y, visibleThreshold, visibleThreshold); // fc -
					// 15.12.2003
					// (bug by
					// PhD)
					g2.setColor(crownColor);
					g2.fill(p);
				}

				// 3.1.2 Bigger than 1 pixel : fill 4 piece of ellipse
				//
			} else {

				List<Shape> shapes = new ArrayList();

				shapes.add(new Arc2D.Double(x - rxmax, y - rymin, 2 * rxmax, 2 * rymin, 0, 90, Arc2D.PIE)); // north-west
				shapes.add(new Arc2D.Double(x - rxmin, y - rymin, 2 * rxmin, 2 * rymin, 90, 90, Arc2D.PIE)); // west-south
				shapes.add(new Arc2D.Double(x - rxmin, y - rymax, 2 * rxmin, 2 * rymax, 180, 90, Arc2D.PIE)); // south-east
				shapes.add(new Arc2D.Double(x - rxmax, y - rymax, 2 * rxmax, 2 * rymax, 270, 90, Arc2D.PIE)); // east-north

//				shapes.add (new Arc2D.Double(x - rxmax, y - rymax, 2*rxmax, 2*rymax, 0, 90, Arc2D.PIE)); //north-west
//				shapes.add (new Arc2D.Double(x - rxmin, y - rymax, 2*rxmin, 2*rymax, 90, 90, Arc2D.PIE)); //west-south
//				shapes.add (new Arc2D.Double(x - rxmin, y - rymin, 2*rxmin, 2*rymin, 180, 90, Arc2D.PIE));	//south-east
//				shapes.add (new Arc2D.Double(x - rxmax, y - rymin, 2*rxmax, 2*rymin, 270, 90, Arc2D.PIE));	//east-north

				for (Shape sh : shapes) {
					shapeMap.addObject(tree, sh);
					Rectangle2D bBox = sh.getBounds2D();
					if (r.intersects(bBox)) {
						g2.setColor(crownColor);
						g2.fill(sh);
						g2.setColor(crownColor.darker());
						g2.draw(sh); // could be deleted
					}
				}

			}
		}

		width = width * mf; // in m. (we draw in Graphics in m.)
		width = Math.max(visibleThreshold, width); // fc - 18.11.2004

		// Draw the trunk : always and after the crown : visible
		if (r.contains(new java.awt.geom.Point2D.Double(x, y))) {
			Shape p = new Ellipse2D.Double(x - width / 2, y - width / 2, width, width); // fc - 18.11.2004
			g2.setColor(getTreeColor());
			g2.fill(p);
		}
	}

}
