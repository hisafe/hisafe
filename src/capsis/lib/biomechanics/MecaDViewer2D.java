/*
 * Biomechanics library for Capsis4.
 *
 * Copyright (C) 2001-2003  Philippe Ancelin.
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

package capsis.lib.biomechanics;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;

import capsis.commongui.util.Helper;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.util.Drawer;
import capsis.util.Panel2D;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Translator;

/**
 * MecaDViewer2D - Interface to view trees top displacements.
 *
 * @author Ph. Ancelin - october 2001
 */
public class MecaDViewer2D extends AmapDialog implements ActionListener, Drawer {
//checked for c4.1.1_08 - fc - 3.2.2003

	private MecaPanel2D panel2D;
	protected JPanel mainPanel;
	protected JScrollPane scrollPane;

	private JButton close;
	private JButton help;

	private MecaProcess mecaProcess;
	private boolean opened;

	static {
		Translator.addBundle("capsis.lib.biomechanics.MecaDViewer2D");
	}


	/**
	 * Constructor.
	 */
	public MecaDViewer2D (AmapDialog parent, MecaProcess mecaProcess, Step step) {
		super (parent);
		setTitle (Translator.swap ("MecaDViewer2D"));

		this.mecaProcess = mecaProcess;
		GScene stand = step.getScene ();

		double x, y, width, height;
		Rectangle.Double r2 = null;
		x = stand.getOrigin ().x;
		y = stand.getOrigin ().y;
		width = stand.getXSize ();
		height = stand.getYSize ();
		r2 = new Rectangle.Double (x, y, width, height);
		panel2D = new MecaPanel2D (this, r2, getPanel2DXMargin (), getPanel2DYMargin (), "viewer");

		opened = true;

		createUI ();
		// location is set by AmapDialog
		//~ setBounds(504, 248, 500, 500);	// fc - 31.1.2003
		
		setModal (false);
		pack ();
		show ();
		//setBounds(450, 200, 500, 500);
	}

	public boolean isOpened () {return opened;}

	/**
	 * Manage gui events.
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (close)) {
			//if (Question.isTrue (Translator.swap ("MecaDViewer2D.confirm"),
			//		Translator.swap ("MecaDViewer2D.confirmClose"))) {
			dispose ();
			//}
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);	// fc - 3.2.2003
			//~ MecaDHelp helpDialog = new MecaDHelp (Translator.swap ("MecaDViewer2D.helpViewer2D"),
					//~ Translator.swap ("MecaDViewer2D.helpDialog"),
					//~ 520, 420);
		}
	}

	/**
	 * Dispose the box.
	 */
	public void dispose () {
		super.dispose ();
		if (panel2D != null) {
			panel2D.dispose ();
			panel2D = null;
		}
		mainPanel = null;
		scrollPane = null;
		opened = false;
	}

	// Manage tool tip.
	//
	private void updateToolTipText () {
		String toolTipText = Translator.swap ("MecaDViewer2D.coordinates");
		panel2D.setToolTipText (toolTipText);
	}

	public int getPanel2DXMargin () {return Panel2D.X_MARGIN_IN_PIXELS;}

	public int getPanel2DYMargin () {return Panel2D.Y_MARGIN_IN_PIXELS;}

	/**
	 * Draw a mecaTree.
	 */
	public void drawTree (Graphics2D g2, Rectangle.Double r, MecaTree tree) {

		double width = tree.getDbh () / 100;
		g2.setColor (Color.green);
		double x = tree.getX ();
		double y = tree.getY ();
		// An oval for the tree
		Shape sh1 = new Ellipse2D.Double (x-width/2, y-width/2, width, width);
		Rectangle2D bBox = sh1.getBounds2D ();
		if (r.intersects (bBox)) {
			g2.draw (sh1);
			g2.fill (sh1);
		}

		/*g2.setColor (new Color (150, 75, 0));
		for (Iterator i = tree.getMecaGUs ().iterator (); i.hasNext ();) {
			MecaGU gu = (MecaGU) i.next ();
			width = gu.getDiameter () / 100;
			x = gu.getXTop ();
			y = gu.getYTop ();
			// An oval for the tree
			sh1 = new Ellipse2D.Double (x-width/2, y-width/2, width, width);
			bBox = sh1.getBounds2D ();
			if (r.intersects (bBox)) {
				g2.fill (sh1);
			}
		}*/

		// Vector from tree base to tree top
		g2.setColor (Color.red);
		double xTop = tree.getXTop ();
		double yTop = tree.getYTop ();

		double rWidth = Math.abs (xTop - x);
		if (rWidth == 0d) {
			rWidth = 0.01;
		}
		double rHeight = Math.abs (yTop - y);
		if (rHeight == 0d) {
			rHeight = 0.01;
		}

		Shape sh2 = new Rectangle2D.Double ();
		if (xTop>=x && yTop>y) {
			sh2 = new Rectangle2D.Double (x, yTop, rWidth, rHeight);
		}
		if (xTop<x && yTop>y) {
			sh2 = new Rectangle2D.Double (xTop, yTop, rWidth, rHeight);
		}
		if (xTop<x && yTop<=y) {
			sh2 = new Rectangle2D.Double (xTop, y, rWidth, rHeight);
		}
		if (xTop>=x && yTop<=y) {
			sh2 = new Rectangle2D.Double (x, y, rWidth, rHeight);
		}

		bBox = sh2.getBounds2D ();
		sh2 = new Line2D.Double (x, y, xTop, yTop);
		if (r.intersects (bBox)) {
			g2.draw (sh2);
		}

		width = 0.2;
		Shape sh3 = new Ellipse2D.Double (xTop-width/2, yTop-width/2, width, width);
		bBox = sh3.getBounds2D ();
		if (r.intersects (bBox)) {
			g2.fill (sh3);
		}

		// A label in detail threshold is reached
		g2.setColor (Color.blue);
		if (r.contains (new Point.Double (x, y))) {
			String label = String.valueOf (tree.getId ());
			if (tree.isNoDamage ()) {
				label += " (ND)";
			} else if (tree.isStemBreakage ()) {
				label += " (SB)";
			} else if (tree.isWindThrow ()) {
				label += " (WT)";
			}
			g2.drawString (label, (float) x, (float) y);
		}
	}

	/**
	 * Draw mecaTrees.
	 */
	public void draw (Graphics g, Rectangle.Double r) {
		Graphics2D g2 = (Graphics2D) g;
		double xmin = Double.MAX_VALUE;
		double ymin = Double.MAX_VALUE;
		double xmax = Double.MIN_VALUE;
		double ymax = Double.MIN_VALUE;
		for (Iterator i = mecaProcess.getTreeIds ().iterator (); i.hasNext ();) {
			Integer integerId = (Integer) i.next ();
			MecaTree mt = (MecaTree) mecaProcess.getTreeId_MecaTree ().get (integerId);
			drawTree (g2, r, mt);
			double x = mt.getX ();
			double y = mt.getY ();
			if (x <= xmin) {
				xmin = x;
			}
			if (x >= xmax) {
				xmax = x;
			}
			if (y <= ymin) {
				ymin = y;
			}
			if (y >= ymax) {
				ymax = y;
			}
		}

		double height = ymax - ymin;
		double width = xmax - xmin;
		xmin = xmin - 2.0;
		ymin = ymin - 2.0;
		double xx = xmin + width + 4.0;
		double yx = ymin;
		double xy = xmin;
		double yy = ymin + height + 4.0;
		Shape sh1 = new Line2D.Double (xmin, ymin, xx, yx);
		Shape sh2 = new Line2D.Double (xmin, ymin, xy, yy);
		g2.setColor (Color.black);
		//g2.drawString ("O", (float) (xmin - panel2D.getUserWidth (10)), (float) (ymin - panel2D.getUserHeight (15)));
		g2.draw (sh1);
		g2.drawString ("x", (float) xx, (float) (yx - panel2D.getUserHeight (15)));
		g2.draw (sh2);
		g2.drawString ("y", (float) (xy - panel2D.getUserWidth (10)), (float) yy);

		updateToolTipText ();
	}

	/**
	 * Select mecaTrees.
	 */
	public JPanel select (Rectangle.Double r, boolean ctrlIsDown) {
		JPanel infoPanel = null;
		Map panels = new Hashtable ();

		for (Iterator i = mecaProcess.getTreeIds ().iterator (); i.hasNext ();) {
			Integer integerId = (Integer) i.next ();
			MecaTree mt = (MecaTree) mecaProcess.getTreeId_MecaTree ().get (integerId);
			Point.Double p = new Point.Double (mt.getX (), mt.getY ());
			if (r.contains (p)) {

				double x, y, width, height;
				height = mt.getZTop () - mt.getZ () + 2d;
				// Here, we suppose that tree top has the largest deflection...
				width = Math.sqrt (	Math.pow (mt.getXTop () - mt.getX (), 2) +
									Math.pow (mt.getYTop () - mt.getY (), 2)) + 2d;
				x = -width / 2;
				y = height / 10;//panel2D.getUserHeight (30);
				Rectangle.Double r2 = new Rectangle.Double (x, y, width, height);

				panels.put (""+mt.getId (), new MecaPTrunkShape (mt, r2));
			}
		}

		if (!panels.isEmpty ()) {
			infoPanel = new JPanel (new BorderLayout ());
			JTabbedPane tabPane = new JTabbedPane ();

			Iterator keys = panels.keySet ().iterator ();
			Iterator values = panels.values ().iterator ();

			while (keys.hasNext () && values.hasNext ()) {
				String key = (String) keys.next ();
				JComponent value = (JComponent) values.next ();
				tabPane.addTab (key, value);
			}

			infoPanel.add (tabPane, BorderLayout.CENTER);
		}

		return infoPanel;
	}

	/**
	 * User interface definition.
	 */
	private void createUI () {
		mainPanel = new JPanel ();
		mainPanel.setLayout (new BorderLayout ());

		// 1. Viewer panel2D
		scrollPane = new JScrollPane (panel2D);

		scrollPane.setPreferredSize (new Dimension (500, 500));	// fc - 31.1.2001

		scrollPane.getViewport().putClientProperty ("EnableWindowBlit", Boolean.TRUE);	// faster
		mainPanel.add (scrollPane, BorderLayout.CENTER);

		// 2. Control panel
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.CENTER));
		close = new JButton (Translator.swap ("Shared.close"));
		close.addActionListener (this);
		help = new JButton (Translator.swap ("Shared.help"));
		help.addActionListener (this);
		pControl.add (close);
		pControl.add (help);

		// set close as default (see AmapDialog)
		close.setDefaultCapable (true);
		getRootPane ().setDefaultButton (close);

		// layout parts
		getContentPane ().add (mainPanel, BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
	}

}




