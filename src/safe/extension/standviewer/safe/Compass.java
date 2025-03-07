/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE 
 * 
 * Authors  
 * C.DUPRAZ       	- INRAE Montpellier France
 * M.GOSME       	- INRAE Montpellier France
 * G.TALBOT       	- INRAE Montpellier France
 * B.COURBAUD      	- INRAE Montpellier France
 * H.SINOQUET		- INRAE Montpellier France
 * N.DONES			- INRAE Montpellier France
 * N.BARBAULT 		- INRAE Montpellier France 
 * I.LECOMTE       	- INRAE Montpellier France
 * M.Van NOORDWIJK  - ICRAF Bogor Indonisia 
 * R.MULIA       	- ICRAF Bogor Indonisia
 * D.HARJA			- ICRAF Bogor Indonisia
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software under the terms of the CC-BY License as published by the Creative Commons Corporation
 *
 * You are free to:
 *		Share — copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt — remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution — 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
 *               
 * Notices:
 * 		You do not have to comply with the license for elements of the material in the public domain or where your use is permitted 
 *      by an applicable exception or limitation .
 *		No warranties are given. The license may not give you all of the permissions necessary for your intended use. 
 *		For example, other rights such as publicity, privacy, or moral rights may limit how you use the material.  
 *
 * For more details see <https://creativecommons.org/licenses/by/4.0/>.
 *
 */

package safe.extension.standviewer.safe;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.Line2D;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;
import capsis.util.Drawer;
import capsis.util.Panel2D;
/**
 * Compass - A component to see North and bottom of slope directions.
 *
 * @author B. Courbaud - January 2003
 * Updated by I Lecomte - April 2003
 */
public class Compass extends JPanel implements Drawer {
	private static final long serialVersionUID = 1L;

	protected Panel2D panel2D;
	protected double angle;	// degrees
	private Font tableFont = new JTable ().getFont ();


	/**	Constructor.
	*/
	public Compass (double angle) {
		super (new BorderLayout ());
		this.angle = angle;

		Rectangle.Double r2 = new Rectangle.Double (-1, -1, 2, 2);

		panel2D = new Panel2D (this,
				r2,
				0, 	// x margin
				0);	// y margin
		panel2D.setPreferredSize (new Dimension (40, 40));
		panel2D.setZoomEnabled (false);
		panel2D.setSelectionEnabled (false);
		panel2D.setMoveEnabled (false);
		panel2D.setInfoIconEnabled (false);	// fc - 19.4.2007
		LinePanel l1 = new LinePanel ();

		//setLayout (new BoxLayout (this, BoxLayout.X_AXIS));

		l1.add (new JWidthLabel ("", 5));
		l1.add (panel2D);

		JLabel n = new JLabel (Translator.swap ("SafeCompass.North"));
		n.setFont (tableFont);
		n.setForeground (Color.RED);
	//	JLabel b = new JLabel (Translator.swap ("SafeCompass.TreeLine"));
	//	b.setFont (tableFont);
	//	b.setForeground (Color.BLACK);
		ColumnPanel l = new ColumnPanel (2, 0);
		l.add (n);
//		l.add (b);
		l.setBackground (Color.WHITE);

		l.addGlue ();

		// fc - 8.3.2004
		int width = l.getPreferredSize ().width;
		int height= 40;
		l.setMaximumSize (new Dimension (width, height));

		l1.add (l);
		l1.addGlue ();
		l1.setBackground (Color.WHITE);

		add (l1, BorderLayout.NORTH);

	}


	/**	From Drawer interface.
	*	This method draws in the Panel2D each time this one must be
	*	repainted.
	*/
	public void draw (Graphics g, Rectangle.Double r) {
		Graphics2D g2 = (Graphics2D) g;

		// Tree Line
		double alpha = 3d*Math.PI/2d;
		double x = 0;
		double y = -1;

		double beta = Math.toRadians (30);
		double l = 0.3d;

		double x1 = x - l*Math.cos (alpha+beta);
		double y1 = y - l*Math.sin (alpha+beta);

		double x2 = x - l*Math.cos (beta-alpha);
		double y2 = y + l*Math.sin (beta-alpha);

		g2.setColor (Color.black);
		Shape bottom = new Line2D.Double (0, 0, x, y);
		Shape bBranch1 = new Line2D.Double (x, y, x1, y1);
		Shape bBranch2 = new Line2D.Double (x, y, x2, y2);
/*		g2.draw (bottom);
		g2.draw (bBranch1);
		g2.draw (bBranch2);*/

		// North arrow
		double red = 0.75d;
		//red = 1d;
		alpha = 3d*Math.PI/2d + Math.toRadians (angle);
		x = red*Math.cos (alpha);
		y = red*Math.sin (alpha);

		beta = Math.toRadians (30);
		l = 0.3d;

		x1 = x - l*Math.cos (alpha+beta);
		y1 = y - l*Math.sin (alpha+beta);

		x2 = x - l*Math.cos (beta-alpha);
		y2 = y + l*Math.sin (beta-alpha);

		g2.setColor (Color.red);
		Shape north = new Line2D.Double (0, 0, x, y);
		Shape nBranch1 = new Line2D.Double (x, y, x1, y1);
		Shape nBranch2 = new Line2D.Double (x, y, x2, y2);
		g2.draw (north);
		g2.draw (nBranch1);
		g2.draw (nBranch2);

	}


	/**	From Drawer interface.
	*	We may receive (from Panel2D) a selection rectangle (in user space i.e. meters)
	*	and return a JPanel containing information about the objects (trees) inside
	*	the rectangle.
	*	If no objects are found in the rectangle, return null.
	*/
	public JPanel select (Rectangle.Double r, boolean ctrlIsDown) {
		return null;	// no selection allowed here
	}

}



