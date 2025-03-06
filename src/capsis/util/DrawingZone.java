/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
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

import java.awt.Font;
import java.awt.Graphics;
import java.awt.geom.AffineTransform;

import javax.swing.JPanel;

/**
 * A zone in a JPanel where you can draw after having applied
 * the base transform. This transform should deal with initial translation
 * and optionaly y axis inversion.
 * <PRE>
 * ex:
 *   AffineTransform t = new AffineTransform ();
 *   t.translate (50, 50);
 *   t.scale (1, -1);
 *   DrawingZone graphZone = new DrawingZone (t);
 *   graphZone.draw (...);	// uses current transform t
 *   ...
 *   
 *   graphZone.setBounds (10, 10, 100, 20);
 *   panelWithLayoutSetToNull.add (graphZone);
 * </PRE>
 * 
 * @author F. de Coligny
 */
public class DrawingZone extends JPanel {
	protected AffineTransform baseTransform;
	protected boolean firstTime;	// used to apply transform only once
	protected Font userFont;

	public DrawingZone () {
		this (new AffineTransform ());
	}
	public DrawingZone (AffineTransform t) {
		super ();
		setSize (1, 1);	// if size is zero, not painted (size is recomputed in paintComponent ())
		baseTransform = t;
		firstTime = true;
		userFont = new Font ("SansSerif", Font.PLAIN, 10);	// default Font
	}
	
	/**
	 * This method allows to place the zone in its parent JPanel
	 * giving the coordinates of one of its corners, width and height.
	 * You can choose to use directly the setBounds () method instead.
	 * <PRE>
	 *  corner1 -------- corner2    ^
	 *  |                      |    |
	 *  |                      |    h
	 *  |                      |    |
	 *  corner3 -------- corner4    v
	 *
	 *  <--------- w ---------->
	 * </PRE>
	 */
	public void setCorner1 (int x, int y) {
		setBounds (x, y, getWidth (), getHeight ());
	}
	public void setCorner2 (int x, int y) {
		setBounds (x-getWidth (), y, getWidth (), getHeight ());
	}
	public void setCorner3 (int x, int y) {
		setBounds (x, y-getHeight (), getWidth (), getHeight ());
	}
	public void setCorner4 (int x, int y) {
		setBounds (x-getWidth (), y-getHeight (), getWidth (), getHeight ());
	}

	public void setBaseTransform (AffineTransform t) {
		baseTransform = t;
	}

	public void setUserFont (Font f) {
		userFont = f;
	}
	
	public Font getUserFont () {
		return userFont;
	}

	public AffineTransform getBaseTransform () {
		return baseTransform;
	}

	public void paintComponent (Graphics g) {
		super.paintComponent (g);
		g.setFont (userFont);
		// drawing code here in a subclass
	}
}

