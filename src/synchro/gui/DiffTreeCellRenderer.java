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

package synchro.gui;

import java.awt.Color;

import javax.swing.tree.DefaultTreeCellRenderer;

import synchro.kernel.DiffNode;

/**
 * A tree cell renderer for JTree containing DiffNode with colors 
 * depending on their status.
 * 
 * @author F. de Coligny - april 2003
 */
public class DiffTreeCellRenderer extends DefaultTreeCellRenderer {

	static private Color zeroColor = Color.lightGray;
	static private Color zeroColor2 = Color.white;
	static private Color identityColor = Color.black;
	static private Color identityColor2 = Color.white;
	static private Color diffColor = new Color (204, 102, 0);
	static private Color diffColor2 = new Color (255, 204, 153);
	static private Color newColor = new Color (0, 150, 0);
	static private Color newColor2 = new Color (200, 255, 200);
	static private Color erasedColor = new Color (150, 0, 0);
	static private Color erasedColor2 = new Color (255, 180, 180);
	static private Color lightBlue = new Color (204, 204, 255);



	public DiffTreeCellRenderer () {
		super ();
			
	}

	public String getText () {
		//~ setBackground (Color.white);
		setOpaque (true);
		setForeground (!selected?identityColor:identityColor);
		setBackground (!selected?identityColor2:lightBlue);
		
		String text = super.getText ();
		int l = text.length ();
		
		if (l > 0) {
			String lastChar = text.substring (text.length ()-1);
			try {
				int code = new Integer (lastChar).intValue ();
				if (code == DiffNode.DIFF) {
					setForeground (!selected?diffColor:diffColor2);
					setBackground (!selected?diffColor2:diffColor);
					
				} else if (code == DiffNode.NEW) {
					setForeground (!selected?newColor:newColor2);
					setBackground (!selected?newColor2:newColor);
					
				} else if (code == DiffNode.ERASED) {
					setForeground (!selected?erasedColor:erasedColor2);
					setBackground (!selected?erasedColor2:erasedColor);
					
				} else if (code == DiffNode.IDENTITY) {
					setForeground (!selected?identityColor:identityColor);
					setBackground (!selected?identityColor2:lightBlue);
					
				} else if (code == DiffNode.ZERO) {
					setForeground (!selected?zeroColor:zeroColor);
					setBackground (!selected?zeroColor2:lightBlue);
					
				}
			} catch (Exception e) {}
			text = text.substring (0, l-1);
		}
		
		return text;
	}

	public static Color getDiffColor () {return DiffTreeCellRenderer.diffColor;}
	public static Color getDiffColor2 () {return DiffTreeCellRenderer.diffColor2;}
	public static Color getNewColor () {return DiffTreeCellRenderer.newColor;}
	public static Color getNewColor2 () {return DiffTreeCellRenderer.newColor2;}
	public static Color getErasedColor () {return DiffTreeCellRenderer.erasedColor;}
	public static Color getErasedColor2 () {return DiffTreeCellRenderer.erasedColor2;}



}



