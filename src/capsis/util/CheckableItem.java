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

import java.awt.Color;

import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.superclass.AbstractDataExtractor;

/**
 * A checkable item, to be used in a checkable JList.
 * From CheckListExample2.java (codeguru.developer.com).
 *
 * @author Nobuo Tamemasa
 */
public class CheckableItem implements Comparable {
	private String  caption;
	private boolean selected;
	private Color color;
	private AbstractDataExtractor extractor;

	public CheckableItem (String caption, boolean selected) {
		this (caption, selected, null, null);
	}

	public CheckableItem (String caption, boolean selected, Color color, AbstractDataExtractor extractor) {
		this.caption = caption;
		this.selected = selected;
		this.color = color;
		this.extractor = extractor;
	}

	public void setCaption (String s) {caption = s;}
	public String getCaption () {return caption;}
	
	public void setSelected (boolean b) {selected = b;}
	public boolean isSelected () {return selected;}
	
	public void setColor (Color c) {color = c;}
	public Color getColor () {return color;}

	public void setExtractor (PaleoDataExtractor v) {extractor = v;}
	public AbstractDataExtractor getExtractor () {return extractor;}
	
	public int compareTo (Object other) {
		if (!(other instanceof CheckableItem)) {return 1;}
		CheckableItem otherItem = (CheckableItem) other;
		return getCaption ().compareTo (otherItem.getCaption ());
		
		
	}

	public String toString () {return caption;}
}

