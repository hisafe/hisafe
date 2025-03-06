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

import java.awt.FlowLayout;

/**
 * A flow layout manager with no space added between components.
 * Default constructor gives a left aligned layout usable to left align
 * components on a single line.
 * 
 * @author F. de Coligny
 */
public class SmartFlowLayout extends FlowLayout {

	public SmartFlowLayout (int align, int hGap, int vGap) {
		super (align);
		setHgap (hGap);
		setVgap (vGap);
	}

	public SmartFlowLayout () {
		this (FlowLayout.LEFT, 0, 0);
	}

	public SmartFlowLayout (int align) {
		this (align, 0, 0);
	}


}