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

import capsis.kernel.AbstractCloneable;
import jeeb.lib.util.Settings;

/**
 * Settings for Panel2D objects.
 * 
 * @author F. de Coligny - May 2002
 */
public class Panel2DSettings extends AbstractCloneable {
//public class Panel2DSettings extends AbstractSettings implements Cloneable {

	// fc-14.10.2020 Replaced AbstractSettings by AbstractCloneable to separate
	// simple tools from Capsis modules' architecture 1.0
	// checked for c4.1.1_08 - fc-3.2.2003

	private float pencilSize;
	private boolean antiAliased;
	private int selectionSquareSize;

	// No redefinition needed because this object contains only primitive types
	// fields.
	// ~ public Object clone () {
	// ~ return super.clone ();
	// ~ }

	public Panel2DSettings() {
		// Inits
		pencilSize = Settings.getProperty("panel2D.pencil.size", 0);
		antiAliased = Settings.getProperty("panel2D.anti.aliased", false);
		selectionSquareSize = Settings.getProperty("panel2D.selection.square.size", 5);

	}

	public float getPencilSize() {
		return pencilSize;
	}

	public void setPencilSize(float v) {
		pencilSize = v;
		Settings.setProperty("panel2D.pencil.size", "" + v);
	}

	public boolean isAntiAliased() {
		return antiAliased;
	}

	public void setAntiAliased(boolean v) {
		antiAliased = v;
		Settings.setProperty("panel2D.anti.aliased", "" + v);
	}

	public int getSelectionSquareSize() {
		return selectionSquareSize;
	}

	public void setSelectionSquareSize(int v) {
		selectionSquareSize = v;
		Settings.setProperty("panel2D.selection.square.size", "" + v);
	}

}
