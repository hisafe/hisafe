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

package capsis.gui;

import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.metal.OceanTheme;


/**
 * A theme with smaller fonts.
 * 
 * @author F. de Coligny - november 2003
 */
//~ public class CapsisSmallTheme extends DefaultMetalTheme {
public class CapsisSmallTheme extends OceanTheme {	// fc - 9.12.2004

	public FontUIResource getWindowTitleFont() {
		return getSubTextFont();
	}

	public FontUIResource getControlTextFont() {
		return getSubTextFont();
	}

	public FontUIResource getSystemTextFont() {
		return getSubTextFont();
	}

	public FontUIResource getUserTextFont() {
		return getSubTextFont();
	}

	public FontUIResource getMenuTextFont() {
		return getSubTextFont();
	}

}
