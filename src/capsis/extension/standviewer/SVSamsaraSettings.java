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


/**
 * SVSamsaraSettings.
 *
 * @author F. de Coligny - june 2000
 */
public class SVSamsaraSettings extends SVSimpleSettings {
//upgraded for c4.0 - fc 9.1.2001

	public static final int NONE = 0;
	public static final int OUTLINED = 1;
	public static final int FILLED = 2;
	public static final int TRANSPARENT = 3;
//	public static final int LIGHT = 4;
//	public static final int REGENERATION = 5;

	public static final boolean SHOW_LEGEND = true;

	protected boolean cellLines;
	protected boolean ascendingSort;
	protected int crownView;
	protected int alphaValue;
//	protected int cellView;

	protected boolean showLegend;


	public SVSamsaraSettings () {
		resetSettings ();
	}

	public void resetSettings () {
		super.resetSettings ();
		cellLines = true;
		ascendingSort = true;
		crownView = FILLED;
		alphaValue = 200;
//		cellView = LIGHT;

		showLegend = SHOW_LEGEND;

	}

// UNUSED: Settings for SVSamsara contain no Objects but only primitive types
//		-> no redefinition needed for clone ()
/*	public Object clone () {
		SVSamsaraSettings o = null;
			o = (SVSamsaraSettings) super.clone ();	// copies cellLine (primitive type: automatic)
		return o;
	} */

	public String toString () {
		return " SVSamsara settings = "
			+super.toString ()
			+" cellLines="+cellLines
			+" ascendingSort="+ascendingSort
			+" crownView="+crownView
			+" alphaValue="+alphaValue
//			+" cellView="+cellView
			+" showLegend="+showLegend;
	}

}



