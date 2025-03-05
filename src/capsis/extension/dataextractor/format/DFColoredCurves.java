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

package capsis.extension.dataextractor.format;

import java.util.List;
import java.util.Vector;

/**	This interface describes the data format to draw curves.
*	These curves may have different colors.
*	See DFCurves superinterface.
* 
*	@author F. de Coligny - february 2006
*/
public interface DFColoredCurves extends DFCurves {
	
	/**	Return a Vector containing n Vectors of 1D coordinates.
	*	<PRE>
	*	ex: 2 points P1 P2 with 4 coordinates x y y' y"
	*	This would draw three curves: (x, y) (x, y') (x, y")
	*	curves = getCurves () contains 4 vectors :
	*	v1 : xP1 xP2
	*	v2 : yP1 yP2
	*	v3 : y'P1 y'P2
	*	v4 : y"P1 y"P2
	* 
	*	ex : 4 points P1 P2 P3 P4 with 3D coordinates x y z
	*	curves = getCurves () contains 3 vectors :
	*	v1 : xP1 xP2 xP3 xP4
	*	v2 : yP1 yP2 yP3 yP4
	*	v3 : zP1 zP2 zP3 zP4
	*	</PRE>
	*	Coordinate type is Integer or Double (exclusive).
	*	Each point's dimension is curves.size ().
	*	Number of points is v1.size () (=v2.size () = v3.size ()).
	*/
	@Override
	public List<List<? extends Number>> getCurves ();
	
	/**	Return (may return null if unused) a vector containing labels associated to each coordinate
	*	described in getCurves ().
	*	<PRE>
	*	ex1 : 4 points P1 P2 P3 P4 with 3D coordinates x y z
	*	v1 : Hetre Chene Charme Orme
	* 
	*	ex2 : 4 points P1 P2 P3 P4 with 3D coordinates x y z
	*	v1 : Hetre Chene Charme Orme
	*	v2 : Min
	*	v3 : Moy
	* 
	*	ex3 : 4 points P1 P2 P3 P4 with 3D coordinates x y z
	*	v1 : 0-10 10-20 20-30 30-40
	*	v2 : 1994 1995 1996 1997
	*	v3 : 1994 1995 1996 1997
	*	</PRE>
	*	Each element type is String (exclusive).
	*/
	@Override
	public List<List<String>> getLabels ();
	
	/**	Return 2 or 3 Strings for axes labelization (resp. x, y[ & z]).
	*	Needed (axes number = getAxesNames ().size ()).
	*/
	@Override
	public List<String> getAxesNames ();
	
	/**	Special case : If there are 3 axes (x, y, z) and more than two curves, 
	*	some are associated with y axis and the others with z axis.
	*	getNY () is the number of curves associated with y axis 
	*	(the first ones in getCurves ()).
	*	The next ones are associated with z axis.
	*/
	@Override
	public int getNY ();
	
	
	/**	Returns a color per curve: getCurves ().size () - 1.
	*/
	public Vector getColors ();
	
	
}
