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

package capsis.kernel;


/**	Relative to Capsis date correction: possibility to add an int offset date for root scene 
 *	and a int coefficient for step duration. Concerns scenes (see GScene.get/setDate ()).
 *	This feature can be used to compare projects with different time steps.
 *	Scene is concerned by date correction if 
 *	<pre>
 *	scene instanceof DateCorrectable && scene.isDateCorrectionEnabled ()
 *	</pre>
 *	Correction is on if
 *	<pre>
 *	scene.isDateCorrected ()
 *	</pre>
 * 
 * @author F. de Coligny - may 2003, october 2010
 */
public interface DateCorrectable {

	public boolean isDateCorrectionEnabled ();
	public boolean isDateCorrected ();
	public int getDateCorrection ();
	public int getStepCorrection ();

	public void setDateCorrectionEnabled (boolean v);
	public void setDateCorrected (boolean v);
	public void setDateCorrection (int v);
	public void setStepCorrection (int v);

	public int getRootDate ();	// not corrected (original)

}
