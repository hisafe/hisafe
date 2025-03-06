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
package capsis.util.methodprovider;


/**
 * This interface describes the data needed for connexion
 * between a given module and ForestGales.
 * It must be implemented by the module's stand object.
 *
 * @author F. de Coligny  - october 2002
 * update C. Meredieu - august 2003
 */
public interface ForestGalesFormat {
	// Tartinate v. tr. CULIN.  To butter; to spread (with);
	// ARGOT. (South-west of France) to use british software

	// 1. Used to create .fgi file
	//
	//public double getHeight ();	// m
	//public double getDbh ();	// m
	public int getAge () ; 
	public double getHg ();	// m
	public double getDg ();	// cm
	public double getSpacing ();	// m
	//public double getUpwindGap ();	// m	// fc - 24.9.2002 - upwind gap is an input data (JTextField)

	// 2. Used to tartinate forestGales values
	// C : Critical in CWindSpeed
	//
	public void setBreakageProbability (double probability);
	public void setOverturningProbability (double probability);
	public void setBreakageCWindSpeed (double speed);
	public void setOverturningCWindSpeed (double speed);
	
	public void setBreakageReturnPeriod (double period);
	public void setOverturningReturnPeriod (double period);

	// 3. Used to retrieve the tartinated values afterwards
	// C : Critical in CWindSpeed
	//
	public double getBreakageProbability ();
	public double getOverturningProbability ();
	public double getBreakageCWindSpeed ();
	public double getOverturningCWindSpeed ();
	
	public double getBreakageReturnPeriod ();
	public double getOverturningReturnPeriod ();


}
