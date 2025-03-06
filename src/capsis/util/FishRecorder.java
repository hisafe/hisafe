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



/**
 * A marker interface for reaches (for grouping).
 *
 * @author F. de Coligny - september 2004
 */
public interface FishRecorder {

	//public float getGenericMeanThetaRecord () {return genericMeanThetaRecord;}
	public double getMeanFISRecord () ;
	//public float [] getFISRecord () {return FISRecord;}
	//public float [] getGenericThetaRecord () {return genericThetaRecord;}
	//public float [] getFSTRecord () {return FSTRecord;}
	//public double getMeanFSTRecord () ;
	public int getCreationDate ();
	//public int getAllelesNumber ();
	//public double [] getAllelicDiversity ();
	//public void setFISRecord (float[] f) {FISRecord = f;}
	//public void setGenericThetaRecord (float[] d) {genericThetaRecord = d;}
	//public void setMeanFISRecord (float f) {meanFISRecord = f;}
	//public void setGenericMeanThetaRecord (float t) {genericMeanThetaRecord = t;}
	//public void setFSTRecord (float[] f) {FSTRecord = f;}
	//public void setMeanFSTRecord (float f) {meanFSTRecord = f;}


}