/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package capsis.extension.modeltool.rockfornet;


/**
 * Rockfornet simulator
 *
 * @author Eric Mermin, Eric Maldonado - november 2006
 */
public class RockfornetResult {

	private double probableResidualRockfallHazard;
	private double standDensityMin;
	private double standDensityMax;
	private double meanStemDiameterMin;
	private double meanStemDiameterMax;
	
	public void setProbableResidualRockfallHazard (double v) {probableResidualRockfallHazard = v;}
	public void setStandDensityMin (double v) {standDensityMin = v;}
	public void setStandDensityMax (double v) {standDensityMax = v;}
	public void setMeanStemDiameterMin (double v) {meanStemDiameterMin = v;}
	public void setMeanStemDiameterMax (double v) {meanStemDiameterMax = v;}
	
	public double getProbableResidualRockfallHazard () {return probableResidualRockfallHazard;}
	public double getStandDensityMin () {return standDensityMin;}
	public double getStandDensityMax () {return standDensityMax;}
	public double getMeanStemDiameterMin () {return meanStemDiameterMin;}
	public double getMeanStemDiameterMax () {return meanStemDiameterMax;}
	
	public RockfornetResult () {}
	
	// fc - 5.7.2007 - deprecated, replaced by above instance variables and accessors
	//~ public String probableResidualRockfallHazard;
	//~ public String standDensity;
	//~ public String meanStemDiameter;
		
	public String toString () {
		return "probableResidualRockfallHazard = "+probableResidualRockfallHazard
				+" standDensity="+standDensityMin+" - "+standDensityMax
				+" meanStemDiameter="+meanStemDiameterMin+" - "+meanStemDiameterMax;
	}
}


