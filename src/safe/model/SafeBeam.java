/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2024 INRAE 
 * 
 * Authors  
 * C.DUPRAZ       	- INRAE Montpellier France
 * M.GOSME       	- INRAE Montpellier France
 * G.TALBOT       	- INRAE Montpellier France
 * B.COURBAUD      	- INRAE Montpellier France
 * H.SINOQUET		- INRAE Montpellier France
 * N.DONES			- INRAE Montpellier France
 * N.BARBAULT 		- INRAE Montpellier France 
 * I.LECOMTE       	- INRAE Montpellier France
 * M.Van NOORDWIJK  - ICRAF Bogor Indonisia 
 * R.MULIA       	- ICRAF Bogor Indonisia
 * D.HARJA			- ICRAF Bogor Indonisia
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Hi-SAFE is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * If not, see <http://www.gnu.org/licenses/>.
 *
 */

package safe.model;

import java.text.NumberFormat;
import java.util.Vector;
import capsis.lib.samsaralight.SLBeam;


/**
 * A light beam (direct or diffuse used in the tree light interception process )
 *
 * @see SafeBeamSet
 * @author B. Courbaud CEMAGREF Grenoble - January 2000 - Benoit.Courbaud@grenoble.cemagref.fr
 */
public class SafeBeam extends SLBeam {
 
	private static final long serialVersionUID = 1L;
	private float diffuseEnergy;	//=(fraction of diffuse radiation allocated to this beam)/(sin(heightAngle)) 	(units : m-2 of beam section)
	private float directEnergy;		//=(fraction of direct radiation allocated to this beam)/(sin(heightAngle)) 	(units : m-2 of beam section)
	private float infraRedEnergy;	//=(fraction of infra-red radiation allocated to this beam)/(sin(heightAngle)) 	(units : m-2 of beam section)

	private Vector<SafeShadingMask> shadingMasks;		//for each CellImpact, a mask of potentially shading neighbor crops.

	public SafeBeam (double a, double h, float difE, float dirE, float ire, float convFactor) {

		super(a, h, convFactor, false);
		
		//lightening a surface of unit projection on horizontal and energy of a beam lightening a unit
		//horizontal surface (=1 when no slope)
		diffuseEnergy=difE;
		directEnergy=dirE;
		infraRedEnergy=ire;
		shadingMasks = new Vector<SafeShadingMask>();

	}

	public float getDiffuseEnergy () {
		return diffuseEnergy;
	}

	public float getDirectEnergy () {
		return directEnergy;
	}

	public void setDirectEnergy (float dirE) {
		directEnergy=dirE;
	}

	public float getInfraRedEnergy () {
		return infraRedEnergy;
	}

	public void setInfraRedEnergy (float ire) {
		infraRedEnergy=ire;
	}

	public void resetDirectEnergy (){
		directEnergy=0;
	}

	public void addShadingMask (SafeShadingMask mask){	
		shadingMasks.add(mask);
	}

	public Vector<SafeShadingMask> getShadingMasks() {	
		return shadingMasks;
	}

	public void removeShadingMasks (){
		shadingMasks.removeAllElements();
	}

	/**
	 * Clear the neighbourCells collection
	 */
	public void removeAllNeighbourCell () {
		sites.clear ();
	}

	public String toString(){
		NumberFormat nf = NumberFormat.getNumberInstance();
		nf.setMaximumFractionDigits(0);

		String str = super.toString ()
			+ " azimut="+nf.format(Math.toDegrees(this.getAzimut_rad()))
			+ " heightAngle="+nf.format(Math.toDegrees(this.getHeightAngle_rad()));
		return str;
	}
	
}