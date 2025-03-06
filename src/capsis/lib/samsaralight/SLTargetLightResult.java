/* 
 * Samsaralight library for Capsis4.
 * 
 * Copyright (C) 2008 / 2012 Benoit Courbaud.
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

package capsis.lib.samsaralight;

import java.io.Serializable;
import java.util.List;
import java.util.Vector;

import capsis.lib.samsaralight.tag.SLTag;
import capsis.lib.samsaralight.tag.SLTagResultInterpretor;
import capsis.lib.samsaralight.tag.SLTagTargetResult;

/**
 * SLTargetLightResult - light related properties of a cell or a sensor.
 * 
 * @author B. Courbaud, N. Dones, M. Jonard, G. Ligot, F. de Coligny - October
 *         2008 / June 2012
 */
public class SLTargetLightResult implements Serializable {

	// fc+fa-1.8.2017 reviewed and refactored
	
	// The target (cell, sensor...) which we are the light properties
	private SLLightableTarget connectedTarget;

	private double aboveCanopyHorizontalEnergy;// in MJ/m2 along the horizontal
												// before interception

	private double horizontalEnergy; // in MJ/m2 along the horizontal
	private double horizontalEnergyDirect; // in MJ/m2 along the horizontal
	private double horizontalEnergyDiffuse; // in MJ/m2 along the horizontal

	private double horizontalRelativeEnergy; // in % along the horizontal
	private double horizontalRelativeEnergyDirect;
	private double horizontalRelativeEnergyDiffuse;

	private double slopeEnergy; // in MJ/m2 along the slope
	private double slopeEnergyDirect; // in MJ/m2 along the slope
	private double slopeEnergyDiffuse; // in MJ/m2 along the slope

	private double slopeRelativeEnergy; // in % along the slope
	private double slopeRelativeEnergyDirect;
	private double slopeRelativeEnergyDiffuse;

	// fc+fa-25.4.2017
	// In tagMode, additional tag level results are added in this list
	public List<SLTagTargetResult> tagResults;

	/**
	 * Constructor.
	 */
	public SLTargetLightResult(SLLightableTarget connectedTarget) {
		this.connectedTarget = connectedTarget;
	}

	public void resetEnergy() {

		aboveCanopyHorizontalEnergy = 0;
		
		horizontalEnergy = 0;
		horizontalEnergyDirect = 0;
		horizontalEnergyDiffuse = 0;
		
		horizontalRelativeEnergy = 0;
		horizontalRelativeEnergyDiffuse = 0;
		horizontalRelativeEnergyDirect = 0;

		slopeEnergy = 0;
		slopeEnergyDiffuse = 0;
		slopeEnergyDirect = 0;

		slopeRelativeEnergy = 0;
		slopeRelativeEnergyDiffuse = 0;
		slopeRelativeEnergyDirect = 0;
		
		// fc+fa-4.8.2017
		tagResults = null;
	}

	/**
	 * Sometimes, the light is not calculated each year to save time and copies
	 * are used instead.
	 */
	public SLTargetLightResult getCopy(SLLightableTarget connectedTarget) {
		SLTargetLightResult sl = new SLTargetLightResult(connectedTarget);
			
		// fc+fa-1.8.2017 this paragraph was missing
		sl.horizontalEnergy = horizontalEnergy;
		sl.horizontalEnergyDirect = horizontalEnergyDirect;
		sl.horizontalEnergyDiffuse = horizontalEnergyDiffuse;
		
		sl.horizontalRelativeEnergy = horizontalRelativeEnergy;
		sl.horizontalRelativeEnergyDirect = horizontalRelativeEnergyDirect;
		sl.horizontalRelativeEnergyDiffuse = horizontalRelativeEnergyDiffuse;
		
		sl.slopeEnergy = slopeEnergy;
		sl.slopeEnergyDirect = slopeEnergyDirect;
		sl.slopeEnergyDiffuse = slopeEnergyDiffuse;

		sl.slopeRelativeEnergy = slopeRelativeEnergy;
		sl.slopeRelativeEnergyDirect = slopeRelativeEnergyDirect;
		sl.slopeRelativeEnergyDiffuse = slopeRelativeEnergyDiffuse;
		
		return sl;
	}

	// fc+fa-25.4.2017
	synchronized public void addTagResult(SLTagTargetResult r) {
		if (r.energy_MJm2 == 0)
			return;

		if (tagResults == null)
			// fc+fa-3.8.2017 Vector to be thread safe
			tagResults = new Vector<>(); // fc+fa-25.4.2017
		
		tagResults.add(r);
	}

	// fc+fa-25.4.2017
	// In tagMode, additional tag level results are added in this list
	public List<SLTagTargetResult> getTagResults() {
		return tagResults;
	}

	//--- getters
	
	public SLLightableTarget getConnectedTarget() {
		return connectedTarget;
	}

	public double get_aboveCanopyHorizontalEnergy() {
		return aboveCanopyHorizontalEnergy;
	}

	public double get_horizontalEnergy() {
		return horizontalEnergy;
	}

	public double get_horizontalEnergyDirect() {
		return horizontalEnergyDirect;
	}

	public double get_horizontalEnergyDiffuse() {
		return horizontalEnergyDiffuse;
	}

	public double get_horizontalRelativeEnergy() {
		return horizontalRelativeEnergy;
	}

	public double get_horizontalRelativeEnergyDiffuse() {
		return horizontalRelativeEnergyDiffuse;
	}

	public double get_horizontalRelativeEnergyDirect() {
		return horizontalRelativeEnergyDirect;
	}

	public double get_slopeEnergy() {
		return slopeEnergy;
	}
	
	public double get_slopeEnergyDirect() {
		return slopeEnergyDirect;
	}

	public double get_slopeEnergyDiffuse() {
		return slopeEnergyDiffuse;
	}

	public double get_slopeRelativeEnergy() {
		return slopeRelativeEnergy;
	}

	public double get_slopeRelativeEnergyDirect() {
		return slopeRelativeEnergyDirect;
	}

	public double get_slopeRelativeEnergyDiffuse() {
		return slopeRelativeEnergyDiffuse;
	}

	//--- setters
	
	public void set_aboveCanopyHorizontalEnergy(double v) {
		this.aboveCanopyHorizontalEnergy = v;
	}

	public void set_horizontalEnergy(double v) {
		this.horizontalEnergy = v;
	}

	public void set_horizontalEnergyDirect(double v) {
		this.horizontalEnergyDirect = v;
	}

	public void set_horizontalEnergyDiffuse(double v) {
		this.horizontalEnergyDiffuse = v;
	}
	
	public void set_horizontalRelativeEnergy(double v) {
		horizontalRelativeEnergy = v;
	}

	public void set_horizontalRelativeEnergyDirect(double v) {
		this.horizontalRelativeEnergyDirect = v;
	}

	public void set_horizontalRelativeEnergyDiffuse(double v) {
		this.horizontalRelativeEnergyDiffuse = v;
	}

	public void set_slopeEnergy(double v) {
		slopeEnergy = v;
	}
	
	public void set_slopeEnergyDirect(double v) {
		slopeEnergyDirect = v;
	}

	public void set_slopeEnergyDiffuse(double v) {
		slopeEnergyDiffuse = v;
	}

	public void set_slopeRelativeEnergy(double v) {
		slopeRelativeEnergy = v;
	}

	public void set_slopeRelativeEnergyDirect(double v) {
		this.slopeRelativeEnergyDirect = v;
	}

	public void set_slopeRelativeEnergyDiffuse(double v) {
		this.slopeRelativeEnergyDiffuse = v;
	}

	

	public double getControlLegacyEnergy () {
		return slopeEnergyDirect + slopeEnergyDiffuse;
	}

	public double getControlTagBasedEnergy () {
		SLTag beamTag = null;
		SLTag radiationTag = null;
		SLTag orientationTag = SLTag.SLOPE; // fa-08.01.2018
//		return SLTagResultInterpretor.getEnergy((SLLightableTarget) connectedTarget, beamTag, radiationTag);
		return SLTagResultInterpretor.getEnergy((SLLightableTarget) connectedTarget, beamTag, radiationTag, orientationTag); // fa-08.01.2018
	}

	
}
