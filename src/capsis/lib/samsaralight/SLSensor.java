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

import capsis.defaulttype.plotofcells.SquareCell;
// fa-12.07.2017
import capsis.util.Shiftable;
import capsis.util.shifter.CoordinateShifter2d;


/**
 * A SamsaraLight class for creating virtual sensors that store radiation energy
 * within the scene canopy. It also stores potential measured values of
 * irradiance.
 * 
 * @author Gauthier Ligot - February 2013
 */
public class SLSensor implements Shiftable, SLLightableTarget, Serializable {

	// fc-19.9.2022 Added Shiftable (see Lilo)
	// fc+fa-1.8.2017 reviewed and refactored

	private int id;
	private double x;
	private double y;
	private double height; // m

	private SquareCell matchingCell;
	
	private SLTargetLightResult lightResult;

	// Given values
	private double measuredRelativeEnergy; // %
	private double measuredRelativeEnergyDirect; // %
	private double measuredRelativeEnergyDiffuse; // %

	private boolean hasMeasuredRelativeEnergy = false;
	private boolean hasMeasuredRelativeEnergyDirect = false;
	private boolean hasMeasuredRelativeEnergyDiffuse = false;

	// fa-12.07.2017
	// In tagMode, additional tag level results are added in this list
//	public List<SLTagTargetResult> tagSensorResults;

	// fc-19.9.2022 Optional
	private CoordinateShifter2d shifter;

	/**
	 * Constructor
	 * 
	 * @param id
	 *            : identifier of the sensor
	 * @param x
	 *            (m)
	 * @param y
	 *            (m)
	 * @param height
	 *            (m)
	 */
	public SLSensor(int id, double x, double y, double height) {
		this.id = id;
		this.x = x;
		this.y = y;
		this.height = height;

		lightResult = new SLTargetLightResult(this);
	}

	/**
	 * Constructor 2, if a coordinate shifter is used (see Lilo).
	 */
	public SLSensor(int id, double x, double y, double height, CoordinateShifter2d shifter) {
		this (id, x, y, height);
		
		// fc-19.9.2022 To restore original coordinates if needed
		this.shifter = shifter;
	}
	
	public SLSensor getCopy() {
		SLSensor copiedSensor = new SLSensor(this.id, this.x, this.y, this.height);

		copiedSensor.lightResult = this.lightResult.getCopy(this);

		if (this.hasMeasuredRelativeEnergy)
			copiedSensor.set_measuredRelativeEnergy(this.measuredRelativeEnergy);
		if (this.hasMeasuredRelativeEnergyDirect)
			copiedSensor.set_measuredRelativeEnergyDirect(this.measuredRelativeEnergyDirect);
		if (this.hasMeasuredRelativeEnergyDiffuse)
			copiedSensor.set_measuredRelativeEnergyDiffuse(this.measuredRelativeEnergyDiffuse);

		return copiedSensor;
	}

	public SLTargetLightResult getLightResult() {
		return lightResult;
	}
	
	public void setLightResult(SLTargetLightResult lightResult) {
		this.lightResult = lightResult;
	}
	
	// fa-12.07.2017
//	public void addTagSensorResult(SLTagTargetResult r) {
//		if (r.energy_MJm2 == 0)
//			return;
//
//		if (tagSensorResults == null)
//			tagSensorResults = new ArrayList<>();
//		tagSensorResults.add(r);
//	}

	// fa-12.07.2017
	// In tagMode, additional tag level results are added in this list
//	public List<SLTagTargetResult> getTagSensorResults() {
//		return tagSensorResults;
//	}

	// --- getters

	public SquareCell getMatchingCell() {
		return matchingCell;
	}

	public double get_measuredRelativeEnergy() {
		return measuredRelativeEnergy;
	}

	public double get_measuredRelativeEnergyDiffuse() {
		return measuredRelativeEnergyDiffuse;
	}

	public double get_measuredRelativeEnergyDirect() {
		return measuredRelativeEnergyDirect;
	}

	public double getX() {
		return x;
	}

	public double getY() {
		return y;
	}

	public double getHeight() {
		return height;
	}

	public int getId() {
		return id;
	}

	/**
	 * The z coordinates require to know over which cell is the sensor If this
	 * cell has not been specified, the method returns 0 with messages in the
	 * samsara Log
	 */
	public double getZ() {
		if (matchingCell != null) {
			return matchingCell.getZCenter() + this.height;
		} else {
			SLReporter.printInStandardOutput("SLSensor.getZ() - z has not been specified yet. sensor.height is returned");
			SLReporter.printInLog("SamsaraLight", "z has not been specified yet. sensor.height is returned");
			return this.height;
		}

	}

	public boolean hasmeasuredRelativeTotalEnergy() {
		return this.hasMeasuredRelativeEnergy;
	}

	public boolean hasmeasuredRelativeDiffusEnergy() {
		return this.hasMeasuredRelativeEnergyDiffuse;
	}

	public boolean hasmeasuredRelativeDirectEnergy() {
		return this.hasMeasuredRelativeEnergyDirect;
	}

	// --- setters

	/**
	 * The cell above which is the sensor
	 */
	public void setMatchingCell(SquareCell cell) {
		this.matchingCell = cell;
	}

	public void set_measuredRelativeEnergy(double v) {
		this.measuredRelativeEnergy = v;
		this.hasMeasuredRelativeEnergy = true;
	}

	public void set_measuredRelativeEnergyDirect(double v) {
		this.measuredRelativeEnergyDirect = v;
		this.hasMeasuredRelativeEnergyDirect = true;
	}

	public void set_measuredRelativeEnergyDiffuse(double v) {
		this.measuredRelativeEnergyDiffuse = v;
		this.hasMeasuredRelativeEnergyDiffuse = true;
	}

	/**
	 * It is possible to change the sensor height
	 */
	public void setHeight(double height) {
		this.height = height;
	}

	// fc-19.9.2022
	public double getUnshiftedX() {
		if (shifter != null)
			return shifter.restoreX(getX());
		else
			return getX ();
	}

	// fc-19.9.2022
	public double getUnshiftedY() {
		if (shifter != null)
			return shifter.restoreY(getY());
		else
			return getY ();
	}

	@Override
	public String toString() {
		return "SLSensor id: "+id;
	}
	
}
