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

import java.util.ArrayList;
import java.util.List;

import capsis.kernel.AbstractSettings;
import jeeb.lib.util.ListMap;

/**
 * SLSettings - List of samsaralight settings.
 * 
 * @author B. Courbaud, N. Donès, M. Jonard, G. Ligot, F. de Coligny - October
 *         2008 / June 2012
 */
public class SLSettings extends AbstractSettings {

	// --- [samsaraLightSettings file parameters...

	// Main parameter file name
	private String fileName;

	// fc+fa-3.8.2017 If true, the parallel mode is activated
	private boolean parallelMode;

	// This tag mode is false by default
	// If set to true, activates the tag mode -> tag level results will be
	// available for all tags in beams and treeParts, see SLBeam and SLTreePart
	private boolean tagMode; // fc+fa-25.4.2017 samsaralight resolution

	// turbidMedium: if false, porous envelop
	private boolean turbidMedium = true;

	// trunkInterception: if true, the algorithm computes trunk interceptions
	private boolean trunkInterception = false;

	private double directAngleStep = 5; // deg

	private double heightAngleMin = 15; // deg

	private double diffuseAngleStep = 15; // deg

	// Standard Overcast Sky, if false: Uniform Overcast Sky
	private boolean soc = true;

	// GMT Time lag between the local time and the local solar time meridian
	// (GL, 06/09/2012)
	// In Occidental Europe : + 1 during the winter; + 2 during the summer. (But
	// default is 0, if no information in the settings file.
	private int GMT = 0;

	private int leafOnDoy = 40; // day of year (see the calendar, fc)

	private int leafOffDoy = 200;

	// Enlight only the virtual sensor?
	private boolean sensorLightOnly = false;

	// ...samsaraLightSettings file parameters] ---

	// --- [Plot location / orientation properties...

	private double plotLatitude_deg = 45; // deg: default is near Grenoble,
											// France

	private double plotLongitude_deg = 5; // deg

	private double plotSlope_deg = 0; // deg

	// Angle of slope bottom on the compass from the North, clockwise rotation
	// northern aspect : 0, eastern aspect : 90, southern aspect : 180, western
	// aspect : 270
	private double plotAspect_deg = 0; // deg

	// Angle from North to x axis clockwise. Default correspond to a Y axis
	// oriented toward the North.
	private double northToXAngle_cw_deg = 90; // deg

	// ...Plot location / orientation properties] ---

	private List<SLMonthlyRecord> monthlyRecords;

	private ListMap<Integer, SLHourlyRecord> hourlyRecords;

	private boolean writeStatusDispatcher = true; // GL 5-6-2013. option to
													// speed
													// up the computations in
													// script mode

	// A list of cells that will be targeted by rays. If the list is null all
	// plot cells will be targeted.
	// if the list is empty, all plot cells will be targeted
	// This works only if the list is updated after each evolution and
	// intervention (not very good)
	// => moved to lightableScene (18/05/2015)
	// private List<SquareCell> cellstoEnlight;

	// sq-23.11.2021 Samsara2 integration. =0 for samsara2; =directAngleStep / 2 by
	// default
	private double beamStartOffset = directAngleStep / 2d; // fc-13.4.2022 degrees, wrote 2d

	// --- Methods

	/**
	 * Default constructor.
	 */
	public SLSettings() {
		// super();
	}

	/**
	 * Copy constructor.
	 */
	public SLSettings(SLSettings settings) {

		this.fileName = settings.fileName;
		this.parallelMode = settings.parallelMode;
		this.tagMode = settings.tagMode;
		this.turbidMedium = settings.turbidMedium;
		this.trunkInterception = settings.trunkInterception;
		this.directAngleStep = settings.directAngleStep;
		this.heightAngleMin = settings.heightAngleMin;
		this.diffuseAngleStep = settings.diffuseAngleStep;
		this.soc = settings.soc;
		this.leafOnDoy = settings.leafOnDoy;
		this.leafOffDoy = settings.leafOffDoy;
		if (settings.monthlyRecords != null) {
			this.monthlyRecords = new ArrayList<SLMonthlyRecord>(settings.monthlyRecords);
		}
		if (settings.hourlyRecords != null) {
			this.hourlyRecords = new ListMap(settings.hourlyRecords);
		}
		this.writeStatusDispatcher = settings.writeStatusDispatcher;
		this.GMT = settings.GMT;
		this.plotLatitude_deg = settings.plotLatitude_deg;
		this.plotLongitude_deg = settings.plotLongitude_deg;
		this.plotSlope_deg = settings.plotSlope_deg;
		this.plotAspect_deg = settings.plotAspect_deg;
		this.northToXAngle_cw_deg = settings.northToXAngle_cw_deg;
		this.sensorLightOnly = settings.sensorLightOnly;
		// sq-23.11.2021 samsara2 integration
		this.beamStartOffset = settings.beamStartOffset;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getFileName() {
		return fileName;
	}

	public void setTurbidMedium(boolean turbidMedium) {
		this.turbidMedium = turbidMedium;
	}

	public boolean isTurbidMedium() {
		return turbidMedium;
	}

	public void setTrunkInterception(boolean trunkInterception) {
		this.trunkInterception = trunkInterception;
	}

	public boolean isTrunkInterception() {
		return trunkInterception;
	}

	public void setDirectAngleStep(double directAngleStep) {
		this.directAngleStep = directAngleStep;
		// sq-23.1.2021 samsara2 integration
		this.setBeamStartOffset(directAngleStep / 2d);
	}

	public double getDirectAngleStep() {
		return directAngleStep;
	}

	public void setHeightAngleMin(double heightAngleMin) {
		this.heightAngleMin = heightAngleMin;
	}

	public double getHeightAngleMin() {
		return heightAngleMin;
	}

	public void setDiffuseAngleStep(double diffuseAngleStep) {
		this.diffuseAngleStep = diffuseAngleStep;
	}

	public double getDiffuseAngleStep() {
		return diffuseAngleStep;
	}

	public void setSoc(boolean soc) {
		this.soc = soc;
	}

	public boolean isSoc() {
		return soc;
	}

	public void setLeafOnDoy(int leafOnDoy) {
		this.leafOnDoy = leafOnDoy;
	}

	public int getLeafOnDoy() {
		return leafOnDoy;
	}

	public void setLeafOffDoy(int leafOffDoy) {
		this.leafOffDoy = leafOffDoy;
	}

	public int getLeafOffDoy() {
		return leafOffDoy;
	}

	public void setWriteStatusDispatcher(boolean writeStatusDispatcher) {
		this.writeStatusDispatcher = writeStatusDispatcher;
	}

	public boolean isWriteStatusDispatcher() {
		return writeStatusDispatcher;
	}

	public void setParallelMode(boolean parallelMode) {
		this.parallelMode = parallelMode;
	}

	public boolean isParallelMode() {
		return parallelMode;
	}

	public void setTagMode(boolean tagMode) {
		this.tagMode = tagMode;
	}

	public boolean isTagMode() {
		return tagMode;
	}

	// Accessors
	public void addMontlyRecord(SLMonthlyRecord r) {
		if (monthlyRecords == null)
			monthlyRecords = new ArrayList<SLMonthlyRecord>();
		monthlyRecords.add(r);
	}

	public List<SLMonthlyRecord> getMontlyRecords() {
		return monthlyRecords;
	}

	/**
	 * A way to replace SamsaraLight irradiation records if needed (see Heterofor
	 * meteo file). fc+mj+fa-27.7.2017
	 */
	public void setHourlyRecords(List<SLHourlyRecord> newRecords) {
		monthlyRecords = null;
		hourlyRecords = null;
		for (SLHourlyRecord r : newRecords) {
			addHourlyRecord(r);
		}

	}

	public void addHourlyRecord(SLHourlyRecord r) {
		if (hourlyRecords == null)
			hourlyRecords = new ListMap<>();
		int month = r.month;
		hourlyRecords.addObject(month, r);
	}

	public ListMap<Integer, SLHourlyRecord> getHourlyRecords() {
		return hourlyRecords;
	}

	// public void addCelltoEnlight(SquareCell c) {
	// if (cellstoEnlight == null)
	// cellstoEnlight = new ArrayList<SquareCell>();
	// cellstoEnlight.add(c);
	// }

	public double getPlotLatitude_deg() {
		return plotLatitude_deg;
	}

	public void setPlotLatitude_deg(double plotLatitude) {
		this.plotLatitude_deg = plotLatitude;
	}

	public double getPlotLongitude_deg() {
		return plotLongitude_deg;
	}

	public void setPlotLongitude_deg(double plotLongitude) {
		this.plotLongitude_deg = plotLongitude;
	}

	public double getPlotSlope_deg() {
		return plotSlope_deg;
	}

	public void setPlotSlope_deg(double plotSlope) {
		this.plotSlope_deg = plotSlope;
	}

	public double getPlotAspect_deg() {
		return plotAspect_deg;
	}

	public void setPlotAspect_deg(double plotAspect) {
		this.plotAspect_deg = plotAspect;
	}

	public double getNorthToXAngle_cw_deg() {
		return northToXAngle_cw_deg;
	}

	public void setNorthToXAngle_cw_deg(double northToXAngle_cw) {
		this.northToXAngle_cw_deg = northToXAngle_cw;
	}

	// public List<SquareCell> getCellstoEnlight() {
	// return cellstoEnlight;
	// }
	//
	// public void clearListOfCellsToEnlight(){
	// cellstoEnlight = new ArrayList<SquareCell>();
	// }

	public int getGMT() {
		return GMT;
	}

	public void setGMT(int gmt) {
		this.GMT = gmt;
	}

	public boolean isSensorLightOnly() {
		return sensorLightOnly;
	}

	public void setSensorLightOnly(boolean sensorLightOnly) {
		this.sensorLightOnly = sensorLightOnly;
	}

	// sq-23.11.2021 samsara2 integration
	public double getBeamStartOffset() {
		return beamStartOffset;
	}

	public void setBeamStartOffset(double beamStartOffset) {
		this.beamStartOffset = beamStartOffset;
	}
}
