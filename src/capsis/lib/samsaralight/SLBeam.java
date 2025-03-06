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

import java.text.NumberFormat;

import capsis.defaulttype.plotofcells.NeighbourhoodMask;
import capsis.lib.samsaralight.tag.SLTag;

/**
 * A light beam.
 * 
 * @author B. Courbaud, N. Donès, M. Jonard, G. Ligot, F. de Coligny - October
 *         2008 / June 2012
 */
public class SLBeam extends NeighbourhoodMask {

	// // Tags are optional, depend on slSettings.tagMode
	// // Legacy: historical process, without tags // fc+fa-24.4.2017
	// public static final SLTag LEGACY = new SLTag("LEGACY");
	//
	// public static final SLTag DIRECT = new SLTag("DIRECT");
	// public static final SLTag DIFFUSE = new SLTag("DIFFUSE");
	// public static final SLTag CROWN_POTENTIAL = new SLTag("CROWN_POTENTIAL");
	// public static final SLTag LOWER_CROWN_POTENTIAL = new
	// SLTag("LOWER_CROWN_POTENTIAL");

	private SLTag tag; // fc+fa-24.4.2017 samsaralight resolution

	private double azimut_rad;
	private double heightAngle_rad;
	private double initialEnergy;
	private double currentEnergy;
	private boolean direct; // true if the beam is direct false if it is diffuse

	// fc+fa-18.5.2017 optional: some beams have a doy in [1,366], can be used
	// with a SLLadProportionManager to get a proportion in processLighting ().
	// Default value is -1, not set
	private int doy = -1;

	// fc+fa-18.5.2017 REMOVED, replaced by doy and SLLadProportionManager.
	// fc+fa-26.4.2017
	// Optional, see getLadProportion ().
//	public Map<Integer, Double> ladProportionMap;

	/**
	 * Create a beam.
	 */
	public SLBeam(double azimut_rad, double heightAngle_rad, double initialEnergy, boolean direct) {

		super(); // fc+fa-25.4.2017

		this.azimut_rad = azimut_rad; // azimut is with anticlockwise
										// (trigonometric) rotation from X axis
		this.heightAngle_rad = heightAngle_rad;
		this.initialEnergy = initialEnergy;
		this.direct = direct;

		// Default tag for SLSettings.tagMode
		// Possible to create beams with user tags, see SLTag
		tag = SLTag.LEGACY;

	}

	/**
	 * Copy constructor, creates a copy of the given beam.
	 */
	public SLBeam(SLBeam modelBeam) {
		
		super (modelBeam);
		
		// fc+fa-3.8.2017 for Parallel mode
		
		this.tag = modelBeam.tag;		
		this.azimut_rad = modelBeam.azimut_rad;		
		this.heightAngle_rad = modelBeam.heightAngle_rad;		
		this.initialEnergy = modelBeam.initialEnergy;		
		this.currentEnergy = modelBeam.currentEnergy;		
		this.direct = modelBeam.direct;		
		this.doy = modelBeam.doy;		
	
	}
	
	public int getDoy() {
		return doy;
	}

	public void setDoy(int doy) {
		this.doy = doy;
	}

	/**
	 * A given beam for a given period can specify a coefficient expressing the
	 * leaf expansion relative to their complete development. E.g. until
	 * unfolding date, this coefficient will be 0, it will increase up to 1
	 * during leaf development until leaf complete development, and it will
	 * decrease conversely from leaf senescence starting date to leaf senescence
	 * ending date, and will be 0 after till the end of the year.
	 */
	// fc+fa-18.5.2017 REMOVED, replaced by doy and SLLadProportionManager.
//	public double getLadProportion(int speciesCode) throws Exception { // fc+fa-26.4.2017
//		if (ladProportionMap == null)
//			return 1;
//
//		Double v = ladProportionMap.get(speciesCode);
//		if (v == null)
//			throw new Exception("SamsaraLight error: could not find a ladProportion for speciesCode: " + speciesCode
//					+ " in beam: " + toString());
//
//		return v;
//	}

	/**
	 * Set the optional ladProportionMap, see getLadProportion (). Expecting
	 * proportions in [0, 1] for all species codes in the scene.
	 */
	// fc+fa-18.5.2017 REMOVED, replaced by doy and SLLadProportionManager.
//	public void setLadProportion(Map<Integer, Double> ladProportionMap) { // fc+fa-26.4.2017
//		this.ladProportionMap = ladProportionMap;
//	}

	// /**
	// * Radiation tag can not be changed, depends on the 'direct' variable.
	// */
	// public SLTag getRadiationTag() {
	// if (direct)
	// return SLBeam.DIRECT;
	// else
	// return SLBeam.DIFFUSE;
	// }

	/**
	 * This tag has a default value: 'Legacy' if tagMode is not activated. If
	 * the mode is activated, the tag can be changed by custom tags related to
	 * the time when the beam was created (e.g. doy / hour around the bud burst
	 * date).
	 */
	public SLTag getTag() { // fc+fa-24.4.2017 samsaralight resolution
		return tag;
	}

	public void setTag(SLTag tag) { // fc+fa-24.4.2017 samsaralight resolution
		this.tag = tag;
	}

	public double getHeightAngle_rad() {
		return heightAngle_rad;
	}

	public double getAzimut_rad() {
		return azimut_rad;
	}

	public double getCurrentEnergy() {
		return currentEnergy;
	}

	public double getInitialEnergy() {
		return initialEnergy;
	}

	public void resetEnergy() {
		currentEnergy = initialEnergy;
	}

	public void reduceCurrentEnergy(double f) {
		currentEnergy -= f;
	}

	public void setInitialEnergy(double e) {
		initialEnergy = e;
	}

	public boolean isDirect() {
		return direct;
	}

	/**
	 * Return a String representation of this object.
	 */
	public String toString() {
		NumberFormat nf = NumberFormat.getNumberInstance();
		nf.setMaximumFractionDigits(2);

		StringBuffer b = new StringBuffer(super.toString());
		b.append("SLBeam");

		b.append(" tag: ");
		b.append(tag);

		b.append(" azimut: ");
		b.append(nf.format(Math.toDegrees(this.getAzimut_rad())));
		b.append(" heightAngle: ");
		b.append(nf.format(Math.toDegrees(this.getHeightAngle_rad())));
		b.append(" initialEnergy: ");
		b.append(nf.format(initialEnergy));
		b.append(" currentEnergy: ");
		b.append(nf.format(currentEnergy));
		b.append(" direct: ");
		b.append(direct);

		return b.toString();
	}

	/**
	 * Return a String representation of this object. adapted by GL (30/04/2013)
	 * to produce a nice csv file in the console
	 */
	public String toCSVString() {

		NumberFormat nf = NumberFormat.getNumberInstance();
		nf.setMaximumFractionDigits(2);

		StringBuffer b = new StringBuffer();
		// StringBuffer b = new StringBuffer(super.toString());
		// b.append("SLBeam");
		// b.append(" azimut: ");
		b.append(nf.format(Math.toDegrees(this.getAzimut_rad())));
		b.append(";");
		// b.append(" heightAngle: ");
		b.append(nf.format(Math.toDegrees(this.getHeightAngle_rad())));
		b.append(";");
		// b.append(" initialEnergy: ");
		b.append(nf.format(initialEnergy));
		b.append(";");
		// b.append(" currentEnergy: ");
		// b.append(nf.format(currentEnergy));
		// b.append(" direct: ");
		b.append(direct);

		return b.toString();
	}
}