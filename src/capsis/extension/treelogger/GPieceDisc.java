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
package capsis.extension.treelogger;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Piece disc description.
 * 
 * @author D. Pont - dec 2005
 */
public class GPieceDisc implements Serializable {
	// modified by fc, cm, tl, fm and pv - 20.3.2006

	// nb-27.08.2018
	//private static final long serialVersionUID = 20100805L;

	private int id; // needed, unique in the piece
	// private int pieceId; // needed, the id of the related piece
	private double centreY_mm; // needed, height of the disc in the piece

	// needed, at least 3: bark (id=1), wood ring under bark (id=2),
	// pith (last id), ordered from external (id=1) to internal,
	// instances of GPieceRing
	@SuppressWarnings("unchecked")
	private ArrayList<GPieceRing> rings;

	// contours inside the disc, all optional
	// open typology: when addings entries here, please notify the members of
	// the wqw project
	public String[] countourNames = { "heartWood", "juvenileWood", "crownBase", "firstDeadBranch" };

	private Map<String, GContour> contours; // optional, if not null, keys must
											// be in contourNames

	/**
	 * Constructor.
	 * 
	 * @param id
	 *            the id of this disc
	 * @param centreY_mm
	 *            the height of the centre of this disc (mm)
	 */
	public GPieceDisc(int id, double centreY_mm) {
		this.id = id;
		this.centreY_mm = centreY_mm;
		rings = new ArrayList<GPieceRing>();
		contours = new HashMap<String, GContour>();

		// Contour management example:
		// ~ GContour juvenileWood = null; // new GContour (...);
		// ~ contours.put ("juvenileWood", juvenileWood);
		// ~ Set contourKeys = contours.keySet ();
		// ~ if (contourKeys.contains ("juvenileWood")) {
		// ~ GContour c = contours.get ("juvenileWood");
		// ~ }

	}

	/**
	 * This method adds a ring to the collection of rings that composes this
	 * disc.
	 * 
	 * @param ring
	 *            a GPieceRing instance
	 */
	public void addRing(GPieceRing ring) {
		getRings().add(ring);
	}

	/**
	 * This method returns the id of the disc.
	 * 
	 * @return an integer
	 */
	public int getId() {
		return id;
	}

	/**
	 * This method returns the height of the disc (m).
	 * 
	 * @return a double
	 */
	public double getHeight_m() {
		return centreY_mm * .001;
	}

	/**
	 * This method returns the rings that compose the disc.
	 * 
	 * @return a Collection of GPieceRing instances
	 */
	public ArrayList<GPieceRing> getRings() {
		return rings;
	}

	/**
	 * This method returns the number of wood rings.
	 * 
	 * @return an integer
	 */
	public int getNbWoodRings() {
		return getRings().size() - 2; // first ring = bark, last ring = pith
	}

	/**
	 * This method returns the ring that corresponds to a given cambial age
	 * (which means the age from the bark towards the pith).
	 * 
	 * @param cambialAge
	 *            the age (yr)
	 * @return a GPieceRing instance
	 */
	public GPieceRing getRing(int cambialAge) {
		return getRings().get(cambialAge);
	}

	/**
	 * This method returns the ring that corresponds to the pith.
	 * 
	 * @return a GPieceRing instance
	 */
	public GPieceRing getPithRing() {
		return getRing(getRings().size() - 1);
	}

	/**
	 * This methods returns the map of the GContour associated to this disc.
	 * 
	 * @return a Map with contourNames as keys and GContour instances as values
	 */
	public Map<String, GContour> getContours() {
		return contours;
	}

	/**
	 * This method returns the age from the pith to the ring designated by this
	 * particular cambial age.
	 * 
	 * @param cambialAge
	 *            an integer (yr)
	 * @return an integer, the age (yr)
	 */
	public int getAgeFromPith(int cambialAge) {
		return getNbWoodRings() + 1 - cambialAge;
	}

	/**
	 * This method returns the radius of heartwood on this disc. If there is no
	 * heartwood it returns 0.
	 * 
	 * @return a double
	 */
	public double getHeartWoodRadius_mm() {
		if (getContours().containsKey("heartWood")) {
			return getContours().get("heartWood").getMeanRadius_mm();
		} else {
			return 0d;
		}
	}

	/**
	 * This method returns the cambial age at a given radius from the pith.
	 * 
	 * @param radius_mm
	 *            the radius in mm
	 * @return an integer - the age (yr)
	 */
	public int getCambialAge(double radius_mm) {
		int nbRings = getNbWoodRings();
		int cambialAge = nbRings;
		if (radius_mm > 0.0) {
			for (; cambialAge > 0; cambialAge--) {
				double r = getExtRadius_mm(cambialAge);
				if (r > radius_mm) {
					// cambialAge --;
					break;
				}
			}
		}
		return cambialAge;
	}

	/**
	 * This method returns the radius at the external side of the ring (this
	 * ring is not accounted for).
	 * 
	 * @param cambialAge
	 *            an integer - the age (yr)
	 * @return a radius (mm)
	 */
	public double getExtRadius_mm(int cambialAge) {
		if (cambialAge < getRings().size()) {
			return getRing(cambialAge).getMeanRadius_mm();
		} else {
			return 0d;
		}
	}

	/**
	 * This method returns the radius at the internal side (toward pith) of the
	 * ring for a given cambial age. It should work for any ring (even the
	 * pith).
	 * 
	 * @param cambialAge age (yr)
	 * @return the radius (mm)
	 */
	public double getIntRadius_mm(int cambialAge) {
		return getExtRadius_mm(cambialAge + 1);
	}

	/**
	 * This method returns the number of sapwood rings.
	 * 
	 * @return an integer
	 */
	public int getNbSapWoodRings() {
		return getCambialAge(getHeartWoodRadius_mm());
	}

	public int getNbClearKnotRings() {
		return getCambialAge(getKnottyCoreRadius_mm());
	}

	/**
	 * This method returns the radius of knotty core on this disc. If there is
	 * no knotty core it returns 0.
	 * 
	 * @return the radius (mm)
	 */
	public double getKnottyCoreRadius_mm() {
		if (getContours().containsKey("crownBase")) {
			return getContours().get("crownBase").getMeanRadius_mm();
		} else {
			return 0d;
		}
	}

	/**
	 * This method returns the radius of the zone associated to the lowest dead
	 * branche. If there is no zone, it returns 0.
	 * 
	 * @return the radius (mm)
	 */
	public double getFirstDeadBranchRadius_mm() {
		if (getContours().containsKey("firstDeadBranch")) {
			return getContours().get("firstDeadBranch").getMeanRadius_mm();
		} else {
			return 0d;
		}
	}

	/**
	 * This method returns the radius of juvenile wood. If there is no juvenile
	 * wood, it returns 0.
	 * 
	 * @return the radius (mm)
	 */
	public double getJuvenileWoodRadius_mm() {
		if (getContours().containsKey("juvenileWood")) {
			return getContours().get("juvenileWood").getMeanRadius_mm();
		} else {
			return 0d;
		}
	}

	/**
	 * This method returns the number of rings of juvenile wood
	 * 
	 * @return an integer
	 */
	public int getNbJuvenileWoodRings() {
		return getCambialAge(getJuvenileWoodRadius_mm());
	}

	/**
	 * This method returns the radius of the disk.
	 * 
	 * @param overBark
	 *            true to have the overbark radius or false to get the underbark
	 *            radius
	 * @return the radius (mm)
	 */
	public double getRadius_mm(boolean overBark) {
		GContour contour = getRing(overBark ? 0 : 1);
		return contour.getMeanRadius_mm();
	}

	/**
	 * This method returns the radius between two cambial ages. The bounds are
	 * included.
	 * 
	 * @param cambialAgeMin
	 *            the first cambial age (yr)
	 * @param cambialAgeMax
	 *            the second cambial age (yr)
	 * @return the radius (mm)
	 */
	private double getWidth_mm(int cambialAgeMin, int cambialAgeMax) {
		// Should works for pith (if lastRingIsPith) and bark (if
		// firstRingisBark).
		return getExtRadius_mm(cambialAgeMin) - getIntRadius_mm(cambialAgeMax);
	}

	/**
	 * This method returns the ring width for a ring associated to a particular
	 * cambial age
	 * 
	 * @param cambialAge
	 *            the cambial age (yr)
	 * @return the ring width (mm)
	 */
	private double getRingWidth_mm(int cambialAge) {
		// Should works for pith (if lastRingIsPith) and bark (if
		// firstRingisBark).
		return getWidth_mm(cambialAge, cambialAge);
	}

	/**
	 * This method returns the bark width.
	 * 
	 * @return the bark width (mm).
	 */
	public double getBarkWidth_mm() {
		return getRingWidth_mm(0);
		// unsafe for the top : return getRadius_mm (disc, true) - getRadius_mm
		// (disc, false);
	}

	/**
	 * This method returns the average ring width for the segment of rings
	 * between two cambial ages. The bounds are included.
	 * 
	 * @param cambialAgeMin
	 *            the first cambial age (yr)
	 * @param cambialAgeMax
	 *            the second cambial age (yr)
	 * @return the average ring width (mm)
	 */
	public double getRingWidth_mm(int cambialAgeMin, int cambialAgeMax) {
		int nbRings = cambialAgeMax - cambialAgeMin + 1;
		return nbRings > 0 ? getWidth_mm(cambialAgeMin, cambialAgeMax) / nbRings : 0.0;
	}

	/**
	 * This method returns the average ring width for the whole disc.
	 * 
	 * @return the average ring width (mm)
	 */
	public double getRingWidth_mm() {
		return getRingWidth_mm(1, getNbWoodRings());
	}

	/**
	 * This method returns the width of the segment from the pith to the bark,
	 * i.e., half the diameter.
	 * 
	 * @return the width from the pith to the bark (mm)
	 */
	public double getPithWidth_mm() {
		GPieceRing pith = getPithRing();
		return pith.getMeanRadius_mm();
	}

	/**
	 * This method returns the surface encompassed between two cambial age.
	 * 
	 * @param cambialAgeMin
	 *            the first cambial age (yr)
	 * @param cambialAgeMax
	 *            the second cambial age (yr)
	 * @return the surface (mm2)
	 */
	public double getSurface_mm2(int cambialAgeMin, int cambialAgeMax) {
		// Should works for pith (if lastRingIsPith) and bark (if
		// firstRingisBark).
		double R2 = getExtRadius_mm(cambialAgeMin);
		double R1 = getIntRadius_mm(cambialAgeMax);
		return Math.PI * (R2 * R2 - R1 * R1);
	}

	/**
	 * This method returns the surface of the ring denoted by the cambial age.
	 * 
	 * @param cambialAge
	 *            the cambial age (yr)
	 * @return the surface of the ring (mm2)
	 */
	public double getSurface_mm2(int cambialAge) {
		// Should works for pith (if lastRingIsPith) and bark (if
		// firstRingisBark).
		return getSurface_mm2(cambialAge, cambialAge);
	}

	// Surface excluding pith and bark (m2) :
	// (used by getVolume methods)
	/**
	 * This method returns the wood surface between two cambial ages. The
	 * surface excludes the pith and the bark. It is used in the getVolume
	 * methods
	 * 
	 * @param cambialAgeMin
	 *            the first cambial age (yr)
	 * @param cambialAgeMax
	 *            the second cambial age (yr)
	 * @return the wood surface (m2)
	 */
	public double getWoodSurface_m2(int cambialAgeMin, int cambialAgeMax) {
		double surf = 0.0;
		int nbWoodRings = getNbWoodRings();
		if (nbWoodRings > 0) {
			if (cambialAgeMax < 1)
				cambialAgeMax = 1;
			if (cambialAgeMin > nbWoodRings)
				cambialAgeMin = nbWoodRings;
			if (cambialAgeMin <= cambialAgeMax) {
				surf = getSurface_mm2(cambialAgeMin, cambialAgeMax) * 1e-6; // m2
			}
		}
		return surf;
	}

	/*
	 * This method returns the coordinates of the pith. UNUSED.
	 */
	// public Vertex3d getDiscPithVertex_mm() {
	// GPieceRing pith = getPithRing();
	// return new Vertex3d (pith.centreX_mm, centreY_mm, pith.centreZ_mm);
	// }

	/**
	 * A representation of the disc in a String to check its structure.
	 */
	public String toString() {
		StringBuffer b = new StringBuffer(" GPieceDisc: " + getId() + " h_mm: " + centreY_mm + " nRings: "
				+ getRings().size());
		for (GPieceRing r : getRings()) {
			b.append("\n");
			b.append(r.toString());

		}
		return b.toString();

	}

}
