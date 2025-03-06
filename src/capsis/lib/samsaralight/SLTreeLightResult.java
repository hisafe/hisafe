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
import java.util.Iterator;

import capsis.lib.samsaralight.tag.SLTag;
import capsis.lib.samsaralight.tag.SLTagResultInterpretor;
import capsis.lib.samsaralight.tag.SLTagTreeResult;
import jeeb.lib.util.ListMapThreadSafe;

/**
 * SLTreeLightResult - light related properties of a tree.
 *
 * @author B. Courbaud, N. Donès, M. Jonard, G. Ligot, F. de Coligny - October
 *         2008 / June 2012
 */
public class SLTreeLightResult implements Serializable {

	private SLLightableTree connectedTree; // The tree which we are the light
											// properties

	private int impactNumber;

	// fc+fa-25.4.2017
	// In tagMode, additional tag level results are added in this map
	public ListMapThreadSafe<String, SLTagTreeResult> tagResultMap; // fc+fa-18.5.2017
																	// map
	// public List<SLTagTreeResult> tagResultMap;

	// fc+fa-26.4.2017 management of crownPotentialEnergy at the tree level
	// REMOVING all references to potential energy in the tree parts
	// trunk potential energy is not computed any more (generally unused)
	protected double crownPotentialEnergy;
	protected double lowerCrownPotentialEnergy; // if available

	/**
	 * Constructor.
	 */
	public SLTreeLightResult(SLLightableTree connectedTree) {
		this.connectedTree = connectedTree;

		this.crownPotentialEnergy = 0;
		this.lowerCrownPotentialEnergy = 0;

	}

	/**
	 * This method resets all energy of the tree, including energy in trunk and
	 * crownParts if any.
	 */
	// fc+fa-26.4.2017
	public void resetEnergy() {
		
		impactNumber = 0;
		crownPotentialEnergy = 0;
		lowerCrownPotentialEnergy = 0;

		tagResultMap = null;

		// fc-10.1.2018 refactored, clearer, removed tree.resetEnergy ()
		// connectedTree.resetEnergy();

		// fc-10.1.2018 moved this code from former tree.resetEnergy ()
		// this code is better here, implemented only once
		if (connectedTree.getTrunk() != null)
			connectedTree.getTrunk().resetEnergy();

		if (connectedTree.getCrownParts() != null)
			for (SLCrownPart cp : connectedTree.getCrownParts()) {
				cp.resetEnergy();
			}

	}

	/**
	 * Sometimes, the light is not calculated each year to save time and copies
	 * are used instead.
	 */
	public SLTreeLightResult getCopy(SLLightableTree newConnectedTree) {
		SLTreeLightResult tl = new SLTreeLightResult(newConnectedTree);

		tl.impactNumber = this.impactNumber;
		tl.tagResultMap = this.tagResultMap; // fc+fa-25.4.2017 // fc-22.6.2017
												// checked

		tl.crownPotentialEnergy = this.crownPotentialEnergy; // fc+fa-26.4.2017
		tl.lowerCrownPotentialEnergy = this.lowerCrownPotentialEnergy; // fc+fa-26.4.2017

		// fc-22.6.2017 REMOVED, we actually 'copy' the map
		// fc-9.5.2017 was missing
		// tl.tagResultMap = null;

		return tl;
	}

	public int numberOfAddResult;

	// fc+fa-25.4.2017
	synchronized public void addTagTreeResult(SLTagTreeResult r) {
		if (r.energy_MJ == 0)
			return;

		if (tagResultMap == null)
			tagResultMap = new ListMapThreadSafe<>(); // fc+fa-25.4.2017

		tagResultMap.addObject(r.beamTag.getName(), r);

		numberOfAddResult++;
	}

	// fc+fa-25.4.2017
	// In tagMode, additional tag level results are added in this list
	public ListMapThreadSafe<String, SLTagTreeResult> getTagResultMap() {
		return tagResultMap;
	}

	/**
	 * Copies the energy values from source tree to dest tree.
	 */
	static public void copyEnergyValues(SLLightableTree sourceTree, SLLightableTree destTree) throws Exception {
		
		// fc+bc-3.2.2022 Trunk is optional (e.g. Samsara2, no trunks)
		if (destTree.getTrunk () != null) {
			// fc+mj-9.3.2017
			destTree.getTrunk().resetEnergy();
			// destTree.getTrunk().addPotentialEnergy(sourceTree.getTrunk().getPotentialEnergy());
			destTree.getTrunk().addDirectEnergy(sourceTree.getTrunk().getDirectEnergy());
			destTree.getTrunk().addDiffuseEnergy(sourceTree.getTrunk().getDiffuseEnergy());
		}
		
		Iterator<SLCrownPart> source = sourceTree.getCrownParts().iterator();
		Iterator<SLCrownPart> dest = destTree.getCrownParts().iterator();
		while (source.hasNext() && dest.hasNext()) {
			SLCrownPart cpSource = source.next();
			SLCrownPart cpDest = dest.next();

			// Reset and set new value
			cpDest.resetEnergy();
			// cpDest.addPotentialEnergy(cpSource.getPotentialEnergy());
			cpDest.addDirectEnergy(cpSource.getDirectEnergy());
			cpDest.addDiffuseEnergy(cpSource.getDiffuseEnergy());

		}

		if (sourceTree == null)
			System.out.println("SLTreeLightResult, sourceTree == null");
		if (sourceTree.getLightResult() == null)
			System.out.println("SLTreeLightResult, sourceTree.getLightResult() == null");
		if (destTree == null)
			System.out.println("SLTreeLightResult, destTree == null");
		if (destTree.getLightResult() == null)
			System.out.println("SLTreeLightResult, destTree.getLightResult() == null");
		
		// fc+nb-22.5.2023 tagResultMap may be null
		if (sourceTree.getLightResult().tagResultMap != null)
			destTree.getLightResult().tagResultMap = sourceTree.getLightResult().tagResultMap; // fc+fa-25.4.2017
		destTree.getLightResult().crownPotentialEnergy = sourceTree.getLightResult().crownPotentialEnergy; // fc+fa-26.4.2017
		destTree.getLightResult().lowerCrownPotentialEnergy = sourceTree.getLightResult().lowerCrownPotentialEnergy; // fc+fa-26.4.2017

	}

	public double getTrunkDirectEnergy() {
		return connectedTree.getTrunk().getDirectEnergy();
	}

	public double getTrunkDiffuseEnergy() {
		return connectedTree.getTrunk().getDiffuseEnergy();
	}

	public double getTrunkEnergy() {
		return getTrunkDirectEnergy() + getTrunkDiffuseEnergy();
	}

	// REMOVED fc-fa-26.4.2017 unused
	// public double getTrunkPotentialEnergy() {
	// return connectedTree.getTrunk().getPotentialEnergy();
	// }

	// fc-9.5.2017 was missing
	public SLLightableTree getConnectedTree() {
		return connectedTree;
	}

	public double getCrownDirectEnergy() {
		double sum = 0;
		for (SLCrownPart p : connectedTree.getCrownParts()) {
			sum += p.getDirectEnergy();
		}
		return sum;
	}

	public double getCrownDiffuseEnergy() {
		double sum = 0;
		for (SLCrownPart p : connectedTree.getCrownParts()) {
			sum += p.getDiffuseEnergy();
		}
		return sum;
	}

	public double getCrownEnergy() {
		return getCrownDirectEnergy() + getCrownDiffuseEnergy();
	}

	// fc-fa-26.4.2017 Added this variable
	public double getCrownPotentialEnergy() {
		return crownPotentialEnergy;
	}

	// fc-fa-26.4.2017 Added this variable
	public double getLowerCrownPotentialEnergy() {
		return lowerCrownPotentialEnergy;
	}

	// fc-fa-26.4.2017
	synchronized public void addCrownPotentialEnergy(double v) {
		crownPotentialEnergy += v;
	}

	// fc-fa-26.4.2017
	synchronized public void addLowerCrownPotentialEnergy(double v) {
		lowerCrownPotentialEnergy += v;
	}

	// fc-fa-26.4.2017 REPLACED by add and get, see upper fc-fa-26.4.2017
	// public double getCrownPotentialEnergy() {
	// double sum = 0;
	// for (SLCrownPart p : connectedTree.getCrownParts()) {
	// sum += p.getPotentialEnergy();
	// }
	// return sum;
	// }

	public int getImpactNumber() {
		return impactNumber;
	}

	// fc+bc-16.2.2022 Unused, see resetEnergy () and incrementImpactNumber ()
//	public void setImpactNumber(int n) {
//		impactNumber = n;
//	}

	public void incrementImpactNumber() {
		impactNumber += 1;
	}

	// public void resetImpactNumber() {
	// impactNumber = 0;
	// }

	public double getControlLegacyTreeEnergy() {
		return getTrunkEnergy() + getCrownEnergy();
	}

	public double getControlTagBasedTreeEnergy() {
		SLTag beamTag = null;
		SLTag energyTag = null;
		return SLTagResultInterpretor.getEnergy(connectedTree, beamTag, energyTag);
	}

}
