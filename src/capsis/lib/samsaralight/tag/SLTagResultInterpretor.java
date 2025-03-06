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

package capsis.lib.samsaralight.tag;

import java.io.Serializable;

import capsis.lib.samsaralight.SLLightableTarget;
import capsis.lib.samsaralight.SLLightableTree;
import capsis.lib.samsaralight.SLTargetLightResult;
import capsis.lib.samsaralight.SLTreeLightResult;
// fa-12.07.2017

/**
 * An interpretor for the tag results given a particular tree or cell for
 * SamsaraLight in tagMode.
 * 
 * @author F. de Coligny, Frédéric André - April 2017
 */
public class SLTagResultInterpretor implements Serializable {

	/**
	 * Given a particular tree, explores the tagTreeResults to summarize results
	 * for a given set of tags. Potential energy results are ignored.
	 * 
	 * Returns energy in MJ
	 */
	static public double getEnergy(SLLightableTree tree, SLTag beamTag, SLTag energyTag) {
		SLTreeLightResult treeLight = tree.getLightResult();

		double sum = 0;

		// fc-6.9.2017 there was an error here during reconstruction for Beech
		if (treeLight.getTagResultMap() == null || treeLight.getTagResultMap().isEmpty ())
			return 0;
		
		for (SLTagTreeResult r : treeLight.getTagResultMap().allValues()) {

			// Ignore the results related to POTENTIAL energy
			boolean ignore = (r.energyTag != null && (r.energyTag.getName ().contains ("POTENTIAL")
					
					));

			if (ignore) 
				continue;
			
//			if (r.energyTag != null && r.energyTag.equals(SLTag.CROWN_POTENTIAL))
//				continue;
//			if (r.energyTag != null && r.energyTag.equals(SLTag.LOWER_CROWN_POTENTIAL))
//				continue;

			// If tags are given, restrict to them
			if (beamTag != null && !r.beamTag.equals(beamTag))
				continue;
			if (energyTag != null && !r.energyTag.equals(energyTag))
				continue;

			sum += r.energy_MJ;

		}

		return sum;
	}

	/**
	 * Given a particular tree, explores the tagTreeResults to summarize results
	 * for a given set of tags. Potential energy results only are considered.
	 */
	static public double getPotentialEnergy(SLLightableTree tree, SLTag beamTag, SLTag energyTag) {
		SLTreeLightResult treeLight = tree.getLightResult();

		double sum = 0;

		for (SLTagTreeResult r : treeLight.getTagResultMap().allValues()) {

			// consider only the results related to POTENTIAL energy
			boolean keep = (r.energyTag != null && (r.energyTag.getName ().contains ("POTENTIAL")
					));

			if (!keep) 
				continue;
			
			// If tags are given, restrict to them
			if (beamTag != null && !r.beamTag.equals(beamTag))
				continue;
			if (energyTag != null && !r.energyTag.equals(energyTag))
				continue;

			sum += r.energy_MJ;

		}

		return sum;
	}

	// getPotentialEnergy (...) LATER

	/**
	 * Given a particular target (cell, sensor...), explores the tagResults to summarize results
	 * for a given set of tags.
	 */
	static public double getEnergy(SLLightableTarget target, SLTag beamTag, SLTag energyTag, SLTag orientationTag) { // fa-08.01.2018: added orientationTag
		SLTargetLightResult lightResult = target.getLightResult();

		double sum = 0;

		// System.out.println("\nSLTagResultInterpretor Cell getEnergy ()...");
		
		//fa-05.01.2018
		if (lightResult.getTagResults() == null) // no energy transmitted to the target
			return sum;

		for (SLTagTargetResult r : lightResult.getTagResults()) {

			if (beamTag != null && !r.beamTag.equals(beamTag))
				continue;
			if (energyTag != null && !r.energyTag.equals(energyTag))
				continue;
			//fa-08.01.2018
			if (orientationTag != null && !r.orientationTag.equals(orientationTag))
				continue;

			// System.out.println("   adding "+r.energy_MJm2+"...");

			sum += r.energy_MJm2;

		}

		// System.out.println("   sum: "+sum);

		return sum;
	}
	
	// fa-12.07.2017
	/**
	 * Given a particular sensor, explores the tagSensorResults to summarize results
	 * for a given set of tags.
	 */
	// fc+fa-2.8.2017 REMOVED, replaced by method just upper, for cells and sensors
//	static public double getEnergy(SLSensor sensor, SLTag beamTag, SLTag energyTag) {
//
//		double sum = 0;
//
//		// System.out.println("\nSLTagResultInterpretor Sensor getEnergy ()...");
//
//		for (SLTagTargetResult r : sensor.getTagSensorResults()) {
//
//			if (beamTag != null && !r.beamTag.equals(beamTag))
//				continue;
//			if (energyTag != null && !r.energyTag.equals(energyTag))
//				continue;
//
//			// System.out.println("   adding "+r.energy_MJm2+"...");
//
//			sum += r.energy_MJm2;
//
//		}
//
//		// System.out.println("   sum: "+sum);
//
//		return sum;
//	}

}
