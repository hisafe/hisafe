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
import java.util.Collection;
import java.util.Iterator;

import jeeb.lib.util.Log;
import jeeb.lib.util.RecordSet;

/**
 * A samsaraLigt class that read txt file with SamsaraLight options
 * 
 * @author B. Courbaud, N. Donès, M. Jonard, G. Ligot, F. de Coligny - October
 *         2008 / June 2012
 */
public class SLSettingsLoader extends RecordSet {

	// fc-1.10.2024 This class was replaced by SLSettingsLoader2024
	
//	private String fileName;
//
//	/**
//	 * Constructor.
//	 */
//	public SLSettingsLoader() throws Exception {
//
//		// fc-8.12.2017 Added this default constructor to comply better with the
//		// RecordSet framework and the CheckingFileChooser (call the default
//		// constructor, then prepareImport (fileName)).
//
//		super();
//		
//		// Add classes for lines recognition
//		addAdditionalClass(SLMonthlyRecord.class);
//		addAdditionalClass(SLHourlyRecord.class);
//	}
//
//	/**
//	 * Constructor.
//	 */
//	public SLSettingsLoader(String fileName) throws Exception {
//		
//		// fc-8.12.2017 Chaining constructors
//		this ();
//		
//		this.fileName = fileName;
//
//		prepareImport (fileName);
//
//	}
//
//	/**
//	 * Interprets the file, loads information in the given settings object.
//	 */
//	public void interpret(SLSettings sets) throws Exception {
//
//		Collection keys = new ArrayList(); // to check if keys are missing
//
//		// nb-11.05.2017. In order to avoid a bad calculation of the total
//		// number
//		// of monthly records in the case the interpret(SLSettings) method is
//		// called several times.
//		if ((sets.getMontlyRecords() != null) && (!sets.getMontlyRecords().isEmpty())) {
//			sets.getMontlyRecords().clear();
//		}
//
//		// nb-11.05.2017. Idem: see above.
//		// if ( (sets.getHourlyRecords() != null) &&
//		// (sets.getHourlyRecords().isEmpty()) ) {
//		// fc-27.7.2017 added missing ! below
//		if ((sets.getHourlyRecords() != null) && (!sets.getHourlyRecords().isEmpty())) {
//			sets.getHourlyRecords().clear();
//		}
//
//		for (Iterator i = this.iterator(); i.hasNext();) {
//			Object record = i.next();
//
//			if (record instanceof SLMonthlyRecord) {
//				SLMonthlyRecord r = (SLMonthlyRecord) record;
//				sets.addMontlyRecord(r);
//			} else if (record instanceof SLHourlyRecord) {
//				SLHourlyRecord r = (SLHourlyRecord) record;
//				sets.addHourlyRecord(r);
//
//			} else if (record instanceof KeyRecord) {
//				KeyRecord r = (KeyRecord) record;
//				keys.add(r.key);
//
//				if (r.hasKey("turbid_medium")) {
//					try {
//						sets.setTurbidMedium(r.getBooleanValue());
//					} catch (Exception e) {
//						Log.println(Log.ERROR, "SLSettingsLoader.interpret ()", "Trouble with turbid_medium", e);
//						throw e;
//					}
//
//				} else if (r.hasKey("trunk_interception")) {
//					try {
//						sets.setTrunkInterception(r.getBooleanValue());
//					} catch (Exception e) {
//						Log.println(Log.ERROR, "SLSettingsLoader.interpret ()", "Trouble with trunkInterception", e);
//						throw e;
//					}
//
//				} else if (r.hasKey("direct_angle_step")) {
//					try {
//						sets.setDirectAngleStep(r.getDoubleValue());
//					} catch (Exception e) {
//						Log.println(Log.ERROR, "SLSettingsLoader.interpret ()", "Trouble with directAngleStep", e);
//						throw e;
//					}
//
//				} else if (r.hasKey("height_angle_min")) {
//					try {
//						sets.setHeightAngleMin(r.getDoubleValue());
//					} catch (Exception e) {
//						Log.println(Log.ERROR, "SLSettingsLoader.interpret ()", "Trouble with heightAngleMin", e);
//						throw e;
//					}
//
//				} else if (r.hasKey("diffuse_angle_step")) {
//					try {
//						sets.setDiffuseAngleStep(r.getDoubleValue());
//					} catch (Exception e) {
//						Log.println(Log.ERROR, "SLSettingsLoader.interpret ()", "Trouble with diffuseAngleStep", e);
//						throw e;
//					}
//
//				} else if (r.hasKey("soc")) {
//					try {
//						sets.setSoc(r.getBooleanValue());
//					} catch (Exception e) {
//						Log.println(Log.ERROR, "SLSettingsLoader.interpret ()", "Trouble with soc", e);
//						throw e;
//					}
//
//				} else if (r.hasKey("GMT")) {
//					try {
//						sets.setGMT(r.getIntValue());
//					} catch (Exception e) {
//						Log.println(Log.ERROR, "SLSettingsLoader.interpret ()", "Trouble with GMT", e);
//						throw e;
//					}
//				} else if (r.hasKey("leaf_on_doy")) {
//					try {
//						sets.setLeafOnDoy(r.getIntValue());
//					} catch (Exception e) {
//						Log.println(Log.ERROR, "SLSettingsLoader.interpret ()", "Trouble with leafOnDoy", e);
//						throw e;
//					}
//
//				} else if (r.hasKey("leaf_off_doy")) {
//					try {
//						sets.setLeafOffDoy(r.getIntValue());
//					} catch (Exception e) {
//						Log.println(Log.ERROR, "SLSettingsLoader.interpret ()", "Trouble with leafOffDoy", e);
//						throw e;
//					}
//
//					// fc-29.6.2017 added this section, sensorLightOnly is now
//					// in SamsaraLight file
//				} else if (r.hasKey("sensorLightOnly")) {
//					try {
//						sets.setSensorLightOnly(r.getBooleanValue());
//					} catch (Exception e) {
//						Log.println(Log.ERROR, "SLSettingsLoader.interpret ()", "Trouble with sensorLightOnly", e);
//						throw e;
//					}
//
//				}
//
//			} else {
//				throw new Exception("wrong format in " + fileName + " near record " + record);
//			}
//
//		}
//
//		// If monthlyRecords were found, we expect 12 exactly
//		if (sets.getMontlyRecords() != null && sets.getMontlyRecords().size() != 12) {
//			throw new Exception("Error in file " + fileName + ", wrong number of monthly records: "
//					+ sets.getMontlyRecords().size() + ", should be 12");
//		}
//
//	}

}
