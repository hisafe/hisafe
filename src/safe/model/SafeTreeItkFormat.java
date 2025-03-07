/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE 
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
 * Hi-SAFE is free software under the terms of the CC-BY License as published by the Creative Commons Corporation
 *
 * You are free to:
 *		Share — copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt — remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution — 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
 *               
 * Notices:
 * 		You do not have to comply with the license for elements of the material in the public domain or where your use is permitted 
 *      by an applicable exception or limitation .
 *		No warranties are given. The license may not give you all of the permissions necessary for your intended use. 
 *		For example, other rights such as publicity, privacy, or moral rights may limit how you use the material.  
 *
 * For more details see <https://creativecommons.org/licenses/by/4.0/>.
 *
 */

package safe.model;


import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.CancellationException;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;

/**
 * TREE management parameters Format
 *
 * @author : Isabelle LECOMTE  - INRA SYSTEM Montpellier (March 2003)
 */
public class SafeTreeItkFormat extends RecordSet {

	// Generic keyword record is described in superclass RecordSet : key = value
	// We use only key records  here

	private static final long serialVersionUID = 1L;

	public SafeTreeItkFormat (String fileName) throws Exception {
		try {
			prepareImport (fileName);
		
		} catch (Exception e) {
			 System.out.println("TREE ITK FILE NOT FOUND " + fileName); 
			 System.exit(1);
		}
	
	}

	//julian days replaced by MM-JJ (IL 25/05/2023)
	public int getJulianDay (String dateMMDD) throws Exception
	{
		if (dateMMDD.equals ("999")) return 999;
		if (dateMMDD.equals ("0")) return 0;
		int year = 1999;	//it could be any year 
		if (dateMMDD.length()!=5) 	throw new CancellationException();	// abort
		if (!dateMMDD.contains("-")) 	throw new CancellationException();	// abort
		String [] part1 = dateMMDD.split("-");
		GregorianCalendar date = new GregorianCalendar();
		date.set(year,Integer.parseInt(part1[0])-1,Integer.parseInt(part1[1]) );
		int jul =  date.get(GregorianCalendar.DAY_OF_YEAR);
		return jul;
	}
	
	/**
	 * Load RecordSet -> updating SafeTreeSpecies
	 */
	public  void load (SafeTreeItk itk, SafeEvolutionParameters ep) throws Exception {

		Set<String> requiredParameters = new HashSet<>();

		requiredParameters.add("plantingYear");
		requiredParameters.add("plantingDay");	
		requiredParameters.add("plantingCohortAge");
		requiredParameters.add("plantingAge");
		requiredParameters.add("plantingHeight");
		requiredParameters.add("plantingCrownBaseHeight");
		requiredParameters.add("plantingCrownRadius");
		
		requiredParameters.add("plantingRootShape");
		requiredParameters.add("plantingRootRepartition");
		requiredParameters.add("plantingRootShapeParam1");
		requiredParameters.add("plantingRootShapeParam2");
		requiredParameters.add("plantingRootShapeParam3");		
		requiredParameters.add("frostDamageActivation");

		int nbYear = 0;
		
		for (Iterator<Record> i = this.iterator (); i.hasNext ();) {
			Record record = i.next ();

		 	if (record instanceof SafeTreeItkFormat.KeyRecord) {

				SafeTreeItkFormat.KeyRecord r = (SafeTreeItkFormat.KeyRecord) record;

		 		String param = r.key;

//SPECIES	 		

				if (param.equals("plantingYear")) {
					
					try {
						itk.plantingYear = r.getIntValue();
						requiredParameters.remove("plantingYear");
			
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : plantingYear");
						throw new CancellationException();	// abort
					}

				} else if (param.equals("plantingDay")) {
					
					try {
						itk.plantingDay = getJulianDay(r.value);
						requiredParameters.remove("plantingDay");
			
					} catch(Exception e){
						System.out.println("Wrong itk tree file format parameters : plantingDay");
						throw new CancellationException();	// abort
					}
	
				} else if (param.equals("plantingAge")) {
					
					try {
						itk.plantingAge = r.getIntValue();
						requiredParameters.remove("plantingAge");
			
					} catch(CancellationException e){
						System.out.println("Wrong itk tree file format parameters : plantingAge");
						throw new CancellationException();	// abort
					}

				} else if  (r.key.equals ("plantingHeight")) {
					
					try {
						itk.plantingHeight = r.getDoubleValue ();
						requiredParameters.remove("plantingHeight");
			
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : plantingHeight");
						throw new CancellationException();	// abort
					}
					
				} else if  (r.key.equals ("plantingCrownBaseHeight")) {
					
					try {
						
						itk.plantingCrownBaseHeight = r.getDoubleValue ();
						requiredParameters.remove("plantingCrownBaseHeight");
			
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : plantingCrownBaseHeight");
						throw new CancellationException();	// abort
					}			
					
				} else if  (r.key.equals ("plantingCrownRadius")) {
					
					try {
						itk.plantingCrownRadius = r.getDoubleValue ();
						requiredParameters.remove("plantingCrownRadius");
			
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : plantingCrownRadius");
						throw new CancellationException();	// abort
					}
					
				} else if  (r.key.equals ("plantingCohortAge")) {
					
					try {
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.plantingCohortAge.add(new Integer(Integer.parseInt(st[k])));
						}
						requiredParameters.remove("plantingCohortAge");		
			
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : plantingCohortAge");
						throw new CancellationException();	// abort
					}

//ROOT SHAPE INIT				
				} else if (param.equals("plantingRootShape")) {
					
					try {
						itk.plantingRootShape = r.getIntValue();
						if (itk.plantingRootShape < 1 || itk.plantingRootShape > 3) 
							System.out.println("Available value for plantingRootShape parameter = [1,2,3]");						
						else requiredParameters.remove("plantingRootShape");
			
						} catch(NumberFormatException e){
							System.out.println("Wrong itk tree file format parameters : plantingRootShape");
							throw new CancellationException();	// abort
						}

					
				} else if (param.equals("plantingRootRepartition")) {
					
					try {
						itk.plantingRootRepartition = r.getIntValue();
						
						if (itk.plantingRootRepartition < 1 || itk.plantingRootRepartition > 3) 
							System.out.println("Available value for plantingRootRepartition parameter = [1,2,3]");						
						else requiredParameters.remove("plantingRootRepartition");

			
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : plantingRootRepartition");
						throw new CancellationException();	// abort
					}
			
				} else if  (r.key.equals ("plantingRootShapeParam1")) {
					
					try {
						itk.plantingRootShapeParam1 = r.getDoubleValue ();
						requiredParameters.remove("plantingRootShapeParam1");
			
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : plantingRootShapeParam1");
						throw new CancellationException();	// abort
					}

				} else if  (r.key.equals ("plantingRootShapeParam2")) {
					
					try {
						itk.plantingRootShapeParam2 = r.getDoubleValue ();
						requiredParameters.remove("plantingRootShapeParam2");
			
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : plantingRootShapeParam2");
						throw new CancellationException();	// abort
					}
	
				} else if  (r.key.equals ("plantingRootShapeParam3")) {
					
					try {
						itk.plantingRootShapeParam3 = r.getDoubleValue ();
						requiredParameters.remove("plantingRootShapeParam3");	
			
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : plantingRootShapeParam3");
						throw new CancellationException();	// abort
					}

				} else if (param.equals("frostDamageActivation")) {
					
					try {
						int b = r.getIntValue();
						if (b < 0 || b > 1) 
							System.out.println("Available value for frostDamageActivation parameter = [0,1]");						
						else {
							itk.frostDamageActivation = true;
							if (b==0) itk.frostDamageActivation = false;
							requiredParameters.remove("frostDamageActivation");	
						}

					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : frostDamageActivation");
						throw new CancellationException();	// abort
					}

// THINNING

				} else if (param.equals("treeHarvestYear")) {
					
					try {
						itk.treeHarvestYear =  r.getIntValue();	

			
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeHarvestYear");
						throw new CancellationException();	// abort
					}

				} else if (param.equals("treeHarvestDay")) {
					
					try {
						itk.treeHarvestDay =  getJulianDay(r.value);

			
					} catch(Exception e){
						System.out.println("Wrong itk tree file format parameters : treeHarvestDay");
						throw new CancellationException();	// abort
					}

// PRUNING
				} else if (param.equals("treePruningYears")) {
					
					try {
						nbYear = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.treePruningYears.add(new Integer(Integer.parseInt(st[k])));
							nbYear++;
						}
						
						requiredParameters.add("treePruningDays");
						requiredParameters.add("treePruningProp");
						requiredParameters.add("treePruningMaxHeight");
						requiredParameters.add("treePruningResiduesIncorporation");	
						requiredParameters.add("treePruningResiduesSpreading");	
						
					} catch(Exception e){
						System.out.println("Wrong itk tree file format parameters : treePruningYears");
						throw new CancellationException();	// abort
					}
				
				} else if (param.equals("treePruningDays")) {
						
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.treePruningDays.add(getJulianDay (st[k]));
							nbDays++; 
						}
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing treePruningDays");
							throw new CancellationException();	// abort
							
						}
						requiredParameters.remove("treePruningDays");
					} catch(Exception e){
						System.out.println("Wrong itk tree file format parameters : treePruningDays");
						throw new CancellationException();	// abort
					}

				} else if (param.equals("treePruningProp")) {
			
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.treePruningProp.add(new Double(Double.parseDouble(st[k])));
							nbDays++; 
						}
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing treePruningProp");
							throw new CancellationException();	// abort
							
						}
						requiredParameters.remove("treePruningProp");	
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treePruningProp");
						throw new CancellationException();	// abort
					}
				} else if (param.equals("treePruningMaxHeight")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.treePruningMaxHeight.add(new Double(Double.parseDouble(st[k])));
							nbDays++; 
						}
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing treePruningMaxHeight");
							throw new CancellationException();	// abort
							
						}
						requiredParameters.remove("treePruningMaxHeight");
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treePruningMaxHeight");
						throw new CancellationException();	// abort
					}


				} else if (param.equals("treePruningResiduesIncorporation")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							nbDays++; 
							int b = Integer.parseInt(st[k]);
							if (b < 0 || b > 1) {
								System.out.println("Available value for treePruningResidusIncorporation parameter = [0,1]");
								throw new CancellationException();	// abort
							}
							else 
								itk.treePruningResiduesIncorporation.add(new Integer(Integer.parseInt(st[k])));					
						}	
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing treePruningResiduesIncorporation");
							throw new CancellationException();	// abort	
						}
						requiredParameters.remove("treePruningResiduesIncorporation");
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treePruningResiduesIncorporation");
						throw new CancellationException();	// abort
					}
					
				} else if (param.equals("treePruningResiduesSpreading")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							nbDays++; 
							int b = Integer.parseInt(st[k]);
							if (b < 0 || b > 2) {
								System.out.println("Available value for treePruningResiduesSpreading parameter = [0,1,2]");
								throw new CancellationException();	// abort
							}
							else 
								itk.treePruningResiduesSpreading.add(new Integer(Integer.parseInt(st[k])));					
						}	
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing treePruningResiduesSpreading");
							throw new CancellationException();	// abort	
						}
						requiredParameters.remove("treePruningResiduesSpreading");
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treePruningResiduesSpreading");
						throw new CancellationException();	// abort
					}					
// ROOT PRUNING
				} else if (param.equals("treeRootPruningYears")) {

					try {
						nbYear = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							nbYear++; 
							itk.treeRootPruningYears.add(new Integer(Integer.parseInt(st[k])));
						}
						
						requiredParameters.add("treeRootPruningDays");
						requiredParameters.add("treeRootPruningDistance");
						requiredParameters.add("treeRootPruningDepth");
						
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeRootPruningYears");
						throw new CancellationException();	// abort
					}

				} else if (param.equals("treeRootPruningDays")) {
					try {
						String[] st = (r.value).split(",");
						int nbDays = 0;
						for (int k = 0; k < st.length; k++) {
							nbDays++; 
							itk.treeRootPruningDays.add(getJulianDay (st[k]));
						}
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing treeRootPruningDays");
							throw new CancellationException();	// abort
							
						}
						
						requiredParameters.remove("treeRootPruningDays");
						
					} catch(Exception e){
						System.out.println("Wrong itk tree file format parameters : treeRootPruningDays");
						throw new CancellationException();	// abort
					}
				} else if (param.equals("treeRootPruningDistance")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							nbDays++; 
							itk.treeRootPruningDistance.add(new Double(Double.parseDouble(st[k])));
						}
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing treeRootPruningDistance");
							throw new CancellationException();	// abort
							
						}
						
						requiredParameters.remove("treeRootPruningDistance");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeRootPruningDistance");
						throw new CancellationException();	// abort
					}
				} else if (param.equals("treeRootPruningDepth")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							nbDays++; 
							itk.treeRootPruningDepth.add(new Double(Double.parseDouble(st[k])));
						}
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing treeRootPruningDepth");
							throw new CancellationException();	// abort
							
						}
						
						requiredParameters.remove("treeRootPruningDepth");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeRootPruningDepth");
						throw new CancellationException();	// abort
					}


// TREE TOPPPING
				} else if (param.equals("treeTopingYears")) {
					try {
						String[] st = (r.value).split(",");
						nbYear = 0;
						for (int k = 0; k < st.length; k++) {
							nbYear++; 
							itk.treeTopingYears.add(new Integer(Integer.parseInt(st[k])));					
						}
						
						requiredParameters.add("treeToppingDays");
						requiredParameters.add("treeToppingHeight");
						requiredParameters.add("treeToppingResiduesIncorporation");	
						requiredParameters.add("treeToppingResiduesSpreading");
						
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeTopingYears");
						throw new CancellationException();	// abort
					}

				} else if (param.equals("treeTopingDays")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							nbDays++; 
							itk.treeTopingDays.add(getJulianDay (st[k]));
						}
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing treeTopingDays");
							throw new CancellationException();	// abort
							
						}
						
						requiredParameters.remove("treeTopingDays");
						
					} catch(Exception e){
						System.out.println("Wrong itk tree file format parameters : treeToppingDays");
						throw new CancellationException();	// abort
					}
					
				} else if (param.equals("treeTopingHeight")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							nbDays++; 
							itk.treeTopingHeight.add(new Double(Double.parseDouble(st[k])));
						}
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing treeTopingHeight");
							throw new CancellationException();	// abort
							
						}
						
						requiredParameters.remove("treeTopingHeight");
						
					} catch(Exception e){
						System.out.println("Wrong itk tree file format parameters : treeTopingHeight");
						throw new CancellationException();	// abort
					}
					
				} else if (param.equals("treeTopingResiduesIncorporation")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							nbDays++; 
							int b = Integer.parseInt(st[k]);
							if (b < 0 || b > 1) {
								System.out.println("Available value for treeTopingResiduesIncorporation parameter = [0,1]");
								throw new CancellationException();	// abort
							}
							else 
								itk.treeTopingResiduesIncorporation.add(new Integer(Integer.parseInt(st[k])));					
						}	
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing treeTopingResiduesIncorporation");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("treeTopingResiduesIncorporation");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeTopingResiduesIncorporation");
						throw new CancellationException();	// abort
					}
				} else if (param.equals("treeTopingResiduesSpreading")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							nbDays++; 
							int b = Integer.parseInt(st[k]);
							if (b < 0 || b > 2) {
								System.out.println("Available value for treeTopingResiduesSpreading parameter = [0,1,2]");
								throw new CancellationException();	// abort
							}
							else 
								itk.treeTopingResiduesSpreading.add(new Integer(Integer.parseInt(st[k])));					
						}	
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing treeTopingResiduesSpreading");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("treeTopingResiduesSpreading");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeTopingResiduesSpreading");
						throw new CancellationException();	// abort
					}					

// LEAF AREA REDUCTION
				} else if (param.equals("leafAreaDensityReductionYears")) {
					try {
						String[] st = (r.value).split(",");
						nbYear = 0;
						for (int k = 0; k < st.length; k++) {
							nbYear++; 
							itk.leafAreaDensityReductionYears.add(new Integer(Integer.parseInt(st[k])));
						}	
						
						requiredParameters.add("leafAreaDensityReductionDays");
						requiredParameters.add("leafAreaDensityReductionThreshold");
						requiredParameters.add("leafAreaDensityReductionFraction");	
						requiredParameters.add("leafAreaDensityReductionResiduesIncorporation");	
						requiredParameters.add("leafAreaDensityReductionResiduesSpreading");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : leafAreaDensityReductionYears");
						throw new CancellationException();	// abort
					}
					
				} else if (param.equals("leafAreaDensityReductionDays")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							nbDays++; 
							itk.leafAreaDensityReductionDays.add(getJulianDay (st[k]));
						}
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing leafAreaDensityReductionDays");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("leafAreaDensityReductionDays");
						
					} catch(Exception e){
						System.out.println("Wrong itk tree file format parameters : leafAreaDensityReductionDays");
						throw new CancellationException();	// abort
					}
				} else if (param.equals("leafAreaDensityReductionThreshold")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							nbDays++; 
							itk.leafAreaDensityReductionThreshold.add(new Double(Double.parseDouble(st[k])));
						}
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing leafAreaDensityReductionThreshold");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("leafAreaDensityReductionThreshold");
						
					} catch(Exception e){
						System.out.println("Wrong itk tree file format parameters : leafAreaDensityReductionThreshold");
						throw new CancellationException();	// abort
					}
				} else if (param.equals("leafAreaDensityReductionFraction")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							nbDays++; 
							itk.leafAreaDensityReductionFraction.add(new Double(Double.parseDouble(st[k])));
						}
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing leafAreaDensityReductionObjective");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("leafAreaDensityReductionFraction");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : leafAreaDensityReductionObjective");
						throw new CancellationException();	// abort
					}
					
				} else if (param.equals("leafAreaDensityReductionResiduesIncorporation")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							nbDays++; 
							
							int b = Integer.parseInt(st[k]);
							if (b < 0 || b > 1)  {
								System.out.println("Available value for leafAreaDensityReductionResiduesIncorporation parameter = [0,1]");	
								throw new CancellationException();	// abort
							}
							else 
								itk.leafAreaDensityReductionResiduesIncorporation.add(new Integer(Integer.parseInt(st[k])));	
			
						}	
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing leafAreaDensityReductionResiduesIncorporation");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("leafAreaDensityReductionResiduesIncorporation");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : leafAreaDensityReductionResiduesIncorporation");
						throw new CancellationException();	// abort
					}
				} else if (param.equals("leafAreaDensityReductionResiduesSpreading")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							nbDays++; 
							
							int b = Integer.parseInt(st[k]);
							if (b < 0 || b > 2)  {
								System.out.println("Available value for leafAreaDensityReductionResiduesSpreading parameter = [0,1,2]");	
								throw new CancellationException();	// abort
							}
							else 
								itk.leafAreaDensityReductionResiduesSpreading.add(new Integer(Integer.parseInt(st[k])));	
			
						}	
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing leafAreaDensityReductionResiduesSpreading");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("leafAreaDensityReductionResiduesSpreading");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : leafAreaDensityReductionResiduesSpreading");
						throw new CancellationException();	// abort
					}					
					
// CANOPY TRIMMING
				} else if (param.equals("canopyTrimmingYears")) {
					try {
						String[] st = (r.value).split(",");
						nbYear = 0;
						for (int k = 0; k < st.length; k++) {
							itk.canopyTrimmingYears.add(new Integer(Integer.parseInt(st[k])));
							nbYear++;
						}
						
						requiredParameters.add("canopyTrimmingDays");
						requiredParameters.add("canopyTrimmingTreeLineTrigger");
						requiredParameters.add("canopyTrimmingTreeLineReductionTarget");	
						requiredParameters.add("canopyTrimmingInterRowTrigger");	
						requiredParameters.add("canopyTrimmingInterRowReductionTarget");
						requiredParameters.add("canopyTrimmingResiduesIncorporation");
						requiredParameters.add("canopyTrimmingResiduesSpreading");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : canopyTrimmingYears");
						throw new CancellationException();	// abort
					}

					
				} else if (param.equals("canopyTrimmingDays")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.canopyTrimmingDays.add(getJulianDay (st[k]));
							nbDays++;
						}
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing canopyTrimmingDays");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("canopyTrimmingDays");
						
					} catch(Exception e){
						System.out.println("Wrong itk tree file format parameters : canopyTrimmingDays");
						throw new CancellationException();	// abort
					}
					
				} else if (param.equals("canopyTrimmingTreeLineTrigger")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.canopyTrimmingTreeLineTrigger.add(new Double(Double.parseDouble(st[k])));
							nbDays++;
						}
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing canopyTrimmingTreeLineTrigger");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("canopyTrimmingTreeLineTrigger");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : canopyTrimmingTreeLineTrigger");
						throw new CancellationException();	// abort
					}
				} else if (param.equals("canopyTrimmingTreeLineReductionTarget")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.canopyTrimmingTreeLineReductionTarget.add(new Double(Double.parseDouble(st[k])));
							nbDays++;
						}	
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing canopyTrimmingTreeLineReductionTarget");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("canopyTrimmingTreeLineReductionTarget");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : canopyTrimmingTreeLineReductionTarget");
						throw new CancellationException();	// abort
					}
					
					
				} else if (param.equals("canopyTrimmingInterRowTrigger")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.canopyTrimmingInterRowTrigger.add(new Double(Double.parseDouble(st[k])));
							nbDays++;
						}
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing canopyTrimmingInterRowTrigger");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("canopyTrimmingInterRowTrigger");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : canopyTrimmingInterRowTrigger");
						throw new CancellationException();	// abort
					}
				} else if (param.equals("canopyTrimmingInterRowReductionTarget")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.canopyTrimmingInterRowReductionTarget.add(new Double(Double.parseDouble(st[k])));
							nbDays++;
						}	
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing canopyTrimmingInterRowReductionTarget");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("canopyTrimmingInterRowReductionTarget");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : canopyTrimmingInterRowReductionTarget");
						throw new CancellationException();	// abort
					}
									
				} else if (param.equals("canopyTrimmingResiduesIncorporation")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							
							int b = Integer.parseInt(st[k]);
							if (b < 0 || b > 1)  {
								System.out.println("Available value for canopyTrimmingResiduesIncorporation parameter = [0,1]");	
								throw new CancellationException();	// abort
							}
							else 
								itk.canopyTrimmingResiduesIncorporation.add(new Integer(Integer.parseInt(st[k])));

							nbDays++;
						}	
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing canopyTrimmingResiduesIncorporation");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("canopyTrimmingResiduesIncorporation");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : canopyTrimmingResiduesIncorporation");
						throw new CancellationException();	// abort
					}
				} else if (param.equals("canopyTrimmingResiduesSpreading")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							
							int b = Integer.parseInt(st[k]);
							if (b < 0 || b > 2)  {
								System.out.println("Available value for canopyTrimmingResiduesSpreading parameter = [0,1,2]");	
								throw new CancellationException();	// abort
							}
							else 
								itk.canopyTrimmingResiduesSpreading.add(new Integer(Integer.parseInt(st[k])));

							nbDays++;
						}	
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing canopyTrimmingResiduesSpreading");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("canopyTrimmingResiduesSpreading");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : canopyTrimmingResiduesSpreading");
						throw new CancellationException();	// abort
					}					
					
// FRUIT THINNING
				} else if (param.equals("fruitThinningYears")) {
					try {
						String[] st = (r.value).split(",");
						nbYear = 0;
						for (int k = 0; k < st.length; k++) {
							itk.fruitThinningYears.add(new Integer(Integer.parseInt(st[k])));
							nbYear++;
						}
						
						requiredParameters.add("fruitThinningMethod");
						requiredParameters.add("fruitThinningDays");
						requiredParameters.add("fruitThinningFruitNbrTarget");	
						requiredParameters.add("fruitOptimalLoadLeafArea");	
						requiredParameters.add("fruitThinningDelayAfterSetting");
						requiredParameters.add("fruitThinningResiduesIncorporation");
						requiredParameters.add("fruitThinningResiduesSpreading");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : fruitThinningYears");
						throw new CancellationException();	// abort
					}
				} else if (param.equals("fruitThinningMethod")) {
					try {
						String[] st = (r.value).split(",");
						int nbDays = 0;
						for (int k = 0; k < st.length; k++) {
							
							int b = Integer.parseInt(st[k]);
							if (b > 3) {
								System.out.println("Available value for fruitThinningMethod parameter = [0,1,2,3]");	
								throw new CancellationException();	// abort
							}
							else 
								itk.fruitThinningMethod.add(new Integer(Integer.parseInt(st[k])));

							nbDays++;
						}
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : fruitThinningMethod");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("fruitThinningMethod");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : fruitThinningMethod");
						throw new CancellationException();	// abort
					}
				} else if (param.equals("fruitThinningDays")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.fruitThinningDays.add(getJulianDay (st[k]));
							nbDays++;
						}
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing fruitThinningDays");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("fruitThinningDays");
						
					} catch(Exception e){
						System.out.println("Wrong itk tree file format parameters : fruitThinningDays");
						throw new CancellationException();	// abort
					}
				} else if (param.equals("fruitThinningFruitNbrTarget")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.fruitThinningFruitNbrTarget.add(new Integer(Integer.parseInt(st[k])));
							nbDays++;
						}		
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing fruitThinningFruitNbrTarget");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("fruitThinningFruitNbrTarget");
						
						
					} catch(Exception e){
						System.out.println("Wrong itk tree file format parameters : fruitThinningFruitNbrTarget");
						throw new CancellationException();	// abort
					}
					
					
				} else if (param.equals("fruitOptimalLoadLeafArea")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.fruitOptimalLoadLeafArea.add(new Double(Integer.parseInt(st[k])));
							nbDays++;
						}		
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing fruitOptimalLoadLeafArea");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("fruitOptimalLoadLeafArea");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : fruitOptimalLoadLeafArea");
						throw new CancellationException();	// abort
					}
					
				} else if (param.equals("fruitThinningDelayAfterSetting")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.fruitThinningDelayAfterSetting.add(new Integer(Integer.parseInt(st[k])));
							nbDays++;
						}
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing fruitThinningDelayAfterSetting");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("fruitThinningDelayAfterSetting");
						
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : fruitThinningDelayAfterSetting");
						throw new CancellationException();	// abort
					}
					
				} else if (param.equals("fruitThinningResiduesIncorporation")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							nbDays++; 
							int b = Integer.parseInt(st[k]);
							if (b < 0 || b > 1) {
								System.out.println("Available value for fruitThinningResiduesIncorporation parameter = [0,1]");
								throw new CancellationException();	// abort
							}
							else 
								itk.fruitThinningResiduesIncorporation.add(new Integer(Integer.parseInt(st[k])));					
						}	
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing fruitThinningResiduesIncorporation");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("fruitThinningResiduesIncorporation");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : fruitThinningResiduesIncorporation");
						throw new CancellationException();	// abort
					}	
					
				} else if (param.equals("fruitThinningResiduesSpreading")) {
					try {
						int nbDays = 0;
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							nbDays++; 
							int b = Integer.parseInt(st[k]);
							if (b < 0 || b > 2) {
								System.out.println("Available value for fruitThinningResiduesSpreading parameter = [0,1,2]");
								throw new CancellationException();	// abort
							}
							else 
								itk.fruitThinningResiduesSpreading.add(new Integer(Integer.parseInt(st[k])));					
						}	
						if (nbDays != nbYear) {
							System.out.println("Wrong itk tree file format parameters : Missing fruitThinningResiduesSpreading");
							throw new CancellationException();	// abort	
						}
						
						requiredParameters.remove("fruitThinningResiduesSpreading");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : fruitThinningResiduesSpreading");
						throw new CancellationException();	// abort
					}							
// FRUIT HARVEST					
				} else if (param.equals("fruitHarvestDays")) {
					try {
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.fruitHarvestDays.add(getJulianDay (st[k]));
						}
					} catch(Exception e){
						System.out.println("Wrong itk tree file format parameters : fruitHarvestDays");
						throw new CancellationException();	// abort
					}

// IRRIGATION
				} else if (param.equals("treeIrrigationType")) {
					try {
						
						int b = r.getIntValue();
						if (b < 0 || b > 2) {
							System.out.println("Available value for treeIrrigationType parameter = [0=none ,1=manual, 2=auto]");	
							throw new CancellationException();	// abort
						}
						else 
							itk.treeIrrigationType = r.getIntValue();
						
						requiredParameters.add("treeIrrigationMethod");
						requiredParameters.add("treeIrrigationRadius");
						requiredParameters.add("treeIrrigationWaterStressTrigger");	
						requiredParameters.add("treeIrrigationAutomaticDose");	
						requiredParameters.add("treeIrrigationDriporSprinklerX");
						requiredParameters.add("treeIrrigationDriporSprinklerY");
						requiredParameters.add("treeIrrigationYears");
						requiredParameters.add("treeIrrigationDays");
						requiredParameters.add("treeIrrigationDose");
					
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeIrrigationType");
						throw new CancellationException();	// abort
					}

				} else if (param.equals("treeIrrigationMethod")) {
					
					int b = r.getIntValue();
					if (b < 1 || b > 3) {
						System.out.println("Available value for treeIrrigationMethod parameter = [1=drip, 2=aspersion, 3=flooding]");	
						throw new CancellationException();	// abort
					}
					else {
						itk.treeIrrigationMethod = r.getIntValue();
						requiredParameters.remove("treeIrrigationMethod");

					}
	
				} else if (param.equals("treeIrrigationRadius")) {
					try {
						itk.treeIrrigationRadius = new Double(Double.parseDouble(r.value));
						requiredParameters.remove("treeIrrigationRadius");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeIrrigationRadius");
						throw new CancellationException();	// abort
					}

				} else if (param.equals("treeIrrigationDriporSprinklerX")) {
					try {
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.treeIrrigationDriporSprinklerX.add(new Double(Double.parseDouble(st[k])));
						}
						
						requiredParameters.remove("treeIrrigationDriporSprinklerX");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeIrrigationDriporSprinklerX");
						throw new CancellationException();	// abort
					}
				} else if (param.equals("treeIrrigationDriporSprinklerY")) {
					try {
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.treeIrrigationDriporSprinklerY.add(new Double(Double.parseDouble(st[k])));
						}
						
						requiredParameters.remove("treeIrrigationDriporSprinklerY");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeIrrigationDriporSprinklerY");
						throw new CancellationException();	// abort
					}
					
				} else if (param.equals("treeIrrigationWaterStressTrigger")) {
					try {
						itk.treeIrrigationWaterStressTrigger = new Double(Double.parseDouble(r.value));
						requiredParameters.remove("treeIrrigationWaterStressTrigger");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeIrrigationWaterStressTrigger");
						throw new CancellationException();	// abort
					}

				} else if (param.equals("treeIrrigationAutomaticDose")) {
					try {
						itk.treeIrrigationAutomaticDose = new Double(Double.parseDouble(r.value));
						requiredParameters.remove("treeIrrigationAutomaticDose");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeIrrigationAutomaticDose");
						throw new CancellationException();	// abort
					}
	
				} else if (param.equals("treeIrrigationYears")) {
					try {
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.treeIrrigationYears.add(new Integer(Integer.parseInt(st[k])));
						}
						
						requiredParameters.remove("treeIrrigationYears");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeIrrigationYears");
						throw new CancellationException();	// abort
					}
					
				} else if (param.equals("treeIrrigationDays")) {
					try {
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.treeIrrigationDays.add(getJulianDay (st[k]));
						}
						requiredParameters.remove("treeIrrigationDays");
						
					} catch(Exception e){
						System.out.println("Wrong itk tree file format parameters : treeIrrigationDays");
						throw new CancellationException();	// abort
					}

				} else if (param.equals("treeIrrigationDose")) {
					try {
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.treeIrrigationDose.add(new Double(Double.parseDouble(st[k])));
						}
						
						requiredParameters.remove("treeIrrigationDose");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeIrrigationDose");
						throw new CancellationException();	// abort
					}
					
// FERTILISATION
				} else if (param.equals("treeFertilizationType")) {
					
					try {
						
						int b = r.getIntValue();
						if (b < 0 || b > 2) {
							System.out.println("Available value for treeFertilizationType parameter = [0,1,2]");	
							throw new CancellationException();	// abort
						}
						else 
							itk.treeFertilizationType = r.getIntValue();
						
						
						requiredParameters.add("treeFertilizationRadius");
						requiredParameters.add("treeFertilizationNitrogenStressTrigger");
						requiredParameters.add("treeFertilizationAutomaticDose");
						requiredParameters.add("treeFertilizerAutomaticCode");
						requiredParameters.add("treeFertilizationYears");	
						requiredParameters.add("treeFertilizationDays");	
						requiredParameters.add("treeFertilizerCode");
						requiredParameters.add("treeFertilizationDose");

						
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeFertilizationType");
						throw new CancellationException();	// abort
					}


					
				} else if (param.equals("treeFertilizationRadius")) {
					try {
						itk.treeFertilizationRadius = new Double(Double.parseDouble(r.value));
						requiredParameters.remove("treeFertilizationRadius");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeFertilizationRadius");
						throw new CancellationException();	// abort
					}
				} else if (param.equals("treeFertilizationNitrogenStressTrigger")) {
					try {
			
						itk.treeFertilizationNitrogenStressTrigger = new Double(Double.parseDouble(r.value));
						requiredParameters.remove("treeFertilizationNitrogenStressTrigger");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeFertilizationNitrogenStressTrigger");
						throw new CancellationException();	// abort
					}
				} else if (param.equals("treeFertilizationAutomaticDose")) {
					
					try {
						itk.treeFertilizationAutomaticDose = new Double(Double.parseDouble(r.value));
						requiredParameters.remove("treeFertilizationAutomaticDose");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeFertilizationAutomaticDose");
						throw new CancellationException();	// abort
					}
				} else if (param.equals("treeFertilizerAutomaticCode")) {
					try {
						itk.treeFertilizerAutomaticCode = r.getIntValue();
						requiredParameters.remove("treeFertilizerAutomaticCode");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeFertilizerAutomaticCode");
						throw new CancellationException();	// abort
					}
					
				} else if (param.equals("treeFertilizationYears")) {
					try {
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.treeFertilizationYears.add(new Integer(Integer.parseInt(st[k])));
						}
						requiredParameters.remove("treeFertilizationYears");
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeFertilizationYears");
						throw new CancellationException();	// abort
					}
					
				} else if (param.equals("treeFertilizationDays")) {
					try {
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.treeFertilizationDays.add(getJulianDay (st[k]));
						}
						requiredParameters.remove("treeFertilizationDays");
					} catch(Exception e){
						System.out.println("Wrong itk tree file format parameters : treeFertilizationDays");
						throw new CancellationException();	// abort
					}

				} else if (param.equals("treeFertilizerCode")) {
					try {
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.treeFertilizerCode.add(new Integer(Integer.parseInt(st[k])));
						}
						requiredParameters.remove("treeFertilizerCode");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeFertilizerCode");
						throw new CancellationException();	// abort
					}
					
				} else if (param.equals("treeFertilizationDose")) {
					try {
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							itk.treeFertilizationDose.add(new Double(Double.parseDouble(st[k])));
						}
						requiredParameters.remove("treeFertilizationDose");
						
					} catch(NumberFormatException e){
						System.out.println("Wrong itk tree file format parameters : treeFertilizationDose");
						throw new CancellationException();	// abort
					}
				}
				
		 	}
		}

		//check table index 
		if (itk.treePruningDays.size()!=itk.treePruningYears.size()) {
			System.out.println("Wrong itk tree file format parameters. Missing treePruningDays. Should be equal to treePruningYears size:"+itk.treePruningYears.size());
			throw new CancellationException();	// abort
		}
		if (itk.treePruningProp.size()!=itk.treePruningYears.size()) {
			System.out.println("Wrong itk tree file format parameters. Missing treePruningProp. Should be equal to treePruningYears size:"+itk.treePruningYears.size());
			throw new CancellationException();	// abort
		}
		if (itk.treePruningMaxHeight.size()!=itk.treePruningYears.size()) {
			System.out.println("Wrong itk tree file format parameters. Missing treePruningMaxHeight. Should be equal to treePruningYears size:"+itk.treePruningYears.size());
			throw new CancellationException();	// abort
		}	
		if (itk.treeRootPruningDays.size()!=itk.treeRootPruningYears.size()) {
			System.out.println("Wrong itk tree file format parameters. Missing treeRootPruningDays. Should be equal to treeRootPruningYears size:"+itk.treeRootPruningYears.size());
			throw new CancellationException();	// abort
		}	
		if (itk.treeRootPruningDistance.size()!=itk.treeRootPruningYears.size()) {
			System.out.println("Wrong itk tree file format parameters. Missing treeRootPruningDistance. Should be equal to treeRootPruningYears size:"+itk.treeRootPruningYears.size());
			throw new CancellationException();	// abort
		}	
		if (itk.treeRootPruningDepth.size()!=itk.treeRootPruningYears.size()) {
			System.out.println("Wrong itk tree file format parameters. Missing treeRootPruningDepth. Should be equal to treeRootPruningYears size:"+itk.treeRootPruningYears.size());
			throw new CancellationException();	// abort
		}	
		if (itk.treeTopingDays.size()!=itk.treeTopingYears.size()) {
			System.out.println("Wrong itk tree file format parameters. Missing treeToppingDays. Should be equal to treeToppingYears size:"+itk.treeTopingYears.size());
			throw new CancellationException();	// abort
		}
		if (itk.treeTopingHeight.size()!=itk.treeTopingYears.size()) {
			System.out.println("Wrong itk tree file format parameters. Missing treeToppingHeight. Should be equal to treeToppingYears size:"+itk.treeTopingYears.size());
			throw new CancellationException();	// abort
		}	
		if (itk.treeTopingResiduesIncorporation.size()!=itk.treeTopingYears.size()) {
			System.out.println("Wrong itk tree file format parameters. Missing treeToppingResidusIncorporation. Should be equal to treeToppingYears size:"+itk.treeTopingYears.size());
			throw new CancellationException();	// abort
		}
		//missing required parameters
		if (!requiredParameters.isEmpty()) {
			System.out.println("Wrong itk tree file format parameters : Missing " + AmapTools.toString(requiredParameters));
			throw new CancellationException();	// abort

		}
		
		
	}
}
