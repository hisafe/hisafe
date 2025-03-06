/*
 * This file is part of the lerfob-forestools library.
 *
 * Copyright (C) 2010-2012 Frederic Mothe for LERFOB INRA/AgroParisTech, 
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed with the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * Please see the license at http://www.gnu.org/copyleft/lesser.html.
 */
package capsis.util.biomassmodel;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Set;

import lerfob.fagacees.FagaceesSpeciesProvider.FgSpecies;

/**
 * The BiomassPredictionModel class implements the biomass models 
 * developed by Genet et al. 2011.
 * @author Frederic Mothe - February 2010
 * @author Mathieu Fortin - January 2013 (refactoring)
 * @see <a href=http://ac.els-cdn.com/S0378112710007486/1-s2.0-S0378112710007486-main.pdf?_tid=8ebdd20a-702d-11e2-ab58-00000aacb361&acdnat=1360135354_d26689444171d6e0e9ab230e86ddd135>
 * Genet et al. 2011. Ontogeny partly explains the apparent heterogeneity of 
 * published biomass equations for Fagus sylvatica in central Europe. For. Ecol. Manage.
 * 261: 1188-1202.
 * </a> 
 */
public class BiomassPredictionModel {

	
	/*
	biomass_kg = p0 + (p1 + p2 * age + p3 * exp (p4 * age)) * d2h ^ (p5 + p6 * age)
	d2h = d_m * d_m * h_m

	name	p0	p1	p2	p3	p4	p5	p6
	LivingBranches	0	29.2	0	108.4	-0.0179	0.974	0.00377
	LivingBranches0-4	0	23.2	0	115.2	-0.04	0.979	0
	LivingBranches4-7	0	23.5	-0.0747	0	0	1.0821	0
	LivingBranches>7	0	11.9	0	0	0	1.770669	0
	Leaves	0	2.6	0	27.3	-0.0644	0.887	-0.000607
	StemBark>7	0	11.6	0.0757	0	0	0.877	0
	StemWood>7	0	193	0	-96.5	-0.0419	0.973	0
	Bole	0	234.6	0	-79.4	-0.022	0.908	0
	BelowGround	0.00949	69.2	0	0	0	0.951	0
	CoarseRoots	0.0753	39.3	0	0	0	1.057	0
	MediumRoots	0.00998	20.1	0	0	0	0.823	0
	SmallRoots	0	10.9	0	0	0	0.673	0
	Aboveground_TMP	0	631.9	-0.195	0	0	1	0
	Aboveground = Aboveground_TMP * f(c130,h)
	 */

	
	/**
	 * Compartments for the biomass models
	 */
	public static enum BiomassCompartment {
		ABOVE_GROUND(false), 
		BRANCHES(false), 
		BRANCHES_0TO4(true), 
		BRANCHES_4TO7(true), 
		BRANCHES_SUP7(true), 
		STEM(true), 
		STEM_SUP7(true), 
		STEM_WOOD_SUP7(false), 
		STEM_BARK_SUP7(false), 
		LEAVES(false), 
		ROOTS(false), 
		ROOTS_0TO1(false), 
		ROOTS_1TO4(false), 
		ROOTS_SUP4(false);
		
		private final boolean eligibleForNutrient;
		
		private static List<BiomassCompartment> EligibleCompartimentsForNutrient;
		
		BiomassCompartment(boolean eligibleForNutrient) {
			this.eligibleForNutrient = eligibleForNutrient;
		}
		
		public static List<BiomassCompartment> getEligibleCompartimentsForNutrient() {
			if (EligibleCompartimentsForNutrient == null) {
				EligibleCompartimentsForNutrient = new ArrayList<BiomassCompartment>();
				for (BiomassCompartment compartment : BiomassCompartment.values()) {
					if (compartment.eligibleForNutrient) {
						EligibleCompartimentsForNutrient.add(compartment);
					}
				}
			}
			return EligibleCompartimentsForNutrient;
		}
		
	}

	/**
	 * Abstract base class for the biomass models
	 */
	private static interface BiomassModel {
		public abstract double getBiomass_kg(int age, double dbh_cm, double h_m);
	}

	/**
	 * Internal class Astrid Genet's model. Corresponds to eq. 11 in the article.
	 */
	private static class BiomassModel_Astrid implements BiomassModel {
		private double p0, p1, p2, p3, p4, p5, p6;

		private BiomassModel_Astrid(double p0, double p1, double p2, double p3, double p4, double p5, double p6) {
			this.p0 = p0;
			this.p1 = p1;
			this.p2 = p2;
			this.p3 = p3;
			this.p4 = p4;
			this.p5 = p5;
			this.p6 = p6;
		}

		@Override
		public double getBiomass_kg(int age, double dbh_cm, double h_m) {
			double d2h_m3 = dbh_cm * dbh_cm * h_m / 1e4;
			return p0 + (p1 + p2 * age + p3 * Math.exp(p4 * age)) * Math.pow(d2h_m3, p5 + p6 * age);
		}
	}

	/**
	 * Biomass model returning the sum of two biomass models
	 */
	private static class BiomassModel_Sum implements BiomassModel {
		private BiomassModel model1, model2;

		private BiomassModel_Sum(BiomassModel model1, BiomassModel model2) {
			this.model1 = model1;
			this.model2 = model2;
		}

		@Override
		public double getBiomass_kg(int age, double dbh_cm, double h_m) {
			return model1.getBiomass_kg(age, dbh_cm, h_m) + model2.getBiomass_kg(age, dbh_cm, h_m);
		}
	}

	/**
	 * Specialised class for the ABOVE_GROUND model for beech. Corresponds to eq. 12 in the 
	 * article.
	 */
	private static class AboveGroundBiomassModel_Beech extends BiomassModel_Astrid {
		
		private AboveGroundBiomassModel_Beech(double p0, double p1, double p2, double p3, double p4, double p5, double p6) {
			super(p0, p1, p2, p3, p4, p5, p6);
		}

		@Override
		public double getBiomass_kg(int age, double dbh_cm, double h_m) {
			double predDM = super.getBiomass_kg(age, dbh_cm, h_m);
			double c130_cm = dbh_cm * Math.PI;
			double factorF_c130_h = (0.395 + 0.000266 * c130_cm + 0.421 * Math.sqrt(c130_cm) / h_m) * (1. + 4.5 / (c130_cm * c130_cm)) * Math.PI / 4.;
			return factorF_c130_h * predDM;
		}
		
	}

	// ==============================================
	// Static variables :
	// ==============================================

	private EnumMap<BiomassCompartment, BiomassModel> biomassModelEMap_Beech;
	
	public BiomassPredictionModel() {
		initBiomassModelEMap_Beech();
	}
	
	// ==============================================
	// Initialisation of the static variables :
	// ==============================================

	/* setup valus for p0-p6, used for biomass computation */

	private void initBiomassModelEMap_Beech() {
		biomassModelEMap_Beech = new EnumMap<BiomassCompartment, BiomassModel>(BiomassCompartment.class);
		BiomassModel stemWoodSup7 = new BiomassModel_Astrid(0., 193.4, 0, -112.4, -0.0482, 0.976, 0);
		BiomassModel stemBarkSup7 = new BiomassModel_Astrid(0, 11.6, 0.075, 0, 0, 0.877, 0);
		biomassModelEMap_Beech.put(BiomassCompartment.ABOVE_GROUND, new AboveGroundBiomassModel_Beech(0, 610.0, -0.222, 0, 0, 1., 0));
		biomassModelEMap_Beech.put(BiomassCompartment.BRANCHES, new BiomassModel_Astrid(0.093, 42.3, 0, 188.7, -0.0372, 1.3, 0.));
		biomassModelEMap_Beech.put(BiomassCompartment.BRANCHES_0TO4, new BiomassModel_Astrid(0, 22.8, 0, 114.0, -0.0381, 0.979, 0));
		biomassModelEMap_Beech.put(BiomassCompartment.BRANCHES_4TO7, new BiomassModel_Astrid(0, 23.7, -0.085, 0, 0, 1.1, 0));
		biomassModelEMap_Beech.put(BiomassCompartment.BRANCHES_SUP7, new BiomassModel_Astrid(0, 12.7, 0, 0, 0, 1.61, 0));
		biomassModelEMap_Beech.put(BiomassCompartment.STEM, new BiomassModel_Astrid(0, 233.4, 0, 112.4, -0.015, 0.93, 0));
		biomassModelEMap_Beech.put(BiomassCompartment.STEM_WOOD_SUP7, stemWoodSup7);
		biomassModelEMap_Beech.put(BiomassCompartment.STEM_BARK_SUP7, stemBarkSup7);
		biomassModelEMap_Beech.put(BiomassCompartment.STEM_SUP7, new BiomassModel_Sum(stemWoodSup7, stemBarkSup7));
		biomassModelEMap_Beech.put(BiomassCompartment.LEAVES, new BiomassModel_Astrid(0, 2.8, 0, 30.4, -0.0705, 0.93, -0.00235));
		biomassModelEMap_Beech.put(BiomassCompartment.ROOTS, new BiomassModel_Astrid(0.00790, 63.0, 0, 0, 0, 0.931, 0));
		biomassModelEMap_Beech.put(BiomassCompartment.ROOTS_0TO1, new BiomassModel_Astrid(0, 16.9, 0, 0, 0, 0.763, 0));
		biomassModelEMap_Beech.put(BiomassCompartment.ROOTS_1TO4, new BiomassModel_Astrid(0, 10.0, 0, 0, 0, 0.656, 0));
		biomassModelEMap_Beech.put(BiomassCompartment.ROOTS_SUP4, new BiomassModel_Astrid(0.080, 50.4, 0, 0, 0, 1.13, 0));
	}


	/**
	 * This method returns the set of BiomassCompartments
	 * @return a Set of BiomassCompartment enum variables
	 */
	public Set<BiomassCompartment> getCompartments() {
		// Should be the same for oak and beech :
		return biomassModelEMap_Beech.keySet();
	}

	/**
	 * This method returns the BiomassModel for the given element and species.
	 * @return a BiomassModel instance or null if isOak is true
	 */
	public BiomassModel getBiomassModel(BiomassCompartment compartment, boolean isOak) {
		return isOak ? null : biomassModelEMap_Beech.get(compartment);
	}

	/**
	 * This method returns the number of compartments.
	 * @return an Integer
	 */
	public final int getNbCompartments() {
		return BiomassCompartment.values().length;
	}


	/**
	 * This method returns the dry biomass (kg) for a given compartment in a particular tree.
	 * @param compartment a BiomassCompartment enum variable
	 * @param age the age of the tree (yr)
	 * @param dbh_cm the diameter at breast height (cm)
	 * @param h_m the tree height (m)
	 * @param isOak a boolean
	 * @return the dry biomass (kg)
	 */
	public double getBiomass_kg(BiomassCompartment compartment, int age, double dbh_cm, double h_m, boolean isOak) {
		BiomassModel model = getBiomassModel(compartment, isOak);
		return model == null ? -1. : model.getBiomass_kg(age, dbh_cm, h_m);
	}

	/**
	 * This method returns the dry biomass (kg) for a given compartment in a particular tree.
	 * @param compartment a String that defines the BiomassCompartment enum variable
	 * @param age the age of the tree (yr)
	 * @param dbh_cm the diameter at breast height (cm)
	 * @param h_m the tree height (m)
	 * @param isOak a boolean
	 * @return the dry biomass (kg)
	 */
	public double getBiomass_kg(String compartment, int age, double dbh_cm, double h_m, boolean isOak) {
		return getBiomass_kg(BiomassCompartment.valueOf(compartment), age, dbh_cm, h_m, isOak);
	}

	/**
	 * This method returns the dry biomass (kg) for a given compartment in a particular tree.
	 * @param compartment a String that defines the BiomassCompartment enum variable
	 * @param t a BiomassCompatibleTree instance
	 * @return the dry biomass (kg)
	 */
	public double getBiomass_kg(String compartment, BiomassCompatibleTree t) {
		return getBiomass_kg(compartment, t.getAgeYr(), t.getDbhCm(), t.getHeightM(), t.getFgSpecies() == FgSpecies.OAK);
	}

	// TODO clean these methods
	/**
	 * This method returns the dry biomass (kg) for a given compartment in a particular tree.
	 * @param compartment a BiomassCompartment enum variable
	 * @param tree a BiomassCompatibleTree instance
	 * @return the dry biomass (kg)
	 */
	public double getDryBiomassKg(BiomassCompartment compartment, BiomassCompatibleTree tree) {
		return getBiomass_kg(compartment, tree.getAgeYr(), tree.getDbhCm(), tree.getHeightM(), tree.getFgSpecies() == FgSpecies.OAK);
	}

	
	
	/**
	 * This method returns the dry biomass (kg) for all compartments in a particular tree.
	 * @param age the age of the tree (yr)
	 * @param dbh_cm the diameter at breast height (cm)
	 * @param h_m the tree height (m)
	 * @param isOak a boolean
	 * @return an array of double with the dry biomasses (kg)
	 */
	public double[] getBiomass_byCompartments(int age, double dbh_cm, double h_m, boolean isOak) {
		double[] results = new double[getNbCompartments()];
		int n = 0;
		for (BiomassCompartment compartment : getCompartments()) {
			results[n++] = getBiomass_kg(compartment, age, dbh_cm, h_m, isOak);
		}
		return results;
	}

	/**
	 * Return an array of length getNbCompartments () with the compartment names
	 */
	public final String[] getCompartmentNames() {
		String[] names = new String[getNbCompartments()];
		int n = 0;
		for (BiomassCompartment c : getCompartments()) {
			names[n++] = c.toString();
		}
		return names;
	}




	/**
	 * For test purpose
	 */
	public static void main(String[] args) {
		final int nbArgsRequired = 5;
		if (args.length == 1 && (args[0].equals("--titre") || args[0].equals("--title"))) {
			System.out.println("treeName, compartment, age, dbh_cm, h_m, species, biomass_kg");
			System.exit (0);
		} else if (args.length != nbArgsRequired) {
			System.out.println("Syntax:\n java -cp class lerfob.fagacees.model.FgBiomass "
				+ "tree_name age dbh_cm h_m species"
				+ "\n or java -cp class lerfob.fagacees.model.FgBiomass --title"
			);
			System.exit (0);
		}
		// System.out.println ("NbCompartments = "  + getNbCompartments ());
		// System.out.println ("NbElements = "  + getNbElements ());

		BiomassPredictionModel bpm = new BiomassPredictionModel();
		int n = 0;
		String treeName = args[n++];
		int age = new Integer(args[n++]);
		double dbh_cm = new Double(args[n++]);
		double h_m = new Double(args[n++]);
		String species = args[n++];
		boolean isOak = species.equals("Oak");
		for (BiomassCompartment c : bpm.getCompartments()) {
			double biomass = bpm.getBiomass_kg(c, age, dbh_cm, h_m, isOak);
			System.out.println(treeName + ", " + c + ", " + age + ", " + dbh_cm + ", " + h_m + ", " + species + ", " + biomass);
		}
	}

}
