/*
 * This file is part of the lerfob-forestools library.
 *
 * Copyright (C) 2010-2012 Frederic Mothe and Mathieu Fortin 
 * for LERFOB INRA/AgroParisTech, 
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
package capsis.util.nutrientmodel;

import java.io.Serializable;
import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;

import capsis.util.biomassmodel.BiomassCompatibleTree;
import capsis.util.biomassmodel.BiomassPredictionModel;
import capsis.util.biomassmodel.BiomassPredictionModel.BiomassCompartment;
import capsis.util.nutrientmodel.NutrientConcentrationProviderObject.Nutrient;
import lerfob.fagacees.FagaceesSpeciesProvider.FgSpecies;
import repicea.math.Matrix;

/**
 * The NutrientConcentrationPredictionModel class predicts the nutrient concentration
 * in some tree compartments using models by Wernsdorfer et al. (20014). IMPORTANT: It is designed for
 * beech only.
 * <br>
 * <br>
 * The nutrients that are considered in this model are : > N: Nitrogen > S: Sulfur > P: Phosphorus > K: Potassium > Ca: Calcium > Mg: Magnesium > Mn: Manganese 
 * <br>
 * @see <a href=http://www.sciencedirect.com/science/article/pii/S037811271400423X#> 
 * Wernsdorfer, H., Jonard, M., Genet, A., Legout, A., Nys, C., Saint-Andre, L., and Ponette, Q. 2014.
 * Modelling of nutrient concentrations in roundwood based on diameter and tissue proportion: Evidence for
 * an additional site-age effect in the case of Fagus sylvatica. Forest Ecology and Management 330: 192-204.
 * </a> 
 * @author Mathieu Fortin - March 2013
 */
public class NutrientConcentrationPredictionModel implements Serializable {

	private static boolean enabled = false ;
	
	public static void setEnabled(boolean enabled) {NutrientConcentrationPredictionModel.enabled = enabled;}
	public static boolean isEnabled() {return NutrientConcentrationPredictionModel.enabled;}


	protected final static List<BiomassCompartment> BranchCompartments = new ArrayList<BiomassCompartment>();
	static {
		BranchCompartments.add(BiomassCompartment.BRANCHES_0TO4);
		BranchCompartments.add(BiomassCompartment.BRANCHES_4TO7);
		BranchCompartments.add(BiomassCompartment.BRANCHES_SUP7);
	}
	
	private Map<Nutrient, NutrientConcentrationSubversionModel> subversionMap;
	protected final BiomassPredictionModel bpm;

	/**
	 * General constructor with all source of uncertainty disabled.
	 */
	public NutrientConcentrationPredictionModel() {
		this(false, false);
	}
	
	/**
	 * Specific constructor.
	 * @param isParametersVariabilityEnabled true to enable the variability or false otherwise
	 * @param isResidualVariabilityEnabled true to enable the variability or false otherwise
	 */
	private NutrientConcentrationPredictionModel(boolean isParametersVariabilityEnabled, boolean isResidualVariabilityEnabled) {
		bpm = new BiomassPredictionModel();
		subversionMap = new EnumMap<Nutrient, NutrientConcentrationSubversionModel>(Nutrient.class);
		subversionMap.put(Nutrient.N, new NutrientSubversionN(this, isParametersVariabilityEnabled, isResidualVariabilityEnabled));
		subversionMap.put(Nutrient.P, new NutrientSubversionP(this, isParametersVariabilityEnabled, isResidualVariabilityEnabled));
		subversionMap.put(Nutrient.K, new NutrientSubversionK(this, isParametersVariabilityEnabled, isResidualVariabilityEnabled));
		subversionMap.put(Nutrient.S, new NutrientSubversionS(this, isParametersVariabilityEnabled, isResidualVariabilityEnabled));
		subversionMap.put(Nutrient.Ca, new NutrientSubversionCa(this, isParametersVariabilityEnabled, isResidualVariabilityEnabled));
		subversionMap.put(Nutrient.Mg, new NutrientSubversionMg(this, isParametersVariabilityEnabled, isResidualVariabilityEnabled));
		subversionMap.put(Nutrient.Mn, new NutrientSubversionMn(this, isParametersVariabilityEnabled, isResidualVariabilityEnabled));
	}

	/**
	 * This method provides a 3x1 Matrix that contains the nutrient concentration (g/kg) for wood, bark and wood plus the bark.
	 * @param nutrient a Nutrient enum instance
	 * @param compartment a BiomassCompartment enum instance
	 * @param tree a NutrientCompatibleTree instance
	 * @return a Matrix instance of g/kg concentration
	 */
	public Matrix getNutrientConcentrations(Nutrient nutrient, BiomassCompartment compartment, NutrientCompatibleTree tree) {
		if (enabled && tree.getFgSpecies() == FgSpecies.BEECH && BiomassCompartment.getEligibleCompartimentsForNutrient().contains(compartment)) {
			Matrix output = subversionMap.get(nutrient).predictNutrientConcentration(compartment, tree);
			return output;
		} else {
			return new Matrix(3,1);
		}
	}

	/**
	 * This method returns the concentration of the wood plus the bark in g/kg.
	 * @param nutrient a Nutrient enum instance
	 * @param compartment a BiomassCompartment enum instance
	 * @param tree a NutrientCompatibleTree instance
	 * @return the concentration (g/kg)
	 */
	public double getBarkPlusWoodNutrientConcentrations(Nutrient nutrient, BiomassCompartment compartment, NutrientCompatibleTree tree) {
		return getNutrientConcentrations(nutrient, compartment, tree).getValueAt(2, 0);
	}

	/**
	 * This method provides a 3x1 Matrix that contains the nutrient concentration (g/kg) for wood, bark and wood plus the bark.
	 * @param nutrient a Nutrient enum instance
	 * @param tree a NutrientCompatibleTree instance
	 * @param largeEndCm the large-end diameter (cm)
	 * @param smallEndCm the small-end diameter (cm)
	 * @return a Matrix instance of g/kg concentration
	 */
	protected Matrix getNutrientConcentrationsForThisSection(Nutrient nutrient, NutrientCompatibleTree tree, double largeEndCm, double smallEndCm) {
		if (enabled && tree.getFgSpecies() == FgSpecies.BEECH) {
			Matrix output = subversionMap.get(nutrient).predictNutrientConcentrationBetweenTheseBounds(tree, largeEndCm, smallEndCm);
			return output;
		} else {
			return new Matrix(3,1);
		}
	}

	
	/**
	 * 
 	 * This method returns the concentration of the wood plus the bark in g/kg for a particular section.
	 * @param nutrient a Nutrient enum instance
	 * @param tree a NutrientCompatibleTree instance
	 * @param largeEnd the large-end diameter (cm)
	 * @param smallEnd the small-end diameter (cm)
	 * @return the concentration (g/kg)
	 */
	public double getBarkPlusWoodNutrientConcentrationsForThisSection(Nutrient nutrient, NutrientCompatibleTree tree, double largeEnd, double smallEnd) {
		return getNutrientConcentrationsForThisSection(nutrient, tree, largeEnd, smallEnd).getValueAt(2, 0);
	}


	/**
	 * This method returns the content in a compartment for a particular nutrient.
	 * @param nutrient a Nutrient enum instance
	 * @param compartment a BiomassCompartment enum instance
	 * @param tree a NutrientCompatibleTree instance
	 * @return the content (g)
	 */
	public double getBarkPlusWoodNutrientContentG(Nutrient nutrient, BiomassCompartment compartment, NutrientCompatibleTree tree) {
		double biomassKg = bpm.getDryBiomassKg(compartment, tree);
		double concentrationGKg = getBarkPlusWoodNutrientConcentrations(nutrient, compartment, tree);
		return biomassKg * concentrationGKg;
	}
	
	
	/**
	 * This method returns the wood ratio in branches with diameter < 7 cm. It was referred to 
	 * as part of Astrid Genet's thesis (2010). However, no reference was found in this work. The original
	 * function as implemented provided the bark ratio. The results were inconsistent though with bark ratio always
	 * larger than 0.8. 
	 * @param ageYr the age of the tree (yr)
	 * @param medianDiameterCm the median diameter (cm)
	 */
	private double getBranchWoodDryBiomassRatio(double ageYr, double medianDiameterCm) {
		double woodRatio = (0.97685 - 0.000292 * ageYr) - 0.142135 * Math.exp(-(0.141074 - 0.000298 * ageYr) * medianDiameterCm);
//		double barkRatio = 1. - woodRatio;
		return Math.min(Math.max(woodRatio, 0.), 1.);
	}

	/**
	 * This method returns the wood ratio of the stem with diameter larger than 7 cm based on the biomass models.
	 * @param ageYr the age of the tree (yr)
	 * @param dbhCm the diameter at breast height (cm)
	 * @param heightM the tree height (m)
	 * @return the ratio wood over wood + bark
	 */
	private double getStemWoodDryBiomassRatio(int ageYr, double dbhCm, double heightM) {
		boolean isOak = false;
		double woodBiomass = bpm.getBiomass_kg(BiomassCompartment.STEM_WOOD_SUP7, ageYr, dbhCm, heightM, isOak);
		double barkBiomass = bpm.getBiomass_kg(BiomassCompartment.STEM_BARK_SUP7, ageYr, dbhCm, heightM, isOak);
		double sum = woodBiomass + barkBiomass;
		return sum == 0. ? 0. : woodBiomass / sum;
	}

	/**
	 * This method returns the wood ratio either for the branch or for the bole.
	 * @param age the age of the tree (yr)
	 * @param medianDiam_cm the median diameter of the section (cm)
	 * @param dbh_cm the diameter at breast height (cm)
	 * @param h_m the tree height (m)
	 * @param isBranch a boolean true if the compartment is branch
	 * @param isOak a boolean
	 */
	private double getWoodDryBiomassRatio(int age, double medianDiam_cm, double dbh_cm, double h_m, boolean isBranch, boolean isOak) {
		double ratio;
		if (isOak) {
			return -1.;
		} else {
			if (isBranch) {
				ratio = getBranchWoodDryBiomassRatio(age, medianDiam_cm);
			} else {
				ratio = getStemWoodDryBiomassRatio(age, dbh_cm, h_m);
			}
		}
		return ratio;
	}

	/**
	 * This method returns the ratio of wood dry biomass : total biomass (bark + wood) of a particular
	 * compartment in a tree. IMPORTANT: Only the compartments BRANCHES_0TO4, BRANCHES_4TO7, BRANCHES_SUP7, STEM, and STEM_SUP7 yield results.
	 * Other compartments return -1
	 * @param tree a BiomassCompatibleTree tree
	 * @param compartment a BiomassCompartment enum instance
	 * @return a double
	 */
	public double getWoodDryBiomassRatio(NutrientCompatibleTree tree, BiomassCompartment compartment) {
		double ratio;
		if (tree.getFgSpecies() != FgSpecies.BEECH || !BiomassCompartment.getEligibleCompartimentsForNutrient().contains(compartment)) {
			return -1.;
		} else {
			if (BranchCompartments.contains(compartment)) {
				double midDiameter = getMedianDiameterCm(tree, compartment);
				ratio = getBranchWoodDryBiomassRatio(tree.getAgeYr(), midDiameter);
			} else {
				ratio = getStemWoodDryBiomassRatio(tree.getAgeYr(), tree.getDbhCm(), tree.getHeightM());
			}
		}
		return ratio;
	}
	
	/**
	 * This method returns the mid diameter of a particular compartment.
	 * @param tree a NutrientCompatibleTree instance
	 * @param compartment a BiomassCompartment instance
	 * @return the mid diameter (cm)
	 */
	public double getMedianDiameterCm(NutrientCompatibleTree tree, BiomassCompartment compartment) {
		double midDiameter;
		switch(compartment) {
		case STEM:
			midDiameter = tree.getDbhCm() * .5;
			break;
		case STEM_SUP7:
			midDiameter = (tree.getDbhCm() + 7d) * .5;
			break;
		case BRANCHES_SUP7:
			double largeEndDiameterCm = tree.getCrossSectionDiameterCm (tree.getCrownBaseHeightM(), true);
			midDiameter = (7d + largeEndDiameterCm) * .5;
			break;
		case BRANCHES_4TO7:
			midDiameter = 5.5;
			break;
		case BRANCHES_0TO4:
			midDiameter = 2.5;
			break;
		default:
			throw new InvalidParameterException("Only comparments STEM, STEM_SUP7, BRANCHES_0TO4, BRANCHES_SUP7 and BRANCHES_4TO7 are allowed!");
		}
		return midDiameter;
	}
	
	
	
	/**
	 * Bark ratio in dry mass (return -1 for oak)
	 */
	protected double getWoodDryBiomassRatio(BiomassCompatibleTree t, double medianDiam_cm, boolean isBranch) {
		return getWoodDryBiomassRatio(t.getAgeYr(), medianDiam_cm, t.getDbhCm(), t.getHeightM(), isBranch, t.getFgSpecies() == FgSpecies.OAK);
	}

}
