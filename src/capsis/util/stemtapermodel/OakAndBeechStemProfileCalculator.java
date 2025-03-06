/*
 * This file is part of the lerfob-forestools library.
 *
 * Copyright (C) 2010-2014 Mathieu Fortin for LERFOB INRA/AgroParisTech, 
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
package capsis.util.stemtapermodel;

import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.List;

import capsis.util.stemtapermodel.OakAndBeechStemTaperTree.Species;
import repicea.math.Matrix;
import repicea.math.SymmetricMatrix;
import repicea.simulation.stemtaper.AbstractStemTaperEstimate;
import repicea.simulation.stemtaper.AbstractStemTaperPredictor;
import repicea.stats.estimates.ConfidenceInterval;
import repicea.stats.estimates.Estimate;

@SuppressWarnings("serial")
public class OakAndBeechStemProfileCalculator extends AbstractStemTaperPredictor {

	public static class OakAndBeechStemTaperEstimate extends AbstractStemTaperEstimate {

		public OakAndBeechStemTaperEstimate(List<Double> computedHeights) {
			super(computedHeights);
		}

		@Override
		protected Matrix getSquaredDiameters(Matrix predictedDiameters) {
			return predictedDiameters.elementWiseMultiply(predictedDiameters);
		}

		@Override
		protected SymmetricMatrix getVarianceOfSquaredDiameter(SymmetricMatrix variancePredictedDiameters) {
			return null;			// useless since this class does not provide any variance for the predictions
		}

		@Override
		protected double getScalingFactor() {
			return Math.PI * .000025; 
		}

		@Override
		public ConfidenceInterval getConfidenceIntervalBounds(double oneMinusAlpha) {
			return null;
		}
		
	}
	
	/**
	 * Constructor
	 */
	public OakAndBeechStemProfileCalculator() {
		super(false, false, false);
	}

	protected final static double BREAST_HEIGHT_m = 1.30;
	
	
 	private double getOakOverBarkRadius_cm(OakAndBeechStemTaperTree tree, double h) {
		double radius = -1d;
		//	Stem profile according to Dh�te 1997
		//	Ref :	J-F. Dh�te, E. Hatsch, D. Ritti� 1997
		//		Profil de la tige et g�om�trie de l'aubier chez le ch�ne sessile
		//		(Quercus petraea Liebl)
		//		Bulletin Technique de l'ONF n�33, ao�t 1997, p59-81
		//
		//	Above crown base height :
		//		diam = mu2 * (mu1 - h)
		//	Below crown base height :
		//		diam = (mu4 * (mu3 - h) + mu2 * mu5 * (mu1-mu3)) *
		//			(1 + mu6 * exp (-h * log (100 * mu6) / mu7));
		//		or :
		//		diam = (mu4 * (mu3 - h) + dRed) * (1 + mu6 * exp (-h * sFac));
		//		with : 	dRed = mu2 * mu5 * (mu1-mu3)
		//				sFac = log (100 * mu6) / mu7;
		//	mu1 = total height (m)
		//	mu3 = crown base height (m)
		//	mu2 = diameter decreasing with length above cbh (cm/m)
		//	mu4 = diameter decreasing with length below cbh (cm/m)
		//	mu5 = diameter change at cbh (no unit)
		//	mu6 = stump effect (no unit)
		//	mu7 = length of the stump effect (m) :
		//	mu4 to mu 6 are estimated through the shape factor
		//		rho = d130 / (ht-h130), see equations below
		//	mu2 is computed using d130 and the stem profile at h130
		//		(supposed to be below cbh) :
		//		d130 = (mu4 * (mu3 - h130) + dRed) * (1 + mu6 * exp (-h130 * sFac));
		//		=>	dRed = d130 / (1 + mu6 * exp (-h130 * sFac)) - mu4 * (mu3 - h130);
		//		mu2 = dRed / (mu5 * (mu1-mu3))


		double diam = 0.0;
		double ht = tree.getHeightM();
		final double h130 = BREAST_HEIGHT_m;
		if (ht > h130) {
			double d130 = tree.getDbhCm();
			double rho = d130 / (ht-h130);	

			double mu1 = ht;	//Dh�te et al. 2000 eq. 9
			double mu3 = Math.min (tree.getCrownBaseHeightM(), 0.95 * ht);	// crown base height (m)
			double mu4 = 1 / (0.56822 + 1.1758 / rho);	//Dh�te et al. 2000 eq. 13
			double mu5 = 1.0;
			double mu6 = 0.11651 + 0.19097 * rho;	//Dh�te et al. 2000 eq. 11
			double mu7 = 0.28439 + 1.2284 * rho;	//Dh�te et al. 2000 eq. 12

			double sFac = Math.log (100 * mu6) / mu7;
			double dRed = d130 / (1 + mu6 * Math.exp (-h130 * sFac))
			- mu4 * (mu3 - h130);

			if (h < 0) {
				// Below ground
				// diam = 0.0;
			} else if (h <= mu3) {
				// Below crown base height
				diam = (mu4 * (mu3-h) + dRed) * (1 + mu6 * Math.exp (-h * sFac));
			} else if (h < mu1) {
				// Above crown base height
				double mu2 = dRed / (mu5 * (mu1 - mu3));
				diam = mu2 * (mu1 - h);
			}
		}

		radius = diam/2;
		return radius;
	}		

 	
	//	Returns the over bark radius in cm at height h
	//	(from stem profiles of P.Vallet 2005, p88)
	private double getBeechOverBarkRadius_cm(OakAndBeechStemTaperTree tree, double h) {
		double radius = -1d;
		double ht = tree.getHeightM();
		if (ht > BREAST_HEIGHT_m) {
			double circ130 = tree.getDbhCm() * Math.PI;
			double hbh = tree.getCrownBaseHeightM();

			//	Stem circumference at crown base height from model of P.Vallet 2005, p87 modified for young trees)
			// Modified 27/05/2009 :
			double cbhModel = -14.3 + 0.70 * circ130; 				// Circumference predicted from the circumference at 1.30m (P.Vallet 2005, p87)
			// Circumference assuming a conic shape (for young trees) :
			double cbhConic = circ130 * (ht - hbh) / (ht - BREAST_HEIGHT_m);
			// Note : cbhConic > cbhModel when hbh is near 1.3 m
			// i.e. (ht - hbh) / (ht - 1.3) > 0.70 - 14.3 / c130

			double circbh = Math.max (cbhModel, cbhConic);

			// parameters of the Trincado & Gadow 1996 taper model

			final double h130 = BREAST_HEIGHT_m;
			// Modified 27/05/2009 :
			if (hbh <= h130) {
				// conic shape :
				double circ = circ130 * (ht - h) / (ht - BREAST_HEIGHT_m);
				radius = circ / (2. * Math.PI);
			} else {

				if (h < hbh) {
					double ii = 0.128740 * (circ130 - circbh);
					final double p = 0.692141;
					final double q = 0.085243;

					//~ radius = circbh/(2*Math.PI)+ii;
					//~ radius += ((circ130-circbh)/(2*Math.PI)-ii) * ( Math.exp(p*(h130-z)) - Math.exp(p*(h130-hbh))) / (1 - Math.exp(p*(h130-hbh)));
					//~ radius += - ii * ( Math.exp(q*(z-hbh)) - Math.exp(q*(h130-hbh))) / (1 - Math.exp(q*(h130-hbh)));

					double r1= circbh / (2. * Math.PI) + ii;

					double m2 = (circ130 - circbh) / (2. * Math.PI) - ii;
					double n2 = Math.exp (p * (h130 - h));
					double p2 = Math.exp (p * (h130 - hbh));
					final double maxp2 = 0.999;
					p2 = Math.min (p2, maxp2);
					double r2 = m2 * (n2 - p2) / (1. - p2);

					double m3 = - ii;
					double n3 = Math.exp (q * (h - hbh));
					double p3 = Math.exp (q * (h130 - hbh));
					final double maxp3 = 0.999;
					p3 = Math.min (p3, maxp3);
					double r3 = m3 * (n3 - p3) / (1. - p3);
					radius = r1 + r2 + r3;

					//~ double r = r1 + r2 + r3;
					//~ radius = circbh/(2*Math.PI)+ii;
					//~ radius += ((circ130-circbh)/(2*Math.PI)-ii) * ( Math.exp(p*(h130-z)) - Math.exp(p*(h130-hbh))) / (1 - Math.exp(p*(h130-hbh)));
					//~ radius += - ii * ( Math.exp(q*(z-hbh)) - Math.exp(q*(h130-hbh))) / (1 - Math.exp(q*(h130-hbh)));

					//~ double err = Math.abs (radius - r);
					//~ if (err > 0.01) {
					//~ System.out.println ("pb err=" + err);
					//~ System.exit (-1);
					//~ }
					//~ radius += Math.max (r3, 0.);

				} else {
					radius = circbh / (2. * Math.PI) * (1. - (h - hbh) / (ht - hbh));
				}
			}
			return Math.max(radius, 0.0);
		}
		return radius;
	}		

	// Unit : same as overBarkRadius
	public static double getUnderBarkRadius (double overBarkRadius,
			double surfaceBarkRatio) {
		// surfaceBarkRatio = (overBarkSurf - underBarkSurf) / overBarkSurf
		//	= 1. - underBarkRadius^2  / overBarkRadius ^2
		// underBarkRadius^2 = overBarkRadius ^2 *  (1-surfaceBarkRatio)
		return overBarkRadius * Math.sqrt (1. - surfaceBarkRatio);
	}

 	public static double getBarkWidth_cm (double radiusOverBark, double surfaceBarkRatio) {
		return radiusOverBark - getUnderBarkRadius (radiusOverBark, surfaceBarkRatio);
	}


	@Override
	public AbstractStemTaperEstimate getPredictedTaperForTheseHeights(BasicStemTaperTree tree, List<Double> heightMeasures, Object... params) {
		if (!(tree instanceof OakAndBeechStemTaperTree)) {
			throw new InvalidParameterException("The OakAndBeechStemProfileCalculator is designed to work with OakAndBeechStemTaperTree instances only!");
		}
		OakAndBeechStemTaperTree t = (OakAndBeechStemTaperTree) tree;
		Species species = t.getSpecies();
		List<Double> outcomes = new ArrayList<Double>();
		switch(species) {
		case Oak:
			for (Double h : heightMeasures) {
				outcomes.add(getOakOverBarkRadius_cm(t, h) * 2);
			}
			break;
		case Beech:
			for (Double h : heightMeasures) {
				outcomes.add(getBeechOverBarkRadius_cm(t, h) * 2);
			}
			break;
		}
		AbstractStemTaperEstimate estimate = new OakAndBeechStemTaperEstimate(heightMeasures);
		estimate.setMean(new Matrix(outcomes));
		return estimate;
	}

	/**
	 * This method returns the estimated heights at which the diameter is the parameter diameterCm.
	 * @param tree the tree instance
	 * @param initialHeightM the stump height for example
	 * @param smallEndCm the targeted diameter (cm)
	 * @return a List of Double whose last element is the height at which the small end diameter is obtained (m)
	 */
	protected List<Double> getHeightsArrayWithSmallEndDiameter(OakAndBeechStemTaperTree tree, double initialHeightM, double smallEndCm) {
		double height = tree.getHeightM();
		double heightDiff = height - initialHeightM;
		List<Double> heightMeasures = new ArrayList<Double>();
		double factor = 1d/100;
		for (int i = 0; i <= 100; i++) {
			heightMeasures.add(initialHeightM + heightDiff * i * factor);
		}
		AbstractStemTaperEstimate taperEstimate = getPredictedTaperForTheseHeights(tree, heightMeasures);
		double estimatedHeight = -1;
		double diameter0;
		double diameter1;
		double height0;
		double height1;
		double slope;
		int i = 0;
		for (i = 0; i < 100; i++) {
			diameter0 = taperEstimate.getMean().getValueAt(i, 0);
			diameter1 = taperEstimate.getMean().getValueAt(i + 1, 0);
			if (diameter0 >= smallEndCm && diameter1 < smallEndCm) {
				height0 = heightMeasures.get(i);
				height1 = heightMeasures.get(i+1);
				slope = (height1 - height0) / (diameter1 - diameter0);
				estimatedHeight = height0 + (smallEndCm - diameter0) * slope;
				break;
			}
		}
		if (estimatedHeight == -1d) { // means the tree is too small and no section has at least the small end diameter
			heightMeasures.clear();
			return heightMeasures;		// return an empty list
		} else {
			List<Double> heightsToRemove = new ArrayList<Double>();
			while (i < 100) {
				heightsToRemove.add(heightMeasures.get(++i));
			}
			heightMeasures.removeAll(heightsToRemove);
			heightMeasures.add(estimatedHeight);
			return heightMeasures;
		}
	}

	/**
	 * This method returns the volume from an initial height up to a small end diameter.
	 * @param tree an OakAndBeechStemTaperTree instance
	 * @param initialHeightM the initial height (m)
	 * @param smallEndCm the small end diameter (cm)
	 * @return the volume (m3)
	 */
	public double getVolumeBetweenThisHeightAndThisSmallEndDiameter(OakAndBeechStemTaperTree tree, double initialHeightM, double smallEndCm) {
		List<Double> heights = getHeightsArrayWithSmallEndDiameter(tree, initialHeightM, smallEndCm);
		if (heights.isEmpty()) {
			return 0d; 	// no volume at all because the tree is too small
		} else {
			AbstractStemTaperEstimate taperEstimate = getPredictedTaperForTheseHeights(tree, heights);
			Estimate<Matrix, SymmetricMatrix, ?> estimate = taperEstimate.getVolumeEstimate();
			double volume = estimate.getMean().getSumOfElements();
			return volume;
		}
	}

	/*
	 * Useless for this class (non-Javadoc)
	 * @see repicea.simulation.ModelBasedSimulator#init()
	 */
	@Override
	protected void init() {}


	
	
}
