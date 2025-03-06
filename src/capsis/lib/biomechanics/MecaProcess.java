/*
 * Biomechanics library for Capsis4.
 *
 * Copyright (C) 2001-2003  Philippe Ancelin.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
package capsis.lib.biomechanics;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import capsis.commongui.util.Tools;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.util.methodprovider.LAIProvider;
import jeeb.lib.util.Log;
import jeeb.lib.util.ProgressListener;
import jeeb.lib.util.Spatialized;

/**
 * MecaProcess - Contains methods to create and compute trees biomechanical structures.
 *
 * @author Ph. Ancelin - october 2001
 */
public class MecaProcess {
//checked for c4.1.1_08 - fc - 4.2.2003

	private Map treeId_MecaTree;
	private Collection treeIds;
	private Collection treeIdsToCut;
	private MecaSettings settings;
	private MecaConstraints constraints;
	private boolean spatialized;
	private boolean analized;

	private int nbTreesWindThrow;
	private int nbTreesStemBreakage;
	private double standVolume;
	private double windThrowVolume;
	private double stemBreakageVolume;
	private double dominantHeight;
	private double meanHeight;
	private double dominantDbh;
	private double meanDbh;
	private double dominantHD;
	private double meanHD;
	private double dominantCrownBaseHeight;
	private double meanCrownBaseHeight;
	private double dominantCrownRatio;
	private double meanCrownRatio;
	private double dominantCrownBaseRadius;
	private double meanCrownBaseRadius;
	private double meanSLonCR;

	private int nbTreesPerHectare;
	private double meanSpacing;
	private double meanCrownDensity;
	private double xEdge;
	private double frontalAreaIndex;
	private double windSpeedEdgeAtH;
	private double windSpeedStandAtH;
	private double windSpeedStandAtHb;
	private double windSpeedEdgeAt10m;

	private double LAI;
	private double alphaExp;
	private double zeroPlaneDispl;
	private double roughnessLength;
	private double gamma;	// = u* / Uh
	private double zw;	// depth of roughness sublayer
	private double windSpeedStandAtZw;


	private double meanWindContributionToMB;
	private double meanWeightContributionToMB;


	//private double windSpeedAt10m;	// in km.h-1
	//public String windProfile;
	private Step step;


	/**
	 * Constructor.
	 */
	public MecaProcess () {
		treeId_MecaTree = new Hashtable ();
		treeIds = new Vector ();
		treeIdsToCut = new ArrayList ();
		settings = new MecaSettings ();
		constraints = new MecaConstraints ();
		analized = false;

		nbTreesWindThrow = 0;
		nbTreesStemBreakage = 0;
		standVolume = 0d;
		windThrowVolume = 0d;
		stemBreakageVolume = 0d;
		dominantHeight = 0d;
		meanHeight = 0d;
		dominantDbh = 0d;
		meanDbh = 0d;
		dominantHD = 0d;
		meanHD = 0d;
		dominantCrownBaseHeight = 0d;
		meanCrownBaseHeight = 0d;
		dominantCrownBaseRadius = 0d;
		meanCrownBaseRadius = 0d;
		dominantCrownRatio = 0d;
		meanCrownRatio = 0d;
		meanSLonCR = 0d;
		meanCrownDensity = 0d;
		frontalAreaIndex = 0d;
		meanWindContributionToMB = 0d;
		meanWeightContributionToMB = 0d;

		//windSpeedAt10m = 0d;
	}

	public Map getTreeId_MecaTree () {return treeId_MecaTree;}

	public Collection getTreeIds () {return treeIds;}

	public Collection getTreeIdsToCut () {return treeIdsToCut;}

	public MecaSettings getSettings () {return settings;}

	public MecaConstraints getConstraints () {return constraints;}

	public void setConstraints (MecaConstraints c) {constraints = (MecaConstraints) c.clone ();}

	public Step getStep () {return step;}

	public boolean isSpatialized () {return spatialized;}

	public boolean isAnalized () {return analized;}

	public int getNbTreesWindThrow () {return nbTreesWindThrow;}

	public int getNbTreesStemBreakage () {return nbTreesStemBreakage;}

	public double getStandVolume () {return standVolume;}

	public double getWindThrowVolume () {return windThrowVolume;}

	public double getStemBreakageVolume () {return stemBreakageVolume;}

	public double getDominantHeight () {return dominantHeight;}

	public double getMeanHeight () {return meanHeight;}

	public double getDominantDbh () {return dominantDbh;}

	public double getMeanDbh () {return meanDbh;}

	public double getDominantHD () {return dominantHD;}

	public double getMeanHD () {return meanHD;}

	public double getDominantCrownBaseHeight () {return dominantCrownBaseHeight;}

	public double getMeanCrownBaseHeight () {return meanCrownBaseHeight;}

	public double getDominantCrownBaseRadius () {return dominantCrownBaseRadius;}

	public double getMeanCrownBaseRadius () {return meanCrownBaseRadius;}

	public double getDominantCrownRatio () {return dominantCrownRatio;}

	public double getMeanCrownRatio () {return meanCrownRatio;}

	public double getMeanSLonCR () {return meanSLonCR;}

	public int getNbTreesPerHectare () {return nbTreesPerHectare;}

	public double getMeanSpacing () {return meanSpacing;}

	public double getMeanCrownDensity () {return meanCrownDensity;}

	public double getXEdge () {return xEdge;}

	public double getFrontalAreaIndex () {return frontalAreaIndex;}

	public double getWindSpeedEdgeAtH () {return windSpeedEdgeAtH;}

	public double getWindSpeedStandAtH () {return windSpeedStandAtH;}

	public double getWindSpeedStandAtHb () {return windSpeedStandAtHb;}

	public double getWindSpeedEdgeAt10m () {return windSpeedEdgeAt10m;}

	public double getLAI () {return LAI;}

	public double getAlphaExp () {return alphaExp;}

	public double getZeroPlaneDispl () {return zeroPlaneDispl;}

	public double getRoughnessLength () {return roughnessLength;}

	public double getGamma () {return gamma;}

	public double getZw () {return zw;}

	public double getWindSpeedStandAtZw () {return windSpeedStandAtZw;}

	public double getMeanWindContributionToMB () {return meanWindContributionToMB;}

	public double getMeanWeightContributionToMB () {return meanWeightContributionToMB;}

	public void setMeanContributionToMB (double wdc, double wtc) {
		meanWindContributionToMB += wdc;
		meanWeightContributionToMB += wtc;
	}

	public void setNbTreesWindThrow () {nbTreesWindThrow ++;}

	public void setNbTreesStemBreakage () {nbTreesStemBreakage ++;}

	public void setStandVolume (double v) {standVolume += v;}

	public void setWindThrowVolume (double v) {windThrowVolume += v;}

	public void setStemBreakageVolume (double v) {stemBreakageVolume += v;}

	public void setMeanCrownDensity (double cd) {meanCrownDensity += cd;}

	public void setFrontalAreaIndex (double fa) {frontalAreaIndex += fa;}

	/**
	 * Create and fill up the map of correspondance between compatible model trees and Mecatrees
	 * then construct their biomechanical structure by creating growth units in trees.
	 */
	public boolean createMecaTrees (	Collection treeIds,
										MecaSettings settings,
										Step step,
										ProgressListener bar) {
		try {
			this.treeIds = treeIds;
			this.settings = (MecaSettings) settings.clone ();
			this.step = step;
			GScene stand = step.getScene ();
			TreeCollection tc = (TreeCollection) stand;
			Tree ft = tc.getTrees ().iterator ().next ();
			if (ft instanceof Spatialized) {
				spatialized = true;
			} else {
				spatialized = false;
			}

			// calculation of wind parameters according to LAI...
			MethodProvider methodProvider = step.getProject ().getModel ().getMethodProvider ();
			if (methodProvider instanceof LAIProvider) {		// fc - 9.6.2007
				LAI = ((LAIProvider) methodProvider).getLAI (stand, tc.getTrees ());// FC - 8.4.2004
			} else {											// fc - 9.6.2007
				LAI = 3.9;
			}
			double aa = 2.14996;
			double ab = 0.44749;
			alphaExp = aa * Math.pow ((LAI/2), ab);
			//alphaExp = 2.92;

			double cd = 9.18641;
			zeroPlaneDispl = 1 - ((1 - Math.exp (-Math.sqrt (cd * LAI))) / (Math.sqrt (cd * LAI)));
			//zeroPlaneDispl = 0.841;

			if (LAI < 0.8 ) {
				gamma = Math.pow ((0.003 + 0.3 * LAI / 2), 0.5);
			} else {
				gamma = 0.334;
			}
			roughnessLength = (1 - zeroPlaneDispl) * Math.exp (-(0.4/gamma) + Math.log (2) - 0.5);
			//roughnessLength = 0.051;

			// fin param (LAI) !


			bar.setMinMax (0, treeIds.size () - 1);
			int ibar = 0;

			// R�partion uniforme de Cd.
			double crownDragCoef = settings.crownDragCoefficient - settings.cdcVariation;
			double cdcIncrement = 2 * settings.cdcVariation / (treeIds.size () - 1);

			// R�partion uniforme de E.
			double eVar = 0d;//2500d;
			double E = settings.youngModulus - eVar;
			double eIncrement = 2 * eVar / (treeIds.size () - 1);

			// R�partion uniforme de roS.
			double roSVar = 0d;//0.2;
			double roS = settings.woodDensity - roSVar;
			double roSIncrement = 2 * roSVar / (treeIds.size () - 1);

			// R�partion uniforme de roC.
			double roCVar = 0d;//0.6;
			double roC = settings.crownDensity - roCVar;
			double roCIncrement = 2 * roCVar / (treeIds.size () - 1);

			// R�partion uniforme de csr.
			double csrVar = 0d;//0.3;
			double csr = settings.crownStemRatio - csrVar;
			double csrIncrement = 2 * csrVar / (treeIds.size () - 1);

			// For the current step, we consider the given tree ids
			// we don't take the past growth data to construct GUS of a tree !!
   			int nbTrees = treeIds.size ();
			double [] diameter = new double [nbTrees];
			double [] height = new double [nbTrees];
			double [] HD = new double [nbTrees];
			double [] crownBaseHeight = new double [nbTrees];
			double [] crownBaseRadius = new double [nbTrees];
			double [] crownRatio = new double [nbTrees];
			int itree = 0;

   			for (Iterator i = tc.getTrees ().iterator (); i.hasNext ();) {
				Tree t = (Tree) i.next ();
				diameter [itree] = t.getDbh ();
				meanDbh += diameter [itree];
				height [itree] = t.getHeight ();
				meanHeight += height [itree];
				HD [itree] = 100 * height [itree] / diameter [itree];
				meanHD += HD [itree];

				MecaTree mt = new MecaTree (this, t, crownDragCoef, E, roS, roC, csr);
				mt.createDefaultGUs (t/*, settings*/);
				Integer integerId = new Integer (t.getId ());
				treeId_MecaTree.put (integerId, mt);

				crownBaseHeight [itree] = mt.getCrownBaseHeight ();
				meanCrownBaseHeight += crownBaseHeight [itree];
				crownBaseRadius [itree] = mt.getCrownRadius ();
				meanCrownBaseRadius += crownBaseRadius [itree];
				crownRatio [itree] = (height [itree] - crownBaseHeight [itree]) / height [itree];
				meanCrownRatio += crownRatio [itree];
				meanSLonCR += (HD [itree] / crownRatio [itree]);

				crownDragCoef += cdcIncrement;
				E += eIncrement;
				roS += roSIncrement;
				roC += roCIncrement;

				bar.setValue (ibar);
				ibar++;
				itree++;
			}

			// Calculation of mean tree.
			meanDbh /= nbTrees;
			meanHeight /= nbTrees;
			meanHD /= nbTrees;
			meanCrownBaseHeight /= nbTrees;
			meanCrownBaseRadius /= nbTrees;
			meanCrownRatio /= nbTrees;
			meanSLonCR /= nbTrees;

			// Calculation of dominant tree.
   			boolean trie;
   			double dtmp, htmp, hdtmp, cbhtmp, cbrtmp, crtmp;
			// Trie selon diam. decroissant.
			do {
				trie = true;
				for (itree = 0; itree < nbTrees - 1; itree++) {
					if (diameter [itree] < diameter [itree + 1]) {
						trie = false;

						dtmp = diameter [itree];
						diameter [itree] = diameter [itree + 1];
						diameter [itree + 1] = dtmp;

						htmp = height [itree];
						height [itree] = height [itree + 1];
						height [itree + 1] = htmp;

						hdtmp = HD [itree];
						HD [itree] = HD [itree + 1];
						HD [itree + 1] = hdtmp;

						cbhtmp = crownBaseHeight [itree];
						crownBaseHeight [itree] = crownBaseHeight [itree + 1];
						crownBaseHeight [itree + 1] = cbhtmp;

						cbrtmp = crownBaseRadius [itree];
						crownBaseRadius [itree] = crownBaseRadius [itree + 1];
						crownBaseRadius [itree + 1] = cbrtmp;

						crtmp = crownRatio [itree];
						crownRatio [itree] = crownRatio [itree + 1];
						crownRatio [itree + 1] = crtmp;
					}
				}
			} while (!trie);

			double standArea = ((GScene) step.getScene ()).getArea () / 100d; // in ares
			int nbTreesDom = (int) (standArea);
			if (standArea <= 30.0) {
				nbTreesDom--;
			}
			if (nbTreesDom == 0) {
				nbTreesDom = 1;
			}

			//Log.println ("\n\tnbTrees =\t" + nbTrees + "\tnbTreesDom =\t" + nbTreesDom);

			if (nbTreesDom >= nbTrees) {
				dominantDbh = meanDbh;
				dominantHeight = meanHeight;
				dominantHD = meanHD;
				dominantCrownBaseHeight = meanCrownBaseHeight;
				dominantCrownBaseRadius = meanCrownBaseRadius;
				dominantCrownRatio = meanCrownRatio;
			} else {
				for (itree = 0; itree < nbTreesDom; itree++) {
					dominantDbh += diameter [itree];
					dominantHeight += height [itree];
					dominantHD += HD [itree];
					dominantCrownBaseHeight += crownBaseHeight [itree];
					dominantCrownBaseRadius += crownBaseRadius [itree];
					dominantCrownRatio += crownRatio [itree];

					//Log.println ("\n\tD =\t" + diameters [itree] + "\tH =\t" + heights [itree]);
				}
				dominantDbh /= nbTreesDom;
				dominantHeight /= nbTreesDom;
				dominantHD /= nbTreesDom;
				dominantCrownBaseHeight /= nbTreesDom;
				dominantCrownBaseRadius /= nbTreesDom;
				dominantCrownRatio /= nbTreesDom;
			}

			nbTreesPerHectare = (int) (100 * nbTrees / standArea);
			meanSpacing = Math.sqrt (10000d / nbTreesPerHectare);

			double xOrigin = ((GScene) step.getScene ()).getOrigin ().x;
			double yOrigin = ((GScene) step.getScene ()).getOrigin ().y;
			double xExt = ((GScene) step.getScene ()).getXSize ();
			double yExt = ((GScene) step.getScene ()).getYSize ();
			xEdge = xOrigin;
			//xEdge = xOrigin + xExt;
			//xEdge = yOrigin;
			//xEdge = yOrigin + yExt;

			bar.stop ();
		} catch (Exception e) {
			Log.println (Log.ERROR, "MecaProcess.createMecaTrees ()", "Error in createMecaTrees () (returned false)", e);
			return false;
		}
		return true;
	}

	/**
	 * Apply the specified constraints on the Mecatrees.
	 */
	public boolean applyConstraints (	MecaConstraints constraints,
										ProgressListener bar) {
		try {
			//this.constraints = (MecaConstraints) constraints.clone ();
			bar.setMinMax (0, treeIds.size () - 1);
			int ibar = 0;

			for (Iterator i = treeIds.iterator (); i.hasNext ();) {
				Integer integerId = (Integer) i.next ();
				MecaTree mt = (MecaTree) treeId_MecaTree.get (integerId);
				//mt.computeBiomechanicalBehaviourLight ();
				mt.computeBiomechanicalBehaviour ();
				//mt.computeBiomechanicalBehaviour (constraints);
				bar.setValue (ibar);
				ibar++;
			}

			int nbTrees = treeIds.size ();
			meanWindContributionToMB /= nbTrees;
			meanWeightContributionToMB /= nbTrees;
			meanCrownDensity /= nbTrees;
			frontalAreaIndex /= ((GScene) step.getScene ()).getArea ();

			bar.stop ();
		} catch (Exception e) {
			Log.println (Log.ERROR, "MecaProcess.applyConstraints ()", "Error in applyConstraints () (returned false)", e);
			return false;
		}
		return true;
	}

	/**
	 * Manage wind speed.
	 */
	public void setWindSpeeds () {
		double h, hb;
		if (constraints.standHeight.equals ("mean")) {
			h = meanHeight;
			//h = 28.119;
			hb = meanCrownBaseHeight;
			//hb = 15.728;
		} else {
			h = dominantHeight;
			hb = dominantCrownBaseHeight;
		}

		double d = zeroPlaneDispl * h;
		double z0 = roughnessLength * h;
		zw = 2 * h - d;
		double z0E = 0.05;
		//double z0E = 0.06 * h;
		//double z0E = 0.5;

		if (constraints.windAt10m) {
			// vitesse � 10 m pour la dynamique et sensibilit� !!!!!
			double uEdgeAt10m = constraints.windSpeedEdgeAt10m; // en m.s-1
			windSpeedEdgeAtH = uEdgeAt10m * Math.log (h / z0E) / Math.log (10d / z0E);
		} else {
			windSpeedEdgeAtH = constraints.windSpeedEdgeAtH; // en m.s-1
		}

		windSpeedEdgeAt10m = windSpeedEdgeAtH * Math.log (10d / z0E) / Math.log (h / z0E);
		double haut = 10 * h;
		//double haut = 281.19;

		double uHaut = windSpeedEdgeAtH * Math.log (haut / z0E) / Math.log (h / z0E);
		windSpeedStandAtZw = uHaut * Math.log ((zw - d) / z0) / Math.log ((haut - d) / z0);
		windSpeedStandAtH = windSpeedStandAtZw * ( Math.log ((zw - d) / z0) + ((h - zw) / (zw - d)) ) / Math.log ((zw - d) / z0);
		//windSpeedStandAtH = uHaut * Math.log ((h - d) / z0) / Math.log ((haut - d) / z0); // test log --> h
		double p = -alphaExp * (1 - (hb / h));
		windSpeedStandAtHb = windSpeedStandAtH * Math.exp (p);
	}

	/**
	 * Compute the wind speed at the height z (m) according to "constraints" parameters.
	 */
	public double windSpeedAt (double z) {
		double Uz = 0d;
		double h, hb;
		if (constraints.standHeight.equals ("mean")) {
			h = meanHeight;
			//h = 28.119;
			hb = meanCrownBaseHeight;
			//hb = 15.728;
		} else {
			h = dominantHeight;
			hb = dominantCrownBaseHeight;
		}

		double d = zeroPlaneDispl * h;
		double z0 = roughnessLength * h;
		zw = 2 * h - d;
		double z0E = 0.05;

		if (constraints.location.equals ("stand")) {
			if (z >= zw) {
				Uz = windSpeedStandAtZw * Math.log ((z - d) / z0) / Math.log ((zw - d) / z0);
			} else if (z >= h) {
				Uz = windSpeedStandAtZw * ( Math.log ((zw - d) / z0) + ((z - zw) / (zw - d)) ) / Math.log ((zw - d) / z0);
				//Uz = windSpeedStandAtH * Math.log ((z - d) / z0) / Math.log ((h - d) / z0); // test log --> h
			} else if (z >= hb) {
				double p = -alphaExp * (1 - (z / h));
				Uz = windSpeedStandAtH * Math.exp (p);
			} else {
				Uz = windSpeedStandAtHb;
			}
		} else {	// i.e. location.equals ("edge")
			Uz = windSpeedEdgeAtH * Math.log (z / z0E) / Math.log (h / z0E);
		}

		if (Uz < 0d) {
			Uz = 0d;
		}
		return Uz;
	}

	/**
	 * Analyse the mechanical state of the Mecatrees after loading i.e. predict damages.
	 */
	public boolean damageAnalysis (ProgressListener bar, boolean searchCWS) {
		try {
			bar.setMinMax (0, treeIds.size () - 1);
			int ibar = 0;
			String damage;
			if (searchCWS) {
				damage = "";//"\n Uh=\t" + (constraints.windSpeedAtH / 3.6);
			} else {
				damage = "\n\n Damage Analysis \n\n";
			}
			for (Iterator i = treeIds.iterator (); i.hasNext ();) {
				Integer integerId = (Integer) i.next ();
				MecaTree mt = (MecaTree) treeId_MecaTree.get (integerId);
				if (searchCWS) {
					int id = mt.getId ();
					if (id == 1 || id == 2 || id == 3) {
						damage += mt.damageAnalysis () + "\tUh=\t" + (constraints.windSpeedEdgeAtH);
						//damage += mt.damageAnalysisLight () + "\tUh=\t" + (constraints.windSpeedEdgeAtH);
					}
				} else {
					String temp = mt.damageAnalysis ();
					//String temp = mt.damageAnalysisLight ();
				}
				bar.setValue (ibar);
				ibar++;
			}
			bar.stop ();
			Log.println (damage);
		} catch (Exception e) {
			Log.println (Log.ERROR, "MecaProcess.damageAnalysis ()", "Error in damageAnalysis () (returned false)", e);
			return false;
		}
		analized = true;
		return true;
	}

	/**
	 * Export in Log.
	 */
	public boolean exportTrees (ProgressListener bar) {
		try {
			Vector attributes;
			attributes = Tools.exportAttributes (this);
			String export = "\n\n Export Attributes\n\n MecaProcess\n";
			for (int i = 0; i < attributes.size (); i=i+2) {
				export += ((String) attributes.get (i)) + "\t";
			}
			export += "\n";
			for (int i = 1; i < attributes.size (); i=i+2) {
				export += ((String) attributes.get (i)) + "\t";
			}
			export += "\n\n  MecaTrees\n";

			bar.setMinMax (0, treeIds.size () - 1);
			int ibar = 0;

			Integer integerIdFirst = (Integer) treeIds.iterator ().next ();
			MecaTree mtFirst = (MecaTree) treeId_MecaTree.get (integerIdFirst);
			attributes = Tools.exportAttributes (mtFirst);
			for (int i = 0; i < attributes.size (); i=i+2) {
				export += ((String) attributes.get (i)) + "\t";
			}
			export += "\n";

			for (Iterator i = treeIds.iterator (); i.hasNext ();) {
				Integer integerId = (Integer) i.next ();
				MecaTree mt = (MecaTree) treeId_MecaTree.get (integerId);
				attributes = Tools.exportAttributes (mt);
				for (int j = 1; j < attributes.size (); j=j+2) {
					export += ((String) attributes.get (j)) + "\t";
				}
				export += "\n";

				bar.setValue (ibar);
				ibar++;
			}
			bar.stop ();
			Log.println (export);
		} catch (Exception e) {
			Log.println (Log.ERROR, "MecaProcess.exportTrees ()", "Error in exportTrees () (returned false)", e);
			return false;
		}
		return true;
	}

	public boolean exportAttributes (ProgressListener bar) {
		try {
			Vector attributes;
			attributes = Tools.exportAttributes (this);
			bar.setMinMax (0, attributes.size () - 1);
			int ibar = 0;
			String export = "\n\n Export Attributes\n\n MecaProcess\n";
			for (int i = 0; i < attributes.size (); i=i+2) {
				export += ((String) attributes.get (i)) + "\t" + ((String) attributes.get (i+1)) + "\n";
				bar.setValue (ibar);
				ibar++;
			}
			bar.stop ();
			Log.println (export);
		} catch (Exception e) {
			Log.println (Log.ERROR, "MecaProcess.exportTrees ()", "Error in exportTrees () (returned false)", e);
			return false;
		}
		return true;
	}


	/**
	 * Analyses the mechanical state of the Mecatrees after loading = predicts damages.
	 */
	/*public boolean damageAnalysis (ProgressListener bar) {
		try {
			bar.setMinMax (0, treeIds.size () - 1);
			int ibar = 0;
			Log.print ("\n\n Damage Analysis \n\n");
			for (Iterator i = treeIds.iterator (); i.hasNext ();) {
				Integer integerId = (Integer) i.next ();
				MecaTree mt = (MecaTree) treeId_MecaTree.get (integerId);
				mt.damageAnalysis ();
				bar.setValue (ibar);
				ibar++;
			}
			bar.stop ();
		} catch (Exception e) {
			Log.println (Log.ERROR, "MecaProcess.damageAnalysis ()", "Error in damageAnalysis () (returned false)", e);
			return false;
		}
		analized = true;
		return true;
	}*/

	/**
	 * Trace method.
	 */
	public String trace () {
		String str = "\n\nmecaProcess.treeId_MecaTree";
		if (spatialized) {
			str += " (spatialized)\n";
		} else {
			str += " (not spatialized)\n";
		}
		for (Iterator i = treeId_MecaTree.keySet ().iterator (); i.hasNext ();) {
			Integer key = (Integer) i.next ();
			MecaTree mt = (MecaTree) treeId_MecaTree.get (key);
			str += "\nkey=" + key + " " + mt.bigString ();
		}
		return str;
	}

// Drafts

	// With search of past growth data to GUs length...
	public boolean createMecaTreesOld (	Collection treeIds,
										MecaSettings settings,
										Step step,
										ProgressListener bar) {
		try {
			this.treeIds = treeIds;
			this.settings = (MecaSettings) settings.clone ();
			Tree ft = ((TreeCollection) step.getScene ()).getTrees ().iterator ().next ();
			if (ft instanceof Spatialized) {
				spatialized = true;
			} else {
				spatialized = false;
			}
			Vector steps = step.getProject ().getStepsFromRoot (step);
			bar.setMinMax (0, (treeIds.size () * steps.size ()) - 1);
			int ibar = 0;

			// R�partion uniforme de Cd.
			double crownDragCoef = settings.crownDragCoefficient - settings.cdcVariation;
			double cdcIncrement = 2 * settings.cdcVariation / (treeIds.size () - 1);

			// R�partion uniforme de E.
			double eVar = 0d;//2500d;
			double E = settings.youngModulus - eVar;
			double eIncrement = 2 * eVar / (treeIds.size () - 1);

			// R�partion uniforme de roS.
			double roSVar = 0d;//0.2;
			double roS = settings.woodDensity - roSVar;
			double roSIncrement = 2 * roSVar / (treeIds.size () - 1);

			// R�partion uniforme de roC.
			double roCVar = 0d;//0.6;
			double roC = settings.crownDensity - roCVar;
			double roCIncrement = 2 * roCVar / (treeIds.size () - 1);

			// Iteration on the steps from root to the reference step.
			for (Iterator i = steps.iterator (); i.hasNext ();) {
				Step s = (Step) i.next ();
				TreeCollection tc = (TreeCollection) s.getScene ();

				// For each step, we consider only the given tree ids
				for (Iterator j = treeIds.iterator (); j.hasNext ();) {
					Integer integerId = (Integer) j.next ();
					int id = integerId.intValue ();
					Tree t = tc.getTree (id);

					if (t != null) {
					//~ MecaTreeInfo tm = (MecaTreeInfo) t;

						if (!treeId_MecaTree.containsKey (integerId)) {
							// Initial process.
							MecaTree mt = new MecaTree (this, t, crownDragCoef, E, roS, roC, 0.5);
							crownDragCoef += cdcIncrement;
							E += eIncrement;
							roS += roSIncrement;
							roC += roCIncrement;
							treeId_MecaTree.put (integerId, mt);
							mt.createDefaultGUs (t/*, settings*/);

						} else {
							MecaTree mt = (MecaTree) treeId_MecaTree.get (integerId);
							//mt.IncrementAge ();
							mt.createGUs (t/*, settings*/);
						}
					}

					bar.setValue (ibar);
					ibar++;
				}
			}
			bar.stop ();
		} catch (Exception e) {
			Log.println (Log.ERROR, "MecaProcess.createMecaTrees ()", "Error in createMecaTrees () (returned false)", e);
			return false;
		}
		return true;
	}

}




