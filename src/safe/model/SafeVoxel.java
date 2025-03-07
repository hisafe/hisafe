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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import jeeb.lib.util.Identifiable;
import jeeb.lib.util.Vertex3d;
import safe.stics.SafeSticsSoil;
/**
 * 3D voxel of soil attached to a cell
 *
 * @author Isabelle Lecomte -  INRAE Montpellier France - July 2002
 */
public class SafeVoxel implements Serializable, Identifiable {

	private static final long serialVersionUID = 1L;

	/**
	 * This class contains immutable instance variables for a Voxel
	 */
	public static class Immutable implements  Cloneable, Serializable  {
		private static final long serialVersionUID = 1L;
		private int 	 	id;					//id of this voxel
		private SafeCell 	cell;				//cell reference for this voxel
		private SafeLayer 	layer;				//layer of reference for this compartment
		private Vertex3d 	gravityCenter;		//coordinates of gravity center with bottom left origin		
		private double 	 	thickness;			//m
		private double 	 	surfaceDepth;		//top depth from the surface m
		private double 	 	volume;				//m3
		private double  	surfaceDistance;	//gravity center distance to the surface in m
		private double[] 	treeDistance;		//gravity center distance to the trees base stem in m
		private int 	 	nbTrees;			//usefull for table size definition	
	}
	protected Immutable immutable;
	
	//FINE ROOTS
	private double   cropRootsDensity; 			// total roots (rl) m m-3
	private double   cropRootsEffectiveDensity; // only effective roots (flracz) m m-3
	private double[]  treeRootsDensity; 		// m m-3 per tree
	private double[]  treeRootsDensitySen; 		// m m-3 per tree
	private int[] 	 treeRootsAgeInWater; 		// ag of root in saturated voxels (days) per tree	
	
	//WATER REPARTITON MODULE
	private double 	 waterStock;				// liters
	private double[] treeWaterUptake;			// liters
	private double   cropWaterUptake;			// liters
	private double   evaporation;				// mm
	private boolean  isSaturated;				// true/false if voxel is water saturated
	private int   	 saturationDuration;		// number of days since saturation occured

	//CALCUL TURFAC/SENFAC
	private double 	 waterStockNormal;			// liters
	private double 	 waterStockTurfac;			// liters
	private double 	 waterStockSenfac;			// liters
	
	//NITROGEN REPARTITON MODULE
	private double   nitrogenDiffusionFactor;	// cm-2 d-1
	private double   nitrogenConcentration;		// g cm-3
	private double   cropNitrogenUptake;		// g
	private double[] treeNitrogenUptake;		// g
	private double   nitrogenNo3Stock;			// g of Nitrogen NO3 in the voxel
	private double   nitrogenNh4Stock;			// g of Nitrogen NH4 in the voxel 
	
	private double   nitrogenAvailableForBoth;
	private double   nitrogenAvailableForTrees;
	private double   nitrogenAvailableForCrops;
	
	//CARBON ROOTS AND SENESCENCE (Natural death, soil management, root pruning, anoxia) 
	private double[] treeCarbonFineRoots;			// kg C 
	private double[] treeCarbonFineRootsSen;		// kg C 
	private double[] treeCarbonCoarseRoots;			// kg C 
	private double[] treeCarbonCoarseRootsTarget;	// kg C 
	private double[] treeCarbonCoarseRootsSen;		// kg C
	private double[] treeNitrogenFineRoots;      	// kg N 
	private double[] treeNitrogenCoarseRoots;    	// kg N
	private double[] treeNitrogenFineRootsSen;      // kg N 
	private double[] treeNitrogenCoarseRootsSen;    // kg N 


	//DEEP TREE ROOT SENESENCE MINERALISATION
	private double   soilTemperature;						// degrees
	private double   cumulatedTreeNitrogenRootsSen;			// kg N (cumulation of all trees)  
	private double   treeDeepRootsMineralisation;			// kg N (cumulation of all trees) 

	//FOR WATER COMPETITION MODULE
	//RAZ EVERY DAY 
	private Collection<SafeRootVoxel> rootMap;		//storing data for water competition		
	

	//ROOT MODULE
	private double fineRootsLength;
	
	//colonisation direction for each tree 
	// 0 = x+ = right dir = 0
	// 1 = x- = left  dir = 0
	// 2 = y+ = front dir = 1
	// 3 = y- = back  dir = 1
	// 4 = z+ = down  dir = 2
	// 5 = z- = up    dir = 2
	private int [] colonisationDirection;		
	
	
	private double additionalRootsToVoxel ;
	private double fineRootsTotalInvestment;
	private double coefWater0;
	private double coefWater1;
	private double coefNitrogen0;
	private double coefNitrogen1;
	private double coefCost0;
	private double coefCost1;
	private double waterMark;
	private double nitrogenMark;
	private double costMark;
	private double totalMark;
	private double proportion;
	private int neighboursColonisedNumber;
	private double voxelFilling;
	private double L0;
	private double L1;
	private double L2;
	private double Lmin;
	private double Lmax;

	private double[] T1threshold;
	private double[] T2threshold;
	private double[] T3threshold;
	private double waterEfficiency;
	private double nitrogenEfficiency;
	private double fineRootsCost;
	private double waterEfficiencyMax;
	private double nitrogenEfficiencyMax;
	private double fineRootsCostMax;	
	private double phiPFSoil;
	private double phiPFCrop;
	private double waterAvailable;
	private double waterUptakePotential;
	

	/**
	 * Constructor to create a new voxel
	 */
	 public SafeVoxel (int id, SafeLayer layer, SafeCell cell, double thickness,
					  double surfaceDepth,  int nbTrees) {

		createImmutable ();
		immutable.id = id;
		immutable.layer = layer;
		immutable.cell = cell;
		immutable.surfaceDepth = surfaceDepth;
		immutable.thickness = thickness;
		immutable.volume = cell.getArea() * thickness;		//Volume of the voxel
		immutable.nbTrees = nbTrees;
		
		if (nbTrees > 0)
			immutable.treeDistance = new double [nbTrees];
		else
			immutable.treeDistance = null;

		//Voxel gravity center
		if (cell.getOrigin() != null)
		{
			double x = cell.getOrigin().x + (cell.getWidth()/2);
			double y = cell.getOrigin().y + (cell.getWidth()/2);
			double z = surfaceDepth +(thickness/2);
			z =  (Math.round (z * Math.pow (10,2)) ) / (Math.pow (10,2));//rounding

			immutable.gravityCenter = new Vertex3d (x,y,z);
			immutable.surfaceDistance =  Math.pow (
										Math.pow((immutable.gravityCenter.x - immutable.gravityCenter.x),2)
			                          + Math.pow((immutable.gravityCenter.y - immutable.gravityCenter.y),2)
			                          + Math.pow((immutable.gravityCenter.z - 0),2)
			                          , 0.5) ;
		}
		else
		{
			double x = 0;
			double y = 0;
			double z = surfaceDepth +(thickness/2);
			z= ((Math.round (z * Math.pow (10,3)) ) / (Math.pow (10,3)));		//rounding

			immutable.gravityCenter = new Vertex3d (x,y,z);
			immutable.surfaceDistance =  Math.pow (
										Math.pow((immutable.gravityCenter.x - immutable.gravityCenter.x),2)
			                          + Math.pow((immutable.gravityCenter.y - immutable.gravityCenter.y),2)
			                          + Math.pow((immutable.gravityCenter.z - 0),2)
			                          , 0.5) ;
		}

		this.cropRootsDensity = 0;
		this.cropRootsEffectiveDensity = 0;

		if (nbTrees > 0){

			treeWaterUptake = new double[nbTrees];
			treeNitrogenUptake = new double[nbTrees];
			colonisationDirection = new int[nbTrees];
			treeRootsAgeInWater = new int[nbTrees];
			
			treeRootsDensity = new double[nbTrees];
			treeRootsDensitySen = new double[nbTrees];
			
			treeCarbonFineRoots = new double[nbTrees];
			treeCarbonFineRootsSen = new double[nbTrees];
			
			treeCarbonCoarseRoots = new double[nbTrees];
			treeCarbonCoarseRootsTarget = new double[nbTrees];
			treeCarbonCoarseRootsSen = new double[nbTrees];
			
			treeNitrogenFineRoots = new double[nbTrees];
			treeNitrogenCoarseRoots = new double[nbTrees];
			treeNitrogenFineRootsSen = new double[nbTrees];
			treeNitrogenCoarseRootsSen = new double[nbTrees];
			
			//RAZ values 
			Arrays.fill(this.treeWaterUptake, 0);
			Arrays.fill(this.treeNitrogenUptake, 0);
			Arrays.fill(this.treeRootsAgeInWater, 0);
			Arrays.fill(this.colonisationDirection, 0);			
			Arrays.fill(this.treeRootsDensity, 0);
			Arrays.fill(this.treeRootsDensitySen, 0);
			Arrays.fill(this.treeCarbonFineRoots, 0);
			Arrays.fill(this.treeCarbonFineRootsSen, 0);		
			Arrays.fill(this.treeCarbonCoarseRoots, 0);
			Arrays.fill(this.treeCarbonCoarseRootsTarget, 0);
			Arrays.fill(this.treeCarbonCoarseRootsSen, 0);
			Arrays.fill(this.treeNitrogenFineRoots, 0);
			Arrays.fill(this.treeNitrogenCoarseRoots, 0);
			Arrays.fill(this.treeNitrogenFineRootsSen, 0);
			Arrays.fill(this.treeNitrogenCoarseRootsSen, 0);

			
		} else {
			this.treeRootsDensity = null;
			this.treeRootsDensitySen = null;
			this.treeRootsAgeInWater = null;
			this.treeCarbonCoarseRootsTarget = null;
			this.treeCarbonCoarseRootsSen = null;
			this.treeCarbonFineRoots = null;
			this.treeCarbonFineRootsSen = null;
			this.treeNitrogenFineRoots = null; 
			this.treeNitrogenCoarseRoots = null;
			this.treeNitrogenFineRootsSen = null; 
			this.treeNitrogenCoarseRootsSen = null;
			this.treeCarbonCoarseRoots = null;
			this.treeWaterUptake = null;
			this.treeNitrogenUptake = null;
			this.colonisationDirection = null;
	
		}

		//WATER REPARTITION
		this.cropWaterUptake = 0;
		this.waterStock = 0;
		this.isSaturated = false;
		this.saturationDuration = 0;
		this.evaporation = 0;

		//ROOTS
		this.rootMap = null;

		//NITROGEN
		this.nitrogenNo3Stock = 0;
		this.nitrogenNh4Stock = 0;
		this.nitrogenConcentration = 0;
		this.nitrogenDiffusionFactor = 0;

		//TEMPERATURE
		this.soilTemperature = 0;

	}

	/**
	 * Create an Immutable object whose class is declared at one level of the hierarchy.
	 * This is called only in constructor for new logical object in superclass.
	 * If an Immutable is declared in subclass, subclass must redefine this method
	 * (same body) to create an Immutable defined in subclass.
	 */
	protected void createImmutable () {immutable = new Immutable ();}

	public void dailyRaz () {
		
		//WATER REPARTITION
		this.cropWaterUptake = 0;
		this.cropNitrogenUptake = 0;
		if (treeWaterUptake!= null) Arrays.fill(this.treeWaterUptake, 0);
		if (treeNitrogenUptake!= null) Arrays.fill(this.treeNitrogenUptake, 0);

		this.evaporation = 0;
		
		for(int i = 0; i<immutable.nbTrees; i++){
			treeRootsDensitySen[i] = 0;
			treeCarbonCoarseRootsSen[i] = 0;
			treeCarbonFineRootsSen[i] = 0;
			treeNitrogenFineRootsSen[i] = 0;
			treeNitrogenCoarseRootsSen[i] = 0;
		}	
		
		nitrogenAvailableForBoth = 0;
		nitrogenAvailableForTrees = 0;
		nitrogenAvailableForCrops = 0;
	}
	

	/**
	 * Compute the voxel gravity center distance this the origin of one tree (m)
	 */
 	public void computeTreeDistance (int treeIndex, Vertex3d treeOrigin) {
		immutable.treeDistance[treeIndex] =  Math.pow(
												Math.pow((immutable.gravityCenter.x - treeOrigin.x),2)
					                          + Math.pow((immutable.gravityCenter.y - treeOrigin.y),2)
					                          + Math.pow((immutable.gravityCenter.z - treeOrigin.z),2)
				                          , 0.5) ;
	}
	/**
	 * Check if the voxel is rooted according to the tree root shape criteria
	 */
	public boolean isVoxelInShape (int shape, Vertex3d shapeOrigin, double paramShape1, double paramShape2, double paramShape3,
									double plotWidth, double plotHeight, SafeEvolutionParameters evolutionParameters) {

		boolean isIntheShape = false;
		double criteria = 0;
		
		//for toric symetry 5 voxels have to be tested
		//the curent voxel and 4 virtual symetric voxels part of plot dimension
		Vertex3d virtualOriginRight = new Vertex3d (immutable.gravityCenter.x + plotWidth, immutable.gravityCenter.y, immutable.gravityCenter.z);
		Vertex3d virtualOriginLeft  = new Vertex3d (immutable.gravityCenter.x - plotWidth, immutable.gravityCenter.y, immutable.gravityCenter.z);
		Vertex3d virtualOriginAbove = new Vertex3d (immutable.gravityCenter.x, immutable.gravityCenter.y + plotHeight, immutable.gravityCenter.z);
		Vertex3d virtualOriginBelow = new Vertex3d (immutable.gravityCenter.x, immutable.gravityCenter.y - plotHeight, immutable.gravityCenter.z);
				
		//distance calculation between the voxel gravity center and the shape center
		double distanceCurrent  =  Math.pow(
										Math.pow((immutable.gravityCenter.x - shapeOrigin.x),2)
								      + Math.pow((immutable.gravityCenter.y - shapeOrigin.y),2)
								      + Math.pow((immutable.gravityCenter.z - shapeOrigin.z),2)
				               	, 0.5);

		double distanceRight  =  Math.pow(
									 Math.pow((virtualOriginRight.x - shapeOrigin.x),2)
				                   + Math.pow((virtualOriginRight.y - shapeOrigin.y),2)
				                   + Math.pow((virtualOriginRight.z - shapeOrigin.z),2)
		               			, 0.5);

		double distanceLeft  =  Math.pow(
									Math.pow((virtualOriginLeft.x - shapeOrigin.x),2)
				                  + Math.pow((virtualOriginLeft.y - shapeOrigin.y),2)
				                  + Math.pow((virtualOriginLeft.z - shapeOrigin.z),2)
		               			, 0.5);

		double distanceAbove  =  Math.pow(
									 Math.pow((virtualOriginAbove.x - shapeOrigin.x),2)
						           + Math.pow((virtualOriginAbove.y - shapeOrigin.y),2)
						           + Math.pow((virtualOriginAbove.z - shapeOrigin.z),2)
		               			, 0.5);

		double distanceBelow  =  Math.pow(
									 Math.pow((virtualOriginBelow.x - shapeOrigin.x),2)
						           + Math.pow((virtualOriginBelow.y - shapeOrigin.y),2)
						           + Math.pow((virtualOriginBelow.z - shapeOrigin.z),2)
		               			, 0.5);

		
		//the voxel to test is the one closest to the shape center
		double distance = distanceCurrent; 
		if (evolutionParameters.toricXp > 0) distance = Math.min (distanceCurrent,distanceRight);
		if (evolutionParameters.toricXn > 0) distance = Math.min (distance,distanceLeft);
		if (evolutionParameters.toricYp > 0) distance = Math.min (distance,distanceAbove);
		if (evolutionParameters.toricYn > 0) distance = Math.min (distance,distanceBelow);

		Vertex3d originForTest = null;
		if (distance == distanceCurrent)
			originForTest = immutable.gravityCenter;

		if (distance == distanceRight)
			originForTest = virtualOriginRight;

		if (distance == distanceLeft)
			originForTest = virtualOriginLeft;

		if (distance == distanceAbove)
			originForTest = virtualOriginAbove;

		if (distance == distanceBelow)
			originForTest = virtualOriginBelow;


		
		
		//Sheric shape
		//the voxel is in the shape if it's distance to the center of the sphere is < sphere radius (param1)
		if (shape == 1) {
			criteria = distance;
			if (criteria <= paramShape1)  isIntheShape = true;
		}

		//Elipsoide shape
		else if (shape == 2) {
			criteria = (Math.pow((originForTest.x - shapeOrigin.x),2)/Math.pow(paramShape1,2))
						+ (Math.pow((originForTest.y - shapeOrigin.y),2)/Math.pow(paramShape2,2))
						+ (Math.pow((originForTest.z),2)/Math.pow(paramShape3,2));
			if ( criteria <= 1) isIntheShape = true;
		}

		//Conic shape
		else if (shape == 3) {
			if (immutable.gravityCenter.z <= paramShape2) {
				criteria =   Math.sqrt(
									Math.pow((originForTest.x - shapeOrigin.x),2)
									+ Math.pow((originForTest.y - shapeOrigin.y),2));

				if (criteria  <= Math.abs((originForTest.z - paramShape2) * (paramShape1/paramShape2)))
					isIntheShape = true;
			}
		}


		
		return isIntheShape;
	}
	/**
	 * Return fine roots repartition coefficient
	 */
 	public double computeRepartitionCoefficient (int treeIndex, int repartition)
 	{
		//1= return voxel volume
		if (repartition == 1)
			return this.getVolume();
		//2= return the reverse of distance tree-voxel
		else if ((repartition == 2) && (immutable.treeDistance[treeIndex] != 0))
			return (1/immutable.treeDistance[treeIndex]);
		//3= return the negative exp of the distance tree-voxel
		else if ((repartition == 3) && (immutable.treeDistance[treeIndex] != 0))
			return (Math.exp(-immutable.treeDistance[treeIndex]));
		else return 0;
	}
	/**
	 * Compute the carbon repartion for fine roots in rooted voxels
	 */
 	public double computeTreeFineRootsRepartition (int treeIndex, int repartition,
 											   double totalTreeRootLength,
 											   double coefficient) {
		//Initial Root lenght (m) have to be reparted in the shape
		//and converted in m m-3
		double rootDensity = 0;

		//uniform : coefficient is the rooted voxel total volume in m3
		if (repartition == 1)  {
			rootDensity = (totalTreeRootLength / coefficient);  // m / m3
		}
		//reverse : coefficient is total of reverse of distance tree-rooted voxels in m
		if (repartition == 2) {
			if (immutable.treeDistance[treeIndex] != 0) {
				double distance = 1 / immutable.treeDistance[treeIndex];
				rootDensity = ((distance / coefficient) * totalTreeRootLength) / this.getVolume();  // m / m3
			}
		}
		//negative exp : coefficient is total of negative exp of the distance tree-rooted voxels in m
		if (repartition == 3) {
			if (immutable.treeDistance[treeIndex] != 0) {
				double distance = Math.exp(-immutable.treeDistance[treeIndex]);
				rootDensity = ((distance / coefficient) * totalTreeRootLength) / this.getVolume();  // m / m3
			}
		}
		//set the value in the voxel
		setTreeRootsDensity (treeIndex, rootDensity);
		return rootDensity;

	}
 	
	/**
	 * Return total roots diameter (all species trees and crop) in m
	 */
	public double getRootsDiameterTotal (SafeStand stand) {

		double sumLength = 0;
		double sumArea = 0;
		double diameterTotal = 0;

		//for each tree rooted in this voxel
		if (immutable.nbTrees > 0) {
			if (treeRootsDensity != null) {
				for (int t=0; t < immutable.nbTrees ; t++) {
					SafeTree tree = (SafeTree) (stand.getTree(t+1));
					if (tree != null) {			//tree can be null because of thinning intervention
						sumLength += treeRootsDensity [t];
						sumArea   += treeRootsDensity [t]					//m m-3
									 * Math.sqrt(tree.getTreeSpecies().getTreeRootDiameter());	//cm
					}
				}
			}
		}

		//for the crop rooted in this voxel
		if (cropRootsDensity > 0) {
			sumLength += cropRootsDensity;
			sumArea   += cropRootsDensity
					     * Math.sqrt(this.immutable.cell.getCrop().getCell().getCropZone().getCropSpecies().getCropRootDiameter());
		}

		if (sumLength > 0) diameterTotal = Math.pow( (sumArea /sumLength),2);

		return diameterTotal;
	}

	/**
	* Voxel initialisation for water end nitrogen
	*/
	public void initializeWaterNitrogen (double thetaInit,
										double nitrogenNo3Init, double nitrogenNh4Init) {

		//to avoid theta > field capacity
		double thetaSat = this.getLayer().getFieldCapacity();		
		if (thetaInit> thetaSat) thetaInit = thetaSat; 
	
		// if voxel is saturated by waterTable, no need to update water content
		if (this.getIsSaturated () == false) {		
			setWaterStock (thetaInit * this.getVolume () * 1000);		    	//convert m3 m-3 in liters
		}
		
		double cellSurface = this.getVolume() / this.getThickness();
		setNitrogenNo3Stock ((nitrogenNo3Init / 10) * cellSurface);				    //convert kg ha-1 in g
		setNitrogenNh4Stock ((nitrogenNh4Init / 10) * cellSurface);				    //convert kg ha-1 in g

	}

	/**
	* Compute Pressure head in soil at plant root surface (cm)
	* and the associated matrix flux potentials phi_pF (cm-2 day-1)
	*/
	public void computePlantRhizospherePotential (Object plant, SafeGeneralParameters safeSettings, SafeVoxel voxel){
		

		double rootDepth = voxel.getZ();
		double rootDistance = 0;
		double density = 0;

		SafePlantRoot plantRoots = null;
		rootDistance = 0;
		if(plant instanceof SafeCrop){	// if the plant is a crop
			plantRoots = ((SafeCrop)plant).getPlantRoots();
			rootDistance = rootDepth;
			density = voxel.getCropRootsDensity();

		} else {												// if the plant is a tree
			SafeTree t = ((SafeTree)plant);
			//if tree has roots
			if (t.getPlantRoots().getRootTopology() != null) {
				plantRoots = t.getPlantRoots();
				rootDistance = t.getPlantRoots().getRootTopology(voxel).getEffectiveDistance();
				density = voxel.getTheTreeRootsDensity(t.getId()-1);
			}
		}

		double actualWaterPotential = plantRoots.getActualWaterPotential();


		if(actualWaterPotential>=0){
			return;
		}
		double longitudinalTransportPotential = plantRoots.getLongitudinalTransportPotential();


		double rhizospherePotential = actualWaterPotential					//cm
									- rootDistance* longitudinalTransportPotential	// m * cm.m-1 = cm	//GT 20/94/2011 rootDistance instead of rootDistance/meanRootDistance
									//- radialTransportPotential		// cm		// gt - 12.11.2009 desactivé
									+ rootDepth*100;							// m -> cm		//GT 20/94/2011 réactivé

		
		double kSat 	= getLayer().getKSat();
		double alpha 	= getLayer().getAlpha();
		double lambda 	= getLayer().getLambda();
		double n 		= getLayer().getN();

		//convertion in log base 10
		double pF_Rhiz = 0;
		if (rhizospherePotential < 0)
			pF_Rhiz = Math.log (-rhizospherePotential) / Math.log (10);

		double deltaPf  = safeSettings.integrationStep;
		double maxPhiPf = safeSettings.maxPhiPF;

		double phiPf =
			SafePedotransferUtil.getPhi (pF_Rhiz, kSat, alpha, lambda, n, deltaPf, maxPhiPf);


		
		//storing result in rootMap and voxelMap
		plantRoots.addRootedVoxelMap (this, new SafeRootVoxel (plantRoots, this, density, rhizospherePotential, phiPf));
		rootMap.add (plantRoots.getRootedVoxelMap (this));

		if(plant instanceof SafeTree){
			SafeTree tree = (SafeTree) plant;
			//if tree has roots
			if (tree.getPlantRoots().getFirstRootNode() != null) {
				tree.getPlantRoots().getRootTopology(this).setWaterRhizospherePotential(rhizospherePotential);
				tree.getPlantRoots().getRootTopology(this).setPhiPf(phiPf);
			}
		}

	}

	public void razWaterUptakePotential () {
		if (getRootMap() == null) return;
		List<SafeRootVoxel> rootMap = (List<SafeRootVoxel>) getRootMap();
		Iterator<SafeRootVoxel> itr = rootMap.iterator();
		while (itr.hasNext()) {
			SafeRootVoxel voxelRoot =  itr.next();
			SafePlantRoot plantRoot  = voxelRoot.getPlantRoots ();
			voxelRoot.setWaterUptakePotential (0);
			plantRoot.setUptakeWaterPotential (0);
		}
	}
	/**
	* Calculation of total water potential on the basis of matric flux potentials Phi
	* @author Meine Van Noordwijk (ICRAF) - September 2004
	* Reformulated by Gregoire Talbot 2011
	*/

	public void countWaterUptakePotential (SafeStand stand, SafeGeneralParameters safeSettings,
											double cellArea, double plotArea, int day,
											boolean debug) {

		//Sorting root map on PHIPF ASC
		if (getRootMap() == null) return;
		List<SafeRootVoxel> sortedRootMap = (List<SafeRootVoxel>) getRootMap();
		
		Collections.sort(sortedRootMap);		//sorting elements is define by the method compareTo in SafeRootVoxel

		double lastPhiPFForRootTotalComputation = 0;
		double rootDensityCum = 0 ;
		double sumRootDiameterTotal = 0;
		double rootDiameterTotal = 0;
		double rootVGnt = 0;
		double nextPhiPF = 0;
		double nextPotential = 0;

		this.phiPFSoil = this.getPhi (safeSettings);


		//for each plant rooted in this voxel
		//order by PHIPF
		Iterator<SafeRootVoxel> itr = sortedRootMap.iterator();
		while (itr.hasNext()) {

			SafeRootVoxel voxelRoot = itr.next();
			SafePlantRoot plantRoot  = voxelRoot.getPlantRoots ();
			double phiPF   = voxelRoot.getPhiPf();
			this.phiPFCrop = phiPF;
			
			if((phiPF > lastPhiPFForRootTotalComputation) && (phiPF<phiPFSoil)){
				double potWaterUpPerFrd = 0;
				double waterAvailable = 0;

				// a second iterator for checking plants with the same phiPf for computing rootDiameterTotal and rootDensityCum
				Iterator<SafeRootVoxel> itr2 = sortedRootMap.iterator();
				double otherPhiPF = phiPF;
				while (itr2.hasNext() && (otherPhiPF==phiPF)) {
					SafeRootVoxel otherVoxelRoot = itr2.next();
					otherPhiPF   = otherVoxelRoot.getPhiPf();
					
					if (otherPhiPF == phiPF) {
						double dens = otherVoxelRoot.getRootDensity()/safeSettings.MM3_TO_CMCM3;
						rootDensityCum += dens;

						if (otherVoxelRoot.getPlantRoots().getPlant() instanceof SafeTree) {						
							sumRootDiameterTotal += dens * Math.sqrt(((SafeTree)(otherVoxelRoot.getPlantRoots().getPlant())).getTreeSpecies().getTreeRootDiameter());
						} else {
							sumRootDiameterTotal += dens * Math.sqrt(((SafeCrop)(otherVoxelRoot.getPlantRoots().getPlant())).getCell().getCropZone().getCropSpecies().getCropRootDiameter());
						}

					} else {
						nextPhiPF = otherPhiPF;
						nextPotential = otherVoxelRoot.getWaterRhizospherePotential();
					}
				}


				
				rootDiameterTotal = Math.pow(sumRootDiameterTotal/rootDensityCum , 2);
				double rho = 1/(rootDiameterTotal/2*Math.sqrt(Math.PI*rootDensityCum));		// rho = R1/R0
				rootVGnt = (0.5*((1-3*Math.pow(rho,2))/4+Math.pow(rho,4)*Math.log(rho)/(Math.pow(rho,2)-1)))/(Math.pow(rho,2)-1);		// van genuchten function as described by Heinen 2001

				
				if(nextPhiPF <= phiPF){
					nextPhiPF = phiPFSoil;		//PhiPF of the soil
					//MODIF IL 22/01/2015 
					//SET Stone option : here we can extact water only in voxel fine soil
					//nextPotential = this.getWaterPotentialTheta();
					nextPotential = this.getWaterPotentialThetaFineSoil();

				}
				
				
				double phiRange = nextPhiPF-phiPF;
				potWaterUpPerFrd =
						10								// conversion from cm to mm
						* Math.PI
						* this.getThickness()*100		//m to cm
						* phiRange						//cm2 day-1
						/ rootVGnt;						// unitless

				
				
				
				
				// calculer la quantite d'eau fournissable par le sol dans cette gamme de phi. 
				//En deduire alors un vrai uptake potential qui prend ca en compte
				waterAvailable = (this.getLayer().getTheta(nextPotential)-this.getLayer().getTheta(voxelRoot.getWaterRhizospherePotential()))		// m3.m-3
								* this.getThickness()		// m
								* 1000;						// from m to mm
				
				this.waterAvailable = waterAvailable;
				
				if(((potWaterUpPerFrd* rootDensityCum) > waterAvailable)||(this.getIsSaturated())){
					potWaterUpPerFrd = waterAvailable/(rootDensityCum);
				}
				lastPhiPFForRootTotalComputation = phiPF;

				
				double density = voxelRoot.getRootDensity() / safeSettings.MM3_TO_CMCM3; //convert m m-3 in cm cm-3
				
				double waterUptakePot = potWaterUpPerFrd	// mm.(m.m-3)-1
										*density					// m.m-3
										*cellArea;				// m2
				
				this.waterUptakePotential = waterUptakePot;

				if (waterUptakePot > 0) {
					voxelRoot.addWaterUptakePotential (waterUptakePot);
					plantRoot.addUptakeWaterPotential (waterUptakePot);
				}

			}
		}
	}

	/**
	* Calculation of total nitrogen potential
	* @author Meine Van Noordwijk (ICRAF) - January 2005
	*/
	public void countNitrogenUptakePotential (SafeStand stand, SafeGeneralParameters safeSettings) {

	// from a general equation for zero-sink uptake (De Willigen and Van Noordwijk, 1994)
	// on the basis of the total root length in that cell, and allocated to each component
	// proportional to its effective root length

		double nitrogenDiffusionConstant 	= safeSettings.nitrogenDiffusionConstant;
		double nitrogenEffectiveDiffusionA0 = safeSettings.nitrogenEffectiveDiffusionA0;
		double nitrogenEffectiveDiffusionA1 = safeSettings.nitrogenEffectiveDiffusionA1;
		double no3AbsorptionConstant 		= safeSettings.no3AbsorptionConstant;
		double nh4AbsorptionConstant 		= safeSettings.nh4AbsorptionConstant;
		double no3Fraction 					= safeSettings.no3Fraction;

		//MODIF IL 05/04/2018
		//correction bug when stone is included in soil
		//double theta = this.getTheta();
		//double volume = this.getVolume();
		double theta = this.getThetaFineSoil();
		double volume = this.getVolumeFineSoil();
		
		double rootDiameterTotal = this.getRootsDiameterTotal (stand);


		double nitrogenDiffusionFactor = nitrogenDiffusionConstant		//cm2 d-1
									* theta								//m3 m-3
									* Math.max (theta,					//m3 m-3
											   (theta*nitrogenEffectiveDiffusionA1 +nitrogenEffectiveDiffusionA0));

		setNitrogenDiffusionFactor (nitrogenDiffusionFactor); 		  // cm-2 d-1


		double absorptionConstant = ((no3AbsorptionConstant + theta) * (nh4AbsorptionConstant + theta))
								  / (no3AbsorptionConstant + theta + no3Fraction  * (nh4AbsorptionConstant - no3AbsorptionConstant))
								  - theta;

		this.setNitrogenConcentration (((this.getNitrogenNo3Stock()	+ this.getNitrogenNh4Stock())	//g
									/ (absorptionConstant + theta))
									/ (volume * safeSettings.M3_TO_CM3));		//cm-3

		//If NO ROOTS return
		if (getRootMap() == null) return;
	
		Iterator<SafeRootVoxel> itr = getRootMap().iterator();


		double totalSinkStrengthLrv = 0;
		double totalDensity = 0;

		//For each PLANT rooted in this voxel
		//Calculation of total sink strenght
		while (itr.hasNext()) {

			SafeRootVoxel voxelRoot = itr.next();
			SafePlantRoot plantRoot  = voxelRoot.getPlantRoots ();

			// Product of sink strengths and root length densities per plant in this voxel
			double nitrogenSinkStrength   = plantRoot.getNitrogenSinkStrength (); 		// kg/m
			double density = voxelRoot.getRootDensity() / safeSettings.MM3_TO_CMCM3;
			totalSinkStrengthLrv += nitrogenSinkStrength * density;		//somme sur les deux plantes!!!!     kg/m*cm/cm3
			totalDensity += density;
		
		}

		double availableNitrogen = 0; 
		if (totalDensity > 0)  {
			double rhoCombined = 1 / (0.5 * rootDiameterTotal * Math.sqrt (Math.PI * totalDensity) );
			
			
			double gModCombined = -3d/8d
								+ (1 / (4 * Math.pow (rhoCombined,2)))
								+ (1d/2d * (Math.pow (rhoCombined,2)/(Math.pow (rhoCombined,2)-1)) * Math.log (rhoCombined));
	


			//Potential zero-sink supply for all plant (g d-1)
			double zeroSinkCombined = ((Math.PI * getNitrogenConcentration() * getNitrogenDiffusionFactor())
										/ gModCombined)
										* volume
										* safeSettings.M3_TO_CM3;
	
			availableNitrogen = Math.min (zeroSinkCombined, (this.getNitrogenNo3Stock() + this.getNitrogenNh4Stock()));

		}

		this.nitrogenAvailableForBoth=availableNitrogen;
		
		//Sum of potential zero-sink supply for all voxels (g d-1)
		double nitrogenZeroSinkTotal = 0;

		//For each PLANT rooted in this voxel
		Iterator<SafeRootVoxel> itr2 = getRootMap().iterator();
		while (itr2.hasNext()) {

			SafeRootVoxel voxelRoot = itr2.next();
			SafePlantRoot plantRoot  = voxelRoot.getPlantRoots ();
			

			//For each PLANT rooted in this voxel
			//Calculation of relative sink strenght

			//Each plant share in combined uptake
			double nitrogenSinkStrength   = plantRoot.getNitrogenSinkStrength ();
			double density = voxelRoot.getRootDensity() / safeSettings.MM3_TO_CMCM3;
			double nitrogenShareUptake = (nitrogenSinkStrength * density) / totalSinkStrengthLrv;		

			double rootDiameter = 0;

			if (plantRoot.getPlant() instanceof SafeTree)
				rootDiameter = ((SafeTree) plantRoot.getPlant()).getTreeSpecies().getTreeRootDiameter();
			else
				rootDiameter = ((SafeCrop) plantRoot.getPlant()).getCell().getCropZone().getCropSpecies().getCropRootDiameter();

			//Potential zero-sink supply per voxel (g d-1)
			double rho = 1 / (0.5 * rootDiameter * Math.sqrt (Math.PI * density));
			double gMod = -3d/8d
						  + (1 / (4 * Math.pow (rho,2)))
						  + (1d/2d * (Math.pow (rho,2)/(Math.pow (rho,2)-1)) * Math.log (rho));


			
			//Potential zero-sink supply per voxel (g d-1)
			double nitrogenZeroSinkPotential = ((Math.PI * getNitrogenConcentration() *  getNitrogenDiffusionFactor())
												/ gMod)
												* volume
												* safeSettings.M3_TO_CM3;

			voxelRoot.setNitrogenZeroSinkPotential (nitrogenZeroSinkPotential);
			voxelRoot.setNitrogenShareUptake (nitrogenShareUptake);

			//Sum of solo uptake potentials
			nitrogenZeroSinkTotal += nitrogenZeroSinkPotential;
		}


		//For each component the potential uptake is at least the combined uptake potential minus the mon uptake of all others,
		//and at most equal to its own 'mono' potential uptake.
		//In between these upper and lower limits the sharing is based on the relative sink strength of the various plants
		// and the relative share in the root length density
		//For each PLANT rooted in this voxel
		
		Iterator<SafeRootVoxel> itr3 = getRootMap().iterator();
		while (itr3.hasNext()) {

			SafeRootVoxel voxelRoot = itr3.next();
			SafePlantRoot plantRoot  = voxelRoot.getPlantRoots ();

			//For each PLANT rooted in this voxel
			//Calculation of potential uptake
			double nitUptakePot=0;
			double nitrogenShareUptake = voxelRoot.getNitrogenShareUptake ();
			double nitrogenZeroSinkPotential = voxelRoot.getNitrogenZeroSinkPotential ();
			
			nitUptakePot = Math.max (availableNitrogen - nitrogenZeroSinkTotal + nitrogenZeroSinkPotential,
										Math.min(nitrogenZeroSinkPotential,availableNitrogen * nitrogenShareUptake));



			
			if (plantRoot.getPlant() instanceof SafeTree) 
				this.nitrogenAvailableForTrees = nitUptakePot;
			else 		
				this.nitrogenAvailableForCrops = nitUptakePot;

		

			if (nitUptakePot > 0) {
				voxelRoot.addNitrogenUptakePotential (nitUptakePot);
				plantRoot.addNitrogenUptakePotential (nitUptakePot);
			}
		}
	}

	/**
	* Declare a voxel saturated with water	AQ 2011-- 
	* This method is rewrite to include Nitrogen leaching by water table
	*/
	public double[] setIsSaturated (boolean b, SafePlotSettings plotSettings, boolean first) {	//aq 09.08.2011 added stand
		
		double[] waterNStockIncrease = {0,0,0,0,0};		// the first value contains water, the second nitrogen

		isSaturated = b;

		if (b == true) {	//When a voxel is flooded by watertable, theta = field capacity
			if(first)	{	// first call to the method for this day
				addSaturationDuration (); 								//Increase saturation duration
				addTreeRootsAgeInWater();								//Increase all trees age in water  
			}
				
			// voxel water is reset to fieldCapacity (fineSoil + stone) 
			double thetaInit = this.getLayer().getFieldCapacity();		//test AQ 08.08.2011 (in the water table, voxels are at saturation)
			waterNStockIncrease[0]= -(this.getWaterStock()); 	//water stock before saturation in liters
			setWaterStock (thetaInit * this.getVolume () * 1000);		    	//from m3 m-3 to liters/voxel
			waterNStockIncrease[0] += this.getWaterStock(); 				//water stock difference after saturation

			// NO3 fluctuation only if water have changed
			if (waterNStockIncrease[0] !=0) {
				double no3ConcentrationInWaterTable = plotSettings.no3ConcentrationInWaterTable;		//AQ in g/L
				
				//test aq 04.11.2011
				double wtNitrogenNo3Stock = this.getWaterStock()*no3ConcentrationInWaterTable;	//g.voxel-1	
				double tempA = this.getNitrogenNo3Stock()-wtNitrogenNo3Stock;	//calcul variation stock NO3 dans le voxel
				if (tempA >0) {
					waterNStockIncrease[1] = tempA;	//lixiv NO3
				}		
				else {			
					waterNStockIncrease[2] = wtNitrogenNo3Stock-this.getNitrogenNo3Stock();	//Apport NO3 par nappe
				}	
				
				setNitrogenNo3Stock(wtNitrogenNo3Stock);			
				//FIN test aq 04.11.2011
							
				// NH4
				double nh4ConcentrationInWaterTable = plotSettings.nh4ConcentrationInWaterTable;		//AQ
				
				//test aq 04.11.2011
				double wtNitrogenNh4Stock = this.getWaterStock()*nh4ConcentrationInWaterTable;	//g.voxel-1	
				double tempB = this.getNitrogenNh4Stock()-wtNitrogenNh4Stock;	//calcul variation stock NH4 dans le voxel
				if (tempB >0) {
					waterNStockIncrease[3] = tempB;	//lixiv NH4
				}		
				else {			
					waterNStockIncrease[4] = wtNitrogenNh4Stock-this.getNitrogenNh4Stock();	//Apport NH4 par nappe
				}	
	
				setNitrogenNh4Stock(wtNitrogenNh4Stock);			
				//FIN test aq 04.11.2011
			}
				
		} else {	//watertable is receeding, theta doesn't change
			setSaturationDuration (0); 								//RAZ saturation duration
			resetTreeRootsAgeInWater(); 	
		}
		return waterNStockIncrease;
	}
	
	//
	/**
	* AQ	Deep senescent roots mineralization: (from STICS algorithms V5 mineral.for)
	*       For each voxel with Z > P_profhum  
	*/	
	public void deepSenescentRootsMineralization (SafeGeneralParameters settings, 
												  SafeSticsSoil sticsSoil,
												  double humificationDepth,
												  double cellArea) {
		//compute if voxel 
		//- is NOT saturated 
		//- is BELLOW humificationDepth 
		//- has tree nitrogen from dead roots
		double voxelBottom = this.getZ()+(this.getThickness()/2);
		if (!this.getIsSaturated() && voxelBottom > humificationDepth && this.getCumulatedTreeNitrogenRootsSen() > 0) {
			
			double fmin1 = settings.fmin1;
			double fmin2 = settings.fmin2;
			double fmin3 = settings.fmin3;
			
			//humidity factor (fh in STICS) 
			double fh = 0.80*((this.getTheta()-getLayer().getWiltingPoint())/(getLayer().getFieldCapacity()-getLayer().getWiltingPoint()))+0.20;
			
			//température factor (fth - ftr in STICS) 
			//Aspect à revoir donc pour le moment pas de paramètres
			double soilTemp = Math.max(-1,this.getSoilTemperature());		// to avoid Nan in the following calculation
			double ft = Math.pow((-0.566+0.62*Math.exp(0.9125*soilTemp/15)),1.026);
		
			double argi = getLayer().getClay();
			double calc = getLayer().getLimestone();
			double kpot = fmin1/(fmin2+argi)/(fmin3+calc);
			double k = kpot*fh*ft;

			
			//calculation in KG N
			double nMinFromRootSen = k*(this.getCumulatedTreeNitrogenRootsSen());
			
			//IL 17/10/2018 to avoid negative values when theta < Wilting Point 
			nMinFromRootSen = Math.max(0, nMinFromRootSen);
			this.setTreeDeepRootsMineralisation(nMinFromRootSen);
			
			if (nMinFromRootSen > 0) {

				this.addCumulatedTreeNitrogenRootsSen(-nMinFromRootSen);
				
				// Desagregation of HISAFE voxels nitrogen content in STICS miniCouches
				for (int z=this.getMiniCoucheMin(); z <= this.getMiniCoucheMax(); z++) {
					int miniCoucheNumber= this.getMiniCoucheNumber();
					sticsSoil.nit[z+1] += (float) (nMinFromRootSen*10000/cellArea/miniCoucheNumber);		// from kg to kg.ha-1
					sticsSoil.amm[z] += (float) (nMinFromRootSen*10000/cellArea/miniCoucheNumber);			// from kg to kg.ha-1
				}		
			}
		}
	}
	
	
	/**
	* Return theta in m3 m-3
	*/
	public double getTheta () {
		return (getWaterStock () / this.getVolume() / 1000);	    //convert liters to m3 m-3
	}
  
	/**
	* Return theta for fine soil in m3 m-3
	*/
	public double getThetaFineSoil () {
		
		if (getLayer().getStone() == 0) return  getTheta ();
		
		double alpha = ( getLayer().getFieldCapacityStone() -  getLayer().getWiltingPointStone()) 
			       / ( getLayer().getFieldCapacityFineSoil() -  getLayer().getWiltingPointFineSoil() );
		
		return getTheta () /(1 - getLayer().getStone() / 100)/(1+alpha*getLayer().getStone()/100/(1 - getLayer().getStone() / 100));

	}
	
	/**
	* Return theta for stone in m3 m-3
	*/
	public double getThetaStone () {
		if (getLayer().getStone() == 0) return  0;
		
		double alpha = ( getLayer().getFieldCapacityStone() -  getLayer().getWiltingPointStone()) 
			       / ( getLayer().getFieldCapacityFineSoil() -  getLayer().getWiltingPointFineSoil() );
		
		return (getThetaFineSoil() * alpha);	   
	}	


		
	/**
	* Return waterPotential in m3 m-3
	*/
	public double getWaterPotentialTheta () {
		double waterPTheta;
		waterPTheta =
			SafePedotransferUtil.getP(
				this.getTheta (),
				getLayer().getThetaSat (),
				getLayer().getAlpha (),
				getLayer().getN ());
		return waterPTheta;
	}
	/**
	* Return waterPotential of fine soil in m3 m-3
	*/
	public double getWaterPotentialThetaFineSoil () {
		double waterPTheta;
		waterPTheta =
			SafePedotransferUtil.getP(
				this.getThetaFineSoil (),
				getLayer().getThetaSat (),
				getLayer().getAlpha (),
				getLayer().getN ());
		return waterPTheta;
	}
	
	/**
	* Return PHI
	*/

	public double getPhi (SafeGeneralParameters safeSettings) {

		double kSat 	= getLayer().getKSat ();
		double alpha 	= getLayer().getAlpha ();
		double lambda 	= getLayer().getLambda ();
		double n 		= getLayer().getN ();

		double deltaPF = safeSettings.integrationStep;
		double maxPhiPf = safeSettings.maxPhiPF;

		double pF =
				SafePedotransferUtil.getpF(
					//MODIF IL 04/04/2018 
					//correction bug when stone is included in soil
					//this.getTheta (),
					this.getThetaFineSoil (),
					getLayer().getThetaSat (),
					alpha,
					n);
	
		double phiTheta =
				SafePedotransferUtil.getPhi(
					pF,
					kSat,
					alpha,
					lambda,
					n, deltaPF, maxPhiPf);

		return phiTheta;
	}


	
	//********** FOR EXPORT EXTENSION !!!!!!!!!!!!!!*****************/
	//  HAVE TO BE DONE FOR EACH NEW TABLE //
	//****************************************************************/

	public int getTreeRootsDensitySize () {
		return immutable.nbTrees;
	}
	public int getTreeRootsDensitySenSize () {
		return immutable.nbTrees;
	}	
	public int getTreeRootsAgeInWaterSize () {
		return immutable.nbTrees;
	}
	public int getTreeCarbonFineRootsSize () {
		return immutable.nbTrees;
	}
	public int getTreeCarbonFineRootsSenSize () {
		return immutable.nbTrees;
	}	
	public int getTreeCarbonCoarseRootsSenSize () {
		return immutable.nbTrees;
	}	
	public int getTreeNitrogenFineRootsSize () {
		return immutable.nbTrees;
	}	
	public int getTreeNitrogenCoarseRootsSize () {
		return immutable.nbTrees;
	}
	public int getTreeNitrogenFineRootsSenSize () {
		return immutable.nbTrees;
	}	
	public int getTreeNitrogenCoarseRootsSenSize () {
		return immutable.nbTrees;
	}		
	public int getTreeCarbonCoarseRootsSize () {
		return immutable.nbTrees;
	}
	public int getTreeCarbonCoarseRootsTargetSize () {
		return immutable.nbTrees;
	}	
	public int getTreeDistanceSize () {
			return immutable.nbTrees;
	}
	public int getTreeWaterUptakeSize () {
		return immutable.nbTrees;
	}
	public int getTreeWaterUptakeMmSize () {
		return immutable.nbTrees;
	}
	public int getTreeNitrogenUptakeSize () {
		return immutable.nbTrees;
	}
	public int getColonisationDirectionSize () {
		return immutable.nbTrees;
	}	

	
	public String getTreeRootsDensityType () {
		return "Double";
	}
	public String getTreeRootsDensitySenType () {
		return "Double";
	}	
	public String getTreeRootsAgeInWaterType () {
		return "Int";
	}	
	public String getTreeCarbonCoarseRootsType () {
		return "Double";
	}
	public String getTreeCarbonCoarseRootsTargetType () {
		return "Double";
	}
	public String getTreeCarbonFineRootsType () {
		return "Double";
	}
	public String getTreeCarbonFineRootsSenType () {
		return "Double";
	}	
	public String getTreeCarbonCoarseRootsSenType () {
		return "Double";
	}	
	public String getTreeNitrogenFineRootsType () {
		return "Double";
	}	
	public String getTreeNitrogenCoarseRootsType () {
		return "Double";
	}
	public String getTreeNitrogenFineRootsSenType () {
		return "Double";
	}	
	public String getTreeNitrogenCoarseRootsSenType () {
		return "Double";
	}		
	public String getTreeDistanceType () {
			return "Double";
	}
	public String getTreeWaterUptakeType () {
		return "Double";
	}
	public String getTreeWaterUptakeMmType () {
		return "Double";
	}
	public String getTreeNitrogenUptakeType () {
		return "Double";
	}
	public String getColonisationDirectionType () {
		return "Int";
	}	
	public String getPhiPfType () {
		return "Double";		//trees + crop
	}
	

	public int getId () {return immutable.id;}
	public SafeLayer getLayer () {return immutable.layer;}
	public SafeCell getCell () {return immutable.cell;}
	public int getIdCell () {return immutable.cell.getId();}
	public int getIdLayer() {return immutable.layer.getId();}
	public int getIdZone() {return immutable.cell.getCropZone().getId();}
	
	public double getThickness () {return  immutable.thickness;}
	public double getSurfaceDepth () {return  immutable.surfaceDepth;}
	public double getSurfaceDistance () {return immutable.surfaceDistance;}
	public double getVolume() {return  immutable.volume;}
	public double getVolumeFineSoil () {
		if (getLayer().getStone() == 0) return  immutable.volume;
		else return  immutable.volume * (100-getLayer().getStone()) / 100; 
	}
	
	public Vertex3d getGravityCenter () {return immutable.gravityCenter;}
	public double getX () {if (immutable.gravityCenter != null)
								return immutable.gravityCenter.x;
						   else return 0;}
	public double getY () {if (immutable.gravityCenter != null)
								return immutable.gravityCenter.y;
							else return 0;}
	public double getZ () {if (immutable.gravityCenter != null)
								return immutable.gravityCenter.z;
							else return 0;}

	public double getZMax () {if (immutable.gravityCenter != null)
								return (immutable.surfaceDepth + immutable.thickness);
							else return 0;}


	public double [] getTreeDistance () {return immutable.treeDistance;}
	public double getTheTreeDistance (int t) {return immutable.treeDistance[t];}



	/* WATER AND NITROGEN STOCK  */
	public double getEvaporation () {return  evaporation;}
	public double getEvaporationMm () {return  (getEvaporation() / getVolume() * getThickness());}
	public void setEvaporation (double v) {evaporation = v;}

	public boolean getIsSaturated () {return isSaturated;}
	public int getSaturationDuration () {return saturationDuration;}
	public void setSaturationDuration (int d) {saturationDuration = d;}
	public void addSaturationDuration () {saturationDuration += 1;}

	public double getWaterStock () {return waterStock;}
	public double getWaterStockFineSoil () {
		return (getThetaFineSoil () * this.getVolumeFineSoil() * 1000);	    //convert liters to m3 m-3
	}
	public double getWaterStockMm () {return (waterStock / getVolume() * getThickness());}
	public void setWaterStock (double d) {waterStock = d;}

	
	public void reduceWaterStock (double d) {
		if(waterStock == 0) return;
		if(d < 0) return;
		waterStock -= d;
		if(waterStock < 0) waterStock = 0;
	}
	
	public void setNitrogenNo3Stock (double v) {nitrogenNo3Stock = v;}
	public void setNitrogenNh4Stock (double v) {nitrogenNh4Stock = v;}
	public double getNitrogenNo3Stock () {return nitrogenNo3Stock;}
	public double getNitrogenNh4Stock () {return nitrogenNh4Stock;}
	public double getMineralNitrogenStock () {return nitrogenNo3Stock+nitrogenNh4Stock;}
	
	public double getSoilTemperature () {return soilTemperature;}
	public void setSoilTemperature (double v) {soilTemperature = v;}

	public void setNitrogenConcentration (double v) {nitrogenConcentration = v;}
	public double getNitrogenConcentration () {return nitrogenConcentration;}
	public void setNitrogenDiffusionFactor (double v) {nitrogenDiffusionFactor = v;}
	public double getNitrogenDiffusionFactor () {return nitrogenDiffusionFactor;}
	
	//Ajout AQ :
	public void addNitrogenNo3Stock (double v){nitrogenNo3Stock += v;}
	
	
	
	/* TREE FINE ROOT ACCESSORS */
 	
	public Collection<SafeRootVoxel> getRootMap () {return rootMap;}
	public void initRootMap () {rootMap = new ArrayList<SafeRootVoxel> ();}
	
	public double getTheTreeRootsDensity (int t) {
		if (treeRootsDensity != null) return treeRootsDensity[t];
		else return 0;
	}
	public double[] getTreeRootsDensity()
	{
	  if (treeRootsDensity != null) {
		  return treeRootsDensity;  
	  }
	  else {
		if (immutable.nbTrees > 0) {
		  return new double[immutable.nbTrees];
		}
		else {
		  return new double[1];
		}
	  }
	}
	public void setTreeRootsDensity (int t, double  v) {
		if (treeRootsDensity == null) treeRootsDensity = new double [immutable.nbTrees];
		this.treeRootsDensity [t] =  v;
	}

	/**
	* Return total root density in m/m3 of all plants rooted in this voxel (trees + crops)
	*/
	public double getTotalRootsDensity () {
		float total = 0;
		if (treeRootsDensity != null) {
			for (int t=0; t < immutable.nbTrees; t++)
				total += treeRootsDensity[t];
		}
		total += cropRootsDensity;

		return total;
	}

	/**
	 * Return total root density  in m/m3 (all trees) to be displayed in hiSAFE standviewer
	 */
	public double getTotalTreeRootsDensity () {
		double total = 0;
		if (treeRootsDensity != null) {
			for (int t=0; t < immutable.nbTrees; t++)
				total += treeRootsDensity[t];
		}

		return total;
	}

	public double getTheTreeRootsDensitySen (int t) {
		if (treeRootsDensitySen != null) return treeRootsDensitySen[t];
		else return 0;
	}
	public double[] getTreeRootsDensitySen()
	{
	  if (treeRootsDensitySen != null) {
		  return treeRootsDensitySen;  
	  }
	  else {
		if (immutable.nbTrees > 0) {
		  return new double[immutable.nbTrees];
		}
		else {
		  return new double[1];
		}
	  }
	}
	public void setTreeRootsDensitySen (int t, double  v) {
		if (treeRootsDensitySen == null) treeRootsDensitySen = new double [immutable.nbTrees];
		this.treeRootsDensitySen [t] = v;
	}
	
	public void addTreeRootsDensitySen (int t, double  v) {
		if (treeRootsDensitySen == null) treeRootsDensitySen = new double [immutable.nbTrees];
		this.treeRootsDensitySen [t] += v;
	}

	/**
	 * Return total root density  in m/m3 (all trees) to be displayed in hiSAFE standviewer
	 */
	public double getTotalTreeRootsDensitySen () {
		double total = 0;
		if (treeRootsDensitySen != null) {
			for (int t=0; t < immutable.nbTrees; t++)
				total +=  treeRootsDensitySen[t];
		}

		return total;
	}
	

	public int getTheTreeRootsAgeInWater (int t) {
		if (treeRootsAgeInWater  != null) return treeRootsAgeInWater [t];
		else return 0;
	}
	public int[] getTreeRootsAgeInWater ()
	{
	  if (treeRootsAgeInWater  != null) {
		  return treeRootsAgeInWater ;  
	  }
	  else {
		if (immutable.nbTrees > 0) {
		  return new int[immutable.nbTrees];
		}
		else {
		  return new int[1];
		}
	  }
	}
	public void setTreeRootsAgeInWater (int t, int  v) {
		if (treeRootsAgeInWater == null) treeRootsAgeInWater = new int [immutable.nbTrees];
		this.treeRootsAgeInWater [t] =  v;
	}

	public void addTreeRootsAgeInWater () {
		if (treeRootsAgeInWater == null) treeRootsAgeInWater = new int [immutable.nbTrees];
		for (int t=0; t<immutable.nbTrees; t++) {
			if (this.getTheTreeCarbonCoarseRoots(t)>0) this.treeRootsAgeInWater [t] += 1;
		}
	}
	
	public double getTotalTreeRootsLength () {
		return getTotalTreeRootsDensity() * this.getVolume ();
	} 
	
	
	/**
	* Remove tree fine root density after soil management
	*/
	public void razTreeRootsDensity (double proportion) {
		if (treeRootsDensity != null) {
			for (int t=0; t<immutable.nbTrees; t++) {
				//	proportion is the proportion of surviving roots
				if (proportion == 0) this.treeRootsDensity [t] = 0;
				else
					setTreeRootsDensity (t , getTheTreeRootsDensity (t) * proportion);
			}
		}
	}
	

	/* TREE COARSE ROOT ACCESSORS */
	public double getTheTreeCarbonCoarseRoots (int t) {
		if (treeCarbonCoarseRoots != null) return treeCarbonCoarseRoots[t];
		else return 0;}

	public double getTheTreeNitrogenCoarseRoots (int t) {
		if (treeNitrogenCoarseRoots != null) return treeNitrogenCoarseRoots[t];
		else return 0;}
	
	public double[] getTreeCarbonCoarseRoots()
	{
	  if (treeCarbonCoarseRoots != null) return treeCarbonCoarseRoots;  // NULL OCCURS IF NO TREE IN SCENE
	  else {
		if (immutable.nbTrees > 0) {
			double[] nullValues = new double[immutable.nbTrees];
		  return nullValues;
		}
		else {
			double[] nullValues = new double[1];
		  return nullValues;
		}
	  }
	}
	
	public double getTotalTreeCarbonCoarseRoots()
	{
		double total = 0;
		if (treeCarbonCoarseRoots != null) {
			for (int t=0; t < immutable.nbTrees; t++)
				total +=  treeCarbonCoarseRoots[t];
		}
	  return total;
	}
	
	public void setTreeCarbonCoarseRoots (int t, double  v) {
		if (treeCarbonCoarseRoots == null) treeCarbonCoarseRoots = new double [immutable.nbTrees];
		this.treeCarbonCoarseRoots [t] =  v;
	}
	public void addTreeCarbonCoarseRoots (int t, double  v) {
			if (treeCarbonCoarseRoots == null) treeCarbonCoarseRoots = new double [immutable.nbTrees];
			this.treeCarbonCoarseRoots [t] +=  v;
	}

	public void addTreeNitrogenCoarseRoots (int t, double  v) {
		if (treeNitrogenCoarseRoots == null) treeNitrogenCoarseRoots = new double [immutable.nbTrees];
		this.treeNitrogenCoarseRoots [t] +=  v;
}

	
	public double[] getTreeCarbonCoarseRootsTarget()
	{
	  if (treeCarbonCoarseRootsTarget != null) return treeCarbonCoarseRootsTarget;  // NULL OCCURS IF NO TREE IN SCENE
	  else {
		if (immutable.nbTrees > 0) {
			double[] nullValues = new double[immutable.nbTrees];
		  return nullValues;
		}
		else {
			double[] nullValues = new double[1];
		  return nullValues;
		}
	  }
	}
	
	public double getTheTreeCarbonCoarseRootsTarget (int t) {
		if (treeCarbonCoarseRootsTarget  != null) return  treeCarbonCoarseRootsTarget [t];
		else return 0;}
	

	
	public double getTotalTreeCarbonCoarseRootsTarget()
	{
		double total = 0;
		if (treeCarbonCoarseRootsTarget != null) {
			for (int t=0; t < immutable.nbTrees; t++)
				total +=   treeCarbonCoarseRootsTarget[t];
		}
	  return total;
	}
	
	
	public double getTotalTreeCarbonCoarseRootsImbalance()
	{
		double total = 0;
		if (treeCarbonCoarseRootsTarget != null) {
			for (int t=0; t < immutable.nbTrees; t++)
				total +=   treeCarbonCoarseRootsTarget[t] - treeCarbonCoarseRoots[t];
		}
	  return total;
	}
	
	public double getTreeCarbonCoarseRootsImbalance(int t)
	{
		if (treeCarbonCoarseRootsTarget != null) return treeCarbonCoarseRootsTarget[t] - treeCarbonCoarseRoots[t];
		else return 0;
	}
	

	public void addTreeCarbonCoarseRootsTarget (int t, double  v) {
		if (treeCarbonCoarseRootsTarget == null) treeCarbonCoarseRootsTarget = new double [immutable.nbTrees];
		this.treeCarbonCoarseRootsTarget [t] +=  v;
	}
	
	public void setTreeCarbonCoarseRootsTarget (int t, double  v) {
		if (treeCarbonCoarseRootsTarget == null) treeCarbonCoarseRootsTarget = new double [immutable.nbTrees];
		this.treeCarbonCoarseRootsTarget [t] =   v;
	}
	
	/* TREE CARBON COARSE ROOT SENESCENCE ACCESSORS */
	public double getTotalTreeCarbonCoarseRootsSen () {
		double total=0; 
		if (this.treeCarbonCoarseRootsSen != null ) {
			for (int t=0; t<immutable.nbTrees; t++) {
				total += this.treeCarbonCoarseRootsSen [t];
			}
		}
		return total;
	}
	public double getTotalTreeNitrogenCoarseRoots () {
		double total=0; 
		if (this.treeNitrogenCoarseRoots != null ) {
			for (int t=0; t<immutable.nbTrees; t++) {
				total += this.treeNitrogenCoarseRoots [t];
			}
		}
		return total;
	}
	public double getTotalTreeNitrogenCoarseRootsSen () {
		double total=0; 
		if (this.treeNitrogenCoarseRootsSen != null ) {
			for (int t=0; t<immutable.nbTrees; t++) {
				total += this.treeNitrogenCoarseRootsSen [t];
			}
		}
		return total;
	}
	public double getTotalTreeCarbonFineRoots () {
		double total=0; 
		if (this.treeCarbonFineRoots != null ) {
			for (int t=0; t<immutable.nbTrees; t++) {
				total += this.treeCarbonFineRoots [t];
			}
		}
		return total;
		}
	public double getTotalTreeCarbonFineRootsSen () {
		double total=0; 
		if (this.treeCarbonFineRootsSen != null ) {
			for (int t=0; t<immutable.nbTrees; t++) {
				total += this.treeCarbonFineRootsSen [t];
			}
		}
		return total;
		}
	public double getTotalTreeNitrogenFineRoots () {
		double total=0; 
		if (this.treeNitrogenFineRoots != null ) {
			for (int t=0; t<immutable.nbTrees; t++) {
				total += this.treeNitrogenFineRoots [t];
			}
		}
		return total;
	}
	public double getTotalTreeNitrogenFineRootsSen () {
		double total=0; 
		if (this.treeNitrogenFineRootsSen != null ) {
			for (int t=0; t<immutable.nbTrees; t++) {
				total += this.treeNitrogenFineRootsSen [t];
			}
		}
		return total;
	}


	public double[] getTreeCarbonFineRoots()
	{
	  if (treeCarbonFineRoots != null) return treeCarbonFineRoots;  // NULL OCCURS IF NO TREE IN SCENE
	  else {
		if (immutable.nbTrees > 0) {
		  double[] nullValues = new double[immutable.nbTrees];
		  return nullValues;
		}
		else {
		  double[] nullValues = new double[1];
		  return nullValues;
		}
	  }
	}
	
	

	public double getTheTreeCarbonFineRoots (int t) {
		if (treeCarbonFineRoots != null) return treeCarbonFineRoots[t];
		else return 0;}
	
	public double[] getTreeCarbonFineRootsSen()
	{
	  if (treeCarbonFineRootsSen != null) return treeCarbonFineRootsSen;  // NULL OCCURS IF NO TREE IN SCENE
	  else {
		if (immutable.nbTrees > 0) {
		  double[] nullValues = new double[immutable.nbTrees];
		  return nullValues;
		}
		else {
		  double[] nullValues = new double[1];
		  return nullValues;
		}
	  }
	}

	public double[] getTreeNitrogenFineRoots()
	{
	  if (treeNitrogenFineRoots != null) return treeNitrogenFineRoots;  // NULL OCCURS IF NO TREE IN SCENE
	  else {
		if (immutable.nbTrees > 0) {
		  double[] nullValues = new double[immutable.nbTrees];
		  return nullValues;
		}
		else {
		  double[] nullValues = new double[1];
		  return nullValues;
		}
	  }
	}
	
	public double getTheTreeNitrogenFineRoots (int t) {
		if (treeNitrogenFineRoots != null) return treeNitrogenFineRoots[t];
		else return 0;}
	
	public double[] getTreeNitrogenFineRootsSen()
	{
	  if (treeNitrogenFineRootsSen != null) return treeNitrogenFineRootsSen;  // NULL OCCURS IF NO TREE IN SCENE
	  else {
		if (immutable.nbTrees > 0) {
		  double[] nullValues = new double[immutable.nbTrees];
		  return nullValues;
		}
		else {
		  double[] nullValues = new double[1];
		  return nullValues;
		}
	  }
	}
	public double[] getTreeCarbonCoarseRootsSen()
	{
	  if (treeCarbonCoarseRootsSen != null) return treeCarbonCoarseRootsSen;  // NULL OCCURS IF NO TREE IN SCENE
	  else {
		if (immutable.nbTrees > 0) {
		  double[] nullValues = new double[immutable.nbTrees];
		  return nullValues;
		}
		else {
		  double[] nullValues = new double[1];
		  return nullValues;
		}
	  }
	}

	public double[] getTreeNitrogenCoarseRootsSen()
	{
	  if (treeNitrogenCoarseRootsSen != null) return treeNitrogenCoarseRootsSen;  // NULL OCCURS IF NO TREE IN SCENE
	  else {
		if (immutable.nbTrees > 0) {
		  double[] nullValues = new double[immutable.nbTrees];
		  return nullValues;
		}
		else {
		  double[] nullValues = new double[1];
		  return nullValues;
		}
	  }
	}
	
	public double[] getTreeNitrogenCoarseRoots()
	{
	  if (treeNitrogenCoarseRoots != null) return treeNitrogenCoarseRoots;  // NULL OCCURS IF NO TREE IN SCENE
	  else {
		if (immutable.nbTrees > 0) {
		  double[] nullValues = new double[immutable.nbTrees];
		  return nullValues;
		}
		else {
		  double[] nullValues = new double[1];
		  return nullValues;
		}
	  }
	}
	
	public void setTreeCarbonCoarseRootsSen (int t, double  v) {
		if (treeCarbonCoarseRootsSen == null) treeCarbonCoarseRootsSen = new double [immutable.nbTrees];
		this.treeCarbonCoarseRootsSen [t] = v;
	}
	public void setTreeNitrogenCoarseRootsSen (int t, double  v) {
		if (treeNitrogenCoarseRootsSen == null) treeNitrogenCoarseRootsSen = new double [immutable.nbTrees];
		this.treeNitrogenCoarseRootsSen [t] = v;
	}
	public void setTreeNitrogenCoarseRoots (int t, double  v) {
		if (treeNitrogenCoarseRoots == null) treeNitrogenCoarseRoots = new double [immutable.nbTrees];
		this.treeNitrogenCoarseRoots [t] = v;
	}
	public void setTreeCarbonFineRoots (int t, double  v) {
		if (treeCarbonFineRoots == null) treeCarbonFineRoots = new double [immutable.nbTrees];
		this.treeCarbonFineRoots [t] = v;
	}

	public void setTreeCarbonFineRootsSen (int t, double  v) {
		if (treeCarbonFineRootsSen == null) treeCarbonFineRootsSen = new double [immutable.nbTrees];
		this.treeCarbonFineRootsSen [t] = v;
	}

	public void addTreeCarbonFineRootsSen (int t, double  v) {
		if (treeCarbonFineRootsSen == null) treeCarbonFineRootsSen = new double [immutable.nbTrees];
		this.treeCarbonFineRootsSen [t] += v;
	}
	
	public void addTreeNitrogenFineRootsSen (int t, double  v) {
		if (treeNitrogenFineRootsSen == null) treeNitrogenFineRootsSen = new double [immutable.nbTrees];
		this.treeNitrogenFineRootsSen [t] += v;
	}
	
	public void addTreeCarbonCoarseRootsSen (int t, double  v) {
		if (treeCarbonCoarseRootsSen == null) treeCarbonCoarseRootsSen = new double [immutable.nbTrees];
		this.treeCarbonCoarseRootsSen [t] += v;
	}
	
	
	public void addTreeNitrogenCoarseRootsSen (int t, double  v) {
		if (treeNitrogenCoarseRootsSen == null) treeNitrogenCoarseRootsSen = new double [immutable.nbTrees];
		this.treeNitrogenCoarseRootsSen [t] += v;
	}
	
	public void setTreeNitrogenFineRootsSen (int t, double  v) {
		if (treeNitrogenFineRootsSen == null) treeNitrogenFineRootsSen = new double [immutable.nbTrees];
		this.treeNitrogenFineRootsSen [t] = v;
	}
	
	public void setTreeNitrogenFineRoots (int t, double  v) {
		if (treeNitrogenFineRoots == null) treeNitrogenFineRoots = new double [immutable.nbTrees];
		this.treeNitrogenFineRoots [t] = v;
	}
	
	public void addTreeCarbonFineRoots (int t, double  v) {
		if (treeCarbonFineRoots == null) treeCarbonFineRoots = new double [immutable.nbTrees];
		this.treeCarbonFineRoots [t] += v;
	}
	public void addTreeNitrogenFineRoots (int t, double  v) {
		if (treeNitrogenFineRoots == null) treeNitrogenFineRoots = new double [immutable.nbTrees];
		this.treeNitrogenFineRoots [t] += v;
	}
	
	public void addCumulatedTreeNitrogenRootsSen (double c) {cumulatedTreeNitrogenRootsSen+=c;}	//AQ
	public double getCumulatedTreeNitrogenRootsSen () {return cumulatedTreeNitrogenRootsSen;}

	public void setTreeDeepRootsMineralisation (double c) {treeDeepRootsMineralisation=c;}	//AQ
	public double getTreeDeepRootsMineralisation () {return treeDeepRootsMineralisation;}
	
	/**
	* return all trees water uptake
	*/
	public double [] getTreeWaterUptake () {
		if (treeWaterUptake != null) return (treeWaterUptake);
		else {
			if (immutable.nbTrees > 0) {
			  double[] nullValues = new double[immutable.nbTrees];
			  return nullValues;
			}
			else {
			  double[] nullValues = new double[1];
			  return nullValues;
			}
		  }
	}
	public double [] getTreeWaterUptakeMm () {
		if (treeWaterUptake != null) {
			double[] mmValues = new double[immutable.nbTrees];
			for (int i=0; i<immutable.nbTrees; i++) {
				mmValues[i] = treeWaterUptake[i] / getVolume() * getThickness();
			}
			return (mmValues);
		}
		else {
			if (immutable.nbTrees > 0) {
			  double[] nullValues = new double[immutable.nbTrees];
			  return nullValues;
			}
			else {
			  double[] nullValues = new double[1];
			  return nullValues;
			}
		  }
	}
	
	/**
	 * Return total tree water uptake (all trees) 
	 */
	public double getTotalTreeWaterUptake() {
		double total = 0;
		if (treeWaterUptake != null) {
			for (int t=0; t < immutable.nbTrees; t++)
				total +=  treeWaterUptake[t];
		}

		return total;
	}
	
	/**
	* return one tree water uptake
	*/
	public double getTheTreeWaterUptake (int t) {
		if (treeWaterUptake == null) return 0;
		else return  treeWaterUptake[t];
	}
	public double getTheTreeWaterUptakeMm (int t) {
		if (treeWaterUptake == null) return 0;
		else return (treeWaterUptake[t] / getVolume() * getThickness());
	}
	/**
	* set one tree water uptake
	*/
	public void setTreeWaterUptake (int t, double v) {
		if (treeWaterUptake == null) treeWaterUptake = new double[immutable.nbTrees];
		treeWaterUptake[t] =  v;
	}
	/**
	* add one tree water uptake
	*/
	public void addTreeWaterUptake (int t, double v) {
		if (treeWaterUptake == null) treeWaterUptake = new double[immutable.nbTrees];
		treeWaterUptake[t] +=  v;
	}
	/**
	* return all trees nitrogen uptake
	*/
	public double [] getTreeNitrogenUptake () {
		if (treeNitrogenUptake != null) return treeNitrogenUptake;
		else {
			if (immutable.nbTrees > 0) {
			  double[] nullValues = new double[immutable.nbTrees];
			  return nullValues;
			}
			else {
			  double[] nullValues = new double[1];
			  return nullValues;
			}
	  }
	}
	
	/**
	 * Return total tree nitrogen uptake (all trees) 
	 */
	public double getTotalTreeNitrogenUptake() {
		double total = 0;
		if (treeNitrogenUptake != null) {
			for (int t=0; t < immutable.nbTrees; t++)
				total +=  treeNitrogenUptake[t];
		}

		return total;
	}	
	/**
	* return one tree nitrogen uptake
	*/
	public double getTheTreeNitrogenUptake (int t) {
		if (treeNitrogenUptake == null) return 0;
		else return  treeNitrogenUptake[t];
	}
	/**
	* set one tree nitrogen uptake
	*/
	public void setTreeNitrogenUptake (int t, double v) {
		if (treeNitrogenUptake == null) treeNitrogenUptake = new double[immutable.nbTrees];
		treeNitrogenUptake[t] =  v;
	}
	/**
	* add one tree water uptake
	*/
	public void addTreeNitrogenUptake (int t, double v) {
		if (treeNitrogenUptake == null) treeNitrogenUptake = new double[immutable.nbTrees];
		treeNitrogenUptake[t] +=  v;
	}
	
	
	
	/*CROP*/
	public double getCropRootsDensity () {return cropRootsDensity;}
	public void  setCropRootsDensity (double v) {cropRootsDensity =  v;}
	public double getCropRootsEffectiveDensity () {return cropRootsEffectiveDensity;}
	public void  setCropRootsEffectiveDensity (double v) {cropRootsEffectiveDensity =  v;}
	
	
	public double getCropWaterUptake () {return cropWaterUptake;}


	public double getCropWaterUptakeMm () {
		return (cropWaterUptake / getVolume() * getThickness());}

	public void setCropWaterUptake (double v) {cropWaterUptake =  v;}
	public void addCropWaterUptake (double v) {cropWaterUptake +=  v;}
	
	public double getCropNitrogenUptake () {return cropNitrogenUptake;}

	public void setCropNitrogenUptake (double v) {cropNitrogenUptake =  v;}
	public void addCropNitrogenUptake (double v) {cropNitrogenUptake +=  v;}
	public double getNitrogenAvailableForBoth() {return nitrogenAvailableForBoth;}
	public double getNitrogenAvailableForTrees() {return nitrogenAvailableForTrees;}
	public double getNitrogenAvailableForCrops() {return nitrogenAvailableForCrops;}
		
	/*COLONISATION*/
	public int[] getColonisationDirection () {		
		if (colonisationDirection != null) return colonisationDirection;
		else {
			if (immutable.nbTrees > 0) {
			  int[] nullValues = new int[immutable.nbTrees];
			  for (int t=0; t < immutable.nbTrees; t++) {
				  nullValues[t] = -9999;
			  }
				  
			  return nullValues;
			}
			else {
			  int[] nullValues = new int[1];
			  nullValues[0] = -9999;
			  return nullValues;
			}
	  }
	}	
	
	public int getTheColonisationDirection (int treeIndex) {
		if (colonisationDirection != null) return colonisationDirection[treeIndex];
		else return -9999;
	}
	

	



	
	//a enlever c'est pour tester le module
	public double getFineRootsTotalInvestment() {return  fineRootsTotalInvestment;}
	public void setFineRootsTotalInvestment (double v) {fineRootsTotalInvestment =  v;}
	public double getAdditionalRootsToVoxel () {return  additionalRootsToVoxel ;}
	public void setAdditionalRootsToVoxel (double v) {additionalRootsToVoxel =  v;}
	
	public double[] getT1threshold() {
		if (T1threshold==null) T1threshold = new double[6];
		return T1threshold;}
	public double[] getT2threshold() {
		if (T2threshold==null) T2threshold = new double[6];
		return T2threshold;}
	public double[] getT3threshold() {
		if (T3threshold==null) T3threshold = new double[6];
		return T3threshold;}

	public int getT1thresholdSize () {
		return 6;
	}
	public int getT2thresholdSize () {
		return 6;
	}	public int getT3thresholdSize () {
		return 6;
	}	
	
	public double getWaterMark() {return waterMark;}
	public double getNitrogenMark() {return nitrogenMark;}
	public double getCostMark() {return costMark;}
	public double getTotalMark() {return totalMark;}
	public double getProportion() {return proportion;}
	public double getVoxelFilling() {return voxelFilling;}
	public int getNeighboursColonisedNumber() {return neighboursColonisedNumber;}
	public double getCoefWater0() {return coefWater0;}
	public double getCoefWater1() {return coefWater1;}
	public double getCoefNitrogen0() {return coefNitrogen0;}
	public double getCoefNitrogen1() {return coefNitrogen1;}
	public double getCoefCost0() {return coefCost0;}
	public double getCoefCost1() {return coefCost1;}	
	public double getFineRootsLength() {return fineRootsLength;}
	
	

	public double getL0() {return L0;}
	public double getL1() {return L1;}
	public double getL2() {return L2;}
	public double getLmin() {return Lmin;}
	public double getLmax() {return Lmax;}
	public double getPhiPFSoil() {return phiPFSoil;}
	public double getPhiPFCrop() {return phiPFCrop;}
	public double getWaterAvailable() {return waterAvailable;}
	public double getWaterUptakePotential() {return waterUptakePotential;}
	

	
	public void setWaterMark(double v) {waterMark = v;}
	public void setNitrogenMark(double v) {nitrogenMark = v;}
	public void setCostMark(double v) {costMark = v;}
	public void setTotalMark(double v) {totalMark = v;}
	public void setProportion(double v) {proportion = v;}
	public void setVoxelFilling(double v) {voxelFilling = v;}
	public void setNeighboursColonisedNumber(int v) {neighboursColonisedNumber = v;}
	public void setColonisationDirection(int treeIndex, int v) {colonisationDirection[treeIndex] = v;}
	
	public void setCoefWater0(double v) {coefWater0 = v;}
	public void setCoefWater1(double v) {coefWater1 = v;}
	public void setCoefNitrogen0(double v) {coefNitrogen0 = v;}
	public void setCoefNitrogen1(double v) {coefNitrogen1 = v;}
	public void setCoefCost0(double v) {coefCost0 = v;}
	public void setCoefCost1(double v) {coefCost1 = v;}
	
	public void setL0(double v) {L0 = v;}
	public void setL1(double v) {L1 = v;}
	public void setL2(double v) {L2 = v;}
	public void setLmin(double v) {Lmin = v;}
	public void setLmax(double v) {Lmax = v;}


	
	public void setFineRootsLength(double v) {fineRootsLength = v;}
	
	public void setT1threshold(int i, double v) {
		if (T1threshold==null) T1threshold = new double[6];
		T1threshold[i] = v;
	}
	public void setT2threshold(int i, double v) {
		if (T2threshold==null) T2threshold = new double[6];
		T2threshold[i] = v;}
	public void setT3threshold(int i, double v) {
		if (T3threshold==null) T3threshold = new double[6];
		T3threshold[i] = v;}

	public double getWaterEfficiency() {return waterEfficiency;}
	public double getNitrogenEfficiency() {return nitrogenEfficiency;}
	public double getFineRootCost() {return fineRootsCost;}

	public double getWaterEfficiencyMax() {return waterEfficiencyMax;}
	public double getNitrogenEfficiencyMax() {return nitrogenEfficiencyMax;}
	public double getFineRootsCostMax() {return fineRootsCostMax;}
	
	
	public void setWaterEfficiency(double v) {waterEfficiency = v;}
	public void setNitrogenEfficiency(double v) {nitrogenEfficiency = v;}
	public void setFineRootsCost(double v) {fineRootsCost = v;}
	public void setWaterEfficiencyMax(double v) {waterEfficiencyMax = v;}
	public void setNitrogenEfficiencyMax(double v) {nitrogenEfficiencyMax = v;}
	public void setFineRootsCostMax(double v) {fineRootsCostMax = v;}


	//STICS MINICOUCHES
	public int getMiniCoucheMin () {
		return (int) (this.getSurfaceDepth() * 100);	
	}
	
	public int getMiniCoucheNumber () {	
		return (int) (this.getThickness() * 100);
	}
	
	public int getMiniCoucheMax () {
		return  getMiniCoucheMin() + getMiniCoucheNumber() - 1;
	}	
	//for export
	public int getIdVoxel() {return getId();}
	
	public float getLracz() {
		float v = 0;
		for (int i=getMiniCoucheMin ();i<=getMiniCoucheMax  ();i++) {
			v += this.immutable.cell.getCrop().sticsCrop.lracz[i];
		}
		return v;
	}
	
	
	public float getLracsenz() {
		float v = 0;
		for (int i=getMiniCoucheMin ();i<=getMiniCoucheMax  ();i++) {
			v += this.immutable.cell.getCrop().sticsCrop.lracsenz[i];
		}
		return v;
	}
	
	public float getRljour() {
		float v = 0;
		for (int i=getMiniCoucheMin ();i<=getMiniCoucheMax  ();i++) {
			v += this.immutable.cell.getCrop().sticsCrop.rljour[i];
		}
		return v;
	}

	public float getFhumirac() {
		float v = 0;
		for (int i=getMiniCoucheMin ();i<=getMiniCoucheMax  ();i++) {
			v += this.immutable.cell.getCrop().sticsCrop.fhumirac[i];
		}
		return v;
	}
	
	
	public float getSurfaceCarbonResidues() {
		float v = 0;
		for (int ir=0 ;ir<10 ;ir++) {
			for (int iz=getMiniCoucheMin ();iz<=getMiniCoucheMax  ();iz++) {
				int index = ir*1000+iz;
				v += this.immutable.cell.getCrop().sticsCommun.Cres[index];
			}
		}
		return v;
	}			
	public float getSurfaceNitrogenResidues() {
		float v = 0;
		for (int r=0 ;r<10 ;r++) {
			for (int i=getMiniCoucheMin ();i<=getMiniCoucheMax  ();i++) {
				int index = r*1000+i;
				v += this.immutable.cell.getCrop().sticsCommun.Nres[index];
			}
		}
		return v;
	}
	
	public float getDeepCarbonResidues() {
		float v = 0;
		for (int ir=10 ;ir<20 ;ir++) {
			for (int iz=getMiniCoucheMin ();iz<=getMiniCoucheMax  ();iz++) {
				int index = ir*1000+iz;
				v += this.immutable.cell.getCrop().sticsCommun.Cres[index];
			}
		}
		return v;
	}			
	public float getDeepNitrogenResidues() {
		float v = 0;
		for (int r=10 ;r<20 ;r++) {
			for (int i=getMiniCoucheMin ();i<=getMiniCoucheMax  ();i++) {
				int index = r*1000+i;
				v += this.immutable.cell.getCrop().sticsCommun.Nres[index];
			}
		}
		return v;
	}
	
	public float getRootCarbonResidues() {
		float v = 0;
		int ir = 20;
		for (int iz=getMiniCoucheMin ();iz<=getMiniCoucheMax  ();iz++) {
			int index = ir*1000+iz;
			v += this.immutable.cell.getCrop().sticsCommun.Cres[index];
		}
		
		return v;
	}			
	public float getRootNitrogenResidues() {
		float v = 0;
		int r = 20;
		for (int i=getMiniCoucheMin ();i<=getMiniCoucheMax  ();i++) {
			int index = r*1000+i;
			v += this.immutable.cell.getCrop().sticsCommun.Nres[index];
		}
		
		return v;
	}
	
	public double getWaterStockSenfac() {return waterStockSenfac;}
	public void setWaterStockSenfac (double v) {waterStockSenfac= v;}
	public double getWaterStockTurfac() {return waterStockTurfac;}
	public void setWaterStockTurfac (double v) {waterStockTurfac= v;}
	
	/**
	 * Calul de waterStockTurfac
	 */
 	public void computeWaterStockTurfac (double psisto, double psiturg) {
 		waterStockNormal  = waterStock; 			
 		waterStockTurfac = waterStock;
 		
 		//on verifie qu'on ne descend pas en dessous du PF
 		double waterStockMin =  this.getLayer().getWiltingPoint() * this.getVolume() * 1000;	
 		if (waterStock > waterStockMin) {
 			waterStockTurfac = waterStock - (this.getVolume() * 1d/80d * Math.log(psisto/psiturg) * 1000);
 			if (waterStockTurfac < waterStockMin) waterStockTurfac = waterStockMin;
 		}
 		
 	}
	 
	/**
	 * Calul de waterStockSenfac
	 */
 	public void computeWaterStockSenfac (double psisto, double psiturg) {			
 		waterStockSenfac = waterStock;
 		
 		//on verifie qu'on ne remonte pas au dessus de FC
 		double waterStockMax =  this.getLayer().getFieldCapacity() * this.getVolume() * 1000;	
 		if (waterStock < waterStockMax) {
 			waterStockSenfac = waterStock + (this.getVolume() * 1d/80d * Math.log(psisto/psiturg) * 1000);
 			if (waterStockSenfac > waterStockMax) waterStockSenfac = waterStockMax;
 		}
 		
 	}
 	
	/**
	 * remet waterStockNormal
	 */
 	public void resetWaterStockNormal () {
 		waterStock = waterStockNormal;
 	}
 	
 	
 	public void resetTreeRootsAgeInWater() {
 		Arrays.fill(this.treeRootsAgeInWater, 0);
 	}

}
