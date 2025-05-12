/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE - CC-BY License
 * 
 * LIST OF AUTHORS
 * --------------- 
 * Christian Dupraz 1, Kevin J.Wolz 1 , Isabelle Lecomte 1, Grégoire Talbot 1, Nicolas Barbault 1, 
 * Grégoire Vincent 2 , Rachmat Mulia 3, François Bussière 4, Harry Ozier-Lafontaine 4,
 * Sitraka Andrianarisoa 1, Nick Jackson 5, Gerry Lawson 5, Nicolas Dones 6, Hervé Sinoquet 6,
 * Betha Lusiana 3, Degi Harja 3, Suzy Domenicano 7 , Francesco Reyes 1 , Marie Gosme 1 ,
 * Meine Van Noordwijk 3, Benoit Courbaud 8
 *
 * 1 INRA (UMR-ABSYS), University of Montpellier, 34090 Montpellier, France
 * 2 IRD (UMR-AMAP), University of Montpellier, 34090 Montpellier, France
 * 3 ICRAF, Bogor 16001, Indonesia
 * 4 INRA (UR ASTRO 1231) Centre Antilles-Guyane, Petit-Bourg, 97170 Guadeloupe, France
 * 5 CEH, NERC,Wallingford OX10 8BB, UK
 * 6 INRA (UMR-PIAF), Université Clermont Auvergne, 63000 Clermont-Ferrand, France
 * 7 Centre d’étude de la forêt, Université du Quebec, Montreal H2X 3Y5, Canada
 * 8 CEMAGREF, Mountain Ecosystems and Landcapes Research Unit, Saint-Martin-d’Hères, France
 *
 *----------------------------------------------------------------------------------------------
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
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;


/**
 * PLANT (Tree of crop) roots system
 *
 * @author : Degi HARJA       - ICRAF, Bogor 16001, Indonesia
 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
 *
 **/

public class SafePlantRoot implements   Serializable {

	/**
	 * This class contains immutable instance variables for a SafeFineRoot
	 */
	public static class Immutable implements  Cloneable, Serializable {
		/** Reference on plant object (SafeTree or SafeCrop)   */
		private Object plant;			
	}
	protected Immutable immutable;

	/** Potential gradient  for root radial transport entry (cm)   */
	private double radialTransportPotential;			
	/** Potential gradient for longitudinal transport (cm)  */
	private double longitudinalTransportPotential;	
	/** Required plant water potential (cm)   */
	private double requiredWaterPotential;				
	/** Required Plant Water Potential with reduced uptake (cm)  */
	private double actualWaterPotential;				
	/** Potential water uptake in all rooted voxels (cm)  */
	private double waterUptakePotential;			
	/** Potential nitrogen uptake in all rooted voxels (g)  */
	private double nitrogenUptakePotential;				
	/** Reduction factor for transpiration demand (dimensionless)   */
	private double waterDemandReductionFactor;		
	/** Plant root nitrogen sink strength (kg N m-1)  */
	private double nitrogenSinkStrength;				//
	/** Total root length in all rooted voxels (density * voxels volume)  (m) */
	private double totalRootsLength;				
	/** First node for coarse root topology */
	private SafeRootNode firstRootNode;		//
	/** Root topology of RootNode */
	private HashMap<SafeVoxel, SafeRootNode> rootTopology;		
		
	/**
	 * Create an Immutable object whose class is declared at one level of the hierarchy.
	 * This is called only in constructor for new logical object in superclass.
	 * If an Immutable is declared in subclass, subclass must redefine this method
	 * (same body) to create an Immutable defined in subclass.
	 */
	protected void createImmutable () {immutable = new Immutable ();}

	/**
	* Root constructor
	* @param plant Reference on the plant object (SafeTree or SafeCrop) 
	*/
	public SafePlantRoot (Object plant)  {
		createImmutable ();

		this.immutable.plant = plant;			
		this.totalRootsLength = 0;
		this.radialTransportPotential = 0;
		this.longitudinalTransportPotential = 0;
		this.requiredWaterPotential = 0;
		this.actualWaterPotential = 0;
		this.waterUptakePotential = 0;
		this.nitrogenUptakePotential = 0;
		this.waterDemandReductionFactor = 0;
   		this.nitrogenSinkStrength = 0;
   		
   		//create empty root topology
		this.rootTopology = new LinkedHashMap<SafeVoxel, SafeRootNode> (); 
		this.firstRootNode = null;
	}

	/**
	* RAZ of daily values
	*/
	public void razDaily () {
		
		this.radialTransportPotential=0;
		this.longitudinalTransportPotential= 0;
	  	this.requiredWaterPotential= 0;
		this.actualWaterPotential= 0;
		this.waterUptakePotential= 0;
		this.nitrogenUptakePotential = 0;
	    this.waterDemandReductionFactor= 0;
   		this.nitrogenSinkStrength = 0;
   		
   		//clone root toology
   		this.rootTopology = new LinkedHashMap<SafeVoxel, SafeRootNode> ();
   		
   		if (firstRootNode != null) {
   			firstRootNode.setWaterUptake (0);
   			firstRootNode.setNitrogenUptake(0);
   			firstRootNode.setFineRootsCost(0);
   			
			cloneNode(this.firstRootNode, this.rootTopology);
			if  (this.firstRootNode.getNodeColonised() != null) {
				this.cloneRootTopology(this.firstRootNode.getNodeColonised(), this.rootTopology);
			}
		}
	}

	/**
	* cloning root topology hash MAP
	 * @param collection The collection of SafeRootNode original
	 * @param after The collection of SafeRootNode cloned
	*/
  	public void cloneRootTopology (Collection<SafeRootNode>  collection, HashMap<SafeVoxel, SafeRootNode> after) {
		if (collection == null) return;

		for (Iterator <SafeRootNode> v = collection.iterator (); v.hasNext ();) {
			SafeRootNode node =  v.next ();
			cloneNode(node, after);
			cloneRootTopology (node.getNodeColonised(), after);
		}
  	} 	

	/**
	* Cloning root topology node : SafeRootNode is not cloned BUT voxel references have changed because of SafeVoxel cloning
	 * @param collection The collection of SafeRootNode original
	 * @param after The collection of SafeRootNode cloned
	*/
  	public void cloneNode (SafeRootNode node, HashMap<SafeVoxel, SafeRootNode> after) {

		SafeVoxel voxelBefore = node.getVoxelRooted ();
		node.setVoxelRooted (voxelBefore);

		node.setWaterUptake (0);
		node.setNitrogenUptake(0);
		node.setFineRootsCost(0);

		
		//ADD the new SET in the HashMap
		after.put (voxelBefore, node);
  	} 

	/**
	* Calculation of plant water potentiel at stem base
	* On the basis of the various resistances in the catenary process
	* @param potentialWaterDemand Potential amount of water demand by plant per day (l*m-2 or mm)
	* @param campbellFactor Campbell factor (dimensionless) 
	* @param halfCurrWaterPotential Water potential where tranpiration demand is half of its potential (mm)
	*/
	public void calculatePotential (double potentialWaterDemand, double campbellFactor, double halfCurrWaterPotential) {

		setWaterDemandReductionFactor (1d / (1d + Math.pow (getRequiredWaterPotential() / halfCurrWaterPotential, campbellFactor)));

		setActualWaterPotential (getRequiredWaterPotential()
								- (1-getWaterDemandReductionFactor ())
								* (getRadialTransportPotential () + getLongitudinalTransportPotential ()));


		setRadialTransportPotential(getRadialTransportPotential()*getWaterDemandReductionFactor());		
		setLongitudinalTransportPotential(getLongitudinalTransportPotential()*getWaterDemandReductionFactor());
		
		return;

	}


	/**
	* Calculation of plant water uptake and allocate it to voxels on the basis of potential uptake rates and roots density
	* On the basis of the various resistances in the catenary process
	* @param stand Reference to the SafeStand object
	* @param plant Reference to the plant object (SafeTree or SafeCrop)
	* @param actualWaterDemand Water demand (mm)
	* @param update 0=no object update just return water uptake value 1=Update object with water uptake  
	* @return water uptake (mm)
	*/
	public double calculateWaterUptake (SafeStand stand, Object plant, double actualWaterDemand, boolean update) {

		double waterUptakeTotal = 0;
		double waterUptakePotentialTotal  	= this.getWaterUptakePotential ();

		if (this.getRootTopology() == null) return 0;

		Iterator<SafeVoxel> itr = this.getRootTopology().iterator();
		
		while (itr.hasNext()) {

			SafeVoxel voxel =  itr.next();
			
  			SafeRootNode rootNode = getRootTopology (voxel);
  			double waterUptake = 0;
  			double waterUptakePotential = rootNode.getWaterUptakePotential();

			//Water uptake potential for this plant in this voxel (liters)
			if (waterUptakePotentialTotal > actualWaterDemand) {
				waterUptake = actualWaterDemand * waterUptakePotential /waterUptakePotentialTotal;
			}
			else {
				waterUptake = waterUptakePotential;
			}


			
			//cumulation of water uptaken in the voxel for each plant
			if (waterUptake > 0) {

				waterUptakeTotal += waterUptake;

				if (update) {
					
					SafeCell cell =  voxel.getCell();
					if (this.getPlant() instanceof SafeCrop) {
						SafeCrop crop = (SafeCrop) (plant) ;
						voxel.addCropWaterUptake (waterUptake);
						rootNode.addWaterUptake (waterUptake);

						if(voxel.getIsSaturated())	// gt - 5.02.2009
							cell.addWaterUptakeInSaturationByCrop(waterUptake/cell.getArea());	//convert liters to mm
					}
					else {
						

						SafeTree tree = (SafeTree) (plant) ;
						int treeIndex = tree.getId() - 1;
						voxel.addTreeWaterUptake  (treeIndex, waterUptake);
						rootNode.addWaterUptake (waterUptake);

						if(voxel.getIsSaturated())	{
							cell.addWaterUptakeInSaturationByTrees(waterUptake/cell.getArea());	//convert liters to mm
							tree.addWaterUptakeInSaturation(waterUptake);	
						}
					}
				}
				
			}
		}
		return waterUptakeTotal;
	}

	
	/**
	* Return nitrogen sink strength (kg N m-1) 
	* 
	* @param nitrogenDemand Nitrogen demand (kg N) 
	*/
	public void calculateNitrogenSinkStrength (double nitrogenDemand) {

		setNitrogenSinkStrength (nitrogenDemand / this.getTotalRootsLength());

	}

	/**
	* Calculation of plant nitrogen uptake and allocate it to voxels on the basis of potential uptake rates and roots density
	* On the basis of the various resistances in the catenary process
	* @param stand Reference to the SafeStand object
	* @param plant Reference to the plant object (SafeTree or SafeCrop)
	* @param nitrogenDemand Nitrogen demand (g N)
	* @return Nitrogen uptake (g N)
	*/

	public double calculateNitrogenUptake (SafeStand stand, Object plant, double nitrogenDemand) {


		double nitrogenUptakeTotal = 0;
		double nitrogenUptakePotentialTotal  	= this.getNitrogenUptakePotential ();	//g

		if (this.getRootTopology() == null) return 0;

		Iterator<SafeVoxel> itr = this.getRootTopology().iterator();

		while (itr.hasNext()) {

			SafeVoxel voxel = itr.next();
			SafeRootNode rootNode = getRootTopology (voxel);

  			double nitrogenUptake = 0;
  			double nitrogenUptakePotential = rootNode.getNitrogenUptakePotential();	//g
 
			if (nitrogenUptakePotentialTotal > nitrogenDemand) {
				nitrogenUptake = nitrogenDemand * nitrogenUptakePotential /nitrogenUptakePotentialTotal;
			}
			else {
				nitrogenUptake = nitrogenUptakePotential;
			}

			//cumulation of nitrogen uptaken in the voxel for each plant
			if (nitrogenUptake > 0) {

				nitrogenUptakeTotal += nitrogenUptake/1000; //convert g in kg
				
				if (this.getPlant() instanceof SafeCrop) {
					SafeCrop crop = (SafeCrop) (plant) ;
					voxel.addCropNitrogenUptake (nitrogenUptake);		//g
					rootNode.addNitrogenUptake (nitrogenUptake); 		//g

					if(voxel.getIsSaturated())	// gt - 5.02.2009
						voxel.getCell().addNitrogenUptakeInSaturationByCrop (nitrogenUptake/1000/(voxel.getCell().getArea()/10000));	//convert g in kg ha-1
				}
				else {	 	
					
					SafeTree tree = (SafeTree) (plant) ;
					int treeIndex = tree.getId() - 1;
					voxel.addTreeNitrogenUptake  (treeIndex, nitrogenUptake);	//g		
					rootNode.addNitrogenUptake (nitrogenUptake);				//g
		
					if(voxel.getIsSaturated())	{
						voxel.getCell().addNitrogenUptakeInSaturationByTrees (nitrogenUptake/1000/(voxel.getCell().getArea()/10000));	//convert g in kg ha-1
						tree.addNitrogenUptakeInSaturation (nitrogenUptake/1000);	//convert g in kg
					}
				}
			}
		}

		return nitrogenUptakeTotal;
	}
	

	//ACCESSORS FOR PLANT WATER POTENTIALS
	public Object getPlant () {return immutable.plant;}
	public String getPlantName () {
		String ret = "";
		if (this.getPlant() instanceof SafeCrop) {
			SafeCrop crop = (SafeCrop) (getPlant ()) ;
			ret=crop.getCell().getId()+" "+crop.getCell().getCropZone().getCropSpecies().getName();
			
		}
		if (this.getPlant() instanceof SafeTree) {
			SafeTree tree = (SafeTree) (getPlant ()) ;
			ret=tree.getId()+" "+tree.getTreeSpecies().getName();
			
		}
		return ret;
	}
	public double getTotalRootsLength () {return totalRootsLength;}
	public double getRadialTransportPotential () {return radialTransportPotential;}
	public double getLongitudinalTransportPotential () {return longitudinalTransportPotential;}
	public double getRequiredWaterPotential () {return requiredWaterPotential;}
	public double getActualWaterPotential () {return actualWaterPotential;}
	public double getWaterUptakePotential () {return waterUptakePotential;}
	public double getWaterDemandReductionFactor () {return waterDemandReductionFactor;}
	public double getNitrogenSinkStrength () {return nitrogenSinkStrength;}
	public double getNitrogenUptakePotential () {return nitrogenUptakePotential;}

	
	public void setTotalRootsLength (double v) {totalRootsLength =  v;}
	public void setRadialTransportPotential (double v) {radialTransportPotential =  v;}
	public void setLongitudinalTransportPotential (double v) {longitudinalTransportPotential =  v;}
	public void setRequiredWaterPotential (double v) {requiredWaterPotential =  v;}
	public void setActualWaterPotential (double v) {actualWaterPotential =  v;}
	public void addWaterUptakePotential (double v) {waterUptakePotential +=  v;}
	public void setWaterUptakePotential (double v) {waterUptakePotential =  v;}
	private void  setNitrogenSinkStrength (double v) {nitrogenSinkStrength =  v;}
	public  void setNitrogenUptakePotential (double v) {nitrogenUptakePotential =  v;}
	public  void addNitrogenUptakePotential (double v) {nitrogenUptakePotential +=  v;}
	public void setWaterDemandReductionFactor (double v) {waterDemandReductionFactor=  v;}

  //ROOT TOPOLOGY
  	public Collection<SafeVoxel> getRootTopology () {
		if (rootTopology == null) return null;
		else return rootTopology.keySet ();
  	}
  	
  	public SafeRootNode getRootTopology (SafeVoxel voxel) {
  		return (SafeRootNode) (rootTopology.get (voxel));}

  	public SafeRootNode getFirstRootNode() {return firstRootNode;}
  	public void setFirstRootNode(SafeRootNode node) {firstRootNode = node;}

	//Set density of an existing fine root
	public void setFineRootTopology (SafeVoxel voxel, double density) {
		SafeRootNode node = (SafeRootNode) (rootTopology.get (voxel));
		node.setFineRootsDensity (density);
	}
	
	//Add a new TREE root in the root topology 
	public void addTreeRootTopology (SafeTree tree, SafeVoxel voxel, SafeVoxel parentVoxel, int day, double fineRootDensity, int direction) {
		if (rootTopology == null) rootTopology = new LinkedHashMap<SafeVoxel, SafeRootNode> ();
		SafeRootNode parentNode = null;
		if (parentVoxel != null)
			parentNode = getRootTopology (parentVoxel);
		
		SafeRootNode node = new SafeRootNode (this, voxel, parentNode, day, fineRootDensity, tree, direction);
		rootTopology.put (voxel, node);
  		//no parent = this node is the first one
  		if (parentVoxel == null)
  			this.firstRootNode = node;
	}
	
	//Add a new CROP root in the root topology 
	public void addCropRootTopology (SafeVoxel voxel, SafeVoxel parentVoxel, int day, double fineRootDensity) {
		if (rootTopology == null) rootTopology = new LinkedHashMap<SafeVoxel, SafeRootNode> ();
		SafeRootNode parentNode = null;
		if (parentVoxel != null)
			parentNode = getRootTopology (parentVoxel);
		
		SafeRootNode node = new SafeRootNode (this, voxel, parentNode, day, fineRootDensity);
		rootTopology.put (voxel, node);
  		//no parent = this node is the first one
  		if (parentVoxel == null)
  			this.firstRootNode = node;
	}
	
  	//Add a new  root in a parent voxel (direction = 4 always from the top) 
  	public void addEmptyRootTopology (SafeVoxel voxel) {
		if (rootTopology == null) rootTopology = new LinkedHashMap<SafeVoxel, SafeRootNode> ();
  		rootTopology.put (voxel, new SafeRootNode (this, voxel, null, 0, 0, null, 4));
  	}
}
