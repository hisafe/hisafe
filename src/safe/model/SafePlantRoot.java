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
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;


/**
 * PLANT (Tree of crop) fine roots
 *
 * @author Degi HARJA          - ICRAF Bogor Indonisia		- July 2004
 * @author Isabelle LECOMTE    - INRA Montpellier France
 *
 **/

public class SafePlantRoot implements   Serializable {

	private static final long serialVersionUID = 1L;

	/**
	 * This class contains immutable instance variables for a SafeFineRoot
	 */
	public static class Immutable implements  Cloneable, Serializable {
		private Object plant;			//Reference on initial SafeTree or SafeCrop object
	}
	protected Immutable immutable;

	//PLANT POTENTIALS
	private double radialTransportPotential;			//Potential gradient  for root radial transport entry (cm)
	private double longitudinalTransportPotential;		//Potential gradient for longitudinal transport (cm)
  	private double requiredWaterPotential;				//Required plant water potential (cm)
	private double actualWaterPotential;				//Required Plant Water Potential with reduced uptake (cm)
	private double waterUptakePotential;				//Potential water uptake in all rooted voxels (cm)
	private double nitrogenUptakePotential;				//Potential nitrogen uptake in all rooted voxels (g)
    private double waterDemandReductionFactor;			//Reduction factor for transpirational demand (dimensionless)
	private double nitrogenSinkStrength;				//dimensionless
	
	//Total root lenght in all rooted voxels (density * voxels volume)  (m)
	private double totalRootsLength;					//m

	private SafeRootNode firstRootNode;		//First node for coarse root topology
	private HashMap<SafeVoxel, SafeRootNode> rootTopology;			//Root topology of RootNode 
	private HashMap<SafeVoxel, SafeRootVoxel> rootedVoxelMap;			//Root topology of RootVoxel
	/**
	 * Create an Immutable object whose class is declared at one level of the hierarchy.
	 * This is called only in constructor for new logical object in superclass.
	 * If an Immutable is declared in subclass, subclass must redefine this method
	 * (same body) to create an Immutable defined in subclass.
	 */
	protected void createImmutable () {immutable = new Immutable ();}

	/**
	* Root constructor
	*/
	public SafePlantRoot (Object plant)  {
		createImmutable ();

		this.immutable.plant = plant;			//tree or crop
		this.totalRootsLength = 0;
		this.rootedVoxelMap = null;
		this.radialTransportPotential = 0;
		this.longitudinalTransportPotential = 0;
		this.requiredWaterPotential = 0;
		this.actualWaterPotential = 0;
		this.waterUptakePotential = 0;
		this.nitrogenUptakePotential = 0;
		this.waterDemandReductionFactor = 0;
   		this.nitrogenSinkStrength = 0;
   		
		this.rootTopology = new LinkedHashMap<SafeVoxel, SafeRootNode> (); // fc+qm-6.8.2014 tracking inconsistency
		this.firstRootNode = null;
		
	}

	

	public void dailyRaz () {
		this.rootedVoxelMap = null;
		this.radialTransportPotential=0;
		this.longitudinalTransportPotential= 0;
	  	this.requiredWaterPotential= 0;
		this.actualWaterPotential= 0;
		this.waterUptakePotential= 0;
		this.nitrogenUptakePotential = 0;
	    this.waterDemandReductionFactor= 0;
   		this.nitrogenSinkStrength = 0;
   		
   		this.rootTopology = new LinkedHashMap<SafeVoxel, SafeRootNode> ();
   		
   		if (firstRootNode != null) {
   			firstRootNode.setWaterUptake (0);
   			firstRootNode.setNitrogenUptake(0);
   			firstRootNode.setFineRootsCost(0);
   			
			cloneNode(this.rootTopology, this.firstRootNode);
			if  (this.firstRootNode.getNodeColonised() != null) {
				this.cloneRootTopology(this.rootTopology, this.firstRootNode.getNodeColonised());
			}

		}
	}
	
	

	/**
	* cloning root topology hash MAP
	*/
  	public void cloneRootTopology (HashMap<SafeVoxel, SafeRootNode> after, Collection<SafeRootNode>  collection) {
		if (collection == null) return;

		for (Iterator <SafeRootNode> v = collection.iterator (); v.hasNext ();) {
			SafeRootNode node =  v.next ();
			cloneNode(after, node);
			cloneRootTopology (after, node.getNodeColonised());
		
		}
  	} 	
    //We have decides that Root node is not cloned
  	//BUT voxel references have changed because of cells cloning
  	public void cloneNode (HashMap<SafeVoxel, SafeRootNode> after, SafeRootNode node) {
		//New voxel reference for the KEY voxel
		SafeVoxel voxelBefore = node.getVoxelRooted ();
		node.setVoxelRooted (voxelBefore);

		node.setWaterUptake (0);
		node.setNitrogenUptake(0);
		node.setFineRootsCost(0);

		
		//ADD the new SET in the HashMap
		after.put (voxelBefore, node);
  	} 
  	
	/**
	* RAZ root topology hash MAP
	*/
  	public void dailyRazRootTopology (HashMap<SafeVoxel, SafeRootNode> topo, Collection<SafeRootNode>  collection) {
		if (collection == null) return;
		for (Iterator <SafeRootNode> v = collection.iterator (); v.hasNext ();) {
			SafeRootNode node =  v.next ();
			node.setWaterUptake (0);
			node.setNitrogenUptake(0);
			node.setFineRootsCost(0);
			dailyRazRootTopology (topo, node.getNodeColonised());
		}
  	} 	

  	
	/**
	* ESTIMATION OF PLANT WATER POTENTIAL (at stem base)
	* On the basis of the various resistances in the catenary process
	*
	* IN  : potentialWaterDemand = Potential amount of water demand by plant per day (l/m^2 or mm)
	*
	*/
	public void calculatePotential (SafeGeneralParameters settings, double potentialWaterDemand) {

		//***************** PLANT SPECIES PARAMETERS *******************************//
		// expected resistance between bulk soil in the voxel and the root surfaces
		
		double campbellFactor = 0;
		double halfCurrWaterPotential = 0;

		//Tree species
  		if (this.getPlant() instanceof SafeTree) {
			  campbellFactor = ((SafeTree) this.getPlant()).getTreeSpecies().getCampbellFactorIcraf();
			  halfCurrWaterPotential =  ((SafeTree) this.getPlant()).getTreeSpecies().getHalfCurrWaterPotentialIcraf();
  		}
  		//Crop species
  		if (this.getPlant() instanceof SafeCrop) {
			  campbellFactor = ((SafeCrop) this.getPlant()).getCell().getCropZone().getCropSpecies().getCampbellFactorIcraf();
			  halfCurrWaterPotential =  ((SafeCrop) this.getPlant()).getCell().getCropZone().getCropSpecies().getHalfCurrWaterPotentialIcraf();
  		}
		
		setWaterDemandReductionFactor (1d / (1d + Math.pow (getRequiredWaterPotential() / halfCurrWaterPotential, campbellFactor)));


		//actualWaterPotential (cm)
		setActualWaterPotential (getRequiredWaterPotential()
								- (1-getWaterDemandReductionFactor ())
								* (getRadialTransportPotential () + getLongitudinalTransportPotential ()));

		setRadialTransportPotential(getRadialTransportPotential()*getWaterDemandReductionFactor());		// gt - 12.11.2009
		setLongitudinalTransportPotential(getLongitudinalTransportPotential()*getWaterDemandReductionFactor());		// gt - 12.11.2009 desactivï¿½	//GT 20/94/2011	rï¿½activï¿½

		return;

	}

	/**
	* CALCULATION OF PLANT WATER ACTUAL UPTAKE
	* as the minimum of demand and total supply
	* and allocate it to voxels on the basis of potential uptake rates and its root density
	*
	* OUT : waterUptakeTotal = Total amount of water uptaken by plant per day (liters)
	*/

	public double calculateWaterUptake (SafeStand stand, Object plant, SafeGeneralParameters settings, double actualWaterDemand,int day) {

		
		double waterUptakeTotal = 0;
		double waterUptakePotentialTotal  	= this.getWaterUptakePotential ();


		if (this.getRootedVoxelMap() == null) return 0;

		Iterator<SafeVoxel> itr = this.getRootedVoxelMap().iterator();
		while (itr.hasNext()) {

			SafeVoxel voxel =  itr.next();
			SafeCell cell =  voxel.getCell();
  			SafeRootVoxel rootVoxel = getRootedVoxelMap (voxel);
  			double waterUptake = 0;
  			double waterUptakePotential = rootVoxel.getWaterUptakePotential();

			//Water uptake potential for this plant in this voxel (liters)
			if (waterUptakePotentialTotal > actualWaterDemand) {
				waterUptake = actualWaterDemand * waterUptakePotential /waterUptakePotentialTotal;
			}
			else {
				waterUptake = waterUptakePotential;
			}

			//cumulation of water uptaken in the voxel for each plant
			if (waterUptake > 0) {

				//rootVoxel.addWaterUptake (waterUptake);
				waterUptakeTotal += waterUptake;

				if (this.getPlant() instanceof SafeCrop) {
					SafeCrop crop = (SafeCrop) (plant) ;
					voxel.addCropWaterUptake (waterUptake);

					if (crop.getPlantRoots().getRootTopology ().containsKey(voxel)) {
						SafeRootNode rootNode = crop.getPlantRoots().getRootTopology (voxel);
						rootNode.addWaterUptake (waterUptake);
					}
					
					if(voxel.getIsSaturated())	// gt - 5.02.2009
						cell.addWaterUptakeInSaturationByCrop(waterUptake/cell.getArea());	//convert liters to mm
				}
				else {
					SafeTree tree = (SafeTree) (plant) ;
					int treeIndex = tree.getId() - 1;
					voxel.addTreeWaterUptake  (treeIndex, waterUptake);
					tree.addWaterUptake (waterUptake);
					
					if (tree.getPlantRoots().getRootTopology().containsKey(voxel)) {
						SafeRootNode node = tree.getPlantRoots().getRootTopology (voxel);
						node.addWaterUptake (waterUptake);
					}
					
					if(voxel.getIsSaturated())	{
						cell.addWaterUptakeInSaturationByTrees(waterUptake/cell.getArea());	//convert liters to mm
						tree.addWaterUptakeInSaturation(waterUptake);	
					}
				}

				
				// gt - 5.02.2009 - waterStock is reduced only if voxel is not saturated 
				//if(!voxel.getIsSaturated()) 	{
				//	voxel.reduceWaterStock  (waterUptake);
				//}
			}
		}
		return waterUptakeTotal;
	}

	/**
	* CALCULATION OF CROP WATER  UPTAKE
	* only to calculate turfac and senfac
	*/
	public double calculateWaterUptake2 (SafeStand stand, Object plant, SafeGeneralParameters settings, double actualWaterDemand,int day) {

		
		double waterUptakeTotal = 0;
		double waterUptakePotentialTotal  	= this.getWaterUptakePotential ();


		if (this.getRootedVoxelMap() == null) return 0;

		Iterator<SafeVoxel> itr = this.getRootedVoxelMap().iterator();
		while (itr.hasNext()) {

			SafeVoxel voxel = itr.next();
  			SafeRootVoxel rootVoxel = getRootedVoxelMap (voxel);
  			double waterUptake = 0;
  			double waterUptakePotential = rootVoxel.getWaterUptakePotential();

			//Water uptake potential for this plant in this voxel (liters)
			if (waterUptakePotentialTotal > actualWaterDemand) {
				waterUptake = actualWaterDemand * waterUptakePotential /waterUptakePotentialTotal;
			}
			else {
				waterUptake = waterUptakePotential;
			}

			//cumulation of water uptaken in the voxel for each plant
			if (waterUptake > 0) {

				//rootVoxel.addWaterUptake (waterUptake);
				waterUptakeTotal += waterUptake;

			}
		}
		return waterUptakeTotal;
	}
	/**
	* CALCULATION OF PLANT NITROGEN SINK STRENGTH
	*
	* OUT : Current demand per unit root lenght (dimensionless, timedependent)
	*/
	public void calculateNitrogenSinkStrength (SafeGeneralParameters settings, double nitrogenDemand) {

		
		double totalRootsLength = this.getTotalRootsLength();		
		
		setNitrogenSinkStrength (nitrogenDemand / totalRootsLength);

	}

	/**
	* CALCULATION OF PLANT NITROGEN ACTUAL UPTAKE
	* as the minimum of demand and total supply
	* and allocate it to voxels on the basis of potential uptake rates and its root density
	*
	* OUT : nitrogenUptakeTotal = Total amount of nitrogen uptaken by plant per day (g)
	*/

	public double calculateNitrogenUptake (SafeStand stand, Object plant, SafeGeneralParameters settings, double nitrogenDemand) {


		double nitrogenUptakeTotal = 0;
		double nitrogenUptakePotentialTotal  	= this.getNitrogenUptakePotential ();	//g


		if (this.getRootedVoxelMap() == null) return 0;

		
		Iterator<SafeVoxel> itr = this.getRootedVoxelMap().iterator();
		while (itr.hasNext()) {

			SafeVoxel voxel = itr.next();
  			SafeRootVoxel rootVoxel = getRootedVoxelMap (voxel);

			
  			double nitrogenUptake = 0;
  			double nitrogenUptakePotential = rootVoxel.getNitrogenUptakePotential();	//g

  			
			//Water uptake potential for this plant in this voxel (liters)
			if (nitrogenUptakePotentialTotal > nitrogenDemand) {
				nitrogenUptake = nitrogenDemand * nitrogenUptakePotential /nitrogenUptakePotentialTotal;
		
				}
			else {
				nitrogenUptake = nitrogenUptakePotential;
			}


			
			//cumulation of nitrogen uptaken in the voxel for each plant
			if (nitrogenUptake > 0) {

				
				//rootVoxel.addNitrogenUptake (nitrogenUptake);
				nitrogenUptakeTotal += nitrogenUptake;
				
				if (this.getPlant() instanceof SafeCrop) {
					SafeCrop crop = (SafeCrop) (plant) ;
					voxel.addCropNitrogenUptake (nitrogenUptake);		//g

					if (crop.getPlantRoots().getRootTopology().containsKey(voxel)) {
						SafeRootNode node = crop.getPlantRoots().getRootTopology (voxel);
						node.addNitrogenUptake (nitrogenUptake/1000); //convert g in kg
					}
	
					if(voxel.getIsSaturated())	// gt - 5.02.2009
						voxel.getCell().addNitrogenUptakeInSaturationByCrop (nitrogenUptake/1000/(voxel.getCell().getArea()/10000));	//convert g in kg ha-1
				}
				else {
					SafeTree tree = (SafeTree) (plant) ;
					int treeIndex = tree.getId() - 1;
					voxel.addTreeNitrogenUptake  (treeIndex, nitrogenUptake);	//g		

					tree.addNitrogenUptake (nitrogenUptake/1000);		//convert g in kg


					if (tree.getPlantRoots().getRootTopology().containsKey(voxel)) {
						SafeRootNode node = tree.getPlantRoots().getRootTopology (voxel);
						node.addNitrogenUptake (nitrogenUptake/1000);	

						
					}

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
	public void addUptakeWaterPotential (double v) {waterUptakePotential +=  v;}
	public void setUptakeWaterPotential (double v) {waterUptakePotential =  v;}
	private void  setNitrogenSinkStrength (double v) {nitrogenSinkStrength =  v;}
	public  void setNitrogenUptakePotential (double v) {nitrogenUptakePotential =  v;}
	public  void addNitrogenUptakePotential (double v) {nitrogenUptakePotential +=  v;}
	public void setWaterDemandReductionFactor (double v) {waterDemandReductionFactor=  v;}


  	public Collection<SafeVoxel> getRootedVoxelMap () {
		if (rootedVoxelMap == null) return null;
		else return rootedVoxelMap.keySet ();
  	}

  	public SafeRootVoxel getRootedVoxelMap (SafeVoxel v) {
		return (SafeRootVoxel) rootedVoxelMap.get (v);
  	}

	//storing water repartition result in rootedVoxelMap
  	public void addRootedVoxelMap (SafeVoxel v, SafeRootVoxel root) {
  		
		if (rootedVoxelMap == null) rootedVoxelMap = new LinkedHashMap<SafeVoxel, SafeRootVoxel>(); // fc+qm-6.8.2014 keep the insertion order (inconsistency tracking)
		rootedVoxelMap.put (v, root);

	}


	/**
	* cloning root topology hash MAP
	*/
  	/*
  	public void cloneRootTopology (HashMap<SafeVoxel, SafeRootNode> after, Collection<SafeRootNode>  collection) {
		if (collection == null) return;
		for (Iterator <SafeRootNode> v = collection.iterator (); v.hasNext ();) {
			SafeRootNode node =  v.next ();
			cloneNode(after, node);
			cloneRootTopology (after, node.getNodeColonised());
		}
  	} 	
    //We have decides that Root node is not cloned
  	//BUT voxel references have changed because of cells cloning
  	public void cloneNode (HashMap<SafeVoxel, SafeRootNode> after, SafeRootNode node) {
		//New voxel reference for the KEY voxel
		SafeVoxel voxelBefore = node.getVoxelRooted ();
		SafeVoxel cloningReference = voxelBefore.getCloningReference();
		node.setVoxelRooted (cloningReference);
		node.setWaterUptake (0);
		node.setNitrogenUptake(0);
		node.setFineRootCost(0);

		
		//ADD the new SET in the HashMap
		after.put (cloningReference, node);
  	} */
  	
  //ROOT TOPOLOGY
  	public HashMap<SafeVoxel, SafeRootNode> getRootTopology() {return rootTopology;}

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
