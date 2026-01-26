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
import java.util.Iterator;
import java.math.BigDecimal;
import jeeb.lib.util.Vertex3d;

/**
 * NODE to build tree and crop coarse roots topology
 * Each node is liked to its parent node and to its colonized nodes to design a tree-like topology
 * 
 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
 * @author : Grégoire Talbot  - INRAE (UMR-SYSTEM), University of Montpellier, France
 */
public class SafeRootNode implements Serializable, Comparable {

	/** Plant type 1=crop 2=tree  */
	private int planteType;				
	/** Reference to the SafePlantRoot object  */
	private SafePlantRoot plantRoots;		
	/** Reference to the SafeVoxel object  */
	private SafeVoxel voxelRooted; 	
	/** Reference of the single parent node SafeRootNode object  */
	private SafeRootNode nodeParent; 	
	/** Collection of colonized nodes */
	private Collection<SafeRootNode> nodeColonised; 		
	/** Date of first colonization */
	private int colonisationDate; 			
	/** Direction of the colonization from father node ( 0 is for x+, 1 for x-, 2 for y+, 3 for y-, 4 for z+, 5 for z-) */
	private int colonisationDirection; 	
	/** Distance to the tree trunk following topology path (m) */									
	private float treeDistance; 		
	/** Distance from father node (m) */	
	private float fatherDistance; 			
	/** Effective distance from tree (m)  */	
	private float effectiveDistance;		
	/** Plant fine roots density (m m-3)  */
	private double fineRootsDensity; 		

	private double fineRootsTotalInvestment;
	/** Plant fine roots cost (kg of CoarseRoot m-1 of fine roots)  */
	private double fineRootsCost; 
	/** Plant phi PF (cm2 day-1)  */
	private double phiPf;					
	/** Plant water rhizosphere potential (cm)  */
	private double waterRhizospherePotential; 
	/** Plant water uptake potential (liters)  */
	private double waterUptakePotential;	
	/** Plant water uptake (liters)  */
	private double waterUptake; 

	private double nitrogenShareUptake;			//ND
	/** Potential zero-sink supply per voxel (g N) */
	private double nitrogenZeroSinkPotential;
	/** Plant nitrogen uptake potential (g N)  */
	private double nitrogenUptakePotential;		
	/** Plant nitrogen uptake (g N)  */
	private double nitrogenUptake; 	
	
	/** Distance from first node in X+ direction */
	private double distanceX0;
	/** Distance from first node in X- direction */
	private double distanceX1;
	/** Distance from first node in Y+ direction */
	private double distanceY2;
	/** Distance from first node in Y- direction */
	private double distanceY3;

	/**
	 * Constructor for TREE
     * @param root Reference to the plant SafePlantRoot object
 	 * @param voxelRooted Reference to the rooted SafeVoxel object
     * @param nodeParent Reference to the SafeRootNode parent object 
     * @param tree Reference to the SafeTree object
     * @param fineRoots Fine root density when the node is colonized
 	 * @param day Day of the node colonization	
 	 * @param direction Colonization direction	
	 */
	public SafeRootNode (SafePlantRoot root, 
						 SafeVoxel voxelRooted, 
						 SafeRootNode nodeParent, 
						 SafeTree tree,
						 double fineRoots,
						 int day, 
						 int direction) {

		this.planteType = 2; 		//2=tree
		this.plantRoots = root;
		this.voxelRooted = voxelRooted;
		this.nodeParent = nodeParent;
		this.colonisationDate = day;
		this.fineRootsDensity = fineRoots;
		this.fineRootsTotalInvestment =  fineRoots;
		this.nodeColonised = null;
		this.waterUptake = 0;
		this.nitrogenUptake = 0;
		this.fineRootsCost = 0;
		this.waterUptakePotential = 0;
		this.nitrogenShareUptake = 0;
		this.nitrogenZeroSinkPotential  = 0;
		this.nitrogenUptakePotential = 0;

		// linking father and son
		if (nodeParent != null) {
			nodeParent.addNodeColonised (this);
			computeDistanceFromFather (direction);
			setTreeDistance (nodeParent.getTreeDistance () + getFatherDistance());
			this.computeEffDist (tree);
			distanceX0 = nodeParent.getDistanceX0();
			distanceX1 = nodeParent.getDistanceX1();
			distanceY2 = nodeParent.getDistanceY2();
			distanceY3 = nodeParent.getDistanceY3();
			
			if (direction==0)  distanceX0 +=  voxelRooted.getCell().getWidth();	
			if (direction==1)  distanceX1 +=  voxelRooted.getCell().getWidth();
			if (direction==2)  distanceY2 +=  voxelRooted.getCell().getWidth();
			if (direction==3)  distanceY3 +=  voxelRooted.getCell().getWidth();

		//first node
		} else {
			setFatherDistance (0.5 * this.voxelRooted.getThickness ());
			setTreeDistance (0.5 * this.voxelRooted.getThickness ());
			setEffectiveDistance (0.5 * this.voxelRooted.getThickness ());
			this.colonisationDirection = 4; // gt - 27.03.2009
			distanceX0 = voxelRooted.getCell().getWidth() / 2;
			distanceX1 = voxelRooted.getCell().getWidth() / 2;
			distanceY2 = voxelRooted.getCell().getWidth() / 2;
			distanceY3 = voxelRooted.getCell().getWidth() / 2;

		}
		// init plant waterRhizospherePotential with soil water potential
		setWaterRhizospherePotential (voxelRooted.getWaterPotentialTheta ());
	}


	/**
	 * Constructor for CROP
     * @param root Reference to the plant SafePlantRoot object
 	 * @param voxelRooted Reference to the rooted SafeVoxel object
     * @param nodeParent Reference to the SafeRootNode parent object 
     * @param fineRootDensity Fine root density when the node is colonized
 	 * @param day Day of the node colonization	
	 */
	public SafeRootNode (SafePlantRoot root, 
						 SafeVoxel voxelRooted, 
						 SafeRootNode nodeParent, 
						 double fineRootDensity, 
						 int day) {

		this.planteType = 1;	//Crop
		this.plantRoots = root;
		this.colonisationDirection = 4; 
		this.voxelRooted = voxelRooted;
		this.nodeParent = nodeParent;
		this.colonisationDate = day;
		this.fineRootsDensity = fineRootDensity;
		this.fineRootsTotalInvestment =  fineRootDensity;
		this.nodeColonised = null;
		this.waterUptake = 0;
		this.nitrogenUptake = 0;
		this.fineRootsCost = 0;
		
		// linking father and son
		if (nodeParent != null) nodeParent.addNodeColonised (this);
		
		setFatherDistance (0.5 * this.voxelRooted.getThickness ());
		setTreeDistance (0.5 * this.voxelRooted.getThickness ());
		setEffectiveDistance (0.5 * this.voxelRooted.getThickness ());
		
		// init plant waterRhizospherePotential with soil water potential
		setWaterRhizospherePotential (voxelRooted.getWaterPotentialTheta ());
	}

	/**
	 * Compute the distance from the father node 
	 * @author Gregoire Talbot. September 2008
 	 * @param direction Colonization direction	
	 */
	public void computeDistanceFromFather (int direction) {

		this.colonisationDirection = direction;
		if (direction >= 4) {
			Vertex3d node = this.getVoxelRooted ().getGravityCenter ();
			Vertex3d father = this.getNodeParent ().getVoxelRooted ().getGravityCenter ();
			setFatherDistance (Math.abs (node.z - father.z));
		}
		else {
			setFatherDistance (this.getVoxelRooted ().getCell().getWidth());
		}
	}

	/**
	 * Compute node effective distance from tree
	 * if the topological distance to the tree (treeDistance)  is as short as possible (shorterWay), 
	 * in other words, if there were no "bypass" for the colonization of the voxel, we take the Euclidean distance to the tree
	 * otherwise, we search in the genealogy of the present node the most distant node N that we can reach without making a bypass 
	 * (that is to say that the distance to this node is the shortest possible). 
	 * And in this case, the effective distance is the sum of the Euclidean distance between the present node and the node N, 
	 * and the effective distance from node N to the tree 
	 * 
	 * @author Gregoire Talbot. September 2008
	 * @param tree Reference to the SafeTree object
	 */
	public void computeEffDist (SafeTree tree) {
		if ((tree != null) && (getNodeParent () != null)) {
			if (this.getTreeDistance () <= this.shorterWay (tree.getX (), tree.getY (), tree.getZ ())) {
				this.setEffectiveDistance (this.euclidist (tree.getX (), tree.getY (), tree.getZ ()));
				
			} else {

				SafeRootNode parent = getNodeParent ();
				if (parent.getNodeParent () != null) {
					Vertex3d parentFatherPosition = parent.getNodeParent ().getVoxelRooted ().getGravityCenter ();
					while ((this.getTreeDistance () - parent.getNodeParent ().getTreeDistance ()) <= this
							.shorterWay (parentFatherPosition.x, parentFatherPosition.y, parentFatherPosition.z)
							&& (parent.getNodeParent ().getNodeParent () != null)) {
						parent = parent.getNodeParent ();
						parentFatherPosition = parent.getNodeParent ().getVoxelRooted ().getGravityCenter ();
					}
				}
				Vertex3d parentPosition = parent.getVoxelRooted ().getGravityCenter ();

				this.setEffectiveDistance (parent.getEffectiveDistance ()
						+ euclidist (parentPosition.x, parentPosition.y, parentPosition.z));
			}
		} else {
			this.setEffectiveDistance (this.getTreeDistance ());	// cas du firstRootNode, situe juste sous l'arbre

		}
	}
	/**
	 * Calculate shorter way between this node and a distant node
	 * 
	 * @author Gregoire Talbot. September 2008
	 * @param x X coordinate of the distant node 
	 * @param y Y coordinate of the distant node 
	 * @param z Z coordinate of the distant node 
	 */
	public double shorterWay (double x, double y, double z) {
		SafeCell cell = (SafeCell) this.getVoxelRooted ().getCell();
		SafePlot plot = (SafePlot) cell.getPlot();
		double plotWidth = plot.getXSize();
		double plotHeight = plot.getYSize();
		Vertex3d nodePosition = this.getVoxelRooted ().getGravityCenter ();
		double nodex = nodePosition.x;
		double nodey = nodePosition.y;

		//correction for toric symetry (IL 16/05/2025)
		double distX = Math.abs (x - nodex) ;
		double distY = Math.abs (y - nodey) ;
		distX = Math.min(distX, Math.abs(distX - plotWidth));
		distY = Math.min(distY, Math.abs(distY - plotHeight));

		double dist = distX + distY + Math.abs (z - nodePosition.z);
		return dist;
	}
	/**
	 * Calculate euclidean distance this node and a distant node
	 * 
	 * @author Gregoire Talbot. September 2008
	 * @param x X coordinate of the distant node 
	 * @param y Y coordinate of the distant node 
	 * @param z Z coordinate of the distant node 
	 */
	public double euclidist (double x, double y, double z) {
		SafeCell cell = (SafeCell) this.getVoxelRooted ().getCell();
		SafePlot plot = (SafePlot) cell.getPlot();
		double plotWidth = plot.getXSize();
		double plotHeight = plot.getYSize();
		Vertex3d nodePosition = this.getVoxelRooted ().getGravityCenter ();

		double nodex = nodePosition.x;
		double nodey = nodePosition.y;

		//correction for toric symetry (IL 16/05/2025)
		double distX = Math.abs (x - nodex) ;
		double distY = Math.abs (y - nodey) ;
		distX = Math.min(distX, Math.abs(distX - plotWidth));
		distY = Math.min(distY, Math.abs(distY - plotHeight));
		
		
		double dist = Math.sqrt (Math.pow (distX, 2) + Math.pow (distY, 2)
				+ Math.pow (z - nodePosition.z, 2));
		return dist;
	}

	/**
	 * Set the node distance with a tree origin
	 * 
	 * @author Gregoire Talbot. September 2008
     * @param d distance (m) 
	 */
	public void setDistance (double d) {
		double distance = d + getFatherDistance ();
		setTreeDistance (distance);
		// here recursitity stops because there is no more colonised voxels bellow
		if (nodeColonised != null) {
			for (Iterator<SafeRootNode> v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised = v.next ();
				nodeColonised.setDistance (distance); // recursivity
			}
		}
	}

	/**
	 * Compute the target coarse root carbon for all nodes and the total coarse root target carbon of the tree 
	 * 
	 *  @author Gregoire Talbot. September 2008
	 *  @param tree Reference to the SafeTree object 
	 *  @param additionalRootLength Table of additional root length (m) by voxels
	 *  @param woodDensity Average branch and stem density (kg m-3)
	 *  @param woodCarbonContent Proportion of C in wood dry yield %
	 *  @param cRAreaToFRLengthRatio  Coarse root area to fine root length ratio 
	 */
	public double computeCarbonCoarseRootsTarget (SafeTree tree, 
												double[] additionalRootLength, 
												double woodDensity,
												double woodCarbonContent, 
												double cRAreaToFRLengthRatio) {


		double fineRootsLength = 0; // Kg C
		
		//loop on all colonised voxels to get the fineRootLenght total to the end of the coarseRoot
		if (nodeColonised != null) {
			for (Iterator<SafeRootNode> v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised =  v.next ();
				double toAdd = nodeColonised
						.computeCarbonCoarseRootsTarget (tree, additionalRootLength, woodDensity, woodCarbonContent, cRAreaToFRLengthRatio);
				fineRootsLength += toAdd;
			}
		}
		int v = this.getVoxelRooted ().getId () - 1;
		int treeIndex = tree.getId () - 1;
		double fineRootDensity = this.getVoxelRooted ().getTheTreeRootsDensity (treeIndex);
		fineRootsLength += fineRootDensity * this.getVoxelRooted ().getVolume () // m m-3 * m3
							* this.getTopologicalToEffDistance (); 				// voir les explication la dessus dans la methode computeEffDist
																				// - 30.03.2009

		if (additionalRootLength != null) {
			fineRootsLength += additionalRootLength[v] // m
							* this.getTopologicalToEffDistance ();
		}

		double carbonTarget =  (fineRootsLength * cRAreaToFRLengthRatio * this.getFatherDistance () // m x m2 m-1 x m = m3
								      * woodDensity * woodCarbonContent); // kg.m-3 x kgC.kg-1 = kgC.m-3

		
		//We remove the voxels with enough carbon in coarse root (negative imbalance)  
		//https://github.com/hisafe/hisafe/issues/123
		// Math.max(...) : if target< carbon : the coarse root can't contribute to carbon allocation as a negative sink
		carbonTarget = Math.max(carbonTarget, voxelRooted.getTheTreeCarbonCoarseRootsTarget(treeIndex));

		voxelRooted.setTreeCarbonCoarseRootsTarget (treeIndex, carbonTarget);

		tree.addTargetCarbonCoarseRoots (carbonTarget);

		return fineRootsLength;
	}
	/**
	 * Growth of carbon coarse root  in KG C : computing of carbon allocation between coarse roots
	 * 
	 * @author Gregoire Talbot. September 2008
	 * @param tree Reference to the SafeTree object 
	 */
	public double computeCarbonCoarseRootsImbalance (SafeTree tree) {

		double nodeCoarseRootsImbalance = Math.max(voxelRooted.getTreeCarbonCoarseRootsImbalance(tree.getId()-1), 0);	

		if (nodeColonised != null) {
			for (Iterator<SafeRootNode>  v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised = v.next ();
				nodeCoarseRootsImbalance += nodeColonised.computeCarbonCoarseRootsImbalance (tree);
			}
		}

		return nodeCoarseRootsImbalance;

	}
	/**
	 * Growth of carbon coarse root  in KG C : computing of carbon allocation between coarse roots
	 * 
	 * @author Gregoire Talbot. September 2008
	 * @param tree Reference to the SafeTree object 
	 * @param carbonCoarseRootsIncrement Carbon coarse roots increment (kg C)
	 * @param nitrogenCoarseRootsIncrement Nitrogen coarse roots increment (kg N)
	 * @param totalNodeImbalance 
	 */
	public void computeCarbonCoarseRoots (SafeTree tree,
											double carbonCoarseRootsIncrement,
											double nitrogenCoarseRootsIncrement,
											double totalNodeImbalance) {

		double proportion = 0;
		double nodeCoarseRootImbalance = voxelRooted.getTreeCarbonCoarseRootsImbalance(tree.getId()-1);	

		int treeIndex = tree.getId () - 1;
		//loop on all colonised voxels to add carbon all along the coarseRoot

		if (nodeColonised != null) {
			for (Iterator<SafeRootNode>  v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised = v.next ();
				nodeColonised
						.computeCarbonCoarseRoots (tree, carbonCoarseRootsIncrement,nitrogenCoarseRootsIncrement,totalNodeImbalance);
			}
		}

		if (nodeCoarseRootImbalance > 0) {

			proportion = nodeCoarseRootImbalance / totalNodeImbalance;
			double additionalCarbonCoarseRoot =  (proportion * carbonCoarseRootsIncrement);

			//IL 13/04/2022 this test is added to avoid negative values for additionalCarbonCoarseRoot
			//but maybe carbon balance is wrong ? 
			//https://github.com/hisafe/hisafe/issues/123
			if (additionalCarbonCoarseRoot > 0) {
				//addCarbonCoarseRoot (additionalCoarseRoot);
				// update coarse root  in rooted voxels
				//Coarse roots link two voxels. 
				//If we assume that they join the centers of gravity of the two voxels, this "virtual cylinder" of coarse roots is physically 
				// half in the voxel and half in the father voxel. Therefore the C allocation needs to be split in two between the two voxels.
				//https://github.com/hisafe/hisafe/issues/121			
				if (nodeParent != null) {
					voxelRooted.addTreeCarbonCoarseRoots (treeIndex, (additionalCarbonCoarseRoot/2));		
					nodeParent.getVoxelRooted ().addTreeCarbonCoarseRoots (treeIndex, (additionalCarbonCoarseRoot/2));				
				} else {
					voxelRooted.addTreeCarbonCoarseRoots (treeIndex, additionalCarbonCoarseRoot);
				}
			}
			
			
			//IL 11/09/2024 : SAME for NITROGEN CR
			double additionalNitrogenCoarseRoot =  (proportion * nitrogenCoarseRootsIncrement);
			if (additionalNitrogenCoarseRoot > 0) {
				if (nodeParent != null) {
					voxelRooted.addTreeNitrogenCoarseRoots (treeIndex, (additionalNitrogenCoarseRoot/2));			
					nodeParent.getVoxelRooted ().addTreeNitrogenCoarseRoots (treeIndex, (additionalNitrogenCoarseRoot/2));					
				} else {
					voxelRooted.addTreeNitrogenCoarseRoots (treeIndex, additionalNitrogenCoarseRoot);
				}
			}
		}

		return;
	}
	/**
	 * Method for calculating the carbon price (Kg) of establishing new fine roots (in m) in a given voxel
	 * This method is first called on firstRootNode in safeTree, 
	 * but it is called recursively to do the calculation on the entire root system
	 * frCost input corresponds to the carbon cost of the new fine roots not to mention the potential need for investment in structural roots
	 * the rest of the method calculates the investment needed in structural roots to put these fine roots in place
	 * for this calculation, we will assume that the new fine roots (addFineRootLength) will be distributed uniformly 
	 * over the entire volume rooted by the tree (rootedVolume)
	 * 
	 * @author Gregoire Talbot. September 2008
	 * @param treeIndex index odf the tree on the plot 
	 * @param addFineRootLength length of fine roots added (m)
	 * @param rootedVolume  (m3)
	 * @param woodDensity Wood density tree species parameter 
	 * @param woodCarbonContent Wood carbon content tree species parameter 
	 * @param cRAreaToFRLengthRatio Coarse root area to fine root length ratio  tree species parameter 
	 * @param frCost Fine roots cost 
	 */
	public double[] computeFineRootsCost (int treeIndex, 
										  double addFineRootLength, 
										  double rootedVolume,
										  double woodDensity, 
										  double woodCarbonContent, 
										  double cRAreaToFRLengthRatio, 
										  double frCost) {

		//Cost of fine roots 
		//This cost will then be increased to take into account the investment they imply in terms of structural roots.
		//by the costDispatching method
		addFineRootsCost (frCost);

		double[] fineRootLength = {0.0, 0.0}; // old fine root length and additionalFineRootLength //  ces variables ne sont pas des longueurs de racines locales dans le voxel ou l'on se trouve, mais la somme des longueurs de racines fines qui en dependent, il faut pour cela ajouter les longueurs de racines dans tous les voxels colonises par la descendance du noeud present, d'ou la recursivite de la methode
		double newFineRootsDensity = addFineRootLength / rootedVolume;		// densite de nouvelles racines fines dans le voxel, sous l'hypothese que les nouvelles racines fines seront reparties uniformement sur tout le volume enracine par l'arbre (rootedVolume)

		if (nodeColonised != null) {
			for (Iterator<SafeRootNode>  v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised = v.next ();
				double[] toAdd = nodeColonised
						.computeFineRootsCost (treeIndex, addFineRootLength, rootedVolume, woodDensity, woodCarbonContent, cRAreaToFRLengthRatio, frCost);		// recursivite : la methode renvoie les valeurs de fineRootLength pour le noeud enfant
				fineRootLength[0] += toAdd[0];
				fineRootLength[1] += toAdd[1];
			}
		}
		double localFineRootLength = getFineRootsDensity () * getVoxelRooted ().getVolume ()
									* getTopologicalToEffDistance ();
		
		//the topologicalToEffDistance ratio allows to correct biases in cost calculations linked to voxelization.
		// If we do nothing: the fine roots located diagonally across the tree relative to the x, y and z axes cost more in carbon than the others
		// because their topological distance to the tree is greater.. 
		// If we corrected by the ratio of Euclidean distance to topological distance, we could not take into account cases where the root system has made bypass.
		// In this case, the path between these roots and the base of the tree must be taken into account.
		// then the calculation of an effective distance (see the computeEffDist method) to replace the Euclidean distance.
		double localNewFineRoots = newFineRootsDensity * getVoxelRooted ().getVolume () * getTopologicalToEffDistance ();

		fineRootLength[0] += localFineRootLength;
		fineRootLength[1] += localNewFineRoots;

		// For the present node, we calculate the necessary increase in structural root biomass to support all the fine roots
		// we consider that the coarse root is a cylinder connecting the present node to its parent,
		// and whose section must be proportional to the length of fine roots which depend on it.
		double totalCost = Math.max (0, (fineRootLength[0] + fineRootLength[1]) 
										* cRAreaToFRLengthRatio
										* getFatherDistance () 
										* woodDensity * woodCarbonContent 
										- voxelRooted.getTheTreeCarbonCoarseRoots(treeIndex));		 
																							
		// the cost of this coarse root increase must be distributed among all the nodes which depend on the present node 	
		this.costDispatching (treeIndex, totalCost, localNewFineRoots, fineRootLength[1], newFineRootsDensity);	

		return fineRootLength;
	}

	/**
	 * Repercussion of the total carbon requirement for a node to all dependent nodes
	 * This method increments the fineRootCost variable 
	 * which represents the total carbon investment to establish new fine roots in a given node
	 * 
	 * @author Gregoire Talbot. September 2008
	 * @param treeIndex index of the tree on the plot 
	 * @param totalCost 
	 * @param localFineRoots
	 * @param totalFineRoots 
	 * @param newFineRootsDensity
	 */
	public void costDispatching (int treeIndex, double totalCost, double localFineRoots, double totalFineRoots,
								double newFineRootsDensity) {

		this.addFineRootsCost (totalCost * getTopologicalToEffDistance () / totalFineRoots);
		
		if (nodeColonised != null) {
			for (Iterator<SafeRootNode>  v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised =  v.next ();
				localFineRoots = newFineRootsDensity * nodeColonised.getVoxelRooted ().getVolume ()
						* nodeColonised.getTopologicalToEffDistance ();
				nodeColonised
						.costDispatching (treeIndex, totalCost, localFineRoots, totalFineRoots, newFineRootsDensity);
			}
		}
	}
	/**
	 * Get the deeper son node of the root topology
	 */
	public double getDeeperSonDepth () {

		double deeperSonDepth = getVoxelRooted ().getSurfaceDepth () + getVoxelRooted ().getThickness ();

		if (nodeColonised != null) {
			for (Iterator<SafeRootNode>  v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised =  v.next ();
				double sonDeeperSonDepth = nodeColonised.getDeeperSonDepth ();
				deeperSonDepth = Math.max (deeperSonDepth, sonDeeperSonDepth);
			}
		}
		return deeperSonDepth;
	}

	
	
	/**
	 * Tree fine roots senescence (computed each day only if budbust has started) 
	 * @param tree Reference of the SafeTree
	 * @param generalParameters Reference of a SafeGeneralParameters
	 * @param humificationDepth Soil humification depth
	 * @param carbonFR Tree carbon fine roots
	 * @param nitrogenFR Tree nitrogen fine roots
	 */
	public void computeFineRootsSenescence (SafeTree tree, 
											SafeGeneralParameters generalParameters, 
											double humificationDepth,
											double carbonFR, double nitrogenFR) {
	
		int treeIndex = tree.getId()-1;

		double carbonToDryMatter = 1d / tree.getTreeSpecies ().getWoodCarbonContent(); 
		double fineRootCost = (1 / (carbonToDryMatter * tree.getTreeSpecies ().getSpecificRootLength () * 1000)); // m
		double nitrogenRemobilisationFraction = tree.getTreeSpecies ().getRootNRemobFraction ();	
		
		// anoxia fine root senescence
		double fineRootLifespan  =  tree.getTreeSpecies ().getFineRootLifespan  ();
		if (this.getVoxelRooted ().getIsSaturated ()) 
			fineRootLifespan =  tree.getTreeSpecies ().getFineRootAnoxiaLifespan  ();
	
		// age fine root senescence
		double fineRootSenescence = 0;
		if (fineRootLifespan != 0) 
			fineRootSenescence =  (1 / fineRootLifespan) *getFineRootsDensity (); // m.m-3
		else System.out.println ("WARNING  fineRootLifespan = 0 !!!! ");

		setFineRootsDensity(getFineRootsDensity () - fineRootSenescence);
		
		this.getVoxelRooted ().setTreeRootsDensity (treeIndex, getFineRootsDensity ());
		this.getVoxelRooted ().addTreeRootsDensitySen (treeIndex, fineRootSenescence);

		//carbon senescence
		double carbonFineRootsSen = fineRootSenescence * this.getVoxelRooted ().getVolume () * fineRootCost; // m.m-3 to KgC
		tree.addCarbonFineRootsSen (carbonFineRootsSen);
		tree.setCarbonFineRoots (tree.getCarbonFineRoots () - carbonFineRootsSen);
		this.getVoxelRooted ().addTreeCarbonFineRoots (treeIndex, -carbonFineRootsSen);
		this.getVoxelRooted ().addTreeCarbonFineRootsSen (treeIndex, carbonFineRootsSen);

		// Nitrogen senescence 
		double nitrogenFineRootsSen = carbonFineRootsSen* (nitrogenFR / carbonFR);
		tree.setNitrogenFineRoots (tree.getNitrogenFineRoots () - nitrogenFineRootsSen);
		this.getVoxelRooted ().addTreeNitrogenFineRoots (treeIndex, -nitrogenFineRootsSen);
		
		//some of the nitrogen lost is back to labile pool
		tree.addNitrogenLabile (nitrogenFineRootsSen * nitrogenRemobilisationFraction);
		double nitrogenFineRootsLoss = nitrogenFineRootsSen * (1 - nitrogenRemobilisationFraction);
		tree.addNitrogenFineRootsSen (nitrogenFineRootsLoss);
		this.getVoxelRooted ().addTreeNitrogenFineRootsSen (treeIndex, nitrogenFineRootsLoss);
		
		//AQ	Deep senescent roots mineralization
		double voxelBottom = this.getVoxelRooted ().getZ()+(this.getVoxelRooted ().getThickness()/2);
		if (voxelBottom > humificationDepth) {
			this.getVoxelRooted ().addCumulatedTreeNitrogenRootsSen (nitrogenFineRootsLoss);//kg 
		}

		if (nodeColonised != null) {
			for (Iterator<SafeRootNode>  it = this.getNodeColonised ().iterator (); it.hasNext ();) {
				SafeRootNode nodeColonised = it.next ();
				nodeColonised.computeFineRootsSenescence (tree, generalParameters, humificationDepth, carbonFR, nitrogenFR);
			}
		}
	}



	/**
	 * Removing roots after soil management, root pruning or anoxia when saturation occurred for a long period
	 * 
	 * @author Gregoire Talbot. June 2009
	 * @param voxel Reference of a SafeVoxel
	 * @param tree Reference of a SafeTree
	 * @param prop Proportion of fine root to remove
	 * @param testAnoxia If true it is anoxia process
	 * @param humificationDepth Soil humification depth
	 * @param carbonFR Tree carbon fine roots
	 * @param nitrogenFR Tree nitrogen fine roots
	 * @param carbonCR Tree carbon coarse roots
	 * @param nitrogenCR Tree nitrogen coarse roots
	 */
	public void removeSonsRoots (SafeVoxel voxel, SafeTree tree,  double prop, boolean testAnoxia , double humificationDepth, 
								double carbonFR, double nitrogenFR, double carbonCR, double nitrogenCR ) {
		

		int treeIndex = tree.getId()-1;
		double carbonToDryMatter = 1d / tree.getTreeSpecies ().getWoodCarbonContent(); 
		double frCost =  1 / (carbonToDryMatter * 1000 * tree.getTreeSpecies ().getSpecificRootLength ());

		//if (prop > 0.5) prop = 1;
		if (nodeColonised != null) {
			for (Iterator<SafeRootNode>  it = this.getNodeColonised ().iterator (); it.hasNext ();) {
				SafeRootNode nodeSon = it.next ();
				SafeVoxel voxelSon = nodeSon.getVoxelRooted ();
				if ((voxel == null) || (voxelSon.getId () == voxel.getId ())) { // if v not specified, all voxel colonized are removed.

					
					
					double carbonFineRootSenescence = prop * nodeSon.getFineRootsDensity () * voxelSon.getVolume () * frCost;
					double nitrogenFineRootSenescence =  carbonFineRootSenescence * nitrogenFR / carbonFR;
					
					voxelSon.addTreeCarbonFineRootsSen (treeIndex, carbonFineRootSenescence);
					voxelSon.addTreeNitrogenFineRootsSen (treeIndex, nitrogenFineRootSenescence);

					double newDensity = (1 - prop) * nodeSon.getFineRootsDensity ();
					double senDensity = prop * nodeSon.getFineRootsDensity ();
					double oldcarbonFineRoot = nodeSon.getFineRootsDensity () * voxelSon.getVolume () * frCost;
	
					nodeSon.setFineRootsDensity (newDensity);
					voxelSon.setTreeRootsDensity (treeIndex, newDensity);
					
					if ((testAnoxia) && (voxelSon.getIsSaturated ())) {
						voxelSon.addTreeRootsDensitySenAnox (treeIndex, senDensity);
					}
					else {
						voxelSon.addTreeRootsDensitySen (treeIndex, senDensity);
					}
							
					double carbonFineRoot = newDensity * voxelSon.getVolume () * frCost;
					voxelSon.setTreeCarbonFineRoots(treeIndex, carbonFineRoot);
				
					//RAZ saturation duration because all roots are dead 
					if (newDensity==0) voxelSon.setTreeRootsAgeInWater(tree.getId()-1, 0);

					//update TREE carbon and nitrogen pool 
					tree.addCarbonFineRootsSen (carbonFineRootSenescence);
					tree.addNitrogenFineRootsSen (nitrogenFineRootSenescence);	
					tree.setCarbonFineRoots (tree.getCarbonFineRoots() - carbonFineRootSenescence);
					tree.setNitrogenFineRoots (tree.getNitrogenFineRoots() - nitrogenFineRootSenescence);

					//AQ	Deep senescent roots mineralization
					double voxelBottom = voxelSon.getZ()+(voxelSon.getThickness()/2);
					if (voxelBottom > humificationDepth) 
						voxelSon.addCumulatedTreeNitrogenRootsSen (nitrogenFineRootSenescence);//kg 

					//in case of ANOXIA
					if ((testAnoxia) && (voxelSon.getIsSaturated ())) {
						tree.addCarbonFineRootsSenAnoxia (carbonFineRootSenescence);
						tree.addNitrogenFineRootsSenAnoxia (nitrogenFineRootSenescence);
					}
					
					//If all fine roots are removed, son roots have also to be killed (and coarse root also) 
					if (prop == 1) {
						//remove sons roots
						nodeSon.removeSonsRoots (null, tree,  prop, testAnoxia, humificationDepth,
												carbonFR, nitrogenFR, carbonCR, nitrogenCR);
	
						double carbonCoarseRootSenescence = voxelSon.getTheTreeCarbonCoarseRoots(treeIndex);
						double nitrogenCoarseRootSenescence = carbonCoarseRootSenescence * nitrogenCR / carbonCR;
						
						voxelSon.setTreeCarbonCoarseRoots(treeIndex, 0);
						voxelSon.setTreeCarbonCoarseRootsTarget(treeIndex, 0);
						voxelSon.setTreeCarbonCoarseRootsSen (treeIndex, carbonCoarseRootSenescence);
						voxelSon.setTreeNitrogenCoarseRootsSen (treeIndex, nitrogenCoarseRootSenescence);
						
						//update TREE carbon and nitrogen pool 
						tree.addCarbonCoarseRootsSen (carbonCoarseRootSenescence);
						tree.addNitrogenCoarseRootsSen (nitrogenCoarseRootSenescence);
						tree.setCarbonCoarseRoots (tree.getCarbonCoarseRoots() - carbonCoarseRootSenescence);
						tree.setNitrogenCoarseRoots (tree.getNitrogenCoarseRoots() - nitrogenCoarseRootSenescence);

						//AQ	Deep senescent roots mineralization
						if (voxelBottom > humificationDepth) {
							voxelSon.addCumulatedTreeNitrogenRootsSen (nitrogenCoarseRootSenescence);//kg 
						}
						if ((testAnoxia) && (voxelSon.getIsSaturated ())) {
							tree.addCarbonCoarseRootsSenAnoxia  (carbonCoarseRootSenescence);
							tree.addNitrogenCoarseRootsSenAnoxia (nitrogenCoarseRootSenescence);
						}
					}
				}
			}
		}
	}
	/**
	 * Compute the total rooted volume from this root node 
	 */
	public double computeRootedVolume () {
		double volume = 0;
		if (nodeColonised != null) {
			for (Iterator<SafeRootNode>  v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised = v.next ();
				volume += nodeColonised.computeRootedVolume ();
			}
		}
		volume += getVoxelRooted ().getVolume ();
		return volume;
	}
	public SafePlantRoot getPlantRoots () {return plantRoots;}
	public int getPlanteType() {return planteType;}

	public SafeVoxel getVoxelRooted () {return voxelRooted;}
	public void setVoxelRooted (SafeVoxel v) {voxelRooted = v;}

	public SafeRootNode getNodeParent () {return nodeParent;}
	public void setNodeParent (SafeRootNode v) {nodeParent = v;}

	public Collection<SafeRootNode> getNodeColonised () {return nodeColonised;}
	public void setNodeColonised (Collection<SafeRootNode> col) {nodeColonised = col;}
	public void addNodeColonised (SafeRootNode newColonisedNode) {
		if (nodeColonised == null) this.nodeColonised = new ArrayList<SafeRootNode> ();
		this.nodeColonised.add (newColonisedNode);
	}

	public int getColonisationDirection () {return colonisationDirection;}
	public void setColonisationDirection (int d) {this.colonisationDirection = d;}
	public int getColonisationDate () {return colonisationDate;}	

	public double getTreeDistance () {	
		BigDecimal bd = new BigDecimal(treeDistance);
		bd= bd.setScale(2,BigDecimal.ROUND_DOWN);
		return (bd.doubleValue());
	}
	public void setTreeDistance (double d) {treeDistance = (float) d;}
	public double getFatherDistance () {
		BigDecimal bd = new BigDecimal(fatherDistance);
		bd= bd.setScale(2,BigDecimal.ROUND_DOWN);
		return (bd.doubleValue());	
	}
	public void setFatherDistance (double d) {fatherDistance = (float) d;}
	public double getEffectiveDistance () {return (double) effectiveDistance;}
	public void setEffectiveDistance (double d) {effectiveDistance = (float) d;}
	public double getTopologicalToEffDistance () {
		return getEffectiveDistance () / getTreeDistance ();
	}
	
	public double getFineRootsCost () {return (double) fineRootsCost;}
	public void addFineRootsCost (double c) {fineRootsCost += (float) c;}
	public void setFineRootsCost (double c) {fineRootsCost = (float) c;}
	
	public double getFineRootsDensity () {return fineRootsDensity;}
	public void setFineRootsDensity (double d) {fineRootsDensity = d;}

	public double getFineRootsLength () {
		return this.getFineRootsDensity () * this.getVoxelRooted ().getVolume ();
	} 

	public double getFineRootsTotalInvestment () {return fineRootsTotalInvestment;}
	public void setFineRootsTotalInvestment (double fr) {fineRootsTotalInvestment = fr;}
	public void addFineRootsTotalInvestment (double fr) {fineRootsTotalInvestment += fr;}
	

	public double getWaterUptake () {return waterUptake;}
	public void addWaterUptake (double d) {waterUptake += d;}
	public void setWaterUptake (double d) {waterUptake = d;}
	public double getNitrogenUptake () {return nitrogenUptake;}
	public void addNitrogenUptake (double v) {nitrogenUptake += v;}
	public void setNitrogenUptake (double v) {nitrogenUptake = v;}

	public double getWaterEfficiency () {
		if ((getFineRootsLength() > 0) && (getWaterUptake() > 0))
			return getWaterUptake()/getFineRootsLength();
		else return 0;
	}
	public double getNitrogenEfficiency () {
		if ((getFineRootsLength() > 0) && (getNitrogenUptake() > 0))
			return (getNitrogenUptake()/1000)/getFineRootsLength();	//convert gN to kgN
		else return 0;		
	}
	
	public double getPhiPf () {return phiPf;}
	public void setPhiPf (double d) {phiPf = d;}
	public double getWaterRhizospherePotential () {return waterRhizospherePotential;}
	public void setWaterRhizospherePotential (double d) {waterRhizospherePotential =  d;}

	public double getWaterUptakePotential () {return waterUptakePotential;}
	
	public void setWaterUptakePotential (double v) {waterUptakePotential =  v;}
	public void addWaterUptakePotential (double v) {waterUptakePotential +=  v;}


	public double getNitrogenShareUptake() {return nitrogenShareUptake;}
	public double getNitrogenZeroSinkPotential () {return  nitrogenZeroSinkPotential;}

	public void setNitrogenShareUptake (double v) {nitrogenShareUptake =   v;}
	public void setNitrogenZeroSinkPotential (double v) {nitrogenZeroSinkPotential =   v;}
	
	public double getNitrogenUptakePotential () {return  nitrogenUptakePotential;}
	public void setNitrogenUptakePotential  (double v) {nitrogenUptakePotential =   v;}
	public void addNitrogenUptakePotential (double v) {nitrogenUptakePotential +=   v;}
	
	public double getDistanceX0() {return distanceX0;}
	public double getDistanceX1() {return distanceX1;}
	public double getDistanceY2() {return distanceY2;}
	public double getDistanceY3() {return distanceY3;}
	
	public double getMaxDistanceX0() {
		
		if (nodeColonised == null) return distanceX0;
		else {
			double value = 0;
			for (Iterator v = nodeColonised.iterator (); v.hasNext ();) {			
				SafeRootNode node = (SafeRootNode) v.next ();
				value = Math.max(value, node.getMaxDistanceX0());
			}
			return  value;
		}
	}
	public double getMaxDistanceX1() {

		if (nodeColonised == null) return distanceX1;
		else {
			double value = 0;
			for (Iterator v = nodeColonised.iterator (); v.hasNext ();) {
				SafeRootNode node = (SafeRootNode) v.next ();
				value = Math.max(value, node.getMaxDistanceX1());
			}
			return  value;
		}
	}	
	public double getMaxDistanceY2 () {
		
		if (nodeColonised == null) return distanceY2;
		else {
			double value = 0;
			for (Iterator v = nodeColonised.iterator (); v.hasNext ();) {
				SafeRootNode node = (SafeRootNode) v.next ();
				value = Math.max(value, node.getMaxDistanceY2());
			}
			return  value;
		}
	}	
	public double getMaxDistanceY3() {
		if (nodeColonised == null) return distanceY3;
		else {
			double value = 0;
			for (Iterator v = nodeColonised.iterator (); v.hasNext ();) {
				SafeRootNode node = (SafeRootNode) v.next ();
				value = Math.max(value, node.getMaxDistanceY3());
			}
			return  value;
		}
	}
	
  	public void drawNodes() {
		System.out.println("drawNodes cell="+getVoxelRooted().getCell().getId()+" node="+this);
		if (nodeColonised == null) return;
		for (Iterator v = nodeColonised.iterator (); v.hasNext ();) {
			SafeRootNode node = (SafeRootNode) v.next ();
			node.drawNodes();
		}
  	}
  	
	public int compareTo (Object other) {
		  double nombre1 = ((SafeRootNode) other).getPhiPf();
		  double nombre2 = this.getPhiPf();
		  if (nombre1 > nombre2)  return -1;
		  else if(nombre1 == nombre2) return 0;
		  else return 1;
		}
	
	public String toString(){
		String str = "";
		String type = "crop";
		if (planteType == 2) type = "tree";
		str = "Node="+type+" cell="+voxelRooted.getCell().getId()+" voxel="+voxelRooted.getZ();
		return str;
	}

}
