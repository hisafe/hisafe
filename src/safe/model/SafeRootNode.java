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
import java.util.Collection;
import java.util.Iterator;
import java.math.BigDecimal;
import jeeb.lib.util.Vertex3d;

/**
 * SafeRootNode is used to build tree coarse roots topology
 * 
 * @author Isabelle Lecomte - INRAE Montpellier France - November 2005
 *         Grégoire Talbot  - INRAE Montpellier France - September 2008
 */
public class SafeRootNode implements Serializable {


	private static final long serialVersionUID = 1L;
	private int planteType;					//1=crop 2=tree
	private SafePlantRoot plantRoots;		// plant (tree or crop) fine root reference address
	private SafeVoxel voxelRooted; 			// Reference of the rooted voxel
	private SafeRootNode nodeParent; 		// Reference of the single parent node
	private Collection<SafeRootNode> nodeColonised; 		// Reference of the collection of colonised nodes

	private int colonisationDate; 			// Date of first colonisation
	private int colonisationDirection; 		// direction of the colonization from father node // gt - 27.03.2009
											// 0 is for x+, 1 for x-, 2 for y+, 3 for y-, 4 for z+, 5 for z-
	
	private float treeDistance; 			// Distance to the tree trunc following topology path (m)
	private float fatherDistance; 			// Distance from father node // GT 17/07/2008
	private float effectiveDistance;		//effective distance from tree (m) 

	private double fineRootsDensity; 		// m m-3
	private double fineRootsTotalInvestment;
	private double fineRootsCost; 			// kg of CoarseRoot m-1 of fine roots.
	
	private double phiPf;						//cm2 day-1
	private double waterRhizospherePotential; 	// cm
	private double nitrogenUptake; 				// kg N
	private double waterUptake; 				// liters
	

	public SafeRootNode (SafePlantRoot root, SafeVoxel voxelRooted, SafeRootNode nodeParent, int day, double fineRoots, SafeTree tree,  int direction) {

		this.planteType = 2; //tree
		this.plantRoots = root;
		this.voxelRooted = voxelRooted;
		this.nodeParent = nodeParent;
		this.colonisationDate = day;
		this.fineRootsDensity = (float) fineRoots;
		this.fineRootsTotalInvestment =  (float) fineRoots;
		this.nodeColonised = null;
		this.waterUptake = 0;
		this.nitrogenUptake = 0;
		this.fineRootsCost = 0;

		// linking father and son
		if (nodeParent != null) {
			nodeParent.addNodeColonised (this);
			computeDistanceFromFather (direction);
			setTreeDistance (nodeParent.getTreeDistance () + getFatherDistance());
			this.computeEffDist (tree);
		} else {
			setFatherDistance (0.5 * this.voxelRooted.getThickness ());
			setTreeDistance (0.5 * this.voxelRooted.getThickness ());
			setEffectiveDistance (0.5 * this.voxelRooted.getThickness ());
			this.colonisationDirection = 4; // gt - 27.03.2009
		}
		// init plant waterRhizospherePotential with soil water potential
		setWaterRhizospherePotential (voxelRooted.getWaterPotentialTheta ());
	}

	public SafeRootNode (SafePlantRoot root, SafeVoxel voxelRooted, SafeRootNode nodeParent, int day, double fineRootDensity) {

		this.planteType = 1;	//Crop
		this.plantRoots = root;
		this.colonisationDirection = 4; 
		this.voxelRooted = voxelRooted;
		this.nodeParent = nodeParent;
		this.colonisationDate = day;
		this.fineRootsDensity = (float) fineRootDensity;
		this.fineRootsTotalInvestment =  (float) fineRootDensity;
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
	 * Compute the distance from the father node // GT 17/07/2008
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
	 * Compute the efficace distance from tree
	 * si la distance topologique a l'arbre (treeDistance) est la plus courte possible (shorterWay), 
	 * autrement dit s'il n'y a pas eu de "detours" pour la colonisation du voxel, on prend la distance euclidienne a l'arbre
	 * sinon, on recherche dans la genealogie du noeud present le noeud N le plus lointain que l'on peut rejoindre sans faire de detour 
	 * (c'est a dire que la distance a ce noeud est la plus courte possible). 
	 * Et dans ce cas, la distance efficace est la somme de la distance euclidienne entre le noeud present et le noeud N, 
	 * et de la distance efficace du noeud N a l'arbre. 
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

	public double shorterWay (double x, double y, double z) {
		SafeCell cell = (SafeCell) this.getVoxelRooted ().getCell();
		SafePlot plot = (SafePlot) cell.getPlot();
		double plotWidth = plot.getXSize();
		double plotHeight = plot.getYSize();
		Vertex3d nodePosition = this.getVoxelRooted ().getGravityCenter ();
		double nodex = nodePosition.x;
		double nodey = nodePosition.y;
		//correction of symetry toric colonisation (il 13-12-2017)
		if ((this.colonisationDirection==0) && (nodex < x) ) nodex = nodex + plotWidth;
		if ((this.colonisationDirection==1) && (nodex > x) ) nodex = nodex - plotWidth;
		if ((this.colonisationDirection==2) && (nodey < y) ) nodey = nodey + plotHeight;
		if ((this.colonisationDirection==3) && (nodey > y) ) nodey = nodey - plotHeight;
		double dist = Math.abs (x - nodex) + Math.abs (y - nodey) + Math.abs (z - nodePosition.z);
		return dist;
	}

	public double euclidist (double x, double y, double z) {
		SafeCell cell = (SafeCell) this.getVoxelRooted ().getCell();
		SafePlot plot = (SafePlot) cell.getPlot();
		double plotWidth = plot.getXSize();
		double plotHeight = plot.getYSize();
		Vertex3d nodePosition = this.getVoxelRooted ().getGravityCenter ();
		double nodex = nodePosition.x;
		double nodey = nodePosition.y;
		//correction of symetry toric colonisation (il 13-12-2017)
		if ((this.colonisationDirection==0) && (nodex < x) ) nodex = nodex + plotWidth;
		if ((this.colonisationDirection==1) && (nodex > x) ) nodex = nodex - plotWidth;
		if ((this.colonisationDirection==2) && (nodey < y) ) nodey = nodey + plotHeight;
		if ((this.colonisationDirection==3) && (nodey > y) ) nodey = nodey - plotHeight;
		double dist = Math.sqrt (Math.pow (x - nodex, 2) + Math.pow (y - nodey, 2)
				+ Math.pow (z - nodePosition.z, 2));
		return dist;
	}

	/**
	 * Set the distance from a tree
	 */
	public void setDistances (double d) {
		double distance = d + getFatherDistance ();
		setTreeDistance (distance);
		// here recursitity stops because there is no more colonised voxels bellow
		if (nodeColonised != null) {
			for (Iterator<SafeRootNode> v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised = v.next ();
				nodeColonised.setDistances (distance); // recursivity
			}
		}
	}

	/**
	 * 
	 * compute the target coarse root carbon for all nodes 
	 *  and the total coarse root target carbon of the tree // GT 17/07/2008
	 */
	public double computeCarbonCoarseRootsTarget (SafeTree tree, 
												double[] additionalRootLength, 
												double cRWoodDensity,
												double cRWoodCarbonContent, 
												double cRAreaToFRLengthRatio) {


		double fineRootsLength = 0; // Kg C
		
		//loop on all colonised voxels to get the fineRootLenght total to the end of the coarseRoot
		if (nodeColonised != null) {
			for (Iterator<SafeRootNode> v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised =  v.next ();
				double toAdd = nodeColonised
						.computeCarbonCoarseRootsTarget (tree, additionalRootLength, cRWoodDensity, cRWoodCarbonContent, cRAreaToFRLengthRatio);
				fineRootsLength += toAdd;
			}
		}
		int v = this.getVoxelRooted ().getId () - 1;
		int treeIndex = tree.getId () - 1;
		double fineRootDensity = this.getVoxelRooted ().getTheTreeRootsDensity (treeIndex);
		fineRootsLength += fineRootDensity * this.getVoxelRooted ().getVolume () // m m-3 * m3
							* this.getTopologicalToEffDistance (); 				// voir les explication la dessus dans la methode computeEffDist
																				// - 30.03.2009

		if (additionalRootLength != null) 
			fineRootsLength += additionalRootLength[v] // m
							* this.getTopologicalToEffDistance ();


		double carbonTarget =  (fineRootsLength * cRAreaToFRLengthRatio * this.getFatherDistance () // m x m2 m-1 x m = m3
								      * cRWoodDensity * cRWoodCarbonContent); // kg.m-3 x kgC.kg-1 = kgC.m-3
		
		//We remove the voxels with enough carbon in coarse root (negative imbalance)  
		//https://github.com/hisafe/hisafe/issues/123
		carbonTarget = Math.max(carbonTarget, voxelRooted.getTheTreeCarbonCoarseRootsTarget(treeIndex));

		voxelRooted.setTreeCarbonCoarseRootsTarget (treeIndex, carbonTarget);

		// Math.max(...) : if target< carbon : the coarse root can't contribute to carbon allocation as a negative sink
		tree.addCarbonCoarseRootsTarget (carbonTarget);


		
		return fineRootsLength;
	}
	/**
	 * Growth of carbon coarse root  in KG C : computing of carbon allocation between coarse roots
	 * 
	 * @author Gregoire Talbot. September 2008
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

			//IL 13/04/2022 je rajoute un test car il arrive que  additionalCoarseRoot soit négatif
			//mais du coup on perd du carbone et de l'azote qui ne sera pas réparti dans les voxels
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
	 * methode pour calculer le prix en carbone (Kg) de la mise en place de nouvelles racines fines (en m) dans un voxel donné
	 * la methode est appelee uniquement sur firstRootNode dans safeTree, mais elle s'appelle recursivement pour faire le calcul sur l'ensemble du systeme racinaire
	 * la variable frCost donnéée en entrée correspond au cout en carbone des nouvelles racines fines sans compter le besoin potentiel d'investissement dans des racines de structure
	 * le reste de la methode calcule l'investissement necessaire en racines de structures pour mettre en place ces racines fines 
	 * pour ce calcul, on fera l'hypothese que les nouvelles racines fines (addFineRootLength) seront reparties uniformement sur tout le volume enracine par l'arbre (rootedVolume)
	 */
	public double[] computeFineRootsCost (int treeIndex, double addFineRootLength, double rootedVolume,
			double cRWoodDensity, double cRWoodCarbonContent, double cRAreaToFRLengthRatio, double frCost) {

		// cout des racines fines en tant que telles 
		// ce cout sera incremente ensuite pour prendre en compte ce qu'elles impliquent comme investissement en racines de structures 
		// par la methode costDispatching
		
		addFineRootsCost (frCost);

		
		double[] fineRootLength = {0.0, 0.0}; // old fine root length and additionalFineRootLength //  ces variables ne sont pas des longueurs de racines locales dans le voxel ou l'on se trouve, mais la somme des longueurs de racines fines qui en dependent, il faut pour cela ajouter les longueurs de racines dans tous les voxels colonises par la descendance du noeud present, d'ou la recursivite de la methode
		double newFineRootsDensity = addFineRootLength / rootedVolume;		// densite de nouvelles racines fines dans le voxel, sous l'hypothese que les nouvelles racines fines seront reparties uniformement sur tout le volume enracine par l'arbre (rootedVolume)

		if (nodeColonised != null) {
			for (Iterator<SafeRootNode>  v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised = v.next ();
				double[] toAdd = nodeColonised
						.computeFineRootsCost (treeIndex, addFineRootLength, rootedVolume, cRWoodDensity, cRWoodCarbonContent, cRAreaToFRLengthRatio, frCost);		// recursivite : la methode renvoie les valeurs de fineRootLength pour le noeud enfant
				fineRootLength[0] += toAdd[0];
				fineRootLength[1] += toAdd[1];
			}
		}
		double localFineRootLength = getFineRootsDensity () * getVoxelRooted ().getVolume ()
									* getTopologicalToEffDistance ();
		// le ratio topologicalToEffDistance permet de corriger des biais des les calculs de cout lies a la voxelisation.
		// Si on ne fait rien : les racines fines situees en diagonale de l'arbre par rapport aux axes x,y et z coutent plus cher en carbone que les autres, 
		// car leur distance topologique a l'arbre est plus importante. 
		// Si on corrigeait par le ratio distance euclidienne sur distance topologique, on ne pourrait pas prendre en compte les cas ou le systeme racinaire a fait des detour. 
		// C'est le cas par exemple lorsque le systeme racinaire recolonise les voxels de surface au milieu des allees depuis ses racines profondes. 
		// Il faut dans ce cas prendre en compte le chemin entre ces racines et la base de l'arbre
		// d'ou le calcule d'une distance efficace (voir la methode computeEffDist) pour remplacer la distance euclidienne.
		double localNewFineRoots = newFineRootsDensity * getVoxelRooted ().getVolume () * getTopologicalToEffDistance ();

		fineRootLength[0] += localFineRootLength;
		fineRootLength[1] += localNewFineRoots;

		// pour le noeud present, on calcule l'augmentation necessaire de biomasse de racines de structures pour supporter l'ensemble des racines fines 
		// (nouvelles et anciennes) qui en dependrons.
		// on considere que la racine de structure est un cylindre reliant le noeud present a son parent, 
		// et dont la section doit petre proportionnelle a la longueur de racines fines qui dependent de lui.
		// le coefficient de proportionnalite est le parametre cRAreaToFRLengthRatio
		double totalCost = Math.max (0, (fineRootLength[0] + fineRootLength[1]) * cRAreaToFRLengthRatio
				* getFatherDistance () * cRWoodDensity * cRWoodCarbonContent - voxelRooted.getTheTreeCarbonCoarseRoots(treeIndex));		 
																							
		// le cout de cette augmentation d'une racine de structure doit etre reparti entre tous les noeuds qui dependent du noeud present, 
		// car ils sont responsables de ce besoin, c'est l'objet de la methode costDispatching		
		this.costDispatching (treeIndex, totalCost, localNewFineRoots, fineRootLength[1], newFineRootsDensity);	

		return fineRootLength;
	}

	/**
	 * methode dont l'objet est, connaissant le besoin total d'investissement dans la racine de structure du noeud present, 
	 * de repercuter ce cout sur l'ensemble des noeuds qui en dependent.	
	 * cette methode incremente la variable fineRootCost 
	 * (qui represente l'investissement total en carbone pour mettre en place des nouvelles racines fines dans un noeud donne) 
	 * pour l'ensemble des noeuds qui dependent du noeud present
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
	 * Tree fine roots and coarse roots anoxia (computed each day only if water table) 
	 */
	public void computeCoarseRootsAnoxia (SafeTree tree, SafeGeneralParameters settings, double humificationDepth) {
		
		SafeVoxel v = this.getVoxelRooted ();
		int treeIndex = tree.getId()-1;
		
		//if the voxel is saturated 
		//roots are killed and colonised voxels also 
		if (v.getIsSaturated ()) {
			if (v.getTheTreeRootsAgeInWater (treeIndex) > tree.getTreeSpecies ().getCoarseRootAnoxiaResistance ()) {
				boolean testAnoxia = true;
				float proportion = 1;
				this.removeSonsRoots (null, tree, settings, proportion, testAnoxia, humificationDepth); 
				v.setTreeRootsAgeInWater (treeIndex, 0);	
			}
		}
		else {
			//we look at all colonised before (recursivity) 
			if (nodeColonised != null) {
				for (Iterator<SafeRootNode>  it = this.getNodeColonised ().iterator (); it.hasNext ();) {
					SafeRootNode nodeColonised = it.next ();
					nodeColonised.computeCoarseRootsAnoxia (tree, settings, humificationDepth);
				}
			}
		}
	}
	
	/**
	 * Tree fine roots senescence (computed each day only if budbust has started) 
	 */
	public void computeFineRootsSenescence (SafeTree tree, SafeGeneralParameters settings, double humificationDepth) {
	

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
			fineRootSenescence =  (1 / fineRootLifespan) * this.fineRootsDensity; // m.m-3
		else System.out.println ("WARNING  fineRootLifespan = 0 !!!! ");
		
		this.fineRootsDensity -= fineRootSenescence;
		this.getVoxelRooted ().setTreeRootsDensity (treeIndex, this.fineRootsDensity);
		this.getVoxelRooted ().addTreeRootsDensitySen (treeIndex, fineRootSenescence);

		//carbon senescence
		double carbonFineRootsSen = fineRootSenescence * this.getVoxelRooted ().getVolume () * fineRootCost; // m.m-3 to KgC
		tree.addCarbonFineRootsSen (carbonFineRootsSen);
		tree.setCarbonFineRoots (tree.getCarbonFineRoots () - carbonFineRootsSen);
		this.getVoxelRooted ().addTreeCarbonFineRoots (treeIndex, -carbonFineRootsSen);
		this.getVoxelRooted ().setTreeCarbonFineRootsSen (treeIndex, carbonFineRootsSen);

			
		// Nitrogen senescence 
		double nitrogenFineRootsSen = carbonFineRootsSen
				* ((tree.getNitrogenFineRoots () / tree.getCarbonFineRoots ()));

		tree.setNitrogenFineRoots (tree.getNitrogenFineRoots () - nitrogenFineRootsSen);
		this.getVoxelRooted ().addTreeNitrogenFineRoots (treeIndex, -nitrogenFineRootsSen);
		
		
		//some of the nitrogen lost is back to labile pool
		tree.addNitrogenLabile (nitrogenFineRootsSen * nitrogenRemobilisationFraction);
		double nitrogenFineRootsLoss = nitrogenFineRootsSen * (1 - nitrogenRemobilisationFraction);
		tree.addNitrogenFineRootsSen (nitrogenFineRootsLoss);
		this.getVoxelRooted ().setTreeNitrogenFineRootsSen (treeIndex, nitrogenFineRootsLoss);
		
		//AQ	Deep senescent roots mineralization
		double voxelBottom = this.getVoxelRooted ().getZ()+(this.getVoxelRooted ().getThickness()/2);
		if (voxelBottom > humificationDepth) {
			this.getVoxelRooted ().addCumulatedTreeNitrogenRootsSen (nitrogenFineRootsLoss);//kg 
		}
		

		if (nodeColonised != null) {
			for (Iterator<SafeRootNode>  it = this.getNodeColonised ().iterator (); it.hasNext ();) {
				SafeRootNode nodeColonised = it.next ();
				nodeColonised.computeFineRootsSenescence (tree, settings, humificationDepth);
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

	/**
	 * Removing roots during soil management or when saturation occured for a long period
	 * gt-09.07.2009
	 * ALSO after ROOT prunnig (IL 12 05 2015) 
	 * ADD cumulation in Carbon and Nitrogen Anoxia (not for root pruning or soil management)  IL 10-04-2018
	 */
	public void removeSonsRoots (SafeVoxel voxel, SafeTree tree, SafeGeneralParameters settings, float prop, boolean testAnoxia , double humificationDepth) {
		
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
					double nitrogenFineRootSenescence=  carbonFineRootSenescence * tree.getNitrogenFineRoots () / tree.getCarbonFineRoots ();
					
					voxelSon.setTreeCarbonFineRootsSen (treeIndex, carbonFineRootSenescence);
					voxelSon.setTreeNitrogenFineRootsSen (treeIndex, nitrogenFineRootSenescence);

					double newDensity = (1 - prop) * nodeSon.getFineRootsDensity ();
					double senDensity = prop * nodeSon.getFineRootsDensity ();
					nodeSon.setFineRootsDensity (newDensity);
					voxelSon.setTreeRootsDensity (treeIndex, newDensity);
					voxelSon.setTreeRootsDensitySen (treeIndex, senDensity);
					
					double carbonFineRoot = newDensity * voxelSon.getVolume () * frCost;
					voxelSon.setTreeCarbonFineRoots(treeIndex, carbonFineRoot);
					
					//RAZ saturation duration because all roots are dead 
					if (newDensity==0) voxelSon.setTreeRootsAgeInWater(tree.getId()-1, 0);

					//update TREE carbon and nitrogen pool 
					tree.addCarbonFineRootsSen (carbonFineRootSenescence);
					tree.addNitrogenFineRootsSen (nitrogenFineRootSenescence);				
					tree.setCarbonFineRoots (tree.getCarbonFineRoots () - carbonFineRootSenescence);
					tree.setNitrogenFineRoots (tree.getNitrogenFineRoots () - nitrogenFineRootSenescence);
					
					//AQ	Deep senescent roots mineralization
					double voxelBottom = voxelSon.getZ()+(voxelSon.getThickness()/2);
					if (voxelBottom > humificationDepth) {
						voxelSon.addCumulatedTreeNitrogenRootsSen (nitrogenFineRootSenescence);//kg 
					}
					
					//in case of ANOXIA
					if ((testAnoxia) && (voxelSon.getIsSaturated ())) {
						tree.addCarbonFineRootsSenAnoxia (carbonFineRootSenescence);
						tree.addNitrogenFineRootsSenAnoxia (nitrogenFineRootSenescence);
					}
					
					
					
					//If all fine roots are removed, son roots have also to be killed (and coarse root also) 
					if (prop == 1) {
						//remove sons roots
						nodeSon.removeSonsRoots (null, tree, settings, prop, testAnoxia, humificationDepth);
						
						double carbonCoarseRootSenescence = voxelSon.getTheTreeCarbonCoarseRoots(treeIndex);
						double nitrogenCoarseRootSenescence = carbonCoarseRootSenescence * tree.getNitrogenCoarseRoots () / tree.getCarbonCoarseRoots ();
						

						voxelSon.setTreeCarbonCoarseRoots(treeIndex, 0);
						voxelSon.setTreeCarbonCoarseRootsTarget(treeIndex, 0);
						voxelSon.setTreeCarbonCoarseRootsSen (treeIndex, carbonCoarseRootSenescence);
						voxelSon.setTreeNitrogenCoarseRootsSen (treeIndex, nitrogenCoarseRootSenescence);
	
						
						
						//update TREE carbon and nitrogen pool 
						tree.addCarbonCoarseRootsSen (carbonCoarseRootSenescence);
						tree.addNitrogenCoarseRootsSen (nitrogenCoarseRootSenescence);
						tree.setCarbonCoarseRoots (tree.getCarbonCoarseRoots () - carbonCoarseRootSenescence);
						tree.setNitrogenCoarseRoots (tree.getNitrogenCoarseRoots () - nitrogenCoarseRootSenescence);
											
						//AQ	Deep senescent roots mineralization
						if (voxelBottom > humificationDepth) {
							voxelSon.addCumulatedTreeNitrogenRootsSen (nitrogenCoarseRootSenescence);//kg 
						}
						if ((testAnoxia) && (voxelSon.getIsSaturated ())) {
							tree.addCarbonCoarseRootsSenAnoxia  (carbonCoarseRootSenescence);
							tree.addNitrogenCoarseRootsSenAnoxia (nitrogenCoarseRootSenescence);
						}
					
						//delete the root node
						it.remove ();
						tree.getPlantRoots().getRootTopology ().remove (voxelSon);
					}
				}
			}
		}
		//The voxel father 
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
	
	public double getFineRootsDensity () {return (double) fineRootsDensity;}
	public void setFineRootsDensity (double d) {fineRootsDensity = (float) d;}

	public double getFineRootsLength () {
		return this.getFineRootsDensity () * this.getVoxelRooted ().getVolume ();
	} 

	public double getFineRootsTotalInvestment () {return (double) fineRootsTotalInvestment;}
	public void setFineRootsTotalInvestment (double fr) {fineRootsTotalInvestment = (float) fr;}
	public void addFineRootsTotalInvestment (double fr) {fineRootsTotalInvestment += (float) fr;}
	

	public double getWaterUptake () {return (double) waterUptake;}
	public void addWaterUptake (double d) {waterUptake += (float) d;}
	public void setWaterUptake (double d) {waterUptake = (float) d;}

	public double getNitrogenUptake () {return (double) nitrogenUptake;}
	public void addNitrogenUptake (double v) {
		nitrogenUptake += (float) v;
	}
	public void setNitrogenUptake (double v) {
		nitrogenUptake = (float) v;
	}

	public double getWaterEfficiency () {
		if ((getFineRootsLength() > 0) && (getWaterUptake() > 0))
			return getWaterUptake()/getFineRootsLength();
		else return 0;
	}
	public double getNitrogenEfficiency () {
		if ((getFineRootsLength() > 0) && (getNitrogenUptake() > 0))
			return getNitrogenUptake()/getFineRootsLength();
		else return 0;		
	}
	
	public double getPhiPf () {return (double) phiPf;}
	public void setPhiPf (double d) {phiPf = (float) d;}
	
	public double getWaterRhizospherePotential () {return (double) waterRhizospherePotential;}
	public void setWaterRhizospherePotential (double d) {waterRhizospherePotential = (float) d;}

	
  	public void drawNodes() {
		System.out.println("drawNodes cell="+getVoxelRooted().getCell().getId()+" node="+this);
		if (nodeColonised == null) return;
		for (Iterator v = nodeColonised.iterator (); v.hasNext ();) {
			SafeRootNode node = (SafeRootNode) v.next ();
			node.drawNodes();
		}
  	}
  	
	public String toString(){
		String str = "";
		str = "Node= "+planteType+" voxel="+voxelRooted.getId()+" z="+voxelRooted.getZ()+" FRDensity="+getFineRootsDensity();
		return str;
	}

}
