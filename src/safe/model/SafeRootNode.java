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
 * @author Isabelle Lecomte - November 2005 updated September 2008 by Grégoire Talbot
 */
public class SafeRootNode implements Serializable {

	private int planteType;					//1=crop 2=tree
	private SafeVoxel voxelRooted; 			// Reference of the rooted voxel
	private SafeRootNode nodeParent; 		// Reference of the single parent node
	private Collection nodeColonised; 		// Reference of the collection of colonised nodes

	private int colonisationDate; 			// Date of first colonisation
	private float treeDistance; 			// Distance to the tree trunc following topology path (m)
	private float fatherDistance; 			// Distance from father node // GT 17/07/2008
	private int colonisationDirection; 		// direction of the colonization from father node // gt - 27.03.2009
											// 0 is for x+, 1 for x-, 2 for y+, 3 for y-, 4 for z+, 5 for z-
	private float effectiveDistance;		//effective distance from tree (m) 
	
	private float coarseRootBiomass; 		// kg C
	private float coarseRootTargetBiomass; 	// kg C : used for carbon allocation between FR and CR
	private float fineRootCost; 			// kg of CoarseRoot m-1 of fine roots.
	private float fineRootDensity; 			// m m-3
	private float fineRootTotalInvestment;
	
	private float phiPf;						//cm2 day-1
	private float waterRhizospherePotential; 	// cm
	private float nitrogenUptake; 				// kg N
	private float waterUptake; 					// liters
	

	public SafeRootNode (SafeVoxel voxelRooted, SafeRootNode nodeParent, int day, double fineRoots, SafeTree tree,  int direction) {

		this.planteType = 2;
		this.voxelRooted = voxelRooted;
		this.nodeParent = nodeParent;
		this.colonisationDate = day;
		this.coarseRootBiomass = 0;
		this.coarseRootTargetBiomass = 0;
		this.fineRootDensity = (float) fineRoots;
		this.fineRootTotalInvestment =  (float) fineRoots;
		this.nodeColonised = null;
		this.waterUptake = 0;
		this.nitrogenUptake = 0;
		this.fineRootCost = 0;

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

	public SafeRootNode (SafeVoxel voxelRooted, SafeRootNode nodeParent, int day, double fineRootDensity) {

		this.planteType = 1;	//Crop
		this.colonisationDirection = 4; 
		this.voxelRooted = voxelRooted;
		this.nodeParent = nodeParent;
		this.colonisationDate = day;
		this.coarseRootBiomass = 0;
		this.coarseRootTargetBiomass = 0;
		this.fineRootDensity = (float) fineRootDensity;
		this.fineRootTotalInvestment =  (float) fineRootDensity;
		this.nodeColonised = null;
		this.waterUptake = 0;
		this.nitrogenUptake = 0;
		this.fineRootCost = 0;
		
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
			for (Iterator v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised = (SafeRootNode) v.next ();
				nodeColonised.setDistances (distance); // recursivity
			}
		}
	}

	/**
	 * 
	 * compute the target coarse root biomass for all nodes 
	 *  and the total coarse root target biomass of the tree // GT 17/07/2008
	 */
	public double computeCoarseRootTargetBiomass (SafeTree tree, 
												double[] additionalRootLength, double cRWoodDensity,
												double cRWoodCarbonContent, double cRAreaToFRLengthRatio) {

		
		double fineRootLength = 0; // Kg C
		if (nodeColonised != null) {
			for (Iterator v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised = (SafeRootNode) v.next ();
				double toAdd = nodeColonised
						.computeCoarseRootTargetBiomass (tree, additionalRootLength, cRWoodDensity, cRWoodCarbonContent, cRAreaToFRLengthRatio);
				fineRootLength += toAdd;

			}
		}
		int v = this.getVoxelRooted ().getId () - 1;
		int treeIndex = tree.getId () - 1;
		double fineRootDensity = this.getVoxelRooted ().getTheTreeRootDensity (treeIndex);
		fineRootLength += fineRootDensity * this.getVoxelRooted ().getVolume () // m m-3 * m3
				* this.getTopologicalToEffDistance (); // voir les explication la dessus dans la methode computeEffDist
												// // gt - 30.03.2009

		
		if (additionalRootLength != null) fineRootLength += additionalRootLength[v] // m
				* this.getTopologicalToEffDistance ();

		
		double targetBiomass = fineRootLength * cRAreaToFRLengthRatio * this.getFatherDistance () // m x m2 m-1 x m = m3
								* cRWoodDensity * cRWoodCarbonContent; // kg.m-3 x kgC.kg-1 = kgC.m-3
		
		this.setCoarseRootTargetBiomass (targetBiomass);
		
		tree.addCoarseRootTotalTargetBiomass (Math.max (getCoarseRootBiomass(), targetBiomass));

		// Math.max(...) : if targetBiomass<biomass : the coarse root can't contribute to carbon
		// allocation
		// as a negative sink

		return fineRootLength;
	}

	/**
	 * methode pour calculer le prix en carbone (Kg) de la mise en place de nouvelles racines fines (en m) dans un voxel donné
	 * la methode est appelee uniquement sur firstRootNode dans safeTree, mais elle s'appelle recursivement pour faire le calcul sur l'ensemble du systeme racinaire
	 * la variable frCost donnéée en entrée correspond au cout en carbone des nouvelles racines fines sans compter le besoin potentiel d'investissement dans des racines de structure
	 * le reste de la methode calcule l'investissement necessaire en racines de structures pour mettre en place ces racines fines 
	 * pour ce calcul, on fera l'hypothese que les nouvelles racines fines (addFineRootLength) seront reparties uniformement sur tout le volume enracine par l'arbre (rootedVolume)
	 */
	public double[] computeFineRootCost (int treeIndex, double addFineRootLength, double rootedVolume,
			double cRWoodDensity, double cRWoodCarbonContent, double cRAreaToFRLengthRatio, double frCost) {

		// cout des racines fines en tant que telles 
		// ce cout sera incremente ensuite pour prendre en compte ce qu'elles impliquent comme investissement en racines de structures 
		// par la methode costDispatching
		
		addFineRootCost (frCost);

		
		double[] fineRootLength = {0.0, 0.0}; // old fine root length and additionalFineRootLength //  ces variables ne sont pas des longueurs de racines locales dans le voxel ou l'on se trouve, mais la somme des longueurs de racines fines qui en dependent, il faut pour cela ajouter les longueurs de racines dans tous les voxels colonises par la descendance du noeud present, d'ou la recursivite de la methode
		double newFineRootsDensity = addFineRootLength / rootedVolume;		// densite de nouvelles racines fines dans le voxel, sous l'hypothese que les nouvelles racines fines seront reparties uniformement sur tout le volume enracine par l'arbre (rootedVolume)

		if (nodeColonised != null) {
			for (Iterator v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised = (SafeRootNode) v.next ();
				double[] toAdd = nodeColonised
						.computeFineRootCost (treeIndex, addFineRootLength, rootedVolume, cRWoodDensity, cRWoodCarbonContent, cRAreaToFRLengthRatio, frCost);		// recursivite : la methode renvoie les valeurs de fineRootLength pour le noeud enfant
				fineRootLength[0] += toAdd[0];
				fineRootLength[1] += toAdd[1];
			}
		}
		double localFineRootLength = getFineRootDensity () * getVoxelRooted ().getVolume ()
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
				* getFatherDistance () * cRWoodDensity * cRWoodCarbonContent - getCoarseRootBiomass ());		 
																							
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

		this.addFineRootCost (totalCost * getTopologicalToEffDistance () / totalFineRoots);
		
		if (nodeColonised != null) {
			for (Iterator v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised = (SafeRootNode) v.next ();
				localFineRoots = newFineRootsDensity * nodeColonised.getVoxelRooted ().getVolume ()
						* nodeColonised.getTopologicalToEffDistance ();
				nodeColonised
						.costDispatching (treeIndex, totalCost, localFineRoots, totalFineRoots, newFineRootsDensity);
			}
		}
	}

	/**
	 * Growth of coarse root biomass in KG C : computing of carbon allocation between coarse roots
	 * 
	 * @author Gregoire Talbot. September 2008
	 */
	public void computeCoarseRootBiomass (int treeIndex, double carbonCoarseRootsIncrement,
			double coarseRootTotalBiomassImbalance, double treeCoarseRootBiomass) {

		if (nodeColonised != null) {
			for (Iterator v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised = (SafeRootNode) v.next ();
				nodeColonised
						.computeCoarseRootBiomass (treeIndex, carbonCoarseRootsIncrement, coarseRootTotalBiomassImbalance, treeCoarseRootBiomass);
			}
		}
		double proportion = 0;
		double coarseRootImbalance = Math.max (getCoarseRootTargetBiomass () - getCoarseRootBiomass (), 0);
		if (coarseRootTotalBiomassImbalance > carbonCoarseRootsIncrement) { // if coarse roots ask
																			// for carbon
			// Carbon is allocated to coarse roots segments
			// proportionnally to their carbon imbalance computed from fine root length
			proportion = coarseRootImbalance / coarseRootTotalBiomassImbalance;
		} else {
			// idem, but carbon is allocated to coarse roots segments
			// proportionnally to their biomass
			proportion = (coarseRootImbalance + (carbonCoarseRootsIncrement - coarseRootTotalBiomassImbalance)
					* getCoarseRootBiomass () / treeCoarseRootBiomass)
					/ carbonCoarseRootsIncrement;
		}

		double additionalCoarseRoot = proportion * carbonCoarseRootsIncrement;
		addCoarseRootBiomass (additionalCoarseRoot);
			// update coarse root biomass in rooted voxels
		if (nodeParent != null) {
			voxelRooted.addTreeCoarseRootBiomass (treeIndex, 0.5 * additionalCoarseRoot);			
			nodeParent.getVoxelRooted ().addTreeCoarseRootBiomass (treeIndex, 0.5 * additionalCoarseRoot);
		} else {
			voxelRooted.addTreeCoarseRootBiomass (treeIndex, additionalCoarseRoot);
		}
	}

	public double getDeeperSonDepth () {

		double deeperSonDepth = getVoxelRooted ().getSurfaceDepth () + getVoxelRooted ().getThickness ();

		if (nodeColonised != null) {
			for (Iterator v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised = (SafeRootNode) v.next ();
				double sonDeeperSonDepth = nodeColonised.getDeeperSonDepth ();
				deeperSonDepth = Math.max (deeperSonDepth, sonDeeperSonDepth);
			}
		}
		return deeperSonDepth;
	}

	/**
	 * Tree fine roots senescence (computed each day only if budbust has started) 
	 */
	
	public void computeRootSenescence (SafeTree tree, SafeInitialParameters settings) {

		int treeIndex = tree.getId()-1;
		
		SafeVoxel v = this.getVoxelRooted ();

		double carbonToDryMatter = 1d / tree.getTreeSpecies ().getWoodCarbonContent(); 
		double frCost = (1 / (carbonToDryMatter * tree.getTreeSpecies ().getSpecificRootLength () * 1000)); // m
																														// to
		double fineRootLifespan  =  tree.getTreeSpecies ().getFineRootLifespan  ();
		double nRemobFrac = tree.getTreeSpecies ().getRootNRemobFraction ();
		if (v.getIsSaturated ()) {
			fineRootLifespan =  tree.getTreeSpecies ().getFineRootAnoxiaLifespan  ();
			if (v.getSaturationDuration () > tree.getTreeSpecies ().getCoarseRootAnoxiaResistance ()) {
				boolean testAnoxia = true; 
				this.removeSonsRoots (1d, null, tree, settings, testAnoxia); 
			}
		}

		//IL 25-04-2018 changing rootAnoxiaHalfLife to fineRootLifespan 
		double localRootSenescence = 0;
		if (fineRootLifespan != 0) 
			localRootSenescence =  (1 / fineRootLifespan) * this.fineRootDensity; // m.m-3
		else System.out.println ("WARNING  fineRootLifespan = 0 !!!! ");
		
		this.fineRootDensity -= localRootSenescence;
		v.setTreeRootDensity (treeIndex, this.fineRootDensity);


		localRootSenescence *= v.getVolume () * frCost; // m.m-3 to KgC
		v.setTreeCarbonFineRootSen (treeIndex, localRootSenescence);

		tree.addCarbonFineRootSen (localRootSenescence);
		
		tree.setCarbonFineRoots (tree.getCarbonFineRoots () - localRootSenescence);

		
		// Pour le calcul de l'azote perdu par senescence des racines fines
		double nitrogenFineRootsSen = localRootSenescence
				* ((tree.getNitrogenFineRoots () / tree.getCarbonFineRoots ()));
		tree.addNitrogenLabile (nitrogenFineRootsSen * nRemobFrac);
		tree.setNitrogenFineRoots (tree.getNitrogenFineRoots () - nitrogenFineRootsSen);
		double nitrogenFineRootsLoss = nitrogenFineRootsSen * (1 - nRemobFrac);
		tree.addNitrogenFineRootSen (nitrogenFineRootsLoss);
		
		//Anoxia
		if (v.getIsSaturated ()) {
			tree.addCarbonFineRootSenAnoxia (localRootSenescence);
			tree.addNitrogenFineRootSenAnoxia (nitrogenFineRootsLoss);
		}
		

		
		v.setTreeNitrogenFineRootSen (treeIndex, nitrogenFineRootsLoss);
		v.addNitrogenRootResidu (nitrogenFineRootsLoss*1000);//kg to g
		
		// Ajout AQ pour la minéralisation des racines en profondeur:
		// double nCoarseRootSen = v.getNitrogenCoarseRootSen();
		// double totalNitrogenRootSen = nitrogenFineRootsLoss+nCoarseRootSen;

		if (nodeColonised != null) {
			for (Iterator it = this.getNodeColonised ().iterator (); it.hasNext ();) {
				SafeRootNode nodeColonised = (SafeRootNode) it.next ();
				nodeColonised.computeRootSenescence (tree, settings);
			}
		}
	}

	/**
	 * Tree fine roots senescence (when the tree died or is cut) 
	 */
	
	public void computeTotalRootSenescence (SafeTree tree, SafeInitialParameters settings) {

		int treeIndex = tree.getId()-1;
		
		SafeVoxel v = this.getVoxelRooted ();
		
		//RAZ before compute new values 
		v.setTreeCarbonFineRootSen(treeIndex, 0);
		v.setTreeCarbonCoarseRootSen(treeIndex, 0);
		v.setTreeNitrogenFineRootSen(treeIndex, 0);
		v.setTreeNitrogenCoarseRootSen(treeIndex, 0);
		
		
		double carbonToDryMatter = 1d / tree.getTreeSpecies ().getWoodCarbonContent(); 
		float frCost = (float) (1 / (carbonToDryMatter * tree.getTreeSpecies ().getSpecificRootLength () * 1000)); // m to KgC

		float nRemobFrac = (float) tree.getTreeSpecies ().getRootNRemobFraction ();

		//total roots are senescent
		float localRootSenescence =  (float) this.fineRootDensity; // m.m-3
		this.fineRootDensity = 0;
		v.setTreeRootDensity (treeIndex, 0);

		
		localRootSenescence *= v.getVolume () * frCost; // m.m-3 to KgC
		v.setTreeCarbonFineRootSen (treeIndex, localRootSenescence);

		
		// Pour le calcul de l'azote perdu par senescence des racines fines
		float nitrogenFineRootsSen = localRootSenescence
				* ((float) (tree.getNitrogenFineRoots () / tree.getCarbonFineRoots ()));

		float nitrogenFineRootsLoss = nitrogenFineRootsSen * (1 - nRemobFrac);

		v.setTreeNitrogenFineRootSen (treeIndex, nitrogenFineRootsLoss);
		v.addNitrogenRootResidu (nitrogenFineRootsLoss*1000); //kg to g 

		if (nodeColonised != null) {
			for (Iterator it = this.getNodeColonised ().iterator (); it.hasNext ();) {
				SafeRootNode nodeColonised = (SafeRootNode) it.next ();
				nodeColonised.computeTotalRootSenescence (tree, settings);
			}
		}
	}


	/**
	 * Compute the total rooted volume from this root node 
	 */
	public double computeRootedVolume () {
		double volume = 0;
		if (nodeColonised != null) {
			for (Iterator v = this.getNodeColonised ().iterator (); v.hasNext ();) {
				SafeRootNode nodeColonised = (SafeRootNode) v.next ();
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
	public void removeSonsRoots (double prop, SafeVoxel voxel, SafeTree tree, SafeInitialParameters settings, boolean testAnoxia) {
		
		int treeIndex = tree.getId()-1;
		double carbonToDryMatter = 1d / tree.getTreeSpecies ().getWoodCarbonContent(); 
		double frCost =  1 / (carbonToDryMatter * 1000 * tree.getTreeSpecies ().getSpecificRootLength ());

		if (prop > 0.5) prop = 1;

		if (nodeColonised != null) {
			for (Iterator it = this.getNodeColonised ().iterator (); it.hasNext ();) {
				SafeRootNode nodeSon = (SafeRootNode) it.next ();
				SafeVoxel voxelSon = nodeSon.getVoxelRooted ();
				if ((voxel == null) || (voxelSon.getId () == voxel.getId ())) { // if v not specified, all voxel colonized are removed.

					double carbonFineRootSenescence = prop * nodeSon.getFineRootDensity () * voxelSon.getVolume () * frCost;
					double nitrogenFineRootSenescence=  carbonFineRootSenescence * tree.getNitrogenFineRoots () / tree.getCarbonFineRoots ();
					voxelSon.setTreeCarbonFineRootSen (treeIndex, carbonFineRootSenescence);
					voxelSon.setTreeNitrogenFineRootSen (treeIndex, nitrogenFineRootSenescence);
					
					double newDensity = (1 - prop) * nodeSon.getFineRootDensity ();
					nodeSon.setFineRootDensity (newDensity);
					voxelSon.setTreeRootDensity (treeIndex, newDensity);
					
					//update TREE carbon and nitrogen pool 
					tree.addCarbonFineRootSen (carbonFineRootSenescence);
					tree.addNitrogenFineRootSen (nitrogenFineRootSenescence);				
					tree.setCarbonFineRoots (tree.getCarbonFineRoots () - carbonFineRootSenescence);
					tree.setNitrogenFineRoots (tree.getNitrogenFineRoots () - nitrogenFineRootSenescence);
					
					//AQ
					voxelSon.addNitrogenRootResidu (nitrogenFineRootSenescence*1000);//kg to g
					
					//in case of ANOXIA
					if ((testAnoxia) && (voxelSon.getIsSaturated ())) {
						tree.addCarbonFineRootSenAnoxia (carbonFineRootSenescence);
						tree.addNitrogenFineRootSenAnoxia (nitrogenFineRootSenescence);
					}
					
					
					
					//If all fine roots are removed, son roots have also to be killed (and coarse root also) 
					if (prop == 1) {
						//remove sons roots
						nodeSon.removeSonsRoots (1d, null, tree, settings, testAnoxia);
						
						double carbonCoarseRootSenescence = nodeSon.getCoarseRootBiomass();
						double nitrogenCoarseRootSenescence = carbonCoarseRootSenescence * tree.getNitrogenCoarseRoots () / tree.getCarbonCoarseRoots ();
						
						
						voxelSon.setTreeCoarseRootBiomass (treeIndex, 0);
						voxelSon.setTreeCarbonCoarseRootSen (treeIndex, carbonCoarseRootSenescence);
						voxelSon.setTreeNitrogenCoarseRootSen (treeIndex, nitrogenCoarseRootSenescence);
						
						//update TREE carbon and nitrogen pool 
						tree.addCarbonCoarseRootSen (carbonCoarseRootSenescence);
						tree.addNitrogenCoarseRootSen (nitrogenCoarseRootSenescence);
						tree.setCarbonCoarseRoots (tree.getCarbonCoarseRoots () - carbonCoarseRootSenescence);
						tree.setNitrogenCoarseRoots (tree.getNitrogenCoarseRoots () - nitrogenCoarseRootSenescence);
											
						//AQ
						voxelSon.addNitrogenRootResidu (nitrogenCoarseRootSenescence*1000);//kg to g
						
						if ((testAnoxia) && (voxelSon.getIsSaturated ())) {
							tree.addCarbonCoarseRootSenAnoxia  (carbonCoarseRootSenescence);
							tree.addNitrogenCoarseRootSenAnoxia (nitrogenCoarseRootSenescence);
						}
					
						
						it.remove ();
						tree.getFineRoots().getRootTopology ().remove (voxelSon);
					}
				}
			}
		}
	}

	public int getPlanteType() {return planteType;}

	public SafeVoxel getVoxelRooted () {return voxelRooted;}
	public void setVoxelRooted (SafeVoxel v) {voxelRooted = v;}

	public SafeRootNode getNodeParent () {return nodeParent;}
	public void setNodeParent (SafeRootNode v) {nodeParent = v;}

	public Collection getNodeColonised () {return nodeColonised;}
	public void setNodeColonised (Collection col) {nodeColonised = col;}
	public void addNodeColonised (SafeRootNode newColonisedNode) {
		if (nodeColonised == null) this.nodeColonised = new ArrayList ();
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

	public void setCoarseRootTargetBiomass (double b) {coarseRootTargetBiomass = (float) b;}
	public double getCoarseRootTargetBiomass () {return (double) coarseRootTargetBiomass;}

	public double getCoarseRootBiomass () {return (double) coarseRootBiomass;}
	public void addCoarseRootBiomass (double d) {coarseRootBiomass += (float) d;}
	public void setCoarseRootBiomass (double d) {coarseRootBiomass = (float) d;}
	
	public double getFineRootCost () {return (double) fineRootCost;}
	public void addFineRootCost (double c) {fineRootCost += (float) c;}
	public void setFineRootCost (double c) {fineRootCost = (float) c;}
	
	public double getFineRootDensity () {return (double) fineRootDensity;}
	public void setFineRootDensity (double d) {fineRootDensity = (float) d;}

	public double getFineRootLength () {
		return this.getFineRootDensity () * this.getVoxelRooted ().getVolume ();
	} 

	public double getFineRootTotalInvestment () {return (double) fineRootTotalInvestment;}
	public void setFineRootTotalInvestment (double fr) {fineRootTotalInvestment = (float) fr;}
	public void addFineRootTotalInvestment (double fr) {fineRootTotalInvestment += (float) fr;}
	

	
	public double getWaterUptake () {return (double) waterUptake;}
	public void addWaterUptake (double d) {waterUptake += (float) d;}
	public void setWaterUptake (double d) {waterUptake = (float) d;}

	public double getNitrogenUptake () {return (double) nitrogenUptake;}
	public void addNitrogenUptake (double v) {nitrogenUptake += (float) v;}
	public void setNitrogenUptake (double v) {nitrogenUptake = (float) v;}

	public double getWaterEfficiency () {
		if ((getFineRootLength() > 0) && (getWaterUptake() > 0))
			return getWaterUptake()/getFineRootLength();
		else return 0;
	}
	public double getNitrogenEfficiency () {
		if ((getFineRootLength() > 0) && (getNitrogenUptake() > 0))
			return getNitrogenUptake()/getFineRootLength();
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
  	
/*	public String toString(){
		String str = "";
		str = "Node= "+planteType+" voxel="+voxelRooted.getId()+" z="+voxelRooted.getZ()+" fR="+fineRootDensity+" np="+nitrogenUptake;
		return str;
	}*/
}
