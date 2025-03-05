package capsis.extension.treelogger;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;


import jeeb.lib.util.Identifiable;
import repicea.simulation.treelogger.LogCategory;
import repicea.simulation.treelogger.LoggableTree;
import repicea.simulation.treelogger.WoodPiece;

/**
 * This class is a light version of the GPiece class. 
 * The GPieceLight objects are stored in a collection in 
 * the Tree-derived objects that implements the TreeProductsFromLogging
 * interface when the CarbonStorageINRA modeltool extension is used
 * @author Mathieu Fortin - January 2010
 */
public class GPieceLight extends WoodPiece implements Identifiable  {

	private static final long serialVersionUID = 20100805L;

	/**
	 * Number of pieces represented by this wood piece. This value is a within tree expansion factor due to 
	 * branching for example. The area expansion factor is handled by the WoodPiece class.
	 */
	private double withinTreeExpansionFactor = 1d;	
	
	
	/**
	 * Constructor based on the loggable tree. This constructor checks if the tree is a Numberable instance. If so, the number of
	 * tree is recorded in the expansionFactor variable.
	 * @param logCategory a TreeLogCategory instance
	 * @param id an Integer that represents the id of this piece
	 * @param tree a LoggableTree instance
	 * @param rank an Integer that is the rank of this log in the tree from the stump to the top
	 * @param withBark a boolean that indicates whether or not the piece was calculated with bark
	 * @param withPith a boolean that indicates whether or not the piece was calculated with pith
	 */
	protected GPieceLight(LogCategory logCategory, int id, LoggableTree tree, int rank, boolean withBark, boolean withPith) {
		super(logCategory, id, tree, rank, withBark, withPith);
	}
	
	
//	/**
//	 * Short constructor.
//	 * @param logCat a TreeLogCategory instance that defines the log class
//	 * @param volume the volume (m3) of the piece (double)
//	 * @param tree a LoggableTree instance, i.e. the tree from which the piece comes from
//	 */
//	public GPieceLight(LogCategory logCat, double volume, LoggableTree tree) {
//		super(logCat, tree);
//		setVolumeM3(volume);
//	}

	
	/**
	 * Official constructor
	 * @param gp
	 */
	public GPieceLight(GPiece gp) {
		super(gp.getLogCategory(),
				gp.getId(), 
				gp.getTreeFromWhichComesThisPiece(), 
				gp.getRank(), 
				gp.isWithBark(), 
				gp.isWithPith());

	}


	@Override
	public double getWeightedWoodVolumeM3() {
		return getWithinTreeExpansionFactor() * super.getWeightedWoodVolumeM3();
	}

	@Override
	public double getWeightedTotalVolumeM3() {
		return getWithinTreeExpansionFactor() * super.getWeightedTotalVolumeM3();
	}

	
	/**
	 * This method sets a within-tree expansion factor for branching for instance. By default, the factor is set to 1.
	 * @param withinTreeExpansionFactor the expansion factor (double)
	 */
	public void setWithinTreeExpansionFactor(double withinTreeExpansionFactor) {this.withinTreeExpansionFactor = withinTreeExpansionFactor;}
		
	
	/**
	 * This method returns the number of pieces represented by this wood piece. This value is a within tree expansion factor 
	 * due to branching for example. The area expansion factor is handled by the WoodPiece class.
	 * @return a double
	 */
	public double getWithinTreeExpansionFactor() {return withinTreeExpansionFactor;}

	
	/**
	 * This static method converts a collection of GPiece objects into a collection
	 * of GPieceLight objects. 
	 * @param pieces a collection of GPiece objects
	 * @return an ArrayList object which is a collection of GPieceLight objects derived 
	 * from the GPiece objects of collection oColl 
	 */
	public static Collection<GPieceLight> turnGPieceCollIntoGPieceLightColl(Collection<GPiece> pieces) {
		Collection<GPieceLight> retColl = new ArrayList<GPieceLight>();
		for (Iterator<GPiece> iter = pieces.iterator(); iter.hasNext();) {
			GPiece gp = iter.next();
			retColl.add(new GPieceLight(gp));
		}
		return retColl;
	}

	
}
