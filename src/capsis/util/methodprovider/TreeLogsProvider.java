package capsis.util.methodprovider;

import java.util.Collection;

import repicea.simulation.treelogger.LoggableTree;
import capsis.extension.treelogger.GPieceLight;

/**
 * This interface provides the methods to set and get a collection of GPiece objects
 * in a particular tree. 
 * @author Mathieu Fortin - January 2010
 */
public interface TreeLogsProvider extends LoggableTree {
	public void setTreeLogs(Collection<GPieceLight> oVec);	// this method set the collection of logs in the tree
	public Collection<GPieceLight> getTreeLogs();			// this method provides the collection of logs of the tree
	public void addLogToTree(GPieceLight lightPiece);		// this method makes it possible to add a particular log to the collection
}
