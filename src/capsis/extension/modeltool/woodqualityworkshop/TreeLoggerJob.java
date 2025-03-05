/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2013  Francois de Coligny, Mathieu Fortin
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE. See the GNU Lesser General Public 
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package capsis.extension.modeltool.woodqualityworkshop;

import jeeb.lib.util.Command;
import jeeb.lib.util.Log;
import jeeb.lib.util.SetMap;
import repicea.simulation.treelogger.TreeLogger;
import capsis.util.Job;

/**
 * The TreeLoggerJob class is a wrapper for a TreeLogger instance. 
 * @author Mathieu Fortin - January 2013
 */
public class TreeLoggerJob extends Job implements Command {

	private static final long serialVersionUID = 20100928L;
	
	public static final String ABORTED = "Aborted";
	
	private TreeLogger treeLogger;
	
	protected TreeLoggerJob(TreeLogger treeLogger) {
		super();
		this.treeLogger = treeLogger;
	}

	protected TreeLogger getTreeLogger() {return treeLogger;}
	
	@Override
	public void run() {
		try {
			treeLogger.run();
			closeJob(); 
		} catch (Exception e) {
			Log.println(Log.ERROR, "TreeLoggerJob.run()", 
					"Unable to perform the job " + toString(), e);
		}
	}
	
	
	private void closeJob() {
		setResult(treeLogger.getWoodPieces());
		execute();
	}

	@Override
	public int execute () {	// will be called when command is over
		if (treeLogger.isCorrectlyTerminated()) {
			setStatus("Finished");
		} else {
			setStatus(ABORTED);
		}
		// setResult ("out file is ...");
		finished ();	// implemented in Job, tell the "callBack" caller if one
		return 0;
	}

	@SuppressWarnings("unchecked")
	@Override
	public String toString () {
		int nbPieces = 0;
		Object o = getResult();
		if (o instanceof SetMap) {
			nbPieces = ((SetMap) o).allValues().size ();
		}
		return "Job "
				+getId ()+" "
				+getType ()+" "
				+getStatus ()+" "
				+"[" + nbPieces +" pieces] "
				+"starting date:"+getInitDate ()+" "
				+"last date:"+getLastChangeDate ();
	}

	@Override
	public String getType () {
		return treeLogger.getClass().getSimpleName();
	}

}
