/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2015 LERFoB AgroParisTech/INRA 
 * 
 * Authors: M. Fortin, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package capsis.util.extendeddefaulttype;

import java.text.NumberFormat;
import java.util.Map;
import java.util.concurrent.BlockingQueue;

import capsis.app.C4Script;
import capsis.kernel.Engine;
import capsis.kernel.Step;
import jeeb.lib.util.Log;
import repicea.io.REpiceaRecordSet;


public class ExtAssistedScriptWorkerThread extends Thread {

//	public static final int RUNNING = 0;
//	public static final int WAITING = 1;
//	public static final int INTERRUPTED = 2;
//	
//	private int status;
	private Exception exception;
	private ExtAssistedScriptJobManager oScriptManager;
	private C4Script oScriptEngine;
	private ExtExportTool oExport;
	private int iStratumId;
	
	protected ExtAssistedScriptWorkerThread(ExtAssistedScriptJobManager scriptManager) throws Exception {
		super("ScriptWorkerThread");
		oScriptManager = scriptManager;
		oScriptEngine = new C4Script(oScriptManager.getModel());
		
		Class<?> c = oScriptManager.getOriginalExportTool().getClass();
		oExport = (ExtExportTool) c.newInstance();
		start();
	}


	@Override
	public void run() {
		try {
	    	NumberFormat formatter  =  NumberFormat.getInstance();
	    	formatter.setMinimumFractionDigits(2);
	    	formatter.setMaximumFractionDigits(2);
	    	double startTime;
	    	double elapsedTime;
	    	
			BlockingQueue<Integer> oQueue = oScriptManager.getTaskQueue();
//			status = WAITING;
			while ((iStratumId = oQueue.take()) != ExtAssistedScriptJobManager.STOP_TOKEN && !isInterrupted()) {

				if (oScriptManager.isCancelRequested())
					break;												// if cancel is requested the loop is broken
				
				startTime = System.currentTimeMillis();
//				status = RUNNING;
				oScriptManager.initScript(this);			// synchronized
				Step s = oScriptEngine.evolve(oScriptManager.getEvolutionParameters());

				oExport.init(oScriptManager.getModel(), s, oScriptManager.getOriginalExportTool());	// the export tool inherits from the parent export tool in the manager
				oExport.setSelectedOptions(oScriptManager.getOriginalExportTool().getSelectedExportFormats());
				Map<Enum, REpiceaRecordSet> exportRecordSets = oExport.justGetRecordSetsForSelectedExportOptions();
				oScriptManager.export(exportRecordSets);				// synchronized
				Engine.getInstance().processCloseProject(oScriptEngine.getProject());			// the project is closed before the job is killed

				elapsedTime = (System.currentTimeMillis() - startTime) * 0.001;
				Log.println("QuebecMRNF", "Stratum " + iStratumId + " took " + formatter.format(elapsedTime) + " sec.");
				System.out.println("Stratum " + iStratumId + " took " + formatter.format(elapsedTime) + " sec.");

				oScriptManager.incrementProgressBar(); 		// synchronized
			}
			if (isInterrupted()) {
				throw new InterruptedException();
			} else {
				oQueue.add(ExtAssistedScriptJobManager.STOP_TOKEN);			// the stop token is put back into the queue to make sure other threads will have it
			}
//			status = WAITING;
		} catch (Exception e) {
			Log.println(Log.ERROR, "ExtScriptWorkerThread.run", "Error while running the QuebecScriptWorkerThread object", e);
//			status = INTERRUPTED;
			exception = e;
			oScriptManager.interruptAllTheads();
		}
	}
	
	protected C4Script getScriptEngine() {return oScriptEngine;}
	protected int getStratumId() {return iStratumId;}
//	protected int getStatus() {return status;};
	protected Exception getException() {return exception;}
	
}

