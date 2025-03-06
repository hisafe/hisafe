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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import javax.swing.SwingWorker;

import capsis.kernel.Engine;
import capsis.kernel.EvolutionParameters;
import capsis.kernel.Project;
import jeeb.lib.util.Log;
import repicea.io.REpiceaRecordSet;
import repicea.io.tools.REpiceaExportTool;


@SuppressWarnings({"rawtypes", "serial"})
public class ExtAssistedScriptJobManager extends SwingWorker<Object, Object> {

	protected static List<Enum> ExportOptions;
	
	static class ScriptJobManagerExportTool extends REpiceaExportTool implements Runnable {
		static class InternalWorker extends InternalSwingWorkerForRecordSet {

			final Object lock = new Object();
			boolean jobRunning;
			
			protected InternalWorker(Enum arg0, REpiceaRecordSet arg1) {
				super(arg0, arg1);
				jobRunning = true;
			}

			@Override
			protected void doThisJob() throws Exception {
				synchronized(lock) {
					while (jobRunning) {
						lock.wait();
					}
				}
			}
			
			
			protected void terminate() {
				synchronized(lock) {
					jobRunning = false;
					lock.notify();
				}
			}
		}
		
		
		private final Map<Enum, REpiceaRecordSet> recordSetMap;
		private final List<InternalWorker> workers;
		
		ScriptJobManagerExportTool(ExtAssistedScriptParameters scriptParms, ExtExportTool originalTool, boolean saveFileEnabled) throws Exception {
			super();
			recordSetMap = new HashMap<Enum, REpiceaRecordSet>();
			workers = new ArrayList<InternalWorker>();
			setSaveFileEnabled(saveFileEnabled);
			if (saveFileEnabled) {
				setFilename(scriptParms.getExportTool().getFilename());
			}
			setMultipleSelection(originalTool.isMultipleSelectionEnabled());
			setSelectedOptions(scriptParms.getExportTool().getSelectedExportFormats());
		}
		
		/*
		 * For extended visibility (non-Javadoc)
		 * @see repicea.io.tools.REpiceaExportTool#setSaveFileEnabled(boolean)
		 */
		@Override
		protected void setSaveFileEnabled(boolean bool) {
			super.setSaveFileEnabled(bool);
		}

		@Override
		protected List<Enum> defineAvailableExportOptions() {
			return ExportOptions;
		}

		@Override
		protected InternalSwingWorkerForRecordSet instantiateInternalSwingWorkerForRecordSet(Enum selectedOption, REpiceaRecordSet recordSet) {
			if (!recordSetMap.containsKey(selectedOption)) {
				recordSetMap.put(selectedOption, recordSet);
			}
			InternalWorker worker = new InternalWorker(selectedOption, recordSet);
			workers.add(worker);
			return worker;
		}

		@Override
		public void run() {
			try {
				exportRecordSets();
			} catch (Exception e) {
				System.out.println("The ScriptJobManagerExportTool has crashed!");
			}
		}

		protected void process(Map<Enum, REpiceaRecordSet> recordSets) {
			for (Enum key : recordSets.keySet()) {
				REpiceaRecordSet recordSet = recordSetMap.get(key);
				REpiceaRecordSet incomingRecordSet = recordSets.get(key);
				recordSet.addAll(incomingRecordSet);
			}
		}
		
		protected void close() {
			for (InternalWorker worker : workers) {
				worker.terminate();
			}
		}
		
//		@Override
//		public void setSelectedOptions(Set<Enum> selectedOptions) throws Exception {
//			if (selectedOptions.size() > 1) {
//				throw new InvalidParameterException("Only one option can be specified at a time!");
//			} else {
//				super.setSelectedOptions(selectedOptions);
//			}
//		}
	}
	
	
	public static final int STOP_TOKEN = Integer.MIN_VALUE;
	
	private ExtModel oModel;
	private ExtInitialParameters oInitialParameters;
	private EvolutionParameters oEvolutionParameters;
	private int iNumberStrataProcessed;
	private int m_iNbThreadsToLunch;
	private boolean isCancelRequested;
	private boolean isCorrectlyTerminated;
	private String endReport;
	private final ExtExportTool originalExportTool;
	private ScriptJobManagerExportTool exportTool;
	private ArrayList<ExtAssistedScriptWorkerThread> oVecJob;
	private BlockingQueue<Integer> oTaskQueue;
	private double progressFactor;
	private Thread exportThread;
	
	/**
	 * General constructor
	 */
	public ExtAssistedScriptJobManager(ExtAssistedScriptParameters scriptParms, boolean saveFileEnabled) throws Exception {
		oModel = scriptParms.getModel();
		originalExportTool = scriptParms.getExportTool();
		isCancelRequested = false;			// default value
		isCorrectlyTerminated = false;		// default value

		ExportOptions = scriptParms.getExportTool().getAvailableExportOptions();
		exportTool = new ScriptJobManagerExportTool(scriptParms, originalExportTool, saveFileEnabled);
		exportThread = new Thread(exportTool, "Script Export Thread");
		exportThread.start();
		
		oInitialParameters = scriptParms.getInitialParameters();
		progressFactor = (double) 100d / this.getInitialParameters().getRecordReader().getGroupList().size();

		oTaskQueue = new LinkedBlockingQueue<Integer>();
		
		for (int i = 0; i < oInitialParameters.getRecordReader().getGroupList().size(); i++) {
			oTaskQueue.add(i);
		}
		
		oTaskQueue.add(ExtAssistedScriptJobManager.STOP_TOKEN);	// that token indicates to the thread they have to stop;
		
		oEvolutionParameters = scriptParms.getEvolutionParameters();
		oVecJob = new ArrayList<ExtAssistedScriptWorkerThread>();

		if (scriptParms.isEnableMultiThreading()) {
			m_iNbThreadsToLunch = ExtSimulationSettings.NB_THREADS;
		} else {
			m_iNbThreadsToLunch = 1;
		}
	}

	
	void interruptAllTheads() {
		for (Thread t : oVecJob) {
			t.interrupt();
		}
	}
	
	/**
	 * This method ensures the synchronization between the initialization of the stands of the different threads.
	 * @param oScript
	 * @throws Exception
	 */
	protected synchronized void initScript(ExtAssistedScriptWorkerThread oScript) throws Exception {
		try {
			oInitialParameters.getRecordReader().setSelectedGroupId(oScript.getStratumId());
			oScript.getScriptEngine().init(oInitialParameters);
		} catch (Exception e) {
			Log.println(Log.ERROR, "ArtScriptJobManager.initScript", "Error while setting the initial parameters in the script", e);
			throw e;
		}
	}
	
	protected ExtInitialParameters getInitialParameters() {return oInitialParameters;} 
	protected EvolutionParameters getEvolutionParameters() {return oEvolutionParameters;}
	protected ExtModel getModel() {return oModel;}
	protected BlockingQueue<Integer> getTaskQueue() {return oTaskQueue;}
	
	protected synchronized void export(Map<Enum, REpiceaRecordSet> recordSets) throws Exception {
		try {
			exportTool.process(recordSets);
		} catch (Exception e) {
			Log.println(Log.ERROR, "ScriptJobManager.export", "Error while saving record set", e);
			throw e;
		}
	}


	public void processStrata() throws Exception {
		iNumberStrataProcessed = 0;

		try {
			for (int i = 0; i < m_iNbThreadsToLunch; i++) {
				oVecJob.add(new ExtAssistedScriptWorkerThread(this));
			}
			
			for (ExtAssistedScriptWorkerThread t : oVecJob) {
					t.join();
			}

			isCorrectlyTerminated = true;

			for (ExtAssistedScriptWorkerThread t : oVecJob) {
				if (t.getException() != null) {
					isCorrectlyTerminated = false;
					if (t.getException().toString() != null) { 	// if the thread got an exception
						throw t.getException();					// then the exception is thrown
					}
				}
			}
			exportTool.close();
			exportThread.join();
		} catch (Exception e) {
			isCorrectlyTerminated = false;
			exportTool.close();
			throw e;
		} 
	}
	
	
	@Override
	public Object doInBackground() {

		try {
			this.oModel.initLogFilesDataValidation();
			processStrata();
			return true;
		} catch (Exception e) {
			endReport = e.getClass().getSimpleName();
			Log.println(Log.ERROR, "ExtAssistedScriptJobManager.run", "Error while managing the threads", e);
			e.printStackTrace();
			return e;
		}
	}		

	
	@Override
	public void done() {
		try {
			if (oVecJob.size()!=0) {
				for (int i = oVecJob.size() - 1; i >= 0; i--) {
					ExtAssistedScriptWorkerThread oWorker = oVecJob.get(i);
					Project project = oWorker.getScriptEngine().getProject();
					if(project.getRoot().getScene() != null){
						Engine.getInstance().processCloseProject(project);			// the project is closed before the job is killed
					}
					oVecJob.remove(oWorker);
				}
			}
		} catch (Exception e) {
			Log.println(Log.ERROR, "ArtScriptJobManager.run", "Error while cleaning the job vector", e);
		}
	}
	
	/**
	 * This method is called from the worker threads. They successively increment the progress.
	 */
	protected synchronized void incrementProgressBar() {
		iNumberStrataProcessed++;
		setProgress((int) (iNumberStrataProcessed * progressFactor));
	}
	
	protected boolean isCancelRequested() {return isCancelRequested;}
	protected void setCancelRequested(boolean b) {isCancelRequested = b;}
	protected boolean hasBeenNormallyTerminated() {return isCorrectlyTerminated;}
	protected String getEndReport() {return endReport;}
	protected ExtExportTool getOriginalExportTool() {return originalExportTool;}

	/**
	 * This method returns the record set corresponding to the enum variable. IMPORTANT: this record set is empty
	 * if the export tool is allowed to save on disk. The method setSaveFileEnabled(false) should be called on the
	 * REpiceaExportTool object before calling this method. 
	 * @param selectedOption
	 * @return a REpiceaRecordSet instance
	 */
	public REpiceaRecordSet getRecordSet(Enum selectedOption) {return exportTool.recordSetMap.get(selectedOption);}
}

