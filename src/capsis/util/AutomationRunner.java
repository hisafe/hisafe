/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
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
package capsis.util;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.task.Task;
import jeeb.lib.util.task.TaskManager;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.gui.DialogWithClose;
import capsis.gui.MainFrame;
import capsis.kernel.Engine;
import capsis.kernel.GModel;
import capsis.kernel.Project;
import capsis.kernel.automation.Automation;
import capsis.kernel.automation.AutomationSummary;

/**
 * @author sdufour
 *
 */
public class AutomationRunner {

	static protected JTextArea textArea = null; ;
	static protected DialogWithClose summaryDialog;

	public static void runInTasks(Automation automation, int nbTasks, 
			boolean summary, boolean keepProject ) throws Exception {

		Automation stopToken = new Automation("", "");

		List<Automation> l;
		l = automation.getVariation().getAutomations(automation);
		final GModel model = automation.instantiateModel();
		final AutomationSummary output = new AutomationSummary(automation, model);

		if(summary) {
			if(textArea == null || summaryDialog == null) {
				textArea = new JTextArea();
				textArea.setEditable(false);
				textArea.setSize(400, 400);
				summaryDialog = new DialogWithClose(MainFrame.getInstance (), 
						new JScrollPane( textArea ), Translator.swap("MainFrame.summary"), false, true);
				
			}

			textArea.setText(output.toString());
			summaryDialog.setVisible(true);

		}

		BlockingQueue<Automation> q = new LinkedBlockingQueue<Automation>();
		StatusDispatcher.print ("Preparing to run " + l.size() + " simulation(s)...");

		TaskManager tm = TaskManager.getInstance();
		final List<Task<?, ?>> tl = new ArrayList<Task<?, ?>>();

		// Create threads and queue
		for(int i=0; i<nbTasks; i++) {
			Task<?, ?> t = new TaskRunner(i, q, stopToken, output, textArea, keepProject) ;
			tl.add(t);
			tm.add(t);
		}

		// Add automations
		if(automation.repetition < 1) { automation.repetition = 1; }
		for(int i=0; i<automation.repetition; i++) {
			q.addAll(l);
		}
		q.add(stopToken);
		StatusDispatcher.print ("Running " + l.size() + " simulation(s)...");

		// create a swing worker to wait for other task to finish
		new SwingWorker() {

			@Override
			public Object construct() {
				// join all tasks
				for(Task<?, ?> t : tl) {
					try {
						t.get();
					} catch (Exception e) {} 
				}
				return null;
			} 

			public void finished() {
				output.getExtractor().callAfterAutomation(model, output);
			}

		};
	}



	public static void runInTread(Automation automation, int nbThreads, String summaryFile) throws Exception {

		GModel m = automation.instantiateModel();
		AutomationSummary output = new AutomationSummary(automation, m);

		List<Automation> l;
		List<Automation> l2 = new ArrayList<Automation>();
		
		l = automation.getVariation().getAutomations(automation);
		
		if(automation.repetition < 1) { automation.repetition = 1; }
		for(int i=0; i<automation.repetition; i++) {
			l2.addAll(l);
		}
		
		StatusDispatcher.print ("Plan : " +  automation.getVariation().plan);
		StatusDispatcher.print ("Preparing to run " + l.size() + " simulation(s)...");

		new MultiThreaded<Automation>(nbThreads, l2, new AutomationRunnable(), output);

		StatusDispatcher.print ("##### All simulation ended ######");

		output.getExtractor().callAfterAutomation(m, output);
		writeOutput(summaryFile, output);
	}


	private static void writeOutput(String summaryFile, AutomationSummary output) {

		BufferedWriter out = null;

		if(summaryFile != null && !summaryFile.equals("")) {
			try {
				out = new BufferedWriter(new FileWriter(summaryFile, true));
				out.write(output.toString());
			} catch (IOException e) {}
		}


		// close summary file
		try {
			if(out != null) {
				out.close();
			}
		} catch (IOException e) {

		} 

	}

	/** Simulation Thread */
	static public class AutomationRunnable implements RunnableWithParam<Automation, AutomationSummary> {

		static protected int index = 0;
		@Override
		public void run(Automation value, AutomationSummary extra) throws Exception {


			StatusDispatcher.print ("==> Start simulation in " + 
					Thread.currentThread().getName());

			
			Project p = value.run();
			p.setName(p.getName() + "_" + index++);
			extra.addLine(value, p.getModel(), p.getLastStep());

			StatusDispatcher.print ("Finish simulation in " + 
					Thread.currentThread().getName());
			StatusDispatcher.print ("=========================");

			// free memory
			Engine.getInstance().processCloseProject(p);

		}
	}


	static class TaskRunner extends Task<Object, Void> {

		private final BlockingQueue<Automation> queue;
		protected final Automation stopToken;
		protected final AutomationSummary output;
		protected final JTextArea summaryWidget;
		protected final boolean keepProject;
		static protected int index = 0;

		public TaskRunner(int id, BlockingQueue<Automation> q, Automation st, 
				AutomationSummary out, JTextArea ta, boolean keepProject) {

			super("Automation Task " + id);
			queue = q;
			stopToken = st;
			output = out;
			summaryWidget = ta;
			this.keepProject = keepProject;
		}

		@Override
		protected void doInEDTafterWorker() {

			if(summaryWidget !=null) {
				synchronized (summaryWidget) {
					String out =output.toString();
					summaryWidget.setText(out);
				}
			}

			StatusDispatcher.print (getName() + " finished");
		}

		@Override
		protected Object doInWorker() {

			try {

				
				while (true) {
					Automation value = queue.take();
					if(value == stopToken ) {
						queue.add(stopToken);
						break;
					}
					
					Project p = value.run();
					p.setName(p.getName() + "_" + index++);
					output.addLine(value, p.getModel(), p.getLastStep());

					if(summaryWidget !=null) {
						synchronized (summaryWidget) {
							String out = output.toString();
							summaryWidget.setText(out);
						}
					}



					if(keepProject) {
						// Update GUI
						SwingUtilities.invokeLater( new Runnable() {

							@Override
							public void run() {

								ProjectManager scenarioManager = ProjectManager.getInstance ();

								if(scenarioManager != null) {
									scenarioManager.update();
								}

							} } );
					} else {
						// close project and free memory
						Engine.getInstance().processCloseProject(p);
					}
					
				}
			}
			catch (Exception e) {
				Log.println(Log.ERROR, "Run Automation", "Error during execution", e);
				StatusDispatcher.print("Error in task " + getName() );
				MessageDialog.print(this, "Error during execution", e);

			}



			return null;
		}


	}


}

