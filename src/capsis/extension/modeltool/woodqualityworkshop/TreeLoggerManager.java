/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package capsis.extension.modeltool.woodqualityworkshop;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import jeeb.lib.util.Log;
import jeeb.lib.util.TicketDispenser;
import jeeb.lib.util.task.ToBeCalledBack;
import capsis.util.Job;
import capsis.util.UpdateEvent;
import capsis.util.UpdateListener;
import capsis.util.UpdateSource;

/**
 *	A Job Manager
 *	Manages subclasses of Job.
 *
 *	@author D. Pont, F. de Coligny - december 2005
 */
public class TreeLoggerManager implements ToBeCalledBack, UpdateSource {

	private String finishedJobsFileName;

	private Map runningJobs;
	private Map finishedJobs;
	private Collection listeners;
	
	private TicketDispenser jobIdDispenser;

	/**	
	 * Constructor.
	 * @param finishedJobsFileName a String which indicates the path of the privious serialized jobs.
	 */
	protected TreeLoggerManager (String finishedJobsFileName) {
		this.finishedJobsFileName = finishedJobsFileName;

		runningJobs = new HashMap ();
		finishedJobs = new HashMap ();
		listeners = new ArrayList ();

		restoreJobs ();
	}

	// Starts the job, tell it to call back when the task is over
	protected void start(TreeLoggerJob job) {
		runningJobs.put(job.getId (), job);
		job.setCallBack(this);
		System.out.println("JM: starting job " + job.getId ()+"...");
		job.run();
	}
	

	// called when a job is over
	@Override
	public synchronized void callBack(Object source, Object param) {
		Job job = (Job) source;
		System.out.println ("JM: job "+job.getId ()+" called back: finished");
		runningJobs.remove (job.getId ());	// fc - bug correction - 1.3.2006
		finishedJobs.put (job.getId (), job);

		saveJobs();

		// fc - 23.11.2007
		//~ updateListeners (job);	// tell the listeners something was finished
		UpdateEvent e = new UpdateEvent(this);
		e.setParam(job);
		fireUpdateEvent(e);
		// fc - 23.11.2007
	}

	// Some objects could like to know about the finished jobs
	// They should register as listener
	public void addUpdateListener (UpdateListener l) {listeners.add (l);}
	public void removeUpdateListener (UpdateListener l) {listeners.remove (l);}
	public void fireUpdateEvent (UpdateEvent event) {
	//~ public void fireUpdateEvent (Job newJob) {
		for (Iterator i = listeners.iterator (); i.hasNext ();) {
			UpdateListener l = (UpdateListener) i.next ();
			//~ l.update (this, newJob);	// source = this, param is the finished job
			l.sourceUpdated (event);	// source = this, param is the finished job
		}
	}
	public Collection<Object> getSelection () {return null;}	// fc - 23.11.2007

	// Accessors
	//
	protected Map getRunningJobs () {return runningJobs;}
	protected Map getFinishedJobs () {return finishedJobs;}

	/**
	 * This method saves the finished jobs into a file whose filename is in the 
	 * finishedJobsFileName member.
	 */
	private synchronized void saveJobs() {
		try {
			ObjectOutputStream out = new ObjectOutputStream (
					new BufferedOutputStream (
					new FileOutputStream (finishedJobsFileName)));
			out.writeObject (finishedJobs);
			out.close ();	// also flushes output
		} catch (java.io.IOException exc) {
			Log.println (Log.ERROR, "JobManager.saveJobs ()",
					"Unable to write finished jobs to disk."
					+" Target file = "+finishedJobsFileName, exc);
		}
	}

	private synchronized void restoreJobs () {
		finishedJobs = new HashMap ();
		runningJobs = new HashMap ();

		try {
			ObjectInputStream in = new ObjectInputStream (
					new BufferedInputStream (
					new FileInputStream (finishedJobsFileName)));
			finishedJobs = (HashMap) in.readObject ();
			in.close ();
		} catch (java.io.FileNotFoundException exc) {
			Log.println (Log.WARNING, "JobManager.restoreJobs ()",
					"File not found: "+finishedJobsFileName);
		} catch (Exception exc) {
			Log.println (Log.ERROR, "JobManager.restoreJobs ()",
					"Error while trying to read jobs. Source file = "
					+finishedJobsFileName, exc);
		}
	}

	// fc - 1.3.2006
	protected void deleteFinishedJob (Job job) {
		System.out.println ("JM: job "+job.getId ()+" being deleted");
		finishedJobs.remove (job.getId ());
		saveJobs ();
	}

}


