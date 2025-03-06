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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.Serializable;
import java.util.Date;

import jeeb.lib.util.Identifiable;
import jeeb.lib.util.Log;
import jeeb.lib.util.TicketDispenser;
import jeeb.lib.util.Translator;
import jeeb.lib.util.task.ToBeCalledBack;
import jeeb.lib.util.PathManager;

/**
 * A job with an id, status and result.
 *
 * @author F. de Coligny - december 2005
 */
public abstract class Job implements Runnable, Identifiable, Serializable {
	static private TicketDispenser jobIdDispenser;

	private int id;				// must be unique
	private String type;		// little classname, translated (ex: "Log2Job")
	private String status;		// running, finished
	private Object result;		// an object (ex: a collection of GLog)
	private Date initDate;			// in the constructor
	private Date lastChangeDate;	// changes each time status changes
	private transient ToBeCalledBack callBack;	// The callBack is not serialized

	/** Constructor
	*/
	public Job () {
		id = nextJobId ();
		type = Translator.swap (this.getClass ().getSimpleName ());
		status = "Running...";	// can also be "Finished"
		initDate = new Date ();
	}

	// Accessors
	public int getId () {return id;}
	public String getType () {return type;}
	public void setStatus (String status) {
		this.status = status;
		this.lastChangeDate = new Date ();
	}
	public String getStatus () {return status;}
	public void setResult (Object result) {this.result = result;}
	public Object getResult () {return result;}
	public Date getInitDate () {return initDate;}
	public Date getLastChangeDate () {return lastChangeDate;}

	/** "callBack" must be set outside the constructor
	*	If set, it will be called back at the end of the job
	*/
	public void setCallBack (ToBeCalledBack callBack) {this.callBack = callBack;}

	/**	Main method in the job
	*/
	public abstract void run ();

	/**	Subclass must call this method when the job is finished,
	*	"callBack" is called back (may be a JobManager)
	*/
	public void finished () {
		if (callBack != null) {callBack.callBack (this, null);}	// param: unused
	}

	/**	Manages job id incrementation between several capsis working session
	*	Uses a file to store and retrieve last job id.
	*/
	static public int nextJobId () {
		File file = null;
		try {
			file = new File (PathManager.getDir("etc")
					+File.separator+"lastJobId.wqw");
		} catch (Throwable t) {
			Log.println (Log.WARNING, "Job.nextJobId ()",
					"Error while accessing file etc/lastJobId.wqw", t);
		}

		if (jobIdDispenser == null) {
			jobIdDispenser = new TicketDispenser ();
			int lastTicketValue = 0;

			// Try to get the last value used in a previous session
			DataInputStream input = null;
			try {
				input = new DataInputStream (
						new BufferedInputStream (
						new FileInputStream (file)));
				lastTicketValue = input.readInt ();
			} catch (FileNotFoundException e) {
				// normal at first time, do nothing
			} catch (Throwable t) {
				Log.println (Log.WARNING, "Job.nextJobId ()",
						"Error while reading in etc/lastJobId.wqw", t);
			} finally {
				try {input.close ();} catch (Exception e) {}
			}
			jobIdDispenser.setCurrentValue (lastTicketValue);
		}

		// Get next value
		int value = jobIdDispenser.getNext ();

		// Try to memo this value for next time (even in case of dirty exiting)
		DataOutputStream output = null;
		try {
			output = new DataOutputStream (
					new BufferedOutputStream (
					new FileOutputStream (file)));
			output.writeInt (value);
		} catch (Throwable t) {
			Log.println (Log.WARNING, "Job.nextJobId ()",
					"Error while writing into etc/lastJobId.wqw", t);
		} finally {
			try {output.close ();} catch (Exception e) {}
		}

		return value;
	}

	public String toString () {
		return "Job "
				+id+" "
				+type+" "
				+status+" "
				+result+" "
				+"starting date:"+initDate+" "
				+"last date:"+initDate;

	}
}
