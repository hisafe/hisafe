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

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.border.TitledBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;

import jeeb.lib.util.Disposable;
import jeeb.lib.util.Identifiable;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.table.NonEditableTableModel;
import jeeb.lib.util.Question;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import repicea.simulation.treelogger.LoggableTree;
import repicea.simulation.treelogger.WoodPiece;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Tree;
import capsis.extension.treelogger.GPiece;
import capsis.gui.DialogWithClose;
import capsis.gui.MainFrame;
import capsis.util.Job;
import capsis.util.SelectionEvent;
import capsis.util.SelectionListener;
import capsis.util.SelectionSource;
import capsis.util.UpdateEvent;
import capsis.util.UpdateListener;

/**
 *	A Log tab with a list of finished log jobs and a table view.
 *	Select first a finished log job in the list.
 *	The TableView shows the logs in the currently selected job.
 *	When individual logs are selected in the TableView component, we are told
 *	and memo them (selectedLogs) for the next Saw job.
 *
 *	@author D. Pont, F. de Coligny - december 2005
 */
public class LogTab extends JPanel implements
ActionListener,			// buttons
SelectionListener, 		// selection in the TabView (for sawing)
Disposable, 			// fc - 8.2.2008
//Command,
UpdateListener, 		// a new job is finished, update job table
MouseListener, 			// fc - 1.3.2006 - job table management
ListSelectionListener {	// a job was just selected in the job table

	// Job table: at the top
	private static final Object[] JOB_TABLE_COLUMNS = {
		Translator.swap ("LogTab.jobId"),
		Translator.swap ("LogTab.type"),
		Translator.swap ("LogTab.status"),
		Translator.swap ("LogTab.result"),
		Translator.swap ("LogTab.initDate"),
		Translator.swap ("LogTab.lastDate")};

	// Log table: in the TableView under
	private static final String[] LOG_TABLE_COLUMNS = {
		Translator.swap ("LogTab.pieceId"),
		Translator.swap ("LogTab.treeId"),
		Translator.swap ("LogTab.rankInTree"),
		Translator.swap ("LogTab.numberOfPieces"),
		Translator.swap ("LogTab.numberOfDiscs"),
		Translator.swap ("LogTab.numberOfBranches"),
		Translator.swap ("LogTab.numberOfPithPoints"),
		Translator.swap ("LogTab.numberOfRadius"),
		Translator.swap ("LogTab.logType")
	};

	private WoodQualityWorkshop wqw;
	private TreeLoggerManager logJobManager;

	//private Collection jobs;	// the list of finished log jobs, tobe processed here
	private Map id2jobs;	// refreshed in makeLogTableModel
	private JTable jobTable;
	private JScrollPane jobScrollPane;
	private int sortColumn;
	private boolean ascending;

	private Map id2items;	// refreshed in makeLogTableModel
	private TableView tableView;

	private Collection candidateLogs;	// according to the current job
	private Collection selectedLogs;	// selected by user in the TableView

	private DateFormat dateFormat;	// fc - 1.3.2006

	private JButton saw;

	private SelectionSource source;	// fc - 8.2.2008


	/**	Constructor.
	 *	We listen to logJobManager
	 *	We may start jobs in sawJobManager
	 */
	public LogTab (WoodQualityWorkshop wqw, TreeLoggerManager logJobManager) {
		super ();

		dateFormat = DateFormat.getDateTimeInstance (DateFormat.SHORT, DateFormat.MEDIUM, Locale.getDefault ());

		this.wqw = wqw;
		this.logJobManager = logJobManager;
		logJobManager.addUpdateListener (this); // our update method will be called each time a job finishes

		// Remember the last sorted column + order
		this.sortColumn = Settings.getProperty ("capsis.woodqualityworkshop.logtab.jobtable.sort.column", 0);;
		this.ascending = Settings.getProperty ("capsis.woodqualityworkshop.logtab.jobtable.sort.order", false);

		id2jobs = logJobManager.getFinishedJobs ();	// From disk
		if (id2jobs == null) {id2jobs = new HashMap ();}

		candidateLogs = null;
		selectedLogs = null;
		id2items = new HashMap ();

		createUI ();

	}

	/**	Reset the table model according to the logs in the current job
	 */
	private NonEditableTableModel makeLogTableModel () {
		if (candidateLogs == null) {return new NonEditableTableModel (
				new Object[1][LOG_TABLE_COLUMNS.length], LOG_TABLE_COLUMNS);}	// first time: no job selected yet

		id2items = new HashMap ();
		Object[] logs = candidateLogs.toArray ();

		String[] colNames = LOG_TABLE_COLUMNS;
		Object[][] mat = new Object[logs.length][LOG_TABLE_COLUMNS.length];

		StringBuffer buffer = new StringBuffer ();

		String BLANK = "";
		String MARKED = "x";

		for (int i = 0; i < logs.length; i++) {
			/*if (Thread.interrupted ()) {
				System.out.println ("*** thread was interrupted in makeTable");
				return null;
			}*/
			GPiece p = (GPiece) logs[i];

			// this map is up to date with the table model under construction
			id2items.put (p.getId(), p);

//			int treeId = p.treeInfo.treeId;
			int treeId = ((Tree) p.getTreeFromWhichComesThisPiece()).getId();
			int numberOfDiscs = p.getDiscs().size ();
			int numberOfBranches = p.getBranches().size ();
			int numberOfPithPoints = p.getPithPoints().size ();

			mat[i][0] = new Integer (p.getId());
			mat[i][1] = new Integer (treeId);
			mat[i][2] = new Integer (p.getRank());
			mat[i][3] = new Double (p.getWithinTreeExpansionFactor());
			mat[i][4] = new Integer (numberOfDiscs);
			mat[i][5] = new Integer (numberOfBranches);
			mat[i][6] = new Integer (numberOfPithPoints);
			mat[i][7] = new Byte (p.getNumberOfRadius());
			// Fred M - 8.3.2006 : compatible with GPiece changes
			//~ mat[i][8] = p.logType;
			mat[i][8] = p.getLogCategory().getName();

			/*try {
				Numberable s = (Numberable) trees[i];
				mat[i][3] = new Integer (s.getNumber ());
			} catch (Exception e) {
				mat[i][3] = BLANK;
			}*/

		}

		// we use a non editable table model (browse only)
		// we add a sorter connected to the column headers
		NonEditableTableModel dataModel = new NonEditableTableModel (mat, colNames);

		return dataModel;
	}

	public void sawAction () {
		// fc - 1.3.2006 - tree selection required
		if (selectedLogs == null || selectedLogs.isEmpty ()) {
			MessageDialog.print (
					this, Translator.swap ("LogTab.logSelectionIsNeeded"));
			return;
		}

		// could add job to list...
		/*new SawJob (...
				selectedLogs,
				null, 	// String workingDirectory,	// ignored if null
				false, 	// boolean waitForCommandTermination,
				true, 	// boolean interactive,
				this	// Command caller
		).run();*/
		// ask job id...
		String logIds = "";
		for (Iterator i = selectedLogs.iterator (); i.hasNext ();) {
			logIds += ((GPiece) i.next ()).getId ()+" ";
		}
		StatusDispatcher.print ("Saw job launched for log: "+logIds+"... [under development]");

	}

	// When a job is terminated, it calls this method
	/*	public void execute () {
		// could update job list...
		StatusDispatcher.print ("Job finished");
	}*/

	// delete job rows in the job table, tell the job manager to forget them
	// fc - 1.3.2006
	private void deleteRows () {
		int[] indices = jobTable.getSelectedRows ();
		if (!Question.ask (MainFrame.getInstance (), 
				Translator.swap ("Shared.confirm"), Translator.swap ("LogTab.deleteTheseRows")
				+" ("
				+indices.length
				+" "
				+Translator.swap ("LogTab.rows")
				+") "
		)) {return;}

		Collection keys = new ArrayList ();
		for (int i = 0; i < indices.length; i++) {
			int numRow = jobTable.convertRowIndexToModel(indices[i]);


			int jobId = (Integer) jobTable.getModel ().getValueAt (numRow, 0);
			Job j = (Job) id2jobs.get (jobId);
			logJobManager.deleteFinishedJob (j);
		}

		id2jobs = logJobManager.getFinishedJobs ();	// From disk
		if (id2jobs == null) {id2jobs = new HashMap ();}
		jobScrollPane.getViewport ().setView (makeJobTable ());
		tableView.setTableModel (null, null);

	}


	// inspect job(s)
	// fc - 1.3.2006
	private void inspect () {
		int[] indices = jobTable.getSelectedRows ();
		Object target = null;

		if (indices.length == 1) {
			int jobId = (Integer) jobTable.getModel ().getValueAt (indices[0], 0);
			Job j = (Job) id2jobs.get (jobId);
			target = j;
		} else {
			Collection c = new ArrayList ();
			for (int i = 0; i < indices.length; i++) {
				int numRow = jobTable.convertRowIndexToModel(indices[i]);
				int jobId = (Integer) jobTable.getModel ().getValueAt (numRow, 0);
				Job j = (Job) id2jobs.get (jobId);
				c.add (j);
			}		
			target = c;
		}

		JComponent inspector = Tools.getIntrospectionPanel (target);
		JScrollPane sp = new JScrollPane (inspector);
		DialogWithClose dlg = new DialogWithClose (this, sp, Translator.swap ("LogTab.JobResults"), false);

	}


	/**	From ActionListener interface.
	 *	Buttons management.
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (saw)) {
			sawAction ();
		} else if (evt.getSource () instanceof JMenuItem) {
			JMenuItem i = (JMenuItem) evt.getSource ();
			if (i.getMnemonic () == JobPopup.DELETE_ROW) {
				System.out.println ("delete");
				deleteRows ();
			} else if (i.getMnemonic () == JobPopup.INSPECT) {
				System.out.println ("inspect");
				inspect ();
			}
		}
	}

	/**	Disposable
	 */
	public void dispose () {
		System.out.println ("LogTab.dispose ()...");
		try {
			source.removeSelectionListener (this);
		} catch (Exception e) {}	// does not matter very much
	}

	/**	SelectionListener interface
	 *	Triggered by TableView when user selects in the table
	 */
	public void sourceSelectionChanged (SelectionEvent e) {	// fc - 8.2.2008
		//~ params xere (Object source, Collection newSelection)
		//~ selectedLogs = newSelection;
		source = e.getSource ();
		selectedLogs = source.getSelection ();	// fc - 8.2.2008
	}

	/**	When JobManager is told a job is over, it tells us here
	 */
	public void sourceUpdated (UpdateEvent e) {		// fc - 23.11.2007
		update (e.getSource (), e.getParam ());		// fc - 23.11.2007
	}												// fc - 23.11.2007

	public void update (Object source, Object param) {
		//		System.out.println ("LogTab.update (): "+param);
		Job job = (Job) param;
		if (job.getStatus ().equals(TreeLoggerJob.ABORTED)) {
			Runnable doRun = new Runnable() {
				@Override
				public void run () {
					JOptionPane.showMessageDialog(LogTab.this, 
							Translator.swap ("TreeTab.errorWhileRunningJob"), 
							Translator.swap ("TreeTab.nbTreesWarningTitle"), 
							JOptionPane.ERROR_MESSAGE);
				}
			};
			SwingUtilities.invokeLater(doRun);
			return;
		}
		id2jobs.put (job.getId (), job);	// param is the finished job
		jobScrollPane.getViewport ().setView (makeJobTable ());
		wqw.print ("Job "+job.getId ()+" finished");

	}

	/**	User interface definition
	 */
	private void createUI () {

		// 1. up contains the table of finished log jobs
		JPanel up = new JPanel (new BorderLayout ());
		up.setBorder (new TitledBorder (Translator.swap (
		"LogTab.resultOfLogJobs")));

		jobScrollPane = new JScrollPane (makeJobTable ());
		up.add (jobScrollPane, BorderLayout.CENTER);

		// 2. middle contains the TableView (a table + an object viewer selector)
		// and the action buttons: Saw...
		JPanel middle = new JPanel (new BorderLayout ());
		middle.setBorder (new TitledBorder (Translator.swap (
		"LogTab.logsOfTheSelectedJob")));

		// 2.1 table view
		tableView = new TableView (this, makeLogTableModel (),
				id2items);
		middle.add (tableView, BorderLayout.CENTER);

		// 2.2 action buttons: Saw...
		JPanel down = new JPanel (new BorderLayout ());
		down.setBorder (new TitledBorder (Translator.swap (
		"LogTab.launchJobsForSelection")));

		LinePanel l3 = new LinePanel ();
		saw = new JButton (Translator.swap ("LogTab.saw"));
		saw.addActionListener (this);
		l3.add (saw);

		l3.addGlue ();
		down.add (l3, BorderLayout.CENTER);

		// aux is for organization
		JPanel aux = new JPanel (new BorderLayout ());
		aux.add (middle, BorderLayout.CENTER);
		aux.add (down, BorderLayout.SOUTH);

		// General layout
		JSplitPane mainPanel = new JSplitPane (JSplitPane.VERTICAL_SPLIT);
		mainPanel.setLeftComponent (up);
		mainPanel.setRightComponent (aux);
		mainPanel.setOneTouchExpandable (true);
		mainPanel.setDividerLocation (100);	// divider location

		setLayout (new GridLayout (1, 1));
		add (mainPanel);
	}



	/**	Maintains up to date the Job table
	 *	If a new job is finished, update the table
	 */
	public JTable makeJobTable () {
		jobTable = new JTable ();
		try {
			int nRows = 0;
			DefaultTableModel m1 = new NonEditableTableModel (JOB_TABLE_COLUMNS, nRows);
			Object[] row = new Object[JOB_TABLE_COLUMNS.length];
			for (Iterator i = id2jobs.values ().iterator (); i.hasNext ();) {
				Job j = (Job) i.next ();

				String result = "";		// fc - 1.3.2006
				if (j.getResult () == null) {
					result = "null";
				} else if (j.getResult () instanceof Collection) {
					Collection c = (Collection) j.getResult ();
					if (c.size () == 0) {
						result = Translator.swap ("LogTab.noLogs");
					} else {
						result = ""+c.size ()+" "+Translator.swap ("LogTab.logs");
					}
				} else {
					result = ""+j.getResult ();	// fc - 16.3.2006
				}

				row[0] = j.getId ();		// fc - 29.1.2004
				row[1] = j.getType ();
				row[2] = j.getStatus ();
				row[3] = result;			// fc - 1.3.2006
				row[4] = dateFormat.format (j.getInitDate ());
				row[5] = dateFormat.format (j.getLastChangeDate ());
				m1.addRow (row);
			}

			jobTable.setModel (m1);
			jobTable.setAutoCreateRowSorter(true);

			jobTable.addMouseListener (this);	// fc - 1.3.2006
			jobTable.setRowSelectionAllowed (true);
			//~ jobTable.setAutoResizeMode (JTable.AUTO_RESIZE_OFF);
			//~ jobTable.setSelectionMode (ListSelectionModel.SINGLE_SELECTION);
			jobTable.setSelectionMode (ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);	// fc - 1.3.2006
			jobTable.getSelectionModel ().addListSelectionListener (this);


		} catch (Exception e) {
			Log.println (Log.ERROR, "LogTab.makeJobTable ()", "Exception (1) occured", e);
		}
		return jobTable;
	}


	/**	Called when selection changed in the job table
	 */
	public void valueChanged (ListSelectionEvent evt) {
		ListSelectionModel sm = (ListSelectionModel) evt.getSource ();

		// several reasons to abort
		if (sm.isSelectionEmpty ()) {return;}	// we are on the header
		//if (jobTable == null) {return;}	// table is null

		// several selections, reset table view - fc - 1.3.2006
		if (jobTable.getSelectedRowCount () > 1) {
			tableView.setTableModel (null, null);
			return;
		}

		int numRow = jobTable.getSelectedRow ();
		numRow = jobTable.convertRowIndexToModel(numRow);
		int itemId = ((Integer) jobTable.getModel ().getValueAt (numRow, 0)).intValue ();
		Identifiable item = (Identifiable) id2jobs.get (itemId);

		Job j = (Job) item;

		
		Map<LoggableTree, Collection<WoodPiece>> map = (Map<LoggableTree, Collection<WoodPiece>>) j.getResult();
		Collection<WoodPiece> coll = new ArrayList<WoodPiece>();
		for (Collection<WoodPiece> subColl : map.values()) {
			coll.addAll(subColl);
		}
		
		// the result of this job is a collection of GLog(s)
//		candidateLogs = ((SetMap) j.getResult ()).allValues();
		candidateLogs = coll;

		// update TabView
		tableView.setTableModel (makeLogTableModel (), id2items);
	}

	/**	From MouseListener interface.
	 */
	public void mouseClicked (MouseEvent mouseEvent) {}

	/**	From MouseListener interface.
	 */
	public void mousePressed (MouseEvent m) {
		Object obj = m.getSource ();
		if (obj instanceof JTable) {
			JTable table = (JTable) obj;

			//~ if (m.isPopupTrigger ()) {	// fc - 4.2.2003 (failed under windows)
			if ((m.getModifiers () & InputEvent.BUTTON3_MASK) == InputEvent.BUTTON3_MASK) {

				int x = m.getX ();
				int y = m.getY ();
				int row = table.rowAtPoint (new Point (x, y));

				// Ensure some selection was made
				if (!table.isRowSelected (row)) {
					table.getSelectionModel ().setSelectionInterval (row, row);
				}

				if (table.equals (jobTable)) {
					JPopupMenu popup = new JobPopup (jobTable, this);
					popup.show (m.getComponent (), m.getX (), m.getY ());
				}
			}
		}				
	}

	/**	From MouseListener interface.
	 */
	public void mouseReleased (MouseEvent mouseEvent) {}

	/**	From MouseListener interface.
	 */
	public void mouseEntered (MouseEvent mouseEvent) {}

	/**	From MouseListener interface.
	 */
	public void mouseExited (MouseEvent mouseEvent) {}

}


