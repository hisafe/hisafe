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
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.Collection;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.SwingWorker;

import capsis.app.CapsisExtensionManager;
import capsis.commongui.util.Helper;
import capsis.extension.DialogModelTool;
import capsis.gui.DProgressBar;
import capsis.gui.MainFrame;
import capsis.kernel.GModel;
import capsis.kernel.Step;
import jeeb.lib.util.Log;
import jeeb.lib.util.LogBrowser;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Question;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * A Wood Quality Workshop.
 *
 * @author D. Pont - December 2005
 */
@SuppressWarnings("serial")
public class WoodQualityWorkshop extends DialogModelTool implements ActionListener, ProgressBarFeatureDialog {
	
	static {
		Translator.addBundle("capsis.extension.modeltool.woodqualityworkshop.WoodQualityWorkshop");
	}

	// nb-07.08.2018
	//static public final String AUTHOR="D. Pont, F. de Coligny";
	//static public final String VERSION="1.2";

	private static final int INITIAL_WIDTH = 800;
	private static final int INITIAL_HEIGHT = 600;

	private Step step;
	private TreeLoggerManager logJobManager;

	private JScrollPane displayScrollPane ;
	private JTextArea displayArea;

	private JButton close;	// after confirmation
	private JButton seeLog;
	private JButton help;


	/**
	 * Phantom constructor for ExtensionManager.
	 */
	public WoodQualityWorkshop () {
		super();
	}
	
	@Override
	public void init(GModel m, Step s){
		
		try {
			step = s;

			String finishedJobsFileName = PathManager.getDir("tmp")
					+ File.separator
					+ "finishedLogJobs";
			logJobManager = new TreeLoggerManager (finishedJobsFileName);

			setTitle (getName ()+" - "+step.getCaption ());

			/*nf = NumberFormat.getInstance(Locale.ENGLISH);
			nf.setMinimumFractionDigits(2);
			nf.setMaximumFractionDigits(2);
			nf.setGroupingUsed (false);*/

			createUI ();
			print (Translator.swap ("WoodQualityWorkshop")+" - "+step.getCaption ());

			setSize (new Dimension (INITIAL_WIDTH, INITIAL_HEIGHT));
			setPreferredSize (getSize ());
			setModal (true);
			pack ();	// sets the size
			show();
			
		} catch (Exception exc) {
			Log.println (Log.ERROR, "WoodQualityWorkshop.c ()", exc.toString (), exc);
		}

	}

	/**	Extension dynamic compatibility mechanism.
	*	This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	*/
	static public boolean matchWith (Object referent) {
		try {
			ExtensionManager extMan = CapsisExtensionManager.getInstance();
			Collection<String> listOfPossibleLoggers; 
			listOfPossibleLoggers = extMan.getExtensionClassNames(CapsisExtensionManager.TREELOGGER, 
					referent);
			if (listOfPossibleLoggers.size() > 0) {
				return true;
			} else return false;
		} catch (Exception e) {
			Log.println (Log.ERROR, "WoodQualityWorkshop.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
//		return false;
	}

	@Override
	public String getName() {
		return Translator.swap("WoodQualityWorkshop.name");
	}

	@Override
	public String getAuthor() {
		return "D. Pont, F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("WoodQualityWorkshop.description");
	}

	@Override
	public String getVersion() {
		return "1.2";
	}

	/**	From ActionListener interface.
	*	Buttons management.
	*/
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource().equals(close)) {
			if (Question.ask(this, Translator.swap ("WoodQualityWorkshop.confirm"), Translator.swap ("WoodQualityWorkshop.confirmClose"))) {
				dispose ();
			}
			
		} else if (evt.getSource ().equals(seeLog)) {

			try {
				new LogBrowser(this);

			} catch (Throwable e) {
				Log.println(Log.ERROR, "WoodQualityWorkshop.actionPerformed ()", "Could not open Log", e);
				MessageDialog.print(this,
						Translator.swap("WoodQualityWorkshop.couldNotOpenLogUseAnEditorToCheckCapsisVarCapsisLog"));
			}
			
		} else if (evt.getSource ().equals(help)) {
			Helper.helpFor(this);
		}
	}

	/**	Called on Escape. Redefinition of method in AmapDialog : ask for user confirmation.
	*/
	protected void escapePressed () {
		if (Question.ask (MainFrame.getInstance (),
				Translator.swap ("WoodQualityWorkshop.confirm"), Translator.swap ("WoodQualityWorkshop.confirmClose"))) {
			dispose ();
		}
	}

	/**	User interface definition
	*/
	private void createUI () {

		// a jtabbed pane with tabs
		JTabbedPane tabs = new JTabbedPane ();

		// 1st tab. Trees
		TreeTab treeTab = new TreeTab (this, step, logJobManager);
		tabs.addTab (Translator.swap ("WoodQualityWorkshop.trees"), treeTab);

		// 2nd tab. Logs
		LogTab logTab = new LogTab (this, logJobManager);
		//LogTab logTab = new LogTab (this, logJobManager, sawJobManager);
		tabs.addTab (Translator.swap ("WoodQualityWorkshop.logs"), logTab);


		/*LinePanel part3 = new LinePanel ();
		saw = new JButton (Translator.swap ("WoodQualityWorkshop.saw"));
		saw.addActionListener (this);
		part3.add (saw);*/

		// 3. Display
		displayArea = new JTextArea ();
		displayArea.setEditable (false);		// for the moment
		//~ displayArea.setPreferredSize (new Dimension (500, 200));
		displayScrollPane = new JScrollPane (displayArea);
		//displayScrollPane.setPreferredSize (new Dimension (500, 150));


		// Control panel at the bottom: Close / Help
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		close = new JButton (Translator.swap ("Shared.close"));
		close.addActionListener (this);
		
		// fc+jsm-11.12.2023
		seeLog = new JButton(Translator.swap("WoodQualityWorkshop.seeLog"));
		seeLog.addActionListener(this);
		
		help = new JButton (Translator.swap ("Shared.help"));
		help.addActionListener (this);
		pControl.add (close);
		pControl.add(seeLog);
		pControl.add (help);
		// set close as default (see AmapDialog)
		close.setDefaultCapable (true);
		getRootPane ().setDefaultButton (close);

		// general layout
		JSplitPane mainPanel = new JSplitPane (JSplitPane.VERTICAL_SPLIT);
		mainPanel.setLeftComponent (tabs);
		mainPanel.setRightComponent (displayScrollPane);
		mainPanel.setOneTouchExpandable (true);
		mainPanel.setResizeWeight (1);
		mainPanel.setDividerLocation (450);	// divider location

		getContentPane ().add (mainPanel, BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
		
	}

	/**	Write in the little log in the model tool
	*/
	public void print (String msg) {
		displayArea.setEditable (false);

		displayArea.getCaret ().setVisible (false);
		displayArea.append (msg+"\n");

		int h = displayArea.getPreferredSize ().height;
		displayArea.scrollRectToVisible (new Rectangle (0, h, 10, 10));  // scrolls to see last line if needed
		//displayArea.revalidate ();
		//displayArea.repaint ();

		StatusDispatcher.print (msg);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void showProgressBar(Object task, SwingWorker job) {
		DProgressBar.showProgressBarWhileJob(this,
				job, 
				Translator.swap("WoodQualityWorkshop.progress"), 
				Translator.swap("WoodQualityWorkshop.logging"));
	}

}


