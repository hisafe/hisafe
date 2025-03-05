/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2003  Francois de Coligny
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

package capsis.extension.generictool.memoryview;

import java.awt.FlowLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.NumberFormat;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.Timer;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;
import capsis.util.SmartFlowLayout;

/**
 * Displays the amount of free memory in the running application.
 *
 * @author Netbeans (adaptated by F. de Coligny april 2000)
 */
//public class MemoryView extends AbstractGenericTool {
public class MemoryView extends AmapDialog {

	static {
		Translator.addBundle("capsis.extension.generictool.memoryview.MemoryView");
	}
	
	private static NumberFormat formater;
	
	// Default update time 
	private static final int UPDATE_TIME = 1000;
	
	// Timer to invoke updating 
	private Timer timer;

	private int totalWidth;
	private int totalHeight;
	
	private long base;	// memory used when opened

	private javax.swing.JPanel jPanel1;
	private javax.swing.JLabel text1;
	private javax.swing.JLabel text2;
	private javax.swing.JProgressBar status;
	private javax.swing.JPanel jPanel2;
	private javax.swing.JButton doGarbage;
	private javax.swing.JButton doRefresh;
	private javax.swing.JButton doClose;
	private javax.swing.JPanel jPanel3;
	private javax.swing.JLabel refreshFrequency;
	private javax.swing.JTextField time;
	private javax.swing.JButton doTime;

	/**
	 * Constructor. 
	 */
	public MemoryView (Window window) {
		super (window);
		
		try {
			formater = NumberFormat.getInstance ();
			formater.setGroupingUsed (true);
			formater.setMaximumFractionDigits (0);
				
			Runtime r = Runtime.getRuntime ();
			base = r.freeMemory ();
			
			initComponents ();
			
			setTitle (Translator.swap ("MemoryView.name"));
			updateStatus ();
			
			timer = new Timer (UPDATE_TIME, new ActionListener () {
				public void actionPerformed (ActionEvent ev) {
				updateStatus ();
				}
			});
			timer.setRepeats (true);
			
			//~ pack ();	// sets the size
			setPreferredSize (new java.awt.Dimension (350, 120));
			setModal (false);
		
			pack ();
			
			setVisible (true);
			
		} catch (Exception exc) {
			Log.println (Log.ERROR, "MemoryView.c ()", exc.toString (), exc);
		}
		
	}

	/** 
	 * Starts the timer.
	 */
	public void addNotify () {
		super.addNotify ();
		timer.start ();
	}

	/**
	 * Stops the timer.
	 */
	public void removeNotify () {
		super.removeNotify ();
		timer.stop ();
	}

	/** 
	 * Updates the status of all components 
	 */
	private void updateStatus () {
		Runtime r = Runtime.getRuntime ();
		long free = r.freeMemory ();
		long total = r.totalMemory ();
		
		// when bigger than integer then divide by two
		while (total > Integer.MAX_VALUE) {
			total = total >> 1;
			free = free >> 1;
		}
		
		int taken = (int) (total - free);
		
		status.setMaximum ((int)total);
		status.setValue (taken);
		status.setString (""+formater.format (100*status.getPercentComplete ())+"%");
		
		String t1 = Translator.swap ("MemoryView.allocatedMemory") +
			" : " +
			formater.format (total);
			
		String t2 = Translator.swap ("MemoryView.used") +
			" : " +
			formater.format (taken);
			
		text1.setText (t1);
		text1.invalidate ();
		
		text2.setText (t2);
		text2.invalidate ();
		
		validate ();
	}

	/** 
	 * This method is called from within the constructor to
	 * initialize the form.
	 */
	private void initComponents () {
		
		JPanel part1 = new JPanel ();
		part1.setLayout (new BoxLayout (part1, BoxLayout.Y_AXIS));
		setContentPane (part1);
		totalWidth = 0;
		totalHeight = 0;
		System.out.println ("totalWidth = "+totalWidth+", totalHeight = "+totalHeight);
		
		// status line
		status = new javax.swing.JProgressBar ();
		status.setBorderPainted (true);
		status.setStringPainted (true);
		LinePanel l1 = new LinePanel ();
		l1.add (status);
		
		
		// Text (Allocated... )
		text1 = new JLabel ();
		LinePanel l2 = new LinePanel ();
		l2.add (text1);
		
		// Text (Allocated... )
		text2 = new JLabel ();
		LinePanel l8 = new LinePanel ();
		l8.add (text2);
		
		// update freq line
		LinePanel l3 = new LinePanel ();
		refreshFrequency = new JWidthLabel (Translator.swap ("MemoryView.refreshTime")+" : ", 50);
		time = new javax.swing.JTextField (5);
		doTime = new javax.swing.JButton (Translator.swap ("MemoryView.apply"));
		doTime.addActionListener (new java.awt.event.ActionListener () {
			public void actionPerformed (java.awt.event.ActionEvent evt) {
				setRefreshTime (evt);
			}
		});
		l3.add (refreshFrequency);
		l3.add (time);
		l3.add (doTime);
//		System.out.println ("l3 : "+Tools.componentSize (l1));
//		System.out.println ("totalWidth = "+totalWidth+", totalHeight = "+totalHeight);
		
		// control line
		JPanel l4 = new JPanel (new SmartFlowLayout (FlowLayout.RIGHT));
		
		doGarbage = new javax.swing.JButton (Translator.swap ("MemoryView.garbageCollect"));
		doGarbage.addActionListener (new java.awt.event.ActionListener () {
			public void actionPerformed (java.awt.event.ActionEvent evt) {
				doGarbageActionPerformed (evt);
			}
		});
		
		doRefresh = new javax.swing.JButton (Translator.swap ("MemoryView.refreshNow"));
		doRefresh.addActionListener (new java.awt.event.ActionListener () {
			public void actionPerformed (java.awt.event.ActionEvent evt) {
				doRefreshActionPerformed (evt);
			}
		});
		
		doClose = new javax.swing.JButton (Translator.swap ("MemoryView.close"));
		doClose.addActionListener (new java.awt.event.ActionListener () {
			public void actionPerformed (java.awt.event.ActionEvent evt) {
				doCloseActionPerformed (evt);
			}
		});
		
		l4.add (doGarbage);
		//~ l4.add (doRefresh);
		l4.add (doClose);
//		System.out.println ("l4: "+Tools.componentSize (l1));
//		System.out.println ("totalWidth = "+totalWidth+", totalHeight = "+totalHeight);
		
		// layout lines
		getContentPane ().add (l2);
		getContentPane ().add (l1);
		getContentPane ().add (l8);
		//~ getContentPane ().add (l3);
		getContentPane ().add (l4);
		
	}

	private void setRefreshTime (java.awt.event.ActionEvent evt) {//GEN-FIRST:event_setRefreshTime
		try {
			int rate = Integer.valueOf (time.getText ()).intValue ();
			timer.setDelay (rate);
		} catch (NumberFormatException ex) {
			time.setText (String.valueOf (timer.getDelay ()));
		}
		time.selectAll ();
		time.requestFocus ();
	}


	private void doCloseActionPerformed (java.awt.event.ActionEvent evt) {//GEN-FIRST:event_doCloseActionPerformed
		dispose ();
	}


	private void doRefreshActionPerformed (java.awt.event.ActionEvent evt) {//GEN-FIRST:event_doRefreshActionPerformed
		updateStatus ();
	}

	private void doGarbageActionPerformed (java.awt.event.ActionEvent evt) {//GEN-FIRST:event_doGarbageActionPerformed
		System.gc ();
		updateStatus ();
	}


	
}
