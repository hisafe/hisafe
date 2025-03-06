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

package safe.extension.ioformat.safeExport;

import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.WindowConstants;
import javax.swing.border.Border;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.Translator;

/**
 * SafeExportDialog is a not modal dialog box displayed when export is in progress
 *
 * @author R. Tuquet Laburre - august 2003
 */
public class SafeExportDialog extends AmapDialog implements ActionListener {

	private JLabel lblCurrent;
	private JLabel lblGlobal;
	private JProgressBar jpbCurrent;
	private JProgressBar jpbGlobal;
	public boolean cancelIsPressed=false;
	private int nbGlobalSteps;
	private int nbCurrentSteps;
	private int currentStep;
	private int currentGlobal;

	private JButton cancel;

	public SafeExportDialog (int nbGlobalSteps) {
		super ();
		this.nbGlobalSteps=nbGlobalSteps;
		createUI ();
		setDefaultCloseOperation (WindowConstants.DO_NOTHING_ON_CLOSE);
		addWindowListener (new WindowAdapter () {
			public void windowClosing (WindowEvent evt) {
				cancelAction ();
			}
		});
		jpbGlobal.setMaximum(1);
		jpbGlobal.setMaximum(nbGlobalSteps);
		pack();
		show ();
	}


	public void initCurrent(String caption, int nbSteps)
	{
		jpbCurrent.setMinimum(1);
		jpbCurrent.setMaximum(nbSteps);
		currentGlobal++;
		jpbGlobal.setValue(currentGlobal);
		lblGlobal.setText(currentGlobal + "/" + nbGlobalSteps + " : " + caption);
		nbCurrentSteps=nbSteps;
		if (nbSteps==1) { setCurrent(caption); }
		currentStep=0;
	}

	public void setCurrent(String caption)
	{
		currentStep++;
		jpbCurrent.setValue(currentStep);
		lblCurrent.setText(currentStep + "/" + nbCurrentSteps + " : " + caption );
		updateScreen();
	}

	public String getCurrent() { return lblCurrent.getText(); }

	public void escapePressed () { cancelAction (); }

	private void cancelAction () {
		cancelIsPressed=true;
		setValidDialog (false);
	}

	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (cancel)) {
			cancelAction ();
		}
	}

	public void updateScreen() {
		paintComponent(this.getGraphics());
	}

	public void paintComponent(Graphics g) {
		super.paintComponents(g);

	}

	//
	// Initializes the dialog's graphical user interface.
	//
	private void createUI () {
		Border etched = BorderFactory.createEtchedBorder ();

		JPanel pGlobal = new ColumnPanel();
		Border borGlobal = BorderFactory.createTitledBorder (etched,Translator.swap ("SafeExport.globalExport"));
		pGlobal.setBorder(borGlobal);
		lblGlobal = new JLabel(Translator.swap("SafeExport.globalExport"));
		pGlobal.add(lblGlobal);
		jpbGlobal = new JProgressBar();
		pGlobal.add(jpbGlobal);

		JPanel pCurrent = new ColumnPanel();
		Border borCurrent = BorderFactory.createTitledBorder (etched,Translator.swap ("SafeExport.stepInProgress"));
		pCurrent.setBorder(borCurrent);
		lblCurrent = new JLabel(Translator.swap("SafeExport.stepInProgress"));
		pCurrent.add(lblCurrent);
		jpbCurrent = new JProgressBar();
		pCurrent.add(jpbCurrent);

		cancel = new JButton (Translator.swap ("Shared.cancel"));
		cancel.setToolTipText (Translator.swap ("Shared.cancel"));
		cancel.addActionListener (this);
		setDefaultButton (cancel);
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.CENTER));
		pControl.add (cancel);

		JPanel pMain = new ColumnPanel();
		pMain.add(pGlobal);
		pMain.add(pCurrent);
		// pMain.add(pControl);

		setContentPane(pMain);

		setTitle (Translator.swap ("SafeExport.exportInProgress")+"...");
		setModal (false);
		
	}
}

