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

package capsis.extension.memorizer;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Translator;

/**
 * Configuration for Frequency Memorizer.
 *
 * @author F. de Coligny - October 2002
 */
public class FrequencyMemorizerPanel extends AmapDialog implements ActionListener {

	// Values to be proposed to user at opening time
	private int candidateFrequency;
	private boolean candidateAlsoMemoEachStepBefore;

	// User choice
	private JTextField frequency;
	private JCheckBox alsoMemoEachStepBefore; // fc-15.3.2019

	private JButton ok;
	private JButton cancel;

	/**
	 * Constructor.
	 */
	public FrequencyMemorizerPanel(Window parentWindow, FrequencyMemorizer baseMemoriser) {
		
		this(parentWindow, baseMemoriser.getFrequency(), baseMemoriser.isAlsoMemoEachStepBefore());
		
	}

	/**
	 * Constructor 2.
	 */
	public FrequencyMemorizerPanel(Window parentWindow, int candidateFrequency, boolean candidateAlsoMemoEachStepBefore) {
		
		super(parentWindow);

		this.candidateFrequency = candidateFrequency;
		this.candidateAlsoMemoEachStepBefore = candidateAlsoMemoEachStepBefore;

		createUI();

		setModal(true);

		pack();
		setVisible (true);
		
	}

	public void okAction() {
		if (!Check.isInt(frequency.getText().trim ())) {
			MessageDialog.print(this, Translator.swap("FrequencyMemorizerPanel.frequencyMustBeAPositiveInteger"));
			return;
		}
		
		int f = Check.intValue(frequency.getText().trim ());
		if (f <= 0) {
			MessageDialog.print(this, Translator.swap("FrequencyMemorizerPanel.frequencyMustBeAPositiveInteger"));
			return;
		}

		setValidDialog(true);
	}

	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(ok)) {
			okAction();
		} else {
			setValidDialog(false);
		}
	}

	private void createUI() {

		ColumnPanel main = new ColumnPanel ();
		
		LinePanel l1 = new LinePanel();
		l1.add(new JLabel(Translator.swap("FrequencyMemorizerPanel.frequency") + " :"));
		frequency = new JTextField(5);
		frequency.setText("" + candidateFrequency);
		l1.add(frequency);
		
		main.add(l1);

		LinePanel l2 = new LinePanel();
		alsoMemoEachStepBefore = new JCheckBox (Translator.swap("FrequencyMemorizerPanel.alsoMemoEachStepBefore"), candidateAlsoMemoEachStepBefore);
		l2.add(alsoMemoEachStepBefore);
		
		main.add(l2);

		JPanel controlPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		ok = new JButton(Translator.swap("Shared.ok"));
		ok.addActionListener(this);
		setDefaultButton(ok);
		cancel = new JButton(Translator.swap("Shared.cancel"));
		cancel.addActionListener(this);
		controlPanel.add(ok);
		controlPanel.add(cancel);

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(main, BorderLayout.NORTH);
		getContentPane().add(controlPanel, BorderLayout.SOUTH);

		setTitle(Translator.swap("FrequencyMemorizerPanel"));

	}

	public FrequencyMemorizer getFrequencyMemorizer() {
		// Caller can get the chosen memorizer once user has closed the dialog
		// by Ok
		// Checks have been done in okAction ()
		int f = new Integer(frequency.getText()).intValue();
		boolean sb = alsoMemoEachStepBefore.isSelected ();
		
		return new FrequencyMemorizer(f, sb);
		

	}

	// public int getFrequency() {
	// return new Integer(frequency.getText()).intValue();
	// }

}
