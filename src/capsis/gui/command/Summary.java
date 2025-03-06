/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski,
 * 
 * This file is part of Capsis Capsis is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the Free Software
 * Foundation, either version 2.1 of the License, or (at your option) any later version.
 * 
 * Capsis is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU lesser General Public License along with Capsis. If
 * not, see <http://www.gnu.org/licenses/>.
 * 
 */
package capsis.gui.command;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;

import capsis.commongui.projectmanager.Current;
import capsis.gui.DialogWithClose;
import capsis.gui.MainFrame;
import capsis.kernel.GModel;
import capsis.kernel.Step;
import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Translator;

/**
 * A way to get a summarized information on a given simulation step.
 * 
 * @author S. Dufour-Kowalski - January 2010
 */

public class Summary extends AbstractAction implements ActionCommand {

	static {
		IconLoader.addPath("capsis/images");
	}
	static private String name = Translator.swap("Summary.summary");
	static private Icon icon = IconLoader.getIcon("empty_16.png"); // for
																	// alignment

	private JFrame frame;

	/**
	 * Constructor
	 */
	public Summary(JFrame frame) {
		super(name, icon);
		this.frame = frame;
		// ~ this.putValue (Action.SHORT_DESCRIPTION, Translator.swap
		// ("NewProject.newProject"));
		this.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_H, ActionEvent.CTRL_MASK));
		// ~ this.putValue (Action.MNEMONIC_KEY, 'N');
	}

	/**
	 * Action interface
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		execute();

	}

	/**
	 * Command interface
	 */
	@Override
	public int execute() {

		Step s = Current.getInstance().getStep();
		if (s != null) {

			// 1. Summary (if any available)
			JScrollPane summaryScroll = null;

			GModel model = s.getProject().getModel();
			capsis.util.SummaryExtractor sm = new capsis.util.SummaryExtractor(model.getSummaryClass());
			String res = sm.getStepSummaryString(model, s);

			// if (res == null || res.equals ("")) {
			// res = Translator.swap ("Summary.summaryNotAvailable");
			// }

			if (res != null && !res.isEmpty()) {

				JTextArea ta = new JTextArea();
				ta.setEditable(false);
				ta.setText(res);

				// fc-22.7.2019
				summaryScroll = new JScrollPane(ta);
				summaryScroll.setPreferredSize(new Dimension(500, 400));
			}

			// fc-4.5.2020 Added an Inspector (always)
			JPanel inspectorPanel = AmapTools.createInspectorPanel(s.getScene());

			// fc-4.5.2020 Accurate layout
			JComponent panel = new JPanel(new BorderLayout());

			if (summaryScroll != null) {

				JTabbedPane tabs = new JTabbedPane();
				tabs.addTab(Translator.swap("Summary.summary"), summaryScroll);
				tabs.addTab(Translator.swap("Summary.inspector"), inspectorPanel);

				panel = tabs;

			} else {
				panel = inspectorPanel;
			}

			DialogWithClose d = new DialogWithClose(MainFrame.getInstance(), panel,
					Translator.swap("Summary.summary")+" "+s.getCaption(), false, true);

			d.setVisible(true);

		}

		return 0;
	}

}
