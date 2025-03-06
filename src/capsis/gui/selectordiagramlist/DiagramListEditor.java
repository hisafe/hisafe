package capsis.gui.selectordiagramlist;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Question;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;

/**
 * An editor for a given DiagramList.
 * 
 * @author F. de Coligny - December 2015
 */
public class DiagramListEditor extends AmapDialog implements ActionListener {

	static {
		Translator.addBundle("capsis.gui.selectordiagramlist.DiagramList");
	}

	private ToolList toolList;
	private DiagramList diagramList;

	private JTextField name;
	private JList list;

	private JLabel statusBar;

	private JButton ok;
	private JButton cancel;
	private JButton help;

	/**
	 * Constructor.
	 */
	public DiagramListEditor(ToolList toolList, DiagramList diagramList) {
		this.toolList = toolList;
		this.diagramList = diagramList;

		createUI();

		setSize(new Dimension(500, 300));
		// pack();
		setVisible(true);

	}

	public void okAction() {

		// Check the fields, tell user if trouble
		String candidateName = name.getText().trim();

		if (candidateName.length() == 0) {
			MessageDialog.print(this, Translator.swap("DiagramListEditor.diagramListNameCanNotBeEmpty"));
			return;
		}

		// Ensure it starts with a capitalized letter
		candidateName = candidateName.substring(0, 1).toUpperCase() + candidateName.substring(1);

		// If the name was changed, check it's unique
		if (!candidateName.equals(diagramList.getName())) {
			// Check the name is unique, propose a solution to user
			String wName = toolList.getUniqueName(candidateName);
			if (!candidateName.equals(wName)) {
				String title = Translator.swap("DiagramListEditor.nameIsNotUnique");
				String question = Translator.swap("DiagramListEditor.doYouAgreeToChooseThisUniqueName") + " : " + wName;
				boolean userAgrees = Question.ask(this, title, question);
				if (!userAgrees)
					return;
				candidateName = wName;
			}
		}

		diagramList.setName(candidateName);

		setValidDialog(true);
	}

	@Override
	public void actionPerformed(ActionEvent e) {

		if (e.getSource().equals(ok)) {
			okAction();
		} else if (e.getSource().equals(cancel)) {
			setValidDialog(false);
		} else if (e.getSource().equals(help)) {
			Helper.helpFor(this);
		}

	}

	private void createUI() {

		// Editing components
		ColumnPanel top = new ColumnPanel();

		LinePanel l1 = new LinePanel();
		l1.add(new JLabel(Translator.swap("DiagramListEditor.name") + " : "));
		name = new JTextField();
		name.setText(diagramList.getName());
		if (name.getText().trim ().length() == 0) {
			name.setText(diagramList.proposedName());
		}
		l1.add(name);
		l1.addStrut0();
		top.add(l1);

		LinePanel l2 = new LinePanel();
		l2.add(new JLabel(Translator.swap("DiagramListEditor.diagrams")));
		l2.addGlue();
		top.add(l2);

		top.addStrut0();

		list = new JList(new Vector(diagramList.getDiagramLines()));
		JScrollPane scroll = new JScrollPane(list);

		LinePanel bottom = new LinePanel();
		statusBar = new JLabel();
		bottom.add(statusBar);
		bottom.addStrut0();

		JPanel main = new JPanel(new BorderLayout());
		main.add(top, BorderLayout.NORTH);
		main.add(scroll, BorderLayout.CENTER);
		main.add(bottom, BorderLayout.SOUTH);

		// Control panel (ok cancel help);
		LinePanel controlPanel = new LinePanel();
		ok = new JButton(Translator.swap("Shared.ok"));
		cancel = new JButton(Translator.swap("Shared.cancel"));
		help = new JButton(Translator.swap("Shared.help"));
		controlPanel.addGlue();
		controlPanel.add(ok);
		controlPanel.add(cancel);
		controlPanel.add(help);
		controlPanel.addStrut0();
		ok.addActionListener(this);
		cancel.addActionListener(this);
		help.addActionListener(this);

		// Sets ok as default
		setDefaultButton(ok);

		setLayout(new BorderLayout());
		getContentPane().add(main, BorderLayout.CENTER);
		getContentPane().add(controlPanel, BorderLayout.SOUTH);

		if (diagramList.isLocked()) {
			name.setEditable(false);
			name.setEnabled(false);
			list.setEnabled(false);
			statusBar.setText(Translator.swap("DiagramListEditor.canNotEditASystemDiagramList"));
		}

		setTitle(Translator.swap("DiagramListEditor.title"));
		setModal(true);

	}

}
