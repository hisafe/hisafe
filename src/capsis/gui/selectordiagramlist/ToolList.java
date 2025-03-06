package capsis.gui.selectordiagramlist;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.DefaultListSelectionModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;

import capsis.app.CapsisExtensionManager;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.util.Helper;
import capsis.extension.AbstractDiagram;
import capsis.extension.DiagramFrame;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataRenderer;
import capsis.extensiontype.ExtractorGroup;
import capsis.extensiontype.StandViewer;
import capsis.gui.Pilot;
import capsis.gui.Positioner;
import capsis.gui.Selector;
import capsis.kernel.GModel;
import capsis.kernel.Step;
import capsis.util.IconButton;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * A list of tools of a given type for a given module. E.g. list of StandViewers
 * / DataExtractors
 * 
 * @author F. de Coligny - December 2015
 */
public class ToolList extends JPanel implements MouseListener, KeyListener, ActionListener {

	static {
		Translator.addBundle("capsis.gui.selectordiagramlist.DiagramList");
	}

	private Selector selector; // fc-3.12.2015
	private Map<String, String> tools;
	private String type;
	private String modelName;

	// fc-14.8.2018 Get rid of the 'model' instance variable, disturbs the Steps
	// correct garbage collection on closeProject
	// private GModel model; // Was added by fc-7.12.2015

	// fc-11.1.2019
	private JLabel filterName; // fc-26.8.2021 Managing its tooltip
	private JTextField filter;
	private JButton cancelFilter;

	private JList list;
	private JScrollPane scrollPane;

	// A list of DiagramList object, only for the ExtractorGroup tab
	private List<DiagramList> diagramLists;

	// Specific to ExtractorGroups fc-3.12.2015
	private JButton addDiagramList;
	private JButton removeDiagramList;
	private JButton editDiagramList;
	private JButton helpDiagramList;

	/**
	 * Constructor. The tools map contains entries for dataExtractors,
	 * standViewers, extractorGroups. The extractorGroups are like diagramLists,
	 * but in classes (i.e. system diagramLists).
	 */
	public ToolList(Selector selector, /* GModel model, */ Map<String, String> tools, String type, String modelName) {
		// fc-26.8.2021 added modelName
		
		super();

		this.selector = selector;
		// We keep a copy of the original map (we may change this copy for
		// diagramLists management, see below, the original map will not be
		// changed)
		this.tools = new HashMap<>(tools); // make a copy of the map
		this.type = type;
		this.modelName = modelName;

		// this.model = model;

		diagramLists = new ArrayList<>();

		setLayout(new BorderLayout());

		// fc-29.9.2016 Added the type at the top (clearer for diagram lists)
		LinePanel l0 = new LinePanel();
		filterName = new JLabel(Translator.swap(type));
		l0.add(filterName);
		l0.addGlue();

		filter = new JTextField(5);
		filter.setToolTipText(Translator.swap("ToolList.filter"));
		filter.addKeyListener(this);
		l0.add(filter);
		ImageIcon icon = IconLoader.getIcon("black-cross_16.png");
		cancelFilter = new JButton(icon);
		cancelFilter.setToolTipText(Translator.swap("ToolList.cancelFilter"));
		cancelFilter.addActionListener(this);
		AmapTools.setSizeExactly(cancelFilter);
		l0.add(cancelFilter);
		l0.addStrut0();

		add(ColumnPanel.addWithStrut0(l0), BorderLayout.NORTH);

		scrollPane = new JScrollPane();
		add(scrollPane, BorderLayout.CENTER);

		// Special feature for ExtractorGroups / DiagramLists fc-3.12.2015
		if (type.equals("ExtractorGroup")) {
			addDiagramListManagementPanel();
			loadDiagramLists();
			addExtractorGroupsToDiagramLists();
		}

		updateToolList();

	}

	/**
	 * Add buttons to manage the diagram lists.
	 */
	private void addDiagramListManagementPanel() {
		LinePanel buttonBar = new LinePanel();
		buttonBar.addGlue(); // To center content

		ImageIcon addIcon = IconLoader.getIcon("black-plus_24.png");
		ImageIcon pressedAddIcon = IconLoader.getIcon("black-plus_22.png");
		ImageIcon removeIcon = IconLoader.getIcon("black-minus_24.png");
		ImageIcon pressedRemoveIcon = IconLoader.getIcon("black-minus_22.png");
		ImageIcon editIcon = IconLoader.getIcon("black-edit_24.png");
		ImageIcon pressedEditIcon = IconLoader.getIcon("black-edit_22.png");
		ImageIcon helpIcon = IconLoader.getIcon("black-page_24.png");
		ImageIcon pressedHelpIcon = IconLoader.getIcon("black-page_22.png");

		// fc-7.9.2021
		addDiagramList = new IconButton(addIcon, pressedAddIcon);
//		addDiagramList = new JButton(addIcon);
//		addDiagramList.setBorder(null);
		addDiagramList.setToolTipText(Translator.swap("ToolList.addDiagramList"));
		addDiagramList.addActionListener(this);
		buttonBar.add(LinePanel.addWithStrut0(addDiagramList));
		
		// fc-7.9.2021
		removeDiagramList = new IconButton(removeIcon, pressedRemoveIcon);
//		removeDiagramList = new JButton(removeIcon);
//		removeDiagramList.setBorder(null);
		removeDiagramList.setToolTipText(Translator.swap("ToolList.removeDiagramList"));
		removeDiagramList.addActionListener(this);
		buttonBar.add(LinePanel.addWithStrut0(removeDiagramList));
		
		// fc-7.9.2021
		editDiagramList = new IconButton(editIcon, pressedEditIcon);
//		editDiagramList = new JButton(editIcon);
//		editDiagramList.setBorder(null);
		editDiagramList.setToolTipText(Translator.swap("ToolList.editDiagramList"));
		editDiagramList.addActionListener(this);
		buttonBar.add(LinePanel.addWithStrut0(editDiagramList));
		
		// fc-7.9.2021
		helpDiagramList = new IconButton(helpIcon, pressedHelpIcon);
//		helpDiagramList = new JButton(helpIcon);
//		helpDiagramList.setBorder(null);
		helpDiagramList.setToolTipText(Translator.swap("ToolList.helpDiagramList"));
		helpDiagramList.addActionListener(this);
		buttonBar.add(LinePanel.addWithStrut0(helpDiagramList));

		buttonBar.addGlue(); // To center content

		add(ColumnPanel.addWithStrut0(buttonBar), BorderLayout.SOUTH);
	}

	/**
	 * Loads the diagram lists.
	 */
	private void loadDiagramLists() {

		// fc-6.12.2017 no more report in diagramList
		try {

			// fc-14.8.2018 Get the model instance of the current Step's Project
			// Get rid of the 'model' instance variable, disturbs the Steps
			// correct garbage collection on closeProject
			Step step = Current.getInstance().getStep();
			GModel model = step.getProject().getModel();

			DiagramListLoader l = new DiagramListLoader(model);

			// fc-11.1.2017
			if (!l.diagramListDirExists())
				return;

			l.load();

			diagramLists.addAll(l.getDiagramLists());

		} catch (Exception e) {
			Log.println(Log.WARNING, "ToolList.loadDiagramLists ()", "Could not load diagramLists", e);
		}

		// // Control trace
		// System.out.println("ToolList loadDiagramLists report...");
		// for (DiagramList dl : diagramLists) {
		// System.out.println("ToolList "+dl.getTrace());
		// }

	}

	/**
	 * Change the extractorGroups into diagramLists (will manage better opening
	 * order).
	 */
	private void addExtractorGroupsToDiagramLists() {
		Vector<String> extractorGroups = new Vector(tools.keySet());

		for (String name : extractorGroups) {
			String egClassName = (String) tools.get(name);
			try {
				ExtractorGroup g = (ExtractorGroup) CapsisExtensionManager.getInstance().instantiate(egClassName);

				// Note: we do not check at this time if the ExractorGroup
				// matchWith (model)
				// The checks will be done at diagrams opening time, one by one

				// List of DataExtractors of the group
				List<String> deClassNames = g.getExtractorClassNames();

				// Create the diagramList
				DiagramList dl = new DiagramList(name);
				// DiagramList dl = new DiagramList(getUniqueName(name));
				dl.setLocked(true); // not editable...
				// Add the diagramLines
				int k = 0;
				for (String className : deClassNames) {
					String type = CapsisExtensionManager.DATA_EXTRACTOR;
					// The encodedBounds are needed: they give the rank of
					// each
					// diagram to sort them, dispatch them on a single line
					String encodedBounds = "" + k + " 0 100 100";
					k += 100;
					dl.addDiagramLine(className, type, encodedBounds);
				}
				// We add a diagramList
				diagramLists.add(dl);

				// And we always remove the extractorGroup
				tools.remove(name);

			} catch (Exception e) {
				Log.println(Log.WARNING, "ToolList.addExtractorGroupsToDiagramLists()",
						"Could not replace an extractorGroup: " + egClassName
								+ " by a diagramList, kept the extractorGroup",
						e);
			}
		}

	}

	/**
	 * Makes sure the candidate diagramList name is unique. If not, returns a
	 * unique name (with a suffix).
	 */
	public String getUniqueName(String candidateName) {

		String wName = candidateName;

		boolean found = false;
		int k = 1;
		do {
			found = false;
			for (DiagramList dl : diagramLists) {
				if (dl.getName().equals(wName)) {
					found = true;
					String radical = candidateName;
					if (radical.contains(" (#")) {
						radical = radical.substring(0, radical.indexOf(" (#"));
					}
					wName = radical + " (#" + k + ")";
					k++;
				}
			}
		} while (found);

		return wName;

	}

	/**
	 * Saves the diagram lists
	 */
	private void saveDiagramLists() {
		try {

			// fc-14.8.2018 Get the model instance of the current Step's Project
			// Get rid of the 'model' instance variable, disturbs the Steps
			// correct garbage collection on closeProject
			Step step = Current.getInstance().getStep();
			GModel model = step.getProject().getModel();

			DiagramListLoader l = new DiagramListLoader(model, diagramLists);

			l.save();

		} catch (Exception e) {
			MessageDialog.print(this, Translator.swap("ToolList.couldNotSaveDiagramLists"), e);
		}
	}

	/**
	 * Updates the ToolList (including DiagramLists).
	 */
	private void updateToolList() {
		
		// Sort the tools according to their names
		Vector v = new Vector(tools.keySet());

		if (!diagramLists.isEmpty()) // fc-3.12.2015
			v.addAll(diagramLists);

		// fc-11.1.2019
		String filterString = filter.getText().toLowerCase();
		if (filterString.length() > 0)
			for (Iterator i = v.iterator(); i.hasNext();) {
				Object entry = i.next();
				String name = entry.toString ();
				
				// Remove entries not matching the filter
				if (!name.toLowerCase().contains(filterString))
					i.remove();
			}

		// fc-26.8.2021 for tooltip
		int nTools = v.size();
		if (nTools <= 0)
			filterName.setToolTipText("");
		else
			filterName.setToolTipText(Translator.swap(type)+" : "+nTools+" "+Translator.swap("ToolList.compatibleWith")+" "+modelName);
		
		if (v.isEmpty())
			v.add(Translator.swap("ToolList.emptyList")); // fc-14.10.2013

		// Sort the tools according to their names
		TreeSet<String> sortedSet = new TreeSet<>(v);

		list = new JList(new Vector(sortedSet));

		ListSelectionModel sm = new DefaultListSelectionModel();
		sm.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		list.setSelectionModel(sm);

		list.addMouseListener(this);
		list.addKeyListener(this);

		// fc-29.9.2016 add a tooltip for the tool under the mouse
		list.addMouseMotionListener(new MouseMotionListener() {

			@Override
			public void mouseDragged(MouseEvent e) {
				// no-op
			}

			@Override
			public void mouseMoved(MouseEvent e) {
				JList l = (JList) e.getSource();
				ListModel m = l.getModel();
				int index = l.locationToIndex(e.getPoint());
				if (index > -1) {
					try {
						String name = m.getElementAt(index).toString();
						String className = (String) tools.get(name);
						String description = ExtensionManager.getDescription(className);
						l.setToolTipText(description);
					} catch (Exception e2) {
						// do nothing
					}

				}
			}
		});

		scrollPane.getViewport().setView(list);

	}

	/**
	 * Adds the list of diagrams currently opened in the graphical user
	 * interface in the diagram list (to be reopened in one double click).
	 */
	private void addDiagramListAction() {
		Positioner p = Pilot.getPositioner();
		Collection<Component> components = p.getAllComponents();

		List<DiagramLine> diagramLines = new ArrayList<>();

		for (Component c : components) {

			// Restrict to the visible components (only the current page for
			// ReportPositioner...)
			if (!c.isShowing())
				continue;

			// A DataRenderer shows a DataBlock with one or several
			// DataExtractors inside. If several, all the DataExtractors are
			// of the same class (they may be synchronized on various steps)
			if (c instanceof DataRenderer) {
				DataRenderer r = (DataRenderer) c;

				DataBlock b = r.getDataBlock();

				String className = b.getExtractorType();
				String type = CapsisExtensionManager.DATA_EXTRACTOR;

				DiagramFrame f = Pilot.getPositioner().getInternalFrame((AbstractDiagram) r);

				DiagramLine d = new DiagramLine(className, type, f.getBounds());
				d.setRendererClassName(r.getClass().getName());

				diagramLines.add(d);

			} else if (c instanceof StandViewer) {
				StandViewer v = (StandViewer) c;

				String className = v.getClass().getName();
				String type = CapsisExtensionManager.STAND_VIEWER;

				DiagramFrame f = Pilot.getPositioner().getInternalFrame((AbstractDiagram) v);

				DiagramLine d = new DiagramLine(className, type, f.getBounds());

				diagramLines.add(d);

			}
		}

		if (diagramLines.isEmpty()) {
			MessageDialog.print(this, Translator.swap("ToolList.noDiagramsFound"));
			return;
		}

		// Create the DiagramList
		DiagramList dl = new DiagramList("");

		dl.addDiagramLines(diagramLines);

		// Ask for a (not empty, unique) name
		DiagramListEditor ed = new DiagramListEditor(this, dl);
		if (!ed.isValidDialog())
			return; // was cancelled

		// Add it
		diagramLists.add(dl);

		// Save in file
		saveDiagramLists();

		// Update the tool list
		updateToolList();

		// Try to select the new dl
		try {
			int index = 0;
			for (int i = 0; i < list.getModel().getSize(); i++) {
				// System.out.println("ToolList: dl: " + dl + " list item: " +
				// list.getModel().getElementAt(i));
				if (dl.equals(list.getModel().getElementAt(i))) {
					index = i;
					break;
				}
			}
			list.setSelectedIndex(index);
		} catch (Exception e) {
			// Do nothing
		}
	}

	/**
	 * Removes the selected diagramList.
	 */
	private void removeDiagramListAction() {

		int index = list.getSelectedIndex();

		Object o = list.getSelectedValue();

		if (o instanceof DiagramList) {
			if (((DiagramList) o).isLocked()) {
				MessageDialog.print(this, Translator.swap("ToolList.canNotRemoveASystemDiagramList") + " : " + o);
				return;
			}
			diagramLists.remove(o);
		} else if (o instanceof String) {
			tools.keySet().remove(o);
		}

		// Save in file
		saveDiagramLists();

		updateToolList();

		// Try to select another item (convenient)
		int lastIndex = list.getModel().getSize() - 1;
		try {
			if (index > lastIndex)
				throw new Exception();
			list.setSelectedIndex(index);
		} catch (Exception e) {
			try {
				list.setSelectedIndex(lastIndex);
			} catch (Exception e2) {
				list.setSelectedIndex(0);
			}
		}

	}

	/**
	 * Edits the selected DiagramList (rename...)
	 */
	private void editDiagramListAction() {

		int index = list.getSelectedIndex();

		Object o = list.getSelectedValue();

		if (o instanceof DiagramList) {
			DiagramList dl = (DiagramList) o;
			DiagramListEditor e = new DiagramListEditor(this, dl);
			if (!e.isValidDialog())
				return; // was cancelled

			// extractorGroups have been turned into diagramLists -> the editor
			// opens but prevents modifications
			// } else if (o instanceof String) {
			// MessageDialog.print(this,
			// Translator.swap("ToolList.canNotEditASystemDiagramList"));
			// return;

		}

		// Save in file
		saveDiagramLists();

		updateToolList();

		// Try to reselect the edited item (convenient)
		try {
			list.setSelectedIndex(index);
		} catch (Exception e) {
			try {
				list.setSelectedIndex(0);
			} catch (Exception e2) {
				// Do nothing
			}
		}

	}

	/**
	 * Diagram lists management (add, remove, edit)
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource().equals(cancelFilter)) {
//			System.out.println("ToolList cancelFilter was hit...");
			filter.setText ("");
			updateToolList();
			
		} else if (e.getSource().equals(addDiagramList)) {
			addDiagramListAction();

		} else if (e.getSource().equals(removeDiagramList)) {
			removeDiagramListAction();

		} else if (e.getSource().equals(editDiagramList)) {
			editDiagramListAction();

		} else if (e.getSource().equals(helpDiagramList)) {
			Helper.helpFor(this);

		}
	}

	/**
	 * Open selected extension
	 */
	public void openSelection() {
		Object selection = list.getSelectedValue();

		if (selection == null)
			return; // No selection

		if (selection.equals(Translator.swap("ToolList.emptyList")))
			return; // fc-14.10.2013

		// fc-14.8.2018 Get the model instance of the current Step's Project
		// Get rid of the 'model' instance variable, disturbs the Steps correct
		// garbage collection on closeProject
		Step step = Current.getInstance().getStep();
		GModel model = step.getProject().getModel();

		if (selection instanceof DiagramList) {

			DiagramList dl = (DiagramList) selection;

			// Make a copy (we may remove diagrams)
			dl = new DiagramList(dl);

			// Remove the diagrams not compatible with model
			for (Iterator<DiagramLine> i = dl.getDiagramLines().iterator(); i.hasNext();) {
				DiagramLine d = i.next();

				// Check if the diagram to be opened is compatible with the
				// model
				boolean match = CapsisExtensionManager.matchWith(d.getClassName(), model);
				if (!match)
					i.remove(); // Removed if not compatible

			}

//			System.out.println("ToolList: opening DiagramList " + dl.getTrace());

			// Tell the positioner about the expected diagrams order
			// The diagram lines are ordered
			Positioner p = Pilot.getPositioner();
			p.prepareUpcomingDiagrams(dl.getDiagramLines());

			// The diagramLines are sorted in requested opening order
			for (DiagramLine d : dl.getDiagramLines()) {
				selector.createTool(d.getClassName(), d.getType(), d.getRendererClassName());
			}

		} else {

			String name = (String) selection;

//			System.out.println("ToolList: opening " + name);

			String className = (String) tools.get(name);
			selector.createTool(className, type);

		}
	}

	@Override
	public void mouseClicked(MouseEvent evt) {

		if (evt.getClickCount() < 2) {

			// fc+bc-21.9.2016 Write tool description in status bar
			Object selection = list.getSelectedValue();

			if (selection == null)
				return; // No selection

			if (selection.equals(Translator.swap("ToolList.emptyList")))
				return; // fc-14.10.2013

			if (selection instanceof DiagramList) {
				DiagramList dl = (DiagramList) selection;
				String msg = dl.getName();

				int n = 0;
				if (dl.getDiagramLines() != null) {
					n = dl.getDiagramLines().size();
					msg += ": " + n + Translator.swap("DiagramList.diagrams");
				}

				StatusDispatcher.print(msg);

			} else {

				String name = (String) selection;
				String className = (String) tools.get(name);
				String description = ExtensionManager.getDescription(className);

				StatusDispatcher.print(name + ": " + description);

			}

			return;
		}

		openSelection();
	}

	@Override
	public void keyPressed(KeyEvent e) {

		if (e.getSource().equals(filter)) {
			updateToolList();
			return;
		}

		// Open
		if (e.getKeyCode() == KeyEvent.VK_ENTER) {
			openSelection();
			return;
		}
	}

	@Override
	public void mouseEntered(MouseEvent evt) {
	}

	@Override
	public void mouseExited(MouseEvent evt) {
	}

	@Override
	public void mousePressed(MouseEvent evt) {
	}

	@Override
	public void mouseReleased(MouseEvent evt) {
	}

	@Override
	public void keyReleased(KeyEvent e) {

		if (e.getSource().equals(filter)) {
			updateToolList();
			return;
		}

	}

	@Override
	public void keyTyped(KeyEvent e) {

		if (e.getSource().equals(filter)) {
			updateToolList();
			return;
		}

	}

}
