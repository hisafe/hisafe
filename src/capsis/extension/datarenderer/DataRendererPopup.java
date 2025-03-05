package capsis.extension.datarenderer;

import java.awt.Color;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import javax.swing.Icon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import capsis.commongui.projectmanager.ButtonColorer;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.commongui.projectmanager.StepButton;
import capsis.extension.dataextractor.configuration.DEPropertyPage;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.extension.dataextractor.format.DFListOfCategories;
import capsis.extension.dataextractor.format.DFListOfXYSeries;
import capsis.extension.dataextractor.format.DFTables;
import capsis.extension.datarenderer.drcurves.DRTableBuilder;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;
import capsis.gui.DialogWithClose;
import capsis.gui.MainFrame;
import capsis.kernel.Step;
import capsis.util.ExportComponent;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.ColoredIcon;
import jeeb.lib.util.JTableOwner;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.PlottingPopup;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.csvfileviewer.CsvFileViewer;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * DataRenderer's Popup menu.
 * 
 * F. de Coligny - December 1999 (with DataBlock)
 */
public class DataRendererPopup extends PlottingPopup implements ActionListener {

	private String rendererFullClassName;

	private AbstractPanelDataRenderer renderer; // fc - 22.7.2004

	private JMenuItem add;
	private JMenuItem openInTable; // fc-28.9.2015
	private JMenuItem export; // fc - 22.7.2004
	private JMenuItem configure;
	private JMenuItem properties; // nb-31.07.2018
	private DataBlock dataBlock;
	private StepButton currentSb;
	private Map<String, DataExtractor> extractors; // (to remove extractor) key
													// = caption & value =
													// extractor reference
	private List<String> captionList; // keeps order fc-9.3.2016

	private List<JMenuItem> extraMenuItems;

	/**
	 * Constructor.
	 */
	public DataRendererPopup(DataBlock db, boolean exportm) {
		this(db, exportm, null);
	}

	/**
	 * Constructor 2. The extraMenuItems list may be null.
	 */
	public DataRendererPopup(DataBlock db, boolean exportm, List<JMenuItem> extraMenuItems) {
		super();

		// fc-9.3.2016 may be null
		this.extraMenuItems = extraMenuItems;

		renderer = (AbstractPanelDataRenderer) db.getRenderer();
		rendererFullClassName = renderer.getClass().getName();

		Step s = Current.getInstance().getStep();
		currentSb = ProjectManager.getInstance().getStepButton(s);
		dataBlock = db;

		// Prepare map for extractors removal
		extractors = new HashMap<String, DataExtractor>();
		captionList = new ArrayList<>();
		Collection<DataExtractor> es = db.getDataExtractors();
		int uniqueNumber = 0;
		for (Iterator i = es.iterator(); i.hasNext();) {
			DataExtractor e = (DataExtractor) i.next();
			String caption = AmapTools.cutIfTooLong(e.getCaption(), 50) + "|" + uniqueNumber++;
			extractors.put(caption, e);
			captionList.add(caption); // keeps order
		}

		createUI(exportm);
	}

	/**
	 * Items for extractors removal (to ease recognition with instanceof).
	 */
	static private class RemoveMenuItem extends JMenuItem {
		public RemoveMenuItem(String s) {
			super(s);
		}

		public RemoveMenuItem(String s, Icon i) {
			super(s, i);
		}
	}

	/**
	 * Actions management (renderers contextual menu).
	 */
	public void actionPerformed(ActionEvent evt) {

		// fc-16.12.2019 MOVED lower
		// -> in case the evt was not processed here, ask the superclass
		// -> added return for each case below
//		super.actionPerformed(evt);

		// Add extractor for current step button
		if (evt.getSource().equals(add)) {

			// Tell ButtonColorer
			ButtonColorer.getInstance().newColor(currentSb);
			// currentSb.colorize ();

			dataBlock.addExtractor(currentSb.getStep());
			
			return; // fc-16.12.2019

			// Remove given extractor
		} else if (evt.getSource() instanceof RemoveMenuItem) {
			RemoveMenuItem item = (RemoveMenuItem) evt.getSource();
			DataExtractor target = (DataExtractor) extractors.get(item.getActionCommand()); // actionCommand
																							// is
																							// unique,
																							// text
																							// maybe
																							// not

			// Tell ButtonColorer
			// ButtonColorer.getInstance ().removeColor (currentSb);

			dataBlock.removeExtractor(target); // triggers renderer update (or
												// close)

			// fc-2.1.2017 release sb colors
			target.dispose();
			
			return; // fc-16.12.2019

			// Export (JPEG...)
		} else if (evt.getSource().equals(export)) {
			new ExportComponent(renderer);
			
			return; // fc-16.12.2019

			// Open the data series in a floating table fc-28.9.2015
		} else if (evt.getSource().equals(openInTable)) {
			// ... CsvFileViewer
			openCsvViewer(dataBlock);
			
			return; // fc-16.12.2019

			// Open configuration dialog
		} else if (evt.getSource().equals(configure)) {

			renderer.openConfigure();
			
			return; // fc-16.12.2019

		} else if (evt.getSource().equals(properties)) { // Properties

			Collection<DataExtractor> extractors = dataBlock.getDataExtractors();
			DataExtractor extractor = extractors.iterator().next(); // first data extractor of the dataBlock			
			DEPropertyPage propertyPage = new DEPropertyPage(extractor, dataBlock);

			new DialogWithClose(this, propertyPage, Translator.swap("DataRenderer.properties"), true, true, true, true);
			
			return; // fc-16.12.2019

		} else { // Swap renderers

			// actionCommand is used to pass the fullClassName of the chosen
			// extractor renderer
			String newBrowserFcn = evt.getActionCommand();

			// fc-27.6.2018, avoid the options of the superclass
			// (sendThisLineToHeader...)
			if (newBrowserFcn.contains("datarenderer")) {
				// datarenderers are in packages which name contains
				// 'datarenderer', e.g. capsis.extension.datarenderer.drcurves
				dataBlock.setRenderer(newBrowserFcn);
			
				return; // fc-16.12.2019
			}

		}
		
		super.actionPerformed(evt);

		
	}

	/**
	 * Open the data of the extractors in the given dataBlock in one or several
	 * CsvViewer(s).
	 */
	private void openCsvViewer(DataBlock dataBlock) {

		try {

			Collection extractors = dataBlock.getDataExtractors();
			Collection specialExtractors = dataBlock.getSpecialExtractors();

			DataExtractor extractor1 = dataBlock.getDataExtractors().iterator().next();

			// fc-19.6.2018 added DFListOfCategories
			if (extractor1 instanceof DFCurves || extractor1 instanceof DFListOfXYSeries
					|| extractor1 instanceof DFListOfCategories) {

				List<DFCurves> allExtractors = new ArrayList<>(extractors);
				allExtractors.addAll(specialExtractors);

				DRTableBuilder builder = new DRTableBuilder(allExtractors);

				Window window = MainFrame.getInstance();
				String name = dataBlock.getName();
				String tableContent = builder.getTableInAString();
				String separator = "\t";

				new CsvFileViewer(window, name, tableContent, separator, Color.BLUE);

			} else if (extractor1 instanceof DFTables) {

				List<DFTables> allExtractors = new ArrayList<>(extractors);
				allExtractors.addAll(specialExtractors);

				for (DFTables e : allExtractors) {
					Window window = MainFrame.getInstance();
					String name = dataBlock.getName();

					// fc-16.10.2015 better rendering for DRTables in CsvViewer
					String[][] extrContent = DRTables.createPrintableTable(e);
					
					// fc-6.12.2019 added columnNames
					String[] columnNames = new String[extrContent[0].length];

					new CsvFileViewer(window, name, columnNames, extrContent, Color.GREEN);

				}

			} else {

				// fc-19.6.2018
				throw new Exception("CsvViewer can not process this data format");

			}

		} catch (Exception e) {
			Log.println(Log.WARNING, "DataRendererPopup.openCsvViewer()",
					"Could not open CsvFileViewer on this dataBlock: " + dataBlock, e);
			MessageDialog.print(this,
					Translator.swap("DataRendererPopup.couldNotOpenCsvViewerOnTheseDataSeeLogForDetails"), e);
		}

	}

	/**
	 * Build the popup.
	 */
	private void createUI(boolean ep) {
		removeAll();

		// Compatible renderers
		Collection<String> v = dataBlock.getCompatibleDataRendererClassNames();
		TreeSet<String> ts = new TreeSet<String>(v);

		boolean addOldRenderers = Settings.getProperty("add.legacy.renderers.in.graph.popup", false);
		if (!addOldRenderers) {
			ts.remove("capsis.extension.datarenderer.drcurves.DRCurves");
			ts.remove("capsis.extension.datarenderer.drcurves.DRHistogram");
			ts.remove("capsis.extension.datarenderer.drcurves.DRScatterPlot");
		}

		// fc-15.10.2015 REMOVED the jfreechart renderers, replaced by the
		// DRGraph family
		ts.remove("capsis.extension.datarenderer.barchart.BarChart"); // also
																		// removed
		ts.remove("capsis.extension.datarenderer.jfreechart.JFCBarChart");
		ts.remove("capsis.extension.datarenderer.jfreechart.JFCLineChart");
		ts.remove("capsis.extension.datarenderer.jfreechart.JFCScatterPlot");

		if (!ts.isEmpty()) {
			for (String fcn : ts) {
				JMenuItem item = new JMenuItem(ExtensionManager.getName(fcn));
				item.setActionCommand(fcn);
				if (fcn.equals(rendererFullClassName)) {
					item.setEnabled(false);
				}
				item.addActionListener(this);
				add(item);
			}
			addSeparator();
		}

		// fc-26.6.2018 Add table header popup configuration
		if (renderer instanceof JTableOwner) { // e.g. DRTable
			// Note: all data renderers are not showing tables, add the table
			// header menu items only for renderers with a JTable

			JTableOwner tableOwner = (JTableOwner) renderer;
			// We pass the name as a key for table header configuration storage,
			// in order to restore the header configuration at next opening time
			init(tableOwner.getJTable(), tableOwner.getName());

			addTableHeaderMenuItems();
			addSeparator();

		}

		// Add extractor
		//
		add = new JMenuItem(Translator.swap("DataRenderer.add") + " " + currentSb.getStep().getCaption());
		add.setEnabled(true);
		add.addActionListener(this);
		add(add);

		// Remove extractors
		//
		JMenu remove = new JMenu(Translator.swap("DataRenderer.remove"));
		remove.setEnabled(true);
		add(remove);

		// Remove : one entry per extractor in data block
		// Iterator captions = extractors.keySet().iterator();
		Iterator captions = captionList.iterator(); // keep order
		while (captions.hasNext()) {
			String uniqueCaption = (String) captions.next();

			String caption = uniqueCaption;

			int index = uniqueCaption.indexOf("|");
			if (index > 0) {
				caption = uniqueCaption.substring(0, index); // fc - 6.5.2003
			}

			DataExtractor x = (DataExtractor) extractors.get(uniqueCaption);
			ColoredIcon icon = new ColoredIcon(x.getColor());

			RemoveMenuItem removeItem = new RemoveMenuItem(caption, icon);
			removeItem.setActionCommand(uniqueCaption);

			removeItem.addActionListener(this);
			remove.add(removeItem);
		}

		// Export extractor (JPEG...)
		if (ep) {
			export = new JMenuItem(Translator.swap("DataRenderer.export"));
			export.setEnabled(true);
			export.addActionListener(this);
			add(export);
		}

		addSeparator();

		// fc-28.9.2015
		openInTable = new JMenuItem(Translator.swap("DataRenderer.openInTable"));
		openInTable.setEnabled(true);
		openInTable.addActionListener(this);
		add(openInTable);

		// fc-9.3.2016
		// Extra menu items are there, Configure is always at the bottom.
		// e.g. resetGraph for a JFreeChart Graph
		if (extraMenuItems != null && !extraMenuItems.isEmpty()) {

			addSeparator();

			for (JMenuItem mi : extraMenuItems) {
				add(mi);
			}
		}

		addSeparator();

		// Configure renderer and/or extractors
		configure = new JMenuItem(Translator.swap("DataRenderer.configure"));
		configure.addActionListener(this);
		add(configure);

		// Properties. nb-01.08.2018
		addSeparator();
		properties = new JMenuItem(Translator.swap("DataRenderer.properties"));
		properties.addActionListener(this);
		add(properties);
	}
}
