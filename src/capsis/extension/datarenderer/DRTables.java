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
package capsis.extension.datarenderer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;

import capsis.commongui.util.Tools;
import capsis.extension.PanelDataRenderer;
import capsis.extension.dataextractor.configuration.DRConfigurationPanel;
import capsis.extension.dataextractor.format.DFTables;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;
import capsis.kernel.AbstractSettings;
import capsis.util.ExcelAdapter;
import capsis.util.WithTableCellRenderer;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JTableOwner;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.PlottingPopup;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.annotation.Param;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import jeeb.lib.util.table.NonEditableTableModel;

/**
 * Renders multiple tables (which elements are strings).
 * 
 * @author F. de Coligny - February 2002
 */
public class DRTables extends PanelDataRenderer implements Configurable, ChangeListener, JTableOwner {

	static {
		Translator.addBundle("capsis.extension.datarenderer.DRTables");
	}

	// nb-06.08.2018
	// static final public String NAME = Translator.swap("DRTables");
	// static final public String VERSION = "1.0";
	// static final public String AUTHOR = "F. de Coligny";
	// static final public String DESCRIPTION =
	// Translator.swap("DRTables.description"); // fc-14.12.2016 Was missing

	static private int nLinMin = 50;
	static private int nColMin = 50;

	@Param
	protected DRTablesSettings settings;
	protected JScrollPane mainScrollPane;

	// fc-27.6.2018
	protected JTabbedPane tabbedPane;
	protected Map<Integer, JTable> tableMap;

	private java.util.List panels; // one per extractor rendered
	private Collection extractors;

	@Override
	public void init(DataBlock db) {
		super.init(db);

		retrieveSettings();
		createUI();
	}

	/**
	 * Tell if the renderer can show an extractor's production. True if the
	 * extractor is an instance of the renderer's compatible data formats Note:
	 * DataExtractor must implement a data format in order to be recognized by
	 * DataRenderers.
	 */
	static public boolean matchWith(Object target) {
		if (target instanceof DataExtractor && target instanceof DFTables) {
			return true;
		}
		return false;
	}

	@Override
	public String getName() {
		return Translator.swap("DRTables.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("DRTables.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	/**
	 * Ask the extension manager for last version of settings for this extension
	 * type. redefinable by subclasses to get settings subtypes.
	 */
	protected void retrieveSettings() {
		settings = new DRTablesSettings();

	}

	/**
	 * Update strategy for subclasses of DRTables. This method is used to refresh
	 * browser after configuration.
	 */
	public void update() {
		super.update(); // fc - 2.4.2003
		removeAll();
		add(createView(), BorderLayout.CENTER);

		revalidate();
	}

	/**
	 * A convenient static method to turn a DFTables extractor into a single table
	 */
	static public String[][] createPrintableTable(DFTables ex) {

		Collection<String> tits = ex.getTitles();
		Iterator<String> titles = null;
		if (tits != null && !tits.isEmpty()) {
			titles = tits.iterator();
		}

		Collection<String[][]> tables = ex.getTables();

		// One extractor may contain several tables : concatenate them in one
		// single table
		int nLin = 0;
		int nCol = 0;
		for (Iterator<String[][]> j = tables.iterator(); j.hasNext();) {
			String[][] table = j.next();
			nLin = Math.max(nLin, table.length);
			nCol += table[0].length;
		}

		String[][] bigTable = new String[nLin + 1][nCol];

		// Init the big table
		for (int z = 0; z < nLin; z++) {
			Arrays.fill(bigTable[z], "");
		}

		// Concatenate smallTables in bigTable
		int colShift = 0;
		for (Iterator j = tables.iterator(); j.hasNext();) {

			String title = titles == null ? null : (String) titles.next();
			String[][] smallTable = (String[][]) j.next();

			int lines = smallTable.length;
			int columns = smallTable[0].length;

			for (int k = 0; k < columns; k++) {
				bigTable[0][colShift + k] = title;
			}
			for (int p = 0; p < lines; p++) {
				for (int k = 0; k < columns; k++) {
					bigTable[p + 1][colShift + k] = smallTable[p][k];
				}
			}

			colShift += columns;
		}

		return bigTable;
	}

	/**
	 * Merges the given table into a table with size at least nLinMin x nColMin.
	 */
	static String[][] mergeInDefaultTable(String[][] sourceTable, int nLinMin, int nColMin) {

		int sourceNLin = sourceTable.length;
		int sourceNCol = sourceTable[0].length;

		int nLin = Math.max(sourceNLin, nLinMin);
		int nCol = Math.max(sourceNCol, nColMin);

		String[][] resultTable = new String[nLin][nCol];
		for (int i = 0; i < sourceNLin; i++) {
			for (int j = 0; j < sourceNCol; j++) {
				resultTable[i][j] = sourceTable[i][j];
			}
			for (int j = sourceNCol; j < nCol; j++) {
				resultTable[i][j] = ""; // complement with ""
			}
		}
		for (int i = sourceNLin; i < nLin; i++) {
			for (int j = 0; j < nCol; j++) {
				resultTable[i][j] = ""; // complement with ""
			}
		}
		return resultTable;
	}

	/**
	 * Create the complex table component to be displayed
	 */
	private JComponent createView() {
		try {

			panels = new ArrayList();
			extractors = dataBlock.getDataExtractors();
			String[] captions = new String[extractors.size()];
			Color[] colors = new Color[extractors.size()];

			tableMap = new HashMap<>();

			// If several extractors, they are drawn on several panels
			int indexExtractor = 0;
			for (Iterator i = extractors.iterator(); i.hasNext();) {
				DFTables ex = (DFTables) i.next();

				// Reorganized this code - fc-15.11.2011
				String[][] table1 = DRTables.createPrintableTable(ex);
				int nCol1 = table1[0].length;

				// fc-25.11.2014 Enlarge table for better rendering
				String[][] niceTable = mergeInDefaultTable(table1, nLinMin, nColMin);
				int nCol2 = niceTable[0].length;

				// Column colors
				Color color = ex.getColor();
				Collection columnColors = new ArrayList();
				for (int k = 0; k < nCol1; k++) {
					columnColors.add(color);
				}
				for (int k = nCol1; k < nCol2; k++) {
					columnColors.add(new Color(245, 245, 245)); // Color for extra column headers: very light gray;
				}

				captions[indexExtractor] = AmapTools.cutIfTooLong(ex.getCaption(), 50); // see
																						// DataFormat
				colors[indexExtractor] = ex.getColor();

				// fc-20.12.2023 Searching an empty table header problem, seems ok here
				// Stays empty: no column names (keep A, B, C...)
				String colNames[] = new String[nCol2];

				DefaultTableModel dataModel = new NonEditableTableModel(niceTable, colNames);
				JTable table = new JTable(dataModel);
				table.setCellSelectionEnabled(true);
				table.addMouseListener(this); // new - fc - 14.2.2002
				table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF); // fc -
																	// 5.10.2002

				// Customizing the table: number of decimal digits
				if (ex instanceof WithTableCellRenderer) {
					
					// fc-25.4.2024 Extractor custom table cell renderer (ModisDiameterClassTable)
					WithTableCellRenderer wtcr = (WithTableCellRenderer) ex;
					table.setDefaultRenderer(Object.class, wtcr.getTableCellRenderer());

				} else {
					
					table.setDefaultRenderer(Object.class, new NumberRenderer(3));
					
				}

				Color[] colorArray = (Color[]) columnColors.toArray(new Color[columnColors.size()]);
				TableCellRenderer headerRenderer = new ColoredHeaderRenderer(table, colorArray);
				table.getTableHeader().setDefaultRenderer(headerRenderer);

				table.getTableHeader().addMouseListener(this);

				// Allows copy to system clipboard
				new ExcelAdapter(table);

				// This restores the table header configuration if any
				new PlottingPopup().init(table, getName());

				tableMap.put(indexExtractor, table);

				JScrollPane scroll = new JScrollPane(table);
				scroll.addMouseListener(this); // fc - 1.4.2003 - for click and
												// ctrl-click detection
				panels.add(scroll);
				indexExtractor++; // next extractor
			}

			// If no panel at all, return directly
			if (panels.size() == 0) {
				return new JPanel();
			}

			// If several panels, create a JTabbedPane with one panel per
			// extractor
			indexExtractor = 0;
			tabbedPane = new JTabbedPane(JTabbedPane.TOP);

			for (Iterator i = panels.iterator(); i.hasNext();) {
				JScrollPane panel = (JScrollPane) i.next();
				String cardTitle = captions[indexExtractor];

				tabbedPane.addTab(cardTitle, null, panel, null);
				indexExtractor++;
			}

			// fc - 26.9.2005 - try to select new tab (ctrl-click)
			boolean selectionDone = false;
			try {
				int lastNumberOfTabs = Settings.getProperty("drtables.last.number.of.tabs", 0);
				if (indexExtractor > lastNumberOfTabs) { // one atab was added
															// (ctrl-click) ->
															// select it
					tabbedPane.setSelectedIndex(indexExtractor - 1); // select last
					// tab
					selectionDone = true;
				}
			} catch (Exception e) {
			} // this try can fail

			// fc - 26.9.2005 - try to reselect memorized tab
			if (!selectionDone) {
				try {
					int memoIndex = Settings.getProperty("drtables.last.selected.index", 0);
					tabbedPane.setSelectedIndex(memoIndex);
					selectionDone = true;
				} catch (Exception e) {
				} // this try can fail
			}

			// Tabs color management
			for (int i = 0; i < indexExtractor; i++) {
				tabbedPane.setBackgroundAt(i, colors[i]);
			}

			// Init selected tab's colored icon
			int sel = tabbedPane.getSelectedIndex();
			Icon icon = Tools.createRoundIcon(10, 10, tabbedPane.getBackgroundAt(sel));
			tabbedPane.setIconAt(sel, icon);

			Settings.setProperty("drtables.last.selected.index", "" + sel); // fc
																			// -
																			// 26.9.2005
			Settings.setProperty("drtables.last.number.of.tabs", "" + indexExtractor); // fc
																						// -
																						// 26.9.2005

			tabbedPane.addMouseListener(this); // fc - 1.4.2003 - for click and
												// ctrl-click detection
			tabbedPane.addChangeListener(this);

			return tabbedPane;

		} catch (Exception e) {
			Log.println(Log.ERROR, "DRTables.createView ()", "Exception ", e);
			return new JPanel();
		}
	}

	/**
	 * User just changed tab.
	 */
	public void stateChanged(ChangeEvent e) {
		JTabbedPane pane = (JTabbedPane) e.getSource();
		int index = pane.getSelectedIndex();

		Settings.setProperty("drtables.last.selected.index", "" + index); // fc
																			// -
																			// 26.9.2005

		// Tabs color management
		for (int i = 0; i < pane.getTabCount(); i++) {
			pane.setIconAt(i, null);
		}

		pane.setIconAt(index, Tools.createRoundIcon(10, 10, pane.getBackgroundAt(index)));
	}

	/**
	 * User interface creation.
	 */
	protected void createUI() {
		add(createView(), BorderLayout.CENTER);
	}

	/**
	 * Check data
	 */
	private boolean dataAreCorrect(Collection extractors) {
		try {
			if (extractors == null) {
				Log.println(Log.WARNING, "DRTables.dataAreCorrect ()", "Reason: extractors == null");
				return false;
			}
			if (extractors.isEmpty()) {
				Log.println(Log.WARNING, "DRTables.dataAreCorrect ()", "Reason: extractors.isEmpty ()");
				return false;
			}
			for (Iterator i = extractors.iterator(); i.hasNext();) {
				DFTables extr = (DFTables) i.next();

				// lacking info
				if (extr.getTables() == null) {
					Log.println(Log.WARNING, "DRTables.dataAreCorrect ()", "Reason: extr.getTables () == null");
					return false;
				}
				if (extr.getTables().isEmpty()) {
					Log.println(Log.WARNING, "DRTables.dataAreCorrect ()", "Reason: extr.getTables ().isEmpty ()");
					return false;
				}
			}
		} catch (Exception e) {
			Log.println(Log.WARNING, "DRTables.dataAreCorrect ()", "Reason: Exception caught", e);
			return false;
		}

		return true;
	}

	/**
	 * Configurable interface. Configurable interface allows to pass a parameter.
	 */
	public ConfigurationPanel getConfigPanel(Object parameter) {
		DRTablesConfigurationPanel panel = new DRTablesConfigurationPanel(this);
		return panel;
	}

	/**
	 * Configurable interface.
	 */
	public void applyConfig(ConfigurationPanel panel) {
		super.applyConfig(panel); // DataRenderer configuration

		DRTablesConfigurationPanel p = (DRTablesConfigurationPanel) panel;
		settings.columnWidth = p.getColumnWidth();
	}

	/**
	 * Configurable interface.
	 */
	public void postConfig() {
		ExtensionManager.recordSettings(this);
	}

	/**
	 * Configuration Panel for DRTables.
	 */
	private class DRTablesConfigurationPanel extends DRConfigurationPanel {
		private DRTables source;
		public JTextField columnWidth;

		public DRTablesConfigurationPanel(Configurable obj) {
			super(obj);
			source = (DRTables) getConfigurable();

			LinePanel l1 = new LinePanel();
			l1.add(new JWidthLabel(Translator.swap("DRTables.columnWidth") + " :", 120));
			columnWidth = new JTextField(5);
			columnWidth.setText("" + settings.columnWidth);
			l1.add(columnWidth);
			l1.addGlue();

			ColumnPanel master = new ColumnPanel();

			mainContent.add(master); // fc-25.11.2014
//			setLayout(new BorderLayout());
//			add(master, BorderLayout.NORTH);

		}

		public int getColumnWidth() {
			return new Integer(columnWidth.getText()).intValue();
		}

		public boolean checksAreOk() {
			if (!Check.isInt(columnWidth.getText())) {
				JOptionPane.showMessageDialog(this, Translator.swap("DRTables.columnWidthMustBeAnInteger"),
						Translator.swap("Shared.warning"), JOptionPane.WARNING_MESSAGE);
				return false;
			}
			return true;
		}
	}

	/**
	 * This color table cell renderer changes cells background in table first row.
	 */
	// UNUSED
//	private static class ColoredRenderer extends DefaultTableCellRenderer implements TableColumnModelListener {
//		private Color[] colors;
//
//		public ColoredRenderer(JTable table, Color[] colors) {
//			super();
//			this.colors = colors;
//			setOpaque(true); // for background
//		}
//
//		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
//				boolean hasFocus, int row, int column) {
//
//			// We inherit a JLabel
//			setFont(table.getFont());
//			if (row == 0) {
//				Color background = colors[column];
//				setBackground(background);
//				
//			} else {
//				if (isSelected) {
//					setBackground(table.getSelectionBackground());
//				} else {
//					setBackground(Color.WHITE);
//				}
//			}
//			setValue(value);
//			return this;
//		}
//
//		public void columnAdded(TableColumnModelEvent e) {
//		}
//
//		public void columnMarginChanged(ChangeEvent e) {
//		}
//
//		public void columnMoved(TableColumnModelEvent e) {
//			int from = e.getFromIndex();
//			int to = e.getToIndex();
//			if (from == to) {
//				return;
//			}
//			Color tmp = colors[to];
//			colors[to] = colors[from];
//			colors[from] = tmp;
//		}
//
//		public void columnRemoved(TableColumnModelEvent e) {
//		}
//
//		public void columnSelectionChanged(ListSelectionEvent e) {
//		}
//
//	}

	/**
	 * This color table cell renderer changes cells background in table header.
	 */
	private static class ColoredHeaderRenderer extends DefaultTableCellRenderer implements TableColumnModelListener {
		private Color[] colors;

		public ColoredHeaderRenderer(JTable table, Color[] colors) {
			super();
			this.colors = colors;
			table.getColumnModel().addColumnModelListener(this);
			setBorder(UIManager.getBorder("TableHeader.cellBorder"));
			setOpaque(true); // for background
		}

		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
				int row, int column) {

			setFont(table.getFont());

			// fc-25.11.2014
			Color background = colors[column];
			setBackground(background);

			// fc-25.11.2014
			int mean = (int) Math.round((background.getRed() + background.getGreen() + background.getBlue()) / 3d);
			setForeground(mean < 128 ? Color.WHITE : Color.BLACK); // mean < 128 -> background is dark

			setValue(value);
			// We inherit a JLabel
			setHorizontalAlignment(JLabel.CENTER);
			return this;
		}

		public void columnAdded(TableColumnModelEvent e) {
		}

		public void columnMarginChanged(ChangeEvent e) {
		}

		public void columnMoved(TableColumnModelEvent e) {
			int from = e.getFromIndex();
			int to = e.getToIndex();
			if (from == to) {
				return;
			}
			Color tmp = colors[to];
			colors[to] = colors[from];
			colors[from] = tmp;
		}

		public void columnRemoved(TableColumnModelEvent e) {
		}

		public void columnSelectionChanged(ListSelectionEvent e) {
		}

	}

	/**
	 * A cell renderer for numbers in the table, manages the number of decimals.
	 */
	private static class NumberRenderer extends DefaultTableCellRenderer {

		private NumberFormat formatter;
		private int nDecimalDigits;

		public NumberRenderer(int nDecimalDigits) {
			super();
			this.nDecimalDigits = nDecimalDigits;
			formatter = NumberFormat.getInstance(Locale.ENGLISH); // decimal dot
																	// (not a
																	// comma)
			formatter.setGroupingUsed(false);
			formatter.setMaximumFractionDigits(nDecimalDigits);

			// fc+mb-8.4.2022 Trying to format year.doy, e.g. 1999.200
			formatter.setMinimumFractionDigits(nDecimalDigits);

		}

		public void setValue(Object value) {
			if (value == null || value.equals("null")) {
				setText("");
			} else if (value instanceof Number) {
				setHorizontalAlignment(SwingConstants.RIGHT);
				setText(formatter.format((Number) value));

				// fc-24.4.2024 Added this for ints
			} else if (value instanceof String && Check.isInt((String) value)) {
				setHorizontalAlignment(SwingConstants.RIGHT);
				int v = Check.intValue((String) value);
				setText("" + v);

			} else if (value instanceof String && Check.isDouble((String) value)) {
				setHorizontalAlignment(SwingConstants.RIGHT);
				double v = Check.doubleValue((String) value);
				setText(formatter.format(v));
			} else {
				setHorizontalAlignment(SwingConstants.LEFT);
				setText("" + value);
			}
		}
	}

	/** DRTables & subclasses settings */
	private static class DRTablesSettings extends AbstractSettings {
		public int columnWidth = 50; // default value, in pixels

	}

	/**
	 * Returns the table of the selected tab
	 */
	@Override
	public JTable getJTable() {
		int i = tabbedPane.getSelectedIndex();
		return tableMap.get(i);

	}

}
