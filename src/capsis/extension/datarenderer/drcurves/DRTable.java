/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2003 Francois de Coligny
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library;
 * if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 */

package capsis.extension.datarenderer.drcurves;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Point;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;

import javax.swing.BoundedRangeModel;
import javax.swing.JLabel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;

import capsis.extension.DataFormat;
import capsis.extension.PanelDataRenderer;
import capsis.extension.dataextractor.format.DFBoxPlots;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.extension.dataextractor.format.DFListOfCategories;
import capsis.extension.dataextractor.format.DFListOfXYSeries;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;
import capsis.util.ExcelAdapter;
import capsis.util.configurable.ConfigurationPanel;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Check;
import jeeb.lib.util.JTableOwner;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.table.NonEditableTableModel;
import jeeb.lib.util.PlottingPopup;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StandardTableHeaderPopup;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * DataRenderer for DFCurves format in a table. Version 2.
 * 
 * @author F. de Coligny - september 2003
 */
public class DRTable extends PanelDataRenderer implements JTableOwner {

	static {
		Translator.addBundle("capsis.extension.datarenderer.drcurves.DRCurves");
	}

	// nb-06.08.2018
	// static final public String NAME = Translator.swap("DRTable");
	// static final public String VERSION = "2.0";
	// static final public String AUTHOR = "F. de Coligny";
	// static final public String DESCRIPTION =
	// Translator.swap("DRTable.description");

	private JScrollPane scrollPane;
	private int nDecimalDigits;

	private JTable table;

	// private int nLinMin = 50;
	// private int nColMin = 50;

	@Override
	public void init(DataBlock db) {
		super.init(db);

		try {

			nDecimalDigits = Settings.getProperty("drtable.nDecimalDigits", 2);

			setOpaque(true);
			setBackground(Color.WHITE);
			repaint();
		} catch (Exception e) {
			Log.println(Log.ERROR, "DRTable.c (DataBlock)", "Exception caught: ", e);
		}
	}

	/**
	 * Tells if the renderer can show an extractor's production. True if the
	 * extractor is an instance of the renderer's compatible data formats Note:
	 * DataExtractor must implement a data format in order to be recognized by
	 * DataRenderers.
	 */
	static public boolean matchWith(Object target) {

		// fc-7.7.2021 Added DFBoxPlots

		return target instanceof DataExtractor && (target instanceof DFCurves || target instanceof DFListOfXYSeries
				|| target instanceof DFListOfCategories || target instanceof DFBoxPlots);

	}

	@Override
	public String getName() {
		return Translator.swap("DRTable.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("DRTable.description");
	}

	@Override
	public String getVersion() {
		return "2.0";
	}

	public void update() {
		super.update();

		try {

			// fc-6.10.2015 reviewed this method
			// accepts extractors all DFCurves or all DFListOfXYSeries

			boolean showLastLine = false;

			Point memoScrollPosition = new Point(0, 0);
			if (scrollPane != null) {
				memoScrollPosition = scrollPane.getViewport().getViewPosition();

				JScrollBar bar = scrollPane.getVerticalScrollBar();

				BoundedRangeModel b = bar.getModel();
				int min = b.getMinimum();
				int val = b.getValue();
				int ext = b.getExtent();
				int max = b.getMaximum();

				if (val + ext == max) {
					showLastLine = true;
				}
			}

			// Removes all components before redrawing
			this.removeAll();

			// Consider all extractors
			Collection extractors = dataBlock.getDataExtractors();
			Collection specialExtractors = dataBlock.getSpecialExtractors();

			List allExtractors = new ArrayList<>(extractors);
			if (specialExtractors != null) {
				allExtractors.addAll(specialExtractors);
			}

			for (Object e : allExtractors) {
				if (e instanceof DataFormat) {
					DataFormat df = (DataFormat) e;
					if (!df.isAvailable()) {
						addMessage(Translator.swap("Shared.notAvailableOnThisStep"));
						return;
					}
				}
			}

			Object representative = allExtractors.iterator().next();

			// 0. Check data
			if (representative instanceof DFCurves) {
				if (!DataChecker.dataAreCorrect(extractors) || extractors == null || extractors.isEmpty()
						|| ((DFCurves) extractors.iterator().next()).getCurves() == null
						|| ((DFCurves) extractors.iterator().next()).getCurves().isEmpty()) {
					Log.println(Log.ERROR, "DRTable.update ()", "Error while checking data (dataAreCorrect ())");
					addWarningMessage();
//					revalidate(); // fc-22.9.2021 moved
					return;
				}
			}

			DRTableBuilder builder = new DRTableBuilder(allExtractors);

//			System.out.println("DRTable, tracing columnNames, builder.getColNames(): "+builder.getColNames());
//			System.out.println("before correction, colNames: "+AmapTools.toString(builder.getColNames()));

			DefaultTableModel dataModel = new NonEditableTableModel(builder.getMat(), builder.getColNames());

			// fc-20.12.2023 NonEditableTableModel now fixed null columnNames -> A, B...
			// CHECKED
//			System.out.println("after correction, colNames: "+AmapTools.toString(builder.getColNames()));

			table = new JTable(dataModel);
			table.setCellSelectionEnabled(true);
			table.addMouseListener(this);

			// // fc-22.9.2015 Optimal column width
			// optimalColumnWidths(table);

			// In order to have an horizontal scroll bar when the number of
			// columns is to high,
			// auto resize must be disabled (stretch bug: columns unreadable)
			table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

			// Customizing the table: number of decimal digits
			table.setDefaultRenderer(Object.class, new NumberRenderer(nDecimalDigits));

			Color[] colorArray = (Color[]) builder.getColors().toArray(new Color[builder.getColors().size()]);
			TableCellRenderer headerRenderer = new ColoredTableCellRenderer(table, colorArray);
			table.getTableHeader().setDefaultRenderer(headerRenderer);

			// To allow cut & paste with system clipboard (MS-Excel & others...)
			new ExcelAdapter(table);

			// fc-20.12.2023 The line below (PlottingPopup) breaks the header, not shown in
			// table The trouble is related to header configuration restoring, removed it in
			// PplottingPopup
			// This restores the table header configuration if any
			new PlottingPopup().init(table, getName());

			scrollPane = new JScrollPane(table); // fc - 5.10.2002
			scrollPane.addMouseListener(this); // fc - 23.12.2004

			add(scrollPane, BorderLayout.CENTER);

			// Manage scroll bar position
			if (showLastLine) {
				scrollPane.getViewport().setViewPosition(new Point(memoScrollPosition.x, Integer.MAX_VALUE)); // show
																												// bottom
			} else {
				scrollPane.getViewport().setViewPosition(memoScrollPosition); // same
																				// position
			}

			// fc-2.2.2011 bug fix: the table did not update correctly on step
			// move
			revalidate();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DRTable.update (int, int)", "Exception caught: ", e);
		}
	}

// fc-27.6.2018
//	public String getName () {return NAME;}

	// Prints a warning message ("see configuration")
	//
	private void addMessage(String message) {
		LinePanel l1 = new LinePanel();
		l1.add(new JLabel(message));
		l1.addGlue();
		l1.setBackground(Color.WHITE);
		add(l1, BorderLayout.NORTH);
		revalidate();
		repaint();
	}

	// Prints a warning message ("see configuration")
	//
	private void addWarningMessage() {
		LinePanel l1 = new LinePanel();
		l1.add(new JLabel(Translator.swap("Shared.seeConfiguration")));
		l1.addGlue();
		l1.setBackground(Color.WHITE);
		add(l1, BorderLayout.NORTH);

		revalidate(); // f-22.9.2021 Missing refresh in DRTable
		repaint(); // f-22.9.2021 Now ok
	}

	/**
	 * Configurable interface
	 */
	@Override
	public String getConfigLabel() {
		// fc-23.12.2020
		return getName();
//		return ExtensionManager.getName(this);
	}

	/**
	 * Configurable interface
	 */
	@Override
	public ConfigurationPanel getConfigPanel(Object param) {
		DRTableConfigurationPanel panel = new DRTableConfigurationPanel(this);
		return panel;
	}

	/**
	 * Configurable interface
	 */
	@Override
	public void applyConfig(ConfigurationPanel panel) {
		this.nDecimalDigits = ((DRTableConfigurationPanel) panel).getNDecimalDigits();
	}

	/**
	 * Configurable interface
	 */
	@Override
	public void postConfig() {
	}

	// --------------------- inner classes

	/**
	 * A special cell renderer with colors for the table header.
	 */
	static private class ColoredTableCellRenderer extends DefaultTableCellRenderer implements TableColumnModelListener {

		private JTable table;
		private Color[] colors;

		/**
		 * Constructor
		 */
		public ColoredTableCellRenderer(JTable table, Color[] colors) {
			super();
			this.colors = colors;
			table.getColumnModel().addColumnModelListener(this);
			setBorder(UIManager.getBorder("TableHeader.cellBorder"));
			setOpaque(true); // for background

			// Add a popup menu on the header fc-22.9.2015
			StandardTableHeaderPopup.addPopup(table);

		}

		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
				int row, int column) {

			setFont(table.getFont());

			// Color background = new Color (245, 245, 245); // default: very
			// light gray
			// try {
			Color background = colors[column];
			// } catch (Exception e) {} // Extra columns may exist for better
			// rendering -> stay light gray fc-24.11.2014

			int mean = (int) Math.round((background.getRed() + background.getGreen() + background.getBlue()) / 3d);
			setForeground(mean < 128 ? Color.WHITE : Color.BLACK); // mean < 128
																	// ->
																	// background
																	// is dark

			setBackground(background);

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
	static private class NumberRenderer extends DefaultTableCellRenderer {

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

	@Override
	public JTable getJTable() {
		return table;
	}

}
