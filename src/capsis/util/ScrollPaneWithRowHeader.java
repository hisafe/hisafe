package capsis.util;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import javax.swing.DefaultRowSorter;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.RowSorter;
import javax.swing.RowSorter.SortKey;
import javax.swing.ScrollPaneConstants;
import javax.swing.SortOrder;
import javax.swing.event.RowSorterEvent;
import javax.swing.event.RowSorterListener;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

/**
 * Complements a given scrollPane for a given table, with a row header
 * containing a given column of the table. Applies the rowSorter on the extra
 * column if any in the table.
 * 
 * Based on
 * http://www.java2s.com/Code/JavaAPI/javax.swing.table/JTablewithRowHeader1.htm
 * 
 * @author F. de Coligny - January 2021
 */
public class ScrollPaneWithRowHeader implements RowSorterListener {

	// Store the rowSorter sort keys
	private List<SortKey> sortKeys;

	/**
	 * Constructor
	 */
	public ScrollPaneWithRowHeader() {
	}

	/**
	 * A table column model for a single column table (with a given column name).
	 */
	private static class OneColumnTableColumnModel extends DefaultTableColumnModel {

		private String columnName;
		private int columnIndex = -1;

		/**
		 * Constructor
		 */
		public OneColumnTableColumnModel(String columnName) {

			this.columnName = columnName;

		}

		/**
		 * Add only the column matching the headerName
		 */
		public void addColumn(TableColumn tc) {

			// Consider only one column, with the expected name
			String name = "" + tc.getHeaderValue();
			if (name != null && name.toLowerCase().equals(columnName.toLowerCase())) {

				if (columnIndex == -1) {
					// Pick the column index
					columnIndex = tc.getModelIndex(); // or viewIndex ?
				}

				tc.setMaxWidth(tc.getPreferredWidth());
				super.addColumn(tc);

			}

		}

		public int getSelectedColumnIndex() {
			return columnIndex;
		}

	}

	/**
	 * Creates a table with one single column for the scrollPane row header (first
	 * column), containing the column in the table with the given header name,
	 * refreshes the given scrollPane to show the rowHeaderColumn and the table.
	 */
	public void updateScrollPane(JScrollPane scrollPane, JTable table, String columnHeaderName) {

		// Store scrollBar position
		int verticalBarValue = scrollPane.getVerticalScrollBar().getValue();
		int horizontalBarValue = scrollPane.getHorizontalScrollBar().getValue();

		TableModel model = table.getModel();

		OneColumnTableColumnModel oneColumnTableColumnModel = new OneColumnTableColumnModel(columnHeaderName);

		JTable headerColumn = new JTable(model, oneColumnTableColumnModel);
		headerColumn.createDefaultColumnsFromModel();

		// share the selection model
		table.setSelectionModel(headerColumn.getSelectionModel());

		headerColumn.setBackground(Color.LIGHT_GRAY);
		headerColumn.setColumnSelectionAllowed(false);
		headerColumn.setCellSelectionEnabled(false);

		// Deal with the sorter...
		RowSorter rowSorter = table.getRowSorter();
		if (rowSorter != null) {
			// Use the same rowSorter than the table if any
			headerColumn.setRowSorter(rowSorter);

			// Rerun the sorter
			int firstRow = 0;
			int endRow = headerColumn.getRowCount() - 1;
			rowSorter.rowsUpdated(firstRow, endRow);

			// fc-5.1.2021 Might be useful, to be checked
			// sorter.setSortsOnUpdates(true);

			// fc-13.1.2021 Restore sortKeys if any
			if (sortKeys == null) {

				// fc-21.1.2021 Sort at first opening time
				try {
					if (rowSorter instanceof DefaultRowSorter) {

						DefaultRowSorter sorter = (DefaultRowSorter) rowSorter;

						// Get the column index matching the columnHeaderName
						int columnIndex = oneColumnTableColumnModel.getSelectedColumnIndex();

						sortKeys = new ArrayList();
						sortKeys.add(new RowSorter.SortKey(columnIndex, SortOrder.ASCENDING));
						sorter.setSortKeys(sortKeys);
						sorter.sort();

					}
				} catch (Exception e) {
					// Just in case there would be an error during this original sorting, do not
					// fail, just ignore
				}

			} else {
				// Restore sorting at subsequent openings
				rowSorter.setSortKeys(sortKeys);
//				rowSorter.addRowSorterListener(this);
			}

			// fc-21.1.2021 Moved here, was just upper
			rowSorter.addRowSorterListener(this);

		}
		// ... deal with the sorter

		// Update the scrollPane
		JViewport jv = new JViewport();
		jv.setView(headerColumn);
		jv.setPreferredSize(headerColumn.getMaximumSize());

		// Update the scrollPane
		scrollPane.setViewportView(table);
		scrollPane.setRowHeader(jv);
		scrollPane.setCorner(ScrollPaneConstants.UPPER_LEFT_CORNER, headerColumn.getTableHeader());

		// Restore scrollBar position
		scrollPane.getVerticalScrollBar().setValue(verticalBarValue);
		scrollPane.getHorizontalScrollBar().setValue(horizontalBarValue);

	}

	@Override
	public void sorterChanged(RowSorterEvent e) {

		try {
			RowSorter rowSorter = e.getSource();
			sortKeys = rowSorter.getSortKeys();
		} catch (Exception ex) {
			// Non essential, ignore
		}

	}

}
