package capsis.util;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Insets;
import java.util.List;

import javax.swing.JPanel;

import jeeb.lib.util.ColumnPanel;

/**
 * A variant of GridLayout for a table with some columns smaller than the
 * others.
 * 
 * An example of use: the header and the CuttingLines below are based on
 * CompactGridLayout, with one single line and 7 columns in each.
 * 
 * getAlignedPanel(list) returns a column with all the panels aligned one below
 * another.
 * 
 * The ColumnPanel is finally added to a JScrollPane, to be itself added in the
 * CENTER of a JPanel with a BorderLayout.
 * 
 * <pre>
 * Based on HetIntervenerDialog and CuttingLine.
 * 
 *	List list = new ArrayList();
 *	list.add(CuttingLine.getHeader());
 *	for (CuttingLine line : cuttingLines)
 *		list.add(line);
 *
 *	ColumnPanel cp = CompactGridLayout.getAlignedPanel(list);
 *	scrollForCuttings.setViewportView(new NorthPanel(cp));
 *
 * </pre>
 * 
 * @author F. de Coligny - June 2024
 */
public class CompactGridLayout extends GridLayout {

	// fc-11.6.2024 based on GridLayout, several lines possible
	// The columns have their widths calculated from their content
	// It is possible to align the columns of several panels with CompactGridLayouts
	// inside (with same number of columns), see the alignColumns () method lower

	// Preferred and minimum width per column (0 <= index < nCols)
	private int[] preferredWidths;
	private int[] minimumWidths;

	// Preferred and minimum width per line (0 <= index < nRows)
	private int[] preferredHeights;
	private int[] minimumHeights;

	/**
	 * Creates a compact grid layout with the given number of rows and columns.
	 */
	public CompactGridLayout(int nRow, int nCol) {
		super(nRow, nCol, 0, 0);
	}

	/**
	 * Creates a compact grid layout with the given number of rows and columns.
	 */
	public CompactGridLayout(int nRow, int nCol, int hgap, int vgap) {
		super(nRow, nCol, hgap, vgap);
	}

	/**
	 * Returns a ColumnPanel with all the given panels one below another, all
	 * aligned. All the given panels must be instances of JPanel and must have a
	 * CompactGridLayout.
	 */
	public static ColumnPanel getAlignedPanel(List panelsWithCompactGridLayout) throws Exception {

		CompactGridLayout.alignColumns(panelsWithCompactGridLayout);

		ColumnPanel c1 = new ColumnPanel();
		for (Object o : panelsWithCompactGridLayout) {
			if (o instanceof JPanel)
				c1.add(((JPanel) o));
			else
				throw new Exception("CompactGridLayout.getAlignedPanel(), error, not a JPanel: " + o);
		}
		c1.addStrut0();

		return c1;
	}

	/**
	 * A method to align all columns in the given list of containers (to be disposed
	 * one below another). All the containers must have a CompactGridLayout and the
	 * same number of columns.
	 */
	public static void alignColumns(List containersWithCompactGridLayout) throws Exception {

		int n = -1;
		int nCol = -1;

		int[] maxPreferredWidths = null;
		int[] maxMinimumWidths = null;

		for (Object o : containersWithCompactGridLayout) {
			if (!(o instanceof Container))
				throw new Exception(
						"CompactGridLayout.alignColumns(), error, all the objects in the list must be instanceof Container");
			Container cont = (Container) o;

			if (n != -1 && cont.getComponentCount() != n)
				throw new Exception(
						"CompactGridLayout.alignColumns(), error, all the containers in the list must contain the same number of components");
			n = cont.getComponentCount();

			if (cont.getLayout() == null || !(cont.getLayout() instanceof CompactGridLayout))
				throw new Exception(
						"CompactGridLayout.alignColumns(), error, all the containers in the list must have a CompactGridLayoutManager");

			CompactGridLayout layout = (CompactGridLayout) cont.getLayout();
			if (nCol != -1 && layout.getColumns() != nCol)
				throw new Exception(
						"CompactGridLayout.alignColumns(), error, all the containers' CompactGridLayoutManagers must have the same number of columns");
			nCol = layout.getColumns();

			layout.resetSizes(cont);

			if (maxPreferredWidths == null) {
				maxPreferredWidths = new int[nCol];
				maxMinimumWidths = new int[nCol];
			}

			// Keep the max preferred and minimum values per column
			for (int j = 0; j < nCol; j++) {
				maxPreferredWidths[j] = Math.max(maxPreferredWidths[j], layout.preferredWidths[j]);
				maxMinimumWidths[j] = Math.max(maxMinimumWidths[j], layout.minimumWidths[j]);
			}

		}

		// Align all columns
		for (Object o : containersWithCompactGridLayout) {
			Container cont = (Container) o;
			CompactGridLayout layout = (CompactGridLayout) cont.getLayout();

			for (int j = 0; j < nCol; j++) {
				layout.preferredWidths[j] = maxPreferredWidths[j];
				layout.minimumWidths[j] = maxMinimumWidths[j];
			}
		}

	}

	/**
	 * Recalculate all column widths and all line heights.
	 */
	protected void resetSizes(Container parent) {

		if (preferredWidths != null)
			return; // already done, do not change (also may have been adapted by alignColumns())

		int n = parent.getComponentCount();

		int nCol = getColumns();
		int nRow = getRows();

		if (nCol * nRow != n)
			throw new RuntimeException("CompactGridLayout.resetSizes(), error, nCol (" + nCol + ") * nRow (" + nRow
					+ ") should be equal to parent.componentCount (" + n + ")");

		preferredWidths = new int[nCol];
		minimumWidths = new int[nCol];

		preferredHeights = new int[nRow];
		minimumHeights = new int[nRow];

		// Component index in parent
		int k = 0;

		for (int i = 0; i < nRow; i++) { // line i

			for (int j = 0; j < nCol; j++) { // column j

				Component comp = parent.getComponent(k++);

				Dimension prefSize = comp.getPreferredSize();
				Dimension minSize = comp.getMinimumSize();

				// Column widths
				preferredWidths[j] = Math.max(preferredWidths[j], prefSize.width);
				minimumWidths[j] = Math.max(minimumWidths[j], minSize.width);

				// Line heights
				preferredHeights[i] = Math.max(preferredHeights[i], prefSize.height);
				minimumHeights[i] = Math.max(minimumHeights[i], minSize.height);

			}
		}
	}

	/**
	 * Determines the preferred size of the container argument using this compact
	 * grid layout.
	 */
	public Dimension preferredLayoutSize(Container parent) {
		// fc-11.6.2024
		synchronized (parent.getTreeLock()) {
			Insets insets = parent.getInsets();
			int n = parent.getComponentCount();
			int nCol = getColumns();
			int nRow = getRows();

			resetSizes(parent);

			int prefWidth = insets.left + insets.right + (nCol - 1) * getHgap();

			for (int j = 0; j < nCol; j++) { // column j
				prefWidth += preferredWidths[j];
			}

			int prefHeight = insets.top + insets.bottom + (nRow - 1) * getVgap();

			for (int i = 0; i < nRow; i++) { // line i
				prefHeight += preferredHeights[i];
			}

			return new Dimension(prefWidth, prefHeight);
		}
	}

	/**
	 * Determines the minimum size of the container argument using this compact grid
	 * layout.
	 */
	public Dimension minimumLayoutSize(Container parent) {
		// fc-11.6.2024
		synchronized (parent.getTreeLock()) {
			Insets insets = parent.getInsets();
			int n = parent.getComponentCount();
			int nCol = getColumns();
			int nRow = getRows();

			resetSizes(parent);

			int minWidth = insets.left + insets.right + (nCol - 1) * getHgap();

			for (int j = 0; j < nCol; j++) { // column j
				minWidth += minimumWidths[j];
			}

			int minHeight = insets.top + insets.bottom + (nRow - 1) * getVgap();

			for (int i = 0; i < nRow; i++) { // line i
				minHeight += minimumHeights[i];
			}

			return new Dimension(minWidth, minHeight);
		}
	}

	/**
	 * Lays out the specified container using this layout.
	 * <p>
	 * This method reshapes the components in the specified target container in
	 * order to satisfy the constraints of the <code>CompactGridLayout</code>
	 * object.
	 * <p>
	 */
	public void layoutContainer(Container parent) {
		synchronized (parent.getTreeLock()) {
			Insets insets = parent.getInsets();
			int n = parent.getComponentCount();
			int nCol = getColumns();
			int nRow = getRows();

			// CompactGridLayout is left to right only
//			boolean ltr = parent.getComponentOrientation().isLeftToRight();

			if (n == 0)
				return;

			resetSizes(parent);

			int totalGapsWidth = (nCol - 1) * getHgap();
			int widthWOInsets = parent.getWidth() - (insets.left + insets.right);
			int widthForComponents = widthWOInsets - totalGapsWidth;

			int prefWidthSum = 0;
			for (int j = 0; j < nCol; j++) { // columns
				prefWidthSum += preferredWidths[j];
			}

			int extraSpaceForEach = 0;
			if (prefWidthSum < widthForComponents) {
				// We may augment width for some components or all of them
				int availableSpace = widthForComponents - prefWidthSum;
				extraSpaceForEach = availableSpace / nCol; // int division

			}

			// Component index in parent
			int k = 0;

			int y = insets.top;

			for (int i = 0; i < nRow; i++) { // line i

				int x = insets.left;

				// First try: all components have their preferred size
				int h = preferredHeights[i];

				for (int j = 0; j < nCol; j++) { // column j

					// First try: all components have their preferred size
					int w = preferredWidths[j] + extraSpaceForEach;

					parent.getComponent(k++).setBounds(x, y, w, h);

					x += (w + getHgap());

				}

				y += (h + getVgap());
			}

		}
	}

}
