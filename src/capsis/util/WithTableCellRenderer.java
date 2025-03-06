package capsis.util;

import javax.swing.table.TableCellRenderer;

/**
 * Objects implementing this interface can provide a TableCellRenderer.
 * 
 * @author F. de Coligny - April 2024
 */
public interface WithTableCellRenderer {

	/**
	 * Returns a configures table cell renderer
	 */
	public TableCellRenderer getTableCellRenderer();

}
