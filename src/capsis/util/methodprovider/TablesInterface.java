package capsis.util.methodprovider;

import java.util.Map;

import capsis.kernel.Step;

/**
 * An interface to be implemented by an object returning 'tables' in Strings.
 * Can be used by a MethodProvider to be compatible with the SVTables viewer.
 * 
 * @author F. de Coligny - March 2012
 */
public interface TablesInterface {

	/**
	 * Returns a Map of Strings (supposed to be tables: several lines with \n)
	 * to be printed in the SVTables. Key in the map is the table name, value is
	 * the table itself.
	 */
	public Map<String, String> getTables(Step step, boolean perHectare);

}
