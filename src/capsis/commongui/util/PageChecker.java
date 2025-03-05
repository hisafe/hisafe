package capsis.commongui.util;

/**
 * Checks if an helper web page is available. Proposes an alternative web page (
 * e.g. the result of a search on the web site).
 * 
 * @author F. de Coligny - August 2018
 */
public interface PageChecker {

	/**
	 * Reads the given web page and evaluate if it exists. E.g. in Capsis, the
	 * web site is a dokuwiki and if we find the text "This topic does not exist
	 * yet", we can deduce the page does not exist.
	 */
	public boolean pageExists(String url);

	/**
	 * Ask user if he wants an alternative help page, returns true if so. If
	 * user cancels, sends an exception.
	 */
	public boolean userWantsAnAlternativePage() throws Exception;

	/**
	 * Make and return an alternative url to be opened. E.g. it is possible to
	 * search in the scope of a dokuwiki web site for a given word picked in the
	 * given className for which we are looking for help.
	 */
	public String getAlternativeUrl(String url, String className);

}
