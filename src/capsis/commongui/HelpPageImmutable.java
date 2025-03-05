package capsis.commongui;

/**
 * This interface can be implemented by Dialog classes. When the user hits help, the Helper.helpFor(this)
 * method checks if the object implements this interface. If it does, the Helper loads the page at the URL
 * specified through the function getHelpPageAddress(). Otherwise, it loads the page that corresponds to 
 * class name of this object.
 * 
 * @author M. Fortin - May 2010
 */
public interface HelpPageImmutable {
	
	public String getHelpPageAddress();
	
}
