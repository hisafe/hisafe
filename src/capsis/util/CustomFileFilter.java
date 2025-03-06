/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package capsis.util;

import java.io.File;
import java.util.Collection;
import java.util.HashSet;

/**
 * CustomFileFilter cann be used with JFileChooser to filter files
 *
 * @author F. de Coligny - august 2004
 */
public class CustomFileFilter extends javax.swing.filechooser.FileFilter {
	private String description;
	private Collection extensions;

	
	/**	Constructor
	*/
	public CustomFileFilter (String description) {
		super ();
		this.description = description;
		extensions = new HashSet ();
	}
	
	/**	Adds an acceptable extension, ex: ".java"
	*/
	public void add (String extension) {
		extensions.add (extension);
	}
	
	/**	FromFileFilter superclass
	*/
	public boolean accept (File file) {
		if (file.isDirectory ()) {return true;}
		String fileName = file.getName ();
		String ext = fileName.substring (fileName.lastIndexOf ("."));
		return extensions.contains (ext);
	}
	
	/**	FromFileFilter superclass
	*/
	public String getDescription () {return description;}
	
}







