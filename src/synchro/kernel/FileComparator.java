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

package synchro.kernel;

import java.io.File;
import java.util.Comparator;

/**
 * A Comparator for files : first directories, then files.
 * 
 * @author F. de Coligny - april 2003
 */
public class FileComparator implements Comparator {
	public boolean equals (Object o) {
		return false;
	}
		
	public int compare (Object o1, Object o2) {
		File file1 = (File) o1;
		File file2 = (File) o2;
		
		// Directories come first
		if (file1.isDirectory () != file2.isDirectory ()) {
			return file1.isDirectory () ? -1 : 1;
		}
		
		// Both directories or both files -
		// compare based on pathname
		return file1.getName ().compareTo (file2.getName ());
	}
}



