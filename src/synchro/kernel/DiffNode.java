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

import javax.swing.tree.DefaultMutableTreeNode;

/**
 * A node in a double SubFileSystem JTree
 * 
 * @author F. de Coligny - april 2003
 */
public class DiffNode extends DefaultMutableTreeNode {

	public static final int ZERO = 0;	// not yet calculated
	public static final int IDENTITY = 1;
	public static final int DIFF = 2;
	public static final int NEW = 3;
	public static final int ERASED = 4;

	private String name;			// file (or directory) name
	private String parentPath;		// ends by File.separator (ex: "dir4/"). parentPath+name = file path, relative to parentPath
									// does not start with a File.separator
	private int status;

	public DiffNode (String name, String parentPath) {
		
		super ();	// we are a DefaultMutableTreeNode
		
		this.name = name;
		if (parentPath != null && !parentPath.equals ("")) {		// root node : parentPath = ""
			if (!parentPath.endsWith (File.separator)) {parentPath = parentPath+File.separator;}
			this.parentPath = parentPath;
		} else {
			this.parentPath = "";
		}
		status = DiffNode.ZERO;
	}
	
	public void setName (String name) {this.name = name;}
	public void setStatus (int status) {this.status = status;}
	
	public String getName () {return name;}
	public String getParentPath () {return parentPath;}
	public int getStatus () {return status;}
	
	public String toString () {
		return name+" "+status;		// normal case, displayed in JTree. Status is discarded by renderer (used for colors)
	}
	
	
	
}
