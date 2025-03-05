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
package capsis.extension.generictool.memoryview;

import java.awt.Window;

import jeeb.lib.util.Translator;
import capsis.extensiontype.GenericTool;

/**
 * MemoryViewExt open MemoryView to check the memory usage.
 * 
 * @author F. de Coligny - October 2011
 */
public class MemoryViewExt implements GenericTool {

	static {
		Translator.addBundle ("capsis.extension.generictool.memoryview.MemoryView");
	}

	// nb-06.08.2018
	//public static final String NAME = Translator.swap ("MemoryView");
	//public static final String DESCRIPTION = Translator.swap ("MemoryView.description");
	//public static final String AUTHOR = "F. de Coligny";
	//public static final String VERSION = "1.0";

	/**
	 * Default constructor.
	 */
	public MemoryViewExt () {
	}

	@Override
	public void init (Window window) throws Exception {

		new MemoryView (window);

	}

	/**
	 * Extension compatibility system. Returns true if the extension can deal
	 * with the given object.
	 */
	static public boolean matchWith (Object referent) {
		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("MemoryView.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("MemoryView.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

}
