/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2017  Francois de Coligny, Nicolas Beudez
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
package capsis.extension.generictool;

import java.awt.Window;

import jeeb.lib.util.MemoryMonitor;
import jeeb.lib.util.Translator;
import capsis.extensiontype.GenericTool;

/**
 * MemoryMonitorExt open MemoryMonitor to check the memory usage.
 * 
 * @author F. de Coligny - October 2011, February 2017
 */
public class MemoryMonitorExt implements GenericTool {

	static {
		Translator.addBundle ("capsis.extension.generictool.MemoryMonitor");
	}

	// nb-06.08.2018
	//public static final String NAME = Translator.swap ("MemoryMonitor");
	//public static final String DESCRIPTION = Translator.swap ("MemoryMonitor.description");
	//public static final String AUTHOR = "F. de Coligny";
	//public static final String VERSION = "1.0";

	/**
	 * Default constructor.
	 */
	public MemoryMonitorExt () {
	}

	@Override
	public void init (Window window) throws Exception {

		new MemoryMonitor (window);

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
		return Translator.swap("MemoryMonitor.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("MemoryMonitor.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}
	
	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

}
