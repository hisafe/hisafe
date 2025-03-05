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
package capsis.extension.generictool;

import groovy.ui.Console;

import java.awt.Window;

import jeeb.lib.util.Translator;
import capsis.extensiontype.GenericTool;

/**
 * LogBrowser shows the file var/capsis.log.
 *
 * @author F. de Coligny - june 2001
 */
//public class GroovyConsole implements GenericTool, PaleoExtension {
public class GroovyConsole implements GenericTool {

	static {
		Translator.addBundle("capsis.extension.generictool.GroovyConsole");
	}

	// nb-06.08.2018
	//public static final String NAME = Translator.swap ("GroovyConsole");
	//public static final String DESCRIPTION = Translator.swap ("GroovyConsole.description");
	//public static final String AUTHOR = "S. Dufour-Kowalski";
	//public static final String VERSION = "1.0";

	/**
	 * Default constructor.
	 */
	public GroovyConsole () {

		// fc-14.12.2020 Unused, deprecated
		//activate();
		
	}

	/**
	 * Extension compatibility system.
	 * Returns true if the extension can deal with the given object.
	 */
	static public boolean matchWith (Object referent) {
		return true;
	}

	public void init (Window window) throws Exception {
		
		Console console = new Console ();
		console.run();
		
	}

	@Override
	public String getName() {
		return Translator.swap ("GroovyConsole.name");
	}

	@Override
	public String getAuthor() {
		return "S. Dufour-Kowalski";
	}

	@Override
	public String getDescription() {
		return Translator.swap("GroovyConsole.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}
	
//-------------- nothing below this line
	
//	/** Official constructor redefinition : chaining with superclass official constructor. */
//	public GroovyConsole (GenericExtensionStarter s) throws Exception {
//		activate();
//	}

		
//	/** From Extension interface. */
//	@Override
//	public String getName () {
//		return Translator.swap ("GroovyConsole");
//	}
//
//	/** From Extension interface. */
//	@Override
//	public String getVersion () {return VERSION;}
//	public static final String VERSION = "1.0";
//
//	/** From Extension interface. */
//	@Override
//	public String getAuthor () {return "S. Dufour-Kowalski";}
//
//	/** From Extension interface. */
//	@Override
//	public String getDescription () {return Translator.swap ("GroovyConsole.description");}


//	@Override
//	public String getClassName() {
//		return this.getClass ().getName ();
//	}
//
//	@Override
//	public String getType() {
//		return PaleoExtension.GENERIC_TOOL;
//	}




}



