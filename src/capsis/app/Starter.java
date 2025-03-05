/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 1999-2010 INRA
 *
 * Authors: F. de Coligny, S. Dufour-Kowalski
 *
 * This file is part of Capsis Capsis is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation,
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License along with Capsis. If not, see <http://www.gnu.org/licenses/>.
 *
 */
package capsis.app;

import java.awt.SplashScreen;
import java.io.File;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

import capsis.commongui.util.Helper;
import capsis.kernel.Engine;
import capsis.kernel.Options;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Log;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Settings;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * Capsis Starter. To launch Capsis: new Starter (commandLineArgs).start ();
 * 
 * @author F. de Coligny - September 2001
 */
public class Starter {

	static public final String CAPSIS_VERSION = "4.2.7";

	// fc-15.9.2021 changed version to 4.2.7 (refactored extractors configuration)
	// fc-9.8.2019 changed version to 4.2.5 (removed edit menu in MainFrame)
	// fc-15.10.2012 changed version to 4.2.4 (graphs/colors)

	static private boolean wasCalled;

	/**
	 * Capsis URL
	 */
	static public String getCapsisURL() {

		// fc-15.2.2022 Added https to the url, defaulting to cirad
		return Settings.getProperty("capsis.url", "https://capsis.cirad.fr");

		// fc-10.4.2019 changed for Philippe Dreyfus, trouble from ONF
//		return Settings.getProperty("capsis.url", "http://www.inra.fr/capsis");

//		return Settings.getProperty("capsis.url", "http://capsis.cirad.fr");
	}

	/**
	 * Deals with main options, then creates the capsis.kernel.Engine. start () must
	 * be called just afterward.
	 */
	public Starter(String[] args) {
		if (Starter.wasCalled) {
			return;
		} // can be called only once (could
			// happen in script mode)
		Starter.wasCalled = true;

		try {
			// Moved below after the call to the constructor of Options class. Indeed, the
			// Translator.addBundle() static block
			// from AmapTools class was called too early (before language from -l option is
			// read in constructor from Options
			// class). nb-05.06.2020
			//// fc-15.9.2014
			// detectOsArchMem();

			// Moved below for same reason. nb-05.06.2020
			// fc-15.5.2020 GC -> in the log and in the terminal
			// Settings.setProperty("GC.information", AmapTools.getGarbaCollectorInformation
			// ());
			// System.out.println(AmapTools.getGarbaCollectorInformation ());

			// This line should be removed, only here for profiling tests -
			// fc-18.4.2011
			// PathManager.setInstallDir ("/home/coligny/workspace/capsis4");

			// fc-11.7.2024 Check if a wrong bin/ directory exists, delete it, not used in
			// Capsis, old story, disturbs its functioning
			dealWithBinDir();

			// Options and properties files
			String optionsFileName = PathManager.getDir("etc") + "/capsis.options";
			String propertiesFileName = PathManager.getDir("etc") + "/capsis.properties";

			// Options inits the Log and loads the properties files
			Options options = new Options("Capsis", CAPSIS_VERSION, optionsFileName, propertiesFileName, args);

			// Now called here, see above. nb-05.06.2020
			detectOsArchMem();

			// Now called here, see above. nb-05.06.2020
			Settings.setProperty("GC.information", AmapTools.getGarbaCollectorInformation());
			System.out.println(AmapTools.getGarbaCollectorInformation());

			// Encoding test
			Charset charset = Charset.defaultCharset();
			Log.println("Default encoding: " + charset + " (Aliases: " + charset.aliases() + ")");

			printShortNotice();

			String pilotname = options.getPilotName();
			String[] pilotArguments = options.getPilotArguments();

			// Models file
			String modelsFileName = PathManager.getDir("etc") + "/capsis.models";

			// Extension manager
			ExtensionManager extMan = CapsisExtensionManager.getInstance();
			Log.println(extMan.toString());

			// Creates the application Engine
			// "Capsis" is the application name, "capsis" is the package name
			// where
			// the pilots can be found, e.g. for a Pilot named "gui" ->
			// capsis.gui.Pilot
			// Engine creates the ModelManager and the Translator
			new Engine("Capsis", "capsis", pilotname, pilotArguments,
					/*
					 * optionsFileName, propertiesFileName,
					 */
					modelsFileName, extMan, CAPSIS_VERSION);
			System.out.println("Working dir: " + PathManager.getInstallDir());

			// Searches for Extensions
			if (options.isSearchExtension()) {
				List<String> packages = new ArrayList<String>();
				packages.add("capsis");
				// Retrieves list of models from etc/capsis.models (the ones with "=true").
				packages.addAll(Engine.getInstance().getModelManager().getPackageNames());
				String filename = CapsisExtensionManager.getExtensionListFileName();
				extMan.updateExtensions(packages, filename);
				System.exit(0);
			}

			// fc-7.8.2018 init the Helper
			Helper.init(getCapsisURL(), new CapsisPageChecker());

			// Try to close splash screen
			try {
				SplashScreen sc = SplashScreen.getSplashScreen();
				if (sc != null) {
					sc.close();
				}

				// fc-10.11.2021 Extended to any Throwable (bug som)
			} catch (Throwable e) {
//			} catch (HeadlessException e) {
				// Ignore
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "Starter.c ()", "Exception in Starter", e);
			System.out.println("Starter: could not start, details may be found in var/capsis.log");
			System.exit(1); // stop
		}
	}

	/**
	 * To be called after the constructor, starts the pilot selected in the app
	 * launch process.
	 */
	public void start() {

		try {
			Engine.getInstance().getPilot().start();

		} catch (Exception e) {
			Log.println(Log.ERROR, "Starter.start ()", "Exception in Starter", e);
			System.out.println("Starter.start (): could not start, see var/capsis.log");
			System.exit(2); // stop
		}
	}

	/**
	 * Prints a short notice about the app, authors and licence in the terminal.
	 */
	private void printShortNotice() {
		System.out.println("Capsis " + CAPSIS_VERSION
				+ ", (c) 2000-2024 F. de Coligny, N. Beudez, S. Dufour-Kowalski (INRAE-AMAP) and the Capsis modellers");
		System.out.println("Capsis comes with ABSOLUTELY NO WARRANTY");
		System.out.println("The core of the Capsis platform (packages capsis.*) is free software ");
		System.out.println("and you are welcome to redistribute it under certain conditions. ");
		System.out.println("Some components in other packages may not be free. See licence files.");
		System.out.println();
	}

	/**
	 * Capsis main entry point.
	 */
	public static void main(String[] args) {

		Starter s = new Starter(args);
		s.start();

	}

	/**
	 * The compiled classes of Capsis are in class/. Originally they were in bin/,
	 * this was changed years ago. There is no bin/ directory any more in Capsis. If
	 * there is one, it may disturb Capsis, so we try to delete it (recusively). If
	 * this fails, stop Capsis and ask User to delete it manually before restarting
	 * Capsis.
	 * 
	 * @throws Exception
	 */
	private void dealWithBinDir() throws Exception {
		// fc-11.7.2024
		String problemacticBinDir = PathManager.getInstallDir() + "/bin";

		File pbd = new File(problemacticBinDir);
		if (pbd.exists()) {
			boolean success = AmapTools.deleteDirectory(pbd, true); // true: bin is also deleted
			String deletionStatus = success ? "deleted" : "could not be deleted, please delete it manually";
			System.out.println("Starter, problematic bin/ directory found: "+problemacticBinDir+", " + deletionStatus);

			if (!success)
				throw new Exception(
						"Found a problematic bin/ directory, could not delete it, please delete it manually");
		}

	}

	/**
	 * Detection of the os, java data model architecture and max memory available.
	 */
	static public void detectOsArchMem() { // fc-17.9.2014

		String os = "unknown";
		if (AmapTools.isWindowsPlatform())
			os = "windows";
		if (AmapTools.isMacPlatform())
			os = "mac";
		if (AmapTools.isLinuxPlatform())
			os = "linux";

		// Detect java architecture ("32", "64" or "unknown"). nb-08.01.2019
		String javaArch = AmapTools.getJavaArchitecture();

		// Detect -Xmx value passed to th JVM
		String maxMem = AmapTools.getJVMParam("-Xmx");

		System.out.println("-> OS/JVM/memory: " + os + "/" + javaArch + "/" + maxMem);

	}

}
