/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski,
 * 
 * This file is part of Capsis Capsis is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the Free Software
 * Foundation, either version 2.1 of the License, or (at your option) any later version.
 * 
 * Capsis is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU lesser General Public License along with Capsis. If
 * not, see <http://www.gnu.org/licenses/>.
 * 
 */

package capsis.kernel;

import java.io.File;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Log;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Settings;

import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

import capsis.app.CapsisExtensionManager;

/**
 * Evaluates the options of the capsis.kernel. The constructor must be called
 * with the command line arguments. Note: was previously named CapsisSettings,
 * split into Options and optional capsis.app.CapsisOptions (sep 2010).
 * 
 * @author F. de Coligny - june 1999, september 2010
 */
public class Options {

	/**
	 * Almost a Singleton pattern. The constructor must be called once (and only
	 * once). Then (and only then), the instance can be retrieved with
	 * ModelManager.getInstance ().
	 */
	static protected Options instance;
	/** The parser for the command line. */
	protected CmdLineParser parser;
	/** The name of the application, e.g. 'Capsis', 'Xplo', 'Simeo' */
	protected String applicationName;
	/** The version of the application, e.g. '4.2.2' */
	protected String applicationVersion;
	/** The name of the pilot, e.g. 'gui', script' */
	protected String pilotName;

	// args4j options
	@Option(name = "-h", usage = "shows help")
	public boolean help = false;

	@Option(name = "-p", metaVar = "[gui, script...]", usage = "pilot (e.g. -p run list)")
	public String pilot = "gui";

	@Option(name = "-l", metaVar = "[en, fr, zh]", usage = "language (e.g. -l en)")
	public String lang = "";

	@Option(name = "-v", usage = "shows app version")
	public boolean version = false;

	@Option(name = "-no", usage = "ignores etc/capsis.options file")
	public boolean nooption = false;

	@Option(name = "-ne", usage = "ignores etc/extension.settings file")
	public boolean noExtensionSettings = false;

	@Option(name = "-se", usage = "updates extension.list by searching new extensions")
	public boolean searchExtension = false;

	@Option(name = "-nowl", usage = "disables windows location memorization (gui only)")
	public boolean nowl = false;

	@Option(name = "-nows", usage = "disables windows size memorization (gui only)")
	public boolean nows = false;

	@Option(name = "-project", metaVar = "FILE", usage = "loads the project file (gui only)")
	public String project = "";

	@Option(name = "-projects", metaVar = "FILE", usage = "loads the projects with names in file (gui only)")
	public String projects = "";

	// -aa
	@Option(name = "-aa", usage = "anti-aliasing")
	public boolean antiAliasing; // will fail if initialized here

	// -dgl
	@Option(name = "-dgl", usage = "open gl debug")
	public boolean debugGL; // will fail if initialized here

	// -cancelLogFilesDeletion
	@Option(name = "-clfd", usage = "cancels log files deletion")
	public boolean cancelLogFilesDeletion = false;

	// Receives other command line parameters than options
	@Argument
	public List<String> arguments;

	// end-of-args4j options

	// Note: further options may be added in a subclass for a particular app

	// fc-27.11.2017 trying to enable -10 parameter for a script (startsWith '-'
	// but should not be considered an option)
	@Override
	public String toString() {
		return "Options" + " -h: " + help + " -p: " + pilot + " -l: " + lang + " -v: " + version + " -no: " + nooption
				+ " -ne: " + noExtensionSettings + " -se: " + searchExtension + " -nowl: " + nowl + " -nows: " + nows
				+ " -project: " + project + " -projects: " + projects + " -aa: " + antiAliasing + " -dgl: " + debugGL
				+ " -cancelLogFilesDeletion: " + cancelLogFilesDeletion + " -arguments: " + AmapTools.toString(arguments);
	}

	/**
	 * Used to get the instance of Options. Use the constructor once before to
	 * build the instance.
	 */
	static public Options getInstance() { // almost a Singleton pattern
		if (instance == null) {
			throw new Error(
					"Design error: Constructor must be CALLED ONCE before calling Options.getInstance (), aborted.");
		}
		return instance;
	}

	/**
	 * Constructor
	 */
	public Options(String applicationName, String applicationVersion, String optionsFileName, String propertiesFileName,
			String[] args) throws Exception {
		init(applicationName, applicationVersion, optionsFileName, propertiesFileName, args);
	}

	/**
	 * Returns true if the given args contain the script option
	 */
	private boolean containsScriptOption(String[] args) {
		// fc-27.11.2017
		if (args == null)
			return false;

		for (String arg : args) {
			if (arg.equals("script"))
				return true;
		}
		return false;
	}

	/**
	 * Returns 2 String arrays. Iterates on the arguments, returns all arguments
	 * until '-script' option is found (included) in the first array, and all
	 * the following arguments in the second array.
	 */
	private String[][] splitAtScriptOption(String[] args) {
		// fc-27.11.2017
		List<String> a = new ArrayList<>();
		List<String> b = new ArrayList<>();
		boolean foundScriptOption = false;

		for (String arg : args) {
			if (!foundScriptOption)
				a.add(arg);
			else
				b.add(arg);

			if (arg.equals("script"))
				foundScriptOption = true;

		}

		String[][] res = new String[2][];
		res[0] = new String[a.size()];
		int i = 0;
		for (String arg : a) {
			res[0][i++] = arg;
		}
		res[1] = new String[b.size()];
		i = 0;
		for (String arg : b) {
			res[1][i++] = arg;
		}

		return res;

	}

	protected void init(String applicationName, String applicationVersion, String optionsFileName,
			String propertiesFileName, String[] args) throws Exception {

		// The init method can only be run once
		if (instance != null) {
			throw new Error("Design error: Options constructor must be called ONLY ONCE, aborted.");
		}
		instance = this;

		this.applicationName = applicationName;
		this.applicationVersion = applicationVersion;

		// Parse the command line arguments
		parser = new CmdLineParser(this);
		try {

			// fc-27.11.2017 scripts may accept parameters looking like options,
			// e.g. -10
			if (!containsScriptOption(args)) {
				parser.parseArgument(args);
			} else {
				String[][] twoCommandLines = splitAtScriptOption(args);

				// Parse until -script option
				parser.parseArgument(twoCommandLines[0]);

				// Parse the following without searching any more options
				parser.stopOptionParsing();
				parser.parseArgument(twoCommandLines[1]);
			}

		} catch (CmdLineException e) {
			System.err.println(e.getMessage());
			usage();
			System.exit(1);
		}

		// fc-27.11.2017 trying to enable -10 parameter for a script (startsWith
		// '-'
		// but should not be considered an option)
		// System.out.println(toString());

		// Help was required
		if (this.help) {
			usage();
			System.exit(0); // normal end
		}

		// Shows version and quits
		if (version) {
			// Reads and updates the revision number
			Engine.readRevision(PathManager.getInstallDir());

			System.out.println(applicationName + " " + applicationVersion + "-" + Engine.getRevision());
			System.exit(0); // normal end
		}

		// Manages the log files deletion.
		Log.setCancelLogFilesDeletion(cancelLogFilesDeletion);

		// Language
		if (this.lang.length() == 2) {
			setDefaultLocale(this.lang);
		}

		// Ensure the default language is in the list of known languages, else
		// use english
		// (otherwise, we may encounter troubles when opening internationalized
		// help
		// pages that do not exist - occurred for ModisPinaster and pt)
		Field field = getClass().getField("lang");
		Annotation annotation = field.getAnnotation(Option.class);
		if (annotation instanceof Option) {
			Option myAnnotation = (Option) annotation;
			String knownLangs = myAnnotation.metaVar();
			String currentLang = Locale.getDefault().getLanguage();
			if (!knownLangs.contains(currentLang)) {
				System.out.println("Current locale is not managed, changing to english...");
				setDefaultLocale("en");
			}
		}

		// Pilot name
		List<String> pilotNames = Arrays.asList(new String[] { "gui", "script", "run", "groovy", "automation" });
		if (!pilotNames.contains(this.pilot)) {
			usage();
			System.exit(1);
		}
		pilotName = this.pilot;
		//
		// // -nowl
		// Settings.setProperty ("veto.dialog.location.memorization",
		// this.nowl);
		//
		// // -nows
		// Settings.setProperty ("veto.dialog.size.memorization", this.nows);

		// //////////////

		// Create needed directories if needed
		mkdir("tmp");
		mkdir("var");
		memoMainPaths();

		// Inits the Log system. Log.println () writes to a file from this
		// point.
		initLogger();

		// Load options and properties files if needed
		if (!nooption && optionsFileName != null) {
			Settings.loadPropertyFile(optionsFileName);
		}
		// Set app default.property.file (saved by Settings.savePropertyFile ())
		Settings.setDefaultPropertyFile(optionsFileName);

		// If the file is not null, these properties overwrite the
		// optionsFileName
		if (propertiesFileName != null) {
			Settings.loadPropertyFile(propertiesFileName);
		}

		// fc-7.1.2016 Added -ne to ignore the extension.settings file
		if (noExtensionSettings) {
			// Rename the file -> it will not be found by CapsisExtensionManager
			// at boot time
			String name = PathManager.getDir("etc") + "/" + CapsisExtensionManager.EXTENSION_SETTINGS_FILE_NAME;
			File newFile = null;

			int i = 1;
			do {
				String newName = name + "." + (i++);
				newFile = new File(newName);
			} while (newFile.exists());

			File f = new File(name);
			f.renameTo(newFile);
		}

		// -nowl
		Settings.setProperty("veto.dialog.location.memorization", this.nowl);

		// -nows
		Settings.setProperty("veto.dialog.size.memorization", this.nows);

		// -aa
		Settings.setProperty("sketch.panel3D.anti.aliasing", this.antiAliasing);

		// -dgl
		Settings.setProperty("sketch.panel3D.debug.gl", this.debugGL);

		// Print properties into the log
		printPropertiesIntoTheLog("Settings properties...", Settings.getProperties());
		printPropertiesIntoTheLog("System properties...", System.getProperties());

	}
	
	/**
	 * Creates the given directory in installDir if needed. E.g. mkdir ("tmp");
	 */
	public void mkdir(String name) {

		// fc-25.4.2018
		// PathManager.getDir ("var") will return installDir if var/ does not
		// exist

		// String dir = PathManager.getDir(name);

		String dir = PathManager.getInstallDir() + "/" + name;

		File f = new File(dir);

		if (!f.exists()) {
			f.mkdir();
			System.out.println("Created directory: " + dir);
		}

	}

	/**
	 * Set main paths in Settings properties.
	 */
	private void memoMainPaths() {

		String etcDir = PathManager.getDir("etc");

		Settings.setProperty("capsis.var", PathManager.getDir("var"));
		Settings.setProperty("capsis.etc", PathManager.getDir("etc"));
		Settings.setProperty("capsis.tmp", PathManager.getDir("tmp"));

	}

	/**
	 * Inits the logger
	 */
	public void initLogger() {

		// fc-27.1.2017
		DateFormat dateFormatter = new SimpleDateFormat("yyyy.MM.dd");
		DateFormat timeFormatter = new SimpleDateFormat("HH:mm:ss z");
		Date now = new Date();
		String date = dateFormatter.format(now);
		String time = timeFormatter.format(now);
		
		// Set log
		Log.initLogger(new File(PathManager.getDir("var")), applicationName.toLowerCase());
		Log.println(applicationName + " log created on " + date + " at " + time + "...");
	}

	/**
	 * Lists all the properties in the given Properties object in the Log. The
	 * properties are sorted. The header is written just before.
	 */
	static private void printPropertiesIntoTheLog(String header, Properties properties) {
		StringBuffer props = new StringBuffer(header);
		Set sortedProps = new TreeSet(properties.keySet());
		for (Object name : sortedProps) {
			String value = properties.getProperty((String) name);
			props.append("\n     ");
			props.append(name);
			props.append(" = ");
			props.append(value);
		}
		Log.println(props.toString());

	}

	/**
	 * Tells if the instance of Options was already loaded or not. Useful in the
	 * early ages of the boot when error messages can occur in Alert and must be
	 * shown carefully to the user if Options constructor is not finished yet.
	 */
	static public boolean isInstanceLoaded() {
		return instance != null;
	}

	/**
	 * Print Usage
	 */
	protected void usage() {

		System.out.println("Options:");

		parser.printUsage(System.out);

		System.out.println("Expert options:");
		System.out.println(" -useJvmOptions : sends the options found in first line of the jvmOptions file to the JVM");
		System.out.println(
				"e.g. to launch " + applicationName + " in english: " + applicationName.toLowerCase() + " -l en");
	}

	/**
	 * Define locale
	 */
	private void setDefaultLocale(String loc) {

		Locale l = new Locale(loc);

		System.out.println("Default locale is: " + loc + " " + l.getCountry());
		Locale.setDefault(l);

	}

	/**
	 * Returns the pilot name
	 */
	public String getPilotName() {
		return pilotName;
	}

	/**
	 * Returns the fileName of the list of projects to be loaded at start time
	 * if any. (a file containing many project file names).
	 */
	public String getProjects() {
		return this.projects;
	}

	/**
	 * Returns the fileName containing the project to be loaded at start time if
	 * any.
	 */
	public String getProject() {
		return this.project;
	}

	/**
	 * Should we to search for the extensions ?
	 */
	public boolean isSearchExtension() {
		return this.searchExtension;
	}

	// public boolean noOptions () {return this.nooption;}

	/**
	 * Return non parsed arguments
	 */
	public String[] getPilotArguments() {
		if (this.arguments == null) {
			return null;
		} else {
			return this.arguments.toArray(new String[this.arguments.size()]);
		}
	}

}
