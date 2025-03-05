/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski
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

package capsis.kernel;

import groovy.lang.GroovyClassLoader;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.StreamCorruptedException;
import java.lang.reflect.Constructor;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import jeeb.lib.util.Log;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import jeeb.lib.util.serial.Reader;
import jeeb.lib.util.serial.SerializerFactory;
import jeeb.lib.util.serial.Writer;

/**	Engine of the application.
*	Contains main organizational processes.
*	Deal with the Session / Project / Step data structure: an history 
*	for the simulations.
*	All actions are invoked by a pilot: graphical user interface, script...
*	The constructor must be called once and only once. Then Engine.getInstance ()
*	then returns the reference to the single Engine object like in the singleton 
*	pattern.
*	
*	@author F. de Coligny - june 2000, september 2010
*/
public class Engine {

//	// Loads the translations for the localized messages in capsis.kernel
//	static {
//		Translator.addBundle ("capsis.kernel.KernelLabels");
//	}
	
	// Implementation notes:
	// - reviewed on sep 2010, Session and Project saving / opening and Project / SessionIdCard
	// - reviewed the staring process: becomes fully multi-applications (Capsis, Simeo, Xplo...)
	// - the extension manager is optional and may be null in Engine
	
	
	/**	Almost a Singleton pattern.
	 *	Call the constructor once only at application launching time 
	 *	and then use Engine.getInstance () when a reference to Engine is needed.
	 */
	private static Engine instance;
	
	/**	Application (may be 'Capsis', 'Simeo', 'Xplo'...) */
	private String applicationName;
	/**	Application package (may be 'capsis', 'jeeb.simeo', 'jeeb.xplo'...) */
	private String applicationPackageName;
	/** The model manager knows all the models the application can run */
	protected ModelManager modelManager;
	/**	The extension manager of the application, optional */
	protected ExtensionManager extensionManager;  // may be null
	/**	The Session contains the Projects */
	private Session session;
	/**	Application version (free number, e.g. '1.3') */
	static private String version = "";  // never null
	/**	Application revision (e.g. '3992M', read in a 'revision' file, generally 
	 *	the svn local copy highest revision, "" if not found, see below)
	 */
	static private String revision = "";  // never null
	/**	Application pilot name (e.g. 'gui', 'script'...) */
	private String pilotName;
	/**	Reference to the Pilot */
	private AbstractPilot pilot;
	/**	The arguments for the pilot (e.g. script to be run and optionally its own params) */
	private String[] pilotArguments;
	
	

	/**	Used to get an instance of Engine.
	*	Use Constructor once before to build the instance.
	*/
	synchronized static public Engine getInstance () {  // almost a Singleton pattern
		if (instance == null) {
			throw new Error ("Design error: Constructor must be CALLED ONCE before calling Engine.getInstance (), aborted.");
		}
		return instance;
	}
	
	/**	For convenience	
	 */
	static public boolean isInstanceLoaded () {
		return instance != null;
	}

	
	/**	Constructor.
	*	We do not use strictly a Singleton pattern here because we need
	*	some parameters at initial construction. This Constructor must be
	*	called at first. Then, Engine.getInstance () can be called
	*	when needed.
	*	Note: some apps may not use an ExtensionManager, and extensionManager 
	*	below may be null
	*/
	public Engine (String applicationName, String applicationPackageName, 
			String pilotName, String[] pilotArguments, 
//			String optionsFileName, String propertiesFileName, 
			String modelsFileName, ExtensionManager extensionManager, 
			String version) throws Exception {
		init (applicationName, applicationPackageName, pilotName, pilotArguments, 
				/*optionsFileName, propertiesFileName,*/ modelsFileName, extensionManager, version);
	}
	
	private void init (String applicationName, String applicationPackageName, 
			String pilotName, String[] pilotArguments, 
//			String optionsFileName, String propertiesFileName, 
			String modelsFileName, ExtensionManager extensionManager, 
			String version) throws Exception {		// almost a Singleton pattern
		
		// The init method can only be run once
		if (instance != null) {
			throw new Error ("Design error: Engine constructor must be called ONLY ONCE, then use getInstance (), aborted.");
		}			
		instance = this;
		
		this.applicationName = applicationName;
		this.applicationPackageName = applicationPackageName;
		this.pilotName = pilotName;
		this.pilotArguments = pilotArguments;

		// Initialise Translator
		try {
			Translator.init (Locale.getDefault ());
		} catch (Exception e) {}
		
		// Add the capsis.kernel translations
		Translator.addSystemBundle ("capsis.kernel.KernelLabels");
		
		this.modelManager = new ModelManager (modelsFileName);
		
		this.extensionManager = extensionManager;
		
		Engine.version = version == null ? "" : version;
		
//		// Create needed directories if needed
//		mkdir ("tmp");
//		mkdir ("var");
//		memoMainPaths ();
//		
//		// Inits the Log system. Log.println () writes to a file from this point. 
//		initLogger ();
		
		readRevision (PathManager.getInstallDir());
		
		// For apple menus at top of screen compatibility
		Settings.setProperty ("apple.laf.useScreenMenuBar", "true");
		
		// Create the pilot
		createPilot (applicationPackageName, pilotName);
		
		String report = applicationName 
				+ " " + getVersionAndRevision()
				+ " with pilot " + pilot.getClass ().getName ()
				+ ": correct boot at " 
				+ new SimpleDateFormat("d MMM yyyy HH:mm:ss z", Locale.ENGLISH).format (new Date ());
		
		// Report to the log
		Log.println (Log.INFO, "Engine.init ()", report);
		
		// Report to the terminal
		System.out.println (report);
//		
//		// Load options and properties files if not null
//		if (optionsFileName != null) {
//			Settings.loadPropertyFile (optionsFileName);
//			// Set app default.property.file (saved by Settings.savePropertyFile ())
//			Settings.setDefaultPropertyFile (optionsFileName);
//		}
//		
//		// If the file is not null, these properties overwrite the optionsFileName
//		if (propertiesFileName != null) {
//			Settings.loadPropertyFile (propertiesFileName);
//		}
//		
//		// Print properties into the log
//		printPropertiesIntoTheLog ("Settings properties...", Settings.getProperties ());
//		printPropertiesIntoTheLog ("System properties...", System.getProperties ());
		
	}
	
	/**	Return the application version.
	 */
	static public String getVersion () {return version;}
	
	/**	Return the application revision.
	 */
	static public String getRevision () {
		return revision;
	}
	
	/**	Return the application version and revision.
	 * 	e.g. '4.2.2-3992MS' / '4.2.2' / '3992MS'
	 */
	static public String getVersionAndRevision () {
		StringBuffer vr = new StringBuffer ();
		if (version != null && version.length () > 0) {
			vr.append (version);
		}
		if (revision != null && revision.length () > 0) {
			if (vr.length () > 0) {vr.append ('-');}
			vr.append (revision);
		}
		return vr.toString ();
	}
	
	/**	Reads the svn revision String in the 'revision' file in 
	 * 	the given directory. If not found or trouble, revision is "" by convention.
	 * 	Note: the revision is not always an integer, can also contain letters.
	 * 	Note: the revision file can be built by ant at each compilation time with 
	 * 	the help of the svnversion OR SubWCRev tools.
	 */
	static public void readRevision (String dir) {
		try {
			BufferedReader rev =  new BufferedReader(new FileReader (new File (dir + "/revision")));
			revision = rev.readLine();  // e.g. '3992' OR '3992MS'
			
			if (revision.equals ("?")) {throw new Exception ();}  // '?' means 'unknown'
			
		} catch (Exception e) {
			revision = "";  // convention
		}
	}
//
//	/**	Creates the given directory in installDir if needed.
//	 * 	E.g. mkdir ("tmp");
//	 */
//	public void mkdir (String name) {
//		
//		String dir = PathManager.getDir (name);
//		File f = new File (dir); 
//		if (!f.exists ()) {
//			f.mkdir ();
//			System.out.println ("Created directory: " + dir);			
//		}
//		
//	}
	
//	
//	/**	Inits the logger 
//	 */
//	public void initLogger() {
//		
//		// Set log
//		Log.initLogger (new File (PathManager.getDir ("var")), applicationName.toLowerCase ());
//		Log.println (applicationName + " booting...");
//		
//	}
//	
//	/**	Lists all the properties in the given Properties object in the Log.
//	 * 	The properties are sorted.
//	 * 	The header is written just before.	
//	 */
//	static private void printPropertiesIntoTheLog (String header, Properties properties) {
//		StringBuffer props = new StringBuffer (header);
//		Set sortedProps = new TreeSet (properties.keySet ());
//		for (Object name : sortedProps) {
//			String value = properties.getProperty ((String) name);
//			props.append ("\n   ");
//			props.append (name);
//			props.append (" = ");
//			props.append (value);
//		}
//		Log.println (props.toString ());
//		
//	}
//	
//	/** Set main paths in Settings properties.
//	 */
//	private void memoMainPaths () {
//		
//		String etcDir = PathManager.getDir("etc");
//		
//		Settings.setProperty ("capsis.var", PathManager.getDir("var"));
//		Settings.setProperty ("capsis.etc", PathManager.getDir("etc"));
//		Settings.setProperty ("capsis.tmp", PathManager.getDir("tmp"));
//
//	}

	public String getApplicationName () {
		return applicationName;
	}
	
	public String getApplicationPackageName () {
		return applicationPackageName;
	}
	
	public ModelManager getModelManager () {
		return modelManager;
	}

	
// Session management: new, open, close, saveAs
	
	public void setSession (Session s) {session = s;}
	
	public Session getSession () {return session;}
	
	/**	Creates the session.
	 */
	public Session processNewSession (String name) {
		Session s = new Session (name);
		this.setSession (s);
		s.setSaved (true);  // contains nothing to be saved yet
		return s;
	}

	
	
	/**	Opens a session.
	 * 	If some projects can not be loaded, writes notes in the given report 
	 * 	and tries to load the other projects.
	 * 	The report is required.
	 */
	public Session processOpenSession (String fileName, StringBuffer report) throws Exception {
		// Complete review - sep 2010 
		// 3 possible errors: revision error, wrong session file, other error

		if (report == null) {throw new Exception ("Engine.processOpenSession (), error: report is null");}
		
		Session s = null;
		List<String> projectFileNames = new ArrayList<String> ();

		try {
			ObjectInputStream in = new ObjectInputStream (
					new BufferedInputStream (
					new FileInputStream (fileName)));
			
			// Read the SessionIdCard compactString
			String cs = (String) in.readObject ();
			SessionIdCard sic = new SessionIdCard (cs);
			
			// Read the list of project file names
			projectFileNames = (List<String>) in.readObject ();

			in.close ();

			s = processNewSession (sic.getSessionName ());

			for (Iterator<String> i = projectFileNames.iterator (); i.hasNext ();) {
				String projectFileName = i.next ();
				StatusDispatcher.print (Translator.swap ("Engine.openingProject") + " " + projectFileName);

				// If trouble with a project, write in report and try the next one
				// processOpenProject () may throw Exceptions
				// (always of type Exception)
				try {
					Project p = processOpenProject (projectFileName);  // includes model relay creation
					s.addProject (p);
					
				} catch (Exception e) {
					report.append ("Could not load project file: " + projectFileName + "\n");
					report.append ("-> Error (see Log for details): " + e + "\n");
				}
			}

			s.setSaved (true);  // nothing to be saved until something is changed in the session
			s.setWasSaved (true);

		} catch (java.io.InvalidClassException e) {
			// Session Revision error
			String message = Translator.swap("Engine.revisionError") + ". "+ applicationName + " " + getVersionAndRevision ()
					+ " " + Translator.swap ("Engine.canNotOpenThisSessionFile") + ": " + fileName; 
			try {
				// Try to know what versionAndRevision of the app can reopen the project
				SessionIdCard sic = new SessionIdCard (new File (fileName));
				message += "\n" + Translator.swap ("Engine.youMayOpenThisSessionWithThisVersion") + ": " + sic.getVersion();
			} catch (Exception e2) {
				message += "\n" + Translator.swap ("Engine.couldNotReadTheVersionInTheSessionFile");
			}
			// Write in the Log, tell the caller
			Log.println (Log.ERROR, "Engine.processOpenSession ()", message, e);
			throw new Exception (message, e);
			
		} catch (StreamCorruptedException e) {
			// Not a session file (e.g. a .pdf...)
			String message = Translator.swap ("Engine.itSeemsThisIsNotACorrectSessionFile") + ": " + fileName;
			// Write in the Log, tell the caller
			Log.println (Log.ERROR, "Engine.processOpenSession ()", message, e);
			throw new Exception (message, e);

		} catch (Exception e) {
			// Other error
			String message = Translator.swap ("Engine.couldNotOpenTheSessionDueToAnException") + ": " + fileName;
			// Write in the Log, tell the caller
			Log.println (Log.ERROR, "Engine.processOpenSession ()", message, e);
			throw new Exception (message, e);

		}

		return s;
	}

	
	/**	Closes a session.
	 */
	public void processCloseSession () {
		if (session != null) {
			session = null;
		}
	}

	
	/**	Saves As a session.
	 */
	public void processSaveAsSession (String fileName) throws Exception {
		// Serialization
		if (session != null) {

			List<String> projectFileNames = new ArrayList<String> ();
			
			for (Project p : session.getProjects ()) {
				String name = p.getFileName ();
				projectFileNames.add (name);
			}

			try {
				ObjectOutputStream out = new ObjectOutputStream (
						new BufferedOutputStream (
						new FileOutputStream (fileName)));
				
				// Write the SessionIdCard compactString 
				out.writeObject (new SessionIdCard (session).getCompactString());
				
				// Write the list of project file names
				out.writeObject (projectFileNames);

				out.flush ();
				out.close ();

				session.setSaved (true);  // it was just saved
				session.setWasSaved (true);
				
			} catch (Exception e) {
				String message = Translator.swap ("Engine.couldNotWriteTheSession") + ": " + fileName;
				// Write in the Log, tell the caller
				Log.println (Log.ERROR, "Engine.processSaveAsSession ()", message, e);
				throw new Exception (message, e);
			}
		}
	}
	
	
// Projects management: new, open, close, saveAs

	/**	Creates a project and adds it to the Engine's current Session.
	 */
	public Project processNewProject (String name, GModel model) {
		// If session does not exist, create an untitled session
		if (session == null) {processNewSession (Translator.swap ("Engine.untitledSession"));}

		Project p = new Project (name, model);
		
		model.setProject(p);
		session.addProject (p);
		session.setSaved (false);
		p.setSaved (false);
	
		// Add the language bundle for this model to the Translator
		Translator.addBundle (model.getIdCard ().modelBundleBaseName);
		
		updateExtensionManager (model);

		return p;
		
	}

	/**	Reads a project in a given file.
	 */
	public Project processOpenProject (String fileName) throws Exception {
		// If no session, create a default session
		if (session == null) {processNewSession (Translator.swap ("Engine.untitledSession"));}

		Project project = null;
		
		try {
			Reader reader = SerializerFactory.getReader (fileName);
			project = (Project) reader.readObject ();
			
//			try {
			
			reader.testCompatibility ();
				
//			} catch (Exception e) {
//				// Object structure seems to have changed since the file was created
//				String msg = Translator.swap ("Engine.loadProjectWarning") + "\n" + e.getMessage ();
//				Log.println(Log.ERROR, "Engine.processOpenProject ()", msg, e);
//				throw new Exception (msg, e);
//			}
			
			session.setSaved (false);
			project.setSaved (true);
			project.setWasSaved (true);
			
			// Add the language bundle for this model to the Translator
			GModel model = project.getModel ();
			Translator.addBundle (model.getIdCard ().modelBundleBaseName);
						
			updateExtensionManager (model);
			
			model.projectJustOpened ();  // hook for model (technical initializations outside java if needed...)

		} catch (java.io.InvalidClassException e) {
			// Revision error
			String message = Translator.swap("Engine.revisionError") + ". "+ applicationName + " " + getVersionAndRevision ()
					+ " " + Translator.swap ("Engine.canNotOpenThisProjectFile") + ": " + fileName; 
			try {
				// Try to know what versionAndRevision of the app can reopen the project
				ProjectIdCard pic = new ProjectIdCard (new File (fileName));
				message += "\n" + Translator.swap ("Engine.youMayOpenThisProjectWithThisVersion") + ": " + pic.getVersion();
			} catch (Exception e2) {
				message += "\n" + Translator.swap ("Engine.couldNotReadTheVersionInTheProjectFile");
			}
			// Write in the Log, tell the caller
			Log.println (Log.ERROR, "Engine.processOpenProject ()", message, e);
			throw new Exception (message, e);
			
		} catch (StreamCorruptedException e) {
			// Not a project file (e.g. a .pdf...)
			String message = Translator.swap ("Engine.itSeemsThisIsNotACorrectProjectFile") + ": " + fileName;
			// Write in the Log, tell the caller
			Log.println (Log.ERROR, "Engine.processOpenProject ()", message, e);
			throw new Exception (message, e);

		} catch (Throwable e) {
			// Other error
			String message = Translator.swap ("Engine.couldNotOpenTheProjectDueToAnException") + ": " + fileName;
			// Write in the Log, tell the caller
			Log.println (Log.ERROR, "Engine.processOpenProject ()", message, e);
			throw new Exception (message, e);

		}

		// Create a relay for the model of the opened project for the current pilot
		try {
			GModel model = project.getModel ();  // the newly opened model has no relay yet
			model.setRelay (modelManager.createRelay (model, pilotName, pilot));
		} catch (Throwable e) {
			// Other error
			String message = Translator.swap ("Engine.couldNotCreateRelayForTheModel") + ": " + fileName;
			// Write in the Log, tell the caller
			Log.println (Log.ERROR, "Engine.processOpenProject ()", message, e);
			throw new Exception (message, e);
		}

		return project;
	}
	
	
	/**	Closes the given project 
	 */
	public void processCloseProject (Project project) {
		// Removes the project from the session
		if (project != null) {
			session.removeProject (project);
			session.setSaved (false);  // session just changed

			project.getRoot().getScene().setStep(null);						// make sure to remove the reference to the first step otherwise it persists and may cause some problem. MF2014-01-19
			// Project dispose may deal with its steps disposal
			project.dispose ();
		}
	}

	/**	Saves As a given project.
	 */
	public void processSaveAsProject (Project project, String fileName) throws Exception {
		if (project == null) {return;}

		// Serialization
		try {
			String desc = new ProjectIdCard (project).getCompactString ();
			
			Writer writer = SerializerFactory.getWriter (fileName);
			
			writer.write (fileName, project, desc);
			
			project.setSaved (true);
			project.setWasSaved (true);
			
		} catch (Exception e) {
			// All errors
			String message = Translator.swap ("Engine.couldNotWriteTheProject") + ": " + project.getName();
			// Write in the Log, tell the caller
			Log.println (Log.ERROR, "Engine.processSaveAsProject ()", message, e);
			throw new Exception (message, e);
		} 
			
	}


// Model loading
	
	/**	Loads a model given its package name (e.g. 'mountain')
	 * 	for the default pilotName.
	 */
	public GModel loadModel (String pkgName) throws Exception {
		return loadModel (pkgName, pilotName);
	}
	
	/**	Loads a model given its package name (e.g. 'mountain')
	 * 	for a given PilotName ('gui', 'script'...) 
	 */
	public GModel loadModel (String pkgName, String pilotName) throws Exception {
		
		GModel model = null;

		try {
			// Instantiation using the default constructor
			model = modelManager.loadModel(pkgName);
			
			updateExtensionManager (model);
			
			// Let's create a pilot relay for the current pilot
			model.setRelay (modelManager.createRelay (model, pilotName, pilot));

		} catch (Exception e) {
			// All errors
			String message = Translator.swap ("Engine.errorWhileLoadingModel") + ": " + pkgName;
			// Write in the Log, tell the caller
			Log.println (Log.ERROR, "Engine.loadModel ()", message, e);
			throw new Exception (message, e);  // fc-28.9.2010 added this throw statement, was missing
		}
		
		return model;
	}


// Pilot management
	
	public String getPilotName () {return pilotName;}
	
	public AbstractPilot getPilot () {return pilot;}
	
	/** Loads the given Pilot. The pilot will then have to be started by start ().
	 * 	If the Pilot can not be created, this is an ERROR (will stop the app). 
	 */
	public void createPilot (String applicationPackageName, String pilotName) {
		
		// Find the pilot className
		String className = null;
		if (pilotName.equals ("script")) {
			// Scripts of all applications can be run with the capsis.script.Pilot
			className = "capsis.script.Pilot";
		} else {
			// Pilot names are normalized
			// e.g. capsis.gui.Pilot, capsis.script.Pilot, jeeb.simeo.gui.Pilot
			className = applicationPackageName + "." + pilotName.trim () + ".Pilot";
		}
		
		try {
			Class<?> c = this.getClass().getClassLoader().loadClass (className);

			Constructor constructor = c.getConstructor (String[].class);
			pilot = (AbstractPilot) constructor.newInstance ((Object) pilotArguments);
			
		} catch (Exception e) {
			// All errors
			String message = Translator.swap ("Engine.couldNotStartPilot") + ": " + className;
			// Write in the Log, tell the caller
			Log.println (Log.ERROR, "Engine.createPilot ()", message, e);
			throw new Error (message, e);
		}
		
	}
	

// Extension manager (optional, may be null)

	/**	The extension manager must be told when a model is loaded. 
	 * 	Note: it may be null in some apps.
	 */
	private void updateExtensionManager (GModel model) {
		// Extension manager is optional
		try {
			extensionManager.getCompatibilityManager().searchVetoes (model);
		} catch (Exception e) {}
		
	}
	
	/**	The capsis.kernel may use the extensionManager if present.
	 * 	This method may return null for some apps.	
	 */
	public ExtensionManager getExtensionManager () {return extensionManager;}
	
// A class loading method for java OR groovy classes

	/**	The groovy class loader.
	 */
	static protected GroovyClassLoader groovyClassLoader;
	
	/**	Inits the groovy class loader.
	 */
	static public void initGroovy ()  {
		groovyClassLoader = null;
	}
	
	/**	Loads a java or groovy class.
	 */
	static public Class<?> loadClass (String name) throws ClassNotFoundException  {
		
		String className = name;
		
		if (name.endsWith (".groovy")) {
			if (groovyClassLoader == null) {
				groovyClassLoader = new GroovyClassLoader (Engine.class.getClassLoader ());
			}
			
			// Try to get the class
			name = name.replace(".groovy", "");
			try {
				return groovyClassLoader.loadClass (name, true, true);
				
			} catch (ClassNotFoundException e) {
				throw e;  // fc-28.9.2010 added this throw (catch clause was empty)
			}
			
//			// try to parse the file
//			name = name.replaceAll("\\.", "/");
//			name = Options.getModelDir() + name + ".groovy";
//			
//			try {
//				System.out.println("parse class");
//				return groovyClassLoader.parseClass(new File(name));
//				
//			} catch (IOException e) {
//				e.printStackTrace();
//				throw new ClassNotFoundException(className);
//			}
		}
		
		return Engine.class.getClassLoader().loadClass(className);
	}


// Other methods
	
	/**	Return The String separator, should not be changed.
	 */
	static public String getStringSeparator () {return "�";}



}



