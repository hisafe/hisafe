package capsis.util;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;

import com.izforge.izpack.util.AbstractUIProcessHandler;

/**
 * Reads files and corrects the eol by the correct eol for the os we are running
 * on. Used to fix the end of line characters after an IzPack install if the
 * source and target os are not the same (e.g. IzPack installer built under
 * Windows, and launched under Linux or Mac -> capsis.sh will contain CRLF end
 * of lines, changes them to LF).
 * 
 * @author F. de Coligny - February 2017
 */
public class EOLCorrector {

	private AbstractUIProcessHandler handler;
	private String installPath;
	private File installDir;
	private String osName;

	/**
	 * Called by IzPack installer according to its installer.xml command file,
	 * linked to a ProcessPanel
	 */
	public void run(AbstractUIProcessHandler handler, String[] args) {
		this.handler = handler;

		handler.logOutput("Post installation script...", false);

		if (args == null || args.length < 1) {
			handler.logOutput("Missing installPath, could not run end of line correction, skipped", false);
			return;
		}

		this.installPath = args[0];
		this.osName = System.getProperty("os.name");
		this.installDir = new File(installPath);

		fixFiles();

		setExecutable();

		handler.logOutput("Post installation script terminated", false);
		// handler.emitNotification("Process finished");
	}

	/**
	 * Returns true if the given file is an Unix script
	 */
	private boolean isUnixScript(File f) {
		return !f.isDirectory() && f.getName().endsWith(".sh");
	}

	/**
	 * Try to set executable the .sh scripts under Unix systems
	 */
	private void setExecutable() {

		// Only for Unix systems
		if (!osName.toLowerCase ().startsWith("linux") && !osName.toLowerCase ().startsWith("mac"))
			return;

		handler.logOutput("Setting scripts executable for " + osName, false);

		// Pathnames for files and directory
		File[] files = installDir.listFiles();

		// For each pathname
		int count = 0;
		int fail = 0;
		for (File f : files) {
			if (isUnixScript(f)) {

				try {
					// May fail depending on user privileges
					f.setExecutable(true);
					count++;
					
				} catch (Exception e) {
					fail++;
				}

			}
		}

		if (count + fail == 0) {
			handler.logOutput("Could not find any Unix script to be processed", false);
			return;
		}
			
		
		if (count > 0)
			handler.logOutput("Processed " + count + " file(s)", false);
		if (fail > 0)
			handler.logOutput("Failed for " + fail + " file(s)", false);

	}

	/**
	 * Returns true if the given file eols must be fixed
	 */
	private boolean isToBeFixed(File f) {
		return !f.isDirectory() && (f.getName().endsWith(".sh") || f.getName().endsWith(".bat"));
	}

	/**
	 * Lists the files to be fixed
	 */
	private void fixFiles() {

		handler.logOutput("Fixing end-of-line characters for " + osName, false);
		handler.logOutput("Base directory: " + installDir.getAbsolutePath(), false);

		// Pathnames for files and directory
		File[] files = installDir.listFiles();

		// For each pathname
		int count = 0;
		for (File f : files) {
			if (isToBeFixed(f)) {
				boolean done = fixEolChars(f);
				if (done)
					count++;
			}
		}

		handler.logOutput("Fixed " + count + " files", false);

	}

	/**
	 * Fixes the eol character for each line in the given file
	 */
	private boolean fixEolChars(File f) {

		StringBuffer b = new StringBuffer("Fixing " + f + "...");

		StringBuffer fixedContent = new StringBuffer();

		boolean processOk = false;

		try {
			// Read all lines, change the eol
			BufferedReader in = new BufferedReader(new FileReader(f));
			String str;
			while ((str = in.readLine()) != null) {

				// This eol is CRLF under Windows and LF under Unix
				fixedContent.append(str + System.getProperty("line.separator"));

			}
			in.close();

			// Write the fixed content into the file
			BufferedWriter out = new BufferedWriter(new FileWriter(f));
			out.write(fixedContent.toString());
			out.close();

			processOk = true;

			b.append(" done");

		} catch (Exception e) {
			b.append(" ERROR: " + e.getMessage());
		}

		handler.logOutput(b.toString(), false);

		return processOk;
	}

}
