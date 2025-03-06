package capsis.lib.cstability.util;

import java.io.BufferedWriter;
import java.io.File;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * The log of the model C-STABILITY.
 * 
 * @author J. Sainte-Marie, F. de Coligny - May 2021
 */
public class Log {

	private static PrintWriter printWriter; // Only to write exception to terminal
	private static BufferedWriter out;
	private static Date date;

	static private boolean logActivated = true; // default

	/**
	 * disable ()
	 */
	public static void disable() {
		Log.logActivated = false;
	}

	/**
	 * init(): to be called at C-STABILITY start time
	 */
	public static void init(String outputDir, String fileName) throws Exception {

		if (!Log.logActivated)
			return;

		try {

			date = new java.util.Date();

			String datefilename = new SimpleDateFormat("yyyy-MM-dd_HHmmss").format(date);
			String logFullFileName = outputDir + "/" + datefilename + "_" + fileName;

			File f = new File(outputDir);
			f.mkdirs();

			printWriter = new PrintWriter(logFullFileName);
			out = new BufferedWriter(printWriter);

			Log.trace("C-STABILITY simulation start at " + date + " with inpout file: " + fileName);

		} catch (Exception e) {
			throw e; // can not log the problem
		}
	}

	/**
	 * flush(): writes the buffer to file.
	 */
	public static void flush() {

		if (!Log.logActivated)
			return;

		try {
			printWriter.flush();
			out.flush();
		} catch (Exception e) {
			// Ignore
		}
	}

	/**
	 * close(): to be called at C-STABILITY ending time
	 */
	public static void close() {

		if (!Log.logActivated)
			return;

		flush(); // seems to be needed
		try {
			printWriter.close();
			out.close();
		} catch (Exception e) {
			// Ignore
		}
	}

	/**
	 * trace()
	 */
	public static void trace(String message) {

		if (!Log.logActivated)
			return;

		Log.println("", message, true);
	}

	/**
	 * println()
	 */
	public static void println(String source, String message) {

		if (!Log.logActivated)
			return;

		Log.println(source, message, false);
	}

	/**
	 * println()
	 */
	public static void println(String source, String message, Exception e) {

		if (!Log.logActivated)
			return;

		Log.println(source, message, e, false);
	}

	/**
	 * println()
	 */
	public static void println(String source, String message, boolean copyToTerminal) {

		if (!Log.logActivated)
			return;

		Log.println(source, message, null, copyToTerminal);
	}

	/**
	 * println()
	 */
	public static void println(String source, String message, Exception e, boolean copyToTerminal) {

		if (!Log.logActivated)
			return;

		try {
			String line = message;

			if (source != null && source.length() > 0)
				line = source + ", " + line;

			out.write(line);
			out.newLine();

			if (copyToTerminal)
				System.out.println(line);

			if (e != null) {
				// Write in Log file
				e.printStackTrace(printWriter);
				// Write to terminal
				if (copyToTerminal)
					e.printStackTrace(System.out);
			}

		} catch (Exception ex) {
			System.out.println("Log.println() could not print in Log, exception: " + ex);
			// Ignore
		}
	}

}
