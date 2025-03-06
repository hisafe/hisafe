package capsis.lib.cstability.app;

import java.io.File;
import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Paths;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.filereader.SetupFileLoader;
import capsis.lib.cstability.observer.ObserverList;
import capsis.lib.cstability.util.Log;

/**
 * Starter of C-STABILITY library.
 *
 * to launch a simulation:
 * 
 * java -cp ./class capsis.lib.cstability.app.Cstability setupFileName
 *
 * @author J. Sainte-Marie, F. de Coligny - February 2021
 * 
 *         TODO : * add a mass conservation check * add a display option to the
 *         main method to log display (-D option) * add a debug option for Log
 */
@SuppressWarnings("serial")
public class Cstability implements Serializable {

	private String outputDir;
	private String simulationName;
	private boolean appendMode;
	private Simulator simulator;
	private SetupFileLoader setupFileLoader;

	private Context context;

	/**
	 * main()
	 */
	public static void main(String[] args) throws Exception {
		if (args.length == 1) {
			String setupFilePath = args[0];
			boolean logActivated = true;
			Cstability starter = new Cstability(setupFilePath, logActivated);
			starter.run();
		} else {
			usage();
		}
	}

	/**
	 * usage()
	 */
	private static void usage() {
		System.out.println("C-STABILITY");
		System.out.println("  Expects a setupFileName parameter:");
		System.out.println("  java -cp ./class capsis.lib.cstability.app.Cstability setupFileName");
	}

	/**
	 * Constructor
	 */
	public Cstability(String setupFilePath, boolean logActivated) throws Exception {

		appendMode = false; // set to false for the moment but may be true in the future

		manageOutputDirectory(setupFilePath);

		if (!logActivated)
			Log.disable();

		Log.init(outputDir, simulationName + ".log");

		Log.trace("Loading file " + setupFilePath + "...");

		setupFileLoader = new SetupFileLoader(setupFilePath);
		setupFileLoader.load();

		this.context = setupFileLoader.getContext();

		simulator = new Simulator(setupFileLoader.getParameters(), setupFileLoader.getState0());

	}

	/**
	 * disableObservers():
	 */
	public void disableObservers() {
		simulator.disableObservers();
	}

	/**
	 * manageOutputDirectory()
	 */
	private void manageOutputDirectory(String filePath) throws Exception {

		File f = new File(filePath);
		String workingDirectory = f.getParent();
		String fileName = f.getName();

		simulationName = fileName;
		if (fileName.contains("."))
			simulationName = fileName.substring(0, fileName.lastIndexOf("."));

		outputDir = workingDirectory + "/output_" + simulationName;

		if (!Files.exists(Paths.get(outputDir))) {
			appendMode = false;
			File od = new File(outputDir);
			od.mkdirs();
		}

		if (!appendMode) {
			for (File file : new File(outputDir).listFiles()) {
				if (file.isFile())
					Files.delete(Paths.get(file.getPath()));
			}
		}
	}

	/**
	 * run()
	 */
	public void run() throws Exception {

		simulator.execute(context, setupFileLoader.getObserverList(), false);

		simulator.writeObservations(outputDir, appendMode);

		Log.close();
	}

	/**
	 * run4R()
	 */
	public ObserverList run4R() throws Exception {

		// when C-Stability is run with R, we always use the default observerlist.
		ObserverList ol = new ObserverList();

		simulator.execute(context, ol, true);

		// simulator.writeObservations(outputDir, appendMode); // is not available with
		// R for the moment

		Log.close();

		return ol;
	}

	/**
	 * getContext()
	 */
	public Context getContext() {
		return context;
	}

	/**
	 * getSimulator()
	 */
	public Simulator getSimulator() {
		return simulator;
	}
}
