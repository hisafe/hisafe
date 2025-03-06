package capsis.gui.selectordiagramlist;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import jeeb.lib.util.Log;
import jeeb.lib.util.Record;
import jeeb.lib.util.fileloader.FileLoader;
import capsis.kernel.GModel;
import jeeb.lib.util.PathManager;

/**
 * A loader for a file of diagram lists. A DiagramList has a name and contains
 * several diagrams. The file contains several DiagramLists.
 * 
 * @author F. de Coligny - December 2015
 */
public class DiagramListLoader extends FileLoader {

	private static final String DIAGRAM_LISTS_DIR = PathManager.getInstallDir() + "/etc/diagramLists/";

	static {
		// Ensure the dir exists
		try {
			new File(DIAGRAM_LISTS_DIR).mkdirs();
		} catch (Exception e) {
			Log.println(Log.ERROR, "DiagramListLoader static initializer", "Could not make directory: "
					+ DIAGRAM_LISTS_DIR, e);
		}
	}

	// Each DiagramList has one DiagramListRecord and several ContentsRecords
	public List<DiagramListRecord> diagramListRecords;
	public List<ContentsRecord> diagramContentRecords;

	private GModel model;

	private List<DiagramList> diagramLists;

	/**
	 * Constructor for loading.
	 * 
	 * <pre>
	 * DiagramListLoader l = new DiagramListLoader(model);
	 * String report = l.load();
	 * if (!l.succeeded()) {...}
	 * List<DiagramList> dls = l.getDiagramLists();
	 * </pre>
	 */
	public DiagramListLoader(GModel model) throws Exception {
		super();
		this.model = model;
	}

	/**
	 * Constructor for saving.
	 * 
	 * <pre>
	 * DiagramListLoader l = new DiagramListLoader(model, diagramLists);
	 * l.save();
	 * </pre>
	 */
	public DiagramListLoader(GModel model, List<DiagramList> diagramLists) throws Exception {
		this.model = model;
		this.diagramLists = diagramLists;

		// Prepare a header for the file
		add(new CommentRecord("DiagramlList file for " + model.getIdCard().getModelPackageName()));
		add(new CommentRecord("This file was created by capsis.gui.selectordiagramlist.DiagramListLoader"));
		add(new CommentRecord("Please do not edit"));
		add(new EmptyRecord());

		diagramListRecords = new ArrayList<>();
		diagramContentRecords = new ArrayList<>();

		int k = 1;
		for (DiagramList dl : diagramLists) {

			// The locked diagramLists were managed by program, not saved
			// fc-10.12.2015
			if (dl.isLocked())
				continue;

			DiagramListRecord r = new DiagramListRecord(k, dl.getName());
			diagramListRecords.add(r);
			this.add(r);

			for (DiagramLine d : dl.getDiagramLines()) {
				ContentsRecord cr = new ContentsRecord(k, d.getType(), d.getClassName(), d.getEncodedBounds(),
						d.getRendererClassName());
				diagramContentRecords.add(cr);
				this.add(cr);
			}

			k++;
		}

	}

	@Override
	protected void checks() throws Exception {
	}

	/**
	 * Returns the name of the diagramLists file for the given model.
	 */
	public String getDiagramListsDir(GModel model) {
		return DIAGRAM_LISTS_DIR + model.getIdCard().getModelPackageName() + ".diagramLists"; // e.g.
																								// pp3
	}

	/**
	 * Returns true if a diagram list file exists for the model
	 */
	public boolean diagramListDirExists () {
		// fc-11.1.2017
		try {
			return new File (getDiagramListsDir(this.model)).exists();
		} catch (Exception e) {
			return false;
		}
	}
	
	/**
	 * Loads the file (super.load ()), then creates the DiagramLists to be
	 * returned. Calling getDiagramLists() after will return the result.
	 */
	public void load() throws Exception {
		load(getDiagramListsDir(model));
	}

	/**
	 * The load () method should be called instead of this method (automatic
	 * file name management).
	 */
	@Override
	public void load(String fileName) throws Exception {
		
		// fc-6.12.2017 no more report in FileLoader: ok or exception
		
		super.load(fileName);

//		String NEW_LINE = "\n";
//		StringBuffer thisReport = new StringBuffer("Rebuilding diagramList instances..." + NEW_LINE);

		// Selector selector = MainFrame.getInstance().getSelector();

		// Create and store the DLs
		Map<Integer, DiagramList> dlMap = new HashMap<>();

		for (DiagramListRecord r : diagramListRecords) {
			DiagramList dl = new DiagramList(r.name);
			dlMap.put(r.id, dl);

//			thisReport.append("Building " + r.name + "..." + NEW_LINE);
		}

		// Read the contents and add diagrams in the DLs
		for (ContentsRecord r : diagramContentRecords) {
			try {
				DiagramList dl = dlMap.get(r.diagramListId);
				DiagramLine d = new DiagramLine(r.className, r.type, r.bounds);
				if (r.rendererClassName != null && !r.rendererClassName.equals("-"))
					d.setRendererClassName(r.rendererClassName);
				dl.addDiagramLine(d);
			} catch (Exception e) {
				Log.println(Log.WARNING, "DiagramListLoader.load (fileName)", 
						"Could not load a diagramLine: "+r, e);
			}
		}

		// Detect and remove the empty diagramLists (security)
		for (Iterator<Integer> i = dlMap.keySet ().iterator (); i.hasNext ();) {
			int id = i.next ();
			DiagramList dl = dlMap.get(id);
			if (dl.isEmpty())
				i.remove ();
		}
		
		diagramLists = new ArrayList<DiagramList>(dlMap.values());

//		thisReport.append("Built " + diagramLists.size() + " diagramLists" + NEW_LINE);
//
//		return ""+thisReport;
	}

	/**
	 * Saves the file with super.load (fileName).
	 */
	public void save() throws Exception {
		save(getDiagramListsDir(model));
	}

	/**
	 * Returns the list of DiagramList objects.
	 */
	public List<DiagramList> getDiagramLists() {
		return diagramLists;
	}

	/**
	 * A line in the file
	 */
	static public class DiagramListRecord extends Record {

		public int id;
		public String name;

		/**
		 * Constructor for loading, relies on Record superclass.
		 */
		public DiagramListRecord(String line) throws Exception {
			super(line);
		}

		/**
		 * Constructor for saving.
		 */
		public DiagramListRecord(int id, String name) {
			this.id = id;
			this.name = name;
		}

	}

	/**
	 * A line in the file
	 */
	static public class ContentsRecord extends Record {

		// The id of the DL this contents belongs to
		public int diagramListId;
		// This is the type of the diagram (see Diagram)
		public String type;
		// This is the className of the diagram
		public String className;
		// Bounds of the diagram: x y w h
		public String bounds;
		// rendererClassName for extractors, "-" if not set
		public String rendererClassName;

		/**
		 * Constructor for loading, relies on Record superclass.
		 */
		public ContentsRecord(String line) throws Exception {
			super(line);
		}

		/**
		 * Constructor for saving.
		 */
		public ContentsRecord(int diagramListId, String type, String className, String bounds, String rendererClassName) {
			this.diagramListId = diagramListId;
			this.type = type;
			this.className = className;
			this.bounds = bounds;
			this.rendererClassName = rendererClassName == null ? "-" : rendererClassName;
		}

	}

}
