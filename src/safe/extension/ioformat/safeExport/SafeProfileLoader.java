package safe.extension.ioformat.safeExport;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Vector;

import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;

/**
 * A loader for the SafeExport profile file. Builds a Map profileName -> SafeProfile.
 * 
 * @author F. de Coligny, Quentin Molto - February 2014
 */
public class SafeProfileLoader extends RecordSet {

	static public class General extends Record {

		public boolean allSteps;
		public boolean currentStep;
		public boolean frequency;
		public int frequencyBegin;
		public int frequencyValue;
		public boolean period;
		public int periodTo;
		public int periodFrom;
		public boolean rootStepToExport;

		public General () {}

		public General (String line) throws Exception {
			super (line);
		}
	}

	// For PlotOfCells, Climate, Layers...
	static public class Subject extends Record {

		public String name;
		public Collection<String> variables;

		public Subject () {}

		public Subject (String line) throws Exception {
			super (line);
		}

	}

	// For Tree, Cell, Crop
	static public class SubjectTreeCellCrop extends Record {

		public String name;
		public Collection<String> variables;
		public boolean exportAll;		//all cells selection
		public Collection ids;		//id cells selection

		public SubjectTreeCellCrop () {}

		public SubjectTreeCellCrop (String line) throws Exception {
			super (line);
		}

	}

	// For Voxel only
	static public class SubjectVoxel extends Record {

		public String name;
		public Collection<String> variables;
		public boolean exportAll;		//all voxels selection
		public Collection ids;			//id cells selection
		public Collection depths;		//voxel depths selection

		public SubjectVoxel () {}

		public SubjectVoxel (String line) throws Exception {
			super (line);
		}

	}


	private SafeExportProfile profile;

	// Load: SafeExportProfile profile = new SafeProfileLoader ().load (fileName);
	// Save: new SafeProfileLoader (profile).save (fileName);

	// -- load

	public SafeProfileLoader () {
		addAdditionalClass (General.class);
		addAdditionalClass (Subject.class);
		addAdditionalClass (SubjectTreeCellCrop.class);
		addAdditionalClass (SubjectVoxel.class);
	}

	/**
	 * Loads and returns the profile with the given name.
	 */
	public SafeExportProfile load (String fileName) throws Exception {

		prepareImport (fileName);

		// interpret
		SafeExportProfile p = new SafeExportProfile ();

		for (Iterator i = this.iterator (); i.hasNext ();) {
			Record record = (Record) i.next ();

			if (record instanceof General) {
				General g = (General) record;
				p.extendAllSteps = g.allSteps;
				p.extendCurrentStep = g.currentStep;
				p.extendFrequency = g.frequency;
				p.extendFrequencyBegin = g.frequencyBegin;
				p.extendFrequencyValue = g.frequencyValue;
				p.extendPeriod = g.period;
				p.extendPeriodTo = g.periodTo;
				p.extendPeriodFrom = g.periodFrom;
				p.extendRootStepToExport = g.rootStepToExport;

			} else if (record instanceof SubjectTreeCellCrop) {
				SubjectTreeCellCrop r = (SubjectTreeCellCrop) record;
				
				SafeExportSubjectToExport aux = null;
				
				if (r.name.equals ("SafeTree")) {
					SafeExportSubjectTreeToExport out = new SafeExportSubjectTreeToExport (r.name, null);
					aux = out;
					out.exportAll = r.exportAll;
					out.ids = r.ids;
					
				} else if (r.name.equals ("SafeLayer")) {
					SafeExportSubjectLayerToExport out = new SafeExportSubjectLayerToExport (r.name, null);
					aux = out;
					out.exportAll = r.exportAll;
					out.ids = r.ids;					
					
				} else if (r.name.equals ("SafeCell")) {
					SafeExportSubjectCellToExport out = new SafeExportSubjectCellToExport (r.name, null);
					aux = out;
					out.exportAll = r.exportAll;
					out.ids = r.ids;
					
				} else { // SafeCrop
					SafeExportSubjectCropToExport out = new SafeExportSubjectCropToExport (r.name, null);
					aux = out;
					out.exportAll = r.exportAll;
					out.ids = r.ids;
					
				}
				
				String[] vars = new String[r.variables.size ()];
				int k = 0;
				for (String v : r.variables) {
					vars[k++] = v;
				}
				aux.setVariables (vars);
				
				if (p.subjectsToExport == null) p.subjectsToExport = new Vector ();
				p.subjectsToExport.add (aux);

			} else if (record instanceof SubjectVoxel) {
				SubjectVoxel r = (SubjectVoxel) record;
				SafeExportSubjectVoxelToExport out = new SafeExportSubjectVoxelToExport (r.name, null);
				out.exportAll = r.exportAll;
				out.cellIds = r.ids;
				
				out.depths = r.depths;
				
				String[] vars = new String[r.variables.size ()];
				int k = 0;
				for (String v : r.variables) {
					vars[k++] = v;
				}
				out.setVariables (vars);
				
				if (p.subjectsToExport == null) p.subjectsToExport = new Vector ();
				p.subjectsToExport.add (out);
				
			} else if (record instanceof Subject) {
				Subject s = (Subject) record;
				SafeExportSubjectToExport out = new SafeExportSubjectToExport (s.name, null);
				
				String[] vars = new String[s.variables.size ()];
				int k = 0;
				for (String v : s.variables) {
					vars[k++] = v;
				}
				out.setVariables (vars);
				
				if (p.subjectsToExport == null) p.subjectsToExport = new Vector ();
				p.subjectsToExport.add (out);
				
			} else {
				throw new Exception ("SafeExportProfile: unknown record: "+record);
			}

		}

		return p;
	}

	// save

	public SafeProfileLoader (SafeExportProfile profile) {
		this.profile = profile;
	}

	public void save (String fileName) throws Exception {

		add (new CommentRecord ("allSteps	currentStep	frequency	frequencyBegin	frequencyValue	period	periodTo	periodFrom	rootStepToExport"));
		
		General g = new General ();

		g.allSteps = profile.extendAllSteps;
		g.currentStep = profile.extendCurrentStep;
		g.frequency = profile.extendFrequency;
		g.frequencyBegin = profile.extendFrequencyBegin;
		g.frequencyValue = profile.extendFrequencyValue;
		g.period = profile.extendPeriod;
		g.periodTo = profile.extendPeriodTo;
		g.periodFrom = profile.extendPeriodFrom;
		g.rootStepToExport = profile.extendRootStepToExport;

		add (g);

		for (Object o : profile.subjectsToExport) {
			
			SafeExportSubjectToExport ste = (SafeExportSubjectToExport) o;
			
			if (o instanceof SafeExportSubjectVoxelToExport) {
				add (new CommentRecord ("subject	variables	exportAll	cellIds	depths"));
				SafeExportSubjectVoxelToExport i = (SafeExportSubjectVoxelToExport) o;
				SubjectVoxel r = new SubjectVoxel ();
				r.exportAll = i.exportAll;
				r.ids = (i.cellIds == null) ? new ArrayList () : i.cellIds;
				r.depths = (i.depths == null) ? new ArrayList () : i.depths;
				
				r.name = ste.getName ();
				r.variables = new ArrayList<String> ();
				for (String s : ste.getVariables ()) {
					r.variables.add (s);
				}
				add (r);
				
			} else if (o instanceof SafeExportSubjectTreeToExport) {
				add (new CommentRecord ("subject	variables	exportAll	ids"));
				SafeExportSubjectTreeToExport i = (SafeExportSubjectTreeToExport) o;
				SubjectTreeCellCrop r = new SubjectTreeCellCrop ();
				r.exportAll = i.exportAll;
				r.ids = (i.ids == null) ? new ArrayList () : i.ids;
				
				r.name = ste.getName ();
				r.variables = new ArrayList<String> ();
				for (String s : ste.getVariables ()) {
					r.variables.add (s);
				}
				add (r);
			} else if (o instanceof SafeExportSubjectLayerToExport) {
				add (new CommentRecord ("subject	variables	exportAll	ids"));
				SafeExportSubjectLayerToExport i = (SafeExportSubjectLayerToExport) o;
				SubjectTreeCellCrop r = new SubjectTreeCellCrop ();
				r.exportAll = i.exportAll;
				r.ids = (i.ids == null) ? new ArrayList () : i.ids;
				
				r.name = ste.getName ();
				r.variables = new ArrayList<String> ();
				for (String s : ste.getVariables ()) {
					r.variables.add (s);
				}
				add (r);				
				
			} else if (o instanceof SafeExportSubjectCellToExport) {
				add (new CommentRecord ("subject	variables	exportAll	ids"));
				SafeExportSubjectCellToExport i = (SafeExportSubjectCellToExport) o;
				SubjectTreeCellCrop r = new SubjectTreeCellCrop ();
				r.exportAll = i.exportAll;
				r.ids = (i.ids == null) ? new ArrayList () : i.ids;
				
				r.name = ste.getName ();
				r.variables = new ArrayList<String> ();
				for (String s : ste.getVariables ()) {
					r.variables.add (s);
				}
				add (r);
				
			} else if (o instanceof SafeExportSubjectCropToExport) {
				add (new CommentRecord ("subject	variables	exportAll	ids"));
				SafeExportSubjectCropToExport i = (SafeExportSubjectCropToExport) o;
				SubjectTreeCellCrop r = new SubjectTreeCellCrop ();
				r.exportAll = i.exportAll;
				r.ids = (i.ids == null) ? new ArrayList () : i.ids;
				
				r.name = ste.getName ();
				r.variables = new ArrayList<String> ();
				for (String s : ste.getVariables ()) {
					r.variables.add (s);
				}
				add (r);
				
			} else { // For PlotOfCells, Climate, Layers...
				add (new CommentRecord ("subject	variables"));
				Subject r = new Subject ();
				
				r.name = ste.getName ();
				r.variables = new ArrayList<String> ();
				for (String s : ste.getVariables ()) {
					r.variables.add (s);
				}
				add (r);
				
			}
			
		}

		super.save (fileName);

	}

	public static void main (String[] args) {

	}

}
