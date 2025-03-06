package safe.extension.ioformat.safeExportNew;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import jeeb.lib.util.Import;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;

import safe.extension.ioformat.safeExportNew.*;

/**
 * Records description for  EXPORT
 *
 * @author Isabelle Lecomte - September 2020
 */
public class SafeExportFormat extends RecordSet {

	@Import
	static public class ExportRecord extends Record {
		public ExportRecord () {super ();}
		public ExportRecord (String line) throws Exception {super (line);}
		public String subject;			//subject : annualCell/cell/plot 
		public String object;			//SafeTree, SafeCell 
		public String name;				//name of the variable
		public String unit;				//unit
		public String description;		//description
		public int order;				//order in output
		public int export;				//export ? 0=no 1=yes
	}

	@Import
	static public class Subject1Record extends Record {
		public Subject1Record () {super ();}
		public Subject1Record (String line) throws Exception {super (line);}
		public String keyWord;			//keyword ProfileDef
		public String subject;			//subject to export
		public String object;			//SafeTree, SafeCell 
		public int frequency;			//export frequency : 1 30 365
	}
	
	@Import
	static public class Subject2Record extends Record {
		public Subject2Record () {super ();}
		public Subject2Record (String line) throws Exception {super (line);}
		public String keyWord;			//keyword ProfileDef
		public String subject;			//subject to export
		public String object;			//SafeTree, SafeCell 
		public int frequency;			//export frequency : 1 30 365
		public String ids;				//list of ids
	}
	
	@Import
	static public class Subject3Record extends Record {
		public Subject3Record () {super ();}
		public Subject3Record (String line) throws Exception {super (line);}
		public String keyWord;			//keyword ProfileDef
		public String subject;			//subject to export
		public String object;			//SafeTree, SafeCell 
		public int frequency;			//export frequency : 1 30 365
		public String ids;				//list of ids
		public String depth;			//list of depth
	}
	
	// fc-12.10.2020 createRecordSet(fileName) was renamed prepareImport (fileName) for clarity
	public SafeExportFormat (String exportFileName) throws Exception {prepareImport (exportFileName);}
//	public SafeExportFormat (String exportFileName) throws Exception {createRecordSet (exportFileName);}

	/**
	 * Load RecordSet -> SafeExportFormat
	 */
	public void load(ArrayList<SafeExportProfile> exports, String outputDir, String projectName) throws Exception {


		for (Iterator i = this.iterator(); i.hasNext();) {
			Record record = (Record) i.next();

			//SUBJECTS : tree, cells, voxels...
			if (record instanceof SafeExportFormat.Subject1Record) {
				
				SafeExportFormat.Subject1Record cr =
							(SafeExportFormat.Subject1Record) record;	
				SafeExportProfile s = new SafeExportProfile(outputDir, projectName, cr.subject, cr.object, cr.frequency);
				exports.add(s);
			}
			else if (record instanceof SafeExportFormat.Subject2Record) {
				
				SafeExportFormat.Subject2Record cr =
							(SafeExportFormat.Subject2Record) record;	
				SafeExportProfile s = new SafeExportProfile(outputDir, projectName, cr.subject, cr.object, cr.frequency, cr.ids);
				exports.add(s);
			}
			else if (record instanceof SafeExportFormat.Subject3Record) {
				
				SafeExportFormat.Subject3Record cr =
							(SafeExportFormat.Subject3Record) record;	
				SafeExportProfile s = new SafeExportProfile(outputDir, projectName, cr.subject, cr.object, cr.frequency, cr.ids, cr.depth);
				exports.add(s);							
			}
			//VARIABLES : name, description, unit
			else if (record instanceof SafeExportFormat.ExportRecord) {
		
				SafeExportFormat.ExportRecord cr =
							(SafeExportFormat.ExportRecord) record;
		
				if (cr.export==1){		//if variable is selected to be exported

				   for (SafeExportProfile e : exports) {
				        if (e.getSubject().equals(cr.subject)) {
				            e.addVariable(cr.name);
				        }
				    }					   
				}
			}
			else {
				throw new Exception ("Unrecognized record : "+record);	// automatic toString () (or null)
			}		
		}
	}
}
