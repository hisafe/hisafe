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
package capsis.extension.ioformat;

// This extension is for export to the Marc Jaeger Blob Format
import java.text.NumberFormat;
import java.util.Iterator;
import java.util.Locale;

import jeeb.lib.defaulttype.SimpleCrownDescription;
import jeeb.lib.util.Import;
import jeeb.lib.util.Log;
import jeeb.lib.util.Record;
import jeeb.lib.util.Translator;
import capsis.defaulttype.SpatializedTree;
import capsis.defaulttype.TreeCollection;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.OFormat;
import capsis.util.StandRecordSet;

/**
 * JaegerBlobFormat contains records description for connection with the Marc Jaeger
 * voxels data structure based platform.
 *
 * @author F. de Coligny (Beijing) - 6.6.2005
 */
public class JaegerBlobFormat extends StandRecordSet implements OFormat {

	// fc-4.12.2020 Checked, is an export, added OFormat

	static {
		Translator.addBundle("capsis.extension.ioformat.JaegerBlobFormat");
	}

	// Generic keyword record is described in superclass: key = value

	static private NumberFormat formater;
	static {
		formater = NumberFormat.getNumberInstance (Locale.ENGLISH);
		formater.setGroupingUsed (false);
		formater.setMaximumFractionDigits (4);
	}

	// JaegerBlobRecord0 record
	// First case (ex: a sphere) : d1, x1, y1, z1 are not needed
	@Import
	static public class JaegerBlobRecord0 extends Record {
		public JaegerBlobRecord0 () {super ();}
		public JaegerBlobRecord0 (String line) throws Exception {super (line);}
		public String getSeparator () {return " ";}	// to change default "\t" separator
		public NumberFormat getNumberFormat () {return JaegerBlobFormat.formater;}	// fc - 16.3.2004
		public int	type;	// 0:sphere 1:cylinder
		public int	color;	// 11:wood 20:crown
		public double d0;	// point1, diameter
		public double x0;	// point1, x
		public double y0;	// point1, y
		public double z0;	// point1, z
	}

	// JaegerBlobRecord1 record
	// Second case (ex: a cylinder) : d1, x1, y1, z1 are required
	@Import
	static public class JaegerBlobRecord1 extends Record {
		public JaegerBlobRecord1 () {super ();}
		public JaegerBlobRecord1 (String line) throws Exception {super (line);}
		public String getSeparator () {return " ";}	// to change default "\t" separator
		public NumberFormat getNumberFormat () {return JaegerBlobFormat.formater;}	// fc - 16.3.2004
		public int	type;	// 0:sphere 1:cylinder
		public int	color;	// 11:wood 20:crown
		public double d0;	// point1, diameter
		public double x0;	// point1, x
		public double y0;	// point1, y
		public double z0;	// point1, z
		public double d1;	// point2, diameter (optionnal)
		public double x1;	// point2, x (optionnal)
		public double y1;	// point2, y (optionnal)
		public double z1;	// point2, z (optionnal)
	}


	/**	Phantom constructor.
	*	Only to ask for extension properties (authorName, version...).
	*/
	public JaegerBlobFormat () {}


	/**	Official constructor
	*	Format in Export mode needs a Stand in starter (then call save (fileName))
	*	Format in Import mode needs fileName in starter (then call load (GModel))
	*/
	@Override
	public void initExport(GModel m, Step s) throws Exception {

		if (s.getScene () != null) {
			// Export mode
			
			JaegerBlobFormatDialog dlg = new JaegerBlobFormatDialog ();
			if (!dlg.isValidDialog ()) {	// user canceled dialog -> stop
				dlg.dispose ();
				return;
			}
			
			// Get settings in the dialog..., pass them to createRecordSet (...)
			
			prepareExport (s.getScene ());
		
		// No import for JaegerBlobFormat
		//~ } else if (s.getString () != null) {
			//~ // Import mode
			//~ createRecordSet (s.getString ());

		} 
	}

	/**	Extension dynamic compatibility mechanism.
	*	This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	*/
	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof GModel)) {return false;}
			GModel m = (GModel) referent;
			GScene s = ((Step) m.getProject ().getRoot ()).getScene ();
			if (!(s instanceof TreeCollection)) {return false;}
			TreeCollection tc = (TreeCollection) s;
			if (tc.getTrees ().isEmpty ()) {return true;}	// will be checked again later
			if (!(tc.getTrees ().iterator ().next () instanceof SpatializedTree)) {return false;}
			if (!(tc.getTrees ().iterator ().next () instanceof SimpleCrownDescription)) {return false;}

		} catch (Exception e) {
			Log.println (Log.ERROR, "JaegerBlobFormat.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getName () {
		return Translator.swap ("JaegerBlobFormat.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor () {
		return "F. de Coligny";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription () {
		return Translator.swap ("JaegerBlobFormat.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion () {
		return "1.0";
	}

	// nb-09.08.2018
	//public static final String VERSION = "1.0";

	/**	Export: Stand -> RecordSet - Implementation here.
	*	(RecorSet -> File in superclass)
	*/
	@Override
	public void prepareExport (GScene stand) throws Exception {
		// super class method adds comments. NO COMMENT LINES in JaegerBlobFormat
		//~ super.createRecordSet (stand);		// deals with RecordSet's source

		try {
			GModel model = stand.getStep ().getProject ().getModel ();
			
			// NO COMMENT LINES in JaegerBlobFormat
			//~ add (new CommentRecord ("JaegerBlobFormatExport"));
			//~ add (new KeyRecord ("speciesNumber", ""+speciesNumber));
			//~ add (new EmptyRecord ());
			
			TreeCollection tc = (TreeCollection) stand;	// The stand must be a tree collection
			
			// NO COMMENT LINES => no header
			setHeaderEnabled (false);

			// 1. JaegerBlobRecords
			for (Iterator i = tc.getTrees ().iterator (); i.hasNext ();) {
				SpatializedTree t = (SpatializedTree) i.next ();
				SimpleCrownDescription c = (SimpleCrownDescription) t;
				
				// Slope enabled / disabled
				double treeZ = 0;
				//~ double treeZ = t.getZ ();
				
				JaegerBlobRecord1 trunk = makeCylinder (1, 11, 
						t.getDbh ()/100, t.getX (), t.getY (), treeZ, 
						0, t.getX (), t.getY (), treeZ + t.getHeight ());
				add (trunk);
				
				// CONIC crown
				if (c.getCrownType () == SimpleCrownDescription.CONIC) {
					JaegerBlobRecord1 crown = makeCylinder (1, 20, 
							c.getCrownRadius () * 2, t.getX (), t.getY (), treeZ + c.getCrownBaseHeight (), 
							0, t.getX (), t.getY (), treeZ + t.getHeight ());
					add (crown);
					
				// SPHERIC crown (ellipsoidal) : 3 conic layers
				} else {
					double crownHeight = t.getHeight () - c.getCrownBaseHeight ();
					double crownDiameter = c.getCrownRadius () * 2;
					double sliceHeight = crownHeight / 3;
					double y1 = treeZ + c.getCrownBaseHeight ();
					double y2 = y1 + sliceHeight;
					double y3 = y2 + sliceHeight;
					double y4 = y3 + sliceHeight;
					
					JaegerBlobRecord1 c0 = makeCylinder (1, 20, 
							0, t.getX (), t.getY (), y1, 
							crownDiameter, t.getX (), t.getY (), y2);
					add (c0);
					
					JaegerBlobRecord1 c1 = makeCylinder (1, 20, 
							crownDiameter, t.getX (), t.getY (), y2, 
							crownDiameter, t.getX (), t.getY (), y3);
					add (c1);
					
					JaegerBlobRecord1 c2 = makeCylinder (1, 20, 
							crownDiameter, t.getX (), t.getY (), y3, 
							0, t.getX (), t.getY (), y4);
					add (c2);
					
				}
			}

		} catch (Exception e) {
			Log.println (Log.ERROR, "JaegerBlobFormat.createRecordSet ()", "Exception ", e);
		}

	}

	//	Create a record of type JaegerBlobRecord0 : SPHERE
	//
	private JaegerBlobRecord0 makeSphere (int type, int color, 
			double d0, double x0, double y0, double z0) {
		JaegerBlobRecord0 sphere = new JaegerBlobRecord0 ();
		sphere.type = type;	// sphere for the sphere
		sphere.color = color;	// sphere
		sphere.d0 = d0;
		sphere.x0 = x0;
		sphere.y0 = y0;
		sphere.z0 = z0;
		return sphere;
	}

	//	Create a record of type JaegerBlobRecord1 : CYLINDER
	//
	private JaegerBlobRecord1 makeCylinder (int type, int color, 
			double d0, double x0, double y0, double z0, 
			double d1, double x1, double y1, double z1) {
		JaegerBlobRecord1 cylinder = new JaegerBlobRecord1 ();
		cylinder.type = type;	// cylinder for the cylinder
		cylinder.color = color;	// cylinder
		cylinder.d0 = d0;
		cylinder.x0 = x0;
		cylinder.y0 = y0;
		cylinder.z0 = z0;
		cylinder.d1 = d1;
		cylinder.x1 = x1;
		cylinder.y1 = y1;
		cylinder.z1 = z1;
		return cylinder;
	}

	/**	Import: special use without extension mechanism (direct).
	*	<pre>
	*	GStand initStand = new <ThisClass> (fileName).load (GModel);
	*	</pre>
	*/
	public JaegerBlobFormat (String fileName) throws Exception {prepareImport (fileName);}	// direct use for Import

	/**	Import: RecordSet -> Stand - Implementation here.
	*	(File -> RecordSet in superclass).
	*/
	public GScene load (GModel model) throws Exception {
		return null;	// import not implemented
 	}

	// fc-7.12.2020 Deprecated
//	////////////////////////////////////////////////// IOFormat stuff
//
//	public boolean isImport () {return false;}
//
//	public boolean isExport () {return true;}

}

