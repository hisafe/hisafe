/*
 * mathutil library for Capsis4.
 *
 * Copyright (C) 2004 Francois de Coligny.
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

package capsis.lib.math;


import java.io.Serializable;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Random;
import java.util.Vector;


/**
 * OneVariableRelationsSet : Contains relations between an y variable and an x variable
 * for the same relation type (for example height=f(dbh)),
 * it is possible to specify different modalities (for example species)
 * each modality have a independent set of parameters for the specified equation
 *
 * @author A. Piboule - April 2004
 */
public class OneVariableRelationsSet implements Serializable {

	private static Random rnd;

	private Hashtable relations;
	private Hashtable variables;


	public String toString () {
		String str = "";
		for (Enumeration e = relations.keys() ; e.hasMoreElements() ;) {
			String varName = (String) e.nextElement();
			str = str + varName + "[";
			Relation r = (Relation) relations.get (varName);

			for (int  i =0;i<r.modules.size () ; i++) {
				CalcModule cm = (CalcModule) r.modules.get (i);
				str = str+cm.lineNb+"_"+cm.before+"_"+cm.operator+"_"+cm.after+";";
			}
			str = str+ "]";
			str = str + "("+r.modalities+"/"+r.numbers+")";

		}
		return str;
	}

	public OneVariableRelationsSet () {
		relations = new Hashtable ();
		rnd = new Random ();
	}

	public Relation addRelation (String relType) {
		Relation rel = new Relation ();
		relations.put (relType.toLowerCase (), rel);
		return rel;
	}

	public Relation getRelation (String relType) {
		return (Relation) relations.get (relType);
	}

	public double computeModule (CalcModule mod) {
		Double b = ((Double) variables.get (mod.before));
		Double a = ((Double) variables.get (mod.after));

		double result = Double.NaN;

		if ((b!=null) && (a!=null)) {
			double before = b.doubleValue ();
			double after = a.doubleValue ();

			if (mod.operator.equals ("+")) {
				result = before + after;
			} else if (mod.operator.equals ("-")) {
				result = before - after;
			} else if (mod.operator.equals ("*")) {
				result = before * after;
			} else if (mod.operator.equals ("/")) {
				result = before / after;
			} else if (mod.operator.equals ("^")) {
				result = Math.pow (before, after);
			} else if (mod.operator.equals ("exp")) {
				result = Math.exp (before);
			} else if (mod.operator.equals ("ln")) {
				result = Math.log (before);
			} else if (mod.operator.equals ("cos")) {
				result = Math.cos (before);
			} else if (mod.operator.equals ("sin")) {
				result = Math.sin (before);
			} else if (mod.operator.equals ("tan")) {
				result = Math.tan (before);
			} else if (mod.operator.equals ("acos")) {
				result = Math.acos (before);
			} else if (mod.operator.equals ("asin")) {
				result = Math.asin (before);
			} else if (mod.operator.equals ("atan")) {
				result = Math.atan (before);
			} else if (mod.operator.equals ("abs")) {
				result = Math.abs (before);
			}
		}

		return result;
	}


	public double getEstimatedValue (String rel, Integer modality, double x) {
		Relation r = (Relation) relations.get (rel.toLowerCase ());
	 	double result = Double.NaN;

	 	if (r!=null) {
		variables = new Hashtable ();
		Hashtable v = (Hashtable) r.modalities.get (modality);

			if (v!=null) {
				for (Enumeration e = v.keys() ; e.hasMoreElements() ;) {
					String varName = (String) e.nextElement();
					variables.put (varName, v.get(varName));
				}
			}

			for (Enumeration e = r.numbers.keys() ; e.hasMoreElements() ;) {
				String varName = (String) e.nextElement();
				variables.put (varName, r.numbers.get(varName));
			}

			variables.put ("x", new Double (x));

			for (int i = 0; i<r.modules.size ();i++) {
				CalcModule mod = (CalcModule) r.modules.get (i);
				result = computeModule (mod);
				variables.put (new String ("l"+mod.lineNb), new Double (result));
			}

			Double sigma = (Double) variables.get ("sigma");

			if (sigma!=null) {
				result = result + sigma.doubleValue ()*rnd.nextGaussian ();
			}
		}

	 	return result;
	}


/***************************************************/
	public static class Relation implements Serializable {
		public Vector modules;
		public Hashtable modalities;
		public Hashtable numbers;

		public Relation () {
			modules = new Vector ();
			modalities = new Hashtable ();
			numbers = new Hashtable ();
		}

		public CalcModule addModule (String l, String b, String o, String a) {

			CalcModule mod = new CalcModule (l, b, o, a);
			modules.add (mod);

			try {
			double db = Double.parseDouble (mod.after);
			numbers.put (mod.after.toLowerCase (), new Double (db));
			} catch (Exception e) {
			}

			return mod;
		}

		public Hashtable addModality (Integer modality) {
			Hashtable mod = new Hashtable ();
			modalities.put (modality, mod);
			return mod;
		}

		public void addVariable (Integer modality, String name, double value) {
			Hashtable prms = (Hashtable) modalities.get (modality);
			if (prms==null) {
				prms = addModality (modality);
			}
			prms.put (name, new Double (value));
		}
	}


	static class CalcModule implements Serializable {
		public String lineNb;
		public String before;
		public String operator;
		public String after;

		public CalcModule (String l, String b, String o, String a) {
			lineNb=l;
			before=b;
			operator=o;
			after=a;
		}
	}


public static void main (String[] argv) {
	OneVariableRelationsSet a = new OneVariableRelationsSet ();
	Relation rl = a.addRelation ("height");
	rl.addModality (new Integer (1));

	rl.addVariable (new Integer (1), "a",7.0781d);
	rl.addVariable (new Integer (1), "b",-4.656d);
	rl.addVariable (new Integer (2), "a",7.0781d);
	rl.addVariable (new Integer (2), "b",-4.656d);
	rl.addVariable (new Integer (2), "sigma",2d);


	rl.addModule ("1", "x","ln","1");
	rl.addModule ("2", "l1","*","a");
	rl.addModule ("3", "l2","+","b");

	System.out.println ("Estimation 1 : "+a.getEstimatedValue ("height", new Integer (1), 80d));
	System.out.println ("Estimation 2 : "+a.getEstimatedValue ("height", new Integer (2), 80d));
	System.out.println (a);
}

}
