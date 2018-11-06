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

package safe.extension.ioformat.safeExport;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;
import java.util.Vector;
import java.util.Locale;

import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;


/**
  * SafeExportTools contains static methods used by SafeExport classes
  *
  * @author R. Tuquet Laburre - June 2003
  */
public class SafeExportTools {
	public static String LIST_DELIM=";";
	public static String BOUND_DELIM="-";

    public static String ERR_GET_VALUE_VARIABLE_NOT_FOUND ="not found!";
    public static String ERR_GET_VALUE_TYPE_NOT_FOUND ="Type!";
    public static String ERR_GET_VALUE_TYPE_EXCEPTION = "Exception/Type!";
    public static String ERR_GET_VALUE_EXCEPTION = "Exception!";

	// Return the method called methodName of the object or false if method not found
/*	public static Method getMethod(Object object, String methodName)
	{
		Collection acc = Tools.getPublicAccessors (object.getClass ());
		for (Iterator it = acc.iterator (); it.hasNext () ;) {
			Method method = (Method) it.next ();
			if (method.getName().equals(methodName)) {
				return method;
			}
		}
		return null;
	}
*/
	public static Method getMethod(Object object, String methodName) {
		Method[] methods = object.getClass().getMethods();
		for (int i=0; i < methods.length ; i++) {
			if (methods[i].getName().equals(methodName)) {
				return methods[i];
			}
		}
		return null;
	}

	// Put in the 'value' argument, the value of the field called fieldName of the object
	// return true if the value has been correctly readed and converted in a primitive type
	public static boolean readPrimitiveField(Object object, String fieldName,StringBuffer value) {
		if (value!=null && value.length()>0) {
			value.delete(0,value.length()-1);
		}
		String value2="";
		boolean ret=false;
		try {
			Class cls = object.getClass();
			Field field = cls.getField(fieldName);
			if (field.getType().equals(Double.TYPE)) {
				double val = field.getDouble(object);
				value2 = "" + val;
				ret = true;
			} else if (field.getType().equals(Float.TYPE)) {
				float val = field.getFloat(object);
				value2 = "" + val;
				ret = true;
			} else if (field.getType().equals(Long.TYPE)) {
				long val = field.getLong(object);
				value2 = "" + val;
				ret = true;
			} else if (field.getType().equals(Integer.TYPE)) {
				int val = field.getInt(object);
				value2 = "" + val;
				ret = true;
			} else if (field.getType().equals(Short.TYPE)) {
				short val = field.getShort(object);
				value2 = "" + val;
				ret = true;
			} else if (field.getType().equals(Character.TYPE)) {
				char val = field.getChar(object);
				value2 = "" + val;
				ret = true;
			} else if (field.getType().equals(Byte.TYPE)) {
				byte val = field.getByte(object);
				value2 = "" + val;
				ret = true;
			} else if (field.getType().equals(Boolean.TYPE)) {
				boolean val = field.getBoolean(object);
				if (val) {
					value2 = Translator.swap("Shared.yes");
				} else {
					value2 = Translator.swap("Shared.no");
				}
				ret = true;
			} else {
				value2 = (String) field.get(object).toString();
				ret = true;
			}
		}
		catch (Exception e) {
			System.out.println("Error in SafeExportTools.readPrimitiveField : " + e);
		}
		value.append(value2);
		return ret;
	}

	// Put values of a variable in the array argument 'values' using the method getVariable
	// > return true if success , false if not success
	public static boolean readArrayOfFloatVariables(Object object, String variable, String [] values)
	{
		boolean ret=false;
		variable = variable.toUpperCase().charAt(0) + variable.substring(1,variable.length());
		variable = "get"+variable;
		try {
			Method m=getMethod(object,variable);
			if (m.getReturnType().isArray()) {
				Object o =  m.invoke (object);	// fc - 2.12.2004 - varargs
				float[] array = (float[]) o;
				for (int i=0;i< array.length && i < values.length ; i++) {
					values[i]="" + array[i];
				}
				if (array.length==values.length) {
					return true;
				}
			}
		} catch(Exception e) {
			System.out.println("Error in SafeExportTools.readArrayOfFloatVariables : " + e);
		}
		if (!ret) {
			values = new String[values.length];
			for (int i=0 ; i<values.length;i++) {
				values[i]="-";
			}
		}
		return ret;
	}

	// Put values of a variable in the array argument 'values' using the method getVariable
	// > return true if success , false if not success
	public static boolean readArrayOfDoubleVariables(Object object, String variable, String [] values)
	{
		boolean ret=false;
		variable = variable.toUpperCase().charAt(0) + variable.substring(1,variable.length());
		variable = "get"+variable;
		try {
			Method m=getMethod(object,variable);
			if (m.getReturnType().isArray()) {
				Object o =  m.invoke (object);	// fc - 2.12.2004 - varargs
				double[] array = (double[]) o;
				for (int i=0;i< array.length && i < values.length ; i++) {
					values[i]="" + array[i];
				}
				if (array.length==values.length) {
					return true;
				}
			}
		} catch(Exception e) {
			System.out.println("Error in SafeExportTools.readArrayOfDoubleVariables : " + e);
		}
		if (!ret) {
			values = new String[values.length];
			for (int i=0 ; i<values.length;i++) {
				values[i]="-";
			}
		}
		return ret;
	}

	
	// Put values of a variable in the array argument 'values' using the method getVariable
	// > return true if success , false if not success
	public static boolean readArrayOfIntVariables(Object object, String variable, String [] values)
	{
		boolean ret=false;
		variable = variable.toUpperCase().charAt(0) + variable.substring(1,variable.length());
		variable = "get"+variable;
		try {
			Method m=getMethod(object,variable);
			if (m.getReturnType().isArray()) {
				Object o =  m.invoke (object);	// fc - 2.12.2004 - varargs
				int[] array = (int[]) o;
				for (int i=0;i< array.length && i < values.length ; i++) {
					values[i]="" + array[i];
				}
				if (array.length==values.length) {
					return true;
				}
			}
		} catch(Exception e) {
			System.out.println("Error in SafeExportTools.readArrayOfIntVariables : " + e);
		}
		if (!ret) {
			values = new String[values.length];
			for (int i=0 ; i<values.length;i++) {
				values[i]="-";
			}
		}
		return ret;
	}
	// Put a variable value in the argument 'value' using the method getVariable
	// return true if the value has been correctly readed and converted in a primitive type
	public static boolean readPrimitiveVariable(Object object, String variable, StringBuffer value) {
		
		if ((object==null) || (variable==null) || (value==null)) {
				return false;
		}
		if (value!=null && value.length()>0) {
			value.delete(0,value.length()-1);
		}
		boolean ret=false;
		try {
			Method m=getMethod(object,variable);
			if (m==null) {
				variable = variable.toUpperCase().charAt(0) + variable.substring(1,variable.length());
				variable = "get"+variable;
			}
			m=getMethod(object,variable);

			if (m!=null) {

				if (Tools.returnsType (m,Double.TYPE)) {
					Double val=null;
					val = (Double) m.invoke (object);	// fc - 2.12.2004 - varargs
					String st = String.format(Locale.US, "%.5e",val); // added sr 22-6-09 (printed export in scientific mode)
					value.append(st);				  	   // added sr 22-6-09	
					//value.append(val);				   // commented added sr 22-6-09	
					ret = true;
				} else if (Tools.returnsType (m,Float.TYPE)) {
					Float val=null;
					val = (Float) m.invoke (object);	// fc - 2.12.2004 - varargs
					String st = String.format(Locale.US, "%.5e", val);
					value.append(st);
					ret = true;
				} else if (Tools.returnsType (m,Long.TYPE)) {
					Long val=null;
					val = (Long) m.invoke (object);	// fc - 2.12.2004 - varargs
					value.append(val);
					ret = true;
				} else if (Tools.returnsType (m,Integer.TYPE)) {
					Integer val=null;
					val = (Integer) m.invoke (object);	// fc - 2.12.2004 - varargs
					value.append(val);
					ret = true;
				} else if (Tools.returnsType (m,Short.TYPE)) {
					Short val=null;
					val = (Short) m.invoke (object);	// fc - 2.12.2004 - varargs
					value.append(val);
					ret = true;
				} else if (Tools.returnsType (m,Byte.TYPE)) {
					Byte val=null;
					val = (Byte) m.invoke (object);	// fc - 2.12.2004 - varargs
					value.append(val);
					ret = true;
				} else if (Tools.returnsType (m,Character.TYPE)) {
					Character val=null;
					val = (Character) m.invoke (object);	// fc - 2.12.2004 - varargs
					value.append(val);
					ret = true;
				} else if (Tools.returnsType (m,Boolean.TYPE)) {
					Boolean val=null;
					val = (Boolean) m.invoke (object);	// fc - 2.12.2004 - varargs
					if (val.booleanValue()) {
						value.append(Translator.swap("Shared.yes"));
					} else
						value.append(Translator.swap("Shared.no"));
					ret = true;
				} else if (Tools.returnsType (m,new String("").getClass())) {
					String val=null;
					val = (String) m.invoke (object);	// fc - 2.12.2004 - varargs
					value.append(val);
					ret = true;
				}
			}
		} catch (Exception e) {
			System.out.println("Error in SafeExportTools.readPrimitiveVariable : " + e);
		}
		if (!ret) {
			value.append("Error!");
		}
		return ret;
	}

	public static boolean isNumberInCollection(Number nb,Collection col) {
		for (Iterator it = col.iterator() ; it.hasNext();) {
			Number nb2 = (Number) it.next();
			if (nb.equals(nb2)) { return true; }
		}
		return false;
	}


	public static boolean isBoundInBoundsString(String num1,String num2,String str) {
		boolean ret=false;
		String item="";
		int index=0;
		double nbr1 = Double.parseDouble(num1);
		double nbr2 = Double.parseDouble(num2);
		double nb;
		double nb1;
		double nb2;

		Vector v = (Vector) stringToBoundsCollection(str);
		if (v!=null) {
			try {
				for (Iterator it=v.iterator();it.hasNext() && (!ret);) {
					nb1=-Double.MIN_VALUE;
					nb2=Double.MAX_VALUE;
					item=(String) it.next();
					index=item.indexOf(BOUND_DELIM);
					if (index==-1) {
						// item = "val" 5.25
					 	nb = Double.parseDouble(item);
						if (nbr1<=nb && nbr2>=nb) {
							ret=true;
						}
					} else if (index==item.length()-1) {
						// item = "val-"
					 	nb = Double.parseDouble(item.substring(0,item.length()-1));
						if (nbr1>=nb || nbr2>=nb) {
							ret=true;
						}
					} else if (index==0) {
						// item = "-val"
						nb = Double.parseDouble(item.substring(1,item.length()));
						if (nbr2<=nb || nbr1<=nb) {
							ret=true;
						}
					} else {
						// item = "val1-val2"
					 	nb1 = Double.parseDouble(item.substring(0,index));
					 	nb2 = Double.parseDouble(item.substring(index+1,item.length()));
						//if ((nbr1>=nb1 && nbr2<=nb2) || nbr1==nb2 || nbr2==nb1) {
						if (nbr1>=nb1 && nbr2<=nb2)  {
							ret=true;
						}
					}
				}
			}
	 		catch (Exception e) {
	 			System.out.println("Error in SafeExportTools.isBoundInBoundsString : " + e);
	 		}
	 	}
		return ret;
	}

	public static Collection stringToBoundsCollection(String str)
	{
		Vector v1 = new Vector();
		Vector v = new Vector();
		StringTokenizer st = new StringTokenizer(str,LIST_DELIM);
	    while (st.hasMoreTokens()) {
	    	String item =st.nextToken().trim().toString();
	    	v1.add(item);
	 	}
		for (Iterator it=v1.iterator();it.hasNext();) {
			String item= (String) it.next();
			String val1=null;
			String val2=null;
			double nb1=-1.0;
			double nb2=-1.0;
			int index = item.indexOf(BOUND_DELIM);
			boolean ok=false;
			if (index==-1) {
				// item = "val"
				nb1 = Double.parseDouble(item);
				ok=true;
			} else if (index==item.length()-1) {
				// item = "val1-"
				val1= item.substring(0,item.length()-1);
				nb1 = Double.parseDouble(val1);
				ok=true;
			} else if (index==0) {
				// item = "-val2"
				val2= item.substring(1,item.length());
			 	nb2 = Double.parseDouble(val2);
			 	ok=true;
			} else {
				// item = "val1-val2"
				val1=item.substring(0,index);
				val2=item.substring(index+1,item.length());
				nb1 = Double.parseDouble(val1);
				nb2 = Double.parseDouble(val2);
				if (nb1<nb2) {
					double nb=nb1;
					nb1=nb2;
					nb2=nb;
				}
				if (nb1>= 0.0 && nb2>=0.0) ok=true;
			}
			if (ok==false) return null;
			if (val1==null && val2==null)
				v.add(item);
			else if (val1!=null && val2!=null)
				v.add(val1 + BOUND_DELIM + val2);
			else if (val1!=null)
				v.add(val1+BOUND_DELIM);
			else if (val2!=null)
				v.add(BOUND_DELIM+val2);
		}
/*
 		System.out.print("Contenu V1 (source)  : ");
		for (Iterator it=v1.iterator();it.hasNext();)
			System.out.print((String) it.next()+" ");
		System.out.println("");
		System.out.print("Contenu v (dest)  : ");
		for (Iterator it=v.iterator();it.hasNext();)
			System.out.print((String) it.next()+" ");
		System.out.println("");
*/
		return (Collection) v;
	}

	public static String collectionToString(Collection col)
	{
		Collections.sort((List) col);
		StringBuffer sb=new StringBuffer();
		int cpt=0;
		for (Iterator it=col.iterator();it.hasNext();cpt++) {
			sb.append(it.next());
			if (cpt!=col.size()-1) {
				sb.append(LIST_DELIM);
			}
		}
		return sb.toString();
	}

	public static int[] collectionToArrayOfIntegers(Collection col)
	{
		
	
		int[] array = new int[col.size()];
		int cpt=0;
		for (Iterator it=col.iterator() ; it.hasNext();cpt++) {
			array[cpt] = ((Double)it.next()).intValue();
		}
		return array;
	}

	public static String[] collectionToArrayOfString(Collection col)
	{

		String[] array = new String[col.size()];
		int cpt=0;
		for (Iterator it=col.iterator() ; it.hasNext();cpt++) {
			Object v = it.next();
			array[cpt] = (String)v.toString();
		}
		return array;
	}
	

	public static double[] collectionToArrayofFloats(Collection col)
	{
		double[] array = new double[col.size()];
		int cpt=0;
		for (Iterator it=col.iterator() ; it.hasNext();cpt++) {
			array[cpt] = ((Double)it.next()).doubleValue();
		}
		return array;
	}

	public static Collection stringToCollectionType(String str, Class type) {
		Vector col = new Vector();
		StringTokenizer st = new StringTokenizer(str,LIST_DELIM);
	    while (st.hasMoreTokens()) {
	    	String idString =st.nextToken().trim().toString();
	     	Object o=null;
	     	if (type.equals(Double.TYPE)) {
	     	 	o = Double.valueOf(idString);
	     	} else if (type.equals(Float.TYPE)) {
	     	 	o = Float.valueOf(idString);
	     	} else if (type.equals(Long.TYPE)) {
	     		o = Long.valueOf(idString);
	   		} else if (type.equals(Integer.TYPE)) {
	     		o = Integer.valueOf(idString);
	   		} else if (type.equals(Short.TYPE)) {
	     		o = Short.valueOf(idString);
	   		} else if (type.equals(Byte.TYPE)) {
	     		o = Byte.valueOf(idString);
	   		} else if (type.equals(Boolean.TYPE)) {
	     		o = Boolean.valueOf(idString);
	   		}
			if (o!=null) {
				col.add(o);
			}
	   	}
         if (col!=null && col.size()>0) {
        	Collections.sort(col);
        }
        return col;
	}
}
