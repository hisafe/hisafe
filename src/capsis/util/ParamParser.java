package capsis.util;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import jeeb.lib.util.ClassUtils;
import au.com.bytecode.opencsv.CSVReader;


/** Parse a csv file and return objects */
public class ParamParser<T> {

	protected 	CSVReader reader;

	public ParamParser(String filename) throws FileNotFoundException {
		reader = new CSVReader(new FileReader(filename), '\t');
	}


	public List<T> parse(Class<?> cl) {

		List<T> ret = new ArrayList<T>();
		
		try {
			String[] header = reader.readNext();


			String [] nextLine;
			while ((nextLine = reader.readNext()) != null) {

				T obj = (T) ClassUtils.instantiateClass(cl);

				int i = 0;
				for(String h : header) {

					try  {
						Field f = cl.getField(h);
						f.setAccessible(true);
						Class<?> dc = f.getType();

						String strval =  nextLine[i];
						Object value = parse(strval, dc);
						f.set(obj, value);
					}  catch(Exception e) {
						e.printStackTrace();
					}


					i++;
				}
				ret.add(obj);

			}
		} catch (Exception e) {
			e.printStackTrace();
		}

		return ret;
	}


	private Object parse(String strval, Class<?> dc) throws Exception {
		// try String construcotr
		Constructor<?> constructor = dc.getConstructor (new Class[] {String.class});
		Object ret = constructor.newInstance (new Object[]{strval});
		return ret;

	}

}
