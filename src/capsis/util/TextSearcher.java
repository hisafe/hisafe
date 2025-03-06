/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package capsis.util;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**	Search some text for some given string.
*	@author F. de Coligny - september 2005
*/
public class TextSearcher {

	private String text;	// we scan this text
	
	
	/**	Constructor.
	*/
	public TextSearcher (String text) {
		this.text = discardComments (text);
	}

	/**	Constructor. Read the text in the given file.
	*/
	public TextSearcher (File file) throws Exception {
		BufferedReader in = new BufferedReader (new FileReader (file));
		
		// read all file into a single text
		String line = in.readLine ();
		StringBuffer b = new StringBuffer ();
		while (line != null) {
			b.append (line);
			b.append ("\n");
			line = in.readLine ();
		}
		in.close ();
		
		this.text = discardComments (b.toString ());
	}

	/**	Discard comments
	*/
	public String discardComments (String text) {
		// /*...*/
		text = Pattern.compile ("/\\*.*?\\*/", Pattern.MULTILINE | Pattern.DOTALL).matcher (text).replaceAll ("");
		text = Pattern.compile ("//.*?$", Pattern.MULTILINE | Pattern.DOTALL).matcher (text).replaceAll ("");
		return text;
	}

	/**	Search the text for the string.
	*/
	public boolean contains (String string) {
		// Note: the word boundaries slow the process but are needed : ACGProvider != AMGACGProvider
		String searchRegex = "\\b"+string+"\\b";	// between two word boundaries
		//~ Pattern searchPattern = Pattern.compile (searchRegex, Pattern.MULTILINE | Pattern.DOTALL);
		Pattern searchPattern = Pattern.compile (searchRegex);
		
		Matcher m0 = searchPattern.matcher (text);
		return m0.find ();
	}
	
	/**	Tests.
	*/
	public static void main (String[] args) {
		//~ TextSearcher s = new TextSearcher ("tralala tsoin tsoin");
		//~ System.out.println ("TextSearcher (tralala tsoin tsoin)");
		//~ System.out.println ("s.contains (tsoin)="+s.contains ("tsoin"));
		//~ System.out.println ("s.contains (ra)="+s.contains ("ra"));
		//~ System.out.println ("s.contains (rup)="+s.contains ("rup"));
		
		try {
			TextSearcher s = new TextSearcher (new File ("/home/coligny/java/capsis4/bin/capsis/util/TextSearcher.java"));
			
			BufferedWriter out = new BufferedWriter (new FileWriter (new File (
					"/home/coligny/java/capsis4/bin/capsis/util/TextSearcher.html")));
			out.write (s.text);
			out.close ();
			System.out.println ("See file without comments in capsis/util/TextSearcher.html");
			
			//~ System.out.println ("TextSearcher (new File (/home/coligny/java/capsis4/bin/capsis/util/TextSearcher.java)");
			//~ System.out.println ("s.contains (class)="+s.contains ("class"));
			//~ System.out.println ("s.contains (close)="+s.contains ("close"));
			//~ System.out.println ("s.contains (johnny)="+s.contains ("johnny"));

		} catch (Exception e) {
			System.out.println ("exception");
			e.printStackTrace (System.out);
		}
	}

}


