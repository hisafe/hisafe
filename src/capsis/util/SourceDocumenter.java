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
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**	Extract documentation information from the given source
*	@author F. de Coligny - september 2005
*/
public class SourceDocumenter {

	private File file;
	
	private String className;
	private String classAuthor;
	private String classComment;
	
	private Collection<String> methodNames;
	private Collection<String> methodComments;
	
	
	
	/**	Constructor
	*/
	public SourceDocumenter (File file) {
		this.file = file;
		methodNames = new ArrayList<String> ();
		methodComments = new ArrayList<String> ();
	}

	/**	Search the source to extract comments
	*/
	public SourceDocumenter run () throws Exception {
		try {
			BufferedReader in = new BufferedReader (new FileReader (file));
			
			// read all file into a single text
			String line = in.readLine ();
			StringBuffer b = new StringBuffer ();
			while (line != null) {
				b.append (line);
				b.append ("\n");
				line = in.readLine ();
			}
			String text = b.toString ();
			
			// try to discard "*" when first "non blank" line character (in comments)
			text = discardCommentStars (text) ;
			
			// look for class name
			int after = extractClassName (text) ;
			
//~ System.out.println ("className="+className);
			
			// look for class comment (also deal with @author tag)
			after = extractClassComment (text) ;
			
//~ System.out.println ("classComment="+classComment);
//~ System.out.println ("classAuthor="+classAuthor);
			
			if (after != -1) {text = text.substring (after);}	// fc - 19.9.2005
			
			int result = extractMethods (text);
			
		} catch (Exception e) {
			//~ System.out.println ("exception in file"+file);
			//~ e.printStackTrace (System.out);
			throw e;
		}
		
		return this;
	}

	// discard stars when at beginning of line (comment stars)
	private String discardCommentStars (String text) {
		// regex = "begin-of-line" "zero-or-more-tabs-or-spaces" "star"
		// all occurences replaced by ""
		text = Pattern.compile ("^\\s*\\*[^/]", Pattern.MULTILINE).matcher (text).replaceAll ("");
		return text;
	}


	// look in text for class (or interface) name
	// return index of first character after class name or -1 if not found
	//
	private int extractClassName (String text) {
		String classNameRegex = "^\\s*?(public|abstract).*?(interface|class).*?(\\{|implements|extends)";
		//~ String classNameRegex = "^.*?public.*?(interface|class).*?(\\{|implements|extends)";
		Pattern classNamePattern = Pattern.compile (classNameRegex, Pattern.MULTILINE | Pattern.DOTALL);
		
		Matcher m0 = classNamePattern.matcher (text);
		if (m0.find ()) {
			
			String c = text.substring (m0.start (), m0.end ());
			
			int i0 = c.indexOf ("interface");
			if (i0 != -1) {c = c.substring (i0+9);}
			
			int i1 = c.indexOf ("class");
			if (i1 != -1) {c = c.substring (i1+5);}
			
			c = c.trim ();
			int i2 = c.indexOf (" ");
			if (i2 != -1) {c = c.substring (0, i2);}
			
			className = c;
			return m0.end ();	// index of next character
		} else {
			return -1;	// not found
		}
	}


	// look in text for class (or interface) comment
	// also deals with @author tag
	// return index of first character after comment or -1 if not found
	//
	private int extractClassComment (String text) {
		String classCommentRegex = "/\\*\\*.*?\\*/.*?public.*?(class|interface)";
		Pattern classCommentPattern = Pattern.compile (classCommentRegex, Pattern.DOTALL);
		
		Matcher m1 = classCommentPattern.matcher (text);
		if (m1.find ()) {
			String c = text.substring (m1.start (), m1.end ());
			
			// look for class author
			int result = extractClassAuthor (c) ;
			
			if (classAuthor != null) {
				c = c.substring (0, c.indexOf ("@author"));
			}
			
			int i0 = c.indexOf ("/**");
			if (i0 != -1) {c = c.substring (i0+3);}
			
			int i1 = c.indexOf ("*/");
			if (i1 != -1) {c = c.substring (0, i1);}
			
			c = c.replace ('\n', ' ');
			c = c.replaceAll ("\\s+", " ");
			c = c.trim ();
			
			classComment = c;
			return m1.end ();	// index of next character
		} else {
			return -1;	// not found
		}
	}

	
	// look in text for class author
	//
	private int extractClassAuthor (String text) {
		// search @author tag
		String authorRegex = "@author.*$";
		Pattern authorPattern = Pattern.compile (authorRegex, Pattern.MULTILINE);
		Matcher m2 = authorPattern.matcher (text);
		if (m2.find ()) {
			String a = text.substring (m2.start (), m2.end ());
			
			a = a.substring (a.indexOf ("@author")+7);
			a = a.trim ();
			
			classAuthor = a;
			return m2.end ();	// index of next character
		} else {
			return -1;	// not found
		}
	}
	
	
	// look in text for public methods and their related comments if any
	//
	private int extractMethods (String text) {
		String methodNameRegex = "^\\s*?public.*?\\)";
		Pattern methodNamePattern = Pattern.compile (methodNameRegex, Pattern.DOTALL | Pattern.MULTILINE);
		
		Matcher m3 = methodNamePattern.matcher (text);
		int i = 0;
		while (m3.find ()) {
			String m = text.substring (m3.start (), m3.end ());
			
			m = m.trim ();
			methodNames.add (m);
//~ System.out.println ("methodName="+m);
			
			// method comment
			String c = "";	// default
			String candidateText = text.substring (i, m3.start ());
			String methodCommentRegex = "/\\*\\*.*?\\*/\\s*?\\z";
			Pattern methodCommentPattern = Pattern.compile (methodCommentRegex, Pattern.DOTALL);
			
			Matcher m4 = methodCommentPattern.matcher (candidateText);
			if (m4.find ()) {
				c = candidateText.substring (m4.start (), m4.end ());
			}
			
			int i0 = c.indexOf ("/**");
			if (i0 != -1) {c = c.substring (i0+3);}
			
			int i1 = c.indexOf ("*/");
			if (i1 != -1) {c = c.substring (0, i1);}
			
			c = c.replace ('\n', ' ');
			c = c.replaceAll ("\\s+", " ");
			c = c.trim ();
			
			methodComments.add (c);
//~ System.out.println ("methodComment="+c);
				
			i = m3.end ();
		}
		return 0;
	}
	
	
	public String getClassName () {return className;}
	public String getClassAuthor () {return classAuthor;}
	public String getClassComment () {return classComment;}
	public Collection<String> getMethodNames () {return methodNames;}
	public Collection<String> getMethodComments () {return methodComments;}
	

	public static void main (String[] args) {
		//~ System.out.println ("Note: SourceDocumenter should be ran from capsis4/bin/");
		//~ System.out.println ("args="+args.length);
		if (args.length != 1) {
			System.out.println ("Usage: SourceDocumenter fileName");
			return;
		}
		
		try {
			new SourceDocumenter (new File (args[0])).run ();
		} catch (Exception e) {
			System.out.println ("exception in SourceDocumenter");
		}
		
		
		
		//~ System.out.println ("");
		//~ String text = "j'ai    faim alors !";
		//~ System.out.println ("text: "+text);
		//~ String regex = ("\\s{2,}");
		//~ System.out.println ("replacement by x: "+text.replaceAll (regex, " "));
		
	}
}


