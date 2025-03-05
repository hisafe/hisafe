Capsis external dependencies


GIS

  * concaveHull.jar (7.9.2020)
    * for Simcop_Qual

  * Lambert-java (fc-5.6.2018)
    * for phenofit4, to convert Lambert93 meter coordinates into WGS84 in degrees
    * Lambert-java-2.1.0.jar
    * Lambert-java-2.1.0-sources.jar
    * LGPL v2.1 licence

  * Geotools
    * gt-*-2.7.5.jar
    * jsr-275-0.9.4.jar
    * hsqldb.jar
    * Tried to upgrade geotools for Ecoaf on 24sep2020, resulted in a terrible mess with missing dependencies (si.uom), went back to 2.7.5

SVN library (fc-10.1.2017)

  * SVNkit 1.8
    * Extract, only to get the current SVN revision of the working copy
    * https://svnkit.com/
    * License: https://svnkit.com/license.html
    * antlr-runtime-3.4.jar
    * sqljet-1.1.10.jar
    * sequence-library-1.0.3.jar
    * svnkit-1.8.14.jar
    * svnkit-cli-1.8.14.jar

Scientific libraries

  * commons-math-2.O.jar : 
  * commons_math3-3.6.1.jar
     * http://commons.apache.org/math/
     * License : Apache License
     * Use in Capsis lib library and ModisPinaster

  * flanagan.jar 
     * Michael Thomas Flanagan's Java Scientific Library  
     * http://www.ee.ucl.ac.uk/~mflanaga/java/ by Dr Michael Thomas Flanagan
     * License : Free for non commercial use
     * Used in JackPine and ModisPinaster
    
  * jep-2.4.1.jar GPL : 
      * A mathematical expression parser
      * http://sourceforge.net/projects/jep/
      * Used by afocelpp / afocelpa
      * License GPL
      * ** JEP > 2.4.1 are distributed under a commercial license**

  * jcobyla.jar : 
     * Constrained Optimization BY Linear Approximation in Java
     * https://github.com/cureos/jcobyla by Anders Gustafsson
     * License : MIT License 
     * Used by espace					
  
Serialisation

  * xstream.jar
     * XML serialization
     * BSD
     * http://xstream.codehaus.org

  * xpp3
     * Fast XML parser
     * http://www.extreme.indiana.edu/xgws/xsoap/xpp/mxp1/index.html

Plotter

  * jfreechart (jfreechart-1.0.6.jar, jfreechart-1.0.6-swt.jar, jcommon-1.0.10.jar)
      * http://www.jfree.org/jfreechart/
      * License : LGPL
      * Used by regix

3D

  * j3dcore.jar, vecmath.jar, j3dutils.jar, slimplot.jar
     * Java 3D : https://java3d.dev.java.net/
     * License : GPL V2 exception classpath
     * Used by stretch

  * jogl.jar  (+ gluegen-rt)
     * https://jogl.dev.java.net/
     * License : BSD

Java look and feel

  * kunststoff.jar 
  * toniclf.jar

Utilities

  * args4j (command line parameter parsing)
    * https://args4j.dev.java.net/
    * MIT license

  * opencsv 
    * http://opencsv.sourceforge.net
    * License: Apache 2.0

  * groovy (scripting)

  * jepp : connection with CPython
    	 
  * commons-csv-1.5.jar : 
    * https://commons.apache.org/proper/commons-csv/
	* Apache license
	
Developpement

  * ant
  * IZPack
  * JUnit






