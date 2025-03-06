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


/**
 * An improved random number generator based on Algorithm B
 * in Knuth Vol 2 p32.
 * Gives a set of random integers that does not exhibit
 * as much correlation as the method used by the Java random number generator.
 * 
 * @version 1.01 15 Feb 1996
 * @author Cay Horstmann
 */

public class RandomIntGenerator
{	/**
	  Constructs an object that generates random integers in a given range
	  @param l the lowest integer in the range
	  @param h the highest integer in the range
	*/

   public RandomIntGenerator(int l, int h)
   {  low = l;
      high = h;
   }

	/**
	  Generates a random integer in a range of integers
	  @return a random integer
	*/

   public int draw()
   {  int r = low
         + (int)((high - low + 1) * nextRandom());
      if (r > high) r = high;
      return r;
   }

	/**
	  test stub for the class
	*/

	/*   public static void main(String[] args)
	{  RandomIntGenerator r1
		 = new RandomIntGenerator(1, 10);
	  RandomIntGenerator r2
		 = new RandomIntGenerator(0, 1);
	  int i;
	  for (i = 1; i <= 100; i++)
		 //System.out.println(r1.draw() + " " + r2.draw());
	}*/

   private static double nextRandom()
   {  int pos =
         (int)(java.lang.Math.random() * BUFFER_SIZE);
      if (pos == BUFFER_SIZE) pos = BUFFER_SIZE - 1;
      double r = buffer[pos];
      buffer[pos] = java.lang.Math.random();
      return r;
   }

   private static final int BUFFER_SIZE = 101;
   private static double[] buffer
      = new double[BUFFER_SIZE];
   static
   {  int i;
      for (i = 0; i < BUFFER_SIZE; i++)
         buffer[i] = java.lang.Math.random();
   }

   private int low;
   private int high;
}
