/* 
 * Biomechanics library for Capsis4.
 * 
 * Copyright (C) 2001-2003  Philippe Ancelin.
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

package capsis.lib.biomechanics;

import capsis.defaulttype.Tree;

/**
 * MecaTools - Tools for tree biomechanics.
 *
 * @author Ph. Ancelin - october 2001
 */
public class MecaTools {
//checked for c4.1.1_08 - fc - 4.2.2003

	/**
	 * Return trunk diameter (cm) at specified height regarding to trunk taper of specified tree.
	 * Taper equation uses tree dbh and total height + species coefficients.
	 */
	static public double getDiameter (Tree t, double h) {
		
		// loi puissance simple
			/*double n = 0.65;
			double a = 1 / Math.pow((t.getHeight () - 1.3), n);
			double diameter;
			diameter = a * t.getDbh () * Math.pow((t.getHeight () - h), n);
			return diameter;*/
			
		// empatement epicea JM Leban et al
			
			// NE
			/*double t10 = 0.7721;
			double t11 = -0.004287;
			double t2 = 0.3554;
			double t30 = 4.3235;
			double t31 = -0.04543;
			double t32 = 0;
			double t4 = 38.95;*/
			
		// MP
		double t10 = 0.7945;
		double t11 = -0.005561;
		double t2 = 0.306;
		double t30 = 1.553;
		double t31 = 0.02188;
		double t32 = 8.29;
		double t4 = 64.04;
			
			// Publi 99
			/*double t10 = 0.7525859;
			double t11 = -0.0040089;
			double t2 = 0.1541139;
			double t30 = 1.6972525;
			double t31 = 0.0770358;
			double t32 = 22.013303;
			double t4 = 341.2329539;*/
			
		double t1 = t10 + t11 * (((t.getHeight () - 1.3) / (t.getDbh () / 100)) - 70);
		double t3 = t30 + t31 * (((t.getHeight () - 1.3) / (t.getDbh () / 100)) - 70) + t32 * (t.getDbh () / 100);
		double hr = h / t.getHeight ();
		double q = t1 + t2 * (hr - 1) + t3 * Math.exp (-(t4 * hr) / t3);
			
		double diameter;
		diameter = t.getDbh () * Math.pow (((t.getHeight () - h) / (t.getHeight () - 1.3)), q);
		return diameter;
			
		// empatement pin maritime AFOCEL
			
			/*double alpha, beta, gama, delta, hr, a, b, C, C130;
			alpha = -13.964;
			beta  = 0.5996;
			gama  = 0.6281;
			delta = 2.668;
			C130 = Math.PI * t.getDbh ();
			hr = h / t.getHeight ();
			b = 1.3 / t.getHeight ();
			a = Math.log (hr) / Math.log (b);
			C = (1 - Math.pow (hr, 3)) - a * (1 - Math.pow (b, 3));
			C *= (alpha + (beta * C130) + (gama * t.getHeight ()));
			C += ((C130 * a) + delta * (1 - a));
			return (C / Math.PI);*/
			
		// données eucalyptus Henri B.
			/*double diameter;
			diameter = -3.6649 * Math.pow (h, 5) + 126.88 * Math.pow (h, 4)
						- 1654.5 * Math.pow (h, 3) + 9894.7 * Math.pow (h, 2) - 33450 * h + 144186;
			diameter /= 10000.0;
			if (diameter < 0.0) {
				diameter *= -1.0;
			}
			return diameter;*/
			
		// données pin Damien
			/*double diameter;
			diameter = -74.77 * Math.pow (h, 5) - 4049.1 * Math.pow (h, 4)
						+ 23824.0 * Math.pow (h, 3) - 39535.0 * Math.pow (h, 2) - 3200.5 * h + 55235;
			diameter /= 10000.0;
			if (diameter < 0.0) {
				diameter *= -1.0;
			}
			return diameter;*/
		
	}

	/*static public void insertDoubleToFile(FILE* fch, char* StrF, double fl) {
		FILE *tempfch = tmpfile();
		char c;
		long CurPos = ftell(fch);
		while ((c=fgetc(fch))!=EOF)
			fputc(c,tempfch);
		fseek(fch, CurPos, SEEK_SET);
		fseek(tempfch, 0L, SEEK_SET);
		fprintf(fch,StrF,fl);
		while ((c=fgetc(tempfch))!=EOF)
			fputc(c,fch);
		fclose(tempfch);
	}
	
	static public void insertIntegerToFile(FILE* fch, char* StrF, int fl) {
		FILE *tempfch = tmpfile();
		char c;
		long CurPos = ftell(fch);
		while ((c=fgetc(fch))!=EOF)
			fputc(c,tempfch);
		fseek(fch, CurPos, SEEK_SET);
		fseek(tempfch, 0L, SEEK_SET);
		fprintf(fch,StrF,fl);
		while ((c=fgetc(tempfch))!=EOF)
			fputc(c,fch);
		fclose(tempfch);
	}*/

}




