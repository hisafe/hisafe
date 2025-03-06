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
//~ package org.fudaa.dodico.fortran;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
* Une classe etendant DataOutputStream et permettant de facilement
* ecrire des fichiers binaires dependants de la machine (sparc, i386, ...)
*
* @version      $Revision: 1.11 $ $Date: 2001/09/11 14:29:51 $ by $Author: deniger $
* @author       Axel von Arnim 
*/
public class NativeBinaryOutputStream
	 extends DataOutputStream
{
	public final static String X86="86";
	public final static String SPARC="sparc";

	int l1, l2, l3, l4, l5, l6, l7, l8;
	int i1, i2, i3, i4;
	int s1, s2;
	byte buf[];

	public NativeBinaryOutputStream(OutputStream _out, String _machine) throws IOException
	{
		super(_out);
		buf=null;
		setMachineType(_machine);
	}

	public void setMachineType(String _machine)
	{
		if( _machine.endsWith(X86) ) {
			l1=i1=s1=0; l2=i2=s2=1; l3=i3=2; l4=i4=3; l5=4; l6=5; l7=6; l8=7;
		} else {
			l1=7; l2=6; l3=5; l4=4; l5=i1=3; l6=i2=2; l7=i3=s1=1; l8=i4=s2=0;
		}
	}

	public void writeInt_8(byte int_8) throws IOException
	{
		buf=new byte[1];
		buf[0]=int_8;
		write(buf);
	}

	public void writeUInt_8(short uint_8) throws IOException
	{
		buf=new byte[1];
		buf[0]=uint_8>127?(byte)(uint_8-256):(byte)uint_8;
		write(buf);
	}

	public void writeInt_16(short int_16) throws IOException
	{
		int tmp;
		int sign_corr;
		if( int_16<0 ) sign_corr=1; else sign_corr=0;
		buf=new byte[2];
		tmp=int_16%0x100;
		buf[s1]=(byte)(tmp);
		tmp=int_16/0x100;
		buf[s2]=(byte)(tmp-sign_corr);
		write(buf);
	}

	public void writeUInt_16(int uint_16) throws IOException
	{
		int tmp;
		buf=new byte[2];
		tmp=uint_16%0x100;
		buf[s1]=(byte)(tmp>127?tmp-256:tmp);
		tmp=uint_16/0x100;
		buf[s2]=(byte)(tmp);//(byte)(tmp>127?tmp-256:tmp);
		write(buf);
	}

	public void writeInt_32(int int_32) throws IOException
	{
		int tmp;
		int sign_corr;
		if( int_32<0 ) sign_corr=1; else sign_corr=0;
		buf=new byte[4];
		tmp=int_32/0x1000000;
		buf[i4]=(byte)(tmp-sign_corr);//(byte)(tmp>127?tmp-25:tmp);
		tmp=int_32/0x10000;
		buf[i3]=(byte)(tmp-sign_corr);//(byte)(tmp>127?tmp-25:tmp);
		tmp=int_32/0x100;
		buf[i2]=(byte)(tmp-sign_corr);//(byte)(tmp>127?tmp-25:tmp);
		buf[i1]=(byte)(int_32%0x100);
		write(buf);
	}

	public void writeUInt_32(long uint_32) throws IOException
	{
		long tmp;
		buf=new byte[4];
		tmp=uint_32/0x1000000L;
		buf[i4]=(byte)(tmp);//(byte)(tmp>127?tmp-256:tmp);
		tmp=uint_32/0x10000L;
		buf[i3]=(byte)(tmp);//(byte)(tmp>127?tmp-256:tmp);
		tmp=uint_32/0x100L;
		buf[i2]=(byte)(tmp);//(byte)(tmp>127?tmp-256:tmp);
		tmp=uint_32%0x100L;
		buf[i1]=(byte)(tmp>127?tmp-256:tmp);
		write(buf);
	}

	public void writeInt_64(long int_64) throws IOException
	{
		long tmp;
		long sign_corr;
		if( int_64<0 ) sign_corr=1; else sign_corr=0;
		buf=new byte[8];
		tmp=int_64/0x100000000000000L;
		buf[l8]=(byte)(tmp-sign_corr);//(byte)(tmp>127?tmp-256:tmp);
		tmp=int_64/0x1000000000000L;
		buf[l7]=(byte)(tmp-sign_corr);//(byte)(tmp>127?tmp-256:tmp);
		tmp=int_64/0x10000000000L;
		buf[l6]=(byte)(tmp-sign_corr);//(byte)(tmp>127?tmp-256:tmp);
		tmp=int_64/0x100000000L;
		buf[l5]=(byte)(tmp-sign_corr);//(byte)(tmp>127?tmp-256:tmp);
		tmp=int_64/0x1000000L;
		buf[l4]=(byte)(tmp-sign_corr);//(byte)(tmp>127?tmp-256:tmp);
		tmp=int_64/0x10000L;
		buf[l3]=(byte)(tmp-sign_corr);//(byte)(tmp>127?tmp-256:tmp);
		tmp=int_64/0x100L;
		buf[l2]=(byte)(tmp-sign_corr);//(byte)(tmp>127?tmp-256:tmp);
		buf[l1]=(byte)(int_64%0x100L);
		write(buf);
	}

	public void writeFloat_32(float float_32) throws IOException
	{
		// B.M. Java autorise la valeur -0.0 qui perturbe le codage en bytes.
		// L'intérêt de cette valeur étant très relative, on la remplace par 0.0
		if (float_32==-0.0f) float_32=0.0f;
		writeInt_32(Float.floatToIntBits(float_32));
	}

	public void writeFloat_64(double float_64) throws IOException
	{
		// B.M. Java autorise la valeur -0.0 qui perturbe le codage en bytes.
		// L'intérêt de cette valeur étant très relative, on la remplace par 0.0
		if (float_64==-0.0) float_64=0.0;
		writeInt_64(Double.doubleToLongBits(float_64));
	}
}

