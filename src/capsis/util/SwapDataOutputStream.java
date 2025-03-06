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

import java.io.DataOutput;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UTFDataFormatException;

/**
* This class is acopy of DataOutputStream with a difference concerning the short / int / long 
* codage. DataOutputStream writes them high byte first. SwapDataOutputStream writes them 
* low byte first, in reverse order. This was made to generate a stream that can be readable 
* through the network by a C program under Linux. For connexion with a C program under 
* IRIX (SGI), use DataOutputStream instead (high byte first codage).
* This code is strongly based upon DataOutputStream.

* @author F. de Coligny - november 2002
*/
public class SwapDataOutputStream extends FilterOutputStream implements DataOutput {
	/**
	* The number of bytes written to the data output stream so far. 
	* If this counter overflows, it will be wrapped to Integer.MAX_VALUE.
	*/
	protected int written;

	/**
	* Creates a new data output stream to write data to the specified 
	* underlying output stream. The counter <code>written</code> is 
	* set to zero.
	*
	* @param   out   the underlying output stream, to be saved for later 
	*                use.
	* @see     java.io.FilterOutputStream#out
	*/
	public SwapDataOutputStream(OutputStream out) {
		super(out);
	}

	/**
	* Increases the written counter by the specified value
	* until it reaches Integer.MAX_VALUE.
	*/
	private void incCount(int value) {
		int temp = written + value;
		if (temp < 0) {
			temp = Integer.MAX_VALUE;
		}
		written = temp;
	}

	/**
	* Writes the specified byte (the low eight bits of the argument 
	* <code>b</code>) to the underlying output stream. If no exception 
	* is thrown, the counter <code>written</code> is incremented by 
	* <code>1</code>.
	* <p>
	* Implements the <code>write</code> method of <code>OutputStream</code>.
	*
	* @param      b   the <code>byte</code> to be written.
	* @exception  IOException  if an I/O error occurs.
	* @see        java.io.FilterOutputStream#out
	*/
	public synchronized void write(int b) throws IOException {
		out.write(b);
		incCount(1);
	}

	/**
	* Writes <code>len</code> bytes from the specified byte array 
	* starting at offset <code>off</code> to the underlying output stream. 
	* If no exception is thrown, the counter <code>written</code> is 
	* incremented by <code>len</code>.
	*
	* @param      b     the data.
	* @param      off   the start offset in the data.
	* @param      len   the number of bytes to write.
	* @exception  IOException  if an I/O error occurs.
	* @see        java.io.FilterOutputStream#out
	*/
	public synchronized void write(byte b[], int off, int len) throws IOException {
		out.write(b, off, len);
		incCount(len);
	}

	/**
	* Flushes this data output stream. This forces any buffered output 
	* bytes to be written out to the stream. 
	* <p>
	* The <code>flush</code> method of <code>DataOuputStream</code> 
	* calls the <code>flush</code> method of its underlying output stream.
	*
	* @exception  IOException  if an I/O error occurs.
	* @see        java.io.FilterOutputStream#out
	* @see        java.io.OutputStream#flush()
	*/
	public void flush() throws IOException {
		out.flush();
	}

	/**
	* Writes a <code>boolean</code> to the underlying output stream as 
	* a 1-byte value. The value <code>true</code> is written out as the 
	* value <code>(byte)1</code>; the value <code>false</code> is 
	* written out as the value <code>(byte)0</code>. If no exception is 
	* thrown, the counter <code>written</code> is incremented by 
	* <code>1</code>.
	*
	* @param      v   a <code>boolean</code> value to be written.
	* @exception  IOException  if an I/O error occurs.
	* @see        java.io.FilterOutputStream#out
	*/
	public final void writeBoolean(boolean v) throws IOException {
		out.write(v ? 1 : 0);
		incCount(1);
	}
	
	/**
	* Writes out a <code>byte</code> to the underlying output stream as 
	* a 1-byte value. If no exception is thrown, the counter 
	* <code>written</code> is incremented by <code>1</code>.
	*
	* @param      v   a <code>byte</code> value to be written.
	* @exception  IOException  if an I/O error occurs.
	* @see        java.io.FilterOutputStream#out
	*/
	public final void writeByte(int v) throws IOException {
		out.write(v);
		incCount(1);
	}

	/**
	* Writes a <code>short</code> to the underlying output stream as two
	* bytes, low byte first. If no exception is thrown, the counter 
	* <code>written</code> is incremented by <code>2</code>.
	*
	* @param      v   a <code>short</code> to be written.
	* @exception  IOException  if an I/O error occurs.
	* @see        java.io.FilterOutputStream#out
	*/
	public final void writeShort(int v) throws IOException {
		OutputStream out = this.out;
		out.write((v >>> 0) & 0xFF);
		out.write((v >>> 8) & 0xFF);
		incCount(2);
	}

	/**
	* Writes a <code>char</code> to the underlying output stream as a 
	* 2-byte value, high byte first. If no exception is thrown, the 
	* counter <code>written</code> is incremented by <code>2</code>.
	*
	* @param      v   a <code>char</code> value to be written.
	* @exception  IOException  if an I/O error occurs.
	* @see        java.io.FilterOutputStream#out
	*/
	public final void writeChar(int v) throws IOException {
		OutputStream out = this.out;
		out.write((v >>> 8) & 0xFF);
		out.write((v >>> 0) & 0xFF);
		incCount(2);
	}

	/**
	* Writes an <code>int</code> to the underlying output stream as four
	* bytes, low byte first. If no exception is thrown, the counter 
	* <code>written</code> is incremented by <code>4</code>.
	*
	* @param      v   an <code>int</code> to be written.
	* @exception  IOException  if an I/O error occurs.
	* @see        java.io.FilterOutputStream#out
	*/
	public final void writeInt(int v) throws IOException {
		OutputStream out = this.out;
		out.write((v >>>  0) & 0xFF);
		out.write((v >>>  8) & 0xFF);
		out.write((v >>> 16) & 0xFF);
		out.write((v >>> 24) & 0xFF);
		incCount(4);
	}

	/**
	* Writes a <code>long</code> to the underlying output stream as eight
	* bytes, low byte first. In no exception is thrown, the counter 
	* <code>written</code> is incremented by <code>8</code>.
	*
	* @param      v   a <code>long</code> to be written.
	* @exception  IOException  if an I/O error occurs.
	* @see        java.io.FilterOutputStream#out
	*/
	public final void writeLong(long v) throws IOException {
		OutputStream out = this.out;
		
		out.write((int)(v >>>  0) & 0xFF);
		out.write((int)(v >>>  8) & 0xFF);
		out.write((int)(v >>> 16) & 0xFF);
		out.write((int)(v >>> 24) & 0xFF);
		out.write((int)(v >>> 32) & 0xFF);
		out.write((int)(v >>> 40) & 0xFF);
		out.write((int)(v >>> 48) & 0xFF);
		out.write((int)(v >>> 56) & 0xFF);
		incCount(8);
	}

	/**
	* Converts the float argument to an <code>int</code> using the 
	* <code>floatToIntBits</code> method in class <code>Float</code>, 
	* and then writes that <code>int</code> value to the underlying 
	* output stream as a 4-byte quantity, low byte first. If no 
	* exception is thrown, the counter <code>written</code> is 
	* incremented by <code>4</code>.
	*
	* @param      v   a <code>float</code> value to be written.
	* @exception  IOException  if an I/O error occurs.
	* @see        java.io.FilterOutputStream#out
	* @see        java.lang.Float#floatToIntBits(float)
	*/
	public final void writeFloat(float v) throws IOException {
		writeInt(Float.floatToIntBits(v));
	}

	/**
	* Converts the double argument to a <code>long</code> using the 
	* <code>doubleToLongBits</code> method in class <code>Double</code>, 
	* and then writes that <code>long</code> value to the underlying 
	* output stream as an 8-byte quantity, low byte first. If no 
	* exception is thrown, the counter <code>written</code> is 
	* incremented by <code>8</code>.
	*
	* @param      v   a <code>double</code> value to be written.
	* @exception  IOException  if an I/O error occurs.
	* @see        java.io.FilterOutputStream#out
	* @see        java.lang.Double#doubleToLongBits(double)
	*/
	public final void writeDouble(double v) throws IOException {
		writeLong(Double.doubleToLongBits(v));
	}

	/**
	* Writes out the string to the underlying output stream as a 
	* sequence of bytes. Each character in the string is written out, in 
	* sequence, by discarding its high eight bits. If no exception is 
	* thrown, the counter <code>written</code> is incremented by the 
	* length of <code>s</code>.
	*
	* @param      s   a string of bytes to be written.
	* @exception  IOException  if an I/O error occurs.
	* @see        java.io.FilterOutputStream#out
	*/
	public final void writeBytes(String s) throws IOException {
		OutputStream out = this.out;
		int len = s.length();
		for (int i = 0 ; i < len ; i++) {
			out.write((byte)s.charAt(i));
		}
		incCount(len);
	}

	/**
	* Writes a string to the underlying output stream as a sequence of 
	* characters. Each character is written to the data output stream as 
	* if by the <code>writeChar</code> method. If no exception is 
	* thrown, the counter <code>written</code> is incremented by twice 
	* the length of <code>s</code>.
	*
	* @param      s   a <code>String</code> value to be written.
	* @exception  IOException  if an I/O error occurs.
	* @see        java.io.DataOutputStream#writeChar(int)
	* @see        java.io.FilterOutputStream#out
	*/
	public final void writeChars(String s) throws IOException {
		OutputStream out = this.out;
		int len = s.length();
		for (int i = 0 ; i < len ; i++) {
		int v = s.charAt(i);
			out.write((v >>> 8) & 0xFF);
			out.write((v >>> 0) & 0xFF);
		}
		incCount(len * 2);
	}

	/**
	* Writes a string to the underlying output stream using UTF-8 
	* encoding in a machine-independent manner. 
	* <p>
	* First, two bytes are written to the output stream as if by the 
	* <code>writeShort</code> method giving the number of bytes to 
	* follow. This value is the number of bytes actually written out, 
	* not the length of the string. Following the length, each character 
	* of the string is output, in sequence, using the UTF-8 encoding 
	* for the character. If no exception is thrown, the counter 
	* <code>written</code> is incremented by the total number of 
	* bytes written to the output stream. This will be at least two 
	* plus the length of <code>str</code>, and at most two plus 
	* thrice the length of <code>str</code>.
	*
	* @param      str   a string to be written.
	* @exception  IOException  if an I/O error occurs.
	*/
	public final void writeUTF(String str) throws IOException {
		writeUTF(str, this);
	}

	/**
	* Writes a string to the specified DataOutput using UTF-8 encoding in a
	* machine-independent manner. 
	* <p>
	* First, two bytes are written to out as if by the <code>writeShort</code>
	* method giving the number of bytes to follow. This value is the number of
	* bytes actually written out, not the length of the string. Following the
	* length, each character of the string is output, in sequence, using the
	* UTF-8 encoding for the character. If no exception is thrown, the
	* counter <code>written</code> is incremented by the total number of 
	* bytes written to the output stream. This will be at least two 
	* plus the length of <code>str</code>, and at most two plus 
	* thrice the length of <code>str</code>.
	*
	* @param      str   a string to be written.
	* @param      out   destination to write to
	* @return     The number of bytes written out.
	* @exception  IOException  if an I/O error occurs.
	*/
	static int writeUTF(String str, DataOutput out) throws IOException {
		int strlen = str.length();
		int utflen = 0;
		char[] charr = new char[strlen];
		int c, count = 0;
		
		str.getChars(0, strlen, charr, 0);
		
		for (int i = 0; i < strlen; i++) {
			c = charr[i];
			if ((c >= 0x0001) && (c <= 0x007F)) {
			utflen++;
			} else if (c > 0x07FF) {
			utflen += 3;
			} else {
			utflen += 2;
			}
		}
		
		if (utflen > 65535)
			throw new UTFDataFormatException();
		
		byte[] bytearr = new byte[utflen+2];
		bytearr[count++] = (byte) ((utflen >>> 8) & 0xFF);
		bytearr[count++] = (byte) ((utflen >>> 0) & 0xFF);
		for (int i = 0; i < strlen; i++) {
			c = charr[i];
			if ((c >= 0x0001) && (c <= 0x007F)) {
				bytearr[count++] = (byte) c;
			} else if (c > 0x07FF) {
				bytearr[count++] = (byte) (0xE0 | ((c >> 12) & 0x0F));
				bytearr[count++] = (byte) (0x80 | ((c >>  6) & 0x3F));
				bytearr[count++] = (byte) (0x80 | ((c >>  0) & 0x3F));
			} else {
				bytearr[count++] = (byte) (0xC0 | ((c >>  6) & 0x1F));
				bytearr[count++] = (byte) (0x80 | ((c >>  0) & 0x3F));
			}
		}
		out.write(bytearr);
		
		return utflen + 2;
	}

	/**
	* Returns the current value of the counter <code>written</code>, 
	* the number of bytes written to this data output stream so far.
	* If the counter overflows, it will be wrapped to Integer.MAX_VALUE.
	*
	* @return  the value of the <code>written</code> field.
	* @see     java.io.DataOutputStream#written
	*/
	public final int size() {
		return written;
	}
}
