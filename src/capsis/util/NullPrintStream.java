package capsis.util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;

/**
 * A print stream writing nowhere, can be used to avoid the System.out.println
 * () messages, use System.setOut (new NullPrintStream ());
 * 
 * @author Vladimir,
 *         https://stackoverflow.com/questions/4799006/in-java-how-can-i-redirect-system-out-to-null-then-back-to-stdout-again
 */
public class NullPrintStream extends PrintStream {

	/**
	 * Constructor
	 */
	public NullPrintStream() {
		super(new NullByteArrayOutputStream());
	}

	// Inner class
	private static class NullByteArrayOutputStream extends ByteArrayOutputStream {

		@Override
		public void write(int b) {
			// do nothing
		}

		@Override
		public void write(byte[] b, int off, int len) {
			// do nothing
		}

		@Override
		public void writeTo(OutputStream out) throws IOException {
			// do nothing
		}

	}

}