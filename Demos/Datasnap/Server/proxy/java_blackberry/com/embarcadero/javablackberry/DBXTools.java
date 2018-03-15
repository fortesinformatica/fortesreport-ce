//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;

/**
 * It provides useful methods to convert an input stream.
 */

public class DBXTools {

	/**
	 * Convert a stream into an array of byte
	 * 
	 * @param in
	 *            The InputStream to read the bytes from.
	 * @return A byte array containing the bytes that were read.
	 * @throws IOException
	 *             If I/O error occurred.
	 */
	  public static final byte[] streamToByteArray(InputStream in)
	    throws IOException
	  {
	    ByteArrayOutputStream out = new ByteArrayOutputStream(4096);
	    if(in!=null)
	    	transfer(in, out);
	    out.close();
	    return out.toByteArray();
	  }
	  
	  /**
	   * Transfers all bytes that can be read from <tt>in</tt> to <tt>out</tt>.
	   *
	   * @param in The InputStream to read data from.
	   * @param out The OutputStream to write data to.
	   * @return The total number of bytes transfered.
	   */
	  public static final long transfer(InputStream in, OutputStream out)
	    throws IOException
	  {
	    long totalBytes = 0;
	    int bytesInBuf = 0;
	    byte[] buf = new byte[4096];

	    while ((bytesInBuf = in.read(buf)) != -1) {
	      out.write(buf, 0, bytesInBuf);
	      totalBytes += bytesInBuf;
	    }

	    return totalBytes;
	  }

	/**
	 * Convert a stream in a UTF-8 formatted String
	 * 
	 * @param is
	 *            the stream to convert
	 * @return
	 * @throws IOException
	 */
	public static String convertStreamToString(InputStream is) throws IOException {
		if (is != null) {
			StringBuffer buffere = new StringBuffer();
			try {
				Reader reader = new InputStreamReader(is,
						"UTF-8");
				int n;
				while ((n = reader.read()) > -1) {
					buffere.append((char)n);
				}
			} finally {
				is.close();
			}
			return buffere.toString();
		} else {
			return "";
		}
	}
}
