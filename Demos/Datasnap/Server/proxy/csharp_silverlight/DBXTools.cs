//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

using System;
using System.IO;

namespace Embarcadero.Datasnap.WindowsPhone7
{
    public class DBXTools {

	    /**
	       * 
	       * @param input The InputStream to read the bytes from.
	       * @return A byte array containing the bytes that were read.
	       */
	      public static byte[] streamToByteArray(Stream input)
	      {
              int initialLength = 32768;
              byte[] buffer = new byte[initialLength];
              int read = 0;

              int chunk;
              while ((chunk = input.Read(buffer, read, buffer.Length - read)) > 0)
              {
                  read += chunk;
                  if (read == buffer.Length)
                  {
                      int nextByte = input.ReadByte();
                      if (nextByte == -1)
                      {
                          return buffer;
                      }
                      byte[] newBuffer = new byte[buffer.Length * 2];
                      Array.Copy(buffer, newBuffer, buffer.Length);
                      newBuffer[read] = (byte)nextByte;
                      buffer = newBuffer;
                      read++;
                  }
              }
              byte[] ret = new byte[read];
              Array.Copy(buffer, ret, read);
              return ret;
	      }

	      /**
	       * Transfers all bytes that can be read from <tt>in</tt> to <tt>out</tt>.
	       *
	       * @param input The InputStream to read data from.
	       * @param output The OutputStream to write data to.
	       * @return The total number of bytes transfered.
	       */
	      public static long transfer(Stream input, Stream output)
	      {
	        long totalBytes = 0;
	        int bytesInBuf = 0;
	        byte[] buf = new byte[4096];

	        while ((bytesInBuf = input.Read(buf,0,bytesInBuf)) != -1) {
	          output.Write(buf, 0, bytesInBuf);
	          totalBytes += bytesInBuf;
	        }

	        return totalBytes;
	      }

	
  	 /**
  	 * @param ins the input stream
  	 * @return a String that represents the param Stream
  	 */
	    public static String convertStreamToString(TStream ins) {
		    if (ins != null) {
                StreamReader reader = new StreamReader(ins);
                return reader.ReadToEnd();
		    } else {
			    return "";
		    }
	    }

    }
}
