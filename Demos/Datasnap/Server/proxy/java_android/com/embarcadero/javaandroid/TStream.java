//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

import java.io.ByteArrayInputStream;

/**
 * 
 * TStream is the base class type for stream objects .
 * 
 * Use the properties and methods of TStream to convert a stream to and from
 * json object.
 */

public class TStream extends ByteArrayInputStream {

	/**
	 * Class constructor , create a new TStream initialized with the specified
	 * array of byte.
	 * 
	 * @param buf
	 */
	public TStream(byte[] buf) {
		super(buf);
	}

	/**
	 * Returns a new TStream create by the value contained in the specified
	 * {@link JSONArray}
	 * 
	 * @param value
	 * @return
	 * @throws JSONException
	 * @throws DBXException
	 */
	public static TStream CreateFrom(TJSONArray value) throws DBXException {
		byte[] b1 = new byte[(int)value.size()];
		for (int i = 0; i < value.size(); i++) {
			b1[i] = value.getInt(i).byteValue();
		}
		return new TStream(b1);
	}

	/**
	 * Returns this Object as an array of byte.
	 * 
	 * @return
	 */
	public byte[] asByteArray() {
		return super.buf.clone();
	}
}
