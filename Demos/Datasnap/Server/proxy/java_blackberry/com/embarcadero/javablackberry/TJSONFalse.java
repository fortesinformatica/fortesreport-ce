//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

/**
 * 
 * Contains and returns the boolean value false like a JSON .
 * 
 */

public class TJSONFalse extends TJSONValue {

	public int getJsonValueType() {
		return JSONValueType.JSONFalse;
	}

	/**
	 * Returns the String "false"
	 * 
	 * @return
	 */
	public String asJSONString() {
		return "false";
	}

	public Object getInternalObject() {
		return Boolean.FALSE;
	}

	public String toString() {
		return asJSONString();
	}

}
