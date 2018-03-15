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
 * Contains and returns the boolean value true like a JSON.
 * 
 */

public class TJSONTrue extends TJSONValue {

	/**
	 * Returns the String "true"
	 * 
	 * @return
	 */
	public String asJSONString() {
		return "true";
	}

	public Object getInternalObject() {
		return Boolean.TRUE;
	}

	public int getJsonValueType() {
		return JSONValueType.JSONTrue;
	}

	public String toString() {
		return asJSONString();
	}
}
