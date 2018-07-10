//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

import org.json.me.bc.JSONObject;

/**
 * Contains and returns the 'null' value like a JSON .
 */

public class TJSONNull extends TJSONValue {

	public static Object Null = JSONObject.NULL;
	
	public int getJsonValueType() {
		return JSONValueType.JSONNull;
	}

	/**
	 * Returns the String "null"
	 * 
	 * @return
	 */
	public String asJSONString() {
		return "null";
	}

	public Object getInternalObject() {
		return JSONObject.NULL;
	}

	public String toString() {
		return asJSONString();
	}
}