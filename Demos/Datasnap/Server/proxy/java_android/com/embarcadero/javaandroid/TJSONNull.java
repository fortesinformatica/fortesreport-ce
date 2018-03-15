//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

import org.json.JSONObject;

/**
 * 
 * Contains and returns the 'null' value like a JSON . 
 *
 */


public class TJSONNull extends TJSONValue {

	@Override
	public JSONValueType getJsonValueType() {
		return JSONValueType.JSONNull;
	}	
	
	/**
	 * Returns the String "null"
	 * @return
	 */
	public String asJSONString() {
		return "null";
	}
	
	@Override
	public Object getInternalObject() {
		return JSONObject.NULL;
	}

	@Override
	public String toString() {
		return asJSONString();
	}	
}