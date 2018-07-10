//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

/**
 * 
 * Contains and returns the boolean value false like a JSON . 
 *
 */

public class TJSONFalse extends TJSONValue {

	@Override
	public JSONValueType getJsonValueType() {
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

	@Override
	public Object getInternalObject() {
		return false;
	}

	@Override
	public String toString() {
		return asJSONString();
	}

}
