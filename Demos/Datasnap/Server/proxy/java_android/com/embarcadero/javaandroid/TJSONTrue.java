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
 * Contains and returns the boolean value true like a JSON .
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

	@Override
	public Object getInternalObject() {
		return true;
	}

	@Override
	public JSONValueType getJsonValueType() {
		return JSONValueType.JSONTrue;
	}

	@Override
	public String toString() {
		return asJSONString();
	}
}
