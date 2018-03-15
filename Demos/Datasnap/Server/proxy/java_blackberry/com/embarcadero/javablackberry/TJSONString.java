//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

/**
 * Represents json string objects.
 * 
 */
public class TJSONString extends TJSONValue {

	protected String value;

	/**
	 * Class constructor, initialized the internal value to null
	 */
	public TJSONString() {
		super();
		value = null;
	}

	/**
	 * Class constructor, initialized the internal value with the value passed
	 */
	public TJSONString(String value) {
		super();
		this.value = value;
	}

	public Object getInternalObject() {
		return value;
	}

	public String toString() {
		if (value != null)
			return value;
		return NullString;
	}

	public int getJsonValueType() {
		return JSONValueType.JSONString;
	}

	/**
	 * Gets the internal value
	 * 
	 * @return
	 */
	public String getValue() {
		return value;
	}
}
