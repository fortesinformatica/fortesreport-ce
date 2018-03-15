//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

public final class TJSONNumber extends TJSONValue {
	protected Double value;

	/**
	 * Class constructor, initialized the internal value to null
	 */
	public TJSONNumber() {
		super();
		value = null;
	}

	/**
	 * Gets the internal value.
	 * 
	 * @return
	 */
	public Double getValue() {
		if (value == null)
			return null;
		else
			return value;
	}

	/**
	 * Class constructor, initialized the internal value by parse the String
	 * passed
	 * 
	 * @param value
	 */
	public TJSONNumber(String value) {
		super();
		this.value = Double.valueOf(value);
	}

	/**
	 * Class constructor, initialized the internal value with the double passed
	 * 
	 * @param value
	 */
	public TJSONNumber(double value) {
		super();
		this.value = new Double(value);
	}

	/**
	 * Class constructor, initialized the internal value with the long passed
	 * 
	 * @param value
	 */
	public TJSONNumber(long value) {
		super();
		this.value = new Double(value);
	}

	/**
	 * Class constructor, initialized the internal value with the int passed
	 * 
	 * @param value
	 */
	public TJSONNumber(int value) {
		super();
		this.value = new Double(value);
	}

	public Object getInternalObject() {
		return value;
	}

	public String toString() {
		if (value != null)
			return value.toString();
		return NullString;
	}

	public int getJsonValueType() {
		return JSONValueType.JSONNumber;
	}
}
