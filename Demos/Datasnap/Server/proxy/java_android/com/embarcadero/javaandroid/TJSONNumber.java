//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

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
		this.value = Double.parseDouble(value);
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

	@Override
	public Object getInternalObject() {
		return value;
	}

	@Override
	public String toString() {
		if (value != null)
			return value.toString();
		return NullString;
	}

	@Override
	public JSONValueType getJsonValueType() {
		return JSONValueType.JSONNumber;
	}
}