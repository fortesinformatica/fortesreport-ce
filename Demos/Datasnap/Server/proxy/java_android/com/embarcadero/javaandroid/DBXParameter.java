//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;


/**
 * Wraps a DBXValue and allows it to represent itself as a JSON
 */

public class DBXParameter extends DBXValueType {
	private DBXWritableValue Value;

	/**
	 * Class constructor
	 */
	public DBXParameter() {
		super();
		Value = new DBXWritableValue();
		Value.Clear();
	}

	/**
	 * It returns internal {@link DBXWritableValue}
	 * 
	 * @return
	 */
	public DBXWritableValue getValue() {
		return Value;
	}

	/**
	 * It set the data Type according to {@link DBXDataTypes} values.
	 */
	public void setDataType(int dataType) {
		Value.setDBXType(dataType);
	}

	/**
	 * JSON representation of a TParam
	 * 
	 * @return a JSONArray that represents a TParam
	 */
	public TJSONArray tojson() {
		TJSONArray arr = new TJSONArray();
		arr.add(getName());
		arr.add(getDataType());
		arr.add(getOrdinal());
		arr.add(getSubType());
		arr.add(getScale());
		arr.add(getSize());
		arr.add(getPrecision());
		arr.add(getChildPosition());
		arr.add(getNullable());
		arr.add(getHidden());
		arr.add(getParameterDirection());
		arr.add(getValueParameter());
		arr.add(getLiteral());
		return arr;
	}

	/**
	 * Returns the specified DBXType value of the wrapped value
	 */
	@Override
	public int getDataType() {
		return getValue().getDBXType();
	}
}
