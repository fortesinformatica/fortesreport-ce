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
 * Wrap the AnsiChars type and allows it to be null.
 *
 */

public class TDBXAnsiCharsValue extends DBXValue {
	protected boolean ValueNull = false;
	private String DBXStringValue;

	/**
	 * Class constructor, initialized this {@link DBXDataTypes} like a WideStringType 
	 */
	public TDBXAnsiCharsValue() {
		super();
		setDBXType(DBXDataTypes.WideStringType);
	}

	/**
	 * Sets this object to null
	 */
	public void setNull() {
		ValueNull = true;
		DBXStringValue = "";
	}

	/**
	 * Returns true if this object is null false otherwise.
	 */
	public boolean isNull() {
		return ValueNull;
	}

	/**
	 * Sets the internal value with the value passed
	 */
	public void SetAsString(String Value) throws DBXException {
		ValueNull = false;
		DBXStringValue = Value;
	}

	/**
	 * Returns the internal value
	 */
	public String GetAsString() throws DBXException {
		return DBXStringValue;
	}
}
