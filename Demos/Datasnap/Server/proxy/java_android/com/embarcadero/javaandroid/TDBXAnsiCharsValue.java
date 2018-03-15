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
 * Wraps the AnsiChars type and allows it to be null
 *
 */

public class TDBXAnsiCharsValue extends DBXValue {
	protected boolean ValueNull = false;
	private String DBXStringValue;

	public TDBXAnsiCharsValue() {
		super();
		setDBXType(DBXDataTypes.WideStringType);
	}
	
	@Override
	public void setNull() {
		ValueNull = true;
		DBXStringValue = "";
	}

	public boolean isNull() {
		return ValueNull;
	}

	@Override
	public void SetAsString(String Value) throws DBXException {
		ValueNull = false;
		DBXStringValue = Value;
	}

	@Override
	public String GetAsString() throws DBXException {
		return DBXStringValue;
	}
}
