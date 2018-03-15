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
 * Wrap the AnsiString type and allows it to be null
 *
 */

public class TDBXAnsiStringValue extends DBXValue {
	protected boolean ValueNull = false;
	private String DBXStringValue;

	public TDBXAnsiStringValue() {
		super();
		setDBXType(DBXDataTypes.WideStringType);
	}
	
	public void setNull() {
		ValueNull = true;
		DBXStringValue = null;
	}

	public boolean isNull() {
		return ValueNull;
	}

	public void SetAsString(String Value) throws DBXException {
		ValueNull = false;
		DBXStringValue = Value;
	}

	public String GetAsString() throws DBXException {
		return DBXStringValue;
	}
}
